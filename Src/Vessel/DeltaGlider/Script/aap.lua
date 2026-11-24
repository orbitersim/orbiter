-- Set up autopilot structures
aap = {}
aap.altid = nil
aap.spdid = nil
aap.bnkid = nil
aap.hdgid = nil
aap.tgtalt = nil
aap.tgtspd = nil
aap.tgtbnk = nil
aap.tgthdg = nil
aap.maxasc = 30
aap.maxdsc = -20

-- -----------------------------------------------------------
-- Speed autopilot parameters (2026-11-22 rework)
--
-- The throttle is an integrator (throttle -> acceleration -> speed), so the
-- speed loop keeps an integrator, but as a proper cascade:
--   outer (speed):      speed error -> target acceleration
--   inner (acceleration): acceleration error -> throttle integrator (+ anti-windup)
-- The acceleration feedback is obtained from the force vector (get_forcevector
-- projected onto the airspeed direction, divided by the mass) instead of the
-- noisy numerical derivative of the airspeed. The old law also had no
-- anti-windup, a 1 m/s deadband and a fixed 0.8/0.2 derivative filter whose
-- time constant depended on dt: together they produced the 0/1 PWM feel.
-- -----------------------------------------------------------
-- Speed autopilot parameters (2026-11-22, classic PID)
--
-- The speed loop is a straight PID on the airspeed whose output is the main
-- throttle position:
--   e = spd_tgt - spd;  thr = clamp(Kp*e + Ki*int(e) + Kd*(-dspd/dt), 0, 1)
-- The derivative acts on the measurement (airspeed derivative, so target
-- steps do not produce a derivative kick) and includes wind/gust effects.
-- The integral has an anti-windup limit; without it the throttle saturates
-- and the loop flips between 0 and 1 (the old PWM feel). The airspeed
-- derivative is low-pass filtered with a fixed time constant, independent of
-- the simulation step size.
aap.kp_spd      = 0.05    -- PID P: throttle per speed error [1/(m/s)]
aap.ki_spd      = 0.01    -- PID I: throttle per speed error integral [1/(m/s*s)]
aap.kd_spd      = 0.10    -- PID D: throttle per acceleration [1/(m/s^2)]
aap.i_max       = 0.5     -- integral term limit [throttle]
aap.db_vel      = 0.2     -- speed error deadband [m/s]
aap.thrott_rate = 2.0     -- max throttle change rate [1/s]
aap.acc_tau     = 0.15    -- acceleration low-pass time constant [s]
aap.i_spd       = 0.0     -- integral state (internal)
aap.acc_f       = 0.0     -- filtered acceleration (internal state)

-- -----------------------------------------------------------
-- Heading/bank autopilot control law (2026-11-22 rework)
--
-- The old implementation used two independent background coroutines with
-- integrator ("rate") laws:
--   heading: d(tgtbank)/dt = -kp*dhdg + kd*hdg_rate
--   bank:    d(ail)/dt     = -kp*dbank  + kd*bank_rate
-- Two cascaded integrators, unfiltered numerical derivatives and a one-frame
-- inter-coroutine delay made the heading loop oscillate at low frequency.
-- The rework uses a plain cascade of proportional-derivative laws with direct
-- outputs (no integrators) and body angular rates from get_angvel() instead of
-- numerical derivatives:
--   outer (position): bank_tgt = -kp_hdg * heading_error + kd_hdg * yaw_rate
--   inner (attitude): ail      = -kp_bank * bank_error  + kd_bank * roll_rate
-- Both are evaluated in the same frame (the outer loop calls the inner loop);
-- aap.bank() keeps working as a stand-alone bank-hold mode using the same
-- inner loop.
--
-- Sign conventions (verified against the core implementation):
--   get_yaw()  increases clockwise (to the right)   -> yaw_rate  = angvel.y
--   get_bank() is positive for a left bank           -> roll_rate = angvel.z
--   positive AILERON level rolls right (BANKRIGHT)
-- -----------------------------------------------------------

-- Tuning parameters live in the aap table so they can be changed live from the
-- Lua console without reloading the script (e.g. aap.kp_bank = 1.2).
--
-- Second iteration (2026-11-22): the first direct-PD version still produced a
-- constant-amplitude limit cycle. Aileron authority is proportional to the
-- dynamic pressure; with fixed gains the inner loop is heavily over-gained at
-- high q (e.g. 10 km / 400 m/s -> q ~ 33 kPa). The loop gains are therefore
-- normalised by a reference dynamic pressure, the aileron rate limit is
-- relaxed (it could itself act as a slew-rate limit cycle) and the angular
-- rates are low-pass filtered with a step-size independent time constant.
aap.kp_hdg   = 0.4       -- outer loop gain [rad bank per rad heading error]
aap.kd_hdg   = 4.0       -- outer loop turn-rate damping [rad bank per rad/s]
aap.kp_bank  = 3.0       -- inner loop gain [aileron per rad bank error]
aap.kd_bank  = 1.0       -- inner loop roll-rate damping [aileron per rad/s]
aap.bank_max = 0.3*PI    -- target bank limit [rad]
aap.ail_rate = 3.0       -- max aileron change rate [1/s] (step-size guard)
aap.q_ref    = 5000.0    -- reference dynamic pressure for gain normalisation [Pa]
aap.q_min    = 0.25      -- gain scale bound at high dynamic pressure
aap.q_max    = 4.0       -- gain scale bound at low dynamic pressure
aap.rate_tau = 0.06      -- angular rate low-pass time constant [s]
aap.rate_y_f = 0.0       -- filtered turn rate (internal state)
aap.rate_z_f = 0.0       -- filtered roll rate (internal state)

local function wrap_pi (a)
    if a > PI then a = a - 2*PI elseif a < -PI then a = a + 2*PI end
    return a
end

-- Gain normalisation: the aileron roll authority is proportional to the
-- dynamic pressure, so the inner loop gains must scale as q_ref/q.
local function q_scale ()
    local q = v:get_dynpressure()
    if q == nil or q < 100.0 then q = 100.0 end
    local s = aap.q_ref / q
    if s < aap.q_min then s = aap.q_min elseif s > aap.q_max then s = aap.q_max end
    return s
end

-- First-order low-pass with a fixed time constant: behaves the same for
-- variable and fixed steps (the old code used a fixed 0.8/0.2 coefficient,
-- i.e. a time constant that changed with dt).
local function rate_filter (prev, rate, dt)
    local a = dt / (aap.rate_tau + dt)
    return prev + a*(rate - prev)
end

-- Inner loop (attitude): bank error + roll rate -> aileron, direct output.
-- Called from the heading loop (same frame) and from the stand-alone
-- bank-hold mode. The aileron change rate is limited so that large fixed
-- steps cannot turn the direct output into a bang-bang control.
local function bank_control (bank_tgt, angvel, dt)
    local scale = q_scale()
    aap.rate_z_f = rate_filter(aap.rate_z_f, angvel.z, dt)
    local bank_err = bank_tgt - v:get_bank()
    local ail = (-aap.kp_bank*bank_err + aap.kd_bank*aap.rate_z_f)*scale
    local prev = v:get_adclevel(AIRCTRL.AILERON)
    local dmax = aap.ail_rate*dt
    if ail > prev+dmax then ail = prev+dmax elseif ail < prev-dmax then ail = prev-dmax end
    if ail > 1 then ail = 1 elseif ail < -1 then ail = -1 end
    v:set_adclevel(AIRCTRL.AILERON, ail)
end

-- Define help page
dg_aap = {file='Html/Script/Stockvessels/DG/aap.chm'}

function setvessel (_v)
    if _v then
        v = _v
        class = _v:get_classname()
        if (class ~= 'DeltaGlider') and (class ~= 'DG-S') then
            term.out('Warning: Autopilot is designed for use with DeltaGlider.')
        end
    end
end

function altitude ()
    alt = v:get_altitude()
    return alt
end

function airspeed ()
    return v:get_airspeed()
end

function slope ()
    as = v:get_airspeedvector(REFFRAME.HORIZON)
    xz = math.sqrt (as.x^2 + as.z^2)
    sl = math.atan2 (as.y, xz)
    return sl
end

--[[ --------------------------------------------------------
altitude autopilot (elevator control)
argument 1: target altitude (m)
note: does not terminate (run as background job and kill
      when done)
--]]

function alt_ap (alt)
    if alt == nil then
        alt = 20e3
    end
    aap.tgtalt = alt
    local dslope0 = 0
    local dslope_rate_f = 0
    -- Tuning (2026-11-21): the original control law used an effective P gain of
    -- 1.0 (elevator rate per slope error) and a D gain of 10 with the raw
    -- numerical derivative of the slope error. The D term amplified the
    -- step-to-step noise of the derivative, which showed up as a high frequency
    -- elevator chatter. Gains are reduced, the derivative is low-pass filtered,
    -- a small altitude deadband and an elevator rate limit were added. The
    -- outer height-error -> target-slope mapping is unchanged.
    local kp_slope = 0.3      -- was 1.0  [1/s]
    local kd_slope = 2.0      -- was 10.0
    local rate_max = 1.0      -- max elevator change rate [1/s] (was unlimited)
    local db_alt   = 2.0      -- altitude deadband [m] (was none)
    while true do
        if aap.tgtalt ~= nil then
            local dt = oapi.get_simstep()
            if dt < 1e-6 then dt = 1e-6 end
            alt = altitude()
            dalt = aap.tgtalt-alt
            tgt_slope = RAD*1e-2 * dalt
	        if tgt_slope > aap.maxasc*RAD then
		        tgt_slope = aap.maxasc*RAD
	        elseif tgt_slope < aap.maxdsc*RAD then
		        tgt_slope = aap.maxdsc*RAD
	        end
            dslope = tgt_slope-slope()
            if dslope0 == 0 then
                dslope_rate = 0
            else
                dslope_rate = (dslope-dslope0)/dt
            end
            dslope0 = dslope
            dslope_rate_f = 0.8*dslope_rate_f + 0.2*dslope_rate   -- low-pass filter
            local delev = 0
            if math.abs(dalt) > db_alt then
                delev = (dslope*kp_slope + dslope_rate_f*kd_slope)*dt
                local dmax = rate_max*dt
                if delev > dmax then delev = dmax elseif delev < -dmax then delev = -dmax end
            end
            elev = v:get_adclevel(AIRCTRL.ELEVATOR)
            elev = elev+delev;
            if elev > 1 then elev = 1 elseif elev < -1 then elev = -1 end
            v:set_adclevel(AIRCTRL.ELEVATOR,elev)
        end
        proc.skip()
    end
end

--[[ --------------------------------------------------------
airspeed autopilot (throttle control)
argument 1: target speed (m/s)
note: does not terminate (run as background job and kill
      when done)
--]]

function spd_ap (spd)
    if spd ~= nil then aap.tgtspd = spd end
    -- Classic PID speed autopilot (output = main throttle position).
    -- The derivative term uses the measured airspeed derivative (with a fixed
    -- time-constant low-pass) so that wind/gust disturbances are seen
    -- immediately, and a target speed step produces no derivative kick.
    local spd0 = airspeed()
    local i_spd = 0.0
    aap.acc_f = 0.0
    while true do
        if aap.tgtspd ~= nil then
            local dt = oapi.get_simstep()
            if dt < 1e-6 then dt = 1e-6 end
            local speed = airspeed()

            -- measured acceleration (D term) with fixed-tau low-pass
            local acc = (speed - spd0)/dt
            spd0 = speed
            local af = dt/(aap.acc_tau + dt)
            aap.acc_f = aap.acc_f + af*(acc - aap.acc_f)

            local e = aap.tgtspd - speed
            local thr = v:get_thrustergrouplevel(THGROUP.MAIN)
            local newth = thr
            if math.abs(e) > aap.db_vel then
                -- integral with anti-windup: only integrate while the output
                -- is not saturated in the direction of the error
                i_spd = i_spd + e*dt
                if i_spd > aap.i_max then i_spd = aap.i_max elseif i_spd < -aap.i_max then i_spd = -aap.i_max end
                newth = aap.kp_spd*e + aap.ki_spd*i_spd - aap.kd_spd*aap.acc_f
                -- throttle rate limit (step-size guard against bang-bang)
                local dmax = aap.thrott_rate*dt
                if newth > thr+dmax then newth = thr+dmax elseif newth < thr-dmax then newth = thr-dmax end
                -- roll back the integral if the output is saturated and the
                -- error keeps pushing in the same direction
                if (newth <= 0.0 and e > 0.0) or (newth >= 1.0 and e < 0.0) then
                    i_spd = i_spd - e*dt
                end
            end
            if newth < 0.0 then newth = 0.0 elseif newth > 1.0 then newth = 1.0 end
            aap.i_spd = i_spd
            v:set_thrustergrouplevel(THGROUP.MAIN, newth)
        end
        proc.skip()
    end
end

--[[ --------------------------------------------------------
bank autopilot (aileron control)
argument 1: bank angle [deg]
note: does not terminate (run as background job and kill
      when done)
--]]

function bank_ap (bnk)
    aap.tgtbnk = bnk
    -- Stand-alone bank-hold mode (script API only). It reuses the common PD
    -- inner loop; the heading autopilot runs the same inner loop inline.
    while true do
        local dt = oapi.get_simstep()
        if dt < 1e-6 then dt = 1e-6 end
        if aap.tgtbnk ~= nil then
            bank_control(aap.tgtbnk*RAD, v:get_angvel(), dt)
        end
        proc.skip()
    end
end

--[[ --------------------------------------------------------
heading autopilot (aileron+pitch control)
argument 1: heading angle [deg]
note: does not terminate (run as background job and kill
      when done)
--]]

function heading_ap (hdg)
    aap.tgthdg = hdg
    -- Heading autopilot: PD cascade with the bank inner loop, executed in the
    -- same frame (no second coroutine, no inter-coroutine delay).
    -- Outer loop (position): heading error and turn rate -> target bank.
    -- Inner loop (attitude): common bank_control() -> aileron.
    while true do
        local dt = oapi.get_simstep()
        if dt < 1e-6 then dt = 1e-6 end
        if aap.tgthdg ~= nil then
            local angvel = v:get_angvel()
            aap.rate_y_f = rate_filter(aap.rate_y_f, angvel.y, dt)
            local hdg_err = wrap_pi(aap.tgthdg*RAD - v:get_yaw())
            local tgtbank = -aap.kp_hdg*hdg_err + aap.kd_hdg*aap.rate_y_f
            if tgtbank > aap.bank_max then tgtbank = aap.bank_max elseif tgtbank < -aap.bank_max then tgtbank = -aap.bank_max end
            aap.tgtbnk = tgtbank*DEG
            bank_control(tgtbank, angvel, dt)
        end
        proc.skip()
    end
end

-- -----------------------------------------------------------
-- User level entry functions
-- -----------------------------------------------------------

-- Invoke altitude autopilot as background job

function aap.alt (alt)
    if alt == nil then
        if aap.altid ~= nil then
            term.out('AP: altitude autopilot disabled')
            proc.kill(aap.altid)
            aap.altid = nil
            v:set_adclevel(AIRCTRL.ELEVATOR,0)
        end
    else
        term.out('AP: altitude target '..alt..' m')
        if aap.altid == nil then
            aap.altid = proc.bg(alt_ap,alt) -- launch altitude autopilot
        else
            aap.tgtalt = alt -- autopilot already running: just reset altitude
        end
    end
end

-- Invoke speed autopilot as background job

function aap.spd (spd)
    if spd == nil then
        if aap.spdid ~= nil then
            term.out('AP: airspeed autopilot disabled')
            proc.kill(aap.spdid)
            aap.spdid = nil
        end
    else
        term.out('AP: airspeed target '..spd..' m/s')
        if aap.spdid == nil then
            aap.spdid = proc.bg(spd_ap,spd) -- launch airspeed autopilot
        else
            aap.tgtspd = spd -- autopilot already running: just reset target speed
        end
    end
end

-- Invoke bank autopilot as background job

function aap.bank (bank)
    if bank == nil then
        if aap.bnkid ~= nil then
            term.out('AP: bank autopilot disabled')
            proc.kill(aap.bnkid)
            aap.bnkid = nil
            v:set_adclevel(AIRCTRL.AILERON,0)
        end
    else
        term.out('AP: bank target '..bank..' deg')
        if aap.bnkid == nil then
            if aap.hdgid ~= nil then
                -- the bank-hold mode takes over the aileron: stop the heading
                -- loop so that the two controllers do not fight
                proc.kill(aap.hdgid)
                aap.hdgid = nil
            end
            aap.bnkid = proc.bg(bank_ap,bank) -- launch bank autopilot
        else
            aap.tgtbnk = bank -- autopilot already running: just reset target angle
        end
    end
end

-- Invoke heading autopilot as background job

function aap.hdg (hdg)
    if hdg == nil then
        if aap.hdgid ~= nil then
            term.out('AP: heading autopilot disabled')
            proc.kill(aap.hdgid)
            aap.hdgid = nil
            v:set_adclevel(AIRCTRL.AILERON, 0) -- level the wings
        end
    else
        term.out('AP: heading target '..hdg..' deg')
        if aap.hdgid == nil then
            -- the heading loop drives the aileron itself: stop a stand-alone
            -- bank-hold mode (if any) so that the two controllers do not fight
            aap.bank()
            aap.hdgid = proc.bg(heading_ap,hdg) -- launch heading autopilot
        else
            aap.tgthdg = hdg -- autopilot already running: just reset target angle
        end
    end
end

-- -----------------------------------------------------------
-- Initialisation code

v = {}

setvessel(V)

term.out('DG: Atmospheric autopilot loaded.')
term.out('For help, type: help(dg_aap)')