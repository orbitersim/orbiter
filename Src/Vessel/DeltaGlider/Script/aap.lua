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
    local dslope_rate
    while true do
        if aap.tgtalt ~= nil then
            local dt = oapi.get_simstep()
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
            delev = (dslope*0.1 + dslope_rate)*(dt*10.0)
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
    local spd0 = airspeed()
    local acc, dpsd
    local alpha = 1
    local beta = 1
    while true do
        if aap.tgtspd ~= nil then
            local dt = oapi.get_simstep()
            spd = airspeed()
            acc = (spd-spd0)/dt
            spd0 = spd
            dspd = aap.tgtspd-spd
            dthrott = (dspd*alpha - acc*beta)*dt
            thrott = v:get_thrustergrouplevel (THGROUP.MAIN)
            v:set_thrustergrouplevel (THGROUP.MAIN, thrott+dthrott)
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
    local bnk0 = v:get_bank()
    while true do
        local dt = oapi.get_simstep()
        bnk = v:get_bank()
        dbnk = bnk-bnk0
        if dbnk < -PI then dbnk = dbnk+2*PI elseif dbnk > PI then dbnk = dbnk-2*PI end -- phase unwrap
        rate = dbnk/dt
        bnk0 = bnk
        dbnk = aap.tgtbnk*RAD - bnk
        if dbnk < -PI then dbnk = dbnk+2*PI elseif dbnk > PI then dbnk = dbnk-2*PI end -- phase unwrap
        dail = (-dbnk*0.1 + rate*0.3)*(dt*5.0) -- the damping term should really depend on atmospheric density
        ail = v:get_adclevel(AIRCTRL.AILERON)
        ail = ail+dail
        if ail > 1 then ail = 1 elseif ail < -1 then ail = -1 end
        v:set_adclevel(AIRCTRL.AILERON,ail)
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
    local hdg0 = v:get_yaw()
    local tgtbank = v:get_bank()
    while true do
        local dt = oapi.get_simstep()
        hdg = v:get_yaw()
        dhdg = hdg-hdg0
        if dhdg < -PI then dhdg = dhdg+2*PI elseif dhdg > PI then dhdg = dhdg-2*PI end -- phase unwrap
        rate = dhdg/dt
        hdg0 = hdg
        dhdg = aap.tgthdg*RAD - hdg
        if dhdg < -PI then dhdg = dhdg+2*PI elseif dhdg > PI then dhdg = dhdg-2*PI end -- phase unwrap
        dbank = (-dhdg*100 + rate*1000)*dt
        tgtbank = v:get_bank() + dbank
        if tgtbank > 0.3*PI then tgtbank = 0.3*PI elseif tgtbank < -0.3*PI then tgtbank = -0.3*PI end
        aap.tgtbnk = tgtbank*DEG
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
            aap.bank() -- kill bank ap
            aap.hdgid = nil
        end
    else
        term.out('AP: heading target '..hdg..' deg')
        if aap.hdgid == nil then
            aap.hdgid = proc.bg(heading_ap,hdg) -- launch heading autopilot
            aap.bank(0) -- invoke bank ap
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