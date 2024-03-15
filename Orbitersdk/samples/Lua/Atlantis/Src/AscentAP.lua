-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--                 ORBITER MODULE: Atlantis
--                  Part of the ORBITER SDK
--
-- AscentAP.lua
-- Class implementation for Atlantis ascent autopilot
-- Automatic control of ascent profile from liftoff to
-- ET separation using engine gimballing of SSME and SRB engines
--
-- Port to Lua by TheGondos
-- ==============================================================

local ascap = {}

-- constants
local SRB_STABILISATION_TIME = 4.0
local PI2   = 6.28318530717958647693
local GGRAV = 6.67259e-11

local function normalize(v)
	return vec.div(v, vec.length(v))
end

-- "class members"
local pitch_profile = {}
local launch_azimuth
local tgt_alt
local ecc_min = 1e10
local t_roll_upright
local launch_lng, launch_lat
local t_launch = oapi.get_simtime()
local met
local met_meco, met_oms_start, met_oms_end, schedule_oms
local met_oms1_start = -1.0
local schedule_oms1 = -1.0
local active
local met_active
local do_oms2
local pt, pspd, acc, pacc, dacc_dt
local pacc_valid

local tgt = {}

function ascap.init()
	active = false
	met_active = false
	do_oms2 = true
	met = 0.0
	met_meco = 0.0
	met_oms_start = 0.0
	met_oms_end = 0.0
	launch_lat = 0.0
	launch_lng = 0.0
	pt = -1.0
	pspd = 0.0
	acc = 0.0
	pacc_valid = false
	ascap.set_defaultprofiles()
end

--------------------------------------------------------------

function ascap.update(simt)
	local eps=1e-5

	tgt.az = ascap.calc_targetazimuth()
	tgt.pitch = ascap.calc_targetpitch()

	if met_active then
		met = simt - t_launch
	end

	if active then
		if status == 0 then
			if met < 0.0 then
				vi:set_thrustergrouplevel(THGROUP.MAIN, math.min(1.0, (SRB_STABILISATION_TIME + met)*0.4))
			else
				t_launch = simt
				t0 = simt
				pET:ignite_SRBs()
				status = 1
			end
		elseif status < 3 then
			if met_meco < 0.0 then
				local apalt, hRef = vi:get_apdist(apalt)
				local fuel_down = pET and pET:get_main_propellant_mass() < 10.0 or false
				apalt = apalt - oapi.get_size(hRef)
				if apalt >= tgt_alt or fuel_down then
					vi:set_thrustergrouplevel(THGROUP.MAIN, 0.0) -- MECO
					met_meco = met
				else
					local th = ascap.SSME_thrustprofile(met)
					if th >= 0.0 then
						vi:set_thrustergrouplevel(THGROUP.MAIN, th)
					end
				end
			elseif met - met_meco >= 10.0 then
				separate_tank()
				local apalt, hRef = vi:get_apdist(apalt)
				apalt = apalt - oapi.get_size(hRef)
				if apalt + 1e3 < tgt_alt then
					schedule_oms1 = met + 20.0
				end
			end
		elseif schedule_oms1 > 0.0 then
			if met >= schedule_oms1 then
				vi:set_thrustergrouplevel(THGROUP.MAIN, 1.0)
				schedule_oms1 = -1.0
				met_oms1_start = met
			end
		elseif met_oms1_start > 0.0 then
			local apalt, hRef = vi:get_apdist(apalt)
			apalt = apalt - oapi.get_size(hRef)
			if apalt >= tgt_alt then
				vi:set_thrustergrouplevel(THGROUP.MAIN, 0.0) -- OMS1 end
				met_oms1_start = -1.0
			end
		elseif do_oms2 then
			if met_oms_start < 0.0 then
				local hRef = vi:get_surfaceref()
				local el, prm = vi:get_elementsex(hRef)
				schedule_oms = prm.ApT - 70.0
				if schedule_oms < 0.0 then
					vi:set_thrustergrouplevel(THGROUP.MAIN, 1.0) -- OMS ignition
					met_oms_start = met
				end
			elseif met_oms_end < 0.0 then
				local perad, hRef = vi:get_pedist()
				local aprad = vi:get_apdist()
				local pealt = perad - oapi.get_size(hRef)
				local ecc = (aprad - perad) / (aprad + perad)
				ecc_min = math.min(ecc_min, ecc)
				if ecc > ecc_min + eps then
					vi:set_thrustergrouplevel(THGROUP.MAIN, 0.0) -- OMS cut off
					met_oms_end = met
					active = false -- turn off ascent autopilot
				end
			end
		else
			active = false
		end
		if not active then
			for i=0, vi:get_thrustercount() - 1 do
				vi:set_thrusterlevel(vi:get_thrusterhandle(i), 0)
			end
		end
	end
end

--------------------------------------------------------------

function ascap.launch()
	if not active and status == 0 and pET then
		local pos = vi:get_equpos()
		launch_lng = pos.lng
		launch_lat = pos.lat
		t_launch = oapi.get_simtime() + SRB_STABILISATION_TIME
		met_meco = -1.0
		met_oms_start = -1.0
		met_oms_end = -1.0
		met_oms1_start = -1.0
		schedule_oms1 = -1.0
		ecc_min = 1e10
		vi:set_rcsmode(RCSMODE.OFF)
		active = true
		met_active = true
	end
end

--------------------------------------------------------------

function ascap.disengage()
	active = false
	for thg = THGROUP.ATT_PITCHUP, THGROUP.ATT_BACK do
		vi:set_thrustergrouplevel(thg, 0.0)
	end
end

--------------------------------------------------------------

function ascap.start_missiontime(simt)
	t_launch = simt + SRB_STABILISATION_TIME
	met_active = true
	return t_launch
end

--------------------------------------------------------------

function ascap.met()
	return met
end

function ascap.get_MET(simt)
	return status == 0 and 0.0 or (simt - t_launch)
end
--------------------------------------------------------------

local p_met = { 0,  5,   10,   20,   30,   40,   50,   60,   70,   80,   90, 100,  120,  140,  164, 195, 250, 300,  420,  530}
local p_val = { 90, 90, 85.8, 76.8, 69.3, 62.8, 56.8, 51.8, 47.5, 42.7, 38.3, 34.8, 28.6, 22.7, 17.0, 12.3, 7.2, 3.8, -1.0, -5.0 }
local n_pitch = 20

function ascap.set_defaultprofiles()
	pitch_profile = {}
	for i = 1, n_pitch do
		pitch_profile[i] = {}
		pitch_profile[i].t = p_met[i]
		pitch_profile[i].v = p_val[i] * RAD
	end

	launch_azimuth = PI05
	tgt_alt = 350e3
	t_roll_upright = 345.0
end

--------------------------------------------------------------

function ascap.SSME_thrustprofile(met)
	local nsample = 5
	local ts = {0, 35, 42, 70, 77}
	local lvls = {1, 1, 0.67, 0.67, 1}

	if status == 0 then -- pre-launch
		if met < 0 then
			return math.min(1.0, (SRB_STABILISATION_TIME + met) * 0.4) -- power up for liftoff
		end
	elseif status < 3 then
		if met < ts[nsample] then
			for i = nsample,1,-1 do
				if met >= ts[i] then
					return (met - ts[i]) / (ts[i + 1] - ts[i]) * (lvls[i + 1] - lvls[i]) + lvls[i]
				end
			end
			return lvls[1]
		elseif met < 400 then
			return 1.0
		else 
			local acc_tgt = 29.5
			local a = -0.05
			local b = -0.01
			local th = -1.0
			local spd = vi:get_groundspeed()
			local t = oapi.get_simtime()
			if pt > 0.0 then
				local dt = t - pt
				if dt > 0.0 then
					acc = (spd - pspd) / dt
				end
				if (pacc_valid) then
					local dacc = acc - pacc
					if dt > 0.0 then
						dacc_dt = dacc / dt
					end
					local dth = a * (acc - acc_tgt) + b * dacc_dt
					th = math.max(0.0, math.min(1.0, vi:get_thrustergrouplevel(THGROUP.MAIN) + dth * dt))
				end
			end
			pt = t
			pspd = spd
			pacc = acc
			pacc_valid = true
			return th
		end
	end
	return 0.0
end

--------------------------------------------------------------

function ascap.set_launchazimuth(azimuth)
	launch_azimuth = azimuth

	-- current launch location in local planet frame
	local saz = math.sin(azimuth)
	local caz = math.cos(azimuth)
	local hRef = vi:get_gravityref()
	local pos = vi:get_globalpos()

	local equ = oapi.global_to_local(hRef, pos)
	local tmp = oapi.local_to_equ(hRef, equ)
	local slng = math.sin(tmp.lng)
	local clng = math.cos(tmp.lng)
	local slat = math.sin(tmp.lat)
	local clat = math.cos(tmp.lat)
	local equ = normalize(equ) -- unit radius vector
	
	-- launch direction in local planet frame
	local dir = {x=-clng*slat*caz - slng*saz, y=clat*caz, z=-slng*slat*caz + clng*saz}

	-- normal of orbital plane in local planet frame
	local nml = vec.crossp(dir, equ)

	-- normal of equator plane in local planet frame
	local ne = _V(0, 1, 0)

	-- direction of ascending node
	local nd = normalize(vec.crossp(nml, ne))

	-- orbit inclination
	tgt.inc = math.acos(vec.dotp(nml, ne))

	-- longitude of ascending node
	tgt.lan = math.atan2(nd.z, nd.x)

	-- rotation matrix from equator plane to target orbit plane
	local sinc = math.sin(tgt.inc)
	local cinc = math.cos(tgt.inc)
	local slan = math.sin(tgt.lan)
	local clan = math.cos(tgt.lan)

	local R1 = _M(1,0,0, 0,cinc,sinc, 0,-sinc,cinc)
	local R2 = _M(clan,0,-slan, 0,1,0, slan,0,clan)
	tgt.R = mat.mmul(R2,R1)
end

--------------------------------------------------------------

function ascap.calc_targetazimuth()
	if status == 0 then
		return launch_azimuth
	end

	local hRef = vi:get_gravityref()
	local pR = oapi.get_rotationmatrix(hRef)
	local pos = vi:get_globalpos()

	local equ = oapi.global_to_local(hRef, pos)    -- vessel position in planet frame
	equ = normalize(equ)
	local ep = mat.tmul(tgt.R, equ)                -- rotate to equator plane
	local elng = math.atan2(ep.z, ep.x)                 -- longitude of rotated position
	local dir = {x=-math.sin(elng), y=0, z=math.cos(elng)}   -- rotated target direction
	dir = mat.mul(tgt.R,dir)                       -- target direction in planet frame
	dir = mat.mul(pR, dir)                         -- target direction in global frame
	local vR = vi:get_rotationmatrix()
	dir = mat.tmul(vR, dir)                        -- target direction in vessel frame
	local hdir = vi:horizonrot(dir)                -- target direction in local horizon frame
	local az = math.atan2(hdir.x,hdir.z)                -- target azimuth

	if status < 3 and met >= t_roll_upright then -- compensate for SSME tilt during roll to avoid azimuth deviation
		local pitch_ofs = 15.1 * RAD
		local bank = vi:get_bank()
		az = az - math.sin(bank) * pitch_ofs
	end
	return az
end

--------------------------------------------------------------

function ascap.calc_targetpitch()
	if status == 0 then
		return PI05
	end

	local tgt_pitch
	if met >= pitch_profile[#pitch_profile].t then
		tgt_pitch = pitch_profile[#pitch_profile].v
	else
		for i=1,#pitch_profile-1 do
			if pitch_profile[i+1].t >= met then
				tgt_pitch = pitch_profile[i].v +
					(pitch_profile[i+1].v - pitch_profile[i].v) * (met-pitch_profile[i].t) / (pitch_profile[i+1].t - pitch_profile[i].t)
				break
			end
		end
	end
	if met >= t_roll_upright then
		local pitch_ofs = 15.1 * RAD
		local bank = vi:get_bank()
		tgt_pitch = tgt_pitch + (math.cos(bank) + 1) * pitch_ofs
	end
	return tgt_pitch
end

--------------------------------------------------------------

function ascap.get_inclination(lat, az)
	local a = PI05-lat
	local B = az
	return PI05 - math.asin(math.sin(a)*math.sin(B))
end

--------------------------------------------------------------

function ascap.get_targetinclination()
	local a = 0.0
	local b = 0.0
	if status == 0 then
		if launch_lat ~= 0.0 and launch_lng ~= 0.0 then
			local pos = vi:get_equpos()
			launch_lng = pos.lng
			launch_lat = pos.lat
		end
		a = PI05 - launch_lat

		-- correct launch azimuth for surface rotation
		local hRef = vi:get_gravityref()
		local R = oapi.get_size(hRef)              -- planet mean radius
		local r = R + tgt_alt                     -- target orbit radius
		local M = oapi.get_mass(hRef)             -- reference body mass
		local v0 = math.sqrt(GGRAV * M / r)       -- target orbit speed
		local vg = PI2 * R / oapi.get_planetperiod(hRef) * math.cos(launch_lat)
		                                          -- surface speed at launch position
		local vx0 = v0 * math.sin(launch_azimuth) -- longitudinal velocity component
		local vx1 = vx0 + vg                      -- corrected for planet rotation
		local vy  = v0 * math.cos(launch_azimuth) -- latitudinal velocity component
		B = math.atan2(vx1, vy)                   -- effective launch azimuth
	end
	return PI05 - math.asin(math.sin(a)*math.sin(B))
end

--------------------------------------------------------------

function ascap.get_targetdirection(met)
	local xz = math.cos(tgt.pitch)
	local dir = vi:horizoninvrot({x=xz * math.sin(tgt.az), y=math.sin(tgt.pitch), z=xz * math.cos(tgt.az)})
	return dir, tgt.az
end

----------------------------------------------------------------

function ascap.active()
	return active
end

function ascap.get_targetrate(met)
	if active then
		if met <= 5.0 then
			return _V(0, 0, 0)
		end

		local pitch_ofs = 15.1 * RAD
		local rate = {}

		local tgtdir, tgt_hdg = ascap.get_targetdirection(met)
		local avel = vi:get_angvel()

		local dpitch = -math.asin(tgtdir.y)
		local dyaw   = -math.atan2(tgtdir.x, tgtdir.z)
		rate.x = ascap.get_targetpitchrate(dpitch, avel.x)
		rate.y = met < 35.0 and 0.0 or ascap.get_targetyawrate(dyaw, avel.y)
		rate.z = met <= 35.0 and ascap.get_targetrollrate(tgt_hdg, true) or
				                ascap.get_targetrollrate(met <= t_roll_upright and PI or 0, false)
		return rate
	else
		return _V(0, 0, 0)
	end
end

--------------------------------------------------------------

function ascap.toggle_OMS2()
	do_oms2 = not do_oms2
end

--------------------------------------------------------------

function ascap.get_targetpitchrate(dpitch, vpitch)
	local a = -0.15
	local b =  0.15
	if dpitch >= PI then
		dpitch = dpitch - PI2
	elseif dpitch < -PI then
		dpitch = dpitch + PI2
	end

--	local bank = vi:get_bank()
	return a*dpitch + b*vpitch
end

--------------------------------------------------------------

function ascap.get_targetyawrate(dyaw, vyaw)
	local a = 0.10
	local b = 0.10
	if dyaw >= PI then
		dyaw = dyaw - PI2
	elseif dyaw < -PI then
		dyaw = dyaw + PI2
	end

	return a*dyaw + b*vyaw
end

--------------------------------------------------------------

function ascap.get_targetrollrate(tgt, tgt_is_heading)
	local a, b, maxrate
	if tgt_is_heading then -- launch roll
		a = 0.60
		b = 0.30
		maxrate = 0.25
	else               -- post launch roll
		a = 0.15
		b = 0.075
		maxrate = 0.15
	end

	local avel = vi:get_angvel()
	local dh
	local droll = avel.z

	if tgt_is_heading then
		local yh = vi:horizonrot(_V(0, 1, 0))
		local yhdg = math.atan2(yh.x, yh.z)
		dh = yhdg - tgt
		if dh > PI then
			dh = dh - PI2
		elseif dh < -PI then
			dh = dh + PI2
		end
	else
		local bank = vi:get_bank()
		dh = bank - tgt
		if dh >= PI then
			dh = dh - PI2
		elseif dh < -PI then
			dh = dh + PI2
		end
	end

	return math.min(maxrate, math.max(-maxrate, a*dh + b*droll))
end

--------------------------------------------------------------

function ascap.save_state(scn)
	oapi.writescenario_string(scn, "MET", string.format("%0.3f %0.3f %0.3f %0.3f", met, met_meco, met_oms_start, met_oms_end))
	oapi.writescenario_string(scn, "ASCENTAP", string.format("%d %d %d %0.0f %0.4f %0.5f %0.5f", active and 1 or 0, met_active and 1 or 0, do_oms2 and 1 or 0, tgt_alt, launch_azimuth, launch_lng, launch_lat))
end

function ascap.parse_scenarioline(line)
	local match = {}
	if scenario_line_match(line, "MET %f %f %f %f", match) then
		met           = match.res[1]
		met_meco      = match.res[2]
		met_oms_start = match.res[3]
		met_oms_end   = match.res[4]
		t_launch      = oapi.get_simtime() - met
		return true
	elseif scenario_line_match(line, "ASCENTAP %b %b %b %f %f %f %f", match) then
		active         = match.res[1]
		met_active     = match.res[2]
		do_oms2        = match.res[3]
		tgt_alt        = match.res[4]
		launch_azimuth = match.res[5]
		launch_lng     = match.res[6]
		launch_lat     = match.res[7]
		return true
	end
	return false
end

function ascap.set_orbitaltitude(alt)
	tgt_alt = alt
end

function ascap.get_launchazimuth()
	return launch_azimuth
end

function ascap.get_targetazimuth()
	return tgt.az
end

function ascap.get_targetpitch()
	return tgt.pitch
end

function ascap.get_orbitaltitude()
	return tgt_alt
end

function ascap.get_OMS2schedule()
	return do_oms2
end

function ascap.active()
	return active
end

function ascap.engage()
	active = true
end

function ascap.get_MT0()
	return t_launch
end

function ascap.get_schedule_oms1()
	return schedule_oms1
end

function ascap.get_schedule_oms()
	return schedule_oms
end
function ascap.get_met_oms1_start()
	return met_oms1_start
end
function ascap.get_met_oms_start()
	return met_oms_start
end
function ascap.get_met_oms_end()
	return met_oms_end
end

return ascap
