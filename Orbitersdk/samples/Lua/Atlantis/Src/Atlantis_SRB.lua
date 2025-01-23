-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--                 ORBITER MODULE: Atlantis
--                  Part of the ORBITER SDK
--
-- Atlantis_SRB.lua
-- Reference implementation of Atlantis SRB(Space Shuttle - Solid
-- Rocket Booster) vessel class module
-- Note: This module takes control of the SRB after separation
-- from the Shuttle's main tank.
--
-- Port to Lua by TheGondos
-- ==============================================================


-- ==============================================================
-- Specialised vessel class Atlantis_SRB
-- ==============================================================

SRB_MAX_PROPELLANT_MASS = 501673.161
-- SRB propellant mass [kg]

SRB_EMPTY_MASS = 87603.65
-- SRB empty mass [kg]

SRB_ISP0 = 2638.89
SRB_ISP1 = 2368.79
-- SRB vacuum and sea-level fuel-specific impulse [m/s]

SRB_THRUST = 1.41e7 -- 14679131.3

SRB_UNDEFINED=0
SRB_LEFT=1
SRB_RIGHT=2
SRB_GIMBAL_SPEED = 0.5

THRUSTPITCH_LAUNCH = -2.3*RAD
THRUSTGIMBAL_LAUNCH = _V(0, math.sin(THRUSTPITCH_LAUNCH), math.cos(THRUSTPITCH_LAUNCH))

srb_contrail = {
	flags = 0,
	srcsize = 12.0,
	srcrate = 3,
	v0 = 200.0,
	srcspread = 0.25,
	lifetime = 12.0,
	growthrate = 11,
	atmslowdown = 10.0,
	ltype = PARTICLE.DIFFUSE,
	levelmap = PARTICLE.LVL_PSQRT,
	lmin = 0,
	lmax = 0.7,
	atmsmap = PARTICLE.ATM_PLOG,
	amin = 1e-6,
	amax = 0.1,
	tex = nil
}

srb_exhaust = {
	flags = 0,
	srcsize = 6.0,
	srcrate = 40,
	v0 = 250.0,
	srcspread = 0.04,
	lifetime = 0.4,
	growthrate = 20,
	atmslowdown = 6.0,
	ltype = PARTICLE.EMISSIVE,
	levelmap = PARTICLE.LVL_PSQRT,
	lmin = 1,
	lmax = 1,
	atmsmap = PARTICLE.ATM_FLAT,
	amin = 1,
	amax = 1,
	tex = nil
}

srb_bolt = {
	flags = 0,
	srcsize = 8.0,
	srcrate = 20,
	v0 = 0.0,
	srcspread = 0.1,
	lifetime = 0.3,
	growthrate = 16,
	atmslowdown = 3.0,
	ltype = PARTICLE.EMISSIVE,
	levelmap = PARTICLE.LVL_LIN,
	lmin = 0,
	lmax = 1,
	atmsmap = PARTICLE.ATM_FLAT,
	amin = 1,
	amax = 1,
	tex = nil
}

function copytdvtx(tdvtx)
    if type(tdvtx)=="table" then
        local res={}
        for k,v in pairs(tdvtx) do 
            res[k] = copytdvtx(v) 
        end
        return res
    else
        return tdvtx
    end
end


tdvtx = {
	{ pos=_V( 0,       2.5,   -21.18), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 2.1651, -1.25,  -21.18), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-2.1651, -1.25,  -21.18), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 2.1651,  1.25,  -21.18), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 0,      -2.5 ,  -21.18), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-2.1651,  1.25,  -21.18), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 0,       1.85,   18.95), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 1.6021, -0.925,  18.95), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-1.6021, -0.925,  18.95), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 1.6021,  0.925,  18.95), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 0,      -1.85,   18.95), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-1.6021,  0.925,  18.95), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 0,       0,      23.78), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 }
}

function create_vessel()
	-- preload mesh
	hSRBMesh = oapi.load_meshglobal("Atlantis/Atlantis_srb")
end

function set_launch_elevation(elev)
	launchelev = elev
	if launchelev ~= 0.0 then
		launch_tdvtx = copytdvtx(tdvtx)
        for k,v in pairs(launch_tdvtx) do
			v.pos.z = v.pos.z - elev
        end

		vi:set_touchdownpoints(launch_tdvtx)
		vi:set_size(23.0 + elev)
	end
end

function thrust_profile(met)
	-- This thrust profile is adapted from STS 107 Columbia Accident
	-- Investigation Board Working Scenario report
	-- http://caib.nasa.gov/news/working_scenario/pdf/sts107workingscenario.pdf

	local ts   = { 0,      8,      22,     50,     78,     110,    117,    126,  135 }
	local lvls = { 0.9153, 0.9772, 1.0000, 0.7329, 0.8306, 0.5375, 0.1954, 0.05, 0 }
    --             0.848,  0.909,  0.933,  0.688,  0.779,  0.506,  0.182,  0.05, 0

	if met > 0.0 and met < ts[#ts] then
		for i=1,#ts-1 do
			if met > ts[i] and met <= ts[i+1] then
				return (met-ts[i])/(ts[i+1]-ts[i]) * (lvls[i+1]-lvls[i]) + lvls[i]
			end
		end
	end
	return 0.0
end

function get_thrust_level()
	return vi:get_thrusterlevel(th_main)
end

function ignite()
	if vi:get_propellantmass(ph_main) == SRB_MAX_PROPELLANT_MASS then
		vi:set_thrusterlevel(th_main, 1.0)
		t0 = oapi.get_simtime()
		bMainEngine = true
		if launchelev ~= 0.0 then
			vi:set_touchdownpoints(tdvtx) -- reset touchdown  points
			vi:set_size(23.0)
		end
		return true
	end
	return false
end

function fire_bolt()
	vi:set_thrusterlevel(th_bolt, 1.0)
	tsep = oapi.get_simtime()
	bSeparationEngine = true
end

function set_thrust_gimbal(dir)
	vi:set_thrusterdir(th_main, dir)
end

function cmd_thrust_gimbal(dir)
	gimbalcmd = dir
	bGimbalCmd = true
end

function get_thrust_gimbal()
	return vi:get_thrusterdir(th_main)
end

-- ==============================================================
-- Callback functions
-- ==============================================================

-- Set SRB class specs
function clbk_setclasscaps(cfg)
	create_vessel()

	vi:set_enablefocus(false)
	-- SRB cannot receive input focus

	-- *********************** physical parameters *********************************

	vi:set_size(23.0)
	vi:set_emptymass(SRB_EMPTY_MASS)
	vi:set_cw({x=1.5, y=1.4, z=0.1, zn=0.3})

	vi:set_crosssections(_V(162.1, 162.1, 26.6))
	vi:set_rotdrag(_V(0.7, 0.7, 0.1))
	vi:set_pmi(_V(154.3, 154.3, 1.83))
	--vi:set_gravitygradientdamping(10.0)
	vi:set_touchdownpoints(tdvtx)
	-- obsolete SetLiftCoeffFunc(0);

	-- ************************* docking port **************************************

	vi:create_dock(_V(1.95, 0, 5), _V(1, 0, 0), _V(0, 0, 1)) -- ET attachment

	-- ************************* propellant specs **********************************

	ph_main = vi:create_propellantresource(SRB_MAX_PROPELLANT_MASS)

	-- *********************** thruster definitions ********************************

	-- main engine
	th_main = vi:create_thruster({pos=_V(0, 0, -21), dir=THRUSTGIMBAL_LAUNCH, maxth0=SRB_THRUST, hprop=ph_main, isp0=SRB_ISP0, ispr=SRB_ISP1})
	tex = oapi.register_exhausttexture("Exhaust2") -- FIXME possible leak?
	srb_exhaust.tex = oapi.register_particletexture("Contrail2")
	vi:add_exhaust(th_main, 16.0, 2.0, tex)
	vi:add_exhauststream(th_main, _V(0, 0, -30), srb_contrail)
	vi:add_exhauststream(th_main, _V(0, 0, -25), srb_exhaust)

	-- separation bolts
	th_bolt = vi:create_thruster({pos=_V(0, 0, 3.0), dir=_V(-1, 0, 0), maxth0=3e6, hprop=ph_main, isp0=1e7})
	-- for simplicity, the separation bolts directly use SRB propellant. We give
	-- them an insanely high ISP to avoid significant propellant drainage

	vi:add_exhaust(th_bolt, 0.7, 0.1, _V(2.1, 0, -8), _V(-1, 0, 0))
	vi:add_exhaust(th_bolt, 0.7, 0.1, _V(2.1, 0, 11), _V(-1, 0, 0))
	vi:add_exhauststream(th_bolt, _V(2.1, 0, 0), srb_bolt)

	-- ************************ visual parameters **********************************

	vi:add_mesh(hSRBMesh)

	bMainEngine = false
	bSeparationEngine = false
	bGimbalCmd = false
	srbpos = SRB_UNDEFINED
end

-- Finish setup
function clbk_postcreation()
	-- find out which side of the ET we are attached to
	hTank = vi:get_dockstatus(vi:get_dockhandle(0))
	if hTank then
		for i=1,2 do
			hSRB = oapi.get_dockstatus(oapi.get_dockhandle(hTank, i))
			if hSRB == vi:get_handle() then
				srbpos = i -- (i == 1) and SRB_LEFT or SRB_RIGHT
				break
			end
		end
	end
	if srbpos ~= SRB_UNDEFINED then
    	vi:set_thrusterdir(th_bolt, (srbpos == SRB_LEFT) and _V(-0.4, -0.9165, 0) or _V(-0.4, 0.9165, 0))
	end
end


-- Read current state
function clbk_loadstateex(scn, vs)
	for line in scenario_lines(scn) do
		local met = line:match("MET (%S+)")
		if met ~= nil then
			met = tonumber(met)
			t0 = oapi.get_simtime() - met
			bMainEngine = true
		else
			vi:parse_scenario_line_ex(line, vs)
		end
	end
end

-- Write current state
function clbk_savestate(scn)
	if bMainEngine then
		oapi.writescenario_float(scn, "MET", oapi.get_simtime() - t0)
	end
end

-- Simulation time step
function clbk_poststep(simt, simdt, mjd)
	local lvl = 0.0
	local met = 0.0
	if bMainEngine then
		met = simt - t0
		lvl = thrust_profile(met)
		vi:set_thrusterlevel(th_main, lvl)
		if lvl == 0.0 then
			bMainEngine = false
		end
	end
	if bSeparationEngine then
		if simt - tsep > 0.5 then
			vi:del_thruster(th_bolt)
			bSeparationEngine = false
		end
	end
	if bGimbalCmd then
		local dg_max = simdt * SRB_GIMBAL_SPEED
		local gimbalcur = get_thrust_gimbal()
		local gdiff = vec.sub(gimbalcmd, gimbalcur)
		local dg = vec.length(gdiff)
		if dg > dg_max then
			gdiff = vec.mul(gdiff, dg_max / dg)
		else
			bGimbalCmd = false
		end
		set_thrust_gimbal(vec.add(gimbalcur, gdiff))
	end
end
