-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--                 ORBITER MODULE: Atlantis
--                  Part of the ORBITER SDK
--
-- Atlantis_Tank.lua
-- Reference implementation of Atlantis Tank vessel class module
-- Note: This module takes control of the tank after separation
-- from the orbiter.
--
-- Port to Lua by TheGondos
-- ==============================================================

LOX_MAX_PROPELLANT_MASS = 624252.0
LH2_MAX_PROPELLANT_MASS = 104463.23
TANK_MAX_PROPELLANT_MASS = LOX_MAX_PROPELLANT_MASS + LH2_MAX_PROPELLANT_MASS
-- Main tank propellant mass [kg]

TANK_EMPTY_MASS = 29937.0
-- Main tank empty mass (assume light-weight tank, STS-6 to STS-90)

THRUSTPITCH_LAUNCH = -2.3*RAD
THRUSTGIMBAL_LAUNCH = _V(0, math.sin(THRUSTPITCH_LAUNCH), math.cos(THRUSTPITCH_LAUNCH))

tdvtx = {
	{ pos=_V( 0,    5.85,   3.1  ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 2.4,  5.1,   -21.3 ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-2.4,  5.1,   -21.3 ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 4.2,  0,     -21   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 2.1, -3.637, -21   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-2.1, -3.637, -21   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-4.2,  0,     -21   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 4.2,  0,      15   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 2.1, -3.637,  15   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-2.1, -3.637,  15   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-4.2,  0,      15   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V(-2.1,  3.637,  15   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 2.1,  3.637,  15   ), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 0,    0,     -23.57), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 },
	{ pos=_V( 0,    0,      23.57), stiffness=1e9, damping=1e7, mu=0.3, mu_lng=0.1 }
}

-- ==============================================================
-- Specialised vessel class Atlantis_Tank
-- ==============================================================

-- Constructor
function create_vessel()
	-- preload mesh
	hTankMesh = oapi.load_meshglobal("Atlantis/Atlantis_tank")

	-- The fuel tank. Note that this is accessed remotely by the orbiter
	-- while it is connected
	hProp = vi:create_propellantresource(TANK_MAX_PROPELLANT_MASS)

	-- docking ports
	vi:create_dock(_V(    0, 5.5, -5  ), _V( 0, 1, 0), _V(0, 0, 1)) -- orbiter attachment
	vi:create_dock(_V(-4.25,   0, -6.4), _V(-1, 0, 0), _V(0, 0, 1)) -- left SRB attachment
	vi:create_dock(_V( 4.25,   0, -6.4), _V( 1, 0, 0), _V(0, 0, 1)) -- right SRB attachment

	-- by default, disable orbiter and SRB connectors
	hDockOrbiter = nil

	pSRB={}
	--pSRB[0] = nil
	--pSRB[1] = nil
end

-- ==============================================================
-- Callback functions
-- ==============================================================

-- Set Tank class specs
function clbk_setclasscaps(cfg)
	create_vessel()

	vi:set_enablefocus(false)
	-- Tank cannot receive input focus

	vi:set_size(24.0)
	vi:set_emptymass(TANK_EMPTY_MASS)

	--SetMaxFuelMass (TANK_MAX_PROPELLANT_MASS);
	-- Note that the Tank instance is only created after separation from
	-- the orbiter, so the actual fuel mass will always be much smaller

	-- SetISP (5000.0); -- no thrusters

	--SetMaxThrust (ENGINE_MAIN, 0);
	--SetMaxThrust (ENGINE_RETRO, 0);
	--SetMaxThrust (ENGINE_HOVER, 0);
	--SetMaxThrust (ENGINE_ATTITUDE, 0);
	-- Tank has no engines of its own

	vi:set_cw({x=1.2, y=1.2, z=0.2, zn=0.3})
	vi:set_crosssections(_V(412.1, 411.8, 72.7))
	vi:set_rotdrag(_V(0.5, 0.5, 0.1))
	vi:set_pmi(_V(145.6, 145.6, 10.5))
	vi:set_pitchmomentscale(1e-4)
	vi:set_yawmomentscale(1e-4)

	-- ************************* docking port **************************************

	vi:set_cameraoffset(_V(0, 0, 0))
	-- Note that the camera offset should not be required
	-- since the Tank doesn't define a 'cockpit'

	--SetCOG_elev (-5.0); -- obsolete
	--SetTouchdownPoints (_V(0,9,3), _V(-1,1,-3), _V(1,1,-3));
	vi:set_touchdownpoints(tdvtx)
	--SetLiftCoeffFunc (0);

	vi:add_mesh(hTankMesh)
end

-- Simulation time step
function clbk_prestep(simt, simdt, mjd)
	pSRB[0] = get_SRB(0)
	pSRB[1] = get_SRB(1)
end

function clbk_poststep(simt, simdt, mjd)
	if vi:get_altitude() < 0.0 then
		oapi.del_vessel(vi:get_handle())
	end
end

function get_SRB(which)
	if which < 0 or which >= 2 then
		return nil
	end

	local hV = vi:get_dockstatus(vi:get_dockhandle(which+1))
	if hV == nil then
		return nil
	end

	pV = vessel.get_interface(hV)
	if pV:get_classname() == "Atlantis_SRB.lua" then
		return pV
	else
		return nil
	end
end

function enable_orbiter_connector()
	--if (!hDockOrbiter) {
	--	hDockOrbiter = CreateDock (_V(0.0, 4.64, -9.285), _V(0,1,0), _V(1,0,0));
	--}
end

function get_main_propellant_mass()
	return vi:get_propellantmass(hProp)
end

function get_prop_handle()
	return hProp
end

function set_SRB_launch_elevation(elev)
	local lsrb = get_SRB(0)
	if lsrb then
		lsrb:set_launch_elevation(elev)
	end

	local rsrb = get_SRB(1)
	if rsrb then
		rsrb:set_launch_elevation(elev)
	end

end

function get_SRB_thrust_level(which)
	return pSRB[which] and pSRB[which]:get_thrust_level() or 0.0
end

function set_SRB_gimbal(angle)
	local dir = {}
	if pSRB[0] then
		dir.x = -math.sin(angle.y)            -- yaw gimbal
		dir.y = math.sin(angle.x - angle.z)   -- pitch+roll gimbal
		dir.z = math.sqrt(1.0 - dir.x * dir.x - dir.y * dir.y)
		pSRB[0]:set_thrust_gimbal(dir)
	end
	if pSRB[1] then
		dir.x = math.sin(angle.y)             -- yaw gimbal
		dir.y = math.sin(-angle.x - angle.z)  -- pitch+roll gimbal 
		dir.z = math.sqrt(1.0 - dir.x * dir.x - dir.y * dir.y)
		pSRB[1]:set_thrust_gimbal(dir)
	end
end

function get_SRB_thrust_dir(which)
	if pSRB[which] then
		return pSRB[which]:get_thrust_gimbal()
	else
		return _V(0, 0, 1)
	end
end

function ignite_SRBs()
	local ok = true
	for i=0,1 do
		if pSRB[i] then
			ok = ok and pSRB[i]:ignite()
			if ok then
				local dir = THRUSTGIMBAL_LAUNCH
				if i ~= 0 then
					dir.y = -dir.y
				end
				pSRB[i]:set_thrust_gimbal(dir)
			end
		else
			ok = false
		end
	end
	return ok
end

function separate_SRBs()
	for i=0,1 do
		if pSRB[i] then
			local angle = 0.25*PI
			dir = _V(0.0, 0.04, 0.9992)
			if i == 1 then
				dir.y = -dir.y
			end
			vi:undock(i + 1)
			pSRB[i]:cmd_thrust_gimbal(dir) -- set SRB gimbals for safe separation
			pSRB[i]:fire_bolt()
			pSRB[i] = nil
		end
	end
end
