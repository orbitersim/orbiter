-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--                 ORBITER MODULE: Atlantis
--                  Part of the ORBITER SDK
--
-- LuAtlantis.lua
-- Reference implementation of Atlantis (Space Shuttle) vessel
-- class module
--
-- RMS, grappling and MMU capabilities by Robert Conley
-- Port to Lua by TheGondos
-- ==============================================================

animstate = require("AnimState")
ascap = require("AscentAP")
plop = require("PayloadBay")
local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP
APMFD = require("APMFD")
local CameraMFD = require("CameraMFD")


MFD_SHOWMODELABELS = 1
ARM_OPERATING_SPEED = 0.005
-- RMS arm joint rotation speed (rad/sec)

ORBITER_EMPTY_MASS = 77564.3
ORBITER_MAX_PROPELLANT_MASS = 11284.23 + 2162.622
ORBITER_CS = _V(234.8, 389.1, 68.2)
ORBITER_DOCKPOS = _V(0.0, 2.40, 10.15)
THRUSTPITCH_LAUNCH = -2.3*RAD
THRUSTGIMBAL_LAUNCH = _V(0, math.sin(THRUSTPITCH_LAUNCH), math.cos(THRUSTPITCH_LAUNCH))
THRUSTREF_SSME0     = _V(-1.55, -0.37, -12.5)
THRUSTREF_SSME1     = _V( 1.55, -0.37, -12.5)
THRUSTREF_SSME2     = _V( 0.0,   2.7 , -12.5)
ORBITER_MAIN_THRUST = 2170732.15
ORBITER_MAIN_ISP0 = 453 * 9.80665
ORBITER_MAIN_ISP1 = 363 * 9.80665

THRUSTREF_OMSL = _V(-2.6, 3.3, -12.8)
THRUSTREF_OMSR = _V( 2.6, 3.3, -12.8)
THRUSTDIR_OMSL = _V( 0.19299542, -0.24495572, 0.95013129)
THRUSTDIR_OMSR = _V(-0.19299542, -0.24495572, 0.95013129)
ORBITER_OMS_ISP0 = 316 * 9.80665
ORBITER_OMS_ISP1 = ORBITER_OMS_ISP0 * 0.75
ORBITER_OMS_THRUST = 26700.0

ORBITER_RCS_THRUST = 7740.0
ORBITER_RCS_ISP0 = ORBITER_OMS_ISP0
ORBITER_RCS_ISP1 = ORBITER_RCS_ISP0 * 0.75

LOCALVERTEXLIST = -1
MAX_GRAPPLING_DIST = 0.5
-- max distance between RMS tip and grappling point for successful grappling
SRB_SEPARATION_TIME = 126.0
-- MET: SRB separation

GEAR_OPERATING_SPEED = 0.3
SPEEDBRAKE_OPERATING_SPEED = 0.20284

function msgproc(msg, ...)
	if msg == OAPI_MSG.MFD_OPENEDEX then
		return APMFD(...)
	end
	return 0
end

function register_APMFD()
	local spec = {
		name = "AscentAP",
		key = OAPI_KEY.B,
		msgproc = msgproc
	}
	return vi:register_mfdmode(spec)
end

function register_CameraMFD()
	local spec = {
		name = "CameraMFD",
		key = OAPI_KEY.C,
		msgproc =	function(msg, ...)
						if msg == OAPI_MSG.MFD_OPENEDEX then
							return CameraMFD(...)
						end
						return 0
					end
	}
	return vi:register_mfdmode(spec)
end

-- Vessel constuction
function clbk_new()
	ascapMfdId = register_APMFD()
	cameraMfdId = register_CameraMFD()
	gfont = oapi.create_font(-11, false, "Arial")
	gbrush = oapi.create_brush(_RGB(0,0,0))

	rms_anim = {}
	ssme_anim = {}
	arm_tip = {}

	ascap.init()
	status          = 3
	gear_state = animstate(0, GEAR_OPERATING_SPEED, animstate.CLOSED)
	spdb_state = animstate(0, SPEEDBRAKE_OPERATING_SPEED, animstate.CLOSED)

	local col_diff = _COLOUR4(1,0.8,0.8,0)
	local col_zero = _COLOUR4(0,0,0,0)

	local attenuation = {
		range = 300,
		att0 = 2e-4,
		att1 = 0,
		att2 = 4e-4
	}

	engine_light = vi:add_pointlight(_V(0,0,-25), attenuation, col_diff, col_zero, col_zero)

	load_meshes()
	plop.init({
		hOrbiterVCMesh = hOrbiterVCMesh,
		set_bay_door_position = set_bay_door_position,
		set_radiator_position = set_radiator_position,
		set_Ku_antenna_position = set_Ku_antenna_position
	})

	reset_sat = false

	mfdbright = {}
	for i=0,9 do
		mfdbright[i] = 1.0
	end

	-- propellant resources
	ph_oms = vi:create_propellantresource(ORBITER_MAX_PROPELLANT_MASS) -- OMS propellant
	vi:set_default_propellantresource(ph_oms) -- display OMS tank level in generic HUD

	-- Orbiter engines	
	create_SSME() -- main thrusters
	create_OMS()  -- OMS thrusters (activated only after tank separation)
	create_RCS()  -- Reaction control system (activated only after tank separation)

	-- Animations
	define_animations()

	-- Aerodynamics
	create_airfoils()

	vi:create_dock(ORBITER_DOCKPOS, _V(0, 1, 0), _V(0, 0, -1))
	hDockET = vi:create_dock(_V(0.0, -2.48, 8.615), _V(0, -1, 0), _V(0, 0, 1))
	pET = nil -- ET reference


	center_arm      = false
	arm_moved       = false
	arm_scheduled   = false
	bManualSeparate = false
	ofs_sts_sat     = _V(0, 0, 0)
	do_eva          = false
	do_plat         = false
	do_cargostatic  = false
	vis             = nil
	cargo_static_ofs  = _V(0, 0, 0)

	-- default arm status: stowed
	arm_sy = 0.5
	arm_sp = 0.0
	arm_ep = 0.0
	arm_wp = 0.5
	arm_wy = 0.5
	arm_wr = 0.5
	arm_tip = {}
	arm_tip[0] = _V(-2.26, 1.71, -6.5)
	arm_tip[1] = _V(-2.26, 1.71, -7.5)
	arm_tip[2] = _V(-2.26, 2.71, -6.5)

	sat_attach = vi:create_attachment(false, ofs_sts_sat, _V(0, 1, 0), _V(0, 0, 1), "X")
	rms_attach = vi:create_attachment(false, arm_tip[0], vec.sub(arm_tip[1], arm_tip[0]), vec.sub(arm_tip[2], arm_tip[0]), "G", true)

	-- Entry particle stream
	rps = {
		flags = 0,
		srcsize = 20,
		srcrate = 20,
		v0 = 0,
		srcspread = 0.03,
		lifetime = 0.5,
		growthrate = 100,
		atmslowdown = 3,
		ltype = PARTICLE.DIFFUSE,
		levelmap = PARTICLE.LVL_FLAT,
		lmin = 1,
		lmax = 1,
		atmsmap = PARTICLE.ATM_PLIN,
		amin = 6e7,
		amax = 12e7,
		tex = nil
	}
	vi:add_reentrystream(rps)
end

function clbk_destroy()
	vi:unregister_mfdmode(ascapMfdId)
	vi:unregister_mfdmode(cameraMfdId)
	oapi.release_font(gfont)
	oapi.release_brush(gbrush)
end

--------------------------------------------------------------
-- Initialise the thrusters for the shuttle main engines
--------------------------------------------------------------
function create_SSME()
	-- Not connected to a propellant resource - they connect to the ET's tank as long
	-- as it is attached (checked in clbkPreStep)
	local ssme0 = vi:create_thruster({pos=THRUSTREF_SSME0, dir=THRUSTGIMBAL_LAUNCH, maxth0=ORBITER_MAIN_THRUST, isp0=ORBITER_MAIN_ISP0, ispr=ORBITER_MAIN_ISP1})
	local ssme1 = vi:create_thruster({pos=THRUSTREF_SSME1, dir=THRUSTGIMBAL_LAUNCH, maxth0=ORBITER_MAIN_THRUST, isp0=ORBITER_MAIN_ISP0, ispr=ORBITER_MAIN_ISP1})
	local ssme2 = vi:create_thruster({pos=THRUSTREF_SSME2, dir=THRUSTGIMBAL_LAUNCH, maxth0=ORBITER_MAIN_THRUST, isp0=ORBITER_MAIN_ISP0, ispr=ORBITER_MAIN_ISP1})
	th_main = {}
	th_main[0] = ssme0
	th_main[1] = ssme1
	th_main[2] = ssme2

	thg_main = vi:create_thrustergroup(th_main, THGROUP.MAIN)
	
	gimbal_pos = THRUSTGIMBAL_LAUNCH -- the initial pitch gimbal setting positions the SSMEs to cancel pitch moment in launch configuration

	local tex_main = oapi.register_reentrytexture("Exhaust_atsme")

	for i=0,2 do
		vi:add_exhaust(th_main[i], 30.0, 2.0, tex_main)
	end
end

--------------------------------------------------------------
-- Initialise the thrusters for the orbital maneuvering system
--------------------------------------------------------------
function create_OMS()
	local omsl = vi:create_thruster({pos=THRUSTREF_OMSL, dir=THRUSTDIR_OMSL, maxth0=ORBITER_OMS_THRUST, hprop=ph_oms, isp0=ORBITER_OMS_ISP0, ispr=ORBITER_OMS_ISP1})
	local omsr = vi:create_thruster({pos=THRUSTREF_OMSR, dir=THRUSTDIR_OMSR, maxth0=ORBITER_OMS_THRUST, hprop=ph_oms, isp0=ORBITER_OMS_ISP0, ispr=ORBITER_OMS_ISP1})
	th_oms = {}
	th_oms[0] = omsl
	th_oms[1] = omsr
	-- we don't yet define a thruster group for the OMS engines
	-- They will be assigned to the MAIN group as soon as the ET is jettisoned
	for i=0,1 do
		vi:add_exhaust(th_oms[i], 4.0, 0.5)
	end
end

--------------------------------------------------------------
-- Attitude controls (RCS) during orbital phase
-- Inactive by default. Activated with EnableRCS()
--------------------------------------------------------------

function create_RCS()
	local tex_rcs = oapi.register_reentrytexture("Exhaust_atrcs")
	local eh = 6.0      -- exhaust length scale
	local ew1 = 0.4     -- exhaust width scales
	local ew2 = 0.8

	-- set of attitude thrusters (idealised). The arrangement is such that no angular
	-- momentum is created in linear mode, and no linear momentum is created in rotational mode.
	local th_att_lin0 = vi:create_thruster({pos=_V(0, 0,  15.5), dir=_V(0,  1, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	local th_att_lin3 = vi:create_thruster({pos=_V(0, 0, -15.5), dir=_V(0, -1, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	local th_att_lin2 = vi:create_thruster({pos=_V(0, 0,  15.5), dir=_V(0, -1, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	local th_att_lin1 = vi:create_thruster({pos=_V(0, 0, -15.5), dir=_V(0,  1, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	local th_att_rot0 = th_att_lin0
	local th_att_rot1 = th_att_lin3
	local th_att_rot2 = th_att_lin2
	local th_att_rot3 = th_att_lin1
	vi:create_thrustergroup({th_att_rot0,th_att_rot1}, THGROUP.ATT_PITCHUP)
	vi:create_thrustergroup({th_att_rot2,th_att_rot3}, THGROUP.ATT_PITCHDOWN)
	vi:create_thrustergroup({th_att_lin0,th_att_lin1}, THGROUP.ATT_UP)
	vi:create_thrustergroup({th_att_lin2,th_att_lin3}, THGROUP.ATT_DOWN)

	vi:add_exhaust(th_att_rot0, eh, ew1, _V( 1.60, -0.20,  18.78), _V( 0.4339, -0.8830, -0.1793), tex_rcs) --F2D
	vi:add_exhaust(th_att_rot0, eh, ew1, _V( 1.68, -0.18,  18.40), _V( 0.4339, -0.8830, -0.1793), tex_rcs) --F4D
	vi:add_exhaust(th_att_rot0, eh, ew1, _V(-1.55, -0.20,  18.78), _V(-0.4339, -0.8830, -0.1793), tex_rcs) --F1D
	vi:add_exhaust(th_att_rot0, eh, ew1, _V(-1.63, -0.18,  18.40), _V(-0.4339, -0.8830, -0.1793), tex_rcs) --F3D

	vi:add_exhaust(th_att_rot1, eh, ew1, _V(-3.46,  3.20, -12.30), _V(0, 1, 0), tex_rcs) --L4U
	vi:add_exhaust(th_att_rot1, eh, ew1, _V(-3.46,  3.20, -12.70), _V(0, 1, 0), tex_rcs) --L2U
	vi:add_exhaust(th_att_rot1, eh, ew1, _V(-3.46,  3.20, -13.10), _V(0, 1, 0), tex_rcs) --L1U

	vi:add_exhaust(th_att_rot1, eh, ew1, _V( 3.43,  3.20, -12.30), _V(0, 1, 0), tex_rcs) --R4U
	vi:add_exhaust(th_att_rot1, eh, ew1, _V( 3.43,  3.20, -12.70), _V(0, 1, 0), tex_rcs) --R2U
	vi:add_exhaust(th_att_rot1, eh, ew1, _V( 3.43,  3.20, -13.10), _V(0, 1, 0), tex_rcs) --R1U

	vi:add_exhaust(th_att_rot2, eh, ew1, _V(-0.4 ,  1.10,  18.3 ), _V(0, 1, 0), tex_rcs) --F1U	
	vi:add_exhaust(th_att_rot2, eh, ew1, _V( 0.0 ,  1.15,  18.3 ), _V(0, 1, 0), tex_rcs) --F3U
	vi:add_exhaust(th_att_rot2, eh, ew1, _V( 0.4 ,  1.10,  18.3 ), _V(0, 1, 0), tex_rcs) --F2U

	vi:add_exhaust(th_att_rot3, eh, ew1, _V(-3.1 ,  1.55, -12.45), _V(-0.2844, -0.9481, -0.1422), tex_rcs) --L4D
	vi:add_exhaust(th_att_rot3, eh, ew1, _V(-3.1 ,  1.6 , -12.8 ), _V(-0.2844, -0.9481, -0.1422), tex_rcs) --L2D
	vi:add_exhaust(th_att_rot3, eh, ew1, _V(-3.1 ,  1.65, -13.15), _V(-0.2844, -0.9481, -0.1422), tex_rcs) --L3D

	vi:add_exhaust(th_att_rot3, eh, ew1, _V( 3.15,  1.55, -12.45), _V( 0.2844, -0.9481, -0.1422), tex_rcs) --R4D
	vi:add_exhaust(th_att_rot3, eh, ew1, _V( 3.15,  1.6 , -12.8 ), _V( 0.2844, -0.9481, -0.1422), tex_rcs) --R2D
	vi:add_exhaust(th_att_rot3, eh, ew1, _V( 3.15,  1.65, -13.15), _V( 0.2844, -0.9481, -0.1422), tex_rcs) --R3D

	th_att_lin0 = vi:create_thruster({pos=_V(0, 0,  15.5), dir=_V(-1, 0, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	th_att_lin3 = vi:create_thruster({pos=_V(0, 0, -15.5), dir=_V( 1, 0, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	th_att_lin2 = vi:create_thruster({pos=_V(0, 0,  15.5), dir=_V( 1, 0, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	th_att_lin1 = vi:create_thruster({pos=_V(0, 0, -15.5), dir=_V(-1, 0, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	th_att_rot0 = th_att_lin0
	th_att_rot1 = th_att_lin3
	th_att_rot2 = th_att_lin2
	th_att_rot3 = th_att_lin1
	vi:create_thrustergroup({th_att_rot0,th_att_rot1}, THGROUP.ATT_YAWLEFT)
	vi:create_thrustergroup({th_att_rot2,th_att_rot3}, THGROUP.ATT_YAWRIGHT)
	vi:create_thrustergroup({th_att_lin0,th_att_lin1}, THGROUP.ATT_LEFT)
	vi:create_thrustergroup({th_att_lin2,th_att_lin3}, THGROUP.ATT_RIGHT)

	vi:add_exhaust(th_att_rot0, eh, ew2, _V( 1.8 , -0.3 , 18.0 ), _V( 1, 0, 0), tex_rcs) --F4R
	vi:add_exhaust(th_att_rot0, eh, ew2, _V( 1.75,  0.1 , 18.05), _V( 1, 0, 0), tex_rcs) --F2R
	vi:add_exhaust(th_att_rot2, eh, ew2, _V(-1.7 , -0.3 , 18.0 ), _V(-1, 0, 0), tex_rcs) --F1L
	vi:add_exhaust(th_att_rot2, eh, ew2, _V(-1.65, -0.1 , 18.05), _V(-1, 0, 0), tex_rcs) --F3L

	vi:add_exhaust(th_att_rot1, eh, ew2, _V(-4.0, 2.35, -12.35), _V(-1, 0, 0), tex_rcs) --L4L
	vi:add_exhaust(th_att_rot1, eh, ew2, _V(-4.0, 2.35, -12.6 ), _V(-1, 0, 0), tex_rcs) --L2L
	vi:add_exhaust(th_att_rot1, eh, ew2, _V(-4.0, 2.35, -13.0 ), _V(-1, 0, 0), tex_rcs) --L3L
	vi:add_exhaust(th_att_rot1, eh, ew2, _V(-4.0, 2.35, -13.35), _V(-1, 0, 0), tex_rcs) --L1L

	vi:add_exhaust(th_att_rot3, eh, ew2, _V( 4.0, 2.35, -12.35), _V(1, 0, 0), tex_rcs) --R4R
	vi:add_exhaust(th_att_rot3, eh, ew2, _V( 4.0, 2.35, -12.6 ), _V(1, 0, 0), tex_rcs) --R2R
	vi:add_exhaust(th_att_rot3, eh, ew2, _V( 4.0, 2.35, -13.0 ), _V(1, 0, 0), tex_rcs) --R3R
	vi:add_exhaust(th_att_rot3, eh, ew2, _V( 4.0, 2.35, -13.35), _V(1, 0, 0), tex_rcs) --R1R

	th_att_rot0 = vi:create_thruster({pos=_V( 2.7, 0, 0), dir=_V(0,  1, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	th_att_rot1 = vi:create_thruster({pos=_V(-2.7, 0, 0), dir=_V(0, -1, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	th_att_rot2 = vi:create_thruster({pos=_V(-2.7, 0, 0), dir=_V(0,  1, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	th_att_rot3 = vi:create_thruster({pos=_V( 2.7, 0, 0), dir=_V(0, -1, 0), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	vi:create_thrustergroup({th_att_rot0,th_att_rot1}, THGROUP.ATT_BANKLEFT)
	vi:create_thrustergroup({th_att_rot2,th_att_rot3}, THGROUP.ATT_BANKRIGHT)

	vi:add_exhaust(th_att_rot0, eh, ew1, _V( 1.60,-0.20, 18.78), _V( 0.4339, -0.8830, -0.1793), tex_rcs) --F2D
	vi:add_exhaust(th_att_rot0, eh, ew1, _V( 1.68,-0.18, 18.40), _V( 0.4339, -0.8830, -0.1793), tex_rcs) --F4D
	vi:add_exhaust(th_att_rot2, eh, ew1, _V(-1.55,-0.20, 18.78), _V(-0.4339, -0.8830, -0.1793), tex_rcs) --F1D
	vi:add_exhaust(th_att_rot2, eh, ew1, _V(-1.63,-0.18, 18.40), _V(-0.4339, -0.8830, -0.1793), tex_rcs) --F3D

	vi:add_exhaust(th_att_rot1, eh, ew1, _V(-3.46, 3.20,-12.30), _V(0, 1, 0), tex_rcs) --L4U
	vi:add_exhaust(th_att_rot1, eh, ew1, _V(-3.46, 3.20,-12.70), _V(0, 1, 0), tex_rcs) --L2U
	vi:add_exhaust(th_att_rot1, eh, ew1, _V(-3.46, 3.20,-13.10), _V(0, 1, 0), tex_rcs) --L1U

	vi:add_exhaust(th_att_rot3, eh, ew1, _V( 3.43, 3.20,-12.30), _V(0, 1, 0), tex_rcs) --R4U
	vi:add_exhaust(th_att_rot3, eh, ew1, _V( 3.43, 3.20,-12.70), _V(0, 1, 0), tex_rcs) --R2U
	vi:add_exhaust(th_att_rot3, eh, ew1, _V( 3.43, 3.20,-13.10), _V(0, 1, 0), tex_rcs) --R1U

	vi:add_exhaust(th_att_rot2, eh, ew1, _V(-3.1 , 1.55,-12.45), _V(-0.2844, -0.9481, -0.1422), tex_rcs) --L4D
	vi:add_exhaust(th_att_rot2, eh, ew1, _V(-3.1 , 1.6 ,-12.8 ), _V(-0.2844, -0.9481, -0.1422), tex_rcs) --L2D
	vi:add_exhaust(th_att_rot2, eh, ew1, _V(-3.1 , 1.65,-13.15), _V(-0.2844, -0.9481, -0.1422), tex_rcs) --L3D

	vi:add_exhaust(th_att_rot0, eh, ew1, _V( 3.15, 1.55,-12.45), _V( 0.2844, -0.9481, -0.1422), tex_rcs) --R4D
	vi:add_exhaust(th_att_rot0, eh, ew1, _V( 3.15, 1.6 ,-12.8 ), _V( 0.2844, -0.9481, -0.1422), tex_rcs) --R2D
	vi:add_exhaust(th_att_rot0, eh, ew1, _V( 3.15, 1.65,-13.15), _V( 0.2844, -0.9481, -0.1422), tex_rcs) --R3D

	th_att_lin0 = vi:create_thruster({pos=_V(0, 0, -16), dir=_V(0, 0,  1), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	th_att_lin1 = vi:create_thruster({pos=_V(0, 0,  16), dir=_V(0, 0, -1), maxth0=ORBITER_RCS_THRUST, isp0=ORBITER_RCS_ISP0, ispr=ORBITER_RCS_ISP1})
	vi:create_thrustergroup({th_att_lin0}, THGROUP.ATT_FORWARD)
	vi:create_thrustergroup({th_att_lin1}, THGROUP.ATT_BACK)

	vi:add_exhaust(th_att_lin0, eh, ew1, _V(-3.59, 2.8, -13.6), _V(0, 0, -1), tex_rcs) --L1A
	vi:add_exhaust(th_att_lin0, eh, ew1, _V(-3.27, 2.8, -13.6), _V(0, 0, -1), tex_rcs) --L3A
	vi:add_exhaust(th_att_lin0, eh, ew1, _V( 3.64, 2.8, -13.6), _V(0, 0, -1), tex_rcs) --R1A
	vi:add_exhaust(th_att_lin0, eh, ew1, _V( 3.27, 2.8, -13.6), _V(0, 0, -1), tex_rcs) --R3A

	vi:add_exhaust(th_att_lin1, eh, ew1, _V( 0.0, 0.75, 19.2), _V(0, 0.0499, 0.9988), tex_rcs) --F3F
	vi:add_exhaust(th_att_lin1, eh, ew1, _V(-0.4, 0.7 , 19.2), _V(0, 0.0499, 0.9988), tex_rcs) --F1F
	vi:add_exhaust(th_att_lin1, eh, ew1, _V( 0.4, 0.7 , 19.2), _V(0, 0.0499, 0.9988), tex_rcs) --F2Fend
end


-----------------------------------------------------------------
-- Airfoil coefficient function
-- Return lift, moment and zero-lift drag coefficients as a
-- function of angle of attack
-- 1. vertical lift component (wings and body)
-----------------------------------------------------------------
	-- lift and moment coefficients from -180 to 180 in 15 degree steps.
	-- This uses a documented lift slope of 0.0437/deg, everything else is rather ad-hoc
local VCL = {0.1, 0.17, 0.2, 0.2, 0.17, 0.1, 0, -0.11, -0.24, -0.38,  -0.5,  -0.5, -0.02, 0.6355,    0.63,   0.46, 0.28, 0.13, 0.0, -0.16, -0.26, -0.29, -0.24, -0.1, 0.1}
local VCM = {  0,    0,   0,   0,    0,   0, 0,     0,    0,0.002,0.004, 0.0025,0.0012,      0,-0.0012,-0.0007,    0,    0,   0,     0,     0,     0,     0,    0,   0}
local Vstep = 15 * RAD
local Vistep = 1 / Vstep
local function VLiftCoeff(hVessel,aoa,M,Re)
	aoa = aoa + PI
	local idx = 1 + math.floor(math.max(0, math.min(23, aoa*Vistep)))
	local d = aoa*Vistep - idx + 1
	local cl = VCL[idx] + (VCL[idx+1]-VCL[idx])*d
	local cm = VCM[idx] + (VCM[idx+1]-VCM[idx])*d
	local cd = 0.055 + oapi.get_induceddrag(cl, 2.266, 0.6)

	return cl,cm,cd
end

-----------------------------------------------------------------
-- Airfoil coefficient functions
-- Return lift, moment and zero-lift drag coefficients as a
-- function of slip angle (beta)
-- 2. horizontal lift component (vertical stabiliser and body)
-----------------------------------------------------------------
local HCL = {0, 0.2, 0.3, 0.2, 0, -0.2, -0.3, -0.2, 0, 0.2, 0.3, 0.2, 0, -0.2, -0.3, -0.2, 0}
local Hstep = 22.5 * RAD
local Histep = 1 / Hstep
local function HLiftCoeff(hVessel,beta,M,Re)
	beta = beta + PI
	local idx = 1 + math.floor(math.max(0, math.min(15, beta*Histep)))
	local d = beta*Histep - idx + 1
	local cl = HCL[idx] + (HCL[idx+1]-HCL[idx])*d
	local cd = 0.02 + oapi.get_induceddrag(cl, 1.5, 0.6)
    return cl,0,cd
end


--------------------------------------------------------------
-- Initialise airfoils, aerodynamic control surfaces and drag elements
--------------------------------------------------------------
function create_airfoils()
	vi:create_airfoil(LIFT.VERTICAL,   _V(0, 0, -0.5), VLiftCoeff, 20, 270, 2.266)
	vi:create_airfoil(LIFT.HORIZONTAL, _V(0, 0, -4.0), HLiftCoeff, 20,  50, 1.5)

	vi:create_controlsurface(AIRCTRL.ELEVATOR, 5.0, 1.5, _V( 0,  0,   -15), AIRCTRL_AXIS.XPOS, 1.0, anim_elev)
	vi:create_controlsurface(AIRCTRL.RUDDER,   2.0, 1.5, _V( 0,  3,   -16), AIRCTRL_AXIS.YPOS, 1.0, anim_rudder)
	vi:create_controlsurface(AIRCTRL.AILERON,  3.0, 1.5, _V( 7, -0.5, -15), AIRCTRL_AXIS.XPOS, 1.0, anim_raileron)
	vi:create_controlsurface(AIRCTRL.AILERON,  3.0, 1.5, _V(-7, -0.5, -15), AIRCTRL_AXIS.XNEG, 1.0, anim_laileron)
	spdb_drag = vi:create_variabledragelement(5, _V(0, 7.5, -14)) -- speedbrake drag
	gear_drag = vi:create_variabledragelement(2, _V(0,-3,0))      -- landing gear drag
	rdoor_drag = vi:create_variabledragelement(7, _V(2.9,0,10))   -- right cargo door drag
	ldoor_drag = vi:create_variabledragelement(7, _V(-2.9,0,10))  -- right cargo door drag
end

----------------------------------------------------------------
-- Enable/disable Space Shuttle Main Engines
----------------------------------------------------------------
function enable_SSME(enable)
	local hProp

	if enable then
		if pET then
			hProp = pET:get_prop_handle()
		else
			return false -- can't activate without attached ET
		end
	end

	for i=0,2 do
		vi:set_thrusterresource(th_main[i], hProp)
	end

	if enable then
		if vi:get_groupthrustercount(THGROUP.MAIN) ~= 3 then
			-- switch MAIN group to SSME
			vi:del_thrustergroup(THGROUP.MAIN)
			thg_main = vi:create_thrustergroup(th_main, THGROUP.MAIN)
		end
		vi:set_default_propellantresource(hProp)
	end
	return true
end


----------------------------------------------------------------
-- Enable/disable Orbital Maneuvering System
----------------------------------------------------------------
function enable_OMS(enable)
	local hProp = enable and ph_oms or nil

	for i=0,1 do
		vi:set_thrusterresource(th_oms[i], hProp)
	end

	if enable then
		if vi:get_groupthrustercount(THGROUP.MAIN) > 2 then
			-- switch MAIN group to OMS
			vi:del_thrustergroup(THGROUP.MAIN)
			thg_main = vi:create_thrustergroup(th_oms, THGROUP.MAIN)
		end
		vi:set_default_propellantresource(ph_oms)
	end
end

----------------------------------------------------------------
-- Enable/disable Reaction Control System
----------------------------------------------------------------
function enable_RCS(mode)
	local hProp = nil
	if mode ~= RCSMODE.OFF then
		hProp = ph_oms
	end

	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_PITCHUP)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_PITCHUP, i), hProp)
	end
	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_PITCHDOWN)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_PITCHDOWN, i), hProp)
	end

	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_YAWLEFT)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_YAWLEFT, i), hProp)
	end
	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_YAWRIGHT)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_YAWRIGHT, i), hProp)
	end

	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_BANKLEFT)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_BANKLEFT, i), hProp)
	end
	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_BANKRIGHT)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_BANKRIGHT, i), hProp)
	end

	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_UP)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_UP, i), hProp)
	end
	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_DOWN)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_DOWN, i), hProp)
	end

	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_LEFT)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_LEFT, i), hProp)
	end
	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_RIGHT)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_RIGHT, i), hProp)
	end

	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_FORWARD)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_FORWARD, i), hProp)
	end
	for i = 0,vi:get_groupthrustercount(THGROUP.ATT_BACK)-1 do
		vi:set_thrusterresource(vi:get_groupthruster(THGROUP.ATT_BACK, i), hProp)
	end

	vi:set_rcsmode(mode)
end


----------------------------------------------------------------
-- Define animation sequences for moving parts
----------------------------------------------------------------
function define_animations()
	local midx = 1 -- mesh index for all external animations
	local vidx = 2 -- mesh index for all VC animations
	-- ***** 1. Cargo door and radiator animations *****
	local RCargoDoorGrp = {GRP.cargodooroutR, GRP.cargodoorinR, GRP.radiatorFR, GRP.radiatorBR}
	local RCargoDoor = MGROUP_ROTATE(midx, RCargoDoorGrp, _V(2.88, 1.3, 0), _V(0, 0, 1), -175.5*RAD)

	local LCargoDoorGrp = {GRP.cargodooroutL, GRP.cargodoorinL, GRP.radiatorFL, GRP.radiatorBL}
	local LCargoDoor = MGROUP_ROTATE(midx, LCargoDoorGrp, _V(-2.88, 1.3, 0), _V(0, 0, 1), 175.5*RAD)

	anim_door = vi:create_animation(0)
	vi:add_animationcomponent(anim_door, 0.0, 0.4632, RCargoDoor)
	vi:add_animationcomponent(anim_door, 0.5368, 1.0, LCargoDoor)

	local RRadiator = MGROUP_ROTATE(midx, GRP.radiatorFR, _V( 2.88, 1.3, 0), _V(0, 0, 1),  35.5*RAD)
	local LRadiator = MGROUP_ROTATE(midx, GRP.radiatorFL, _V(-2.88, 1.3, 0), _V(0, 0, 1), -35.5*RAD)

	anim_rad = vi:create_animation(0)
	vi:add_animationcomponent(anim_rad, 0, 1, RRadiator)
	vi:add_animationcomponent(anim_rad, 0, 1, LRadiator)

	-- ***** 2. Landing gear animation *****

	local LNosewheelDoor = MGROUP_ROTATE(midx, GRP.nosedoorL, _V(-0.78, -2.15, 17), _V(0, 0.195, 0.981), -60*RAD)
	local RNosewheelDoor = MGROUP_ROTATE(midx, GRP.nosedoorR, _V( 0.78, -2.15, 17), _V(0, 0.195, 0.981),  60*RAD)

	local NosewheelGrp = {GRP.nosewheel, GRP.nosegear}
	local Nosewheel = MGROUP_ROTATE(midx, NosewheelGrp, _V(0.0, -1.95, 17.45), _V(1, 0, 0), 109*RAD)

	local RGearDoor = MGROUP_ROTATE(midx, GRP.geardoorR, _V( 4.35, -2.64, -1.69), _V(0, 0.02, 0.9),  96.2*RAD)
	local LGearDoor = MGROUP_ROTATE(midx, GRP.geardoorL, _V(-4.35, -2.64, -1.69), _V(0, 0.02, 0.9), -96.2*RAD)

	local MainGearGrp = {GRP.wheelR, GRP.gearR, GRP.wheelL, GRP.gearL}
	local MainGear = MGROUP_ROTATE(midx, MainGearGrp, _V(0, -2.66, -3.68), _V(1, 0, 0), 94.5*RAD)

	anim_gear = vi:create_animation(0)
	vi:add_animationcomponent(anim_gear, 0,   0.5, LNosewheelDoor)
	vi:add_animationcomponent(anim_gear, 0,   0.5, RNosewheelDoor)
	vi:add_animationcomponent(anim_gear, 0.4, 1.0, Nosewheel)
	vi:add_animationcomponent(anim_gear, 0,   0.5, RGearDoor)
	vi:add_animationcomponent(anim_gear, 0,   0.5, LGearDoor)
	vi:add_animationcomponent(anim_gear, 0.4, 1.0, MainGear)

	-- ***** 3. Ku-band antenna animation *****

	local KuBand1Grp = {GRP.startrackers, GRP.KUband1, GRP.KUband2}
	local KuBand1 = MGROUP_ROTATE(midx, KuBand1Grp, _V(2.85, 0.85, 0), _V(0, 0, 1), -18*RAD)

	local KuBand2 = MGROUP_ROTATE(midx, GRP.KUband2,  _V(2.78, 1.7, 0), _V(0, 0, 1), -90*RAD)

	local KuBand3Grp = {GRP.KUband1, GRP.KUband2}
	local KuBand3 = MGROUP_ROTATE(midx, KuBand3Grp, _V(2.75, 2.05, 11.47), _V(0, 1, 0), -113*RAD)

	anim_kubd = vi:create_animation(0)
	vi:add_animationcomponent(anim_kubd, 0,     0.333, KuBand1)
	vi:add_animationcomponent(anim_kubd, 0.333, 0.667, KuBand2)
	vi:add_animationcomponent(anim_kubd, 0.667, 0.999, KuBand3)

	-- ***** 4. Elevator animation of elevons *****

	local ElevGrp = {GRP.flapR, GRP.flapL, GRP.aileronL, GRP.aileronR}
	local Elevator = MGROUP_ROTATE(midx, ElevGrp, _V(0, -2.173, -8.84), _V(1, 0, 0), 30*RAD)

	anim_elev = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_elev, 0, 1, Elevator)

	-- ***** 5. Aileron animation of elevons *****

	local LAileronGrp = {GRP.flapL, GRP.aileronL}
	local LAileron = MGROUP_ROTATE(midx, LAileronGrp, _V(0, -2.173, -8.84), _V(-1, 0, 0), 10*RAD)

	local RAileronGrp = {GRP.flapR, GRP.aileronR}
	local RAileron = MGROUP_ROTATE(midx, RAileronGrp, _V(0, -2.173, -8.84), _V(1, 0, 0), 10*RAD)

	anim_laileron = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_laileron, 0, 1, LAileron)
	anim_raileron = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_raileron, 0, 1, RAileron)

	-- ***** 6. Rudder animation *****

	local RudderGrp = {GRP.rudderR, GRP.rudderL}
	local Rudder = MGROUP_ROTATE(midx, RudderGrp, _V(0, 5.77, -12.17), _V(-0.037, 0.833, -0.552), -54.2*RAD)

	anim_rudder = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_rudder, 0, 1, Rudder)

	-- ***** 7. Speedbrake animation *****

	local SB1 = MGROUP_ROTATE(midx, GRP.rudderR, _V( 0.32, 5.77, -12.17), _V(-0.037, 0.833, -0.552), -49.3*RAD)
	local SB2 = MGROUP_ROTATE(midx, GRP.rudderL, _V(-0.32, 5.77, -12.17), _V( 0.037, 0.833, -0.552),  49.3*RAD)

	anim_spdb = vi:create_animation(0)
	vi:add_animationcomponent(anim_spdb, 0, 1, SB1)
	vi:add_animationcomponent(anim_spdb, 0, 1, SB2)

	-- ***** 8. RMS arm animation *****
	-- Note that the animation components can't be declared static here, since
	-- their rotation parameters are modified by the respective parent transforms

	local parent

	rms_anim[0] = MGROUP_ROTATE(midx, GRP.Shoulder, _V(-2.26, 1.70, 9.65), _V(0, 1, 0), -360*RAD) -- -180 .. +180

	anim_arm_sy = vi:create_animation(0.5)
	parent = vi:add_animationcomponent(anim_arm_sy, 0, 1, rms_anim[0])

	rms_anim[1] = MGROUP_ROTATE(midx, GRP.Humerus, _V(-2.26, 1.70, 9.65), _V(1, 0, 0), 147*RAD) -- -2 .. +145

	anim_arm_sp = vi:create_animation(0.0136)
	parent = vi:add_animationcomponent(anim_arm_sp, 0, 1, rms_anim[1], parent)

	local RMSElbowPitchGrp = {GRP.radii, GRP.RMScamera, GRP.RMScamera_pivot}
	rms_anim[2] = MGROUP_ROTATE(midx, RMSElbowPitchGrp, _V(-2.26, 1.55, 3.10), _V(1, 0, 0), -162*RAD) -- -160 .. +2

	anim_arm_ep = vi:create_animation(0.0123)
	parent = vi:add_animationcomponent(anim_arm_ep, 0, 1, rms_anim[2], parent)

	rms_anim[3] = MGROUP_ROTATE(midx, GRP.wrist, _V(-2.26, 1.7, -3.55), _V(1, 0, 0), 240*RAD) -- -120 .. +120

	anim_arm_wp = vi:create_animation(0.5)
	parent = vi:add_animationcomponent(anim_arm_wp, 0, 1, rms_anim[3], parent)

	rms_anim[4] = MGROUP_ROTATE(midx, GRP.endeffecter, _V(-2.26, 1.7, -4.9), _V(0, 1, 0), -240*RAD) -- -120 .. +120

	anim_arm_wy = vi:create_animation(0.5)
	parent = vi:add_animationcomponent(anim_arm_wy, 0, 1, rms_anim[4], parent)

	local armtip = {arm_tip[0], arm_tip[1], arm_tip[2]}
	rms_anim[5] = MGROUP_ROTATE(LOCALVERTEXLIST, armtip, _V(-2.26, 1.7, -6.5), _V(0, 1, 0), -894*RAD) -- -447 .. +447

	anim_arm_wr = vi:create_animation(0.5)
	hAC_arm = vi:add_animationcomponent(anim_arm_wr, 0, 1, rms_anim[5], parent)

	-- ***** 9. SSME pitch gimbal animations
	local init_gimbal = -10*RAD
	local max_gimbal = -0.2*PI
	anim_ssme = vi:create_animation(init_gimbal/max_gimbal)

	ssme_anim[0] = MGROUP_ROTATE(midx, GRP.SSMEL, _V(-1.55, -0.37, -12.5), _V(1, 0, 0), max_gimbal)
	vi:add_animationcomponent(anim_ssme, 0, 1, ssme_anim[0])

	ssme_anim[1] = MGROUP_ROTATE(midx, GRP.SSMER, _V(1.55, -0.37, -12.5), _V(1, 0, 0), max_gimbal)
	vi:add_animationcomponent(anim_ssme, 0, 1, ssme_anim[1])

	ssme_anim[2] = MGROUP_ROTATE(midx, GRP.SSMET, _V(0, 2.7, -12.5), _V(-1, 0, 0), max_gimbal)
	vi:add_animationcomponent(anim_ssme, 0, 1, ssme_anim[2])

	-- ======================================================
	-- VC animation definitions
	-- ======================================================
	plop.define_animations(vidx)
end

----------------------------------------------------------------
-- Jettison both SRBs from ET
----------------------------------------------------------------
function separate_boosters(met)
	local hET = vi:get_dockstatus(vi:get_dockhandle(1))
	if hET then
		pET = vessel.get_interface(hET)
		pET:separate_SRBs()
	end

	-- reconfigure
	status = 2
	vi:record_event("JET", "SRB")
end

----------------------------------------------------------------
-- Jettison ET from orbiter
----------------------------------------------------------------
function separate_tank()
	if hDockET then
		vi:undock(1)
		vi:del_dock(hDockET)
		hDockET = nil
	end
	pET = nil
	enable_SSME(false)
	enable_RCS(RCSMODE.ROT)
	enable_OMS(true)

	-- reconfigure
	status = 3
	vi:record_event("JET", "ET")
end

function attach_child_with_mass(hChild, attachment, child_attachment)
	vi:attach_child(hChild, attachment, child_attachment)
	vi:set_emptymass(vi:get_emptymass() + oapi.get_emptymass(hChild))
end

function detach_child_with_mass(attachment,  vel)
	local hChild = vi:get_attachmentstatus(attachment)
	if hChild then
		vi:detach_child(attachment, vel)
		vi:set_emptymass(vi:get_emptymass() - oapi.get_emptymass(hChild))
	end
end

-- helper to iterate over other vessels
local function iterate_other_vessels()
	local idx = 0
	local me = vi:get_handle()
	local nVessel = vessel.get_count()
    return function()
			  repeat
				  local hV = vessel.get_handle(idx)
				  idx = idx + 1
				  if hV ~= me then -- skip if self
					  return hV
				  end
			  until idx >= nVessel
           end
end

-- iterate grapple points
local function iterate_grapple_points(v)
	local nAttach = v:get_attachmentcount(true)
	local idx = 0
	return function()
				repeat
				   local hAtt = v:get_attachmenthandle(true, idx)
				   idx = idx + 1
				   local id = v:get_attachmentid(hAtt)
				   if id:find("^GS") then -- attachment point compatible
					   return hAtt
				   end
				until idx >= nAttach
           end
end

function toggle_grapple()
	local hV = vi:get_attachmentstatus(rms_attach)

	if hV then  -- release satellite
		local hAtt = can_arrest()
		detach_child_with_mass(rms_attach)
		-- check whether the object being ungrappled is ready to be clamped into the payload bay
		if hAtt then
			attach_child_with_mass(hV, sat_attach, hAtt)
		end

	else        -- grapple satellite
		local grms = vi:local2global(arm_tip[0])  -- global position of RMS tip
		
		-- Search the complete vessel list for a grappling candidate.
		-- Not very scalable ...
		for hV in iterate_other_vessels() do
			local gpos = oapi.get_globalpos(hV)
			if vec.dist(gpos, grms) < oapi.get_size(hV) then -- in range
				local v = vessel.get_interface(hV)
				for hAtt in iterate_grapple_points(v) do
					local pos, dir, rot = v:get_attachmentparams(hAtt)
					gpos = v:local2global(pos)
					if vec.dist(gpos, grms) < MAX_GRAPPLING_DIST then -- found one!
						-- check whether satellite is currently clamped into payload bay
						if hV == vi:get_attachmentstatus(sat_attach) then
							detach_child_with_mass(sat_attach)
						end
						attach_child_with_mass(hV, rms_attach, hAtt)
						return
					end
				end
			end
		end
	end
end


function get_SSME_gimbal_pos(which)
	local dir
	if status < 3 then
		dir = vi:get_thrusterdir(th_main[which])
	else
		dir = _V(0, 0, 1)
	end
	pitch = math.asin(dir.y)+10.5*RAD
	yaw = math.atan(dir.x/dir.z)

	return pitch, yaw
end

function get_SRB_gimbal_pos(which)
	local dir
	if status < 2 and pET then
		dir = pET:get_SRB_thrust_dir(which)
	else
		dir = _V(0, 0, 1)
	end

	pitch = math.asin(dir.y)
	if which ~= 0 then
		pitch = -pitch
		yaw = -yaw
	end
	yaw = math.atan(dir.x/dir.z)

	return pitch, yaw
end

function toggle_arrest()
	if sat_stowed() then -- purge satellite
		detach_child_with_mass(sat_attach, 0.1)
	elseif can_arrest() then           -- try to arrest satellite
		toggle_grapple()
	end
end


-- check whether the currently grappled object can be stowed in the cargo bay
function can_arrest()
	local hV = vi:get_attachmentstatus(rms_attach)
	if hV == nil then
		return nil
	end

	local v = vessel.get_interface(hV)
	local nAttach = v:get_attachmentcount(true)
	local pos, dir, rot = vi:get_attachmentparams(sat_attach)
	local gbay = vi:local2global(pos)
	for j=0, nAttach do
		local hAtt = v:get_attachmenthandle(true, j)
		if v:get_attachmentId(hAtt):find('^XS') then -- attachment point compatible
			v:get_attachmentparams(hAtt, pos, dir, rot)
			local gpos = v:local2global(pos)
			if vec.dist(gpos, gbay) < MAX_GRAPPLING_DIST then
				return hAtt
			end
		end
	end
	return nil
end

function separate_MMU()
	-- Create MMU at docking port
	local hDock = vi:get_dockhandle(0)
	if vi:get_dockstatus(hDock) then  -- something is already attached to this docking port
		return
	end

	local name = vi:get_name()
	local idx = 0
	local hVessel
	repeat
		idx = idx + 1
		hVessel = vessel.get_handle(name.."-MMU-"..idx)
	until hVessel == nil

	local vs = vi:get_rawstatus(1)
	local hMMU = oapi.create_vessel(name, "Nasa_MMU", vs)
	vi:dock(hMMU, 0, 0, 1)
	oapi.set_focusobject(hMMU)
end


function get_SRB_thrust_level(which)
	return pET and pET:get_SRB_thrust_level(which) or 0
end

function set_SSME_gimbal(angle)
	local pitch_gimbal_max = -0.2*PI
	local dir = {}

	dir.x = -math.sin(angle.y)        -- yaw gimbal
	dir.y = math.sin(angle.x-angle.z) -- pitch+roll gimbal
	dir.z = math.sqrt(1.0-dir.x*dir.x-dir.y*dir.y)
	vi:set_thrusterdir(th_main[0], dir) -- left SSME

	dir.y = math.sin(angle.x+angle.z) -- pitch+roll gimbal
	dir.z = math.sqrt(1.0-dir.x*dir.x-dir.y*dir.y)
	vi:set_thrusterdir(th_main[1], dir) -- right SSME

	dir.y = math.sin(angle.x) -- pitch gimbal
	dir.z = math.sqrt(1.0-dir.x*dir.x-dir.y*dir.y)
	vi:set_thrusterdir(th_main[2], dir) -- top SSME

	set_SSME_position(gimbal_pos.x/pitch_gimbal_max)
end


----------------------------------------------------------------
-- Autopilot function: set target pitch angle [rad]
-- during launch phase (until ET separation)
----------------------------------------------------------------
function get_ascent_pitch_rate(tgt_pitch)
	local a = 0.07
	local b = 0.035
	local avel = vi:get_angvel()
	local dpitch = avel.x    -- pitch rate
	local pitch = vi:get_pitch() -- current pitch value

	return a*(pitch-tgt_pitch) + b*dpitch
end

----------------------------------------------------------------
-- Automatic gimbal adjustment for SSME and SRB engines to correct
-- for CG shift, SRB thrust variations, atmospheric effects, etc.
-- 
-- NOTE: We use SSME gimbal for adjusting pitch rate, SRB gimbal for
-- adjusting yaw and roll rate
-- 
-- The gimbal changes are implemented individually for each axis as
-- damped harmonic oscillators around target rates
----------------------------------------------------------------
function auto_gimbal(tgt_rate)
	-- Harmonic oscillator design parameters
	local a_pitch = 2e0
	local b_pitch = 1e0
	local a_yaw = 1e-1
	local b_yaw = 3e-2
	local a_roll_srb = 1e-1
	local b_roll_srb = 3e-2
	local a_roll_ssme = 8e-2
	local b_roll_ssme = 5e-2

	local avel = vi:get_angvel()
	local aacc = vi:get_angularacc()
	local dt = oapi.get_simstep()
	local pitch_gimbal_max = -21.0*RAD
	local yaw_gimbal_max = 4*RAD
	local roll_gimbal_max_srb = 8.0*RAD
	local roll_gimbal_max_ssme = 6*RAD

	local roll_gimbal_max, a_roll, b_roll
	if status < 2 and pET then
		roll_gimbal_max = roll_gimbal_max_srb
		a_roll = a_roll_srb
		b_roll = b_roll_srb
	else
		roll_gimbal_max = roll_gimbal_max_ssme
		a_roll = a_roll_ssme
		b_roll = b_roll_ssme
	end

	-- Pitch gimbal settings
	maxdg = dt*0.3 -- max gimbal speed [rad/s]
	dgimbal = a_pitch*(avel.x-tgt_rate.x) + b_pitch*aacc.x
	dgimbal = math.max(-maxdg, math.min(maxdg, dgimbal))
	gimbal_pos.x = math.min(0.0, math.max(pitch_gimbal_max, gimbal_pos.x+dgimbal))

	-- Yaw gimbal settings
	dgimbal = a_yaw*(avel.y-tgt_rate.y) + b_yaw*aacc.y
	gimbal_pos.y = math.min(yaw_gimbal_max, math.max(-yaw_gimbal_max, gimbal_pos.y+dgimbal))

	-- Roll gimbal settings
	dgimbal = a_roll*(avel.z-tgt_rate.z) + b_roll*aacc.z
	gimbal_pos.z = math.min(roll_gimbal_max, math.max(-roll_gimbal_max, gimbal_pos.z+dgimbal))

	-- Set SRB gimbals
	if status < 2 and pET then
		pET:set_SRB_gimbal(gimbal_pos)
		set_SSME_gimbal(_V(gimbal_pos.x,0,0)) -- If SRBs are available, we gimbal the SSMEs only in pitch
	else
		set_SSME_gimbal(gimbal_pos)
	end
end

----------------------------------------------------------------
-- RCS automatic control for commanding a target attitude rate
-- Used by the ascent autopilot when gimbal control is no longer available
-- (SSME cut off)
----------------------------------------------------------------
function auto_RCS(tgt_rate)
	-- Harmonic oscillator design parameters
	local a_pitch = 4
	local b_pitch = 2
	local a_yaw = 2e-1
	local b_yaw = 6e-2
	local a_roll = 2e-1
	local b_roll = 6e-2

	local avel = vi:get_angvel()
	local aacc = vi:get_angularacc()
	local dt = oapi.get_simstep()
	local drcs

	-- Pitch RCS settings
	drcs = a_pitch*(tgt_rate.x-avel.x) - b_pitch*aacc.x
	if drcs > 0.0 then
		vi:set_thrustergrouplevel(THGROUP.ATT_PITCHUP, math.min(drcs, 1.0))
		vi:set_thrustergrouplevel(THGROUP.ATT_PITCHDOWN, 0)
	else
		vi:set_thrustergrouplevel(THGROUP.ATT_PITCHUP, 0)
		vi:set_thrustergrouplevel(THGROUP.ATT_PITCHDOWN, math.min(-drcs, 1.0))
	end

	-- Yaw RCS settings
	drcs = a_yaw*(tgt_rate.y-avel.y) - b_yaw*aacc.y
	if drcs > 0.0 then
		vi:set_thrustergrouplevel(THGROUP.ATT_YAWLEFT, math.min(drcs, 1.0))
		vi:set_thrustergrouplevel(THGROUP.ATT_YAWRIGHT, 0)
	else
		vi:set_thrustergrouplevel(THGROUP.ATT_YAWLEFT, 0)
		vi:set_thrustergrouplevel(THGROUP.ATT_YAWRIGHT, math.min(-drcs, 1.0))
	end

	-- Roll RCS settings
	drcs = a_roll*(tgt_rate.z-avel.z) - b_roll*aacc.z
	if drcs > 0.0 then
		vi:set_thrustergrouplevel(THGROUP.ATT_BANKRIGHT, math.min(drcs, 1.0))
		vi:set_thrustergrouplevel(THGROUP.ATT_BANKLEFT, 0)
	else
		vi:set_thrustergrouplevel(THGROUP.ATT_BANKRIGHT, 0)
		vi:set_thrustergrouplevel(THGROUP.ATT_BANKLEFT, math.min(-drcs, 1.0))
	end
end


local tdvtx = {
	{ pos=_V( 0,    -3.3, 18.75 ), stiffness=1e8, damping=1e6, mu=1.6, mu_lng=0.1 },
	{ pos=_V(-3.96, -5.5,  -3.2 ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0.2 },
	{ pos=_V( 3.96, -5.5,  -3.2 ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0.2 },
	{ pos=_V(-11.9, -2.1,  -10  ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V( 11.9, -2.1,  -10  ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V(-11.3, -2.1,  -6   ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V( 11.3, -2.1,  -6   ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V(-2.95, -2.0, -14.35), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V( 2.95, -2.0, -14.35), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V(-1.9,  -1.0, -14.8 ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V( 1.9,  -1.0, -14.8 ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V( 0,    11.2, -16.4 ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V( 0,    11.3, -14.0 ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 },
	{ pos=_V( 0,    -0.9,  20.6 ), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0 }
}

local geardn_vtx = {
	{ pos=_V( 0,    -3.95, 17.5), stiffness=1e8, damping=1e6, mu=1.6, mu_lng=0.1 },
	{ pos=_V(-3.96, -5.5,  -3.2), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0.2 },
	{ pos=_V( 3.96, -5.5,  -3.2), stiffness=1e8, damping=1e6, mu=3,   mu_lng=0.2 },
}

local gearup_vtx = {
	{ pos=_V( 0,    -2.2, 16.75), stiffness=1e8, damping=1e6, mu=3, mu_lng=0 },
	{ pos=_V(-3.96, -2.7,  -3.2), stiffness=1e8, damping=1e6, mu=3, mu_lng=0 },
	{ pos=_V( 3.96, -2.7,  -3.2), stiffness=1e8, damping=1e6, mu=3, mu_lng=0 },
}

function set_gear_parameters(state)
	if state == 1.0 then -- gear fully deployed
		tdvtx[1] = geardn_vtx[1]
		tdvtx[2] = geardn_vtx[2]
		tdvtx[3] = geardn_vtx[3]
		vi:set_touchdownpoints(tdvtx)
		vi:set_surfacefrictioncoeff(0.05, 0.4)
	else
		tdvtx[1] = gearup_vtx[1]
		tdvtx[2] = gearup_vtx[2]
		tdvtx[3] = gearup_vtx[3]
		vi:set_touchdownpoints(tdvtx)
		vi:set_surfacefrictioncoeff(0.4, 0.4)
	end
end

function jettison()
	if status == 1 then
		separate_boosters(oapi.get_simtime()-t0) 
	elseif status == 2 then
		separate_tank()
	end
end


-- Update moving parts of the orbiter's visual: payload bay doors and gear
-- This should only be called when the visual exists, e.g. from within
-- clbkVisualCreated or clbkAnimate

function update_mesh()
	-- update animation states
	vi:set_animation(anim_gear, gear_state.proc)
	vi:set_animation(anim_spdb, spdb_state.proc)
	vi:set_animation(anim_door, plop.get_baydoor_pos())
	vi:set_animation(anim_rad,  plop.get_radiator_pos())
	vi:set_animation(anim_kubd, plop.get_kuantenna_pos())

	set_animation_arm(anim_arm_sy, arm_sy)
	set_animation_arm(anim_arm_sp, arm_sp)
	set_animation_arm(anim_arm_ep, arm_ep)
	set_animation_arm(anim_arm_wp, arm_wp)
	set_animation_arm(anim_arm_wy, arm_wy)
	set_animation_arm(anim_arm_wr, arm_wr)

	-- update MFD brightness
	if vis then
		local mat = {}
		local hMesh = vi:get_devmesh(vis, mesh_vc)
		for i=0,9 do
			mat.r = mfdbright[i]
			mat.g = mfdbright[i]
			mat.b = mfdbright[i]
			mat.a = 1.0
			oapi.set_materialex(hMesh, 10+i, MATPROP.LIGHT, mat)
		end
	end
end

function set_bay_door_position(pos)
	vi:set_animation(anim_door, pos)
	rdoor_drag:set(math.sqrt(math.min(1.0, pos*3.0)))
	ldoor_drag:set(math.sqrt(math.min(1.0, math.max(0.0, pos-0.3656)*3.0)))
end

function set_radiator_position(pos)
	vi:set_animation(anim_rad, pos)
end

function set_Ku_antenna_position(pos)
	vi:set_animation(anim_kubd, pos)
end

function set_SSME_position(pos)
	vi:set_animation(anim_ssme, pos)
end

function operate_landing_gear(action)
	if status < 3 then
		return
	end
	-- operate landing gear only once the orbiter is free from the tank
	if action == animstate.OPENING and vi:get_groundcontact() then
		vnml = vi:horizoninvrot(_V(0,1,0))
		if vnml.y > 0.0 then
			return
		end
	end
	-- don't extend landing gear if standing on the ground

	gear_state.status = action
	vi:record_event("GEAR", action == animstate.CLOSING and "UP" or "DOWN")

end

function revert_landing_gear()
	if status < 3 then
		return
	end
	-- operate landing gear only once the orbiter is free from the tank

	operate_landing_gear((gear_state.status == animstate.CLOSED or gear_state.status == animstate.CLOSING) and animstate.OPENING or animstate.CLOSING)
end

function operate_speedbrake(action)
	spdb_state.status = action
	vi:record_event("SPEEDBRAKE", action == animstate.CLOSING and "CLOSE" or "OPEN")
end

function revert_speedbrake()
	operate_speedbrake((spdb_state.status == animstate.CLOSED or spdb_state.status == animstate.CLOSING) and animstate.OPENING or animstate.CLOSING)
end

function set_animation_arm(anim, state)
	vi:set_animation(anim, state)
	arm_scheduled = true
end

function redraw_panel_MFD_button(surf, mfd)
	local pSkp = oapi.get_sketchpad(surf)

	-- D. Beachy: BUGFIX: if MFD powered off, cover separator lines and do not paint buttons
    if oapi.get_mfdmode(mfd) == MFDMODE.NONE then
		pSkp:set_pen(nil)
		pSkp:set_brush(gbrush)
        pSkp:rectangle(0, 0, 255, 13)
    else   -- MFD powered on
		local pOld = pSkp:set_font(gfont)
		pSkp:set_textcolor(_RGB(0,255,216))
		pSkp:set_textalign(SKP.CENTER, SKP.TOP)
		pSkp:set_backgroundmode(SKP.TRANSPARENT)

		local x = 24

		for bt = 0,4 do
			local label = oapi.mfd_buttonlabel(mfd, bt)
			if label then
				pSkp:text(x, 1, label, label:len())
			end
			x = x + 42
		end
		pSkp:text(234, 1, "PG", 2)
		pSkp:set_font(pOld)
	end
	oapi.release_sketchpad(pSkp)
end

function load_meshes()
	-- Retrieve mesh handles
	hOrbiterMesh        = oapi.load_meshglobal("Atlantis/Atlantis")
	hOrbiterCockpitMesh = oapi.load_meshglobal("Atlantis/AtlantisCockpit")
	hOrbiterVCMesh      = oapi.load_meshglobal("Atlantis/AtlantisVC")
	
	-- Load meshes
	mesh_cockpit = vi:add_mesh(hOrbiterCockpitMesh)
	vi:set_mesh_visibility_mode(mesh_cockpit, MESHVIS.EXTERNAL)

	mesh_orbiter = vi:add_mesh(hOrbiterMesh)
	vi:set_mesh_visibility_mode(mesh_orbiter,  bit.bor(MESHVIS.EXTERNAL, MESHVIS.VC, MESHVIS.EXTPASS))

	mesh_vc = vi:add_mesh(hOrbiterVCMesh)
	vi:set_mesh_visibility_mode(mesh_vc, MESHVIS.VC)

	-- Optional meshes
	mesh_cargo      = nil
	mesh_platform   = nil

	-- Visual handle
	vis             = nil
end

----------------------------------------------------------------
--Overloaded callback functions
----------------------------------------------------------------

----------------------------------------------------------------
--Set vessel class capabilities from config file
----------------------------------------------------------------
function clbk_setclasscaps(cfg)
	-- *********************** physical parameters *********************************
	
	vi:set_size(19.6)
	vi:set_emptymass(ORBITER_EMPTY_MASS)
	vi:set_pmi(_V(78.2, 82.1, 10.7))
	vi:set_gravitygradientdamping(20.0)
	vi:set_crosssections(ORBITER_CS)
	vi:set_rotdrag(_V(0.43, 0.43, 0.29)) -- angular drag
	vi:set_trimscale(0.05)
	launchelev = 0.0
end

----------------------------------------------------------------
-- Read status from scenario file
----------------------------------------------------------------

function clbk_loadstateex(scn, vs2)
	local met = 0.0 -- mission elapsed time
	local srbtime = 0.0
	local sts_sat_x = 0.0
	local sts_sat_y = 0.0
	local sts_sat_z = 0.0
	spdb_state.status = animstate.CLOSED
	spdb_state.proc = 0.0
	gear_state.status = animstate.CLOSED
	gear_state.proc = 0.0
	
	local match = {}
	for line in scenario_lines(scn) do
		if scenario_line_match(line, "CONFIGURATION %d", match) then
			status = match.res[1]
		elseif scenario_line_match(line, "GEAR %d %f", match) then
			gear_state.status = match.res[1] + 1
			gear_state.proc = match.res[2]
		elseif scenario_line_match(line, "SPEEDBRAKE %d %f", match) then
			spdb_state.status = match.res[1] + 1
			spdb_state.proc = match.res[2]
		elseif scenario_line_match(line, "SRB_IGNITION_TIME %f", match) then
			srbtime = match.res[1]
		elseif scenario_line_match(line, "SAT_OFS_X %f", match) then
			sts_sat_x = match.res[1]
		elseif scenario_line_match(line, "SAT_OFS_Y %f", match) then
			sts_sat_y = match.res[1]
		elseif scenario_line_match(line, "SAT_OFS_Z %f", match) then
			sts_sat_z = match.res[1]
		elseif scenario_line_match(line, "CARGO_STATIC_MESH %s", match) then
			cargo_static_mesh_name = match.res[1]
			do_cargostatic = true
		elseif scenario_line_match(line, "CARGO_STATIC_OFS %f %f %f", match) then
			cargo_static_ofs.x = match.res[1]
			cargo_static_ofs.y = match.res[2]
			cargo_static_ofs.z = match.res[3]
		elseif scenario_line_match(line, "ARM_STATUS %f %f %f %f %f %f", match) then
			arm_sy = match.res[1]
			arm_sp = match.res[2]
			arm_ep = match.res[3]
			arm_wp = match.res[4]
			arm_wy = match.res[5]
			arm_wr = match.res[6]
		else
			if plop.parse_scenario_line(line) then  -- offer the line to bay door operations
				local _ = 0
			elseif not ascap.parse_scenarioline(line) then -- offer to ascent autopilot
				vi:parse_scenario_line_ex(line, vs2)  -- unrecognised option - pass to Orbiter's generic parser
			end
		end
    end
	if status == 0 then
		vs = vs2:get()
		if bit.anyset(vs.status, 0x1) then -- idle flag
			launchelev = math.max(0.0, vs.vrot.x - 18.962)
			if vs.arot.x > 4.0 then   -- rotation matrix not defined - need to construct manually
				local slng = math.sin(vs.surf_lng)
				local clng = math.cos(vs.surf_lng)
				local slat = math.sin(vs.surf_lat)
				local clat = math.cos(vs.surf_lat)
				local sdir = math.sin(vs.surf_hdg)
				local cdir = math.cos(vs.surf_hdg)
				vs.arot.x =  math.atan2(slat, clat*slng)
				vs.arot.y = -math.asin(clng*clat)
				vs.arot.z =  math.atan2(clng*slat*cdir+slng*sdir, clng*slat*sdir-slng*cdir)
				vs2:set(vs)
			end
		else
			local rad = vec.length(vs.rpos)
			local alt = rad - oapi.get_size(vs.rbody)
			launchelev = math.max(0.0, alt - 18.962)
		end
	end

	if sts_sat_x ~= 0.0 or sts_sat_y ~= 0.0 or sts_sat_z ~= 0.0 then
		ofs_sts_sat.x = sts_sat_x
		ofs_sts_sat.y = sts_sat_y
		ofs_sts_sat.z = sts_sat_z
		vi:set_attachmentparams(sat_attach, ofs_sts_sat, _V(0, 1, 0), _V(0, 0, 1))
	end

	-- optional meshes
	if do_cargostatic and not mesh_cargo then
		mesh_cargo = vi:add_mesh(cargo_static_mesh_name, cargo_static_ofs)
	end
	if do_plat and not mesh_platform then
		mesh_platform = vi:add_mesh("shuttle_eva_plat", _V(-2.59805, 1.69209, -5.15524))
	end
	t0 = ascap.get_MT0()

	set_gear_parameters(gear_state.proc)
end


----------------------------------------------------------------
-- Write status to scenario file
----------------------------------------------------------------
function clbk_savestate(scn)
	-- custom parameters
	oapi.writescenario_int(scn, "CONFIGURATION", status)

	--if (status == 1)
	--	oapiWriteScenario_float (scn, "MET", oapiGetSimTime()-t0);

	oapi.writescenario_string(scn, "GEAR", tostring(gear_state.status-1).." "..tostring(gear_state.proc))

	if spdb_state.status ~= animstate.CLOSED then
		oapi.writescenario_string(scn, "SPEEDBRAKE", tostring(spdb_state.status-1).." "..tostring(spdb_state.proc))
	end

	--if (status == 0 && launchelev)
	--	oapiWriteScenario_float (scn, "LAUNCHELEVATION", launchelev);

	oapi.writescenario_string(scn, "ARM_STATUS", string.format("%0.4f %0.4f %0.4f %0.4f %0.4f %0.4f", arm_sy, arm_sp, arm_ep, arm_wp, arm_wy, arm_wr))

	oapi.writescenario_float(scn, "SAT_OFS_X", ofs_sts_sat.x)
	oapi.writescenario_float(scn, "SAT_OFS_Y", ofs_sts_sat.y)
	oapi.writescenario_float(scn, "SAT_OFS_Z", ofs_sts_sat.z)

	if do_cargostatic then
		oapi.writescenario_string(scn, "CARGO_STATIC_MESH", cargo_static_mesh_name)
		oapi.writescenario_vec(scn, "CARGO_STATIC_OFS", cargo_static_ofs)
	end

	-- save bay door operations status
	plop.save_state(scn)
	ascap.save_state(scn)
end



--------------------------------------------------------------

function clbk_postcreation()
	ascap.set_launchazimuth(ascap.get_launchazimuth())

	local drymass = ORBITER_EMPTY_MASS
	local payload = vi:get_attachmentstatus(sat_attach)
	if payload then
		drymass = drymass + oapi.get_emptymass(payload)
	end

	payload = vi:get_attachmentstatus(rms_attach)
	if payload then
		drymass = drymass + oapi.get_emptymass(payload)
	end

	if drymass ~= ORBITER_EMPTY_MASS then
		vi:set_emptymass(drymass)
	end

	if status < 3 then
		local hET = vi:get_dockstatus(vi:get_dockhandle(1))
		if not hET then
			local name = vi:get_name().."_ET"
			hET = vessel.get_handle(name)
			if not hET or vessel.get_interface(hET):get_classname() ~= "Atlantis_Tank.lua" then
				local vs = vi:get_rawstatus(1)
				hET = oapi.create_vessel(name, "Atlantis_Tank.lua", vs)
			end
			vi:dock(hET, 1, 0, 1)
		end
		pET = vessel.get_interface(hET)
		if status < 2 then
			local pV = vessel.get_interface(hET)
			for i=0,1 do
				local hSRB = pV:get_dockstatus(pV:get_dockhandle(i+1))
				if not hSRB then
					local name = string.format("%s-SRB%d", vi:get_name(), i+1)
					hSRB = vessel.get_handle(name)
					if not hSRB or vessel.get_interface(hSRB):get_classname() ~= "Atlantis_SRB.lua" then
						local vs = vi:get_rawstatus(1)
						hSRB = oapi.create_vessel(name, "Atlantis_SRB.lua", vs)
					end
					pV:dock(hSRB, i+1, 0, 1)
				end
			end
			if status < 1 then
				if launchelev ~= 0.0 then
					pET:set_SRB_launch_elevation(launchelev)
				end
			end
		end
	else
		if hDockET then
			vi:del_dock(hDockET) -- remove the ET docking port
			hDockET = nil
		end
	end
	enable_SSME(status < 3)
	enable_RCS(status == 3 and RCSMODE.ROT or RCSMODE.OFF)
	enable_OMS(status == 3)
	vi:set_adcmode(status < 4 and ADCMODE.OFF or ADCMODE.ON)

	update_mesh()
end

----------------------------------------------------------------
-- Vessel gains or loses input focus
----------------------------------------------------------------
function clbk_focuschanged(getfocus, newv, oldv)
	if getfocus then
		oapi.disable_mfdmode(MFDMODE.LANDING)
		-- no VTOL MFD mode for Atlantis
	end
end


----------------------------------------------------------------
-- Simulation time step
----------------------------------------------------------------
function clbk_prestep(simt, simdt, mjd)
	ascap.update(simt)
	--if (ascentApDlg) ascentApDlg->Update (simt);

	local met = ascap.get_MET(simt)

	local engine_light_level = vi:get_thrustergrouplevel(THGROUP.MAIN)
	engine_light:set_intensity(engine_light_level)

	local tgt_rate = _V(0, 0, 0) -- target rotation rates - used for setting engine gimbals

	if status >= 1 and status <= 3 then
		-- ascent autopilot
		tgt_rate = ascap.get_targetrate(met)

		-- manual override
		local man_pitch = vi:get_manualcontrollevel(THGROUP.ATT_PITCHUP, MANCTRL.ROTMODE, MANCTRL.ANYDEVICE)
		if man_pitch == 0.0 then
			man_pitch = -vi:get_manualcontrollevel(THGROUP.ATT_PITCHDOWN, MANCTRL.ROTMODE, MANCTRL.ANYDEVICE)
		end
		if man_pitch ~= 0.0 then
			tgt_rate.x = man_pitch*0.07
		end

		local man_yaw   = vi:get_manualcontrollevel(THGROUP.ATT_YAWLEFT, MANCTRL.ROTMODE, MANCTRL.ANYDEVICE)
		if man_yaw == 0.0 then
			man_yaw = -vi:get_manualcontrollevel(THGROUP.ATT_YAWRIGHT, MANCTRL.ROTMODE, MANCTRL.ANYDEVICE)
		end
		if man_yaw ~= 0 then
			tgt_rate.y = man_yaw*0.07
		end

		local man_roll  =-vi:get_manualcontrollevel(THGROUP.ATT_BANKLEFT, MANCTRL.ROTMODE, MANCTRL.ANYDEVICE)
		if man_roll == 0.0 then
			man_roll = vi:get_manualcontrollevel(THGROUP.ATT_BANKRIGHT, MANCTRL.ROTMODE, MANCTRL.ANYDEVICE)
		end
		if man_roll~= 0.0 then
			tgt_rate.z = man_roll*0.07
		end
	end

	if status == 0 then -- launch configuration
		if not ascap.active() and pET and vi:get_thrustergrouplevel(THGROUP.MAIN) > 0.95 then
			pET:ignite_SRBs()
			t0 = ascap.start_missiontime(simt)
			--//t0 = simt /*+ SRB_STABILISATION_TIME*/;   -- store designated liftoff time
			status = 1
		--elseif vi:get_thrustergrouplevel(THGROUP.MAIN) > 0.0 then
			-- //AutoGimbal (tgt_rate);
		end
	elseif status == 1 then -- SRBs ignited
		if met > SRB_SEPARATION_TIME and not vi:playback() or bManualSeparate  then
			separate_boosters(met)
			bManualSeparate = false
		else
			auto_gimbal(tgt_rate)
		end
	elseif status == 2 then -- Orbiter+ET configuration
		auto_gimbal(tgt_rate)
		if pET and bManualSeparate then
			separate_tank()
			bManualSeparate = false
		end
	elseif status == 3 then -- Orbiter
		if ascap.active() then
			auto_RCS(tgt_rate)
		end

		if bManualSeparate and vi:get_attachmentstatus(sat_attach) then
			detach_child_with_mass(sat_attach, 0.1)
			bManualSeparate = false
		end

		if do_eva then
			local name = vi:get_name().."-MMU"
			if not vessel.get_handle(name) then
				separate_MMU()
			end
			do_eva = false
		end

		if vi:get_dynpressure() > 1000.0 then
			-- 1000Pa ~ 20psf, see Mission Profile, https://science.ksc.nasa.gov/shuttle/technology/sts-newsref/mission_profile.html
			enable_RCS(RCSMODE.OFF)
			vi:set_adcmode(ADCMODE.ON)
			-- note: in reality, control doesn't switch from RCS to control surfaces completely in one go,
			-- but at different stages for different components
			status = 4
		end
--	elseif status == 4 then -- reentry
	end

	-- Execute payload bay operations
	plop.step(simt, simdt)

	-- ***** Animate landing gear *****
	if gear_state:moving() then
		gear_state:move(simdt)
		vi:set_animation(anim_gear, gear_state.proc)
		set_gear_parameters(gear_state.proc)
		gear_drag:set(gear_state.proc)
	end

	-- ***** Animate speedbrake *****

	if spdb_state:moving() then
		spdb_state:move(simdt)
		vi:set_animation(anim_spdb, spdb_state.proc)
		spdb_drag:set(spdb_state.proc)
	end

	-- ***** Stow RMS arm *****

	if center_arm then
		local t0 = oapi.get_simtime()
		local dt = t0 - center_arm_t       -- time step
		local da = ARM_OPERATING_SPEED*dt  -- total rotation angle

		-- work from the wrist down to the shoulder
		if da ~= 0 and arm_wr ~= 0.5 then    -- zero wrist roll
			if da >= math.abs(arm_wr-0.5) then -- finished
				arm_wr = 0.5
				da = da - math.abs(arm_wr-0.5)
			else
				arm_wr = arm_wr - (arm_wr > 0.5 and da or -da)
				da = 0
			end
			set_animation_arm(anim_arm_wr, arm_wr)
		end
		if da ~= 0 and arm_wy ~= 0.5 then    -- zero wrist yaw
			if da >= math.abs(arm_wy-0.5) then -- finished
				arm_wy = 0.5
				da = da - math.abs(arm_wy-0.5)
			else
				arm_wy = arm_wy - (arm_wy > 0.5 and da or -da)
				da = 0
			end
			set_animation_arm(anim_arm_wy, arm_wy)
		end
		if da ~= 0 and arm_wp ~= 0.5 then    -- zero wrist pitch
			if da >= math.abs(arm_wp-0.5) then -- finished
				arm_wp = 0.5
				da = da - math.abs(arm_wp-0.5)
			else
				arm_wp = arm_wp - (arm_wp > 0.5 and da or -da)
				da = 0
			end
			set_animation_arm(anim_arm_wp, arm_wp)
		end
		if da ~= 0 and arm_ep ~= 0 then             -- zero elbow pitch
			if da >= arm_ep then           -- finished
				arm_ep = 0.0
				da = da - arm_ep
			else
				arm_ep = arm_ep - da
				da = 0
			end
			set_animation_arm(anim_arm_ep, arm_ep)
		end
		if da ~= 0 and arm_sy ~= 0.5 then    -- zero shoulder yaw
			if da >= math.abs(arm_sy-0.5) then -- finished
				arm_sy = 0.5
				da = da - math.abs(arm_sy-0.5)
			else
				arm_sy = arm_sy - (arm_sy > 0.5 and da or -da)
				da = 0
			end
			set_animation_arm(anim_arm_sy, arm_sy)
		end
		if da ~= 0 and arm_sp ~= 0 then             -- zero shoulder pitch
			if da >= arm_sp then           -- finished
				arm_sp = 0.0
				da = da - arm_sp
			else
				arm_sp = arm_sp - da
				da = 0
			end
			set_animation_arm(anim_arm_sp, arm_sp)
		end
		center_arm_t = t0
		if da ~= 0 then
			center_arm = false -- finished stowing
		end
	end

	if arm_moved then
		vi:set_attachmentparams(rms_attach, arm_tip[0], vec.sub(arm_tip[1], arm_tip[0]), vec.sub(arm_tip[2], arm_tip[0]))
		arm_moved = false
	end
	if arm_scheduled then
		arm_scheduled = false
		arm_moved = true
	end
end

----------------------------------------------------------------
-- Respond to playback event
----------------------------------------------------------------
function clbk_playbackevent(simt, event_t, event_type, event)
	if event_type == "JET" then
		if event == "SRB" then
			bManualSeparate = true
			return true
		elseif event == "ET" then
			bManualSeparate = true
			return true
		end
	elseif event_type == "STATUS" then
		if event == "SRB_IGNITION" then
			status = 1
			t0 = event_t + SRB_STABILISATION_TIME
			return true
		end
	elseif event_type == "ADJUST_LAUNCHTIME" then
		to = tonumber(event)
		return true
	elseif event_type == "CARGODOOR" then
		if event == "OPEN" then
			plop.set_door_action(animstate.OPENING, true)
		elseif event == "CLOSE" then
			plop.set_door_action(animstate.CLOSING, true)
		elseif event == "ISOPEN" then
			plop.set_door_action(animstate.OPEN, true)
		elseif event == "ISCLOSED" then
			plop.set_door_action(animstate.CLOSED, true)
		end
		return true
	elseif event_type == "GEAR" then
		operate_landing_gear(event == "UP" and animstate.CLOSING or animstate.OPENING)
		return true
	elseif event_type == "SPEEDBRAKE" then
		operate_landing_gear(event == "CLOSE" and animstate.CLOSING or animstate.OPENING)
		return true
	elseif event_type == "KUBAND" then
		plop.set_Ku_antenna_action(event == "CLOSE" and animstate.CLOSING or animstate.OPENING)
		return true
	end

	return false
end

----------------------------------------------------------------
-- Atlantis mesh loaded
----------------------------------------------------------------
function clbk_visualcreated(_vis, refcount)
	if refcount > 1 then
		return -- we don't support more than one visual per object
	end
	vis = _vis

	-- make sure the RMS attachment point is in sync with the animation state of the visual
	vi:set_attachmentparams(rms_attach, arm_tip[0], vec.sub(arm_tip[1], arm_tip[0]), vec.sub(arm_tip[2], arm_tip[0]))
end

----------------------------------------------------------------
-- Atlantis mesh discarded
----------------------------------------------------------------
function clbk_visualdestroyed(_vis, refcount)
	if vis == _vis then
		vis = nil
	end
end

----------------------------------------------------------------
-- Update mesh animation state
----------------------------------------------------------------
function clbk_animate(simt)
	update_mesh()
end

-- define MFD function buttons
local AID_CDR1_BUTTONS =  1
local AID_CDR2_BUTTONS =  2
local AID_PLT1_BUTTONS =  3
local AID_PLT2_BUTTONS =  4
local AID_MFD1_BUTTONS =  5
local AID_MFD2_BUTTONS =  6
local AID_MFD3_BUTTONS =  7
local AID_MFD4_BUTTONS =  8
local AID_MFD5_BUTTONS =  9
local AID_MFDA_BUTTONS = 10
-- D. Beachy: define power buttons
local AID_CDR1_PWR = 11
local AID_CDR2_PWR = 12
local AID_PLT1_PWR = 13
local AID_PLT2_PWR = 14
local AID_MFD1_PWR = 15
local AID_MFD2_PWR = 16
local AID_MFD3_PWR = 17
local AID_MFD4_PWR = 18
local AID_MFD5_PWR = 19
local AID_MFDA_PWR = 20
-- MFD brightness buttons
local AID_CDR1_BRT = 21
local AID_CDR2_BRT = 22
local AID_PLT1_BRT = 23
local AID_PLT2_BRT = 24
local AID_MFD1_BRT = 25
local AID_MFD2_BRT = 26
local AID_MFD3_BRT = 27
local AID_MFD4_BRT = 28
local AID_MFD5_BRT = 29
local AID_MFDA_BRT = 30
-- Panel R13L (payload bay operations)
local AID_R13L_MIN   = 100
local AID_R13L       = 100
local AID_R13L_TKBK1 = 101
local AID_R13L_TKBK2 = 102
local AID_R13L_TKBK3 = 103
local AID_R13L_TKBK4 = 104
local AID_R13L_TKBK5 = 105
local AID_R13L_TKBK6 = 106
local AID_R13L_MAX   = 120

----------------------------------------------------------------
-- Respond to MFD mode change
----------------------------------------------------------------
function clbk_MFDmode(mfd, mode)
	oapi.VC_trigger_redrawarea(-1, AID_CDR1_BUTTONS + mfd - MFDID.LEFT)
end

----------------------------------------------------------------
-- Load generic glass cockpit mode
----------------------------------------------------------------
function clbk_loadgenericcockpit()
	vi:set_cameraoffset(_V(-0.67, 2.55, 14.4))
	vi:set_cameradefaultdirection(_V(0, 0, 1))
	return true
end

----------------------------------------------------------------
-- register VC buttons for the 2 commander MFDs
-- (accessible from commander position only)
----------------------------------------------------------------
local function registerVC_CdrMFD()
	-- activate MFD function buttons
	oapi.VC_set_areaclickmode_quadrilateral(AID_CDR1_BUTTONS, _V(-0.9239,2.0490,15.0595), _V(-0.7448,2.0490,15.0595),  _V(-0.9239,2.0280,15.0595), _V(-0.7448,2.0280,15.0595))
	oapi.VC_set_areaclickmode_quadrilateral(AID_CDR2_BUTTONS, _V(-0.6546,2.0490,15.0595), _V(-0.4736,2.0490,15.0595),  _V(-0.6546,2.0280,15.0595), _V(-0.4736,2.0280,15.0595))

    -- D. Beachy: register+activate MFD power buttons
    local powerButtonRadius = 0.0075 -- radius of power button on each MFD
	oapi.VC_register_area(AID_CDR1_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_CDR2_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
    oapi.VC_set_areaclickmode_spherical(AID_CDR1_PWR, _V(-0.950, 2.060, 15.060), powerButtonRadius)
    oapi.VC_set_areaclickmode_spherical(AID_CDR2_PWR, _V(-0.680, 2.060, 15.060), powerButtonRadius)

	-- register+activate MFD brightness buttons
	oapi.VC_register_area(AID_CDR1_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_CDR2_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_set_areaclickmode_quadrilateral(AID_CDR1_BRT, _V(-0.729,2.0675,15.060), _V(-0.714,2.0675,15.060), _V(-0.729,2.0525,15.060), _V(-0.714,2.0525,15.060))
	oapi.VC_set_areaclickmode_quadrilateral(AID_CDR2_BRT, _V(-0.459,2.0675,15.060), _V(-0.444,2.0675,15.060), _V(-0.459,2.0525,15.060), _V(-0.444,2.0525,15.060))
end


----------------------------------------------------------------
-- register VC buttons for the 2 pilot MFDs
-- (accessible from pilot position only)
----------------------------------------------------------------
local function registerVC_PltMFD()
	-- activate MFD function buttons
	oapi.VC_set_areaclickmode_quadrilateral(AID_PLT1_BUTTONS, _V(0.4759,2.0490,15.0595), _V(0.6568,2.0490,15.0595),  _V(0.4759,2.0280,15.0595), _V(0.6568,2.0280,15.0595))
	oapi.VC_set_areaclickmode_quadrilateral(AID_PLT2_BUTTONS, _V(0.7461,2.0490,15.0595), _V(0.9271,2.0490,15.0595),  _V(0.7461,2.0280,15.0595), _V(0.9271,2.0280,15.0595))

	-- D. Beachy: register+activate MFD power buttons
    local powerButtonRadius = 0.0075 -- radius of power button on each MFD
	oapi.VC_register_area(AID_PLT1_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_PLT2_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
    oapi.VC_set_areaclickmode_spherical(AID_PLT1_PWR, _V( 0.450, 2.060, 15.060), powerButtonRadius)
    oapi.VC_set_areaclickmode_spherical(AID_PLT2_PWR, _V( 0.720, 2.060, 15.060), powerButtonRadius)

	-- register+activate MFD brightness buttons
	oapi.VC_register_area(AID_PLT1_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_PLT2_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_set_areaclickmode_quadrilateral(AID_PLT1_BRT, _V(0.671,2.0675,15.060), _V(0.686,2.0675,15.060), _V(0.671,2.0525,15.060), _V(0.686,2.0525,15.060))
	oapi.VC_set_areaclickmode_quadrilateral(AID_PLT2_BRT, _V(0.941,2.0675,15.060), _V(0.956,2.0675,15.060), _V(0.941,2.0525,15.060), _V(0.956,2.0525,15.060))
end


----------------------------------------------------------------
-- register VC buttons for the 5 MFDs on the central panel
-- (accessible from commander and pilot positions)
----------------------------------------------------------------
local function registerVC_CntMFD()
	-- activate MFD function buttons
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD1_BUTTONS, _V(-0.3579,2.1451,15.0863), _V(-0.1770,2.1451,15.0863), _V(-0.3579,2.1241,15.0863), _V(-0.1770,2.1241,15.0863))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD2_BUTTONS, _V(-0.3579,1.9143,15.0217), _V(-0.1770,1.9143,15.0217), _V(-0.3579,1.8933,15.0217), _V(-0.1770,1.8933,15.0217))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD3_BUTTONS, _V(-0.0888,2.0288,15.0538), _V(0.0922,2.0288,15.0538), _V(-0.0888,2.0078,15.0538), _V(0.0922,2.0078,15.0538))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD4_BUTTONS, _V(0.1795,2.1451,15.0863), _V(0.3604,2.1451,15.0863), _V(0.1795,2.1241,15.0863), _V(0.3604,2.1241,15.0863))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD5_BUTTONS, _V(0.1795,1.9143,15.0217), _V(0.3604,1.9143,15.0217), _V(0.1795,1.8933,15.0217), _V(0.3604,1.8933,15.0217))

	-- D. Beachy: register+activate MFD power buttons
    local powerButtonRadius = 0.0075 -- radius of power button on each MFD
	oapi.VC_register_area(AID_MFD1_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_MFD2_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_MFD3_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_MFD4_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_MFD5_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
    oapi.VC_set_areaclickmode_spherical(AID_MFD1_PWR, _V(-0.383, 2.153, 15.090), powerButtonRadius)
    oapi.VC_set_areaclickmode_spherical(AID_MFD2_PWR, _V(-0.383, 1.922, 15.023), powerButtonRadius)
    oapi.VC_set_areaclickmode_spherical(AID_MFD3_PWR, _V(-0.114, 2.037, 15.058), powerButtonRadius)
    oapi.VC_set_areaclickmode_spherical(AID_MFD4_PWR, _V( 0.155, 2.153, 15.090), powerButtonRadius)
    oapi.VC_set_areaclickmode_spherical(AID_MFD5_PWR, _V( 0.155, 1.922, 15.023), powerButtonRadius)

	-- register+activate MFD brightness buttons
	oapi.VC_register_area(AID_MFD1_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_MFD2_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_MFD3_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_MFD4_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_register_area(AID_MFD5_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD1_BRT, _V(-0.162,2.1605,15.090), _V(-0.147,2.1605,15.090), _V(-0.162,2.1455,15.090), _V(-0.147,2.1455,15.090))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD2_BRT, _V(-0.162,1.9295,15.023), _V(-0.147,1.9295,15.023), _V(-0.162,1.9145,15.023), _V(-0.147,1.9145,15.023))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD3_BRT, _V(0.107,2.0445,15.058), _V(0.122,2.0445,15.058), _V(0.107,2.0295,15.058), _V(0.122,2.0295,15.058))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD4_BRT, _V(0.376,2.1605,15.090), _V(0.391,2.1605,15.090), _V(0.376,2.1455,15.090), _V(0.391,2.1455,15.090))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFD5_BRT, _V(0.376,1.9295,15.023), _V(0.391,1.9295,15.023), _V(0.376,1.9145,15.023), _V(0.391,1.9145,15.023))
end

----------------------------------------------------------------
-- register VC buttons for the aft MFD at the starbord panel
-- (accessible from payload control position only)
----------------------------------------------------------------
local function registerVC_AftMFD()
	-- register+activate aft MFD function buttons
	local tex1 = oapi.get_texturehandle(hOrbiterVCMesh, 7)
	oapi.VC_register_area(AID_MFDA_BUTTONS, _R(0,127,255,140), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFDA_BUTTONS, _V(1.3862,2.2570,13.8686), _V(1.3862,2.2570,13.6894), _V(1.3678,2.2452,13.8686), _V(1.3678,2.2452,13.6894))

	-- register+activate MFD power button
    local powerButtonRadius = 0.0075 -- radius of power button on each MFD
	oapi.VC_register_area(AID_MFDA_PWR, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.ONREPLAY))
    oapi.VC_set_areaclickmode_spherical(AID_MFDA_PWR, _V(1.3929,2.2632,13.8947), powerButtonRadius)

	-- register+activate MFD brightness buttons
	oapi.VC_register_area(AID_MFDA_BRT, PANEL_REDRAW.NEVER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY))
	oapi.VC_set_areaclickmode_quadrilateral(AID_MFDA_BRT, _V(1.4024,2.2675,13.6736), _V(1.4024,2.2675,13.6586), _V(1.3893,2.2590,13.6736), _V(1.3893,2.2590,13.6586))
end


----------------------------------------------------------------
-- Load virtual cockpit mode
----------------------------------------------------------------
function clbk_loadVC(id)
	local huds = {                  -- common HUD specs
		nmesh = mesh_vc,
		ngroup = GRP_VC.VirtualHUD,
		hudcnt = _V(0,0,0),         -- hudcnt (to be filled)
		size = 0.176558
	}
	local mfds = {                -- common MFD specs
		pos = _R(0,0,0,0),
		nmesh = mesh_vc,
		ngroup = 0,                  -- ngroup (to be filled)
		flag = MFD_SHOWMODELABELS,
		nbt1 = 5,
		nbt2 = 0,
		bt_yofs = 512/6,
		bt_ydist = 512/7
	}
	local mfdgrp = {
		GRP_VC.CDR1,GRP_VC.CDR2,GRP_VC.PLT1,GRP_VC.PLT2,
		GRP_VC.MFD1, GRP_VC.MFD2, GRP_VC.MFD3, GRP_VC.MFD4, GRP_VC.MFD5,
		GRP_VC.MFD_aft
	}

	local ok = false

	-- register MFD function buttons
	-- this needs to be done globally, so that the labels are correctly updated from all VC positions
	local tex1 = oapi.get_texturehandle(hOrbiterVCMesh, 7)

	-- commander MFD function buttons
	oapi.VC_register_area(AID_CDR1_BUTTONS, _R(0,1,255,14), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	oapi.VC_register_area(AID_CDR2_BUTTONS, _R(0,15,255,28), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	-- pilot MFD function buttons
	oapi.VC_register_area(AID_PLT1_BUTTONS, _R(0,29,255,42), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	oapi.VC_register_area(AID_PLT2_BUTTONS, _R(0,43,255,56), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	-- central console MFD function buttons
	oapi.VC_register_area(AID_MFD1_BUTTONS, _R(0, 57,255, 70), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	oapi.VC_register_area(AID_MFD2_BUTTONS, _R(0, 71,255, 84), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	oapi.VC_register_area(AID_MFD3_BUTTONS, _R(0, 85,255, 98), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	oapi.VC_register_area(AID_MFD4_BUTTONS, _R(0, 99,255,112), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)
	oapi.VC_register_area(AID_MFD5_BUTTONS, _R(0,113,255,126), PANEL_REDRAW.USER, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBUP, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.ONREPLAY), PANEL_MAP.BACKGROUND, tex1)

	if id == 0 then -- commander position
		vi:set_cameraoffset(_V(-0.67,2.55,14.4))
		vi:set_cameradefaultdirection(_V(0,0,1))
		vi:set_cameramovement(_V(0,0,0.3), 0, 0, _V(-0.3,0,0), 75*RAD, -5*RAD, _V(0.3,0,0), -20*RAD, -27*RAD)
		huds.hudcnt = _V(-0.671257, 2.523535, 14.969)
		oapi.VC_set_neighbours(-1, 1, -1, 2)

		registerVC_CdrMFD() -- activate commander MFD controls
		registerVC_CntMFD() -- activate central panel MFD controls

		ok = true
	elseif id == 1 then -- pilot position
		vi:set_cameraoffset(_V(0.67,2.55,14.4))
		vi:set_cameradefaultdirection(_V(0,0,1))
		vi:set_cameramovement(_V(0,0,0.3), 0, 0, _V(-0.3,0,0), 20*RAD, -27*RAD, _V(0.3,0,0), -75*RAD, -5*RAD)
		huds.hudcnt = _V(0.671257, 2.523535, 14.969)
		oapi.VC_set_neighbours(0, -1, -1, 2)

		registerVC_PltMFD() -- activate pilot MFD controls
		registerVC_CntMFD() -- activate central panel MFD controls

		ok = true
	elseif id == 2 then -- payload view position
		vi:set_cameraoffset(_V(0.4,3.15,13.0))
		vi:set_cameradefaultdirection(_V(0,0,-1))
		vi:set_cameramovement(_V(0,-0.1,-0.1), 0, 80.0*RAD, _V(0.3,-0.3,0.15), 60.0*RAD, -50.0*RAD, _V(-0.8,0,0), 0, 0)
		oapi.VC_set_neighbours(1, 0, -1, 0)

		registerVC_AftMFD() -- activate aft MFD controls
		plop.register_VC()  -- register panel R13L interface
		ok = true
	end

	if ok then
		-- register the HUDs (synced)
		oapi.VC_registerHUD(huds)
		-- register all MFD displays
		for i=0, 9 do
			mfds.ngroup = mfdgrp[i + 1]
			oapi.register_mfd(MFDID.LEFT + i, mfds)
		end
		-- update panel R13L
		plop.update_VC()
	end
	return ok
end

----------------------------------------------------------------
-- Respond to virtual cockpit mouse event
----------------------------------------------------------------
VCME_sel_counting = false
VCME_sel_t0 = 0.0

function clbk_VCmouseevent(id, event, p)
	-- handle MFD selection buttons
	local case_selection = {
	  [AID_CDR1_BUTTONS] = true,
	  [AID_CDR2_BUTTONS] = true,
	  [AID_PLT1_BUTTONS] = true,
	  [AID_PLT2_BUTTONS] = true,
	  [AID_MFD1_BUTTONS] = true,
	  [AID_MFD2_BUTTONS] = true,
	  [AID_MFD3_BUTTONS] = true,
	  [AID_MFD4_BUTTONS] = true, 
	  [AID_MFD5_BUTTONS] = true,
	  [AID_MFDA_BUTTONS] = true
	}
	if case_selection[id] then
		local mfd = id - AID_CDR1_BUTTONS + MFDID.LEFT
		local bt = p.x * 5.99
		if bt < 5 then
			oapi.process_mfdbutton(mfd, bt, event)
		else
			if bit.allset(event, PANEL_MOUSE.LBDOWN) then
				VCME_sel_t0 = oapi.get_systime()
				VCME_sel_counting = true
			elseif bit.allset(event, PANEL_MOUSE.LBUP) and VCME_sel_counting then
				oapi.send_mfdkey(mfd, OAPI_KEY.F2)
				VCME_sel_counting = false
			elseif bit.allset(event, PANEL_MOUSE.LBPRESSED) and VCME_sel_counting and (oapi.get_systime() - VCME_sel_t0 >= 1.0) then
				oapi.send_mfdkey(mfd, OAPI_KEY.F1)
				VCME_sel_counting = false
			end
		end
		return true
	end

    -- D. Beachy: handle power buttons
	local case_pwr = {
     [AID_CDR1_PWR] = true,
     [AID_CDR2_PWR] = true,
	 [AID_PLT1_PWR] = true,
	 [AID_PLT2_PWR] = true,
     [AID_MFD1_PWR] = true,
     [AID_MFD2_PWR] = true,
     [AID_MFD3_PWR] = true,
	 [AID_MFD4_PWR] = true, 
	 [AID_MFD5_PWR] = true,
	 [AID_MFDA_PWR] = true
	}
	if case_pwr[id] then
        local mfd = id - AID_CDR1_PWR + MFDID.LEFT
        oapi.send_mfdkey(mfd, OAPI_KEY.ESCAPE)
        return true
    end
	-- handle MFD brightness buttons
	local case_brt = {
	 [AID_CDR1_BRT] = true,
	 [AID_CDR2_BRT] = true,
	 [AID_PLT1_BRT] = true,
	 [AID_PLT2_BRT] = true,
	 [AID_MFD1_BRT] = true,
	 [AID_MFD2_BRT] = true,
	 [AID_MFD3_BRT] = true, 
	 [AID_MFD4_BRT] = true, 
	 [AID_MFD5_BRT] = true,
	 [AID_MFDA_BRT] = true
	}
	if case_brt[id] then
		local mfd = id - AID_CDR1_BRT
		if bit.allset(event, PANEL_MOUSE.LBDOWN) then
			brt_up = (p.x >= 0.5)
			brt_t0 = oapi.get_systime()
			brt_brt0 = mfdbright[mfd]
		elseif bit.allset(event, PANEL_MOUSE.LBPRESSED) then
			local dt = oapi.get_systime() - brt_t0
			local brt
			local dbrt = dt * 0.2
			if brt_up then
				brt = math.min(1.0, brt_brt0 + dbrt)
			else
				brt = math.max(0.25, brt_brt0 - dbrt)
			end
			mfdbright[mfd] = brt
			if vis then
				local hMesh = vi:get_devmesh(vis, mesh_vc)
				local mat = {
					diffuse  = _COLOUR4(0,0,0,0),
					ambient  = _COLOUR4(0,0,0,0),
					specular = _COLOUR4(0,0,0,0),
					emissive = _COLOUR4(brt,brt,brt,1.0),
					power   = 0
				}
				oapi.set_material(hMesh, 10 + mfd, mat)
			end
		end
		return false
	end
	-- handle panel R13L events (payload bay operations)
	if id == AID_R13L then
		return plop.VC_mouse_event(id, event, p)
	end
	return false
end


----------------------------------------------------------------
-- Respond to virtual cockpit area redraw request
----------------------------------------------------------------
function clbk_VCredrawevent(id, event, surf)
	local case_selection = {
	  [AID_CDR1_BUTTONS] = true,
	  [AID_CDR2_BUTTONS] = true,
	  [AID_PLT1_BUTTONS] = true,
	  [AID_PLT2_BUTTONS] = true,
	  [AID_MFD1_BUTTONS] = true,
	  [AID_MFD2_BUTTONS] = true,
	  [AID_MFD3_BUTTONS] = true,
	  [AID_MFD4_BUTTONS] = true, 
	  [AID_MFD5_BUTTONS] = true,
	  [AID_MFDA_BUTTONS] = true
	}
	if case_selection[id] then
		local mfd = id - AID_CDR1_BUTTONS + MFDID.LEFT
		redraw_panel_MFD_button(surf, mfd)
		return true
	else
		if id >= AID_R13L_MIN and id <= AID_R13L_MAX then
			return plop.VC_redraw_event(id, event, surf)
		end
	end
	return false
end


----------------------------------------------------------------
-- Respond to a HUD redraw request
----------------------------------------------------------------
function clbk_drawHUD(mode, hps, skp)
	local cx = hps.CX
	local cy = hps.CY

	-- show OMS thrust marker
	if status >= 3 then
		local omsy = cy + math.floor(15.0 * hps.Scale)
		local dx = math.floor(1.0 * hps.Scale)
		skp:line(cx - 2 * dx, omsy, cx + 2 * dx, omsy)
		skp:line(cx, omsy - dx, cx, omsy + dx)
	end

	-- show RCS mode
	if status >= 3 and oapi.cockpit_mode() == COCKPIT.VIRTUAL then
		local rcsmode = vi:get_rcsmode()
		if rcsmode == RCSMODE.ROT then
			skp:text(0, hps.H - 13, "RCS ROT", 7)
		elseif rcsmode == RCSMODE.LIN then
			skp:text(0, hps.H - 13, "RCS LIN", 7)
		end
	end
	return true
end

----------------------------------------------------------------
-- Keyboard interface handler (buffered key events)
----------------------------------------------------------------
function clbk_consumebufferedkey(key, down, kstate)
    if not down then -- only process keydown events
        return false
    end

	if oapi.keydown(kstate, OAPI_KEY.LSHIFT) or oapi.keydown(kstate, OAPI_KEY.RSHIFT) then
		if key == OAPI_KEY.E then
			if status ~= 3 then
				return true -- Allow MMU only after orbiter has detached from MT
			end
			return true
		end
	elseif oapi.keydown(kstate, OAPI_KEY.LCONTROL) or oapi.keydown(kstate, OAPI_KEY.RCONTROL) then
		if key == OAPI_KEY.SPACE then -- open RMS control dialog
--			oapiOpenDialogEx (g_Param.hDLL, IDD_CTRL, Atlantis_DlgProc, 0, this);
			return true
		elseif key == OAPI_KEY.B then -- deploy/retract speedbrake
			if not vi:playback() then
				revert_speedbrake()
			end
			return true
		elseif key == OAPI_KEY.U then -- deploy/store Ku-band antenna
			if not vi:playback() then plop.revert_Ku_antenna_action() end
			return true
		end
	else -- unmodified keys
		if key == OAPI_KEY.G then  -- "Landing gear"
			if not vi:playback() then
				revert_landing_gear()
			end
			return true
		elseif key == OAPI_KEY.J then  -- "Jettison"
			if not vi:playback() then
				bManualSeparate = true
			end
			return true
		elseif key == OAPI_KEY.K then  -- "Cargo bay doors"
			if not vi:playback() then plop.revert_door_action() end
			return true
		elseif key == OAPI_KEY.KEY8 then
			toggle_grapple()
			return true
		elseif key == OAPI_KEY.KEY9 then
			center_arm = true
			return true
		elseif key == OAPI_KEY.E then
			do_eva = true
			return true
		end
	end
	return false
end


function get_Ap()
	return ascap
end

function get_status()
	return status
end

-- helper function to launch from a LuaMFD instance
function launch()
	ascap.launch()
end
