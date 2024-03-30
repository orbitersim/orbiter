-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local ComponentVessel = require("ComponentVessel")
local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP
local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local MainRetroSubsystem = require("MainRetroSubsystem")
local MfdSubsystem = require("MfdSubsystem")
local GearSubsystem = require("GearSubsystem")
local AerodynCtrlSubsystem = require("AerodynCtrlSubsystem")
local RcsSubsystem = require("RcsSubsystem")
local HoverSubsystem = require("HoverSubsystem")
local HUDControl = require("HUDControl")
local AvionicsSubsystem = require("AvionicsSubsystem")
local LightCtrlSubsystem = require("LightCtrlSubsystem")
local ThermalSubsystem = require("ThermalSubsystem")
local PressureSubsystem = require("PressureSubsystem")
local DockingCtrlSubsystem = require("DockingCtrlSubsystem")

local EMPTY_MASS    = 11000.0  -- standard configuration
local EMPTY_MASS_SC = 13000.0  -- ramjet configuration
-- DG mass w/o fuel

local TANK1_CAPACITY = 10400.0
local TANK2_CAPACITY =  2500.0
-- Main fuel tank capacities [kg] (can be split between rocket
-- fuel and scramjet fuel)

local RCS_FUEL_CAPACITY = 600.0
-- Max fuel capacity: RCS tank [kg]


local PSNGR_MASS    =     85.0
-- mass per passenger (including life support system)

-- ============ Damage parameters ==============

local WINGLOAD_MAX =  16e3
local WINGLOAD_MIN = -10e3
-- Max. allowed positive and negative wing load [N/m^2]

local DYNP_MAX = 300e3
-- Max. allowed dynamic pressure [Pa]

-- =============================================
-- 2D instrument panel parameters

local PANEL2D_WIDTH = 1280  -- panel width [pixel]
local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height
local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024

local AID_MAINDISP1    =   32 -- obsolete
local AID_MAINDISP2    =   33 -- obsolete
local AID_MAINDISP3    =   34 -- obsolete
local AID_SCRAMDISP2   =   36

-- Panel 1
local AID_SCRAMTEMPDISP  =  114

-- Virtual cockpit-specific area identifiers:
local AID_MFD1_PWR     =   300
local AID_MFD2_PWR     =   301

local CAM_GENERIC = 0
local CAM_PANELMAIN = 1
local CAM_PANELUP = 2
local CAM_PANELDN = 3
local CAM_VCPILOT = 4
local CAM_VCPSNGR1 = 5
local CAM_VCPSNGR2 = 6
local CAM_VCPSNGR3 = 7
local CAM_VCPSNGR4 = 8

local MAX_MAIN_THRUST = {2.0e5, 1.6e5}
-- Main engine max vacuum thrust [N] per engine. (x2 for total)

local MAX_RETRO_THRUST = 3.4e4
-- Retro engine max vacuum thrust [N] per engine. (x2 for total)

local MAX_HOVER_THRUST = {1.4e5, 1.1e5}
-- Hover engine max vacuum thrust [N] (x2 for total)

local MAX_RCS_THRUST = 2.5e3
-- Attitude control system max thrust [N] per engine.

local ISP = 4e4
-- Vacuum Isp (fuel-specific impulse) for all thrusters [m/s]

local tdvtx_geardown = {
	{ pos = _V( 0   ,-2.57,10   ), stiffness = 1e6, damping = 1e5, mu = 1.6, mu_lng = 0.1},
	{ pos = _V(-3.5 ,-2.57,-1   ), stiffness = 1e6, damping = 1e5, mu = 3.0, mu_lng = 0.2},
	{ pos = _V( 3.5 ,-2.57,-1   ), stiffness = 1e6, damping = 1e5, mu = 3.0, mu_lng = 0.2},
	{ pos = _V(-8.5 ,-0.3 ,-7.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V( 8.5 ,-0.3 ,-7.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V(-8.5 ,-0.4 ,-3   ), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V( 8.5 ,-0.4 ,-3   ), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V(-8.85, 2.3 ,-5.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V( 8.85, 2.3 ,-5.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V(-8.85, 2.3 ,-7.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V( 8.85, 2.3 ,-7.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V( 0   , 2   , 6.2 ), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 },
	{ pos = _V( 0   ,-0.6 ,10.65), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0 }
}

local tdvtx_gearup = {
	{ pos = _V( 0   ,-1.5 ,9),     stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 3.0},
	{ pos = _V(-6   ,-0.8 ,-5),    stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 3.0},
	{ pos = _V( 3   ,-1.2 ,-5),    stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 3.0},
	{ pos = _V(-8.5 ,-0.3 ,-7.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V( 8.5 ,-0.3 ,-7.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V(-8.5 ,-0.4 ,-3   ), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V( 8.5 ,-0.4 ,-3   ), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V(-8.85, 2.3 ,-5.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V( 8.85, 2.3 ,-5.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V(-8.85, 2.3 ,-7.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V( 8.85, 2.3 ,-7.05), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V( 0   , 2   , 6.2 ), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0},
	{ pos = _V( 0   ,-0.6 ,10.65), stiffness = 1e7, damping = 1e5, mu = 3.0, mu_lng = 0}
}

-- ==============================================================
-- Airfoil coefficient functions
-- Return lift, moment and zero-lift drag coefficients as a
-- function of angle of attack (alpha or beta)
-- ==============================================================

-- 1. vertical lift component (wings and body)

local function VLiftCoeff(hVessel,aoa,M,Re)
	local nabsc = 9
	local AOA = {-180*RAD,-60*RAD,-30*RAD, -2*RAD, 15*RAD,20*RAD,25*RAD,60*RAD,180*RAD}
	local CL  = {       0,      0,   -0.4,      0,    0.7,     1,   0.8,     0,      0}
	local CM  = {       0,      0,  0.014, 0.0039, -0.006,-0.008,-0.010,     0,      0}

	local i = 1
	while AOA[i+1] < aoa and i < nabsc do
		i = i + 1
	end

	local cl, cm, cd
	if i < nabsc then
		local f = (aoa - AOA[i]) / (AOA[i + 1] - AOA[i])
		cl = CL[i] + (CL[i + 1] - CL[i]) * f  -- aoa-dependent lift coefficient
		cm = CM[i] + (CM[i + 1] - CM[i]) * f  -- aoa-dependent moment coefficient
	else
		cl = CL[nabsc]
		cm = CM[nabsc]
	end
	local saoa = math.sin(aoa)
	local pd = 0.015 + 0.4*saoa*saoa  -- profile drag
	cd = pd + oapi.get_induceddrag (cl, 1.5, 0.7) + oapi.get_wavedrag (M, 0.75, 1.0, 1.1, 0.04)
	-- profile drag + (lift-)induced drag + transonic/supersonic wave (compressibility) drag
	return cl, cm, cd
end

-- 2. horizontal lift component (vertical stabilisers and body)

local function HLiftCoeff(hVessel,beta,M,Re)
	local nabsc = 8
	local BETA = {-180*RAD,-135*RAD,-90*RAD,-45*RAD,45*RAD,90*RAD,135*RAD,180*RAD}
	local CL   = {       0,     0.3,      0,   -0.3,   0.3,     0,   -0.3,      0}

	local i = 1
	while BETA[i+1] < beta and i < nabsc do
		i = i + 1
	end

	local cl, cd
	if i < nabsc then
		cl = CL[i] + (CL[i + 1] - CL[i]) * (beta - BETA[i]) / (BETA[i + 1] - BETA[i])
	else
		cl = CL[nabsc]
	end
	cd = 0.015 + oapi.get_induceddrag (cl, 1.5, 0.6) + oapi.get_wavedrag (M, 0.75, 1.0, 1.1, 0.04);
	return cl, 0, cd
end

g_Param = {}
g_Param.pen = {}
g_Param.pen[0] = oapi.create_pen (0, 1, _RGB(224,224,224))
g_Param.pen[1] = oapi.create_pen (0, 3, _RGB(164,164,164))
g_Param.surf = oapi.load_texture ("DG/blitsrc1.dds", true)


-- ==============================================================
-- Specialised vessel class DeltaGlider
-- ==============================================================

-- --------------------------------------------------------------
-- Constructor
-- --------------------------------------------------------------

DeltaGlider = VesselClass(ComponentVessel)

function DeltaGlider:new(fmodel)
--	oapi.write_log("DeltaGlider:new")
	ComponentVessel.new(self)

	self.modelidx = fmodel ~= 0 and 2 or 1

	-- Subsystem definitions
	self.ssys_mainretro    = self:AddSubsystem(MainRetroSubsystem(self))
	self.ssys_hoverctrl    = self:AddSubsystem(HoverSubsystem (self))
	self.ssys_rcs          = self:AddSubsystem(RcsSubsystem (self))
	self.ssys_aerodyn      = self:AddSubsystem(AerodynCtrlSubsystem (self))
	self.ssys_gear         = self:AddSubsystem(GearSubsystem (self))
	self.ssys_hud          = self:AddSubsystem(HUDControl (self))
	self.ssys_pressurectrl = self:AddSubsystem(PressureSubsystem (self))
	self.ssys_thermal      = self:AddSubsystem(ThermalSubsystem (self))
	self.ssys_docking      = self:AddSubsystem(DockingCtrlSubsystem (self))
	self.ssys_light        = self:AddSubsystem(LightCtrlSubsystem (self))
	self.ssys_avionics     = self:AddSubsystem(AvionicsSubsystem (self))
--	self.ssys_failure      = self:AddSubsystem(FailureSubsystem (self))

	self.ssys_mfd = {}
	self.ssys_mfd[1] = self:AddSubsystem(MfdSubsystem (self, MFDID.LEFT))
	self.ssys_mfd[2] = self:AddSubsystem(MfdSubsystem (self, MFDID.RIGHT))

	self.ssys_scram = nil -- creation deferred to clbkSetClassCaps

	self.visual            = nil
	self.exmesh            = nil
	self.vcmesh            = nil
	self.vcmesh_tpl        = nil
	self.insignia_tex      = nil
	self.contrail_tex      = nil
	self.hPanelMesh        = nil
	self.panelcol          = 0
	self.campos            = CAM_GENERIC
	self.th_main_level     = 0.0
	self.skinpath = nil
	self.skin = {}
	self.psngr = {false, false, false, false}
	self.scflowidx = {0, 0}
	self.mainflowidx = {-1, -1}
	self.retroflowidx = {-1, -1}
	self.scTSFCidx = {-1, -1}

	self.mainTSFCidx = -1
	self.hoverflowidx = -1
	
	-- damage parameters
	self.bDamageEnabled = self:get_damagemodel() ~= 0
	self.lwingstatus = 1.0
	self.rwingstatus = 1.0
	self.aileronfail = {false, false, false, false}

	self:DefineAnimations()
end


-- --------------------------------------------------------------
-- Destructor
-- --------------------------------------------------------------
function DeltaGlider:destroy()
	if self.insignia_tex then
		oapi.destroy_surface(self.insignia_tex)
	end

	for i=1,3 do
		if self.skin[i] then
			oapi.release_texture(self.skin[i])
		end
	end
	oapi.release_texture(g_Param.surf)
end

-- --------------------------------------------------------------
-- Set vessel mass excluding propellants
-- --------------------------------------------------------------
function DeltaGlider:SetEmptyMass()
	local emass = self.ssys_scram and EMPTY_MASS_SC or EMPTY_MASS
	-- add passengers+life support to empty vessel mass
	for i=1,4 do
		if self.psngr[i] then emass = emass + PSNGR_MASS end
	end

	ComponentVessel.set_emptymass (self, emass)
end

function DeltaGlider:ScramVersion() 
	return self.ssys_scram ~= nil
end

-- --------------------------------------------------------------
-- Define animation sequences for moving parts
-- --------------------------------------------------------------
function DeltaGlider:DefineAnimations()
	-- ***** Rudder animation *****
	local RRudderGrp = {GRP.RRudder1,GRP.RRudder2}
	local RRudder = MGROUP_ROTATE(0, RRudderGrp, _V( 8.668,0.958,-6.204), _V( 0.143,0.975,-0.172), -60*RAD)
	local LRudderGrp = {GRP.LRudder1,GRP.LRudder2}
	local LRudder = MGROUP_ROTATE(0, LRudderGrp, _V(-8.668,0.958,-6.204), _V(-0.143,0.975,-0.172), -60*RAD)
	self.anim_rudder = self:create_animation (0.5)
	self:add_animationcomponent (self.anim_rudder, 0, 1, RRudder)
	self:add_animationcomponent (self.anim_rudder, 0, 1, LRudder)

	-- ***** Elevator animation *****
	local ElevatorGrp = {GRP.LUAileron1,GRP.LUAileron2,GRP.LLAileron1,GRP.LLAileron2,GRP.RUAileron1,GRP.RUAileron2,GRP.RLAileron1,GRP.RLAileron2}
	local Elevator = MGROUP_ROTATE(0, ElevatorGrp, _V(0,-0.4,-6.0), _V(1,0,0), 40*RAD)
	self.anim_elevator = self:create_animation (0.5)
	self:add_animationcomponent (self.anim_elevator, 0, 1, Elevator)

	-- ***** Elevator trim animation *****
	local ElevatorTrim = MGROUP_ROTATE(0, ElevatorGrp, _V(0,-0.4,-6.0), _V(1,0,0), 10*RAD)
	self.anim_elevatortrim = self:create_animation (0.5)
	self:add_animationcomponent (self.anim_elevatortrim, 0, 1, ElevatorTrim)

	-- ***** Aileron animation *****
	local LAileronGrp = {GRP.LUAileron1,GRP.LUAileron2,GRP.LLAileron1,GRP.LLAileron2}
	local LAileron = MGROUP_ROTATE (0, LAileronGrp, _V(0,-0.4,-6.0), _V(1,0,0), -20*RAD)
	self.anim_laileron = self:create_animation (0.5)
	self:add_animationcomponent (self.anim_laileron, 0, 1, LAileron)

	local RAileronGrp = {GRP.RUAileron1,GRP.RUAileron2,GRP.RLAileron1,GRP.RLAileron2}
	local RAileron = MGROUP_ROTATE(0, RAileronGrp, _V(0,-0.4,-6.0), _V(1,0,0), 20*RAD)
	self.anim_raileron = self:create_animation (0.5)
	self:add_animationcomponent (self.anim_raileron, 0, 1, RAileron)
end


-- --------------------------------------------------------------
-- Apply custom skin to the current mesh instance
-- --------------------------------------------------------------
function DeltaGlider:ApplySkin ()
	if not self.exmesh then return end
	if self.skin[1] then oapi.set_texture (self.exmesh, 2, self.skin[1]) end
	if self.skin[2] then oapi.set_texture (self.exmesh, 3, self.skin[2]) end
	oapi.set_texture (self.exmesh, 5, self.insignia_tex)

	local panelmat = {
		{diffuse=_COLOUR4(0.5, 0.6, 0.6, 1), ambient=_COLOUR4(0.5, 0.6, 0.6, 1), specular=_COLOUR4(0.2,0.2,0.2,1), emissive=_COLOUR4(0.15,0.15,0.15,1), power=10},-- default
		{diffuse=_COLOUR4(0.6, 0.55,0.45,1), ambient=_COLOUR4(0.6, 0.55,0.45,1), specular=_COLOUR4(0.1,0.1,0.1,1), emissive=_COLOUR4(0.15,0.15,0.15,1), power=5}, -- brown
		{diffuse=_COLOUR4(0.2, 0.2, 0.15,1), ambient=_COLOUR4(0.5, 0.5, 0.5, 1), specular=_COLOUR4(0.6,0.6,0.6,1), emissive=_COLOUR4(0.15,0.15,0.15,1), power=20},-- shiny anthrazit
		{diffuse=_COLOUR4(0.75,0.75,0.65,1), ambient=_COLOUR4(0.75,0.75,0.65,1), specular=_COLOUR4(0.3,0.3,0.3,1), emissive=_COLOUR4(0.15,0.15,0.15,1), power=20},-- shiny anthrazit
		{diffuse=_COLOUR4(0.5, 0.1, 0.1, 1), ambient=_COLOUR4(0.5, 0.1, 0.1, 1), specular=_COLOUR4(0.2,0.2,0.2,1), emissive=_COLOUR4(0.15,0.15,0.15,1), power=10} -- DG-red
	}
	if self.panelcol > 0 and self.panelcol <= 4 then
		oapi.set_material(self.vcmesh, 14, panelmat[self.panelcol+1])
	end
--	time_t lt; time(&lt); struct tm *st = localtime(&lt);
--	if (vcmesh && st->tm_mon==3 && st->tm_mday==1) {
--		SURFHANDLE t = oapi.load_texture ("generic/noisep.dds");
--		if (t) oapi.set_texture (vcmesh, 17, t);
--	}
end

-- --------------------------------------------------------------
-- Paint individual vessel markings
-- --------------------------------------------------------------
function DeltaGlider:PaintMarkings(tex)
	local skp = oapi.get_sketchpad(tex)
	if skp then
		local font1 = oapi.create_font(38, true, "Sans", FONT.BOLD)
		skp:set_font (font1)
		skp:set_textcolor (0xD0D0D0)
		skp:set_textalign (SKP.CENTER)
		local name = self:get_name():sub(1,10)
		skp:text(193, 10, name)
		skp:text(193, 74, name)
		oapi.release_font(font1)

		local font2 = oapi.create_font(36, true, "Sans", FONT.BOLD)
		skp:set_font(font2)
		skp:set_textcolor (0x808080)
		skp:set_textalign (SKP.RIGHT)
		skp:text (125, 156, name)
		skp:set_textalign (SKP.LEFT)
		skp:text (131, 156, name)
		oapi.release_sketchpad (skp)
		oapi.release_font(font2)
	end
end


-- --------------------------------------------------------------
-- Load panel animation bitmaps and initialise panel state
-- --------------------------------------------------------------
function DeltaGlider:InitPanel(panel)
	ComponentVessel.clbkReset2D(self, panel, self.hPanelMesh)

	if panel == 0 then -- main panel
		self.mainflowidx = {-1, -1}
		self.retroflowidx = {-1, -1}
		self.scTSFCidx = {-1, -1}
		self.scflowidx = {-1, -1}
		self.hoverflowidx = -1
		self.mainTSFCidx = -1
	end
end

-- --------------------------------------------------------------
-- Load VC animation bitmaps and initialise VC state
-- --------------------------------------------------------------
function DeltaGlider:InitVC(vc)
	if vc == 0 then
		-- reset state flags for panel instruments
		self.mainflowidx = {-1, -1}
		self.retroflowidx = {-1, -1}
		self.scTSFCidx = {-1, -1}
		self.scflowidx = {-1, -1}
		self.hoverflowidx = -1
		self.mainTSFCidx = -1
	end
end

function DeltaGlider:clbkDrawHUD(mode, hps, skp)
	if oapi.cockpit_mode() ~= COCKPIT.VIRTUAL then return false end

	-- Default HUD elements and subsystem HUD drawing
	ComponentVessel.clbkDrawHUD(self, mode, hps, skp)

	local cx = hps.CX
	local cy = hps.CY

	-- show RCS mode
	local rcsmode = self:get_rcsmode()
	if rcsmode == RCSMODE.ROT then
		skp:text (0, hps.H-20, "RCS ROT", 7)
	elseif rcsmode == RCSMODE.LIN then
		skp:text (0, hps.H-20, "RCS_LIN", 7)
	end
	-- show airbrake status
	if not self.ssys_aerodyn:AirbrakeState():IsClosed() then
		if not self.ssys_aerodyn:AirbrakeState():IsActive() then
			local int, frac = math.modf(oapi.get_simtime())
			if frac < 0.5 then
				skp:set_textalign(SKP.CENTER, SKP.BOTTOM)
				skp:text(cx, cy - hps.Markersize/2, "AIRBRK", 6)
			end
		end
	end
	if oapi.get_hudmode() == HUD.DOCKING then
		if not self.ssys_docking:NconeState():IsOpen() then
			local d = hps.Markersize*5
			local int, frac = math.modf(oapi.get_simtime())
			if self.ssys_docking:NconeState():IsClosed() or frac < 0.5 then
				skp:line (cx-d,cy-d,cx+d,cy+d)
				skp:line (cx-d,cy+d,cx+d,cy-d)
			end
			local str = "NOSECONE"
			local w = skp:get_textwidth (str)
			skp:text (cx-w/2, cy-d, str, 8)
		end
	end
	return true
end

local vgear = oapi.create_ntvertexarray(12)
local vnose = oapi.create_ntvertexarray(16)
local vbrk  = oapi.create_ntvertexarray(4)
local scl = 0

local igear = oapi.create_indexarray({
	0,3,1, 3,0,2,
	4,7,5, 7,4,6,
	8,11,9, 11,8,10
})
local inose = oapi.create_indexarray({
	0,1,2, 2,3,0,
	0,6,1, 6,7,1,
	1,8,2, 8,9,2,
	2,10,3, 10,11,3,
	3,4,0, 0,4,5,
	12,15,13, 12,14,15
})
local ibrk = oapi.create_indexarray({
	0,3,1, 0,2,3
})

function DeltaGlider:clbkRenderHUD(mode, hps, hTex)
	ComponentVessel.clbkRenderHUD (self, mode, hps, hTex)

	local texw = 512.0
	local texh = 256.0
	local cx = hps.CX
	local cy = hps.CY

	local vtx = oapi.create_ntvertexarray(12+16+4)
	local idx = oapi.create_indexarray(18+36+6)

	if scl ~= hps.Markersize*0.25 then -- resize
		scl = hps.Markersize*0.25
		vgear:zeroize()
		local x = {-4,-2,-4,-2,2,4,2,4,-1,1,-1,1}
		local y = {-2,-2,-4,-4,-2,-2,-4,-4,-6,-6,-8,-8}
		for i=1,12 do
			vgear[i].x = cx + x[i]*scl
			vgear[i].y = cy + y[i]*scl
			vgear[i].tu = (405.0 + (18.0 * ((i-1)%2)))/texw
			vgear[i].tv = (104.0 - (18.0 * math.floor(((i-1)%4)/2)))/texh
		end
		vnose:zeroize()
		local xn = {0,1,0,-1,-31,-30,30,31,31,30,-30,-31, -13,13,-13,13}
		local yn = {-1,0,1,0,-30,-31,-31,-30,30,31,31,30, -25,-25,-28.9,-28.9}
		local un = {392.5, 397.0, 392.5, 388.0, 388.0, 392.5, 392.5, 397.0, 397.0, 392.5, 392.5, 388.0, 124.0, 204.0, 124.0, 204.0}
		local vn = {92.0, 96.5, 101.0, 96.5, 96.5, 92.0, 92.0, 96.5, 96.5, 101.0, 101.0, 96.5, 118.0, 118.0, 106.0, 106.0}
		for i=1,16 do
			vnose[i].x = cx + xn[i]*scl*0.4
			vnose[i].y = cy + yn[i]*scl*0.4
			vnose[i].tu = un[i]/texw
			vnose[i].tv = vn[i]/texh
		end
		vbrk:zeroize()
		local xb = {-9.1, 9.1, -9.1, 9.1}
		local yb = {-30.0, -30.0, -33.9, -33.9}
		local ub = {205.0, 261.0, 205.0, 261.0}
		local vb = {118.0, 118.0, 106.0, 106.0}
		for i=1,4 do
			vbrk[i].x = cx + xb[i]*scl*0.4
			vbrk[i].y = cy + yb[i]*scl*0.4
			vbrk[i].tu = ub[i]/texw
			vbrk[i].tv = vb[i]/texh
		end
	end

	local int, frac = math.modf(oapi.get_simtime())
	local blink = frac < 0.5

	vtx:reset()
	idx:reset()

	-- show gear deployment status
	if self.ssys_gear:GearState():IsOpen() or (not self.ssys_gear:GearState():IsClosed() and blink) then
		idx:append(igear)
		vtx:append(vgear)
	end

	-- show nosecone status
	if oapi.get_hudmode() == HUD.DOCKING and not self.ssys_docking:NconeState():IsOpen() then
		if self.ssys_docking:NconeState():IsClosed() or blink then
			idx:append(inose, #vtx)
			vtx:append(vnose)
		end
	end

	-- show airbrake status
	if not self.ssys_aerodyn:AirbrakeState():IsClosed() then
		if not self.ssys_aerodyn:AirbrakeState():IsActive() or blink then
			idx:append(ibrk, #vtx)
			vtx:append(vbrk)
		end
	end
	if #vtx > 0 then
		local grp = {}
		grp.Vtx = vtx
		grp.Idx = idx

		local hmesh = oapi.create_mesh({grp})
		oapi.render_hud (hmesh, {hTex})
		oapi.delete_mesh (hmesh)
	end
end


function DeltaGlider:SetGearParameters (state)
	if state == 1.0 then
		if not self.bGearIsDown then
			self:set_touchdownpoints (tdvtx_geardown)
			self:set_nosewheelsteering (true)
			self.bGearIsDown = true
		end
	elseif self.bGearIsDown then
		self:set_touchdownpoints (tdvtx_gearup)
		self:set_nosewheelsteering (false)
		self.bGearIsDown = false
	end
end

function DeltaGlider:SetMainRetroLevel (which, lmain, lretro)
	if which == 3 then   -- set main thruster group
		self:set_thrustergrouplevel (THGROUP.MAIN, lmain)
		self:set_thrustergrouplevel (THGROUP.RETRO, lretro)
	else            -- set individual engine
		self:set_thrusterlevel (self.th_main [which], lmain)
		self:set_thrusterlevel (self.th_retro[which], lretro)
	end
end

function DeltaGlider:EnableRetroThrusters (state)
	self:set_thrusterresource (self.th_retro[1], state and self.ph_main or nil)
	self:set_thrusterresource (self.th_retro[2], state and self.ph_main or nil)
end

function DeltaGlider:GetMaxHoverThrust ()
	local th0 = 0.0
	th0 = th0 + self:get_thrustermax (self.th_hover[1])
	th0 = th0 + self:get_thrustermax (self.th_hover[2])
	return th0
end

function DeltaGlider:TestDamage ()
	local newdamage = false
	local dt = oapi.get_simstep()

	-- airframe damage as a result of wingload stress
	-- or excessive dynamic pressure

	local load = self:get_lift() / 190.0 -- L/S
	local dynp = self:get_dynpressure()  -- dynamic pressure
	if load > WINGLOAD_MAX or load < WINGLOAD_MIN or dynp > DYNP_MAX then
		local alpha = math.max ((dynp-DYNP_MAX) * 1e-5,
			(load > 0 and load-WINGLOAD_MAX or WINGLOAD_MIN-load) * 5e-5)
		local p = 1.0 - math.exp (-alpha*dt) -- probability of failure
		if oapi.rand() < p then
			-- simulate structural failure by distorting the airfoil definition
			local rfail = oapi.rand() * 4.0
			if rfail < 1 then -- fail left wing
				self.lwingstatus = self.lwingstatus * math.exp (-alpha*oapi.rand())
			elseif rfail < 2 then -- fail right wing
				self.rwingstatus = self.rwingstatus * math.exp (-alpha*oapi.rand())
			elseif rfail < 3 then -- fail left aileron
				if self.hlaileron then
					self:del_controlsurface (self.hlaileron)
					self.hlaileron = nil
				end
				self.aileronfail[api.rand()<0.5 and 0 or 1] = true
			else -- fail right aileron
				if self.hraileron then
					self:del_controlsurface (self.hraileron)
					self.hraileron = nil
				end
				self.aileronfail[api.rand()<0.5 and 2 or 3] = true
			end
			newdamage = true
		end
	end

	if newdamage then
		self.ssys_failure:MWSActivate()
		self:ApplyDamage ();
	end
end

function DeltaGlider:ApplyDamage ()
	local balance = (self.rwingstatus-self.lwingstatus)*3.0
	local surf    = (self.rwingstatus+self.lwingstatus)*35.0 + 20.0
	self:edit_airfoil (self.hwing, 0x09, _V(balance,0,-0.3), 0, 0, surf, 0)

	if self.rwingstatus < 1 or self.lwingstatus < 1 then
		self.ssys_failure:MWSActivate()
	end
	self:SetDamageVisuals()
end

function DeltaGlider:RepairDamage ()
	self.lwingstatus = 1.0
	self.rwingstatus = 1.0

	self:edit_airfoil (self.hwing, 0x09, _V(0,0,-0.3), 0, 0, 90.0, 0)
	if not self.hlaileron then
		self.hlaileron = self:create_controlsurface (AIRCTRL.AILERON, 0.3, 1.5, _V( 7.5,0,-7.2), AIRCTRL_AXIS.XPOS, 1, self.anim_raileron)
	end
	if not self.hraileron then
		self.hraileron = self:create_controlsurface (AIRCTRL.AILERON, 0.3, 1.5, _V(-7.5,0,-7.2), AIRCTRL_AXIS.XNEG, 1, self.anim_laileron)
	end

	self.aileronfail = {false, false, false, false}
	self.ssys_pressurectrl:RepairDamage ()
	self.ssys_failure:MWSReset ()
	self:SetDamageVisuals()
end

function DeltaGlider:RedrawPanel_ScramFlow (surf)
	local redraw = false
	for i = 0, 1 do
		local p = math.min (66, math.floor(self.ssys_scram:DMF(i)/3.0*67.0))
		if p ~= self.scflowidx[i+1] then
			self.scflowidx[i+1] = p
			redraw = true
		end
	end
	if redraw then
		oapi.blt_panelareabackground (AID_SCRAMDISP2, surf)
		return true
	end
	return false
end

function DeltaGlider:RedrawPanel_ScramTempDisp (surf)
	local rad = 19.0
	local isVC = oapi.cockpit_mode() == COCKPIT.VIRTUAL

	local skp = oapi.get_sketchpad(surf)
	skp:set_pen(g_Param.pen[0]);
	for j = 0, 2 do
		for i = 0, 1 do
			local T = self.ssys_scram:Temp (i, j)
			local phi = PI * math.min (T,3900.0)/2000.0
			local dx = rad*math.sin(phi)
			local dy = rad*math.cos(phi)
			local x0 = (isVC and 20 or (22-j)) + i*43
			local y0 = 19+j*46
			skp:moveto (x0, y0)
			skp:lineto (x0+dx, y0-dy)
		end
	end
	oapi.release_sketchpad (skp)
	return true
end

-- D. Beachy: begin refactored section to fix flow rate panels
function DeltaGlider:RedrawPanel_MainFlow (surf)
	local gaugeSize = 66.99  -- pointer can move 66 pixels; also round up to next pixel
	local redraw = false
	for i = 1, 2 do
		local flowRate = self:GetThrusterFlowRate(self.th_main[i])
        local p = math.floor(math.min(flowRate*gaugeSize/5.1, gaugeSize))  -- gauge maxes out at 5.1
		if p ~= self.mainflowidx[i] then
			self.mainflowidx[i] = p
			redraw = true
		end
	end
	if redraw then
		oapi.blt_panelareabackground (AID_MAINDISP1, surf)
		return true
	end
	return false
end

function  DeltaGlider:RedrawPanel_RetroFlow (surf)
    local gaugeSize = 66.99  -- pointer can move 66 pixels; also round up to next pixel
	local redraw = false
	for i = 1, 2 do
		local flowRate = self:GetThrusterFlowRate(self.th_retro[i]) 
		local p = math.floor(math.min(flowRate*gaugeSize/0.9,gaugeSize)) -- gauge maxes out at 0.9
		if p ~= self.retroflowidx[i] then
			self.retroflowidx[i] = p
			redraw = true
		end
	end
	if redraw then
		oapi.blt_panelareabackground (AID_MAINDISP2, surf)
		return true
	end
	return false
end

function DeltaGlider:RedrawPanel_HoverFlow (surf)
    local gaugeSize = 66.99  -- pointer can move 66 pixels; also round up to next pixel
    -- since hover flow rates are always locked we can assume the second hover thruster has the same flow as the first
    local flowRate = self:GetThrusterFlowRate(self.th_hover[1]) 
	local p = math.floor(math.min(flowRate*gaugeSize/3.6,gaugeSize)) -- gauge maxes out at 3.6
	if p ~= self.hoverflowidx then
		self.hoverflowidx = p
		oapi.blt_panelareabackground (AID_MAINDISP3, surf)
		return true
	end
	return false
end

function DeltaGlider:GetThrusterFlowRate(th)
    local level  = self:get_thrusterlevel(th) -- throttle level
    local isp    = self:get_thrusterisp0(th)
    local thrust = self:get_thrustermax0(th)
    local flow   = thrust*level/isp
    
    return flow
end
-- D. Beachy: end refactored section

-- return texture coordinate depending on the animation state
local function BlinkStateCoord(anim)
	local xon = 0.845
	local xoff = 0.998
	if anim:IsClosed() then return xon end
	if anim:IsOpen() then return xoff end
	local int, frac = math.modf(oapi.get_simtime())
	if frac < 0.5 then
		return xon
	else
		return xoff
	end
end

function DeltaGlider:UpdateStatusIndicators ()
	if not self.vcmesh then return end
	local x

	local vtx = oapi.create_ntvertexarray(16)
	local vidx = oapi.create_indexarray({0,1,4,5,20,21,8,9,24,25,16,17,12,13,28,29})
	local ges = {}
	ges.flags = GRPEDIT.VTXTEXU
	ges.vIdx = vidx
	ges.Vtx = vtx

	-- gear indicator
	x = BlinkStateCoord(self.ssys_gear:GearState())
	vtx[1].tu = x
	vtx[2].tu = x

	-- retro cover indicator
	x = BlinkStateCoord(self.ssys_mainretro:RetroCoverState())
	vtx[3].tu = x
	vtx[4].tu = x

	-- airbrake indicator
	x = BlinkStateCoord(self.ssys_aerodyn:AirbrakeState())
	vtx[5].tu = x
	vtx[6].tu = x

	-- nose cone indicator
	x = BlinkStateCoord(self.ssys_docking:NconeState())
	vtx[7].tu = x
	vtx[8].tu = x

	-- top hatch indicator
	x = BlinkStateCoord(self.ssys_pressurectrl:HatchState())
	vtx[9].tu = x
	vtx[10].tu = x

	-- radiator indicator
	x = BlinkStateCoord(self.ssys_thermal:RadiatorState())
	vtx[11].tu = x
	vtx[12].tu = x

	-- outer airlock indicator
	x = BlinkStateCoord(self.ssys_pressurectrl:OLockState())
	vtx[13].tu = x
	vtx[14].tu = x

	-- inner airlock indicator
	x = BlinkStateCoord(self.ssys_pressurectrl:ILockState())
	vtx[15].tu = x
	vtx[16].tu = x

	oapi.edit_meshgroup (self.vcmesh, GRP_VC.STATUS_INDICATOR, ges)
end

function DeltaGlider:SetPassengerVisuals ()
	if not self.vcmesh or not self.exmesh then return end
	local expsngridx = {GRP.Psngr1, GRP.Psngr2, GRP.Psngr3, GRP.Psngr4}
	local exvisoridx = {GRP.Visor1, GRP.Visor2, GRP.Visor3, GRP.Visor4}
	local vcpsngridx = {GRP_VC.PASSENGER1, GRP_VC.PASSENGER2, GRP_VC.PASSENGER3, GRP_VC.PASSENGER4}
	local vcvisoridx = {GRP_VC.PASSENGER1_VISOR, GRP_VC.PASSENGER2_VISOR, GRP_VC.PASSENGER3_VISOR, GRP_VC.PASSENGER4_VISOR}

	for i=1,4 do
		local ges = {}
		if self.psngr[i] then
			ges.flags = GRPEDIT.SETUSERFLAG
			ges.UsrFlag = 1
		else
			ges.flags = GRPEDIT.ADDUSERFLAG
			ges.UsrFlag = 3
		end
		oapi.edit_meshgroup (self.exmesh, expsngridx[i], ges)
		oapi.edit_meshgroup (self.exmesh, exvisoridx[i], ges)
		oapi.edit_meshgroup (self.vcmesh, vcpsngridx[i], ges)
		oapi.edit_meshgroup (self.vcmesh, vcvisoridx[i], ges)
	end
end

local AileronGrp = {GRP.RUAileron1,GRP.LUAileron1,GRP.LUAileron2,GRP.RUAileron2,GRP.LLAileron1,GRP.RLAileron1,GRP.LLAileron2,GRP.RLAileron2}

function DeltaGlider:SetDamageVisuals ()
	if not self.exmesh then return end
	-- ailerons
	for i=0,3 do
		for j=1,2 do
			local ges = {}
			if self.aileronfail[i+1] then
				ges.flags = GRPEDIT.ADDUSERFLAG
				ges.UsrFlag = 3
				oapi.edit_meshgroup (self.exmesh, AileronGrp[i*2+j], ges)
			else
				ges.flags = GRPEDIT.SETUSERFLAG
				ges.UsrFlag = 0
				oapi.edit_meshgroup (self.exmesh, AileronGrp[i*2+j], ges)
			end
		end
	end
end

function DeltaGlider:InitVCMesh()
	if self.vcmesh then
		-- hide pilot head in VCPILOT position
		local ges = {}
		ges.flags = self.campos < CAM_VCPSNGR1 and GRPEDIT.ADDUSERFLAG or GRPEDIT.DELUSERFLAG
		ges.UsrFlag = 3
		oapi.edit_meshgroup (self.vcmesh, GRP_VC.PILOT_HEAD, ges)
		oapi.edit_meshgroup (self.vcmesh, GRP_VC.PILOT_VISOR, ges)

		--ComponentVessel::clbkResetVC (0, vcmesh);
	end
end


-- ==============================================================
-- Overloaded callback functions
-- ==============================================================

-- --------------------------------------------------------------
-- Set vessel class parameters
-- --------------------------------------------------------------
function DeltaGlider:clbkSetClassCaps (cfg)
--	oapi.write_log("DeltaGlider:clbkSetClassCaps")
	-- *************** physical parameters **********************

--	if (oapiReadItem_bool (cfg, (char*)"SCRAMJET", b) && b) -- set up scramjet configuration
--		AddSubsystem (ssys_scram = new ScramSubsystem (this));

	ComponentVessel.set_emptymass (self, self.ssys_scram and EMPTY_MASS_SC or EMPTY_MASS)
	local r = {_V(0,0,6), _V(0,0,-4)}
	self:set_size (10.0)
	self:set_visibilitylimit (7.5e-4, 1.5e-3)
	self:set_albedoRGB (_V(0.77,0.20,0.13))
	self:set_gravitygradientdamping (20.0)
	self:set_cw ({z=0.09, zn=0.09, x=2, y=1.4})
	
	self:set_wingaspect (0.7)
	self:set_wingeffectiveness (2.5)
	self:set_crosssections (_V(53.0,186.9,25.9))
	self:set_maxwheelbrakeforce (2e5)
	self:set_pmi (_V(15.5,22.1,7.7))

	self:set_dockparams (_V(0,-0.49,10.076), _V(0,0,1), _V(0,1,0))
	self:set_touchdownpoints (tdvtx_geardown)
	self:set_nosewheelsteering (true)
	self.bGearIsDown = true
	self:enable_transponder (true)
	local render_cockpit = true

	-- ******************** NAV radios **************************

	self:init_navradios (4)

	-- ****************** propellant specs **********************

	self.tankconfig = self.ssys_scram and 1 or 0
	if self.tankconfig == 0 then
		self.max_rocketfuel = TANK1_CAPACITY + TANK2_CAPACITY
	else
		self.max_rocketfuel = TANK1_CAPACITY
	end

	self.ph_main  = self:create_propellantresource (self.max_rocketfuel)    -- main tank (fuel + oxydant)
	self.ph_rcs   = self:create_propellantresource (RCS_FUEL_CAPACITY) -- RCS tank  (fuel + oxydant)
	self:set_default_propellantresource (self.ph_main)

	-- **************** thruster definitions ********************

	local ispscale = self.modelidx == 2 and 0.8 or 1.0
	-- Reduction of thrust efficiency at normal pressure

	self.contrail_tex = oapi.register_particletexture ("Contrail1a")

	local contrail = {
		flags = 0,
		srcsize = 8.0,
		srcrate = 4,
		v0 = 150.0,
		srcspread = 0.25,
		lifetime = 3.0,
		growthrate = 4,
		atmslowdown = 2.0,
		ltype = PARTICLE.DIFFUSE,
		levelmap = PARTICLE.LVL_PSQRT,
		lmin = 0,
		lmax = 2,
		atmsmap = PARTICLE.ATM_PLOG,
		amin = 1e-4,
		amax = 1,
		tex = self.contrail_tex
	}

	local exhaust_main = {
		flags = 0,
		srcsize = 2.0,
		srcrate = 13,
		v0 = 150.0,
		srcspread = 0.1,
		lifetime = 0.2,
		growthrate = 16,
		atmslowdown = 1.0,
		ltype = PARTICLE.EMISSIVE,
		levelmap = PARTICLE.LVL_PSQRT,
		lmin = 0,
		lmax = 1,
		atmsmap = PARTICLE.ATM_PLOG,
		amin = 1e-5,
		amax = 0.1,
		tex = nil
	}

	local exhaust_hover = {
		flags = 0,
		srcsize = 1.5,
		srcrate = 30,
		v0 = 150.0,
		srcspread = 0.1,
		lifetime = 0.1,
		growthrate = 12,
		atmslowdown = 1.0,
		ltype = PARTICLE.EMISSIVE,
		levelmap = PARTICLE.LVL_PSQRT,
		lmin = 0,
		lmax = 1,
		atmsmap = PARTICLE.ATM_PLOG,
		amin = 1e-5,
		amax = 0.1,
		tex = nil
	}

	-- main thrusters
	self.th_main = {}
	self.th_main[1] = self:create_thruster ({pos=_V(-1,0.0,-7.7), dir=_V(0,0,1), maxth0=MAX_MAIN_THRUST[self.modelidx], hprop=self.ph_main, isp0=ISP, ispr=ISP*ispscale})
	self.th_main[2] = self:create_thruster ({pos=_V( 1,0.0,-7.7), dir=_V(0,0,1), maxth0=MAX_MAIN_THRUST[self.modelidx], hprop=self.ph_main, isp0=ISP, ispr=ISP*ispscale})
	self.thg_main = self:create_thrustergroup (self.th_main, THGROUP.MAIN)

	self:add_exhaust(self.th_main[1], 12, 1)
	self:add_exhaust(self.th_main[2], 12, 1)

	self:add_exhauststream (self.th_main[1], _V(-1,0,-15), contrail)
	self:add_exhauststream (self.th_main[2], _V( 1,0,-15), contrail)
	self:add_exhauststream (self.th_main[1], _V(-1,0,-10), exhaust_main)
	self:add_exhauststream (self.th_main[2], _V( 1,0,-10), exhaust_main)

	-- retro thrusters
	-- note that we have to tilt retros slightly downwards to avoid inducing
	-- an angular momentum, since they are mounted below the level of CG.
	-- This also means that retros will induce an upward linear component.

	self.th_retro = {}
	self.th_retro[1] = self:create_thruster ({pos=_V(-3,-0.236,5.6), dir=_V(0,0.04210548,-0.99911317), maxth0=MAX_RETRO_THRUST, hprop=self.ph_main, isp0=ISP, ispr=ISP*ispscale})
	self.th_retro[2] = self:create_thruster ({pos=_V( 3,-0.236,5.6), dir=_V(0,0.04210548,-0.99911317), maxth0=MAX_RETRO_THRUST, hprop=self.ph_main, isp0=ISP, ispr=ISP*ispscale})
	self.thg_retro = self:create_thrustergroup (self.th_retro, THGROUP.RETRO)

	self:add_exhaust(self.th_retro[1], 3, 0.4)
	self:add_exhaust(self.th_retro[2], 3, 0.4)

	-- hover thrusters

	self.th_hover = {}
	self.th_hover[1] = self:create_thruster ({pos=_V(0,0,3), dir=_V(0,1,0), maxth0=MAX_HOVER_THRUST[self.modelidx], hprop=self.ph_main, isp0=ISP, ispr=ISP*ispscale})
	self.th_hover[2] = self:create_thruster ({pos=_V(-3,0,-4.55), dir=_V(0,1,0), maxth0=3.0/4.55*0.5*MAX_HOVER_THRUST[self.modelidx], hprop=self.ph_main, isp0=ISP, ispr=ISP*ispscale})
	self.th_hover[3] = self:create_thruster ({pos=_V( 3,0,-4.55), dir=_V(0,1,0), maxth0=3.0/4.55*0.5*MAX_HOVER_THRUST[self.modelidx], hprop=self.ph_main, isp0=ISP, ispr=ISP*ispscale})
	self.thg_hover = self:create_thrustergroup (self.th_hover, THGROUP.HOVER)
	local hoverp0 = _V(0,-1.5, 3)
	local hoverp1 = _V(-3,-1.3,-4.55)
	local hoverp2 = _V(3,-1.3,-4.55)
	local hoverd = _V(0,-1,0)

	self:add_exhaust(self.th_hover[1], 6, 0.5, hoverp0, hoverd)
	self:add_exhaust(self.th_hover[2], 6, 0.5, hoverp1, hoverd)
	self:add_exhaust(self.th_hover[3], 6, 0.5, hoverp2, hoverd)


	self:add_exhauststream (self.th_hover[1], _V(0,-4,0), contrail)
	self:add_exhauststream (self.th_hover[1], _V(0,-2,3), exhaust_hover)
	self:add_exhauststream (self.th_hover[2], _V(-3,-2,-4.55), exhaust_hover)
	self:add_exhauststream (self.th_hover[3], _V( 3,-2,-4.55), exhaust_hover)

	-- set of attitude thrusters (idealised). The arrangement is such that no angular
	-- momentum is created in linear mode, and no linear momentum is created in rotational mode.
	local th_att_rot = {}
	local th_att_lin = {}
	th_att_lin[0] = self:create_thruster ({pos=_V(0,0, 8), dir=_V(0, 1,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_lin[3] = self:create_thruster ({pos=_V(0,0,-8), dir=_V(0,-1,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_lin[2] = self:create_thruster ({pos=_V(0,0, 8), dir=_V(0,-1,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_lin[1] = self:create_thruster ({pos=_V(0,0,-8), dir=_V(0, 1,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})

	th_att_rot[0] = th_att_lin[0]
	th_att_rot[1] = th_att_lin[3]
	th_att_rot[2] = th_att_lin[2]
	th_att_rot[3] = th_att_lin[1]

	self:create_thrustergroup ({th_att_rot[0], th_att_rot[1]}, THGROUP.ATT_PITCHUP)
	self:create_thrustergroup ({th_att_rot[2], th_att_rot[3]}, THGROUP.ATT_PITCHDOWN)
	self:create_thrustergroup ({th_att_lin[0], th_att_lin[1]}, THGROUP.ATT_UP)
	self:create_thrustergroup ({th_att_lin[2], th_att_lin[3]}, THGROUP.ATT_DOWN)
	self:add_exhaust (th_att_rot[0], 0.6,  0.078, _V( -0.816081, -0.616431, 9.594813 ), _V(0,-1,0))
	self:add_exhaust (th_att_rot[0], 0.6,  0.078, _V( 0.816081, -0.616431, 9.594813 ), _V(0,-1,0))
	self:add_exhaust (th_att_rot[1], 0.79, 0.103, _V( -0.120063, 0.409999, -7.357354 ), _V(0, 1,0))
	self:add_exhaust (th_att_rot[1], 0.79, 0.103, _V( 0.120063, 0.409999, -7.357354 ), _V(0, 1,0))
	self:add_exhaust (th_att_rot[2], 0.6,  0.078, _V( -0.816081, -0.35857, 9.594813 ), _V(0, 1,0))
	self:add_exhaust (th_att_rot[2], 0.6,  0.078, _V( 0.816081, -0.35857, 9.594813 ), _V(0, 1,0))
	self:add_exhaust (th_att_rot[3], 0.79, 0.103, _V( -0.120063, -0.409999, -7.357354 ), _V(0,-1,0))
	self:add_exhaust (th_att_rot[3], 0.79, 0.103, _V( 0.120063, -0.409999, -7.357354 ), _V(0,-1,0))

	th_att_lin[0] = self:create_thruster ({pos=_V(0,0, 6), dir=_V(-1,0,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_lin[3] = self:create_thruster ({pos=_V(0,0,-6), dir=_V( 1,0,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_lin[2] = self:create_thruster ({pos=_V(0,0, 6), dir=_V( 1,0,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_lin[1] = self:create_thruster ({pos=_V(0,0,-6), dir=_V(-1,0,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_rot[0] = th_att_lin[0]
	th_att_rot[1] = th_att_lin[3]
	th_att_rot[2] = th_att_lin[2]
	th_att_rot[3] = th_att_lin[1]
	self:create_thrustergroup ({th_att_rot[0], th_att_rot[1]}, THGROUP.ATT_YAWLEFT)
	self:create_thrustergroup ({th_att_rot[2], th_att_rot[3]}, THGROUP.ATT_YAWRIGHT)
	self:create_thrustergroup ({th_att_lin[0], th_att_lin[1]}, THGROUP.ATT_LEFT)
	self:create_thrustergroup ({th_att_lin[2], th_att_lin[3]}, THGROUP.ATT_RIGHT)
	self:add_exhaust (th_att_rot[0], 0.6,  0.078, _V( 0.888971, -0.488177, 9.3408 ), _V(1,0,0))
	self:add_exhaust (th_att_rot[1], 0.94, 0.122, _V( -2.029295, 0.182903, -6.043046 ), _V(-1,0,0))
	self:add_exhaust (th_att_rot[2], 0.6,  0.078, _V( -0.888971, -0.488177, 9.3408 ), _V(-1,0,0))
	self:add_exhaust (th_att_rot[3], 0.94, 0.122, _V( 2.029295, 0.182903, -6.043046 ), _V(1,0,0))

	th_att_rot[0] = self:create_thruster ({pos=_V( 6,0,0), dir=_V(0, 1,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_rot[1] = self:create_thruster ({pos=_V(-6,0,0), dir=_V(0,-1,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_rot[2] = self:create_thruster ({pos=_V(-6,0,0), dir=_V(0, 1,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_rot[3] = self:create_thruster ({pos=_V( 6,0,0), dir=_V(0,-1,0), maxth0=MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	self:create_thrustergroup ({th_att_rot[0], th_att_rot[1]}, THGROUP.ATT_BANKLEFT)
	self:create_thrustergroup ({th_att_rot[2], th_att_rot[3]}, THGROUP.ATT_BANKRIGHT)
	self:add_exhaust (th_att_rot[0], 1.03, 0.134, _V( -5.121185, -0.073903, 0.375386 ), _V(0, 1,0))
	self:add_exhaust (th_att_rot[1], 1.03, 0.134, _V( 5.121185, -0.654322, 0.375386 ), _V(0,-1,0))
	self:add_exhaust (th_att_rot[2], 1.03, 0.134, _V( 5.121185, -0.073903, 0.375386 ), _V(0, 1,0))
	self:add_exhaust (th_att_rot[3], 1.03, 0.134, _V( -5.121185, -0.654322, 0.375386 ), _V(0,-1,0))

	th_att_lin[0] = self:create_thruster ({pos=_V(0,0,-7), dir=_V(0,0, 1), maxth0=2*MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	th_att_lin[1] = self:create_thruster ({pos=_V(0,0, 7), dir=_V(0,0,-1), maxth0=2*MAX_RCS_THRUST, hprop=self.ph_rcs, isp0=ISP})
	self:create_thrustergroup ({th_att_lin[0]}, THGROUP.ATT_FORWARD)
	self:create_thrustergroup ({th_att_lin[1]}, THGROUP.ATT_BACK)
	self:add_exhaust (th_att_lin[0], 0.6, 0.078, _V( 0.0, -0.228914, -7.462329 ), _V(0,0,-1))
	self:add_exhaust (th_att_lin[0], 0.6, 0.078, _V( 0.0, 0.229, -7.462329 ), _V(0,0,-1))
	self:add_exhaust (th_att_lin[1], 0.6, 0.078, _V( -0.817096, -0.488177, 9.729635 ), _V(0,0,1))
	self:add_exhaust (th_att_lin[1], 0.6, 0.078, _V( 0.817096, -0.488177, 9.729635 ), _V(0,0,1))

	--[[
	COLOUR4 col_d = {0.9,0.8,1,0};
	COLOUR4 col_s = {1.9,0.8,1,0};
	COLOUR4 col_a = {0,0,0,0};
	COLOUR4 col_white = {1,1,1,0};
	LightEmitter *le = AddPointLight (_V(0,0,-10), 200, 1e-3, 0, 2e-3, col_d, col_s, col_a);
	le->SetIntensityRef (&th_main_level);
	]]
	-- ********************* aerodynamics ***********************

	self.hwing = self:create_airfoil (LIFT.VERTICAL, _V(0,0,-0.3), VLiftCoeff, 5, 90, 1.5)
	-- wing and body lift+drag components

	self:create_airfoil (LIFT.HORIZONTAL, _V(0,0,-4), HLiftCoeff, 5, 15, 1.5)
	-- vertical stabiliser and body lift and drag components

	self:create_controlsurface (AIRCTRL.ELEVATOR,     1.4, 1.7, _V(   0,0,-7.2), AIRCTRL_AXIS.XPOS, 1.0, self.anim_elevator)
	self:create_controlsurface (AIRCTRL.RUDDER,       0.8, 1.7, _V(   0,0,-7.2), AIRCTRL_AXIS.YPOS, 1.0, self.anim_rudder)
	self.hlaileron = self:create_controlsurface (AIRCTRL.AILERON, 0.3, 1.7, _V( 7.5,0,-7.2), AIRCTRL_AXIS.XPOS, 1.0, self.anim_raileron)
	self.hraileron = self:create_controlsurface (AIRCTRL.AILERON, 0.3, 1.7, _V(-7.5,0,-7.2), AIRCTRL_AXIS.XNEG, 1.0, self.anim_laileron)
	self:create_controlsurface (AIRCTRL.ELEVATORTRIM, 0.3, 1.7, _V(   0,0,-7.2), AIRCTRL_AXIS.XPOS, 1.0, self.anim_elevatortrim)

	self.ssys_gear:GearState():SetDragElement(self:create_variabledragelement (0.8, _V(0, -1, 0)))     -- landing gear
	self.ssys_mainretro:RetroCoverState():SetDragElement(self:create_variabledragelement (0.2, _V(0,-0.5,6.5))) -- retro covers
	self.ssys_docking:NconeState():SetDragElement(self:create_variabledragelement (3, _V(0, 0, 8)))       -- nose cone
	self.ssys_thermal:RadiatorState():SetDragElement(self:create_variabledragelement (1, _V(0,1.5,-4)))   -- radiator
	self.ssys_aerodyn:AirbrakeState():SetDragElement(self:create_variabledragelement (4, _V(0,0,-8)))     -- airbrake

	self:set_rotdrag (_V(0.10,0.13,0.04))
	-- angular damping

	-- ************************* mesh ***************************

	-- ********************* beacon lights **********************
	local beaconpos = {_V(-8.6,0,-3.3), _V(8.6,0,-3.3), _V(0,0.5,-7.5), _V(0,2.2,2), _V(0,-1.4,2), _V(-8.9,2.5,-5.4), _V(8.9,2.5,-5.4), _V(2.5,-0.5,6.5)}
	local beaconpos_scram = _V(0,-1.8,2)
	local beaconcol = {_V(1.0,0.5,0.5), _V(0.5,1.0,0.5), _V(1,1,1), _V(1,0.6,0.6), _V(1,0.6,0.6), _V(1,1,1), _V(1,1,1) , _V(1,1,1)}
	self.beacon = {}
	for i=1,8 do
		self.beacon[i] = oapi.create_beacon({
			shape = i < 4 and BEACONSHAPE.DIFFUSE or BEACONSHAPE.STAR,
			pos = beaconpos[i],
			col = beaconcol[i],
			size = (i < 4 or i == 8) and 0.3 or 0.55,
			falloff = i < 4 and 0.4 or 0.6,
			period = i < 4 and 0 or (i < 6 and 2 or (i < 8 and 1.13 or 0)),
			duration = i < 6 and 0.1 or 0.05,
			tofs = (6-i-1)*0.2,
			active = false
		})
		self:add_beacon (self.beacon[i])
	end
	if self.ssys_scram then
		beacon[5].pos = beaconpos_scram
	end

	self.exmesh_tpl = oapi.load_mesh_global(self:ScramVersion() and "DG/deltaglider" or "DG/deltaglider_ns")
	self:set_mesh_visibility_mode (self:add_mesh (self.exmesh_tpl), MESHVIS.EXTERNAL)
	self.panelmesh0 = oapi.load_mesh_global ("DG/dg_2dpanel0")
	self.panelmesh1 = oapi.load_mesh_global ("DG/dg_2dpanel1")

	self.vcmesh_tpl = oapi.load_mesh_global ("DG/deltaglider_vc")
	self:set_mesh_visibility_mode (self:add_mesh (self.vcmesh_tpl), MESHVIS.VC)

	-- **************** vessel-specific insignia ****************

	self.insignia_tex = oapi.create_surface (256, 256, bit.bor(OAPISURFACE.RENDERTARGET, OAPISURFACE.TEXTURE, OAPISURFACE.MIPMAPS))
	local hTex = oapi.get_texturehandle (self.exmesh_tpl, 5)
	if hTex then
		oapi.blt (self.insignia_tex, hTex, 0, 0, 0, 0, 256, 256)
	end
end

-- --------------------------------------------------------------
-- Read status from scenario file
-- --------------------------------------------------------------
function DeltaGlider:clbkLoadStateEx (scn, vs)
--	oapi.write_log("DeltaGlider:clbkLoadStateEx")
	local match = {}
	for line in scenario_lines(scn) do
		if scenario_line_match(line, "TANKCONFIG %d", match) then
			if self.ssys_scram then
				self.tankconfig = match.res[1]
			end
		elseif scenario_line_match(line, "PSNGR %b %b %b %b", match) then
			self.psngr = match.res
		elseif scenario_line_match(line, "SKIN %s", match) then
			self.skinpath = "DG/Skins/"..match.res[1]
			
			self.skin[1] = oapi.load_texture(self.skinpath.."/dgmk4_1.dds")
			self.skin[2] = oapi.load_texture(self.skinpath..(self.ssys_scram and "/dgmk4_2.dds" or "dgmk4_2_ns.dds"))
			self.skin[3] = oapi.load_texture(self.skinpath.."/idpanel1.dds")
			if self.skin[3] then
				oapi.blt (self.insignia_tex, self.skin[3], 0, 0, 0, 0, 256, 256)
				oapi.release_texture (self.skin[3])
				self.skin[3] = nil
			end
		elseif scenario_line_match(line, "PANELCOL %d", match) then
			self.panelcol = match.res[1]
		elseif not ComponentVessel.clbkParseScenarioLine (self, line) then
			self:parse_scenario_line_ex(line, vs)  -- unrecognised option - pass to Orbiter's generic parser
		end
	end
end


-- --------------------------------------------------------------
-- Write status to scenario file
-- --------------------------------------------------------------
function DeltaGlider:clbkSaveState (scn)
--	oapi.write_log("DeltaGlider:clbkSaveState")
	-- Write default and subsystem vessel parameters
	ComponentVessel.clbkSaveState (self, scn)

	-- Write custom parameters
	oapi.writescenario_string(scn, "PSNGR", string.format("%s %s %s %s", tostring(self.psngr[1]), tostring(self.psngr[2]), tostring(self.psngr[3]), tostring(self.psngr[4])))

	if self.skinpath then
		oapi.writescenario_string (scn, "SKIN", self.skinpath)
	end

	if self.panelcol ~= 0 then
		oapi.writescenario_int (scn, "PANELCOL", self.panelcol)
	end

	local written = false
	for i=1,8 do
		if self.beacon[i].active and not written then
			local cbuf = string.format("%d %d %d %d", self.beacon[1].active, self.beacon[4].active, self.beacon[6].active, self.beacon[8].active)
			oapi.writescenario_string (scn, "LIGHTS", cbuf)
			written = true
		end
	end
	if self.tankconfig ~= 0 then
		oapi.writescenario_int (scn, "TANKCONFIG", self.tankconfig)
	end
end


-- --------------------------------------------------------------
-- Finalise vessel creation
-- --------------------------------------------------------------
function DeltaGlider:clbkPostCreation ()
--	oapi.write_log("DeltaGlider:clbkPostCreation")
	ComponentVessel.clbkPostCreation (self)

	self:SetEmptyMass ()
	if self.tankconfig ~= 0 then
		if not self.ssys_scram then
			self.tankconfig = 0
		else
			local max_scramfuel
			if self.tankcondof == 1 then
				self.max_rocketfuel = TANK1_CAPACITY
				max_scramfuel = TANK2_CAPACITY
			else
				self.max_rocketfuel = TANK2_CAPACITY
				max_scramfuel  = TANK1_CAPACITY
			end
			self:set_propellantmaxmass (self.ph_main, max_rocketfuel)
			self.ssys_scram:SetPropellantMaxMass (max_scramfuel)
		end
	end

	if self.insignia_tex then
		self:PaintMarkings (self.insignia_tex)
	end
end

-- --------------------------------------------------------------
-- Create DG visual
-- --------------------------------------------------------------
function DeltaGlider:clbkVisualCreated (vis, refcount)
--	oapi.write_log("DeltaGlider:clbkVisualCreated")
	self.visual = vis
	self.exmesh = self:get_devmesh (vis, 0)
	self.vcmesh = self:get_devmesh (vis, 1)
	self:SetPassengerVisuals()
	self:SetDamageVisuals()

	if self.vcmesh and not self:ScramVersion() then -- disable scram-specific components
		local ges = {}
		ges.flags = GRPEDIT.ADDUSERFLAG
		ges.UsrFlag = 3
		local vcscramidx = {
			GRP_VC.SCRAMGIMBAL_L,GRP_VC.SCRAMGIMBAL_R,
			GRP_VC.SCRAM_GIMBAL_FRAME,GRP_VC.SCRAM_INDICATOR_LABEL,GRP_VC.SCRAM_STATUS,
			GRP_VC.SCRAM_TEMP,GRP_VC.SCRAM_GIMBAL_INDICATOR,
			GRP_VC.THROTTLE_SCRAM_L1,GRP_VC.THROTTLE_SCRAM_R1,
			GRP_VC.THROTTLE_SCRAM_L2,GRP_VC.THROTTLE_SCRAM_R2}

		for i = 1, 11 do
			oapi.edit_meshgroup (self.vcmesh, vcscramidx[i], ges)
		end
	end

	self:ApplySkin()

	self:UpdateStatusIndicators()
	if oapi.cockpit_mode() == COCKPIT.VIRTUAL then
		self:InitVCMesh()
	end

	if self.vcmesh then
		ComponentVessel.clbkResetVC (self, 0, self.vcmesh)
	end

end

-- --------------------------------------------------------------
-- Destroy DG visual
-- --------------------------------------------------------------
function DeltaGlider:clbkVisualDestroyed (vis, refcount)
--	oapi.write_log("DeltaGlider:clbkVisualDestroyed")
	self.visual = nil
	self.exmesh = nil
	self.vcmesh = nil
end


-- --------------------------------------------------------------
-- Respond to MFD mode change
-- --------------------------------------------------------------
function DeltaGlider:clbkMFDMode (mfd, mode)
	self.ssys_mfd[mfd-MFDID.LEFT+1]:ModeChanged ()
end

-- --------------------------------------------------------------
-- Respond to RCS mode change
-- --------------------------------------------------------------
function DeltaGlider:clbkRCSMode (mode)
	self.ssys_rcs:SetMode (mode)
end

-- --------------------------------------------------------------
-- Respond to control surface mode change
-- --------------------------------------------------------------
function DeltaGlider:clbkADCtrlMode (mode)
	self.ssys_aerodyn:SetMode (mode)
end

-- --------------------------------------------------------------
-- Respond to HUD mode change
-- --------------------------------------------------------------
function DeltaGlider:clbkHUDMode (mode)
	self.ssys_hud:SetHUDMode (mode)
end

-- --------------------------------------------------------------
-- Respond to navmode change
-- --------------------------------------------------------------
function DeltaGlider:clbkNavMode (mode, active)
	if mode == NAVMODE.HOLDALT then
		self.ssys_hoverctrl:ActivateHold (active)
	else
		self.ssys_rcs:SetProg (mode, active)
	end
end

-- --------------------------------------------------------------
-- Respond to docking/undocking event
-- --------------------------------------------------------------
function DeltaGlider:clbkDockEvent (dock, mate)
	self.ssys_docking:clbkDockEvent (dock, mate)
end

-- --------------------------------------------------------------
-- Respond to navmode processing request
-- --------------------------------------------------------------
function DeltaGlider:clbkNavProcess (mode)
--	if (mode & NAVBIT_HOLDALT) {
--		//mode ^= NAVBIT_HOLDALT;
--		//ProcessHoverHoldalt();
--	}
	return mode
end

-- --------------------------------------------------------------
-- Frame update
-- --------------------------------------------------------------
function DeltaGlider:clbkPostStep (simt, simdt, mjd)
--	oapi.write_log("DeltaGlider:clbkPostStep")
	self.th_main_level = self:get_thrustergrouplevel (THGROUP.MAIN)

	-- damage/failure system
	if self.bDamageEnabled then
		self:TestDamage ()
	end

	ComponentVessel.clbkPostStep (self, simt, simdt, mjd)
end

function DeltaGlider:clbkLoadGenericCockpit ()
--	oapi.write_log("DeltaGlider:clbkLoadGenericCockpit")
	self:set_cameraoffset (_V(0,1.467,6.782))
	oapi.set_defnavdisplay (1)
	oapi.set_defrcsdisplay (1)
	self.campos = CAM_GENERIC
	return true
end

-- --------------------------------------------------------------
-- Load 2-D instrument panel mode
-- --------------------------------------------------------------

function DeltaGlider:clbkLoadPanel2D (id, hPanel, viewW, viewH)
--	oapi.write_log("DeltaGlider:clbkLoadPanel2D")
	-- set up subsystem panel elements
	ComponentVessel.clbkLoadPanel2D (self, id, hPanel, viewW, viewH)

	if id == 0 then
		self:DefinePanelMain (hPanel)
		self:SetPanelScale (hPanel, viewW, viewH)
		oapi.set_panelneighbours (-1,-1,1,-1)
		self:set_cameradefaultdirection (_V(0,0,1)) -- forward
		oapi.set_cameracockpitdir (0,0)         -- look forward
		return true
	elseif id == 1 then
		self:DefinePanelOverhead (hPanel)
		self:SetPanelScale (hPanel, viewW, viewH)
		oapi.set_panelneighbours (-1,-1,-1,0)
		self:set_cameradefaultdirection (_V(0,0,1)) -- forward
		oapi.set_cameracockpitdir (0,20*RAD)    -- look up
		return true
	else
		return false
	end
end

function DeltaGlider:SetPanelScale (hPanel, viewW, viewH)
	local defscale = viewW/PANEL2D_WIDTH
	local extscale = math.max (defscale, 1.0)
	self:set_panelscaling (hPanel, defscale, extscale)
end

function DeltaGlider:DefinePanelMain (hPanel)
	self.hPanelMesh = self.panelmesh0
	local panel2dtex = oapi.get_texturehandle(self.hPanelMesh,1)

	local panelw = PANEL2D_WIDTH
	local panelh = 572

	self:set_panelbackground (hPanel, nil, self.hPanelMesh, panelw, panelh, 190, PANEL.ATTACH_BOTTOM + PANEL.MOVEOUT_BOTTOM)

	-- Define MFD layout (display and buttons)
	self:register_panelmfdgeometry (hPanel, MFDID.LEFT, 0, GRP_P0.LMFD_DISPLAY)
	self:register_panelmfdgeometry (hPanel, MFDID.RIGHT, 0, GRP_P0.RMFD_DISPLAY)

	local ges = {}
	ges.flags = GRPEDIT.ADDUSERFLAG
	ges.UsrFlag = 3
	--FIXME:	grp->UsrFlag = (ScramVersion() ? 0 : 3);

	oapi.edit_meshgroup (self.hPanelMesh, GRP_P0.SCRAM_INSTRUMENTS, ges)

--	//aap->RegisterPanel (hPanel);
	self:InitPanel (0)
end

function DeltaGlider:DefinePanelOverhead (hPanel)
	self.hPanelMesh = self.panelmesh1
	local panelw = PANEL2D_WIDTH
	local panelh = 283

	self:set_panelbackground (hPanel, nil, self.hPanelMesh, panelw, panelh, 0, PANEL.ATTACH_TOP + PANEL.MOVEOUT_TOP)

	self:InitPanel (1)
end

-- --------------------------------------------------------------
-- Respond to panel mouse event
-- --------------------------------------------------------------
function DeltaGlider:clbkPanelMouseEvent (id, event, mx, my, context)
	if context then
		return context:ProcessMouse2D (event, mx, my)
	end
	return false
end

-- --------------------------------------------------------------
-- Respond to panel redraw event
-- --------------------------------------------------------------
function DeltaGlider:clbkPanelRedrawEvent (id, event, surf, context)
--	oapi.write_log(string.format("clbkPanelRedrawEvent %f %d %d %s", oapi.get_simtime(), id, event, tostring(context)))
	if context then
		return context:Redraw2D (surf)
	end

	return false
end

-- --------------------------------------------------------------
-- Load virtual cockpit mode
-- --------------------------------------------------------------
function DeltaGlider:clbkLoadVC (id)
--	oapi.write_log(string.format("DeltaGlider:clbkLoadVC %d", id))
	local mfds_left  = {nmesh=1, ngroup=GRP_VC.LMFD_DISPLAY}
	local mfds_right = {nmesh=1, ngroup=GRP_VC.RMFD_DISPLAY}
	local huds = {nmesh=1, ngroup=GRP_VC.HUDDISP, hudcnt=_V(0,1.462,7.09), size=0.15}
	local tex1 = oapi.get_texturehandle (self.vcmesh_tpl, 16)
	local tex2 = oapi.get_texturehandle (self.vcmesh_tpl, 18)
	local tex3 = oapi.get_texturehandle (self.vcmesh_tpl, 14)
	self.vctex = oapi.get_texturehandle (self.vcmesh_tpl, 20)

	self:InitVC (id)

	self:set_cameradefaultdirection (_V(0,0,1)) -- forward
	oapi.VC_registerHUD (huds) -- HUD parameters
	oapi.VC_registermfd (MFDID.LEFT, mfds_left)   -- left MFD
	oapi.VC_registermfd (MFDID.RIGHT, mfds_right) -- right MFD

	if id == 0 then -- pilot
		self:set_cameraoffset (_V(0,1.467,6.782))
		self:set_camerashiftrange (_V(0,0,0.1), _V(-0.2,0,0), _V(0.2,0,0))
		oapi.VC_set_neighbours (1, 2, -1, -1)
		
		-- main/retro/hover engine indicators
		oapi.VC_register_area (AID_MAINDISP1, _R( 50,16, 63,89), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, PANEL_MAP.BGONREQUEST, tex1)
		oapi.VC_register_area (AID_MAINDISP2, _R( 85,16, 98,89), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, PANEL_MAP.BGONREQUEST, tex1)
		oapi.VC_register_area (AID_MAINDISP3, _R(120,16,133,89), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, PANEL_MAP.BGONREQUEST, tex1)

		-- scram engine indicators
		if self:ScramVersion() then
			oapi.VC_register_area (AID_SCRAMDISP2, _R(195,16,208,89),  PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, PANEL_MAP.BGONREQUEST, tex1)
			oapi.VC_register_area (AID_SCRAMTEMPDISP, _R(6,10,87,140), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, PANEL_MAP.BACKGROUND, tex2)
		end

		self.campos = CAM_VCPILOT
	elseif id == 1 then -- front left passenger
		self:set_cameraoffset (_V(-0.7, 1.15, 5.55))
		self:set_cameramovement (_V(0.2,-0.05,0.3), -10*RAD, 10*RAD, _V(-0.3,0,0), 80*RAD, 0, _V(0.4,0,0), -90*RAD, 0)
		oapi.VC_set_neighbours (-1, 2, 0, 3)
		self.campos = CAM_VCPSNGR1
	elseif id == 2 then -- front right passenger
		self:set_cameraoffset (_V(0.7, 1.15, 5.55))
		self:set_cameramovement (_V(-0.2,-0.05,0.3), 10*RAD, 10*RAD, _V(-0.4,0,0), 90*RAD, 0, _V(0.3,0,0), -80*RAD, 0)
		oapi.VC_set_neighbours (1, -1, 0, 4)
		self.campos = CAM_VCPSNGR2
	elseif id == 3 then -- rear left passenger
		self:set_cameraoffset (_V(-0.8, 1.2, 4.4))
		self:set_cameramovement (_V(0.4,0,0), 0, 0, _V(-0.3,0,0), 70*RAD, 0, _V(0.4,0,0), -90*RAD, 0)
		oapi.VC_set_neighbours (-1, 4, 1, -1)
		self.campos = CAM_VCPSNGR3
	elseif id == 4 then -- rear right passenger
		self:set_cameraoffset (_V(0.8, 1.2, 4.4))
		self:set_cameramovement (_V(-0.4,0,0), 0, 0, _V(-0.4,0,0), 90*RAD, 0, _V(0.3,0,0), -70*RAD, 0)
		oapi.VC_set_neighbours (3, -1, 2, -1)
		self.campos = CAM_VCPSNGR4
	else
		return false
	end

	ComponentVessel.clbkLoadVC (self, id)
	self:InitVCMesh()
	return true
end

-- --------------------------------------------------------------
-- Respond to virtual cockpit mouse event
-- --------------------------------------------------------------
function DeltaGlider:clbkVCMouseEvent (id, event, p)
--	oapi.write_log(string.format("clbkVCMouseEvent %d %d %f,%f,%f",id,event,p.x,p.y,p.z))
	-- standalone id
	if id == AID_MFD1_PWR then
		oapi.toggle_mfdon (MFDID.LEFT)
		return true
	elseif id == AID_MFD2_PWR then
		oapi.toggle_mfdon (MFDID.RIGHT)
		return true
	end

	-- distribute to subsystems
	return ComponentVessel.clbkVCMouseEvent (self, id, event, p)
end

-- --------------------------------------------------------------
-- Respond to virtual cockpit area redraw request
-- --------------------------------------------------------------
function DeltaGlider:clbkVCRedrawEvent (id, event, surf)
--	oapi.write_log(string.format("DeltaGlider:clbkVCRedrawEvent %d", id))
	if not self.vcmesh then return false end

	-- standalone id
	if id == AID_MAINDISP1 then
		return self:RedrawPanel_MainFlow (surf)
	elseif id == AID_MAINDISP2 then
		return self:RedrawPanel_RetroFlow (surf)
	elseif id == AID_MAINDISP3 then
		return self:RedrawPanel_HoverFlow (surf)
	elseif id == AID_SCRAMDISP2 then
		return self:RedrawPanel_ScramFlow (surf)
	elseif id == AID_SCRAMTEMPDISP then
		return self:RedrawPanel_ScramTempDisp (surf)
	end

	-- distribute to subsystems
	return ComponentVessel.clbkVCRedrawEvent (self, id, event, self.vcmesh, surf)
end

-- --------------------------------------------------------------
-- Process buffered key events
-- --------------------------------------------------------------
function DeltaGlider:clbkConsumeBufferedKey (key, down, kstate)
	if not down then return false end -- only process keydown events
	if self:playback() then return false end -- don't allow manual user input during a playback
	return ComponentVessel.clbkConsumeBufferedKey (self, key, down, kstate)
end


function DeltaGlider:GetMainThrusterLevel (which)
	return self:get_thrusterlevel (self.th_main[which])
end

function DeltaGlider:GetRetroThrusterLevel (which) 
	return self:get_thrusterlevel (self.th_retro[which])
end

function DeltaGlider:GetMainThrusterDir (which)
	return self:get_thrusterdir(self.th_main[which])
end

function DeltaGlider:SetMainThrusterDir (which, dir) 
	self:set_thrusterdir(self.th_main[which], dir)
end

function DeltaGlider:GetHoverThrusterLevel (which) 
	return self:get_thrusterlevel(self.th_hover[which])
end
function DeltaGlider:SetHoverThrusterLevel (which, lvl)
	self:set_thrusterlevel(self.th_hover[which], lvl)
end

function DeltaGlider:SubsysPressure()
	return self.ssys_pressurectrl
end

function DeltaGlider:SubsysDocking()
	return self.ssys_docking
end

register_vesselclass(DeltaGlider, true) -- use C++ names (clbkCamelCase)
