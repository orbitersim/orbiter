-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: GearControl.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local AnimState2 = require("AnimState2")
local GearLever = require("GearLever")
local GearIndicator = require("GearIndicator")

local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local GEAR_OPERATING_SPEED = 0.15
-- Opening/closing speed of landing gear (1/sec)
-- => gear cycle ~ 6.7 sec

-- GEARLEVER (VC): rotation reference
local VC_GEARLEVER_ref = _V(-0.37400,0.92634,7.20505)

-- GEARLEVER (VC): rotation axis
local VC_GEARLEVER_axis = _V(1.00000,0.00000,0.00000)

-- GEARLEVER (VC): mouse catch area
local VC_GEARLEVER_mousearea = {_V(-0.38900,0.90989,7.13172),_V(-0.35900,0.90989,7.13172),_V(-0.38900,1.00108,7.19712),_V(-0.35900,1.00108,7.19712)}

local GearControl = Class(DGSubsystem)

function GearControl:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.gear_state = AnimState2(GEAR_OPERATING_SPEED)
	self.glever_state = AnimState2(4.0)

	self.ELID_LEVER, self.lever = self:AddElement (GearLever (self))
	self.ELID_INDICATOR, self.indicator = self:AddElement (GearIndicator (self))

	-- Landing gear animation
	local NWheelStrutGrp = {GRP.NWheelStrut1,GRP.NWheelStrut2}
	local NWheelStrut = MGROUP_ROTATE (0, NWheelStrutGrp, _V(0,-1.048,8.561), _V(1,0,0), -95*RAD)
	local NWheelFCoverGrp = {GRP.NWheelFCover1,GRP.NWheelFCover2}
	local NWheelFCover = MGROUP_ROTATE (0, NWheelFCoverGrp, _V(0,-1.145,8.65), _V(1,0,0), -90*RAD)
	local NWheelLCoverGrp = {GRP.NWheelLCover1,GRP.NWheelLCover2}
	local NWheelLCover1 = MGROUP_ROTATE (0, NWheelLCoverGrp, _V(-0.3,-1.222,7.029), _V(0,0.052,0.999), -90*RAD)
	local NWheelLCover2 = MGROUP_ROTATE (0, NWheelLCoverGrp, _V(-0.3,-1.222,7.029), _V(0,0.052,0.999),  90*RAD)
	local NWheelRCoverGrp = {GRP.NWheelRCover1,GRP.NWheelRCover2}
	local NWheelRCover1 = MGROUP_ROTATE (0, NWheelRCoverGrp, _V( 0.3,-1.222,7.029), _V(0,0.052,0.999),  90*RAD)
	local NWheelRCover2 = MGROUP_ROTATE (0, NWheelRCoverGrp, _V( 0.3,-1.222,7.029), _V(0,0.052,0.999), -90*RAD)
	local LWheelStrutGrp = {GRP.LWheelStrut1,GRP.LWheelStrut2}
	local LWheelStrut = MGROUP_ROTATE (0, LWheelStrutGrp, _V(-3.607,-1.137,-3.08), _V(0,0,1), -90*RAD)
	local RWheelStrutGrp = {GRP.RWheelStrut1,GRP.RWheelStrut2}
	local RWheelStrut = MGROUP_ROTATE (0, RWheelStrutGrp, _V( 3.607,-1.137,-3.08), _V(0,0,1), 90*RAD)
	local LWheelOCoverGrp = {GRP.LWheelOCover1,GRP.LWheelOCover2,GRP.LWheelOCover3,GRP.LWheelOCover4}
	local LWheelOCover = MGROUP_ROTATE (0, LWheelOCoverGrp, _V(-3.658,-1.239,-3.038), _V(0,0,1), -110*RAD)
	local LWheelICoverGrp = {GRP.LWheelICover1,GRP.LWheelICover2}
	local LWheelICover1 = MGROUP_ROTATE (0, LWheelICoverGrp, _V(-2.175,-1.178,-3.438), _V(0,0,1),  90*RAD)
	local LWheelICover2 = MGROUP_ROTATE (0, LWheelICoverGrp, _V(-2.175,-1.178,-3.438), _V(0,0,1), -90*RAD)
	local RWheelOCoverGrp = {GRP.RWheelOCover1,GRP.RWheelOCover2,GRP.RWheelOCover3,GRP.RWheelOCover4}
	local RWheelOCover = MGROUP_ROTATE  (0, RWheelOCoverGrp, _V( 3.658,-1.239,-3.038), _V(0,0,1),  110*RAD)
	local RWheelICoverGrp = {GRP.RWheelICover1,GRP.RWheelICover2}
	local RWheelICover1 = MGROUP_ROTATE  (0, RWheelICoverGrp, _V( 2.175,-1.178,-3.438), _V(0,0,1), -90*RAD)
	local RWheelICover2 = MGROUP_ROTATE  (0, RWheelICoverGrp, _V( 2.175,-1.178,-3.438), _V(0,0,1),  90*RAD)
	self.anim_gear = self:DG():create_animation (1)
	self:DG():add_animationcomponent (self.anim_gear, 0.3, 1, NWheelStrut)
	self:DG():add_animationcomponent (self.anim_gear, 0.3, 0.9, NWheelFCover)
	self:DG():add_animationcomponent (self.anim_gear, 0, 0.3, NWheelLCover1)
	self:DG():add_animationcomponent (self.anim_gear, 0.7, 1.0, NWheelLCover2)
	self:DG():add_animationcomponent (self.anim_gear, 0, 0.3, NWheelRCover1)
	self:DG():add_animationcomponent (self.anim_gear, 0.7, 1.0, NWheelRCover2)
	self:DG():add_animationcomponent (self.anim_gear, 0, 1, LWheelStrut)
	self:DG():add_animationcomponent (self.anim_gear, 0, 1, RWheelStrut)
	self:DG():add_animationcomponent (self.anim_gear, 0, 1, LWheelOCover)
	self:DG():add_animationcomponent (self.anim_gear, 0, 0.3, LWheelICover1)
	self:DG():add_animationcomponent (self.anim_gear, 0.7, 1, LWheelICover2)
	self:DG():add_animationcomponent (self.anim_gear, 0, 1, RWheelOCover)
	self:DG():add_animationcomponent (self.anim_gear, 0, 0.3, RWheelICover1)
	self:DG():add_animationcomponent (self.anim_gear, 0.7, 1, RWheelICover2)

	-- VC gear lever animation
	local GearLeverTransform = MGROUP_ROTATE (1, GRP_VC.GEAR_LEVER, VC_GEARLEVER_ref, VC_GEARLEVER_axis, -70*RAD)
	self.anim_gearlever = self:DG():create_animation (0.5)
	self:DG():add_animationcomponent (self.anim_gearlever, 0, 1, GearLeverTransform)
end

--------------------------------------------------------------

function GearControl:LowerGear ()
	if self:DG():get_groundcontact() then
		local vnml = self:DG():horizoninvrot(_V(0,1,0))
		if vnml.y > 0.0 then return end
	end
	-- we cannot deploy the landing gear if we are already sitting on the ground

	self.gear_state:Open()
	self.glever_state:Open()
	self:DG():UpdateStatusIndicators()
	self:DG():trigger_panelredrawarea (0, self.ELID_LEVER)
	self:DG():trigger_redrawarea (2, 0, self.ELID_INDICATOR)
	self:DG():record_event ("GEAR", "DOWN")
end

--------------------------------------------------------------

function GearControl:RaiseGear ()
	self.gear_state:Close()
	self.glever_state:Close()
	self:DG():UpdateStatusIndicators()
	self:DG():trigger_panelredrawarea (0, self.ELID_LEVER)
	self:DG():trigger_redrawarea (2, 0, self.ELID_INDICATOR)
	self:DG():record_event ("GEAR", "UP")
end

--------------------------------------------------------------

function GearControl:RevertGear ()
	if self.gear_state:IsOpen() or self.gear_state:IsOpening() then
		self:RaiseGear()
	else
		self:LowerGear()
	end
end

--------------------------------------------------------------

function GearControl:clbkPostStep (simt, simdt, mjd)
	-- animate landing gear
	if self.gear_state:Process (simdt) then
		self:DG():set_animation (self.anim_gear, self.gear_state:State())
		self:DG():SetGearParameters (self.gear_state:State())
		self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)
		self:DG():UpdateStatusIndicators()
	end

	-- animate gear lever
	if self.glever_state:Process (simdt) then 
		self:DG():set_animation (self.anim_gearlever, self.glever_state:State())
	end
end

--------------------------------------------------------------

function GearControl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_LEVER, _R(93,373,125,598), PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN, panel2dtex, self.lever)
	self:DG():register_panelarea (hPanel, self.ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, panel2dtex, self.indicator)

	return true
end

--------------------------------------------------------------

function GearControl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	local tex1 = oapi.get_texturehandle (self:DG().vcmesh_tpl, 16)

	-- Gear lever
	oapi.VC_register_area (self.ELID_LEVER, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_LEVER, VC_GEARLEVER_mousearea[1], VC_GEARLEVER_mousearea[2], VC_GEARLEVER_mousearea[3], VC_GEARLEVER_mousearea[4])

	-- Gear indicator
	oapi.VC_register_area (self.ELID_INDICATOR, PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE)
	oapi.VC_register_area (self.ELID_INDICATOR, _R(32,127,61,158), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, PANEL_MAP.BACKGROUND, tex1)

	return true
end

--------------------------------------------------------------

function GearControl:clbkSaveState (scn)
	self.gear_state:SaveState (scn, "GEAR")
end

--------------------------------------------------------------

function GearControl:clbkParseScenarioLine (line)
	if self.gear_state:ParseScenarioLine (line, "GEAR") then
		if self.gear_state:IsOpen() or self.gear_state:IsOpening() then
			self.glever_state:SetOpened()
		else
			self.glever_state:SetClosed()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function GearControl:clbkPostCreation ()
	self:DG():set_animation (self.anim_gear, self.gear_state:State())
	self:DG():set_animation (self.anim_gearlever, self.glever_state:State())
	self:DG():SetGearParameters (self.gear_state:State())
end

--------------------------------------------------------------

function GearControl:clbkDrawHUD (mode, hps, skp)
	-- show gear deployment status
	local cx = hps.CX
	local cy = hps.CY
	
	local int, frac = math.modf(oapi.get_simtime())
	local blink = frac < 0.5

	if self.gear_state:IsOpen() or not self.gear_state:IsClosed() and blink then
		local d = hps.Markersize/2
		if cx >= -d*3 and cx < hps.W+d*3 and cy >= d and cy < hps.H+d*5 then
			skp:rectangle (cx-d/2, cy-d*5, cx+d/2, cy-d*4)
			skp:rectangle (cx-d*3, cy-d*2, cx-d*2, cy-d)
			skp:rectangle (cx+d*2, cy-d*2, cx+d*3, cy-d)
		end
	end
	return true
end

--------------------------------------------------------------

function GearControl:clbkPlaybackEvent (simt, event_t, event_type, event)
	if event_type == "GEAR" then
		if event == "UP" then
			self:RaiseGear()
		else
			self:LowerGear()
		end
		return true
	end
	return false
end

--------------------------------------------------------------
function GearControl:clbkConsumeBufferedKey (key, down, kstate)
	if KEYMOD_ALT(kstate) or KEYMOD_CONTROL(kstate) or KEYMOD_SHIFT(kstate) then
		return false
	end
	if key == OAPI_KEY.G then
		self:RevertGear()
		return true
	end
	return false
end

function GearControl:GearState()
	return self.gear_state
end

return GearControl
