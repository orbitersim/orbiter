-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local AirbrakeLever = require("AirbrakeLever")
local AnimState2 = require("AnimState2")

local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local AIRBRAKE_OPERATING_SPEED = 0.3
-- Deployment speed of airbrakes
-- => cycle = 3.3 sec

-- AIRBRAKELEVER (VC): mouse catch area
local VC_AIRBRAKELEVER_mousearea = {_V(-0.33700,0.97772,7.17631),_V(-0.29300,0.97772,7.17631),_V(-0.33700,1.04273,7.22293),_V(-0.29300,1.04273,7.22293)}

-- AIRBRAKELEVER (VC): rotation reference
local VC_AIRBRAKELEVER_ref = _V(-0.31500,0.94718,7.25322)

-- AIRBRAKELEVER (VC): rotation axis
local VC_AIRBRAKELEVER_axis = _V(1.00000,0.00000,0.00000)


local Airbrake = Class(DGSubsystem)

function Airbrake:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.brake_state = AnimState2 (AIRBRAKE_OPERATING_SPEED)
	self.lever_state = AnimState2 (4.0)
	self.airbrake_tgt = 0
	self.ELID_LEVER, self.lever = self:AddElement (AirbrakeLever (self))

	-- Airbrake animation
	local RRudderGrp = {GRP.RRudder1,GRP.RRudder2}
	local LRudderGrp = {GRP.LRudder1,GRP.LRudder2}
	local UpperBrakeGrp = {GRP.RUAileron1,GRP.LUAileron1,GRP.LUAileron2,GRP.RUAileron2}
	local UpperBrake = MGROUP_ROTATE (0, UpperBrakeGrp, _V(0,-0.4,-6.0), _V(1,0,0), 50*RAD)
	local LowerBrakeGrp = {GRP.LLAileron1,GRP.RLAileron1,GRP.LLAileron2,GRP.RLAileron2}
	local LowerBrake = MGROUP_ROTATE (0, LowerBrakeGrp, _V(0,-0.4,-6.0), _V(1,0,0), -50*RAD)
	local RRudderBrake = MGROUP_ROTATE (0, RRudderGrp, _V( 8.668,0.958,-6.204), _V( 0.143,0.975,-0.172),  25*RAD)
	local LRudderBrake = MGROUP_ROTATE (0, LRudderGrp, _V(-8.668,0.958,-6.204), _V(-0.143,0.975,-0.172), -25*RAD)

	self.anim_brake = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_brake, 0, 1, UpperBrake)
	self:DG():add_animationcomponent (self.anim_brake, 0, 1, LowerBrake)
	self:DG():add_animationcomponent (self.anim_brake, 0, 1, RRudderBrake)
	self:DG():add_animationcomponent (self.anim_brake, 0, 1, LRudderBrake)

	-- Airbrake lever animation
	local AirbrakeLeverGrp = {GRP_VC.AIRBRAKE_LEVER}
	local AirbrakeLeverTransform = MGROUP_ROTATE (1, AirbrakeLeverGrp, VC_AIRBRAKELEVER_ref, VC_AIRBRAKELEVER_axis, -40*RAD)
	self.anim_airbrakelever = self:DG():create_animation(0.8)
	self:DG():add_animationcomponent (self.anim_airbrakelever, 0, 1, AirbrakeLeverTransform)
end

--------------------------------------------------------------

function Airbrake:Extend ()
	local eps = 1e-8
	self.brake_state:Open()
	self.lever_state:Open()
	self.airbrake_tgt = (self.lever_state:State() < 0.5-eps) and 1 or 2
	self:DG():trigger_panelredrawarea (0, self.ELID_LEVER)
	self:DG():record_event ("AIRBRAKE", "OPEN")
end

--------------------------------------------------------------

function Airbrake:Retract ()
	local eps = 1e-8
	self.brake_state:Close()
	self.lever_state:Close()
	self.airbrake_tgt = (self.lever_state:State() > 0.5+eps) and 1 or 0
	self:DG():trigger_panelredrawarea (0, self.ELID_LEVER)
	self:DG():record_event ("AIRBRAKE", "CLOSE")
end

--------------------------------------------------------------

function Airbrake:clbkPostStep (simt, simdt, mjd)
	-- animate airbrake
	if self.brake_state:Process (simdt) then
		if self.airbrake_tgt == 1 then -- intermediate position
			if (self.brake_state:IsClosing() and self.brake_state:State() < 0.5) or
			   (self.brake_state:IsOpening() and self.brake_state:State() > 0.5) then
				self.brake_state:SetState (0.5, 0.0)
			end
		end
		self:DG():set_animation (self.anim_brake, self.brake_state:State())
		self:DG():UpdateStatusIndicators()
	end

	-- animate airbrake lever
	if self.lever_state:Process (simdt) then
		if self.airbrake_tgt == 1 then -- intermediate position
			if (self.lever_state:IsClosing() and self.lever_state:State() < 0.5) or
			   (self.lever_state:IsOpening() and self.lever_state:State() > 0.5) then
				self.lever_state:SetState (0.5, 0.0)
			end
		end
		self:DG():set_animation (self.anim_airbrakelever, self.lever_state:State())
	end
end

--------------------------------------------------------------

function Airbrake:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	-- airbrake lever
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_LEVER, _R(138,299,158,359), PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN, panel2dtex, self.lever)

	return true;
end

--------------------------------------------------------------

function Airbrake:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Airbrake lever
	oapi.VC_register_area (self.ELID_LEVER, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_LEVER, VC_AIRBRAKELEVER_mousearea[1], VC_AIRBRAKELEVER_mousearea[2], VC_AIRBRAKELEVER_mousearea[3], VC_AIRBRAKELEVER_mousearea[4])

	return true
end

--------------------------------------------------------------

function Airbrake:clbkSaveState (scn)
	self.brake_state:SaveState (scn, "AIRBRAKE")
end

--------------------------------------------------------------

function Airbrake:clbkParseScenarioLine (line)
	local eps = 1e-8

	if self.brake_state:ParseScenarioLine (line, "AIRBRAKE") then
		if (not self.brake_state:IsActive() and math.abs(self.brake_state:State()-0.5) < eps) or
		   (self.brake_state:IsClosing() and self.brake_state:State() > 0.5) or
		   (self.brake_state:IsOpening() and self.brake_state:State() < 0.5) then
			self.airbrake_tgt = 1
		elseif self.brake_state:State() < 0.5 then
			self.airbrake_tgt = 0
		else
			self.airbrake_tgt = 2
		end
		return true
	end
	self.lever_state:SetState (self.airbrake_tgt*0.5, 0)
	return false
end

--------------------------------------------------------------

function Airbrake:clbkPostCreation ()
	self:DG():set_animation (self.anim_brake, self.brake_state:State())
	self:DG():set_animation (self.anim_airbrakelever, self.lever_state:State())
end

--------------------------------------------------------------

function Airbrake:clbkPlaybackEvent (simt, event_t, event_type, event)
	if event_type == "AIRBRAKE" then
		if event == "CLOSE" then
			self:Retract()
		else
			self:Extend()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function Airbrake:clbkConsumeBufferedKey (key, down, kstate)
	if KEYMOD_ALT(kstate) or KEYMOD_SHIFT(kstate) then
		return false
	end

	if key == OAPI_KEY.B then
		if KEYMOD_CONTROL(kstate) then
			self:Retract()
		else
			self:Extend()
		end
		return true
	end
	return false
end

function Airbrake:State() 
	return self.brake_state
end

function Airbrake:TargetState()
	return self.airbrake_tgt -- 0,1,2
end

return Airbrake
