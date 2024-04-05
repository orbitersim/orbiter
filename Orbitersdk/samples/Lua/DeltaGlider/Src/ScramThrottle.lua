-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: ScramThrottle.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local ScramThrottleLever = require("ScramThrottleLever")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP


local ScramThrottle = Class(DGSubsystem)

function ScramThrottle:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.ELID_LEVER, self.lever = self:AddElement (ScramThrottleLever (self))

	self.anim_lever = {}
	-- VC animation: Left scram engine throttle
	local ScramThrottleLGrp = {GRP_VC.THROTTLE_SCRAM_L1,GRP_VC.THROTTLE_SCRAM_L2}
	local ScramThrottleL = MGROUP_ROTATE (1, ScramThrottleLGrp, _V(0,0.7849,6.96), _V(1,0,0), 30*RAD)
	self.anim_lever[1] = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_lever[1], 0, 1, ScramThrottleL)

	-- VC animation: Right scram engine throttle
	local ScramThrottleRGrp = {GRP_VC.THROTTLE_SCRAM_R1,GRP_VC.THROTTLE_SCRAM_R2}
	local ScramThrottleR = MGROUP_ROTATE (1, ScramThrottleRGrp, _V(0,0.7849,6.96), _V(1,0,0), 30*RAD)
	self.anim_lever[2] = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_lever[2], 0, 1, ScramThrottleR)
end

--------------------------------------------------------------

function ScramThrottle:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_LEVER, _R(4,456,57,558), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED, panel2dtex, self.lever)

	return true
end

--------------------------------------------------------------

function ScramThrottle:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Throttle lever animations
	oapi.VC_register_area (self.ELID_LEVER, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBPRESSED)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_LEVER, _V(-0.45,0.98,6.94), _V(-0.39,0.98,6.94), _V(-0.45,0.95,7.07), _V(-0.39,0.95,7.07));

	return true
end

return ScramThrottle
