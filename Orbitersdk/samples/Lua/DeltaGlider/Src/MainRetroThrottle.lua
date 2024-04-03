-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: MainRetroThrottle.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local MainRetroThrottleLevers = require("MainRetroThrottleLevers")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- ==============================================================
-- Main/retro engine throttle
-- ==============================================================
local MainRetroThrottle = Class(DGSubsystem)

function MainRetroThrottle:new(_subsys)
	DGSubsystem.new(self, _subsys)

	self.ELID_LEVERS, self.levers = self:AddElement (MainRetroThrottleLevers (self))

	-- VC animation: Left main engine throttle
	local MainThrottleLGrp = {GRP_VC.THROTTLE_MAIN_L1,GRP_VC.THROTTLE_MAIN_L2}
	local MainThrottleL = MGROUP_ROTATE (1, MainThrottleLGrp, _V(0,0.72,6.9856), _V(1,0,0), 50*RAD)
	self.anim_lever = {}
	self.anim_lever[1] = self:DG():create_animation (0.4)
	self:DG():add_animationcomponent (self.anim_lever[1], 0, 1, MainThrottleL)

	-- VC animation: Right main engine throttle
	local MainThrottleRGrp = {GRP_VC.THROTTLE_MAIN_R1,GRP_VC.THROTTLE_MAIN_R2}
	local MainThrottleR = MGROUP_ROTATE (1, MainThrottleRGrp, _V(0,0.72,6.9856), _V(1,0,0), 50*RAD)
	self.anim_lever[2] = self:DG():create_animation (0.4)
	self:DG():add_animationcomponent (self.anim_lever[2], 0, 1, MainThrottleR)
end

--------------------------------------------------------------

function MainRetroThrottle:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_LEVERS, _R(4,122,57,297), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED, panel2dtex, self.levers)

	return true
end

--------------------------------------------------------------

function MainRetroThrottle:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Throttle lever animations
	oapi.VC_register_area (self.ELID_LEVERS, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_LEVERS, _V(-0.372,0.918,6.905), _V(-0.279,0.918,6.905), _V(-0.372,0.885,7.11), _V(-0.279,0.885,7.11))

	return true
end

return MainRetroThrottle
