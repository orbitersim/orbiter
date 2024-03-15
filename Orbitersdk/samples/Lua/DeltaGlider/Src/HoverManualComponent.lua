-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local HoverSubsystemComponent = require("HoverSubsystemComponent")
local HoverThrottle = require("HoverThrottle")


local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local HoverManualComponent = Class(HoverSubsystemComponent)

function HoverManualComponent:new (_subsys)
	HoverSubsystemComponent.new(self, _subsys)

	self.ELID_THROTTLE, self.throttle = self:AddElement (HoverThrottle (self))

	-- Hover throttle VC animation
	local HoverThrottleGrp = {GRP_VC.THROTTLE_HOVER_1,GRP_VC.THROTTLE_HOVER_2}
	local HoverThrottle = MGROUP_ROTATE (1, HoverThrottleGrp, _V(-0.41,0.85,6.9226), _V(1,0,0), 50*RAD)
	self.anim_hoverthrottle = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_hoverthrottle, 0, 1, HoverThrottle)
end

--------------------------------------------------------------

function HoverManualComponent:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)

	self:DG():register_panelarea (hPanel, self.ELID_THROTTLE,  _R( 4,304, 57,444), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBPRESSED, panel2dtex, self.throttle)

	return true
end

--------------------------------------------------------------

function HoverManualComponent:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Hover throttle
	oapi.VC_register_area (self.ELID_THROTTLE, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBPRESSED)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_THROTTLE, _V(-0.44,0.87,6.81), _V(-0.35,0.87,6.81), _V(-0.44,0.95,6.91), _V(-0.35,0.95,6.91))

	return true
end

return HoverManualComponent
