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
local WheelbrakeLever = require("WheelbrakeLever")

local Wheelbrake = Class(DGSubsystem)

function Wheelbrake:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.ELID_LEVER, self.lever = self:AddElement (WheelbrakeLever (self))
end

--------------------------------------------------------------

function Wheelbrake:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_LEVER, _R(1221,494,1273,557), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP, panel2dtex, self.lever)

	return true
end

return Wheelbrake

