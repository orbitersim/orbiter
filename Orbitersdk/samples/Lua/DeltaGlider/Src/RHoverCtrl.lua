-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: RHoverCtrl.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch2 = require("DGSwitch2")

local RHoverCtrl = Class(DGSwitch2)

function RHoverCtrl:new (_ctrl)
	DGSwitch2.new(self, _ctrl:DG())
	self.ctrl = _ctrl
end

--------------------------------------------------------------

local state2d = 0
function RHoverCtrl:ProcessMouse2D (event, mx, my)
	if DGSwitch2.ProcessMouse2D (self, event, mx, my) then
		state2d = self:GetState()
	end
	self.ctrl:IncRHover (state2d)
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

--------------------------------------------------------------
local statevc = 0
function RHoverCtrl:ProcessMouseVC (event, p)
	if DGSwitch2.ProcessMouseVC (self, event, p) then
		statevc = self:GetState()
	end
	self.ctrl:IncRHover (statevc)
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

return RHoverCtrl
