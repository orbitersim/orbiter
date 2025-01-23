-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: PHoverCtrl.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch2 = require("DGSwitch2")

local PHoverCtrl = Class(DGSwitch2)

function PHoverCtrl:new (_ctrl)
	DGSwitch2.new(self, _ctrl:DG())
	self.ctrl = _ctrl
end

--------------------------------------------------------------
local state2d = 0
function PHoverCtrl:ProcessMouse2D (event, mx, my)
	if DGSwitch2.ProcessMouse2D (self, event, mx, my) then
		state2d = self:GetState()
	end
	self.ctrl:IncPHover (state2d)
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

--------------------------------------------------------------
local statevc = 0
function PHoverCtrl:ProcessMouseVC (event, p)
	if DGSwitch2.ProcessMouseVC (self, event, p) then
		statevc = self:GetState()
	end
	self.ctrl:IncPHover (statevc)
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

return PHoverCtrl
