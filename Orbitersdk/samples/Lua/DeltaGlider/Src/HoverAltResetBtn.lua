-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HoverAltResetBtn.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGButton2 = require("DGButton2")

local HoverHoldComponent = {}
HoverHoldComponent.HOLD_NONE = 0
HoverHoldComponent.HOLD_ALT = 1
HoverHoldComponent.HOLD_VSPD = 2

local HoverAltResetBtn = Class(DGButton2)

function HoverAltResetBtn:new (hhac)
	DGButton2.new(self, hhac:DG())
	self.ctrl = hhac
end

--------------------------------------------------------------

function HoverAltResetBtn:ProcessMouse2D (event, mx, my)
	DGButton2.ProcessMouse2D (self, event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if self.ctrl:GetHoverMode() == HoverHoldComponent.HOLD_ALT then
			self.ctrl:SetTargetAltCurrent ()
		else
			self.ctrl:SetTargetVspd (0.0)
		end
	end
	return false -- no animation
end

--------------------------------------------------------------

function HoverAltResetBtn:ProcessMouseVC (event, p)
	DGButton2.ProcessMouseVC (self, event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if self.ctrl:GetHoverMode() == HoverHoldComponent.HOLD_ALT then
			self.ctrl:SetTargetAltCurrent ()
		else
			self.ctrl:SetTargetVspd (0.0)
		end
	end
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

return HoverAltResetBtn
