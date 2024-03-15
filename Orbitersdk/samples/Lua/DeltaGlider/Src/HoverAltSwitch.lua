-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch2 = require("DGSwitch2")

local HoverAltSwitch = Class(DGSwitch2)

function HoverAltSwitch:new (hhac)
	DGSwitch2.new(self, hhac:DG())
	self.ctrl = hhac
end


--------------------------------------------------------------

function HoverAltSwitch:Set (state, refT)
	if state ~= 0 then
		local prm = self.ctrl:TargetPrm()
		local t = oapi.get_systime()
		local dt = oapi.get_sysstep()
		local downt = t-refT
		local dprm = dt * math.max(math.abs(prm),1.0)
		if downt < 10.0 then
			dprm = dprm * (1e-6 + downt*(1.0-1e-6)/10.0)
		end
		if state == 1 then
			dprm = -dprm
		end
		self.ctrl:SetTargetPrm (prm + dprm)
	end
end

--------------------------------------------------------------

local state2d = 0
local refT2d = 0
function HoverAltSwitch:ProcessMouse2D (event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		refT2d = oapi.get_systime()
	end

	if DGSwitch2.ProcessMouse2D (self, event, mx, my) then
		state2d = self:GetState()
	end
	
	self:Set (state2d, refT2d)

	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

--------------------------------------------------------------

local statevc = 0
local refTvc = 0
function HoverAltSwitch:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		refTvc = oapi.get_systime()
	end

	if DGSwitch2.ProcessMouseVC (self, event, p) then
		statevc = self:GetState()
	end

	self:Set (statevc, refTvc)

	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

return HoverAltSwitch
