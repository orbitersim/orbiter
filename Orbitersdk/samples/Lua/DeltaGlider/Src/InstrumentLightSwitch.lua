-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: InstrumentLightSwitch.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch1 = require("DGSwitch1")
local InstrumentLightSwitch = Class(DGSwitch1)

function InstrumentLightSwitch:new (comp)
	DGSwitch1.new(self, comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function InstrumentLightSwitch:ResetVC (hMesh)
	self:SetState (self.component:GetLight() and DGSwitch1.UP or DGSwitch1.DOWN)
	DGSwitch1.ResetVC (self, hMesh)
end

--------------------------------------------------------------

function InstrumentLightSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		local state = self:GetState()
		self.component:SetLight (state == DGSwitch1.UP)
		return true
	end
	return false
end

return InstrumentLightSwitch
