-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: CockpitLightSwitch.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch1 = require("DGSwitch1")

local CockpitLightSwitch = Class(DGSwitch1)

function CockpitLightSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.THREESTATE)
	self.component = comp
end

--------------------------------------------------------------

function CockpitLightSwitch:ResetVC (hMesh)
	self:SetState (self.component:GetLight())
	DGSwitch1.ResetVC (self, hMesh)
end

--------------------------------------------------------------

function CockpitLightSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		local state = self:GetState()
		self.component:SetLight (state)
		return true
	end
	return false
end

return CockpitLightSwitch
