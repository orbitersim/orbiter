-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: LandDockLightSwitch.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch1 = require("DGSwitch1")

local LandDockLightSwitch = Class(DGSwitch1)

function LandDockLightSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.THREESTATE)
	self.component = comp
end
 --------------------------------------------------------------

function LandDockLightSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		local state = self:GetState()
		self.component:SetLight (state)
		return true
	end
	return false
end

--------------------------------------------------------------

function LandDockLightSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		local state = self:GetState()
		self.component:SetLight (state)
		return true
	end
	return false
end

--------------------------------------------------------------

function LandDockLightSwitch:Reset2D (panelid, hMesh)
	self:SetState (self.component:GetLight())
	DGSwitch1.Reset2D (self, panelid, hMesh)
end

--------------------------------------------------------------

function LandDockLightSwitch:ResetVC (hMesh)
	self:SetState (self.component:GetLight())
	DGSwitch1.ResetVC (self, hMesh)
end

return LandDockLightSwitch
