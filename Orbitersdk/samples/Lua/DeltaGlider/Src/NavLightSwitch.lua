-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: NavLightSwitch.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch1 = require("DGSwitch1")

local NavLightSwitch = Class(DGSwitch1)

function NavLightSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.TWOSTATE)
	self.component = comp
end

--------------------------------------------------------------

function NavLightSwitch:Reset2D (panelid, hMesh)
	self:SetState (self.component:GetLight() and DGSwitch1.UP or DGSwitch1.DOWN)
	DGSwitch1.Reset2D (self, panelid, hMesh)
end

--------------------------------------------------------------

function NavLightSwitch:ResetVC (hMesh)
	self:SetState (self.component:GetLight() and DGSwitch1.UP or DGSwitch1.DOWN)
	DGSwitch1.ResetVC (self, hMesh)
end

--------------------------------------------------------------

function NavLightSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		local state = self:GetState()
		self.component:SetLight (state == DGSwitch1.UP)
		return true
	end
	return false
end

--------------------------------------------------------------

function NavLightSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		local state = self:GetState()
		self.component:SetLight (state == DGSwitch1.UP)
		return true
	end
	return false
end

return NavLightSwitch
