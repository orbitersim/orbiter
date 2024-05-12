-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: CoolantPumpSwitch.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch1 = require("DGSwitch1")

local CoolantPumpSwitch = Class(DGSwitch1)

function CoolantPumpSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.TWOSTATE)
	self.component = comp
end

--------------------------------------------------------------

function CoolantPumpSwitch:Reset2D (panelid, hMesh)
	self:SetState (self.component:PumpActive() and DGSwitch1.UP or DGSwitch1.DOWN)
end

--------------------------------------------------------------

function CoolantPumpSwitch:ResetVC (hMesh)
	DGSwitch1.ResetVC (self, hMesh)
	self:SetState (self.component:PumpActive() and DGSwitch1.UP or DGSwitch1.DOWN)
end

--------------------------------------------------------------

function CoolantPumpSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		self.component:ActivatePump (self:GetState() == DGSwitch1.UP)
		return true
	end
	return false
end

--------------------------------------------------------------

function CoolantPumpSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		self.component:ActivatePump (self:GetState() == DGSwitch1.UP)
		return true
	end
	return false
end

return CoolantPumpSwitch
