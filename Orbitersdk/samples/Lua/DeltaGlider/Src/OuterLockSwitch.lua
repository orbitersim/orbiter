-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: OuterLockSwitch.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch1 = require("DGSwitch1")

local OuterLockSwitch = Class(DGSwitch1)

function OuterLockSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.TWOSTATE)
	self.component = comp
end

--------------------------------------------------------------

function OuterLockSwitch:Reset2D (panelid, hMesh)
	self:SetState ((self.component.ostate:IsClosed() or self.component.ostate:IsClosing()) and DGSwitch1.DOWN or DGSwitch1.UP)
end

--------------------------------------------------------------

function OuterLockSwitch:ResetVC (hMesh)
	DGSwitch1.ResetVC (self, hMesh)
	self:SetState ((self.component.ostate:IsClosed() or self.component.ostate:IsClosing()) and DGSwitch1.DOWN or DGSwitch1.UP)
end

--------------------------------------------------------------

function OuterLockSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		if self:GetState() == DGSwitch1.UP then
			self.component:OpenOuterLock()
		else
			self.component:CloseOuterLock()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function OuterLockSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		if self:GetState() == DGSwitch1.UP then
			self.component:OpenOuterLock()
		else
			self.component:CloseOuterLock()
		end
		return true
	end
	return false
end

return OuterLockSwitch
