local DGSwitch1 = require("DGSwitch1")


local StrobeLightSwitch = Class(DGSwitch1)

function StrobeLightSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.TWOSTATE)
	self.component = comp
end

--------------------------------------------------------------

function StrobeLightSwitch:Reset2D (panelid, hMesh)
	self:SetState (self.component:GetLight() and DGSwitch1.UP or DGSwitch1.DOWN)
	DGSwitch1.Reset2D (self, panelid, hMesh)
end

--------------------------------------------------------------

function StrobeLightSwitch:ResetVC (hMesh)
	self:SetState (self.component:GetLight() and DGSwitch1.UP or DGSwitch1.DOWN)
	DGSwitch1.ResetVC (self, hMesh)
end

--------------------------------------------------------------

function StrobeLightSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		local state = self:GetState()
		self.component:SetLight (state == DGSwitch1.UP)
		return true
	end
	return false
end

--------------------------------------------------------------

function StrobeLightSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		local state = self:GetState()
		self.component:SetLight (state == DGSwitch1.UP)
		return true
	end
	return false
end

return StrobeLightSwitch
