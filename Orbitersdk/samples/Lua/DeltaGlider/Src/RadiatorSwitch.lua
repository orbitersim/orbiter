local DGSwitch1 = require("DGSwitch1")

local RadiatorSwitch = Class(DGSwitch1)

function RadiatorSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.TWOSTATE)
	self.component = comp
end

--------------------------------------------------------------

function RadiatorSwitch:Reset2D (panelid, hMesh)
	self:SetState (self.component:GetRadiator() and DGSwitch1.UP or DGSwitch1.DOWN)
	DGSwitch1.Reset2D (self, panelid, hMesh)
end

--------------------------------------------------------------

function RadiatorSwitch:ResetVC (hMesh)
	self:SetState (self.component:GetRadiator() and DGSwitch1.UP or DGSwitch1.DOWN)
	DGSwitch1.ResetVC (self, hMesh)
end

--------------------------------------------------------------

function RadiatorSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		if self:GetState() == DGSwitch1.UP then
			self.component:OpenRadiator()
		else
			self.component:CloseRadiator()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function RadiatorSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		if self:GetState() == DGSwitch1.UP then
			self.component:OpenRadiator()
		else
			self.component:CloseRadiator()
		end
		return true
	end
	return false
end

return RadiatorSwitch
