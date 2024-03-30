local DGSwitch1 = require("DGSwitch1")

local HatchCtrlSwitch = Class(DGSwitch1)

function HatchCtrlSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.TWOSTATE)
	self.component = comp
end

--------------------------------------------------------------

function HatchCtrlSwitch:Reset2D (panelid, hMesh)
	self:SetState ((self.component.hatch_state:IsClosed() or self.component.hatch_state:IsClosing()) and DGSwitch1.DOWN or DGSwitch1.UP)
end

--------------------------------------------------------------

function HatchCtrlSwitch:ResetVC (hMesh)
	DGSwitch1.ResetVC (self, hMesh)
	self:SetState ((self.component.hatch_state:IsClosed() or self.component.hatch_state:IsClosing()) and DGSwitch1.DOWN or DGSwitch1.UP)
end

--------------------------------------------------------------

function HatchCtrlSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		if self:GetState() == DGSwitch1.UP then
			self.component:OpenHatch()
		else
			self.component:CloseHatch()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function HatchCtrlSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		if self:GetState() == DGSwitch1.UP then
			self.component:OpenHatch()
		else
			self.component:CloseHatch()
		end
		return true
	end
	return false
end


return HatchCtrlSwitch
