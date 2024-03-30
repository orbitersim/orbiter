local DGSwitch1 = require("DGSwitch1")

local InnerLockSwitch = Class(DGSwitch1)

function InnerLockSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.TWOSTATE)
	self.component = comp
end

--------------------------------------------------------------

function InnerLockSwitch:Reset2D (panelid, hMesh)
	self:SetState ((self.component.istate:IsClosed() or self.component.istate:IsClosing()) and DGSwitch1.DOWN or DGSwitch1.UP)
end

--------------------------------------------------------------

function InnerLockSwitch:ResetVC (hMesh)
	DGSwitch1.ResetVC (self, hMesh);
	self:SetState ((self.component.istate:IsClosed() or self.component.istate:IsClosing()) and DGSwitch1.DOWN or DGSwitch1.UP)
end

--------------------------------------------------------------

function InnerLockSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		if self:GetState() == DGSwitch1.UP then
			self.component:OpenInnerLock()
		else
			self.component:CloseInnerLock()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function InnerLockSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		if self:GetState() == DGSwitch1.UP then
			self.component:OpenInnerLock()
		else
			self.component:CloseInnerLock()
		end
		return true
	end
	return false
end

return InnerLockSwitch
