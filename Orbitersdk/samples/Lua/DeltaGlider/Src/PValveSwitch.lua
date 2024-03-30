local DGSwitch1 = require("DGSwitch1")

local PValveSwitch = Class(DGSwitch1)

function PValveSwitch:new (_subsys, id)
	DGSwitch1.new(self, _subsys:DG(), DGSwitch1.TWOSTATE)
	self.subsys = _subsys
	self.vid = id
end

--------------------------------------------------------------

function PValveSwitch:Reset2D (panelid, hMesh)
	self:SetState (self.subsys:GetPValve(self.vid) and DGSwitch1.UP or DGSwitch1.DOWN)
end

--------------------------------------------------------------

function PValveSwitch:ResetVC (hMesh)
	DGSwitch1.ResetVC (self, hMesh)
	self:SetState (self.subsys:GetPValve(self.vid) and DGSwitch1.UP or DGSwitch1.DOWN)
end

--------------------------------------------------------------

function PValveSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		local state = self:GetState()
		self.subsys:SetPValve (self.vid, state == DGSwitch1.UP)
		return true
	end
	return false
end

--------------------------------------------------------------

function PValveSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		local state = self:GetState()
		self.subsys:SetPValve (self.vid, state==DGSwitch1.UP)
		return true
	end
	return false
end

return PValveSwitch
