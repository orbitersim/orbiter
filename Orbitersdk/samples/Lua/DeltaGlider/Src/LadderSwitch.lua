local DGSwitch1 = require("DGSwitch1")

local LadderSwitch = Class(DGSwitch1)

function LadderSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.SPRING)
	self.component = comp
end

--------------------------------------------------------------

function LadderSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		if self:GetState() == DGSwitch1.UP then
			self.component:RetractLadder()
		elseif self:GetState() == DGSwitch1.DOWN then
			self.component:ExtendLadder()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function LadderSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		if self:GetState() == DGSwitch1.UP then
			self.component:RetractLadder()
		elseif self:GetState() == DGSwitch1.DOWN then
			self.component:ExtendLadder()
		end
		return true
	end
	return false
end

return LadderSwitch
