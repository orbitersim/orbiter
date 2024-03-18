local DGSwitch1 = require("DGSwitch1")

local HUDUpDownSwitch = Class(DGSwitch1)

function HUDUpDownSwitch:new (hc)
	DGSwitch1.new(self, hc:DG(), DGSwitch1.SPRING)
	self.ctrl = hc
end

--------------------------------------------------------------

function HUDUpDownSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		if self:GetState() == DGSwitch1.UP then
			self.ctrl:RetractHud()
		elseif self:GetState() == DGSwitch1.DOWN then
			self.ctrl:ExtendHud()
		end
		return true
	end
	return false
end

return HUDUpDownSwitch
