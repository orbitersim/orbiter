local PanelElement = require("PanelElement")

local CoolantPumpDial = Class(PanelElement)

function CoolantPumpDial:new (comp)
	PanelElement.new (self, comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function CoolantPumpDial:ProcessMouseVC (event, p)
	self.component:IncPumprate (p.x > 0.5)
	return true
end


return CoolantPumpDial
