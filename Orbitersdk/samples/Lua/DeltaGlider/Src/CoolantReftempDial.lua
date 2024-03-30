local PanelElement = require("PanelElement")

local CoolantReftempDial = Class(PanelElement)

function CoolantReftempDial:new (comp)
	PanelElement.new (self, comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function CoolantReftempDial:ProcessMouseVC (event, p)
	self.component:IncReftemp (p.x > 0.5)
	return true
end

return CoolantReftempDial
