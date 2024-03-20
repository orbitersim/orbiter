local PanelElement = require("PanelElement")

local CockpitBrightnessDial = Class(PanelElement)

function CockpitBrightnessDial:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function CockpitBrightnessDial:ProcessMouseVC (event, p)
	self.component:ModBrightness (p.x > 0.5)
	return true
end


return CockpitBrightnessDial
