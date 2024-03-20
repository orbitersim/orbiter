local PanelElement = require("PanelElement")

local InstrumentBrightnessDial = Class(PanelElement)


function InstrumentBrightnessDial:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function InstrumentBrightnessDial:ProcessMouseVC (event, p)
	self.component:ModBrightness (p.x > 0.5)
	return true
end


return InstrumentBrightnessDial
