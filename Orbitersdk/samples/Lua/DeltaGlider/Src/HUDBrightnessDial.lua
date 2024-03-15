local PanelElement = require("PanelElement")

local HUDBrightnessDial = Class(PanelElement)

function HUDBrightnessDial:new(hc)
	PanelElement.new (self, hc:DG())
	self.ctrl = hc
end

--------------------------------------------------------------

function HUDBrightnessDial:ProcessMouseVC (event, p)
	self.ctrl:ModHUDBrightness (p.x > 0.5)
	return true
end

return HUDBrightnessDial
