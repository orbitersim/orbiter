-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: InstrumentBrightnessDial.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

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
