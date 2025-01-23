-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: CockpitBrightnessDial.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

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
