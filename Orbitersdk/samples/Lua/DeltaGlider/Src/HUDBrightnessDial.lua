-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HUDBrightnessDial.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

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
