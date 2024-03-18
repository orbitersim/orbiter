-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local DGPanelElement = Class(PanelElement)

function DGPanelElement:new(v)
	PanelElement.new(self, v)
	self.dg = v
end

return DGPanelElement
