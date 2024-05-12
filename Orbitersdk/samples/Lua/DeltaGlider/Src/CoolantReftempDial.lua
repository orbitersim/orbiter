-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: CoolantReftempDial.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

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
