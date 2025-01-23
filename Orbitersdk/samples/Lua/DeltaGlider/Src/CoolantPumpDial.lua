-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: CoolantPumpDial.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

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
