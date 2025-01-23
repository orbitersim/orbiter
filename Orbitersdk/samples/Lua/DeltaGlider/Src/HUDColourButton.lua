-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HUDColourButton.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGButton2 = require("DGButton2")

local HUDColourButton = Class(DGButton2)

function HUDColourButton:new (hc)
	DGButton2.new(self, hc:DG())
	self.ctrl = hc
end

--------------------------------------------------------------

function HUDColourButton:ProcessMouseVC (event, p)
	DGButton2.ProcessMouseVC (self, event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		oapi.toggle_hudcolour ()
	end
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

return HUDColourButton
