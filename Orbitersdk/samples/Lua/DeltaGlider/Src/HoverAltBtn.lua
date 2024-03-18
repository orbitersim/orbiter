-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGButton3 = require("DGButton3")

local PANEL2D_TEXH  = 1024  -- texture height

local HoverAltBtn = Class(DGButton3)

function HoverAltBtn:new (hhac)
	DGButton3.new (self, hhac:DG())
	self.ctrl = hhac
end

--------------------------------------------------------------

function HoverAltBtn:ProcessMouse2D (event, mx, my)
	if DGButton3.ProcessMouse2D (self, event, mx, my) then
		local state = self:GetState()
		if state == DGButton3.OFF then
			self.ctrl:Activate (false)
		elseif state == DGButton3.ON then
			self.ctrl:Activate (true)
		end
	end
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

--------------------------------------------------------------

function HoverAltBtn:Redraw2D (hSurf)
	local texh = PANEL2D_TEXH
	if self.state ~= self.vstate then
		local ofs = self.state == DGButton3.OFF and 0 or 30
		for i=1,4 do
			self.grp.Vtx[self.vtxofs+i].tv = (391+ofs+math.floor((i-1)/2)*30)/texh
		end
		self.vstate = self.state
	end
	return false
end

--------------------------------------------------------------

function HoverAltBtn:ProcessMouseVC (event, p)
	if DGButton3.ProcessMouseVC (self, event, p) then
		local state = self:GetState()
		if state == DGButton3.OFF then
			self.ctrl:Activate (false)
		elseif state == DGButton3.ON then
			self.ctrl:Activate (true)
		end
	end
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

return HoverAltBtn
