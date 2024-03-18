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

--local HoverHoldComponent = require("HoverHoldComponent")
--HACK: circular dependency if we try to include HoverHoldComponent :\
local HoverHoldComponent = {}
HoverHoldComponent.HOLD_NONE = 0
HoverHoldComponent.HOLD_ALT = 1
HoverHoldComponent.HOLD_VSPD = 2

local HoverHoldAltIndicator = Class(PanelElement)


function HoverHoldAltIndicator:new (hhac, blitsrc)
	PanelElement.new (self, hhac:DG())
	self.ctrl = hhac
	self.bsrc = blitsrc
	self.btgt = nil
end

--------------------------------------------------------------

function HoverHoldAltIndicator:Reset2D (panelid, hMesh)
	self.btgt = oapi.get_texturehandle (hMesh, 3)
end

--------------------------------------------------------------

function HoverHoldAltIndicator:ResetVC (hMesh)
	self.btgt = oapi.get_texturehandle (self.ctrl:DG().vcmesh_tpl, 14)
	self.holdstr = "         "
	self.holdmode_disp = HoverHoldComponent.HOLD_NONE
	self.hold_disp = false
end

--------------------------------------------------------------

function HoverHoldAltIndicator:Redraw2D (surf)
	return self:Redraw()
end

--------------------------------------------------------------

function HoverHoldAltIndicator:RedrawVC (hMesh, hSurf)
	return self:Redraw()
end

--------------------------------------------------------------

function HoverHoldAltIndicator:Redraw ()
	local refresh = false
	if not self.btgt then return refresh end

	if self.holdmode_disp ~= self.ctrl.hovermode then
		oapi.blt (self.btgt, self.bsrc, 314, 2, (self.ctrl.hovermode-1)*25, 14, 25, 8)
		self.holdmode_disp = self.ctrl.hovermode
		refresh = true
	end

	if self.hold_disp ~= self.ctrl.active then
		oapi.blt (self.btgt, self.bsrc, 348, 2, self.ctrl.active and 51 or 78, 14, 27, 8)
		self.hold_disp = self.ctrl.active
		refresh = true
	end

	local cbuf = oapi.formatvalue(self.holdmode_disp == HoverHoldComponent.HOLD_ALT and self.ctrl.holdalt or self.ctrl.holdvspd, 4)
	if self.holdstr ~= cbuf then
		self.holdstr = cbuf
		self:UpdateReadout (cbuf)
		refresh = true
	end
	return refresh
end

--------------------------------------------------------------

function HoverHoldAltIndicator:UpdateReadout (str)
	local tgtx = 314
	local tgty = 12
	local srcy = 0
	local w = 8
	local h = 11

	local lut = {
		['0'] = 0,
		['1'] = 8,
		['2'] = 16,
		['3'] = 24,
		['4'] = 32,
		['5'] = 40,
		['6'] = 48,
		['7'] = 56,
		['8'] = 64,
		['9'] = 72,
		['.'] = 10*8,
		['+'] = 11*8,
		['-'] = 12*8,
		['k'] = 13*8,
		['M'] = 14*8,
		['G'] = 15*8,
	}

	local len = #str
	for i = 1, len do
		local c = str:sub(i,i)
		local srcx = lut[c] or 16*8
		oapi.blt (self.btgt, self.bsrc, tgtx, tgty, srcx, srcy, w, h)
		tgtx = tgtx + w
	end
end

return HoverHoldAltIndicator
