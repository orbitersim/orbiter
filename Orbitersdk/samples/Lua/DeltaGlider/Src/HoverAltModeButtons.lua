-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HoveAltModeButtons.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")
local DGButton3 = require("DGButton3")
--local HoverHoldComponent = require("HoverHoldComponent")
--HACK: circular dependency if we try to include HoverHoldComponent :\
local HoverHoldComponent = {}
HoverHoldComponent.HOLD_NONE = 0
HoverHoldComponent.HOLD_ALT = 1
HoverHoldComponent.HOLD_VSPD = 2

local PANEL2D_TEXH  = 1024  -- texture height


local HoverAltModeButtons = Class(PanelElement)

function HoverAltModeButtons:new (hhac)
	PanelElement.new (self, hhac:DG())
	self.ctrl = hhac

	self.vmode = HoverHoldComponent.HOLD_NONE
	self.btn = {
		DGButton3 (self.vessel),
		DGButton3 (self.vessel)
	}
end

--------------------------------------------------------------

function HoverAltModeButtons:DefineAnimation2D (meshgrp, vofs)
	self.gidx = meshgrp
	self.vtxofs = vofs
end

--------------------------------------------------------------

function HoverAltModeButtons:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, self.gidx)
end

--------------------------------------------------------------

function HoverAltModeButtons:DefineAnimationsVC (axis, meshgrp, meshgrp_label, vofs, vofs_label)
	self.btn[1]:DefineAnimationVC (axis, meshgrp, meshgrp_label, vofs[1], vofs_label[1])
	self.btn[2]:DefineAnimationVC (axis, meshgrp, meshgrp_label, vofs[2], vofs_label[2])
end

--------------------------------------------------------------

function HoverAltModeButtons:RedrawVC (hMesh, surf)
	self.btn[1]:RedrawVC (hMesh, surf)
	self.btn[2]:RedrawVC (hMesh, surf)
	return false
end

--------------------------------------------------------------

function HoverAltModeButtons:ProcessMouse2D (event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		self.ctrl:SetHoverMode (mx < 27 and HoverHoldComponent.HOLD_ALT or HoverHoldComponent.HOLD_VSPD)
		return true
	end
	return false
end

--------------------------------------------------------------

function HoverAltModeButtons:Redraw2D (hSurf)
	local texh = PANEL2D_TEXH
	local ofs = self.ctrl:GetHoverMode() == HoverHoldComponent.HOLD_ALT and 0 or 30
	for i=1,4 do
		self.grp.Vtx[self.vtxofs+i].tv = (391+ofs+math.floor((i-1)/2)*30)/texh
	end
	return false
end

--------------------------------------------------------------

function HoverAltModeButtons:ProcessMouseVC (event, p)
	local ix = math.floor(p.x*64.0)
	local b = math.floor(ix/33)
	if ix-b*33 >= 30 then return false end

	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		self.btn[1]:SetState (0==b and DGButton3.PRESSED_FROM_OFF or DGButton3.OFF)
		self.btn[2]:SetState (1==b and DGButton3.PRESSED_FROM_OFF or DGButton3.OFF)
		self.vmode = b+1
		self.ctrl:SetHoverMode (self.vmode)
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		self.btn[b+1]:SetState (DGButton3.ON)
	end
	return true
end

--------------------------------------------------------------

function HoverAltModeButtons:ResetVC (hMesh)
	self.btn[1]:ResetVC (hMesh)
	self.btn[2]:ResetVC (hMesh)

	if self.vmode ~= self.ctrl:GetHoverMode() then
		self.vmode = self.ctrl:GetHoverMode()
		self.btn[1]:SetState (1 == self.vmode and DGButton3.ON or DGButton3.OFF)
		self.btn[2]:SetState (2 == self.vmode and DGButton3.ON or DGButton3.OFF)
	end
end

return HoverAltModeButtons
