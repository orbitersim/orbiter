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

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP

local HoverThrottle = Class(PanelElement)

function HoverThrottle:new (_ctrl)
	PanelElement.new(self, _ctrl:DG())
	self.ctrl = _ctrl
	self.ppos = 0.0
end

--------------------------------------------------------------

function HoverThrottle:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 56
end

--------------------------------------------------------------

function HoverThrottle:ResetVC (hMesh)
	self.sliderpos = -1
end

--------------------------------------------------------------

function HoverThrottle:Redraw2D (surf)
	-- constants for texture coordinates
	local tx_dy = 18.0
	local bb_y0 = 428.5

	local sy = {bb_y0,bb_y0,bb_y0+tx_dy,bb_y0+tx_dy}

	local dg = self.ctrl:DG()
	local level = dg:GetHoverThrusterLevel (1)
	local pos = -level*116.0
	if pos ~= self.ppos then
		for j=1,4 do
			self.grp.Vtx[self.vtxofs+j].y = sy[j]+pos
		end
		self.ppos = pos
	end
	return false
end

--------------------------------------------------------------

function HoverThrottle:ProcessMouse2D (event, mx, my)
	local dg = self.ctrl:DG()
	my = math.max (0, math.min (116, my-9))
	dg:set_thrustergrouplevel (dg.thg_hover, 1.0-my/116.0)
	return true
end

--------------------------------------------------------------

local pyvc = 0.0
function HoverThrottle:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then -- record which slider to operate
		pyvc = p.y
	else
		local dg = self.ctrl:DG()
		local lvl = math.max (0.0, math.min (1.0, dg:GetHoverThrusterLevel (1) + (p.y-pyvc)))
		if lvl < 0.01 then
			lvl = 0.0
		end
		for i=1,3 do
			dg:SetHoverThrusterLevel (i, lvl)
		end
		pyvc = p.y
	end
	return true
end

--------------------------------------------------------------

function HoverThrottle:RedrawVC (hMesh, surf)
	local dg = self.ctrl:DG()
	local level = dg:GetHoverThrusterLevel (1)
	local pos = level*500.0
	if pos ~= self.sliderpos then
		dg:set_animation (self.ctrl.anim_hoverthrottle, level)
		self.sliderpos = pos
	end
	return false
end

return HoverThrottle
