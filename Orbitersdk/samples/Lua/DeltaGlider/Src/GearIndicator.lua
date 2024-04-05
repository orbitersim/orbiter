-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: GearIndicator.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local PANEL2D_TEXW  = 2048  -- texture width
local VC_GEAR_INDICATOR_vofs = 8

local GearIndicator = Class(PanelElement)

function GearIndicator:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
	self.tofs = oapi.rand()
	self.light = true
end

--------------------------------------------------------------

function GearIndicator:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 72
end

--------------------------------------------------------------

function GearIndicator:ResetVC (hMesh)
	self.light = true
end

--------------------------------------------------------------

function GearIndicator:Redraw2D (surf)
	local texw = PANEL2D_TEXW -- texture width


	local xofs
	if self.component:GearState():IsClosed() then
		xofs = 1018
	elseif self.component:GearState():IsOpen() then
		xofs = 1030
	else
		local int, frac = math.modf(oapi.get_simtime() + self.tofs)
		local blink = frac < 0.5
		if blink then
			xofs = 1042
		else
			xofs = 1020
		end
	end

	for i=0, 2 do
		for j = 1, 4 do
			self.grp.Vtx[self.vtxofs+i*4+j].tu = (xofs + ((j-1)%2)*10)/texw
		end
	end
	return false
end

--------------------------------------------------------------

function GearIndicator:RedrawVC (hMesh, surf)
	if not hMesh then return false end

	local showlights
	if self.component:GearState():IsClosed() then
		showlights = false
	elseif self.component:GearState():IsOpen() then
		showlights = true
	else
		local int, frac = math.modf(oapi.get_simtime() + self.tofs)
		local showlights = frac < 0.5
	end

	if showlights ~= self.light then
		local ges = {}
		local nvtx = 2
		local vidx = oapi.create_indexarray({VC_GEAR_INDICATOR_vofs,VC_GEAR_INDICATOR_vofs+1})
		local v = {0.2427,0.3042}
		local vtx = oapi.create_ntvertexarray(nvtx)
		for i = 1, nvtx do
			vtx[i].tv = v[showlights and 2 or 1]
		end
		ges.flags = GRPEDIT.VTXTEXV
		ges.Vtx = vtx
		ges.vIdx = vidx
		oapi.edit_meshgroup (hMesh, GRP_VC.VC4_LIT, ges)
		self.light = showlights
	end
	return false
end

return GearIndicator
