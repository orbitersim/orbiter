-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DocksealIndicator.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local VC_SEAL_INDICATOR_vofs = 31

local DocksealIndicator = Class(PanelElement)

function DocksealIndicator:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp

	self.vlight_2D = false
	self.vlight_VC = false
end

--------------------------------------------------------------

function DocksealIndicator:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 196
end

--------------------------------------------------------------

function DocksealIndicator:ResetVC (hMesh)
	self.vlight_VC = false
end

--------------------------------------------------------------

function DocksealIndicator:Redraw2D (surf)
	local showlights = false
	if self.component.isDocked then
		local dt = oapi.get_simtime() - self.component.dockTime
		if dt > 10.0 then
			showlights = true
		else
			local int, frac = math.modf(dt)
			showlights = frac < 0.5
		end
	end
	if showlights ~= self.vlight_2D then
		local v = (showlights and 420.0 or 412.0)/1024.0
		for i=1,4 do
			self.grp.Vtx[self.vtxofs+i].tv = v
		end
		self.vlight_2D = showlights
	end
	return false
end

--------------------------------------------------------------

function DocksealIndicator:RedrawVC (hMesh, surf)
	if not hMesh then return false end

	local showlights = false
	if self.component.isDocked then
		local dt = oapi.get_simtime() - self.component.dockTime
		if dt > 10.0 then
			showlights = true
		else
			local int, frac = math.modf(dt)
			showlights = frac < 0.5
		end
	end

	if showlights ~= self.vlight_VC then
		local vtxofs = VC_SEAL_INDICATOR_vofs
		local vidx = oapi.create_indexarray({vtxofs,vtxofs+1,vtxofs+2,vtxofs+3})
		local vtx = oapi.create_ntvertexarray(4)
		for i = 1,4 do
			vtx[i].tu = showlights and 0.0713 or 0.0586
		end
		local ges = {}
		ges.flags = GRPEDIT.VTXTEXU
		ges.Vtx = vtx
		ges.vIdx = vidx
		oapi.edit_meshgroup (hMesh, GRP_VC.VC4_LIT, ges)
		self.vlight_VC = showlights
	end
	return false
end

return DocksealIndicator
