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
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP
local VC_RCOVER_INDICATOR_vofs = 23

local RetroCoverIndicator = Class(PanelElement)

function RetroCoverIndicator:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp

	self.vlight_2D = false
	self.vlight_VC = false
end

--------------------------------------------------------------

function RetroCoverIndicator:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 188
	self.vlight_2D = false
end

--------------------------------------------------------------

function RetroCoverIndicator:ResetVC (hMesh)
	self.vlight_VC = false
end

--------------------------------------------------------------

function RetroCoverIndicator:Redraw2D (surf)
--	oapi.dbg_out(string.format("RetroCoverIndicator %f", oapi.get_simtime()))
	local state = self.component:State()

	local int, frac = math.modf(oapi.get_simtime()*1.7)
	local blink = frac < 0.5
	--oapi.dbg_out(string.format("%f %f %f %s %f", state:State(), state:Speed(), frac, tostring(blink), oapi.get_simtime()))
	local showlights = state:State() == 1.0 or (state:Speed() ~= 0 and blink)
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

function RetroCoverIndicator:RedrawVC (hMesh, surf)
	if not hMesh then return false end
	local state = self.component:State()

	local int, frac = math.modf(oapi.get_simtime()*1.7)
	local blink = frac < 0.5

	local showlights = state:State() == 1.0 or (state:Speed() ~= 0 and blink)

	if showlights ~= self.vlight_VC then
		local vtxofs = VC_RCOVER_INDICATOR_vofs

		local vidx = oapi.create_indexarray({vtxofs,vtxofs+1,vtxofs+2,vtxofs+3})
		local u = {0.0586,0.0713}
		
		local vtx = oapi.create_ntvertexarray(4)
		for i=1,4 do
			vtx[i].tu = u[showlights and 2 or 1]
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

return RetroCoverIndicator
