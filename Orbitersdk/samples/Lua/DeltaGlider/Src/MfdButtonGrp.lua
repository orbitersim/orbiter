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

local MfdButtonGrp = Class(PanelElement)

function MfdButtonGrp:new (_subsys, _nbtn)
	PanelElement.new(self,_subsys:DG())
	self.subsys = _subsys
	self.nbtn = _nbtn

	self.ispushed = {}
	self.pending_action = 0
end

--------------------------------------------------------------

function MfdButtonGrp:PushButtonVC (hMesh, meshgrp, btn, down)
	if down == self.ispushed[btn] then return end -- nothing to do
	self.ispushed[btn] = down

	local nvtx_per_button = 12
	local depth = 0.004   -- button travel
	local tilt = 20.0*RAD -- inclination of MFD panel
	local dz_down =  depth*math.cos(tilt)
	local dy_down = -depth*math.sin(tilt)
	local dvtx = oapi.create_ntvertexarray(nvtx_per_button)
	local vofs = oapi.create_indexarray(nvtx_per_button)
	local dz = down and dz_down or -dz_down
	local dy = down and dy_down or -dy_down
	for i=1,nvtx_per_button do
		dvtx[i].y = dy;
		dvtx[i].z = dz;
		vofs[i] = btn*nvtx_per_button + i - 1
	end
	local ges = {}
	ges.flags = GRPEDIT.VTXCRDADDY+GRPEDIT.VTXCRDADDZ
	ges.Vtx = dvtx
	ges.vIdx = vofs
	oapi.edit_meshgroup (hMesh, meshgrp, ges)
end

return MfdButtonGrp
