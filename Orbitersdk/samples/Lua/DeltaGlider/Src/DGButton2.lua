-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DGButton2.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local DGButton2 = Class(PanelElement)

DGButton2.OFF = 0
DGButton2.ON = 1

local nvtx = 20

--------------------------------------------------------------

function DGButton2:new (v)
	PanelElement.new(self, v)
	self.state = DGButton2.OFF
	self.vstate = DGButton2.OFF
end

--------------------------------------------------------------

function DGButton2:DefineAnimationVC (axis, meshgrp, vofs)
	self.mgrp = meshgrp
	self.vtxofs = vofs
	self.ax = axis
end

--------------------------------------------------------------

function DGButton2:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		self.state = DGButton2.ON
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		self.state = DGButton2.OFF
	end
	return self.state ~= self.vstate
end

--------------------------------------------------------------

function DGButton2:RedrawVC (hMesh, surf)
	local zpos = {0, 0.004}
	if self.state ~= self.vstate then
		local dz = zpos[self.state+1]-zpos[self.vstate+1]
		local shift = vec.mul(self.ax, dz)

		-- animate button
		local vtx = oapi.create_ntvertexarray(nvtx)
		local vperm = oapi.create_indexarray(nvtx)
		for i=1, nvtx do
			vperm[i] = self.vtxofs+i-1
		end

		local grs = {}
		grs.Vtx = vtx
		grs.VtxPerm = vperm

		oapi.get_meshgroup (hMesh, self.mgrp, grs)
		for i=1,nvtx do
			vtx[i].pos = vec.add(vtx[i].pos, shift)
		end
		local ges = {}
		ges.flags = GRPEDIT.VTXCRD
		ges.Vtx = vtx
		ges.vIdx = vperm
		oapi.edit_meshgroup (hMesh, self.mgrp, ges)

		self.vstate = self.state
	end
	return false
end

--------------------------------------------------------------

function DGButton2:SetState (newstate)
	self.state = newstate
end

function DGButton2:GetState()
	return self.state
end

return DGButton2
