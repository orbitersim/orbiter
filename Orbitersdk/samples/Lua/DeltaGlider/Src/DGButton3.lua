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

local DGButton3 = Class(PanelElement)

DGButton3.OFF = 0
DGButton3.ON =  1
DGButton3.PRESSED_FROM_OFF = 2
DGButton3.PRESSED_FROM_ON  = 3

local nvtx = 20
local nvtx_lbl = 8


--------------------------------------------------------------

function DGButton3:new (v)
	PanelElement.new(self, v)
	self.state = DGButton3.OFF
	self.vstate = DGButton3.OFF
end

--------------------------------------------------------------

function DGButton3:DefineAnimation2D (meshgrp, vofs)
	self.mgrp = meshgrp
	self.vtxofs = vofs
end

--------------------------------------------------------------

function DGButton3:DefineAnimationVC (axis, meshgrp, meshgrp_label, vofs, vofs_label)
	self.mgrp = meshgrp
	self.mgrp_lbl = meshgrp_label
	self.vtxofs = vofs
	self.vtxofs_lbl = vofs_label
	self.ax = axis
end

--------------------------------------------------------------

function DGButton3:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, self.mgrp)
end

--------------------------------------------------------------

function DGButton3:ResetVC (hMesh)
	self.vstate = DGButton3.OFF
end

--------------------------------------------------------------

function DGButton3:ProcessMouse2D (event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		self.state = self.state == DGButton3.OFF and DGButton3.PRESSED_FROM_OFF or DGButton3.PRESSED_FROM_ON
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		self.state = self.state == DGButton3.PRESSED_FROM_OFF and DGButton3.ON or DGButton3.OFF
	end
	return self.state ~= self.vstate
end

--------------------------------------------------------------

function DGButton3:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		self.state = self.state == DGButton3.OFF and DGButton3.PRESSED_FROM_OFF or DGButton3.PRESSED_FROM_ON
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		self.state = self.state == DGButton3.PRESSED_FROM_OFF and DGButton3.ON or DGButton3.OFF
	end
	return self.state ~= self.vstate
end

--------------------------------------------------------------

function DGButton3:RedrawVC (hMesh, surf)
	local zpos = {0, 0.005, 0.0065, 0.0065}
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

		-- animate label
		local vtx_lbl= oapi.create_ntvertexarray(nvtx_lbl)
		local vperm_lbl = oapi.create_indexarray(nvtx_lbl)

		for i=1,nvtx_lbl do
			vperm_lbl[i] = self.vtxofs_lbl+i-1
		end
		local grs_lbl = {}
		grs_lbl.Vtx = vtx_lbl
		grs_lbl.VtxPerm = vperm_lbl
		oapi.get_meshgroup (hMesh, self.mgrp_lbl, grs_lbl)
		for i=1,nvtx_lbl do
			vtx_lbl[i].pos = vec.add(vtx_lbl[i].pos, shift)
		end

		-- show/hide indicator
		local ges_flag = GRPEDIT.VTXCRD
		local have_ind = self.vstate ~= DGButton3.OFF
		local need_ind = self.state ~= DGButton3.OFF
		if have_ind ~= need_ind then
			vtx_lbl[7].tv = (need_ind and 1.5 or 10.5)/1024.0
			vtx_lbl[8].tv = (need_ind and 1.5 or 10.5)/1024.0
			ges_flag = bit.bor(ges_flag, GRPEDIT.VTXTEXV)
		end
		local ges_lbl = {}
		ges_lbl.flags = ges_flag
		ges_lbl.Vtx = vtx_lbl
		ges_lbl.vIdx = vperm_lbl
		
		oapi.edit_meshgroup (hMesh, self.mgrp_lbl, ges_lbl)

		self.vstate = self.state
	end
	return false
end

--------------------------------------------------------------

function DGButton3:SetState (newstate)
	self.state = newstate
end

function DGButton3:GetState() 
	return self.state
end

return DGButton3
