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

local DGSwitch2 = Class(PanelElement)

DGSwitch2.CENTER=0
DGSwitch2.UP=1
DGSwitch2.DOWN=2
DGSwitch2.VERT=0
DGSwitch2.HORZ=1
DGSwitch2.HORZ_RL=2


local nvtx = 28
local travel = 15.0*RAD
local PANEL2D_TEXW  = 2048  -- texture width

--------------------------------------------------------------

function DGSwitch2:new (v)
	PanelElement.new(self, v)

	self.orient = DGSwitch2.VERT
	self.state  = DGSwitch2.CENTER
	self.vstate = DGSwitch2.CENTER
end

--------------------------------------------------------------

function DGSwitch2:DefineAnimation2D (o, meshgrp, vofs)
	self.orient = o
	self.mgrp = meshgrp
	self.vtxofs = vofs
end

--------------------------------------------------------------

function DGSwitch2:DefineAnimationVC (ref, axis, meshgrp, vofs)
	self.rf = ref
	self.ax = axis
	self.mgrp = meshgrp
	self.vtxofs = vofs
end

--------------------------------------------------------------

function DGSwitch2:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, self.mgrp)
end

--------------------------------------------------------------

function DGSwitch2:ProcessMouse2D (event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if self.orient == DGSwitch2.VERT then
			self:SetState (my <  22 and DGSwitch2.UP or DGSwitch2.DOWN)
		else
			self:SetState (mx >= 22 and DGSwitch2.UP or DGSwitch2.DOWN)
		end
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		self:SetState (DGSwitch2.CENTER)
	end
	return self.state ~= self.vstate
end

--------------------------------------------------------------

function DGSwitch2:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		self:SetState (p.y < 0.5 and DGSwitch2.DOWN or DGSwitch2.UP)
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		self:SetState (DGSwitch2.CENTER)
	end
	return self.state ~= self.vstate
end

--------------------------------------------------------------

function DGSwitch2:Redraw2D (surf)
	local texw = PANEL2D_TEXW -- texture width
	if self.state ~= self.vstate then
		local ofs = self.state*16
		if self.orient == DGSwitch2.HORZ_RL and self.state ~= 0  then
			ofs = 48-ofs
		end
		for i=1,4 do
			self.grp.Vtx[self.vtxofs+i].tu = (1053.5+ofs+((i-1)%2)*15)/texw
		end
		self.vstate = self.state
	end
	return false
end

--------------------------------------------------------------

function DGSwitch2:RedrawVC (hMesh, surf)
	local phi = {0.0, travel, -travel};
	if self.state ~= self.vstate then
		local phi0 = phi[self.vstate+1]
		local phi1 = phi[self.state+1]
		local dphi = phi1-phi0;
		local R = mat.rotm(self.ax,dphi) -- rotation matrix from current to new state

		local vtx = oapi.create_ntvertexarray(nvtx)
		local vperm = oapi.create_indexarray(nvtx)

		for i=1,nvtx do
			vperm[i] = self.vtxofs + i - 1
		end

		local grs = {}
		grs.Vtx = vtx
		grs.VtxPerm = vperm

		oapi.get_meshgroup (hMesh, self.mgrp, grs)
		for i=1, nvtx do
			local p = vec.sub(vtx[i].pos, self.rf)
			local pt = mat.mul(R,p)
			vtx[i].pos = vec.add(pt, self.rf)
			p = vtx[i].normal
			pt = mat.mul(R,p)
			vtx[i].normal = pt
		end
		local ges={}
		ges.flags = GRPEDIT.VTXCRD+GRPEDIT.VTXNML
		ges.Vtx = vtx
		ges.vIdx = vperm
		oapi.edit_meshgroup (hMesh, self.mgrp, ges)
		self.vstate = self.state
	end
	return false
end

--------------------------------------------------------------

function DGSwitch2:SetState (s)
	if self.state ~= s then
		self.state = s
		return true
	end
	return false
end



function DGSwitch2:GetState()
	return self.state
end

return DGSwitch2
