-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DGDial1.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local DGDial1 = Class(PanelElement)

--------------------------------------------------------------

local nvtx = 76



function DGDial1:new (v, np, pos0, delta)
	PanelElement.new(self, v)
	self.npos = np
	self.p0 = pos0
	self.dp = delta
	self.pos = 0
	self.vpos = -1 -- undefined
end

--------------------------------------------------------------

function DGDial1:DefineAnimationVC (ref, axis, meshgrp, vtxofs)
	self.rf = ref
	self.ax = axis
	self.mgrp = meshgrp
	self.vofs = vtxofs
end

--------------------------------------------------------------

function DGDial1:ResetVC (hMesh)
	self.vpos = -1
end

--------------------------------------------------------------

function DGDial1:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if p.x < 0.5 then
			self:Left()
		else
			self:Right()
		end
	end
	return self.pos ~= self.vpos
end

--------------------------------------------------------------

function DGDial1:RedrawVC (hMesh, surf)
	if self.pos ~= self.vpos then
		local phi0 = self.vpos >= 0 and (self.p0 + self.vpos*self.dp) or 0.0
		local phi1 = self.p0 + self.pos*self.dp
		local dphi = phi0-phi1
		local R = mat.rotm(self.ax,dphi)

		local vtx = oapi.create_ntvertexarray(nvtx)
		local vperm = oapi.create_indexarray(nvtx)
		for i=1,nvtx do
			vperm[i] = self.vofs + i - 1
		end

		local grs = {}
		grs.Vtx = vtx
		grs.VtxPerm = vperm
		oapi.get_meshgroup (hMesh, self.mgrp, grs)
		for i = 1, nvtx do
			local p = vec.sub(vtx[i].pos, self.rf)
			local pt = mat.mul(R,p)
			vtx[i].pos = vec.add(pt, self.rf)
			vtx[i].normal = mat.mul(R, vtx[i].normal)
		end
		local ges = {}
		ges.flags = GRPEDIT.VTXCRD + GRPEDIT.VTXNML
		ges.Vtx = vtx
		ges.vIdx = vperm
		oapi.edit_meshgroup (hMesh, self.mgrp, ges)

		self.vpos = self.pos
	end
	return false
end

--------------------------------------------------------------

function DGDial1:SetPosition (newpos)
	if newpos ~= self.pos then
		self.pos = newpos
		return true
	end
	return false
end

--------------------------------------------------------------

function DGDial1:Left ()
	if self.pos > 0 then
		self.pos = self.pos - 1
	end
	return self.pos
end

--------------------------------------------------------------

function DGDial1:Right ()
	if self.pos < self.npos-1 then
		self.pos = self.pos + 1
	end
	return self.pos
end

function DGDial1:GetPosition()
	return self.pos
end

return DGDial1
