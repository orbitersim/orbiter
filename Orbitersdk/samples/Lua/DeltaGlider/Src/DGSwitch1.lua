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


local DGSwitch1 = Class(PanelElement)
DGSwitch1.TWOSTATE = 0
DGSwitch1.THREESTATE = 1
DGSwitch1.SPRING = 2

DGSwitch1.CENTER = 0
DGSwitch1.UP = 1
DGSwitch1.DOWN = 2

local travel = 28.0*RAD
local nvtx = 33
local tu0 = {53.0/2048.0, 105.0/2048.0, 79.0/2048.0}
local tv0 = 337.0/1024.0
local tw  =  26.0/2048.0
local th  =  52.0/1024.0

--------------------------------------------------------------

function DGSwitch1:new (v, m)
	PanelElement.new(self, v)
	self.mode = m
	-- we always initiate as centered, even for 2state switches
	self.state   = DGSwitch1.CENTER
	self.vstate  = DGSwitch1.CENTER
	self.vstate2 = DGSwitch1.CENTER
end

 --------------------------------------------------------------

function DGSwitch1:DefineAnimationVC (ref, axis,meshgrp, vtxofs)
	self.rf = ref
	self.ax = axis
	self.mgrp = meshgrp
	self.vofs = vtxofs
end

--------------------------------------------------------------

function DGSwitch1:DefineAnimation2D (hMesh, meshgrp, vtxofs)
	self.grp = oapi.mesh_group (hMesh, meshgrp)
	self.mgrp = meshgrp
	self.vofs = vtxofs
end

--------------------------------------------------------------

function DGSwitch1:ResetVC (hMesh)
	PanelElement.ResetVC (self.hMesh)
	self.vstate = DGSwitch1.CENTER
end

--------------------------------------------------------------

function DGSwitch1:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if p.y < 0.5 then
			self:Down()
		else
			self:Up()
		end
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		if self.mode == DGSwitch1.SPRING then
			self:SetState (DGSwitch1.CENTER)
		end
	end
	return self.state ~= self.vstate
end

--------------------------------------------------------------

function DGSwitch1:ProcessMouse2D (event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if my < 26 then
			self:Up()
		else
			self:Down()
		end
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		if self.mode == DGSwitch1.SPRING then
			self:SetState (DGSwitch1.CENTER)
		end
	end
	return self.state ~= self.vstate2
end

--------------------------------------------------------------

function DGSwitch1:RedrawVC (hMesh, surf)
	local phi = {0.0, travel, -travel}
	if self.state ~= self.vstate then
		local phi0 = phi[self.vstate+1]
		local phi1 = phi[self.state+1]
		local dphi = phi1-phi0
		local R = mat.rotm(self.ax,dphi) -- rotation matrix from current to new state

		local vtx = oapi.create_ntvertexarray(nvtx)
		local vperm = oapi.create_indexarray(nvtx)
		for i=1, nvtx do
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
			p = vtx[i].normal
			pt = mat.mul(R,p)
			vtx[i].normal = pt
		end
		local ges = {}
		ges.flags = GRPEDIT.VTXCRD + GRPEDIT.VTXNML
		ges.Vtx = vtx
		ges.vIdx = vperm
		oapi.edit_meshgroup (hMesh, self.mgrp, ges)
		self.vstate = self.state
	end
	return false
end

--------------------------------------------------------------

function DGSwitch1:Redraw2D (surf)
	if self.state ~= self.vstate2 then
		for i=1, 4 do
			self.grp.Vtx[self.vofs+i].tu = tu0[self.state+1] + (i%2 ~= 0 and tw or 0)
		end
		self.vstate2 = self.state
	end
	return false
end

--------------------------------------------------------------

function DGSwitch1:SetState (s)
	-- note: it is admissable to force a 2-state switch to center position
	if self.state ~= s then
		self.state = s
		return true
	end
	return false
end

--------------------------------------------------------------

function DGSwitch1:Up ()
	if self.state ~= DGSwitch1.UP then
		self:SetState ((self.state == DGSwitch1.DOWN and self.mode ~= DGSwitch1.TWOSTATE) and DGSwitch1.CENTER or DGSwitch1.UP)
	end
	return self.state
end

--------------------------------------------------------------

function DGSwitch1:Down ()
	if self.state ~= DGSwitch1.DOWN then
		self:SetState ((self.state == DGSwitch1.UP and self.mode ~= DGSwitch1.TWOSTATE) and DGSwitch1.CENTER or DGSwitch1.DOWN)
	end
	return self.state
end

function DGSwitch1:GetState()
	return self.state
end

return DGSwitch1
