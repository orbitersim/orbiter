-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HoverDisp.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local VC_HOVER_INDICATOR_vofs = 64
local PHOVER_RANGE = 10.0*RAD
local RHOVER_RANGE = 10.0*RAD
-- max hover-induced pitch and roll values

-- Lower front panel: tilt from vertical
local vc_lpanel_tilt = 0.622165
-- HOVER_INDICATOR (VC): rotation reference
local VC_HOVER_INDICATOR_ref = _V(-0.16158,0.98457,7.18405)

local HoverDisp = Class(PanelElement)

function HoverDisp:new (_ctrl)
	PanelElement.new(self, _ctrl:DG())
	self.ctrl = _ctrl

	self.pofs_cur = 0
	self.rofs_cur = 0
	self.pofs_cmd = 0
	self.rofs_cmd = 0

	self.vc_grp = {}

	self.vperm = oapi.create_indexarray(8)
	for i=1,8 do
		self.vperm[i] = i+VC_HOVER_INDICATOR_vofs-1
	end
end

--------------------------------------------------------------

function HoverDisp:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 152
end

--------------------------------------------------------------

function HoverDisp:ResetVC (hMesh)
	self.vc_grp.Vtx = oapi.create_ntvertexarray(8)
	if oapi.get_meshgroup (hMesh, GRP_VC.VC_INSTR, self.vc_grp) ~= 0 then -- problems
		self.vc_grp.Vtx = nil
	end
end

--------------------------------------------------------------

function HoverDisp:Redraw2D (surf)
	local x0 = 371.5
	local y0 = 515.5
	local dx =  10.0
	local dy =  10.0
	local g
	local ofs

	g = math.max (-PHOVER_RANGE, math.min(PHOVER_RANGE, self.ctrl:PHover()))
	ofs = math.floor((g/PHOVER_RANGE)*18+0.5)
	if ofs ~= self.pofs_cur then
		for j=1,4 do
			self.grp.Vtx[self.vtxofs+j].y = y0 + dy*math.floor((j-1)/2) + ofs
		end
		self.pofs_cur = ofs
	end
	g = math.max (-RHOVER_RANGE, math.min(RHOVER_RANGE, self.ctrl:RHover()))
	ofs = math.floor((g/RHOVER_RANGE)*18+0.5)
	if ofs ~= self.rofs_cur then
		for j=1,4 do
			self.grp.Vtx[self.vtxofs+j].x = x0 + dx*((j-1)%2) - ofs
		end
		self.rofs_cur = ofs
	end

	g = math.max (-PHOVER_RANGE, math.min(PHOVER_RANGE, self.ctrl:PHover(false)))
	ofs = math.floor((g/PHOVER_RANGE)*18+0.5)
	if ofs ~= self.pofs_cmd then
		for j=1,4 do
			self.grp.Vtx[self.vtxofs+4+j].y = y0 + dy*math.floor((j-1)/2) + ofs
		end
		self.pofs_cmd = ofs
	end
	g = math.max (-RHOVER_RANGE, math.min(RHOVER_RANGE, self.ctrl:RHover(false)))
	ofs = math.floor((g/RHOVER_RANGE)*18+0.5)
	if ofs ~= self.rofs_cmd then
		for j=1,4 do
			self.grp.Vtx[self.vtxofs+4+j].x = x0 + dx*((j-1)%2) - ofs
		end
		self.rofs_cmd = ofs
	end

	return false
end

--------------------------------------------------------------

function HoverDisp:RedrawVC (hMesh, surf)
	local cosa = math.cos(vc_lpanel_tilt)
	local sina = math.sin(vc_lpanel_tilt)
	local indsize = 0.002586
	local xrange = 0.0103/RHOVER_RANGE
	local yrange = 0.0103/PHOVER_RANGE

	local Vtx = self.vc_grp.Vtx
	if hMesh and Vtx then
		local dx = -math.max (-RHOVER_RANGE, math.min(RHOVER_RANGE, self.ctrl:RHover()))*xrange
		local dy = -math.max (-PHOVER_RANGE, math.min(PHOVER_RANGE, self.ctrl:PHover()))*yrange
		for j=1,4 do
			Vtx[4+j].x = VC_HOVER_INDICATOR_ref.x + dx + indsize*((j-1)%2 ~= 0 and 1 or -1)
			Vtx[4+j].y = dy + indsize*(math.floor((j-1)/2) ~= 0 and 1 or -1)
		end
		dx = -self.ctrl:RHover(false)*xrange
		dy = -self.ctrl:PHover(false)*yrange
		for j=1,4 do
			Vtx[j].x = VC_HOVER_INDICATOR_ref.x + dx + indsize*((j-1)%2 ~= 0 and 1 or -1)
			Vtx[j].y = dy + indsize*(math.floor((j-1)/2) ~=0 and 1 or -1)
		end
		for i=1,8 do
			local y = Vtx[i].y
			local z = i < 5 and -0.0002 or -0.0004
			Vtx[i].y = VC_HOVER_INDICATOR_ref.y + y*cosa - z*sina
			Vtx[i].z = VC_HOVER_INDICATOR_ref.z + y*sina + z*cosa
		end
		local ges = {}
		ges.flags = GRPEDIT.VTXCRD
		ges.Vtx = Vtx
		ges.vIdx = self.vperm
		oapi.edit_meshgroup (hMesh, GRP_VC.VC_INSTR, ges)
	end
	return false
end

return HoverDisp
