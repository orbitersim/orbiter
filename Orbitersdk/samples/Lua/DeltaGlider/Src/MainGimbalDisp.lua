-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: MainGimbalDisp.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP


local VC_GIMBAL_INDICATOR_LEFT_vofs = 48

local MAIN_PGIMBAL_RANGE = math.tan (5.0*RAD)
local MAIN_YGIMBAL_RANGE = 1.0/7.7
-- main engine pitch and yaw gimbal range (tan)

-- Lower front panel: tilt from vertical
local vc_lpanel_tilt = 0.622165

-- GIMBAL_INDICATOR (VC): rotation reference
local VC_GIMBAL_INDICATOR_ref = {_V(-0.27012,0.98457,7.18405),_V(-0.24598,0.98457,7.18405)}

local MainGimbalDisp = Class(PanelElement)

function MainGimbalDisp:new (gc)
	PanelElement.new (self, gc:DG())
	self.ctrl = gc
	self.pofs_cur = {0,0}
	self.yofs_cur = {0,0}
	self.pofs_cmd = {0,0}
	self.yofs_cmd = {0,0}
	self.vc_grp = {}
	self.vperm = oapi.create_indexarray(16)

	for i=1,16 do
		self.vperm[i] = i-1+VC_GIMBAL_INDICATOR_LEFT_vofs
	end
end

--------------------------------------------------------------

function MainGimbalDisp:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 116
end

--------------------------------------------------------------

function MainGimbalDisp:ResetVC (hMesh)
	self.vc_grp.Vtx = oapi.create_ntvertexarray(16)
	if oapi.get_meshgroup (hMesh, GRP_VC.VC_INSTR, self.vc_grp) ~= 0 then -- problems
		self.vc_grp.Vtx = nil
	end
end

--------------------------------------------------------------

function MainGimbalDisp:Redraw2D (surf)
	local dg = self.vessel
	local x0 = 197.5
	local xx =  42.0
	local y0 = 515.5
	local dx =  10.0
	local dy =  10.0

	for i=1,2 do
		local g = self.ctrl:MainPGimbal(i)
		local ofs = math.floor((g/MAIN_PGIMBAL_RANGE)*18+0.5)
		if ofs ~= self.pofs_cur[i] then
			for j = 1,4 do
				self.grp.Vtx[self.vtxofs+4*(i-1)+j].y = y0 + dy*math.floor((j-1)/2) + ofs
			end
			self.pofs_cur[i] = ofs
		end
		g = self.ctrl:MainYGimbal(i)
		ofs = math.floor((g/MAIN_YGIMBAL_RANGE)*18+0.5)
		if ofs ~= self.yofs_cur[i] then
			for j=1,4 do
				self.grp.Vtx[self.vtxofs+4*(i-1)+j].x = x0 + (i-1)*xx + dx*math.floor((j-1)%2) - ofs
			end
			self.yofs_cur[i] = ofs
		end
	end

	for i=1,2 do
		local g = self.ctrl:MainPGimbal(i, false)
		local ofs = math.floor((g/MAIN_PGIMBAL_RANGE)*18+0.5)
		if ofs ~= self.pofs_cmd[i] then
			for j = 1, 4 do
				self.grp.Vtx[self.vtxofs+8+4*(i-1)+j].y = y0 + dy*math.floor((j-1)/2) + ofs
			end
			self.pofs_cmd[i] = ofs
		end
		g = self.ctrl:MainYGimbal(i, false)
		ofs = math.floor((g/MAIN_YGIMBAL_RANGE)*18+0.5)
		if ofs ~= self.yofs_cmd[i] then
			for j=1,4 do
				self.grp.Vtx[self.vtxofs+8+4*(i-1)+j].x = x0 + (i-1)*xx + dx*((j-1)%2) - ofs
			end
			self.yofs_cmd[i] = ofs
		end
	end

	return false
end

--------------------------------------------------------------

function MainGimbalDisp:RedrawVC (hMesh, surf)
	local cosa = math.cos(vc_lpanel_tilt)
	local sina = math.sin(vc_lpanel_tilt)
	local indsize = 0.002586
	local xrange = 0.0103/MAIN_YGIMBAL_RANGE
	local yrange = 0.0103/MAIN_PGIMBAL_RANGE

	local dg = self.vessel
	local Vtx = self.vc_grp.Vtx
	if hMesh ~= nil and Vtx ~= nil then
		for i=1,2 do
			local dx = -self.ctrl:MainYGimbal(i)*xrange
			local dy = -self.ctrl:MainPGimbal(i)*yrange
			for j=1,4 do
				Vtx[4+(i-1)*8+j].x = VC_GIMBAL_INDICATOR_ref[i].x + dx + indsize*((j-1)%2 ~= 0 and 1 or -1)
				Vtx[4+(i-1)*8+j].y = dy + indsize*(j>2 and 1 or -1)
			end
			dx = -self.ctrl:MainYGimbal(i,false)*xrange
			dy = -self.ctrl:MainPGimbal(i,false)*yrange
			for j=1,4 do
				Vtx[(i-1)*8+j].x = VC_GIMBAL_INDICATOR_ref[i].x + dx + indsize*((j-1)%2 ~= 0 and 1 or -1)
				Vtx[(i-1)*8+j].y = dy + indsize*(j>2 and 1 or -1)
			end
		end
		for i = 1, 16 do
			local y = Vtx[i].y
			local z = ((i-1)%8) < 4 and -0.0002 or -0.0004
			Vtx[i].y = VC_GIMBAL_INDICATOR_ref[1].y + y*cosa - z*sina
			Vtx[i].z = VC_GIMBAL_INDICATOR_ref[1].z + y*sina + z*cosa
		end
		local ges = {}
		ges.flags = GRPEDIT.VTXCRD
		ges.Vtx = self.vc_grp.Vtx
		ges.vIdx = self.vperm
		oapi.edit_meshgroup (hMesh, GRP_VC.VC_INSTR, ges)
	end
	return false
end

return MainGimbalDisp
