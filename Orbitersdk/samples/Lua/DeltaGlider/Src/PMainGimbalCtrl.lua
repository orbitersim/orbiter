-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: PMainGimbalCtrl.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local nvtx_per_switch = 28

-- GIMBAL_PSWITCH (VC): rotation reference
local VC_GIMBAL_PSWITCH_ref = _V(-0.21342,1.01432,7.20581)

-- GIMBAL_PSWITCH (VC): rotation axis
local VC_GIMBAL_PSWITCH_axis = _V(1.00000,0.00000,0.00000)


local PMainGimbalCtrl = Class(PanelElement)

function PMainGimbalCtrl:new (gc)
	PanelElement.new(self, gc:DG())
	self.ctrl = gc
	self.vc_state = {0, 0}
	self.vtx0 = oapi.create_ntvertexarray(nvtx_per_switch*2)
end

--------------------------------------------------------------

function PMainGimbalCtrl:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 132
end

--------------------------------------------------------------

function PMainGimbalCtrl:ResetVC (hMesh)
	local grs = {}
	grs.Vtx = self.vtx0
	grs.nVtx = nvtx_per_switch*2
	oapi.get_meshgroup (hMesh, GRP_VC.SWITCH2, grs)
end

--------------------------------------------------------------
local PANEL2D_TEXW  = 2048  -- texture width

function PMainGimbalCtrl:Redraw2D (surf)
	local texw = PANEL2D_TEXW -- texture width
	for i = 1,2 do
		local state = self.ctrl.mpswitch[i]
		for j = 1, 4 do
			self.grp.Vtx[self.vtxofs+(i-1)*4+j].tu = (1053.5+state*16+((j-1)%2)*15)/texw
		end
	end
	return false
end

--------------------------------------------------------------

function PMainGimbalCtrl:RedrawVC (hMesh, surf)
	local ref = VC_GIMBAL_PSWITCH_ref
	local tilt = {0,15*RAD,-15*RAD}

	local redraw = false
	for i=1,2 do
		local state = self.ctrl.mpswitch[i]
		if state ~= self.vc_state[i] then
			self.vc_state[i] = state
			redraw = true
		end
	end
	if not redraw then return false end

	local vtx = self.vtx0:copy()

	for i=1,2 do
		local ofs = (i-1)*nvtx_per_switch
		local state = self.vc_state[i]
		if state ~= 0 then
			local R = mat.rotm(VC_GIMBAL_PSWITCH_axis,tilt[state+1])
			for j=1, nvtx_per_switch do
				local v = vec.sub(vtx[ofs+j].pos, ref)
				local vr = mat.mul(R,v)
				vtx[ofs+j].pos = vec.add(vr, ref)
				local n = vtx[ofs+j].normal
				local nr = mat.mul(R,n)
				vtx[ofs+j].normal = nr
			end
		end
	end

	local grpid = GRP_VC.SWITCH2
	local ges = {}
	ges.flags = GRPEDIT.VTXCRD + GRPEDIT.VTXNML
	ges.Vtx = vtx
	oapi.edit_meshgroup (hMesh, grpid, ges)
	return false
end

--------------------------------------------------------------

local state2d = 0
local mode2d = 0
function PMainGimbalCtrl:ProcessMouse2D (event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if mx < 10 then
			state2d = 1
		elseif mx >= 25 then
			state2d = 2
		else
			state2d = 3
		end
		if my < 22 then
			mode2d = 2
		else
			mode2d = 1
		end
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		state2d = 0
	end
	return self.ctrl:IncMainPGimbal (state2d, mode2d)
end

--------------------------------------------------------------

local statevc = 0
local modevc = 0
function PMainGimbalCtrl:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if p.x < 0.25 then
			statevc = 1
		elseif p.x > 0.75 then
			statevc = 2
		else
			statevc = 3
		end
		if p.y < 0.5 then
			modevc = 1
		else
			modevc = 2
		end
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		statevc = 0
	end
	self.ctrl:IncMainPGimbal (statevc, modevc)
	return bit.anyset(event, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
end

return PMainGimbalCtrl
