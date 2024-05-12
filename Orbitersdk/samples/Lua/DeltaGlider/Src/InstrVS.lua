-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: InstrVS.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP


local VC_VSTAPE_vofs = 24
local VC_VS_READOUT_vofs = 28
local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024


local InstrVS = Class(PanelElement)

function InstrVS:new (v)
	PanelElement.new (self, v)

	self.pvmin = 100000 -- invalidate

	self.vperm = oapi.create_indexarray(4)
	for i=1,4 do
		self.vperm[i] = i + VC_VSTAPE_vofs - 1
	end

	self.vc_grp = {}
	self.vc_grp.VtxPerm = self.vperm
	self.vc_grp.nVtx = 4

	self.vperm_readout = oapi.create_indexarray(20)
	for i=1,20 do
		self.vperm_readout[i] = i+VC_VS_READOUT_vofs - 1
	end

	self.vc_grp_readout = {}
	self.vc_grp_readout.VtxPerm = self.vperm_readout
	self.vc_grp_readout.nVtx = 20
end

--------------------------------------------------------------

function InstrVS:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_BELOW)
	self.sf = oapi.get_texturehandle (hMesh, self.grp.TexIdx+1)
	self.vtxofs = 160
end

--------------------------------------------------------------

function InstrVS:ResetVC (hMesh)
	if not self.vc_grp.Vtx then
		self.vc_grp.Vtx = oapi.create_ntvertexarray(self.vc_grp.nVtx)
	end
	if not self.vc_grp_readout.Vtx then
		self.vc_grp_readout.Vtx = oapi.create_ntvertexarray(self.vc_grp_readout.nVtx)
	end
	
	if oapi.get_meshgroup (hMesh, GRP_VC.VC_INSTR, self.vc_grp) ~= 0 then -- problems
		self.vc_grp.Vtx = nil
	end
	if oapi.get_meshgroup (hMesh, GRP_VC.VC_INSTR, self.vc_grp_readout) ~= 0 then -- problems
		self.vc_grp_readout.Vtx = nil
	end
	self.sf = oapi.get_texturehandle (self.vessel.vcmesh_tpl, 19)
end

--------------------------------------------------------------

function InstrVS:AddMeshData2D (hMesh, grpidx)
	local texw = INSTR3D_TEXW
	local texh = INSTR3D_TEXH
	local tapex0 = texw-197.5
	local tapew = 41.0
	local tapey0 = texh-764
	local tapeh = 512.0
	local xcnt = 682.0
	local ycnt = 311.0

	local VTX = oapi.create_ntvertexarray({
		-- VS tape
		{xcnt-22,ycnt-59,0,  0,0,0,  tapex0/texw,        tapey0/texh},
		{xcnt+22,ycnt-59,0,  0,0,0,  (tapex0+tapew)/texw,tapey0/texh},
		{xcnt-22,ycnt+59,0,  0,0,0,  tapex0/texw,        (tapey0+tapeh)/texh},
		{xcnt+22,ycnt+59,0,  0,0,0,  (tapex0+tapew)/texw,(tapey0+tapeh)/texh},
		-- VS readout
		{xcnt+ 6.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+13.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+ 6.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+13.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+13.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+20.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+13.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+20.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+20.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+27.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+20.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+27.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+27.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+34.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+27.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+34.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+34.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+41.0,ycnt-7.0,0,  0,0,0,  0, 0},
		{xcnt+34.0,ycnt+7.0,0,  0,0,0,  0, 0},
		{xcnt+41.0,ycnt+7.0,0,  0,0,0,  0, 0}
	})
	local IDX = oapi.create_indexarray({
		0,1,2, 3,2,1,
		4,5,6, 7,6,5,
		8,9,10, 11,10,9,
		12,13,14, 15,14,13,
		16,17,18, 19,18,17,
		20,21,22, 23,22,21,
	})
--[[
#ifdef UNDEF
	// DEBUG
	std::ofstream ofs("tmp.dat");
	for (int j = 0; j < NVTX; j++) {
			ofs << VTX[j].x << ' ' << VTX[j].y << ' ' << 0 << ' ';
			ofs << VTX[j].nx << ' ' << VTX[j].ny << ' '  << -1 << ' ';
			ofs << VTX[j].tu << ' ' << VTX[j].tv << std::endl;
	}
#endif
]]
	--AddGeometry (hMesh, grpidx, VTX, NVTX, IDX, NIDX);
end

--------------------------------------------------------------
local ascii0 = string.byte('0')
local ascii9 = string.byte('9')

local texw = INSTR3D_TEXW
local texh = INSTR3D_TEXH
local lutdx = {
	['0'] = 0,
	['1'] = 0,
	['2'] = 0,
	['3'] = 0,
	['4'] = 0,
	['5'] = 0,
	['6'] = 0,
	['7'] = 0,
	['8'] = 0,
	['9'] = 0,
	['.'] = 10.0/texw,
	['-'] = 10.0/texw,
	['+'] = 10.0/texw,
}

local lutdy = {
	['0'] = 0,
	['1'] = 1 * 17.0/texh,
	['2'] = 2 * 17.0/texh,
	['3'] = 3 * 17.0/texh,
	['4'] = 4 * 17.0/texh,
	['5'] = 5 * 17.0/texh,
	['6'] = 6 * 17.0/texh,
	['7'] = 7 * 17.0/texh,
	['8'] = 8 * 17.0/texh,
	['9'] = 9 * 17.0/texh,
	['.'] = 0,
	['-'] = 34.0/texh,
	['+'] = 51.0/texh,
	['k'] = 68.0/texh,
}


function InstrVS:Redraw (vtx, vtxr)
	local V = self.vessel:get_airspeedvector (REFFRAME.HORIZON)
	local vspd = V.y*0.1 -- unit is 10m

	local texw = INSTR3D_TEXW
	local texh = INSTR3D_TEXH
	local scalecnt = texh-764.0+152.0
	local scaleunit = 15
	local viewh = 50.0
	local ycnt

	local centered = math.abs(vspd) <= 4.0

	local dy = vspd-math.floor(vspd)
	if centered then
		ycnt = scalecnt - vspd*scaleunit
	else
		if vspd > 0.0 then
			ycnt = scalecnt - (5.0+dy)*scaleunit
		else
			ycnt = scalecnt + (5.0-dy)*scaleunit
		end
	end
	local y0 = ycnt-viewh
	local y1 = ycnt+viewh
	vtx[0+1].tv = y0/texh
	vtx[1+1].tv = y0/texh
	vtx[2+1].tv = y1/texh
	vtx[3+1].tv = y1/texh

	-- copy labels onto scale
	local labelx = math.floor(texw-185)
	local vmin = math.floor(vspd)-3
	if vmin ~= self.pvmin then
		self.pvmin = vmin
		local vmax = vmin+7
		for i = vmin, vmax do
			local str = string.format("%d", math.abs(i)%1000*10)
			local len = #str
			local ignore = false
			local iy
			if centered then
				if i == 0 then
					ignore = true
				end
				iy = scalecnt-i*scaleunit-5
			else
				if i > 0 then
					iy = scalecnt-(2+i-vmin)*scaleunit-5
				else
					iy = scalecnt+(8-i+vmin)*scaleunit-5
				end
			end
			if not ignore then
				for j=0,3 do
					local xsrc, ysrc
					if j < len then
						local n = string.byte(str:sub(j+1,j+1))-ascii0
						xsrc = texw-184
						ysrc = texh-428+n*8
					else
						xsrc = texw-184
						ysrc = texh-348
					end
					if i < 0 then
						ysrc = ysrc + 88
					end
					oapi.blt (self.sf, self.sf, labelx+j*6, iy, xsrc, ysrc, 6, 8)
				end
			end
		end
	end

	-- VS readout
	local cbuf
	if math.abs(vspd) < 1e3 then
		cbuf = string.format(math.abs(vspd) < 10.0 and "%+0.1f" or "%+0.0f", vspd*10.0)
	elseif math.abs(vspd) < 1e6 then
		cbuf = string.format("%+3.0fk", vspd > 0.0 and math.floor(vspd*0.01) or math.ceil(vspd*0.01))
	else
		cbuf = "----"
	end

	local numx = texw-177.0
	local numy = texh-423.5
	local numw = 10.0
	local numh = 19.0
	local tu_num = {numx/texw,(numx+numw)/texw,numx/texw,(numx+numw)/texw}
	local tv_num = {(numy+numh)/texh,(numy+numh)/texh,numy/texh,numy/texh}

	local len = math.min(4, #cbuf)
	for i = 1, len do
		local c = cbuf:sub(i,i)
		local dx = lutdx[c] or 10.0/texw
		local dy = lutdy[c] or 17.0/texw

		for j=1,4 do
			vtxr[(i-1)*4+j].tu = tu_num[j]+dx
			vtxr[(i-1)*4+j].tv = tv_num[j]+dy
		end
	end
--[[

	local numx = texw-177.0
	local numy = texh-423.5
	local numw = 10.0
	local numh = 19.0
	local tu_num = {numx/texw,(numx+numw)/texw,numx/texw,(numx+numw)/texw}
	local tv_num = {(numy+numh)/texh,(numy+numh)/texh,numy/texh,numy/texh}
	local vofs = 4+self.vtxofs

	for i=0,4 do
		local c = cbuf:sub(i+1,i+1)
		local ascii = string.byte(c)
		if ascii then
			local dx, dy
			if ascii >= ascii0 and ascii <= ascii9 then
				dx = 0.0
				dy = ((ascii-ascii0) * 17.0)/texh
			else
				dx = 10.0/texw
				if c == '.' then
					dy = 0
				elseif c == '-' then
					dy = 34.0/texh
				elseif c == '+' then
					dy = 51.0/texh
				elseif c == 'k' then
					dy = 68.0/texh
				else
					dy = 17.0/texh
				end
			end

			for j = 1,4 do
				vtxr[i*4+j].tu = tu_num[j]+dx
				vtxr[i*4+j].tv = tv_num[j]+dy
			end
		end
	end
	]]
end

--------------------------------------------------------------

function InstrVS:Redraw2D (surf)
	local V = self.vessel:get_airspeedvector (REFFRAME.HORIZON)
	local vspd = V.y*0.1 -- unit is 10m

	local texw = INSTR3D_TEXW
	local texh = INSTR3D_TEXH
	local scalecnt = texh-764.0+152.0
	local scaleunit = 15
	local viewh = 50.0
	local centered = math.abs(vspd) <= 4.0

	local dy = vspd-math.floor(vspd)
	local ycnt
	if centered then
		ycnt = scalecnt - vspd*scaleunit
	else
		if vspd > 0.0 then
			ycnt = scalecnt - (5.0+dy)*scaleunit
		else
			ycnt = scalecnt + (5.0-dy)*scaleunit
		end
	end
	local y0 = ycnt-viewh
	local y1 = ycnt+viewh
	self.grp.Vtx[0+self.vtxofs+1].tv = y0/texh
	self.grp.Vtx[1+self.vtxofs+1].tv = y0/texh
	self.grp.Vtx[2+self.vtxofs+1].tv = y1/texh
	self.grp.Vtx[3+self.vtxofs+1].tv = y1/texh

	-- copy labels onto scale
	local labelx = texw-185
	local vmin = math.floor(vspd)-3
	if vmin ~= self.pvmin then
		self.pvmin = vmin
		local vmax = vmin+7
		for i = vmin,vmax do
			local cbuf = string.format("%d", math.abs(i)%1000*10)
			local len = #cbuf
			local ignore = false
			local iy
			if centered then
				if i == 0 then
					ignore = true
				end
				iy = scalecnt-i*scaleunit-5
			else
				if i > 0 then
					iy = scalecnt-(2+i-vmin)*scaleunit-5
				else
					iy = scalecnt+(8-i+vmin)*scaleunit-5
				end
			end
			if not ignore then
				for j=0,3 do
					local xsrc, ysrc
					if j < len then
						local n = string.byte(cbuf:sub(j+1,j+1))-ascii0
						xsrc = texw-184
						ysrc = texh-428+n*8
					else
						xsrc = texw-184
						ysrc = texh-348
					end
					if i < 0 then
						ysrc = ysrc + 88
					end
					oapi.blt (self.sf, self.sf, labelx+j*6, iy, xsrc, ysrc, 6, 8)
				end
			end
		end
	end

	-- VS readout
	local cbuf
	if math.abs(vspd) < 1e3 then
		cbuf = string.format(math.abs(vspd) < 10.0 and "%+0.1f " or "%+0.0f ", vspd*10.0)
	elseif math.abs(vspd) < 1e6 then
		cbuf = string.format("%+3.0fk ", vspd > 0.0 and math.floor(vspd*0.01) or math.ceil(vspd*0.01))
	else
		cbuf = "---- "
	end

	local numx = texw-177.0
	local numy = texh-423.5
	local numw = 10.0
	local numh = 19.0
	local tu_num = {numx/texw,(numx+numw)/texw,numx/texw,(numx+numw)/texw}
	local tv_num = {numy/texh,numy/texh,(numy+numh)/texh,(numy+numh)/texh}
	local vofs = 4+self.vtxofs

	for i=0,4 do
		local c = cbuf:sub(i+1,i+1)
		local ascii = string.byte(c)
		if ascii then
			local dx, dy
			if ascii >= ascii0 and ascii <= ascii9 then
				dx = 0.0
				dy = ((ascii-ascii0) * 17.0)/texh
			else
				dx = 10.0/texw;
				if c == '.' then
					dy = 0
				elseif c == '-' then
					dy = 34.0/texh
				elseif c == '+' then
					dy = 51.0/texh
				elseif c == 'k' then
					dy = 68.0/texh
				else
					dy = 17.0/texh
				end
			end
			for j = 1,4 do
				self.grp.Vtx[i*4+j+vofs].tu = tu_num[j]+dx
				self.grp.Vtx[i*4+j+vofs].tv = tv_num[j]+dy
			end
		end
	end

	return false
end

--------------------------------------------------------------

function InstrVS:RedrawVC (hMesh, surf)
	local Vtx = self.vc_grp.Vtx
	local VtxR = self.vc_grp_readout.Vtx
	if hMesh and Vtx and VtxR then
		self:Redraw (Vtx, VtxR)

		local ges = {}
		ges.flags = GRPEDIT.VTXTEXV
		ges.Vtx = Vtx
		ges.vIdx = self.vperm
		oapi.edit_meshgroup (hMesh, GRP_VC.VC_INSTR, ges)

		local gesr = {}
		gesr.flags = GRPEDIT.VTXTEX
		gesr.Vtx = VtxR
		gesr.vIdx = self.vperm_readout
		oapi.edit_meshgroup (hMesh, GRP_VC.VC_INSTR, gesr)
	end
	return false
end

return InstrVS
