local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP


local VC_AOA_vofs = 0

local VC_AOA_READOUT_vofs = 8
local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024

local WINGLOAD_MAX =  16e3
local WINGLOAD_MIN = -10e3
-- Max. allowed positive and negative wing load [N/m^2]


local InstrAOA = Class(PanelElement)

-- ==============================================================

function InstrAOA:new(v)
	PanelElement.new (self, v)

	self.paoa = 0.0

	self.vperm = oapi.create_indexarray(8)
	for i=1,8 do
		self.vperm[i] = i + VC_AOA_vofs - 1
	end
	self.vc_grp = {}
	self.vc_grp.VtxPerm = self.vperm

	self.vperm_readout = oapi.create_indexarray(16)
	for i=1,16 do
		self.vperm_readout[i] = i + VC_AOA_READOUT_vofs - 1
	end
	self.vc_grp_readout = {}
	self.vc_grp_readout.VtxPerm = self.vperm_readout
end


--------------------------------------------------------------

function InstrAOA:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_BELOW)
	self.vtxofs = 120
	self.ycnt = 311.0
	self.disph = 118.0
end

--------------------------------------------------------------

function InstrAOA:ResetVC (hMesh)
	if not self.vc_grp.Vtx then
		self.vc_grp.Vtx = oapi.create_ntvertexarray(8)
	end
	if not self.vc_grp_readout.Vtx then
		self.vc_grp_readout.Vtx = oapi.create_ntvertexarray(16)
	end

	if oapi.get_meshgroup (hMesh, GRP_VC.VC_INSTR, self.vc_grp) ~= 0 then -- problems
		self.vc_grp.Vtx = nil
	end
	if oapi.get_meshgroup (hMesh, GRP_VC.VC_INSTR, self.vc_grp_readout) ~= 0 then -- problems
		self.vc_grp_readout.Vtx = nil
	end
end

--------------------------------------------------------------

function InstrAOA:LoadPanel2D (panelid, hPanel, viewW, viewH)
	self.ycnt = 311.0
	self.disph = 118.0
end

--------------------------------------------------------------

function InstrAOA:LoadVC (vcid)
	self.ycnt = 1.09597
	self.disph = 0.069540024
end

--------------------------------------------------------------

function InstrAOA:AddMeshData2D (hMesh, grpidx)
	local texw = INSTR3D_TEXW
	local texh = INSTR3D_TEXH
	local scaleh = 512
	local horzx = texw-239.5
	local horzw = 41.0
	local horzy = texh-765
	local xcnt = 593.0
	local ycnt = 311.0
	local NVTX = 40
	local NIDX = 66

	local VTX = oapi.create_ntvertexarray({
		-- AOA tape
		{xcnt-22.0, ycnt-59,0,  0,0,0,  horzx/texw,        horzy/texh},
		{xcnt+22.0, ycnt-59,0,  0,0,0,  (horzx+horzw)/texw,horzy/texh},
		{xcnt-22.0, ycnt-59,0,  0,0,0,  horzx/texw,        horzy/texh},
		{xcnt+22.0, ycnt-59,0,  0,0,0,  (horzx+horzw)/texw,horzy/texh},
		{xcnt-22.0, ycnt+59,0,  0,0,0,  horzx/texw,        (horzy+scaleh)/texh},
		{xcnt+22.0, ycnt+59,0,  0,0,0,  (horzx+horzw)/texw,(horzy+scaleh)/texh},
		{xcnt-22.0, ycnt+59,0,  0,0,0,  horzx/texw,        (horzy+scaleh)/texh},
		{xcnt+22.0, ycnt+59,0,  0,0,0,  (horzx+horzw)/texw,(horzy+scaleh)/texh},
		-- Wing load background
		{xcnt+29.0,ycnt-60.5,0, 0,0,0, (texw-187.0)/texw,(texh-303.5)/texh},
		{xcnt+34.0,ycnt-60.5,0, 0,0,0, (texw-187.0)/texw,(texh-303.5)/texh},
		{xcnt+29.0,ycnt- 9.5,0, 0,0,0, (texw-187.0)/texw,(texh-252.5)/texh},
		{xcnt+34.0,ycnt- 9.5,0, 0,0,0, (texw-187.0)/texw,(texh-252.5)/texh},
		{xcnt+29.0,ycnt+ 9.5,0, 0,0,0, (texw-185.0)/texw,(texh-252.5)/texh},
		{xcnt+34.0,ycnt+ 9.5,0, 0,0,0, (texw-185.0)/texw,(texh-252.5)/texh},
		{xcnt+29.0,ycnt+60.5,0, 0,0,0, (texw-185.0)/texw,(texh-303.5)/texh},
		{xcnt+34.0,ycnt+60.5,0, 0,0,0, (texw-185.0)/texw,(texh-303.5)/texh},
		-- Wing load masks
		{xcnt+29.0,ycnt-60.5,0, 0,0,0,  (texw-186.0)/texw,(texh-305.0)/texh},
		{xcnt+34.0,ycnt-60.5,0, 0,0,0,  (texw-186.0)/texw,(texh-305.0)/texh},
		{xcnt+29.0,ycnt     ,0, 0,0,0,  (texw-186.0)/texw,(texh-305.0)/texh},
		{xcnt+34.0,ycnt     ,0, 0,0,0,  (texw-186.0)/texw,(texh-305.0)/texh},
		{xcnt+29.0,ycnt     ,0, 0,0,0,  (texw-186.0)/texw,(texh-305.0)/texh},
		{xcnt+34.0,ycnt     ,0, 0,0,0,  (texw-186.0)/texw,(texh-305.0)/texh},
		{xcnt+29.0,ycnt+60.5,0, 0,0,0,  (texw-186.0)/texw,(texh-305.0)/texh},
		{xcnt+34.0,ycnt+60.5,0, 0,0,0,  (texw-186.0)/texw,(texh-305.0)/texh},
		-- AOA readout
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
		{xcnt+34.0,ycnt+7.0,0,  0,0,0,  0, 0}
	})

	local IDX = oapi.create_indexarray({
		0,1,2, 3,2,1, 2,3,4, 5,4,3, 4,5,6, 7,6,5,
		8,9,10, 11,10,9,
		12,13,14, 15,14,13,
		16,17,18, 19,18,17,
		20,21,22, 23,22,21,
		24,25,26, 27,26,25,
		28,29,30, 31,30,29,
		32,33,34, 35,34,33,
		36,37,38, 39,38,37,
	})

	PanelElement.AddGeometry (self, hMesh, grpidx, VTX, IDX)
end

--------------------------------------------------------------
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
}

function InstrAOA:Redraw (vtx, vtxr)
	local aoa = self.vessel:get_aoa()
	if aoa ~= aoa then return end -- NaN check
	local aoa_abs = math.abs(aoa)

	-- tape range limits
	if aoa_abs > 45.0*RAD then
		aoa_abs = 45.0*RAD
		aoa = (aoa >= 0.0 and 45.0*RAD or -45.0*RAD)
	end

	-- tape response delay
	local tapespeed = 20.0*RAD
	local daoa = aoa-self.paoa
	if math.abs(daoa)/oapi.get_simstep() > tapespeed then
		aoa = self.paoa + oapi.get_simstep()*(daoa>0 and tapespeed or -tapespeed)
		aoa_abs = math.abs(aoa)
	end
	self.paoa = aoa

	-- AOA tape
	local h2=self.disph*0.5
	local rescale0 = false
	local rescale1 = false

	local viewh = 50.0
	local scaley = texh-765.0
	local scaleh = 512.0
	local scalecnt = 0.5*scaleh+scaley+1.0

	local dy
	if aoa_abs <= 5.0*RAD then
		dy = -aoa*DEG*12.0
	else
		dy = (aoa_abs*DEG-5.0)*5.0+60.0
		if aoa >= 0.0 then
			dy = -dy
		end
	end

	local y0 = dy-viewh
	local y1 = dy+viewh
	local tv0
	if y0 < -scaleh/2 then
		tv0 = scaley/texh
		rescale1 = true
	else
		tv0 = (y0+scalecnt)/texh
	end
	local tv1
	if y1 > scaleh/2 then
		tv1 = (scaley+scaleh)/texh
		rescale0 = true
	else
		tv1 = (y1+scalecnt)/texh
	end
	local vy0
	if rescale0 then
		local h = (self.disph * (tv1-tv0)/(2.0*viewh)*texh)
		vy0 = self.ycnt+h2-h;
	else
		vy0 = self.ycnt-h2
	end
	local vy1
	if rescale1 then
		local h = (self.disph * (tv1-tv0)/(2.0*viewh)*texh)
		vy1 = self.ycnt-h2+h
	else
		vy1 = self.ycnt+h2
	end

	vtx[3].y = vy1
	vtx[4].y = vy1
	vtx[5].y = vy0
	vtx[6].y = vy0

	vtx[3].tv = tv0
	vtx[4].tv = tv0
	vtx[5].tv = tv1
	vtx[6].tv = tv1

	-- AOA readout
	local numx = texw-177.0
	local numy = texh-423.5
	local numw = 10.0
	local numh = 19.0
	local tu_num = {numx/texw,(numx+numw)/texw,numx/texw,(numx+numw)/texw}
	local tv_num = {(numy+numh)/texh,(numy+numh)/texh,numy/texh,numy/texh}

	local aoastr = string.format(DEG*aoa_abs < 10.0 and "%+0.1f" or "%+0.0f", aoa*DEG)

	local len = math.min(4, #aoastr)
	for i = 1, len do
		local c = aoastr:sub(i,i)
		local dx = lutdx[c] or 10.0/texw
		local dy = lutdy[c] or 17.0/texw

		for j=1,4 do
			vtxr[(i-1)*4+j].tu = tu_num[j]+dx
			vtxr[(i-1)*4+j].tv = tv_num[j]+dy
		end
	end
end

--------------------------------------------------------------

function InstrAOA:Redraw2D (surf)
	if self.grp then
		local aoa = self.vessel:get_aoa()
		if aoa ~= aoa then return false end -- NaN check
		local aoa_abs = math.abs(aoa);

		local aoastr = string.format(DEG*aoa_abs < 10.0 and "%+0.1f" or "%+0.0f", aoa*DEG)

		-- tape range limits
		if aoa_abs > 45.0*RAD then
			aoa_abs = 45.0*RAD
			aoa = (aoa >= 0.0 and 45.0*RAD or -45.0*RAD)
		end

		-- tape response delay
		local tapespeed = 20.0*RAD
		local daoa = aoa-self.paoa
		if math.abs(daoa)/oapi.get_simstep() > tapespeed then
			aoa = self.paoa + oapi.get_simstep()*(daoa>0 and tapespeed or -tapespeed)
			aoa_abs = math.abs(aoa)
		end
		self.paoa = aoa

		local h2=self.disph*0.5

		local rescale0 = false
		local rescale1 = false

		local texw = INSTR3D_TEXW
		local texh = INSTR3D_TEXH
		local viewh = 50.0
		local scaley = texh-765.0
		local scaleh = 512.0
		local scalecnt = 0.5*scaleh+scaley+1.0

		-- AOA tape
		local dy
		if aoa_abs <= 5.0*RAD then
			dy = -aoa*DEG*12.0
		else
			dy = (aoa_abs*DEG-5.0)*5.0+60.0
			if aoa >= 0.0 then
				dy = -dy
			end
		end

		local y0 = dy-viewh
		local y1 = dy+viewh
		local tv0
		if y0 < -scaleh/2 then
			tv0 = scaley/texh
			rescale1 = true
		else
			tv0 = (y0+scalecnt)/texh
		end
		local tv1
		if y1 > scaleh/2 then
			tv1 = (scaley+scaleh)/texh
			rescale0 = true
		else
			tv1 = (y1+scalecnt)/texh
		end
		local vy0
		if rescale0 then
			local h = (self.disph * (tv1-tv0)/(2.0*viewh)*texh)
			vy0 = self.ycnt+h2-h;
		else
			vy0 = self.ycnt-h2
		end
		local vy1
		if rescale1 then
			local h = (self.disph * (tv1-tv0)/(2.0*viewh)*texh)
			vy1 = self.ycnt-h2+h
		else
			vy1 = self.ycnt+h2
		end

		self.grp.Vtx[2+self.vtxofs+1].y  = vy0
		self.grp.Vtx[3+self.vtxofs+1].y  = vy0
		self.grp.Vtx[4+self.vtxofs+1].y  = vy1
		self.grp.Vtx[5+self.vtxofs+1].y  = vy1
		self.grp.Vtx[2+self.vtxofs+1].tv = tv0
		self.grp.Vtx[3+self.vtxofs+1].tv = tv0
		self.grp.Vtx[4+self.vtxofs+1].tv = tv1
		self.grp.Vtx[5+self.vtxofs+1].tv = tv1

		-- AOA readout
		local numx = texw-177.0
		local numy = texh-423.5
		local numw = 10.0
		local numh = 19.0
		local tu_num = {numx/texw,(numx+numw)/texw,numx/texw,(numx+numw)/texw}
		local tv_num = {numy/texh,numy/texh,(numy+numh)/texh,(numy+numh)/texh}
		local vofs = 24+self.vtxofs

		local len = math.min(4, #aoastr)
		for i = 1, len do
			local c = aoastr:sub(i,i)
			local dx = lutdx[c] or 10.0/texw
			local dy = lutdy[c] or 17.0/texw

			for j=1,4 do
				self.grp.Vtx[(i-1)*4+j+vofs].tu = tu_num[j]+dx
				self.grp.Vtx[(i-1)*4+j+vofs].tv = tv_num[j]+dy
			end
		end

		-- wing load LEDs
		local load = self.vessel:get_lift() / 190.0
		local rowh = 60.0
		local loadmax = WINGLOAD_MAX*60.0/51.0
		local h = math.min(math.abs(load)/loadmax,1.0)*rowh
		vofs = 16+self.vtxofs
		if load >= 0 then
			self.grp.Vtx[vofs+2+1].y = self.ycnt-h
			self.grp.Vtx[vofs+3+1].y = self.ycnt-h
			self.grp.Vtx[vofs+4+1].y = self.ycnt
			self.grp.Vtx[vofs+5+1].y = self.ycnt
		else
			self.grp.Vtx[vofs+2+1].y = self.ycnt
			self.grp.Vtx[vofs+3+1].y = self.ycnt
			self.grp.Vtx[vofs+4+1].y = self.ycnt+h
			self.grp.Vtx[vofs+5+1].y = self.ycnt+h
		end
	end
	return false
end

--------------------------------------------------------------

function InstrAOA:RedrawVC (hMesh, surf)
	local Vtx = self.vc_grp.Vtx
	local VtxR = self.vc_grp_readout.Vtx
	if hMesh and Vtx and VtxR then
		self:Redraw (Vtx, VtxR)
		local v1 = Vtx[7].z + (Vtx[1].z-Vtx[7].z)*(Vtx[3].y-Vtx[7].y)/(Vtx[1].y-Vtx[7].y)
		Vtx[3].z = v1
		Vtx[4].z = v1
		local v2 = Vtx[7].z + (Vtx[1].z-Vtx[7].z)*(Vtx[5].y-Vtx[7].y)/(Vtx[1].y-Vtx[7].y)
		Vtx[5].z = v2
		Vtx[6].z = v2

		local ges = {}
		ges.flags = GRPEDIT.VTXCRDY+GRPEDIT.VTXCRDZ+GRPEDIT.VTXTEXV
		ges.Vtx = Vtx
		ges.vIdx = self.vperm
		oapi.edit_meshgroup (hMesh, GRP_VC.VC_INSTR, ges)

		local gesr = {}
		gesr.flags = GRPEDIT.VTXTEX
		gesr.Vtx = self.vc_grp_readout.Vtx
		gesr.vIdx = self.vperm_readout
		oapi.edit_meshgroup (hMesh, GRP_VC.VC_INSTR, gesr)
	end
	return false
end

return InstrAOA
