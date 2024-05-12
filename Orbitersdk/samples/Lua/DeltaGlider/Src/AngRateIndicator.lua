-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: AngRateIndicator.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP
local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP


local AngRateIndicator = Class(PanelElement)

function AngRateIndicator:new (v, blitsrc)
	PanelElement.new(self, v)
	self.bsrc =  blitsrc
	self.nvtx = 7*3*3
	self.upt = 0
	self.xcnt_VC = {}
	self.ycnt_VC = {}
	self.zcnt_VC = {}
	self.xcnt_2D = {}
	self.ycnt_2D = {}
	self.zcnt_2D = {}
end

function AngRateIndicator:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.ANGVELDISP)
	if self.grp then
		if not self.vtxbuf_2D then
			self.vtxbuf_2D = self.grp.Vtx:copy(1, self.nvtx)
			self.w0_2D = self.vtxbuf_2D[1].x - self.vtxbuf_2D[2].x
			for i=0,2 do
				self.xcnt_2D[i+1] = self.vtxbuf_2D[6+i*7+1].x
				self.ycnt_2D[i+1] = self.vtxbuf_2D[6+i*21+1].y
				self.zcnt_2D[i+1] = 0.0
			end
			self.cost_2D = -1.0
			self.sint_2D = 0.0
			self:LoadPanel2D()
		end
	end
end

function AngRateIndicator:ResetVC (hMesh)
	if not self.vtxbuf_VC then
		self.vtxbuf_VC = oapi.create_ntvertexarray(self.nvtx)
		local grs = {}
		grs.Vtx = self.vtxbuf_VC
		oapi.get_meshgroup(hMesh, GRP_VC.ANGVEL_DISP_OVR, grs)
		self.w0_VC = self.vtxbuf_VC[1].x - self.vtxbuf_VC[2].x
		for i=0,2 do
			self.xcnt_VC[i+1] = self.vtxbuf_VC[6+i*7+1].x
			self.ycnt_VC[i+1] = self.vtxbuf_VC[6+i*21+1].y
			self.zcnt_VC[i+1] = self.vtxbuf_VC[6+i*21+1].z
		end
		local tilt = math.atan2(self.zcnt_VC[1]-self.zcnt_VC[2], self.ycnt_VC[1]-self.ycnt_VC[2])
		self.cost_VC = math.cos(tilt)
		self.sint_VC = math.sin(tilt)
		self:LoadVC()
	end
end

function AngRateIndicator:LoadPanel2D (panelid, hPanel, viewW, viewH)
	self.vtx0 = self.vtxbuf_2D
	self.w0 = self.w0_2D
	self.cost = self.cost_2D
	self.sint = self.sint_2D
	self.xcnt = self.xcnt_2D
	self.ycnt = self.ycnt_2D
	self.zcnt = self.zcnt_2D
end

function AngRateIndicator:LoadVC (vcid)
	self.vtx0 = self.vtxbuf_VC
	self.w0 = self.w0_VC
	self.cost = self.cost_VC
	self.sint = self.sint_VC
	self.xcnt = self.xcnt_VC
	self.ycnt = self.ycnt_VC
	self.zcnt = self.zcnt_VC
end

function AngRateIndicator:UncoverScale (which, axis, phi, vtx)
	local vofs = which*21 + axis*7 + 1
	which = which + 1
	axis = axis + 1
	if axis < 3 then
		if phi < 0 then
			if phi >= -0.25*PI then
				vtx[vofs+0].x = self.xcnt[axis] + self.w0 * math.tan(phi)
			else
				vtx[vofs+0].x = self.xcnt[axis] - self.w0
				vtx[vofs+1].x = self.xcnt[axis] - self.w0
				local y = self.w0 * math.tan(phi+PI05)
				vtx[vofs+0].y = self.ycnt[which] + y * self.cost
				vtx[vofs+1].y = self.ycnt[which] + y * self.cost

				vtx[vofs+0].z = self.zcnt[which] + y * self.sint
				vtx[vofs+1].z = self.zcnt[which] + y * self.sint
			end
		else
			if phi <= 0.25*PI then
				vtx[vofs+3].x = self.xcnt[axis] + self.w0 * math.tan(phi)
			else
				vtx[vofs+3].x = self.xcnt[axis] + self.w0
				vtx[vofs+4].x = self.xcnt[axis] + self.w0
				local y = self.w0 * math.tan(PI05-phi)
				vtx[vofs+3].y = self.ycnt[which] + y * self.cost
				vtx[vofs+4].y = self.ycnt[which] + y * self.cost

				vtx[vofs+3].z = self.zcnt[which] + y * self.sint
				vtx[vofs+4].z = self.zcnt[which] + y * self.sint
			end
		end
	else
		if phi < 0 then
			if phi >= -0.25*PI then
				local y = self.w0 * math.tan(phi)
				vtx[vofs+3].y = self.ycnt[which] + y * self.cost
				vtx[vofs+3].z = self.zcnt[which] + y * self.sint
			else
				local x = self.w0 * math.tan(PI05+phi)
				vtx[vofs+3].x = self.xcnt[axis]+x
				vtx[vofs+4].x = self.xcnt[axis]+x
				vtx[vofs+3].y = vtx[vofs+4].y
				vtx[vofs+3].z = vtx[vofs+4].z
			end
		else
			if phi <= 0.25*PI then
				local y = self.w0 * math.tan(phi)
				vtx[vofs+0].y = self.ycnt[which] + y * self.cost
				vtx[vofs+0].z = self.zcnt[which] + y * self.sint
			else
				local x = self.w0 * math.tan(PI05-phi)
				vtx[vofs+0].x = self.xcnt[axis]+x
				vtx[vofs+1].x = self.xcnt[axis]+x
				vtx[vofs+0].y = vtx[vofs+1].y
				vtx[vofs+0].z = vtx[vofs+1].z
			end
		end
	end
end

local vtxVC = nil
function AngRateIndicator:RedrawVC (hMesh, surf)
	local t = oapi.get_simtime()
	if t < self.upt and t > self.upt-1.0 then return false end
	self.upt = t + 0.1

	if not vtxVC then
		vtxVC = oapi.create_ntvertexarray(self.nvtx)
	end
	vtxVC:write(self.vtx0)

	local prm = self.vessel:get_angvel()
	local lut = {-prm.y, prm.z, prm.x}
	for axis = 0, 2 do
		local v = lut[axis+1] * DEG
		local av = math.abs(v)
		if av > 1e-1 then
			local phi = math.min ((math.log10(av)+1.0)*40.0*RAD, 0.75*PI)
			if v < 0 then
				phi = -phi
			end
			self:UncoverScale (0, axis, phi, vtxVC)
		end
		if surf then
			local cbuf = self:ValStr (v)
			self:BlitReadout (0, axis, cbuf, surf)
		end
	end
	prm = self.vessel:get_angularacc(prm)
	lut = {-prm.y, prm.z, prm.x}
	for axis = 0,2 do
		local v = lut[axis+1] * DEG
		local av = math.abs(v)
		if av > 1e-1 then
			local phi = math.min ((math.log10(av)+1.0)*40.0*RAD, 0.75*PI)
			if v < 0 then
				phi = -phi
			end
			self:UncoverScale (1, axis, phi, vtxVC)
		end
		if surf then
			local cbuf = self:ValStr (v)
			self:BlitReadout (1, axis, cbuf, surf)
		end
	end
	prm = self.vessel:get_angularmoment()
	lut = {-prm.y, prm.z, prm.x}
	for axis = 0,2 do
		local v = lut[axis+1] * DEG
		local av = math.abs(v*1e-3)
		if av > 1e-1 then
			local phi = math.min ((math.log10(av)+1.0)*40.0*RAD, 0.75*PI)
			if v < 0 then
				phi = -phi
			end
			self:UncoverScale (2, axis, phi, vtxVC)
		end
		if surf then
			local cbuf = self:ValStr (v)
			self:BlitReadout (2, axis, cbuf, surf)
		end
	end

	local ges = {}
	ges.flags = GRPEDIT.VTXCRD
	ges.Vtx = vtxVC
	oapi.edit_meshgroup(hMesh, GRP_VC.ANGVEL_DISP_OVR, ges)
	return false
end

function AngRateIndicator:Redraw2D (surf)
	local t = oapi.get_simtime()
	if t < self.upt and t > self.upt-1.0 then return false end
	self.upt = t + 0.1
	self.grp.Vtx:write(self.vtx0)
	local prm = self.vessel:get_angvel()
	local lut = {-prm.y, prm.z, prm.x}
	for axis = 0, 2 do
		local v = lut[axis+1] * DEG
		local av = math.abs(v)
		if av > 1e-1 then
			local phi = math.min ((math.log10(av)+1.0)*40.0*RAD, 0.75*PI)
			if v < 0 then
				phi = -phi
			end
			self:UncoverScale (0, axis, phi, self.grp.Vtx)
		end
	end
	prm = self.vessel:get_angularacc(prm)
	lut = {-prm.y, prm.z, prm.x}
	for axis = 0, 2 do
		local v = lut[axis+1] * DEG
		local av = math.abs(v)
		if av > 1e-1 then
			local phi = math.min ((math.log10(av)+1.0)*40.0*RAD, 0.75*PI)
			if v < 0 then
				phi = -phi
			end
			self:UncoverScale (1, axis, phi, self.grp.Vtx)
		end
	end
	prm = self.vessel:get_angularmoment()
	lut = {-prm.y, prm.z, prm.x}
	for axis = 0, 2 do
		local v = lut[axis+1] * DEG
		local av = math.abs(v*1e-3)
		if av > 1e-1 then
			local phi = math.min ((math.log10(av)+1.0)*40.0*RAD, 0.75*PI)
			if v < 0 then
				phi = -phi
			end
			self:UncoverScale (2, axis, phi, self.grp.Vtx)
		end
	end
	return false
end

function AngRateIndicator:ValStr (v)
	local sgn = '+';
	if v < 0.0 then
		v = -v
		sgn = '-'
	end
	v = math.abs(v)
	local pfix = ' '
	if v > 0.995e3 then
		v = v * 1e-3
		pfix = 'k'
		if v > 0.995e3 then
			v = v * 1e-3
			pfix = 'M'
			if v > 0.995e3 then
				v = v * 1e-3
				pfix = 'G'
				if v > 9.995e3 then
					return "----"
				end
			end
		end
	end

	if v < 9.995 then
		return string.format("%s%0.2f%s", sgn, v, pfix)
	elseif v < 99.95 then
		return string.format("%s%0.1f%s", sgn, v, pfix)
	else
		return string.format("%s%0.0f%s", sgn, v, pfix)
	end
end

local lutblit = {
	['0'] = 0,
	['1'] = 8,
	['2'] = 16,
	['3'] = 24,
	['4'] = 32,
	['5'] = 40,
	['6'] = 48,
	['7'] = 56,
	['8'] = 64,
	['9'] = 72,
	['.'] = 80,
	['+'] = 88,
	['-'] = 96,
	['k'] = 104,
	['M'] = 112,
	['G'] = 120,
}
function AngRateIndicator:BlitReadout (which, axis, str, tgt)
	local tgtx = 3
	local tgty = 3 + (which*3 + axis)*16
	local srcy = 0;
	local w = 8
	local h = 11


	for i = 1, #str do
		local c = str:sub(i,i)
		local srcx = lutblit[c] or 128

		oapi.blt (tgt, self.bsrc, tgtx, tgty, srcx, srcy, w, h)
		tgtx = tgtx + w
	end
end

return AngRateIndicator
