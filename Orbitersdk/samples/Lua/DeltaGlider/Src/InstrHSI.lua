-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: InstrHSI.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================
local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local PANEL2D_WIDTH = 1280  -- panel width [pixel]
local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height
local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024

-- HSI (VC): rotation reference
local VC_HSI_ref = _V(0.00000,1.00610,7.20195)

-- HSI (VC): rotation axis
local VC_HSI_axis = _V(0.00000,0.58280,-0.81262)


local InstrHSI = Class(PanelElement)

function InstrHSI:new (v)
	PanelElement.new (self, v)
	self.crs = 0.0
	self.dev = 0.0
	self.nav = nil
	self.navType = TRANSMITTER.NONE
	self.vc_grp = {}
end

-- ==============================================================

function InstrHSI:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_BELOW)
	self.vtxofs = 76
end

-- ==============================================================

function InstrHSI:ResetVC (hMesh)
	self.vc_grp.nVtx = 48
	if not self.vc_grp.Vtx then
		self.vc_grp.Vtx = oapi.create_ntvertexarray(self.vc_grp.nVtx)
	end

	if oapi.get_meshgroup (hMesh, GRP_VC.HSI, self.vc_grp) ~= 0 then -- problems
		self.vc_grp.Vtx = nil
	end
end

-- ==============================================================

function InstrHSI:SetCrs (newcrs)
	if self.navType ~= TRANSMITTER.ILS then -- course indicator fixed for ILS approach
		self.crs = newcrs
		while self.crs < 0.0 do
			self.crs = self.crs + PI2
		end
		while self.crs >= PI2 do
			self.crs = self.crs - PI2
		end
	end
end

-- ==============================================================

function InstrHSI:GetCrs ()
	return self.crs
end

-- ==============================================================

function InstrHSI:Redraw (Vtx)
	local yaw = self.vessel:get_yaw()
	if yaw < 0.0 then
		yaw = yaw + PI2
	end

	local siny = math.sin(yaw)
	local cosy = math.cos(yaw)

	local dev = 0.0
	local brg = 0.0

	local texw = INSTR3D_TEXW
	local texh = INSTR3D_TEXH

	local tp = TRANSMITTER.NONE
	local nv = self.vessel:get_navsource (0)
	if nv then
		tp = oapi.get_navtype(nv)
		if tp ~= TRANSMITTER.VOR and tp ~= TRANSMITTER.ILS then
			nv = nil
		end
	end
	if nv ~= self.nav then
		self.nav = nv
		if nv then
			self.navRef = self.vessel:get_surfaceref()
			self.navType = tp
			if self.navRef then
				local npos = oapi.get_navpos (self.nav)
				local pos = oapi.global_to_equ (self.navRef, npos)
				self.navlng = pos.lng
				self.navlat = pos.lat
				if self.navType == TRANSMITTER.ILS then
					local data = oapi.get_navdata(self.nav)
					self.crs = data.appdir
				end
			else
				self.nav = nil
			end
		else
			self.navType = TRANSMITTER.NONE
		end
	end

	-- hide/show glideslope background and indicator
	local gs_tu = {(texw-311.5)/texw, (texw-156.5)/texw}
	local vofs = 8
	for i = 3,4 do
		Vtx[vofs+i].tu = (self.navType == TRANSMITTER.ILS and gs_tu[2] or gs_tu[1])
	end

	vofs = 28
	local gi_tu = {(texw-217.5)/texw, (texw-204.5)/texw, (texw-217.5)/texw, (texw-204.5)/texw}
	local gi_tu_off = (texw-315.5)/texw
	for i=1,4 do
		Vtx[vofs+i].tu = (self.navType == TRANSMITTER.ILS and gi_tu[i] or gi_tu_off)
	end

	-- apply glideslope
	local yshift = 0.0
	if self.nav then
		local pos, hRef = self.vessel:get_equpos ()
		if hRef and hRef == self.navRef then
			local adist, brg = self:Orthodome (pos.lng, pos.lat, self.navlng, self.navlat);
			adist = adist * oapi.get_size (hRef)
			dev = brg-self.crs
			if dev < -PI then
				dev = dev + PI2
			elseif dev >= PI then
				dev = dev - PI2
			end
			if dev < -PI05 then
				dev = -PI-dev
			elseif dev >= PI05 then
				dev =  PI-dev
			end

			-- calculate slope
			if self.navType == TRANSMITTER.ILS then
				local s = adist * math.cos(self.crs-brg)
				local alt = self.vessel:get_altitude()
				local slope = math.atan2 (alt, s) * DEG

				-- transform glideslope indicator
				local tgtslope = 4.0
				local dslope = slope - tgtslope
				yshift = math.min(math.abs(dslope)*20.0,45.0)
				if dslope > 0.0 then
					yshift = -yshift
				end
			end
		end
	end
	local gs_x = {64.0,76.5,64.0,76.5}
	local gs_y = {4.0,4.0,-4.0,-4.0}
	vofs = 28
	for i = 1,4 do
		Vtx[vofs+i].x = gs_x[i]
		Vtx[vofs+i].y = gs_y[i]+yshift
	end

	-- rotate compass rose
	local xp = {-60.5,60.5,-60.5,60.5}
	local yp = {60.5,60.5,-60.5,-60.5}
	vofs = 0
	for i=1,4 do
		Vtx[vofs+i].x = cosy*xp[i] - siny*yp[i]
		Vtx[vofs+i].y = siny*xp[i] + cosy*yp[i]
	end

	-- rotate source bearing indicator
	vofs = 12
	if self.nav then
		local c = yaw-brg;
		local sinc = math.sin(c)
		local cosc = math.cos(c)
		local xs = {-6.2,6.2,-6.2,6.2}
		local ys = {61,61,45,45}
		for i = 1,4 do
			Vtx[i+vofs].x = cosc*xs[i] - sinc*ys[i]
			Vtx[i+vofs].y = sinc*xs[i] + cosc*ys[i]
		end
	else -- hide indicator
		for i = 1,4 do
			Vtx[i+vofs].x = -65.0
			Vtx[i+vofs].y = 0.0
		end
	end

	-- rotate course+deviation indicator and scale
	local c = yaw-self.crs
	local sinc = math.sin(c)
	local cosc = math.cos(c)
	local xd = {-3.65,3.65,-3.65,3.65}
	local yd = {25.82,25.82,-25.82,-25.82}
	local xc = {-32.2,32.2,-32.2,32.2,-6.2, 6.2, -6.2,  6.2, 0,0,0,0}
	local yc = {  4.7, 4.7, -4.7,-4.7,60.5,60.5,-60.5,-60.5, 26.82,26.82,-26.82,-26.82}
	vofs = 16
	local dx = math.min(8.0,math.abs(dev)*DEG)*5.175
	if dev < 0.0 then
		dx = -dx
	end

	for i=1,4 do
		xc[i+8] = xd[i]+dx
	end
	for i=1,12 do
		Vtx[i+vofs].x = cosc*xc[i] - sinc*yc[i]
		Vtx[i+vofs].y = sinc*xc[i] + cosc*yc[i]
	end

	-- course readout
	local icrs = math.floor(self.crs*DEG+0.5) % 360
	
	local cbuf = string.format("%03d", icrs)
	vofs = 32
	local numw = 10.0
	local num_ofs = texw-311.0
	local tu_num = {0,numw/texw,0,numw/texw}

	local lut = {
		['0'] = 0,
		['1'] = 1,
		['2'] = 2,
		['3'] = 3,
		['4'] = 4,
		['5'] = 5,
		['6'] = 6,
		['7'] = 7,
		['8'] = 8,
		['9'] = 9,
	}
	for i = 1,3 do
		local x = (lut[cbuf:sub(i,i)] * numw + num_ofs)/texw
		for j = 1,4 do
			Vtx[(i-1)*4+j+vofs].tu = tu_num[j]+x
		end
	end
end

-- ==============================================================

function InstrHSI:Redraw2D (surf)
	if self.grp then
		self:Redraw (self.grp.Vtx:view(self.vtxofs+1))

		-- transform vertices to 2D panel location
		local xcnt = 0.5*PANEL2D_WIDTH+1.0
		local ycnt = 473.0
		for i = 0, 31 do
			if i < 4 or i >= 12 then
				self.grp.Vtx[i+self.vtxofs+1].x = self.grp.Vtx[i+self.vtxofs+1].x + xcnt
				self.grp.Vtx[i+self.vtxofs+1].y = ycnt - self.grp.Vtx[i+self.vtxofs+1].y
			end
		end
	end
	return false
end

-- ==============================================================

function InstrHSI:RedrawVC (hMesh, surf)
	local Vtx = self.vc_grp.Vtx
	if hMesh and Vtx then
		self:Redraw (Vtx)

		-- transform vertices to VC location
		local rad = 0.06           -- display size param
		local ycnt = VC_HSI_ref.y  -- y-position of display centre (x is assumed 0)
		local zcnt = VC_HSI_ref.z  -- z-position of display centre
		local cosa = VC_HSI_axis.z
		local sina = VC_HSI_axis.y
		local scale = rad/108.0

		for i = 0,31 do
			if i < 4 or i >= 12 then
				-- scale and rotate for bank
				Vtx[i+1].x = Vtx[i+1].x * scale
				local y = Vtx[i+1].y*scale
				local z = (i < 8 and 0.0 or (i < 20 and -0.0005 or -0.001))
				-- tilt to panel inclination
				Vtx[i+1].y = ycnt - y*cosa - z*sina
				Vtx[i+1].z = zcnt + y*sina - z*cosa
			end
		end

		-- write back to mesh group
		local ges = {}
		ges.flags = GRPEDIT.VTXCRD + GRPEDIT.VTXTEX
		ges.nVtx = self.vc_grp.nVtx
		ges.Vtx  = self.vc_grp.Vtx
		oapi.edit_meshgroup (hMesh, GRP_VC.HSI, ges)
	end
	return false
end

-- ==============================================================

function InstrHSI:Orthodome (lng1, lat1, lng2, lat2)
	local A     = lng2-lng1
	local sinA  = math.sin(A)
	local cosA  = math.cos(A)
	local slat1 = math.sin(lat1)
	local clat1 = math.cos(lat1)
	local slat2 = math.sin(lat2)
	local clat2 = math.cos(lat2)
	local cosa  = slat2*slat1 + clat2*clat1*cosA
	local dist = math.acos (cosa)
	local dir  = math.asin (clat2*sinA/math.sin(dist))
	if lat2 < lat1 then
		dir = PI-dir -- point 2 further south than point 1
	end
	if dir < 0.0 then
		dir = dir + PI2     -- quadrant 4
	end

	return dist, dir
end

return InstrHSI
