-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: FuelMFD.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024
local RCS_FUEL_CAPACITY = 600.0
-- Max fuel capacity: RCS tank [kg]
local ISP = 4e4
-- Vacuum Isp (fuel-specific impulse) for all thrusters [m/s]

-- constants for texture coordinates
local texw = INSTR3D_TEXW
local texh = INSTR3D_TEXH
local fd_y0 = 395.5
local fuelh =  86.0
local fuelw =  28.0
local fuely = fd_y0+29.5+fuelh

local int = math.floor

-- ==============================================================
local FuelMFD = Class(PanelElement)

function FuelMFD:new (v)
	PanelElement.new (self, v)

	self.isScram = false
	self.Mmain = 0.0
	self.Mrcs = 0.0
	self.Mscram = 0.0
	self.vc_grp = {}
	self.Tsample = oapi.get_simtime()
	self.oldmainmass = ""
	self.oldmaindv = ""
	self.oldMmain = ""
	self.oldrcsmass = ""
	self.oldrcsdv = ""
	self.oldMrcs = ""
	self.oldscrammass = ""
	self.oldscramdv = ""
	self.oldMscram = ""
	self.crd_VC = {}
end

-- ==============================================================

function FuelMFD:Reset2D (panelid, hMesh)
	if panelid ~= 0 then return end

	local dg = self.vessel
	self.isScram = dg:ScramVersion()

	local ges = {}
	ges.flags = GRPEDIT.SETUSERFLAG
	ges.UsrFlag = self.isScram and 0 or 3
	oapi.edit_meshgroup (hMesh, GRP_P0.FUEL_DISP_SCRAM, ges)
	ges.UsrFlag = self.isScram and 3 or 0
	oapi.edit_meshgroup (hMesh, GRP_P0.FUEL_DISP_NOSCRAM, ges)

	self.grp = oapi.mesh_group (hMesh, self.isScram and GRP_P0.FUEL_DISP_SCRAM or GRP_P0.FUEL_DISP_NOSCRAM)

	self.crd_2D = {}
	self.crd_2D[1] = 511.5
	self.crd_2D[2] = 425.5
	self.crd_2D[3] =   0.0
	self.crd_2D[4] =   0.0

	self.Mmain = dg:get_propellantmass(dg.ph_main)
	self.Mrcs = dg:get_propellantmass(dg.ph_rcs)
	if self.isScram then
		self.Mscram = dg:SubsysScram():GetPropellantMass()
	end
end

-- ==============================================================

function FuelMFD:ResetVC (hMesh)
	-- NEED TO DO VERTEX TRANSFORMATIONS HERE!

	local dg = self.vessel
	self.isScram = dg:ScramVersion()

	self.vc_grp.nVtx = 20
	if not self.vc_grp.Vtx then
		self.vc_grp.Vtx = oapi.create_ntvertexarray(self.vc_grp.nVtx)
	end

	local ges = {}
	ges.flags = GRPEDIT.SETUSERFLAG
	ges.UsrFlag = self.isScram and 3 or 0
	oapi.edit_meshgroup(hMesh, GRP_VC.PROPELLANT_STATUS_NOSCRAM, ges)
	ges.UsrFlag = self.isScram and 0 or 3
	oapi.edit_meshgroup(hMesh, GRP_VC.PROPELLANT_STATUS_SCRAM, ges)

	self.grpId = self.isScram and GRP_VC.PROPELLANT_STATUS_SCRAM or GRP_VC.PROPELLANT_STATUS_NOSCRAM
	if oapi.get_meshgroup(hMesh, self.grpId, self.vc_grp) ~= 0 then -- problems
		self.vc_grp.Vtx = nil
	else 
		local Vtx = self.vc_grp.Vtx
		self.crd_VC[1] = Vtx[9].y
		self.crd_VC[2] = Vtx[11].y
		self.crd_VC[3] = Vtx[9].z
		self.crd_VC[4] = Vtx[11].z
	end

	self.Mmain = dg:get_propellantmass(dg.ph_main)
	self.Mrcs = dg:get_propellantmass(dg.ph_rcs)
	if self.isScram then
		self.Mscram = dg:SubsysScram():GetPropellantMass()
	end
end

-- ==============================================================

function FuelMFD:Redraw (Vtx, surf, crd)
	local dg = self.vessel

	local xofs = INSTR3D_TEXW-424
	local yofs = 0
	local T = oapi.get_simtime()
	local dT = T-self.Tsample
	local m0 = dg:get_mass()

	-- main level
	local m = dg:get_propellantmass (dg.ph_main)
	local lvl = m / math.max (1.0, dg.max_rocketfuel)
	local isp = dg:get_thrusterisp (dg.th_main[1])
	local dv = isp * math.log(m0/(m0-m))
	local y = crd[1] + lvl*(crd[2]-crd[1])
	local z = crd[3] + lvl*(crd[4]-crd[3])
	local vofs = 8
	Vtx[vofs+2+1].y = y
	Vtx[vofs+3+1].y = y
	Vtx[vofs+2+1].z = z
	Vtx[vofs+3+1].z = z

	local str = string.format("% 6d", (int)(m+0.5)):sub(2)
	self:BltString (str, self.oldmainmass, 5, xofs+42, yofs+78, surf)
	self.oldmainmass = str

	str = string.format("% 6d", (int)(dv+0.5)):sub(2)
	self:BltString (str, self.oldmaindv, 5, xofs+42, yofs+106, surf)
	self.oldmaindv = str
	if dT > 0.0 then
		str = string.format("% 5.2f", (self.Mmain-m)/(T-self.Tsample))
		self:BltString (str, self.oldMmain, 5, xofs+42, yofs+156, surf)
		self.oldMmain = str
		self.Mmain = m
	end

	-- rcs level
	m = dg:get_propellantmass (dg.ph_rcs)
	lvl = m / RCS_FUEL_CAPACITY
	isp = ISP
	dv = isp * math.log(m0/(m0-m))
	y = crd[1] + lvl*(crd[2]-crd[1])
	z = crd[3] + lvl*(crd[4]-crd[3])
	vofs = 12
	Vtx[vofs+2+1].y = y
	Vtx[vofs+3+1].y = y
	Vtx[vofs+2+1].z = z
	Vtx[vofs+3+1].z = z

	str = string.format("% 6d", (int)(m+0.5)):sub(2)
	self:BltString (str, self.oldrcsmass, 5, xofs+134, yofs+78, surf)
	self.oldrcsmass = str

	str = string.format("% 6d", (int)(dv+0.5)):sub(2)
	self:BltString (str, self.oldrcsdv, 5, xofs+134, yofs+106, surf)
	self.oldrcsdv = str
	if dT > 0.0 then
		str = string.format("% 5.2f", (self.Mrcs-m)/(T-self.Tsample))
		self:BltString (str, self.oldMrcs, 5, xofs+134, yofs+156, surf)
		self.Mrcs = m
	end

	if self.isScram then
		-- scram level
		m = dg:SubsysScram():GetPropellantMass ()
		lvl = m / math.max (1.0, dg:SubsysScram():GetPropellantMaxMass())
		isp = dg:SubsysScram():GetThrusterIsp (1)
		dv = isp * math.log(m0/(m0-m))
		y = crd[1] + lvl*(crd[2]-crd[1])
		z = crd[3] + lvl*(crd[4]-crd[3])
		vofs = 16
		Vtx[vofs+2+1].y = y
		Vtx[vofs+3+1].y = y
		Vtx[vofs+2+1].z = z
		Vtx[vofs+3+1].z = z
		str = string.format("% 6d", (int)(m+0.5)):sub(2)
		self:BltString (str, self.oldscrammass, 5, xofs+226, yofs+78, surf)
		self.oldscrammass = str
		str = string.format("% 6d", (int)(dv+0.5)):sub(2)
		self:BltString (str, self.oldscramdv, 5, xofs+226, yofs+106, surf)
		self.oldscramdv = str
		if dT > 0.0 then
			str = string.format("% 5.2f", (self.Mscram-m)/(T-self.Tsample))
			self:BltString (str, self.oldMscram, 5, xofs+226, yofs+156, surf)
			self.oldMscram = str
			self.Mscram = m
		end
	end
	self.Tsample = T
end

-- ==============================================================

function FuelMFD:Redraw2D (surf)
	self:Redraw (self.grp.Vtx, surf, self.crd_2D)
	return false
end

-- ==============================================================

function FuelMFD:RedrawVC (hMesh, surf)
	if hMesh and surf then
		self:Redraw (self.vc_grp.Vtx, surf, self.crd_VC)
		local ges = {}
		ges.flags = GRPEDIT.VTXCRDY + GRPEDIT.VTXCRDZ
		ges.Vtx = self.vc_grp.Vtx
		oapi.edit_meshgroup (hMesh, self.grpId, ges)
	end
	return false
end

-- ==============================================================
local lutxofs = {
	['0'] = 0*8,
	['1'] = 1*8,
	['2'] = 2*8,
	['3'] = 3*8,
	['4'] = 4*8,
	['5'] = 5*8,
	['6'] = 6*8,
	['7'] = 7*8,
	['8'] = 8*8,
	['9'] = 9*8,
	['.'] = 80,
}

function FuelMFD:BltString (str, oldstr, maxlen, x, y, surf)
	local xofs = INSTR3D_TEXW-293
	local ysrc = 1

	local len = math.min(maxlen, #str)

	for i=1,len do
		local c = str:sub(i,i)
		local oldc = oldstr:sub(i,i)
		if c ~= oldc then
			local xsrc = xofs + (lutxofs[c] or 88)
			oapi.blt (surf, surf, x, y, xsrc, ysrc, 7, 9)
		end
		x = x + 7
	end
end

return FuelMFD
