local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024
local PANEL2D_WIDTH = 1280  -- panel width [pixel]


local InstrAtt = Class(PanelElement)

function InstrAtt:new (v)
	PanelElement.new (self, v)

	self.vc_grp = {}
end

-------------------------------------------------------------

function InstrAtt:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_BELOW)
	self.vtxofs = 0
end

-------------------------------------------------------------

function InstrAtt:ResetVC (hMesh)
	self.vc_grp.nVtx = 80
	if not self.vc_grp.Vtx then
		self.vc_grp.Vtx = oapi.create_ntvertexarray(self.vc_grp.nVtx)
	end
	if oapi.get_meshgroup (hMesh, GRP_VC.HORIZON, self.vc_grp) ~= 0 then -- problems
		self.vc_grp.Vtx = nil
	end
end

-------------------------------------------------------------

local alpha = {
	['0'] = 0,
	['1'] = 1,
	['2'] = 2,
	['3'] = 3,
	['4'] = 4,
	['5'] = 5,
	['6'] = 6,
	['7'] = 7,
	['8'] = 8,
	['9'] = 9
}

function InstrAtt:Redraw (Vtx)
	local bank  = self.vessel:get_bank()
	local pitch = self.vessel:get_pitch()
	local yaw   = self.vessel:get_yaw()
	if yaw < 0.0 then
		yaw = yaw + PI2
	end
	local alt   = self.vessel:get_altitude(ALTMODE.GROUND)
	local spd   = self.vessel:get_airspeed()
	local sinb = math.sin(bank)
	local cosb = math.cos(bank)

	local texw = INSTR3D_TEXW
	local texh = INSTR3D_TEXH

	local scaleh = 900.0
	local scalew = 154.0

	local horzx2 = texw-312

	-- transform articfical horizon
	local pitchscale = 315.0/(texh*PI05);  -- texcrd/rad
	local dy = pitchscale * (PI05/2.0)
	local dy2 = dy*0.5
	local scalecnt = (texh-scaleh*0.5)/texh
	local xp = {-108.0,-54.0,54.0,108.0,-108.0,-54.0,54.0,108.0,-6,6,-6,6}
	local yp = {54.0,108.0,108.0,54.0,-54.0,-108.0,-108.0,-54.0,49,49,37,37}
	local tv = {scalecnt-dy2,scalecnt-dy,scalecnt-dy,scalecnt-dy2,scalecnt+dy2,scalecnt+dy,scalecnt+dy,scalecnt+dy2}

	for i=1,12 do
		Vtx[i+self.vtxofs].x =  cosb*xp[i] + sinb*yp[i]
		Vtx[i+self.vtxofs].y = -sinb*xp[i] + cosb*yp[i]
		if i < 9 then
			Vtx[i+self.vtxofs].tv = tv[i]-pitch*pitchscale
		end
	end

	-- transform compass ribbon
	local yawrange = 121.0/texh
	local yawscale = 864.0/(texh*PI2)
	local tv_ofs = {1.0,1.0-yawrange,1.0,1.0-yawrange}
	for i=1,4 do
		Vtx[i+12+self.vtxofs].tv = tv_ofs[i] - yaw*yawscale
	end
	-- speed and altitude readout
	for disp = 0, 2 do
		local str
		local vofs, maxnum
		if disp == 0 then
			vofs = 16
			maxnum = 6
			str = oapi.formatvalue (alt):sub(2)
		elseif disp == 1 then
			vofs = 40
			maxnum = 6
			str = oapi.formatvalue (spd):sub(2)
		elseif disp == 2 then
			vofs = 64
			maxnum = 3
			str = string.format("%03d", math.floor(yaw*DEG+0.5))
		end

		vofs = vofs + self.vtxofs
		local numw = 10.0
		local num_ofs = texw-311.0

		local tu_num = {0,numw/texw,0,numw/texw}

		local lutofs = {
			[' '] = horzx2+107.5,
			['.'] = horzx2+101.0,
			['k'] = horzx2+116.0,
			['M'] = horzx2+127.0,
			['G'] = horzx2+137.0,
		}

		local len = math.min(#str, maxnum)
		for i = 1, len do
			local c = str:sub(i,i)
			local n = alpha[c]
			if n then
				local x = (n * numw + num_ofs)/texw
				for j=1,4 do
					Vtx[(i-1)*4+j+vofs].tu = tu_num[j]+x
				end
			else
				local ofs = lutofs[c] or horzx2
				if ofs > 0.0 then
					local tu = ofs/texw

					Vtx[(i-1)*4+vofs+1].tu = tu
					Vtx[(i-1)*4+vofs+2+1].tu = tu

					tu = numw/texw + tu
					Vtx[(i-1)*4+vofs+1+1].tu = tu
					Vtx[(i-1)*4+vofs+3+1].tu = tu
				end
			end
		end
	end
end

-------------------------------------------------------------

function InstrAtt:Redraw2D (surf)
	if self.grp then
		self:Redraw (self.grp.Vtx)

		-- transform vertices to 2D panel location
		local xcnt = 0.5*PANEL2D_WIDTH+1.0
		local ycnt = 150.0
		for i=1,12 do
			self.grp.Vtx[i+self.vtxofs].x = xcnt + self.grp.Vtx[i+self.vtxofs].x
			self.grp.Vtx[i+self.vtxofs].y = ycnt - self.grp.Vtx[i+self.vtxofs].y
		end
	end
	return false
end

-------------------------------------------------------------

function InstrAtt:RedrawVC (hMesh, surf)
	local Vtx = self.vc_grp.Vtx
	if hMesh and Vtx then
		self:Redraw (Vtx)

		-- transform vertices to VC location
		local rad = 0.055      -- display size param
		local tilt = 20.0*RAD  -- display tilt around x-axis
		local ycnt = 1.189     -- y-position of display centre (x is assumed 0)
		local zcnt = 7.285     -- z-position of display centre
		local cosa = math.cos(tilt)
		local sina = math.sin(tilt)
		local scale = rad/108.0
		for i=1,12 do
			-- scale and rotate for bank
			Vtx[i].x = Vtx[i].x * scale
			local y = Vtx[i].y*scale
			local z = i < 9 and -0.0005 or -0.001
			-- tilt to panel inclination
			Vtx[i].y = ycnt + y*cosa - z*sina
			Vtx[i].z = zcnt + y*sina + z*cosa
		end

		-- write back to mesh group
		local ges = {}
		ges.flags = GRPEDIT.VTXCRD + GRPEDIT.VTXTEX
		ges.Vtx  = self.vc_grp.Vtx
		oapi.edit_meshgroup (hMesh, GRP_VC.HORIZON, ges)
	end
	return false
end

return InstrAtt
