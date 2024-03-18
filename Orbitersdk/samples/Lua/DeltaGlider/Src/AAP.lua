-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local meshres = require("meshres_p0")
local GRP = meshres.GRP

local PANEL2D_WIDTH = 1280  -- panel width [pixel]
local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height
local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024

local texw = PANEL2D_TEXW -- texture width
local texh = PANEL2D_TEXH -- texture height
local xofs = 742
local yofs = 400


local AAP = Class()

function AAP:AttachHSI(_hsi) self.hsi = _hsi end

-- ==============================================================

function AAP:new(_subsys)
	DGPanelElement.new(self, _subsys:DG())
	self.subsys = _subsys
	
	self.hsi = nil
	run('dg/aap') -- load the autopilot code

	setvessel(_subsys:DG()) -- set autopilot vessel

	self.active_block = -1

	self.tgt = {0, 0, 0}
	self.active = {false, false, false}
	self.pactive = {false, false, false}

	self.scanmode = 0
	self.scanpmode = 0
end

-- ==============================================================

function AAP:Reset2D(panelid, hMesh)
	self.grp = oapi.mesh_group(hMesh, GRP.AAP_P0)
	if self.grp then
		self.vtxofs = 0
	end

	self.readout = {"      ", "      ", "      "}
end

-- ==============================================================

function AAP:Redraw2D(surf)
	-- readouts
	local c = self:DispStr(self.tgt[1]) -- altitude
	self.readout[1] = self:UpdateStr(c, 6, self.grp.vtx, self.vtxofs)
	c = self:DispStr(self.tgt[2]) -- airspeed
	self.readout[2] = self:UpdateStr (c, 6, self.grp.vtx, self.vtxofs+6*4)
	c = string.format("%03d", math.floor(self.tgt[3]*DEG+0.5) % 360)
	self.readout[3] = self:UpdateStr (c, 3, self.grp.vtx,self.vtxofs+12*4)

	-- activation buttons
	for i = 1,3 do
		if self.active[i] ~= self.pactive[i] then
			self.pactive[i] = self.active[i]

			local yofs = texh - 683 + (self.active[i] and 14 or 0)
			local vofs = vtxofs + 18*4 + (i-1)*4

			for j=0,3 do
				local vtx = self.grp.vtx[vofs+j]
				vtx.tv = (yofs + (j/2)*12.0)/texh
				self.grp.vtx[vofs+j] = vtx
			end
		end
	end

	if self.active_block >= 0 then
		-- scan switch
		if self.scanmode ~= self.scanpmode then
			self.scanpmode = self.scanmode
			local xofs = 1068.0 + (scanmode == 0 and 0 or (scanmode == -1 and 16 or 32))
			local vofs = vtxofs + 21*4 + active_block*4
			for j=0,3 do
				local vtx = self.grp.vtx[vofs+j]
				vtx.tu = (xofs - (j/2)*14.0)/texw
				self.grp.vtx[vofs+j] = vtx
			end
		end
	end
	return false
end

-- ==============================================================

local AAP_ProcessMouse2D_t0 = 0
local AAP_ProcessMouse2D_tp = 0
function AAP:ProcessMouse2D(event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		if my >= 0 and my < 124 then
			self.active_block = my/46
			local dmy = my-self.active_block*46
			if mx >= 0 and mx < 14 and dmy >= 0 and dmy < 14 then
				-- activation button
				self:ToggleActive(self.active_block)
				return true;
			elseif mx >= 21 and mx < 65 and dmy >= 19 and dmy < 32 then
				-- scan switch
				self.scanmode = mx-21 < 22 and -1 or 1
				AAP_ProcessMouse2D_t0 = oapi.get_systime()
				AAP_ProcessMouse2D_tp = AAP_ProcessMouse2D_t0
				return true
			end
		else
			self.active_block = -1
		end
	elseif event == PANEL_MOUSE.LBUP then
		self.scanmode = 0
		return self.scanmode ~= self.scanpmode
	end
	
	if self.scanmode ~= 0 and event == PANEL_MOUSE.LBPRESSED then
		local t = oapi.get_systime()
		local dt = math.max(t - AAP_ProcessMouse2D_t0, 0.0)

		if self.active_block == 0 or self.active_block == 1 then
			local mag = dt < 1 and 2 or (dt < 2 and 1 or 0)
			if t-AAP_ProcessMouse2D_tp > 0.5-mag*0.2 then
				AAP_ProcessMouse2D_tp = t
				local step = math.max(1.0, math.min(1e4,math.pow(10,math.floor(math.log10 (math.max(self.tgt[self.active_block+1],1.0)))-mag)))
				self.tgt[self.active_block+1] = math.max(0.0,math.floor(self.tgt[self.active_block+1]/step)*step + self.scanmode*step)
				if self.active[self.active_block+1] then
					self:SetValue (self.active_block, self.tgt[self.active_block+1])
				end
				return true
			end
		elseif self.active_block == 2 then -- heading/course
			local mag = math.min (math.sqrt(dt)*0.2, 0.8)
			local step = oapi.get_sysstep() * mag * self.scanmode
			self.tgt[3] = self.tgt[3] + step

			while self.tgt[3] < 0.0 do
				self.tgt[3] = self.tgt[3] + PI2
			end
			while self.tgt[3] >= PI2 do
				self.tgt[3] = self.tgt[3] - PI2
			end

			if self.active[3] then
				self:SetValue (2, self.tgt[3])
			end
			if self.hsi then
				self.hsi:SetCrs (tgt[3])
			end
			return true
		end
	end
	return false
end

---------------------------------------------------------------

function AAP:SetValue (block, val)
	if block == 0 then
		aap.alt(val)
	elseif block == 1 then
		aap.spd(val)
	elseif block == 2 then
		aap.hdg(val*DEG)
	end
end

function  AAP:ToggleActive (block)
	self:SetActive (block, not self.active[block+1])
end

function AAP:SetActive (block, activate)
	if activate == self.active[block+1] then return end -- nothing to do

	self.active[block+1] = activate

	if block == 0 then
		if activate then
			aap.alt(val)
		else
			aap.alt()
		end
	elseif block == 1 then
		if activate then
			aap.spd(val)
		else
			aap.spd()
		end
	elseif block == 2 then
		if activate then
			aap.hdg(val*DEG)
		else
			aap.hdg()
		end
	end
end

function AAP:WriteScenario(scn)
	oapi.writeScenario_string (scn, "AAP", string.format("%d:%g %d:%g %d:%g", self.active[1], self.tgt[1],
		self.active[2], self.tgt[2],self.active[3], self.tgt[3]))
end

function AAP:SetState(str)
	local res = {}
	if scenario_line_match(str, "AAP %d:%f %d:%f %d:%f", res) then
		self.tgt[1] = res[1]
		self:SetActive(0, res[2] ~= 0)
		self.tgt[2] = res[3]
		self:SetActive(0, res[4] ~= 0)
		self.tgt[3] = res[5]
		self:SetActive(0, res[6] ~= 0)
	end
end

-- ==============================================================

function AAP:UpdateStr (str, vtx, vtxofs)
	local dx = 9.0
	-- flush right
	str = ("                             "..str):sub(-len)

	local lut = {
		["0"] = 1126,
		["1"] = 1126 + dx,
		["2"] = 1126 + 2 * dx,
		["3"] = 1126 + 3 * dx,
		["4"] = 1126 + 4 * dx,
		["5"] = 1126 + 5 * dx,
		["6"] = 1126 + 6 * dx,
		["7"] = 1126 + 7 * dx,
		["8"] = 1126 + 8 * dx,
		["9"] = 1126 + 9 * dx,
		["."] = 1215.5,
		["k"] = 1229.5,
		["M"] = 1238.5,
		["G"] = 1221.5,
	}

	for i = 1, #str do
		local c = str:sub(i,i)

		local x = lut[c]
		if x == nil then
			x = 1221.5
		end

		for j = 0, 2 do
			local v = vtx[(i-1)*4+j + vtxofs + 1]
			v.tu = (x + (j%2)*dx)/texw
			vtx[(i-1)*4+j + vtxofs + 1] = v
		end

	end
	return str
end

return AAP
