-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: AAP.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================
local DGPanelElement = require("DGPanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP

local PANEL2D_WIDTH = 1280  -- panel width [pixel]
local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height
local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024

local texw = PANEL2D_TEXW -- texture width
local texh = PANEL2D_TEXH -- texture height
local xofs = 742
local yofs = 400




local aap = Class()

function aap:new(vessel)
	self.vessel = vessel
	self.altid = nil
	self.spdid = nil
	self.bnkid = nil
	self.hdgid = nil
	self.tgtalt = nil
	self.tgtspd = nil
	self.tgtbnk = nil
	self.tgthdg = nil
	self.maxasc = 30
	self.maxdsc = -20
end

function aap:slope ()
    local as = self.vessel:get_airspeedvector(REFFRAME.HORIZON)
    local xz = math.sqrt (as.x^2 + as.z^2)
    local sl = math.atan2 (as.y, xz)
    return sl
end

--[[ --------------------------------------------------------
altitude autopilot (elevator control)
argument 1: target altitude (m)
note: does not terminate (run as background job and kill
      when done)
--]]

local toto = 0
function aap:alt_ap (alt)
    if alt == nil then
        alt = 20e3
    end
    self.tgtalt = alt
    local dslope0 = 0
    local dslope_rate
    while true do
		toto = toto + 1
        if self.tgtalt ~= nil then
            local dt = oapi.get_simstep()
            alt = self.vessel:get_altitude()
            local dalt = self.tgtalt-alt
            local tgt_slope = RAD*1e-2 * dalt
	        if tgt_slope > self.maxasc*RAD then
		        tgt_slope = self.maxasc*RAD
	        elseif tgt_slope < self.maxdsc*RAD then
		        tgt_slope = self.maxdsc*RAD
	        end
            local dslope = tgt_slope-self:slope()
            if dslope0 == 0 then
                dslope_rate = 0
            else
                dslope_rate = (dslope-dslope0)/dt
            end
            dslope0 = dslope
            local delev = (dslope*0.1 + dslope_rate)*(dt*10.0)
            local elev = self.vessel:get_adclevel(AIRCTRL.ELEVATOR)
            elev = elev+delev
            if elev > 1 then elev = 1 elseif elev < -1 then elev = -1 end
            self.vessel:set_adclevel(AIRCTRL.ELEVATOR,elev)
        end
        proc.skip()
    end
end

--[[ --------------------------------------------------------
airspeed autopilot (throttle control)
argument 1: target speed (m/s)
note: does not terminate (run as background job and kill
      when done)
--]]

function aap:spd_ap (spd)
    if spd ~= nil then self.tgtspd = spd end
    local spd0 = self.vessel:get_airspeed()
    local acc, dpsd
    local alpha = 1
    local beta = 1
    while true do
        if self.tgtspd ~= nil then
            local dt = oapi.get_simstep()
            spd = self.vessel:get_airspeed()
            acc = (spd-spd0)/dt
            spd0 = spd
            local dspd = self.tgtspd-spd
            local dthrott = (dspd*alpha - acc*beta)*dt
            local thrott = self.vessel:get_thrustergrouplevel (THGROUP.MAIN)
            self.vessel:set_thrustergrouplevel (THGROUP.MAIN, thrott+dthrott)
        end
		proc.skip()
    end
end

--[[ --------------------------------------------------------
bank autopilot (aileron control)
argument 1: bank angle [deg]
note: does not terminate (run as background job and kill
      when done)
--]]

function aap:bank_ap (bnk)
    self.tgtbnk = bnk
    local bnk0 = self.vessel:get_bank()
    while true do
        local dt = oapi.get_simstep()
        bnk = self.vessel:get_bank()
        local dbnk = bnk-bnk0
        if dbnk < -PI then dbnk = dbnk+2*PI elseif dbnk > PI then dbnk = dbnk-2*PI end -- phase unwrap
        local rate = dbnk/dt
        bnk0 = bnk
        dbnk = self.tgtbnk*RAD - bnk
        if dbnk < -PI then dbnk = dbnk+2*PI elseif dbnk > PI then dbnk = dbnk-2*PI end -- phase unwrap
        local dail = (-dbnk*0.1 + rate*0.3)*(dt*5.0) -- the damping term should really depend on atmospheric density
        local ail = self.vessel:get_adclevel(AIRCTRL.AILERON)
        ail = ail+dail
        if ail > 1 then ail = 1 elseif ail < -1 then ail = -1 end
        self.vessel:set_adclevel(AIRCTRL.AILERON,ail)
        proc.skip()
    end
end

--[[ --------------------------------------------------------
heading autopilot (aileron+pitch control)
argument 1: heading angle [deg]
note: does not terminate (run as background job and kill
      when done)
--]]

function aap:heading_ap (hdg)
    self.tgthdg = hdg
    local hdg0 = self.vessel:get_yaw()
    local tgtbank = self.vessel:get_bank()
    while true do
        local dt = oapi.get_simstep()
        hdg = self.vessel:get_yaw()
        local dhdg = hdg-hdg0
        if dhdg < -PI then dhdg = dhdg+2*PI elseif dhdg > PI then dhdg = dhdg-2*PI end -- phase unwrap
        local rate = dhdg/dt
        hdg0 = hdg
        dhdg = self.tgthdg*RAD - hdg
        if dhdg < -PI then dhdg = dhdg+2*PI elseif dhdg > PI then dhdg = dhdg-2*PI end -- phase unwrap
        local dbank = (-dhdg*100 + rate*1000)*dt
        local tgtbank = self.vessel:get_bank() + dbank
        if tgtbank > 0.3*PI then tgtbank = 0.3*PI elseif tgtbank < -0.3*PI then tgtbank = -0.3*PI end
        self.tgtbnk = tgtbank*DEG
        proc.skip()
    end
end

-- -----------------------------------------------------------
-- User level entry functions
-- -----------------------------------------------------------

-- Invoke altitude autopilot as background job

function aap:alt (alt)
    if alt == nil then
        if self.altid ~= nil then
            proc.kill(self.altid)
            self.altid = nil
            self.vessel:set_adclevel(AIRCTRL.ELEVATOR,0)
        end
    else
        if self.altid == nil then
            self.altid = proc.bg(self.alt_ap, self, alt) -- launch altitude autopilot
        else
            self.tgtalt = alt -- autopilot already running: just reset altitude
        end
    end
end

-- Invoke speed autopilot as background job

function aap:spd (spd)
    if spd == nil then
        if self.spdid ~= nil then
            proc.kill(self.spdid)
            self.spdid = nil
        end
    else
        if self.spdid == nil then
            self.spdid = proc.bg(self.spd_ap, self, spd) -- launch airspeed autopilot
        else
            self.tgtspd = spd -- autopilot already running: just reset target speed
        end
    end
end

-- Invoke bank autopilot as background job

function aap:bank (bank)
    if bank == nil then
        if self.bnkid ~= nil then
            proc.kill(self.bnkid)
            self.bnkid = nil
            self.vessel:set_adclevel(AIRCTRL.AILERON,0)
        end
    else
        if self.bnkid == nil then
            self.bnkid = proc.bg(self.bank_ap, self, bank) -- launch bank autopilot
        else
            self.tgtbnk = bank -- autopilot already running: just reset target angle
        end
    end
end

-- Invoke heading autopilot as background job

function aap:hdg (hdg)
    if hdg == nil then
        if self.hdgid ~= nil then
            proc.kill(self.hdgid)
            self:bank() -- kill bank ap
            self.hdgid = nil
        end
    else
        if self.hdgid == nil then
            self.hdgid = proc.bg(self.heading_ap, self, hdg) -- launch heading autopilot
            self:bank(0) -- invoke bank ap
        else
            self.tgthdg = hdg -- autopilot already running: just reset target angle
        end
    end
end






local AAP = Class(DGPanelElement)

function AAP:AttachHSI(_hsi) self.hsi = _hsi end

-- ==============================================================

function AAP:new(_subsys)
	DGPanelElement.new(self, _subsys:DG())
	self.subsys = _subsys
	
	self.hsi = nil
	self.aap = aap(_subsys:DG())

	self.active_block = -1

	self.tgt = {0, 0, 0}
	self.active = {false, false, false}
	self.pactive = {false, false, false}

	self.scanmode = 0
	self.scanpmode = 0
end

-- ==============================================================

function AAP:Reset2D(panelid, hMesh)
	self.grp = oapi.mesh_group(hMesh, GRP_P0.AAP)
	if self.grp then
		self.vtxofs = 0
	end

	self.readout = {"      ", "      ", "      "}
end

-- ==============================================================

function AAP:Redraw2D(surf)
	-- readouts
	local c = self:DispStr(self.tgt[1]) -- altitude
	self.readout[1] = self:UpdateStr(c, 6, self.grp.Vtx, self.vtxofs)
	c = self:DispStr(self.tgt[2]) -- airspeed
	self.readout[2] = self:UpdateStr (c, 6, self.grp.Vtx, self.vtxofs+6*4)
	c = string.format("%03d", math.floor(self.tgt[3]*DEG+0.5) % 360)
	self.readout[3] = self:UpdateStr (c, 3, self.grp.Vtx,self.vtxofs+12*4)

	-- activation buttons
	for i = 1,3 do
		if self.active[i] ~= self.pactive[i] then
			self.pactive[i] = self.active[i]

			local yofs = texh - 683 + (self.active[i] and 14 or 0)
			local vofs = self.vtxofs + 18*4 + (i-1)*4

			for j=0,3 do
				self.grp.Vtx[vofs+j+1].tv = (yofs + math.floor(j/2)*12.0)/texh
			end
		end
	end

	if self.active_block >= 0 then
		-- scan switch
		if self.scanmode ~= self.scanpmode then
			self.scanpmode = self.scanmode
			local xofs = 1068.0 + (self.scanmode == 0 and 0 or (self.scanmode == -1 and 16 or 32))
			local vofs = self.vtxofs + 21*4 + self.active_block*4
			for j=0,3 do
				self.grp.Vtx[vofs+j+1].tu = (xofs - math.floor(j/2)*14.0)/texw
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
			self.active_block = math.floor(my/46)
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
				self.hsi:SetCrs (self.tgt[3])
			end
			return true
		end
	end
	return false
end

---------------------------------------------------------------

function AAP:SetValue (block, val)
	if block == 0 then
		self.aap:alt(val)
	elseif block == 1 then
		self.aap:spd(val)
	elseif block == 2 then
		self.aap:hdg(val*DEG)
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
			self.aap:alt(self.tgt[block+1])
		else
			self.aap:alt()
		end
	elseif block == 1 then
		if activate then
			self.aap:spd(self.tgt[block+1])
		else
			self.aap:spd()
		end
	elseif block == 2 then
		if activate then
			self.aap:hdg(self.tgt[block+1]*DEG)
		else
			self.aap:hdg()
		end
	end
end

function AAP:WriteScenario(scn)
	oapi.writescenario_string (scn, "AAP", string.format("%d:%f %d:%f %d:%f", self.active[1] and 1 or 0, self.tgt[1],
		self.active[2] and 1 or 0, self.tgt[2],self.active[3] and 1 or 0, self.tgt[3]))
end

function AAP:SetState(str)
	local res = {}
	if scenario_line_match(str, "AAP %b:%f %b:%f %b:%f", res) then
		self.tgt[1] = res[2]
		self:SetActive(0, res[1])
		self.tgt[2] = res[4]
		self:SetActive(0, res[3])
		self.tgt[3] = res[6]
		self:SetActive(0, res[5])
	end
end

-- ==============================================================

function AAP:UpdateStr (str, len, vtx, vtxofs)
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

		local x = lut[c] or 1221.5

		for j = 0, 3 do
			vtx[(i-1)*4+j + vtxofs + 1].tu = (x + (j%2)*dx)/texw
		end

	end
	return str
end

return AAP
