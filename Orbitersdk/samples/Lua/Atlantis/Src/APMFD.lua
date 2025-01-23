-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--                 ORBITER MODULE: Atlantis
--                  Part of the ORBITER SDK
--
-- AscentAP.lua
-- Class implementation for Atlantis ascent autopilot
-- Automatic control of ascent profile from liftoff to
-- ET separation using engine gimballing of SSME and SRB engines
--
-- Port to Lua by TheGondos
-- ==============================================================


-- ==============================================================
-- class AscentApMfd: MFD interface for ascent autopilot
-- ==============================================================

local AscentApMfd = Class()

local snd = xrsound.create_instance("Atlantis.lua")
snd:load_wav(1, "XRSound/Default/Autopilot On.wav",  PlaybackType.InternalOnly)
snd:load_wav(2, "XRSound/Default/Autopilot Off.wav", PlaybackType.InternalOnly)

-- ==============================================================
-- init: called automatically when the MFD instance is created.
--        This is your one chance to save the vessel handle
--        and the MFD width and height
-- ==============================================================
function AscentApMfd:new(mfd, w, h, vessel)
	self.vessel = vessel
	self.w = w
	self.h = h
	self.ap = vessel.get_Ap()
	self.ap.set_launchazimuth(self.ap.get_launchazimuth())
	self.cpg = 0
	self.pen = oapi.create_pen(1, 1, _RGB(96, 96, 96))
	self.ref_t = oapi.get_systime()
end


-- ==============================================================
-- update : refresh the MFD display
-- ==============================================================
function AscentApMfd:update(skp)
	local title = string.format("Ascent P%d/4       MET:%s", self.cpg + 1, MetStr(self.ap.met()))
	self:set_title(skp, title)

	if self.ap.active() then
		local ch, cw = skp:get_charsize()
		skp:set_backgroundmode(SKP.OPAQUE)
		skp:set_backgroundcolor(0xffffff)
		skp:set_textcolor(0x000000)
		skp:text((27*cw)/2, 0, "ACT", 3)
		skp:set_backgroundmode(SKP.TRANSPARENT)
	end
	skp:set_textcolor(0x00ff00)

	if self.cpg == 0 then
		self:update_pg_prm(skp)
	elseif self.cpg == 1 then
		self:update_pg_gbl(skp)
	end

	return true
end

-- ==============================================================
-- update_pg_prm : display the primary MFD page
-- ==============================================================
function AscentApMfd:update_pg_prm(skp)
	local ch, cw = skp:get_charsize()
	if self.vessel.get_status() == 0 then
		skp:text(cw/2, (ch*3)/2, string.format("Launch azimuth: %0.1fº", self.ap.get_launchazimuth()*DEG))
		skp:text(cw/2, (ch*5)/2, string.format("Orbit inc:      %0.1fº", self.ap.get_targetinclination()*DEG))
		skp:text(cw/2, (ch*7)/2, string.format("Orbit altitude: %0.1fkm", self.ap.get_orbitaltitude()*1e-3))
		skp:text(cw/2, (ch*9)/2, string.format("OMS2 scheduled: %s", self.ap.get_OMS2schedule() and "yes" or "no"))
	else
		local hRef = self.vessel:get_gravityref()
		local R = oapi.get_size(hRef)
		local az_tgt = self.ap.get_targetazimuth()
		local az_cur = self.vessel:get_yaw()
		local az_err = math.abs(az_cur-az_tgt)
		if az_err > PI then az_err = PI2-az_err end
		if az_cur < az_tgt then az_err = -az_err end
		local pt_tgt = self.ap.get_targetpitch()
		local pt_cur = self.vessel:get_pitch()
		local pt_err = pt_cur-pt_tgt
		local alt_tgt = self.ap.get_orbitaltitude()
		local alt_ap_cur = self.vessel:get_apdist() - R
		local alt_pe_cur = self.vessel:get_pedist() - R
		skp:text(cw*13, ch*2, "Cur    Tgt    D", 15)
		skp:text(cw/2,  ch*3, "Azimuth [º]", 11)
		skp:text(cw*13, ch*3, string.format("%0.1f", az_cur*DEG))
		skp:text(cw*20, ch*3, string.format("%0.1f", az_tgt*DEG))
		skp:text(cw*27, ch*3, string.format("%+0.2f", az_err*DEG))
		skp:text(cw/2,  ch*4, "Pitch [º]", 9)
		skp:text(cw*13, ch*4, string.format("%0.1f", pt_cur*DEG))
		skp:text(cw*20, ch*4, string.format("%0.1f", pt_tgt*DEG))
		skp:text(cw*27, ch*4, string.format("%+0.2f", pt_err*DEG))
		skp:text(cw/2,  ch*5, "Ap.Alt [km]", 11)
		skp:text(cw*12, ch*5, string.format("% 0.1f", alt_ap_cur*1e-3))
		skp:text(cw*20, ch*5, string.format("%0.1f", alt_tgt*1e-3))
		skp:text(cw*27, ch*5, string.format("%+0.2f", (alt_ap_cur-alt_tgt)*1e-3))
		skp:text(cw/2,  ch*6, "Pe.Alt [km]", 11)
		skp:text(cw*12, ch*6, string.format("% 0.1f", alt_pe_cur*1e-3))
		skp:text(cw*20, ch*6, string.format("%0.1f", alt_tgt*1e-3))
		skp:text(cw*27, ch*6, string.format("%+0.2f", (alt_pe_cur-alt_tgt)*1e-3))

		if self.vessel.get_status() == 3 then
			if self.ap.get_schedule_oms1() > 0.0 then
			elseif self.ap.get_met_oms1_start() > 0.0 then
				skp:text(cw / 2, ch * 8, "OMS-1", 5)
			elseif self.ap.get_OMS2schedule() then
				if self.ap.get_met_oms_start() < 0.0 then
					local dt = self.ap.get_schedule_oms()
					skp:text(cw / 2, ch * 8, string.format("OMS-2: MET%+0.0f", dt))
				elseif self.ap.get_met_oms_end() < 0.0 then
					skp:text(cw / 2, ch * 8, "OMS-2", 5)
				end
			end
		end
	end
end

-- ==============================================================
-- update_pg_gbl : display the "gimbal" MFD page
-- ==============================================================
function AscentApMfd:update_pg_gbl(skp)
	local ch, cw = skp:get_charsize()
	local iW = self.w
	local s2 = iW/16
	local s1 = s2*2
	local cx = iW/2
	local cy = ch*3+s1
	local ssme_cx = {(2*iW)/10, (8*iW)/10, cx}
	local ssme_cy = {cy+(3*s1)/2, cy+(3*s1)/2, cy}

	skp:set_textalign(SKP.CENTER)
	skp:text(iW/2, cy-s1-(3*ch)/2, "Gimbal SSME", 11)
	for i=0,2 do
		local pitch, yaw = self.vessel.get_SSME_gimbal_pos(i)
		self:draw_gimbal(skp, ssme_cx[i+1], ssme_cy[i+1], pitch, yaw)
	end

	if self.vessel.get_status() < 2 then
		local srb_cy = self.h-s1-ch
		local srb_cx = {(2*iW)/10, (8*iW)/10}
		skp:line(0, srb_cy-s1-ch*2, iW, srb_cy-s1-ch*2)
		skp:text(iW/2, srb_cy-s1-(3*ch)/2, "Gimbal SRB", 10)
		for i=0,1 do
			local pitch, yaw = self.vessel.get_SRB_gimbal_pos(i)
			self:draw_gimbal(skp, srb_cx[i+1], srb_cy, pitch, yaw)
		end
	end
end

-- ==============================================================
-- draw_gimbal : draw a gimbal indicator
-- ==============================================================
function AscentApMfd:draw_gimbal(skp, cx, cy, pitch, yaw)
	local s2 = self.w/16
	local s1 = s2*2
	local s = s2/2

	local range = 10.5*RAD
	local x = yaw/range * s1 + 0.5
	local y = pitch/range * s1 + 0.5
	local ppen = skp:set_pen(self.pen)
	skp:rectangle(cx-s1, cy-s1, cx+s1+1, cy+s1+1)
	skp:rectangle(cx-s2, cy-s2, cx+s2+1, cy+s2+1)
	skp:line(cx-s1, cy, cx+s1, cy)
	skp:line(cx, cy-s1, cx, cy+s1)
	skp:set_pen(ppen)

	skp:line(cx+x-s, cy+y, cx+x+s+1, cy+y)
	skp:line(cx+x, cy+y-s, cx+x, cy+y+s+1)
end

-- ==============================================================
-- buttonlabel : return the label for an MFD button
-- ==============================================================
function AscentApMfd:buttonlabel(bt)
	if bt == 0 then
		return self.ap.active() and "DA" or (self.vessel.get_status() == 0 and "L" or "EA")
	end

	if bt <= 2 then
		local label = {"PG-", "PG+"}
		return label[bt]
	end

	if self.cpg == 0 then
		if self.ap.active() or self.vessel.get_status() ~=0 then return nil end
		local label = {"AZ-", "AZ+", "AL-", "AL+", "OM2"}
		return bt < 8 and label[bt-2] or nil
	end

	return nil
end

-- ==============================================================
-- buttonmenu : return the menu entries for the MFD
-- ==============================================================
function AscentApMfd:buttonmenu()
	local mnu = {
		{                                         sel = 'L'},
		{l1 = "Prev page",                        sel = ','},
		{l1 = "Next page",                        sel = '.'},
		{l1 = "Decrease Launch", l2 = "azimuth",  sel = ';'},
		{l1 = "Increase Launch", l2 = "azimuth",  sel = "'"},
		{l1 = "Decrease Target", l2 = "altitude", sel = '-'},
		{l1 = "Increase Target", l2 = "altitude", sel = '='},
		{l1 = "Schedule OMS2",   l2 = "",         sel = 'O'}
	}
	if not self.ap.active() then
		if self.vessel.get_status() == 0 then
			mnu[1].line1 = "Launch"
			mnu[1].sel = 'L'
		else
			mnu[1].line1 = "Engage AP"
			mnu[1].sel = 'E'
		end
	else
		mnu[1].line1 = "Disengage AP"
		mnu[1].sel = 'D'
	end
	local nmnu = (self.cpg == 0 and not self.ap.active() and self.vessel.get_status() == 0) and 8 or 3
	return mnu, nmnu
end

-- ==============================================================
-- consumekeybuffered : handle key events
-- ==============================================================
function AscentApMfd:consumekeybuffered(key)
	local commands = {
		[OAPI_KEY.L] = self.on_launch,
		[OAPI_KEY.D] = self.on_disengage,
		[OAPI_KEY.E] = self.on_engage,
		[OAPI_KEY.COMMA] = self.dec_page,
		[OAPI_KEY.PERIOD] = self.inc_page,
		[OAPI_KEY.SEMICOLON] = self.init_dec_azimuth,
		[OAPI_KEY.APOSTROPHE] = self.init_inc_azimuth,
		[OAPI_KEY.MINUS] = self.init_dec_altitude,
		[OAPI_KEY.EQUALS] = self.init_inc_altitude,
		[OAPI_KEY.O] = self.toggle_OMS2_schedule,
	}
	if commands[key] then
		return commands[key](self)
	end

	return false
end

-- ==============================================================
-- consumebutton : handle button events
-- ==============================================================
function AscentApMfd:consumebutton(bt, event)
	if bt == 0 and bit.allset(event, PANEL_MOUSE.LBDOWN) then
		if self.ap.active() then
			return self:on_disengage()
		elseif self.vessel.get_status() == 0 then
			return self:on_launch()
		else
			return self:on_engage()
		end
	end

	if bt < 3 and bit.allset(event, PANEL_MOUSE.LBDOWN) then
		if bt == 1 then
			return self:dec_page()
		elseif bt == 2 then
			return self:inc_page()
		end
	end

	if bt < 8 and self.cpg == 0 and not self.ap.active() and self.vessel.get_status() == 0 then
		if bit.allset(event, PANEL_MOUSE.LBDOWN) then
			if bt == 3 then
				return self:init_dec_azimuth()
			elseif bt == 4 then
				return self:init_inc_azimuth()
			elseif bt == 5 then
				return self:init_dec_altitude()
			elseif bt == 6 then
				return self:init_inc_altitude()
			elseif bt == 7 then
				return self:toggle_OMS2_schedule()
			end
		elseif bit.allset(event, PANEL_MOUSE.LBUP) then
			if self.set_mode ~= "MODE_NONE" then
				self.set_mode = "MODE_NONE"
				return true
			end
		elseif bit.allset(event, PANEL_MOUSE.LBPRESSED) then
			if bt == 3 then
				return self:dec_azimuth()
			elseif bt == 4 then
				return self:inc_azimuth()
			elseif bt == 5 then
				return self:dec_altitude()
			elseif bt == 6 then
				return self:inc_altitude()
			end
		end
	end

	return false
end

-- ==============================================================
-- dec_page / inc_page : cycle through the MFD pages
-- ==============================================================
local npage = 4
function AscentApMfd:dec_page()
	self.cpg = self.cpg == 0 and npage-3 or self.cpg-1
	self:invalidate_buttons()
	self:invalidate_display()
	return true
end


function AscentApMfd:inc_page()
	self.cpg = self.cpg == npage-1 and 0 or self.cpg+1
	self:invalidate_buttons()
	self:invalidate_display()
	return true
end

-- ==============================================================
-- on_launch : start the launch sequence
-- ==============================================================
function AscentApMfd:on_launch()
	if not self.ap.active() then
		if self.vessel.get_status() == 0 then
			snd:play_wav(1)
			self.ap.launch()
			self:invalidate_buttons()
		end
	end
	return true
end


-- ==============================================================
-- on_engage : engage the autopilot
-- ==============================================================
function AscentApMfd:on_engage()
	if not self.ap.active() then
		snd:play_wav(1)
		self.ap.engage()
		self:invalidate_buttons()
	end
	return true
end


-- ==============================================================
-- on_disengage : disengage the autopilot
-- ==============================================================
function AscentApMfd:on_disengage()
	if self.ap.active() then
		snd:play_wav(2)
		self.ap.disengage()
		self:invalidate_buttons()
	end
	return true
end


-- ==============================================================
-- on_disengage : disengage the autopilot
-- ==============================================================
function AscentApMfd:init_dec_azimuth()
	self.set_mode = "MODE_AZIMUTH_DEC"
	self.ref_t = oapi.get_systime()
	self.ref_val = math.max(self.ap.get_launchazimuth()-RAD*0.1, 0.0)
	self.ap.set_launchazimuth(self.ref_val)
	self:invalidate_display()
	return true
end


function AscentApMfd:init_inc_azimuth()
	self.set_mode = "MODE_AZIMUTH_INC"
	self.ref_t = oapi.get_systime()
	self.ref_val = math.min(self.ap.get_launchazimuth()+RAD*0.1, PI2)
	self.ap.set_launchazimuth(self.ref_val)
	self:invalidate_display()
	return true
end


function AscentApMfd:dec_azimuth()
	local dt = oapi.get_systime()-self.ref_t
	if dt < 0.2 then return true end
	local da = -math.min(3.0,dt)*RAD*0.2
	if dt > 3.0 then
		da = da - math.min(dt-3.0,3.0)*RAD*2.0
	end
	if dt > 6.0 then
		da = da - (dt-6.0)*RAD*20.0
	end
	local az = math.max(self.ref_val + da, 0.0)
	self.ap.set_launchazimuth(az)
	self:invalidate_display()
	return true
end


function AscentApMfd:inc_azimuth()
	local dt = oapi.get_systime()-self.ref_t
	if dt < 0.2 then return true end
	local da = math.min(3.0,dt)*RAD*0.2
	if dt > 3.0 then
		da = da + math.min(dt-3.0,3.0)*RAD*2.0
	end
	if dt > 6.0 then
		da = da + (dt-6.0)*RAD*20.0
	end
	local az = math.min(self.ref_val + da, PI2)
	self.ap.set_launchazimuth(az)
	self:invalidate_display()
	return true
end

function AscentApMfd:init_dec_altitude()
	self.set_mode = "MODE_AZIMUTH_DEC"
	self.ref_t = oapi.get_systime()
	self.ref_val = math.max(self.ap.get_orbitaltitude()-100, 0.0)
	self.ap.set_orbitaltitude(self.ref_val)
	self:invalidate_display()
	return true
end


function AscentApMfd:init_inc_altitude()
	self.set_mode = "MODE_AZIMUTH_INC"
	self.ref_t = oapi.get_systime()
	self.ref_val = self.ap.get_orbitaltitude()+100
	self.ap.set_orbitaltitude(self.ref_val)
	self:invalidate_display()
	return true
end


function AscentApMfd:dec_altitude()
	local dt = oapi.get_systime()-self.ref_t
	if dt < 0.2 then return true end
	local da = -math.min(3.0,dt)*1e3*0.2
	if dt > 3.0 then
		da = da - math.min(dt-3.0,3.0)*1e3*2.0
	end
	if dt > 6.0 then
		da = da - (dt-6.0)*1e3*20.0
	end
	local alt = math.max(self.ref_val + da, 0.0)
	self.ap.set_orbitaltitude(alt)
	self:invalidate_display()
	return true
end


function AscentApMfd:inc_altitude()
	local dt = oapi.get_systime()-self.ref_t
	if dt < 0.2 then return true end
	local da = math.min(3.0,dt)*1e3*0.2
	if dt > 3.0 then
		da = da + math.min(dt-3.0,3.0)*1e3*2.0
	end
	if dt > 6.0 then
		da = da + (dt-6.0)*1e3*20.0
	end
	local alt = self.ref_val + da
	self.ap.set_orbitaltitude(alt)
	self:invalidate_display()
	return true
end


function AscentApMfd:toggle_OMS2_schedule()
	self.ap.toggle_OMS2()
	self:invalidate_display()
	return true
end

-- ==============================================================
-- MetStr: pretty print the MET
-- ==============================================================
function MetStr(met)
	local sign = ' '
	if met < 0 then
		sign = '-'
		met = -met
	end
	local h = math.floor(met/3600.0)
	met = met - h * 3600
	local m = math.floor(met/60.0)
	met = met - m * 60
	local h = math.min(h, 999)
	return string.format("%s%03d:%02d:%04.1f", sign, h, m, met)
end

-- ==============================================================
-- Return a module created from AscentApMfd
-- ==============================================================
return MFDModule(AscentApMfd)
