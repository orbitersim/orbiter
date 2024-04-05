-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HoverAttitudeComponent.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local HoverSubsystemComponent = require("HoverSubsystemComponent")
local HoverCtrlDial = require("HoverCtrlDial")
local PHoverCtrl = require("PHoverCtrl")
local RHoverCtrl = require("RHoverCtrl")
local HoverDisp = require("HoverDisp")
local DGSwitch2 = require("DGSwitch2")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local PHOVER_RANGE = 10.0*RAD
local RHOVER_RANGE = 10.0*RAD
-- max hover-induced pitch and roll values

local MAX_AUTO_HOVER_ATT = 30*RAD
-- max pitch/roll angle for hover control

-- HOVER_DIAL (VC): mouse catch area
local VC_HOVER_DIAL_mousearea = {_V(-0.17793,1.00130,7.19359),_V(-0.14793,1.00130,7.19359),_V(-0.17793,1.02568,7.21107),_V(-0.14793,1.02568,7.21107)}

-- HOVER_DIAL (VC): rotation reference
local VC_HOVER_DIAL_ref = _V(-0.16293,1.01349,7.20233)

-- HOVER_DIAL (VC): rotation axis
local VC_HOVER_DIAL_axis = _V(0.00000,-0.58280,0.81262)
local VC_HOVER_DIAL_vofs = 152

-- HOVER_PSWITCH (VC): mouse catch area
local VC_HOVER_PSWITCH_mousearea = {_V(-0.12101,1.00616,7.19708),_V(-0.11289,1.00616,7.19708),_V(-0.12101,1.02521,7.21074),_V(-0.11289,1.02521,7.21074)}

-- HOVER_PSWITCH (VC): rotation reference
local VC_HOVER_PSWITCH_ref = _V(-0.11695,1.01432,7.20581)

-- HOVER_PSWITCH (VC): rotation axis
local VC_HOVER_PSWITCH_axis = _V(1.00000,0.00000,0.00000)

-- HOVER_RSWITCH (VC): mouse catch area
local VC_HOVER_RSWITCH_mousearea = {_V(-0.12867,0.99298,7.18762),_V(-0.12867,0.98638,7.18289),_V(-0.10523,0.99298,7.18762),_V(-0.10523,0.98638,7.18289)}

-- HOVER_RSWITCH (VC): rotation reference
local VC_HOVER_RSWITCH_ref = _V(-0.11695,0.98831,7.18716)

-- HOVER_RSWITCH (VC): rotation axis
local VC_HOVER_RSWITCH_axis = _V(0.00000,-0.81262,-0.58280)
local VC_HOVER_PSWITCH_vofs = 112
local VC_HOVER_RSWITCH_vofs = 140


local HoverAttitudeComponent = Class(HoverSubsystemComponent)
	
function HoverAttitudeComponent:new (_subsys)
	HoverSubsystemComponent.new(self, _subsys)

	self.mode = 0

	self.phover = 0.0
	self.phover_cmd = 0.0
	self.rhover = 0.0
	self.rhover_cmd = 0.0

	self.ELID_MODEDIAL    , self.modedial     = self:AddElement (HoverCtrlDial (self))
	self.ELID_PHOVERSWITCH, self.phoverswitch = self:AddElement (PHoverCtrl (self))
	self.ELID_RHOVERSWITCH, self.rhoverswitch = self:AddElement (RHoverCtrl (self))
	self.ELID_DISPLAY     , self.hoverdisp    = self:AddElement (HoverDisp (self))
end

--------------------------------------------------------------

function HoverAttitudeComponent:IncPHover (dir)
	if dir ~= 0 and self.mode == 2 then
		local cmd_speed = 0.5
		local dcmd = oapi.get_simstep() * cmd_speed * PHOVER_RANGE * (dir == 1 and -1.0 or 1.0)
		self.phover_cmd = math.min (PHOVER_RANGE, math.max (-PHOVER_RANGE, self.phover_cmd+dcmd))
	end
	return true
end

--------------------------------------------------------------

function HoverAttitudeComponent:IncRHover (dir)
	if dir ~= 0 and self.mode == 2 then
		local cmd_speed = 0.5
		local dcmd = oapi.get_simstep() * cmd_speed * RHOVER_RANGE * (dir == 1 and -1.0 or 1.0)
		self.rhover_cmd = math.min (RHOVER_RANGE, math.max (-RHOVER_RANGE, self.rhover_cmd+dcmd))
	end
	return true
end

--------------------------------------------------------------

function HoverAttitudeComponent:AutoHoverAtt ()
	local lvl

	-- Pitch command
	lvl = -self:DG():get_manualcontrollevel(THGROUP.ATT_PITCHDOWN, MANCTRL.ROTMODE)
	if lvl == 0 then
		lvl = self:DG():get_manualcontrollevel(THGROUP.ATT_PITCHUP, MANCTRL.ROTMODE)
	end
	self.phover_cmd = lvl*PHOVER_RANGE

	-- Roll command
	lvl = -self:DG():get_manualcontrollevel(THGROUP.ATT_BANKRIGHT, MANCTRL.ROTMODE)
	if lvl == 0 then
		lvl = self:DG():get_manualcontrollevel(THGROUP.ATT_BANKLEFT, MANCTRL.ROTMODE)
	end
	self.rhover_cmd = lvl*RHOVER_RANGE
end

--------------------------------------------------------------

function HoverAttitudeComponent:TrackHoverAtt ()
	if self.mode ~= 0 then
		self.phover = self:DG():get_pitch()
		self.rhover = self:DG():get_bank()
		local fb_scale = 3.0/4.55 -- scaling between front and back hovers (distance from CG)
		local Lf = self:HoverSubsys():GetThrusterLevel(1)
		local Ll = self:HoverSubsys():GetThrusterLevel(2)
		local Lr = self:HoverSubsys():GetThrusterLevel(3)
		local Lb = (Ll+Lr)*0.5
		local Lm = (Lf+Lb)*0.5
		local Tf = Lf
		local Tb = Lb*fb_scale
		local Tm = (Tf+Tb)*0.5

		if math.abs(self.phover) <= MAX_AUTO_HOVER_ATT and math.abs(self.rhover) <= MAX_AUTO_HOVER_ATT then -- within control range
			local p_alpha = 0.2
			local p_beta = 0.6
			local r_alpha = 0.2
			local r_beta = 0.6
			local dp = self.phover - self.phover_cmd
			local dr = self.rhover - self.rhover_cmd
			local avel = self:DG():get_angvel()
			
			local dpv =  avel.x
			local drv = -avel.z
			local balance_fb = -p_alpha*dp - p_beta*dpv
			local balance_lr = -r_alpha*dr - r_beta*drv

			if Lf ~= 0 or Lb ~= 0 then
				-- front/back balance
				local Lf_cmd = Lm+balance_fb
				local Lb_cmd = Lm-balance_fb
				local D = (Lf_cmd-Lf + (Lb_cmd-Lb)*fb_scale)/(1.0+fb_scale)
				Lf_cmd = Lf_cmd - D
				Lb_cmd = Lb_cmd - D

				local Lmin = math.min (Lf_cmd, Lb_cmd)
				local Lmax = math.max (Lf_cmd, Lb_cmd)
				if Lmin < 0.0 then
					if Lf_cmd < 0.0 then
						Lf_cmd = 0.0
						Lb_cmd = Lb_cmd + Lmin/fb_scale
					else
						Lb_cmd = 0.0
						Lf_cmd = Lf_cmd + Lmin*fb_scale
					end
				end
				if Lmax > 1.0 then
					if Lf_cmd > 1.0 then
						Lf_cmd = 1.0
						Lb_cmd = Lb_cmd + (Lmax-1.0)/fb_scale
					else
						Lb_cmd = 1.0
						Lf_cmd= Lf_cmd + (Lmax-1.0)*fb_scale
					end
				end
				-- left/right balance
				local Ll_cmd = Lb_cmd-balance_lr
				local Lr_cmd = Lb_cmd+balance_lr
				Lmin = math.min (Ll_cmd, Lr_cmd)
				Lmax = math.max (Ll_cmd, Lr_cmd)
				if Lmin < 0.0 then
					if Ll_cmd < 0.0 then
						Ll_cmd = 0.0
						Lr_cmd = Lr_cmd + Lmin
					else
						Lr_cmd = 0.0
						Ll_cmd = Ll_cmd + Lmin
					end
				end
				if Lmax > 1.0 then
					if Ll_cmd > 1.0 then
						Ll_cmd = 1.0
						Lr_cmd = Lr_cmd + Lmax-1.0
					else
						Lr_cmd = 1.0
						Ll_cmd = Ll_cmd + Lmax-1.0
					end
				end
				self:HoverSubsys():SetThrusterLevel (1, Lf_cmd)
				self:HoverSubsys():SetThrusterLevel (2, Ll_cmd)
				self:HoverSubsys():SetThrusterLevel (3, Lr_cmd)
			end
		else
			local L_cmd = 2.0*Tm / (1.0 + fb_scale)
			for i=1,3 do
				self:HoverSubsys():SetThrusterLevel (i, L_cmd)
			end
		end
	else
		self.phover = 0.0
		self.rhover = 0.0
		self.phover_cmd = 0.0
		self.rhover_cmd = 0.0
	end
	self:DG():trigger_redrawarea (0, 0, self.ELID_DISPLAY)
end

--------------------------------------------------------------

function HoverAttitudeComponent:clbkSaveState (scn)
	if self.mode ~= 0 then
		if self.mode == 1 then -- auto
			oapi.writescenario_int (scn, "HOVERMODE", self.mode)
		else -- manual
			local sbuf = string.format("%d %0.3f %0.3f", self.mode, self.phover_cmd, self.rhover_cmd)
			oapi.writescenario_string (scn, "HOVERMODE", cbuf)
		end
	end
end

--------------------------------------------------------------

function HoverAttitudeComponent:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "HOVERMODE 2 %f %f", match) then
		self.mode = 2
		self.phover_cmd = match.res[1]
		self.rhover_cmd = match.res[2]
	elseif line == "HOVERMODE 1" then
		self.mode = 1
	end
	return false
end

--------------------------------------------------------------

function HoverAttitudeComponent:clbkPostStep (simt, simdt, mjd)
	if self.mode == 1 then
		self:AutoHoverAtt()
	end
	self:TrackHoverAtt()
end

--------------------------------------------------------------

function HoverAttitudeComponent:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)

	-- Hover control dial
	self:DG():register_panelarea (hPanel, self.ELID_MODEDIAL,     _R(356,426,396,470), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN, panel2dtex, self.modedial)

	-- Hover manual switches
	self:DG():register_panelarea (hPanel, self.ELID_PHOVERSWITCH, _R(436,434,452,478), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP, panel2dtex, self.phoverswitch)
	self.phoverswitch:DefineAnimation2D (DGSwitch2.VERT, GRP_P0.INSTRUMENTS_ABOVE, 160)
	self:DG():register_panelarea (hPanel, self.ELID_RHOVERSWITCH, _R(423,513,467,529), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP, panel2dtex, self.rhoverswitch)
	self.rhoverswitch:DefineAnimation2D (DGSwitch2.HORZ, GRP_P0.INSTRUMENTS_ABOVE, 164)

	-- Hover balance display
	self:DG():register_panelarea (hPanel, self.ELID_DISPLAY,      _R( 0,  0, 0,  0), PANEL_REDRAW.USER,   PANEL_MOUSE.IGNORE, panel2dtex, self.hoverdisp)

	return true
end

--------------------------------------------------------------

function HoverAttitudeComponent:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Hover control dial
	oapi.VC_register_area (self.ELID_MODEDIAL, PANEL_REDRAW.USER + PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_MODEDIAL, VC_HOVER_DIAL_mousearea[1], VC_HOVER_DIAL_mousearea[2], VC_HOVER_DIAL_mousearea[3], VC_HOVER_DIAL_mousearea[4])
	self.modedial:DefineAnimationVC (VC_HOVER_DIAL_ref, VC_HOVER_DIAL_axis, GRP_VC.DIAL1, VC_HOVER_DIAL_vofs)

	-- Hover manual switches
	oapi.VC_register_area (self.ELID_PHOVERSWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PHOVERSWITCH, VC_HOVER_PSWITCH_mousearea[1], VC_HOVER_PSWITCH_mousearea[2], VC_HOVER_PSWITCH_mousearea[3], VC_HOVER_PSWITCH_mousearea[4])
	self.phoverswitch:DefineAnimationVC (VC_HOVER_PSWITCH_ref, VC_HOVER_PSWITCH_axis, GRP_VC.SWITCH2, VC_HOVER_PSWITCH_vofs)
	oapi.VC_register_area (self.ELID_RHOVERSWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_RHOVERSWITCH, VC_HOVER_RSWITCH_mousearea[1], VC_HOVER_RSWITCH_mousearea[2], VC_HOVER_RSWITCH_mousearea[3], VC_HOVER_RSWITCH_mousearea[4])
	self.rhoverswitch:DefineAnimationVC (VC_HOVER_RSWITCH_ref, VC_HOVER_RSWITCH_axis, GRP_VC.SWITCH2, VC_HOVER_RSWITCH_vofs)

	-- Hover balance display
	oapi.VC_register_area (self.ELID_DISPLAY, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)

	return true
end

function HoverAttitudeComponent:Mode()
	return self.mode
end

function HoverAttitudeComponent:SetMode (newmode)
	self.mode = newmode
end

function HoverAttitudeComponent:PHover (actual)
	if actual == nil then actual = true end
	return actual and self.phover or self.phover_cmd
end

function HoverAttitudeComponent:RHover (actual)
	if actual == nil then actual = true end
	return actual and self.rhover or self.rhover_cmd
end

return HoverAttitudeComponent
