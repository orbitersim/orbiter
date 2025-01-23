-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HoverHoldComponent.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local HoverSubsystemComponent = require("HoverSubsystemComponent")
local HoverHoldAltIndicator = require("HoverHoldAltIndicator")
local HoverAltBtn = require("HoverAltBtn")
local HoverAltSwitch = require("HoverAltSwitch")
local HoverAltResetBtn = require("HoverAltResetBtn")
local HoverAltModeButtons = require("HoverAltModeButtons")
local DGSwitch2 = require("DGSwitch2")
local DGButton3 = require("DGButton3")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- BTN_HOVER_HOLDALT (VC): mouse catch radius
local VC_BTN_HOVER_HOLDALT_mouserad = 0.015778

-- BTN_HOVER_HOLDALT (VC): rotation reference
local VC_BTN_HOVER_HOLDALT_ref = _V(-0.07638,1.02668,7.21179)

-- BTN_HOVER_HOLDALT (VC): rotation axis
local VC_BTN_HOVER_HOLDALT_axis = _V(0.00000,-0.58280,0.81262)
local VC_BTN_HOVER_HOLDALT_vofs = 0
local VC_BTN_HOVER_HOLDALT_LABEL_vofs = 8

-- HOVER_HOLDALT_SWITCH (VC): mouse catch area
local VC_HOVER_HOLDALT_SWITCH_mousearea = {_V(-0.09396,1.00140,7.19366),_V(-0.09396,0.99481,7.18893),_V(-0.07052,1.00140,7.19366),_V(-0.07052,0.99481,7.18893)}

-- HOVER_HOLDALT_SWITCH (VC): rotation reference
local VC_HOVER_HOLDALT_SWITCH_ref = _V(-0.08224,0.99674,7.19320)

-- HOVER_HOLDALT_SWITCH (VC): rotation axis
local VC_HOVER_HOLDALT_SWITCH_axis = _V(0.00000,-0.81262,-0.58280)
local VC_HOVER_HOLDALT_SWITCH_vofs = 168

-- BTN_HOVER_HOLDALT_CUR (VC): mouse catch radius
local VC_BTN_HOVER_HOLDALT_CUR_mouserad = 0.009000

-- BTN_HOVER_HOLDALT_CUR (VC): rotation reference
local VC_BTN_HOVER_HOLDALT_CUR_ref = _V(-0.06330,0.99811,7.19130)
local VC_BTN_HOVER_HOLDALT_CUR_vofs = 0

-- HOVERMODE_BUTTONS (VC): mouse catch area
local VC_HOVERMODE_BUTTONS_mousearea = {_V(-0.09080,0.97649,7.17580),_V(-0.06195,0.97649,7.17580),_V(-0.09080,0.98821,7.18420),_V(-0.06195,0.98821,7.18420)}

local VC_BTN_HOVERMODE_1_vofs = 20
local VC_BTN_HOVERMODE_1_LABEL_vofs = 16
local VC_BTN_HOVERMODE_2_vofs = 40
local VC_BTN_HOVERMODE_2_LABEL_vofs = 24

-- BTN_HOVERMODE_1 (VC): rotation axis
local VC_BTN_HOVERMODE_1_axis = _V(0.00000,-0.58280,0.81262)

-- BTN_HOVER_HOLDALT_CUR (VC): rotation axis
local VC_BTN_HOVER_HOLDALT_CUR_axis = _V(0.00000,-0.58280,0.81262)


local HoverHoldComponent = Class(HoverSubsystemComponent)

HoverHoldComponent.HOLD_NONE = 0
HoverHoldComponent.HOLD_ALT = 1
HoverHoldComponent.HOLD_VSPD = 2


function HoverHoldComponent:new (_subsys)
	HoverSubsystemComponent.new(self, _subsys)

	self.holdalt   = 0.0
	self.holdvspd  = 0.0
	self.active = false
	self.altmode = ALTMODE.GROUND
	self.hovermode = HoverHoldComponent.HOLD_ALT

	self.ELID_DISPLAY    , self.holddisp = self:AddElement (HoverHoldAltIndicator(self, g_Param.surf))
	self.ELID_HOLDBTN    , self.holdbtn = self:AddElement (HoverAltBtn (self))
	self.ELID_ALTSET     , self.altset = self:AddElement (HoverAltSwitch (self))
	self.ELID_ALTRESET   , self.altreset = self:AddElement (HoverAltResetBtn (self))
	self.ELID_MODEBUTTONS, self.modebuttons = self:AddElement (HoverAltModeButtons (self))
end

--------------------------------------------------------------

function HoverHoldComponent:Activate (ison)
	if ison ~= self.active then
		self.active = ison

		self.holdbtn:SetState(self.active and DGButton3.ON or DGButton3.OFF)
		self:DG():trigger_redrawarea (0, 0, self.ELID_HOLDBTN)

		if self.hovermode == HoverHoldComponent.HOLD_ALT then  -- use default VESSEL method for altitude hold
			if self.active then
				self:DG():set_hoverholdaltitude (self.holdalt, self.altmode==ALTMODE.GROUND)
			else
				self:DG():set_navmode (NAVMODE.HOLDALT, false)
			end
		else
			-- vspd hold is implemented below
			if self.active then -- initial conditions
				self.holdT = oapi.get_simtime()
				local v = self:DG():get_airspeedvector(REFFRAME.HORIZON)
				self.pvh = v.y
			end
		end
	end
end

--------------------------------------------------------------

function HoverHoldComponent:SetTargetAlt (alt)
	self.holdalt = alt
	if self.active then
		self:DG():set_hoverholdaltitude (self.holdalt, self.altmode==ALTMODE.GROUND)
	end
end

--------------------------------------------------------------

function HoverHoldComponent:SetTargetVspd (vspd)
	self.holdvspd = vspd
end

--------------------------------------------------------------

function HoverHoldComponent:SetTargetPrm (prm)
	if self.hovermode == HoverHoldComponent.HOLD_ALT then
		self:SetTargetAlt (prm)
	else
		self:SetTargetVspd (prm)
	end
end

--------------------------------------------------------------

function HoverHoldComponent:SetTargetAltCurrent ()
	self:SetTargetAlt (self:DG():get_altitude (self.altmode))
end

--------------------------------------------------------------

function HoverHoldComponent:SetHoverMode (mode)
	if mode ~= self.hovermode then
		if self.active then
			self:Activate (false)
			if mode == HoverHoldComponent.HOLD_ALT then
				-- if switching directly from active VSPD to active ALT mode,
				-- set current altitude as target
				self:SetTargetAltCurrent()
			elseif mode == HoverHoldComponent.HOLD_VSPD then
				-- if switching directly from active ALT to active VSPD mode,
				-- set target vspd to current vspd
				local v = self:DG():get_airspeedvector(REFFRAME.HORIZON)
				self:SetTargetVspd (v.y)
			end
			self.hovermode = mode
			self:Activate (true)
		else
			self.hovermode = mode
		end
	end
end

--------------------------------------------------------------

function HoverHoldComponent:HoverHoldVspd (vh_tgt)
	local t = oapi.get_simtime()
	local dt = t - self.holdT
	if dt == 0 then return end
	self.holdT = t

	local v = self:DG():get_airspeedvector(REFFRAME.HORIZON)
	local vh = v.y
	local ah = (vh-self.pvh)/dt
	self.pvh = vh

	local dvh = vh_tgt-vh
	local a_tgt = dvh
	local da = a_tgt - ah
	local a_max = self:DG():GetMaxHoverThrust()/self:DG():get_mass()
	local dlvl = da/a_max
	local dlvl_max = dt

	if math.abs(dlvl) > dlvl_max then
		dlvl = (dlvl > 0.0 and dlvl_max or -dlvl_max)
	end
	self:HoverSubsys():IncGroupLevel (dlvl)
end

--------------------------------------------------------------

function HoverHoldComponent:clbkSaveState (scn)
	local cbuf = string.format("%d %d %0.4f %0.4f", self.active and 1 or 0, self.hovermode, self.holdalt, self.holdvspd)
	oapi.writescenario_string (scn, "HOVERHOLD", cbuf)
end

--------------------------------------------------------------

function HoverHoldComponent:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "HOVERHOLD %b %d %f %f", match) then
		if match.res[1] then
			self:Activate(true)
		end
		self.hovermode = match.res[2]
		self.holdalt = match.res[3]
		self.holdvspd = match.res[4]
		return true
	end
	return false
end

--------------------------------------------------------------

function HoverHoldComponent:clbkPostStep (simt, simdt, mjd)
	-- vertical speed hover autopilot
	if self.active and self.hovermode == HoverHoldComponent.HOLD_VSPD then
		self:HoverHoldVspd (self.holdvspd)
	end
end

--------------------------------------------------------------

function HoverHoldComponent:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)

	-- readouts
	self:DG():register_panelarea (hPanel, self.ELID_DISPLAY, _R(486,442,543,466), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, panel2dtex, self.holddisp)

	-- Hover hold activate button
	self:DG():register_panelarea (hPanel, self.ELID_HOLDBTN, _R(491,415,533,445), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP, panel2dtex, self.holdbtn)
	self.holdbtn:DefineAnimation2D (GRP_P0.INSTRUMENTS_ABOVE, 172)

	-- Hover hold select switch
	self:DG():register_panelarea (hPanel, self.ELID_ALTSET, _R(484,491,528,507), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP, panel2dtex, self.altset)
	self.altset:DefineAnimation2D (DGSwitch2.HORZ, GRP_P0.INSTRUMENTS_ABOVE, 168)

	-- Hover hold reset button
	self:DG():register_panelarea (hPanel, self.ELID_ALTRESET, _R(532,492,546,506), PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP, panel2dtex, self.altreset)

	-- Hover hold mode selector buttons
	self:DG():register_panelarea (hPanel, self.ELID_MODEBUTTONS, _R(487,520,541,544), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN, panel2dtex, self.modebuttons)
	self.modebuttons:DefineAnimation2D (GRP_P0.INSTRUMENTS_ABOVE, 176)

	return true
end

--------------------------------------------------------------

function HoverHoldComponent:clbkLoadVC (vcid)
	if vcid == 0 then -- VC pilot position
		-- readouts
		oapi.VC_register_area (self.ELID_DISPLAY, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)

		-- Hover hold activate button
		oapi.VC_register_area (self.ELID_HOLDBTN, PANEL_REDRAW.MOUSE + PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
		oapi.VC_set_areaclickmode_spherical (self.ELID_HOLDBTN, VC_BTN_HOVER_HOLDALT_ref, VC_BTN_HOVER_HOLDALT_mouserad)
		self.holdbtn:DefineAnimationVC (VC_BTN_HOVER_HOLDALT_axis, GRP_VC.BUTTON3, GRP_VC.LIT_SURF, VC_BTN_HOVER_HOLDALT_vofs, VC_BTN_HOVER_HOLDALT_LABEL_vofs)

		-- Hover hold select switch
		oapi.VC_register_area (self.ELID_ALTSET, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
		oapi.VC_set_areaclickmode_quadrilateral (self.ELID_ALTSET, VC_HOVER_HOLDALT_SWITCH_mousearea[1], VC_HOVER_HOLDALT_SWITCH_mousearea[2], VC_HOVER_HOLDALT_SWITCH_mousearea[3], VC_HOVER_HOLDALT_SWITCH_mousearea[4])
		self.altset:DefineAnimationVC (VC_HOVER_HOLDALT_SWITCH_ref, VC_HOVER_HOLDALT_SWITCH_axis, GRP_VC.SWITCH2, VC_HOVER_HOLDALT_SWITCH_vofs)

		-- Hover hold reset button
		oapi.VC_register_area (self.ELID_ALTRESET, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
		oapi.VC_set_areaclickmode_spherical (self.ELID_ALTRESET, VC_BTN_HOVER_HOLDALT_CUR_ref, VC_BTN_HOVER_HOLDALT_CUR_mouserad)
		self.altreset:DefineAnimationVC (VC_BTN_HOVER_HOLDALT_CUR_axis, GRP_VC.BUTTON2, VC_BTN_HOVER_HOLDALT_CUR_vofs)

		-- Hover mode selector buttons
		oapi.VC_register_area (self.ELID_MODEBUTTONS, PANEL_REDRAW.USER + PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
		oapi.VC_set_areaclickmode_quadrilateral (self.ELID_MODEBUTTONS, VC_HOVERMODE_BUTTONS_mousearea[1], VC_HOVERMODE_BUTTONS_mousearea[2], VC_HOVERMODE_BUTTONS_mousearea[3], VC_HOVERMODE_BUTTONS_mousearea[4])

		local hoverbtn_vofs = {VC_BTN_HOVERMODE_1_vofs,VC_BTN_HOVERMODE_2_vofs}
		local hoverbtn_label_vofs = {VC_BTN_HOVERMODE_1_LABEL_vofs, VC_BTN_HOVERMODE_2_LABEL_vofs}
		self.modebuttons:DefineAnimationsVC (VC_BTN_HOVERMODE_1_axis, GRP_VC.BUTTON3, GRP_VC.LIT_SURF, hoverbtn_vofs, hoverbtn_label_vofs)
		

	end
	return true
end


function HoverHoldComponent:TargetAlt()
	return self.holdalt
end
function HoverHoldComponent:TargetVspd()
	return self.holdvspd
end
function HoverHoldComponent:TargetPrm()
	return self.hovermode == HoverHoldComponent.HOLD_ALT and self.holdalt or self.holdvspd
end
function HoverHoldComponent:GetHoverMode ()
	return self.hovermode
end

return HoverHoldComponent

