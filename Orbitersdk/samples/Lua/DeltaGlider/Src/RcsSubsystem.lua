-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: RcsSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local RcsProgButtons = require("RcsProgButtons")
local RcsModeSelector = require("RcsModeSelector")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- NAV_BUTTONS (VC): mouse catch area
local VC_NAV_BUTTONS_mousearea = {_V(0.10191,1.25710,7.26800),_V(0.17809,1.25710,7.26800),_V(0.10191,1.28640,7.26800),_V(0.17809,1.28640,7.26800)}

local VC_BTN_NAVMODE_1_vofs = 120
local VC_BTN_NAVMODE_1_LABEL_vofs = 56
local VC_BTN_NAVMODE_2_vofs = 140
local VC_BTN_NAVMODE_2_LABEL_vofs = 64
local VC_BTN_NAVMODE_3_vofs = 160
local VC_BTN_NAVMODE_3_LABEL_vofs = 72
local VC_BTN_NAVMODE_4_vofs = 180
local VC_BTN_NAVMODE_4_LABEL_vofs = 80
local VC_BTN_NAVMODE_5_vofs = 200
local VC_BTN_NAVMODE_5_LABEL_vofs = 88
local VC_BTN_NAVMODE_6_vofs = 220
local VC_BTN_NAVMODE_6_LABEL_vofs = 96

-- BTN_NAVMODE_1 (VC): rotation axis
local VC_BTN_NAVMODE_1_axis = _V(0.00000,0.00000,1.00000)


local RcsSubsystem = Class(DGSubsystem)

function RcsSubsystem:new (dg)
	DGSubsystem.new (self, dg)

	-- create component instances
	self.modeselector = self:AddSubsystem (RcsModeSelector (self))

	self.ELID_PROGBUTTONS, self.progbuttons = self:AddElement (RcsProgButtons(self))
end

--------------------------------------------------------------

function RcsSubsystem:SetMode (mode)
	self.modeselector:SetMode (mode)
end

--------------------------------------------------------------

function RcsSubsystem:SetProg (prog, active)
	self.progbuttons:SetMode (prog, active)
	self:DG():trigger_redrawarea (0, 0, self.ELID_PROGBUTTONS)
end

--------------------------------------------------------------

function RcsSubsystem:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	-- RCS program buttons
	self:DG():register_panelarea (hPanel, self.ELID_PROGBUTTONS, _R(1124,62,1272,110), PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN, 0, self.progbuttons)

	return DGSubsystem.clbkLoadPanel2D (self, panelid, hPanel, viewW, viewH)
end

--------------------------------------------------------------

function RcsSubsystem:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Navmode indicator/selector on the top right of the front panel
	oapi.VC_register_area (self.ELID_PROGBUTTONS, PANEL_REDRAW.USER + PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PROGBUTTONS, VC_NAV_BUTTONS_mousearea[1], VC_NAV_BUTTONS_mousearea[2], VC_NAV_BUTTONS_mousearea[3], VC_NAV_BUTTONS_mousearea[4])

	local navbtn_vofs = {VC_BTN_NAVMODE_1_vofs, VC_BTN_NAVMODE_2_vofs, VC_BTN_NAVMODE_3_vofs,
			                        VC_BTN_NAVMODE_4_vofs, VC_BTN_NAVMODE_5_vofs, VC_BTN_NAVMODE_6_vofs}
	local navlbl_vofs = {VC_BTN_NAVMODE_1_LABEL_vofs, VC_BTN_NAVMODE_2_LABEL_vofs, VC_BTN_NAVMODE_3_LABEL_vofs,
			                        VC_BTN_NAVMODE_4_LABEL_vofs, VC_BTN_NAVMODE_5_LABEL_vofs, VC_BTN_NAVMODE_6_LABEL_vofs}

	self.progbuttons:DefineAnimationsVC (VC_BTN_NAVMODE_1_axis, GRP_VC.BUTTON3, GRP_VC.LIT_SURF, navbtn_vofs, navlbl_vofs);

	return DGSubsystem.clbkLoadVC (self, vcid)
end

return RcsSubsystem
