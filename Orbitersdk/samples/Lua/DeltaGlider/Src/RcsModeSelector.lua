-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: RcsModeSelector.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local RcsModeDial = require("RcsModeDial")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- RCS_DIAL (VC): mouse catch area
local VC_RCS_DIAL_mousearea = {_V(0.05000,1.26200,7.26800),_V(0.08000,1.26200,7.26800),_V(0.05000,1.29200,7.26800),_V(0.08000,1.29200,7.26800)}

-- RCS_DIAL (VC): rotation reference
local VC_RCS_DIAL_ref = _V(0.06500,1.27700,7.26800)

-- RCS_DIAL (VC): rotation axis
local VC_RCS_DIAL_axis = _V(0.00000,0.00000,1.00000)

local VC_RCS_DIAL_vofs = 228


local RcsModeSelector = Class(DGSubsystem)

function RcsModeSelector:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.ELID_DIAL, self.dial = self:AddElement (RcsModeDial (self))
end

--------------------------------------------------------------

function RcsModeSelector:GetMode ()
	return self:DG():get_rcsmode()
end

--------------------------------------------------------------

function RcsModeSelector:SetMode (mode)
	local curmode = self:GetMode()
	if curmode ~= mode then
		self:DG():set_rcsmode (mode)
	end
	self:DG():trigger_redrawarea (0, 0, self.ELID_DIAL)
end

--------------------------------------------------------------

function RcsModeSelector:IncMode ()
	local mode = self:GetMode()
	if mode < 2 then
		self:SetMode (mode+1)
		return true
	end
	return false
end

--------------------------------------------------------------

function RcsModeSelector:DecMode ()
	local mode = self:GetMode()
	if mode ~= 0 then
		self:SetMode (mode-1)
		return true
	end
	return false
end

--------------------------------------------------------------

function RcsModeSelector:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	-- RCS mode dial
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_DIAL, _R(100, 69,140,113), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN, 0, self.dial)

	return true
end

--------------------------------------------------------------

function RcsModeSelector:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- RCS mode dial
	oapi.VC_register_area (self.ELID_DIAL, PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_DIAL, VC_RCS_DIAL_mousearea[1], VC_RCS_DIAL_mousearea[2], VC_RCS_DIAL_mousearea[3], VC_RCS_DIAL_mousearea[4])
	self.dial:DefineAnimationVC (VC_RCS_DIAL_ref, VC_RCS_DIAL_axis, GRP_VC.DIAL1, VC_RCS_DIAL_vofs)

	return true
end

return RcsModeSelector
