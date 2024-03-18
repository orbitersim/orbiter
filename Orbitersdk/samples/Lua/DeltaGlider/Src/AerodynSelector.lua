-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local AerodynSelectorDial = require("AerodynSelectorDial")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP


-- AF_DIAL (VC): mouse catch area
local VC_AF_DIAL_mousearea = {_V(-0.32300,1.07786,7.23673),_V(-0.29300,1.07786,7.23673),_V(-0.32300,1.10605,7.24699),_V(-0.29300,1.10605,7.24699)}

-- AF_DIAL (VC): rotation reference
local VC_AF_DIAL_ref = _V(-0.30800,1.09196,7.24186)

-- AF_DIAL (VC): rotation axis
local VC_AF_DIAL_axis = _V(0.00000,-0.34202,0.93969)

local VC_AF_DIAL_vofs = 0

local AerodynSelector = Class(DGSubsystem)

function AerodynSelector:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.ELID_DIAL, self.dial = self:AddElement (AerodynSelectorDial (self))
end

--------------------------------------------------------------

function AerodynSelector:SetMode (mode)
	local curmode = self:DG():get_adcmode()
	if curmode ~= mode then
		self:DG():set_adcmode (mode)
	end
	self:DG():trigger_redrawarea (0, 0, self.ELID_DIAL)
end

--------------------------------------------------------------

function AerodynSelector:GetMode ()
	return self:DG():get_adcmode()
end

--------------------------------------------------------------

function AerodynSelector:IncMode ()
	local mode = self:DG():get_adcmode()
	if mode <= 1 then
		self:DG():set_adcmode (mode ~=0 and 7 or 1)
		return true
	end
	return false
end

--------------------------------------------------------------

function AerodynSelector:DecMode ()
	local mode = math.min (self:DG():get_adcmode(),2)
	if mode ~= 0 then
		self:DG():set_adcmode (mode-1)
		return true
	end
	return false
end

--------------------------------------------------------------

function AerodynSelector:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	-- mode dial
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_DIAL, _R(23,69,63,113), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN, panel2dtex, self.dial)

	return true
end

--------------------------------------------------------------

function AerodynSelector:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- mode dial
	oapi.VC_register_area (self.ELID_DIAL, PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_DIAL, VC_AF_DIAL_mousearea[1], VC_AF_DIAL_mousearea[2], VC_AF_DIAL_mousearea[3], VC_AF_DIAL_mousearea[4])
	self.dial:DefineAnimationVC (VC_AF_DIAL_ref, VC_AF_DIAL_axis, GRP_VC.DIAL1, VC_AF_DIAL_vofs)

	return true
end

return AerodynSelector
