-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DocksealCtrl.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local DocksealIndicator = require("DocksealIndicator")


local DocksealCtrl = Class(DGSubsystem)

function DocksealCtrl:new (_subsys)
	DGSubsystem.new (self, _subsys)

	self.isDocked = false
	self.dockTime = -1e10

	self.ELID_INDICATOR, self.indicator = self:AddElement (DocksealIndicator (self))
end

--------------------------------------------------------------

function DocksealCtrl:SetDockStatus (docked)
	self.isDocked = docked
	self.dockTime = oapi.get_simtime()
	if not self.docked or self.dockTime < 1.0 then
		self.dockTime = self.dockTime - 1e10
		self.isSealing = false
	else
		self.isSealing = true
	end
	self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)
end

--------------------------------------------------------------

function DocksealCtrl:clbkPostStep (simt, simdt, mjd)
	if self.isSealing then
		self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)
		self.isSealing = (simt-simdt-self.dockTime) <= 10.0
	end
end

--------------------------------------------------------------

function DocksealCtrl:clbkPostCreation ()
	local hDock = self:DG():get_dockhandle (0)
	local mate = self:DG():get_dockstatus(hDock)
	if mate then
		self.isDocked = true
		self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)
	end
end

--------------------------------------------------------------

function DocksealCtrl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, panel2dtex, self.indicator)

	return true
end

--------------------------------------------------------------

function DocksealCtrl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- dock seal indicator
	oapi.VC_register_area (self.ELID_INDICATOR, PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE)

	return false
end

return DocksealCtrl
