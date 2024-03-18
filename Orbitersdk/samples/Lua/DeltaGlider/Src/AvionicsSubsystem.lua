local DGSubsystem = require("DGSubsystem")
--local AAPSubsystem = require("AAPSubsystem")
local InstrAtt = require("InstrAtt")
--local InstrHSI = require("InstrHSI")
local InstrAOA = require("InstrAOA")
local InstrVS = require("InstrVS")
local FuelMFD = require("FuelMFD")
--local AngRateIndicator = require("AngRateIndicator")

local AvionicsSubsystem = Class(DGSubsystem)

function AvionicsSubsystem:new (v)
	DGSubsystem.new (self, v)

	-- create component instances
	self.ELID_INSTRATT, self.instratt = self:AddElement (InstrAtt (v))
--	self.ELID_INSTRHSI, self.instrhsi = self:AddElement (InstrHSI (v))
	self.ELID_INSTRAOA, self.instraoa = self:AddElement (InstrAOA (v))
	self.ELID_INSTRVS, self.instrvs   = self:AddElement (InstrVS (v))
	self.ELID_FUELMFD, self.fuelmfd   = self:AddElement (FuelMFD (v))
--	self.ELID_ANGRATEIND, self.angrateind = self:AddElement (AngRateIndicator (v, g_Param.surf))

	-- create subsystem instances
--	self.aapssys = self:AddSubsystem (AAPSubsystem (self))
--	self.aapssys:AttachHSI(self.instrhsi)
end

--------------------------------------------------------------

function AvionicsSubsystem:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	DGSubsystem.clbkLoadPanel2D (self, panelid, hPanel, viewW, viewH)

	if panelid ~= 0 then return false end

	local instr2dtex = oapi.get_texturehandle(self:DG().panelmesh0,2)
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)

	-- Artifical horizon display
	self:DG():register_panelarea (hPanel, self.ELID_INSTRATT, _R(0,0,0,0), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, 0, self.instratt)

	-- HSI indicator
--	self:DG():register_panelarea (hPanel, self.ELID_INSTRHSI, _R(0,0,0,0), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, 0, self.instrhsi)

	-- AOA/VS tape
	self:DG():register_panelarea (hPanel, self.ELID_INSTRAOA, _R(0,0,0,0), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, panel2dtex, self.instraoa)
	self:DG():register_panelarea (hPanel, self.ELID_INSTRVS,  _R(0,0,0,0), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, panel2dtex, self.instrvs)

	-- Propellant status display
	self:DG():register_panelarea (hPanel, self.ELID_FUELMFD,  _R(0,0,0,0), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, instr2dtex, self.fuelmfd)

	-- angular rate indicators
--	self:DG():register_panelarea (hPanel, self.ELID_ANGRATEIND, _R(0,0,0,0), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, panel2dtex, self.angrateind)

	return true
end

--------------------------------------------------------------

function AvionicsSubsystem:clbkLoadVC (vcid)
	DGSubsystem.clbkLoadVC (self, vcid)

	if vcid ~= 0 then return false end

	-- Artifical horizon display
	oapi.VC_register_area (self.ELID_INSTRATT, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)

	-- HSI indicator
--	oapi.VC_register_area (self.ELID_INSTRHSI, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)

	-- AOA/VS tapes
	oapi.VC_register_area (self.ELID_INSTRAOA, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)
	oapi.VC_register_area (self.ELID_INSTRVS, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)

	-- Propellant status display
	oapi.VC_register_area (self.ELID_FUELMFD, _R(0,0,1,1), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, PANEL_MAP.DIRECT, oapi.get_texturehandle (self:DG().vcmesh_tpl, 19))

	-- angular rate indicators
--	oapi.VC_register_area (self.ELID_ANGRATEIND, _R(0,0,1,1), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, PANEL_MAP.DIRECT, oapi.get_texturehandle (self:DG().vcmesh_tpl, 14))

	return true
end

return AvionicsSubsystem
