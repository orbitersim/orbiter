-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: NavLight.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local NavLightSwitch = require("NavLightSwitch")
local DGSwitch1 = require("DGSwitch1")

local meshres_p1 = require("meshres_p1")
local GRP_P1 = meshres_p1.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- NAVLIGHT_SWITCH (VC): mouse catch area
local VC_NAVLIGHT_SWITCH_mousearea = {_V(0.13519,1.65220,6.93405),_V(0.15519,1.65220,6.93405),_V(0.13519,1.66256,6.89541),_V(0.15519,1.66256,6.89541)}

-- NAVLIGHT_SWITCH (VC): rotation reference
local VC_NAVLIGHT_SWITCH_ref = _V(0.14519,1.66028,6.91550)

--- NAVLIGHT_SWITCH (VC): rotation axis
local VC_NAVLIGHT_SWITCH_axis = _V(1.00000,0.00000,0.00000)
local VC_NAVLIGHT_SWITCH_vofs = 231


local NavLight = Class(DGSubsystem)

function NavLight:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.light_on = false
	self.ELID_SWITCH, self.sw = self:AddElement (NavLightSwitch (self))
end

--------------------------------------------------------------

function NavLight:SetLight (on)
	if on ~= self.light_on then
		self.light_on = on
		self:DG().beacon[1].active = on
		self:DG().beacon[2].active = on
		self.sw:SetState(on and DGSwitch1.UP or DGSwitch1.DOWN)
		self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	end
end

--------------------------------------------------------------

function NavLight:clbkSaveState (scn)
	if self.light_on then
		oapi.writescenario_int (scn, "NAVLIGHT", 1)
	end
end

--------------------------------------------------------------

function NavLight:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "NAVLIGHT %b", match) then
		self.light_on = match.res[1]
		return true
	end
	return false
end

--------------------------------------------------------------

function NavLight:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 1 then return false end

	-- Nav light switch
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh1,1)
	self:DG():register_panelarea (hPanel, self.ELID_SWITCH, _R(800,192,826,244), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.sw)
	self.sw:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 20)

	return true
end

--------------------------------------------------------------

function NavLight:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Nav light switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_NAVLIGHT_SWITCH_mousearea)
	self.sw:DefineAnimationVC (VC_NAVLIGHT_SWITCH_ref, VC_NAVLIGHT_SWITCH_axis, GRP_VC.SWITCH1, VC_NAVLIGHT_SWITCH_vofs)

	return true
end

--------------------------------------------------------------

function NavLight:clbkResetVC (vcid, hMesh)
	self:SetLight (self.light_on)
end


function NavLight:GetLight ()
	return self.light_on
end

return NavLight
