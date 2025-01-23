-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: StrobeLight.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local StrobeLightSwitch = require("StrobeLightSwitch")
local DGSwitch1 = require("DGSwitch1")

local meshres_p1 = require("meshres_p1")
local GRP_P1 = meshres_p1.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- STROBELIGHT_SWITCH (VC): mouse catch area
local VC_STROBELIGHT_SWITCH_mousearea = {_V(0.11265,1.65220,6.93405),_V(0.13265,1.65220,6.93405),_V(0.11265,1.66256,6.89541),_V(0.13265,1.66256,6.89541)}

-- STROBELIGHT_SWITCH (VC): rotation reference
local VC_STROBELIGHT_SWITCH_ref = _V(0.12265,1.66028,6.91550)

-- STROBELIGHT_SWITCH (VC): rotation axis
local VC_STROBELIGHT_SWITCH_axis = _V(1.00000,0.00000,0.00000)
local VC_STROBELIGHT_SWITCH_vofs = 198


local StrobeLight = Class(DGSubsystem)

function StrobeLight:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.light_on = false
	self.ELID_SWITCH, self.sw = self:AddElement (StrobeLightSwitch (self))
end

--------------------------------------------------------------

function StrobeLight:SetLight (on)
	if on ~= self.light_on then
		self.light_on = on
		for i=4,7 do
			self:DG().beacon[i].active = on
		end
		self.sw:SetState(on and DGSwitch1.UP or DGSwitch1.DOWN)
		self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	end
end

--------------------------------------------------------------

function StrobeLight:clbkSaveState (scn)
	if self.light_on then
		oapi.writescenario_int (scn, "STROBELIGHT", self.light_on and 1 or 0)
	end
end

--------------------------------------------------------------

function StrobeLight:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "STROBELIGHT %b", match) then
		self.light_on = match.res[1]
		return true
	end
	return false
end

--------------------------------------------------------------

function StrobeLight:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 1 then return false end

	-- Landing/docking light switch
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh1,1)
	self:DG():register_panelarea (hPanel, self.ELID_SWITCH, _R(754,192,780,244), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.sw)
	self.sw:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 16)

	return true
end

--------------------------------------------------------------

function StrobeLight:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Strobe light switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_STROBELIGHT_SWITCH_mousearea)
	self.sw:DefineAnimationVC (VC_STROBELIGHT_SWITCH_ref, VC_STROBELIGHT_SWITCH_axis, GRP_VC.SWITCH1, VC_STROBELIGHT_SWITCH_vofs)

	return true
end

--------------------------------------------------------------

function StrobeLight:clbkResetVC (vcid, hMesh)
	self:SetLight (self.light_on)
end

function StrobeLight:GetLight ()
	return self.light_on
end

return StrobeLight
