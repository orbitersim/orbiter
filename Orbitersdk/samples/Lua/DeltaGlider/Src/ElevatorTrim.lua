-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: ElevatorTrim.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local ElevatorTrimWheel = require("ElevatorTrimWheel")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- ETRIMWHEEL (VC): mouse catch area
local VC_ETRIMWHEEL_mousearea = {_V(-0.34600,0.87841,7.10545),_V(-0.31600,0.87841,7.10545),_V(-0.34600,0.96780,7.16956),_V(-0.31600,0.96780,7.16956)}

-- ETRIMWHEEL (VC): rotation reference
local VC_ETRIMWHEEL_ref = _V(-0.33100,0.89659,7.17448)

-- ETRIMWHEEL (VC): rotation axis
local VC_ETRIMWHEEL_axis = _V(1.00000,0.00000,0.00000)



local ElevatorTrim = Class(DGSubsystem)

function ElevatorTrim:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.ELID_TRIMWHEEL, self.trimwheel = self:AddElement (ElevatorTrimWheel (self))

	-- Trim wheel animation
	local TrimWheelGrp = {GRP_VC.ETRIM_WHEEL}
	local TrimWheelTransform = MGROUP_ROTATE (1, TrimWheelGrp, VC_ETRIMWHEEL_ref, VC_ETRIMWHEEL_axis, PI*0.06)
	self.anim_vc_trimwheel = self:DG():create_animation (0.5)
	self:DG():add_animationcomponent (self.anim_vc_trimwheel, 0, 1, TrimWheelTransform)
end

--------------------------------------------------------------

function ElevatorTrim:clbkSaveState (scn)
	local trim = self:DG():get_adclevel (AIRCTRL.ELEVATORTRIM)
	if trim ~= 0 then
		oapi.writescenario_float (scn, "TRIM", trim)
	end
end

--------------------------------------------------------------

function ElevatorTrim:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "TRIM %f", match) then
		self:DG():set_adclevel (AIRCTRL.ELEVATORTRIM, match.res[1], true)
		return true
	end
	return false
end

--------------------------------------------------------------

function ElevatorTrim:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	-- elevator trim wheel
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_TRIMWHEEL, _R(87,299,107,359), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED, panel2dtex, self.trimwheel)

	return true
end

--------------------------------------------------------------

function ElevatorTrim:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Elevator trim wheel
	oapi.VC_register_area (self.ELID_TRIMWHEEL, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBPRESSED)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_TRIMWHEEL, VC_ETRIMWHEEL_mousearea[1], VC_ETRIMWHEEL_mousearea[2], VC_ETRIMWHEEL_mousearea[3], VC_ETRIMWHEEL_mousearea[4])

	return true
end

return ElevatorTrim
