-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: InstrumentLight.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local InstrumentLightSwitch = require("InstrumentLightSwitch")
local InstrumentBrightnessDial = require("InstrumentBrightnessDial")
local DGSwitch1 = require("DGSwitch1")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- INSTR_BRIGHTNESS (VC): mouse catch area
local VC_INSTR_BRIGHTNESS_mousearea = {_V(-0.17376,1.65484,6.92419),_V(-0.15416,1.65484,6.92419),_V(-0.17376,1.65992,6.90526),_V(-0.15416,1.65992,6.90526)}

-- INSTR_BRIGHTNESS (VC): rotation reference
local VC_INSTR_BRIGHTNESS_ref = _V(-0.16396,1.65738,6.91473)

-- INSTR_BRIGHTNESS (VC): rotation axis
local VC_INSTR_BRIGHTNESS_axis = _V(0.00000,0.96593,0.25882)

-- INSTRLIGHT_SWITCH (VC): mouse catch area
local VC_INSTRLIGHT_SWITCH_mousearea = {_V(-0.14510,1.65220,6.93405),_V(-0.12510,1.65220,6.93405),_V(-0.14510,1.66256,6.89541),_V(-0.12510,1.66256,6.89541)}

-- INSTRLIGHT_SWITCH (VC): rotation reference
local VC_INSTRLIGHT_SWITCH_ref = _V(-0.13510,1.66028,6.91550)

-- INSTRLIGHT_SWITCH (VC): rotation axis
local VC_INSTRLIGHT_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local VC_INSTRLIGHT_SWITCH_vofs = 66


local InstrumentLight = Class(DGSubsystem)

function InstrumentLight:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.light_on   = false
	self.brightness = 0.5
	self.light_col  = 0

	self.ELID_SWITCH, self.sw = self:AddElement (InstrumentLightSwitch (self))
	self.ELID_DIAL, self.dial = self:AddElement (InstrumentBrightnessDial (self))

	-- Instrument brightness dial animation
	local InstrBDialTransform = MGROUP_ROTATE (1, {GRP_VC.INSTR_BRIGHTNESS}, VC_INSTR_BRIGHTNESS_ref, VC_INSTR_BRIGHTNESS_axis, -280*RAD)
	self.anim_dial = self:DG():create_animation (0.5)
	self:DG():add_animationcomponent (self.anim_dial, 0, 1, InstrBDialTransform)
end

--------------------------------------------------------------
local norm = {
	diffuse = _COLOUR4(1,1,1,1),
	ambient = _COLOUR4(0.8,0.8,0.8,1),
	specular = _COLOUR4(0.1,0.1,0.1,0),
	emissive = _COLOUR4(0.15,0.15,0.15,1),
	power = 5
}

local label_glow = {
	{
		diffuse = _COLOUR4(0,0,0,0),
		ambient = _COLOUR4(0,0,0,0),
		specular = _COLOUR4(0,0,0,0),
		emissive = _COLOUR4(0.35,1,0.35,1),
		power = 0
	},
	{
		diffuse = _COLOUR4(0,0,0,0),
		ambient = _COLOUR4(0,0,0,0),
		specular = _COLOUR4(0,0,0,0),
		emissive = _COLOUR4(1,0.7,0.15,1),
		power = 0
	},
	{
		diffuse = _COLOUR4(0,0,0,0),
		ambient = _COLOUR4(0,0,0,0),
		specular = _COLOUR4(0,0,0,0),
		emissive = _COLOUR4(0.6,0.6,1,1),
		power = 0
	}
}

local btn_glow = {
	diffuse = _COLOUR4(1,1,1,1),
	ambient = _COLOUR4(1,1,1,1),
	specular = _COLOUR4(0.1,0.1,0.1,1),
	emissive = _COLOUR4(0.6,0.6,0.6,1),
	power = 5
}

function InstrumentLight:SetLight (on, force)
	if force == nil then
		force = false
	end

	if on == self.light_on and not force then return end -- nothing to do

	if on ~= self.light_on then
		self.light_on = on
		self.sw:SetState (self.light_on and DGSwitch1.UP or DGSwitch1.DOWN)
		self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	end

	if self:DG().vcmesh then
		oapi.set_material (self:DG().vcmesh, 0, on and btn_glow or norm)
		local idx = math.max(0, math.min(2, self.light_col))
		local mat
		if on then
			local scale = 0.2 + self.brightness*0.8
			mat = simplecopy(label_glow[self.light_col+1])

			mat.emissive.r = mat.emissive.r * scale
			mat.emissive.g = mat.emissive.g * scale
			mat.emissive.b = mat.emissive.b * scale
		end
		oapi.set_material (self:DG().vcmesh, 11, on and mat or norm)
		local ges = {}
		ges.flags = on and GRPEDIT.ADDUSERFLAG or GRPEDIT.DELUSERFLAG
		ges.UsrFlag = 0x18
		oapi.edit_meshgroup (self:DG().vcmesh, GRP_VC.LIT_LABELS, ges)
	end
end

--------------------------------------------------------------

function InstrumentLight:ModBrightness (up)
	local dt = oapi.get_simstep()
	local db = dt * (up and 0.3 or -0.3)
	self.brightness = math.max(0.0, math.min (1.0, self.brightness + db))
	self:DG():set_animation (self.anim_dial, self.brightness)
	if self.light_on then
		self:SetLight (true, true)
	end
end

--------------------------------------------------------------

function InstrumentLight:clbkSaveState (scn)
	if self.light_on or self.light_col ~= 0 or self.brightness ~= 0.5 then
		oapi.writescenario_string (scn, "INSTRLIGHT", string.format("%d %d %0.2f", self.light_on and 1 or 0, self.light_col, self.brightness))
	end
end

--------------------------------------------------------------

function InstrumentLight:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "INSTRLIGHT %b %d %f", match) then
		self.light_on = match.res[1]
		self.light_col = match.res[2]
		self.brightness = match.res[3]
		return true
	end
	return false
end

--------------------------------------------------------------

function InstrumentLight:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Instrument light switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_INSTRLIGHT_SWITCH_mousearea)
	self.sw:DefineAnimationVC (VC_INSTRLIGHT_SWITCH_ref, VC_INSTRLIGHT_SWITCH_axis, GRP_VC.SWITCH1, VC_INSTRLIGHT_SWITCH_vofs)

	-- Instrument brightness dial
	oapi.VC_register_area (self.ELID_DIAL, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_DIAL, VC_INSTR_BRIGHTNESS_mousearea)

	return true
end

--------------------------------------------------------------

function InstrumentLight:clbkResetVC (vcid, hMesh)
	self:SetLight (self.light_on, true)
	self:DG():set_animation (self.anim_dial, self.brightness)
end

function InstrumentLight:GetLight ()
	return self.light_on
end


return InstrumentLight
