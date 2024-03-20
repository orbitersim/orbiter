local DGSubsystem = require("DGSubsystem")
local CockpitLightSwitch = require("CockpitLightSwitch")
local CockpitBrightnessDial = require("CockpitBrightnessDial")
local DGSwitch1 = require("DGSwitch1")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- FLOODLIGHT_SWITCH (VC): mouse catch area
local VC_FLOODLIGHT_SWITCH_mousearea = {_V(-0.12031,1.65220,6.93405),_V(-0.10031,1.65220,6.93405),_V(-0.12031,1.66256,6.89541),_V(-0.10031,1.66256,6.89541)}

-- FLOODLIGHT_SWITCH (VC): rotation reference
local VC_FLOODLIGHT_SWITCH_ref = _V(-0.11031,1.66028,6.91550)

-- FLOODLIGHT_SWITCH (VC): rotation axis
local VC_FLOODLIGHT_SWITCH_axis = _V(1.00000,0.00000,0.00000)
local VC_FLOODLIGHT_SWITCH_vofs = 99

-- FLOOD_BRIGHTNESS (VC): mouse catch area
local VC_FLOOD_BRIGHTNESS_mousearea = {_V(-0.09081,1.65484,6.92419),_V(-0.07121,1.65484,6.92419),_V(-0.09081,1.65992,6.90526),_V(-0.07121,1.65992,6.90526)}

-- FLOOD_BRIGHTNESS (VC): rotation reference
local VC_FLOOD_BRIGHTNESS_ref = _V(-0.08101,1.65738,6.91473)

-- FLOOD_BRIGHTNESS (VC): rotation axis
local VC_FLOOD_BRIGHTNESS_axis = _V(0.00000,0.96593,0.25882)

local CockpitLight = Class(DGSubsystem)

function CockpitLight:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.light = nil
	self.light_mode = 0
	self.brightness = 0.7

	self.ELID_SWITCH, self.sw = self:AddElement (CockpitLightSwitch (self))
	self.ELID_DIAL, self.dial = self:AddElement (CockpitBrightnessDial (self))

	-- Floodlight brightness dial animation
	local FloodBDialTransform = MGROUP_ROTATE (1, {GRP_VC.FLOOD_BRIGHTNESS}, VC_FLOOD_BRIGHTNESS_ref, VC_FLOOD_BRIGHTNESS_axis, -280*RAD)
	self.anim_dial = self:DG():create_animation (0.5)
	self:DG():add_animationcomponent (self.anim_dial, 0, 1, FloodBDialTransform)
end

--------------------------------------------------------------
local zero = _COLOUR4(0.0,0.0,0.0,0.0)
local wcol = _COLOUR4(1.0,1.0,1.0,0.0)
local rcol = _COLOUR4(0.6,0.05,0.0,0.0)
local att = {
	range = 3,
	att0 = 0,
	att1 = 0,
	att2 = 3
}

function CockpitLight:SetLight (mode, force)
	if mode == self.light_mode and not force then return end -- nothing to do

	if mode ~= self.light_mode then
		self.light_mode = mode
		self.sw:SetState (mode == 0 and DGSwitch1.CENTER or (mode == 1 and DGSwitch1.UP or DGSwitch1.DOWN))
		self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	end

	if self.light then
		self:DG():del_lightemitter (self.light)
		self.light = nil
	end
	if mode ~= 0 then
		local col = (mode == 1 and wcol or rcol)
		self.light = self:DG():add_pointlight(_V(0,1.65,6.68), att, col, col, zero)
		self.light:set_visibility (VIS.COCKPIT)
		self.light:activate(true)
		local intens = 0.2 + self.brightness*0.8
		self.light:set_intensity (intens)
	end
end

--------------------------------------------------------------

function CockpitLight:ModBrightness (up)
	local dt = oapi.get_simstep()
	local db = dt * (up and 0.3 or -0.3)
	self.brightness = math.max(0.0, math.min (1.0, self.brightness + db))
	self:DG():set_animation (self.anim_dial, self.brightness)
	if self.light_mode ~= 0 then
		self:SetLight (self.light_mode, true)
	end
end

--------------------------------------------------------------

function CockpitLight:clbkSaveState (scn)
	if self.light_mode ~= 0 or self.brightness ~= 0.7 then
		oapi.writescenario_string (scn, "FLOODLIGHT", string.format("%d %0.2f", self.light_mode, self.brightness))
	end
end

--------------------------------------------------------------

function CockpitLight:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "FLOODLIGHT %d %f", match) then
		self.light_mode = math.max (0, math.min (2, math.res[1]))
		self.brightness = math.max (0, math.min (1, math.res[2]))
		return true
	end
	return false
end

--------------------------------------------------------------

function CockpitLight:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Floodlight switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_FLOODLIGHT_SWITCH_mousearea)
	self.sw:DefineAnimationVC (VC_FLOODLIGHT_SWITCH_ref, VC_FLOODLIGHT_SWITCH_axis, GRP_VC.SWITCH1, VC_FLOODLIGHT_SWITCH_vofs)

	-- Floodlight brightness dial
	oapi.VC_register_area (self.ELID_DIAL, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_DIAL, VC_FLOOD_BRIGHTNESS_mousearea)

	return true
end

function CockpitLight:clbkResetVC (vcid, hMesh)
	self:SetLight (self.light_mode, true)
	self:DG():set_animation (self.anim_dial, self.brightness)
end

function CockpitLight:GetLight ()
	return self.light_mode
end

return CockpitLight
