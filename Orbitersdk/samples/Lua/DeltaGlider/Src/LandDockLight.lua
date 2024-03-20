local DGSubsystem = require("DGSubsystem")
local LandDockLightSwitch = require("LandDockLightSwitch")
local DGSwitch1 = require("DGSwitch1")

local meshres_p1 = require("meshres_p1")
local GRP_P1 = meshres_p1.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- LANDINGLIGHT_SWITCH (VC): mouse catch area
local VC_LANDINGLIGHT_SWITCH_mousearea = {_V(0.09011,1.65220,6.93405),_V(0.11011,1.65220,6.93405),_V(0.09011,1.66256,6.89541),_V(0.11011,1.66256,6.89541)}

-- LANDINGLIGHT_SWITCH (VC): rotation reference
local VC_LANDINGLIGHT_SWITCH_ref = _V(0.10011,1.66028,6.91550)

-- LANDINGLIGHT_SWITCH (VC): rotation axis
local VC_LANDINGLIGHT_SWITCH_axis = _V(1.00000,0.00000,0.00000)
local VC_LANDINGLIGHT_SWITCH_vofs = 165



local LandDockLight = Class(DGSubsystem)

function LandDockLight:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.light_mode = 0
	self.light = nil
	self.ELID_SWITCH, self.sw = self:AddElement (LandDockLightSwitch (self))
end

--------------------------------------------------------------

function LandDockLight:SetLight (mode, force)
	if mode == self.light_mode and not force then return end -- nothing to do

	if mode ~= self.light_mode then
		self.light_mode = mode
		self.sw:SetState(mode == 0 and DGSwitch1.CENTER or (mode == 1 and DGSwitch1.UP or DGSwitch1.DOWN))
		self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	end
		
	if self.light then
		self:DG():del_lightemitter (self.light)
		self.light = nil
	end
	if mode ~= 0 then
		local col_a = _COLOUR4(0,0,0,0)
		local col_white = _COLOUR4(1,1,1,0)
		if mode == 1 then
			local att = {
				range = 150,
				att0 = 1e-3,
				att1 = 0,
				att2 = 1e-3,
				umbra = RAD*30,
				penumbra = RAD*60
			}
			self.light = self:DG():add_spotlight(_V(0.3,0.3,8.5), _V(0,0,1), att, col_white, col_white, col_a)
		else
			local att = {
				range = 5000,
				att0 = 1e-3,
				att1 = 1e-5,
				att2 = 2e-7,
				umbra = RAD*25,
				penumbra = RAD*40
			}
			local tilt = -10.0*RAD
			self.light = self:DG():add_spotlight(_V(0.1,-0.3,7.5), _V(0,math.sin(tilt),math.cos(tilt)), att, col_white, col_white, col_a)
		end
		self.light:set_visibility (VIS.ALWAYS)
	end
end

--------------------------------------------------------------

function LandDockLight:clbkSaveState (scn)
	if self.light_mode ~= 0 then
		oapi.writescenario_int (scn, "LANDDOCKLIGHT", self.light_mode)
	end
end

--------------------------------------------------------------

function LandDockLight:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "LANDDOCKLIGHT %d", match) then
		self.light_mode = math.max (0, math.min (2, match.res[1]))
		return true
	end
	return false
end

--------------------------------------------------------------

function LandDockLight:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 1 then return false end

	-- Landing/docking light switch
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh1,1)
	self:DG():register_panelarea (hPanel, self.ELID_SWITCH, _R(708,192,734,244), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.sw)
	self.sw:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 12)

	return true
end

--------------------------------------------------------------

function LandDockLight:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Landing/docking light switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_LANDINGLIGHT_SWITCH_mousearea)
	self.sw:DefineAnimationVC (VC_LANDINGLIGHT_SWITCH_ref, VC_LANDINGLIGHT_SWITCH_axis, GRP_VC.SWITCH1, VC_LANDINGLIGHT_SWITCH_vofs)

	return true
end

--------------------------------------------------------------

function LandDockLight:clbkResetVC (vcid, hMesh)
	self:SetLight (self.light_mode, true)
end

function LandDockLight:GetLight (vcid, hMesh)
	return self.light_mode
end

return LandDockLight
