local DGSubsystem = require("DGSubsystem")
local RadiatorSwitch = require("RadiatorSwitch")
local DGSwitch1 = require("DGSwitch1")
local AnimState2 = require("AnimState2")

local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_p1 = require("meshres_p1")
local GRP_P1 = meshres_p1.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local RADIATOR_OPERATING_SPEED = 0.02
-- Deployment speed of radiator (1/sec)
-- => cycle = 50 sec

-- RADIATOR_SWITCH (VC): mouse catch area
local VC_RADIATOR_SWITCH_mousearea = {_V(0.16945,1.65220,6.93405),_V(0.18945,1.65220,6.93405),_V(0.16945,1.66256,6.89541),_V(0.18945,1.66256,6.89541)}

-- RADIATOR_SWITCH (VC): rotation reference
local VC_RADIATOR_SWITCH_ref = _V(0.17945,1.66028,6.91550)

-- RADIATOR_SWITCH (VC): rotation axis
local VC_RADIATOR_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local VC_RADIATOR_SWITCH_vofs = 264

local RadiatorControl = Class(DGSubsystem)

function RadiatorControl:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.radiator_state = AnimState2(RADIATOR_OPERATING_SPEED)
	self.radiator_extend = false

	self.ELID_SWITCH, self.sw = self:AddElement (RadiatorSwitch (self))

	-- Radiator animation
	local RaddoorGrp = {GRP.Raddoor1,GRP.Raddoor2,GRP.Radiator4}
	local Raddoor = MGROUP_ROTATE (0, RaddoorGrp, _V(0,1.481,-3.986), _V(1,0,0), 175*RAD)
	local FRadiatorGrp = {GRP.Radiator4}
	local FRadiator = MGROUP_ROTATE (0, FRadiatorGrp, _V(0,1.91,-2.965), _V(1,0,0), 185*RAD)
	local RadiatorGrp = {GRP.Radiator1,GRP.Radiator1a,GRP.Radiator1b,
		GRP.Radiator2,GRP.Radiator2a,GRP.Radiator2b,GRP.Radiator3}
	local Radiator = MGROUP_TRANSLATE (0, RadiatorGrp, _V(0,0.584,-0.157))
	local LRadiatorGrp = {GRP.Radiator1,GRP.Radiator1a,GRP.Radiator1b}
	local LRadiator = MGROUP_ROTATE (0, LRadiatorGrp, _V(-0.88,1.94,-4.211), _V(0,0.260,0.966), 145*RAD)
	local RRadiatorGrp = {GRP.Radiator2,GRP.Radiator2a,GRP.Radiator2b}
	local RRadiator = MGROUP_ROTATE (0, RRadiatorGrp, _V(0.93,1.91,-4.211), _V(0,0.260,0.966), -145*RAD)
	local axis1 = _V(math.cos(145*RAD),math.sin(145*RAD)*math.cos(0.26292),math.sin(145*RAD)*math.sin(-0.26292))
	local LaRadiatorGrp = {GRP.Radiator1a}
	local LaRadiator = MGROUP_ROTATE (0, LaRadiatorGrp, _V(-0.91, 1.86, -5.055), axis1, 180*RAD)
	local LbRadiatorGrp = {GRP.Radiator1b}
	local LbRadiator = MGROUP_ROTATE (0, LbRadiatorGrp, _V(-0.91, 2.075, -4.315), axis1, -180*RAD)
	local axis2 = _V(math.cos(-145*RAD),math.sin(-145*RAD)*math.cos(0.26292),math.sin(-145*RAD)*math.sin(-0.26292))
	local RaRadiatorGrp = {GRP.Radiator2a}
	local RaRadiator = MGROUP_ROTATE (0, RaRadiatorGrp, _V(0.91, 1.675, -5.01), axis2, 180*RAD)
	local RbRadiatorGrp = {GRP.Radiator2b}
	local RbRadiator = MGROUP_ROTATE (0, RbRadiatorGrp, _V(0.91, 1.89, -4.27), axis2, -180*RAD)
	self.anim_radiator = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_radiator, 0, 0.25, Raddoor)
	self:DG():add_animationcomponent (self.anim_radiator, 0.28, 0.53, FRadiator)
	self:DG():add_animationcomponent (self.anim_radiator, 0.20, 0.4, Radiator)
	self:DG():add_animationcomponent (self.anim_radiator, 0.4, 0.6, RRadiator)
	self:DG():add_animationcomponent (self.anim_radiator, 0.6, 0.8, LRadiator)
	self:DG():add_animationcomponent (self.anim_radiator, 0.9, 1.0, LaRadiator)
	self:DG():add_animationcomponent (self.anim_radiator, 0.8, 0.9, LbRadiator)
	self:DG():add_animationcomponent (self.anim_radiator, 0.9, 1.0, RaRadiator)
	self:DG():add_animationcomponent (self.anim_radiator, 0.8, 0.9, RbRadiator)
end

--------------------------------------------------------------

function RadiatorControl:OpenRadiator ()
	self.radiator_extend = true
	self.radiator_state:Open()
	if self.sw:GetState() ~= DGSwitch1.UP then
		self.sw:SetState(DGSwitch1.UP)
		self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	end
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("RADIATOR", "OPEN")
end

--------------------------------------------------------------

function RadiatorControl:CloseRadiator ()
	self.radiator_extend = false
	self.radiator_state:Close()
	if self.sw:GetState() ~= DGSwitch1.DOWN then
		self.sw:SetState(DGSwitch1.DOWN)
		self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	end
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("RADIATOR", "CLOSE")
end

--------------------------------------------------------------

function RadiatorControl:Revert ()
	if self.radiator_state:IsOpen() or self.radiator_state:IsOpening() then
		self:CloseRadiator()
	else
		self:OpenRadiator()
	end
end

--------------------------------------------------------------

function RadiatorControl:clbkPostCreation ()
	self:DG():set_animation (self.anim_radiator, self.radiator_state:State())
	self.radiator_extend = (self.radiator_state:IsOpen() or self.radiator_state:IsOpening())
end

--------------------------------------------------------------

function RadiatorControl:clbkSaveState (scn)
	self.radiator_state:SaveState (scn, "RADIATOR")
end

--------------------------------------------------------------

function RadiatorControl:clbkParseScenarioLine (line)
	return self.radiator_state:ParseScenarioLine (line, "RADIATOR")
end

--------------------------------------------------------------

function RadiatorControl:clbkPostStep (simt, simdt, mjd)
	-- animate radiator
	if self.radiator_state:Process (simdt) then
		self:DG():set_animation (self.anim_radiator, self.radiator_state:State())
		self:DG():UpdateStatusIndicators()
	end
end

--------------------------------------------------------------

function RadiatorControl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	local res = DGSubsystem.clbkLoadPanel2D (self, panelid, hPanel, viewW, viewH)

	if panelid ~= 1 then return res end

	-- Radiator switch
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh1,1)
	self:DG():register_panelarea (hPanel, self.ELID_SWITCH, _R(846,192,872,244), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.sw)
	self.sw:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 44)

	return true
end


--------------------------------------------------------------

function RadiatorControl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Radiator switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_RADIATOR_SWITCH_mousearea)
	self.sw:DefineAnimationVC (VC_RADIATOR_SWITCH_ref, VC_RADIATOR_SWITCH_axis, GRP_VC.SWITCH1, VC_RADIATOR_SWITCH_vofs)

	return true
end

--------------------------------------------------------------

function RadiatorControl:clbkResetVC (vcid, hMesh)
	--if (radiator_extend) OpenRadiator();
	--else                 CloseRadiator();
end

--------------------------------------------------------------

function RadiatorControl:clbkPlaybackEvent (simt, event_t, event_type, event)
	if event_type == "RADIATOR" then
		if event == "CLOSE" then
			self:CloseRadiator()
		else
			self:OpenRadiator()
		end
		return true
	end
	return false
end

--------------------------------------------------------------
function RadiatorControl:clbkConsumeBufferedKey (key, down, kstate)
	if KEYMOD_ALT(kstate) or KEYMOD_CONTROL(kstate) or KEYMOD_SHIFT(kstate) then
		return false
	end

	if key == OAPI_KEY.D then
		self:Revert()
		return true
	end
	return false
end

function RadiatorControl:State()
	return self.radiator_state
end

function RadiatorControl:GetRadiator ()
	return self.radiator_extend
end

return RadiatorControl
