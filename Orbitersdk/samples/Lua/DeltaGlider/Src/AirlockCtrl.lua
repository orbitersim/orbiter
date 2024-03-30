local DGSubsystem = require("DGSubsystem")
local AnimState2 = require("AnimState2")
local OuterLockSwitch = require("OuterLockSwitch")
local InnerLockSwitch = require("InnerLockSwitch")
local DGSwitch1 = require("DGSwitch1")

local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP
local meshres_p1 = require("meshres_p1")
local GRP_P1 = meshres_p1.GRP


local AIRLOCK_OPERATING_SPEED = 0.1
-- Opening/closing speed of outer airlock (1/sec)
-- => cycle = 10 sec

-- ILOCK_SWITCH (VC): mouse catch area
local VC_ILOCK_SWITCH_mousearea = {_V(-0.13150,1.66582,6.88325),_V(-0.11150,1.66582,6.88325),_V(-0.13150,1.67617,6.84461),_V(-0.11150,1.67617,6.84461)}

-- ILOCK_SWITCH (VC): rotation reference
local VC_ILOCK_SWITCH_ref = _V(-0.12150,1.67389,6.86471)

-- ILOCK_SWITCH (VC): rotation axis
local VC_ILOCK_SWITCH_axis = _V(1.00000,0.00000,0.00000)

-- OLOCK_SWITCH (VC): mouse catch area
local VC_OLOCK_SWITCH_mousearea = {_V(-0.10445,1.66582,6.88325),_V(-0.08445,1.66582,6.88325),_V(-0.10445,1.67617,6.84461),_V(-0.08445,1.67617,6.84461)}

-- OLOCK_SWITCH (VC): rotation reference
local VC_OLOCK_SWITCH_ref = _V(-0.09445,1.67389,6.86471)

-- OLOCK_SWITCH (VC): rotation axis
local VC_OLOCK_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local VC_ILOCK_SWITCH_vofs = 330

local VC_OLOCK_SWITCH_vofs = 363



local AirlockCtrl = Class(DGSubsystem)

function AirlockCtrl:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.ostate = AnimState2 (AIRLOCK_OPERATING_SPEED)
	self.istate = AnimState2 (AIRLOCK_OPERATING_SPEED)

	self.ELID_OSWITCH, self.osw = self:AddElement (OuterLockSwitch (self))
	self.ELID_ISWITCH, self.isw = self:AddElement (InnerLockSwitch (self))

	-- Outer airlock animation
	local OLockGrp = {GRP.OLock1,GRP.OLock2}
	local OLock = MGROUP_ROTATE (0, OLockGrp, _V(0,-0.080,9.851), _V(1,0,0), 110*RAD)
	local VCOLockGrp = {13}
	local VCOLock = MGROUP_ROTATE (1, VCOLockGrp, _V(0,-0.080,9.851), _V(1,0,0), 110*RAD)
	self.anim_olock = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_olock, 0, 1, OLock)
	self:DG():add_animationcomponent (self.anim_olock, 0, 1, VCOLock)

	-- Inner airlock animation
	local ILockGrp = {GRP.ILock1,GRP.ILock2}
	local ILock = MGROUP_ROTATE  (0, ILockGrp, _V(0,-0.573,7.800), _V(1,0,0), 85*RAD)
	-- virtual cockpit mesh animation (inner airlock visible from cockpit)
	local VCILockGrp = {GRP_VC.ILOCK1,GRP_VC.ILOCK2,GRP_VC.ILOCK3,GRP_VC.ILOCK_GLASS}
	local VCILock = MGROUP_ROTATE  (1, VCILockGrp, _V(0,-0.573,7.800), _V(1,0,0), 85*RAD)
	self.anim_ilock = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_ilock, 0, 1, ILock)
	self:DG():add_animationcomponent (self.anim_ilock, 0, 1, VCILock)
end

--------------------------------------------------------------

function AirlockCtrl:OpenOuterLock ()
	self.ostate:Open()
	self.osw:SetState(DGSwitch1.UP);
	self:DG():trigger_redrawarea(1, 0, self.ELID_OSWITCH)
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("OLOCK", "OPEN")
end

--------------------------------------------------------------

function AirlockCtrl:CloseOuterLock ()
	self.ostate:Close()
	self.osw:SetState(DGSwitch1.DOWN)
	self:DG():trigger_redrawarea(1, 0, self.ELID_OSWITCH)
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("OLOCK", "CLOSE")
end

--------------------------------------------------------------

function AirlockCtrl:RevertOuterLock ()
	if self.ostate:IsOpen() or self.ostate:IsOpening() then
		self:CloseOuterLock()
	else
		self:OpenOuterLock()
	end
end

--------------------------------------------------------------

function AirlockCtrl:OpenInnerLock ()
	self.istate:Open()
	self.isw:SetState(DGSwitch1.UP)
	self:DG():trigger_redrawarea(1, 0, self.ELID_ISWITCH)
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("ILOCK", "OPEN")
end

--------------------------------------------------------------

function AirlockCtrl:CloseInnerLock ()
	self.istate:Close()
	self.isw:SetState(DGSwitch1.DOWN)
	self:DG():trigger_redrawarea(1, 0, self.ELID_ISWITCH)
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("ILOCK", "CLOSE")
end

--------------------------------------------------------------

function AirlockCtrl:RevertInnerLock ()
	if self.istate:IsOpen() or self.istate:IsOpening() then
		self:CloseInnerLock()
	else
		self:OpenInnerLock()
	end
end

--------------------------------------------------------------

function AirlockCtrl:clbkSaveState (scn)
	self.ostate:SaveState (scn, "AIRLOCK")
	self.istate:SaveState (scn, "IAIRLOCK")
end

--------------------------------------------------------------

function AirlockCtrl:clbkParseScenarioLine (line)
	return (self.ostate:ParseScenarioLine (line, "AIRLOCK") or
		    self.istate:ParseScenarioLine (line, "IAIRLOCK"))
end

--------------------------------------------------------------

function AirlockCtrl:clbkPostCreation ()
	self:DG():set_animation (self.anim_olock, self.ostate:State())
	self:DG():set_animation (self.anim_ilock, self.istate:State())	
end

--------------------------------------------------------------

function AirlockCtrl:clbkPostStep (simt, simdt, mjd)
	-- animate outer airlock
	if self.ostate:Process (simdt) then
		self:DG():set_animation (self.anim_olock, self.ostate:State())
		self:DG():UpdateStatusIndicators()
	end

	-- animate inner airlock
	if self.istate:Process (simdt) then
		self:DG():set_animation (self.anim_ilock, self.istate:State())
		self:DG():UpdateStatusIndicators()
	end
end

--------------------------------------------------------------

function AirlockCtrl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 1 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh1,1)

	-- Inner airlock open/close switch
	self:DG():register_panelarea (hPanel, self.ELID_ISWITCH, _R(480,192,506,244), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.isw)
	self.isw:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 4)

	-- Outer airlock open/close switch
	self:DG():register_panelarea (hPanel, self.ELID_OSWITCH, _R(526,192,552,244), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.osw)
	self.osw:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 8)

	return true
end

--------------------------------------------------------------

function AirlockCtrl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Outer airlock open/close switch
	oapi.VC_register_area (self.ELID_OSWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_OSWITCH, VC_OLOCK_SWITCH_mousearea)
	self.osw:DefineAnimationVC (VC_OLOCK_SWITCH_ref, VC_OLOCK_SWITCH_axis, GRP_VC.SWITCH1, VC_OLOCK_SWITCH_vofs)

	-- Inner airlock open/close switch
	oapi.VC_register_area (self.ELID_ISWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_ISWITCH, VC_ILOCK_SWITCH_mousearea)
	self.isw:DefineAnimationVC (VC_ILOCK_SWITCH_ref, VC_ILOCK_SWITCH_axis, GRP_VC.SWITCH1, VC_ILOCK_SWITCH_vofs)

	return true
end

--------------------------------------------------------------

function AirlockCtrl:clbkPlaybackEvent (simt, event_t, event_type, event)
	if event_type == "OLOCK" then
		if event == "CLOSE" then
			self:CloseOuterLock()
		else
			self:OpenOuterLock()
		end
		return true
	elseif event_type == "ILOCK" then
		if event == "CLOSE" then
			self:CloseInnerLock()
		else
			self:OpenInnerLock()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function AirlockCtrl:clbkConsumeBufferedKey (key, down, kstate)
	if KEYMOD_ALT(kstate) or KEYMOD_SHIFT(kstate) then
		return false
	end

	if key == OAPI_KEY.O then
		if KEYMOD_CONTROL(kstate) then
			self:RevertInnerLock()
		else
			self:RevertOuterLock()
		end
		return true
	end
	return false
end

function AirlockCtrl:OLockState()
	return self.ostate
end

function AirlockCtrl:ILockState()
	return self.istate
end

return AirlockCtrl
