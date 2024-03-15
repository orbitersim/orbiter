local DGSubsystem = require("DGSubsystem")
local AnimState2 = require("AnimState2")
local HUDModeButtons = require("HUDModeButtons")
local HUDBrightnessDial = require("HUDBrightnessDial")
local HUDColourButton = require("HUDColourButton")
local HUDUpDownSwitch = require("HUDUpDownSwitch")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local HUD_OPERATING_SPEED = 0.15

-- HUD_BRIGHTNESS (VC): rotation reference
local VC_HUD_BRIGHTNESS_ref = _V(-0.01040,1.27171,7.26800)

-- HUD_BRIGHTNESS (VC): rotation axis
local VC_HUD_BRIGHTNESS_axis = _V(0.00000,0.00000,1.00000)

-- HUD_BUTTONS (VC): mouse catch area
local VC_HUD_BUTTONS_mousearea = {_V(-0.02840,1.28605,7.26800),_V(0.02840,1.28605,7.26800),_V(-0.02840,1.30047,7.26800),_V(0.02840,1.30047,7.26800)}

local VC_BTN_HUDMODE_1_vofs = 60

local VC_BTN_HUDMODE_1_LABEL_vofs = 32

local VC_BTN_HUDMODE_2_vofs = 80

local VC_BTN_HUDMODE_2_LABEL_vofs = 40

local VC_BTN_HUDMODE_3_vofs = 100

local VC_BTN_HUDMODE_3_LABEL_vofs = 48

-- BTN_HUDMODE_1 (VC): rotation axis
local VC_BTN_HUDMODE_1_axis = _V(0.00000,0.00000,1.00000)

-- HUD_BRIGHTNESS (VC): mouse catch area
local VC_HUD_BRIGHTNESS_mousearea = {_V(-0.02020,1.26191,7.26800),_V(-0.00060,1.26191,7.26800),_V(-0.02020,1.28151,7.26800),_V(-0.00060,1.28151,7.26800)}


-- HUD_COLBUTTON (VC): mouse catch radius
local VC_HUD_COLBUTTON_mouserad = 0.009000

-- HUD_COLBUTTON (VC): rotation reference
local VC_HUD_COLBUTTON_ref = _V(0.01650,1.27171,7.26800)

-- HUD_COLBUTTON (VC): rotation axis
local VC_HUD_COLBUTTON_axis = _V(0.00000,0.00000,1.00000)

local VC_HUD_COLBUTTON_vofs = 20

-- HUDRETRACT_SWITCH (VC): mouse catch area
local VC_HUDRETRACT_SWITCH_mousearea = {_V(0.05585,1.65220,6.93405),_V(0.07585,1.65220,6.93405),_V(0.05585,1.66256,6.89541),_V(0.07585,1.66256,6.89541)}

-- HUDRETRACT_SWITCH (VC): rotation reference
local VC_HUDRETRACT_SWITCH_ref = _V(0.06585,1.66028,6.91550)

-- HUDRETRACT_SWITCH (VC): rotation axis
local VC_HUDRETRACT_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local VC_HUDRETRACT_SWITCH_vofs = 132



local HUDControl = Class(DGSubsystem)


function HUDControl:new (vessel)
	DGSubsystem.new (self, vessel)

	self.last_mode = HUDMODE.NONE
	self.hud_brightness = 1.0
	self.hud_state = AnimState2 (HUD_OPERATING_SPEED)

	self.ELID_MODEBUTTONS  , self.modebuttons = self:AddElement (HUDModeButtons (self))
	self.ELID_HUDBRIGHTNESS, self.brightdial = self:AddElement (HUDBrightnessDial (self))
	self.ELID_HUDCOLOUR    , self.colbutton = self:AddElement (HUDColourButton (self))
	self.ELID_HUDRETRACT   , self.updownswitch = self:AddElement (HUDUpDownSwitch (self))

	-- HUD brightness dial animation
	local HudBDialGrp = {GRP_VC.HUD_BRIGHTNESS}
	local HudBDialTransform = MGROUP_ROTATE (1, HudBDialGrp, VC_HUD_BRIGHTNESS_ref, VC_HUD_BRIGHTNESS_axis, -280*RAD)
	self.anim_vc_hudbdial = self:DG():create_animation (0.5)
	self:DG():add_animationcomponent (self.anim_vc_hudbdial, 0, 1, HudBDialTransform)

	-- Fold up HUD animation
	local HudGrp1 = {GRP_VC.HUD_FRAME, GRP_VC.HUD_PANE}
	local HudGrp2 = {GRP_VC.HUD_FRAME, GRP_VC.HUD_PANE, GRP_VC.HUD_RAIL}
	local HudTransform1 = MGROUP_ROTATE  (1, HudGrp1, _V(0,1.5836,7.1280), _V(1,0,0), -62*RAD)
	local HudTransform2 = MGROUP_ROTATE  (1, HudGrp2, _V(0,0.99,6.53), _V(1,0,0), -26*RAD)
	self.anim_vc_hud = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_vc_hud, 0, 0.4, HudTransform1)
	self:DG():add_animationcomponent (self.anim_vc_hud, 0.4, 1, HudTransform2)

end

--------------------------------------------------------------

function HUDControl:clbkSaveState(scn)
	if self.last_mode ~= HUDMODE.NONE then
		oapi.writescenario_int(scn, "HUDMode", self.last_mode)
	end
	if self.hud_brightness < 1.0 then
		oapi.writescenario_float(scn, "HUDBrightness", self.hud_brightness)
	end
end

--------------------------------------------------------------

function HUDControl:clbkParseScenarioLine(line)
	local match = {}
	if scenario_line_match(line, "HUDMode %d", match) then
		self.last_mode = match.res[1]
	elseif scenario_line_match(line, "HUDBrightness %f", match) then
		self.hud_brightness = match.res[1]
		oapi.set_hudintensity(self.hud_brightness)
		return true
	end
	return false
end

--------------------------------------------------------------

function HUDControl:GetHUDMode ()
	return self.last_mode
end

--------------------------------------------------------------

function HUDControl:SetHUDMode (mode)
	if self.mode ~= HUDMODE.NONE then
		self.last_mode = mode
		if oapi.cockpit_mode() ~= COCKPIT.VIRTUAL or self.hud_state:IsClosed() then
			oapi.set_hudmode (mode)
		end
		self:DG():trigger_redrawarea (0, 0, self.ELID_MODEBUTTONS)
		self.modebuttons:SetMode (mode)
	end
end

--------------------------------------------------------------

function HUDControl:ToggleHUDMode ()
	self:SetHUDMode (self.last_mode == 3 and 1 or self.last_mode+1)
end

--------------------------------------------------------------

function HUDControl:RetractHud ()
	self.hud_state:Open()
	local hudmode = oapi.get_hudmode()
	if hudmode ~= HUDMODE.NONE then
		self.last_mode = hudmode
		oapi.set_hudmode (HUDMODE.NONE)
	end
	self:DG():record_event ("HUD", "RETRACT")
end

--------------------------------------------------------------

function HUDControl:ExtendHud ()
	self.hud_state:Close()
	self:DG():record_event ("HUD", "EXTEND")
end

-------------------------------------------------------------

function HUDControl:RevertHud ()
	if self.hud_state:IsClosed() or self.hud_state:IsClosing() then
		self:RetractHud()
	else
		self:ExtendHud()
	end
end

--------------------------------------------------------------

function HUDControl:ModHUDBrightness (increase)
	if increase then
		oapi.inc_hudintensity()
	else
		oapi.dec_hudintensity()
	end

	self.hud_brightness = oapi.get_hudintensity()
	self:DG():set_animation (self.anim_vc_hudbdial, self.hud_brightness)
end

--------------------------------------------------------------

function HUDControl:clbkPostStep (simt, simdt, mjd)
	-- animate HUD
	if self.hud_state:Process (simdt) then
		if oapi.cockpit_mode() ~= COCKPIT.VIRTUAL then
			if self.hud_state:IsClosing() then
				self.hud_state:SetClosed()
			elseif self.hud_state:IsOpening() then
				self.hud_state:SetOpened()
			end
		end
		self:DG():set_animation(self.anim_vc_hud, self.hud_state:State())
		if self.hud_state:IsClosed() then
			oapi.set_hudmode (self.last_mode)
		end
	end
end

--------------------------------------------------------------

function HUDControl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)

	self:DG():register_panelarea (hPanel, self.ELID_MODEBUTTONS, _R(  15, 18, 122, 33), PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBPRESSED+PANEL_MOUSE.ONREPLAY, panel2dtex, self.modebuttons)

	return true
end

--------------------------------------------------------------

function HUDControl:clbkLoadVC (vcid)
	DGSubsystem.clbkLoadVC (self, vcid)

	if vcid ~= 0 then return false end

	-- HUD mode indicator/selector buttons on the dash panel
	oapi.VC_register_area (self.ELID_MODEBUTTONS, PANEL_REDRAW.USER + PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_MODEBUTTONS, VC_HUD_BUTTONS_mousearea[1], VC_HUD_BUTTONS_mousearea[2], VC_HUD_BUTTONS_mousearea[3], VC_HUD_BUTTONS_mousearea[4])

	local hudbtn_vofs = {VC_BTN_HUDMODE_1_vofs,VC_BTN_HUDMODE_2_vofs,VC_BTN_HUDMODE_3_vofs}
	local hudbtn_label_vofs = {VC_BTN_HUDMODE_1_LABEL_vofs, VC_BTN_HUDMODE_2_LABEL_vofs, VC_BTN_HUDMODE_3_LABEL_vofs}
	self.modebuttons:DefineAnimationsVC (VC_BTN_HUDMODE_1_axis, GRP_VC.BUTTON3, GRP_VC.LIT_SURF, hudbtn_vofs, hudbtn_label_vofs)

	-- HUD brightness dial
	oapi.VC_register_area (self.ELID_HUDBRIGHTNESS, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_HUDBRIGHTNESS, VC_HUD_BRIGHTNESS_mousearea[1], VC_HUD_BRIGHTNESS_mousearea[2], VC_HUD_BRIGHTNESS_mousearea[3], VC_HUD_BRIGHTNESS_mousearea[4])

	-- HUD colour selector button
	oapi.VC_register_area (self.ELID_HUDCOLOUR, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_spherical (self.ELID_HUDCOLOUR, VC_HUD_COLBUTTON_ref, VC_HUD_COLBUTTON_mouserad)
	self.colbutton:DefineAnimationVC (VC_HUD_COLBUTTON_axis, GRP_VC.BUTTON2, VC_HUD_COLBUTTON_vofs)

	-- HUD extend/retract switch
	oapi.VC_register_area (self.ELID_HUDRETRACT, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_HUDRETRACT, VC_HUDRETRACT_SWITCH_mousearea[1], VC_HUDRETRACT_SWITCH_mousearea[2], VC_HUDRETRACT_SWITCH_mousearea[3], VC_HUDRETRACT_SWITCH_mousearea[4])
	self.updownswitch:DefineAnimationVC (VC_HUDRETRACT_SWITCH_ref, VC_HUDRETRACT_SWITCH_axis, GRP_VC.SWITCH1, VC_HUDRETRACT_SWITCH_vofs)

	return true
end

--------------------------------------------------------------

function HUDControl:clbkReset2D(panelid, hMesh)
	DGSubsystem.clbkReset2D(self, panelid, hMesh)
	if self.last_mode == HUDMODE.NONE then
		self:SetHUDMode(oapi.get_hudmode())
	elseif oapi.get_hudmode() ~= self.last_mode then
		oapi.set_hudmode(self.last_mode)
	end
	oapi.set_hudintensity(self.hud_brightness)
end

--------------------------------------------------------------

function HUDControl:clbkResetVC (vcid, hMesh)
	DGSubsystem.clbkResetVC(self, vcid, hMesh)
	if self.last_mode == HUDMODE.NONE then
		self:SetHUDMode(oapi.get_hudmode())
	elseif oapi.get_hudmode() ~= self.last_mode then
		oapi.set_hudmode(self.last_mode)
	end
	oapi.set_hudintensity(self.hud_brightness)
	self:DG():set_animation (self.anim_vc_hudbdial, self.hud_brightness)
	local hudmode = oapi.get_hudmode()
	if hudmode ~= HUDMODE.NONE and not self.hud_state:IsClosed() then
		self.hud_state:SetState (0, -1)
	end
end

--------------------------------------------------------------

function HUDControl:clbkConsumeBufferedKey (key, down, kstate)
	if KEYMOD_ALT(kstate) or KEYMOD_SHIFT(kstate) then
		return 0
	end

	if key == OAPI_KEY.H then
		if KEYMOD_CONTROL(kstate) then
			self:RevertHud()
		else
			self:ToggleHUDMode()
		end
		return 1
	end
	return 0
end

return HUDControl
