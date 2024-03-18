-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local AnimState2 = require("AnimState2")
local RetroCoverSwitch = require("RetroCoverSwitch")
local RetroCoverIndicator = require("RetroCoverIndicator")

local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local RCOVER_OPERATING_SPEED = 0.3
-- Retro cover opening/closing speed

-- RCOVER_SWITCH (VC): mouse catch area
local VC_RCOVER_SWITCH_mousearea = {_V(0.30700,0.86332,7.09217),_V(0.32700,0.86332,7.09217),_V(0.30700,0.89583,7.11548),_V(0.32700,0.89583,7.11548)}

-- RCOVER_SWITCH (VC): rotation reference
local VC_RCOVER_SWITCH_ref = _V(0.31700,0.87783,7.10626)

-- RCOVER_SWITCH (VC): rotation axis
local VC_RCOVER_SWITCH_axis = _V(1.00000,0.00000,0.00000)
local VC_RCOVER_SWITCH_vofs = 0

local RetroCoverControl = Class(DGSubsystem)

function RetroCoverControl:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.rcover_state = AnimState2(RCOVER_OPERATING_SPEED)

	self.ELID_SWITCH, self. sw   = self:AddElement (RetroCoverSwitch (self))
	self.ELID_INDICATOR, self.indicator = self:AddElement (RetroCoverIndicator(self))

	-- Retro cover animation
	local RCoverTLGrp = {GRP.RCoverTL1,GRP.RCoverTL2}
	local RCoverTL = MGROUP_ROTATE(0, RCoverTLGrp, _V(-2.156,-0.49,6.886), _V(-0.423,0.23,-0.877), 70*RAD)
	local RCoverBLGrp = {GRP.RCoverBL1,GRP.RCoverBL2}
	local RCoverBL = MGROUP_ROTATE(0, RCoverBLGrp, _V(-2.156,-0.49,6.886), _V(-0.434,-0.037,-0.9), -70*RAD)
	local RCoverTRGrp = {GRP.RCoverTR1,GRP.RCoverTR2}
	local RCoverTR = MGROUP_ROTATE(0, RCoverTRGrp, _V( 2.156,-0.49,6.886), _V( 0.423,0.23,-0.877), -70*RAD)
	local RCoverBRGrp = {GRP.RCoverBR1,GRP.RCoverBR2}
	local RCoverBR = MGROUP_ROTATE  (0, RCoverBRGrp, _V( 2.156,-0.49,6.886), _V( 0.434,-0.037,-0.9), 70*RAD)
	self.anim_rcover = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_rcover, 0, 1, RCoverTL)
	self:DG():add_animationcomponent (self.anim_rcover, 0, 1, RCoverBL)
	self:DG():add_animationcomponent (self.anim_rcover, 0, 1, RCoverTR)
	self:DG():add_animationcomponent (self.anim_rcover, 0, 1, RCoverBR)

end

--------------------------------------------------------------

function RetroCoverControl:OpenRetroCover ()
	self.rcover_state:Open()
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("RCOVER", "OPEN")
end

--------------------------------------------------------------

function RetroCoverControl:CloseRetroCover ()
	self.rcover_state:Close()
	self:DG():UpdateStatusIndicators()
	self:DG():EnableRetroThrusters (false)
	self:DG():record_event ("RCOVER", "Close")
end

--------------------------------------------------------------

function RetroCoverControl:clbkPostCreation ()
	self:DG():EnableRetroThrusters (self.rcover_state:IsOpen())
	self:DG():set_animation (self.anim_rcover, self.rcover_state:State())
end

--------------------------------------------------------------

function RetroCoverControl:clbkSaveState (scn)
	self.rcover_state:SaveState (scn, "RCOVER")
end

--------------------------------------------------------------

function RetroCoverControl:clbkParseScenarioLine (line)
	return self.rcover_state:ParseScenarioLine (line, "RCOVER")
end

--------------------------------------------------------------

function RetroCoverControl:clbkPostStep (simt, simdt, mjd)
	-- animate retro covers
	if self.rcover_state:Process (simdt) then
		self:DG():set_animation (self.anim_rcover, self.rcover_state:State())
		self:DG():UpdateStatusIndicators()
		if self.rcover_state:IsOpen() then
			self:DG():EnableRetroThrusters(true)
		end
		self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)
	end
end

--------------------------------------------------------------

function RetroCoverControl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)

	-- Retro engine cover switch
	self:DG():register_panelarea (hPanel, self.ELID_SWITCH, _R(1129,496,1155,548), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.sw)
	self.sw:DefineAnimation2D (self:DG().panelmesh0, GRP_P0.INSTRUMENTS_ABOVE, 180)

	-- Retro engine cover indicator
	self:DG():register_panelarea (hPanel, self.ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, panel2dtex, self.indicator)

	return true
end

--------------------------------------------------------------

function RetroCoverControl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Retro engine cover switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_RCOVER_SWITCH_mousearea[1], VC_RCOVER_SWITCH_mousearea[2], VC_RCOVER_SWITCH_mousearea[3], VC_RCOVER_SWITCH_mousearea[4])
	self.sw:DefineAnimationVC (VC_RCOVER_SWITCH_ref, VC_RCOVER_SWITCH_axis, GRP_VC.SWITCH1, VC_RCOVER_SWITCH_vofs)

	-- Retro engine cover indicator
	oapi.VC_register_area (self.ELID_INDICATOR, PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE)

	return true
end

--------------------------------------------------------------

function RetroCoverControl:clbkPlaybackEvent (simt, event_t, event_type, event)
	if event_type == "RCOVER" then
		if event == "CLOSE" then
			self:CloseRetroCover()
		else
			self:OpenRetroCover()
		end
		return true
	end
	return false
end

function RetroCoverControl:State()
	return self.rcover_state
end


return RetroCoverControl
