-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: EscapeLadderCtrl.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local AnimState2 = require("AnimState2")
local LadderSwitch = require("LadderSwitch")
local LadderIndicator = require("LadderIndicator")

local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- ELADDER_SWITCH (VC): mouse catch area
local VC_ELADDER_SWITCH_mousearea = {_V(0.34200,0.86332,7.09217),_V(0.36200,0.86332,7.09217),_V(0.34200,0.89583,7.11548),_V(0.36200,0.89583,7.11548)}

-- ELADDER_SWITCH (VC): rotation reference
local VC_ELADDER_SWITCH_ref = _V(0.35200,0.87783,7.10626)

-- ELADDER_SWITCH (VC): rotation axis
local VC_ELADDER_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local LADDER_OPERATING_SPEED = 0.1
-- Deployment speed of escape ladder

local VC_ELADDER_SWITCH_vofs = 33


local EscapeLadderCtrl = Class(DGSubsystem)

function EscapeLadderCtrl:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.ladder_state = AnimState2 (LADDER_OPERATING_SPEED)
	self.ELID_SWITCH, self.sw = self:AddElement (LadderSwitch (self))
	self.ELID_INDICATOR, self.indicator = self:AddElement (LadderIndicator (self))

	-- Escape ladder animation
	local LadderGrp = {GRP.Ladder1,GRP.Ladder2}
	local Ladder1 = MGROUP_TRANSLATE (0, LadderGrp, _V(0,0,1.1))
	local Ladder2 = MGROUP_ROTATE  (0, LadderGrp, _V(0,-1.05,9.85), _V(1,0,0), 80*RAD)
	self.anim_ladder = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_ladder, 0, 0.5, Ladder1)
	self:DG():add_animationcomponent (self.anim_ladder, 0.5, 1, Ladder2)
end

--------------------------------------------------------------

function EscapeLadderCtrl:ExtendLadder ()
	if not self:Parent():NconeState():IsOpen() then return end
	-- don't extend ladder if nosecone is closed

	if self:DG():get_dockstatus(self:DG():get_dockhandle(0)) then return end
	-- don't extend ladder if dock is engaged

	self.ladder_state:Open()
	self:DG():record_event ("LADDER", "OPEN")
end

--------------------------------------------------------------

function EscapeLadderCtrl:RetractLadder ()
	self.ladder_state:Close()
	self:DG():record_event ("LADDER", "CLOSE")
end

--------------------------------------------------------------

function EscapeLadderCtrl:clbkPostCreation ()
	self:DG():set_animation (self.anim_ladder, self.ladder_state:State())
end

--------------------------------------------------------------

function EscapeLadderCtrl:clbkPostStep (simt, simdt, mjd)
	-- animate escape ladder
	if self.ladder_state:Process (simdt) then
		self:DG():set_animation (self.anim_ladder, self.ladder_state:State())
		self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)
	end
end

--------------------------------------------------------------

function EscapeLadderCtrl:clbkSaveState (scn)
	self.ladder_state:SaveState (scn, "LADDER")
end

--------------------------------------------------------------

function EscapeLadderCtrl:clbkParseScenarioLine (line)
	return self.ladder_state:ParseScenarioLine (line, "LADDER")
end

--------------------------------------------------------------

function EscapeLadderCtrl:clbkPlaybackEvent (simt, event_t, event_type, event)
	if event_type == "LADDER" then
		if event == "CLOSE" then
			self:RetractLadder()
		else
			self:ExtendLadder()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function EscapeLadderCtrl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	-- Ladder extend/retract switch

	self:DG():register_panelarea (hPanel, self.ELID_SWITCH, _R(1171,496,1197,548), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.sw)
	self.sw:DefineAnimation2D (self:DG().panelmesh0, GRP_P0.INSTRUMENTS_ABOVE, 44)

	-- Ladder indicator
	self:DG():register_panelarea (hPanel, self.ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, panel2dtex, self.indicator)

	return true
end

--------------------------------------------------------------

function EscapeLadderCtrl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Ladder extend/retract switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_ELADDER_SWITCH_mousearea)
	self.sw:DefineAnimationVC (VC_ELADDER_SWITCH_ref, VC_ELADDER_SWITCH_axis, GRP_VC.SWITCH1, VC_ELADDER_SWITCH_vofs)

	-- Ladder indicator
	oapi.VC_register_area (self.ELID_INDICATOR, PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE)

	return true
end

function EscapeLadderCtrl:State()
	return self.ladder_state
end

return EscapeLadderCtrl
