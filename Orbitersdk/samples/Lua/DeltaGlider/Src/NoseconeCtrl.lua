local DGSubsystem = require("DGSubsystem")
local NoseconeLever = require("NoseconeLever")
local NoseconeIndicator = require("NoseconeIndicator")
local AnimState2 = require("AnimState2")

local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP


local NOSE_OPERATING_SPEED = 0.05
-- Opening/closing speed of nose cone docking mechanism (1/sec)
-- => cycle = 20 sec

-- NCONELEVER (VC): mouse catch area
local VC_NCONELEVER_mousearea = {_V(0.35900,0.90989,7.13172),_V(0.38900,0.90989,7.13172),_V(0.35900,1.00108,7.19712),_V(0.38900,1.00108,7.19712)}

-- NCONELEVER (VC): rotation reference
local VC_NCONELEVER_ref = _V(0.37400,0.92634,7.20505)

-- NCONELEVER (VC): rotation axis
local VC_NCONELEVER_axis = _V(1.00000,0.00000,0.00000)


local NoseconeCtrl = Class(DGSubsystem)

function NoseconeCtrl:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.ncone_state = AnimState2 (NOSE_OPERATING_SPEED)
	self.nlever_state = AnimState2 (4.0)

	self.ELID_LEVER, self.lever = self:AddElement (NoseconeLever (self))
	self.ELID_INDICATOR, self.indicator = self:AddElement (NoseconeIndicator (self))

	-- Nosecone animation
	local NConeTLGrp = {GRP.NConeTL1,GRP.NConeTL2}
	local NConeTL = MGROUP_ROTATE (0, NConeTLGrp, _V(-0.424,-0.066,9.838), _V(-0.707,-0.707,0), 150*RAD)
	local NConeTRGrp = {GRP.NConeTR1,GRP.NConeTR2}
	local NConeTR = MGROUP_ROTATE (0, NConeTRGrp, _V( 0.424,-0.066,9.838), _V(-0.707, 0.707,0), 150*RAD)
	local NConeBLGrp = {GRP.NConeBL1,GRP.NConeBL2}
	local NConeBL = MGROUP_ROTATE (0, NConeBLGrp, _V(-0.424,-0.914,9.838), _V( 0.707,-0.707,0), 150*RAD)
	local NConeBRGrp = {GRP.NConeBR1,GRP.NConeBR2}
	local NConeBR = MGROUP_ROTATE (0, NConeBRGrp, _V( 0.424,-0.914,9.838), _V( 0.707, 0.707,0), 150*RAD)
	local NConeDockGrp = {GRP.NConeDock}
	local NConeDock = MGROUP_TRANSLATE (0, NConeDockGrp, _V(0,0,0.06))
	-- virtual cockpit mesh animation (nose cone visible from cockpit)
	local VCNConeTLGrp = {GRP_VC.NOSECONE_L}
	local VCNConeTL = MGROUP_ROTATE (1, VCNConeTLGrp, _V(-0.424,-0.066,9.838), _V(-0.707,-0.707,0), 150*RAD)
	local VCNConeTRGrp = {GRP_VC.NOSECONE_R}
	local VCNConeTR = MGROUP_ROTATE (1, VCNConeTRGrp, _V( 0.424,-0.066,9.838), _V(-0.707, 0.707,0), 150*RAD)
	self.anim_nose = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_nose, 0.01, 0.92, NConeTL)
	self:DG():add_animationcomponent (self.anim_nose, 0.01, 0.92, VCNConeTL)
	self:DG():add_animationcomponent (self.anim_nose, 0.02, 0.925, NConeTR)
	self:DG():add_animationcomponent (self.anim_nose, 0.02, 0.925, VCNConeTR)
	self:DG():add_animationcomponent (self.anim_nose, 0, 0.91, NConeBL)
	self:DG():add_animationcomponent (self.anim_nose, 0.015, 0.915, NConeBR)
	self:DG():add_animationcomponent (self.anim_nose, 0.8, 1, NConeDock)

	-- Nosecone lever VC animatuion
	local NoseconeLeverGrp = {GRP_VC.NOSECONE_LEVER}
	local NoseconeLeverTransform = MGROUP_ROTATE (1, NoseconeLeverGrp, VC_NCONELEVER_ref, VC_NCONELEVER_axis, -70*RAD)
	self.anim_noselever = self:DG():create_animation (0.5)
	self:DG():add_animationcomponent (self.anim_noselever, 0, 1, NoseconeLeverTransform)
end

--------------------------------------------------------------

function NoseconeCtrl:OpenNcone ()
	self.ncone_state:Open()
	self.nlever_state:Open()
	self:DG():UpdateStatusIndicators()
	self:DG():trigger_panelredrawarea (0, self.ELID_LEVER)
	self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)
	self:DG():record_event ("NOSECONE", "OPEN")
end

--------------------------------------------------------------

function NoseconeCtrl:CloseNcone ()
	self.ncone_state:Close()
	self.nlever_state:Close()
	self:DG():UpdateStatusIndicators()
	self:DG():trigger_panelredrawarea (0, self.ELID_LEVER)
	self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)

	if not self:Parent():LadderState():IsClosed() then
		self:Parent():RetractLadder() -- retract ladder before closing the nose cone
	end

	self:DG():record_event ("NOSECONE", "CLOSE")
end

--------------------------------------------------------------

function NoseconeCtrl:RevertNcone ()
	if self.ncone_state:IsOpen() or self.ncone_state:IsOpening() then
		self:CloseNcone()
	else
		self:OpenNcone()
	end
end

--------------------------------------------------------------

function NoseconeCtrl:clbkPostStep (simt, simdt, mjd)
	-- animate nose cone
	if self.ncone_state:Process (simdt) then
		self:DG():set_animation (self.anim_nose, self.ncone_state:State())
		self:DG():trigger_redrawarea (0, 0, self.ELID_INDICATOR)
		self:DG():UpdateStatusIndicators()

		-- don't allow closing the nosecone while docked
		if self:DG():get_dockstatus(self:DG():get_dockhandle(0)) then
			if self.ncone_state:IsClosing() and self.ncone_state:State() < 0.98 then
				self:OpenNcone()
			end
		end
	end

	-- animate VC nosecone lever
	if self.nlever_state:Process (simdt) then
		self:DG():set_animation (self.anim_noselever, self.nlever_state:State())
	end
end

--------------------------------------------------------------

function NoseconeCtrl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_LEVER, _R(1221,359,1260,473), PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN, panel2dtex, self.lever)
	self:DG():register_panelarea (hPanel, self.ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, panel2dtex, self.indicator)

	return true
end

--------------------------------------------------------------

function NoseconeCtrl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Nosecone lever
	oapi.VC_register_area (self.ELID_LEVER, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_LEVER, VC_NCONELEVER_mousearea)

	-- Nosecone indicator
	oapi.VC_register_area (self.ELID_INDICATOR, PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE)

	return true
end

--------------------------------------------------------------

function NoseconeCtrl:clbkSaveState (scn)
	self.ncone_state:SaveState (scn, "NOSECONE")
end

--------------------------------------------------------------

function NoseconeCtrl:clbkParseScenarioLine (line)
	if self.ncone_state:ParseScenarioLine (line, "NOSECONE") then
		if self.ncone_state:IsOpen() or self.ncone_state:IsOpening() then
			self.nlever_state:SetOpened()
		else
			self.nlever_state:SetClosed()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function NoseconeCtrl:clbkPostCreation ()
	self:DG():set_animation (self.anim_nose, self.ncone_state:State())
	self:DG():set_animation (self.anim_noselever, self.nlever_state:State())
end

--------------------------------------------------------------

function NoseconeCtrl:clbkPlaybackEvent (simt, event_t, event_type, event)
	if event_type == "NOSECONE" then
		if event == "CLOSE" then
			self:CloseNcone()
		else
			self:OpenNcone()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function NoseconeCtrl:clbkConsumeBufferedKey (key, down, kstate)
	if KEYMOD_ALT(kstate) or KEYMOD_CONTROL(kstate) or KEYMOD_SHIFT(kstate) then
		return false
	end

	if key == OAPI_KEY.K then
		self:RevertNcone()
		return true
	end
	return false
end

function NoseconeCtrl:NconeState()
	return self.ncone_state
end

return NoseconeCtrl
