local DGSubsystem = require("DGSubsystem")
local AnimState2 = require("AnimState2")
local HatchCtrlSwitch = require("HatchCtrlSwitch")
local DGSwitch1 = require("DGSwitch1")

local meshres = require("meshres")
local GRP = meshres.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP
local meshres_p1 = require("meshres_p1")
local GRP_P1 = meshres_p1.GRP

-- HATCH_SWITCH (VC): mouse catch area
local VC_HATCH_SWITCH_mousearea = {_V(-0.15855,1.66582,6.88325),_V(-0.13855,1.66582,6.88325),_V(-0.15855,1.67617,6.84461),_V(-0.13855,1.67617,6.84461)}

-- HATCH_SWITCH (VC): rotation reference
local VC_HATCH_SWITCH_ref = _V(-0.14855,1.67389,6.86471)

-- HATCH_SWITCH (VC): rotation axis
local VC_HATCH_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local VC_HATCH_SWITCH_vofs = 297

local HATCH_OPERATING_SPEED = 0.15
-- Opening/closing speed of top hatch

local TophatchCtrl = Class(DGSubsystem)

function TophatchCtrl:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.hatch_state = AnimState2 (HATCH_OPERATING_SPEED)
	self.hatch_vent = nil
	self.hatchfail = 0

	self.ELID_SWITCH, self.sw = self:AddElement (HatchCtrlSwitch (self))

	-- Top hatch animation
	local HatchGrp = {GRP.Hatch1,GRP.Hatch2}
	local Hatch = MGROUP_ROTATE (0, HatchGrp, _V(0,2.069,5.038), _V(1,0,0), 110*RAD)
	local VCHatchGrp = {GRP_VC.HATCH}
	local VCHatch = MGROUP_ROTATE (1, VCHatchGrp, _V(0,2.069,5.038), _V(1,0,0), 110*RAD)
	local RearLadderGrp = {GRP.RearLadder1,GRP.RearLadder2}
	local RearLadder1 = MGROUP_ROTATE (0, RearLadderGrp, _V(0,1.7621,4.0959), _V(1,0,0), -20*RAD)
	local RearLadder2 = MGROUP_ROTATE (0, {GRP.RearLadder2}, _V(0,1.1173,4.1894), _V(1,0,0), 180*RAD)

	-- virtual cockpit ladder animation
	local VCRearLadderGrp = {GRP_VC.LADDER1,GRP_VC.LADDER2}
	local VCRearLadder1 = MGROUP_ROTATE (1, VCRearLadderGrp, _V(0,1.7621,4.0959), _V(1,0,0), -20*RAD)
	local VCRearLadder2 = MGROUP_ROTATE (1, {GRP_VC.LADDER2}, _V(0,1.1173,4.1894), _V(1,0,0), 180*RAD)
	self.anim_hatch = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_hatch, 0, 1, Hatch)
	self:DG():add_animationcomponent (self.anim_hatch, 0, 1, VCHatch)
	self:DG():add_animationcomponent (self.anim_hatch, 0, 0.25, RearLadder1)
	self:DG():add_animationcomponent (self.anim_hatch, 0.25, 0.8, RearLadder2)
	self:DG():add_animationcomponent (self.anim_hatch, 0, 0.25, VCRearLadder1)
	self:DG():add_animationcomponent (self.anim_hatch, 0.25, 0.8, VCRearLadder2)
end

--------------------------------------------------------------

function TophatchCtrl:destroy ()
	if self.hatch_vent then
		self:DG():del_exhauststream (self.hatch_vent)
	end
end

--------------------------------------------------------------

function TophatchCtrl:OpenHatch ()
	if self.hatch_state:IsClosed() and not self.hatch_vent and self:DG():get_atmpressure() < 10e3 then
		local airvent = {
			flags = 0,
			srcsize = 1.0,
			srcrate = 15,
			v0 = 0.5,
			srcspread = 0.3,
			lifetime = 2,
			growthrate = 0.3,
			atmslowdown = 1.0,
			ltype = PARTICLE.EMISSIVE,
			levelmap = PARTICLE.LVL_LIN,
			lmin = 0.1,
			lmax = 0.1,
			atmsmap = PARTICLE.ATM_FLAT,
			amin = 0.1,
			amax = 0.1,
			tex = nil
		}
		local pos = _V(0,2,4)
		local dir = _V(0,1,0)
		local lvl = 0.1

		self.hatch_vent = self:DG():add_particlestream (airvent, pos, dir, lvl)
		self.hatch_vent_t = oapi.get_simtime()
	end
	self.hatch_state:Open()
	self.sw:SetState(DGSwitch1.UP)
	self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("HATCH", "OPEN")
end

--------------------------------------------------------------

function TophatchCtrl:CloseHatch ()
	self.hatch_state:Close()
	self.sw:SetState(DGSwitch1.DOWN)
	self:DG():trigger_redrawarea(1, 0, self.ELID_SWITCH)
	self:DG():UpdateStatusIndicators()
	self:DG():record_event ("HATCH", "CLOSE")
end

--------------------------------------------------------------

function TophatchCtrl:Revert ()
	if self.hatch_state:IsOpen() or self.hatch_state:IsOpening() then
		self:CloseHatch()
	else
		self:OpenHatch()
	end
end

--------------------------------------------------------------

function TophatchCtrl:clbkSaveState (scn)
	self.hatch_state:SaveState (scn, "HATCH")
end

--------------------------------------------------------------

function TophatchCtrl:clbkParseScenarioLine (line)
	return self.hatch_state:ParseScenarioLine (line, "HATCH")
end

--------------------------------------------------------------

function TophatchCtrl:clbkPostCreation ()
	self:DG():set_animation (self.anim_hatch, self.hatch_state:State())	
end

--------------------------------------------------------------

function TophatchCtrl:clbkPostStep (simt, simdt, mjd)
	-- animate top hatch
	if self.hatchfail == 0 and self.hatch_state:Process (simdt) then
		self:DG():set_animation (self.anim_hatch, self.hatch_state:State())
		self:DG():UpdateStatusIndicators()
	end

	-- air venting particle stream
	if self.hatch_vent and simt > self.hatch_vent_t + 1.0 then
		self:DG():del_exhauststream (self.hatch_vent)
		self.hatch_vent = nil
	end

	-- test for damage condition
	if self.hatchfail < 2 and self.hatch_state:State() > 0.05 and self:DG():get_dynpressure() > 30e3 then
		if oapi.rand() < 1.0 - math.exp(-simdt*0.2) then
			self.hatchfail = self.hatchfail + 1
			if self.hatchfail == 1 then -- jam hatch
				self.hatch_state:SetState (0.2, 0.0)
				self:DG():set_animation(self.anim_hatch, 0.2)
			else                        -- tear off hatch
				local HatchGrp = {12,88}
				local ges = {}
				ges.flags = GRPEDIT.SETUSERFLAG
				ges.UsrFlag = 3
				for i=1,2 do
					oapi.edit_meshgroup (self:DG().exmesh, HatchGrp[i], ges)
				end
			end
		end
	end
end

--------------------------------------------------------------

function TophatchCtrl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 1 then return false end

	-- Hatch open/close switch
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh1,1)
	self:DG():register_panelarea (hPanel, self.ELID_SWITCH, _R(434,192,460,244), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.sw)
	self.sw:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 0)

	return true
end

--------------------------------------------------------------

function TophatchCtrl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Hatch open/close switch
	oapi.VC_register_area (self.ELID_SWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_SWITCH, VC_HATCH_SWITCH_mousearea)
	self.sw:DefineAnimationVC (VC_HATCH_SWITCH_ref, VC_HATCH_SWITCH_axis, GRP_VC.SWITCH1, VC_HATCH_SWITCH_vofs)

	return true
end

--------------------------------------------------------------

function TophatchCtrl:clbkPlaybackEvent (simt, event_t, event_type, event)
	if event_type == "HATCH" then
		if event == "CLOSE" then
			self:CloseHatch()
		else
			self:OpenHatch()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function TophatchCtrl:clbkConsumeBufferedKey(key, down, kstate)
	if KEYMOD_ALT(kstate) or KEYMOD_SHIFT(kstate) then
		return false
	end

	if key == OAPI_KEY.U then
		self:Revert()
		return true
	end
	return false
end

--------------------------------------------------------------

function TophatchCtrl:RepairDamage ()
	if self.hatchfail ~= 0 then
		self.hatch_state:SetState (0.0, 0.0)
		self:DG():set_animation (self.anim_hatch, 0.0)
		local HatchGrp = {12,88}
		local ges = {}
		ges.flags = GRPEDIT.SETUSERFLAG
		ges.UsrFlag = 0
		for i=1,2 do
			oapi.edit_meshgroup (self:DG().exmesh, HatchGrp[i], ges)
		end
	end
end

function TophatchCtrl:State()
	return self.hatch_state
end

return TophatchCtrl
