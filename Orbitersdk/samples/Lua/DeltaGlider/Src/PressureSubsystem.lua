-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: PressureSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local AirlockCtrl = require("AirlockCtrl")
local TophatchCtrl = require("TophatchCtrl")
local PValveSwitch = require("PValveSwitch")
local PressureIndicator = require("PressureIndicator")

local meshres_p1 = require("meshres_p1")
local GRP_P1 = meshres_p1.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- CABIN_O2_SWITCH (VC): mouse catch area
local VC_CABIN_O2_SWITCH_mousearea = {_V(-0.18560,1.68723,6.80334),_V(-0.16560,1.68723,6.80334),_V(-0.18560,1.69758,6.76470),_V(-0.16560,1.69758,6.76470)}

-- CABIN_O2_SWITCH (VC): rotation reference
local VC_CABIN_O2_SWITCH_ref = _V(-0.17560,1.69530,6.78479)

-- CABIN_O2_SWITCH (VC): rotation axis
local VC_CABIN_O2_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local VC_CABIN_O2_SWITCH_vofs = 396

-- VALVE1_SWITCH (VC): mouse catch area
local VC_VALVE1_SWITCH_mousearea = {_V(-0.15855,1.68723,6.80334),_V(-0.13855,1.68723,6.80334),_V(-0.15855,1.69758,6.76470),_V(-0.13855,1.69758,6.76470)}

-- VALVE1_SWITCH (VC): rotation reference
local VC_VALVE1_SWITCH_ref = _V(-0.14855,1.69530,6.78479)

-- VALVE1_SWITCH (VC): rotation axis
local VC_VALVE1_SWITCH_axis = _V(1.00000,0.00000,0.00000)

-- VALVE2_SWITCH (VC): mouse catch area
local VC_VALVE2_SWITCH_mousearea = {_V(-0.13150,1.68723,6.80334),_V(-0.11150,1.68723,6.80334),_V(-0.13150,1.69758,6.76470),_V(-0.11150,1.69758,6.76470)}

-- VALVE2_SWITCH (VC): rotation reference
local VC_VALVE2_SWITCH_ref = _V(-0.12150,1.69530,6.78479)

-- VALVE2_SWITCH (VC): rotation axis
local VC_VALVE2_SWITCH_axis = _V(1.00000,0.00000,0.00000)

-- VALVE3_SWITCH (VC): mouse catch area
local VC_VALVE3_SWITCH_mousearea = {_V(-0.10445,1.68723,6.80334),_V(-0.08445,1.68723,6.80334),_V(-0.10445,1.69758,6.76470),_V(-0.08445,1.69758,6.76470)}

-- VALVE3_SWITCH (VC): rotation reference
local VC_VALVE3_SWITCH_ref = _V(-0.09445,1.69530,6.78479)

-- VALVE3_SWITCH (VC): rotation axis
local VC_VALVE3_SWITCH_axis = _V(1.00000,0.00000,0.00000)

-- LOCK_O2_SWITCH (VC): mouse catch area
local VC_LOCK_O2_SWITCH_mousearea = {_V(-0.07740,1.68723,6.80334),_V(-0.05740,1.68723,6.80334),_V(-0.07740,1.69758,6.76470),_V(-0.05740,1.69758,6.76470)}

-- LOCK_O2_SWITCH (VC): rotation reference
local VC_LOCK_O2_SWITCH_ref = _V(-0.06740,1.69530,6.78479)

-- LOCK_O2_SWITCH (VC): rotation axis
local VC_LOCK_O2_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local VC_VALVE1_SWITCH_vofs = 429

local VC_VALVE2_SWITCH_vofs = 462

local VC_VALVE3_SWITCH_vofs = 495

local VC_LOCK_O2_SWITCH_vofs = 528


local PressureSubsystem = Class(DGSubsystem)


local v_cabin = 24.0
local v_airlock = 4.0
local p_target = 100e3

--------------------------------------------------------------

function PressureSubsystem:new (vessel)
	DGSubsystem.new(self, vessel)

	self.p_cabin = 100e3
	self.p_airlock = 100e3
	self.p_ext_hatch = 0.0
	self.p_ext_lock = 0.0
	self.docked = false

	self.airlockctrl = self:AddSubsystem (AirlockCtrl (self))
	self.hatchctrl = self:AddSubsystem (TophatchCtrl (self))

	self.ELID_PVALVESWITCH = {}
	self.valve_status = {}
	self.valve_switch = {}
	for i = 1,5 do
		self.ELID_PVALVESWITCH[i], self.valve_switch[i] = self:AddElement (PValveSwitch (self, i))
		self.valve_status[i] = false
	end
	self.ELID_DISPLAY, self.pind = self:AddElement (PressureIndicator (self, g_Param.surf))
end

--------------------------------------------------------------

function PressureSubsystem:OLockState ()
	return self.airlockctrl:OLockState()
end

--------------------------------------------------------------

function PressureSubsystem:ILockState ()
	return self.airlockctrl:ILockState()
end

--------------------------------------------------------------

function PressureSubsystem:OpenOuterAirlock ()
	self.airlockctrl:OpenOuterLock ()
end

--------------------------------------------------------------

function PressureSubsystem:CloseOuterAirlock ()
	self.airlockctrl:CloseOuterLock ()
end

--------------------------------------------------------------

function PressureSubsystem:OpenInnerAirlock ()
	self.airlockctrl:OpenInnerLock ()
end

--------------------------------------------------------------

function PressureSubsystem:CloseInnerAirlock ()
	self.airlockctrl:CloseInnerLock ()
end

--------------------------------------------------------------

function PressureSubsystem:OpenHatch ()
	self.hatchctrl:OpenHatch()
end

--------------------------------------------------------------

function PressureSubsystem:CloseHatch ()
	self.hatchctrl:CloseHatch()
end

--------------------------------------------------------------

function PressureSubsystem:HatchState ()
	return self.hatchctrl:State()
end

--------------------------------------------------------------

function PressureSubsystem:RepairDamage ()
	self.hatchctrl:RepairDamage ()
end

-------------------------------------------------------------

function PressureSubsystem:clbkPostStep (simt, simdt, mjd)
	DGSubsystem.clbkPostStep (self, simt, simdt, mjd)

	self.docked = self:DG():dockingstatus(0) ~= 0
	local p_static = self:DG():get_atmpressure()
	self.p_ext_hatch = p_static
	if not self.docked then
		self.p_ext_lock = p_static
		if not self:DG():SubsysDocking():NconeState():IsClosed() then
			self.p_ext_lock = self.p_ext_lock + self:DG():get_dynpressure() * self:DG():SubsysDocking():NconeState():State()
		end
	else
		self.v_extdock = 2.0 -- for now
	end

	local pdiff, dvol, dpc, dpa, pc, pa
	-- exchange cabin - ext.hatch
	local cs = self.valve_status[2] and 2e-4 or 0.0
	cs = cs + 0.1*self:HatchState():State()
	if cs ~= 0 then
		pdiff = self.p_ext_hatch-self.p_cabin
		dvol = pdiff*cs*simdt*1e3
		dpc = dvol/v_cabin;
		pc = self.p_cabin + dpc;
		if self.p_cabin > self.p_ext_hatch then
			pc = math.max(pc, self.p_ext_hatch)
		else
			pc = math.min(pc,self.p_ext_hatch)
		end
		self.p_cabin = pc
	end

	-- exchange airlock - ext.lock
	cs = self.valve_status[4] and 2e-4 or 0.0
	cs = cs + self:OLockState():State()
	if cs ~= 0 then
		pdiff = self.p_ext_lock-self.p_airlock
		dvol = pdiff*cs*simdt*1e3
		dpa = dvol/v_airlock
		pa = self.p_airlock + dpa
		if self.docked then
			dpc = -dvol/self.v_extdock
			pc = self.p_ext_lock + dpc
			if (self.p_airlock-self.p_ext_lock)*(pa-pc) < 0.0 then
				pa = (pa*v_airlock + pc*self.v_extdock)/(v_airlock+self.v_extdock)
				pc = pa
			end
			self.p_ext_lock = pc
		else
			if self.p_airlock > self.p_ext_lock then
				pa = math.max(pa,self.p_ext_lock)
			else
				pa = math.min(pa,self.p_ext_lock)
			end
		end
		self.p_airlock = pa
	end

	-- exchange cabin - airlock
	cs = self.valve_status[3] and 2e-4 or 0.0
	cs = cs + self:ILockState():State()
	if cs ~= 0 then
		pdiff = self.p_cabin-self.p_airlock
		dvol = pdiff*cs*simdt*1e3
		dpc = -dvol/v_cabin
		dpa = dvol/v_airlock
		pc = self.p_cabin + dpc
		pa = self.p_airlock + dpa
		if (self.p_airlock-self.p_cabin)*(pa-pc) < 0.0 then
			pa = (pa*v_airlock + pc*v_cabin)/(v_airlock+v_cabin)
			pc = pa
		end
		self.p_cabin = pc
		self.p_airlock = pa
	end

	-- supply cabin
	cs = self.valve_status[1] and 5e-5 or 0.0
	if cs ~= 0 then
		pdiff = 400e3-self.p_cabin
		dvol = pdiff*cs*simdt*1e3
		dpc = dvol/v_cabin
		pc = self.p_cabin + dpc
		pc = math.min (pc, p_target)
		self.p_cabin = pc
	end

	-- supply airlock
	cs = self.valve_status[5] and 5e-5 or 0.0
	if cs ~= 0 then
		pdiff = 400e3-self.p_airlock
		dvol = pdiff*cs*simdt*1e3
		dpa = dvol/v_airlock;
		pa = self.p_airlock + dpa
		pa = math.min (pa, p_target)
		self.p_airlock = pa
	end
end

--------------------------------------------------------------

function PressureSubsystem:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	local res = DGSubsystem.clbkLoadPanel2D (self, panelid, hPanel, viewW, viewH)

	if panelid ~= 1 then return res end

	-- Pressure indicator display
	local surf = oapi.get_texturehandle(self:DG().panelmesh1,2)
	self:DG():register_panelarea (hPanel, self.ELID_DISPLAY, _R(0,0,0,0), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, surf, self.pind)

	-- Pressure valve switches
	surf = oapi.get_texturehandle(self:DG().panelmesh1,1)
	for i=1,5 do
		self:DG():register_panelarea (hPanel, self.ELID_PVALVESWITCH[i], _R(388+(i-1)*46,42,414+(i-1)*46,94), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, surf, self.valve_switch[i])
		self.valve_switch[i]:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 24+(i-1)*4)
	end

	return true
end

--------------------------------------------------------------

function PressureSubsystem:clbkLoadVC (vcid)
	DGSubsystem.clbkLoadVC (self, vcid)

	if vcid ~= 0 then return true end

	-- Pressure indicator display
	oapi.VC_register_area (self.ELID_DISPLAY, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)

	-- Pressure valve switches
	oapi.VC_register_area (self.ELID_PVALVESWITCH[1], PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PVALVESWITCH[1], VC_CABIN_O2_SWITCH_mousearea)
	self.valve_switch[1]:DefineAnimationVC (VC_CABIN_O2_SWITCH_ref, VC_CABIN_O2_SWITCH_axis, GRP_VC.SWITCH1, VC_CABIN_O2_SWITCH_vofs)

	oapi.VC_register_area (self.ELID_PVALVESWITCH[2], PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PVALVESWITCH[2], VC_VALVE1_SWITCH_mousearea)
	self.valve_switch[2]:DefineAnimationVC (VC_VALVE1_SWITCH_ref, VC_VALVE1_SWITCH_axis, GRP_VC.SWITCH1, VC_VALVE1_SWITCH_vofs)

	oapi.VC_register_area (self.ELID_PVALVESWITCH[3], PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PVALVESWITCH[3], VC_VALVE2_SWITCH_mousearea)
	self.valve_switch[3]:DefineAnimationVC (VC_VALVE2_SWITCH_ref, VC_VALVE2_SWITCH_axis, GRP_VC.SWITCH1, VC_VALVE2_SWITCH_vofs)

	oapi.VC_register_area (self.ELID_PVALVESWITCH[4], PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PVALVESWITCH[4], VC_VALVE3_SWITCH_mousearea)
	self.valve_switch[4]:DefineAnimationVC (VC_VALVE3_SWITCH_ref, VC_VALVE3_SWITCH_axis, GRP_VC.SWITCH1, VC_VALVE3_SWITCH_vofs)

	oapi.VC_register_area (self.ELID_PVALVESWITCH[5], PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PVALVESWITCH[5], VC_LOCK_O2_SWITCH_mousearea)
	self.valve_switch[5]:DefineAnimationVC (VC_LOCK_O2_SWITCH_ref, VC_LOCK_O2_SWITCH_axis, GRP_VC.SWITCH1, VC_LOCK_O2_SWITCH_vofs)

	return true
end


function PressureSubsystem:PCabin()
	return self.p_cabin
end

function PressureSubsystem:PAirlock()
	return self.p_airlock
end

function PressureSubsystem:PExtHatch()
	return self.p_ext_hatch
end

function PressureSubsystem:PExtLock()
	return self.p_ext_lock
end

function PressureSubsystem:GetPValve (i)
	return self.valve_status[i]
end

function PressureSubsystem:SetPValve (i, status)
	self.valve_status[i] = status
end


return PressureSubsystem
