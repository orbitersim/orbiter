local DGSubsystem = require("DGSubsystem")
local CoolantPumpSwitch = require("CoolantPumpSwitch")
local CoolantPumpDial = require("CoolantPumpDial")
local CoolantReftempDial = require("CoolantReftempDial")
local CoolantLoopDisplay = require("CoolantLoopDisplay")


local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP
local meshres_p1 = require("meshres_p1")
local GRP_P1 = meshres_p1.GRP

-- COOLING_PUMP_DIAL (VC): mouse catch area
local VC_COOLING_PUMP_DIAL_mousearea = {_V(0.05820,1.67813,6.83729),_V(0.07780,1.67813,6.83729),_V(0.05820,1.68320,6.81836),_V(0.07780,1.68320,6.81836)}

-- COOLING_PUMP_DIAL (VC): rotation reference
local VC_COOLING_PUMP_DIAL_ref = _V(0.06800,1.68067,6.82782)

-- COOLING_PUMP_DIAL (VC): rotation axis
local VC_COOLING_PUMP_DIAL_axis = _V(0.00000,0.96593,0.25882)

-- COOLING_REFTEMP_DIAL (VC): mouse catch area
local VC_COOLING_REFTEMP_DIAL_mousearea = {_V(0.05820,1.66810,6.87474),_V(0.07780,1.66810,6.87474),_V(0.05820,1.67317,6.85580),_V(0.07780,1.67317,6.85580)}

-- COOLING_REFTEMP_DIAL (VC): rotation reference
local VC_COOLING_REFTEMP_DIAL_ref = _V(0.06800,1.67063,6.86527)

-- COOLING_REFTEMP_DIAL (VC): rotation axis
local VC_COOLING_REFTEMP_DIAL_axis = _V(0.00000,0.96593,0.25882)

-- PUMP_SWITCH (VC): mouse catch area
local VC_PUMP_SWITCH_mousearea = {_V(0.05800,1.68249,6.82101),_V(0.07800,1.68249,6.82101),_V(0.05800,1.69284,6.78238),_V(0.07800,1.69284,6.78238)}

-- PUMP_SWITCH (VC): rotation reference
local VC_PUMP_SWITCH_ref = _V(0.06800,1.69057,6.80247)

-- PUMP_SWITCH (VC): rotation axis
local VC_PUMP_SWITCH_axis = _V(1.00000,0.00000,0.00000)

local VC_PUMP_SWITCH_vofs = 561


local PUMP = 1
local SPLITTER_HEATSINKBYPASS = 2      -- radiator+wing tank bypass
local EXCHANGER_RADIATOR = 3           -- radiator heat exchanger
local SPLITTER_WINGBYPASS = 4          -- wing tank bypass
local SPLITTER_WINGDISTRIBUTE = 5      -- wing tank distribute
local EXCHANGER_PROPLWING = 6          -- left wing tank heat exchanger
local EXCHANGER_PROPRWING = 7          -- right wing tank heat exchanger
local MERGER_WINGDISTRIBUTE = 8        -- wing tank distribute
local MERGER_WINGBYPASS = 9            -- wing tank bypass
local MERGER_HEATSINKBYPASS = 10       -- radiator+wing tank bypass
local EXCHANGER_CABIN = 11             -- cabin heat exchanger
local EXCHANGER_AVIONICSCOLDPLATE = 12 -- avionics/instrument block cold plate

-- from ThermalSubsystem
local RADIATOR = 13
local AVIONICS = 8
local CABIN = 9
local PROPELLANT_LEFTWING = 10
local PROPELLANT_RIGHTWING = 11

local cp = 0.935e3

local NodeParam = Class()
NodeParam.PUMP = 0
NodeParam.EXCHANGER = 1
NodeParam.SPLITTER = 2
NodeParam.MERGER = 3

function NodeParam:new(type)
	self.nodetype = type
end

function NodeParam:connect(up, down, p1, p2)
	self.upstream = up
	self.dnstream = down
	if self.nodetype == NodeParam.PUMP then
		self.pumprate = p1
	elseif self.nodetype == NodeParam.EXCHANGER then
		self.cprm = p1
		p2 = p2 or 300.0
		self.k = p2
	elseif self.nodetype == NodeParam.SPLITTER then
		p1 = p1 or 1.0
		self.split = p1
	end
end

function NodeParam:Flowrate (dn)
	if self.nodetype == NodeParam.PUMP then
		return self.pumprate
	elseif self.nodetype == NodeParam.EXCHANGER then
		return self.upstream:Flowrate(self)
	elseif self.nodetype == NodeParam.SPLITTER then
		return self.upstream:Flowrate(self) * (dn == self.dnstream[1] and self.split or (1.0-self.split))
	else -- MERGER
		return self.upstream[1]:Flowrate(self) + self.upstream[2]:Flowrate(self)
	end
end

--------------------------------------------------------------

function NodeParam:Update (simdt)
	if self.nodetype == NodeParam.PUMP or self.nodetype == NodeParam.SPLITTER then
		if self.upstream:Flowrate(self) ~= 0 then
			self.T0 = self.upstream.T1
			self.T1 = self.T0
		end
	elseif self.nodetype == NodeParam.MERGER then
		local flow0 = self.upstream[1]:Flowrate(self)
		local flow1 = self.upstream[2]:Flowrate(self)
		local flow = flow0 + flow1
		if flow ~= 0 then
			self.T0 = (self.upstream[1].T1*flow0 + self.upstream[2].T1*flow1)/flow
			self.T1 = self.T0
		end
	else -- EXCHANGER
		if self.cprm then
			local T = self.cprm.T -- reservoir temperature
			local flowrate = self:Flowrate(self)
			if flowrate ~= 0 then
				self.T0 = self.upstream.T1
				local efficiency = flowrate*cp             -- transfer efficiency
				local dTrel = math.exp(-self.k/efficiency) -- 0: T1=T(reservoir); 1: T1=T0
				self.T1 = T + dTrel*(self.T0-T)            -- coolant exit temp
				local q = (self.T1-self.T0)*efficiency     -- heat flow rate [W]
				self.cprm.T = self.cprm.T - q * simdt / (self.cprm.mass * self.cprm.cp) -- update reservoir temp

				-- DEBUG
				if math.abs(self.k-15.0) < 1e-6 then
					oapi.dbg_out(string.format("T0=%f, T1=%f, T(av)=%f, Q=%f", T0, T1, self.cprm.T, q))
				end
			--else
				-- todo
			end
		end
	end
end



local CoolantLoop = Class(DGSubsystem)

function CoolantLoop:new (_subsys)
	DGSubsystem.new(self,_subsys)
	self.ssys_th = _subsys
	self.nnode = 12

	self.bPumpActive = false
	self.ELID_PUMPSWITCH, self.psw = self:AddElement (CoolantPumpSwitch (self))
	self.ELID_PUMPDIAL, self.pdial = self:AddElement (CoolantPumpDial (self))
	self.ELID_REFTEMPDIAL, self.tdial = self:AddElement (CoolantReftempDial (self))
	self.ELID_DISPLAY, self.disp = self:AddElement (CoolantLoopDisplay (self, g_Param.surf))

	-- pump dial animation
	local PumpDialTransform = MGROUP_ROTATE (1, {GRP_VC.COOLING_PUMP_DIAL}, VC_COOLING_PUMP_DIAL_ref, VC_COOLING_PUMP_DIAL_axis, -280*RAD)
	self.anim_vc_pumpdial = self:DG():create_animation (0.5)
	self:DG():add_animationcomponent (self.anim_vc_pumpdial, 0, 1, PumpDialTransform)

	-- reference temperature dial animation
	local ReftempDialTransform = MGROUP_ROTATE (1, {GRP_VC.COOLING_REFTEMP_DIAL}, VC_COOLING_REFTEMP_DIAL_ref, VC_COOLING_REFTEMP_DIAL_axis, -280*RAD)
	self.anim_vc_reftempdial = self:DG():create_animation (0.5)
	self:DG():add_animationcomponent (self.anim_vc_reftempdial, 0, 1, ReftempDialTransform)

	-- configure connections
	self.node = {}
	self.node[PUMP] = NodeParam(NodeParam.PUMP)
	self.node[SPLITTER_HEATSINKBYPASS] = NodeParam(NodeParam.SPLITTER)
	self.node[EXCHANGER_RADIATOR] = NodeParam(NodeParam.EXCHANGER)
	self.node[SPLITTER_WINGBYPASS] = NodeParam(NodeParam.SPLITTER)
	self.node[SPLITTER_WINGDISTRIBUTE] = NodeParam(NodeParam.SPLITTER)
	self.node[EXCHANGER_PROPLWING] = NodeParam(NodeParam.EXCHANGER)
	self.node[EXCHANGER_PROPRWING] = NodeParam(NodeParam.EXCHANGER)
	self.node[MERGER_WINGDISTRIBUTE] = NodeParam(NodeParam.MERGER)
	self.node[MERGER_WINGBYPASS] = NodeParam(NodeParam.MERGER)
	self.node[MERGER_HEATSINKBYPASS] = NodeParam(NodeParam.MERGER)
	self.node[EXCHANGER_CABIN] = NodeParam(NodeParam.EXCHANGER)
	self.node[EXCHANGER_AVIONICSCOLDPLATE] = NodeParam(NodeParam.EXCHANGER)

	self.node[PUMP]:connect(self.node[EXCHANGER_AVIONICSCOLDPLATE], self.node[SPLITTER_HEATSINKBYPASS], 0.0)
	self.node[SPLITTER_HEATSINKBYPASS]:connect(self.node[PUMP], {self.node[EXCHANGER_RADIATOR],	self.node[MERGER_HEATSINKBYPASS]}, 1.0)
	self.node[EXCHANGER_RADIATOR]:connect(self.node[SPLITTER_HEATSINKBYPASS], self.node[SPLITTER_WINGBYPASS], self.ssys_th.cprm[RADIATOR], 300.0)
	self.node[SPLITTER_WINGBYPASS]:connect(self.node[EXCHANGER_RADIATOR], {self.node[SPLITTER_WINGDISTRIBUTE], self.node[MERGER_WINGBYPASS]}, 1.0)
	self.node[SPLITTER_WINGDISTRIBUTE]:connect(self.node[SPLITTER_WINGBYPASS], {self.node[EXCHANGER_PROPLWING], self.node[EXCHANGER_PROPRWING]}, 0.5)
	self.node[EXCHANGER_PROPLWING]:connect(self.node[SPLITTER_WINGDISTRIBUTE], self.node[MERGER_WINGDISTRIBUTE], self.ssys_th.cprm[PROPELLANT_LEFTWING], 40.0)
	self.node[EXCHANGER_PROPRWING]:connect(self.node[SPLITTER_WINGDISTRIBUTE], self.node[MERGER_WINGDISTRIBUTE], self.ssys_th.cprm[PROPELLANT_RIGHTWING], 40.0)
	self.node[MERGER_WINGDISTRIBUTE]:connect({self.node[EXCHANGER_PROPLWING], self.node[EXCHANGER_PROPRWING]}, self.node[MERGER_WINGBYPASS])
	self.node[MERGER_WINGBYPASS]:connect({self.node[MERGER_WINGDISTRIBUTE], self.node[SPLITTER_WINGBYPASS]}, self.node[MERGER_HEATSINKBYPASS])
	self.node[MERGER_HEATSINKBYPASS]:connect({self.node[MERGER_WINGBYPASS], self.node[SPLITTER_HEATSINKBYPASS]}, self.node[EXCHANGER_CABIN])
	self.node[EXCHANGER_CABIN]:connect(self.node[MERGER_HEATSINKBYPASS], self.node[EXCHANGER_AVIONICSCOLDPLATE], self.ssys_th.cprm[CABIN], 100.0)
	self.node[EXCHANGER_AVIONICSCOLDPLATE]:connect(self.node[EXCHANGER_CABIN], self.node[PUMP], self.ssys_th.cprm[AVIONICS], 10.0)

	-- default propellant temperature
	for i=1,self.nnode do
		self.node[i].T0 = 293.0
		self.node[i].T1 = 293.0
	end
	self.Tref_tgt = 287.0
	self.pumprate = 0.5
end

--------------------------------------------------------------

function CoolantLoop:ActivatePump (on)
	self.bPumpActive = on
	self.node[PUMP].pumprate = on and self.pumprate or 0.0
end

--------------------------------------------------------------

function CoolantLoop:SetPumprate (rate)
	self.pumprate = rate
	if self.bPumpActive then
		self.node[PUMP].pumprate = self.pumprate
	end
	self:DG():set_animation (self.anim_vc_pumpdial, self.pumprate*1.0)
	self.disp:Refresh()
end

--------------------------------------------------------------

function CoolantLoop:IncPumprate (increase)
	local rate
	local dT = oapi.get_simstep()*0.2
	if increase then
		rate = math.min(1.0, self.pumprate+dT)
	else
		rate = math.max(0.0, self.pumprate-dT)
	end
	self:SetPumprate (rate)
end

--------------------------------------------------------------

function CoolantLoop:SetReftemp (temp)
	self.Tref_tgt = temp;
	self:DG():set_animation (self.anim_vc_reftempdial, (self.Tref_tgt-282.0)*0.1)
	self.disp:Refresh()
end

--------------------------------------------------------------

function CoolantLoop:IncReftemp (increase)
	local temp
	local dT = oapi.get_simstep()*2.0
	if increase then
		temp = math.min(292.0, self.Tref_tgt+dT)
	else
		temp = math.max(282.0, self.Tref_tgt-dT)
	end
	self:SetReftemp (temp)
end

--------------------------------------------------------------

function CoolantLoop:clbkPreStep (simt, simdt, mjd)
	self.node[EXCHANGER_CABIN].k = self.node[EXCHANGER_CABIN].cprm.mass*3.7

	-- set the wing tank bypass rate
	local Ta = self.node[MERGER_WINGBYPASS].upstream[1].T1
	local Tb = self.node[MERGER_WINGBYPASS].upstream[2].T1
	self.node[SPLITTER_WINGBYPASS].split = ((Ta ~= Tb) and math.min (1.0, math.max (0.0, (self.Tref_tgt-Tb)/(Ta-Tb))) or 1.0)

	-- set the heatsink bypass rate
	Ta = self.node[MERGER_HEATSINKBYPASS].upstream[1].T1
	Tb = self.node[MERGER_HEATSINKBYPASS].upstream[2].T1
	self.node[SPLITTER_HEATSINKBYPASS].split = ((Ta ~= Tb) and math.min(1.0, math.max(0.0, (self.Tref_tgt-Tb)/(Ta-Tb))) or 1.0)

	for i=1,self.nnode do
		self.node[i]:Update (simdt)
	end
end

--------------------------------------------------------------

function CoolantLoop:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	local res = DGSubsystem.clbkLoadPanel2D (self, panelid, hPanel, viewW, viewH)

	if panelid ~= 1 then return res end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh1,1)

	-- Temperature display
	local surf = oapi.get_texturehandle(self:DG().panelmesh1,2)
	self:DG():register_panelarea (hPanel, self.ELID_DISPLAY, _R(0,0,0,0), PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE, surf, self.disp)

	-- Coolant pump switch
	self:DG():register_panelarea (hPanel, self.ELID_PUMPSWITCH, _R(708,9,734,61), PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.psw)
	self.psw:DefineAnimation2D (self:DG().panelmesh1, GRP_P1.INSTRUMENTS_ABOVE, 48)

	return true
end

--------------------------------------------------------------

function CoolantLoop:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- coolant loop display
	oapi.VC_register_area (self.ELID_DISPLAY, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)

	-- coolant pump switch
	oapi.VC_register_area (self.ELID_PUMPSWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PUMPSWITCH, VC_PUMP_SWITCH_mousearea)
	self.psw:DefineAnimationVC (VC_PUMP_SWITCH_ref, VC_PUMP_SWITCH_axis, GRP_VC.SWITCH1, VC_PUMP_SWITCH_vofs)

	-- coolant pump dial
	oapi.VC_register_area (self.ELID_PUMPDIAL, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PUMPDIAL, VC_COOLING_PUMP_DIAL_mousearea)

	-- coolant reference temperature dial
	oapi.VC_register_area (self.ELID_REFTEMPDIAL, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_REFTEMPDIAL, VC_COOLING_REFTEMP_DIAL_mousearea)

	return true
end

--------------------------------------------------------------

function CoolantLoop:clbkSaveState (scn)
	local cbuf = string.format("%d %0.3f %0.3f", self.bPumpActive and 1 or 0, self.pumprate, self.Tref_tgt)
	oapi.writescenario_string (scn, "COOLANT_STATE", cbuf)
	DGSubsystem.clbkSaveState (self, scn)
end

 --------------------------------------------------------------

function CoolantLoop:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "COOLANT_STATE %b %f %f", match) then
		self:ActivatePump (match.res[1])
		self:SetPumprate (match.res[2])
		self:SetReftemp (match.res[3])
		return true
	end
	return DGSubsystem.clbkParseScenarioLine (self, line)
end


function CoolantLoop:PumpActive()
	return self.bPumpActive
end

return CoolantLoop
