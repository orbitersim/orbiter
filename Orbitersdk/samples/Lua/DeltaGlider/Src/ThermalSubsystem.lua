-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: ThermalSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local CoolantLoop = require("CoolantLoop")
local RadiatorControl = require("RadiatorControl")


local sigma = 5.670e-8   -- Boltzmann constant

-- heat capacity coefficients [J kg^-1 K^-1]
local c_metal = 0.6e3
local c_propellant = 4.181e3
local c_ceramic = 0.85e3
local c_air = 1.01e3
local c_radiator = 0.2e3

-- ==============================================================
-- Thermal control subsystem
-- ==============================================================

local Ax_fuselage = 27.6
local Ay_fuselage = 62.4
local Az_fuselage = 10.6
local Ay_wing = 58.8
local A_radiatorpanel1 = 3.58
local A_radiatorpanel2 = 4.4
local A_maintank = 30.0
local A_cabin = 80.0
local A_avionics = 5.0
local alpha_upper = 0.5
local alpha_lower = 0.6
local alpha_radiator = 0.2
local eps_radiator = 0.95
local k_upper = 0.34
local k_lower = 0.034
local k_cabin = 0.024
local k_convect = 5e-4


local SURFUPPERFUSELAGE = 1
local SURFLOWERFUSELAGE = 2
local SURFUPPERLEFTWING = 3
local SURFLOWERLEFTWING = 4
local SURFUPPERRIGHTWING = 5
local SURFLOWERRIGHTWING = 6
local INTERIORFUSELAGE = 7
local AVIONICS = 8
local CABIN = 9
local PROPELLANT_LEFTWING = 10
local PROPELLANT_RIGHTWING = 11
local PROPELLANT_MAIN = 12
local RADIATOR = 13

local function CompartmentParam(mass, cp, T)
	return {mass = mass, cp=cp, T=T}
end

local ThermalSubsystem = Class(DGSubsystem)

function ThermalSubsystem:new (v)
	DGSubsystem.new (self, v)

	-- compartment masses
	local m0 = v:get_emptymass()
	self.cprm = {}
	self.cprm[SURFUPPERFUSELAGE] = CompartmentParam(m0*0.2, c_metal, 293.0)
	self.cprm[SURFLOWERFUSELAGE] = CompartmentParam(m0*0.15, c_ceramic, 293.0)
	self.cprm[SURFUPPERLEFTWING] = CompartmentParam(m0*0.075, c_metal, 293.0)
	self.cprm[SURFLOWERLEFTWING] = CompartmentParam(m0*0.075, c_ceramic, 293.0)
	self.cprm[SURFUPPERRIGHTWING] = CompartmentParam(m0*0.075, c_metal, 293.0)
	self.cprm[SURFLOWERRIGHTWING] = CompartmentParam(m0*0.075, c_ceramic, 293.0)
	self.cprm[INTERIORFUSELAGE] = CompartmentParam(m0*31, c_metal, 293.0)
	self.cprm[AVIONICS] = CompartmentParam(m0*0.03, c_metal, 500.0)
	self.cprm[CABIN] = CompartmentParam(0.0, c_air, 293.0)
	self.cprm[PROPELLANT_LEFTWING] = CompartmentParam(0.0, c_propellant, 240.0)
	self.cprm[PROPELLANT_RIGHTWING] = CompartmentParam(0.0, c_propellant, 240.0)
	self.cprm[PROPELLANT_MAIN] = CompartmentParam(0.0, c_propellant, 240.0)
	self.cprm[RADIATOR] = CompartmentParam(m0*0.01, c_radiator, 293.0)

	self.eps = 0.7
	self.sr_updt = -1e10

	-- create component instances
	self.coolantloop = self:AddSubsystem (CoolantLoop (self))
	self.radiatorctrl = self:AddSubsystem (RadiatorControl (self))
end

--------------------------------------------------------------

function ThermalSubsystem:OpenRadiator ()
	self.radiatorctrl:OpenRadiator()
end

--------------------------------------------------------------

function ThermalSubsystem:CloseRadiator ()
	self.radiatorctrl:CloseRadiator()
end

--------------------------------------------------------------

function ThermalSubsystem:RadiatorState()
	return self.radiatorctrl:State()
end

--------------------------------------------------------------

function ThermalSubsystem:clbkPreStep (simt, simdt, mjd)
	if simdt == 0 then return end -- sanity check

	-- constants and spacecraft parameters
	local R = 287.0          -- gas constant [J/kg/K]
	local v_cabin = 24.0     -- cabin volume
	local v_airlock = 4.0    -- airlock volume

	-- dynamic compartment masses
	self.cprm[CABIN].mass = (self:DG():SubsysPressure():PCabin() * v_cabin + self:DG():SubsysPressure():PAirlock() * v_airlock)/(R*self.cprm[CABIN].T)
	local main_mass = self:DG():get_propellantmass(self:DG().ph_main)
	if self:DG():ScramVersion() then
		self.cprm[PROPELLANT_LEFTWING].mass = 0.5 * main_mass
		self.cprm[PROPELLANT_RIGHTWING].mass = 0.5 * main_mass
		self.cprm[PROPELLANT_MAIN].mass = self:DG():get_propellantmass(self:DG().ph_rcs) + self:DG():SubsysScram():GetPropellantMass()
	else
		self.cprm[PROPELLANT_LEFTWING].mass = 0.35 * main_mass
		self.cprm[PROPELLANT_RIGHTWING].mass = 0.35 * main_mass
		self.cprm[PROPELLANT_MAIN].mass = main_mass*0.3 + self:DG():get_propellantmass(self:DG().ph_rcs)
	end

	self.atm_p = self:DG():get_atmpressure()
	self.atm_T = self:DG():get_atmtemperature()
	self.eps = self.atm_p ~= 0 and math.exp(-0.5e-5*self.atm_p)*0.7 or 0.7

	DGSubsystem.clbkPreStep (self, simt, simdt, mjd)

	if simt > self.sr_updt + 1.0 or simt < self.sr_updt then
		-- compute solar irradiance at vessel position (0 if in shadow)
		self.H0, self.sdir = self:SolarRadiation ()
		if self.atm_p ~= 0 then
			self.H0 = self.H0 * math.exp(-3.63e-6*self.atm_p)
		end
		-- compute IR irradiation from orbited body
		self.H1, self.pdir = self:PlanetRadiation()
		self.sr_updt = simt
	end

	local dQ = {}
	for i=1,13 do
		dQ[i] = 0.0
	end

	-- irradiance
	if self.H0 ~= 0 then
		self:AddIrradiance (self.H0, self.sdir, dQ) -- add solar irrandiance
	end
	if self.H1 ~= 0 then
		self:AddIrradiance (self.H1, self.pdir, dQ) -- add planet IR irrandiance
	end
	if self.H0 ~= 0 and self.H1 ~= 0 then
		self:AddAlbedoReflection (self.H0, self.pdir, dQ) -- add reflected planet irradiance
	end
	-- add internal heat generation
	local active = bit.band(self:DG():get_flightstatus(), 1) == 0
	dQ[AVIONICS] = (active and 6e3 or 2e3) -- simplistic power management for now

	-- heat exchange from crew to cabin atmosphere (convection)
	local p = self:DG():SubsysPressure():PCabin()
	local dQ_crew = 1e-3 * p -- heat exchange is pressure-dependent
	dQ[CABIN] = dQ_crew -- pilot
	for i = 1,4 do
		if self:DG().psngr[i] then
			dQ[CABIN] = dQ[CABIN] + dQ_crew
		end
	end

	-- subtract black-body radiation
	self:SubtractBlackbodyRadiation (dQ)

	-- atmospheric heat convection
	if self.atm_p ~= 0 and self.atm_T ~= 0 then
		self:AtmosphericConvection (dQ)
	end

	-- internal heat conduction
	self:HeatConduction (dQ)

	-- compute temperature change
	for i = 1,13 do
		if self.cprm[i].mass > 0 then
			self.cprm[i].T = self.cprm[i].T + dQ[i] * simdt / (self.cprm[i].mass * self.cprm[i].cp)
		end
	end
	--oapi.dbg_out(string.format("T(inner)=%f, T(outer)=%f", self.cprm[INTERIORFUSELAGE].T, self.cprm[SURFUPPERFUSELAGE].T))
end

--------------------------------------------------------------

function ThermalSubsystem:clbkSaveState (scn)
	local cbuf = string.format("%0.2f %0.2f %0.2f %0.2f %0.2f %0.2f %0.2f %0.2f %0.2f %0.2f %0.2f %0.2f %0.2f",
		self.cprm[1].T, self.cprm[2].T, self.cprm[3].T, self.cprm[4].T, self.cprm[5].T, self.cprm[6].T, self.cprm[7].T, self.cprm[8].T, self.cprm[9].T,
		self.cprm[10].T, self.cprm[11].T, self.cprm[12].T, self.cprm[12].T)
	oapi.writescenario_string (scn, "COMPARTMENT_TEMP", cbuf)
	DGSubsystem.clbkSaveState (self, scn)
end

--------------------------------------------------------------

function ThermalSubsystem:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "COMPARTMENT_TEMP %f %f %f %f %f %f %f %f %f %f %f %f %f", match) then
		for i=1,13 do
			self.cprm[i].T = match.res[i]
		end
		return true
	end
	return DGSubsystem.clbkParseScenarioLine (self, line)
end

--------------------------------------------------------------

function ThermalSubsystem:SolarRadiation()
	-- Check if we are in the shadow of the closest celestial body
	-- For simplicity, assume sun position is at origin
	local Vpos = self:DG():get_globalpos()
	local vdist = vec.length(Vpos) -- distance from sun
	local hObj = self:DG():get_surfaceref()
	while hObj and oapi.get_objecttype(hObj) == OBJTP.PLANET do
		local prad = oapi.get_size(hObj)
		local Ppos = oapi.get_globalpos(hObj)
		local pdist = vec.length(Ppos)
		if vdist > pdist then
			local d = vec.length(vec.crossp(Ppos, vec.sub(Ppos,Vpos)))/vdist
			if d < prad then
				return 0.0
			end
		end
		hObj = oapi.get_gbodyparent(hObj)
	end

	local Vrot = self:DG():get_rotationmatrix()
	local sdir = mat.tmul(Vrot, vec.div(Vpos, -vdist))

	local hSun = oapi.get_gbody(0)
	local srad = oapi.get_size(hSun)
	local Hsun = 62499432.6; -- should be queried from hSun
	return Hsun * (srad*srad)/(vdist*vdist), sdir
end

--------------------------------------------------------------

function ThermalSubsystem:PlanetRadiation()
	local hObj = self:DG():get_surfaceref()
	if not hObj then return 0.0 end
	
	local prad = oapi.get_size(hObj)
	local Ppos = oapi.get_globalpos(hObj)
	local Vpos = self:DG():get_globalpos()
	local pdist = vec.length(vec.sub(Ppos, Vpos))
	local pdir = vec.div(vec.sub(Ppos, Vpos),pdist)
	local alt_ratio = prad/pdist
	local Hplanet = 237.0 -- W/m^2; only true for Earth
	return Hplanet * (alt_ratio*alt_ratio), pdir
end

--------------------------------------------------------------

function ThermalSubsystem:AddIrradiance (rPower, dir, compartmentQ)
	self:AddFuselageIrradiance (rPower, dir, compartmentQ)
	self:AddWingIrradiance (rPower, dir, compartmentQ)
	self:AddRadiatorIrradiance (rPower, dir, compartmentQ)
end

--------------------------------------------------------------

function ThermalSubsystem:AddAlbedoReflection (rPower, dir, compartmentQ)
	local albedo = 0.3  -- for now

	local hObj = self:DG():get_surfaceref()
	local Ppos = oapi.get_globalpos(hObj)
	local Vpos = self:DG():get_globalpos()
	local cosphi = vec.dotp(vec.unit(vec.sub(Vpos,Ppos)), vec.unit(vec.mul(Ppos,-1)))
	local irelalt = oapi.get_size(hObj)/vec.length(vec.sub(Vpos, Ppos))
	rPower = rPower * albedo * irelalt*irelalt * cosphi
	self:AddIrradiance (rPower, dir, compartmentQ)
end

--------------------------------------------------------------

function ThermalSubsystem:AddFuselageIrradiance (rPower, dir, compartmentQ)
	if dir.y >= 0.0 then -- fuselage irradiated from above
		local cs = math.abs(dir.x)*Ax_fuselage + dir.y*Ay_fuselage + math.abs(dir.z)*Az_fuselage -- projected cross section
		local dq = cs * alpha_upper * rPower
		compartmentQ[SURFUPPERFUSELAGE] = compartmentQ[SURFUPPERFUSELAGE] + dq
	else -- fuselage irradiated from below
		local cs = math.abs(dir.x)*Ax_fuselage*0.4 + math.abs(dir.z)*Az_fuselage -- Ax*0.4: assume shadowing by wing
		local dq = cs * alpha_upper * rPower
		compartmentQ[SURFUPPERFUSELAGE] = compartmentQ[SURFUPPERFUSELAGE] + dq
		cs = -dir.y*Ay_fuselage
		dq = cs * alpha_lower * rPower
		compartmentQ[SURFLOWERFUSELAGE] = compartmentQ[SURFLOWERFUSELAGE] + dq
	end
end

--------------------------------------------------------------

function ThermalSubsystem:AddWingIrradiance (rPower, dir, compartmentQ)
	if dir.y >= 0.0 then  -- wings irradiated from above
		local cs = dir.y * Ay_wing -- projected cross section
		local dq = cs * alpha_upper * rPower
		if dir.y > 0.3 or math.abs(dir.x) < 0.3 then
			compartmentQ[SURFUPPERLEFTWING] = compartmentQ[SURFUPPERLEFTWING] + dq
			compartmentQ[SURFUPPERRIGHTWING] = compartmentQ[SURFUPPERRIGHTWING] + dq
		else -- wing shadowed by fuselage
			compartmentQ[dir.x > 0.0 and SURFUPPERRIGHTWING or SURFUPPERLEFTWING] = compartmentQ[dir.x > 0.0 and SURFUPPERRIGHTWING or SURFUPPERLEFTWING] + dq
		end
	else -- wings irradiated from below
		local cs = -dir.y * Ay_wing
		local dq = cs * alpha_lower * rPower
		compartmentQ[SURFLOWERLEFTWING] = compartmentQ[SURFLOWERLEFTWING] + dq
		compartmentQ[SURFLOWERRIGHTWING] = compartmentQ[SURFLOWERRIGHTWING] + dq
	end
end

--------------------------------------------------------------

function ThermalSubsystem:AddRadiatorIrradiance (rPower, dir, compartmentQ)
	local rstate = self:RadiatorState():State()
	if rstate == 0 then return end

	-- panel 1
	local pprog = math.min(1.0, rstate/0.33)
	local alpha = (pprog*175.0-100.0)*RAD
	local paneldir = _V(0, math.sin(alpha), -math.cos(alpha))
	local cosa = vec.dotp(dir, paneldir) -- irradiance cosine
	if cosa > 0.0 then
		if pprog < 0.5 then
			cosa = cosa*pprog*2.0
		end
		local cs = cosa * A_radiatorpanel2
		local dq = cs * alpha_radiator * rPower
		compartmentQ[RADIATOR] = compartmentQ[RADIATOR] + dq
	end

	-- panel 2
	pprog = math.max (0.0, math.min(1.0, (rstate-0.5)*4.0))
	alpha = pprog*145.0*RAD
	paneldir = _V(-math.sin(alpha), -math.cos(alpha), 0)
	cosa = vec.dotp(dir, paneldir)
	if cosa > 0.0 then
		if pprog < 0.5 then
			cosa = cosa*pprog*2.0
		end
		local cs = cosa * A_radiatorpanel1
		local dq = cs * alpha_radiator * rPower
		compartmentQ[RADIATOR] = compartmentQ[RADIATOR] + dq
	end

	-- panel 3
	pprog = math.max (0.0, math.min(1.0, (rstate-0.75)*4.0))
	alpha = pprog*145.0*RAD
	paneldir = _V(math.sin(alpha), -math.cos(alpha), 0)
	cosa = vec.dotp(dir, paneldir)
	if cosa > 0.0 then
		if pprog < 0.5 then
			cosa = cosa*pprog*2.0
		end
		local cs = cosa * A_radiatorpanel1
		local dq = cs * alpha_radiator * rPower
		compartmentQ[RADIATOR] = compartmentQ[RADIATOR] + dq
	end
end

--------------------------------------------------------------

function ThermalSubsystem:SubtractBlackbodyRadiation (compartmentQ)
	-- radiation from vessel surface
	compartmentQ[SURFUPPERFUSELAGE]  = compartmentQ[SURFUPPERFUSELAGE]  - (Ax_fuselage*2.0 + Ay_fuselage + Az_fuselage*2.0) * self.eps * sigma * math.pow(self.cprm[SURFUPPERFUSELAGE].T, 4.0)
	compartmentQ[SURFLOWERFUSELAGE]  = compartmentQ[SURFLOWERFUSELAGE]  - Ay_fuselage * self.eps * sigma * math.pow(self.cprm[SURFLOWERFUSELAGE].T, 4.0)
	compartmentQ[SURFUPPERLEFTWING]  = compartmentQ[SURFUPPERLEFTWING]  - Ay_wing * self.eps * sigma * math.pow(self.cprm[SURFUPPERLEFTWING].T, 4.0)
	compartmentQ[SURFLOWERLEFTWING]  = compartmentQ[SURFLOWERLEFTWING]  - Ay_wing * self.eps * sigma * math.pow(self.cprm[SURFLOWERLEFTWING].T, 4.0)
	compartmentQ[SURFUPPERRIGHTWING] = compartmentQ[SURFUPPERRIGHTWING] - Ay_wing * self.eps * sigma * math.pow(self.cprm[SURFUPPERRIGHTWING].T, 4.0)
	compartmentQ[SURFLOWERRIGHTWING] = compartmentQ[SURFLOWERRIGHTWING] - Ay_wing * self.eps * sigma * math.pow(self.cprm[SURFLOWERRIGHTWING].T, 4.0)
	
	local rstate = self:RadiatorState():State()
	if rstate ~= 0 then
		local A_radiator = A_radiatorpanel2 + 2.0*A_radiatorpanel1 * 1.4 -- 1.4: assume fractional emission from lower panel surfaces
		compartmentQ[RADIATOR] = compartmentQ[RADIATOR] - rstate * A_radiator * eps_radiator * sigma * math.pow(self.cprm[RADIATOR].T, 4.0)
	end
end

--------------------------------------------------------------

function ThermalSubsystem:AtmosphericConvection (compartmentQ)
	local dq
	local cprm = self.cprm
	local atm_T = self.atm_T
	local atm_p = self.atm_p

	dq = (cprm[SURFLOWERFUSELAGE].T - atm_T) * k_convect * atm_p * Ay_fuselage
	compartmentQ[SURFLOWERFUSELAGE] = compartmentQ[SURFLOWERFUSELAGE] - dq
	dq = (cprm[SURFUPPERFUSELAGE].T - atm_T) * k_convect * atm_p * (Ay_fuselage + 2.0*Ax_fuselage + 2.0*Az_fuselage)
	compartmentQ[SURFUPPERFUSELAGE] = compartmentQ[SURFUPPERFUSELAGE] - dq
	dq = (cprm[SURFUPPERLEFTWING].T - atm_T) * k_convect * atm_p * Ay_wing
	compartmentQ[SURFUPPERLEFTWING] = compartmentQ[SURFUPPERLEFTWING] - dq
	dq = (cprm[SURFLOWERLEFTWING].T - atm_T) * k_convect * atm_p * Ay_wing
	compartmentQ[SURFLOWERLEFTWING] = compartmentQ[SURFLOWERLEFTWING] - dq
	dq = (cprm[SURFUPPERRIGHTWING].T - atm_T) * k_convect * atm_p * Ay_wing
	compartmentQ[SURFUPPERRIGHTWING] = compartmentQ[SURFUPPERRIGHTWING] - dq
	dq = (cprm[SURFLOWERRIGHTWING].T - atm_T) * k_convect * atm_p * Ay_wing
	compartmentQ[SURFLOWERRIGHTWING] = compartmentQ[SURFLOWERRIGHTWING] - dq
	
	local rstate = self:RadiatorState():State()
	if rstate ~= 0 then
		local A_radiator = A_radiatorpanel2 + 4.0*A_radiatorpanel1
		dq = (cprm[RADIATOR].T - atm_T) * k_convect * atm_p * A_radiator
		compartmentQ[RADIATOR] = compartmentQ[RADIATOR] - dq
	end

	local pssys = self:DG():SubsysPressure()
	if pssys:HatchState():IsOpen() or pssys:OLockState():IsOpen() and pssys:ILockState():IsOpen() then
		dq = (cprm[CABIN].T - atm_T) * k_convect * atm_p * cprm[CABIN].mass
		compartmentQ[CABIN] = compartmentQ[CABIN] - dq
	end
end

--------------------------------------------------------------

function ThermalSubsystem:HeatConduction (compartmentQ)
	local q
	local cprm = self.cprm
	--   left wing surface <--> left wing tank
	if cprm[PROPELLANT_LEFTWING].mass > 0 then
		q = (cprm[SURFUPPERLEFTWING].T - cprm[PROPELLANT_LEFTWING].T) * k_upper * Ay_wing
		compartmentQ[SURFUPPERLEFTWING] = compartmentQ[SURFUPPERLEFTWING] - q
		compartmentQ[PROPELLANT_LEFTWING] = compartmentQ[PROPELLANT_LEFTWING] + q
		q = (cprm[SURFLOWERLEFTWING].T - cprm[PROPELLANT_LEFTWING].T) * k_lower * Ay_wing
		compartmentQ[SURFLOWERLEFTWING] = compartmentQ[SURFLOWERLEFTWING] - q
		compartmentQ[PROPELLANT_LEFTWING] = compartmentQ[PROPELLANT_LEFTWING] + q
	end
	--   right wing surface <--> right wing tank
	if cprm[PROPELLANT_RIGHTWING].mass > 0 then
		q = (cprm[SURFUPPERRIGHTWING].T - cprm[PROPELLANT_RIGHTWING].T) * k_upper * Ay_wing
		compartmentQ[SURFUPPERRIGHTWING] = compartmentQ[SURFUPPERRIGHTWING] - q
		compartmentQ[PROPELLANT_RIGHTWING] = compartmentQ[PROPELLANT_RIGHTWING] + q
		q = (cprm[SURFLOWERRIGHTWING].T - cprm[PROPELLANT_RIGHTWING].T) * k_lower * Ay_wing
		compartmentQ[SURFLOWERRIGHTWING] = compartmentQ[SURFLOWERRIGHTWING] - q
		compartmentQ[PROPELLANT_RIGHTWING] = compartmentQ[PROPELLANT_RIGHTWING] + q
	end
	-- fuselage surface <--> fuselage interior
	q = (cprm[SURFUPPERFUSELAGE].T - cprm[INTERIORFUSELAGE].T) * k_upper * (Ax_fuselage*2.0 + Ay_fuselage + Az_fuselage*2.0)
	compartmentQ[SURFUPPERFUSELAGE] = compartmentQ[SURFUPPERFUSELAGE] - q
	compartmentQ[INTERIORFUSELAGE] = compartmentQ[INTERIORFUSELAGE] + q
	q = (cprm[SURFLOWERFUSELAGE].T - cprm[INTERIORFUSELAGE].T) * k_lower * Ay_fuselage
	compartmentQ[SURFLOWERFUSELAGE] = compartmentQ[SURFLOWERFUSELAGE] - q
	compartmentQ[INTERIORFUSELAGE] = compartmentQ[INTERIORFUSELAGE] + q
	-- fuselage <--> wings
	q = (cprm[SURFUPPERFUSELAGE].T - cprm[SURFUPPERLEFTWING].T) * k_upper * 3.0
	compartmentQ[SURFUPPERFUSELAGE] = compartmentQ[SURFUPPERFUSELAGE] - q
	compartmentQ[SURFUPPERLEFTWING] = compartmentQ[SURFUPPERLEFTWING] + q
	q = (cprm[SURFUPPERFUSELAGE].T - cprm[SURFUPPERRIGHTWING].T) * k_upper * 3.0
	compartmentQ[SURFUPPERFUSELAGE] = compartmentQ[SURFUPPERFUSELAGE] - q
	compartmentQ[SURFUPPERRIGHTWING] = compartmentQ[SURFUPPERRIGHTWING] + q
	q = (cprm[SURFLOWERFUSELAGE].T - cprm[SURFLOWERLEFTWING].T) * k_lower * 3.0
	compartmentQ[SURFLOWERFUSELAGE] = compartmentQ[SURFLOWERFUSELAGE] - q
	compartmentQ[SURFLOWERLEFTWING] = compartmentQ[SURFLOWERLEFTWING] + q
	q = (cprm[SURFLOWERFUSELAGE].T - cprm[SURFLOWERRIGHTWING].T) * k_lower * 3.0
	compartmentQ[SURFLOWERFUSELAGE] = compartmentQ[SURFLOWERFUSELAGE] - q
	compartmentQ[SURFLOWERRIGHTWING] = compartmentQ[SURFLOWERRIGHTWING] + q
	-- fuselage interior <--> interior tank
	if cprm[PROPELLANT_MAIN].mass > 0 then
		q = (cprm[INTERIORFUSELAGE].T - cprm[PROPELLANT_MAIN].T) * k_upper * A_maintank
		compartmentQ[INTERIORFUSELAGE] = compartmentQ[INTERIORFUSELAGE] - q
		compartmentQ[PROPELLANT_MAIN] = compartmentQ[PROPELLANT_MAIN] + q
	end
	-- fuselage interior <--> cabin
	q = (cprm[INTERIORFUSELAGE].T - cprm[CABIN].T) * k_cabin * A_cabin * cprm[CABIN].mass
	compartmentQ[INTERIORFUSELAGE] = compartmentQ[INTERIORFUSELAGE] - q
	compartmentQ[CABIN] = compartmentQ[CABIN] + q
	-- fuselage interior <--> avionics
	q = (cprm[INTERIORFUSELAGE].T - cprm[AVIONICS].T) * k_upper * A_avionics
	compartmentQ[INTERIORFUSELAGE] = compartmentQ[INTERIORFUSELAGE] - q
	compartmentQ[AVIONICS] = compartmentQ[AVIONICS] + q
	-- cabin <--> avionics
	q = (cprm[CABIN].T - cprm[AVIONICS].T) * k_cabin * A_avionics * cprm[CABIN].mass
	compartmentQ[CABIN] = compartmentQ[CABIN] - q
	compartmentQ[AVIONICS] = compartmentQ[AVIONICS] + q
	-- radiator <--> fuselage exterior
	local rstate = self:RadiatorState():State()
	if rstate ~= 0 then
		q = (cprm[RADIATOR].T - cprm[SURFUPPERFUSELAGE].T) * 0.02 * A_radiatorpanel2
		compartmentQ[RADIATOR] = compartmentQ[RADIATOR] - q
		compartmentQ[SURFUPPERFUSELAGE] = compartmentQ[SURFUPPERFUSELAGE] + q
	end
end


return ThermalSubsystem
