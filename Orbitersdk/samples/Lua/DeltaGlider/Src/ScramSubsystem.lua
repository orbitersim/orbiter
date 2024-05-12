-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: ScramjetSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local Scramjet = require("Scramjet")
local ScramThrottle = require("ScramThrottle")

local TANK1_CAPACITY = 10400.0
local TANK2_CAPACITY =  2500.0
-- Main fuel tank capacities [kg] (can be split between rocket
-- fuel and scramjet fuel)

local SCRAM_DEFAULT_DIR = 9.0*RAD
-- Default scramjet thrust angle (rad)

local SCRAM_INTAKE_AREA = 1.0
-- Scramjet intake cross section (per engine) [m^2]

local SCRAM_TEMAX = {3500.0, 3200.0}
-- Max. scramjet exhaust temperature [K]

local SCRAM_FHV = {3.5e8, 2.0e8}
-- Scramjet fuel heating value [J/kg]: Amount of heat energy
-- obtained from burning 1kg of propellant

local SCRAM_MAX_DMF = {2.0,3.0}
-- Max. scramjet fuel flow rate [kg/s]

local MAX_MAIN_THRUST = {2.0e5, 1.6e5}
-- Main engine max vacuum thrust [N] per engine. (x2 for total)


local ScramSubsystem = Class(DGSubsystem)

function ScramSubsystem:new (dg)
	DGSubsystem.new (self, dg)

	self.modelidx = dg:get_flightmodel() + 1
	self.scram = Scramjet (dg)
	self.fuel_maxmass = TANK2_CAPACITY
	self.hProp = dg:create_propellantresource (self.fuel_maxmass)
	local dir = _V(0.0, math.sin(SCRAM_DEFAULT_DIR), math.cos(SCRAM_DEFAULT_DIR))

	local exhaust_scram = {
		flags = 0,
		srcsize = 2.0,
		srcrate = 10,
		v0 = 150,
		srcspread = 0.1,
		lifetime = 0.2,
		growthrate = 16,
		atmslowdown = 1.0,
		ltype = PARTICLE.EMISSIVE,
		levelmap = PARTICLE.LVL_SQRT,
		lmin = 0,
		lmax = 1,
		atmsmap = PARTICLE.ATM_PLOG,
		amin = 1e-5,
		amax = 0.1,
		tex = nil
	}

	self.hScram = {}
	self.hScram[1] = dg:create_thruster ({pos=_V(-0.9, -0.8, -5.6), dir=dir, maxth0=0, hprop=self.hProp, isp0=0})
	self.hScram[2] = dg:create_thruster ({pos=_V( 0.9, -0.8, -5.6), dir=dir, maxth0=0, hprop=self.hProp, isp0=0})
	self.scram:AddThrusterDefinition (self.hScram[1], SCRAM_FHV[self.modelidx], SCRAM_INTAKE_AREA, SCRAM_TEMAX[self.modelidx], SCRAM_MAX_DMF[self.modelidx])
	self.scram:AddThrusterDefinition (self.hScram[2], SCRAM_FHV[self.modelidx], SCRAM_INTAKE_AREA, SCRAM_TEMAX[self.modelidx], SCRAM_MAX_DMF[self.modelidx])

	local ph
	self.scram_intensity = {}
	ph = self:DG():add_exhauststream (self.hScram[1], _V(-1,-1.1,-5.4), exhaust_scram)
	self.scram_intensity[1] = oapi.particle_getlevelref(ph)
	ph = self:DG():add_exhauststream (self.hScram[2], _V( 1,-1.1,-5.4), exhaust_scram)
	self.scram_intensity[2] = oapi.particle_getlevelref(ph)
	self.scram_max = {0.0,0.0}

	self.scram_intensity[1]:set(0)
	self.scram_intensity[2]:set(0)

	self.throttle = self:AddSubsystem (ScramThrottle (self))
end

--------------------------------------------------------------

function ScramSubsystem:SetPropellantMaxMass (mass)
	self.fuel_maxmass = mass
	self:DG():set_propellantmaxmass (self.hProp,  mass)
end

--------------------------------------------------------------

function ScramSubsystem:IncThrusterLevel (which, dlvl)
	if which == 2 or which == 0 then
		self:DG():inc_thrusterlevel (self.hScram[1], dlvl)
		self.scram_intensity[1]:set(self:DG():get_thrusterlevel (self.hScram[1]) * self.scram_max[1])
	end
	if which == 2 or which == 1 then
		self:DG():inc_thrusterlevel (self.hScram[2], dlvl)
		self.scram_intensity[2]:set(self:DG():get_thrusterlevel (self.hScram[2]) * self.scram_max[2])
	end
end

--------------------------------------------------------------

function ScramSubsystem:SetThrusterLevel (which, lvl)
	if which == 2 or which == 0 then
		self:DG():set_thrusterlevel (self.hScram[1], lvl)
		self.scram_intensity[1]:set(lvl * self.scram_max[1])
	end
	if which == 2 or which == 1 then
		self:DG():set_thrusterlevel (self.hScram[2], lvl)
		self.scram_intensity[2]:set(lvl * self.scram_max[2])
	end
end

--------------------------------------------------------------

function ScramSubsystem:clbkPostStep (simt, simdt, mjd)
	DGSubsystem.clbkPostStep (self, simt, simdt, mjd)

	-- compute scramjet parameters
	local eps = 1e-8
	local Fnominal = 2.5*MAX_MAIN_THRUST[self.modelidx];

	local Fscram = self.scram:Thrust ()

	for i=1,2 do
		local level = self:DG():get_thrusterlevel (self.hScram[i])
		local Fmax  = Fscram[i]/(level+eps)
		self:DG():set_thrustermax0 (self.hScram[i], Fmax)
		self:DG():set_thrusterisp (self.hScram[i], math.max (1.0, Fscram[i]/(self.scram:DMF(i)+eps))) -- don't allow ISP=0

		-- the following are used for calculating exhaust density
		self.scram_max[i] = math.min (Fmax/Fnominal, 1.0)
		self.scram_intensity[i]:set(level * self.scram_max[i])
	end
end

--------------------------------------------------------------

function ScramSubsystem:clbkConsumeDirectKey (kstate)
	if KEYMOD_ALT (kstate) then
		if oapi.keydown (kstate, OAPI_KEY.ADD) then -- increment scram thrust
			self:IncThrusterLevel (2, 0.3 * oapi.get_simstep())
			oapi.resetkey (kstate, OAPI_KEY.ADD)
		end
		if oapi.keydown (kstate, OAPI_KEY.SUBTRACT) then -- decrement scram thrust
			self:IncThrusterLevel (2, -0.3 * oapi.get_simstep())
			oapi.resetkey (kstate, OAPI_KEY.SUBTRACT)
		end
	end
	return false
end


function ScramSubsystem:GetPropellantMaxMass ()
	return self.fuel_maxmass
end

function ScramSubsystem:GetPropellantMass ()
	return self:DG():get_propellantmass (self.hProp)
end

function ScramSubsystem:GetThrusterIsp (which)
	return self:DG():get_thrusterisp (self.hScram[which])
end

function ScramSubsystem:GetThrusterLevel (which)
	return self:DG():get_thrusterlevel (self.hScram[which])
end

function ScramSubsystem:Temp(idx, which)
	return self.scram:Temp (idx, which)
end

function ScramSubsystem:TSFC (which)
	return self.scram:TSFC (which)
end

function ScramSubsystem:DMF (which)
	return self.scram:DMF (which)
end

return ScramSubsystem
