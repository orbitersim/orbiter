-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: Scramjet.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local Scramjet = Class()

function Scramjet:new (v)
	self.vessel = v
	self.thdef = {}
end

----------------------------------------------------------------
-- add new thruster definition to list

function Scramjet:AddThrusterDefinition (th, Qr, Ai, Tb_max, dmf_max)
	local thd = {}
	thd.th      = th
	thd.Qr      = Qr
	thd.Ai      = Ai
	thd.Tb_max  = Tb_max
	thd.dmf_max = dmf_max
	thd.dmf     = 0.0
	thd.F       = 0.0
	thd.T = {0,0,0}

	self.thdef[#self.thdef+1] = thd
end

----------------------------------------------------------------
-- calculate current thrust force for all engines

function Scramjet:Thrust ()
	local hBody = self.vessel:get_atmref()
	local atm = hBody and oapi.get_planetatmconstants (hBody) or nil

	if atm then -- atmospheric parameters available
		local eps = 1e-4
		local dma_scale = 2.7e-4

		local M   = self.vessel:get_machnumber()                      -- Mach number
		local T0  = self.vessel:get_atmtemperature()                  -- freestream temperature
		local p0  = self.vessel:get_atmpressure()                     -- freestream pressure
		local rho = self.vessel:get_atmdensity()                      -- freestream density
		local cp  = atm.gamma * atm.R / (atm.gamma-1.0)              -- specific heat (pressure)
		local v0  = M * math.sqrt (atm.gamma * atm.R * T0)           -- freestream velocity
		local tr  = (1.0 + 0.5*(atm.gamma-1.0) * M*M)                -- temperature ratio
		local Td  = T0 * tr                                          -- diffuser temperature
		local pd  = p0 * math.pow (Td/T0, atm.gamma/(atm.gamma-1.0)) -- diffuser pressure
		local precov = math.max (0.0, 1.0-0.075*math.pow (math.max(M,1.0)-1.0, 1.35)) -- pressure recovery
		local dmafac = dma_scale*precov*pd

		local ret = {}
	    for _, thdef in ipairs(self.thdef) do
			local Tb0 = thdef.Tb_max                                  -- max burner temperature
			if Tb0 > Td then                                          -- we are within operational range
				local lvl  = self.vessel:get_thrusterlevel(thdef.th)  -- throttle level
				local D    = (Tb0-Td) / (thdef.Qr/cp - Tb0)           -- max fuel-to-air ratio (what if negative?)
				D = D * lvl                                           -- actual fuel-to-air ratio

				local dma = dmafac * thdef.Ai                         -- air mass flow rate [kg/s]
				local dmf  = D * dma                                  -- fuel mass flow rate
				if dmf > thdef.dmf_max then                           -- max fuel rate exceeded
					dmf = thdef.dmf_max
					D = dmf/dma
				end
				local Tb   = (D*thdef.Qr/cp + Td) / (1.0+D)                   -- actual burner temperature
				local Te   = Tb * math.pow (p0/pd, (atm.gamma-1.0)/atm.gamma) -- exhaust temperature
				local ve   = math.sqrt (2.0*cp*(Tb-Te))                       -- exhaust velocity
			    local Fs  = (1.0+D)*ve - v0                                   -- specific thrust
				thdef.F =  math.max (0.0, Fs*dma)                             -- thrust force
				ret[#ret+1] = math.max (0.0, Fs*dma)                          -- thrust force
				thdef.dmf = dmf
				thdef.T[2] = Tb
				thdef.T[3] = Te
			else    -- overheating!
				thdef.F = 0.0
				ret[#ret+1] = 0.0
				thdef.dmf = 0.0;
				thdef.T[2] = Td
				thdef.T[3] = Td
			end
			thdef.T[1] = Td
		end
		return ret
	else   -- no atmospheric parameters
		local ret = {}
	    for _, v in ipairs(self.thdef) do
			v.F = 0
			v.dmf = 0
			ret[#ret+1] = 0
		end
		return ret
	end
end

--------------------------------------------------------------

function Scramjet:TSFC (idx)
	local eps = 1e-5
	return self.thdef[idx].dmf/(self.thdef[idx].F+eps)
end

function Scramjet:DMF (idx)
	return self.thdef[idx].dmf
end

function Scramjet:Temp (idx, which)
	return self.thdef[idx].T[which]
end

return Scramjet
