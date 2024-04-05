-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: CoolantLoopDisplay.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

-- redeclared here to prevent circular dependency with CoolantLoop.lua
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


local CoolantLoopDisplay = Class(PanelElement)

function CoolantLoopDisplay:new (comp, blitsrc)
	PanelElement.new(self, comp:DG())
	self.component = comp
	self.bsrc = blitsrc
	self.btgt = nil
end

--------------------------------------------------------------

function CoolantLoopDisplay:ResetVC (hMesh)
	self.upt = 0.0
	self.btgt = oapi.get_texturehandle (self.component:DG().vcmesh_tpl, 14)
end

--------------------------------------------------------------

function CoolantLoopDisplay:Redraw2D (surf)
	self.btgt = surf
	return self:Redraw()
end

--------------------------------------------------------------

function CoolantLoopDisplay:RedrawVC (hMesh, surf)
	return self:Redraw()
end

--------------------------------------------------------------

function CoolantLoopDisplay:Redraw ()
	if not self.btgt then return false end

	local t = oapi.get_simtime()
	if t < self.upt and t > self.upt-1.0 then return false end
	self.upt = t + 0.5

	local cbuf
	cbuf = string.format("%5.2f", self.component.node[PUMP].pumprate)
	self:BlitReadout (0, 67, 78, cbuf)

	cbuf = string.format("%5.1f", self.component.Tref_tgt)
	self:BlitReadout (1, 67, 227, cbuf)

	cbuf = string.format("%5.1f", self.component.node[EXCHANGER_CABIN].T0)
	self:BlitReadout (2, 67, 200, cbuf)

	cbuf = string.format("%5.1f", self.component.node[EXCHANGER_CABIN].T1)
	self:BlitReadout (3, 67, 149, cbuf)

	cbuf = string.format("%5.1f", self.component.node[EXCHANGER_CABIN].cprm.T)
	self:BlitReadout (4, 67, 178, cbuf)

	cbuf = string.format("%5.1f", self.component.node[EXCHANGER_AVIONICSCOLDPLATE].T1)
	self:BlitReadout (5, 67, 98, cbuf)

	cbuf = string.format("%5.1f", self.component.node[EXCHANGER_RADIATOR].T1)
	self:BlitReadout (6, 238, 113, cbuf)

	cbuf = string.format("%5.1f", self.component.node[MERGER_WINGDISTRIBUTE].T1)
	self:BlitReadout (7, 238, 189, cbuf)

	cbuf = string.format("%5.1f", self.component.node[MERGER_WINGBYPASS].T1)
	self:BlitReadout (8, 238, 211, cbuf)

	cbuf = string.format("%5.1f", 0.5*(self.component.node[EXCHANGER_PROPRWING].cprm.T+self.component.node[EXCHANGER_PROPLWING].cprm.T))
	self:BlitReadout (10, 238, 169, cbuf)

	cbuf = string.format("%4.2f", self.component.node[SPLITTER_WINGBYPASS]:Flowrate(self.component.node[MERGER_WINGBYPASS]))
	self:BlitReadout (11, 167, 125, cbuf, 4)

	cbuf = string.format("%4.2f", self.component.node[SPLITTER_WINGBYPASS]:Flowrate(self.component.node[SPLITTER_WINGDISTRIBUTE]))
	self:BlitReadout (12, 202, 125, cbuf, 4)

	cbuf = string.format("%4.2f", self.component.node[SPLITTER_HEATSINKBYPASS]:Flowrate(self.component.node[MERGER_HEATSINKBYPASS]))
	self:BlitReadout (13, 145, 60, cbuf, 4)

	cbuf = string.format("%4.2f", self.component.node[SPLITTER_HEATSINKBYPASS]:Flowrate(self.component.node[EXCHANGER_RADIATOR]))
	self:BlitReadout (14, 180, 60, cbuf, 4)

	return false
end

--------------------------------------------------------------

local lut_srcx = {
	['0'] = 0,
	['1'] = 8,
	['2'] = 16,
	['3'] = 24,
	['4'] = 32,
	['5'] = 40,
	['6'] = 48,
	['7'] = 56,
	['8'] = 64,
	['9'] = 72,
	['.'] = 10*8+2,
	['+'] = 11*8,
	['-'] = 12*8,
	['k'] = 13*8,
	['M'] = 14*8,
	['G'] = 15*8,
}

function CoolantLoopDisplay:BlitReadout (which, tgtx, tgty, str, maxchar)
	local w = 8
	local h = 11
	local srcy = 0
	local len = #str
	for i = 1, len do
		local c = str:sub(i,i)
		local srcx = lut_srcx[c] or 16*8
		oapi.blt (self.btgt, self.bsrc, tgtx, tgty, srcx, srcy, w, h)
		tgtx = tgtx + (c == '.' and 4 or w)
	end
end


function CoolantLoopDisplay:Refresh()
	self.upt = 0.0
end

return CoolantLoopDisplay
