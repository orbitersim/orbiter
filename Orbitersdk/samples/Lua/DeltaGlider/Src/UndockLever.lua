-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: UndockLever.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP

local PANEL2D_WIDTH = 1280  -- panel width [pixel]
local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height
local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024


local UndockLever = Class(PanelElement)

function UndockLever:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
	self.btndown = false
end

--------------------------------------------------------------

function UndockLever:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 100
	self.btndown = false
end

--------------------------------------------------------------

function UndockLever:Redraw2D (surf)
	local texh = PANEL2D_TEXH -- texture height
	local bb_y0 =  368.0      -- top edge of button block
	local tx_dy = 103.0       -- texture block height
	local tx_y0 = texh-356.0  -- top edge of texture block

	local y = self.btndown and (bb_y0+tx_dy) or bb_y0
	local tv = (self.btndown and (tx_y0+tx_dy) or tx_y0)/texh
	self.grp.Vtx[self.vtxofs+3].y = y
	self.grp.Vtx[self.vtxofs+4].y = y
	self.grp.Vtx[self.vtxofs+3].tv = tv
	self.grp.Vtx[self.vtxofs+4].tv = tv
	return false
end

--------------------------------------------------------------

function UndockLever:ProcessMouse2D (event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		self.component:DG():undock (0)
	end
	self.btndown = (event == PANEL_MOUSE.LBDOWN)
	return true
end

--------------------------------------------------------------

function UndockLever:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		self.component:PullLever()
	else
		self.component:ReleaseLever()
	end
	return false
end

return UndockLever
