-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: NoseconeLever.lua
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


local NoseconeLever = Class(PanelElement)

function NoseconeLever:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function NoseconeLever:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 84
end

--------------------------------------------------------------

function NoseconeLever:Redraw2D (surf)
	local texh = PANEL2D_TEXH -- texture height
	local leverdown
	if self.component:NconeState():Speed() ~= 0 then
		leverdown = self.component:NconeState():IsOpening()
	else
		leverdown = self.component:NconeState():IsOpen()
	end

	local y0, dy, tv0
	if leverdown then
		y0 = 432.5
		dy = 21.0
		tv0 = texh-677.5
	else
		y0 = 358.5
		dy = 19.0
		tv0 = texh-696.5
	end

	for j=1,4 do
		self.grp.Vtx[self.vtxofs+j].y = y0 + math.floor((j-1)/2)*dy
		self.grp.Vtx[self.vtxofs+j].tv = (tv0 + math.floor((j-1)/2)*dy)/texh
	end
	return false
end

--------------------------------------------------------------

function NoseconeLever:ProcessMouse2D (event, mx, my)
	if self.component:NconeState():IsClosed() or self.component:NconeState():IsClosing() then
		if my > 36 then
			self.component:OpenNcone()
		end
	else
		if my < 58 then
			self.component:CloseNcone()
		end
	end
	return false
end

--------------------------------------------------------------

function NoseconeLever:ProcessMouseVC (event, p)
	if p.y > 0.5 then
		self.component:CloseNcone()
	else
		self.component:OpenNcone()
	end
	return false
end

return NoseconeLever
