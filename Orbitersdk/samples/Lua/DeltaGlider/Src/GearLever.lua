-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")
local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP

local GearLever = Class(PanelElement)

function GearLever:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function GearLever:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 68
end

--------------------------------------------------------------

function GearLever:Redraw2D (surf)
	local tx_dx =  176.0
	local bb_y0 =  413.0
	local leverdown
	if self.component:GearState():Speed() ~= 0 then
		leverdown = self.component:GearState():IsOpening()
	else
		leverdown = self.component:GearState():IsOpen()
	end

	local y = leverdown and (bb_y0+tx_dx) or bb_y0
	self.grp.Vtx[self.vtxofs+2+1].y = y
	self.grp.Vtx[self.vtxofs+3+1].y = y

	return false
end

--------------------------------------------------------------

function GearLever:ProcessMouse2D (event, mx, my)
	if self.component:GearState():IsClosed() or self.component:GearState():IsClosing() then
		if my < 151 then
			self.component:LowerGear()
		end
	else
		if my >  46 then
			self.component:RaiseGear()
		end
	end
	return false
end

--------------------------------------------------------------

function GearLever:ProcessMouseVC (event, p)
	if p.y > 0.5 then
		self.component:RaiseGear()
	else
		self.component:LowerGear()
	end
	return false
end

return GearLever
