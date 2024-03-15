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

local AirbrakeLever = Class(PanelElement)

function AirbrakeLever:new (comp)
	PanelElement.new(comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function AirbrakeLever:Reset2D (panelid, hMesh)
	self.state = -1
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 64
end

--------------------------------------------------------------

function AirbrakeLever:ResetVC (hMesh)
	local dg = self.component:DG()
	dg:set_animation (self.component.anim_airbrakelever, self.component.lever_state:State())
end

--------------------------------------------------------------

function AirbrakeLever:Redraw2D (surf)
	-- constants for panel coordinates
	local bb_y0 =  301.5
	local bb_dy =    7.0

	local dg = self.component:DG()
	local newstate = self.component.airbrake_tgt
	if newstate ~= self.state then
		self.state = newstate
		local yp = {bb_y0, bb_y0, bb_y0+bb_dy, bb_y0+bb_dy}
		local yshift = self.state*24.0
		for i=1,4 do
			self.grp.Vtx[self.vtxofs+i].y = yp[i]+yshift
		end
	end
	return false
end

--------------------------------------------------------------

function AirbrakeLever:ProcessMouse2D (event, mx, my)
	if my > 30 then
		self.component:Extend()
	else
		self.component:Retract()
	end
	return false
end

--------------------------------------------------------------

function AirbrakeLever:ProcessMouseVC (event, p)
	if p.y > 0.5 then
		self.component:Retract()
	else
		self.component:Extend()
	end
	return false
end

return AirbrakeLever
