-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: ScramThrottleLever.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP


local ScramThrottleLever = Class(PanelElement)

function ScramThrottleLever:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
	self.ppos={0,0}
end

--------------------------------------------------------------

function ScramThrottleLever:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.SCRAM_INSTRUMENTS)
	self.vtxofs = 0
end

--------------------------------------------------------------

function ScramThrottleLever:ResetVC (hMesh)
	self.sliderpos = {-1,-1}
end

--------------------------------------------------------------

function ScramThrottleLever:Redraw2D (surf)
	local tx_dy = 18.0
	local bb_y0 = 541.5

	local sy = {bb_y0,bb_y0,bb_y0+tx_dy,bb_y0+tx_dy}

	for i=1,2 do
		local level = self.component:Parent():GetThrusterLevel(i)
		local pos = -level*84.0
		if pos ~= self.ppos[i] then
			local vofs = self.vtxofs+(i-1)*4
			for j = 1,4 do
				self.grp.Vtx[vofs+j].y = sy[j]+pos
			end
			self.ppos[i] = pos
		end
	end
	return false
end

--------------------------------------------------------------
local ctrl2d = 0
function ScramThrottleLever:ProcessMouse2D (event, mx, my)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then -- record which slider to operate
		if     mx <  12 then
			ctrl2d = 0  -- left engine
		elseif mx >= 37 then
			ctrl2d = 1  -- right engine
		else
			ctrl2d = 2  -- both
		end
	end
	self.component:Parent():SetThrusterLevel (ctrl2d, math.max (0.0, math.min (1.0, 1.0-my/84.0)))
	return true
end

--------------------------------------------------------------

function ScramThrottleLever:RedrawVC (hMesh, hSurf)
	for i = 1,2 do
		local level = self.component:Parent():GetThrusterLevel (i)
		local pos = math.floor(level*500.0)
		if pos ~= self.sliderpos[i] then
			self.component:DG():set_animation (self.component.anim_lever[i], level)
			self.sliderpos[i] = pos
		end
	end
	return true
end

--------------------------------------------------------------

local ctrlVC = 0
local pyVC = 0.0
function ScramThrottleLever:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then -- record which slider to operate
		if     p.x < 0.3 then
			ctrlVC = 0  -- left engine
		elseif p.x > 0.7 then
			ctrlVC = 1  -- right engine
		else
			ctrlVC = 2  -- both
		end
		pyVC = p.y
	else
		for i = 1,2 do
			if ctrlVC == i - 1 or ctrlVC == 2 then
				local lvl = self.component:Parent():GetThrusterLevel (i)
				lvl = math.max (0.0, math.min (1.0, lvl + (p.y-pyVC)))
				if lvl < 0.01 then
					lvl = 0.0
				end
				self.component:Parent():SetThrusterLevel (i, lvl)
			end
		end
		pyVC = p.y
	end
	return true
end

return ScramThrottleLever
