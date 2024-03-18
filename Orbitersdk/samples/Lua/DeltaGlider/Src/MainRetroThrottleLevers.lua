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

local MainRetroThrottleLevers = Class(PanelElement)

function MainRetroThrottleLevers:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
	self.ppos = {0.0,0.0}
end

--------------------------------------------------------------

function MainRetroThrottleLevers:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 48
end

--------------------------------------------------------------

function MainRetroThrottleLevers:ResetVC (hMesh)
	self.sliderpos = {-1, -1}
end

--------------------------------------------------------------

function MainRetroThrottleLevers:Redraw2D (surf)
	-- constants for texture coordinates
	local tx_dy = 18.0
	local bb_y0 = 241.5

	local sy = {bb_y0,bb_y0,bb_y0+tx_dy,bb_y0+tx_dy}

	local dg = self.component:DG()
	for i = 1, 2 do
		local pos
		local level = dg:GetMainThrusterLevel (i)
		if level > 0 then
			pos = -8.0-level*108.0
		else
			level = dg:GetRetroThrusterLevel (i)
			if level > 0 then
				pos = 8.0+level*30.0
			else
				pos = 0.0
			end
		end

		if pos ~= self.ppos[i] then
			local vofs = self.vtxofs+(i-1)*4
			for j = 1, 4 do
				self.grp.Vtx[vofs+j].y = sy[j]+pos
			end
			self.ppos[i] = pos
		end
	end
	return false
end

--------------------------------------------------------------

function MainRetroThrottleLevers:RedrawVC (hMesh, hSurf)
	local dg = self.component:DG()
	for i=1, 2 do
		local level = dg:GetMainThrusterLevel(i)
		local pos
		if level > 0 then
			pos = 150 + level*300.0
		else
			level = dg:GetRetroThrusterLevel(i)
			pos = 150 - level*150.0
		end
		if pos ~= self.sliderpos[i] then
			self.sliderpos[i] = pos
			dg:set_animation (self.component.anim_lever[i], pos/450.0)
		end
	end
	return true
end

--------------------------------------------------------------
local ctrl2d = 1
function MainRetroThrottleLevers:ProcessMouse2D (event, mx, my)
	local dg = self.component:DG()
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then -- record which slider to operate
		if     mx <  12 then
			ctrl2d = 1 -- left engine
		elseif mx >= 37 then
			ctrl2d = 2 -- right engine
		else
			ctrl2d = 3 -- both
		end
	end
	my = my - 9
	if my < 0 then
		my = 0
	elseif my > 157 then
		my = 157
	end
	dg:SetMainRetroLevel (ctrl2d, my <= 108 and (1.0-my/108.0) or 0.0,   -- main thruster level
			                    my >= 125 and ((my-125)/32.0) or 0.0)  -- retro thruster level
	return true
end

--------------------------------------------------------------
local ctrlvc = 1
local modevc = 0
local pyvc = 0
function MainRetroThrottleLevers:ProcessMouseVC (event, p)
	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then -- record which slider to operate
		if p.x < 0.3 then 
			ctrlvc = 1 -- left engine
		elseif p.x > 0.7 then
			ctrlvc = 2 -- right engine
		else
			ctrlvc = 3 -- both
		end
		modevc = 2
		pyvc = p.y
	else
		local dg = self.component:DG()
		for i = 1,2 do
			if ctrlvc == i or ctrlvc == 3 then
				local lvl = dg:GetMainThrusterLevel(i) - dg:GetRetroThrusterLevel(i)
				if lvl > 0.0 then
					modevc = 0
				elseif lvl < 0.0 then
					modevc = 1
				end
				local lmin = modevc == 0 and 0.0 or -1.0 -- prevent direct crossover from main to retro
				local lmax = modevc == 1 and 0.0 or  1.0 -- prevent direct crossover from retro to main
				lvl = math.max (lmin, math.min (lmax, lvl + 2.0*(p.y-pyvc)))
				if math.fabs (lvl) < 0.01 then
					lvl = 0.0
				end
				if lvl >= 0.0 then
					dg:SetMainRetroLevel (i, lvl, 0.0)
				else
					dg:SetMainRetroLevel (i, 0.0, -lvl)
				end
			end
		end
		pyvc = p.y
	end
	return true
end

return MainRetroThrottleLevers
