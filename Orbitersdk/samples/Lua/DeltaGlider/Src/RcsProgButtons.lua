-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: RcsProgButtons.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = require("PanelElement")
local DGButton3 = require("DGButton3")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP

local PANEL2D_TEXH  = 1024  -- texture height

local RcsProgButtons = Class(PanelElement)

function RcsProgButtons:new (_subsys)
	PanelElement.new (self, _subsys:DG())
	self.subsys = _subsys

	self.btn = {
		DGButton3 (self.subsys:DG()),
		DGButton3 (self.subsys:DG()),
		DGButton3 (self.subsys:DG()),
		DGButton3 (self.subsys:DG()),
		DGButton3 (self.subsys:DG()),
		DGButton3 (self.subsys:DG()),
	}
end

--------------------------------------------------------------

function RcsProgButtons:SetMode (mode, active)
	local modebtn = {1,6,3,2,5,4}
	local b = modebtn[mode]
	if active then
		if self.btn[b]:GetState () == DGButton3.OFF then
			self.btn[b]:SetState (DGButton3.ON)
		end
	else
		self.btn[b]:SetState (DGButton3.OFF)
	end
end

--------------------------------------------------------------

function RcsProgButtons:DefineAnimationsVC (axis, meshgrp, meshgrp_label, vofs, vofs_label)
	for i=1,6 do
		self.btn[i]:DefineAnimationVC (axis, meshgrp, meshgrp_label, vofs[i], vofs_label[i])
	end
end

--------------------------------------------------------------

function RcsProgButtons:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 20
end

--------------------------------------------------------------

function RcsProgButtons:ResetVC (hMesh)
	for i=1,6 do
		self.btn[i]:ResetVC (hMesh)
	end
end

--------------------------------------------------------------

function RcsProgButtons:Redraw2D (surf)
	-- constants for texture coordinates
	local texh = PANEL2D_TEXH  -- texture height
	local tx_y0 = texh-597.0   -- top edge of texture block
	local tx_dy = 24.0         -- texture block height
	local tv0_active = (tx_y0)/texh
	local tv1_active = (tx_y0+tx_dy)/texh
	local tv0_idle = (tx_y0+tx_dy+0.5)/texh
	local tv1_idle = (tx_y0+tx_dy+0.5)/texh

	local tv0, tv1
	for i = NAVMODE.KILLROT, NAVMODE.ANTINORMAL do
		if self.subsys:DG():get_navmode (i) then
			tv0 = tv0_active
			tv1 = tv1_active
		else
			tv0 = tv0_idle
			tv1 = tv1_idle
		end
		local vofs = self.vtxofs+(i-NAVMODE.KILLROT)*4+1
		self.grp.Vtx[vofs+0].tv = tv0
		self.grp.Vtx[vofs+1].tv = tv0
		self.grp.Vtx[vofs+2].tv = tv1
		self.grp.Vtx[vofs+3].tv = tv1
	end
		
	return false
end

--------------------------------------------------------------

function RcsProgButtons:RedrawVC (hMesh, surf)
	for i=1,6 do
		self.btn[i]:RedrawVC (hMesh, surf)
	end
	return false
end

--------------------------------------------------------------

function RcsProgButtons:ProcessMouse2D (event, mx, my)
	local navmode = {
		{nil,NAVMODE.PROGRADE,NAVMODE.NORMAL,nil},
		{NAVMODE.KILLROT,NAVMODE.RETROGRADE,NAVMODE.ANTINORMAL,NAVMODE.HLEVEL}
	}
	local mode = navmode[math.floor(my/24)+1][math.floor(mx/37)+1]
	if mode then
		self.subsys:DG():toggle_navmode (mode)
	end
	return true-- mode ~= nil
end

--------------------------------------------------------------

function RcsProgButtons:ProcessMouseVC (event, p)
	local modemap = {{1,4,6,2},{0,3,5,0}}
	local btnmode = {1,4,3,6,5,2}
	local modebtn = {1,6,3,2,5,4}
	local ix = p.x*169.0
	local iy = p.y*63
	local br = math.floor(ix/43)+1
	local bc = math.floor(iy/33)+1
	if ix-br*43 >= 40 then return false end
	if iy-bc*33 >= 30 then return false end

	local mode = modemap[bc][br]
	if mode == 0 then return false end

	local b = modebtn[mode]

	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		for i=1,6 do
			if i==b then
				self.btn[i]:SetState (self.btn[i]:GetState() == DGButton3.OFF and DGButton3.PRESSED_FROM_OFF or DGButton3.PRESSED_FROM_ON)
			else
				local ison = self.subsys:DG():get_navmode (btnmode[i])
				self.btn[i]:SetState (ison and DGButton3.ON or DGButton3.OFF)
			end
		end
		self.subsys:DG():toggle_navmode (mode)
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		self.btn[b]:SetState (self.btn[b]:GetState() == DGButton3.PRESSED_FROM_OFF and DGButton3.ON or DGButton3.OFF)
	end
	return true
end

return RcsProgButtons
