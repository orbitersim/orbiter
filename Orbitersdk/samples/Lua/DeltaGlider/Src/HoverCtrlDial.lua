-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HoverCtrlDial.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGDial1 = require("DGDial1")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP

local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height


local HoverCtrlDial = Class(DGDial1)


function HoverCtrlDial:new (_ctrl)
	DGDial1.new (self, _ctrl:DG(), 3, -50*RAD, 50*RAD)
	self.ctrl = _ctrl
end

--------------------------------------------------------------

function HoverCtrlDial:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 148
end

--------------------------------------------------------------

function HoverCtrlDial:ResetVC (hMesh)
	DGDial1.ResetVC (self, hMesh);
	local mode = self.ctrl:Mode()
	self:SetPosition (mode)
end

--------------------------------------------------------------

function HoverCtrlDial:Redraw2D (surf)
	-- constants for texture coordinates
	local texw = PANEL2D_TEXW -- texture width
	local texh = PANEL2D_TEXH -- texture height
	local tx_x0 = 1160.5      -- left edge of texture block
	local tx_y0 = texh-615.5  -- top edge of texture block
	local tx_dx = 39.0        -- texture block width
	local tx_dy = 43.0        -- texture block height
	local tu = {tx_x0/texw,(tx_x0+tx_dx)/texw,tx_x0/texw,(tx_x0+tx_dx)/texw}

	local dtu = self.ctrl:Mode()*40.0/texw
	for i=1,4 do
		self.grp.Vtx[self.vtxofs+i].tu = tu[i]+dtu
	end
	return false
end

--------------------------------------------------------------

function HoverCtrlDial:ProcessMouse2D (event, mx, my)
	local mode = self.ctrl:Mode()

	if mx < 20 then -- dial turn left
		if mode > 0 then
			self.ctrl:SetMode (mode-1)
			return true
		end
	else -- dial turn right
		if mode < 2 then
			self.ctrl:SetMode (mode+1)
			return true
		end
	end
	return false
end

--------------------------------------------------------------

function HoverCtrlDial:ProcessMouseVC (event, p)
	if DGDial1.ProcessMouseVC (self, event, p) then
		local pos = self:GetPosition()
		self.ctrl:SetMode (pos)
		return true
	end
	return false
end


return HoverCtrlDial
