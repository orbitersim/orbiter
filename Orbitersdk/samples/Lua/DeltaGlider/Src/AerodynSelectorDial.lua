-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: AerodynSelectorDial.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGDial1 = require("DGDial1")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP

local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height

local AerodynSelectorDial = Class(DGDial1)

function AerodynSelectorDial:new (comp)
	DGDial1.new(self, comp:DG(), 3, -50*RAD, 50*RAD)
	self.component = comp
end

--------------------------------------------------------------

function AerodynSelectorDial:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 4
end

--------------------------------------------------------------

function AerodynSelectorDial:ResetVC (hMesh)
	DGDial1.ResetVC (self, hMesh)
	local mode = self.vessel:get_adcmode()
	self:SetPosition (mode == 0 and 0 or (mode == 7 and 1 or 2))
end

--------------------------------------------------------------

function AerodynSelectorDial:Redraw2D (surf)
	-- constants for texture coordinates
	local texw = PANEL2D_TEXW       -- texture width
	local texh = PANEL2D_TEXH       -- texture height
	local tx_x0 = 1160.5            -- left edge of texture block
	local tx_y0 = texh-615.5        -- top edge of texture block
	local tx_dx = 39.0              -- texture block width
	local tx_dy = 43.0              -- texture block height
	local tu = {tx_x0/texw,(tx_x0+tx_dx)/texw,tx_x0/texw,(tx_x0+tx_dx)/texw}

	local dtu = math.min(self.vessel:get_adcmode(),2)*40.0/texw
	for i=1,4 do
		self.grp.Vtx[self.vtxofs+i].tu = tu[i]+dtu
	end
	return false
end

--------------------------------------------------------------

function AerodynSelectorDial:RedrawVC (hMesh, surf)
	local mode = self.component:GetMode()
	self:SetPosition(mode == 0 and 0 or (mode == 7 and 1 or 2))
	return DGDial1.RedrawVC (self, hMesh, surf)
end

--------------------------------------------------------------

function AerodynSelectorDial:ProcessMouse2D (event, mx, my)
	if mx < 20 then
		return self.component:DecMode()
	else
		return self.component:IncMode()
	end
end

--------------------------------------------------------------

function AerodynSelectorDial:ProcessMouseVC (event, p)
	if DGDial1.ProcessMouseVC (self, event, p) then
		local pos = self:GetPosition()
		self.component:SetMode (pos == 0 and 0 or (pos == 1 and 7 or 1))
		return true
	end
	return false
end

return AerodynSelectorDial
