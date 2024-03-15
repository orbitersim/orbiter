local PanelElement = require("PanelElement")
local DGButton3 = require("DGButton3")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP


local HUDModeButtons = Class(PanelElement)

function HUDModeButtons:new (hc)
	PanelElement.new (self, hc:DG())
	self.ctrl = hc
	self.vmode = 0
	self.btn = {
		DGButton3 (self.vessel),
		DGButton3 (self.vessel),
		DGButton3 (self.vessel),
	}
end

--------------------------------------------------------------

function HUDModeButtons:DefineAnimationsVC (axis, meshgrp, meshgrp_label, vofs, vofs_label)
	for i=1,3 do
		self.btn[i]:DefineAnimationVC (axis, meshgrp, meshgrp_label, vofs[i], vofs_label[i])
	end
end

--------------------------------------------------------------

function HUDModeButtons:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 8
end

--------------------------------------------------------------

function HUDModeButtons:ResetVC (hMesh)
	for i=1,3 do
		self.btn[i]:ResetVC (hMesh)
	end
end

--------------------------------------------------------------

function HUDModeButtons:LoadPanel2D(panelid, hPanel, viewW, viewH)
	self:SetMode(self.ctrl:GetHUDMode())
end

--------------------------------------------------------------

function HUDModeButtons:LoadVC (vcid)
	self:SetMode(self.ctrl:GetHUDMode())
end

--------------------------------------------------------------

function HUDModeButtons:Redraw2D (surf)
	-- constants for texture coordinates
	local tx_dy =  4.0       -- texture block height
	local bb_y0 = 19.5       -- top edge of button block

	local y0 = bb_y0
	local y1 = bb_y0+tx_dy

	local mode = self.ctrl:GetHUDMode()
	for i=0,2 do
		local y = (i+1 == mode) and y1 or y0
		for j=3,4 do
			self.grp.Vtx[self.vtxofs+i*4+j].y = y
		end
	end
	return false
end

--------------------------------------------------------------

function HUDModeButtons:RedrawVC (hMesh, surf)
	for i=1,3 do
		self.btn[i]:RedrawVC (hMesh, surf)
	end
	return false
end

--------------------------------------------------------------

function HUDModeButtons:ProcessMouse2D (event, mx, my)
	if math.floor(mx%29) < 20 then
		local mode = HUDMODE.NONE+math.floor(mx/29)
		if mode == HUDMODE.NONE then
			self.ctrl:ModHUDBrightness(my < 8)
		elseif bit.anyset(event, PANEL_MOUSE.LBDOWN) then
			self.ctrl:SetHUDMode (mode)
		end
	end
	return false
end

--------------------------------------------------------------

function HUDModeButtons:ProcessMouseVC (event, p)
	local ix = math.floor(p.x*126.0)
	local b = math.floor(ix/43)
	if ix-b*43 >= 40 then return false end

	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		for i=1,3 do
			self.btn[i]:SetState ((i-1)==b and DGButton3.PRESSED_FROM_OFF or DGButton3.OFF)
		end
		local mode = {HUDMODE.ORBIT,HUDMODE.SURFACE,HUDMODE.DOCKING}
		self.vmode = mode[b+1]
		self.ctrl:SetHUDMode (self.vmode)
	elseif bit.anyset(event, PANEL_MOUSE.LBUP) then
		self.btn[b+1]:SetState (DGButton3.ON)
	end
	return true
end

--------------------------------------------------------------

function HUDModeButtons:SetMode (mode)
	if mode ~= self.vmode then
		local b = mode-HUDMODE.ORBIT
		for i=1,3 do
			self.btn[i]:SetState ((i-1)==b and DGButton3.ON or DGButton3.OFF)
		end
		self.vmode = mode
	end
end

return HUDModeButtons
