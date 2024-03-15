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

local PANEL2D_TEXH  = 1024  -- texture height

local WheelbrakeLever = Class(PanelElement)

function WheelbrakeLever:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp

	self.isdown={false,false}
end

--------------------------------------------------------------

function WheelbrakeLever:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 104
end

--------------------------------------------------------------

function WheelbrakeLever:Redraw2D (surf)
	local texh = PANEL2D_TEXH -- texture height
	local tx_y0 = texh-650.0
	local tx_dy = 77.0

	for i = 1,2 do
		local lvl = self.vessel:get_wheelbrakelevel (i)
		local down = lvl > 0.5
		if down ~= self.isdown[i] then
			local tv = (down and (tx_y0+tx_dy) or tx_y0)/texh
			for j = 3, 4 do
				self.grp.Vtx[self.vtxofs+(i-1)*4+j].tv = tv
			end
			self.isdown[i] = down
		end
	end
	return false
end

--------------------------------------------------------------

function WheelbrakeLever:ProcessMouse2D (event, mx, my)
	local which = 0
	if mx < 15 then
		which = 11
	elseif mx > 37 then
		which = 2
	end

	local press = event == PANEL_MOUSE.LBDOWN
	self.vessel:set_wheelbrakelevel (press and 1.0 or 0.0, which)
	return false
end

return WheelbrakeLever
