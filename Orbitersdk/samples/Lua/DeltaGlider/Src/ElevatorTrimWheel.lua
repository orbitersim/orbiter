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
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local VC_ETRIMSCALE_vofs = 12

-- ETRIMSCALE (VC): rotation reference
local VC_ETRIMSCALE_ref = {_V(-0.31250,0.92067,7.13576),_V(-0.31950,0.92310,7.13751),_V(-0.31250,0.92554,7.13925)}

-- ETRIMSCALE (VC): rotation axis
local VC_ETRIMSCALE_axis = _V(0.00000,0.81262,0.58280)


local ElevatorTrimWheel = Class(PanelElement)

function ElevatorTrimWheel:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
end

--------------------------------------------------------------

function ElevatorTrimWheel:Reset2D (panelid, hMesh)
	self.trimpos2D = 0.0
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 60
end

--------------------------------------------------------------

function ElevatorTrimWheel:ResetVC (hMesh)
	self.trimposVC = 0.0
end

--------------------------------------------------------------

function ElevatorTrimWheel:Redraw2D (surf)
	-- constants for panel coordinates
	local bb_y0 =  325.5
	local bb_dy =    7.0

	local level = self.vessel:get_adclevel (AIRCTRL.ELEVATORTRIM)
	if level ~= self.trimpos2D then
		local yp = {bb_y0, bb_y0, bb_y0+bb_dy, bb_y0+bb_dy}
		local yshift = level*24.0
		for i=1,4 do
			self.grp.Vtx[self.vtxofs+i].y = yp[i]+yshift
		end
		self.trimpos2D = level
	end
	return false
end

--------------------------------------------------------------

function ElevatorTrimWheel:RedrawVC (hMesh, surf)
	if not hMesh then return false end

	local level = self.vessel:get_adclevel (AIRCTRL.ELEVATORTRIM)
	if level ~= self.trimposVC then
		local vidx = oapi.create_indexarray({VC_ETRIMSCALE_vofs,VC_ETRIMSCALE_vofs+1,VC_ETRIMSCALE_vofs+2})
		local vtx = oapi.create_ntvertexarray(3)
		local ges = {}
		ges.flags = GRPEDIT.VTXCRDY+GRPEDIT.VTXCRDZ
		ges.Vtx = vtx
		ges.vIdx = vidx

		local tilt = math.atan(VC_ETRIMSCALE_axis.z/VC_ETRIMSCALE_axis.y)
		local y0 = {VC_ETRIMSCALE_ref[1].y,VC_ETRIMSCALE_ref[2].y,VC_ETRIMSCALE_ref[3].y}
		local z0 = {VC_ETRIMSCALE_ref[1].z,VC_ETRIMSCALE_ref[2].z,VC_ETRIMSCALE_ref[3].z}
		local range = 0.032
		local dy = -range*math.cos(tilt)
		local dz = -range*math.sin(tilt)
		for i=1,3 do
			vtx[i].y = y0[i] + level*dy
			vtx[i].z = z0[i] + level*dz
		end
		oapi.edit_meshgroup (hMesh, GRP_VC.VC4_LIT, ges)

		local int, frac = math.modf((1-level)*20)
		self.vessel:set_animation (self.component.anim_vc_trimwheel, frac)

		self.trimposVC = level
	end
	return false
end

--------------------------------------------------------------

function ElevatorTrimWheel:ProcessMouse2D (event, mx, my)
	local tgtlvl = self.vessel:get_adclevel (AIRCTRL.ELEVATORTRIM)
	tgtlvl = tgtlvl + oapi.get_simstep() * (my < 30 and -0.2 or 0.2)
	tgtlvl = math.max (-1.0, math.min (1.0, tgtlvl))
	self.vessel:set_adclevel (AIRCTRL.ELEVATORTRIM, tgtlvl)
	return true
end

--------------------------------------------------------------

function ElevatorTrimWheel:ProcessMouseVC (event, p)
	local dtrim = oapi.get_simstep() * (p.y < 0.5 and 0.2 or -0.2)
	local trim0 = self.vessel:get_adclevel (AIRCTRL.ELEVATORTRIM)
	local trim1 = math.max(-1.0, math.min(1.0, trim0+dtrim))

	if trim0 ~= trim1 then
		self.vessel:set_adclevel (AIRCTRL.ELEVATORTRIM, trim1)
		return true
	end
	return false
end

return ElevatorTrimWheel
