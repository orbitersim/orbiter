-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: PanelElement.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local PanelElement = Class()

function PanelElement:new(v)
	self.vessel = v
	self.grp = 0
	self.vtxofs = 0
	self.mesh = 0
	self.gidx = 0
end

--------------------------------------------------------------

function PanelElement:Reset2D(panelid) end
function PanelElement:Reset2D(panelid, hMesh) end
function PanelElement:ResetVC(hMesh) end
function PanelElement:LoadVC(vcid) end
function PanelElement:LoadPanel2D(panelid, hPanel, viewW, viewH) end

function PanelElement:Redraw2D(surf)
	return false
end

function PanelElement:RedrawVC(hMesh, surf)
	return false
end

function PanelElement:ProcessMouse2D(event, mx, my)
	return false
end

function PanelElement:ProcessMouseVC(event, p)
	return false
end

--------------------------------------------------------------

function PanelElement:AddGeometry(hMesh, grpidx, vtx, idx)
	self.mesh = hMesh
	self.gidx = grpidx
	self.grp  = oapi.mesh_group(hMesh, grpidx)
	self.vtxofs = #self.grp.Vtx
	oapi.add_meshgroupblock(hMesh, grpidx, vtx, idx)
end

--------------------------------------------------------------

function PanelElement:SelectGeometry(hMesh, grpidx, vofs)
	self.mesh = hMesh
	self.gidx = grpidx
	self.grp  = oapi.mesh_group(hMesh, grpidx)
	self.vtxofs = vofs
end

--------------------------------------------------------------

function PanelElement:DispStr(dist, precision)
	precision = precision or 4
	return oapi.formatvalue(dist, precision)
end

return PanelElement
