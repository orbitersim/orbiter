-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
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
	oapi.dbg_out("FIXME: PanelElement:DispStr(dist, precision)")
	return tostring(dist)
	--[[
	static char strbuf[32];
	double absd = fabs (dist);
	if (absd < 1e4) {
		if      (absd < 1e3)  sprintf (strbuf, "% 6.*f ", precision-3, dist);
		else                  sprintf (strbuf, "% 0.*fk", precision-1, dist*1e-3);
	} else if (absd < 1e7) {
		if      (absd < 1e5)  sprintf (strbuf, "% 0.*fk", precision-2, dist*1e-3);
		else if (absd < 1e6)  sprintf (strbuf, "% 0.*fk", precision-3, dist*1e-3);
		else                  sprintf (strbuf, "% 0.*fM", precision-1, dist*1e-6);
	} else if (absd < 1e10) {
		if      (absd < 1e8)  sprintf (strbuf, "% 0.*fM", precision-2, dist*1e-6);
		else if (absd < 1e9)  sprintf (strbuf, "% 0.*fM", precision-3, dist*1e-6);
		else                  sprintf (strbuf, "% 0.*fG", precision-1, dist*1e-9);
	} else {
		if      (absd < 1e11) sprintf (strbuf, "% 0.*fG", precision-2, dist*1e-9);
		else if (absd < 1e12) sprintf (strbuf, "% 0.*fG", precision-3, dist*1e-9);
		else                  strcpy (strbuf, "--.--");
	}
	return strbuf;
	]]
end

return PanelElement
