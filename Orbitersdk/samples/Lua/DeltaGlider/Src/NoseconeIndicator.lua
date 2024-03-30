local PanelElement = require("PanelElement")

local meshres_p0 = require("meshres_p0")
local GRP_P0 = meshres_p0.GRP
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local PANEL2D_WIDTH = 1280  -- panel width [pixel]
local PANEL2D_TEXW  = 2048  -- texture width
local PANEL2D_TEXH  = 1024  -- texture height
local INSTR3D_TEXW  =  512
local INSTR3D_TEXH  = 1024
local VC_NCONE_INDICATOR_vofs = 19

local NoseconeIndicator = Class(PanelElement)

function NoseconeIndicator:new (comp)
	PanelElement.new(self, comp:DG())
	self.component = comp
	self.tofs = oapi.rand()
	self.light = true
end

--------------------------------------------------------------

function NoseconeIndicator:Reset2D (panelid, hMesh)
	self.grp = oapi.mesh_group (hMesh, GRP_P0.INSTRUMENTS_ABOVE)
	self.vtxofs = 88
end

--------------------------------------------------------------

function NoseconeIndicator:ResetVC (hMesh)
	self.light = true
end

--------------------------------------------------------------

function NoseconeIndicator:Redraw2D (surf)
	local texw = PANEL2D_TEXW -- texture width

	local xofs
	
	local int, frac = math.modf(oapi.get_simtime()+self.tofs)
	local blink = frac < 0.5

	if self.component:NconeState():IsClosed() then
		xofs = 1014
	elseif self.component:NconeState():IsOpen() then
		xofs = 1027
    elseif blink then
		xofs = 1040
	else
		xofs = 1014
	end
	
	for i=0,3 do
		for j=0,2 do
			self.grp.Vtx[self.vtxofs+i*3+j+1].tu = (xofs + (j%2)*12)/texw
		end
	end

	return false
end

--------------------------------------------------------------

function NoseconeIndicator:RedrawVC (hMesh, surf)
	if not hMesh then return false end

	local showlights
	
	if self.component:NconeState():IsClosed() then
		showlights = false
	elseif self.component:NconeState():IsOpen() then
		showlights = true
	else
		local int, frac = math.modf(oapi.get_simtime()+self.tofs)
		showlights = frac < 0.5
	end

	if showlights ~= self.light then
		local ges = {}
		local vtxofs = VC_NCONE_INDICATOR_vofs
		local vidx = oapi.create_indexarray({vtxofs, vtxofs+1})
		local vtx = oapi.create_ntvertexarray(2)
		for i=1,2 do
			vtx[i].tv = showlights and 0.3003 or 0.2427
		end
		ges.flags = GRPEDIT.VTXTEXV
		ges.Vtx = vtx
		ges.vIdx = vidx
		oapi.edit_meshgroup (hMesh, GRP_VC.VC4_LIT, ges)
		self.light = showlights
	end
	return false
end

return NoseconeIndicator
