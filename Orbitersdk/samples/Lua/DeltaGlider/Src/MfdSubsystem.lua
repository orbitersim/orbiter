-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: MfdSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local MfdButtonRow = require("MfdButtonRow")
local MfdButtonCol = require("MfdButtonCol")

local MfdSubsystem = Class(DGSubsystem)


function MfdSubsystem:new (v, mfdident)
	DGSubsystem.new(self, v)
	self.mfdid = mfdident
	self.ELID_BTNROW, self.btnrow = self:AddElement (MfdButtonRow (self))
	self.ELID_BTNCOL = {}
	self.btncol = {}
	self.ELID_BTNCOL[1], self.btncol[1] = self:AddElement (MfdButtonCol (self, 0))
	self.ELID_BTNCOL[2], self.btncol[2] = self:AddElement (MfdButtonCol (self, 1))
end

--------------------------------------------------------------

function MfdSubsystem:ModeChanged ()
	for i=1,2 do
		self:DG():trigger_redrawarea (0, 0, self.ELID_BTNCOL[i])
	end
end

--------------------------------------------------------------

function MfdSubsystem:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local xofs = self.mfdid == MFDID.LEFT and 173 or 736
	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_BTNROW, _R( 51+xofs,359,321+xofs,377), PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.ONREPLAY, panel2dtex, self.btnrow) -- bottom button row
	self:DG():register_panelarea (hPanel, self.ELID_BTNCOL[1], _R(    xofs,100, 25+xofs,323), PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBPRESSED+PANEL_MOUSE.ONREPLAY, panel2dtex, self.btncol[1]) -- left button column
	self:DG():register_panelarea (hPanel, self.ELID_BTNCOL[2], _R(348+xofs,100,373+xofs,323), PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBPRESSED+PANEL_MOUSE.ONREPLAY, panel2dtex, self.btncol[2]) -- right button column

	return true
end

 --------------------------------------------------------------

function MfdSubsystem:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	self.vctex = oapi.get_texturehandle (self:DG().vcmesh_tpl, 20)
	local xofs = self.mfdid == MFDID.LEFT and -0.2684 or 0.0616

	oapi.VC_register_area (self.ELID_BTNROW, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP+PANEL_MOUSE.LBPRESSED+PANEL_MOUSE.ONREPLAY)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_BTNROW, _V(0.0840+xofs, 1.0745, 7.2238), _V(0.1228+xofs, 1.0745, 7.2238), _V(0.0840+xofs, 1.0587, 7.2180), _V(0.1228+xofs, 1.0587, 7.2180))

	oapi.VC_register_area (self.ELID_BTNCOL[1], PANEL_REDRAW.MOUSE+PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP+PANEL_MOUSE.LBPRESSED+PANEL_MOUSE.ONREPLAY)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_BTNCOL[1], _V(0+xofs, 1.2155, 7.2751), _V(0.0168+xofs, 1.2155, 7.2751), _V(0+xofs, 1.0963, 7.2317), _V(0.0168+xofs, 1.0963, 7.2317))

	oapi.VC_register_area (self.ELID_BTNCOL[2], PANEL_REDRAW.MOUSE+PANEL_REDRAW.USER, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP+PANEL_MOUSE.LBPRESSED+PANEL_MOUSE.ONREPLAY)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_BTNCOL[2], _V(0.1900+xofs, 1.2155, 7.2751), _V(0.2068+xofs, 1.2155, 7.2751), _V(0.1900+xofs, 1.0963, 7.2317), _V(0.2068+xofs, 1.0963, 7.2317))

	return true
end



function MfdSubsystem:MfdId() 
	return self.mfdid
end

function MfdSubsystem:VcTex()
	return self.vctex
end

return MfdSubsystem
