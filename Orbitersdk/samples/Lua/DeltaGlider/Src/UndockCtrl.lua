local DGSubsystem = require("DGSubsystem")
local AnimState2 = require("AnimState2")
local UndockLever = require("UndockLever")

local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

-- UNDOCKLEVER (VC): mouse catch area
local VC_UNDOCKLEVER_mousearea = {_V(0.30450,0.91660,7.13284),_V(0.33950,0.91660,7.13284),_V(0.30450,1.01412,7.20278),_V(0.33950,1.01412,7.20278)}

-- UNDOCKLEVER (VC): rotation reference
local VC_UNDOCKLEVER_ref = _V(0.32200,0.92110,7.15145)

-- UNDOCKLEVER (VC): rotation axis
local VC_UNDOCKLEVER_axis = _V(1.00000,0.00000,0.00000)


local UndockCtrl = Class(DGSubsystem)

function UndockCtrl:new (_subsys)
	DGSubsystem.new(self, _subsys)

	self.undock_state = AnimState2 (10.0)
	self.ELID_LEVER, self.lever = self:AddElement (UndockLever (self))

	-- Undock lever animation
	local UndockLeverTransform = MGROUP_ROTATE (1, {GRP_VC.UNDOCK_LEVER}, VC_UNDOCKLEVER_ref, VC_UNDOCKLEVER_axis, -90*RAD)
	self.anim_undocklever = self:DG():create_animation (0)
	self:DG():add_animationcomponent (self.anim_undocklever, 0, 1, UndockLeverTransform)
end

--------------------------------------------------------------

function UndockCtrl:PullLever ()
	self.undock_state:Open()
	self:DG():undock(0)
end

--------------------------------------------------------------

function UndockCtrl:ReleaseLever ()
	self.undock_state:Close()
end

--------------------------------------------------------------

function UndockCtrl:clbkPostStep (simt, simdt, mjd)
	-- animate undock lever
	if self.undock_state:Process (simdt) then
		self:DG():set_animation (self.anim_undocklever, self.undock_state:State())
	end
end

--------------------------------------------------------------

function UndockCtrl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)
	self:DG():register_panelarea (hPanel, self.ELID_LEVER, _R(1151,369,1193,450), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP, panel2dtex, self.lever)

	return true
end

--------------------------------------------------------------

function UndockCtrl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Undock lever
	oapi.VC_register_area (self.ELID_LEVER, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN+PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_LEVER, VC_UNDOCKLEVER_mousearea)

	return true
end

return UndockCtrl
