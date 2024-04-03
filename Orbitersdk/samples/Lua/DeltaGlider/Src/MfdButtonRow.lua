-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: MfdButtonRow.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local MfdButtonGrp = require("MfdButtonGrp")
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP

local MfdButtonRow = Class(MfdButtonGrp)

function MfdButtonRow:new (_subsys)
	MfdButtonGrp.new(self, _subsys, 2)
	self.curbtn = -1
end

--------------------------------------------------------------

function MfdButtonRow:RedrawVC (hMesh, surf)
	local grpid = {GRP_VC.LMFD_BBUTTONS, GRP_VC.RMFD_BBUTTONS}
	if self.pending_action ~= 0 then
		self:PushButtonVC (hMesh, grpid[self.subsys:MfdId()+1], self.pending_btn, self.pending_action==1)
		self.pending_action = 0
	end
	return false
end

--------------------------------------------------------------

function MfdButtonRow:ProcessMouse2D (event, mx, my)
	local proc = false
	if mx < 26 then
		oapi.toggle_mfdon (self.subsys:MfdId())
		proc = true
	elseif mx >= 214 and mx < 240 then
		oapi.send_mfdkey (self.subsys:MfdId(), OAPI_KEY.F1)
		proc = true
	elseif mx > 244 then
		oapi.send_mfdkey (self.subsys:MfdId(), OAPI_KEY.GRAVE)
		proc = true
	end
	return proc
end

--------------------------------------------------------------

function MfdButtonRow:ProcessMouseVC (event, p)
	local proc = false
	local pushed = false
	local anim_btn = -1

	if bit.anyset(event, PANEL_MOUSE.LBDOWN) then
		local int, frac = math.modf(p.x*7.0/4.0)

		if frac < 0.75 then
			anim_btn = int
			self.curbtn = anim_btn
			pushed = true
			if self.curbtn == 0 then
				oapi.send_mfdkey (self.subsys:MfdId(), OAPI_KEY.F1)
				proc = true
			elseif self.curbtn == 1 then
				oapi.send_mfdkey (self.subsys:MfdId(), OAPI_KEY.GRAVE)
				proc = true
			end
		end
	elseif self.curbtn >= 0 then
		if bit.anyset(event, PANEL_MOUSE.LBUP) then
			anim_btn = self.curbtn
			pushed = false
			self.curbtn = -1
		end
	end
	-- animate button pushes
	if anim_btn >= 0 then
		self.pending_btn = anim_btn
		self.pending_action = pushed and 1 or 2
		return true
	end

	return false
end

return MfdButtonRow
