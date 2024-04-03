-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: AAPSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local AAP = require("AAP")

local AAPSubsystem = Class(DGSubsystem)

function AAPSubsystem:new(...)
	DGSubsystem.new(self, ...)
	self.ELID_AAP, self.aap = self:AddElement (AAP(self))
end

function AAPSubsystem:clbkLoadPanel2D(panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local xofs = 90
	local yofs = 149

	self:DG():register_panelarea(hPanel, self.ELID_AAP, _R(xofs,yofs,xofs+65,yofs+124), PANEL_REDRAW.MOUSE, bit.bor(PANEL_MOUSE.LBDOWN, PANEL_MOUSE.LBPRESSED, PANEL_MOUSE.LBUP), 0, self.aap)
	return true
end

function AAPSubsystem:clbkSaveState(scn)
	self.aap:WriteScenario(scn)
end

function AAPSubsystem:clbkParseScenarioLine(line)
	if line:sub(1,3) == "AAP" then
		self.aap:SetState(line)
		return true
	end
	return false
end


function AAPSubsystem:AttachHSI(_hsi)
	self.aap:AttachHSI(_hsi)
end

return AAPSubsystem
