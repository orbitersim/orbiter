-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: RetroCoverSwitch.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSwitch1 = require("DGSwitch1")

local RetroCoverSwitch = Class(DGSwitch1)

function RetroCoverSwitch:new (comp)
	DGSwitch1.new(self, comp:DG(), DGSwitch1.SPRING)
	self.component = comp
end

--------------------------------------------------------------

function RetroCoverSwitch:ProcessMouse2D (event, mx, my)
	if DGSwitch1.ProcessMouse2D (self, event, mx, my) then
		local state = self:GetState()
		if state == DGSwitch1.UP then
			self.component:CloseRetroCover()
		elseif state == DGSwitch1.DOWN then
			self.component:OpenRetroCover()
		end
		return true
	end
	return false
end

--------------------------------------------------------------

function RetroCoverSwitch:ProcessMouseVC (event, p)
	if DGSwitch1.ProcessMouseVC (self, event, p) then
		local state = self:GetState()
		if state == DGSwitch1.UP then
			self.component:CloseRetroCover()
		elseif state == DGSwitch1.DOWN then
			self.component:OpenRetroCover()
		end
		return true
	end
	return false
end

return RetroCoverSwitch
