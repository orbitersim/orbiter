-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HoverSubsystemComponent.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")

local HoverSubsystemComponent = Class(DGSubsystem)

function HoverSubsystemComponent:new (_subsys)
	DGSubsystem.new(self, _subsys)
end

function HoverSubsystemComponent:HoverSubsys()
	return self:Parent()
end

return HoverSubsystemComponent
