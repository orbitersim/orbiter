-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local Subsystem = require("Subsystem")

local DGSubsystem = Class(Subsystem)

function DGSubsystem:new(subsys)
	Subsystem.new(self, subsys)
end

function DGSubsystem:DG()
	return self:Vessel()
end

return DGSubsystem
