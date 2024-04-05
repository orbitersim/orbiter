-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: GearSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local GearControl = require("GearControl")
local Wheelbrake = require("Wheelbrake")

local GearSubsystem = Class(DGSubsystem)

function GearSubsystem:new (v)
	DGSubsystem.new (self, v)
	-- create component instances
	self.gearctrl = self:AddSubsystem (GearControl (self))
	self.wheelbrake = self:AddSubsystem (Wheelbrake (self))
end

--------------------------------------------------------------

function GearSubsystem:LowerGear ()
	self.gearctrl:LowerGear ()
end

--------------------------------------------------------------

function GearSubsystem:RaiseGear ()
	self.gearctrl:RaiseGear ()
end

--------------------------------------------------------------

function GearSubsystem:GearState()
	return self.gearctrl:GearState()
end

return GearSubsystem
