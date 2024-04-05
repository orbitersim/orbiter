-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: LightCtrlSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local InstrumentLight = require("InstrumentLight")
local CockpitLight = require("CockpitLight")
local LandDockLight = require("LandDockLight")
local StrobeLight = require("StrobeLight")
local NavLight = require("NavLight")

local LightCtrlSubsystem = Class(DGSubsystem)

function LightCtrlSubsystem:new (v)
	DGSubsystem.new (self, v)

	-- create component instances
	self.instrlight = self:AddSubsystem (InstrumentLight (self))
	self.cockpitlight = self:AddSubsystem (CockpitLight (self))
	self.landdocklight = self:AddSubsystem (LandDockLight (self))
	self.strobelight = self:AddSubsystem (StrobeLight (self))
	self.navlight = self:AddSubsystem (NavLight (self))
end

return LightCtrlSubsystem
