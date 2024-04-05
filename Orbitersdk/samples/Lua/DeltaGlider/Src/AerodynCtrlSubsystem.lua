-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: AerodynCtrlSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local AerodynSelector = require("AerodynSelector")
local Airbrake = require("Airbrake")
local ElevatorTrim = require("ElevatorTrim")

local AerodynCtrlSubsystem = Class(DGSubsystem)


function AerodynCtrlSubsystem:new (v)
	DGSubsystem.new (self, v)
	-- create component instances
	self.selector = self:AddSubsystem (AerodynSelector (self))
	self.airbrake = self:AddSubsystem (Airbrake (self))
	self.elevtrim = self:AddSubsystem (ElevatorTrim (self))
end

--------------------------------------------------------------

function AerodynCtrlSubsystem:SetMode (mode)
	self.selector:SetMode (mode)
end

--------------------------------------------------------------

function AerodynCtrlSubsystem:ExtendAirbrake ()
	self.airbrake:Extend()
end

--------------------------------------------------------------

function AerodynCtrlSubsystem:RetractAirbrake ()
	self.airbrake:Retract()
end

--------------------------------------------------------------

function AerodynCtrlSubsystem:AirbrakeState()
	return self.airbrake:State()
end


function AerodynCtrlSubsystem:AirbrakeSubsys()
	return self.airbrake
end

return AerodynCtrlSubsystem
