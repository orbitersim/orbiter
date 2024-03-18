-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local MainRetroThrottle = require("MainRetroThrottle")
local GimbalControl = require("GimbalControl")
local RetroCoverControl = require("RetroCoverControl")

local MainRetroSubsystem = Class(DGSubsystem)

function MainRetroSubsystem:new (v)
	DGSubsystem.new(self, v)

	-- create component instances
	self.throttle = self:AddSubsystem (MainRetroThrottle (self))
	self.gimbalctrl = self:AddSubsystem (GimbalControl (self))
	self.retrocover = self:AddSubsystem (RetroCoverControl (self))
end

--------------------------------------------------------------

function MainRetroSubsystem:OpenRetroCover ()
	self.retrocover:OpenRetroCover()
end

--------------------------------------------------------------

function MainRetroSubsystem:CloseRetroCover ()
	self.retrocover:CloseRetroCover()
end

--------------------------------------------------------------

function MainRetroSubsystem:RetroCoverState()
	return self.retrocover:State()
end

--------------------------------------------------------------

function MainRetroSubsystem:clbkReset2D (panelid, hMesh)
	if panelid ~= 0 then return end
	DGSubsystem.clbkReset2D (self, panelid, hMesh)
end

--------------------------------------------------------------

function MainRetroSubsystem:clbkResetVC (vcid, hMesh)
	if vcid ~= 0 then return end
	DGSubsystem.clbkResetVC (self, vcid, hMesh)
end

return MainRetroSubsystem
