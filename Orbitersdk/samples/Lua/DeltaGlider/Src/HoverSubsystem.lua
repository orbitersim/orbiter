-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: HoverSubsystem.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local HoverAttitudeComponent = require("HoverAttitudeComponent")
local HoverHoldComponent = require("HoverHoldComponent")
local HoverManualComponent = require("HoverManualComponent")

local HoverSubsystem = Class(DGSubsystem)


function HoverSubsystem:new (dg)
	DGSubsystem.new (self, dg)

	-- create the subsystem components
	self.attctrl = self:AddSubsystem (HoverAttitudeComponent (self))
	self.holdctrl = self:AddSubsystem (HoverHoldComponent (self))
	self.manctrl = self:AddSubsystem (HoverManualComponent (self))

	self.hoverlevel={0,0,0}
end

--------------------------------------------------------------

function HoverSubsystem:IncGroupLevel (dlvl)
	for i=1,3 do
		self.hoverlevel[i] = math.max (0.0, math.min (1.0, self.hoverlevel[i]+dlvl))
	end
end

--------------------------------------------------------------

function HoverSubsystem:ActivateHold (active)
	self.holdctrl:Activate (active)
end

--------------------------------------------------------------

function HoverSubsystem:clbkPostStep (simt, simdt, mjd)
	local hoverlevel_cur = {}

	for i=1,3 do
		hoverlevel_cur[i] = self:DG():GetHoverThrusterLevel (i)
		self.hoverlevel[i] = hoverlevel_cur[i]
	end

	DGSubsystem.clbkPostStep (self, simt, simdt, mjd)

	for i=1,3 do
		if self.hoverlevel[i] ~= hoverlevel_cur[i] then
			self:DG():SetHoverThrusterLevel (i, self.hoverlevel[i])
		end
	end
end

--------------------------------------------------------------

function HoverSubsystem:clbkReset2D (panelid, hMesh)
	if panelid ~= 0 then return end
	DGSubsystem.clbkReset2D (self, panelid, hMesh)
end

--------------------------------------------------------------

function HoverSubsystem:clbkResetVC (vcid, hMesh)
	if vcid ~= 0 then return end
	DGSubsystem.clbkResetVC (self, vcid, hMesh)
end

function HoverSubsystem:GetThrusterLevel (i)
	return self.hoverlevel[i]
end

function HoverSubsystem:SetThrusterLevel (i, lvl)
	self.hoverlevel[i] = lvl
end

return HoverSubsystem
