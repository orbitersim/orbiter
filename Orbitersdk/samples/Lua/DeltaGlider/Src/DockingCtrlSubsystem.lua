local DGSubsystem = require("DGSubsystem")
local NoseconeCtrl = require("NoseconeCtrl")
local UndockCtrl = require("UndockCtrl")
local EscapeLadderCtrl = require("EscapeLadderCtrl")
local DocksealCtrl = require("DocksealCtrl")

local DockingCtrlSubsystem = Class(DGSubsystem)

function DockingCtrlSubsystem:new (v)
	DGSubsystem.new (self, v)

	-- create component instances
	self.noseconectrl = self:AddSubsystem (NoseconeCtrl (self))
	self.undockctrl = self:AddSubsystem (UndockCtrl (self))
	self.eladderctrl = self:AddSubsystem (EscapeLadderCtrl (self))
	self.dsealctrl = self:AddSubsystem (DocksealCtrl (self))
end

--------------------------------------------------------------

function DockingCtrlSubsystem:NconeState()
	return self.noseconectrl:NconeState()
end

--------------------------------------------------------------

function DockingCtrlSubsystem:OpenNcone ()
	self.noseconectrl:OpenNcone()
end

--------------------------------------------------------------

function DockingCtrlSubsystem:CloseNcone ()
	self.noseconectrl:CloseNcone()
end

--------------------------------------------------------------

function DockingCtrlSubsystem:ExtendLadder ()
	self.eladderctrl:ExtendLadder()
end

--------------------------------------------------------------

function DockingCtrlSubsystem:RetractLadder ()
	self.eladderctrl:RetractLadder()
end

--------------------------------------------------------------

function DockingCtrlSubsystem:LadderState ()
	return self.eladderctrl:State()
end

--------------------------------------------------------------

function DockingCtrlSubsystem:clbkDockEvent (dock, mate)
	self.dsealctrl:SetDockStatus (mate ~= nil)
end

return DockingCtrlSubsystem
