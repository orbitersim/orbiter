// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// dockcvrswitch.cpp
// User interface dockingport cover switch
// ==============================================================

#define STRICT 1
#include "dockcvrswitch.h"

// ==============================================================

DockCoverSwitch::DockCoverSwitch (ShuttleA *v, MESHHANDLE hMesh)
: PanelSwitch1 (v, false, hMesh, 2, 8)
{
}

// --------------------------------------------------------------

int DockCoverSwitch::GetTargetState ()
{
	return (sh->dock_status == ShuttleA::DOOR_CLOSED || sh->dock_status == ShuttleA::DOOR_CLOSING ? 2 : 0);
}

// --------------------------------------------------------------

void DockCoverSwitch::SetTargetState (int state)
{
	sh->ActivateDockingPort (state == 2 ? ShuttleA::DOOR_CLOSING : ShuttleA::DOOR_OPENING);
}


// ==============================================================

DockCoverIndicator::DockCoverIndicator (ShuttleA *v, MESHHANDLE hMesh)
: PanelIndicator1 (v, hMesh, 2, 48)
{
}

// --------------------------------------------------------------

int DockCoverIndicator::GetTargetState ()
{
	return (sh->dock_status == ShuttleA::DOOR_CLOSED ? 6 :
		    sh->dock_status == ShuttleA::DOOR_OPEN   ? 5 : 0);
}