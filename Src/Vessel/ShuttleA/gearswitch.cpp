// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// gearswitch.cpp
// User interface gear switch
// ==============================================================

#define STRICT 1
#include "gearswitch.h"

// ==============================================================

GearSwitch::GearSwitch (ShuttleA *v, MESHHANDLE hMesh)
: PanelSwitch1 (v, false, hMesh, 2, 12)
{
}

// --------------------------------------------------------------

int GearSwitch::GetTargetState ()
{
	return (sh->gear_status == ShuttleA::DOOR_CLOSED || sh->gear_status == ShuttleA::DOOR_CLOSING ? 2 : 0);
}

// --------------------------------------------------------------

void GearSwitch::SetTargetState (int state)
{
	sh->ActivateLandingGear (state == 2 ? ShuttleA::DOOR_CLOSING : ShuttleA::DOOR_OPENING);
}


// ==============================================================

GearIndicator::GearIndicator (ShuttleA *v, MESHHANDLE hMesh)
: PanelIndicator1 (v, hMesh, 2, 44)
{
}

// --------------------------------------------------------------

int GearIndicator::GetTargetState ()
{
	return (sh->gear_status == ShuttleA::DOOR_CLOSED ? 3 :
		    sh->gear_status == ShuttleA::DOOR_OPEN   ? 4 : 0);
}