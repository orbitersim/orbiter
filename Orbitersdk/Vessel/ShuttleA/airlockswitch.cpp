// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// airlockswitch.cpp
// User interface airlock switches
// ==============================================================

#define STRICT 1
#include "airlockswitch.h"

// ==============================================================

AirlockSwitch::AirlockSwitch (ShuttleA *v, int which, MESHHANDLE hMesh)
: PanelSwitch1 (v, false, hMesh, 2, 4*which), lockid(which)
{
}

// --------------------------------------------------------------

int AirlockSwitch::GetTargetState ()
{
	return (sh->lock_status[lockid] == ShuttleA::DOOR_CLOSED || sh->lock_status[lockid] == ShuttleA::DOOR_CLOSING ? 2 : 0);
}

// --------------------------------------------------------------

void AirlockSwitch::SetTargetState (int state)
{
	sh->ActivateAirlock (lockid, state == 2 ? ShuttleA::DOOR_CLOSING : ShuttleA::DOOR_OPENING);
}


// ==============================================================

AirlockIndicator::AirlockIndicator (ShuttleA *v, int which, MESHHANDLE hMesh)
: PanelIndicator1 (v, hMesh, 2, 52+which*4), lockid(which)
{
}

// --------------------------------------------------------------

int AirlockIndicator::GetTargetState ()
{
	return (sh->lock_status[lockid] == ShuttleA::DOOR_CLOSED ? 6 :
		    sh->lock_status[lockid] == ShuttleA::DOOR_OPEN   ? 5 : 0);
}