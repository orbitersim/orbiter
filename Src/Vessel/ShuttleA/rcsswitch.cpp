// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// rcsswitch.cpp
// User interface RCS switch
// ==============================================================

#define STRICT 1
#include "rcsswitch.h"

// ==============================================================

RCSSwitch::RCSSwitch (ShuttleA *v): PanelSwitch1 (v, 390.5f, 52.5f, true)
{
}

// --------------------------------------------------------------

int RCSSwitch::GetTargetState ()
{
	return sh->GetAttitudeMode();
}

// --------------------------------------------------------------

void RCSSwitch::SetTargetState (int state)
{
	sh->SetAttitudeMode (state);
}


// ==============================================================

RCSIndicator::RCSIndicator (ShuttleA *v): PanelIndicator1 (v, 388.0f, 102.0f)
{
}

// --------------------------------------------------------------

int RCSIndicator::GetTargetState ()
{
	static int idx[3] = {7,8,9};
	return idx[sh->GetAttitudeMode()];
}