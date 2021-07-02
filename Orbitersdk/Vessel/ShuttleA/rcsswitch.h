// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// rcsswitch.h
// User interface RCS switch
// ==============================================================

#ifndef __RCSSWITCH_H
#define __RCSSWITCH_H

#include "switches.h"

// ==============================================================

class RCSSwitch: public PanelSwitch1 {
public:
	RCSSwitch (ShuttleA *v);
	int GetTargetState();
	void SetTargetState (int state);
};


// ==============================================================

class RCSIndicator: public PanelIndicator1 {
public:
	RCSIndicator (ShuttleA *v);
	int GetTargetState ();
};

#endif // !__RCSSWITCH_H