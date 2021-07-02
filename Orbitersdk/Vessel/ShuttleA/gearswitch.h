// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// gearswitch.h
// User interface gear switch
// ==============================================================

#ifndef __GEARSWITCH_H
#define __GEARSWITCH_H

#include "switches.h"

// ==============================================================

class GearSwitch: public PanelSwitch1 {
public:
	GearSwitch (ShuttleA *v, MESHHANDLE hMesh);
	int GetTargetState();
	void SetTargetState (int state);
};


// ==============================================================

class GearIndicator: public PanelIndicator1 {
public:
	GearIndicator (ShuttleA *v, MESHHANDLE hMesh);
	int GetTargetState ();
};

#endif // !__GEARSWITCH_H