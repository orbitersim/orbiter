// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
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