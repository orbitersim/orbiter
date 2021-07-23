// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// airlockswitch.h
// User interface airlockswitches
// ==============================================================

#ifndef __AIRLOCKSWITCH_H
#define __AIRLOCKSWITCH_H

#include "switches.h"

// ==============================================================

class AirlockSwitch: public PanelSwitch1 {
public:
	AirlockSwitch (ShuttleA *v, int which, MESHHANDLE hMesh);
	int GetTargetState();
	void SetTargetState (int state);

private:
	int lockid;
};


// ==============================================================

class AirlockIndicator: public PanelIndicator1 {
public:
	AirlockIndicator (ShuttleA *v, int which, MESHHANDLE hMesh);
	int GetTargetState ();

private:
	int lockid;
};

#endif // !__AIRLOCKSWITCH_H