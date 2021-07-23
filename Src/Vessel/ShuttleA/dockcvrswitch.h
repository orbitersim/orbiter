// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// dockcvrswitch.h
// User interface dockingport cover switch
// ==============================================================

#ifndef __DOCKCVRSWITCH_H
#define __DOCKCVRSWITCH_H

#include "switches.h"

// ==============================================================

class DockCoverSwitch: public PanelSwitch1 {
public:
	DockCoverSwitch (ShuttleA *v, MESHHANDLE hMesh);
	int GetTargetState();
	void SetTargetState (int state);
};


// ==============================================================

class DockCoverIndicator: public PanelIndicator1 {
public:
	DockCoverIndicator (ShuttleA *v, MESHHANDLE hMesh);
	int GetTargetState ();
};

#endif // !__DOCKCVRSWITCH_H