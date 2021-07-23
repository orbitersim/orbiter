// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// payloadctrl.h
// User interface payload controls
// ==============================================================

#ifndef __PAYLOADCTRL_H
#define __PAYLOADCTRL_H

#include "switches.h"

// ==============================================================

class PayloadRelease: public PanelElement {
public:
	PayloadRelease (ShuttleA *v, MESHHANDLE hMesh);
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);

private:
	int vstate[6];
};

// ==============================================================

class PayloadArmSwitch: public PanelSwitch1 {
public:
	PayloadArmSwitch (ShuttleA *v, MESHHANDLE hMesh);
	int GetTargetState();
	void SetTargetState (int state);
};


// ==============================================================

class PayloadArmIndicator: public PanelIndicator1 {
public:
	PayloadArmIndicator (ShuttleA *v, MESHHANDLE hMesh);
	int GetTargetState ();
};

#endif // !__PAYLOADCTRL_H