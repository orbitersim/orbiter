// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// AirlockSwitch.h
// User interface for row of switches on overhead panel
// ==============================================================

#ifndef __AIRLOCKSWITCH_H
#define __AIRLOCKSWITCH_H

#include "..\Common\Vessel\Instrument.h"

// ==============================================================

class AirlockSwitch: public PanelElement {
public:
	AirlockSwitch (VESSEL3 *v);
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx);
	void Reset2D ();
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);

private:
	int btnstate[3]; // 0=up, 1=down
};

#endif // !__SWITCHARRAY_H