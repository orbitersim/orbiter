// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// hudbutton.h
// User interface for HUD button controls
// ==============================================================

#ifndef __HUDBUTTON_H
#define __HUDBUTTON_H

#ifndef __linux__
#include "..\Common\Instrument.h"
#else // __linux__
#include "../Common/Instrument.h"
#endif // __linux__

// ==============================================================

class HUDButton: public PanelElement {
public:
	HUDButton (VESSEL3 *v);
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
};

#endif // !__HUDBUTTON_H
