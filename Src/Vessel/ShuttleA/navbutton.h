// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// navbutton.h
// User interface for navigation buttons
// ==============================================================

#ifndef __NAVBUTTON_H
#define __NAVBUTTON_H

#include "..\Common\Instrument.h"

// ==============================================================

class NavButton: public PanelElement {
public:
	NavButton (VESSEL3 *v);
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
};

#endif // !__NAVBUTTON_H