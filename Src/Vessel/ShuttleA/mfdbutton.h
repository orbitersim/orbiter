// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// mfdbutton.h
// User interface for MFD buttons
// ==============================================================

#ifndef __MFDBUTTON_H
#define __MFDBUTTON_H

#include "ShuttleA.h"
#include "..\Common\Instrument.h"

// ==============================================================

class MFDButtonCol: public PanelElement {
public:
	MFDButtonCol (VESSEL3 *v, DWORD _mfdid, DWORD _lr);
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);

private:
	DWORD mfdid;
	DWORD lr;
	DWORD xcnt;
};

// ==============================================================

class MFDButtonRow: public PanelElement {
public:
	MFDButtonRow (VESSEL3 *v, DWORD _mfdid);
	bool ProcessMouse2D (int event, int mx, int my);

private:
	DWORD mfdid;
};

#endif // !__MFDBUTTON_H