// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// MfdSubsys.h
// Subsystem for MFD instruments
// ==============================================================

#ifndef __MFDSUBSYS_H
#define __MFDSUBSYS_H

#include "DeltaGlider.h"
#include "DGSubsys.h"

// ==============================================================
// MFD subsystem
// ==============================================================

class MfdButtonRow;
class MfdButtonCol;

class MfdSubsystem: public DGSubsystem {
public:
	MfdSubsystem (DeltaGlider *v, int mfdident);
	inline int MfdId() const { return mfdid; }
	inline SURFHANDLE VcTex() const { return vctex; }
	void ModeChanged ();
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);

private:
	int mfdid;
	MfdButtonRow *btnrow;
	MfdButtonCol *btncol[2];

	int ELID_BTNROW;
	int ELID_BTNCOL[2];

	SURFHANDLE vctex;
};

// ==============================================================
// MfdButtonGrp: row/column group of MFD buttons
// ==============================================================

class MfdButtonGrp: public PanelElement {
public:
	MfdButtonGrp (MfdSubsystem *_subsys, DWORD _nbtn);
	~MfdButtonGrp ();

protected:
	void PushButtonVC (DEVMESHHANDLE hMesh, int meshgrp, int btn, bool down);
	MfdSubsystem *subsys;
	DWORD nbtn;           // number of buttons in the group
	DWORD pending_btn;    // button waiting to be animated
	DWORD pending_action; // 0=none, 1=down, 2=up
	bool *ispushed;
};

// ==============================================================
// MfdButtonRow: POW/SEL/MNU buttons
// ==============================================================

class MfdButtonRow: public MfdButtonGrp {
public:
	MfdButtonRow (MfdSubsystem *_subsys);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	int curbtn;       // currently pressed button
};

// ==============================================================
// MfdButtonCol: Left/right MFD button column
// ==============================================================

class MfdButtonCol: public MfdButtonGrp {
public:
	MfdButtonCol (MfdSubsystem *_subsys, int side);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	int sd;       // 0=left, 1=right button column
	DWORD xcnt;   // x-offset of button centre line in texture
	int curbtn;   // currently pressed button
};

#endif // !__MFDSUBSYS_H