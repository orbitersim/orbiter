// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// FailureSubsys.h
// Subsystem for failure and warning displays
// ==============================================================

#ifndef __FAILURESUBSYS_H
#define __FAILURESUBSYS_H

#include "DGSubsys.h"

// ==============================================================
// Failure subsystem
// ==============================================================

class FailureSubsystem: public DGSubsystem {
	friend class MwsButton;

public:
	FailureSubsystem (DeltaGlider *v);
	inline bool MWSActive() const { return bMWSActive; }
	inline void MWSActivate() { bMWSActive = true; }
	inline void MWSReset() { bMWSActive = false; }
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);

private:
	bool bMWSActive, bMWSOn;                     // master warning flags
	MwsButton *mws;
	int ELID_MWS;
};

// ==============================================================

class MwsButton: public PanelElement {
public:
	MwsButton (FailureSubsystem *_subsys);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	FailureSubsystem *subsys;
	bool active;
	bool islit;
};

#endif // !__FAILURESUBSYS_H