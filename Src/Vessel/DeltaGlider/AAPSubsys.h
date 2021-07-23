// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// AAPSubsys.h
// Atmospheric autopilot
//
// Notes:
// The atmospheric autopilot provides functions for altitude,
// airspeed and heading/course.
// The autopilot drives the aerodynamic control surfaces, but
// not the RCS thrusters. It works only at sufficient 
// atmospheric pressure.
// The actual autopilot algorithms are implemented as scripts
// (Script/DG/aap.lua). This class simply provides the user
// interface to the script functions.
// ==============================================================

#ifndef __AAPSUBSYS_H
#define __AAPSUBSYS_H

#include "Orbitersdk.h"
#include "DeltaGlider.h"
#include "DGSubsys.h"

class InstrHSI;
class AAP;

// ==============================================================
// Atmospheric autopilot subsystem
// ==============================================================

class AAPSubsystem: public DGSubsystem {
public:
	AAPSubsystem (DGSubsystem *parent);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void AttachHSI (InstrHSI *_hsi);

private:
	AAP *aap;
	int ELID_AAP;
};

// ==============================================================

class AAP: public DGPanelElement {
public:
	AAP (AAPSubsystem *_subsys);
	~AAP();
	void Reset2D (int panelid, MESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	void AttachHSI (InstrHSI *_hsi) { hsi = _hsi; }
	void WriteScenario (FILEHANDLE scn);
	void SetState (const char *str);

protected:
	void ToggleActive (int block);
	void SetActive (int block, bool activate);
	void SetValue (int block, double val);
	void UpdateStr (char *str, char *pstr, int n, NTVERTEX *vtx);

private:
	AAPSubsystem *subsys;
	INTERPRETERHANDLE hAAP;       // script interpreter
	InstrHSI *hsi;                // attached HSI instrument
	int active_block;             // active AAP segment (0=alt, 1=spd, 2=hdg, -1=none)
	double tgt[3];                // target values for : altitude [m], speed [m/s], heading [deg]
	bool active[3], pactive[3];   // AP segment active?
	int  scanmode, scanpmode;     // scan mode for currently active block
	char readout[3][8];           // readout strings
};

#endif // !__AAPSUBSYS_H