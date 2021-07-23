// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// PressureSubsys.h
// Cabin and airlock pressure control subsystem
// ==============================================================

#ifndef __PRESSURESUBSYS_H
#define __PRESSURESUBSYS_H

#include "DeltaGlider.h"
#include "DGSubsys.h"
#include "DGSwitches.h"

// ==============================================================

class AirlockCtrl;
class TophatchCtrl;
class PValveSwitch;
class PressureIndicator;

// ==============================================================
// Pressure control module
// ==============================================================

class PressureSubsystem: public DGSubsystem {
public:
	PressureSubsystem (DeltaGlider *vessel);
	~PressureSubsystem ();
	inline double PCabin() const { return p_cabin; }
	inline double PAirlock() const { return p_airlock; }
	inline double PExtHatch() const { return p_ext_hatch; }
	inline double PExtLock() const { return p_ext_lock; }
	inline int GetPValve (int i) const { return valve_status[i]; }
	inline void SetPValve (int i, int status) { valve_status[i] = status; }
	const AnimState2 &OLockState () const;
	const AnimState2 &ILockState () const;
	void OpenOuterAirlock ();
	void CloseOuterAirlock ();
	void OpenInnerAirlock ();
	void CloseInnerAirlock ();
	void OpenHatch ();
	void CloseHatch ();
	const AnimState2 &HatchState() const;
	void RepairDamage ();
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);     // create the VC elements for this module

private:
	bool docked;
	double p_cabin, p_airlock;      // current pressure in cabin and airlock [Pa]
	double p_ext_hatch, p_ext_lock; // current pressure outside hatch and airlock [Pa]
	static double v_cabin;          // cabin volume [m^3]
	static double v_airlock;        // airlock volume [m^3]
	double v_extdock;               // volume of compartment outside airlock
	static double p_target;         // target pressure in cabin and airlock when supply valves are open
	AirlockCtrl *airlockctrl;       // airlock controls
	TophatchCtrl *hatchctrl;        // top hatch controls
	PValveSwitch *valve_switch[5];  // the switches controlling supply and relief valves
	int valve_status[5];            // 0=closed, 1=open
	PressureIndicator *pind;

	// local panel element identifiers
	int ELID_PVALVESWITCH[5];
	int ELID_DISPLAY;
};

// ==============================================================
// Airlock controls
// ==============================================================

class AirlockCtrl: public DGSubsystem {
	friend class PressureSubsystem;
	friend class OuterLockSwitch;
	friend class InnerLockSwitch;

public:
	AirlockCtrl (PressureSubsystem *_subsys);
	void OpenOuterLock ();
	void CloseOuterLock ();
	void RevertOuterLock ();
	void OpenInnerLock ();
	void CloseInnerLock ();
	void RevertInnerLock ();
	inline const AnimState2 &OLockState() const { return ostate; }
	inline const AnimState2 &ILockState() const { return istate; }
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void clbkPostCreation ();
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	int clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);

private:
	AnimState2 ostate, istate;

	OuterLockSwitch *osw;
	InnerLockSwitch *isw;

	int ELID_OSWITCH;
	int ELID_ISWITCH;

	UINT anim_olock;            // handle for outer airlock animation
	UINT anim_ilock;            // handle for inner airlock animation
};

// ==============================================================

class OuterLockSwitch: public DGSwitch1 {
public:
	OuterLockSwitch (AirlockCtrl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	AirlockCtrl *component;
};

// ==============================================================

class InnerLockSwitch: public DGSwitch1 {
public:
	InnerLockSwitch (AirlockCtrl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	AirlockCtrl *component;
};

// ==============================================================
// Top hatch controls
// ==============================================================

class TophatchCtrl: public DGSubsystem {
	friend class PressureSubsystem;
	friend class HatchCtrlSwitch;

public:
	TophatchCtrl (PressureSubsystem *_subsys);
	~TophatchCtrl ();
	void OpenHatch();
	void CloseHatch();
	void Revert ();
	inline const AnimState2 &State() const { return hatch_state; }
	void RepairDamage();
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void clbkPostCreation ();
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);     // create the VC elements for this module
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	int clbkConsumeBufferedKey(DWORD key, bool down, char *kstate);

private:
	AnimState2 hatch_state;
	HatchCtrlSwitch *sw;
	int ELID_SWITCH;
	PSTREAM_HANDLE hatch_vent;
	double hatch_vent_t;
	UINT anim_hatch;            // handle for top hatch animation
	int hatchfail;
};

// ==============================================================

class HatchCtrlSwitch: public DGSwitch1 {
public:
	HatchCtrlSwitch (TophatchCtrl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	TophatchCtrl *component;
};

// ==============================================================
// Switch for pressure valve operation

class PValveSwitch: public DGSwitch1 {
public:
	PValveSwitch (PressureSubsystem *_subsys, int id);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	PressureSubsystem *subsys;
	int vid; // valve identifier
};

// ==============================================================
// Pressure indicator display

class PressureIndicator: public PanelElement {
public:
	PressureIndicator (PressureSubsystem *_subsys, SURFHANDLE blitsrc);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	bool Redraw ();
	void ValStr (double p, char *cbuf);
	void BlitReadout (int which, const char *str);
	PressureSubsystem *subsys;
	double upt;
	SURFHANDLE bsrc, btgt;
	char label[4][8];
};

#endif // !___PRESSURESUBSYS_H