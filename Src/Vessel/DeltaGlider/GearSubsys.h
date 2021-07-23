// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// GearSubsys.h
// Subsystem for landing gear control
// ==============================================================

#ifndef __GEARSUBSYS_H
#define __GEARSUBSYS_H

#include "DeltaGlider.h"
#include "DGSubsys.h"
#include "DGSwitches.h"

// ==============================================================
// Landing gear subsystem
// ==============================================================

class GearControl;
class Wheelbrake;

class GearSubsystem: public DGSubsystem {
public:
	GearSubsystem (DeltaGlider *v);
	void LowerGear ();
	void RaiseGear ();
	const AnimState2 &GearState() const;

private:
	GearControl *gearctrl;
	Wheelbrake *wheelbrake;
};

// ==============================================================
// Gear control: lever+indicator
// ==============================================================

class GearLever;
class GearIndicator;

class GearControl: public DGSubsystem {
public:
	GearControl (GearSubsystem *_subsys);
	void LowerGear ();
	void RaiseGear ();
	void RevertGear ();
	inline const AnimState2 &GearState() const { return gear_state; }
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void clbkPostCreation ();
	bool clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp);
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	int clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);

private:
	AnimState2 gear_state, glever_state;
	UINT anim_gear;             // handle for landing gear animation
	UINT anim_gearlever;        // VC gear lever

	GearLever *lever;
	GearIndicator *indicator;

	int ELID_LEVER;
	int ELID_INDICATOR;
};

// ==============================================================

class GearLever: public PanelElement {
public:
	GearLever (GearControl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	GearControl *component;
};

// ==============================================================

class GearIndicator: public PanelElement {
public:
	GearIndicator (GearControl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	GearControl *component;
	double tofs;
	bool light;
};

// ==============================================================
// Wheelbrake
// ==============================================================

class WheelbrakeLever;

class Wheelbrake: public DGSubsystem {
public:
	Wheelbrake (GearSubsystem *_subsys);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);

private:
	WheelbrakeLever *lever;
	int ELID_LEVER;
};

// ==============================================================

class WheelbrakeLever: public PanelElement {
public:
	WheelbrakeLever (Wheelbrake *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);

private:
	Wheelbrake *component;
	bool isdown[2];
};

#endif // !__GEARSUBSYS_H