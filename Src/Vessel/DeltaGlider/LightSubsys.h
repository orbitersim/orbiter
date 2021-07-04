// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// LightSubsys.h
// Cockpit and external light control: instrument/overhead lights,
// landing/docking/navigation/strobe lights
// ==============================================================

#ifndef __LIGHTSUBSYS_H
#define __LIGHTSUBSYS_H

#include "DGSwitches.h"
#include "DGSubsys.h"

// ==============================================================
// Light control subsystem
// ==============================================================

class InstrumentLight;
class CockpitLight;
class LandDockLight;
class StrobeLight;
class NavLight;

class LightCtrlSubsystem: public DGSubsystem {
public:
	LightCtrlSubsystem (DeltaGlider *v);
	inline InstrumentLight *InstrumentlightSubsys() { return instrlight; }
	inline CockpitLight *CockpitlightSubsys() { return cockpitlight; }
	inline LandDockLight *LandDocklightSubsys() { return landdocklight; }
	inline StrobeLight *StrobelightSubsys() { return strobelight; }
	inline NavLight *NavlightSubsys() { return navlight; }

private:
	InstrumentLight *instrlight;
	CockpitLight *cockpitlight;
	LandDockLight *landdocklight;
	StrobeLight *strobelight;
	NavLight *navlight;
};

// ==============================================================
// Instrument lights
// ==============================================================

class InstrumentLight: public DGSubsystem {
	friend class InstrumentLightSwitch;
	friend class InstrumentBrightnessDial;

public:
	InstrumentLight (LightCtrlSubsystem *_subsys);
	void SetLight (bool on, bool force=false);
	inline bool GetLight () const { return light_on; }
	void ModBrightness (bool up);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	bool clbkLoadVC (int vcid);
	void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);

private:
	bool light_on;                     // instrument illumination switch status
	int light_col;                     // instr. light colour index, 0=default
	double brightness;                 // instrument illumination brightness setting

	InstrumentLightSwitch *sw;
	InstrumentBrightnessDial *dial;
	int ELID_SWITCH;
	int ELID_DIAL;
	UINT anim_dial;                    // VC instrument brightness dial
};

// ==============================================================

class InstrumentLightSwitch: public DGSwitch1 {
public:
	InstrumentLightSwitch (InstrumentLight *comp);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	InstrumentLight *component;
};

// ==============================================================

class InstrumentBrightnessDial: public PanelElement {
public:
	InstrumentBrightnessDial (InstrumentLight *comp);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	InstrumentLight *component;
};

// ==============================================================
// Cockpit floodlights
// ==============================================================

class CockpitLight: public DGSubsystem {
	friend class CockpitLightSwitch;
	friend class CockpitBrightnessDial;

public:
	CockpitLight (LightCtrlSubsystem *_subsys);
	void SetLight (int mode, bool force=false);
	inline int GetLight () const { return light_mode; }
	void ModBrightness (bool up);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	bool clbkLoadVC (int vcid);
	void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);

private:
	int light_mode;                  // 0=off, 1=red, 2=white
	double brightness;               // floodlight brightness
	PointLight *light;               // local light object

	CockpitLightSwitch *sw;
	CockpitBrightnessDial *dial;
	int ELID_SWITCH;
	int ELID_DIAL;
	UINT anim_dial;                  // VC floodlight brightness dial
};

// ==============================================================

class CockpitLightSwitch: public DGSwitch1 {
public:
	CockpitLightSwitch (CockpitLight *comp);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	CockpitLight *component;
};

// ==============================================================

class CockpitBrightnessDial: public PanelElement {
public:
	CockpitBrightnessDial (CockpitLight *comp);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	CockpitLight *component;
};

// ==============================================================
// Landing/docking lights
// ==============================================================

class LandDockLight: public DGSubsystem {
	friend class LandDockLightSwitch;

public:
	LandDockLight (LightCtrlSubsystem *_subsys);
	void SetLight (int mode, bool force=false);
	inline int GetLight () const { return light_mode; }
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);

private:
	int light_mode;                      // 0=off, 1=docking, 2=landing
	SpotLight *light;
	LandDockLightSwitch *sw;
	int ELID_SWITCH;
};

// ==============================================================

class LandDockLightSwitch: public DGSwitch1 {
public:
	LandDockLightSwitch (LandDockLight *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	LandDockLight *component;
};

// ==============================================================
// Strobes
// ==============================================================

class StrobeLight: public DGSubsystem {
	friend class StrobeLightSwitch;

public:
	StrobeLight (LightCtrlSubsystem *_subsys);
	void SetLight (bool on);
	inline bool GetLight () const { return light_on; }
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);

private:
	bool light_on;                        // false=off, true=on
	StrobeLightSwitch *sw;
	int ELID_SWITCH;
};

// ==============================================================

class StrobeLightSwitch: public DGSwitch1 {
public:
	StrobeLightSwitch (StrobeLight *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	StrobeLight *component;
};

// ==============================================================
// Navigation lights
// ==============================================================

class NavLight: public DGSubsystem {
	friend class NavLightSwitch;

public:
	NavLight (LightCtrlSubsystem *_subsys);
	inline bool GetLight () const { return light_on; }
	void SetLight (bool on);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);

private:
	bool light_on;                           // false=off, true=on
	NavLightSwitch *sw;
	int ELID_SWITCH;
};

// ==============================================================

class NavLightSwitch: public DGSwitch1 {
public:
	NavLightSwitch (NavLight *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	NavLight *component;
};

#endif // !__LIGHTSUBSYS_H