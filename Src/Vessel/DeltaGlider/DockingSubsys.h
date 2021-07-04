// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// DockingSubsys.h
// Nosecone and undock controls
// ==============================================================

#ifndef __DOCKINGSUBSYS_H
#define __DOCKINGSUBSYS_H

#include "DeltaGlider.h"
#include "DGSubsys.h"
#include "DGSwitches.h"

// ==============================================================
// Docking control subsystem
// ==============================================================

class NoseconeCtrl;
class UndockCtrl;
class EscapeLadderCtrl;
class DocksealCtrl;

class DockingCtrlSubsystem: public DGSubsystem {
public:
	DockingCtrlSubsystem (DeltaGlider *v);
	const AnimState2 &NconeState() const;
	void OpenNcone ();
	void CloseNcone ();

	const AnimState2 &LadderState() const;
	void ExtendLadder();
	void RetractLadder();

	void clbkDockEvent (int dock, OBJHANDLE mate);

private:
	NoseconeCtrl *noseconectrl;
	UndockCtrl *undockctrl;
	EscapeLadderCtrl *eladderctrl;
	DocksealCtrl *dsealctrl;
};


// ==============================================================
// Nosecone control
// ==============================================================

class NoseconeCtrl: public DGSubsystem {
	friend class NoseconeLever;
	friend class NoseconeIndicator;

public:
	NoseconeCtrl (DockingCtrlSubsystem *_subsys);
	inline const AnimState2 &NconeState() const { return ncone_state; }
	void OpenNcone();
	void CloseNcone();
	void RevertNcone ();
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void clbkPostCreation ();
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	int clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);

private:
	NoseconeLever *lever;
	NoseconeIndicator *indicator;

	int ELID_LEVER;
	int ELID_INDICATOR;

	UINT anim_nose;             // handle for nose cone animation
	UINT anim_noselever;        // handle for nose cone lever animation

	AnimState2 ncone_state, nlever_state;
};

// ==============================================================

class NoseconeLever: public PanelElement {
public:
	NoseconeLever (NoseconeCtrl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	NoseconeCtrl *component;
};

// ==============================================================

class NoseconeIndicator: public PanelElement {
public:
	NoseconeIndicator (NoseconeCtrl *comp);
	void ResetVC (DEVMESHHANDLE hMesh);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	NoseconeCtrl *component;
	double tofs;
	bool light;
};


// ==============================================================
// Undock control
// ==============================================================

class UndockCtrl: public DGSubsystem {
	friend class UndockLever;

public:
	UndockCtrl (DockingCtrlSubsystem *_subsys);
	void PullLever ();
	void ReleaseLever ();
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);

private:
	UndockLever *lever;
	int ELID_LEVER;
	UINT anim_undocklever;      // handle for undock lever animation
	AnimState2 undock_state;
};

// ==============================================================

class UndockLever: public PanelElement {
public:
	UndockLever (UndockCtrl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	UndockCtrl *component;
	bool btndown;
};


// ==============================================================
// Escape ladder control
// ==============================================================

class EscapeLadderCtrl: public DGSubsystem {
	friend class LadderSwitch;
	friend class LadderIndicator;

public:
	EscapeLadderCtrl (DockingCtrlSubsystem *_subsys);
	void ExtendLadder ();
	void RetractLadder ();
	inline const AnimState2 &State() const { return ladder_state; }
	void clbkPostCreation ();
	void clbkPostStep (double simt, double simdt, double mjd);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);

private:
	LadderSwitch *sw;
	LadderIndicator *indicator;
	int ELID_SWITCH;
	int ELID_INDICATOR;
	UINT anim_ladder;           // handle for front escape ladder animation
	AnimState2 ladder_state;
};

// ==============================================================

class LadderSwitch: public DGSwitch1 {
public:
	LadderSwitch (EscapeLadderCtrl *comp);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	EscapeLadderCtrl *component;
	bool light;
};

// ==============================================================

class LadderIndicator: public PanelElement {
public:
	LadderIndicator (EscapeLadderCtrl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	EscapeLadderCtrl *component;
	bool vlight_2D, vlight_VC;
};


// ==============================================================
// Dock seal control
// ==============================================================

class DocksealCtrl: public DGSubsystem {
	friend class DocksealIndicator;

public:
	DocksealCtrl (DockingCtrlSubsystem *_subsys);
	void SetDockStatus (bool docked);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	void clbkPostStep (double simt, double simdt, double mjd);
	void clbkPostCreation ();

private:
	DocksealIndicator *indicator;
	int ELID_INDICATOR;
	bool isDocked;
	bool isSealing;
	double dockTime;
};

// ==============================================================

class DocksealIndicator: public PanelElement {
public:
	DocksealIndicator (DocksealCtrl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	DocksealCtrl *component;
	bool vlight_2D, vlight_VC;
};

#endif // !__DOCKINGSUBSYS_H