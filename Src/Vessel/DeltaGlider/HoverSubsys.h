// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// HoverSubsys.h
// Hover engine control subsystem: manual input and automatic modes
// ==============================================================

#ifndef __HOVERSUBSYS_H
#define __HOVERSUBSYS_H

#include "DeltaGlider.h"
#include "DGSubsys.h"
#include "DGSwitches.h"
#include <vector>

// ==============================================================
// Hover control subsystem
// ==============================================================

class HoverAttitudeComponent;
class HoverHoldComponent;
class HoverManualComponent;

class HoverSubsystem: public DGSubsystem {
public:
	HoverSubsystem (DeltaGlider *dg);

	inline double GetThrusterLevel (int i) const { return hoverlevel[i]; }
	inline void   SetThrusterLevel (int i, double lvl) { hoverlevel[i] = lvl; }
	void IncGroupLevel (double dlvl);

	void ActivateHold (bool active);

	void clbkPostStep (double simt, double simdt, double mjd);
	void clbkReset2D (int panelid, MESHHANDLE hMesh);
	void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);

private:
	HoverAttitudeComponent *attctrl;      // attitude control submode
	HoverHoldComponent *holdctrl;         // altitude/vspd control submode
	HoverManualComponent *manctrl;        // manual hover control

	double hoverlevel[3];                 // current hover engine levels
};


// ==============================================================
// Base class for hover submodes
// ==============================================================

class HoverSubsystemComponent: public DGSubsystem {
public:
	HoverSubsystemComponent (HoverSubsystem *_subsys);
	inline HoverSubsystem *HoverSubsys() { return (HoverSubsystem*)Parent(); }
};

// ==============================================================
// Automatic hover attitude balance submode
// ==============================================================

class HoverCtrlDial;
class PHoverCtrl;
class RHoverCtrl;
class HoverDisp;

class HoverAttitudeComponent: public HoverSubsystemComponent {
public:
	HoverAttitudeComponent (HoverSubsystem *_subsys);
	inline int Mode() const { return mode; }
	inline void SetMode (int newmode) { mode = newmode; }
	inline double PHover (bool actual=true) const { return actual ? phover : phover_cmd; }
	inline double RHover (bool actual=true) const { return actual ? rhover : rhover_cmd; }
	bool IncPHover (int dir);     // manually change hover pitch command
	bool IncRHover (int dir);     // manually change hover roll command
	void AutoHoverAtt ();         // set hover pitch/roll commands from user input
	void TrackHoverAtt ();
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);

private:
	int mode;                     // balance mode: 0=off, 1=auto, 2=manual

	double phover, phover_cmd;    // current/commanded hover pitch angle (tan)
	double rhover, rhover_cmd;    // current/commanded hover roll angle (tan)

	HoverCtrlDial *modedial;      // mode dial object
	PHoverCtrl    *phoverswitch;  // pitch hover balance switch object
	RHoverCtrl    *rhoverswitch;  // roll hover balance switch object
	HoverDisp     *hoverdisp;     // hover balance display object

	int ELID_MODEDIAL;            // element ID: mode dial
	int ELID_PHOVERSWITCH;        // element ID: pitch hover balance switch
	int ELID_RHOVERSWITCH;        // element ID: roll hover balance switch
	int ELID_DISPLAY;             // element ID: balance display
};


// ==============================================================
// Automatic hover hold altitude/vspeed submode
// ==============================================================

class HoverAltBtn;
class HoverAltSwitch;
class HoverAltResetBtn;
class HoverAltModeButtons;

class HoverHoldComponent: public HoverSubsystemComponent {
	friend class HoverHoldAltIndicator;

public:
	enum HoverMode { HOLD_NONE, HOLD_ALT, HOLD_VSPD };

	HoverHoldComponent (HoverSubsystem *_subsys);
	void Activate (bool ison);
	double TargetAlt() const { return holdalt; }
	void SetTargetAlt (double alt);
	void SetTargetAltCurrent ();
	double TargetVspd() const { return holdvspd; }
	void SetTargetVspd (double vspd);
	double TargetPrm() const { return (hovermode == HOLD_ALT ? holdalt : holdvspd); }
	void SetTargetPrm (double prm);
	void SetHoverMode (HoverMode mode);
	HoverMode GetHoverMode () const { return hovermode; }
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);

protected:
	void HoverHoldVspd (double vspd);

private:
	double holdalt;     // current hold altitude
	double holdvspd;    // current hold vertical speed
	double holdT;       // for time interval
	double pvh;         // previous vertical speed
	bool active;        // hover hold altitude active?
	AltitudeMode altmode;
	HoverMode hovermode;

	HoverHoldAltIndicator *holddisp;
	HoverAltBtn *holdbtn;
	HoverAltSwitch *altset;
	HoverAltResetBtn *altreset;
	HoverAltModeButtons *modebuttons;

	int ELID_DISPLAY;
	int ELID_HOLDBTN;
	int ELID_ALTSET;
	int ELID_ALTRESET;
	int ELID_MODEBUTTONS;
};


// ==============================================================
// Manual hover control submode
// ==============================================================

class HoverManualComponent: public HoverSubsystemComponent {
	friend class HoverThrottle;

public:
	HoverManualComponent (HoverSubsystem *_subsys);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);

private:
	HoverThrottle *throttle;
	int ELID_THROTTLE;          // hover throttle panel ID
	UINT anim_hoverthrottle;    // VC hover throttle animation ID
};


// ==============================================================
// Panel elements for attitude submode
// ==============================================================

class HoverCtrlDial: public DGDial1 {
public:
	HoverCtrlDial (HoverAttitudeComponent *_ctrl);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	HoverAttitudeComponent *ctrl;
};

// ==============================================================

class HoverDisp: public PanelElement {
public:
	HoverDisp (HoverAttitudeComponent *_ctrl);
	~HoverDisp ();
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);

private:
	HoverAttitudeComponent *ctrl;
	int pofs_cur, pofs_cmd;
	int rofs_cur, rofs_cmd;
	GROUPREQUESTSPEC vc_grp; ///< Buffered VC vertex data
	WORD vperm[8];
};

// ==============================================================

class PHoverCtrl: public DGSwitch2 {
public:
	PHoverCtrl (HoverAttitudeComponent *_ctrl);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	HoverAttitudeComponent *ctrl;
};

// ==============================================================

class RHoverCtrl: public DGSwitch2 {
public:
	RHoverCtrl (HoverAttitudeComponent *_ctrl);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	HoverAttitudeComponent *ctrl;
};


// ==============================================================
// Panel elements for hold alt/vspd submode
// ==============================================================

// ==============================================================
// Hover hold altitude button

class HoverAltBtn: public DGButton3 {
public:
	HoverAltBtn (HoverHoldComponent *hhac);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);
	bool Redraw2D (SURFHANDLE hSurf);

private:
	HoverHoldComponent *ctrl;
	SURFHANDLE bsrc;
};

// ==============================================================
// Hover mode buttons

class HoverAltModeButtons: public PanelElement {
public:
	HoverAltModeButtons (HoverHoldComponent *hhac);
	~HoverAltModeButtons();
	void DefineAnimation2D (int meshgrp, int vofs);
	void DefineAnimationsVC (const VECTOR3 &axis, DWORD meshgrp, DWORD meshgrp_label,
		DWORD vofs[2], DWORD vofs_label[2]);
	bool Redraw2D (SURFHANDLE hSurf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);

private:
	HoverHoldComponent *ctrl;
	DGButton3 *btn[2];
	HoverHoldComponent::HoverMode vmode; // currently displayed hover mode
};

// ==============================================================
// Hover altitude/rate selector switch

class HoverAltSwitch: public DGSwitch2 {
public:
	HoverAltSwitch (HoverHoldComponent *hhac);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	void Set (int state, double refT);
	HoverHoldComponent *ctrl;
};

// ==============================================================
// Hover "copy current" button

class HoverAltResetBtn: public DGButton2 {
public:
	HoverAltResetBtn (HoverHoldComponent *hhac);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	HoverHoldComponent *ctrl;
};

// ==============================================================
// Hover hold altitude indicator displays

class HoverHoldAltIndicator: public PanelElement {
public:
	HoverHoldAltIndicator (HoverHoldComponent *hhac, SURFHANDLE blitsrc);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE hSurf);

private:
	bool Redraw ();
	void UpdateReadout (const char *tgtstr, char *curstr);
	HoverHoldComponent *ctrl;
	SURFHANDLE btgt, bsrc;
	HoverHoldComponent::HoverMode holdmode_disp;
	bool hold_disp;
	char holdstr[10];   // current hold altitude readout
};


// ==============================================================
// Panel elements for manual control submode
// ==============================================================

// ==============================================================
// Hover throttle

class HoverThrottle: public PanelElement {
public:
	HoverThrottle (HoverManualComponent *_ctrl);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE hSurf);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	HoverManualComponent *ctrl;
	float ppos;
	UINT sliderpos;
};

#endif // !__HOVERSUBSYS_H