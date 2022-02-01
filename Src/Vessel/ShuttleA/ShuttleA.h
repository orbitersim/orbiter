// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE:  ShuttleA
//                  Part of the ORBITER SDK
//
// ShuttleA.h
// Class interface for Shuttle-A vessel class module
// ==============================================================

#ifndef __SHUTTLEA_H
#define __SHUTTLEA_H

#include "orbitersdk.h"

// ==========================================================
// Some vessel class caps
// ==========================================================

//56000 Liters per box... @20 tones per box * 6 boxes+ 13 tons the skeleton

const double EMPTY_MASS = 13000;
// Empty mass (excluding fuel) [kg]

//fuel tanks are: 1.7radius and 4m length = 36315 Liters / tank
//2 fuel tanks + internal one = 10000 Liters 
// Total = 82631 Liters for fuel
// LOX LH2 density  0.865g/cc =>
const double MAX_MAIN_FUEL = 71500.0; //oau !?
// Max fuel capacity: main tank [kg]

const double MAX_RCS_FUEL = 2700.0;
// Max fuel capacity: RCS tank [kg]

const double MAX_MAIN_THRUST = 2128720.0/2.0;
// Main engine thrust [N] per engine

const double MAX_HOVER_THRUST = 1489950.0/2.0;
// Hover engine thrust [N] per engine

const double MAX_RETRO_THRUST = 770000.0/2.0;
// Max thrust [N] of retro/hover pods per engine

const double MAX_RCS_THRUST = 4500.0;
// Max thrust [N] for each attitude thruster

const double ISP = 3e4;
// Sea-level Isp (fuel-specific impulse) for all thrusters [m/s] 

const double ISP_P0 =3.3e4;
// A better thrust/ISP in vacuum for all but RCS

const double DOCK_OPERATING_SPEED = 0.05;
// Opening/closing speed of docking hatch mechanism (1/sec)
// => cycle = 20 sec

const double AIRLOCK_OPERATING_SPEED = 0.1;
// Opening/closing speed of outer airlock (1/sec)
// => cycle = 10 sec

const double GEAR_OPERATING_SPEED = 0.25;
// Opening/closing speed of gear (1/sec)
// => cycle = 4 sec

const double POD_ROTREQUEST_SPEED = 0.4;
// speed at which the pod tilt switches react to preset changes

const double POD_ROT_SPEED = 0.2;
// speed at which pods actually rotate

const int nsurf = 6; // number of bitmap handles

const double MAX_GRAPPLING_DIST = 0.5f;
const double MAX_GRAPPLING_ANG  = 0.9f; //(cos(angle)>0.9f)
//distance for payload grappling.. being a bit generous here

const DWORD PANEL2D_TEXW  = 2048;
const DWORD PANEL2D_TEXH  = 1024;
const DWORD PANELEL_TEXW  =  512;
const DWORD PANELEL_TEXH  =  512;
const DWORD PANEL2D_MAINW = 1280;
const DWORD PANEL2D_MAINH  = 382;
const DWORD PANEL2D_OVRHW = 1280;
const DWORD PANEL2D_OVRHH =  299;

const DWORD LMFD_X     =  56;  // left MFD reference x-pos
const DWORD RMFD_X     = 966;  // right MFD reference x-pos
const DWORD NAVBTN_X   = 105;  // navigation button reference x-pos
const DWORD HUDBTN_X   = 912;  // HUD button reference x-pos
const DWORD THROTTLE_X = 521;  // Throttle block reference x-pos
const DWORD THROTTLE_Y = 244;  // Throttle block reference y-pos
const DWORD ADIBALL_X  = 733;  // ADI ball reference x-pos
const DWORD ADIBALL_Y  =  29;  // ADI ball reference y-pos
const DWORD ADICTRL_X  = 733;  // ADI control panel reference x-pos
const DWORD ADICTRL_Y  = 200;  // ADI control panel reference y-pos
const DWORD PODCTRL_X  = 379;  // Aux thruster pod panel reference x-pos
const DWORD PODCTRL_Y  = 255;  // Aux thruster pod panel reference y-pos

class PanelElement;
class ADIBall;
class ADICtrl;
class AuxPodCtrl;
class AttitudeReference;

// ==========================================================
// Interface for derived vessel class: ShuttleA
// ==========================================================

class ShuttleA: public VESSEL4 {
	friend class PodTiltPreset;

public:
	ShuttleA (OBJHANDLE hObj, int fmodel);
	~ShuttleA ();
	void ReleaseSurfaces ();
	void InitPanel (int panel);
	bool RotatePods (UINT which, UINT mode);
	void SetPodAngle (UINT which, double angle);
	void CommandPodAngle (UINT which, double angle);
	inline double GetPodAngle (UINT which) { return pod_angle[which]; }

	void RedrawPanel_MFDButton (SURFHANDLE surf, int mfd, int side);
	void RedrawPanel_Navmode (SURFHANDLE surf);
	bool RedrawPanel_Throttle (SURFHANDLE surf);
	bool RedrawPanel_Hover (SURFHANDLE surf);
	bool RedrawPanel_Podlevel (SURFHANDLE surf);
	bool RedrawPanel_EngineIndicator (SURFHANDLE surf);
	bool RedrawPanel_PodangleIndicator (SURFHANDLE surf);
	void RedrawPanel_Fuelstatus (SURFHANDLE surf, int part);
	void RedrawPanel_CargoOpen(SURFHANDLE surf);
	void RedrawPanel_CargoPresent(SURFHANDLE surf);

	SURFHANDLE srf[nsurf];
	PROPELLANT_HANDLE ph_main, ph_rcs;
	THRUSTER_HANDLE th_main[2], th_hover[2], th_pod[2];
	MESHHANDLE vcmesh_tpl,exmesh_tpl;
	UINT podswitch[2];
	double dock_proc, lock_proc[2], gear_proc;
	enum DoorStatus { DOOR_CLOSED, DOOR_OPEN, DOOR_CLOSING, DOOR_OPENING }
		dock_status, lock_status[2], gear_status;

	ATTACHMENTHANDLE  payload_attachment[6];
	int cargo_open[6];		//Cargo control buttons status
	int cargo_arm_status;
	bool ToggleGrapple (int grapple);

	// 2D panel definition
	MESHHANDLE hPanelMesh;                 // panel mesh handle
	static SURFHANDLE panel2dtex;          // panel texture handle
	static SURFHANDLE paneleltex;          // panel element texture handle
	static SURFHANDLE aditex;              // texture for ADI ball
	SURFHANDLE main_tex;

	void ActivateDockingPort (DoorStatus action);
	void RevertDockingPort ();
	void ActivateAirlock (int which, DoorStatus action);
	void RevertAirlock (int which);
	void ActivateLandingGear (DoorStatus action);
	void RevertLandingGear();
	void ActivateCargo (int status);

	bool SetADILayout (int layout);
	AttitudeReference *GetAttref() { return attref; }
	bool SetAttrefMode (int mode);
	bool SetAttrefTgtMode (int mode);
	void SetAttrefOffset (const VECTOR3 &ofs);
	void SetAtttgtOffset (const VECTOR3 &ofs);
	bool SetAttOffsetMode (int mode);
	bool SetAtttgtFrameMode (int mode);

	// Overloaded callback functions
	void clbkSetClassCaps (FILEHANDLE cfg);
	void clbkLoadStateEx (FILEHANDLE scn, void *vs);
	void clbkSaveState (FILEHANDLE scn);
	void clbkPostCreation ();
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	void clbkPostStep (double simt, double simdt, double mjd);
	void clbkMFDMode (int mfd, int mode);
	void clbkNavMode (int mode, bool active);
	void clbkHUDMode (int mode);
	void clbkRCSMode (int mode);
	int  clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);
	int  clbkConsumeDirectKey (char *kstate);
	int  clbkGeneric (int msgid, int prm, void *context);
	bool clbkLoadPanel2D (int id, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkPanelMouseEvent (int id, int event, int mx, int my, void *context);
	bool clbkPanelRedrawEvent (int id, int event, SURFHANDLE surf, void *context);
	bool clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp);
	void clbkRenderHUD (int mode, const HUDPAINTSPEC *hps, SURFHANDLE hTex);
	bool clbkLoadVC (int id);
	bool clbkVCRedrawEvent (int id, int event, SURFHANDLE surf);
	bool clbkVCMouseEvent (int id, int event, VECTOR3 &p);
	void clbkVisualCreated (VISHANDLE vis, int refcount);

private:
	void DefineMainPanel (PANELHANDLE hPanel);
	void DefineOverheadPanel (PANELHANDLE hPanel);
	void ScalePanel (PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	void DefineAnimations ();
	double payload_mass;
	void ComputePayloadMass();
	void ApplySkin ( DEVMESHHANDLE exmesh );
	void PaintMarkings (SURFHANDLE tex);
	
	// Lua interface methods
	int Lua_InitInstance (void *context);

	AttitudeReference *attref; // attitude reference platform

	THGROUP_HANDLE thg_main, thg_hover, thg_pod;
	double pod_angle[2], pod_angle_request[2];
	PanelElement *pel[36];
	ADIBall *adiball;
	ADICtrl *adictrl;
	AuxPodCtrl *podctrl;
	int npel;
	int adi_layout; // (0=standard (yaw-major), 1=pitch-major

	UINT anim_pod[2], anim_dock;
	UINT anim_lock[2]; // 0=outer, 1=inner
	UINT sliderpos_main[2], sliderpos_hovr[2];
	UINT sliderpos_retro[2], sliderpos_auxhovr[2], sliderpos_pod[2];
    UINT anim_gear;

	UINT sliderpos_main_v[2], sliderpos_hovr_v[2];
	UINT sliderpos_retro_v[2], sliderpos_auxhovr_v[2], sliderpos_pod_v[2];

	UINT anim_pod_thrust_left;      // VC pod thruster animation
	UINT anim_pod_thrust_right;      // VC pod thruster animation

	UINT anim_main_thrust_left;
	UINT anim_main_thrust_right;

	UINT anim_hover_thrust_left;
	UINT anim_hover_thrust_right;

    UINT anim_pod_angle;
	UINT anim_rcs_mode;

	UINT anim_dock_switch;
	UINT anim_airlock_switch;
	UINT anim_gear_switch;
	UINT anim_cargo_switch;

	MESHHANDLE hPanelMesh0; // mesh for 2D main panel
	MESHHANDLE hPanelMesh1; // mesh for 2D overhead panel

	void RedrawVC_ThPOD();
	void RedrawVC_ThMain();
	void RedrawVC_ThHover();
};

typedef struct {
	HINSTANCE hDLL;
	HFONT hFont[1];
	HPEN hPen[3];
	HBRUSH hBrush[2];
	oapi::Font* pFont[1];
	oapi::Pen* pPen[3];
	oapi::Brush* pBrush[2];
} GDIParams;

#define AID_MFD1_LBUTTONS      0
#define AID_MFD1_RBUTTONS      1
#define AID_MFD1_BBUTTONS      2
#define AID_MFD2_LBUTTONS      3
#define AID_MFD2_RBUTTONS      4
#define AID_MFD2_BBUTTONS      5

#define AID_ENGINEMAIN         6
#define AID_ENGINEHOVER        7
#define AID_ENGINEPODLEVEL     8
#define AID_ENGINEINDICATOR    9
#define AID_SPDINSTR          10
#define AID_ALTINSTR          11
#define AID_VSINSTR           12
#define AID_VACCINSTR         13
#define AID_PODANGLEINDICATOR 14
#define AID_PODCTRL           15
#define AID_PODANGLEPRESET    16
#define AID_PODANGLESWITCH    17
#define AID_NAVMODE           18
#define AID_HUDMODE           19
#define AID_ATTITUDEMODE      20
#define AID_ATTITUDEINDICATOR 21
#define AID_ADIBALL           22
#define AID_ADICTRL           23
#define AID_THMAINNEEDLE      24
#define AID_THHOVERNEEDLE     25
#define AID_THPODNEEDLE       26
#define AID_PHMAINNEEDLE      27
#define AID_PHRCSNEEDLE       28

#define AID_FUELSTATUS1       29
#define AID_FUELSTATUS2       30
#define AID_FUELSTATUS3       31
#define AID_FUELSTATUS4       32
#define AID_AIRLOCK1SWITCH    33
#define AID_AIRLOCK2SWITCH    34
#define AID_AIRLOCK1INDICATOR 35
#define AID_AIRLOCK2INDICATOR 36
#define AID_DOCKSWITCH        37
#define AID_DOCKINDICATOR     38
#define AID_GEARSWITCH		  39
#define AID_GEARINDICATOR	  40

#define AID_CARGO_OPEN		  41
#define AID_GARGOARMSWITCH	  42
#define AID_CARGOARMINDICATOR 43
#define AID_CARGOPRESENT	  44 // TODO
#endif // !__SHUTTLEA_H