// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// DeltaGlider.h
// Class interface for delta glider vessel class module
//
// Notes:
// * Two alternative sets of vessel capabilities are provided
//   ("easy" and "complex"), depending on user-selected option.
// * This interface covers both the "classic" (DeltaGlider) and
//   scramjet (DG-S) versions, distinguished by the SCRAMJET
//   entry in the definition file.
// ==============================================================

#ifndef __DELTAGLIDER_H
#define __DELTAGLIDER_H

#define STRICT 1

#include "orbitersdk.h"
#include "..\Common\Instrument.h"

// ==============================================================
// Some vessel class caps
// Where an entry consists of two values, the first refers to the
// "easy", the second to the "complex" flight model.
// ==============================================================

const double EMPTY_MASS    = 11000.0;  // standard configuration
const double EMPTY_MASS_SC = 13000.0;  // ramjet configuration
// DG mass w/o fuel

const double PSNGR_MASS    =     85.0;
// mass per passenger (including life support system)

const double TANK1_CAPACITY = 10400.0;
const double TANK2_CAPACITY =  2500.0;
// Main fuel tank capacities [kg] (can be split between rocket
// fuel and scramjet fuel)

const double RCS_FUEL_CAPACITY = 600.0;
// Max fuel capacity: RCS tank [kg]

const double MAX_MAIN_THRUST[2] = {2.0e5, 1.6e5};
// Main engine max vacuum thrust [N] per engine. (x2 for total)

const double MAX_RETRO_THRUST = 3.4e4;
// Retro engine max vacuum thrust [N] per engine. (x2 for total)

const double MAX_HOVER_THRUST[2] = {1.4e5, 1.1e5};
// Hover engine max vacuum thrust [N] (x2 for total)

const double MAX_RCS_THRUST = 2.5e3;
// Attitude control system max thrust [N] per engine.

const double ISP = 4e4;
// Vacuum Isp (fuel-specific impulse) for all thrusters [m/s]

const double GEAR_OPERATING_SPEED = 0.15;
// Opening/closing speed of landing gear (1/sec)
// => gear cycle ~ 6.7 sec

const double NOSE_OPERATING_SPEED = 0.05;
// Opening/closing speed of nose cone docking mechanism (1/sec)
// => cycle = 20 sec

const double AIRLOCK_OPERATING_SPEED = 0.1;
// Opening/closing speed of outer airlock (1/sec)
// => cycle = 10 sec

const double RADIATOR_OPERATING_SPEED = 0.02;
// Deployment speed of radiator (1/sec)
// => cycle = 50 sec

const double AIRBRAKE_OPERATING_SPEED = 0.3;
// Deployment speed of airbrakes
// => cycle = 3.3 sec

const double LADDER_OPERATING_SPEED = 0.1;
// Deployment speed of escape ladder

const double HATCH_OPERATING_SPEED = 0.15;
// Opening/closing speed of top hatch

const double RCOVER_OPERATING_SPEED = 0.3;
// Retro cover opening/closing speed

// ========= Main engine parameters ============

const double MAIN_PGIMBAL_RANGE = tan (5.0*RAD);
const double MAIN_YGIMBAL_RANGE = 1.0/7.7;
// main engine pitch and yaw gimbal range (tan)

const double MAIN_GIMBAL_SPEED = 0.06;
// operating speed of main engine pitch and yaw gimbals

const double PHOVER_RANGE = 10.0*RAD;
const double RHOVER_RANGE = 10.0*RAD;
// max hover-induced pitch and roll values

const double MAX_AUTO_HOVER_ATT = 30*RAD;
// max pitch/roll angle for hover control

const double MAX_HOVER_IMBALANCE = 4e3;
// max thrust imbalance between front and aft hover engines [N]

const double HOVER_BALANCE_SPEED = 2e3;
// operating speed of hover balance shift control

// ========== Scramjet parameters ==============

const double SCRAM_TEMAX[2] = {3500.0, 3200.0};
// Max. scramjet exhaust temperature [K]

const double SCRAM_FHV[2] = {3.5e8, 2.0e8};
// Scramjet fuel heating value [J/kg]: Amount of heat energy
// obtained from burning 1kg of propellant

const double SCRAM_MAX_DMF[2] = {2.0,3.0};
// Max. scramjet fuel flow rate [kg/s]

const double SCRAM_INTAKE_AREA = 1.0;
// Scramjet intake cross section (per engine) [m^2]

const double SCRAM_DEFAULT_DIR = 9.0*RAD;
// Default scramjet thrust angle (rad)

// ============ Damage parameters ==============

const double WINGLOAD_MAX =  16e3;
const double WINGLOAD_MIN = -10e3;
// Max. allowed positive and negative wing load [N/m^2]

const double DYNP_MAX = 300e3;
// Max. allowed dynamic pressure [Pa]

// =============================================
// 2D instrument panel parameters

const DWORD PANEL2D_WIDTH = 1280;  // panel width [pixel]
const DWORD PANEL2D_TEXW  = 2048;  // texture width
const DWORD PANEL2D_TEXH  = 1024;  // texture height
const DWORD INSTR3D_TEXW  =  512;
const DWORD INSTR3D_TEXH  = 1024;

// ==========================================================
// Forward declarations
// ==========================================================

// DG subsystems
class DGSubsystem;
class HUDControl;
class MainRetroSubsystem;
class HoverSubsystem;
class RcsSubsystem;
class ScramSubsystem;
class AerodynCtrlSubsystem;
class GearSubsystem;
class AvionicsSubsystem;
class MfdSubsystem;
class PressureSubsystem;
class ThermalSubsystem;
class DockingCtrlSubsystem;
class LightCtrlSubsystem;
class FailureSubsystem;
class AAPSubsystem;
class DlgControl;

// ==========================================================
// Interface for derived vessel class: DeltaGlider
// ==========================================================

class DeltaGlider: public ComponentVessel {
	friend class FuelMFD;
	friend class ThermalSubsystem;

public:
	DeltaGlider (OBJHANDLE hObj, int fmodel);
	~DeltaGlider ();
	short FlightModel() const { return flightmodel; }
	void SetEmptyMass () const;
	void DefineAnimations ();
	void InitPanel (int panel);
	void InitVC (int vc);
	inline bool ScramVersion() const { return ssys_scram != NULL; }
	void UpdateStatusIndicators();
	void SetPassengerVisuals ();
	void SetDamageVisuals ();
	void SetPanelScale (PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	void DefinePanelMain (PANELHANDLE hPanel);
	void DefinePanelOverhead (PANELHANDLE hPanel);
	bool RedrawPanel_ScramTempDisp (SURFHANDLE surf);
	bool RedrawPanel_MainFlow (SURFHANDLE surf);
	bool RedrawPanel_RetroFlow (SURFHANDLE surf);
	bool RedrawPanel_HoverFlow (SURFHANDLE surf);
	bool RedrawPanel_ScramFlow (SURFHANDLE surf);
	void InitVCMesh ();

	double GetMainThrusterLevel (int which) const { return GetThrusterLevel (th_main[which]); }
	double GetRetroThrusterLevel (int which) const { return GetThrusterLevel (th_retro[which]); }
	void   SetMainRetroLevel (int which, double lmain, double lretro);
	void   EnableRetroThrusters (bool state);
	void   GetMainThrusterDir (int which, VECTOR3 &dir) const { GetThrusterDir(th_main[which], dir); }
	void   SetMainThrusterDir (int which, const VECTOR3 &dir) { SetThrusterDir(th_main[which], dir); }
	double GetHoverThrusterLevel (int th) const { return GetThrusterLevel(th_hover[th]); }
	void   SetHoverThrusterLevel (int th, double lvl) { SetThrusterLevel(th_hover[th], lvl); }
	double GetMaxHoverThrust () const;

	void TestDamage ();
	void ApplyDamage ();
	void RepairDamage ();

	// Overloaded callback functions
	void clbkSetClassCaps (FILEHANDLE cfg);
	void clbkLoadStateEx (FILEHANDLE scn, void *vs);
	void clbkSaveState (FILEHANDLE scn);
	void clbkPostCreation ();
	void clbkVisualCreated (VISHANDLE vis, int refcount);
	void clbkVisualDestroyed (VISHANDLE vis, int refcount);
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp);
	void clbkRenderHUD (int mode, const HUDPAINTSPEC *hps, SURFHANDLE hTex);
	void clbkRCSMode (int mode);
	void clbkADCtrlMode (DWORD mode);
	void clbkHUDMode (int mode);
	void clbkMFDMode (int mfd, int mode);
	void clbkNavMode (int mode, bool active);
	void clbkDockEvent (int dock, OBJHANDLE mate);
	int  clbkNavProcess (int mode);
	int  clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);
	bool clbkLoadGenericCockpit ();
	bool clbkLoadPanel2D (int id, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkPanelMouseEvent (int id, int event, int mx, int my, void *context);
	bool clbkPanelRedrawEvent (int id, int event, SURFHANDLE surf, void *context);
	bool clbkLoadVC (int id);
	bool clbkVCMouseEvent (int id, int event, VECTOR3 &p);
	bool clbkVCRedrawEvent (int id, int event, SURFHANDLE surf);
	int  clbkGeneric (int msgid, int prm, void *context);

	bool psngr[4];              // passengers?
	bool bDamageEnabled;        // damage/failure testing?

	// parameters for failure modelling
	double lwingstatus, rwingstatus;
	bool aileronfail[4];

	void SetGearParameters (double state);

	// Animation handles
	UINT anim_rudder;           // handle for rudder animation
	UINT anim_elevator;         // handle for elevator animation
	UINT anim_elevatortrim;     // handle for elevator trim animation
	UINT anim_laileron;         // handle for left aileron animation
	UINT anim_raileron;         // handle for right aileron animation

	SURFHANDLE insignia_tex;    // vessel-specific fuselage markings
	SURFHANDLE contrail_tex;    // contrail particle texture
	SURFHANDLE vctex;
	MESHHANDLE exmesh_tpl;      // vessel mesh: global template
	MESHHANDLE vcmesh_tpl;      // VC mesh: global template
	MESHHANDLE panelmesh0;      // 2D main panel
	MESHHANDLE panelmesh1;      // 2D overhead panel
	DEVMESHHANDLE exmesh;       // vessel mesh: instance
	DEVMESHHANDLE vcmesh;       // VC mesh: instance
	THGROUP_HANDLE thg_main;
	THGROUP_HANDLE thg_retro;
	THGROUP_HANDLE thg_hover;

	enum {CAM_GENERIC, CAM_PANELMAIN, CAM_PANELUP, CAM_PANELDN, CAM_VCPILOT, CAM_VCPSNGR1, CAM_VCPSNGR2, CAM_VCPSNGR3, CAM_VCPSNGR4} campos;

	BEACONLIGHTSPEC beacon[8];                   // light beacon definitions

	double GetThrusterFlowRate(THRUSTER_HANDLE th);  // D. Beachy: get thruster flow rate in kg/s

	// subsystem access functions
	inline MainRetroSubsystem *SubsysMainRetro() { return ssys_mainretro; }
	inline ScramSubsystem *SubsysScram() { return ssys_scram; }
	inline GearSubsystem *SubsysGear() { return ssys_gear; }
	inline DockingCtrlSubsystem *SubsysDocking() { return ssys_docking; }
	inline AerodynCtrlSubsystem *SubsysAerodyn() { return ssys_aerodyn; }
	inline PressureSubsystem *SubsysPressure() { return ssys_pressurectrl; }
	inline ThermalSubsystem *SubsysThermal() { return ssys_thermal; }
	inline LightCtrlSubsystem *SubsysLights() { return ssys_light; }

	// script interface-related methods
	int Lua_InitInterpreter (void *context);
	int Lua_InitInstance (void *context);

private:
	void ApplySkin();                            // apply custom skin
	void PaintMarkings (SURFHANDLE tex);         // paint individual vessel markings

	// Vessel subsystems -------------------------------------------------------------
	HUDControl           *ssys_hud;              // HUD control system
	MainRetroSubsystem   *ssys_mainretro;        // main engine gimbal control system
	HoverSubsystem       *ssys_hoverctrl;        // hover engine control system
	RcsSubsystem         *ssys_rcs;              // reaction control subsystem
	ScramSubsystem       *ssys_scram;            // scramjet module (NULL = none)
	AerodynCtrlSubsystem *ssys_aerodyn;          // aerodynamic control subsystem
	GearSubsystem        *ssys_gear;             // landing gear control subsystem
	PressureSubsystem    *ssys_pressurectrl;     // pressure control system
	ThermalSubsystem     *ssys_thermal;          // cooling subsystem
	DockingCtrlSubsystem *ssys_docking;          // docking control subsystem
	AvionicsSubsystem    *ssys_avionics;         // avionics subsystem
	MfdSubsystem         *ssys_mfd[2];           // MFD instruments
	LightCtrlSubsystem   *ssys_light;            // cockpit/external light controls
	FailureSubsystem     *ssys_failure;          // failure/warning controls

	bool bGearIsDown;                            // touchdown point state flag
	int modelidx;                                // flight model index
	int tankconfig;                              // 0=rocket fuel only, 1=scramjet fuel only, 2=both
	double max_rocketfuel;                       // max capacity for rocket and scramjet fuel
	VISHANDLE visual;                            // handle to DG visual representation
	SURFHANDLE skin[3];                          // custom skin textures, if applicable
	MESHHANDLE hPanelMesh;                       // 2-D instrument panel mesh handle
	char skinpath[32];                           // skin directory, if applicable
	PROPELLANT_HANDLE ph_main, ph_rcs;           // propellant resource handles
	THRUSTER_HANDLE th_main[2];                  // main engine handles
	THRUSTER_HANDLE th_retro[2];                 // retro engine handles
	THRUSTER_HANDLE th_hover[3];                 // hover engine handles
	double th_main_level;                        // mean thruster main level
	AIRFOILHANDLE hwing;                         // airfoil handle for wings
	CTRLSURFHANDLE hlaileron, hraileron;         // control surface handles
	int panelcol;                                // panel light colour index, 0=default

	int mainflowidx[2], retroflowidx[2], hoverflowidx, scflowidx[2];
	int mainTSFCidx, scTSFCidx[2];

	std::unique_ptr<DlgControl> dlg_ctrl;
};

// ==============================================================

typedef struct {
	HINSTANCE hDLL;
	DWORD col[4];
	oapi::Pen *pen[2];
	SURFHANDLE surf;
} GDIParams;

// Panel area identifiers:
// Panel 0
#define AID_MAINDISP1           32 // obsolete
#define AID_MAINDISP2           33 // obsolete
#define AID_MAINDISP3           34 // obsolete
#define AID_SCRAMDISP2          36

// Panel 1
#define AID_SCRAMTEMPDISP      114

// Virtual cockpit-specific area identifiers:
#define AID_MFD1_PWR           300
#define AID_MFD2_PWR           301

#endif // !__DELTAGLIDER_H