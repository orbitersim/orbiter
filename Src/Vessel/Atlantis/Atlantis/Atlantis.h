// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Atlantis
//                  Part of the ORBITER SDK
//
// Atlantis.h
// Class interface of Atlantis (Space Shuttle) vessel class
// module and associated subclasses (SRB, tank)
// ==============================================================

#ifndef __ATLANTIS_H
#define __ATLANTIS_H

#include "Orbitersdk.h"
#include <math.h>

#ifdef ATLANTIS_TANK_MODULE
#define TANKFUNC DLLEXPORT
#else
#define TANKFUNC DLLIMPORT
#endif

#ifdef ATLANTIS_SRB_MODULE
#define SRBFUNC DLLEXPORT
#else
#define SRBFUNC DLLIMPORT
#endif

// ==========================================================
// Some Orbiter-related parameters
// ==========================================================

const double ORBITER_EMPTY_MASS = 77564.3;
// Orbiter empty mass [kg]

const double ORBITER_MAX_PROPELLANT_MASS = 11284.23 + 2162.622;
// Amount of fuel the orbiter can hold in internal OMS tanks

const double ORBITER_MAIN_THRUST = 2170732.15; // 1668652.0 * 1.25;
// Vacuum thrust rating per main engine [N] (x3 for total)
// assuming vacuum thrust is 5/4 liftoff thrust

const double ORBITER_OMS_THRUST = 26700.0;
// Vacuum thrust per unit for Orbital Maneuvering System [N] (x2 for total)

const double ORBITER_RCS_THRUST = 7740.0;
// Vacuum thrust rating for attitude thrusters (Reaction Control System) [N]

const double ORBITER_MAIN_ISP0 = 453 * 9.80665;
const double ORBITER_MAIN_ISP1 = 363 * 9.80665;
// Vacuum and sea-level fuel-specific impulse for orbiter main engines [m/s]
// using H2/O2 (hydrogen/oxygen)

const double ORBITER_OMS_ISP0 = 316 * 9.80665;
const double ORBITER_OMS_ISP1 = ORBITER_OMS_ISP0*0.75;
// Vacuum and sea-level fuel-specific impulse for Orbital Maneuvering System [m/s]
// using MMH/N2O4 (monomethyl hydrazine/nitrogen tetroxide)

const double ORBITER_RCS_ISP0 = ORBITER_OMS_ISP0;
const double ORBITER_RCS_ISP1 = ORBITER_RCS_ISP0*0.75;
// Vacuum and sea-level fuel-specific impulse for Reaction Control System [m/s]

const double GEAR_OPERATING_SPEED = 0.3;
// Opening/closing speed of landing gear (1/sec)
// => gear cycle ~ 3 sec

const double DOOR_OPERATING_SPEED = 0.007353;
// Opening/closing speed of payload bay doors (1/sec)
// This contains the door opening sequence (63 sec for each door) and an
// interval of 10 sec between the two door operations

const double RAD_OPERATING_SPEED = 0.025;
// Deployment/stowing speed of radiators (1/sec)
// => radiator cycle = 40 sec

const double RADLATCH_OPERATING_SPEED = 0.2;
// Release/engaging speed of radiator latches (1/sec)
// => radiator latch cycle = 5 sec

const double KU_OPERATING_SPEED = 0.0435;
// Deployment speed of the Ku Band antenna (1/sec)
// cycle is 23 sec

const double SPEEDBRAKE_OPERATING_SPEED = 0.20284;
// Deployment speed of the speedbrake (1/sec)
// cycle is 4.93 sec

const double ARM_OPERATING_SPEED = 0.005;
// RMS arm joint rotation speed (rad/sec)

const VECTOR3 ORBITER_CS = {234.8,389.1,68.2};
// Orbiter cross sections (projections into principal axes) [m^2]

const VECTOR3 ORBITER_CS_GEAR = {10.0,0.0,3.0};
// Contribution of fully extended landing gear to cross sections

const double MAX_GRAPPLING_DIST = 0.5;
// max distance between RMS tip and grappling point for successful grappling

// ==========================================================
// Some Tank-related parameters
// ==========================================================

const double LOX_MAX_PROPELLANT_MASS = 624252.0;
const double LH2_MAX_PROPELLANT_MASS = 104463.23;
const double TANK_MAX_PROPELLANT_MASS = LOX_MAX_PROPELLANT_MASS + LH2_MAX_PROPELLANT_MASS;
// Main tank propellant mass [kg]

const double TANK_EMPTY_MASS = 29937.0;
// Main tank empty mass (assume light-weight tank, STS-6 to STS-90)

// ==========================================================
// Some SRB-related parameters
// ==========================================================

const double SRB_MAX_PROPELLANT_MASS = 501673.161;
// SRB propellant mass [kg]

const double SRB_EMPTY_MASS = 87603.65;
// SRB empty mass [kg]

const double SRB_ISP0 = 2638.89;
const double SRB_ISP1 = 2368.79;
// SRB vacuum and sea-level fuel-specific impulse [m/s]

const double SRB_THRUST = 1.41e7; // 14679131.3;
// Mean vacuum SRB thrust per unit [N]

//const double SRB_THRUST_MAX = 1.7070e+07;
// Peak vacuum SRB thrust per unit [N]

const double MAX_ATT_LAUNCH = 1e5;
const double MAX_ROLL_SRB = 2.5e5;
// Max attitude thrust during launch phase (temporary)

const double SRB_STABILISATION_TIME = 4.0;
// MET: -SRB ignition

const double SRB_SEPARATION_TIME = 126.0;
// MET: SRB separation

const double SRB_CUTOUT_TIME = 135.0;
// MET: engine shutdown

const double SRB_GIMBAL_SPEED = 0.5;

// ==========================================================
// Thruster reference positions and thrust directions
// ==========================================================

const VECTOR3 THRUSTREF_SSME0      = {-1.55,-0.37,-12.5};
const VECTOR3 THRUSTREF_SSME1      = { 1.55,-0.37,-12.5};
const VECTOR3 THRUSTREF_SSME2      = { 0.0,  2.7, -12.5};
const VECTOR3 THRUSTREF_OMSL       = {-2.6,  3.3, -12.8};
const VECTOR3 THRUSTREF_OMSR       = { 2.6,  3.3, -12.8};
const VECTOR3 THRUSTREF_SRB        = { 0.0,  0.0, -20.4};

const double THRUSTPITCH_LAUNCH = -2.3*RAD;
const VECTOR3 THRUSTGIMBAL_LAUNCH = {0, sin(THRUSTPITCH_LAUNCH), cos(THRUSTPITCH_LAUNCH)};

const VECTOR3 THRUSTDIR_OMSL = { 0.19299542, -0.24495572, 0.95013129};
const VECTOR3 THRUSTDIR_OMSR = {-0.19299542, -0.24495572, 0.95013129};

// ==========================================================
// Docking port position
// ==========================================================

const VECTOR3 ORBITER_DOCKPOS      = { 0.0, 2.40, 10.15};

// ==========================================================
// panel area identifiers
// ==========================================================

// define MFD function buttons
#define AID_CDR1_BUTTONS   1
#define AID_CDR2_BUTTONS   2
#define AID_PLT1_BUTTONS   3
#define AID_PLT2_BUTTONS   4
#define AID_MFD1_BUTTONS   5
#define AID_MFD2_BUTTONS   6
#define AID_MFD3_BUTTONS   7
#define AID_MFD4_BUTTONS   8
#define AID_MFD5_BUTTONS   9
#define AID_MFDA_BUTTONS  10
// D. Beachy: define power buttons
#define AID_CDR1_PWR      11
#define AID_CDR2_PWR      12
#define AID_PLT1_PWR      13
#define AID_PLT2_PWR      14
#define AID_MFD1_PWR      15
#define AID_MFD2_PWR      16
#define AID_MFD3_PWR      17
#define AID_MFD4_PWR      18
#define AID_MFD5_PWR      19
#define AID_MFDA_PWR      20
// MFD brightness buttons
#define AID_CDR1_BRT      21
#define AID_CDR2_BRT      22
#define AID_PLT1_BRT      23
#define AID_PLT2_BRT      24
#define AID_MFD1_BRT      25
#define AID_MFD2_BRT      26
#define AID_MFD3_BRT      27
#define AID_MFD4_BRT      28
#define AID_MFD5_BRT      29
#define AID_MFDA_BRT      30
// Panel R13L (payload bay operations)
#define AID_R13L_MIN     100
#define AID_R13L         100
#define AID_R13L_TKBK1   101
#define AID_R13L_TKBK2   102
#define AID_R13L_TKBK3   103
#define AID_R13L_TKBK4   104
#define AID_R13L_TKBK5   105
#define AID_R13L_TKBK6   106
#define AID_R13L_MAX     120

typedef struct {
	HINSTANCE hDLL;
	SURFHANDLE tkbk_label;
	oapi::Font *font[1];
	oapi::Brush* brush[1];
} GDIParams;

class Atlantis_Tank;
class Atlantis;

class AtlantisDialog: public ImGuiDialog {
	Atlantis *m_atlantis;
public:
	AtlantisDialog(Atlantis *);
	void OnDraw() override;
};

class RMSDialog: public ImGuiDialog {
	Atlantis *m_atlantis;
public:
	RMSDialog(Atlantis *);
	void OnDraw() override;
};

// ==========================================================
// Interface for derived vessel class: Atlantis
// ==========================================================

class Atlantis: public VESSEL4 {
	friend class AscentAP;
	friend class PayloadBayOp;
	friend class RMSDialog;
	friend class AtlantisDialog;

public:
	AnimState::Action gear_status, spdb_status;
	Atlantis (OBJHANDLE hObj, int fmodel);
	~Atlantis();

	AscentAP *AscentAutopilot() { return ascap; }
	int RegisterAscentApMfd();

	void SeparateBoosters (double srb_time);
	// Jettison both SRBs from ET

	void SeparateTank ();
	// Jettison ET from orbiter

	void AttachChildWithMass(OBJHANDLE hChild, ATTACHMENTHANDLE attachment, ATTACHMENTHANDLE child_attachment);
	// attachment including mass reconfiguration

	void DetachChildWithMass(ATTACHMENTHANDLE attachment, double vel = 0.0);
	// detachment including mass reconfiguration

	void GetSSMEGimbalPos (int which, double &pitch, double &yaw);
	void GetSRBGimbalPos (int which, double &pitch, double &yaw);

	void SeparateMMU (void);
	void ToggleGrapple (void);
	void ToggleArrest (void);
	void SetGearParameters (double state);
	void Jettison ();
	void UpdateMesh ();
	void SetBayDoorPosition (double pos);
	void SetRadiatorPosition (double pos);
	void SetRadLatchPosition (double pos) {}
	void SetKuAntennaPosition (double pos);
	void SetSSMEPosition (double pos);
	void OperateLandingGear (AnimState::Action action);
	void RevertLandingGear ();
	void OperateSpeedbrake (AnimState::Action action);
	void RevertSpeedbrake ();
	void SetAnimationArm (UINT anim, double state);

	double GetSRBThrustLevel (int which);
	// returns the thrust level from left (which=0) or right (which=1)
	// SRB, or 0 if SRB is detached

	void RegisterVC_CdrMFD ();
	void RegisterVC_PltMFD ();
	void RegisterVC_CntMFD ();
	void RegisterVC_AftMFD ();
	void RedrawPanel_MFDButton (SURFHANDLE surf, int mfd);

	void OpenRMSDlg();

	int status; // 0=launch configuration
	            // 1=SRB's engaged
	            // 2=SRB's separated
	            // 3=Tank separated (orbiter only)

	double t0;          // reference time: designated liftoff time
	WORD srb_id1, srb_id2;

	double gear_proc; // landing gear deployment state (0=retracted, 1=deployed)
	//double kubd_proc; // Ku-band antenna deployment state (0=retracted, 1=deployed)
	double spdb_proc; // Speedbrake deployment state (0=retracted, 1=deployed)
	double ldoor_drag, rdoor_drag; // drag components from open cargo doors
	bool center_arm;
	bool arm_moved, arm_scheduled;
	double center_arm_t;
	bool do_eva;
	bool do_plat;
	bool do_cargostatic;
	VECTOR3 ofs_sts_sat;
	VECTOR3 cargo_static_ofs;
	VISHANDLE vis;      // handle for visual - note: we assume that only one visual per object is created!
	MESHHANDLE hOrbiterMesh, hOrbiterCockpitMesh, hOrbiterVCMesh; // mesh handles
	char cargo_static_mesh_name[256];
	ATTACHMENTHANDLE sat_attach, rms_attach;
	VECTOR3 arm_tip[3];

	// Overloaded callback functions
	void clbkSetClassCaps (FILEHANDLE cfg);
	void clbkSetStateEx (const void *status);
	void clbkLoadStateEx (FILEHANDLE scn, void *vs);
	void clbkSaveState (FILEHANDLE scn);
	void clbkPostCreation ();
	void clbkFocusChanged (bool getfocus, OBJHANDLE hNewVessel, OBJHANDLE hOldVessel);
	void clbkPreStep (double simt, double simdt, double mjd);
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	int  clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);
	void clbkVisualCreated (VISHANDLE vis, int refcount);
	void clbkVisualDestroyed (VISHANDLE vis, int refcount);
	void clbkAnimate (double simt);
	void clbkMFDMode (int mfd, int mode);
	bool clbkLoadGenericCockpit ();
	bool clbkLoadVC (int id);
	bool clbkVCMouseEvent (int id, int event, VECTOR3 &p);
	bool clbkVCRedrawEvent (int id, int event, SURFHANDLE surf);
	bool clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp);

	PayloadBayOp *plop; // control and status of payload bay operations

protected:
	void LoadMeshes();
	// Load the meshes for cockpit and exterior

	void CreateSSME();
	// Initialise the thrusters for the shuttle main engines

	void CreateOMS();
	// Initialise the thrusters for the orbital maneuvering system

	void CreateRCS();
	// Initialise the thrusters for the reaction control system

	void CreateAirfoils();
	// Initialise airfoils, aerodynamic control surfaces and drag elements

	static void VLiftCoeff (double aoa, double M, double Re, double *cl, double *cm, double *cd);
	static void HLiftCoeff (double beta, double M, double Re, double *cl, double *cm, double *cd);
	// airfoil coefficient functions

	void DefineAnimations (void);
	// Initialises all animation objects

private:
	void SetSSMEGimbal (const VECTOR3 &angle);
	// Set the gimbal positions for the shuttle main engines

	double GetAscentPitchRate (double tgt_pitch);
	// Autopilot function: returns the pitch rate for the specified
	// target pitch during launch phase (until ET separation)

	void AutoGimbal (const VECTOR3 &tgt_rate);
	// Automatic gimbal adjustment for SSME and SRB engines to correct
	// for CG shift, SRB thrust variations, atmospheric effects, etc.

	void AutoRCS (const VECTOR3 &tgt_rate);
	// Automatic RCS control for ascent autopilot

	void LaunchClamps();
	bool EnableSSME (bool enable);
	void EnableOMS (bool enable);
	void EnableRCS (int mode);

	bool SatGrappled() const { return GetAttachmentStatus (rms_attach) != 0; }
	bool SatStowed() const { return GetAttachmentStatus (sat_attach) != 0; }
	ATTACHMENTHANDLE CanArrest() const;

	AscentAP *ascap;                           // ascent autopilot
	int ascapMfdId;                            // ID for ascent autopilot MFD mode
	Atlantis_Tank *pET;                        // pointer to ET object (if attached)
	DOCKHANDLE hDockET;                        // docking connector to ET

	VECTOR3 gimbal_pos;                        // gimbal settings for pitch,yaw,roll

	UINT anim_door;                            // handle for cargo door animation
	UINT anim_rad;                             // handle for radiator animation
	UINT anim_gear;                            // handle for landing gear animation
	UINT anim_kubd;                            // handle for Ku-band antenna animation
	UINT anim_elev;                            // handle for elevator animation
	UINT anim_laileron;						   // handle for left aileron animation
	UINT anim_raileron;						   // handle for right aileron animation
	UINT anim_rudder;						   // handle for rudder animation
	UINT anim_spdb;                            // handle for speed brake animation
	UINT anim_ssme;                            // handle for SSME pitch gimbal animation
	UINT mesh_orbiter;                         // index for orbiter mesh
	UINT mesh_cockpit;                         // index for cockpit mesh for external view
	UINT mesh_vc;                              // index for virtual cockpit mesh
	UINT mesh_cargo;                           // index for static cargo mesh
	UINT mesh_platform;                        // index for payload platform mesh
	PROPELLANT_HANDLE ph_oms;                  // handles for propellant resources
	THRUSTER_HANDLE th_main[3];                // handles for orbiter main engines
	THRUSTER_HANDLE th_oms[2];                 // handles for orbiter oms engines
	THGROUP_HANDLE thg_main, thg_oms;          // handles for thruster groups

	// RMS arm animation status
	ANIMATIONCOMPONENT_HANDLE hAC_arm, hAC_sat, hAC_satref;
	MGROUP_TRANSFORM *rms_anim[6];
	MGROUP_TRANSFORM *ssme_anim[3];
	UINT anim_arm_sy, anim_arm_sp, anim_arm_ep, anim_arm_wp, anim_arm_wy, anim_arm_wr;
	double arm_sy, arm_sp, arm_ep, arm_wp, arm_wy, arm_wr;
	double launchelev;                         // elevation of launch stack on the ground [m]

	MGROUP_TRANSFORM *sat_anim, *sat_ref;

	bool bManualSeparate; // flag for user-induced booster or tank separation
	bool reset_sat;
	OBJHANDLE hMMU, hSAT;
	bool render_cockpit;
	double mfdbright[10];

	LightEmitter *engine_light;
	double engine_light_level;

	std::unique_ptr<RMSDialog> rmsDlg;
	std::unique_ptr<AtlantisDialog> ctlDlg;
};

// ==========================================================
// Interface for derived vessel class: Atlantis_SRB
// ==========================================================

class SRBFUNC Atlantis_SRB: public VESSEL2 {
public:
	Atlantis_SRB (OBJHANDLE hObj);
	// Construct interface from existing object

	void SetLaunchElevation (double elev);
	double ThrustProfile (double met);
	double GetThrustLevel () const;

	bool Ignite ();
	void FireBolt ();
	void SetThrustGimbal (const VECTOR3 &dir);
	void CmdThrustGimbal (const VECTOR3 &dir);
	VECTOR3 GetThrustGimbal ();

	// Overloaded callback functions
	void clbkSetClassCaps (FILEHANDLE cfg);
	void clbkPostStep (double simt, double simdt, double mjd);
	void clbkPostCreation ();
	void clbkLoadStateEx (FILEHANDLE scn, void *vs);
	void clbkSaveState (FILEHANDLE scn);

private:
	double launchelev;          // elevation above ground at launch [m]
	double t0;                  // reference time: liftoff
	double tsep;                // simulation time at which SRB separation was initiated
	VECTOR3 gimbalcmd;          // commanded gimbal direction
	bool bGimbalCmd;            // is gimbal command active?
	bool bMainEngine;           // main engine firing?
	bool bSeparationEngine;     // separation thrusters firing?
	enum SRBPosition { SRB_UNDEFINED, SRB_LEFT, SRB_RIGHT } srbpos;
	MESHHANDLE hSRBMesh;
	PROPELLANT_HANDLE ph_main;  // handle for propellant resource
	THRUSTER_HANDLE th_main;    // engine handle
	THRUSTER_HANDLE th_bolt;    // separation bolt
};

// ==========================================================
// Interface for derived vessel class: Atlantis_Tank
// ==========================================================

class TANKFUNC Atlantis_Tank: public VESSEL2 {
	friend class Atlantis;

public:
	Atlantis_Tank (OBJHANDLE hObj);
	// Construct interface from existing object

	PROPELLANT_HANDLE GetPropHandle() { return hProp; }

	double GetMainPropellantMass () const;

	void SetSRBLaunchElevation (double elev) const;
	double GetSRBThrustLevel (int which) const;
	void SetSRBGimbal (const VECTOR3 &angle) const;
	VECTOR3 GetSRBThrustDir (int which) const;

	bool IgniteSRBs () const;
	void SeparateSRBs ();

	// Overloaded callback functions
	void clbkSetClassCaps (FILEHANDLE cfg);
	void clbkPreStep (double simt, double simdt, double mjd);
	void clbkPostStep (double simt, double simdt, double mjd);

protected:
	Atlantis_SRB *GetSRB (int which) const;

	// enable docking port for connecting orbiter
	void EnableOrbiterConnector ();

private:
	MESHHANDLE hTankMesh;      // mesh handle
	PROPELLANT_HANDLE hProp;   // propellant resource handle
	DOCKHANDLE hDockOrbiter;   // connector to shuttle orbiter

	Atlantis *pAtlantis;
	Atlantis_SRB *pSRB[2];
};

#endif // !__ATLANTIS_H
