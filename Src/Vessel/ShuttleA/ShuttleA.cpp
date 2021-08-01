// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE:  ShuttleA
//                  Part of the ORBITER SDK
//
// ShuttleA.cpp
// Reference implementation of Shuttle-A vessel class module
// ==============================================================

#define STRICT 1
#define ORBITER_MODULE

#include "ShuttleA.h"
#include "ScnEditorAPI.h"
#include "DlgCtrl.h"
#include "attref.h"
#include "mfdbutton.h"
#include "navbutton.h"
#include "hudbutton.h"
#include "rcsswitch.h"
#include "airlockswitch.h"
#include "dockcvrswitch.h"
#include "gearswitch.h"
#include "payloadctrl.h"
#include "sliderpair.h"
#include "needlepair.h"
#include "adiball.h"
#include "adictrl.h"
#include "auxpodctrl.h"
#include "InstrVs.h"
#include "resource.h"
#include <math.h>
#include <stdio.h>

#define LOADBMP(id) (LoadBitmap (g_Param.hDLL, MAKEINTRESOURCE (id)))

// ==============================================================
// Global parameters
// ==============================================================

GDIParams g_Param;

static const int ntdvtx = 16;
static TOUCHDOWNVTX tdvtx[ntdvtx] = {
	{_V(-3  ,-3.05, 12.5), 3.5e6, 3.5e5, 3},
	{_V(-3  ,-3.05,-13.5), 3e6,   3e5,   3},
	{_V( 3  ,-3.05,-13.5), 3e6,   3e5,   3},
	{_V( 3  ,-3.05, 12.5), 3.5e6, 3.5e5, 3},
	{_V(-7.7, 0   ,-0.4 ), 3e7,   3e6,   3},
	{_V( 7.7, 0   ,-0.4 ), 3e7,   3e6,   3},
	{_V(-1.5, 3   ,13.5 ), 3e7,   3e6,   3},
	{_V( 1.5, 3   ,13.5 ), 3e7,   3e6,   3},
	{_V(-1.3, 2.8 ,17   ), 3e7,   3e6,   3},
	{_V( 1.3, 2.8 ,17   ), 3e7,   3e6,   3},
	{_V(-1.8, 0   ,18.3 ), 3e7,   3e6,   3},
	{_V( 1.8, 0   ,18.3 ), 3e7,   3e6,   3},
	{_V(-1.9, 2.2 ,-13.8), 3e7,   3e6,   3},
	{_V( 1.9, 2.2 ,-13.8), 3e7,   3e6,   3},
	{_V(-3.3, 0   ,-14.9), 3e7,   3e6,   3},
	{_V( 3.3, 0   ,-14.9), 3e7,   3e6,   3}
};

// ==============================================================
// Airfoil definition
// ==============================================================

void Shuttle_MomentCoeff (double aoa,double M,double Re,double *cl,double *cm,double *cd)
{
	int i;
	const int nabsc = 7;
	static const double AOA[nabsc] = {-180*RAD, -90*RAD,-30*RAD, 0*RAD, 60*RAD,90*RAD,180*RAD};
	static const double CL[nabsc]  = {       0,      0,   -0.004,     0,     0.008,     0,      0};
	static const double CM[nabsc]  = {       0,      0,   0.0014,  0,-0.0012,     0,      0};
	
	for (i = 0; i < nabsc-1 && AOA[i+1] < aoa; i++);
	double f = (aoa-AOA[i]) / (AOA[i+1]-AOA[i]);
	*cl = CL[i] + (CL[i+1]-CL[i]) * f;  // aoa-dependent lift coefficient
	*cm = CM[i] + (CM[i+1]-CM[i]) * f;  // aoa-dependent moment coefficient
	double saoa = sin(aoa);
	double pd = 0.045 + 0.4*saoa*saoa;  // profile drag
	*cd = pd + oapiGetInducedDrag (*cl, 0.1,0.7) + oapiGetWaveDrag (M, 0.75, 1.0, 1.1, 0.04);
	// profile drag + (lift-)induced drag + transonic/supersonic wave (compressibility) drag
}
// ==============================================================
// Specialised vessel class ShuttleA
// ==============================================================

// --------------------------------------------------------------
// Static member initialisation
// --------------------------------------------------------------
SURFHANDLE ShuttleA::panel2dtex = NULL;
SURFHANDLE ShuttleA::paneleltex = NULL;
SURFHANDLE ShuttleA::aditex = NULL;

// --------------------------------------------------------------
// Constructor
// --------------------------------------------------------------
ShuttleA::ShuttleA (OBJHANDLE hObj, int fmodel)
: VESSEL4 (hObj, fmodel)
{
	int i;

	attref = new AttitudeReference (this);
	dock_proc = 0.0;
	dock_status = DOOR_CLOSED;
	for (i = 0; i < 2; i++) {
		lock_status[i] = DOOR_CLOSED;
		lock_proc[i] = 0.0;
	}
	gear_proc = 0.0;
	gear_status = DOOR_CLOSED;
	DefineAnimations ();
	for (i = 0; i < nsurf; i++)
		srf[i] = 0;
	for (i = 0; i < 2; i++) {
		pod_angle[i] = pod_angle_request[i] = 0.0;
	};
	for (i = 0; i < 6; i++) {
		cargo_open[i]=0;	//not opened. not jettisoned
		cargo_arm_status = 0;	//not armed
	}
	hPanelMesh = NULL;
	npel = 0;
}

// --------------------------------------------------------------
// Destructor
// --------------------------------------------------------------
ShuttleA::~ShuttleA ()
{
	int i;
	delete attref;
	ReleaseSurfaces();
	if (hPanelMesh0) oapiDeleteMesh (hPanelMesh0);
	for (i = 0; i < npel; i++)
		delete pel[i];
}

// --------------------------------------------------------------
// Define animation sequences for moving parts
// --------------------------------------------------------------
void ShuttleA::DefineAnimations ()
{
	static UINT LeftPodGrp[4] = {57,58,59,60};
	static MGROUP_ROTATE leftpod(0,LeftPodGrp,4,_V(0,0,0),_V(1,0,0),(float)PI);
	
	static UINT RightPodGrp[4] = {61,62,63,64};
	static MGROUP_ROTATE rightpod(0,RightPodGrp,4,_V(0,0,0),_V(1,0,0),(float)PI);

	
	

	// Register animation for hover/retro pods
	anim_pod[0] = CreateAnimation (0);
	AddAnimationComponent (anim_pod[0], 0.0f,1.0f, &leftpod);
	anim_pod[1] = CreateAnimation (0);
	AddAnimationComponent (anim_pod[1], 0.0f,1.0f,&rightpod);


	static UINT UpperDockHatch = 47;
	static MGROUP_ROTATE upperhatch(0,&UpperDockHatch,1,_V(0,0.554f,18.317f),_V(-1,0,0),(float)PI);
	static UINT LowerDockHatch = 48;
	static MGROUP_ROTATE lowerhatch(0,&LowerDockHatch,1,_V(0,-0.544f,18.317f),_V(1,0,0),(float)PI);
	anim_dock = CreateAnimation (0);
	AddAnimationComponent (anim_dock,0.0f,1.0f, &upperhatch);
	AddAnimationComponent (anim_dock,0.2f,1.0f,  &lowerhatch);

	static UINT OuterAirlock = 21;
	static MGROUP_ROTATE outerairlock(0,&OuterAirlock,1,_V(0,0.495f,18.252f),_V(1,0,0),(float)(0.4f*PI));

	// outer airlock
	anim_lock[0] = CreateAnimation (0);
	AddAnimationComponent (anim_lock[0], 0.0f ,1.0f,&outerairlock);
	
	// inner airlock - dummy
	anim_lock[1] = CreateAnimation (0);

	//Gear animation
	static UINT GEAR_left_leg[3]={67,70,79};
	static UINT GEAR_right_leg[3]={73,74,82};

	static UINT GEAR_left_leg_front_p1=65;
	static UINT GEAR_left_leg_front_p2=66;
	static UINT GEAR_left_leg_mid_p1=68;
	static UINT GEAR_left_leg_mid_p2=69;
	static UINT GEAR_left_leg_back_p1=77;
	static UINT GEAR_left_leg_back_p2=78;

	static MGROUP_TRANSLATE MGEAR_left_leg_first (0, GEAR_left_leg, 3, _V(0.194,0.224,0.0));
	static MGROUP_TRANSLATE MGEAR_left_leg_second (0, GEAR_left_leg, 3, _V(0.091,0.331,0.0));
	static MGROUP_TRANSLATE MGEAR_right_leg_first (0, GEAR_right_leg, 3, _V(-0.194,0.224,0.0));
	static MGROUP_TRANSLATE MGEAR_right_leg_second (0, GEAR_right_leg, 3, _V(-0.091,0.331,0.0));

	static MGROUP_ROTATE MGEAR_left_leg_front_p1(0,&GEAR_left_leg_front_p1,1,_V(1.655f,-1.942f,0.0f),_V(0,0,1),0.9948f);
	static MGROUP_ROTATE MGEAR_left_leg_front_p2(0,&GEAR_left_leg_front_p2,1,_V(1.112f,-1.718f,0.0f),_V(0,0,1),0.5235f);
	static MGROUP_ROTATE MGEAR_left_leg_mid_p1(0,&GEAR_left_leg_mid_p1,1,_V(3.007f,-1.942f,0.0f),_V(0,0,1),0.9948f);
	static MGROUP_ROTATE MGEAR_left_leg_mid_p2(0,&GEAR_left_leg_mid_p2,1,_V(2.464f,-1.718f,0.0f),_V(0,0,1),0.5235f);
	static MGROUP_ROTATE MGEAR_left_leg_back_p1(0,&GEAR_left_leg_back_p1,1,_V(2.49f,-1.942f,0.0f),_V(0,0,1),0.9948f);
	static MGROUP_ROTATE MGEAR_left_leg_back_p2(0,&GEAR_left_leg_back_p2,1,_V(1.947f,-1.718f,0.0f),_V(0,0,1),0.5235f);

	static UINT GEAR_right_leg_front_p1=71;
	static UINT GEAR_right_leg_front_p2=72;
	static UINT GEAR_right_leg_mid_p1=75;
	static UINT GEAR_right_leg_mid_p2=76;
	static UINT GEAR_right_leg_back_p1=80;
	static UINT GEAR_right_leg_back_p2=81;

	static MGROUP_ROTATE MGEAR_right_leg_front_p1(0,&GEAR_right_leg_front_p1,1,_V(-1.655f,-1.942f,0.0f),_V(0,0,-1),0.9948f);
	static MGROUP_ROTATE MGEAR_right_leg_front_p2(0,&GEAR_right_leg_front_p2,1,_V(-1.112f,-1.718f,0.0f),_V(0,0,-1),0.5235f);
	static MGROUP_ROTATE MGEAR_right_leg_mid_p1(0,&GEAR_right_leg_mid_p1,1,_V(-3.007f,-1.942f,0.0f),_V(0,0,-1),0.9948f);
	static MGROUP_ROTATE MGEAR_right_leg_mid_p2(0,&GEAR_right_leg_mid_p2,1,_V(-2.464f,-1.718f,0.0f),_V(0,0,-1),0.5235f);
	static MGROUP_ROTATE MGEAR_right_leg_back_p1(0,&GEAR_right_leg_back_p1,1,_V(-2.49f,-1.942f,0.0f),_V(0,0,-1),0.9948f);
	static MGROUP_ROTATE MGEAR_right_leg_back_p2(0,&GEAR_right_leg_back_p2,1,_V(-1.947f,-1.718f,0.0f),_V(0,0,-1),0.5235f);



	anim_gear = CreateAnimation(0.0);
	AddAnimationComponent (anim_gear, 0.0f, 0.5f, &MGEAR_left_leg_first);
	AddAnimationComponent (anim_gear, 0.5f, 1.0f, &MGEAR_left_leg_second);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_left_leg_front_p1);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_left_leg_front_p2);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_left_leg_mid_p1);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_left_leg_mid_p2);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_left_leg_back_p1);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_left_leg_back_p2);

	AddAnimationComponent (anim_gear, 0.0f, 0.5f, &MGEAR_right_leg_first);
	AddAnimationComponent (anim_gear, 0.5f, 1.0f, &MGEAR_right_leg_second);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_right_leg_front_p1);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_right_leg_front_p2);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_right_leg_mid_p1);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_right_leg_mid_p2);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_right_leg_back_p1);
	AddAnimationComponent (anim_gear, 0.0f, 1.0f, &MGEAR_right_leg_back_p2);

	//********** VC animations ************

	//auxiliary thrusters
	static UINT POD_thruster_left[1] = {22};
	static UINT POD_thruster_right[1] = {30};
	static MGROUP_TRANSLATE MPOD_thruster_left (1, POD_thruster_left, 1, _V(0,0.05,0.023));
	static MGROUP_TRANSLATE MPOD_thruster_right (1, POD_thruster_right, 1, _V(0,0.05,0.023));
	
	anim_pod_thrust_left = CreateAnimation (0);
	AddAnimationComponent (anim_pod_thrust_left, 0, 1.0, &MPOD_thruster_left);
	anim_pod_thrust_right = CreateAnimation (0);
	AddAnimationComponent (anim_pod_thrust_right, 0, 1.0, &MPOD_thruster_right);
	
	//hover thrusters
	static UINT HOVER_thruster_left[1]={31};
	static UINT HOVER_thruster_right[1]={32};
	static MGROUP_TRANSLATE MHOVER_thruster_left(1,HOVER_thruster_left,1,_V(0,0.085,0.037));
	static MGROUP_TRANSLATE MHOVER_thruster_right(1,HOVER_thruster_right,1,_V(0,0.085,0.037));

	anim_hover_thrust_left= CreateAnimation(0);
	AddAnimationComponent (anim_hover_thrust_left, 0, 1.0, &MHOVER_thruster_left);
	anim_hover_thrust_right= CreateAnimation(0);
	AddAnimationComponent (anim_hover_thrust_right, 0, 1.0, &MHOVER_thruster_right);

	//main thrusters
	static UINT MAIN_thruster_left[1]={27};
	static UINT MAIN_thruster_right[1]={28};
	static MGROUP_TRANSLATE MMAIN_thruster_left (1,MAIN_thruster_left,1,_V(0,0.085,0.037));
	static MGROUP_TRANSLATE MMAIN_thruster_right (1,MAIN_thruster_right,1,_V(0,0.085,0.037));
	
	anim_main_thrust_left = CreateAnimation (0);
	AddAnimationComponent (anim_main_thrust_left, 0, 1.0, &MMAIN_thruster_left);
	anim_main_thrust_right = CreateAnimation (0);
	AddAnimationComponent (anim_main_thrust_right, 0, 1.0, &MMAIN_thruster_right);
	
	// POD angle switch
	static UINT POD_angle_switch[2]={34,35};
	static MGROUP_ROTATE MPOD_angle_switch(1,POD_angle_switch,2,_V(-0.596666398f,1.98931781f,16.28778112f),//added 0.10 to Z
															    _V(0.996194179f,0.036831321f,-0.078997542f),1.570796327f);
	anim_pod_angle= CreateAnimation(0.5);
	AddAnimationComponent(anim_pod_angle,0.0f,1.0f,&MPOD_angle_switch);

	// RCS mode switch
	static UINT RCS_mode_switch=33;
	static MGROUP_ROTATE MRCS_mode_switch(1,&RCS_mode_switch,1,_V(-0.479842445f,2.100993049f,16.32856942f),//added 0.10 to Z
																_V(0.996194179f,0.036831321f,-0.078997542f),1.570796327f);
		
	anim_rcs_mode= CreateAnimation(0.5);
	AddAnimationComponent(anim_rcs_mode,0.0f,1.0f,&MRCS_mode_switch);

	//DOCK port switch
	static UINT DOCK_switch=39;		
	static MGROUP_ROTATE MDOCK_switch (1,&DOCK_switch,1,_V(-0.212890075f,2.608840923f,16.09495988f),
														_V(0.0f,0.061554834f,-0.998103703f),1.570796327f/2.0f);
	anim_dock_switch= CreateAnimation(0.5);
	AddAnimationComponent(anim_dock_switch,0.0f,1.0f,&MDOCK_switch);

	//AIRLOCK switch
	static UINT AIRLOCK_switch=41;
	static MGROUP_ROTATE MAIRLOCK_switch (1,&AIRLOCK_switch,1,_V(-0.243815575f,2.639114618f,16.09778152f),
														_V(0.0f,0.061554834f,-0.998103703f),1.570796327f/2.0f);
	anim_airlock_switch = CreateAnimation(0.5);
	AddAnimationComponent(anim_airlock_switch,0.0f,1.0f,&MAIRLOCK_switch);

	//GEAR switch
	static UINT GEAR_switch=49;
	static MGROUP_ROTATE MGEAR_switch (1, &GEAR_switch,1, _V(-0.212890075f,2.610353215f,16.07043827f),
														_V(0.0f,0.061554834f,-0.998103703f),1.570796327f/2.0f);
	anim_gear_switch = CreateAnimation(0.5);
	AddAnimationComponent(anim_gear_switch,0.0f,1.0f,&MGEAR_switch);


	//CARGO ARM switch

	static UINT CARGO_switch=54;
	static MGROUP_ROTATE MCARGO_switch (1, &CARGO_switch,1, _V(-0.212890075f,2.616076201f,15.97764079f),
														_V(0.0f,0.061554834f,-0.998103703f),1.570796327f/2.0f);
	anim_cargo_switch = CreateAnimation(0.5);
	AddAnimationComponent(anim_cargo_switch,0.0f,1.0f,&MCARGO_switch);

}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::ReleaseSurfaces ()
{
	for (int i = 0; i < nsurf; i++)
		if (srf[i]) {
			oapiDestroySurface (srf[i]);
			srf[i] = 0;
		}
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::InitPanel (int panel)
{
	int i;

	switch (panel) {
	case -1: //VC resources
		srf[2] = oapiCreateSurface (LOADBMP (IDB_BUTTON1));
		srf[3] = oapiCreateSurface (LOADBMP (IDB_INDICATOR1));
		srf[4] = oapiCreateSurface (LOADBMP (IDB_INDICATOR2));
		srf[5] = oapiCreateSurface (LOADBMP (IDB_BUTTON3)); 

		break;
	case 0:
		srf[0] = oapiCreateSurface (LOADBMP (IDB_SLIDER1));
		srf[1] = oapiCreateSurface (LOADBMP (IDB_SWITCH1));
		srf[2] = oapiCreateSurface (LOADBMP (IDB_BUTTON1));
		
		for (i = 0; i < 2; i++) {
			sliderpos_main[i] = sliderpos_hovr[i] =
			sliderpos_retro[i] = sliderpos_auxhovr[i] = 
			sliderpos_pod[i] = (UINT)-1;
			podswitch[i] = 0;
		}
		break;
	case 1:
		srf[0] = oapiCreateSurface (LOADBMP (IDB_SWITCH2));
		srf[1] = oapiCreateSurface (LOADBMP (IDB_SWITCH3));
		srf[2] = oapiCreateSurface (LOADBMP (IDB_SWITCH4));
		srf[3] = oapiCreateSurface (LOADBMP (IDB_INDICATOR1));
		srf[4] = oapiCreateSurface (LOADBMP (IDB_INDICATOR2));
		srf[5] = oapiCreateSurface (LOADBMP (IDB_BUTTON3));
		break;
	}
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::RotatePods (UINT which, UINT mode)
{
	if (Playback())
		return false;  // disable during playback

	bool modified = false;
	UINT pod, p0 = (which & 1 ? 0 : 1), p1 = (which & 2 ? 1 : 0);

	for (pod = p0; pod <= p1; pod++) {
		if (mode) {
			double dt = oapiGetSimStep();

			if (mode == 2) { // turn retro
				pod_angle_request[pod] -= dt*POD_ROTREQUEST_SPEED;
				if (pod_angle_request[pod] < 0.0) pod_angle_request[pod] = 0.0;
			} else {         // turn forward
				pod_angle_request[pod] += dt*POD_ROTREQUEST_SPEED;
				if (pod_angle_request[pod] > PI) pod_angle_request[pod] = PI;
			}
			oapiTriggerPanelRedrawArea (0, AID_PODCTRL);
		}

		if (podswitch[pod] != mode) {
			char cbuf[256];
			if (mode == 0) {
				sprintf (cbuf, "%d SET %f", pod+1, pod_angle_request[pod]);
			} else {
				sprintf (cbuf, "%d %s", pod+1, mode==1 ? "FWD" : "BACK");
			}
			RecordEvent ("POD", cbuf);
			podswitch[pod] = mode;
			modified = true;
		}
	}
	return modified;
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::CommandPodAngle (UINT which, double angle)
{
	for (int i = 0; i < 2; i++) {
		if (which & (1<<i))
			pod_angle_request[i] = angle;
	}
	oapiTriggerPanelRedrawArea (0, AID_PODANGLEINDICATOR);

	char cbuf[256];
	sprintf (cbuf, "%d SET %f", which, angle);
	RecordEvent ("POD", cbuf);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::SetPodAngle (UINT which, double angle)
{
	for (int i = 0; i < 2; i++) {
		if (which & (1<<i)) {
			pod_angle[i] = angle;
			double sina = sin(pod_angle[i]), cosa = cos(pod_angle[i]);
			SetThrusterDir (th_pod[i], _V(0,sina,-cosa));
			SetAnimation (anim_pod[i], pod_angle[i]/PI);
		}
	}
	CommandPodAngle (which, angle);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::ActivateDockingPort (DoorStatus action)
{
	bool close = (action == DOOR_CLOSED || action == DOOR_CLOSING);
	dock_status = action;
	if (action <= DOOR_OPEN) {
		dock_proc = (action == DOOR_CLOSED ? 0.0 : 1.0);
		SetAnimation (anim_dock, dock_proc);
	}
	oapiTriggerRedrawArea (1, 0,AID_DOCKSWITCH);
	oapiTriggerRedrawArea (1, 0,AID_DOCKINDICATOR);
	RecordEvent ("DOCK", close ? "CLOSE" : "OPEN");
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RevertDockingPort ()
{
	ActivateDockingPort (dock_status == DOOR_CLOSED || dock_status == DOOR_CLOSING ?
						 DOOR_OPENING : DOOR_CLOSING);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::ActivateAirlock (int which, DoorStatus action)
{
	bool close = (action == DOOR_CLOSED || action == DOOR_CLOSING);
	lock_status[which] = action;
	if (action <= DOOR_OPEN) {
		lock_proc[which] = (action == DOOR_CLOSED ? 0.0 : 1.0);
		SetAnimation (anim_lock[which], lock_proc[which]);
	}
	oapiTriggerRedrawArea (1, 0, AID_AIRLOCK1SWITCH+which);
	oapiTriggerRedrawArea (1, 0, AID_AIRLOCK1INDICATOR+which);
	RecordEvent (which ? "IAIRLOCK":"AIRLOCK", close ? "CLOSE" : "OPEN");
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RevertAirlock (int which)
{
	ActivateAirlock (which, lock_status[which] == DOOR_CLOSED || lock_status[which] == DOOR_CLOSING ?
		             DOOR_OPENING : DOOR_CLOSING);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::ActivateLandingGear (DoorStatus action)
{
	bool close = (action == DOOR_CLOSED || action == DOOR_CLOSING);
	gear_status = action;
	if (action <= DOOR_OPEN) {
		gear_proc = (action == DOOR_CLOSED ? 0.0 : 1.0);
		SetAnimation (anim_gear, gear_proc);
	}
	oapiTriggerRedrawArea (1, 0, AID_GEARSWITCH);
	oapiTriggerRedrawArea (1, 0, AID_GEARINDICATOR);
	RecordEvent ("GEAR", close ? "UP" : "DOWN");
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RevertLandingGear ()
{
	ActivateLandingGear (gear_status == DOOR_CLOSED || gear_status == DOOR_CLOSING ?
		             DOOR_OPENING : DOOR_CLOSING);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::ActivateCargo (int status)
{
	cargo_arm_status= status;

	for (int i=0;i<6;i++)
		if (GetAttachmentStatus (payload_attachment[i]))
			cargo_open[i]=0;
		else 
			cargo_open[i]=1;

	oapiTriggerRedrawArea(1,0,AID_CARGOARMINDICATOR);
	oapiTriggerRedrawArea(1,0,AID_GARGOARMSWITCH);
	oapiTriggerRedrawArea(1,0,AID_CARGO_OPEN);
	RecordEvent ("CARGO", status ? "ARM" : "DISARM");
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::SetADILayout (int layout)
{
	if (layout >= 0 && layout < 2) {
		adiball->SetLayout (layout);
		return true;
	} else
		return false;
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::SetAttrefMode (int mode)
{
	if (mode >= 0 && mode < 6) {
		if (mode < 4) {
			attref->SetMode (mode);
		} else {
			attref->SetMode (4);
			attref->SetNavid (mode-4);
		}
		oapiTriggerRedrawArea (0,0,AID_ADICTRL);
		return true;
	} else
		return false;
}

bool ShuttleA::SetAttrefTgtMode (int mode)
{
	if (mode >= 0 && mode < 4) {
		attref->SetTgtmode (mode);
		oapiTriggerRedrawArea (0,0,AID_ADICTRL);
		return true;
	} else
		return false;
}

void ShuttleA::SetAttrefOffset (const VECTOR3 &ofs)
{
	attref->SetEulerOffset (ofs);
	oapiTriggerRedrawArea (0,0,AID_ADICTRL);
}

void ShuttleA::SetAtttgtOffset (const VECTOR3 &ofs)
{
	attref->SetTgtOffset (ofs);
	oapiTriggerRedrawArea (0,0,AID_ADICTRL);
}

bool ShuttleA::SetAttOffsetMode (int mode)
{
	if (mode >= 0 && mode < 2) {
		adictrl->settgt = (mode > 0);
		oapiTriggerRedrawArea (0,0,AID_ADICTRL);
		return true;
	} else return false;
}

bool ShuttleA::SetAtttgtFrameMode (int mode)
{
	if (mode >= 0 && mode < 2) {
		adictrl->errmode_is_local = (mode > 0);
		adiball->SetRateMode (mode > 0);
		oapiTriggerRedrawArea (0,0,AID_ADICTRL);
		return true;
	} else return false;
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::ToggleGrapple (int grapple)
{
	OBJHANDLE hV = GetAttachmentStatus (payload_attachment[grapple]);
	char cbuf[256];
	sprintf (cbuf, "GRAPPLE %d", grapple);

	if (hV) {  // release payload
		if (cargo_arm_status == 0)  return false; //jettison is not armed
		DetachChild (payload_attachment[grapple]);
		ComputePayloadMass();
		//RecordEvent ("CARGO", cbuf);
		// no need to record this, since it is already handled by the default "Detach" event
		return true;

	} else {             // grapple payload

		VECTOR3 gpos, grms, pos, dir, rot;
		VECTOR3 gdir,grot;
		VECTOR3 gcdir,gcrot;
		GetAttachmentParams (payload_attachment[grapple], pos, dir, rot);
		Local2Global (pos, grms);	//local attach point to global frame
		GlobalRot(rot,grot);
		GlobalRot(dir,gdir);
		// Search the complete vessel list for a grappling candidate.
		// Not very scalable ...
		for (DWORD i = 0; i < oapiGetVesselCount(); i++) {
			OBJHANDLE hV = oapiGetVesselByIndex (i);
			if (hV == GetHandle()) continue; // we don't want to grapple ourselves ...
			oapiGetGlobalPos (hV, &gpos);
			if (dist (gpos, grms) < oapiGetSize (hV)) { // in range
				VESSEL *v = oapiGetVesselInterface (hV);
				DWORD nAttach = v->AttachmentCount (true);
				for (DWORD j = 0; j < nAttach; j++) { // now scan all attachment points of the candidate
					ATTACHMENTHANDLE hAtt = v->GetAttachmentHandle (true, j);
					const char *id = v->GetAttachmentId (hAtt);
					if (strncmp (id, "SH", 2)) continue; // attachment point not compatible
					v->GetAttachmentParams (hAtt, pos, dir, rot);
					v->Local2Global (pos, gpos);
					if (dist (gpos, grms) < MAX_GRAPPLING_DIST) { // found one!
						
						// check if the attachment points are pointing the right way
						v->GlobalRot(rot,gcrot);
						v->GlobalRot(dir,gcdir); //should be normal by now
						if ((dotp(grot,gcrot)>MAX_GRAPPLING_ANG)&&(dotp(gdir,gcdir)<-MAX_GRAPPLING_ANG))//dotrot=1 and dotdir=-1(same up vector, but opposing dir vectors)
						{
							AttachChild (hV, payload_attachment[grapple], hAtt);
							ComputePayloadMass();
							RecordEvent ("CARGO", cbuf);
							return true;
						}
					}
				}
			}
		}
	}

	return false;
}


// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::ComputePayloadMass()
{
	OBJHANDLE hV;
	VESSEL *v;
	payload_mass=0;
	for (int i=0;i<6;i++)
	{
		hV = GetAttachmentStatus (payload_attachment[i]);
		if (hV)
		{
			v = oapiGetVesselInterface (hV);
			payload_mass+=v->GetMass();

		}
	}
	SetEmptyMass (EMPTY_MASS + payload_mass);

}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RedrawPanel_MFDButton (SURFHANDLE surf, int mfd, int side)
{
	HDC hDC = oapiGetDC (surf);
	SelectObject (hDC, g_Param.hFont[0]);
	SetTextColor (hDC, RGB(0, 200, 0));
	SetTextAlign (hDC, TA_CENTER);
	SetBkMode (hDC, TRANSPARENT);
	const char *label;
	for (int bt = 0; bt < 6; bt++) {
		if (label = oapiMFDButtonLabel (mfd, bt+side*6))
			TextOut (hDC, 13, 3+38*bt, label, strlen(label));
		else break;
	}
	oapiReleaseDC (surf, hDC);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RedrawPanel_Navmode (SURFHANDLE surf)
{
	for (DWORD i = NAVMODE_KILLROT; i < NAVMODE_HOLDALT; i++)
		if (GetNavmodeState (i))
			oapiBlt (surf, srf[2], (6-i)*44, 0, (i-1)*42, 0, 42, 31);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::RedrawPanel_Throttle (SURFHANDLE surf)
{
	UINT i, pos;
	bool redraw = false;

	for (i = 0; i < 2; i++) {
		double level = GetThrusterLevel (th_main[i]);
		pos = (UINT)((1.0-level)*180.0);
		if (pos != sliderpos_main[i])
			sliderpos_main[i] = pos, redraw = true;
	}
	if (redraw)
		for (i = 0; i < 2; i++)
			oapiBlt (surf, srf[0], i*30, sliderpos_main[i], 0, 0, 23, 19);
	return redraw;
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::RedrawPanel_Hover (SURFHANDLE surf)
{
	UINT i, pos;
	bool redraw = false;

	for (i = 0; i < 2; i++) {
		double level = GetThrusterLevel (th_hover[i]);
		pos = (UINT)((1.0-level)*180.0);
		if (pos != sliderpos_hovr[i])
			sliderpos_hovr[i] = pos, redraw = true;
	}
	if (redraw)
		for (i = 0; i < 2; i++)
			oapiBlt (surf, srf[0], i*30, sliderpos_hovr[i], 0, 0, 23, 19);
	return redraw;
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::RedrawPanel_Podlevel (SURFHANDLE surf)
{
	UINT i, pos;
	bool redraw = false;

	for (i = 0; i < 2; i++) {
		double level = GetThrusterLevel (th_pod[i]);
		pos = (UINT)((1.0-level)*90.0);
		if (pos != sliderpos_pod[i])
			sliderpos_pod[i] = pos, redraw = true;
	}
	if (redraw)
		for (i = 0; i < 2; i++)
			oapiBlt (surf, srf[0], i*30, sliderpos_pod[i], 0, 0, 23, 19);
	return redraw;
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::RedrawPanel_EngineIndicator (SURFHANDLE surf)
{
	const double rad = 25.0;
	const int cntx[2] = {29, 95};
	const int txtx[2] = {16, 83};
	const int cnty[3] = {29, 96, 163};
	const int txty0a =  34, txty0b =  46;
	const int txty1a = 101, txty1b = 113;
	const int txty2a = 168, txty2b = 180;

	UINT i;
	double level, th, angle, dx, dy;
	char cbuf[16];
	double m = GetMass();

	HDC hDC = oapiGetDC (surf);
	SelectObject (hDC, g_Param.hFont[0]);
	SelectObject (hDC, g_Param.hPen[0]);
	SetTextColor (hDC, RGB(120,220,120));
	SetTextAlign (hDC, TA_RIGHT);
	SetBkMode (hDC, TRANSPARENT);

	for (i = 0; i < 2; i++) {
		level = GetThrusterLevel (th_main[i]);
		th    = level*GetThrusterMax (th_main[i]);
		angle = level * (1.5*PI);
		dx = rad*cos(angle), dy = rad*sin(angle);
		MoveToEx (hDC, cntx[i], cnty[0], NULL);
		LineTo (hDC, cntx[i]-(int)dx, cnty[0]-(int)dy);
		sprintf (cbuf, "%0.0f", th*1e-3);
		TextOut (hDC, txtx[i], txty0a, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1f", th/m);
		TextOut (hDC, txtx[i], txty0b, cbuf, strlen (cbuf));

		level = GetThrusterLevel (th_hover[i]);
		th    = level*GetThrusterMax (th_hover[i]);
		angle = level * (1.5*PI);
		dx = rad*cos(angle), dy = rad*sin(angle);
		MoveToEx (hDC, cntx[i], cnty[1], NULL);
		LineTo (hDC, cntx[i]-(int)dx, cnty[1]-(int)dy);
		sprintf (cbuf, "%0.0f", th*1e-3);
		TextOut (hDC, txtx[i], txty1a, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1f", th/m);
		TextOut (hDC, txtx[i], txty1b, cbuf, strlen (cbuf));

		level = GetThrusterLevel (th_pod[i]);
		th    = level*GetThrusterMax (th_pod[i]);
		angle = level * (1.5*PI);
		dx = rad*cos(angle), dy = rad*sin(angle);
		MoveToEx (hDC, cntx[i], cnty[2], NULL);
		LineTo (hDC, cntx[i]-(int)dx, cnty[2]-(int)dy);
		sprintf (cbuf, "%0.0f", th*1e-3);
		TextOut (hDC, txtx[i], txty2a, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1f", th/m);
		TextOut (hDC, txtx[i], txty2b, cbuf, strlen (cbuf));
	}
	oapiReleaseDC (surf, hDC);

	return true;
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
bool ShuttleA::RedrawPanel_PodangleIndicator (SURFHANDLE surf)
{
	const int cntx[2] = {36,110}, cnty = 14;
	const int txtx[2] = {45,119}, txty =  0;
	const double rad = 24.0, radi = 29.0, radw = 36.0;
	const double da = 0.1;

	UINT i;
	int x, y, ia;
	double angle, angle1, angle2;
	char cbuf[16];

	HDC hDC = oapiGetDC (surf);
	SelectObject (hDC, g_Param.hFont[0]);
	SetBkMode (hDC, TRANSPARENT);

	for (i = 0; i < 2; i++) {
		// draw preset indicator
		SelectObject (hDC, g_Param.hPen[1]);
		angle = pod_angle_request[i];
		angle1 = angle-da;
		angle2 = angle+da;
		x = cntx[i]-(int)(radi*cos(angle)), y = cnty+(int)(radi*sin(angle));
		MoveToEx (hDC, x, y, NULL);
		LineTo (hDC, cntx[i]-(int)(radw*cos(angle1)), cnty+(int)(radw*sin(angle1)));
		LineTo (hDC, cntx[i]-(int)(radw*cos(angle2)), cnty+(int)(radw*sin(angle2)));
		LineTo (hDC, x, y);
		ia = (int)(DEG*angle+0.5);
		if      (ia < 90) sprintf (cbuf, "%02dR", ia);
		else if (ia > 90) sprintf (cbuf, "%02dF", 180-ia);
		else              sprintf (cbuf, "90");
		SetTextColor (hDC, RGB(220,220,120));
		TextOut (hDC, txtx[i]-27, txty, cbuf, strlen(cbuf));

		// draw pod status indicator
		SelectObject (hDC, g_Param.hPen[0]);
		angle = pod_angle[i];
		MoveToEx (hDC, cntx[i], cnty, NULL);
		LineTo (hDC, cntx[i]-(int)(rad*cos(angle)), cnty+(int)(rad*sin(angle)));
		ia = (int)(DEG*angle+0.5);
		if      (ia < 90) sprintf (cbuf, "%02dR", ia);
		else if (ia > 90) sprintf (cbuf, "%02dF", 180-ia);
		else              sprintf (cbuf, "90");
		SetTextColor (hDC, RGB(120,220,120));
		TextOut (hDC, txtx[i], txty, cbuf, strlen(cbuf));
	}
	oapiReleaseDC (surf, hDC);
	return true;
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RedrawPanel_Fuelstatus (SURFHANDLE surf, int part)
{
	char cbuf[20];
	int len;
	double m, m0, rate, lvl;

	HDC hDC = oapiGetDC (surf);
	SelectObject (hDC, g_Param.hFont[0]);
	SelectObject (hDC, g_Param.hBrush[1]);
	SelectObject (hDC, g_Param.hPen[2]);
	SetTextColor (hDC, RGB(224,224,224));
	SetBkMode (hDC, TRANSPARENT);
	SetTextAlign (hDC, TA_RIGHT);

	switch (part) {
	case 0:
		sprintf (cbuf, "%0.1f", GetThrusterLevel (th_hover[0]) * MAX_HOVER_THRUST / ISP);
		Rectangle (hDC, 0, 2, 20, 11); TextOut (hDC, 21, 0, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1f", GetThrusterLevel (th_hover[1]) * MAX_HOVER_THRUST / ISP);
		Rectangle (hDC, 0, 32, 20, 41); TextOut (hDC, 21, 30, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1f", GetThrusterLevel (th_pod[1]) * MAX_RETRO_THRUST / ISP);
		Rectangle (hDC, 0, 60, 20, 69); TextOut (hDC, 21, 58, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1f", GetThrusterLevel (th_pod[0]) * MAX_RETRO_THRUST / ISP);
		Rectangle (hDC, 0, 90, 20, 99); TextOut (hDC, 21, 88, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1f", GetThrusterLevel (th_main[0]) * MAX_MAIN_THRUST / ISP);
		Rectangle (hDC, 0, 117, 20, 126); TextOut (hDC, 21, 115, cbuf, strlen(cbuf));
		sprintf (cbuf, "%0.1f", GetThrusterLevel (th_main[1]) * MAX_MAIN_THRUST / ISP);
		Rectangle (hDC, 0, 147, 20, 156); TextOut (hDC, 21, 145, cbuf, strlen(cbuf));
		break;
	case 1:
		SelectObject (hDC, g_Param.hBrush[0]);
		m = GetPropellantMass (ph_main);
		if (m > MAX_MAIN_FUEL*0.2) {
			rate = GetPropellantFlowrate (ph_main);
			lvl = m*1.25/MAX_MAIN_FUEL - 0.25;
			Rectangle (hDC,  0, 50, 32, (int)((1.0-lvl)*50.0));
			Rectangle (hDC, 40, 50, 72, (int)((1.0-lvl)*50.0));
			m0 = 0.5 * (m - MAX_MAIN_FUEL*0.2);
		} else {
			rate = lvl = m0 = 0;
		}
		sprintf (cbuf, "%0.0f", m0);
		SetTextAlign (hDC, TA_CENTER);
		TextOut (hDC, 16, 20, cbuf, strlen (cbuf));
		TextOut (hDC, 56, 20, cbuf, strlen (cbuf));
		sprintf (cbuf, "%0.1f", 0.5*rate);
		SetTextAlign (hDC, TA_RIGHT);
		SelectObject (hDC, g_Param.hBrush[1]);
		Rectangle (hDC, 21, 57, 42, 66); TextOut (hDC, 42, 55, cbuf, strlen(cbuf));
		Rectangle (hDC, 61, 57, 82, 66); TextOut (hDC, 82, 55, cbuf, strlen(cbuf));
		break;
	case 2:
		m = GetPropellantMass (ph_main);
		if (m < MAX_MAIN_FUEL*0.2) {
			rate = GetPropellantFlowrate (ph_main);
			lvl = 5.0*m/MAX_MAIN_FUEL;
		} else {
			rate = 0;
			lvl = 1;
			m = MAX_MAIN_FUEL*0.2;
		}
		if (lvl > 0) {
			SelectObject (hDC, g_Param.hBrush[0]);
			Rectangle (hDC,  0, 57, 32, 19+(int)((1.0-lvl)*38.0));
		}
		sprintf (cbuf, "%0.0f", m);
		SetTextAlign (hDC, TA_CENTER);
		TextOut (hDC, 16, 33, cbuf, strlen (cbuf));
		sprintf (cbuf, "%0.1f", rate);
		SetTextAlign (hDC, TA_RIGHT);
		SelectObject (hDC, g_Param.hBrush[1]);
		Rectangle (hDC, 21, 2, 42, 11); TextOut (hDC, 42, 0, cbuf, strlen(cbuf));
		break;
	case 3:
		m = GetPropellantMass (ph_rcs);
		if (m > 0) {
			SelectObject (hDC, g_Param.hBrush[0]);
			Rectangle (hDC, 0, 25, 32, (int)((1.0-m/MAX_RCS_FUEL)*25.0));
		}
		SetTextAlign (hDC, TA_CENTER);
		sprintf (cbuf, "%0.0f", m);
		TextOut (hDC, 16, 7, cbuf, strlen(cbuf));
		SetTextAlign (hDC, TA_RIGHT);
		SelectObject (hDC, g_Param.hBrush[1]);
		sprintf (cbuf, "%0.2f", GetPropellantFlowrate (ph_rcs));
		Rectangle (hDC, 21, 30, 42, 39); TextOut (hDC, 42, 28, cbuf, strlen(cbuf));
		break;
	} 
	oapiReleaseDC (surf, hDC);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RedrawPanel_CargoOpen(SURFHANDLE surf)
{
	int mx,my;
	
	for (int i=0;i<6;i++)
		if (cargo_open[i])
		{	
			mx = (i>2?0:45);
			my = (i % 3)*44;
			oapiBlt (surf, srf[5], mx, my, 0, 0, 38, 42);

		} else if (cargo_arm_status ==1)
		{
			mx = (i>2?0:45);
			my = (i % 3)*44;
			oapiBlt (surf, srf[5], mx+1, my, 0, 42, 38, 42);
		}
}


// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RedrawVC_ThPOD ()
{

		double level = GetThrusterLevel (th_pod[1]);
		UINT pos = (UINT)((1.0-level)*90.0);
		
		if (pos != sliderpos_pod_v[0]) {
			SetAnimation (anim_pod_thrust_right, level);
			sliderpos_pod_v[0] = pos;
								};

		level = GetThrusterLevel (th_pod[0]);
		pos = (UINT)((1.0-level)*90.0);
		
		if (pos != sliderpos_pod_v[1]) {
			SetAnimation (anim_pod_thrust_left, level);
			sliderpos_pod_v[1] = pos;
								};
	
};

// --------------------------------------------------------------
//
// --------------------------------------------------------------
void ShuttleA::RedrawVC_ThHover()
{

		double level = GetThrusterLevel (th_hover[0]);
		UINT pos = (UINT)((1.0-level)*90.0);
		
		if (pos != sliderpos_hovr_v[0]) {
			SetAnimation (anim_hover_thrust_left, level);
			sliderpos_hovr_v[0] = pos;
								};

		level = GetThrusterLevel (th_hover[1]);
		pos = (UINT)((1.0-level)*90.0);
		
		if (pos != sliderpos_hovr_v[1]) {
			SetAnimation (anim_hover_thrust_right, level);
			sliderpos_hovr_v[1] = pos;
								};
	
	
};

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA::RedrawVC_ThMain()
{
		double level = GetThrusterLevel (th_main[0]);
		UINT pos = (UINT)((1.0-level)*90.0);
		
		if (pos != sliderpos_main_v[0]) {
			SetAnimation (anim_main_thrust_left, level);
			sliderpos_main_v[0] = pos;
								};

		level = GetThrusterLevel (th_main[1]);
		pos = (UINT)((1.0-level)*90.0);
		
		if (pos != sliderpos_main_v[1]) {
			SetAnimation (anim_main_thrust_right, level);
			sliderpos_main_v[1] = pos;
								};
}
// ==============================================================
// Overloaded callback functions
// ==============================================================

// --------------------------------------------------------------
// Set vessel class caps
// --------------------------------------------------------------
void ShuttleA::clbkSetClassCaps (FILEHANDLE cfg)
{
	int i, j;

	SetSize (17.0);
	SetPMI (_V(86.6, 89.8, 5.5));
	SetEmptyMass (EMPTY_MASS);
	payload_mass = 0.0;
	VECTOR3 r[2] = {{0,0,8}, {0,0,-8}};
	SetGravityGradientDamping (20.0);
	SetCW (0.2, 0.2, 1.5, 1.5);
	SetCrossSections (_V(132.2, 237.9, 42.4));
	SetRotDrag (_V(0.7, 0.7, 0.3));
	SetSurfaceFrictionCoeff (0.5, 0.5);
	SetCameraOffset (_V(-0.575f,2.4f,15.9f));
	SetDockParams (_V(0,0,18.32), _V(0,0,1), _V(0,1,0));

	SetTouchdownPoints (tdvtx, ntdvtx);

	EnableTransponder (true);

	adi_layout = 0;
	if (oapiReadItem_int (cfg, "ADI_DEFAULT_LAYOUT", adi_layout))
		adi_layout = max (0, min (1, adi_layout));

	// ******************** NAV radios **************************

	InitNavRadios (2);

	// ***************** propellant specs ***********************

	ph_main = CreatePropellantResource (MAX_MAIN_FUEL); // main tank
	ph_rcs  = CreatePropellantResource (MAX_RCS_FUEL);  // RCS tank

	// *************** thruster definitions *********************

	static PARTICLESTREAMSPEC contrail_main = {
		0, 8.0, 5, 150, 0.3, 4.0, 4, 2.0, PARTICLESTREAMSPEC::DIFFUSE,
		PARTICLESTREAMSPEC::LVL_SQRT, 0, 1,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-4, 1
	};
	static PARTICLESTREAMSPEC contrail_hover = {
		0, 6.0, 3, 150, 0.3, 4.0, 4, 2.0, PARTICLESTREAMSPEC::DIFFUSE,
		PARTICLESTREAMSPEC::LVL_SQRT, 0, 1,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-4, 1
	};
	static PARTICLESTREAMSPEC contrail_pod = {
		0, 4.0, 3, 150, 0.3, 3.0, 4, 2.0, PARTICLESTREAMSPEC::DIFFUSE,
		PARTICLESTREAMSPEC::LVL_SQRT, 0, 1,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-4, 1
	};
	static PARTICLESTREAMSPEC exhaust_main = {
		0, 2.0, 20, 150, 0.1, 0.2, 16, 2.0, PARTICLESTREAMSPEC::EMISSIVE,
		PARTICLESTREAMSPEC::LVL_SQRT, 0, 1,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-5, 0.1
	};

	// main thrusters
	th_main[0] = CreateThruster (_V(-1.8,0,-16.55), _V(0,0,1), MAX_MAIN_THRUST, ph_main,ISP_P0, ISP);
	th_main[1] = CreateThruster (_V( 1.8,0,-16.55), _V(0,0,1), MAX_MAIN_THRUST, ph_main, ISP_P0, ISP);
	thg_main = CreateThrusterGroup (th_main, 2, THGROUP_MAIN);
	AddExhaust (th_main[0], 12, 1);
	AddExhaust (th_main[1], 12, 1);
	AddExhaustStream (th_main[0], _V(0,0,-24), &contrail_main);
	AddExhaustStream (th_main[0], _V(-1.8,0,-19), &exhaust_main);
	AddExhaustStream (th_main[1], _V( 1.8,0,-19), &exhaust_main);

	// hover and retro thrusters
	th_hover[0] = CreateThruster (_V(0,-2.4, 13.25), _V(0,1,0), MAX_HOVER_THRUST, ph_main, ISP_P0, ISP);
	th_hover[1] = CreateThruster (_V(0,-2.4,-13.25), _V(0,1,0), MAX_HOVER_THRUST, ph_main, ISP_P0, ISP);
	thg_hover = CreateThrusterGroup (th_hover, 2, THGROUP_HOVER);
	AddExhaust (th_hover[0], 10, 1);
	AddExhaust (th_hover[1], 10, 1);
	AddExhaustStream (th_hover[0], _V(0,-6, 13.25), &contrail_hover);
	AddExhaustStream (th_hover[1], _V(0,-6,-13.25), &contrail_hover);
	AddExhaustStream (th_hover[0], _V(0,-4, 13.25), &exhaust_main);
	AddExhaustStream (th_hover[1], _V(0,-4,-13.25), &exhaust_main);

	// retro/hover thrusters
	th_pod[0] = CreateThruster (_V(-7,0,0/*1.5*/), _V(0,0,-1), MAX_RETRO_THRUST, ph_main,ISP_P0, ISP);
	th_pod[1] = CreateThruster (_V( 7,0,0/*1.5*/), _V(0,0,-1), MAX_RETRO_THRUST, ph_main, ISP_P0, ISP);
	thg_pod = CreateThrusterGroup (th_pod, 2, THGROUP_USER);
	AddExhaust (th_pod[0], 6, 0.7, 1.5);
	AddExhaust (th_pod[1], 6, 0.7, 1.5);
	AddExhaustStream (th_pod[0], &contrail_pod);
	AddExhaustStream (th_pod[1], &contrail_pod);

	// attitude thrusters
	THRUSTER_HANDLE th_att_rot[4], th_att_lin[4];
	th_att_rot[0] = CreateThruster (_V(-6, 0.7,-0.4), _V(0,-1,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[1] = CreateThruster (_V( 6,-0.7,-0.4), _V(0, 1,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[2] = CreateThruster (_V(-6,-0.7,-0.4), _V(0, 1,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[3] = CreateThruster (_V( 6, 0.7,-0.4), _V(0,-1,0), MAX_RCS_THRUST, ph_rcs, ISP);
	CreateThrusterGroup (th_att_rot,   2, THGROUP_ATT_BANKLEFT);
	CreateThrusterGroup (th_att_rot+2, 2, THGROUP_ATT_BANKRIGHT);
	for (i = 0; i < 4; i++) AddExhaust (th_att_rot[i], 0.7, 0.08);
	th_att_rot[0] = th_att_lin[0] = CreateThruster (_V(0,0, 15), _V(0, 1,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[1] = th_att_lin[2] = CreateThruster (_V(0,0,-15), _V(0,-1,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[2] = th_att_lin[3] = CreateThruster (_V(0,0, 15), _V(0,-1,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[3] = th_att_lin[1] = CreateThruster (_V(0,0,-15), _V(0, 1,0), MAX_RCS_THRUST, ph_rcs, ISP);
	CreateThrusterGroup (th_att_rot,   2, THGROUP_ATT_PITCHUP);
	CreateThrusterGroup (th_att_rot+2, 2, THGROUP_ATT_PITCHDOWN);
	CreateThrusterGroup (th_att_lin,   2, THGROUP_ATT_UP);
	CreateThrusterGroup (th_att_lin+2, 2, THGROUP_ATT_DOWN);
	AddExhaust (th_att_rot[0], 0.7, 0.08, _V(-0.2, -1.1,   17.5), _V(0,-1,0));
	AddExhaust (th_att_rot[0], 0.7, 0.08, _V( 0.2, -1.1,   17.5), _V(0,-1,0));
	AddExhaust (th_att_rot[1], 0.7, 0.08, _V( 0,    2.3,-14.6), _V(0, 1,0));
	AddExhaust (th_att_rot[1], 0.7, 0.08, _V( 0,    2.3,-14.3), _V(0, 1,0));
	AddExhaust (th_att_rot[2], 0.7, 0.08, _V(-0.15, 2.1, 17.6), _V(0, 1,0));
	AddExhaust (th_att_rot[2], 0.7, 0.08, _V( 0.15, 2.1, 17.6), _V(0, 1,0));
	AddExhaust (th_att_rot[3], 0.7, 0.08, _V(-0.15,-1.9,-14.5), _V(0,-1,0));
	AddExhaust (th_att_rot[3], 0.7, 0.08, _V( 0.15,-1.9,-14.5), _V(0,-1,0));
	th_att_rot[0] = th_att_lin[0] = CreateThruster (_V(0,0, 15), _V( 1,0,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[1] = th_att_lin[2] = CreateThruster (_V(0,0,-15), _V(-1,0,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[2] = th_att_lin[3] = CreateThruster (_V(0,0, 15), _V(-1,0,0), MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_rot[3] = th_att_lin[1] = CreateThruster (_V(0,0,-15), _V( 1,0,0), MAX_RCS_THRUST, ph_rcs, ISP);
	CreateThrusterGroup (th_att_rot,   2, THGROUP_ATT_YAWRIGHT);
	CreateThrusterGroup (th_att_rot+2, 2, THGROUP_ATT_YAWLEFT);
	CreateThrusterGroup (th_att_lin,   2, THGROUP_ATT_RIGHT);
	CreateThrusterGroup (th_att_lin+2, 2, THGROUP_ATT_LEFT);
	AddExhaust (th_att_rot[0], 0.7, 0.08, _V(-2.4, 0.15, 17.5), _V(-1,0,0));
	AddExhaust (th_att_rot[0], 0.7, 0.08, _V(-2.4,-0.15, 17.5), _V(-1,0,0));
	AddExhaust (th_att_rot[1], 0.7, 0.08, _V( 3.5, 0,   -14.6), _V( 1,0,0));
	AddExhaust (th_att_rot[1], 0.7, 0.08, _V( 3.5, 0,   -14.3), _V( 1,0,0));
	AddExhaust (th_att_rot[2], 0.7, 0.08, _V( 2.4, 0.15, 17.5), _V( 1,0,0));
	AddExhaust (th_att_rot[2], 0.7, 0.08, _V( 2.4,-0.15, 17.5), _V( 1,0,0));
	AddExhaust (th_att_rot[3], 0.7, 0.08, _V(-3.5, 0,   -14.6), _V(-1,0,0));
	AddExhaust (th_att_rot[3], 0.7, 0.08, _V(-3.5, 0,   -14.3), _V(-1,0,0));
	th_att_lin[0] = CreateThruster (_V( 0,0,0), _V(0,0, 1), 2*MAX_RCS_THRUST, ph_rcs, ISP);
	th_att_lin[1] = CreateThruster (_V( 0,0,0), _V(0,0,-1), 2*MAX_RCS_THRUST, ph_rcs, ISP);
	CreateThrusterGroup (th_att_lin,   1, THGROUP_ATT_FORWARD);
	CreateThrusterGroup (th_att_lin+1, 1, THGROUP_ATT_BACK);
	AddExhaust (th_att_lin[0], 0.7, 0.08, _V( 6,0,-1.6), _V(0,0,-1));
	AddExhaust (th_att_lin[0], 0.7, 0.08, _V(-6,0,-1.6), _V(0,0,-1));
	AddExhaust (th_att_lin[1], 0.7, 0.08, _V( 6,0, 1), _V(0,0, 1));
	AddExhaust (th_att_lin[1], 0.7, 0.08, _V(-6,0, 1), _V(0,0, 1));

	// ************************ Attachment points ****************************
	char attach_id[8]={"SH"};
	
	payload_attachment[0]  = CreateAttachment (false,_V(1.76f,0.0f, 5.821f),_V(0.0f,0.0f,-1.0f),_V(0.0f,1.0f,0.0f),attach_id);
	payload_attachment[1]  = CreateAttachment (false,_V(1.76f,0.0f,-1.725f),_V(0.0f,0.0f,-1.0f),_V(0.0f,1.0f,0.0f),attach_id);
	payload_attachment[2]  = CreateAttachment (false,_V(1.76f,0.0f,-7.000f),_V(0.0f,0.0f,-1.0f),_V(0.0f,1.0f,0.0f),attach_id);

	payload_attachment[3]  = CreateAttachment (false,_V(-1.76f,0.0f, 5.821f),_V(0.0f,0.0f,-1.0f),_V(0.0f,1.0f,0.0f),attach_id);
	payload_attachment[4]  = CreateAttachment (false,_V(-1.76f,0.0f,-1.725f),_V(0.0f,0.0f,-1.0f),_V(0.0f,1.0f,0.0f),attach_id);
	payload_attachment[5]  = CreateAttachment (false,_V(-1.76f,0.0f,-7.000f),_V(0.0f,0.0f,-1.0f),_V(0.0f,1.0f,0.0f),attach_id);

	// ************************ Airfoil  ****************************
	ClearAirfoilDefinitions();
	CreateAirfoil (LIFT_VERTICAL, _V(0,0,0), Shuttle_MomentCoeff,  8, 140, 0.1);


	// ************************ Meshes ****************************

	SetMeshVisibilityMode (AddMesh (exmesh_tpl = oapiLoadMeshGlobal ("ShuttleA\\ShuttleA")), MESHVIS_EXTERNAL);
	SetMeshVisibilityMode (AddMesh (vcmesh_tpl = oapiLoadMeshGlobal ("ShuttleA\\ShuttleA_vc")), MESHVIS_VC);
	hPanelMesh0 = 0;
	hPanelMesh1 = oapiLoadMeshGlobal ("ShuttleA\\ShuttleA_2dpanel1");


	// ************************ Blit Ship Name ****************************
	SURFHANDLE insignia_tex = oapiGetTextureHandle (exmesh_tpl, 2);

	HDC hDC = oapiGetDC (insignia_tex);
	HFONT hFont = CreateFont(22, 0, 0, 0, 700, 0, 0, 0, 0, 0, 0, 0, 0, "Impact");
	HFONT hFontVertical = CreateFont(22, 0, 900, 900, 700, 0, 0, 0, 0, 0, 0, 0, 0, "Impact");
	HFONT pFont = (HFONT)SelectObject (hDC, hFont);
	SetTextColor (hDC, 0xD0D0D0);
	SetBkMode (hDC, TRANSPARENT);
	SetTextAlign (hDC, TA_CENTER);
	char cbuf[32];

	strncpy (cbuf, GetName(), 10);
	int len = min(strlen(GetName()), 10);
	TextOut (hDC, 66, 37, cbuf, len);
	TextOut (hDC, 209, 25, cbuf, len);
	SelectObject (hDC, hFontVertical);
	TextOut (hDC, 64, 187, cbuf, len);
	SelectObject (hDC, pFont);
	DeleteObject (hFont);
	DeleteObject (hFontVertical);
	oapiReleaseDC (insignia_tex, hDC);

	// ******************** Assign panel elements *************************
	npel = 0;
	pel[npel++] = adiball = new ADIBall (this, attref);
	pel[npel++] = adictrl = new ADICtrl (this);
	pel[npel++] = podctrl = new AuxPodCtrl (this);
	pel[npel++] = new InstrSpd (this);
	pel[npel++] = new InstrAlt (this);
	pel[npel++] = new InstrVS (this);
	pel[npel++] = new NavButton (this);
	pel[npel++] = new HUDButton (this);

	for (i = MFD_LEFT; i <= MFD_RIGHT; i++) {
		for (j = 0; j < 2; j++)
			pel[npel++] = new MFDButtonCol (this, i, j);
		pel[npel++] = new MFDButtonRow (this, i);
	}
	pel[npel++] = new RCSSwitch (this);
	pel[npel++] = new MainThrottle_NeedlePair (this, 628.0f, 242.5f, 73.0f, th_main);
	pel[npel++] = new Throttle_NeedlePair (this, 691.0f, 242.5f, 73.0f, 16, 40.0, th_hover);
	pel[npel++] = new Throttle_NeedlePair (this, 565.0f, 242.5f, 73.0f, 32, 40.0, th_pod);
	pel[npel++] = new Propellant_NeedlePair (this, 482.0f, 242.5f, 73.0f, 48, 75000.0, 120.0, ph_main);
	pel[npel++] = new Propellant_NeedlePair (this, 419.0f, 242.5f, 73.0f, 64, 3000.0, 1.0, ph_rcs);
	pel[npel++] = new ThrottleMain (this, th_main);
	pel[npel++] = new ThrottleHover (this, th_hover);
	pel[npel++] = new ThrottlePod (this, th_pod);
	pel[npel++] = new InstrVAcc (this);
	pel[npel++] = new RCSIndicator (this);

	for (i = 0; i < 2; i++)
		pel[npel++] = new AirlockSwitch (this, i, hPanelMesh1);
	pel[npel++] = new DockCoverSwitch (this, hPanelMesh1);
	pel[npel++] = new GearSwitch (this, hPanelMesh1);
	pel[npel++] = new PayloadRelease (this, hPanelMesh1);
	pel[npel++] = new PayloadArmSwitch (this, hPanelMesh1);
	pel[npel++] = new GearIndicator (this, hPanelMesh1);
	pel[npel++] = new DockCoverIndicator (this, hPanelMesh1);
	for (i = 0; i < 2; i++)
		pel[npel++] = new AirlockIndicator (this, i, hPanelMesh1);
	pel[npel++] = new PayloadArmIndicator (this, hPanelMesh1);
}

// --------------------------------------------------------------
// Read status from scenario file
// --------------------------------------------------------------
void ShuttleA::clbkLoadStateEx (FILEHANDLE scn, void *vs)
{
	char *line;

	while (oapiReadScenario_nextline (scn, line)) {
		if (!_strnicmp (line, "PODANGLE", 8)) {
			sscanf (line+8, "%lf%lf", pod_angle+0, pod_angle+1);
		} else if (!_strnicmp (line, "DOCKSTATE", 9)) {
			sscanf (line+9, "%d%lf", &dock_status, &dock_proc);
		} else if (!_strnicmp (line, "AIRLOCK", 7)) {
			sscanf (line+7, "%d%lf", &lock_status[0], &lock_proc[0]);
		} else if (!_strnicmp (line, "IAIRLOCK", 7)) {
			sscanf (line+7, "%d%lf", &lock_status[1], &lock_proc[1]);
		} else if (!_strnicmp (line, "GEAR", 4)) {
			sscanf (line+4, "%d%lf", &gear_status, &gear_proc);
		} else if (!_strnicmp (line, "PAYLOAD MASS", 12)) {
			sscanf (line+12, "%lf%d", &payload_mass,&cargo_arm_status);
		} else if (!_strnicmp (line, "ATTREF", 6)) {
			int mode, tgtmode, navid;
			sscanf (line+6, "%d%d%d", &mode, &tgtmode, &navid);
			attref->SetMode (mode);
			attref->SetTgtmode (tgtmode);
			attref->SetNavid (navid);
		} else if (!_strnicmp (line, "ADI_LAYOUT", 10)) {
			int layout = 0;
			if (sscanf (line+10, "%d", &layout) && layout >= 0 && layout <= 1)
				adi_layout = layout;
		} else {
			ParseScenarioLineEx (line, vs);
			// unrecognised option - pass to Orbiter's generic parser
		}
	}

	TOUCHDOWNVTX tdv[ntdvtx];
	memcpy(tdv, tdvtx, ntdvtx*sizeof(TOUCHDOWNVTX));
	for (int i = 0; i < ntdvtx; i++)
		tdv[i].pos.y += gear_proc*0.555;
	SetTouchdownPoints (tdv, ntdvtx);

	adiball->SetLayout (adi_layout);
}

// --------------------------------------------------------------
// Write status to scenario file
// --------------------------------------------------------------
void ShuttleA::clbkSaveState (FILEHANDLE scn)
{
	char cbuf[256];

	// default vessel parameters
	VESSEL2::clbkSaveState (scn);

	// custom parameters
	sprintf (cbuf, "%0.4f %0.4f", pod_angle[0], pod_angle[1]);
	oapiWriteScenario_string (scn, "PODANGLE", cbuf);

	sprintf (cbuf, "%d %0.4f", dock_status, dock_proc);
	oapiWriteScenario_string (scn, "DOCKSTATE", cbuf);

	if (lock_status[0] != DOOR_CLOSED) {
		sprintf (cbuf, "%d %0.4f", lock_status[0], lock_proc[0]);
		oapiWriteScenario_string (scn, "AIRLOCK", cbuf);
	}

	if (lock_status[1] != DOOR_CLOSED) {
		sprintf (cbuf, "%d %0.4f", lock_status[1], lock_proc[1]);
		oapiWriteScenario_string (scn, "IAIRLOCK", cbuf);
	}

	sprintf (cbuf, "%d %0.4f", gear_status, gear_proc);
	oapiWriteScenario_string (scn, "GEAR", cbuf);

	sprintf (cbuf, "%0.1f %d", payload_mass,cargo_arm_status);
	oapiWriteScenario_string (scn, "PAYLOAD MASS", cbuf);

	sprintf (cbuf, "%d %d %d", attref->GetMode(), attref->GetTgtmode(), attref->GetNavid());
	oapiWriteScenario_string (scn, "ATTREF", cbuf);

	oapiWriteScenario_int (scn, "ADI_LAYOUT", adi_layout);
}

// --------------------------------------------------------------
// Respond to playback event
// --------------------------------------------------------------
bool ShuttleA::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "DOCK")) {
		ActivateDockingPort (!_stricmp (event, "CLOSE") ? DOOR_CLOSING : DOOR_OPENING);
		return true;
	} else if (!_stricmp (event_type, "AIRLOCK")) {
		ActivateAirlock (0, !_stricmp (event, "CLOSE") ? DOOR_CLOSING : DOOR_OPENING);
		return true;
	} else if (!_stricmp (event_type, "IAIRLOCK")) {
		ActivateAirlock (1, !_stricmp (event, "CLOSE") ? DOOR_CLOSING : DOOR_OPENING);
		return true;
	} else if (!_stricmp (event_type, "GEAR")) {
		ActivateLandingGear (!_stricmp (event, "UP") ? DOOR_CLOSING : DOOR_OPENING);
		return true;
	} else if (!_stricmp (event_type, "POD")) {
		UINT which;
		double angle;
		char action[256];
		sscanf (event, "%d %s %lf", &which, action, &angle);
		if (!_stricmp (action, "SET")) CommandPodAngle (which, angle);
		else if (!_stricmp (action, "FWD")) CommandPodAngle (which, PI);
		else if (!_stricmp (action, "BACK")) CommandPodAngle (which, 0);
		return true;
	} else if (!_stricmp (event_type, "CARGO")) {
		if (!_strnicmp (event, "ARM", 3))
			ActivateCargo (1);
		else if (!_strnicmp (event, "DISARM", 6))
			ActivateCargo (0);
		else if (!_strnicmp (event, "GRAPPLE", 7)) {
			int grapple;
			sscanf (event+7, "%d", &grapple);
			ToggleGrapple (grapple);
		}
		return true;
	}
	return false;
}

// --------------------------------------------------------------
// Final initialisation steps
// --------------------------------------------------------------
void ShuttleA::clbkPostCreation ()
{
	UINT i;
	for (i = 0; i < 2; i++) {
		pod_angle_request[i] = pod_angle[i];
		double sina = sin(pod_angle[i]), cosa = cos(pod_angle[i]);
		SetAnimation (anim_pod[i], pod_angle[i]/PI);
		SetThrusterDir (th_pod[i], _V(0,sina,-cosa));
	}
	SetAnimation (anim_dock, dock_proc);
	for (i = 0; i < 2; i++)
		SetAnimation (anim_lock[i], lock_proc[i]);
	SetAnimation (anim_dock, dock_proc);
	SetAnimation (anim_gear, gear_proc);

	SetEmptyMass (EMPTY_MASS + payload_mass);
}

// --------------------------------------------------------------
// Frame update
// --------------------------------------------------------------
void ShuttleA::clbkPostStep (double simt, double simdt, double mjd)
{
	// Update attitude reference frame
	attref->PostStep (simt, simdt, mjd);

	// animate auxiliary engine pods
	bool redraw = false;
	for (int i = 0; i < 2; i++) {
		if (pod_angle[i] != pod_angle_request[i]) {
			double da = simdt * POD_ROT_SPEED;
			if (pod_angle[i] < pod_angle_request[i]) {
				pod_angle[i] += da;
				if (pod_angle[i] > pod_angle_request[i])
					pod_angle[i] = pod_angle_request[i];
			} else {
				pod_angle[i] -= da;
				if (pod_angle[i] < pod_angle_request[i])
					pod_angle[i] = pod_angle_request[i];
			}
			double sina = sin(pod_angle[i]), cosa = cos(pod_angle[i]);
			SetThrusterDir (th_pod[i], _V(0,sina,-cosa));
			SetAnimation (anim_pod[i], pod_angle[i]/PI);
			redraw = true;
		}
	}
	if (redraw) {
		oapiTriggerPanelRedrawArea (0, AID_PODCTRL);
	}

	// animate docking hatch
	if (dock_status >= DOOR_CLOSING) {
		double da = simdt * DOCK_OPERATING_SPEED;
		if (dock_status == DOOR_CLOSING) {
			if (dock_proc > 0.0)
				dock_proc = max (0.0, dock_proc-da);
			else {
				dock_status = DOOR_CLOSED;
				oapiTriggerRedrawArea (1, 0,AID_DOCKINDICATOR);
			}
		} else { // hatch opening
			if (dock_proc < 1.0)
				dock_proc = min (1.0, dock_proc+da);
			else {
				dock_status = DOOR_OPEN;
				oapiTriggerRedrawArea (1,0, AID_DOCKINDICATOR);
			}
		}
		SetAnimation (anim_dock, dock_proc);
	}

	// animate airlocks
	for (int lock = 0; lock < 2; lock++) {
		if (lock_status[lock] >= DOOR_CLOSING) {
			double da = simdt * AIRLOCK_OPERATING_SPEED;
			if (lock_status[lock] == DOOR_CLOSING) {
				if (lock_proc[lock] > 0.0)
					lock_proc[lock] = max (0.0, lock_proc[lock]-da);
				else {
					lock_status[lock] = DOOR_CLOSED;
					oapiTriggerRedrawArea (1,0, AID_AIRLOCK1INDICATOR+lock);
				}
			} else { // door opening
				if (lock_proc[lock] < 1.0)
					lock_proc[lock] = min (1.0, lock_proc[lock]+da);
				else {
					lock_status[lock] = DOOR_OPEN;
					oapiTriggerRedrawArea (1,0, AID_AIRLOCK1INDICATOR+lock);
				}
			}
			SetAnimation (anim_lock[lock], lock_proc[lock]);
		}
	}

	// animate gear
	if (gear_status >= DOOR_CLOSING) {
		double da = simdt * GEAR_OPERATING_SPEED;
		if (gear_status == DOOR_CLOSING) {
			if (gear_proc > 0.0)
				gear_proc = max (0.0, gear_proc-da);
			else {
				gear_status = DOOR_CLOSED;
				oapiTriggerRedrawArea (1,0, AID_GEARINDICATOR);
			}
		} else { // door opening
			if (gear_proc < 1.0)
				gear_proc = min (1.0, gear_proc+da);
			else {
				gear_status = DOOR_OPEN;
			    oapiTriggerRedrawArea (1,0, AID_GEARINDICATOR);
			}
		}
		SetAnimation (anim_gear, gear_proc);
		TOUCHDOWNVTX tdv[ntdvtx];
		memcpy(tdv, tdvtx, ntdvtx*sizeof(TOUCHDOWNVTX));
		for (int i = 0; i < 4; i++)
			tdv[i].pos.y += gear_proc*0.555;
		SetTouchdownPoints (tdv, ntdvtx);
	}
}


// --------------------------------------------------------------
// Respond to MFD mode change
// --------------------------------------------------------------
void ShuttleA::clbkMFDMode (int mfd, int mode)
{
	switch (mfd) {
	case MFD_LEFT:
		oapiTriggerRedrawArea (0,0, AID_MFD1_LBUTTONS);
		oapiTriggerRedrawArea (0,0, AID_MFD1_RBUTTONS);
		break;
	case MFD_RIGHT:
		oapiTriggerRedrawArea (0,0, AID_MFD2_LBUTTONS);
		oapiTriggerRedrawArea (0,0, AID_MFD2_RBUTTONS);
		break;
	}
}

// --------------------------------------------------------------
// Respond to HUD mode change
// --------------------------------------------------------------
void ShuttleA::clbkHUDMode (int mode)
{
	oapiTriggerRedrawArea (0, 0, AID_HUDMODE);
}

//---------------------------------------------------------------
// Respond attitude control mode change
// --------------------------------------------------------------
void ShuttleA::clbkRCSMode (int mode)
{
	oapiTriggerRedrawArea (0, 0, AID_ATTITUDEMODE);
	oapiTriggerRedrawArea (0, 0, AID_ATTITUDEINDICATOR);
}

//---------------------------------------------------------------
// Respond to navmode change
// --------------------------------------------------------------
void ShuttleA::clbkNavMode (int mode, bool active)
{
	oapiTriggerRedrawArea (0, 0,AID_NAVMODE);
}

// --------------------------------------------------------------
// Load a 2-D instrument panel
// --------------------------------------------------------------
bool ShuttleA::clbkLoadPanel2D (int id, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	switch (id) {
	case 0:
		DefineMainPanel (hPanel);
		ScalePanel (hPanel, viewW, viewH);
		oapiSetPanelNeighbours (-1,-1,1,-1);
		SetCameraDefaultDirection (_V(0,0,1)); // forward
		oapiCameraSetCockpitDir (0,0);         // look forward
		return true;
	case 1:
		DefineOverheadPanel (hPanel);
		ScalePanel (hPanel, viewW, viewH);
		oapiSetPanelNeighbours (-1,-1,-1,0);
		SetCameraDefaultDirection (_V(0,0,1)); // forward
		oapiCameraSetCockpitDir (0,20*RAD);    // look up
		return true;
	default:
		return false;
	}
}

void ShuttleA::DefineMainPanel (PANELHANDLE hPanel)
{
	float panelW = (float)PANEL2D_MAINW;
	float panelH = (float)PANEL2D_MAINH;
	float texW   = (float)PANEL2D_TEXW;
	float texH   = (float)PANEL2D_TEXH;
	static NTVERTEX VTX[4] = {
		{     0,     0,0,   0,0,0,   0.0f,        1.0f - panelH/texH},
		{     0,panelH,0,   0,0,0,   0.0f,        1.0f},
		{panelW,panelH,0,   0,0,0,   panelW/texW, 1.0f},
		{panelW,     0,0,   0,0,0,   panelW/texW, 1.0f - panelH/texH}
	};
	static WORD IDX[6] = {
		0,2,1,
		2,0,3
	};

	static NTVERTEX VTX_MFD[2][4] = {
		{{LMFD_X,     84,0,  0,0,0,  0,0},
		 {LMFD_X+260, 84,0,  0,0,0,  1,0},
		 {LMFD_X,    344,0,  0,0,0,  0,1},
		 {LMFD_X+260,344,0,  0,0,0,  1,1}},
		{{RMFD_X,     84,0,  0,0,0,  0,0},
		 {RMFD_X+260, 84,0,  0,0,0,  1,0},
		 {RMFD_X,    344,0,  0,0,0,  0,1},
		 {RMFD_X+260,344,0,  0,0,0,  1,1}}
	};
	static WORD IDX_MFD[6] = {
		0,1,2,
		3,2,1
	};

	int i;
	DWORD panel_grp, back_grp0, back_grp1, back_grp2, frnt_grp0, frnt_grp1, frnt_grp2, lmfd_grp, rmfd_grp;

	if (hPanelMesh0) oapiDeleteMesh(hPanelMesh0);
	hPanelMesh0 = oapiCreateMesh(0,0);
	MESHGROUP zero_grp0 = {0, 0, 0, 0, 0, 0, 0, 0, 0};
	MESHGROUP zero_grp1 = {0, 0, 0, 0, 0, 1, 0, 0, 0};
	MESHGROUP zero_grp2 = {0, 0, 0, 0, 0, 2, 0, 0, 0};
	MESHGROUP grp = {VTX, IDX, 4, 6, 0, 0, 0, 0, 0};
	MESHGROUP grp_lmfd = {VTX_MFD[0], IDX_MFD, 4, 6, 0, 0, 0, 0, 0};
	MESHGROUP grp_rmfd = {VTX_MFD[1], IDX_MFD, 4, 6, 0, 0, 0, 0, 0};

	back_grp0 = oapiAddMeshGroup (hPanelMesh0, &zero_grp0); // mesh elements behind background using background texture
	back_grp1 = oapiAddMeshGroup (hPanelMesh0, &zero_grp1); // mesh elements behind background using panel element texture
	back_grp2 = oapiAddMeshGroup (hPanelMesh0, &zero_grp2); // mesh elements behind background using texture index 2 (ADI ball)
	panel_grp = oapiAddMeshGroup (hPanelMesh0, &grp);       // the panel background
	frnt_grp0 = oapiAddMeshGroup (hPanelMesh0, &zero_grp0); // mesh elements in front of background using background texture
	frnt_grp1 = oapiAddMeshGroup (hPanelMesh0, &zero_grp1); // mesh elements in front of background using panel element texture
	frnt_grp2 = oapiAddMeshGroup (hPanelMesh0, &zero_grp2); // mesh elements in front of background using texture index 2 (ADI ball)
	lmfd_grp = oapiAddMeshGroup (hPanelMesh0, &grp_lmfd);   // left MFD
	rmfd_grp = oapiAddMeshGroup (hPanelMesh0, &grp_rmfd);   // right MFD

	adiball->AddMeshData2D (hPanelMesh0, back_grp2, frnt_grp2);
	adictrl->AddMeshData2D (hPanelMesh0, frnt_grp0, frnt_grp1);
	podctrl->AddMeshData2D (hPanelMesh0, frnt_grp0, frnt_grp1);

	for (i = 3; i < 6; i++) pel[i]->AddMeshData2D (hPanelMesh0, back_grp1);
	for (; i < 8; i++) pel[i]->AddMeshData2D (hPanelMesh0, frnt_grp0);
	for (; i < 25; i++) pel[i]->AddMeshData2D (hPanelMesh0, frnt_grp1);
	hPanelMesh = hPanelMesh0;
	SURFHANDLE paneltex[3] = {panel2dtex,paneleltex,aditex};
	SetPanelBackground (hPanel, paneltex, 3, hPanelMesh, PANEL2D_MAINW, PANEL2D_MAINH, 0, PANEL_ATTACH_BOTTOM | PANEL_MOVEOUT_BOTTOM);

	for (i = 0; i < 25; i++) pel[i]->Reset2D (0);

	RegisterPanelMFDGeometry (hPanel, MFD_LEFT, 0, lmfd_grp);
	RegisterPanelMFDGeometry (hPanel, MFD_RIGHT, 0, rmfd_grp);

	RegisterPanelArea (hPanel, AID_ADIBALL,       _R(   0,   0,   0,   0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, panel2dtex, adiball);
	RegisterPanelArea (hPanel, AID_ADICTRL,       _R(ADICTRL_X+9, ADICTRL_Y+83, ADICTRL_X+152, ADICTRL_Y+171), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_DOWN|PANEL_MOUSE_LBUP|PANEL_MOUSE_LBPRESSED, paneleltex, adictrl);
	RegisterPanelArea (hPanel, AID_PODCTRL,       _R(PODCTRL_X, PODCTRL_Y+87, PODCTRL_X+140, PODCTRL_Y+115), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_LBUP, paneleltex, podctrl);
	RegisterPanelArea (hPanel, AID_SPDINSTR,      _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[3]);
	RegisterPanelArea (hPanel, AID_ALTINSTR,      _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[4]);
	RegisterPanelArea (hPanel, AID_VSINSTR,       _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[5]);
	RegisterPanelArea (hPanel, AID_NAVMODE,       _R(NAVBTN_X, 17, NAVBTN_X+262, 48), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN, panel2dtex, pel[6]);
	RegisterPanelArea (hPanel, AID_HUDMODE,       _R(HUDBTN_X, 17, HUDBTN_X+174, 48), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_ONREPLAY, panel2dtex, pel[7]);

	RegisterPanelArea (hPanel, AID_MFD1_LBUTTONS, _R(LMFD_X- 38, 109,  52, 318), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, paneleltex, pel[8]);
	RegisterPanelArea (hPanel, AID_MFD1_RBUTTONS, _R(LMFD_X+272, 109, 362, 318), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, paneleltex, pel[9]);
	RegisterPanelArea (hPanel, AID_MFD1_BBUTTONS, _R(LMFD_X+ 63, 349, 262, 368), PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_ONREPLAY, paneleltex, pel[10]);
	RegisterPanelArea (hPanel, AID_MFD2_LBUTTONS, _R(RMFD_X- 38, 109, 942, 318), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, paneleltex, pel[11]);
	RegisterPanelArea (hPanel, AID_MFD2_RBUTTONS, _R(RMFD_X+272, 109,1252, 318), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, paneleltex, pel[12]);
	RegisterPanelArea (hPanel, AID_MFD2_BBUTTONS, _R(RMFD_X+ 63, 349,1152, 368), PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_ONREPLAY, paneleltex, pel[13]);

	RegisterPanelArea (hPanel, AID_ATTITUDEMODE,  _R( 389, 52, 411, 94), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN, panel2dtex, pel[14]);
	RegisterPanelArea (hPanel, AID_ATTITUDEINDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, paneleltex, pel[24]);
	RegisterPanelArea (hPanel, AID_THMAINNEEDLE,  _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[15]);
	RegisterPanelArea (hPanel, AID_THHOVERNEEDLE, _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[16]);
	RegisterPanelArea (hPanel, AID_THPODNEEDLE,   _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[17]);
	RegisterPanelArea (hPanel, AID_PHMAINNEEDLE,  _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[18]);
	RegisterPanelArea (hPanel, AID_PHRCSNEEDLE,   _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[19]);
	RegisterPanelArea (hPanel, AID_ENGINEMAIN,    _R(THROTTLE_X+ 78, THROTTLE_Y-3, THROTTLE_X+131, THROTTLE_Y+136), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED, paneleltex, pel[20]);
	RegisterPanelArea (hPanel, AID_ENGINEHOVER,   _R(THROTTLE_X+150, THROTTLE_Y-3, THROTTLE_X+208, THROTTLE_Y+136), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED, paneleltex, pel[21]);
	RegisterPanelArea (hPanel, AID_ENGINEPODLEVEL,_R(THROTTLE_X+  6, THROTTLE_Y-3, THROTTLE_X+ 59, THROTTLE_Y+136), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED, paneleltex, pel[22]);
	RegisterPanelArea (hPanel, AID_VACCINSTR,     _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, paneleltex, pel[23]);
}

void ShuttleA::DefineOverheadPanel (PANELHANDLE hPanel)
{
	hPanelMesh = hPanelMesh1;
	SURFHANDLE tex[3] = {oapiGetTextureHandle (hPanelMesh1, 1), oapiGetTextureHandle (hPanelMesh1, 2), oapiGetTextureHandle (hPanelMesh1, 3)};
	SetPanelBackground (hPanel, tex, 3, hPanelMesh, PANEL2D_OVRHW, PANEL2D_OVRHH, 0, PANEL_ATTACH_TOP | PANEL_MOVEOUT_TOP);

	RegisterPanelArea (hPanel, AID_FUELSTATUS1, _R(243,100,263,255), 0, _R(127, 77,147,232), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_CURRENT);
	RegisterPanelArea (hPanel, AID_FUELSTATUS2, _R(287, 42,368,107), 0, _R(171, 19,252, 84), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_CURRENT);
	RegisterPanelArea (hPanel, AID_FUELSTATUS3, _R(322,209,363,265), 0, _R(206,186,247,242), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_CURRENT);
	RegisterPanelArea (hPanel, AID_FUELSTATUS4, _R(234, 42,275, 80), 0, _R(118, 19,159, 57), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_CURRENT);

	RegisterPanelArea (hPanel, AID_AIRLOCK1SWITCH,    _R(530, 39,552, 81), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN, panel2dtex, pel[25]);
	RegisterPanelArea (hPanel, AID_AIRLOCK1INDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, paneleltex, pel[33]);
	RegisterPanelArea (hPanel, AID_AIRLOCK2SWITCH,    _R(582, 39,604, 81), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN, panel2dtex, pel[26]);
	RegisterPanelArea (hPanel, AID_AIRLOCK2INDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, paneleltex, pel[34]);
	RegisterPanelArea (hPanel, AID_DOCKSWITCH,        _R(530,133,552,175), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN, panel2dtex, pel[27]);
	RegisterPanelArea (hPanel, AID_DOCKINDICATOR,     _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, paneleltex, pel[32]);
	RegisterPanelArea (hPanel, AID_GEARSWITCH,        _R(582,133,604,175), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN, panel2dtex, pel[28]);
	RegisterPanelArea (hPanel, AID_GEARINDICATOR,     _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, paneleltex, pel[31]);

	RegisterPanelArea (hPanel, AID_CARGO_OPEN,        _R(723, 50,818,173), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN, panel2dtex, pel[29]);
	RegisterPanelArea (hPanel, AID_GARGOARMSWITCH,    _R(850, 90,872,132), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN, panel2dtex, pel[30]);
	RegisterPanelArea (hPanel, AID_CARGOARMINDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, paneleltex, pel[35]);

	for (int i = 25; i < npel; i++)
		pel[i]->Reset2D(1);
}

void ShuttleA::ScalePanel (PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	double defscale = (double)viewW/(double)PANEL2D_MAINW;
	double magscale = max (defscale, 1.0);
	SetPanelScaling (hPanel, defscale, magscale);
}

// --------------------------------------------------------------
// Respond to panel mouse event
// --------------------------------------------------------------
bool ShuttleA::clbkPanelMouseEvent (int id, int event, int mx, int my, void *context)
{
	if (context) {
		PanelElement *pe = (PanelElement*)context;
		return pe->ProcessMouse2D (event, mx, my);
	} else
		return false;
}

// --------------------------------------------------------------
// Respond to panel redraw event
// --------------------------------------------------------------
bool ShuttleA::clbkPanelRedrawEvent (int id, int event, SURFHANDLE surf, void *context)
{
	if (context) {
		PanelElement *pe = (PanelElement*)context;
		return pe->Redraw2D (surf);
	} else {
		switch (id) {
		case AID_FUELSTATUS1:
		case AID_FUELSTATUS2:
		case AID_FUELSTATUS3:
		case AID_FUELSTATUS4:
			RedrawPanel_Fuelstatus (surf, id-AID_FUELSTATUS1);
			return true;
		}
		return false;
	}
}

// --------------------------------------------------------------
// Respond to buffered keyboard events
// --------------------------------------------------------------
int ShuttleA::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	if (!down) return 0;       // only process keydown events
	if (Playback()) return 0;  // don't allow manual user input during a playback

	if (KEYMOD_SHIFT (kstate)) {
	} else if (KEYMOD_CONTROL (kstate)) {
		switch (key) {
		case OAPI_KEY_O:       // "operate inner airlock"
			RevertAirlock (1);
			return 1;
		}
	} else {
		switch (key) {
		case OAPI_KEY_K:       // "operate docking port"
			RevertDockingPort ();
			return 1;
		case OAPI_KEY_O:       // "operate outer airlock"
			RevertAirlock (0);
			return 1;
		case OAPI_KEY_G:       //gear
			RevertLandingGear();
			return 1;
		}
	}
	return 0;
}

// --------------------------------------------------------------
// Respond to immediate keyboard events
// --------------------------------------------------------------
int ShuttleA::clbkConsumeDirectKey(char *kstate)
{
	if (KEYMOD_CONTROL (kstate)) {
		if (KEYDOWN (kstate, OAPI_KEY_DECIMAL))
				{
				 IncThrusterGroupLevel(thg_pod,-0.01);
				 oapiTriggerPanelRedrawArea (0, AID_ENGINEPODLEVEL);
				 return 1;
				}
		
		if (KEYDOWN (kstate, OAPI_KEY_NUMPAD0))
				{
				 IncThrusterGroupLevel(thg_pod,0.01);
				 oapiTriggerPanelRedrawArea (0, AID_ENGINEPODLEVEL);
				 return 1;
				}
			
		}

	return 0;
}

// --------------------------------------------------------------
// Respond to generic messages
// --------------------------------------------------------------
int ShuttleA::clbkGeneric (int msgid, int prm, void *context)
{
	switch (msgid) {
	case VMSG_LUAINSTANCE:
		return Lua_InitInstance (context);
	}
	return 0;
}

// --------------------------------------------------------------
// Draw custom HUD elements
// --------------------------------------------------------------
bool ShuttleA::clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp)
{
	if (oapiCockpitMode() != COCKPIT_VIRTUAL) return false;

	// draw the default HUD
	VESSEL4::clbkDrawHUD (mode, hps, skp);
	int cx = hps->CX, cy = hps->CY;

	// show gear deployment status
	if (gear_status == DOOR_CLOSED || (gear_status >= DOOR_CLOSING && fmod (oapiGetSimTime(), 1.0) < 0.5)) {
		int d = hps->Markersize/2;
		if (cx >= -d*3 && cx < hps->W+d*3 && cy >= d && cy < hps->H+d*5) {
			skp->Rectangle (cx-d*3, cy-d*2, cx-d*2, cy-d);
			skp->Rectangle (cx+d*2, cy-d*2, cx+d*3, cy-d);

			skp->Rectangle (cx-d*3, cy+d*2, cx-d*2, cy+d);
			skp->Rectangle (cx+d*2, cy+d*2, cx+d*3, cy+d);

			skp->Rectangle (cx-d*3, cy-d*4, cx-d*2, cy-d*5);
			skp->Rectangle (cx+d*2, cy-d*4, cx+d*3, cy-d*5);
		}
	}

	// show RCS mode
	if (oapiCockpitMode() == COCKPIT_VIRTUAL) {
		switch (GetAttitudeMode()) {
		case RCS_ROT:
			skp->Text (0, hps->H-13, "RCS ROT", 7);
			break;
		case RCS_LIN:
			skp->Text (0, hps->H-13, "RCS_LIN", 7);
			break;
		}
	}

	if (oapiGetHUDMode() == HUD_DOCKING) {
		if (dock_status != DOOR_OPEN) {
			int d = hps->Markersize*5;
			double tmp;
			if (dock_status == DOOR_CLOSED || modf (oapiGetSimTime(), &tmp) < 0.5) {
				skp->Line (cx-d,cy-d,cx+d,cy+d);
				skp->Line (cx-d,cy+d,cx+d,cy-d);
			}
		}
	}

	//show cargo jett. arm
	if ((cargo_arm_status == 1) && (fmod (oapiGetSimTime(), 1.0) < 0.5)) {
		skp->SetTextAlign (oapi::Sketchpad::CENTER);
		skp->Text (hps->CX, hps->H-13, "CARGO ARM", 9);
		skp->SetTextAlign (oapi::Sketchpad::LEFT);
	}
	return true;
}

// --------------------------------------------------------------
// Render custom HUD elements
// --------------------------------------------------------------
void ShuttleA::clbkRenderHUD (int mode, const HUDPAINTSPEC *hps, SURFHANDLE hTex)
{
	VESSEL4::clbkRenderHUD (mode, hps, hTex);

	static float texw = 512.0f, texh = 256.0f;
	float cx = (float)hps->CX, cy = (float)hps->CY;
	int i, nvtx = 0, nidx = 0;
	static NTVERTEX vtx[24+12];
	static WORD idx[36+30];
	static float scl = 0;
	static NTVERTEX vgear[24];
	static NTVERTEX vnose[12];
	static WORD igear[36] = {
		 0, 3, 1,  2, 3, 0,
		 4, 7, 5,  6, 7, 4,
		 8,11, 9, 10,11, 8,
		12,15,13, 14,15,12,
		16,19,17, 18,19,16,
		20,23,21, 22,23,20
	};
	static WORD inose[30] = {
		0,1,2, 2,3,0,
		0,6,1, 6,7,1,
		1,8,2, 8,9,2,
		2,10,3, 10,11,3,
		3,4,0, 0,4,5
	};

	if (scl != hps->Markersize*0.25f) {
		scl = hps->Markersize*0.25f;
		memset (vgear, 0, 24*sizeof(NTVERTEX));
		float x[24] = {-5,-3,-5,-3,-5,-3,-5,-3,-5,-3,-5,-3, 3,5,3,5,3,5,3,5,3,5,3,5};
		float y[24] = {-6.5,-6.5,-8.5,-8.5,-1.5,-1.5,-3.5,-3.5,3.5,3.5,1.5,1.5,-6.5,-6.5,-8.5,-8.5,-1.5,-1.5,-3.5,-3.5,3.5,3.5,1.5,1.5};
		for (i = 0; i < 24; i++) {
			vgear[i].x = cx + x[i]*scl;
			vgear[i].y = cy + y[i]*scl;
			vgear[i].tu = (405.0f + (18.0f * (i%2)))/texw;
			vgear[i].tv = (104.0f - (18.0f * ((i%4)/2)))/texh;
		}
		memset (vnose, 0, 12*sizeof(NTVERTEX));
		float xn[12] = {0,1,0,-1,-31,-30,30,31,31,30,-30,-31};
		float yn[12] = {-1,0,1,0,-30,-31,-31,-30,30,31,31,30};
		float un[12] = {392.5f, 397.0f, 392.5f, 388.0f, 388.0f, 392.5f, 392.5f, 397.0f, 397.0f, 392.5f, 392.5f, 388.0f};
		float vn[12] = {92.0f, 96.5f, 101.0f, 96.5f, 96.5f, 92.0f, 92.0f, 96.5f, 96.5f, 101.0f, 101.0f, 96.5f};
		for (i = 0; i < 12; i++) {
			vnose[i].x = cx + xn[i]*scl*0.4f;
			vnose[i].y = cy + yn[i]*scl*0.4f;
			vnose[i].tu = un[i]/texw;
			vnose[i].tv = vn[i]/texh;
		}
	}

	// show gear deployment status
	if (gear_status == DOOR_CLOSED || (gear_status >= DOOR_CLOSING && fmod (oapiGetSimTime(), 1.0) < 0.5)) {
		memcpy (vtx+nvtx, vgear, 24*sizeof(NTVERTEX));
		for (i = 0; i < 36; i++) idx[nidx+i] = igear[i]+nvtx;
		nvtx += 24;
		nidx += 36;
	}

	// show dock cover status
	if (oapiGetHUDMode() == HUD_DOCKING && dock_status != DOOR_OPEN) {
		double tmp;
		if (dock_status == DOOR_CLOSED || modf (oapiGetSimTime(), &tmp) < 0.5) {
			memcpy (vtx+nvtx, vnose, 12*sizeof(NTVERTEX));
			for (i = 0; i < 30; i++) idx[nidx+i] = inose[i]+nvtx;
			nvtx += 12;
			nidx += 30;
		}
	}

	if (nvtx) {
		MESHGROUP grp = {vtx, idx, nvtx, nidx, 0, 0, 0, 0, 0};
		MESHHANDLE hmesh = oapiCreateMesh (1, &grp);
		oapiRenderHUD (hmesh, &hTex);
		oapiDeleteMesh (hmesh);
	}
}

// --------------------------------------------------------------
// Setup the virtual cockpit instruments
// --------------------------------------------------------------
bool ShuttleA::clbkLoadVC (int id)
{
	static VCMFDSPEC mfds_left  = {1, 23};
	static VCMFDSPEC mfds_right = {1, 24}; 
	static VCHUDSPEC hud_pilot  = {1, 52,{-0.600,2.441,16.5381},0.38};
	ReleaseSurfaces();

	switch (id) {
	case 0:
		// MFDs on the front panel
		oapiVCRegisterMFD (MFD_LEFT, &mfds_left);
		oapiVCRegisterMFD (MFD_RIGHT, &mfds_right);
		oapiVCRegisterHUD (&hud_pilot);
		
		oapiVCRegisterArea (AID_ENGINEPODLEVEL, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED);
		oapiVCSetAreaClickmode_Quadrilateral (AID_ENGINEPODLEVEL,
					_V(-0.662748635f,2.000830898f,16.28952821f),
					_V(-0.637967796f,2.001747096f,16.2875631f),
					_V(-0.662748635f,1.951983908f,16.26675439f),
					_V(-0.637967796f,1.952900106f,16.26478928f));

		oapiVCRegisterArea (AID_ENGINEHOVER, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED);
		oapiVCSetAreaClickmode_Quadrilateral (AID_ENGINEHOVER,
					_V(-0.610236858f,2.113885847f,16.33716825f),
					_V(-0.584865999f,2.114823859f,16.33515635f),
					_V(-0.610236858f,2.017265428f,16.29212113f),
					_V(-0.584865999f,2.01820344f,16.29010923f));

		oapiVCRegisterArea (AID_ENGINEMAIN, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED);
		oapiVCSetAreaClickmode_Quadrilateral (AID_ENGINEMAIN,
					_V(-0.662158615f,2.111966194f,16.34128561f),
					_V(-0.637377776f,2.112882392f,16.3393205f),
					_V(-0.662158615f,2.015345775f,16.29623849f),
					_V(-0.637377776f,2.016261973f,16.29427338f));

		SURFHANDLE tex1 = oapiGetTextureHandle (vcmesh_tpl,10); //engine thrust tex
		oapiVCRegisterArea (AID_ENGINEINDICATOR, _R( 4,4, 129,194), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND, tex1);

		tex1 = oapiGetTextureHandle (vcmesh_tpl,11); //podangle tex
		oapiVCRegisterArea (AID_PODANGLEINDICATOR, _R( 0,0, 150,51), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND, tex1);
		
		tex1 = oapiGetTextureHandle (vcmesh_tpl,12); //navmode tex
		oapiVCRegisterArea (AID_NAVMODE, _R( 0,0, 262,32), PANEL_REDRAW_MOUSE|PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN, PANEL_MAP_BACKGROUND, tex1);
		oapiVCSetAreaClickmode_Quadrilateral (AID_NAVMODE,
					_V(-0.843884764f,2.123497933f,16.36420527f),
					_V(-0.689299533f,2.129213263f,16.35194677f),
					_V(-0.843884764f,2.106857749f,16.35644716f),
					_V(-0.689299533f,2.112573079f,16.34418865f));
		
		oapiVCRegisterArea (AID_PODANGLEPRESET, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral (AID_PODANGLEPRESET,
					_V(-0.553004921f,2.007572246f,16.28207691f),
					_V(-0.528224082f,2.008488444f,16.28011181f),
					_V(-0.553004921f,1.980733241f,16.26956382f),
					_V(-0.528224082f,1.981649439f,16.26759872f));

		oapiVCRegisterArea (AID_PODANGLESWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_LBUP);
		oapiVCSetAreaClickmode_Quadrilateral (AID_PODANGLESWITCH,
					_V(-0.602566598f,2.000372049f,16.2835045f),
					_V(-0.5730656f,2.001462761f,16.28116509f),
					_V(-0.602566598f,1.976753725f,16.27249298f),
					_V(-0.5730656f,1.977844437f,16.27015357f));

		oapiVCRegisterArea (AID_ATTITUDEMODE, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral (AID_ATTITUDEMODE,
					_V(-0.485742645f,2.112584069f,16.32454306f),
					_V(-0.473942246f,2.113020354f,16.32360729f),
					_V(-0.485742645f,2.088965745f,16.31353154f),
					_V(-0.473942246f,2.089402029f,16.31259578f));

		// MFD1  buttons 
		tex1 = oapiGetTextureHandle (vcmesh_tpl,13); //mfd buttons tex
		oapiVCRegisterArea (AID_MFD1_LBUTTONS, _R( 8 ,9, 32,218), PANEL_REDRAW_MOUSE|PANEL_REDRAW_USER,PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, PANEL_MAP_BACKGROUND, tex1);		
		oapiVCSetAreaClickmode_Quadrilateral(AID_MFD1_LBUTTONS,
							_V(-0.873975f, 2.07783f, 16.3458f),
							_V(-0.898166f, 2.07693f, 16.3477f),
							_V(-0.873975f, 1.95705f, 16.2895f),
							_V(-0.898166f, 1.95616f, 16.2914f));
	
		oapiVCRegisterArea (AID_MFD1_RBUTTONS, _R( 49,9, 73,218), PANEL_REDRAW_MOUSE|PANEL_REDRAW_USER,PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, PANEL_MAP_BACKGROUND, tex1);
		oapiVCSetAreaClickmode_Quadrilateral(AID_MFD1_RBUTTONS,
							_V(-0.691069f, 2.08459f, 16.3313f),
							_V(-0.715260f, 2.08459f, 16.3313f),
							_V(-0.691069f, 1.96381f, 16.2750f),
							_V(-0.715260f, 1.96381f, 16.2750f));

		oapiVCRegisterArea (AID_MFD1_BBUTTONS,PANEL_REDRAW_NEVER,PANEL_MOUSE_LBDOWN|PANEL_MOUSE_ONREPLAY);
		oapiVCSetAreaClickmode_Quadrilateral(AID_MFD1_BBUTTONS,
					_V(-0.833854424f,1.94565778f,16.28032296f),
					_V(-0.755381769f,1.948559073f,16.27410013f),
					_V(-0.833854424f,1.935458958f,16.27556799f),
					_V(-0.755381769f,1.938360251f,16.26934516f));
		
		//MFD2 buttons 
		oapiVCRegisterArea (AID_MFD2_LBUTTONS, _R(90 ,9,114,218), PANEL_REDRAW_MOUSE|PANEL_REDRAW_USER,PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, PANEL_MAP_BACKGROUND, tex1);		
		oapiVCSetAreaClickmode_Quadrilateral(AID_MFD2_LBUTTONS,
							_V(-0.424380f, 2.09445f, 16.3101f),
							_V(-0.448571f, 2.09356f, 16.3120f),
							_V(-0.424380f, 1.97367f, 16.2538f),
							_V(-0.448571f, 1.97278f, 16.2557f));
	
		oapiVCRegisterArea (AID_MFD2_RBUTTONS, _R( 131,9, 155,218), PANEL_REDRAW_MOUSE|PANEL_REDRAW_USER,PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, PANEL_MAP_BACKGROUND, tex1);
		oapiVCSetAreaClickmode_Quadrilateral(AID_MFD2_RBUTTONS,
							_V(-0.241474f, 2.10121f, 16.2956f),
							_V(-0.265665f, 2.10032f, 16.2975f),
							_V(-0.241474f, 1.98044f, 16.2393f),
							_V(-0.265665f, 1.97954f, 16.2412f));

		oapiVCRegisterArea (AID_MFD2_BBUTTONS,PANEL_REDRAW_NEVER,PANEL_MOUSE_LBDOWN|PANEL_MOUSE_ONREPLAY);
		oapiVCSetAreaClickmode_Quadrilateral(AID_MFD2_BBUTTONS,
					_V(-0.384259211f,2.015958238f,16.26969654f),
					_V(-0.305786556f,2.018859532f,16.26347371f),
					_V(-0.384259211f,1.952081406f,16.23991538f),
					_V(-0.305786556f,1.954982699f,16.23369255f));
			
		
		//OVERHEAD Panel
		tex1 = oapiGetTextureHandle (vcmesh_tpl,14); //fuel management tex
		oapiVCRegisterArea (AID_FUELSTATUS1,_R( 127,  77, 147, 232), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_CURRENT,tex1);
		oapiVCRegisterArea (AID_FUELSTATUS2,_R( 171,  19, 252,  84), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_CURRENT,tex1);
		oapiVCRegisterArea (AID_FUELSTATUS3,_R( 206, 186, 247, 242), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND,tex1);
		oapiVCRegisterArea (AID_FUELSTATUS4,_R( 118,  19, 159,  57), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND,tex1);

		tex1 = oapiGetTextureHandle (vcmesh_tpl,15); //indicator tex
		oapiVCRegisterArea(AID_DOCKINDICATOR,    _R(0,0,34,8), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND,tex1);
		oapiVCRegisterArea(AID_AIRLOCK1INDICATOR,_R(0,8,34,16), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND,tex1);
		oapiVCRegisterArea(AID_GEARINDICATOR,    _R(0,16,34,24), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND,tex1);
		oapiVCRegisterArea(AID_CARGOARMINDICATOR,_R(0,24,34,32), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND,tex1);

		oapiVCRegisterArea(AID_DOCKSWITCH,PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral(AID_DOCKSWITCH,
						_V(-0.227115067f,2.608473414f,16.10024382f),
						_V(-0.227115067f,2.609185081f,16.08870423f),
						_V(-0.209934233f,2.591621747f,16.09921048f),
						_V(-0.209934233f,2.592333414f,16.0876709f));

		oapiVCRegisterArea(AID_AIRLOCK1SWITCH,PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral(AID_AIRLOCK1SWITCH,
						_V(-0.258040567f,2.638747108f,16.10306545f),
						_V(-0.258040567f,2.639458775f,16.09152586f),
						_V(-0.240859733f,2.621895442f,16.10203211f),
						_V(-0.240859733f,2.622607108f,16.09049253f));
		oapiVCRegisterArea(AID_GEARSWITCH,PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral(AID_GEARSWITCH,
						_V(-0.227115067f,2.609985706f,16.0757222f),
						_V(-0.227115067f,2.610697372f,16.06418262f),
						_V(-0.209934233f,2.593134039f,16.07468887f),
						_V(-0.209934233f,2.593845706f,16.06314928f));

		tex1 = oapiGetTextureHandle (vcmesh_tpl,16); //cargo tex
		oapiVCRegisterArea(AID_CARGO_OPEN, _R( 0,  25, 84, 156), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN,PANEL_MAP_BACKGROUND,tex1);					
		oapiVCSetAreaClickmode_Quadrilateral(AID_CARGO_OPEN,
						_V(-0.250824617f,2.635761492f,16.03627884f),
						_V(-0.250824617f,2.638222672f,15.99637112f),
						_V(-0.20615445f,2.591947158f,16.03359218f),
						_V(-0.20615445f,2.594408339f,15.99368445f));

		oapiVCRegisterArea(AID_GARGOARMSWITCH,PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN);
		oapiVCSetAreaClickmode_Quadrilateral(AID_GARGOARMSWITCH,
						_V(-0.227115067f,2.615708692f,15.98292472f),
						_V(-0.227115067f,2.616420358f,15.97138514f),
						_V(-0.209934233f,2.598857025f,15.98189139f),
						_V(-0.209934233f,2.599568692f,15.9703518f));


		
	};

	InitPanel(-1);// VC is -1
 return true;
}

// --------------------------------------------------------------
// Respond to virtual cockpit mouse events
// --------------------------------------------------------------
bool ShuttleA::clbkVCMouseEvent (int id, int event, VECTOR3 &p)
{
	static int ctrl = 3;
	float th_level;
	int mode,pmode;
	int mfd;
	int mx,my;
	
	switch (id) {
	case AID_MFD1_LBUTTONS:
	case AID_MFD1_RBUTTONS:
	case AID_MFD2_LBUTTONS:
	case AID_MFD2_RBUTTONS:
		my=(int)(p.y*209);
		if (my%39 < 20) {
			mfd = (id <= AID_MFD1_RBUTTONS ? MFD_LEFT : MFD_RIGHT);
			int bt = my/39 + (id == AID_MFD1_LBUTTONS || id == AID_MFD2_LBUTTONS ? 0 : 6);
			oapiProcessMFDButton (mfd, bt, event);
			return true;
		}
		break;
	case AID_MFD1_BBUTTONS:
	case AID_MFD2_BBUTTONS:
		mfd = (id == AID_MFD1_BBUTTONS ? MFD_LEFT : MFD_RIGHT);
		mx=(int)(p.x * 133.0f);
		if      (mx <  32)            oapiToggleMFD_on (mfd);
		else if (mx >= 50 && mx < 82) oapiSendMFDKey (mfd, OAPI_KEY_F1);
		else if (mx >= 100)           oapiSendMFDKey (mfd, OAPI_KEY_GRAVE);
		return true;
	case AID_ENGINEPODLEVEL: //engine pog thruster
		
		if (p.x<  0.2) ctrl = 2;  // left engine
				else if (p.x >0.8) ctrl = 1;  // right engine
				else               ctrl = 3;  // both
			th_level=1.1f-(float)p.y*1.2f;
			if (th_level<0.0f) th_level=0.0f;
			if (th_level>1.0f) th_level=1.0f;
			if (ctrl & 1) SetThrusterLevel (th_pod[1], th_level);
			if (ctrl & 2) SetThrusterLevel (th_pod[0], th_level);

			return true;
	case AID_ENGINEHOVER: //engine hover thruster
			if (p.x<  0.2) ctrl = 1;  // left engine
				else if (p.x >0.8) ctrl = 2;  // right engine
				else             ctrl = 3;  // both
			th_level=1.1f-(float)p.y*1.2f;
			if (th_level<0.0f) th_level=0.0f;
			if (th_level>1.0f)  th_level=1.0f;
			if (ctrl & 1)	SetThrusterLevel (th_hover[0], th_level);
			if (ctrl & 2)   SetThrusterLevel (th_hover[1], th_level);
			return true;
	case AID_ENGINEMAIN: //engine main thruster
			if (p.x<  0.2) ctrl = 1;  // left engine
				else if (p.x >0.8) ctrl = 2;  // right engine
				else             ctrl = 3;  // both
			th_level=1.1f-(float)p.y*1.2f;
			if (th_level<0.0f) th_level=0.0f;
			if (th_level>1.0f)  th_level=1.0f;
			if (ctrl & 1)	SetThrusterLevel (th_main[0], th_level);
			if (ctrl & 2)   SetThrusterLevel (th_main[1], th_level);
			
			return true;
	case AID_NAVMODE:
		ctrl =(int)(p.x*262.0f);
		if ((ctrl % 44) < 42) {
			ToggleNavmode (6 -ctrl/44);
		//	oapiTriggerPanelRedrawArea (0, AID_NAVMODE);
			return true;
		}
		break;
	case AID_PODANGLEPRESET:
		ctrl =(int)(p.y*50.0f);
		if ((ctrl % 18) > 14) return false;
		mode = ctrl / 18;
		CommandPodAngle (3, (2-mode)*0.5*PI);
		return true;
	case AID_PODANGLESWITCH:
	
		ctrl=3; // always both from VC !? ?
		//pmode = podswitch[0];
		my=(int)(p.y*44.0f);
		if (event & PANEL_MOUSE_LBUP) mode = 0;
		else                          mode = (my < 22 ? 1:2);
		return RotatePods (ctrl, mode);
		//RotatePod (0, podswitch[0] = mode);
		//RotatePod (1, podswitch[1] = mode);
		//return mode != pmode;
	case AID_ATTITUDEMODE:
		pmode = GetAttitudeMode();
		my =(int)(p.y*44);
		if (my < 22) {
			static int nmode[3] = {1,2,2};
			mode = nmode[pmode];
		} else {
			static int nmode[3] = {0,0,1};
			mode = nmode[pmode];
		}
		if (mode != pmode) SetAttitudeMode (mode);
		return mode != pmode;
	case AID_DOCKSWITCH:
		ActivateDockingPort (p.y < 0.5 ? DOOR_CLOSING : DOOR_OPENING);
		return true;
	case AID_AIRLOCK1SWITCH:
		ActivateAirlock (0, p.y < 0.5 ? DOOR_CLOSING : DOOR_OPENING);
		return true;
	case AID_AIRLOCK2SWITCH:
		ActivateAirlock (1, p.y < 0.5 ? DOOR_CLOSING : DOOR_OPENING);
		return true;
	case AID_GEARSWITCH:
		ActivateLandingGear(p.y<0.5 ? DOOR_CLOSING:DOOR_OPENING);
		return true;
	case AID_CARGO_OPEN:
		mx=(int)(p.x*83);
		my=(int)(p.y*130);
		mode =(mx >45?0:3);
		mode +=	(my / 44);
		if (ToggleGrapple(mode))
			cargo_open[mode]=!cargo_open[mode];		
		return true;
	case AID_GARGOARMSWITCH:
		ActivateCargo(p.y>0.5? 1:0);
		return true;

	}
return false;
}


// --------------------------------------------------------------
// Draw the virtual cockpit instruments
// --------------------------------------------------------------
bool ShuttleA::clbkVCRedrawEvent (int id, int event, SURFHANDLE surf)
{

	switch (id) {
	case AID_MFD1_LBUTTONS:
			RedrawPanel_MFDButton (surf, MFD_LEFT, 0);
			return true;
	case AID_MFD1_RBUTTONS:
			RedrawPanel_MFDButton (surf, MFD_LEFT, 1);
			return true;
	case AID_MFD2_LBUTTONS:
			RedrawPanel_MFDButton (surf, MFD_RIGHT, 0);
			return true;
	case AID_MFD2_RBUTTONS:
			RedrawPanel_MFDButton (surf, MFD_RIGHT, 1);
			return true;
	case AID_ENGINEPODLEVEL:
			RedrawVC_ThPOD();
			return false;
	case AID_ENGINEHOVER:
			RedrawVC_ThHover();
			return false;
	case AID_ENGINEMAIN:
			RedrawVC_ThMain();
			return false;
	case AID_ENGINEINDICATOR:
			return RedrawPanel_EngineIndicator (surf);
	case AID_PODANGLEINDICATOR:
			return RedrawPanel_PodangleIndicator (surf);
	case AID_NAVMODE:
			RedrawPanel_Navmode (surf);
			return true;
	case AID_PODANGLESWITCH:
			if (podswitch[0]==0)
						SetAnimation (anim_pod_angle, 0.5f);
			else if (podswitch[1]>1)
						SetAnimation (anim_pod_angle, 0.0f);
			else SetAnimation (anim_pod_angle,1.0f);
			return false;
	case AID_ATTITUDEMODE:
			SetAnimation(anim_rcs_mode,(float)(GetAttitudeMode())/2.0f);
			return false;
	// panel 1 events:
	case AID_FUELSTATUS1:
	case AID_FUELSTATUS2:
	case AID_FUELSTATUS3:
	case AID_FUELSTATUS4:
			RedrawPanel_Fuelstatus (surf, id-AID_FUELSTATUS1);
			return true;
	case AID_AIRLOCK1INDICATOR:
		switch (lock_status[0]) {
		case DOOR_CLOSED: oapiBlt (surf, srf[3], 0, 0, 0,  0, 34, 8); break;
		case DOOR_OPEN:   oapiBlt (surf, srf[3], 0, 0, 0, 16, 34, 8); break;
		default:          oapiBlt (surf, srf[3], 0, 0, 0,  8, 34, 8); break;
		}
		return true;
	case AID_DOCKINDICATOR:
		switch (dock_status) {
		case DOOR_CLOSED: oapiBlt (surf, srf[4], 0, 0, 0,  0, 34, 8); break;
		case DOOR_OPEN:   oapiBlt (surf, srf[4], 0, 0, 0, 16, 34, 8); break;
		default:          oapiBlt (surf, srf[4], 0, 0, 0,  8, 34, 8); break;
		}
		return true;
	case AID_GEARINDICATOR:
		switch (gear_status) {
		case DOOR_CLOSED: oapiBlt (surf, srf[4], 0, 0, 0,  0, 34, 8); break;
		case DOOR_OPEN:   oapiBlt (surf, srf[4], 0, 0, 0, 16, 34, 8); break;
		default:          oapiBlt (surf, srf[4], 0, 0, 0,  8, 34, 8); break;
		}
		return true;
	case AID_DOCKSWITCH:
		  SetAnimation  (anim_dock_switch,dock_status == DOOR_OPEN ||
			dock_status == DOOR_OPENING ? 0.0:1.0);
		return false;
	case AID_AIRLOCK1SWITCH:
			  SetAnimation  (anim_airlock_switch,lock_status[0] == DOOR_OPEN ||
			lock_status[0] == DOOR_OPENING ? 0.0:1.0);
		return false;
	case AID_GEARSWITCH:
			  SetAnimation  (anim_gear_switch,gear_status == DOOR_OPEN ||
			gear_status == DOOR_OPENING ? 0.0:1.0);
		return false;
	case AID_CARGO_OPEN:
		RedrawPanel_CargoOpen(surf);
		return true;
	case AID_GARGOARMSWITCH:
		SetAnimation (anim_cargo_switch,cargo_arm_status == 1 ? 0.0:1.0);
		return false;
	case AID_CARGOARMINDICATOR:
		oapiBlt (surf, srf[4], 0, 0, 0,  cargo_arm_status == 1? 16:0, 34, 8); 
		 return true;
		

	}
return false;
};

// ==============================================================
// API callback interface
// ==============================================================

// --------------------------------------------------------------
// Module initialisation
// --------------------------------------------------------------
DLLCLBK void InitModule (HINSTANCE hModule)
{
	g_Param.hDLL = hModule;
	oapiRegisterCustomControls (hModule);

	// allocate GDI resources
	g_Param.hFont[0] = CreateFont (-10, 0, 0, 0, 400, 0, 0, 0, 0, 0, 0, 0, 0, "Arial");
	g_Param.hPen[0] = CreatePen (PS_SOLID, 3, RGB (120,220,120));
	g_Param.hPen[1] = CreatePen (PS_SOLID, 1, RGB (220,220,120));
	g_Param.hPen[2] = CreatePen (PS_SOLID, 1, RGB (0,0,0));
	g_Param.hBrush[0] = CreateSolidBrush (RGB(0,128,0));
	g_Param.hBrush[1] = CreateSolidBrush (RGB(0,0,0));

	// load 2D panel texture
	ShuttleA::panel2dtex = oapiLoadTexture ("ShuttleA\\panel2d.dds");
	ShuttleA::paneleltex = oapiLoadTexture ("ShuttleA\\panel_el.dds");
	ShuttleA::aditex = oapiLoadTexture ("Common\\adiball_grey.dds");
}

// --------------------------------------------------------------
// Module cleanup
// --------------------------------------------------------------
DLLCLBK void ExitModule (HINSTANCE hModule)
{
	int i;
	// deallocate GDI resources
	for (i = 0; i < 1; i++) DeleteObject (g_Param.hFont[i]);
	for (i = 0; i < 3; i++) DeleteObject (g_Param.hPen[i]);
	for (i = 0; i < 2; i++) DeleteObject (g_Param.hBrush[i]);

	// deallocated 2D panel texture
	oapiDestroySurface (ShuttleA::panel2dtex);
	oapiDestroySurface (ShuttleA::paneleltex);
	oapiDestroySurface (ShuttleA::aditex);
	oapiUnregisterCustomControls (g_Param.hDLL);
}

// --------------------------------------------------------------
// Vessel initialisation
// --------------------------------------------------------------
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new ShuttleA (hvessel, flightmodel);
}

// --------------------------------------------------------------
// Vessel cleanup
// --------------------------------------------------------------
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (ShuttleA*)vessel;
}

// ==============================================================
// Scenario editor interface
// ==============================================================

ShuttleA *GetV (HWND hDlg)
{
	// retrieve DG interface from scenario editor
	OBJHANDLE hVessel;
	SendMessage (hDlg, WM_SCNEDITOR, SE_GETVESSEL, (LPARAM)&hVessel);
	return (ShuttleA*)oapiGetVesselInterface (hVessel);
}

void UpdatePodSliders (HWND hDlg, ShuttleA *v)
{
	int lpos = (int)(v->GetPodAngle(0)/PI*100.0+0.5);
	int rpos = (int)(v->GetPodAngle(1)/PI*100.0+0.5);
	oapiSetGaugePos (GetDlgItem (hDlg, IDC_LAUX_POS), lpos);
	oapiSetGaugePos (GetDlgItem (hDlg, IDC_RAUX_POS), rpos);
	oapiSetGaugePos (GetDlgItem (hDlg, IDC_AUX_POS), (lpos+rpos)/2);
}

void InitEdPg1 (HWND hDlg, OBJHANDLE hVessel)
{
	ShuttleA *v = (ShuttleA*)oapiGetVesselInterface (hVessel);
	GAUGEPARAM gp = { 0, 100, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
	oapiSetGaugeParams (GetDlgItem (hDlg, IDC_LAUX_POS), &gp);
	oapiSetGaugeParams (GetDlgItem (hDlg, IDC_RAUX_POS), &gp);
	oapiSetGaugeParams (GetDlgItem (hDlg, IDC_AUX_POS), &gp);
	ShowWindow (GetDlgItem (hDlg, IDC_LAUX_POS), SW_HIDE);
	ShowWindow (GetDlgItem (hDlg, IDC_RAUX_POS), SW_HIDE);
	ShowWindow (GetDlgItem (hDlg, IDC_AUX_POS), SW_SHOW);
	SendDlgItemMessage (hDlg, IDC_AUX_SYNC, BM_SETCHECK, BST_CHECKED, 0);
	UpdatePodSliders (hDlg, v);
}

// --------------------------------------------------------------
// Message procedure for editor page 1 (animation settings)
// --------------------------------------------------------------
INT_PTR CALLBACK EdPg1Proc (HWND hTab, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		InitEdPg1 (hTab, (OBJHANDLE)lParam);
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_GEAR_UP:
			GetV(hTab)->ActivateLandingGear (ShuttleA::DOOR_OPEN);
			return TRUE;
		case IDC_GEAR_DOWN:
			GetV(hTab)->ActivateLandingGear (ShuttleA::DOOR_CLOSED);
			return TRUE;
		case IDC_DPORT_CLOSE:
			GetV(hTab)->ActivateDockingPort (ShuttleA::DOOR_CLOSED);
			return TRUE;
		case IDC_DPORT_OPEN:
			GetV(hTab)->ActivateDockingPort (ShuttleA::DOOR_OPEN);
			return TRUE;
		case IDC_OLOCK_CLOSE:
			GetV(hTab)->ActivateAirlock (0, ShuttleA::DOOR_CLOSED);
			return TRUE;
		case IDC_OLOCK_OPEN:
			GetV(hTab)->ActivateAirlock (0, ShuttleA::DOOR_OPEN);
			return TRUE;
		case IDC_AUX_RETRO: {
			ShuttleA *v = GetV(hTab);
			v->SetPodAngle (3, 0.0);
			UpdatePodSliders (hTab, v);
			} return TRUE;
		case IDC_AUX_HOVER: {
			ShuttleA *v = GetV(hTab);
			v->SetPodAngle (3, PI05);
			UpdatePodSliders (hTab, v);
			} return TRUE;
		case IDC_AUX_FWD: {
			ShuttleA *v = GetV(hTab);
			v->SetPodAngle (3, PI);
			UpdatePodSliders (hTab, v);
			} return TRUE;
		case IDC_AUX_SYNC:
			if (SendDlgItemMessage (hTab, IDC_AUX_SYNC, BM_GETCHECK, 0, 0) == BST_CHECKED) {
				ShowWindow (GetDlgItem (hTab, IDC_LAUX_POS), SW_HIDE);
				ShowWindow (GetDlgItem (hTab, IDC_RAUX_POS), SW_HIDE);
				ShowWindow (GetDlgItem (hTab, IDC_AUX_POS), SW_SHOW);
				GetV(hTab)->SetPodAngle (3, oapiGetGaugePos (GetDlgItem (hTab, IDC_AUX_POS))*0.01*PI);
			} else {
				ShowWindow (GetDlgItem (hTab, IDC_AUX_POS), SW_HIDE);
				ShowWindow (GetDlgItem (hTab, IDC_LAUX_POS), SW_SHOW);
				ShowWindow (GetDlgItem (hTab, IDC_RAUX_POS), SW_SHOW);
			}
		}
		break;
	case WM_HSCROLL: {
		ShuttleA *v = GetV (hTab);
		int id = GetDlgCtrlID ((HWND)lParam);
		switch (id) {
		case IDC_LAUX_POS:
		case IDC_RAUX_POS:
		case IDC_AUX_POS:
			switch (LOWORD (wParam)) {
			case SB_THUMBTRACK:
			case SB_LINELEFT:
			case SB_LINERIGHT:
				if (id == IDC_LAUX_POS || id == IDC_AUX_POS)
					v->SetPodAngle (1, HIWORD(wParam)*0.01*PI);
				if (id == IDC_RAUX_POS || id == IDC_AUX_POS)
					v->SetPodAngle (2, HIWORD(wParam)*0.01*PI);
				UpdatePodSliders (hTab, v);
				return TRUE;
			}
			break;
		}
		} break;
	}
	return FALSE;
}

// --------------------------------------------------------------
// Add vessel-specific pages into scenario editor
// --------------------------------------------------------------
DLLCLBK void secInit (HWND hEditor, OBJHANDLE hVessel)
{
	EditorPageSpec eps1 = {"Animations", g_Param.hDLL, IDD_EDITOR_PG1, EdPg1Proc};
	SendMessage (hEditor, WM_SCNEDITOR, SE_ADDPAGEBUTTON, (LPARAM)&eps1);
}
