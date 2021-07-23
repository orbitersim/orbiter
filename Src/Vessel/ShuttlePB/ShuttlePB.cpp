// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttlePB
//                  Part of the ORBITER SDK
//
// ShuttlePB.cpp
// Control module for ShuttlePB vessel class
//
// Notes:
// This is an example for a "minimal" vessel implementation which
// only overloads the clbkSetClassCaps method to define vessel
// capabilities and otherwise uses the default VESSEL class
// behaviour.
// ==============================================================

#define STRICT
#define ORBITER_MODULE

#include "orbitersdk.h"

// ==============================================================
// Some vessel parameters
// ==============================================================
const double  PB_SIZE       = 3.5;             // mean radius [m]
const VECTOR3 PB_CS         = {10.5,15.0,5.8}; // x,y,z cross sections [m^2]
const VECTOR3 PB_PMI        = {2.28,2.31,0.79};// principal moments of inertia (mass-normalised) [m^2]
const VECTOR3 PB_RD         = {0.025,0.025,0.02};//{0.05,0.1,0.05};  // rotation drag coefficients
const double  PB_EMPTYMASS  = 500.0;           // empty vessel mass [kg]
const double  PB_FUELMASS   = 750.0;           // max fuel mass [kg]
const double  PB_ISP        = 5e4;             // fuel-specific impulse [m/s]
const VECTOR3 PB_COP        = {0,0,0};//{0,0,-0.1};      // centre of pressure for airfoils [m]
const double  PB_VLIFT_C    = 2.0;             // chord length [m]
const double  PB_VLIFT_S    = 2.0;             // wing area [m^2]
const double  PB_VLIFT_A    = 2.5;             // wing aspect ratio
const double  PB_HLIFT_C    = 2.0;             // chord length [m]
const double  PB_HLIFT_S    = 1.5;             // wing area [m^2]
const double  PB_HLIFT_A    = 2.0;             // wing aspect ratio

const double  PB_MAXMAINTH  = 3e4;             
const double  PB_MAXHOVERTH = 1.5e4;
const double  PB_MAXRCSTH   = 2e2;

const VECTOR3 PB_DOCK_POS   = {0,1.3,-1};      // docking port location [m]
const VECTOR3 PB_DOCK_DIR   = {0,1,0};         // docking port approach direction
const VECTOR3 PB_DOCK_ROT   = {0,0,-1};        // docking port alignment direction

// Define impact convex hull
static const DWORD ntdvtx = 12;
static TOUCHDOWNVTX tdvtx[ntdvtx] = {
	{_V( 0,  -1.5, 2  ), 2e4, 1e3, 1.6, 1},
	{_V(-1,  -1.5,-1.5), 2e4, 1e3, 3.0, 1},
	{_V( 1,  -1.5,-1.5), 2e4, 1e3, 3.0, 1},
	{_V(-0.5,-0.75,3  ), 2e4, 1e3, 3.0},
	{_V( 0.5,-0.75,3  ), 2e4, 1e3, 3.0},
	{_V(-2.6,-1.1,-1.9), 2e4, 1e3, 3.0},
	{_V( 2.6,-1.1,-1.9), 2e4, 1e3, 3.0},
	{_V(-1,   1.3, 0  ), 2e4, 1e3, 3.0},
	{_V( 1,   1.3, 0  ), 2e4, 1e3, 3.0},
	{_V(-1,   1.3,-2  ), 2e4, 1e3, 3.0},
	{_V( 1,   1.3,-2  ), 2e4, 1e3, 3.0},
	{_V( 0,   0.3,-3.8), 2e4, 1e3, 3.0}
};

// Calculate lift coefficient [Cl] as a function of aoa (angle of attack) over -Pi ... Pi
// Implemented here as a piecewise linear function
double LiftCoeff (double aoa)
{
	int i;
	const int nlift = 9;
	static const double AOA[nlift] = {-180*RAD,-60*RAD,-30*RAD,-1*RAD,15*RAD,20*RAD,25*RAD,60*RAD,180*RAD};
	static const double CL[nlift]  = {       0,      0,   -0.1,     0,   0.2,  0.25,   0.2,     0,      0};
	static const double SCL[nlift] = {(CL[1]-CL[0])/(AOA[1]-AOA[0]), (CL[2]-CL[1])/(AOA[2]-AOA[1]),
		                              (CL[3]-CL[2])/(AOA[3]-AOA[2]), (CL[4]-CL[3])/(AOA[4]-AOA[3]),
									  (CL[5]-CL[4])/(AOA[5]-AOA[4]), (CL[6]-CL[5])/(AOA[6]-AOA[5]),
									  (CL[7]-CL[6])/(AOA[7]-AOA[6]), (CL[8]-CL[7])/(AOA[8]-AOA[7])};
	for (i = 0; i < nlift-1 && AOA[i+1] < aoa; i++);
	return CL[i] + (aoa-AOA[i])*SCL[i];
}

// ==============================================================
// Shuttle-PB class interface
// ==============================================================

class ShuttlePB: public VESSEL3 {
public:
	ShuttlePB (OBJHANDLE hVessel, int flightmodel);
	~ShuttlePB ();
	void clbkSetClassCaps (FILEHANDLE cfg);

private:
	static void vlift (VESSEL *v, double aoa, double M, double Re,
		void *context, double *cl, double *cm, double *cd);
	static void hlift (VESSEL *v, double aoa, double M, double Re,
		void *context, double *cl, double *cm, double *cd);

	// transformations for control surface animations
	static MGROUP_ROTATE trans_Laileron, trans_Raileron;
	static MGROUP_ROTATE trans_Lelevator, trans_Relevator;
};

ShuttlePB::ShuttlePB (OBJHANDLE hVessel, int flightmodel)
: VESSEL3 (hVessel, flightmodel)
{
}

ShuttlePB::~ShuttlePB ()
{
}

// animation transformation definitions
static UINT GRP_LWING = 2;
static UINT GRP_RWING = 3;
static VECTOR3 LWING_REF  = {-1.3,-0.725,-1.5};
static VECTOR3 LWING_AXIS = {-0.9619,-0.2735,0};
static VECTOR3 RWING_REF  = {1.3,-0.725,-1.5};
static VECTOR3 RWING_AXIS = {0.9619,-0.2735,0};
static float AILERON_RANGE = (float)(20.0*RAD);
static float ELEVATOR_RANGE = (float)(30.0*RAD);
MGROUP_ROTATE ShuttlePB::trans_Laileron (0, &GRP_LWING, 1, LWING_REF, LWING_AXIS, AILERON_RANGE);
MGROUP_ROTATE ShuttlePB::trans_Raileron (0, &GRP_RWING, 1, RWING_REF, RWING_AXIS, AILERON_RANGE);
MGROUP_ROTATE ShuttlePB::trans_Lelevator (0, &GRP_LWING, 1, LWING_REF, LWING_AXIS, -ELEVATOR_RANGE);
MGROUP_ROTATE ShuttlePB::trans_Relevator (0, &GRP_RWING, 1, RWING_REF, RWING_AXIS, ELEVATOR_RANGE);


// ==============================================================
// Overloaded callback functions
// ==============================================================

// --------------------------------------------------------------
// Set the capabilities of the vessel class
// --------------------------------------------------------------
void ShuttlePB::clbkSetClassCaps (FILEHANDLE cfg)
{
	THRUSTER_HANDLE th_main, th_hover, th_rcs[14], th_group[4];

	// physical vessel parameters
	SetSize (PB_SIZE);
	SetEmptyMass (PB_EMPTYMASS);
	SetPMI (PB_PMI);
	SetCrossSections (PB_CS);
	SetRotDrag (PB_RD);
	SetTouchdownPoints (tdvtx, ntdvtx);

	// docking port definitions
	SetDockParams (PB_DOCK_POS, PB_DOCK_DIR, PB_DOCK_ROT);

	// airfoil definitions
	CreateAirfoil3 (LIFT_VERTICAL,   PB_COP, vlift, NULL, PB_VLIFT_C, PB_VLIFT_S, PB_VLIFT_A);
	CreateAirfoil3 (LIFT_HORIZONTAL, PB_COP, hlift, NULL, PB_HLIFT_C, PB_HLIFT_S, PB_HLIFT_A);

	// control surface animations
	UINT anim_Laileron = CreateAnimation (0.5);
	UINT anim_Raileron = CreateAnimation (0.5);
	UINT anim_elevator = CreateAnimation (0.5);
	AddAnimationComponent (anim_Laileron, 0, 1, &trans_Laileron);
	AddAnimationComponent (anim_Raileron, 0, 1, &trans_Raileron);
	AddAnimationComponent (anim_elevator, 0, 1, &trans_Lelevator);
	AddAnimationComponent (anim_elevator, 0, 1, &trans_Relevator);

	// aerodynamic control surface defintions
	CreateControlSurface (AIRCTRL_ELEVATOR, 1.5, 0.7, _V( 0,0,-2.5), AIRCTRL_AXIS_XPOS, anim_elevator);
	CreateControlSurface (AIRCTRL_AILERON, 1.5, 0.25, _V( 1,0,-2.5), AIRCTRL_AXIS_XPOS, anim_Laileron);
	CreateControlSurface (AIRCTRL_AILERON, 1.5, 0.25, _V(-1,0,-2.5), AIRCTRL_AXIS_XNEG, anim_Raileron);

	// propellant resources
	PROPELLANT_HANDLE hpr = CreatePropellantResource (PB_FUELMASS);

	// main engine
	th_main = CreateThruster (_V(0,0,-4.35), _V(0,0,1), PB_MAXMAINTH, hpr, PB_ISP);
	CreateThrusterGroup (&th_main, 1, THGROUP_MAIN);
	AddExhaust (th_main, 8, 1, _V(0,0.3,-4.35), _V(0,0,-1));

	PARTICLESTREAMSPEC contrail_main = {
		0, 5.0, 16, 200, 0.15, 1.0, 5, 3.0, PARTICLESTREAMSPEC::DIFFUSE,
		PARTICLESTREAMSPEC::LVL_PSQRT, 0, 2,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-4, 1
	};
	PARTICLESTREAMSPEC exhaust_main = {
		0, 2.0, 20, 200, 0.05, 0.1, 8, 1.0, PARTICLESTREAMSPEC::EMISSIVE,
		PARTICLESTREAMSPEC::LVL_SQRT, 0, 1,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-5, 0.1
	};
	AddExhaustStream (th_main, _V(0,0.3,-10), &contrail_main);
	AddExhaustStream (th_main, _V(0,0.3,-5), &exhaust_main);

	// hover engine
	th_hover = CreateThruster (_V(0,-1.5,0), _V(0,1,0), PB_MAXHOVERTH, hpr, PB_ISP);
	CreateThrusterGroup (&th_hover, 1, THGROUP_HOVER);
	AddExhaust (th_hover, 8, 1, _V(0,-1.5,1), _V(0,-1,0));
	AddExhaust (th_hover, 8, 1, _V(0,-1.5,-1), _V(0,-1,0));

	PARTICLESTREAMSPEC contrail_hover = {
		0, 5.0, 8, 200, 0.15, 1.0, 5, 3.0, PARTICLESTREAMSPEC::DIFFUSE,
		PARTICLESTREAMSPEC::LVL_PSQRT, 0, 2,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-4, 1
	};
	PARTICLESTREAMSPEC exhaust_hover = {
		0, 2.0, 10, 200, 0.05, 0.05, 8, 1.0, PARTICLESTREAMSPEC::EMISSIVE,
		PARTICLESTREAMSPEC::LVL_SQRT, 0, 1,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-5, 0.1
	};

	AddExhaustStream (th_hover, _V(0,-3, 1), &contrail_hover);
	AddExhaustStream (th_hover, _V(0,-3,-1), &contrail_hover);
	AddExhaustStream (th_hover, _V(0,-2, 1), &exhaust_hover);
	AddExhaustStream (th_hover, _V(0,-2,-1), &exhaust_hover);

	// RCS engines
	th_rcs[ 0] = CreateThruster (_V( 1,0, 3), _V(0, 1,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 1] = CreateThruster (_V( 1,0, 3), _V(0,-1,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 2] = CreateThruster (_V(-1,0, 3), _V(0, 1,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 3] = CreateThruster (_V(-1,0, 3), _V(0,-1,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 4] = CreateThruster (_V( 1,0,-3), _V(0, 1,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 5] = CreateThruster (_V( 1,0,-3), _V(0,-1,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 6] = CreateThruster (_V(-1,0,-3), _V(0, 1,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 7] = CreateThruster (_V(-1,0,-3), _V(0,-1,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 8] = CreateThruster (_V( 1,0, 3), _V(-1,0,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[ 9] = CreateThruster (_V(-1,0, 3), _V( 1,0,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[10] = CreateThruster (_V( 1,0,-3), _V(-1,0,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[11] = CreateThruster (_V(-1,0,-3), _V( 1,0,0), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[12] = CreateThruster (_V( 0,0,-3), _V(0,0, 1), PB_MAXRCSTH, hpr, PB_ISP);
	th_rcs[13] = CreateThruster (_V( 0,0, 3), _V(0,0,-1), PB_MAXRCSTH, hpr, PB_ISP);

	th_group[0] = th_rcs[0];
	th_group[1] = th_rcs[2];
	th_group[2] = th_rcs[5];
	th_group[3] = th_rcs[7];
	CreateThrusterGroup (th_group, 4, THGROUP_ATT_PITCHUP);

	th_group[0] = th_rcs[1];
	th_group[1] = th_rcs[3];
	th_group[2] = th_rcs[4];
	th_group[3] = th_rcs[6];
	CreateThrusterGroup (th_group, 4, THGROUP_ATT_PITCHDOWN);

	th_group[0] = th_rcs[0];
	th_group[1] = th_rcs[4];
	th_group[2] = th_rcs[3];
	th_group[3] = th_rcs[7];
	CreateThrusterGroup (th_group, 4, THGROUP_ATT_BANKLEFT);

	th_group[0] = th_rcs[1];
	th_group[1] = th_rcs[5];
	th_group[2] = th_rcs[2];
	th_group[3] = th_rcs[6];
	CreateThrusterGroup (th_group, 4, THGROUP_ATT_BANKRIGHT);

	th_group[0] = th_rcs[0];
	th_group[1] = th_rcs[4];
	th_group[2] = th_rcs[2];
	th_group[3] = th_rcs[6];
	CreateThrusterGroup (th_group, 4, THGROUP_ATT_UP);

	th_group[0] = th_rcs[1];
	th_group[1] = th_rcs[5];
	th_group[2] = th_rcs[3];
	th_group[3] = th_rcs[7];
	CreateThrusterGroup (th_group, 4, THGROUP_ATT_DOWN);

	th_group[0] = th_rcs[8];
	th_group[1] = th_rcs[11];
	CreateThrusterGroup (th_group, 2, THGROUP_ATT_YAWLEFT);

	th_group[0] = th_rcs[9];
	th_group[1] = th_rcs[10];
	CreateThrusterGroup (th_group, 2, THGROUP_ATT_YAWRIGHT);

	th_group[0] = th_rcs[8];
	th_group[1] = th_rcs[10];
	CreateThrusterGroup (th_group, 2, THGROUP_ATT_LEFT);

	th_group[0] = th_rcs[9];
	th_group[1] = th_rcs[11];
	CreateThrusterGroup (th_group, 2, THGROUP_ATT_RIGHT);

	CreateThrusterGroup (th_rcs+12, 1, THGROUP_ATT_FORWARD);
	CreateThrusterGroup (th_rcs+13, 1, THGROUP_ATT_BACK);

	// camera parameters
	SetCameraOffset (_V(0,0.8,0));

	// associate a mesh for the visual
	AddMesh ("ShuttlePB");
}

// ==============================================================
// Airfoil lift/drag functions
// ==============================================================

void ShuttlePB::vlift (VESSEL *v, double aoa, double M, double Re,
	void *context, double *cl, double *cm, double *cd)
{
	static const double clp[] = {  // lift coefficient from -pi to pi in 10deg steps
		-0.1,-0.5,-0.4,-0.1,0,0,0,0,0,0,0,0,0,0,-0.2,-0.6,-0.6,-0.4,0.2,0.5,0.9,0.8,0.2,0,0,0,0,0,0,0,0,0,0.1,0.4,0.5,0.3,-0.1,-0.5
	};
	static const double aoa_step = 10.0*RAD;
	double a, fidx, saoa = sin(aoa);
	a = modf((aoa+PI)/aoa_step, &fidx);
	int idx = (int)(fidx+0.5);
	*cl = clp[idx]*(1.0-a) + clp[idx+1]*a;     // linear interpolation
	*cm = 0.0; //-0.03*sin(aoa-0.1);
	*cd = 0.03 + 0.4*saoa*saoa;                // profile drag
	*cd += oapiGetInducedDrag (*cl, 1.0, 0.5); // induced drag
	*cd += oapiGetWaveDrag (M, 0.75, 1.0, 1.1, 0.04);  // wave drag
}

void ShuttlePB::hlift (VESSEL *v, double aoa, double M, double Re,
	void *context, double *cl, double *cm, double *cd)
{
	static const double clp[] = {  // lift coefficient from -pi to pi in 45deg steps
		0,0.4,0,-0.4,0,0.4,0,-0.4,0,0.4
	};
	static const double aoa_step = 45.0*RAD;
	double a, fidx;
	a = modf((aoa+PI)/aoa_step, &fidx);
	int idx = (int)(fidx+0.5);
	*cl = clp[idx]*(1.0-a) + clp[idx+1]*a;     // linear interpolation
	*cm = 0.0;
	*cd = 0.03;
	*cd += oapiGetInducedDrag (*cl, 1.5, 0.6); // induced drag
	*cd += oapiGetWaveDrag (M, 0.75, 1.0, 1.1, 0.04);  // wave drag
}

// ==============================================================
// API callback interface
// ==============================================================

// --------------------------------------------------------------
// Vessel initialisation
// --------------------------------------------------------------
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new ShuttlePB (hvessel, flightmodel);
}

// --------------------------------------------------------------
// Vessel cleanup
// --------------------------------------------------------------
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (ShuttlePB*)vessel;
}
