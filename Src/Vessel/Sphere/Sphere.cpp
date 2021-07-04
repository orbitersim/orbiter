// ==============================================================
//                 ORBITER MODULE: Sphere
//                  Part of the ORBITER SDK
//          Copyright (C) 2002-2004 Martin Schweiger
//                   All rights reserved
//
// Sphere.cpp
// Control module for Sphere vessel class
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
const VECTOR3 PB_TDP[3]     = {{0,-1.5,2},{-1,-1.5,-1.5},{1,-1.5,-1.5}}; // touchdown points [m]
const VECTOR3 PB_COP        = {0,0,0};         // centre of pressure for airfoils [m]
const double  PB_VLIFT_C    = 2.0;             // chord length [m]
const double  PB_VLIFT_S    = 2.0;             // wing area [m^2]
const double  PB_VLIFT_A    = 2.0;             // wing aspect ratio
const double  PB_HLIFT_C    = 2.0;             // chord length [m]
const double  PB_HLIFT_S    = 2.0;             // wing area [m^2]
const double  PB_HLIFT_A    = 2.0;             // wing aspect ratio

// ==============================================================
// Sphere class interface
// ==============================================================

class Sphere: public VESSEL3 {
public:
	Sphere (OBJHANDLE hVessel, int flightmodel);
	~Sphere ();
	void clbkSetClassCaps (FILEHANDLE cfg);

private:
	static void vlift (VESSEL *v, double aoa, double M, double Re,
		void *context, double *cl, double *cm, double *cd);
	static void hlift (VESSEL *v, double aoa, double M, double Re,
		void *context, double *cl, double *cm, double *cd);
};

Sphere::Sphere (OBJHANDLE hVessel, int flightmodel)
: VESSEL3 (hVessel, flightmodel)
{
}

Sphere::~Sphere ()
{
}

#ifdef UNDEF
static const DWORD ntdvtx_geardown = 42;
static TOUCHDOWNVTX tdvtx_geardown[ntdvtx_geardown] = {
	{_V(0, 0, 1), 1e5, 1e2},
	{_V(0.27639, 0.85065, 0.44721), 1e5, 1e2},
	{_V(0.89443, 0, 0.44721), 1e5, 1e2},
	{_V(-0.72361, 0.52573, 0.44721), 1e5, 1e2},
	{_V(-0.72361, -0.52573, 0.44721), 1e5, 1e2},
	{_V(0.27639, -0.85065, 0.44721), 1e5, 1e2},
	{_V(0.72361, 0.52573, -0.44721), 1e5, 1e2},
	{_V(-0.27639, 0.85065, -0.44721), 1e5, 1e2},
	{_V(-0.89443, 0, -0.44721), 1e5, 1e2},
	{_V(-0.27639, -0.85065, -0.44721), 1e5, 1e2},
	{_V(0.72361, -0.52573, -0.44721), 1e5, 1e2},
	{_V(0, 0, -1), 1e5, 1e2},
	{_V(0.52573, 0, 0.85065), 1e5, 1e2},
	{_V(0.16246, 0.5, 0.85065), 1e5, 1e2},
	{_V(0.68819, 0.5, 0.52573), 1e5, 1e2},
	{_V(-0.42533, 0.30902, 0.85065), 1e5, 1e2},
	{_V(-0.26287, 0.80902, 0.52573), 1e5, 1e2},
	{_V(-0.42533, -0.30902, 0.85065), 1e5, 1e2},
	{_V(-0.85065, 0, 0.52573), 1e5, 1e2},
	{_V(0.16246, -0.5, 0.85065), 1e5, 1e2},
	{_V(-0.26287, -0.80902, 0.52573), 1e5, 1e2},
	{_V(0.68819, -0.5, 0.52573), 1e5, 1e2},
	{_V(0.95106, 0.30902, 0), 1e5, 1e2},
	{_V(0.58779, 0.80902, 0), 1e5, 1e2},
	{_V(0, 1, 0), 1e5, 1e2},
	{_V(-0.58779, 0.80902, 0), 1e5, 1e2},
	{_V(-0.95106, 0.30902, 0), 1e5, 1e2},
	{_V(-0.95106, -0.30902, 0), 1e5, 1e2},
	{_V(-0.58779, -0.80902, 0), 1e5, 1e2},
	{_V(0, -1, 0), 1e5, 1e2},
	{_V(0.58779, -0.80902, 0), 1e5, 1e2},
	{_V(0.95106, -0.30902, 0), 1e5, 1e2},
	{_V(0.26287, 0.80902, -0.52573), 1e5, 1e2},
	{_V(-0.68819, 0.5, -0.52573), 1e5, 1e2},
	{_V(-0.68819, -0.5, -0.52573), 1e5, 1e2},
	{_V(0.26287, -0.80902, -0.52573), 1e5, 1e2},
	{_V(0.85065, 0, -0.52573), 1e5, 1e2},
	{_V(0.42533, 0.30902, -0.85065), 1e5, 1e2},
	{_V(-0.16246, 0.5, -0.85065), 1e5, 1e2},
	{_V(-0.52573, 0, -0.85065), 1e5, 1e2},
	{_V(-0.16246, -0.5, -0.85065), 1e5, 1e2},
	{_V(0.42533, -0.30902, -0.85065), 1e5, 1e2}
};
#endif

// ==============================================================
// Overloaded callback functions
// ==============================================================

// --------------------------------------------------------------
// Set the capabilities of the vessel class
// --------------------------------------------------------------
void Sphere::clbkSetClassCaps (FILEHANDLE cfg)
{
	// physical vessel parameters
	SetSize (1.0);
	SetEmptyMass (500.0);
	SetPMI (_V(0.4,0.4,0.4));
	SetCrossSections (_V(PI,PI,PI));
	SetRotDrag (_V(0,0,0));
	//SetTouchdownPoints (tdvtx_geardown, ntdvtx_geardown);
	SetSurfaceFrictionCoeff(2e-1,2e-1);

	// airfoil definitions
	CreateAirfoil3 (LIFT_VERTICAL,   PB_COP, vlift, NULL, PB_VLIFT_C, PB_VLIFT_S, PB_VLIFT_A);
	CreateAirfoil3 (LIFT_HORIZONTAL, PB_COP, hlift, NULL, PB_HLIFT_C, PB_HLIFT_S, PB_HLIFT_A);

	// camera parameters
	SetCameraOffset (_V(0,0.8,0));

	// associate a mesh for the visual
	AddMesh ("Sphere3");
}

// ==============================================================
// Airfoil lift/drag functions
// ==============================================================

void Sphere::vlift (VESSEL *v, double aoa, double M, double Re,
	void *context, double *cl, double *cm, double *cd)
{
	*cl = 0;
	*cm = 0;
	*cd = 0.005;
}

void Sphere::hlift (VESSEL *v, double aoa, double M, double Re,
	void *context, double *cl, double *cm, double *cd)
{
	*cl = 0;
	*cm = 0;
	*cd = 0.005;
}

// ==============================================================
// API callback interface
// ==============================================================

// --------------------------------------------------------------
// Vessel initialisation
// --------------------------------------------------------------
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new Sphere (hvessel, flightmodel);
}

// --------------------------------------------------------------
// Vessel cleanup
// --------------------------------------------------------------
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (Sphere*)vessel;
}
