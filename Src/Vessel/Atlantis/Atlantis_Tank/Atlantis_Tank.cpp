// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Atlantis
//                  Part of the ORBITER SDK
//
// Atlantis_Tank.cpp
// Reference implementation of Atlantis Tank vessel class module
// Note: This module takes control of the tank after separation
// from the orbiter.
// ==============================================================

#define ORBITER_MODULE
#define ATLANTIS_TANK_MODULE

#include "Atlantis.h"
#include "math.h"

static const int ntdvtx = 15;
static const TOUCHDOWNVTX tdvtx[ntdvtx] = {
	{_V( 0,   5.85, 3.1  ), 1e9, 1e7, 0.3 },
	{_V( 2.4, 5.1, -21.3 ), 1e9, 1e7, 0.3 },
	{_V(-2.4, 5.1, -21.3 ), 1e9, 1e7, 0.3 },
	{_V( 4.2, 0,   -21   ), 1e9, 1e7, 0.3 },
	{_V( 2.1,-3.637,-21  ), 1e9, 1e7, 0.3 },
	{_V(-2.1,-3.637,-21  ), 1e9, 1e7, 0.3 },
	{_V(-4.2, 0,   -21   ), 1e9, 1e7, 0.3 },
	{_V( 4.2, 0,    15   ), 1e9, 1e7, 0.3 },
	{_V( 2.1,-3.637,15   ), 1e9, 1e7, 0.3 },
	{_V(-2.1,-3.637,15   ), 1e9, 1e7, 0.3 },
	{_V(-4.2, 0,    15   ), 1e9, 1e7, 0.3 },
	{_V(-2.1, 3.637,15   ), 1e9, 1e7, 0.3 },
	{_V( 2.1, 3.637,15   ), 1e9, 1e7, 0.3 },
	{_V( 0,   0,   -23.57), 1e9, 1e7, 0.3 },
	{_V( 0,   0,    23.57), 1e9, 1e7, 0.3 }
};

// ==============================================================
// Specialised vessel class Atlantis_SRB
// ==============================================================

// Constructor
Atlantis_Tank::Atlantis_Tank (OBJHANDLE hObj)
: VESSEL2(hObj)
{
	// preload mesh
	hTankMesh = oapiLoadMeshGlobal ("Atlantis\\Atlantis_tank");

	// The fuel tank. Note that this is accessed remotely by the orbiter
	// while it is connected
	hProp = CreatePropellantResource (TANK_MAX_PROPELLANT_MASS);

	// docking ports
	CreateDock (_V(0,5.5,-5), _V(0,1,0), _V(0,0,1));      // orbiter attachment
	CreateDock (_V(-4.25,0,-6.4), _V(-1,0,0), _V(0,0,1)); // left SRB attachment
	CreateDock (_V( 4.25,0,-6.4), _V( 1,0,0), _V(0,0,1)); // right SRB attachment

	// by default, disable orbiter and SRB connectors
	hDockOrbiter = NULL;

	pAtlantis = NULL;
	pSRB[0] = NULL;
	pSRB[1] = NULL;
}

// ==============================================================
// Callback functions
// ==============================================================

// Set Tank class specs
void Atlantis_Tank::clbkSetClassCaps (FILEHANDLE cfg)
{
	SetEnableFocus (false);
	// Tank cannot receive input focus

	SetSize (24.0);
	SetEmptyMass (TANK_EMPTY_MASS);

	//SetMaxFuelMass (TANK_MAX_PROPELLANT_MASS);
	// Note that the Tank instance is only created after separation from
	// the orbiter, so the actual fuel mass will always be much smaller

	SetISP (5000.0);

	SetMaxThrust (ENGINE_MAIN, 0);
	SetMaxThrust (ENGINE_RETRO, 0);
	SetMaxThrust (ENGINE_HOVER, 0);
	SetMaxThrust (ENGINE_ATTITUDE, 0);
	// Tank has no engines of its own

	SetCW (0.2, 0.3, 1.2, 1.2);
	VECTOR3 cs = {412.1,411.8,72.7};
	SetCrossSections (cs);
	VECTOR3 rd = {0.5,0.5,0.1};
	SetRotDrag (rd);
	VECTOR3 pmi = {145.6,145.6,10.5};
	SetPMI (pmi);
	SetPitchMomentScale (1e-4);
	SetYawMomentScale (1e-4);

	// ************************* docking port **************************************

	VECTOR3 co = {0,0,0};
	SetCameraOffset (co);
	// Note that the camera offset should not be required
	// since the Tank doesn't define a 'cockpit'

	SetCOG_elev (-5.0);
	//SetTouchdownPoints (_V(0,9,3), _V(-1,1,-3), _V(1,1,-3));
	SetTouchdownPoints (tdvtx, ntdvtx);
	SetLiftCoeffFunc (0);

	AddMesh (hTankMesh);
}

// Simulation time step
void Atlantis_Tank::clbkPreStep (double simt, double simdt, double mjd)
{
	OBJHANDLE hV;
	VESSEL *pV;

	pAtlantis = NULL;
	if (hV = GetDockStatus (GetDockHandle (0))) {
		pV = oapiGetVesselInterface (hV);
		if (!strcmp (pV->GetClassName (), "Atlantis"))
			pAtlantis = (Atlantis*)pV;
	}

	for (int i = 0; i < 2; i++) {
		pSRB[i] = GetSRB(i);
	}
}

void Atlantis_Tank::clbkPostStep (double simt, double simdt, double mjd)
{
	if (GetAltitude() < 0.0) oapiDeleteVessel (GetHandle());
}

Atlantis_SRB *Atlantis_Tank::GetSRB (int which) const
{
	if (which < 0 || which >= 2) return NULL;

	OBJHANDLE hV = GetDockStatus (GetDockHandle (which+1));
	if (!hV) return NULL;
	VESSEL *pV = oapiGetVesselInterface (hV);
	return (strcmp (pV->GetClassName(), "Atlantis_SRB") ? NULL : (Atlantis_SRB*)pV);
}

void Atlantis_Tank::EnableOrbiterConnector ()
{
	//if (!hDockOrbiter) {
	//	hDockOrbiter = CreateDock (_V(0.0, 4.64, -9.285), _V(0,1,0), _V(1,0,0));
	//}
}

double Atlantis_Tank::GetMainPropellantMass () const
{
	return GetPropellantMass (hProp);
}

void Atlantis_Tank::SetSRBLaunchElevation (double elev) const
{
	for (int i = 0; i < 2; i++) {
		Atlantis_SRB *srb = GetSRB(i);
		if (srb) srb->SetLaunchElevation (elev);
	}
}

double Atlantis_Tank::GetSRBThrustLevel (int which) const
{
	return (pSRB[which] ? pSRB[which]->GetThrustLevel () : 0.0);
}

void Atlantis_Tank::SetSRBGimbal (const VECTOR3 &angle) const
{
	VECTOR3 dir;
	if (pSRB[0]) {
		dir.x = -sin(angle.y);          // yaw gimbal
		dir.y = sin(angle.x-angle.z);   // pitch+roll gimbal
		dir.z = sqrt(1.0-dir.x*dir.x-dir.y*dir.y);
		pSRB[0]->SetThrustGimbal (dir);
	}
	if (pSRB[1]) {
		dir.x = sin(angle.y);           // yaw gimbal
		dir.y = sin(-angle.x-angle.z);  // pitch+roll gimbal 
		dir.z = sqrt(1.0-dir.x*dir.x-dir.y*dir.y);
		pSRB[1]->SetThrustGimbal (dir);
	}
}

VECTOR3 Atlantis_Tank::GetSRBThrustDir (int which) const
{
	if (pSRB[which]) return pSRB[which]->GetThrustGimbal();
	else return _V(0,0,1);
}

bool Atlantis_Tank::IgniteSRBs () const
{
	bool ok = true;

	for (int i = 0; i < 2; i++) {
		if (pSRB[i]) {
			ok = ok && pSRB[i]->Ignite ();
			if (ok) {
				VECTOR3 dir = THRUSTGIMBAL_LAUNCH;
				if (i) dir.y = -dir.y;
				pSRB[i]->SetThrustGimbal (dir);
			}
		} else
			ok = false;
	}
	return ok;
}

void Atlantis_Tank::SeparateSRBs ()
{
	for (int i = 0; i < 2; i++) {
		if (pSRB[i]) {
			const double angle = 0.25*PI;
			VECTOR3 dir = _V(0.0, i ? -0.04 : 0.04, 0.9992);
			Undock (i+1);
			pSRB[i]->CmdThrustGimbal (dir); // set SRB gimbals for safe separation
			pSRB[i]->FireBolt();
			pSRB[i] = NULL;
		}
	}
}

// ==============================================================
// API interface
// ==============================================================

// Initialisation
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new Atlantis_Tank (hvessel);
}

// Cleanup
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (Atlantis_Tank*)vessel;
}
