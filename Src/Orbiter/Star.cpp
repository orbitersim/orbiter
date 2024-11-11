// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Implementation of class StarSpec

#include <fstream>
#include <stdio.h>
#include "Orbiter.h"
#include "Config.h"
#include "Star.h"
#include "Camera.h"
#include "Log.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern Camera *g_camera;
extern char DBG_MSG[256];


Star::Star (double _mass, double _mean_radius)
: CelestialBody (_mass, _mean_radius)
{
	Setup ();
}

Star::Star (char *fname)
: CelestialBody (fname)
{
	ifstream ifs (g_pOrbiter->ConfigPath (fname));
	if (!ifs) return;
	bDynamicPosVel = false;
	// read star-specific parameters here
	InitDeviceObjects ();
	Setup ();
}

Star::~Star ()
{
	DestroyDeviceObjects ();
}

void Star::Setup ()
{
	CelestialBody::Setup ();
	ExternPosition(); // try to init from module
}

void Star::Update (bool force)
{
	if (bDynamicPosVel || module && module->bEphemeris())
		CelestialBody::Update (force);
	// otherwise don't update: keep star in the origin
	// of the global coordinate system
}

Vector Star::Pos2Barycentre (Vector &pos)
{
	// by definition, the barycentre of stars (root objects) is the origin
	return Vector();
}

Vector4 Star::GetLightColor ()
{
	return {1,1,1,1};
}
