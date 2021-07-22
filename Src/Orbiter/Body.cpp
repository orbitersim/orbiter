// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Implementation of class Body

#define OAPI_IMPLEMENTATION

#include <string.h>
#include <fstream>
#include "Orbiter.h"
#include "Config.h"
#include "Psys.h"
#include "Body.h"
#include "Element.h"
#include "Log.h"
#include <stdio.h>

using namespace std;
using namespace oapi;

// =======================================================================
// Externals

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern Camera *g_camera;        // observer camera
extern PlanetarySystem *g_psys; // pointer to planetary system
extern char DBG_MSG[256];

// =======================================================================
// class Body

Body::Body ()
{
	Setup ();
	SetName ();
}

Body::Body (double _mass, double _size)
{
	Setup ();
	SetName ();
	mass = _mass;
	size = _size;
}

Body::Body (char *fname)
{
	char *cpath = g_pOrbiter->ConfigPath (fname);

	//g_pOrbiter->OutputLoadStatus (fname, 0);
	g_pOrbiter->OutputLoadStatus (cpath, 1);

	Setup ();
	ifstream ifs (cpath);
	if (!ifs) return;

	char cbuf[256], *_name = 0;
	if (GetItemString (ifs, "Name", cbuf)) _name = cbuf;
	SetName (_name);

	if (!GetItemReal (ifs, "Mass", mass)) mass = 1.0; // desperate default
	if (!GetItemReal (ifs, "Size", size)) size = 1.0; // desperate default

	if (!GetItemVector (ifs, "AlbedoRGB", albedo)) albedo.Set (1,1,1);

	GetItemVector (ifs, "InitPos", s0->pos);
	GetItemVector (ifs, "InitVel", s0->vel);

	rpos_base.Set (s0->pos); rpos_add.Set (0,0,0);
	rvel_base.Set (s0->vel); rvel_add.Set (0,0,0);
}

Body::~Body ()
{
	if (name) delete []name;
}

void Body::Setup ()
{
	extern bool g_bStateUpdate;
	s0          = sv;
	s1          = (g_bStateUpdate ? sv+1 : sv);
	cbody       = 0;
	hVis        = NULL;
	name        = 0;
	mass        = 0.0;
	size        = 0.0;
	albedo.Set (1,1,1);
	vislimit    = spotlimit = 1e-3;       // default visibility limit
	updcount = irand (100); // spread update times
	s0->R = IMatrix();       // align body with its parent's orientation
}

void Body::SetName (char *_name)
{
	static int noname = 0;
	char cbuf[16];
	if (!_name) {
		sprintf (cbuf, "obj%d", ++noname);
		_name = cbuf;
	}
	name = new char[strlen(_name)+1]; TRACENEW
	strcpy (name, _name);
}

void Body::SetSize (double newsize)
{
	size = newsize;
	g_pOrbiter->NotifyObjectSize (this);
}

void Body::RPlace (const Vector &rpos, const Vector &rvel)
{
	StateVectors *s = (s1 ? s1 : s0); // should RPlace be allowed outside update phase?
	s->pos = rpos_base = rpos;
	rpos_add.Set (0,0,0);
	s->vel = rvel_base = rvel;
	rvel_add.Set (0,0,0);
}

void Body::SetRPos (const Vector &p)
{
	dASSERT(s1, "Update state not available");
	rpos_base = s1->pos = p;
	rpos_add.Set (0,0,0);
}

void Body::AddRPos (const Vector &dp) {
	rpos_base += dp;
	if (s1) s1->pos = rpos_base + rpos_add;
	else    s0->pos = rpos_base + rpos_add;
}

void Body::FlushRPos ()
{
	rpos_base = (s1 ? s1->pos:s0->pos);
	rpos_add.Set(0,0,0);
}

void Body::SetRVel (const Vector &v)
{
	dASSERT(s1, "Update state not available");
	rvel_base = s1->vel = v;
	rvel_add.Set (0,0,0);
}

void Body::AddRVel (const Vector &dv)
{
	dASSERT(s1, "Update state not available");
	rvel_base += dv;
	s1->vel = rvel_base + rvel_add;
}

void Body::FlushRVel ()
{
	rvel_base = (s1 ? s1->vel:s0->vel);
	rvel_add.Set(0,0,0);
}

void Body::LocalToEquatorial (const Vector &loc, double &lng, double &lat, double &rad) const
{
	rad = loc.length();
	//loc *= 1.0/rad;
	lng = atan2 (loc.z, loc.x);
	lat = asin  (loc.y / rad);
	//lat = atan  (loc.y / hypot (loc.x, loc.z));
}

bool Body::SurfpointVisible (double lng, double lat, const Vector &gcam) const
{
	Vector sp;
	EquatorialToGlobal (lng, lat, size, sp);
	return (dotp (sp-GPos(), gcam-sp) >= 0.0);
}

void Body::RegisterVisual (VISHANDLE vis)
{
	hVis = vis;
}

void Body::UnregisterVisual ()
{
	hVis = NULL;
}

void Body::BroadcastVisMsg (DWORD msg, UINT content)
{
	oapi::GraphicsClient *gc;
	if (hVis && (gc = g_pOrbiter->GetGraphicsClient())) {
		gc->clbkVisEvent ((OBJHANDLE)this, hVis, msg, content);
	}
}

void Body::Update (bool force)
{
}

void Body::BeginStateUpdate ()
{
	// enable the update state
	if (s0 == sv) s1 = sv+1;
	else          s1 = sv;
}

void Body::EndStateUpdate ()
{
	FlipState ();
	s1 = NULL;
	// disable the update state, to avoid it being addressed outside the update phase
}

void Body::FlipState ()
{
	if (s0 == sv) s0 = sv+1, s1 = sv;
	else          s0 = sv, s1 = sv+1;
}

void Body::DestroyDeviceObjects ()
{
}