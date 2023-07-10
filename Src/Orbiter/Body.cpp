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
#include <string>

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
	extern bool g_bStateUpdate;
	s0 = sv;
	s1 = (g_bStateUpdate ? sv + 1 : sv);
	updcount = irand(100); // spread update times
	s0->R = IMatrix();       // align body with its parent's orientation
}

Body::Body (double _mass, double _size)
	: Body()
{
	mass = _mass;
	size = _size;
}

Body::Body(char* fname)
	: Body()
{
	char *cpath = g_pOrbiter->ConfigPath (fname);

	//g_pOrbiter->OutputLoadStatus (fname, 0);
	g_pOrbiter->OutputLoadStatus (cpath, 1);

	ifstream ifs (cpath);
	if (!ifs) return;
	
	filename = cpath;

	char cbuf[256], *_name = 0;
	if (GetItemString (ifs, "Name", cbuf)) _name = cbuf;
	SetName (_name);

	if (!GetItemReal (ifs, "Mass", mass)) mass = 1.0; // desperate default
	if (!GetItemReal (ifs, "Size", size)) size = 1.0; // desperate default

	if (!GetItemVector (ifs, "AlbedoRGB", albedo)) albedo = {1, 1, 1};

	GetItemVector (ifs, "InitPos", s0->pos);
	GetItemVector (ifs, "InitVel", s0->vel);

	rpos_base = s0->pos; rpos_add = {0, 0, 0};
	rvel_base = s0->vel; rvel_add = {0, 0, 0};
}

void Body::SetName (char *_name)
{
	static int noname = 0;
	if (!_name) name = "obj" + std::to_string(++noname);
	else name = _name;
}

void Body::SetSize (double newsize)
{
	size = newsize;
	g_pOrbiter->NotifyObjectSize (this);
}

void Body::RPlace (const Vector &rpos, const Vector &rvel)
{
	// should RPlace be allowed outside update phase?
	s1->pos = rpos_base = rpos;
	rpos_add = {0, 0, 0};
	s1->vel = rvel_base = rvel;
	rvel_add = {0, 0, 0};
}

void Body::SetRPos (const Vector &p)
{
	dCHECK(s1 != s0, "Update state not available")
	rpos_base = s1->pos = p;
	rpos_add = {0, 0, 0};
}

void Body::AddRPos (const Vector &dp) {
	rpos_base += dp;
	s1->pos = rpos_base + rpos_add;
}

void Body::FlushRPos ()
{
	rpos_base = s1->pos;
	rpos_add = {0, 0, 0};
}

void Body::SetRVel (const Vector &v)
{
	dCHECK(s1 != s0, "Update state not available")
	rvel_base = s1->vel = v;
	rvel_add = {0, 0, 0};
}

void Body::AddRVel (const Vector &dv)
{
	dCHECK(s1 != s0, "Update state not available")
	rvel_base += dv;
	s1->vel = rvel_base + rvel_add;
}

void Body::FlushRVel ()
{
	rvel_base = s1->vel;
	rvel_add = {0, 0, 0};
}

void Body::LocalToEquatorial (const Vector &loc, double &lng, double &lat, double &rad) const
{
	rad = len(loc);
	//loc *= 1.0/rad;
	lng = atan2 (loc.z, loc.x);
	lat = asin  (loc.y / rad);
	//lat = atan  (loc.y / hypot (loc.x, loc.z));
}

bool Body::SurfpointVisible (double lng, double lat, const Vector &gcam) const
{
	Vector sp;
	EquatorialToGlobal (lng, lat, size, sp);
	return (dot(sp - GPos(), gcam - sp) >= 0.0);
}

void Body::BroadcastVisMsg (DWORD msg, DWORD_PTR content)
{
	oapi::GraphicsClient *gc;
	if (hVis && (gc = g_pOrbiter->GetGraphicsClient())) {
		gc->clbkVisEvent ((OBJHANDLE)this, hVis, msg, content);
	}
}

void Body::BeginStateUpdate ()
{
	// enable the update state
	s1 = (s0 == sv ? sv + 1 : sv);
}

void Body::EndStateUpdate ()
{
	// disable the update state, to avoid it being addressed outside the update phase
	s1 = s0 = (s0 == sv ? sv + 1 : sv);
}
