// ==============================================================
// VStar.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
// ==============================================================

// ==============================================================
// class vStar (interface)
// Renders the central star as a billboard mesh.
// ==============================================================

#include "Mesh.h"
#include "VStar.h"
#include "Surface.h"
#include "Config.h"

SURFHANDLE vStar::deftex = 0;

vStar::vStar(OBJHANDLE _hObj, const Scene *scene): vObject (_hObj, scene)
{
	maxdist = 0.5*scene->GetCameraFarPlane();
}

vStar::~vStar ()
{
}

void vStar::GlobalInit (oapi::vkClient *gc)
{
	deftex = SURFACE(gc->clbkLoadTexture("star.dds", 0));
}

void vStar::GlobalExit()
{
	DELETE_SURFACE(deftex);
}

bool vStar::Update (bool bMainScene)
{
	_TRACE;
	if (!active) return false;

	vObject::Update (bMainScene);

	return true;
}

bool vStar::Render(LPDIRECT3DDEVICE9 dev)
{
	_TRACE;

	if (Config->bGlares) return true;

	double dist_scale;
	float rad_scale = float(size);
	float size_hack; // make star look bigger at distance

	VECTOR3 bdir (unit(cpos));
	double hz = std::hypot (bdir.x, bdir.z);
	// double phi = atan2 (bdir.z, bdir.x);
	// FLOAT sphi = (FLOAT)sin(phi), cphi = (FLOAT)cos(phi);
	// FLOAT tx = (FLOAT)cpos.x, ty = (FLOAT)cpos.y, tz = (FLOAT)cpos.z;
	mWorld.m11 =  (FLOAT)bdir.x; //cphi;
	mWorld.m12 =  (FLOAT)bdir.y; //0;
	mWorld.m13 =  (FLOAT)bdir.z; //sphi;
	mWorld.m31 = -(FLOAT)(bdir.z/hz); //-sphi;
	mWorld.m32 =  0;
	mWorld.m33 =  (FLOAT)(bdir.x/hz); //cphi;
	mWorld.m21 =  -(mWorld.m12*mWorld.m33 - mWorld.m32*mWorld.m13); //0;
	mWorld.m22 =  -(mWorld.m13*mWorld.m31 - mWorld.m33*mWorld.m11); //1;
	mWorld.m23 =  -(mWorld.m11*mWorld.m32 - mWorld.m31*mWorld.m12); // 0;

	// artificially reduce size reduction with distance
	// to make star appear larger
	size_hack = float(1.0+pow(cdist,0.6)*1e-6);

	rad_scale *= max(size_hack, 7.5f);

	maxdist = 0.1*scn->GetCameraFarPlane();

	if (cdist > maxdist) {
		dist_scale = maxdist/cdist;
		rad_scale *= float(dist_scale);
		mWorld.m41 = float(cpos.x*dist_scale);
		mWorld.m42 = float(cpos.y*dist_scale);
		mWorld.m43 = float(cpos.z*dist_scale);
	}

	// scale up sphere radius from 1 to planet radius
	mWorld.m11 *= rad_scale; mWorld.m12 *= rad_scale; mWorld.m13 *= rad_scale;
	mWorld.m21 *= rad_scale; mWorld.m22 *= rad_scale; mWorld.m23 *= rad_scale;
	mWorld.m31 *= rad_scale; mWorld.m32 *= rad_scale; mWorld.m33 *= rad_scale;

	//vkEffect::RenderBillboard(&mWorld, scn->GetSunTexture(), 1.0f);
	vkEffect::RenderBillboard(&mWorld, SURFACE(deftex)->GetTexture(), 1.0f);
	return true;
}
