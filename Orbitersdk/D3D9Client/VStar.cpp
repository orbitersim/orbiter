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
#include "Texture.h"
#include "VStar.h"
#include "D3D9Surface.h"
#include "D3D9Config.h"

SURFHANDLE vStar::deftex = 0;

vStar::vStar(OBJHANDLE _hObj, const Scene *scene): vObject (_hObj, scene)
{
	maxdist = 0.5*scene->GetCameraFarPlane();
}

vStar::~vStar ()
{
}

void vStar::GlobalInit (oapi::D3D9Client *gc)
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
	if (Config->PostProcess == PP_LENSFLARE && cdist < 150e9) return false;

	double dist_scale;
	float rad_scale = float(size);
	float size_hack; // make star look bigger at distance

	VECTOR3 bdir (unit(cpos));
	double hz = _hypot (bdir.x, bdir.z);
	// double phi = atan2 (bdir.z, bdir.x);
	// FLOAT sphi = (FLOAT)sin(phi), cphi = (FLOAT)cos(phi);
	// FLOAT tx = (FLOAT)cpos.x, ty = (FLOAT)cpos.y, tz = (FLOAT)cpos.z;
	mWorld._11 =  (FLOAT)bdir.x; //cphi;
	mWorld._12 =  (FLOAT)bdir.y; //0;
	mWorld._13 =  (FLOAT)bdir.z; //sphi;
	mWorld._31 = -(FLOAT)(bdir.z/hz); //-sphi;
	mWorld._32 =  0;
	mWorld._33 =  (FLOAT)(bdir.x/hz); //cphi;
	mWorld._21 =  -(mWorld._12*mWorld._33 - mWorld._32*mWorld._13); //0;
	mWorld._22 =  -(mWorld._13*mWorld._31 - mWorld._33*mWorld._11); //1;
	mWorld._23 =  -(mWorld._11*mWorld._32 - mWorld._31*mWorld._12); // 0;

	// artificially reduce size reduction with distance
	// to make star appear larger
	size_hack = float(1.0+pow(cdist,0.6)*1e-6);

	rad_scale *= max(size_hack, 7.5f);

	maxdist = 0.1*scn->GetCameraFarPlane();

	if (cdist > maxdist) {
		dist_scale = maxdist/cdist;
		rad_scale *= float(dist_scale);
		mWorld._41 = float(cpos.x*dist_scale);
		mWorld._42 = float(cpos.y*dist_scale);
		mWorld._43 = float(cpos.z*dist_scale);
	}

	// scale up sphere radius from 1 to planet radius
	mWorld._11 *= rad_scale; mWorld._12 *= rad_scale; mWorld._13 *= rad_scale;
	mWorld._21 *= rad_scale; mWorld._22 *= rad_scale; mWorld._23 *= rad_scale;
	mWorld._31 *= rad_scale; mWorld._32 *= rad_scale; mWorld._33 *= rad_scale;

	D3D9Effect::RenderBillboard(&mWorld, deftex);

	return true;
}
