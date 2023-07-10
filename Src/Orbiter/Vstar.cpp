// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define D3D_OVERLOADS
#define OAPI_IMPLEMENTATION

#include "D3dmath.h"
#include "Orbiter.h"
#include "Texture.h"
#include "Vecmat.h"
#include "Mesh.h"
#include "Camera.h"
#include "Pane.h"
#include "Scene.h"
#include "Star.h"
#include "VStar.h"
#include "Body.h"
#include "Log.h"
#include "OGraphics.h"
#include <stdio.h>
#include <fstream>

extern Orbiter *g_pOrbiter;
extern TextureManager *g_texmanager;
extern Camera *g_camera;
extern Pane *g_pane;
extern double g_farplane;
extern char DBG_MSG[256];

// =======================================================================
// Local variables and prototypes

static double maxdist = 0.5*g_farplane;

Mesh CreateSquare ();

// =======================================================================
// Class VStar

// Create static meshes
Mesh VStar::BillboardMesh = CreateSquare();

VStar::VStar (const Star *_star): VObject (_star)
{
	star = _star;
	resolution = 1;

	// Create the star's material
	SetMaterial ();
}

VStar::~VStar ()
{}

void VStar::SetMaterial ()
{
    ZeroMemory (&mtrl, sizeof(D3DMATERIAL7));
    mtrl.dcvEmissive.r = 1.0f;
    mtrl.dcvEmissive.g = 1.0f;
    mtrl.dcvEmissive.b = 1.0f;
}

void VStar::Update (bool moving, bool force)
{
	FLOAT dist_scale;
	FLOAT rad_scale = (FLOAT)body->Size();
	FLOAT size_hack; // make star look bigger at distance

	VObject::Update (moving, force);

	Vector bdir = unit(cpos);
	double hz = std::hypot (bdir.x, bdir.z);
	double phi = atan2 (bdir.z, bdir.x);
	FLOAT sphi = (FLOAT)sin(phi), cphi = (FLOAT)cos(phi);
	FLOAT tx = (FLOAT)cpos.x, ty = (FLOAT)cpos.y, tz = (FLOAT)cpos.z;
	mWorld._11 =  (FLOAT)bdir.x; //cphi;
	mWorld._12 =  (FLOAT)bdir.y; //0;
	mWorld._13 =  (FLOAT)bdir.z; //sphi;
	mWorld._31 = -(FLOAT)(bdir.z/hz); //-sphi;
	mWorld._32 =  0;
	mWorld._33 =  (FLOAT)(bdir.x/hz); //cphi;
	mWorld._21 =  -(mWorld._12*mWorld._33 - mWorld._32*mWorld._13); //0;
	mWorld._22 =  -(mWorld._13*mWorld._31 - mWorld._33*mWorld._11); //1;
	mWorld._23 =  -(mWorld._11*mWorld._32 - mWorld._31*mWorld._12); // 0;
	mWorld._41 =  (FLOAT)cpos.x;
	mWorld._42 =  (FLOAT)cpos.y;
	mWorld._43 =  (FLOAT)cpos.z;

	// artificially reduce size decrease with distance
	// to make star appear larger
	//size_hack = (FLOAT)(1.0+2e-8*pow(cdist,0.7));
	size_hack = 7.5;
	rad_scale *= size_hack;

	if (cdist > maxdist) {
		dist_scale = (FLOAT)(maxdist/cdist);
		rad_scale *= dist_scale;
		mWorld._41 *= dist_scale;
		mWorld._42 *= dist_scale;
		mWorld._43 *= dist_scale;
	}

	// scale up sphere radius from 1 to planet radius
	mWorld._11 *= rad_scale; mWorld._12 *= rad_scale; mWorld._13 *= rad_scale;
	mWorld._21 *= rad_scale; mWorld._22 *= rad_scale; mWorld._23 *= rad_scale;
	mWorld._31 *= rad_scale; mWorld._32 *= rad_scale; mWorld._33 *= rad_scale;
}

void VStar::BlindColour (const Camera *cam, Vector &col)
{
	//double alpha = xangle (cam->Direction(), cpos);
	//double ratio = alpha/cam->Aperture();
	//if (ratio <= 1.3) {                // otherwise black
		//double scl = 2.5*(1.3-ratio);
		//if (scl > 1.0) scl = 1.0;
		double appsize = body->Size()/(cdist*cam->TanAperture());
		appsize += 0.02;
		if (appsize > 0.3) appsize = 0.3;
		double val = /* scl* */ appsize*3.0;  // 0.9 max
		col = {val, val, val};         // grey
	//}
}

COLORREF VStar::CenterPixelColor ()
{
	Vector orig (cpos * (1.0/cdist));
	int x, y;
	COLORREF col;
	if (g_pane->GlobalToScreen (orig, x, y)) {
		HDC hDC;
		LPDIRECTDRAWSURFACE7 surf = g_pOrbiter->GetInlineGraphicsClient()->GetRenderTarget();
		surf->GetDC (&hDC);
		col = GetPixel (hDC, x, y);
		surf->ReleaseDC (hDC);
	} else {
		col = (COLORREF)-1;
	}
	return col;
}

void VStar::Render (LPDIRECT3DDEVICE7 dev)
{
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
	if (resolution <= 3) {
		dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
		dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
	    dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		dev->SetTexture (0, star->tex);
		dev->SetMaterial (&mtrl);
		BillboardMesh.Render (dev);
		dev->SetMaterial (&Scene::default_mtrl);
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
		dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
	}
}

Mesh CreateSquare ()
{
	static int idx[6] = {0,1,2, 3,2,1};
	int i, nVtx = 4, nIdx = 6;
	NTVERTEX* Vtx = new NTVERTEX[nVtx];  TRACENEW
	WORD *Idx = new WORD[nIdx]; TRACENEW
	for (i = 0; i < nVtx; i++) {
		Vtx[i].x = 0;
		Vtx[i].y = (i%2 ? 1.0f:-1.0f);
		Vtx[i].z = (i<2 ? 1.0f:-1.0f);
		Vtx[i].nx = -1.0f;
		Vtx[i].ny = 0;
		Vtx[i].nz = 0;
		Vtx[i].tu = (i<2 ? 0:1.0f);
		Vtx[i].tv = (i%2 ? 1.0f:0);
	}
	for (int i = 0; i < nIdx; i++) Idx[i] = idx[i];
	return Mesh (Vtx, nVtx, Idx, nIdx, SPEC_INHERIT, SPEC_INHERIT);
}