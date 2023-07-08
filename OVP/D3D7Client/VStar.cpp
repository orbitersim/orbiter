// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VStar.cpp
// class vStar (interface)
//
// Renders the central star as a billboard mesh.
// ==============================================================

#include "Mesh.h"
#include "Texture.h"
#include "VStar.h"
#include "Camera.h"

D3D7Mesh *vStar::billboard = 0;
LPDIRECTDRAWSURFACE7 vStar::deftex = 0;

vStar::vStar (OBJHANDLE _hObj, const Scene *scene): vObject (_hObj, scene)
{
	size = oapiGetSize (_hObj);
	maxdist = 0.5*scene->GetCamera()->GetFarlimit();

	// set material definition
    ZeroMemory (&mtrl, sizeof(D3DMATERIAL7));
    mtrl.dcvEmissive.r = 1.0f;
    mtrl.dcvEmissive.g = 1.0f;
    mtrl.dcvEmissive.b = 1.0f;
}

vStar::~vStar ()
{
}

void vStar::GlobalInit (oapi::D3D7Client *gc)
{
	// create billboard mesh for sprite representation
	billboard = new D3D7Mesh (gc);
	static WORD idx[6] = {0,1,2, 3,2,1};
	MESHGROUPEX mg;
	memset (&mg, 0, sizeof(MESHGROUPEX));
	mg.MtrlIdx = SPEC_INHERIT;
	mg.TexIdx = SPEC_INHERIT;
	mg.nVtx = 4;
	mg.nIdx = 6;
	mg.Idx = idx;
	mg.Vtx = new NTVERTEX[mg.nVtx];
	DWORD i;
	for (i = 0; i < mg.nVtx; i++) {
		mg.Vtx[i].x = 0;
		mg.Vtx[i].y = (i%2 ? 1.0f:-1.0f);
		mg.Vtx[i].z = (i<2 ? 1.0f:-1.0f);
		mg.Vtx[i].nx = -1.0f;
		mg.Vtx[i].ny = 0;
		mg.Vtx[i].nz = 0;
		mg.Vtx[i].tu = (i<2 ? 0:1.0f);
		mg.Vtx[i].tv = (i%2 ? 1.0f:0);
	}
	billboard->AddGroup (&mg);
	delete []mg.Vtx;

	// load the default texture
	gc->GetTexMgr()->LoadTexture ("star.dds", &deftex, 0);
}

void vStar::GlobalExit ()
{
	if (billboard) delete billboard;
	if (deftex)    deftex->Release();
}

bool vStar::Update ()
{
	if (!active) return false;

	vObject::Update ();

	FLOAT dist_scale;
	FLOAT rad_scale = (FLOAT)size;
	FLOAT size_hack; // make star look bigger at distance
	VECTOR3 bdir (unit(cpos));
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

	// artificially reduce size reduction with distance
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

	return true;
}

bool vStar::Render (LPDIRECT3DDEVICE7 dev)
{
	// Render states expected on entry:
	//    D3DRENDERSTATE_ZENABLE=FALSE
	//    D3DRENDERSTATE_ZWRITEENABLE=FALSE
	//    D3DRENDERSTATE_ALPHABLENDENABLE=TRUE
	// Render states modified on exit:
	//    D3DTRANSFORMSTATE_WORLD
	//    Texture

	D3DMATERIAL7 pmtrl;

	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
	dev->SetTexture (0, deftex);
	dev->GetMaterial (&pmtrl);
	dev->SetMaterial (&mtrl);
	billboard->Render (dev);
	dev->SetMaterial (&pmtrl);

	return true;
}
