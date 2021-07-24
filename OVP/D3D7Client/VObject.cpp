// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VObject.cpp
// class vObject (implementation)
//
// A "vObject" is the visual representation of an Orbiter object
// (vessel, planet/moon/sun, surface base). vObjects usually have
// one or more meshes associated with it that define their visual
// appearance, but they can be arbitrarily complex (e.g. planets
// with clould layers, atmospheric haze, etc.)
// Visual objects don't persist as their "logical" counterparts,
// but are created and deleted as they pass in and out of the
// visual range of a camera. vObjects are therefore associated
// with a particular scene. In multi-scene environments, a single
// logical object may have multiple vObjects associated with it.
// ==============================================================

#include "VObject.h"
#include "VVessel.h"
#include "VPlanet.h"
#include "VStar.h"
#include "VBase.h"
#include "Camera.h"
#include "Texture.h"
#include "D3D7Util.h"

using namespace oapi;

// Initialisation of static members
const D3D7Client *vObject::gc = NULL;
LPDIRECTDRAWSURFACE7 vObject::blobtex[3] = {0,0,0};

// Constructor
vObject::vObject (OBJHANDLE _hObj, const Scene *scene): VisObject (_hObj)
{
	active = true;
	hObj = _hObj;
	scn  = scene;
	size = oapiGetSize(hObj);
	cdist = 0.0;
	dmWorld = identity4();
	D3DMAT_Identity (&mWorld);
}

vObject *vObject::Create (OBJHANDLE _hObj, const Scene *scene)
{
	switch (oapiGetObjectType (_hObj)) {
	case OBJTP_VESSEL:
		return new vVessel (_hObj, scene);
	case OBJTP_PLANET:
		return new vPlanet (_hObj, scene);
	case OBJTP_STAR:
		return new vStar (_hObj, scene);
	case OBJTP_SURFBASE:
		return new vBase (_hObj, scene);
	default:
		return new vObject (_hObj, scene);
	}
}

void vObject::GlobalInit (const D3D7Client *gclient)
{
	gc = gclient;

	for (int i = 0; i < 3; i++) {
		static char *fname[3] = {"Ball.dds","Ball2.dds","Ball3.dds"};
		gc->GetTexMgr()->LoadTexture (fname[i], blobtex+i, 0);
	}
}

void vObject::GlobalExit ()
{
	for (int i = 0; i < 3; i++) {
		if (blobtex[i]) blobtex[i]->Release();
	}
}

void vObject::Activate (bool isactive)
{
	active = isactive;
}

bool vObject::Update ()
{
	if (!active) return false;

	MATRIX3 grot;
	oapiGetRotationMatrix (hObj, &grot);
	oapiGetGlobalPos (hObj, &cpos);
	cpos -= *scn->GetCamera()->GetGPos();
	// object positions are relative to camera

	cdist = length (cpos);
	// camera distance

	dmWorld = _M(grot.m11, grot.m21, grot.m31, 0,
		         grot.m12, grot.m22, grot.m32, 0,
				 grot.m13, grot.m23, grot.m33, 0,
				 cpos.x,   cpos.y,   cpos.z,   1);
	D3DMAT_SetInvRotation (&mWorld, &grot);
	D3DMAT_SetTranslation (&mWorld, &cpos);
	// update the object's world matrix

	CheckResolution();
	return true;
}

void vObject::RenderSpot (LPDIRECT3DDEVICE7 dev, const VECTOR3 *ofs, float size, const VECTOR3 &col, bool lighting, int shape)
{
	static D3DMATRIX W = {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1};
	MATRIX3 grot;
	oapiGetRotationMatrix (hObj, &grot);
	VECTOR3 gpos;
	oapiGetGlobalPos (hObj, &gpos);
	VECTOR3 pos (cpos);
	if (ofs) pos += mul (grot, *ofs);
	double dist = length (pos);
	VECTOR3 bdir (pos/dist);
	const VECTOR3 &camp = *scn->GetCamera()->GetGPos();
	double hz = _hypot (bdir.x, bdir.z);
	double phi = atan2 (bdir.z, bdir.x);
	float sphi = (float)sin(phi), cphi = (float)cos(phi);

	const double ambient = 0.2;
	double cosa = dotp (unit(gpos), unit(gpos - camp));
	double intens = (lighting ? 0.5 * ((1.0-ambient)*cosa + 1.0+ambient) : 1.0);

	W._11 =  (float)bdir.x;
	W._12 =  (float)bdir.y;
	W._13 =  (float)bdir.z;
	W._31 =  (-(float)(bdir.z/hz));
	/*W._32 =  0;*/
	W._33 =  (float)(bdir.x/hz);
	W._21 =  (/*W._32*W._13*/ - W._12*W._33);
	W._22 =  (W._33*W._11 - W._13*W._31);
	W._23 =  (W._31*W._12 /*- W._11*W._32*/);
	W._41 =  (float)pos.x;
	W._42 =  (float)pos.y;
	W._43 =  (float)pos.z;

	W._11 *= size; W._12 *= size; W._13 *= size;
	W._21 *= size; W._22 *= size; W._23 *= size;
	W._31 *= size; /*W._32 *= size;*/ W._33 *= size;

	static WORD idx[6] = {0,1,2, 3,2,1};
	static D3DVERTEX vtx[4] = {
		{0,-1, 1,  -1,0,0,  0,0},
		{0, 1, 1,  -1,0,0,  0,1},
		{0,-1,-1,  -1,0,0,  1,0},
		{0, 1,-1,  -1,0,0,  1,1}
	};
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &W);
	dev->SetTexture (0, blobtex[shape]);
	dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGB(col.x*intens, col.y*intens, col.z*intens));
	dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
	dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
	dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, D3DFVF_VERTEX, vtx, 4, idx, 6, 0);
	dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
	dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
}
