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
#include <algorithm>

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

bool veccomp(const vObject::BodyVectorRec& v1, const vObject::BodyVectorRec& v2)
{
	return v1.dist > v2.dist; // sort from furthest to nearest
}

bool vObject::Update ()
{
	if (!active) return false;

	MATRIX3 grot;
	oapiGetRotationMatrix (hObj, &grot);
	oapiGetGlobalPos (hObj, &cpos);
	cpos -= *scn->GetCamera()->GetGPos(); // global object position relative to camera
	campos = tmul(grot, -cpos); // camera position in object frame

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
	UpdateRenderVectors();
	std::sort(veclist.begin(), veclist.end(), veccomp);

	return true;
}

void vObject::UpdateRenderVectors()
{
	veclist.clear();

	// render object frame axes
	DWORD flag = *(DWORD*)gc->GetConfigParam(CFGPRM_FRAMEAXISFLAG);
	if (flag & FAV_ENABLE) {
		int tp = oapiGetObjectType(hObj);
		if ((tp == OBJTP_VESSEL && flag & FAV_VESSEL) ||
			(tp == OBJTP_PLANET && flag & FAV_CELBODY) ||
			(tp == OBJTP_SURFBASE && flag & FAV_BASE)) {

			double scale = size * *(float*)gc->GetConfigParam(CFGPRM_FRAMEAXISSCALE);
			double rad = size * 0.01;
			float alpha = *(float*)gc->GetConfigParam(CFGPRM_FRAMEAXISOPACITY);
			AddVector(_V(scale, 0, 0), _V(0, 0, 0), rad, std::string("+x"), _V(1, 1, 1), alpha, D3DRGB(1, 1, 1));
			AddVector(_V(0, scale, 0), _V(0, 0, 0), rad, std::string("+y"), _V(1, 1, 1), alpha, D3DRGB(1, 1, 1));
			AddVector(_V(0, 0, scale), _V(0, 0, 0), rad, std::string("+z"), _V(1, 1, 1), alpha, D3DRGB(1, 1, 1));
			if (flag & FAV_NEGATIVE) {
				AddVector(_V(-scale, 0, 0), _V(0, 0, 0), rad, std::string("-x"), _V(1, 1, 1), alpha, D3DRGB(1, 1, 1));
				AddVector(_V(0, -scale, 0), _V(0, 0, 0), rad, std::string("-y"), _V(1, 1, 1), alpha, D3DRGB(1, 1, 1));
				AddVector(_V(0, 0, -scale), _V(0, 0, 0), rad, std::string("-z"), _V(1, 1, 1), alpha, D3DRGB(1, 1, 1));
			}
		}
	}
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

void vObject::AddVector(const VECTOR3& v, const VECTOR3& orig, double rad, const std::string& label, const VECTOR3& col, float alpha, DWORD lcol, float lsize)
{
	double len = length(v);
	if (len < 2.0 * rad) return; // too short to be rendered

	VECTOR3 vu = v / len;
	double dist = length(vu - campos); // distance of vector tip from camera

	BodyVectorRec rec;
	rec.v = v;
	rec.orig = orig;
	rec.rad = rad;
	rec.dist = dist;
	rec.col = col;
	rec.alpha = alpha;
	rec.label = label;
	rec.lcol = lcol;
	rec.lsize = lsize;
	
	veclist.push_back(rec);
}

void vObject::RenderVectors(LPDIRECT3DDEVICE7 dev)
{
	// Render and texture stage states are assumed to be set correctly on call

	if (veclist.size()) {
		float palpha = -1.0f;
		VECTOR3 pcol = { -1,-1,-1 };
		for (auto&& vec : veclist) {
			if (vec.alpha != palpha || vec.col.x != pcol.x || vec.col.y != pcol.y || vec.col.z != pcol.z) {
				dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(vec.col.x, vec.col.y, vec.col.z, vec.alpha));
				palpha = vec.alpha; pcol = vec.col;
			}
			if (DrawVector(dev, vec.v, vec.orig, vec.rad) && vec.label.size()) {
				double scale3 = (vec.lsize >= 0 ? vec.lsize : size);
				//scene->Render3DLabel(mul(body->GRot(), ve->v + ve->v.unit() * (scale3 * 0.1)) + body->GPos(),
				//	ve->label, scale3, ve->lcol);
			}
		}
	}
}

bool vObject::DrawVector(LPDIRECT3DDEVICE7 dev, const VECTOR3& end, const VECTOR3& orig, double rad)
{
	static const float EPS = 1e-2f;
	static const int nseg = 8;
	static const int nVtx = nseg * 5;
	static const int nIdx0 = nseg * 6 + nseg * 3;
	static const int nIdx1 = nIdx0 + (nseg - 2) * 3;
	int i, nIdx;
	WORD* Idx;
	static D3DVERTEX* Vtx0 = 0, * Vtx = 0;
	static WORD* Idx0 = 0, * Idx1 = 0;
	if (!Vtx0) { // create vector mesh template (pointing to +y)
		int i0, i1;
		Vtx0 = new D3DVERTEX[nVtx];
		Vtx = new D3DVERTEX[nVtx];
		Idx0 = new WORD[nIdx0];
		Idx1 = new WORD[nIdx1];
		for (i = 0; i < nseg; i++) {
			Vtx0[i + 2 * nseg].x = Vtx0[i + 3 * nseg].x = 2.0f * (Vtx0[i].x = Vtx0[i + nseg].x = Vtx0[i].nx = Vtx0[i + nseg].nx = (float)cos(PI2 * (double)i / (double)nseg));
			Vtx0[i + 2 * nseg].z = Vtx0[i + 3 * nseg].z = 2.0f * (Vtx0[i].z = Vtx0[i + nseg].z = Vtx0[i].nz = Vtx0[i + nseg].nz = (float)sin(PI2 * (double)i / (double)nseg));
			Vtx0[i + 4 * nseg].x = Vtx0[i + 4 * nseg].z = 0.0f;
			Vtx0[i].y = 0.0f; Vtx0[i + nseg].y = Vtx0[i + 2 * nseg].y = Vtx0[i + 3 * nseg].y = Vtx0[i + 4 * nseg].y = 1.0f;
			Vtx0[i + 3 * nseg].nx = Vtx0[i + 4 * nseg].nx = (float)(0.894 * cos(PI2 * (double)(i + 0.5) / (double)nseg));
			Vtx0[i + 3 * nseg].nz = Vtx0[i + 4 * nseg].nz = (float)(0.894 * sin(PI2 * (double)(i + 0.5) / (double)nseg));
			Vtx0[i + 3 * nseg].ny = Vtx0[i + 4 * nseg].ny = 0.447f;
			Vtx0[i].ny = Vtx0[i + nseg].ny = Vtx0[i + 2 * nseg].nx = Vtx0[i + 2 * nseg].nz = 0.0f;
			Vtx0[i + 2 * nseg].ny = -1.0f;
		}
		for (i = i0 = 0, i1 = 6 * (nseg - 1); i < nseg; i++) { // stem
			Idx0[i0++] = Idx1[i1++] = (WORD)i;
			Idx0[i0++] = Idx1[i1++] = (WORD)(i + nseg);
			Idx0[i0++] = Idx1[i1++] = (WORD)((i + 1) % nseg + nseg);
			Idx0[i0++] = Idx1[i1++] = (WORD)i;
			Idx0[i0++] = Idx1[i1++] = (WORD)((i + 1) % nseg + nseg);
			Idx0[i0++] = Idx1[i1++] = (WORD)((i + 1) % nseg);
		}
		for (i = 0, i1 = nseg * 3; i < nseg - 2; i++) {      // tip base
			Idx1[i1++] = nseg * 2;
			Idx1[i1++] = nseg * 2 + 1 + i;
			Idx1[i1++] = nseg * 2 + 2 + i;
		}
		for (i = i1 = 0, i0 = nseg * 6; i < nseg; i++) {   // tip
			Idx0[i0++] = Idx1[i1++] = nseg * 3 + i;
			Idx0[i0++] = Idx1[i1++] = nseg * 4 + i;
			Idx0[i0++] = Idx1[i1++] = 3 * nseg + (i + 1) % nseg;
		}
	}

	float w = (float)rad;
	float h = (float)length(end);
	if (h < EPS) return false;
	float hb = max(h - 4.0f * w, 0);

	memcpy(Vtx, Vtx0, nVtx * sizeof(D3DVERTEX));

	for (i = 0; i < nseg; i++) {
		Vtx[i + 3 * nseg].y = Vtx[i + 2 * nseg].y = Vtx[i + nseg].y = hb;
		Vtx[i + 4 * nseg].y = h;
		Vtx[i].x = Vtx[i + nseg].x *= w, Vtx[i].z = Vtx[i + nseg].z *= w;
		Vtx[i + 3 * nseg].x = Vtx[i + 2 * nseg].x *= w, Vtx[i + 3 * nseg].z = Vtx[i + 2 * nseg].z *= w;
	}

	// rotate vector
	VECTOR3 d = end / h;
	double tht = acos(d.y), phi = atan2(d.z, d.x);
	float cost = (float)cos(tht), sint = (float)sin(tht);
	float cosp = (float)cos(phi), sinp = (float)sin(phi);
	D3DMATRIX W, R = { cost * cosp, -sint, cost * sinp, 0,
					   sint * cosp,  cost, sint * sinp, 0,
					  -sinp,       0   , cosp,      0,
					   0,          0,    0,         1 };

	// shift vector
	R._41 = (float)orig.x;
	R._42 = (float)orig.y;
	R._43 = (float)orig.z;
	D3DMAT_MatrixMultiply(&W, &mWorld, &R);
	dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &W);

	MATRIX3 grot;
	VECTOR3 gpos;
	oapiGetRotationMatrix(hObj, &grot);
	VECTOR3 cp = tmul(grot, -cpos);
	if (dotp(d, unit(end - cp)) > 0)
		Idx = Idx1, nIdx = nIdx1;
	else
		Idx = Idx0, nIdx = nIdx0;

	dev->DrawIndexedPrimitive(
		D3DPT_TRIANGLELIST, D3DFVF_VERTEX, Vtx, nVtx, Idx, nIdx, 0);
	dev->SetTransform(D3DTRANSFORMSTATE_WORLD, &mWorld);
	return true;
}
