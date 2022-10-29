// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION

#include "Orbiter.h"
#include "D3dmath.h"
#include "Vecmat.h"
#include "Camera.h"
#include "Scene.h"
#include "Body.h"
#include "Vobject.h"
#include "Planet.h"
#include "Texture.h"
#include "OGraphics.h"
#include "Log.h"
#include "Astro.h"
#include <stdio.h>
#include <algorithm>

using namespace oapi;

class Vessel;

extern Orbiter *g_pOrbiter;
extern Camera *g_camera;
extern char DBG_MSG[256];
extern Vessel *g_focusobj;
extern bool g_bExternalView;
extern TextureManager2 *g_texmanager2;

OrbiterGraphics *VObject::gc = NULL;
LPDIRECTDRAWSURFACE7 VObject::blobtex[3] = {0,0,0};
Scene *VObject::scene = NULL;

VObject::VObject (const Body *_body): body(_body)
{
	dmWorld = identity4();
	VMAT_identity (mWorld);
	isdist_old = 0.0;
	apprad_factor = g_camera->TanAperture() / (body->Size() * g_pOrbiter->ViewH()*0.5);
}

void VObject::CreateDeviceObjects (OrbiterGraphics *gclient)
{
	FILE *file;
	gc = gclient;
	for (int i = 0; i < 3; i++) {
		static char *fname[3] = {"Ball","Ball2","Ball3"};
		file = fopen (g_pOrbiter->TexPath (fname[i]), "rb");
		if (file) {
			g_texmanager2->ReadTexture(file, blobtex + i);
			fclose(file);
		}
	}
}

void VObject::DestroyDeviceObjects ()
{
	for (int i = 0; i < 3; i++)
		if (blobtex[i]) {
			blobtex[i]->Release();
			blobtex[i] = 0;
		}
}

bool veccomp(const VObject::BodyVectorRec& v1, const VObject::BodyVectorRec& v2)
{
	return v1.dist > v2.dist; // sort from furthest to nearest
}

void VObject::Update (bool moving, bool force)
{
	if (body == g_camera->Target())
		cpos.Set (-(*g_camera->GSPosPtr()));
	else
		cpos.Set (body->GPos() - g_camera->GPos());
	campos = tmul(body->GRot(), -cpos);
	cdist = cpos.length();

	if (force) {
		apprad_factor = g_camera->TanAperture() /
			(body->Size() * g_pOrbiter->ViewH()*0.5);
	}

	// inverse apparent radius
	iapprad = cdist * apprad_factor;

	// resolution check
	double sdist, sdist_ratio;
	sdist_ratio = (sdist = cdist-body->Size()) * isdist_old;
	if (sdist_ratio < 0.9 || sdist_ratio > 1.1 || force) {
		CheckResolution (iapprad);
		isdist_old = (sdist ? 1.0/sdist : 1.0);
	}

	// update world transform matrix
	// note we need inverse of rotation matrix because rotations in logical
	// interface are counterclockwise, but clockwise in D3D
	const Matrix &W = body->GRot();
	dmWorld = _M(W.m11, W.m21, W.m31, 0,
		         W.m12, W.m22, W.m32, 0,
				 W.m13, W.m23, W.m33, 0,
                 cpos.x,cpos.y,cpos.z,1);
	SetInvD3DRotation (mWorld, body->GRot());
	SetD3DTranslation (mWorld, cpos);

	UpdateRenderVectors();
	std::sort(veclist.begin(), veclist.end(), veccomp);
}

void VObject::UpdateRenderVectors()
{
	veclist.clear();

	// render object frame axes
	DWORD flag = *(DWORD*)gc->GetConfigParam(CFGPRM_FRAMEAXISFLAG);
	if (flag & FAV_ENABLE) {
		int tp = body->Type();
		if ((tp == OBJTP_VESSEL && flag & FAV_VESSEL) ||
			(tp == OBJTP_PLANET && flag & FAV_CELBODY) ||
			(tp == OBJTP_SURFBASE && flag & FAV_BASE)) {

			double scale = body->Size() * *(float*)gc->GetConfigParam(CFGPRM_FRAMEAXISSCALE);
			double rad = body->Size() * 0.01;
			float alpha = *(float*)gc->GetConfigParam(CFGPRM_FRAMEAXISOPACITY);
			AddVector(Vector(scale, 0, 0), Vector(0, 0, 0), rad, std::string("+x"), Vector(1, 1, 1), alpha, D3DRGB(1, 1, 1));
			AddVector(Vector(0, scale, 0), Vector(0, 0, 0), rad, std::string("+y"), Vector(1, 1, 1), alpha, D3DRGB(1, 1, 1));
			AddVector(Vector(0, 0, scale), Vector(0, 0, 0), rad, std::string("+z"), Vector(1, 1, 1), alpha, D3DRGB(1, 1, 1));
			if (flag & FAV_NEGATIVE) {
				AddVector(Vector(-scale, 0, 0), Vector(0, 0, 0), rad, std::string("-x"), Vector(1, 1, 1), alpha, D3DRGB(1, 1, 1));
				AddVector(Vector(0, -scale, 0), Vector(0, 0, 0), rad, std::string("-y"), Vector(1, 1, 1), alpha, D3DRGB(1, 1, 1));
				AddVector(Vector(0, 0, -scale), Vector(0, 0, 0), rad, std::string("-z"), Vector(1, 1, 1), alpha, D3DRGB(1, 1, 1));
			}
		}
	}
}

void VObject::RenderAsDisc (LPDIRECT3DDEVICE7 dev)
{
	// disc size
	// scale up disc radius for small apparent radii
	double rad = body->Size();
	if (iapprad > 0.25) rad *= pow(4.0*iapprad,0.75);

	// scale down intensity with angle of illumination
	const double ambient = 0.3; // assumed unlit/lit emission ratio
	double cosa = dotp (body->GPos().unit(), (body->GPos() - g_camera->GPos()).unit());
	double illum = 0.5 * ((1.0-ambient)*cosa + 1.0+ambient);

	// scale down intensity with distance from sun (only very gently)
	double sundist = body->GPos().length() * iAU;
	illum *= pow(sundist, -0.4);

	// scale with albedo components
	const Vector &albedo = body->Albedo();
	double abr = albedo.x*illum;
	double abg = albedo.y*illum;
	double abb = albedo.z*illum;
	double abmax = max (abr, max (abg, abb));
	if (abmax > 1.0) {
		rad *= sqrt(abmax);
		abr *= 1.0/abmax;
		abg *= 1.0/abmax;
		abb *= 1.0/abmax;
	}

	RenderSpot (dev, 0, (float)rad, Vector(abr,abg,abb), false, 0);
}

void VObject::RenderAsPixel (LPDIRECT3DDEVICE7 dev)
{
	// Render distant planet as 2x2 pixel block

	D3DVECTOR homog, cdir = {-mWorld._41, -mWorld._42, -mWorld._43};
	D3DMath_Normalise (cdir);
	D3DMath_VectorMatrixMultiply (homog, cdir, *g_camera->D3D_ProjViewMatrix());

	if (homog.x >= -1.0f && homog.x <= 1.0f &&
		homog.y >= -1.0f && homog.y <= 1.0f &&
		homog.z >=  0.0) {
		RECT r;
		int x, y;
		DWORD viewW = g_pOrbiter->ViewW(), viewH = g_pOrbiter->ViewH();
		if (_hypot (homog.x, homog.y) < 1e-6) {
			x = viewW/2, y = viewH/2;
		} else {
			x = (int)(viewW*0.5*(1.0f+homog.x));
			y = (int)(viewH*0.5*(1.0f-homog.y));
		}
		r.left = x-1, r.right  = x+2;
		r.top  = y-1, r.bottom = y+2;
		DDBLTFX ddbltfx;
		ddbltfx.dwSize = sizeof (ddbltfx);

		// pixel intensity
		// 1. apparent radius
		double intens = 4.0 * pow (1.0/(3.0*iapprad), 0.4); // scale SLOWLY with apparent radius (fudge screen intensity limits)

		// 2. angle of illumination
		const double ambient = 0.3;
		double cosa = dotp (body->GPos().unit(), (body->GPos() - g_camera->GPos()).unit());
		intens *= 0.5 * ((1.0-ambient)*cosa + 1.0+ambient);

		const Vector &albedo = body->Albedo();
		double abr = albedo.x;
		double abg = albedo.y;
		double abb = albedo.z;
		double abmax = max (abr, max (abg, abb));
		intens *= abmax;
		abr *= 256.0/abmax;
		abg *= 256.0/abmax;
		abb *= 256.0/abmax;

		if (scene->BgBrightnessLevel() > intens*150.0) return;
		// sky too bright for planet to be visible

		// 3. albedo
		HDC hDC;
		int ir, ig, ib, c;
		double ip;

		g_pOrbiter->GetInlineGraphicsClient()->GetRenderTarget()->GetDC (&hDC);
		ip = min (1.0, intens*2.0);
		ir = min (255, (int)(ip*abr));
		ig = min (255, (int)(ip*abg));
		ib = min (255, (int)(ip*abb));
		c = (ib << 16) + (ig << 8) + ir;
		SetPixel (hDC, x, y, c);
		if ((ip = min (1.0, (intens-0.5)*2.0)) > 0.0) {
			ir = min (255, (int)(ip*abr));
			ig = min (255, (int)(ip*abg));
			ib = min (255, (int)(ip*abb));
			c = (ib << 16) + (ig << 8) + ir;
			SetPixel (hDC, x-1, y, c);
			SetPixel (hDC, x+1, y, c);
			SetPixel (hDC, x, y-1, c);
			SetPixel (hDC, x, y+1, c);
			if ((ip = min (1.0, (intens-2.0)*1.0)) > 0.0) {
				ir = min (255, (int)(ip*abr));
				ig = min (255, (int)(ip*abg));
				ib = min (255, (int)(ip*abb));
				c = (ib << 16) + (ig << 8) + ir;
				SetPixel (hDC, x-1, y-1, c);
				SetPixel (hDC, x-1, y+1, c);
				SetPixel (hDC, x+1, y-1, c);
				SetPixel (hDC, x+1, y+1, c);
			}
		}
		g_pOrbiter->GetInlineGraphicsClient()->GetRenderTarget()->ReleaseDC (hDC);
	}
}

void VObject::RenderSpot (LPDIRECT3DDEVICE7 dev, const Vector *ofs, float size, const Vector &col, bool lighting, int shape)
{
	static D3DMATRIX W = {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1};

	Vector pos (cpos);
	if (ofs) pos += mul (body->GRot(), *ofs);
	double dist = pos.length();

	double maxdist = 0.9*g_camera->Farplane();
	if (dist > maxdist) {
		pos *= maxdist/dist;
		size *= (float)(maxdist/dist);
		dist = maxdist;
	}

	Vector bdir(pos/dist);
	double hz = _hypot (bdir.x, bdir.z);
	double phi = atan2 (bdir.z, bdir.x);
	float sphi = (float)sin(phi), cphi = (float)cos(phi);
	DWORD alphamode;

	const double ambient = 0.2;
	double cosa = dotp (body->GPos().unit(), (body->GPos() - g_camera->GPos()).unit());
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
	dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &alphamode);
	if (alphamode == FALSE) dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
	dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
	dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, D3DFVF_VERTEX, vtx, 4, idx, 6, 0);
	dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
	dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
	if (alphamode == FALSE) dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
}

void VObject::RenderAsSpot (LPDIRECT3DDEVICE7 dev, D3DCOLORVALUE *illumination)
{
	double d0 = 0.5*g_camera->Apprad_dist (1.0, body->Size());             // distance at which we switch to spot representation
	double d1 = body->Size()/(body->SpotLimit()*g_camera->TanAperture());  // limit of spot visibility
	if (d1 < d0) return;
	float size = (float)(body->Size() * cdist*(d1-cdist) / (d0*(d1-d0)));
	// scale blob size so that apparent size reduces linearly from d0 to d1
	if (size < 0.0) return; // sanity check
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
	RenderSpot (dev, 0, size, illumination ? body->Albedo()*Vector(illumination->r,illumination->g,illumination->b) : body->Albedo(), true);
	dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
}

void VObject::AddVector (const Vector &v, const Vector &orig, double rad, const std::string& label, const Vector &col, float alpha, DWORD lcol, float lsize)
{
	double len = v.length();
	if (len < 2.0*rad) return; // too short to be rendered

	Vector vu = v / len;
	double dist = (vu - campos).length();

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

void VObject::RenderVectors (LPDIRECT3DDEVICE7 dev)
{
	if (veclist.size()) {
		float palpha = -1.0f;
		Vector pcol(-1, -1, -1);
		for (auto&& vec : veclist) {
			if (vec.alpha != palpha || vec.col.x != pcol.x || vec.col.y != pcol.y || vec.col.z != pcol.z) {
				dev->SetRenderState(D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(vec.col.x, vec.col.y, vec.col.z, vec.alpha));
				palpha = vec.alpha; pcol = vec.col;
			}
			DrawVector(dev, vec.v, vec.orig, vec.rad);
		}
	}
}

void VObject::RenderVectorLabels(LPDIRECT3DDEVICE7 dev)
{
	if (veclist.size()) {
		for (auto&& vec : veclist) {
			if (vec.label.size()) {
				double scale = (vec.lsize >= 0 ? vec.lsize : body->Size());
				scene->Render3DLabel (mul (body->GRot(), vec.v + vec.v.unit()*(scale*0.1)) + body->GPos(),
					vec.label.c_str(), scale, vec.lcol);
			}
		}
	}
}

bool VObject::DrawVector (LPDIRECT3DDEVICE7 dev, const Vector &end, const Vector &orig, double rad)
{
	static const float EPS = 1e-2f;
	static const int nseg = 8;
	static const int nVtx = nseg*5;
	static const int nIdx0 = nseg*6 + nseg*3;
	static const int nIdx1 = nIdx0 + (nseg-2)*3;
	int i, nIdx;
	WORD *Idx;
	static D3DVERTEX *Vtx0 = 0, *Vtx = 0;
	static WORD *Idx0 = 0, *Idx1 = 0;
	if (!Vtx0) { // create vector mesh template (pointing to +y)
		int i0, i1;
		Vtx0 = new D3DVERTEX[nVtx]; TRACENEW
		Vtx = new D3DVERTEX[nVtx]; TRACENEW
		Idx0 = new WORD[nIdx0]; TRACENEW
		Idx1 = new WORD[nIdx1]; TRACENEW
		for (i = 0; i < nseg; i++) {
			Vtx0[i+2*nseg].x = Vtx0[i+3*nseg].x = 2.0f * (Vtx0[i].x = Vtx0[i+nseg].x = Vtx0[i].nx = Vtx0[i+nseg].nx = (float)cos(Pi2*(double)i/(double)nseg));
			Vtx0[i+2*nseg].z = Vtx0[i+3*nseg].z = 2.0f * (Vtx0[i].z = Vtx0[i+nseg].z = Vtx0[i].nz = Vtx0[i+nseg].nz = (float)sin(Pi2*(double)i/(double)nseg));
			Vtx0[i+4*nseg].x = Vtx0[i+4*nseg].z = 0.0f;
			Vtx0[i].y = 0.0f; Vtx0[i+nseg].y = Vtx0[i+2*nseg].y = Vtx0[i+3*nseg].y = Vtx0[i+4*nseg].y = 1.0f;
			Vtx0[i+3*nseg].nx = Vtx0[i+4*nseg].nx = (float)(0.894*cos(Pi2*(double)(i+0.5)/(double)nseg));
			Vtx0[i+3*nseg].nz = Vtx0[i+4*nseg].nz = (float)(0.894*sin(Pi2*(double)(i+0.5)/(double)nseg));
			Vtx0[i+3*nseg].ny = Vtx0[i+4*nseg].ny = 0.447f;
			Vtx0[i].ny = Vtx0[i+nseg].ny = Vtx0[i+2*nseg].nx = Vtx0[i+2*nseg].nz = 0.0f;
			Vtx0[i+2*nseg].ny = -1.0f;
		}
		for (i = i0 = 0, i1 = 6*(nseg-1); i < nseg; i++) { // stem
			Idx0[i0++] = Idx1[i1++] = (WORD)i;
			Idx0[i0++] = Idx1[i1++] = (WORD)(i+nseg);
			Idx0[i0++] = Idx1[i1++] = (WORD)((i+1)%nseg + nseg);
			Idx0[i0++] = Idx1[i1++] = (WORD)i;
			Idx0[i0++] = Idx1[i1++] = (WORD)((i+1)%nseg + nseg);
			Idx0[i0++] = Idx1[i1++] = (WORD)((i+1)%nseg);
		}
		for (i = 0, i1 = nseg*3; i < nseg-2; i++) {      // tip base
			Idx1[i1++] = nseg*2;
			Idx1[i1++] = nseg*2 + 1 + i;
			Idx1[i1++] = nseg*2 + 2 + i;
		}
		for (i = i1 = 0, i0 = nseg*6; i < nseg; i++) {   // tip
			Idx0[i0++] = Idx1[i1++] = nseg*3 + i;
			Idx0[i0++] = Idx1[i1++] = nseg*4 + i;
			Idx0[i0++] = Idx1[i1++] = 3*nseg + (i+1)%nseg;
		}
	}

	float w = (float)rad;
	float h = (float)end.length();
	if (h < EPS) return false;
	float hb = max (h-4.0f*w, 0);

	memcpy (Vtx, Vtx0, nVtx*sizeof(D3DVERTEX));

	for (i = 0; i < nseg; i++) {
		Vtx[i+3*nseg].y = Vtx[i+2*nseg].y = Vtx[i+nseg].y = hb;
		Vtx[i+4*nseg].y = h;
		Vtx[i].x = Vtx[i+nseg].x *= w, Vtx[i].z = Vtx[i+nseg].z *= w;
		Vtx[i+3*nseg].x = Vtx[i+2*nseg].x *= w, Vtx[i+3*nseg].z = Vtx[i+2*nseg].z *= w;
	}

	// rotate vector
	Vector d(end/h);
	double tht = acos(d.y), phi = atan2(d.z, d.x);
	float cost = (float)cos(tht), sint = (float)sin(tht);
	float cosp = (float)cos(phi), sinp = (float)sin(phi);
	D3DMATRIX W, R = { cost*cosp, -sint, cost*sinp, 0,
	 	               sint*cosp,  cost, sint*sinp, 0,
				      -sinp,       0   , cosp,      0,
				       0,          0,    0,         1};

	// shift vector
	R._41 = (float)orig.x;
	R._42 = (float)orig.y;
	R._43 = (float)orig.z;
	D3DMath_MatrixMultiply (W, mWorld, R);
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &W);

	Vector cp (tmul (body->GRot(), -cpos));
	if (dotp (d, (end - cp).unit()) > 0)
		Idx = Idx1, nIdx = nIdx1;
	else
		Idx = Idx0, nIdx = nIdx0;

	dev->DrawIndexedPrimitive (
		D3DPT_TRIANGLELIST, D3DFVF_VERTEX, Vtx, nVtx, Idx, nIdx, 0);
	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
	return true;
}
