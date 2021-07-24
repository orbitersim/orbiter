// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VVessel.cpp
// Vessel visualisation
// ==============================================================

#include "VVessel.h"
#include "MeshMgr.h"
#include "Texture.h"

using namespace oapi;

// ==============================================================
// Local prototypes

void TransformPoint (VECTOR3 &p, const D3DMATRIX &T);
void TransformDirection (VECTOR3 &a, const D3DMATRIX &T, bool normalise);

// ==============================================================
// class vVessel (implementation)
//
// A vVessel is the visual representation of a vessel object.
// ==============================================================

vVessel::vVessel (OBJHANDLE _hObj, const Scene *scene): vObject (_hObj, scene)
{
	vessel = oapiGetVesselInterface (_hObj);
	nmesh = 0;
	nanim = 0;
	bLocalLight = false;
	localLight = *scene->GetLight();
	tCheckLight = oapiGetSimTime()-1.0;
	LoadMeshes ();
	InitAnimations ();
}

vVessel::~vVessel ()
{
	ClearAnimations();
	ClearMeshes();
}

void vVessel::GlobalInit (D3D7Client *gc)
{
	const DWORD texsize = *(int*)gc->GetConfigParam (CFGPRM_PANELMFDHUDSIZE);

	if (mfdsurf) mfdsurf->Release();
	mfdsurf = (LPDIRECTDRAWSURFACE7)gc->clbkCreateTexture (texsize, texsize);

	if (defexhausttex) defexhausttex->Release();
	gc->GetTexMgr()->LoadTexture ("Exhaust.dds", &defexhausttex, 0);
}

void vVessel::GlobalExit ()
{
	if (mfdsurf) {
		mfdsurf->Release();
		mfdsurf = 0;
	}
	if (defexhausttex) {
		defexhausttex->Release();
		defexhausttex = 0;
	}
}

void vVessel::clbkEvent (DWORD event, UINT context)
{
	switch (event) {
	case EVENT_VESSEL_INSMESH:
		InsertMesh (context);
		break;
	case EVENT_VESSEL_DELMESH:
		DelMesh (context);
		break;
	case EVENT_VESSEL_MESHVISMODE:
		// todo
		break;
	case EVENT_VESSEL_MESHOFS: {
		DWORD idx = (DWORD)context;
		if (idx < nmesh) {
			VECTOR3 ofs;
			vessel->GetMeshOffset (idx, ofs);
			if (length(ofs)) {
				if (!meshlist[idx].trans)
					meshlist[idx].trans = new D3DMATRIX;
				D3DMAT_Identity (meshlist[idx].trans);
				D3DMAT_SetTranslation (meshlist[idx].trans, &ofs);
			} else {
				if (meshlist[idx].trans) {
					delete meshlist[idx].trans;
					meshlist[idx].trans = 0;
				}
			}
		}
		} break;
	case EVENT_VESSEL_MODMESHGROUP:
		MessageBeep (-1);
		break;
	}
}

MESHHANDLE vVessel::GetMesh (UINT idx)
{
	return (idx < nmesh ? meshlist[idx].mesh : NULL);
}

bool vVessel::Update ()
{
	if (!active) return false;

	vObject::Update ();
	UpdateAnimations ();

	if (oapiGetSimTime() > tCheckLight)
		bLocalLight = ModLighting (&localLight);

	return true;
}

void vVessel::LoadMeshes ()
{
	if (nmesh) ClearMeshes();
	MESHHANDLE hMesh;
	const D3D7Mesh *mesh;
	VECTOR3 ofs;
	UINT idx;
	MeshManager *mmgr = gc->GetMeshMgr();

	nmesh = vessel->GetMeshCount();
	meshlist = new MESHREC[nmesh];
	memset (meshlist, 0, nmesh*sizeof(MESHREC));

	for (idx = 0; idx < nmesh; idx++) {
		if ((hMesh = vessel->GetMeshTemplate (idx)) && (mesh = mmgr->GetMesh (hMesh))) {
			// copy from preloaded template
			meshlist[idx].mesh = new D3D7Mesh (*mesh);
		} else if (hMesh = vessel->CopyMeshFromTemplate (idx)) {
			// load on the fly and discard after copying
			meshlist[idx].mesh = new D3D7Mesh (gc, hMesh);
			oapiDeleteMesh (hMesh);
		}
		if (meshlist[idx].mesh) {
			meshlist[idx].vismode = vessel->GetMeshVisibilityMode (idx);
			vessel->GetMeshOffset (idx, ofs);
			if (length(ofs)) {
				meshlist[idx].trans = new D3DMATRIX;
				D3DMAT_Identity (meshlist[idx].trans);
				D3DMAT_SetTranslation (meshlist[idx].trans, &ofs);
				// currently only mesh translations are supported
			}
		}
	}
}

void vVessel::InsertMesh (UINT idx)
{
	UINT i;

	if (idx >= nmesh) { // append a new entry to the list
		MESHREC *tmp = new MESHREC[idx+1];
		if (nmesh) {
			memcpy (tmp, meshlist, nmesh*sizeof(MESHREC));
			delete []meshlist;
		}
		meshlist = tmp;
		for (i = nmesh; i < idx; i++) { // zero any intervening entries
			meshlist[i].mesh = 0;
			meshlist[i].trans = 0;
			meshlist[i].vismode = 0;
		}
		nmesh = idx+1;
	} else if (meshlist[idx].mesh) { // replace existing entry
		delete meshlist[idx].mesh;
		if (meshlist[idx].trans) {
			delete meshlist[idx].trans;
			meshlist[idx].trans = 0;
		}
	}

	// now add the new mesh
	MESHHANDLE hMesh;
	const D3D7Mesh *mesh;
	MeshManager *mmgr = gc->GetMeshMgr();
	VECTOR3 ofs;
	if ((hMesh = vessel->GetMeshTemplate (idx)) && (mesh = mmgr->GetMesh (hMesh))) {
		meshlist[idx].mesh = new D3D7Mesh (*mesh);
	} else if (hMesh = vessel->CopyMeshFromTemplate (idx)) {
		meshlist[idx].mesh = new D3D7Mesh (gc, hMesh);
		oapiDeleteMesh (hMesh);
	} else {
		meshlist[idx].mesh = 0;
	}
	if (meshlist[idx].mesh) {
		meshlist[idx].vismode = vessel->GetMeshVisibilityMode (idx);
		vessel->GetMeshOffset (idx, ofs);
		if (length(ofs)) {
			meshlist[idx].trans = new D3DMATRIX;
			D3DMAT_Identity (meshlist[idx].trans);
			D3DMAT_SetTranslation (meshlist[idx].trans, &ofs);
			// currently only mesh translations are supported
		} else {
			meshlist[idx].trans = 0;
		}
	}
}

void vVessel::ClearMeshes ()
{
	if (nmesh) {
		for (UINT i = 0; i < nmesh; i++) {
			if (meshlist[i].mesh) delete meshlist[i].mesh;
			if (meshlist[i].trans) delete meshlist[i].trans;
		}
		delete []meshlist;
		nmesh = 0;
	}
}

void vVessel::DelMesh (UINT idx)
{
	if (idx >= nmesh) return;
	if (!meshlist[idx].mesh) return;
	delete meshlist[idx].mesh;
	meshlist[idx].mesh = 0;
	if (meshlist[idx].trans) {
		delete meshlist[idx].trans;
		meshlist[idx].trans = 0;
	}
}

void vVessel::InitAnimations ()
{
	if (nanim) ClearAnimations();
	nanim = vessel->GetAnimPtr (&anim);
	if (nanim) {
		UINT i;
		animstate = new double[nanim];
		for (i = 0; i < nanim; i++)
			animstate[i] = anim[i].defstate; // reset to default mesh states
	}
}

void vVessel::ClearAnimations ()
{
	if (nanim) {
		delete []animstate;
		nanim = 0;
	}
}

void vVessel::UpdateAnimations (UINT mshidx)
{
	double newstate;
	for (UINT i = 0; i < nanim; i++) {
		if (!anim[i].ncomp) continue;
		if (animstate[i] != (newstate = anim[i].state)) {
			Animate (i, newstate, mshidx);
			animstate[i] = newstate;
		}
	}
}

bool vVessel::Render (LPDIRECT3DDEVICE7 dev)
{
	if (!active) return false;
	Render (dev, false);
	return true;
}

bool vVessel::Render (LPDIRECT3DDEVICE7 dev, bool internalpass)
{
	if (!active) return false;
	UINT i, mfd;
	bool bWorldValid = false;

	bool bCockpit = (oapiCameraInternal() && hObj == oapiGetFocusObject());
	// render cockpit view

	bool bVC = (bCockpit && oapiCockpitMode() == COCKPIT_VIRTUAL);
	// render virtual cockpit

	const VCHUDSPEC *hudspec;
	const VCMFDSPEC *mfdspec[MAXMFD];
	SURFHANDLE sHUD, sMFD[MAXMFD];
	D3DLIGHT7 globalLight;

	if (bLocalLight) {
		dev->GetLight (0, &globalLight);
		dev->SetLight (0, &localLight);
	}

	if (bVC) {
		sHUD = gc->GetVCHUDSurface (&hudspec);
		for (mfd = 0; mfd < MAXMFD; mfd++)
			sMFD[mfd] = gc->GetVCMFDSurface (mfd, &mfdspec[mfd]);
	}

	for (i = 0; i < nmesh; i++) {

		if (!meshlist[i].mesh) continue;

		// check if mesh should be rendered in this pass
		WORD vismode = meshlist[i].vismode;
		if (bCockpit) {
			if (internalpass && (vismode & MESHVIS_EXTPASS)) continue;
			if (!(vismode & MESHVIS_COCKPIT)) {
				if ((!bVC) || (!(vismode & MESHVIS_VC))) continue;
			}
		} else {
			if (!(vismode & MESHVIS_EXTERNAL)) continue;
		}

		// transform mesh
		if (meshlist[i].trans) {
			D3DMATRIX mWorldTrans;
			D3DMAT_MatrixMultiply (&mWorldTrans, &mWorld, meshlist[i].trans);
			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorldTrans);
			bWorldValid = false;
		} else if (!bWorldValid) {
			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
			bWorldValid = true;
		}

		if (bVC) { // link MFD textures for rendering
			for (mfd = 0; mfd < MAXMFD; mfd++) {
				if (mfdspec[mfd] && mfdspec[mfd]->nmesh == i) {
					meshlist[i].mesh->GetGroup(mfdspec[mfd]->ngroup)->TexIdx = TEXIDX_MFD0+mfd;
				}
			}
		}

		// render mesh
		meshlist[i].mesh->Render (dev);

		// render VC HUD and MFDs
		if (bVC) {

			// render VC HUD
			if (sHUD && hudspec->nmesh == i) {
				gc->clbkBlt (mfdsurf, 0, 0, sHUD);
				// we need to copy the HUD surface here, because the generic sHUD handle
				// doesn't contain a texture attribute, so can't be used as a texture
				dev->SetTexture (0, mfdsurf);
				dev->SetRenderState (D3DRENDERSTATE_LIGHTING, FALSE);
				dev->SetRenderState (D3DRENDERSTATE_ZENABLE, FALSE);
				dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
				meshlist[i].mesh->RenderGroup (dev, meshlist[i].mesh->GetGroup(hudspec->ngroup));
				dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
				dev->SetRenderState (D3DRENDERSTATE_LIGHTING, TRUE);
				dev->SetRenderState (D3DRENDERSTATE_ZENABLE, TRUE);
			}
		}
	}

	if (bLocalLight)
		dev->SetLight (0, &globalLight);
	return true;
}

bool vVessel::RenderExhaust (LPDIRECT3DDEVICE7 dev)
{
	if (!active) return false;
	DWORD i, nexhaust = vessel->GetExhaustCount();
	if (!nexhaust) return true; // nothing to do

	bool need_setup = true;
	double lvl, xsize, zsize;
	VECTOR3 cdir;
	LPDIRECTDRAWSURFACE7 tex, ptex = 0;
	EXHAUSTSPEC es;
	static D3DMATERIAL7 engmat = { // emissive material for engine exhaust
		{0,0,0,1},
		{0,0,0,1},
		{0,0,0,1},
		{1,1,1,1},
		0.0
	};
	static VERTEX_XYZ_TEX ExhaustVtx[8] = {
		{0,0,0, 0.24f,0},
		{0,0,0, 0.24f,1},
		{0,0,0, 0.01f,0},
		{0,0,0, 0.01f,1},
		{0,0,0, 0.50390625f, 0.00390625f},
		{0,0,0, 0.99609375f, 0.00390625f},
		{0,0,0, 0.50390625f, 0.49609375f},
		{0,0,0, 0.99609375f, 0.49609375f}
	};
	static WORD ExhaustIdx[12] = {0,1,2, 3,2,1, 4,5,6, 7,6,5};

	for (i = 0; i < nexhaust; i++) {
		if (!(lvl = vessel->GetExhaustLevel (i))) continue;
		vessel->GetExhaustSpec (i, &es);

		if (need_setup) { // initialise render state
			MATRIX3 R;
			vessel->GetRotationMatrix (R);
			cdir = tmul (R, cpos);
			dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
			dev->SetMaterial (&engmat);
			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
			dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
			dev->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_TFACTOR);
			need_setup = false;
		}
		tex = LPDIRECTDRAWSURFACE7(es.tex);
		if (!tex) tex = defexhausttex;
		if (tex != ptex) dev->SetTexture (0, ptex = tex);

		//xsize = sqrt (zsize = lvl);
		xsize = zsize = 1.0;

		//const double irmax = 0.1/(double)RAND_MAX;
		double alpha = *es.level;
		if (es.modulate) alpha *= ((1.0 - es.modulate)+(double)rand()* es.modulate/(double)RAND_MAX);
		dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(1,1,1,alpha));

		SetExhaustVertices (-(*es.ldir), cdir, *es.lpos, zsize*es.lsize, xsize*es.wsize, ExhaustVtx);
		dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, FVF_XYZ_TEX,
			ExhaustVtx, 8, ExhaustIdx, 12, 0);

	}
	if (!need_setup) { // reset render state
		dev->SetTextureStageState (0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1);
		dev->SetTextureStageState (0, D3DTSS_ALPHAARG2, D3DTA_CURRENT);
		dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
	}
	return true;
}

void vVessel::RenderBeacons (LPDIRECT3DDEVICE7 dev)
{
	DWORD idx = 0;
	const BEACONLIGHTSPEC *bls = vessel->GetBeacon(idx);
	if (!bls) return; // nothing to do
	bool need_setup = true;
	double simt = oapiGetSimTime();
	DWORD doalpha;
	dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &doalpha);
	for (; bls; bls = vessel->GetBeacon (++idx)) {
		if (bls->active) {
			if (bls->period && (fmod(simt+bls->tofs, bls->period) > bls->duration))
				continue;
			double size = bls->size;
			if (cdist > 50.0)
				size *= pow (cdist/50.0, bls->falloff);
			if (need_setup) {
				dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
				dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, FALSE);
				dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 5);
				need_setup = false;
			}
			RenderSpot (dev, bls->pos, (float)size, *bls->col, false, bls->shape);
		}
	}
	// undo device modifications
	if (!need_setup) {
		if (!doalpha) dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		dev->SetRenderState (D3DRENDERSTATE_ZWRITEENABLE, TRUE);
		dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 0);
	}
}

void vVessel::RenderGroundShadow (LPDIRECT3DDEVICE7 dev, OBJHANDLE hPlanet)
{
	static const double eps = 1e-2;
	static const double shadow_elev_limit = 0.07;
	double d, alt, R;
	VECTOR3 pp, sd, pvr;
	oapiGetGlobalPos (hPlanet, &pp); // planet global pos
	vessel->GetGlobalPos (sd);       // vessel global pos
	pvr = sd-pp;                     // planet-relative vessel position
	d = length(pvr);                 // vessel-planet distance
	R = oapiGetSize (hPlanet);       // planet mean radius
	R += vessel->GetSurfaceElevation();  // Note: this only works at low vessel altitudes (shadow close to vessel position)
	alt = d-R;                       // altitude above surface
	if (alt*eps > vessel->GetSize()) // too high to cast a shadow
		return;
	normalise (sd);                  // shadow projection direction

	// calculate the intersection of the vessel's shadow with the planet surface
	double fac1 = dotp (sd, pvr);
	if (fac1 > 0.0)                  // shadow doesn't intersect planet surface
		return;
	double csun = -fac1/d;           // sun elevation above horizon
	if (csun < shadow_elev_limit)    // sun too low to cast shadow
		return;
	double arg  = fac1*fac1 - (dotp (pvr, pvr) - R*R);
	if (arg <= 0.0)                  // shadow doesn't intersect with planet surface
		return;
	double a = -fac1 - sqrt(arg);

	MATRIX3 vR;
	vessel->GetRotationMatrix (vR);
	VECTOR3 sdv = tmul (vR, sd);     // projection direction in vessel frame
	VECTOR3 shp = sdv*a;             // projection point
	VECTOR3 hn, hnp = vessel->GetSurfaceNormal();
	vessel->HorizonInvRot (hnp, hn);

	// perform projections
	double nr0 = dotp (hn, shp);
	double nd  = dotp (hn, sdv);
	VECTOR3 sdvs = sdv / nd;

	DWORD j;

	// build shadow projection matrix
	D3DMATRIX mProj, mProjWorld, mProjWorldShift;
	mProj._11 = 1.0f - (float)(sdvs.x*hn.x);
	mProj._12 =      - (float)(sdvs.y*hn.x);
	mProj._13 =      - (float)(sdvs.z*hn.x);
	mProj._14 = 0;
	mProj._21 =      - (float)(sdvs.x*hn.y);
	mProj._22 = 1.0f - (float)(sdvs.y*hn.y);
	mProj._23 =      - (float)(sdvs.z*hn.y);
	mProj._24 = 0;
	mProj._31 =      - (float)(sdvs.x*hn.z);
	mProj._32 =      - (float)(sdvs.y*hn.z);
	mProj._33 = 1.0f - (float)(sdvs.z*hn.z);
	mProj._34 = 0;
	mProj._41 =        (float)(sdvs.x*nr0);
	mProj._42 =        (float)(sdvs.y*nr0);
	mProj._43 =        (float)(sdvs.z*nr0);
	mProj._44 = 1;
	D3DMAT_MatrixMultiply (&mProjWorld, &mWorld, &mProj);
	bool isProjWorld = false;

	// modify depth of shadows at dawn/dusk
	DWORD tfactor;
	bool resetalpha = false;
	if (gc->UseStencilBuffer()) {
		double scale = min (1, (csun-0.07)/0.015);
		if (scale < 1) {
			dev->GetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, &tfactor);
			float modalpha = (float)(scale*RGBA_GETALPHA(tfactor)/256.0);
			dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0,0,modalpha));
			resetalpha = true;
		}
	}

	// project all vessel meshes. This should be replaced by a dedicated shadow mesh
	for (UINT i = 0; i < nmesh; i++) {
		if (!meshlist[i].mesh) continue;
		if (!(meshlist[i].vismode & MESHVIS_EXTERNAL)) continue; // only render shadows for externally visible meshes
		D3D7Mesh *mesh = meshlist[i].mesh;
		if (meshlist[i].trans) {
			// add mesh offset to transformation
			D3DMAT_MatrixMultiply (&mProjWorldShift, &mProjWorld, meshlist[i].trans);
			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mProjWorldShift);
			isProjWorld = false;
		} else {
			if (!isProjWorld) {
				dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mProjWorld);
				isProjWorld = true;
			}
		}
		for (j = 0; j < mesh->GroupCount(); j++) {
			D3D7Mesh::GROUPREC *grp = mesh->GetGroup(j);
			if (grp->UsrFlag & 1) continue; // "no shadow" flag
			mesh->RenderGroup (dev, grp);	
		}
	}

	if (resetalpha)
		dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, tfactor);
}

void vVessel::SetExhaustVertices (const VECTOR3 &edir, const VECTOR3 &cdir, const VECTOR3 &ref,
	double lscale, double wscale, VERTEX_XYZ_TEX *ev)
{
	// need to rotate the billboard so it faces the observer
	const float flarescale = 7.0;
	VECTOR3 sdir = crossp (cdir, edir); normalise (sdir);
	VECTOR3 tdir = crossp (cdir, sdir); normalise (tdir);
	D3DVALUE rx = (D3DVALUE)ref.x, ry = (D3DVALUE)ref.y, rz = (D3DVALUE)ref.z;
	D3DVALUE sx = (D3DVALUE)(sdir.x*wscale);
	D3DVALUE sy = (D3DVALUE)(sdir.y*wscale);
	D3DVALUE sz = (D3DVALUE)(sdir.z*wscale);
	D3DVALUE ex = (D3DVALUE)(edir.x*lscale);
	D3DVALUE ey = (D3DVALUE)(edir.y*lscale);
	D3DVALUE ez = (D3DVALUE)(edir.z*lscale);
	ev[1].x = (ev[0].x = rx + sx) + ex;
	ev[1].y = (ev[0].y = ry + sy) + ey;
	ev[1].z = (ev[0].z = rz + sz) + ez;
	ev[3].x = (ev[2].x = rx - sx) + ex;
	ev[3].y = (ev[2].y = ry - sy) + ey;
	ev[3].z = (ev[2].z = rz - sz) + ez;
	wscale *= flarescale, sx *= flarescale, sy *= flarescale, sz *= flarescale;
	D3DVALUE tx = (D3DVALUE)(tdir.x*wscale);
	D3DVALUE ty = (D3DVALUE)(tdir.y*wscale);
	D3DVALUE tz = (D3DVALUE)(tdir.z*wscale);
	ev[4].x = rx - sx + tx;   ev[5].x = rx + sx + tx;
	ev[4].y = ry - sy + ty;   ev[5].y = ry + sy + ty;
	ev[4].z = rz - sz + tz;   ev[5].z = rz + sz + tz;
	ev[6].x = rx - sx - tx;   ev[7].x = rx + sx - tx;
	ev[6].y = ry - sy - ty;   ev[7].y = ry + sy - ty;
	ev[6].z = rz - sz - tz;   ev[7].z = rz + sz - tz;
}

bool vVessel::ModLighting (LPD3DLIGHT7 light)
{
	VECTOR3 GV, GS, GP, S, P;

	// we only test the closest celestial body for shadowing
	OBJHANDLE hP = vessel->GetSurfaceRef();
	OBJHANDLE hS = oapiGetGbodyByIndex(0); // the central star
	CELBODY *cb = oapiGetCelbodyInterface(hP);
	CELBODY2 *cb2 = (cb->Version() >= 2 ? (CELBODY2*)cb : NULL);
	vessel->GetGlobalPos(GV);
	oapiGetGlobalPos (hS, &GS);
	S = GS-GV; // sun's position from vessel
	double s = length(S);
	double as = asin(oapiGetSize(hS)/s);
	VECTOR3 lcol = {1,1,1};
	double amb = 0;
	double dt = 1.0;
	bool lightmod = false;
	int i, j;

	// calculate shadowing by planet

	for (i = 0;; i++) {
		oapiGetGlobalPos (hP, &GP);
		P = GP-GV;
		double p = length(P);
		if (p < s) {                                      // shadow only if planet closer than sun
			double psize = oapiGetSize(hP);
			double phi = acos (dotp(S,P)/(s*p));          // angular distance between sun and planet
			double ap = (psize < p ? asin(psize / p) : PI05);  // apparent size of planet disc [rad]

			const ATMCONST *atm = (oapiGetObjectType(hP)==OBJTP_PLANET ? oapiGetPlanetAtmConstants (hP) : NULL);
			if (atm) {  // case 1: planet has atmosphere
				double alt = p-psize;                // vessel altitude
				double altlimit = *(double*)oapiGetObjectParam (hP, OBJPRM_PLANET_ATTENUATIONALT);
				double ap1 = ap * (altlimit+psize)/psize; 
				if (alt < altlimit) {
					static const double delta0 = RAD*100.0;
					// This is the angular separation between planet centre and star below which
					// the atmosphere affects lighting when on the planet surface. (100: when sun
					// is 10 deg above horizon). Should possibly be made atmosphere-specific.
					ap1 = delta0 / (1.0 + alt*(delta0-ap1)/(altlimit*ap1));
				}

				if (as+ap1 >= phi && ap/as > 0.1) {       // overlap and significant planet size
					double dap = ap1-ap;
					VECTOR3 plight = {1,1,1};
					if (as < ap) {                        // planet disc larger than sun disc
						if (phi < ap-as) {                // totality (sun below horizon)
							plight.x = plight.y = plight.z = 0.0;
						} else {
							double dispersion = max (0.02, min (0.9, log (atm->rho0+1.0)));
							double r0 = 1.0-0.35*dispersion;
							double g0 = 1.0-0.75*dispersion;
							double b0 = 1.0-1.0 *dispersion;
							if (phi > as+ap) {            // sun above horizon
								double f = (phi-as-ap)/dap;
								plight.x = f*(1.0-r0) + r0;
								plight.y = f*(1.0-g0) + g0;
								plight.z = f*(1.0-b0) + b0;
							} else {                      // sun partially below horizon
								double f = (phi-ap+as)/(2.0*as);
								plight.x = f*r0;
								plight.y = f*g0;
								plight.z = f*b0;
							}
							dt = 0.1;
						}
					} else {  // planet disc smaller than sun disc
						double maxcover = ap*ap / (as*as);
						if (phi < as-ap)
							plight.x = plight.y = plight.z = 1.0-maxcover; // annularity
						else {
							double frac = 1.0 - 0.5*maxcover * (1.0 + (as-phi)/ap); // partial cover
							plight.x = plight.y = plight.z = frac;
							dt = 0.1;
						}
					}
					for	(j = 0; j < 3; j++) lcol.data[j] = min (lcol.data[j], plight.data[j]);
					lightmod = true;
				}

				// modification of ambient lighting
				if (!i && vessel->GetAtmRef()) {
					double sunelev = phi-ap;
					if (sunelev > - 14.0*RAD) {
						double amb0 = min (0.7, log (atm->rho0+1.0)*0.4);
						double alt = p-psize;
						amb = amb0 / (alt*0.5e-4 + 1.0);
						amb *= min (1.0, (sunelev+14.0*RAD)/(20.0*RAD));
						if (!lightmod) lightmod = (amb > 0.05);
						amb = max (0, amb-0.05);
						// reduce direct light component to avoid overexposure
						lcol *= 1.0-amb*0.5;
					}
				}

			} else {  // case 2: planet has no atmosphere

				if (phi < as+ap && ap/as > 0.1) {         // overlap and significant planet size
					double lfrac;
					if (as < ap) {                        // planet disc larger than sun disc
						if (phi <= ap-as)                 // totality
							lfrac = 0.0;
						else {                            // partial cover
							lfrac = (phi+as-ap)/(2.0*as);
							dt = 0.1;
						}
					} else {                              // sun disc larger than planet disc
						double maxcover = ap*ap / (as*as);
						if (phi < as-ap) {                // annularity
							lfrac = 1.0-maxcover;
						} else {                          // partial cover
							lfrac = 1.0 - 0.5*maxcover * (1.0 + (as-phi)/ap);
							dt = 0.1;
						}
					}
					for (j = 0; j < 3; j++) lcol.data[j] = min (lcol.data[j], lfrac);
					lightmod = true;
				}
			}
		}
		if (!cb2 || (oapiGetObjectType (hP = cb2->GetParent()) != OBJTP_PLANET))
			break;
			// if this is a moon, also check the parent planet
			// warning: currently this only works for moons defined via
			// CELBODY2 interface
		cb = oapiGetCelbodyInterface(hP);
		cb2 = (cb->Version() >= 2 ? (CELBODY2*)cb : NULL);
	}

	if (lightmod) {
		//D3DCOLORVALUE starcol = sun->GetLightColor();
		D3DCOLORVALUE starcol = {1,1,1,1}; // for now
		light->dcvDiffuse.r = light->dcvSpecular.r = starcol.r * (float)lcol.x;
		light->dcvDiffuse.g = light->dcvSpecular.g = starcol.g * (float)lcol.y;
		light->dcvDiffuse.b = light->dcvSpecular.b = starcol.b * (float)lcol.z;
		light->dcvAmbient.r = (float)amb;
		light->dcvAmbient.g = (float)amb;
		light->dcvAmbient.b = (float)amb;
		S /= s;
		light->dvDirection.x = -(float)S.x;
		light->dvDirection.y = -(float)S.y;
		light->dvDirection.z = -(float)S.z;
	}

	tCheckLight = oapiGetSimTime() + dt;
	// we might be able to increase the interval when far from the
	// boundary, but then need to force a check in the case of sudden
	// movement (e.g. editor)

	return lightmod;
}

void vVessel::Animate (UINT an, double state, UINT mshidx)
{
	double s0, s1, ds;
	UINT i, ii;
	D3DMATRIX T;
	ANIMATION *A = anim+an;
	for (ii = 0; ii < A->ncomp; ii++) {
		i = (state > animstate[an] ? ii : A->ncomp-ii-1);
		ANIMATIONCOMP *AC = A->comp[i];
		if (mshidx != (UINT)-1 && mshidx != AC->trans->mesh) continue;
		s0 = animstate[an]; // current animation state in the visual
		if      (s0 < AC->state0) s0 = AC->state0;
		else if (s0 > AC->state1) s0 = AC->state1;
		s1 = state;           // required animation state
		if      (s1 < AC->state0) s1 = AC->state0;
		else if (s1 > AC->state1) s1 = AC->state1;
		if ((ds = (s1-s0)) == 0) continue; // nothing to do for this component
		ds /= (AC->state1 - AC->state0);   // stretch to range 0..1

		// Build transformation matrix
		switch (AC->trans->Type()) {
		case MGROUP_TRANSFORM::NULLTRANSFORM:
			D3DMAT_Identity (&T);
			AnimateComponent (AC, T);
			break;
		case MGROUP_TRANSFORM::ROTATE: {
			MGROUP_ROTATE *rot = (MGROUP_ROTATE*)AC->trans;
			D3DVECTOR ax = {D3DVAL(rot->axis.x), D3DVAL(rot->axis.y), D3DVAL(rot->axis.z)};
			D3DMAT_RotationFromAxis (ax, (float)ds*rot->angle, &T);
			D3DVALUE dx = D3DVAL(rot->ref.x), dy = D3DVAL(rot->ref.y), dz = D3DVAL(rot->ref.z);
			T._41 = dx - T._11*dx - T._21*dy - T._31*dz;
			T._42 = dy - T._12*dx - T._22*dy - T._32*dz;
			T._43 = dz - T._13*dx - T._23*dy - T._33*dz;
			AnimateComponent (AC, T);
			} break;
		case MGROUP_TRANSFORM::TRANSLATE: {
			MGROUP_TRANSLATE *lin = (MGROUP_TRANSLATE*)AC->trans;
			D3DMAT_Identity (&T);
			T._41 = (float)(ds*lin->shift.x);
			T._42 = (float)(ds*lin->shift.y);
			T._43 = (float)(ds*lin->shift.z);
			AnimateComponent (AC, T);
			} break;
		case MGROUP_TRANSFORM::SCALE: {
			MGROUP_SCALE *scl = (MGROUP_SCALE*)AC->trans;
			s0 = (s0-AC->state0)/(AC->state1-AC->state0);
			s1 = (s1-AC->state0)/(AC->state1-AC->state0);
			D3DMAT_Identity (&T);
			T._11 = (float)((s1*(scl->scale.x-1)+1)/(s0*(scl->scale.x-1)+1));
			T._22 = (float)((s1*(scl->scale.y-1)+1)/(s0*(scl->scale.y-1)+1));
			T._33 = (float)((s1*(scl->scale.z-1)+1)/(s0*(scl->scale.z-1)+1));
			T._41 = (float)scl->ref.x * (1.0f-T._11);
			T._42 = (float)scl->ref.y * (1.0f-T._22);
			T._43 = (float)scl->ref.z * (1.0f-T._33);
			AnimateComponent (AC, T);
			} break;
		}
	}
}

void vVessel::AnimateComponent (ANIMATIONCOMP *comp, const D3DMATRIX &T)
{
	UINT i;
	MGROUP_TRANSFORM *trans = comp->trans;

	if (trans->mesh == LOCALVERTEXLIST) { // transform a list of individual vertices

		VECTOR3 *vtx = (VECTOR3*)trans->grp;
		for (i = 0; i < trans->ngrp; i++)
			TransformPoint (vtx[i], T);

	} else {                              // transform mesh groups

		if (trans->mesh >= nmesh) return; // mesh index out of range
		D3D7Mesh *mesh = meshlist[trans->mesh].mesh;
		if (!mesh) return;

		if (trans->grp) { // animate individual mesh groups
			for (i = 0; i < trans->ngrp; i++)
				mesh->TransformGroup (trans->grp[i], &T);
		} else {          // animate complete mesh
//			mesh->Transform (T);
		}
	}

	// recursively transform all child animations
	for (i = 0; i < comp->nchildren; i++) {
		ANIMATIONCOMP *child = comp->children[i];
		AnimateComponent (child, T);
		switch (child->trans->Type()) {
		case MGROUP_TRANSFORM::NULLTRANSFORM:
			break;
		case MGROUP_TRANSFORM::ROTATE: {
			MGROUP_ROTATE *rot = (MGROUP_ROTATE*)child->trans;
			TransformPoint (rot->ref, T);
			TransformDirection (rot->axis, T, true);
			} break;
		case MGROUP_TRANSFORM::TRANSLATE: {
			MGROUP_TRANSLATE *lin = (MGROUP_TRANSLATE*)child->trans;
			TransformDirection (lin->shift, T, false);
			} break;
		case MGROUP_TRANSFORM::SCALE: {
			MGROUP_SCALE *scl = (MGROUP_SCALE*)child->trans;
			TransformPoint (scl->ref, T);
			// we can't transform anisotropic scaling vector
			} break;
		}
	}
}

LPDIRECTDRAWSURFACE7 vVessel::mfdsurf = 0;
LPDIRECTDRAWSURFACE7 vVessel::defexhausttex = 0;

// ==============================================================
// Nonmember helper functions

void TransformPoint (VECTOR3 &p, const D3DMATRIX &T)
{
	double x = p.x*T._11 + p.y*T._21 + p.z*T._31 + T._41;
	double y = p.x*T._12 + p.y*T._22 + p.z*T._32 + T._42;
	double z = p.x*T._13 + p.y*T._23 + p.z*T._33 + T._43;
	double w = p.x*T._14 + p.y*T._24 + p.z*T._34 + T._44;
    p.x = x/w;
	p.y = y/w;
	p.z = z/w;
}

void TransformDirection (VECTOR3 &a, const D3DMATRIX &T, bool normalise)
{
	double x = a.x*T._11 + a.y*T._21 + a.z*T._31;
	double y = a.x*T._12 + a.y*T._22 + a.z*T._32;
	double z = a.x*T._13 + a.y*T._23 + a.z*T._33;
	a.x = x, a.y = y, a.z = z;
	if (normalise) {
		double len = sqrt (x*x + y*y + z*z);
		a.x /= len;
		a.y /= len;
		a.z /= len;
	}
}

