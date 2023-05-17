// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "OGraphics.h"
#include "Orbiter.h"
#include "VBase.h"
#include "Psys.h"
#include "Mesh.h"
#include "Texture.h"
#include "Camera.h"
#include "Scene.h"
#include "Baseobj.h"
#include "Config.h"
#include "Log.h"
#include "Spherepatch.h"
#include "Astro.h"
#include <fstream>

#include <stdio.h>

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern Camera *g_camera;
extern TextureManager2 *g_texmanager2;
extern DWORD g_vtxcount;
extern TimeData td;
extern char DBG_MSG[256];

static Mesh mshSurf1;
const double CheckLightDT = 2.0;
bool VBase::interpolate_textures = true;

VBase::VBase (const Base *_base): VObject (_base), base(_base)
{
	CheckLightT = td.SimT0-1.0;
	BlinkT = 0.0;
	lght = *scene->GetLight();
	std_ambient = g_pOrbiter->Cfg()->AmbientColour;
	enable_shadows = g_pOrbiter->Cfg()->CfgVisualPrm.bShadows;
	have_shadows = false;
	surftile_alpha = false;
	mod_sunlight = false;
	padlight_on = false;
	//mesh_us = mesh_os = NULL;

	sundir = base->SunDirection ();  // direction of sun in local coords
	csun = -10;
	lights = false;

	DWORD i, j, nvtx, nidx, npad = 0;

	nrender_pre = nrender_post = nrender_shdw = 0;
	shadow.nvtx = shadow.nidx = 0;

	// set up exported object meshes
	base->ExportBaseStructures (&mesh_us, &nmesh_us, &mesh_os, &nmesh_os);

	// set up self-rendering objects and exported shadows
	for (i = 0; i < base->nobj; i++) {
		BaseObject *bo = base->obj[i];
		bo->Activate ();
		DWORD spec = bo->GetSpecs();
		if (spec & OBJSPEC_RENDERBEFORESHADOW)
			nrender_pre++;
		if (spec & OBJSPEC_RENDERAFTERSHADOW)
			nrender_post++;
		if (enable_shadows) {
			if (spec & OBJSPEC_EXPORTSHADOW) {
				if (bo->GetShadowSpec (nvtx, nidx)) {
					shadow.nvtx += nvtx;
					shadow.nidx += nidx;
				}
			}
			if (spec & OBJSPEC_RENDERSHADOW)
				nrender_shdw++;
		}
	}
	if (shadow.nvtx) {
		shadow.vtx = new VERTEX_XYZ[shadow.nvtx]; TRACENEW
		shadow.idx = new WORD[shadow.nidx]; TRACENEW
		shadow.nvtx = 0;
		shadow.nidx = 0;
	}
	if (nrender_pre) {
		render_pre = new BaseObject*[nrender_pre]; TRACENEW
		nrender_pre = 0;
	}
	if (nrender_post) {
		render_post = new BaseObject*[nrender_post]; TRACENEW
		nrender_post = 0;
	}
	if (nrender_shdw) {
		render_shdw = new BaseObject*[nrender_shdw]; TRACENEW
		nrender_shdw = 0;
	}
	// assemble self-rendering objects and self-rendering/exported shadows from objects
	for (i = 0; i < base->nobj; i++) {
		BaseObject *bo = base->obj[i];
		DWORD spec = bo->GetSpecs();
		if (spec & OBJSPEC_RENDERBEFORESHADOW)
			render_pre[nrender_pre++] = bo;
		if (spec & OBJSPEC_RENDERAFTERSHADOW)
			render_post[nrender_post++] = bo;
		if (enable_shadows) {
			if (spec & OBJSPEC_EXPORTSHADOW) {
				bo->GetShadowSpec (nvtx, nidx);
				bo->ExportShadow (shadow.vtx+shadow.nvtx, shadow.idx+shadow.nidx);
				WORD nv = (WORD)shadow.nvtx;
				for (j = 0; j < nidx; j++) shadow.idx[shadow.nidx+j] += nv; // offset indices
				shadow.nvtx += nvtx;
				shadow.nidx += nidx;
			}
			if (spec & OBJSPEC_RENDERSHADOW)
				render_shdw[nrender_shdw++] = bo;
		}
	}

	// construct the meshes required for structure shadow projections
	SetupShadowMeshes ();

	if (enable_shadows) // disable shadow flag if there is nothing to render
		enable_shadows = shadow.nvtx || nrender_shdw || nshmesh;

	// generate surface tiles
	const SurftileSpec *tspec;
	nsurftile = base->GetTileList (&tspec);
	if (nsurftile) {

		//char cbuf[256];
		static DWORD nlat[10] = {128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536};
		float cphi = (float)cos (base->lng), sphi = (float)sin (base->lng);
		float ctht = (float)cos (base->lat), stht = (float)sin (base->lat);
		float rad  = (float)base->rad;

		D3DMATRIX R = {(float)base->s0->R.m11, (float)base->s0->R.m12, (float)base->s0->R.m13, 0,
			           (float)base->s0->R.m21, (float)base->s0->R.m22, (float)base->s0->R.m23, 0,
					   (float)base->s0->R.m31, (float)base->s0->R.m32, (float)base->s0->R.m33, 0,
					   0,                     0,                      0,                    1};

		tile = new SurfTile[nsurftile]; TRACENEW
		for (i = 0; i < nsurftile; i++) {
			tile[i].mesh = (Mesh*)tspec[i].mesh;
			tile[i].dtex = (LPDIRECTDRAWSURFACE7)tspec[i].tex;
			if (tile[i].dtex && (base->tile[i].texflag & 2))
				surftile_alpha = true;
		}
	}
	Update (true, true);
}

VBase::~VBase ()
{
	DWORD i;

	for (i = 0; i < base->nobj; i++) {
		BaseObject *bo = base->obj[i];
		bo->Deactivate ();
	}
	//if (mesh_us) delete mesh_us;
	//if (mesh_os) delete mesh_os;

	if (shadow.nvtx)  delete []shadow.vtx;
	if (shadow.nidx)  delete []shadow.idx;

	if (nrender_pre)  delete []render_pre;
	if (nrender_post) delete []render_post;
	if (nrender_shdw) delete []render_shdw;

	if (nsurftile)    delete []tile;

	if (nshmesh) {
		for (i = 0; i < nshmesh; i++) {
			shmesh[i].vbuf->Release();
			delete []shmesh[i].idx;
		}
		delete []shmesh;
	}
}

void VBase::SetupShadowMeshes ()
{
	nshmesh = 0;

	DWORD i, j, k, m, nmesh, ngrp, nssh;
	Mesh **ssh;
	double *ecorr;
	base->ExportShadowGeometry (&ssh, &ecorr, &nssh);
	if (!nssh) return;

	// Re-assemble meshes according to surface elevation correction heights.
	// All objects with similar corrections can use the same transformation
	// matrix and can therefore be merged for improved performance.
	// This only works for static meshes. Any dynamically animated meshes
	// should be stored separately.
	struct EGROUP {
		Mesh **mesh;
		DWORD nmesh, nvtx, nidx;
		int bin;
	} *eg;
	const double d_ecorr = 0.2; // correction bin width
	for (i = 0; i < nssh; i++) {
		int bin = (int)(ecorr[i]/d_ecorr);
		for (j = 0; j < nshmesh; j++)
			if (bin == eg[j].bin) break;
		if (j == nshmesh) {   // create new bin
			EGROUP *tmp = new EGROUP[nshmesh+1]; TRACENEW
			if (nshmesh) {
				memcpy (tmp, eg, nshmesh*sizeof(EGROUP));
				delete []eg;
			}
			eg = tmp;
			eg[nshmesh].nmesh = eg[nshmesh].nvtx = eg[nshmesh].nidx = 0;
			eg[nshmesh].bin = bin;
			nshmesh++;
		}
		nmesh = eg[j].nmesh;
		Mesh **tmp = new Mesh*[nmesh+1]; TRACENEW
		if (nmesh) {
			memcpy (tmp, eg[j].mesh, nmesh*sizeof(Mesh*));
			delete []eg[j].mesh;
		}
		eg[j].mesh = tmp;
		eg[j].mesh[nmesh] = ssh[i];
		ngrp = oapiMeshGroupCount (ssh[i]);
		for (k = 0; k < ngrp; k++) {
			GroupSpec *grp = ssh[i]->GetGroup (k);
			if (grp->UsrFlag & 1) continue; // "no shadows" flag
			eg[j].nvtx += grp->nVtx;
			eg[j].nidx += grp->nIdx;
		}
		eg[j].nmesh++;
	}

	shmesh = new ShadowMesh[nshmesh]; TRACENEW
	LPDIRECT3D7 d3d = gc->GetDirect3D7();
	D3DVERTEXBUFFERDESC vbd = {
		sizeof(D3DVERTEXBUFFERDESC), 
		gc->isTLDevice() ? (DWORD)0 : (DWORD)D3DVBCAPS_SYSTEMMEMORY,
		(DWORD)D3DFVF_XYZ, (DWORD)0
	};
	VERTEX_XYZ *vtx;
	for (i = 0; i < nshmesh; i++) {
		vbd.dwNumVertices = eg[i].nvtx;
		d3d->CreateVertexBuffer (&vbd, &shmesh[i].vbuf, 0);
		shmesh[i].vbuf->Lock (DDLOCK_WAIT | DDLOCK_WRITEONLY | DDLOCK_DISCARDCONTENTS, (LPVOID*)&vtx, NULL);
		shmesh[i].idx = new WORD[eg[i].nidx]; TRACENEW
		shmesh[i].nvtx = 0;
		shmesh[i].nidx = 0;
		shmesh[i].ecorr = (eg[i].bin-0.5)*d_ecorr;
		for (j = 0; j < eg[i].nmesh; j++) {
			Mesh *mesh = eg[i].mesh[j];
			ngrp = mesh->nGroup();
			for (k = 0; k < ngrp; k++) {
				GroupSpec *grp = mesh->GetGroup (k);
				if (grp->UsrFlag & 1) continue; // "no shadows" flag
				VERTEX_XYZ *vtgt = vtx + shmesh[i].nvtx;
				WORD *itgt = shmesh[i].idx + shmesh[i].nidx;
				NTVERTEX *vsrc = grp->Vtx;
				WORD *isrc = grp->Idx;
				WORD iofs = (WORD)shmesh[i].nvtx;
				for (m = 0; m < grp->nVtx; m++) {
					vtgt[m].x = vsrc[m].x;
					vtgt[m].y = vsrc[m].y;
					vtgt[m].z = vsrc[m].z;
				}
				for (m = 0; m < grp->nIdx; m++)
					*itgt++ = *isrc++ + iofs;
				shmesh[i].nvtx += grp->nVtx;
				shmesh[i].nidx += grp->nIdx;
			}
		}
		shmesh[i].vbuf->Unlock();
	}

	for (i = 0; i < nshmesh; i++)
		delete []eg[i].mesh;
	delete []eg;

	//for (i = 0; i < nssh; i++)
	//	delete ssh[i];
}

const double lightson = RAD*1.0; // sun position at which lights are switched on

void VBase::Update (bool moving, bool force)
{
	DWORD i;

	VObject::Update (moving, force);
	if (!moving && !force) return;

	if (td.SimT0 > CheckLightT)
		mod_sunlight = ModLighting (&lght);

	sundir = *base->SunDirectionBuffered();
	if (sundir.y != csun) {
		csun = sundir.y;                 // cosine of sun over horizon
		if (have_shadows = (enable_shadows && csun > 0.07)) {
			Vector shdir = sundir/(-csun);
			double az = atan2 (shdir.z, shdir.x);
			for (DWORD i = 0; i < base->nobj; i++)
				base->obj[i]->UpdateShadow (shdir, az);
			shadowstrength = (float)min (1, (csun-0.07)/0.015);
		}
		bool night = csun < lightson;
		if (lights != night) {
			for (i = 0; i < nmesh_us; i++)
				mesh_us[i]->SetTexMixture (1, night ? 1.0f:0.0f);
			for (i = 0; i < nmesh_os; i++)
				mesh_os[i]->SetTexMixture (1, night ? 1.0f:0.0f);
			lights = night;
		}
	}

	// update base objects who require it
	for (i = 0; i < base->nobj; i++)
		if (base->obj[i]->GetSpecs() & OBJSPEC_UPDATEVERTEX)
			base->obj[i]->Update ();

	// blink landing lights
	if (td.SimT0 > BlinkT) {
		padlight_on = !padlight_on;
		BlinkT = td.SimT0+1.0;
	}
}

void VBase::Timejump (bool moving)
{
	CheckLightT = td.SimT0-1.0;
	base->SunDirection(); // force update
	Update (moving, true);
}

void VBase::CheckResolution (double iar)
{
}

void VBase::Render (LPDIRECT3DDEVICE7 dev)
{
	RenderSurfaceTiles (dev);
	RenderSurfaceDecals (dev);
	RenderShadows (dev);
	RenderStructures (dev);
}

void VBase::RenderSurfaceTiles (LPDIRECT3DDEVICE7 dev)
{
#ifdef _DEBUG
	// check expected render states on input:
	// D3DRENDERSTATE_ALPHABLENDENABLE == TRUE
	// D3DRENDERSTATE_ZENABLE == FALSE
	// D3DTSS_ADDRESS(0) == D3DTADDRESS_WRAP
	DWORD val;
	dev->GetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, &val);
	dCHECK(val == TRUE, "LPDIRECT3DDEVICE7::GetRenderState: expected return TRUE")
	dev->GetRenderState(D3DRENDERSTATE_ZENABLE, &val);
	dCHECK(val == FALSE, "LPDIRECT3DDEVICE7::GetRenderState: expected return FALSE")
	dev->GetTextureStageState (0, D3DTSS_ADDRESS, &val);
	dCHECK(val == D3DTADDRESS_WRAP, "LPDIRECT3DDEVICE7::GetTextureStageState: expected return D3DTADDRESS_WRAP")
#endif

	if (nsurftile) {
		DWORD i;
		bool modlight = false;
		dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
		if (mod_sunlight && !modlight)
			dev->SetLight (0, &lght), modlight = true;
		dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);
		for (i = 0; i < nsurftile; i++) {
			dev->SetTexture (0, tile[i].dtex);
			tile[i].mesh->Render (dev);
		}
		dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);

		if (modlight)   // restore lighting
			dev->SetLight (0, scene->GetLight());
	}
}

void VBase::RenderSurfaceDecals (LPDIRECT3DDEVICE7 dev)
{
	// we render base objects only if the apparent size of the object scale
	// parameter is > 1 pixel
	if (iapprad * base->Size() / base->objscale < 1.0) {

		DWORD i, texwrap;
		bool modlight = false;
		if (mod_sunlight && !modlight)
			dev->SetLight (0, &lght), modlight = true;
		if (!day) {
			dev->GetTextureStageState (1, D3DTSS_ADDRESS, &texwrap);
			dev->SetTextureStageState (1, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
		}
		dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);

		// 1: render exported groups before shadows
		for (i = 0; i < nmesh_us; i++) mesh_us[i]->Render (dev);
#ifdef UNDEF
		bool lightsactive = false;
		for (i = 0; i < ngrp; i++) {
			if (!grp[i].undershadow) continue;
			dev->SetTexture (0, grp[i].daytex);
			bool usenighttex = (!day && grp[i].nighttex != 0);
			if (usenighttex) {
				dev->SetTexture (1, grp[i].nighttex);
				if (!lightsactive) {
					dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_ADD);
					//dev->SetTextureStageState (1, D3DTSS_TEXCOORDINDEX, 0);
					lightsactive = true;
				}
			} else if (lightsactive) {
				dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
				lightsactive = false;
			}
			dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
				grp[i].vtx, grp[i].nvtx, grp[i].idx, grp[i].nidx, 0);
		}
		if (lightsactive)
			dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
#endif
		if (!day) {
			dev->SetTexture (1, 0);
			dev->SetTextureStageState (1, D3DTSS_ADDRESS, texwrap);
		}

		// 2: render self-rendering objects before shadows
		for (i = 0; i < nrender_pre; i++) render_pre[i]->Render (dev, day);

		if (modlight)   // restore lighting
			dev->SetLight (0, scene->GetLight());
	}
}

void VBase::RenderShadows (LPDIRECT3DDEVICE7 dev)
{
#ifdef _DEBUG
	// check expected render states on input:
	// D3DRENDERSTATE_ALPHABLENDENABLE == TRUE
	// D3DRENDERSTATE_ZENABLE == FALSE
	// Expected device render state on input:
	// ALPHABLENDENABLE=TRUE
	DWORD val;
	dev->GetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, &val);
	dCHECK(val == TRUE, "LPDIRECT3DDEVICE7::GetRenderState: expected return TRUE");
	dev->GetRenderState(D3DRENDERSTATE_ZENABLE, &val);
	dCHECK(val == FALSE, "LPDIRECT3DDEVICE7::GetRenderState: expected return FALSE");
#endif

	// we render base objects only if the apparent size of the object scale
	// parameter is > 1 pixel
	if (have_shadows && iapprad * base->Size() / base->objscale < 1.0) {

		// modify shadow alpha during dawn and dusk
		DWORD tfactor, tfactor_mod;
		dev->GetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, &tfactor);
		tfactor_mod = tfactor;
		if (gc->GetStencilDepth() > 0) {
			if (shadowstrength < 1) {
				float modalpha = shadowstrength*RGBA_GETALPHA(tfactor)/256.0f;
				dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, tfactor_mod = D3DRGBA(0,0,0,modalpha));
			}
		}

		// render exported and self-rendering shadows
		dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);
		if (shadow.nvtx) {
			dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, D3DFVF_XYZ,
				shadow.vtx, shadow.nvtx, shadow.idx, shadow.nidx, 0);
			g_vtxcount += shadow.nvtx;
		}
		for (DWORD i = 0; i < nrender_shdw; i++)
			render_shdw[i]->RenderShadow (dev);

		// render internally maintained shadows
		if (nshmesh) {
			double d, nr0;
			const Planet *planet = base->RefPlanet();
			Vector pp = planet->GPos();              // planet global pos
			Vector sd = base->GPos();                // base global pos
			Vector pvr = sd-pp;                      // planet-relative base position
			d = pvr.length();                        // planet radius at base location
			sd.unify();                              // shadow projection direction

			//double fac1 = dotp (sd, pvr);
			//if (fac1 > 0.0)                          // base is on planet night-side
			//	return;

			Matrix vR = base->GRot();
			Vector sdv = tmul (vR, sd);              // projection direction in base frame
			Vector hnp = pvr.unit();
			Vector hn = tmul (vR, hnp);              // horizon normal in vessel frame

			// perform projections
			double nd = dotp (hn, sdv);
			Vector sdvs = sdv / nd;
			if (!sdvs.y) return; // required for plane offset correction

			DWORD i;

			// build shadow projection matrix
			D3DMATRIX mProj, mProjWorld, mProjWorldShift;
			mProj._11 = (float)(1.0 - sdvs.x*hn.x);
			mProj._12 = (float)(    - sdvs.y*hn.x);
			mProj._13 = (float)(    - sdvs.z*hn.x);
			mProj._14 = 0.0f;
			mProj._21 = (float)(    - sdvs.x*hn.y);
			mProj._22 = (float)(1.0 - sdvs.y*hn.y);
			mProj._23 = (float)(    - sdvs.z*hn.y);
			mProj._24 = 0.0f;
			mProj._31 = (float)(    - sdvs.x*hn.z);
			mProj._32 = (float)(    - sdvs.y*hn.z);
			mProj._33 = (float)(1.0 - sdvs.z*hn.z);
			mProj._34 = 0.0f;
			mProj._41 = 0.0f;
			mProj._42 = 0.0f;
			mProj._43 = 0.0f;
			mProj._44 = 1.0f;
			D3DMath_MatrixMultiply (mProjWorld, mWorld, mProj);
			memcpy (&mProjWorldShift, &mProjWorld, sizeof(D3DMATRIX));

			dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mProjWorld);
			for (i = 0; i < nshmesh; i++) {

				// add shadow plane offset to transformation
				nr0 = shmesh[i].ecorr/sdvs.y;
				mProjWorldShift._41 = mProjWorld._41 + (float)(nr0*(sdvs.x*mWorld._11 + sdvs.y*mWorld._21 + sdvs.z*mWorld._31));
				mProjWorldShift._42 = mProjWorld._42 + (float)(nr0*(sdvs.x*mWorld._12 + sdvs.y*mWorld._22 + sdvs.z*mWorld._32));
				mProjWorldShift._43 = mProjWorld._43 + (float)(nr0*(sdvs.x*mWorld._13 + sdvs.y*mWorld._23 + sdvs.z*mWorld._33));
				dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mProjWorldShift);

				dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, shmesh[i].vbuf, 0, shmesh[i].nvtx, shmesh[i].idx, shmesh[i].nidx, 0);
				g_vtxcount += shmesh[i].nvtx;
			}
		}

		if (tfactor != tfactor_mod)
			dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, tfactor);
	}
}

void VBase::RenderStructures (LPDIRECT3DDEVICE7 dev)
{
	// we render base objects only if the apparent size of the object scale
	// parameter is > 1 pixel
	if (iapprad * base->Size() / base->objscale > 1.0) return;

	DWORD i, j, texwrap;
	bool day = csun > lightson;

	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mWorld);

	// switch to mipmap filters without interpolation
	if (!interpolate_textures) {
		dev->SetTextureStageState (0, D3DTSS_MIPFILTER, D3DTFP_POINT);
		dev->SetTextureStageState (0, D3DTSS_MINFILTER, D3DTFN_POINT);
		dev->SetTextureStageState (0, D3DTSS_MAGFILTER, D3DTFG_POINT);
	}

	if (mod_sunlight)   // modify lighting
		dev->SetLight (0, &lght);
	if (!day) {
		dev->GetTextureStageState (1, D3DTSS_ADDRESS, &texwrap);
		dev->SetTextureStageState (1, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
	}

	DWORD balpha;
	dev->GetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, &balpha);
	if (!balpha)
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);

	// 4: render exported groups after shadows
	for (i = 0; i < nmesh_os; i++) mesh_os[i]->Render (dev);

#ifdef UNDEF
	bool lightsactive = false;
	for (i = 0; i < ngrp; i++) {
		if (grp[i].undershadow) continue;
		dev->SetTexture (0, grp[i].daytex);
		bool usenighttex = (!day && grp[i].nighttex != 0);
		if (usenighttex) {
			dev->SetTexture (1, grp[i].nighttex);
			if (!lightsactive) {
				dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_ADD);
				lightsactive = true;
			}
		} else if (lightsactive) {
			dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
			lightsactive = false;
		}
		dev->DrawIndexedPrimitive (D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
			grp[i].vtx, grp[i].nvtx, grp[i].idx, grp[i].nidx, 0);
	}
	if (lightsactive)
		dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
#endif
	if (!day) {
		dev->SetTexture (1, 0);
		dev->SetTextureStageState (1, D3DTSS_ADDRESS, texwrap);
	}

	// 5: render self-rendering objects after shadows
	for (i = 0; i < nrender_post; i++) render_post[i]->Render (dev, day);

	// switch filter back to default linear
	if (!balpha)
		dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);

	if (!interpolate_textures) {
		dev->SetTextureStageState (0, D3DTSS_MIPFILTER, D3DTFP_NONE);
		dev->SetTextureStageState (0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
		dev->SetTextureStageState (0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
	}

	if (mod_sunlight)   // restore lighting
		dev->SetLight (0, scene->GetLight());

	// landing lights
	if (padlight_on) {
		for (i = 0; i < base->nPad(); i++) {
			if (base->PadStatus(i)->status == 2) {
				static float x[12] = {-37,-37,-28, -9,9,0, 37,37,28, 9,-9,0};
				static float z[12] = {-9,9,0, 37,37,28, 9,-9,0, -37,-37,-28};
				float dx = (float)base->PadStatus(i)->relpos.x;
				float dz = (float)base->PadStatus(i)->relpos.z;
				struct { float x; float y; float z; DWORD col; } vtx[12];
				for (j = 0; j < 12; j++) {
					vtx[j].x = x[j] + dx;
					vtx[j].z = z[j] + dz;
					vtx[j].y = 0.0;
					vtx[j].col = ((j+1)%3 ? 0x0060ff00 : 0x00a0ff40);
				}
				dev->SetTexture (0,0);
				dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 10);
				dev->SetRenderState (D3DRENDERSTATE_LIGHTING, FALSE);
				dev->DrawPrimitive (D3DPT_TRIANGLELIST, D3DFVF_XYZ | D3DFVF_DIFFUSE , vtx, 12, NULL);
				dev->SetRenderState (D3DRENDERSTATE_LIGHTING, TRUE);
				dev->SetRenderState (D3DRENDERSTATE_ZBIAS, 0);
			}
		}
	}
}

void VBase::RenderGroundShadow (LPDIRECT3DDEVICE7 dev)
{
	if (!nshmesh) return; // nothing to do

	static const double shadow_elev_limit = 0.07;
	double d, csun, nr0;
	const Planet *planet = base->RefPlanet();
	Vector pp = planet->GPos();              // planet global pos
	Vector sd = base->GPos();                // base global pos
	Vector pvr = sd-pp;                      // planet-relative base position
	d = pvr.length();                        // planet radius at base location
	sd.unify();                              // shadow projection direction

	double fac1 = dotp (sd, pvr);
	if (fac1 > 0.0)                          // base is on planet night-side
		return;
	csun = -fac1/d;                          // sun elevation above horizon
	if (csun < shadow_elev_limit)            // sun too low to cast shadow
		return;

	Matrix vR = base->GRot();
	Vector sdv = tmul (vR, sd);              // projection direction in base frame
	Vector hnp = pvr.unit();
	Vector hn = tmul (vR, hnp);              // horizon normal in vessel frame

	// perform projections
	double nd = dotp (hn, sdv);
	Vector sdvs = sdv / nd;
	if (!sdvs.y) return; // required for plane offset correction

	DWORD i;

	// build shadow projection matrix
	D3DMATRIX mProj, mProjWorld, mProjWorldShift;
	mProj._11 = (float)(1.0 - sdvs.x*hn.x);
	mProj._12 = (float)(    - sdvs.y*hn.x);
	mProj._13 = (float)(    - sdvs.z*hn.x);
	mProj._14 = 0.0f;
	mProj._21 = (float)(    - sdvs.x*hn.y);
	mProj._22 = (float)(1.0 - sdvs.y*hn.y);
	mProj._23 = (float)(    - sdvs.z*hn.y);
	mProj._24 = 0.0f;
	mProj._31 = (float)(    - sdvs.x*hn.z);
	mProj._32 = (float)(    - sdvs.y*hn.z);
	mProj._33 = (float)(1.0 - sdvs.z*hn.z);
	mProj._34 = 0.0f;
	mProj._41 = 0.0f;
	mProj._42 = 0.0f;
	mProj._43 = 0.0f;
	mProj._44 = 1.0f;
	D3DMath_MatrixMultiply (mProjWorld, mWorld, mProj);
	memcpy (&mProjWorldShift, &mProjWorld, sizeof(D3DMATRIX));

	// modify depth of shadows at dawn/dusk
	DWORD tfactor;
	bool resetalpha = false;
	if (gc->GetStencilDepth() > 0) {
		double scale = min (1, (csun-0.07)/0.015);
		if (scale < 1) {
			dev->GetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, &tfactor);
			float modalpha = (float)(scale*RGBA_GETALPHA(tfactor)/256.0);
			dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0,0,modalpha));
			resetalpha = true;
		}
	}

	dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mProjWorld);
	for (i = 0; i < nshmesh; i++) {

		// add shadow plane offset to transformation
		nr0 = shmesh[i].ecorr/sdvs.y;
		mProjWorldShift._41 = mProjWorld._41 + (float)(nr0*(sdvs.x*mWorld._11 + sdvs.y*mWorld._21 + sdvs.z*mWorld._31));
		mProjWorldShift._42 = mProjWorld._42 + (float)(nr0*(sdvs.x*mWorld._12 + sdvs.y*mWorld._22 + sdvs.z*mWorld._32));
		mProjWorldShift._43 = mProjWorld._43 + (float)(nr0*(sdvs.x*mWorld._13 + sdvs.y*mWorld._23 + sdvs.z*mWorld._33));
		dev->SetTransform (D3DTRANSFORMSTATE_WORLD, &mProjWorldShift);

		dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, shmesh[i].vbuf, 0, shmesh[i].nvtx, shmesh[i].idx, shmesh[i].nidx, 0);
		g_vtxcount += shmesh[i].nvtx;
	}
}

bool VBase::ModLighting (LPD3DLIGHT7 light)
{
	const CelestialBody *cb = base->RefPlanet();
	Star *sun = g_psys->GetStar(0); // should really loop over all suns
	Vector S(sun->GPos() - base->GPos());
	double s = S.length();
	double as = asin (sun->Size()/s);                     // apparent size of sun disc [rad]
	Vector lcol (1,1,1);
	double amb = 0;
	bool lightmod = false;
	int j;

	// calculate shadowing by planet

	Vector P(cb->GPos() - base->GPos());
	double p = P.length();
	double phi = acos (dotp(S,P)/(s*p));          // angular distance between sun and planet
	static const double ap = PI05;                // apparent size of planet disc [rad]

	if (cb->Type() == OBJTP_PLANET && ((Planet*)cb)->HasAtmosphere()) { // case 1: planet has atmosphere

		const ATMCONST *atm = ((Planet*)cb)->AtmParams();
		double ap1 = RAD*100.0;
		// This is the angular separation between planet centre and star below which
		// the atmosphere affects lighting when on the planet surface. (100: when sun
		// is 10 deg above horizon). Should possibly be made atmosphere-specific.

		if (as+ap1 >= phi) {                      // overlap
			double dap = ap1-ap;
			Vector plight(1,1,1);
			if (phi < ap-as) {                // totality (sun below horizon)
				plight.Set(0,0,0);
			} else {
				double dispersion = max (0.02, min (0.9, log (atm->rho0+1.0)));
				double r0 = 1.0-0.40*dispersion;
				double g0 = 1.0-/*0.75*/0.65*dispersion;
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
			}
			for	(j = 0; j < 3; j++) lcol.data[j] = min (lcol.data[j], plight.data[j]);
			lightmod = true;
		}

		// modification of ambient lighting
		double sunelev = phi-ap;
		if (sunelev > - 14.0*RAD) {
			double amb0 = min (0.7, log (atm->rho0+1.0)*0.4);
			amb = amb0 * min (1.0, (sunelev+14.0*RAD)/(20.0*RAD));
			if (!lightmod) lightmod = (amb > 0.05);
			amb = max (0, amb-0.05);
			// reduce direct light component to avoid overexposure
			lcol *= 1.0-amb*0.5;
		}

	} else {                                                            // case 2: planet has no atmosphere

		if (phi < as+ap) {                        // overlap
			double lfrac = (phi <= ap-as ? 0.0 : (phi+as-ap)/(2.0*as));
			for (j = 0; j < 3; j++) lcol.data[j] = min (lcol.data[j], lfrac);
			lightmod = true;
		}

	}

	if (lightmod) {
		D3DCOLORVALUE starcol = ColorToD3D(sun->GetLightColor());
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

	CheckLightT = td.SimT0 + max (1.0, 50.0*(sqrt(fabs(phi-Pi05)))); // next test
	return lightmod;
}
