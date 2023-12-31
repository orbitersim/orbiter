// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// SurfMgr.cpp
// class SurfaceManager (implementation)
//
// Planetary surface rendering management, including a simple
// LOD (level-of-detail) algorithm for surface patch resolution.
// ==============================================================

#include "SurfMgr.h"
#include "VPlanet.h"
#include "Texture.h"
#include <algorithm>

using std::min;
using namespace oapi;

int nrender[15]; // temporary

// =======================================================================

SurfaceManager::SurfaceManager (const D3D7Client *gclient, const vPlanet *vplanet)
: TileManager (gclient, vplanet)
{
	maxlvl = min (*(int*)gc->GetConfigParam (CFGPRM_SURFACEMAXLEVEL),        // global setting
	              *(int*)oapiGetObjectParam (obj, OBJPRM_PLANET_SURFACEMAXLEVEL)); // planet-specific setting
	maxbaselvl = min (8, maxlvl);
	pcdir = {1,0,0};
	lightfac = *(double*)gc->GetConfigParam (CFGPRM_SURFACELIGHTBRT);
	spec_base = 0.95f;
	atmc = oapiGetPlanetAtmConstants (obj);

	int maxidx = patchidx[maxbaselvl];
	tiledesc = new TILEDESC[maxidx];
	memset (tiledesc, 0, maxidx*sizeof(TILEDESC));

	LoadPatchData ();
	LoadTileData ();
	LoadTextures ();
	LoadSpecularMasks ();
}

// =======================================================================

void SurfaceManager::SetMicrotexture (const char *fname)
{
	TileManager::SetMicrotexture (fname);
	spec_base = (microtex ? 1.05f : 0.95f); // increase specular intensity to compensate for "ripple" losses
}

// =======================================================================

void SurfaceManager::Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double scale, int level, double viewap, bool bfog)
{
	memset(nrender,0,15*sizeof(int)); // temporary

	dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP);

	// modify colour of specular reflection component
	if (bGlobalSpecular) {
		extern D3DMATERIAL7 watermat;
		SpecularColour (&watermat.specular);
		watermat.power = (microtex ? 20.0f : 25.0f);
	}

	TileManager::Render (dev, wmat, scale, level, viewap, bfog);

	dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);

	// temporary
	//if (RenderParam.tgtlvl >= 10)
	//	sprintf (oapiDebugString(), "Tiles rendered: %d (14), %d (13), %d (12), %d (11), %d (10), %d (9) %d (8)", nrender[14], nrender[13], nrender[12], nrender[11], nrender[10], nrender[9], nrender[8]);
}

// =======================================================================

void SurfaceManager::RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, double sdist,
	TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag)
{
	extern D3DMATERIAL7 pmat;
	extern D3DMATERIAL7 watermat;
	LPDIRECT3DVERTEXBUFFER7 vb;        // processed vertex buffer
	VBMESH &mesh = PATCH_TPL[lvl][ilat]; // patch template

	if (range.tumin == 0 && range.tumax == 1) {
		vb = mesh.vb; // use vertex buffer directly
	} else {
		if (!tile->vtx) {
			D3DVERTEXBUFFERDESC vbd = 
				{ sizeof(D3DVERTEXBUFFERDESC), vbMemCaps | D3DVBCAPS_WRITEONLY, FVF_2TEX, mesh.nv };
			gc->GetDirect3D7()->CreateVertexBuffer (&vbd, &tile->vtx, 0);
			ApplyPatchTextureCoordinates (mesh, tile->vtx, range);
			tile->vtx->Optimize (RenderParam.dev, 0); // no more change, so we can optimize
		}
		vb = tile->vtx; // use buffer with transformed texture coords
	}

	bool hasspec = ((flag & 2) == 2);
	bool purespec = ((flag & 3) == 2);
	bool mixedspec = ((flag & 3) == 3);
	bool spec_singlerender = (purespec && !microtex);
	bool lights = ((flag & 4) && (sdist > 1.4));

	// step 1: render full patch diffusely
	RenderParam.dev->SetTexture (0, tex);
	RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
		mesh.nv, mesh.idx, mesh.ni, 0);

	// if there is no specularity and no emissive lights, we are done
	if (!hasspec && !lights) return;

	// disable fog on additional render passes
	if (RenderParam.bfog) {
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_FOGENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_NONE);
	}

	// step 2: add city lights
	// note: I didn't find a way to include this as a texture stage in the
	// previous pass, because the lights need to be multiplied with a factor before
	// adding
	if (lights) {
		double fac = lightfac;
		if (sdist < 1.9) fac *= (sdist-1.4)/(1.9-1.4);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(fac,fac,fac,1));
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG2, D3DTA_TFACTOR);
		RenderParam.dev->SetTexture (0, ltex);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_ONE);

		RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
			mesh.nv, mesh.idx, mesh.ni, 0);

		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG2, D3DTA_CURRENT);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
	}

	// step 3: add specular highlights (mixed patches only)
	if (mixedspec) {

		RenderParam.dev->GetMaterial (&pmat);
		RenderParam.dev->SetMaterial (&watermat);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_INVSRCALPHA);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		RenderParam.dev->SetTexture (0, ltex);

		if (microtex) {
			RenderParam.dev->SetTexture (1, microtex);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_CURRENT);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_ADD/*MODULATE*/);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_TEXCOORDINDEX, 1);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
		}

		RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
			mesh.nv, mesh.idx, mesh.ni, 0);

		if (microtex) {
			RenderParam.dev->SetTexture (1, 0);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_COLOROP, D3DTOP_DISABLE);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_COLORARG1, D3DTA_TEXTURE);
			RenderParam.dev->SetTextureStageState (1, D3DTSS_TEXCOORDINDEX, 0);
		}

		RenderParam.dev->SetMaterial (&pmat);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);

	} else if (purespec) {

		RenderParam.dev->GetMaterial (&pmat);
		RenderParam.dev->SetMaterial (&watermat);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TFACTOR);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_TEXTUREFACTOR, D3DRGBA(0,0,0,1));
		if (microtex) {
			RenderParam.dev->SetTexture (0, microtex);
			RenderParam.dev->SetTextureStageState (0, D3DTSS_TEXCOORDINDEX, 1);
			RenderParam.dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP);
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_INVSRCALPHA);
		} else {
			RenderParam.dev->SetTexture (0, 0);
		}

		RenderParam.dev->DrawIndexedPrimitiveVB (D3DPT_TRIANGLELIST, vb, 0,
			mesh.nv, mesh.idx, mesh.ni, 0);

		if (microtex) {
			RenderParam.dev->SetTextureStageState (0, D3DTSS_TEXCOORDINDEX, 0);
			RenderParam.dev->SetTextureStageState (0, D3DTSS_ADDRESS, D3DTADDRESS_CLAMP); // the default for planet surfaces
			RenderParam.dev->SetRenderState (D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA);
		}
		RenderParam.dev->SetMaterial (&pmat);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLOROP, D3DTOP_MODULATE);
		RenderParam.dev->SetTextureStageState (0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_SPECULARENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_ALPHABLENDENABLE, FALSE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);

	}

	if (RenderParam.bfog) {
		// turn fog back on
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_FOGENABLE, TRUE);
		RenderParam.dev->SetRenderState (D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_EXP);
	}
}
