// ==============================================================
// SurfMgr.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 - 2016 Martin Schweiger
//				 2011 - 2016 Jarmo Nikkanen (D3D9Client modification)  
// ==============================================================

// ==============================================================
// class SurfaceManager (implementation)
//
// Planetary surface rendering management, including a simple
// LOD (level-of-detail) algorithm for surface patch resolution.
// ==============================================================

#include "SurfMgr.h"
#include "VPlanet.h"
#include "Texture.h"
#include "D3D9Surface.h"
#include "DebugControls.h"

using namespace oapi;

D3DMATERIAL9 watermat = {{1,1,1,1},{1,1,1,1},{1,1,1,1},{0,0,0,0},20.0f};
D3DMATERIAL9 def_mat = {{1,1,1,1},{1,1,1,1},{1,1,1,1},{0,0,0,1},0};

int nrender[15]; // temporary

// =======================================================================

SurfaceManager::SurfaceManager (D3D9Client *gclient, const vPlanet *vplanet)
: TileManager(gclient, vplanet)
{

	maxlvl = min (*(int*)gc->GetConfigParam (CFGPRM_SURFACEMAXLEVEL),				// global setting
	              *(int*)oapiGetObjectParam (obj, OBJPRM_PLANET_SURFACEMAXLEVEL));	// planet-specific setting

	maxbaselvl = min(8, maxlvl);

	pcdir = _V(1,0,0);
	lightfac = *(double*)gc->GetConfigParam (CFGPRM_SURFACELIGHTBRT);
	spec_base = 0.95f;
	atmc = oapiGetPlanetAtmConstants (obj);

	int maxidx = patchidx[maxbaselvl];
	tiledesc = new TILEDESC[maxidx];
	memset2 (tiledesc, 0, maxidx*sizeof(TILEDESC));
}

// =======================================================================

void SurfaceManager::LoadData()
{
	if (ntex!=0) return;
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

void SurfaceManager::Render(LPDIRECT3DDEVICE9 dev, D3DXMATRIX &wmat, double scale, int level, double viewap, bool bfog)
{
	if (ntex==0) LoadData();

	// modify colour of specular reflection component
	if (bGlobalSpecular) {
		extern D3DMATERIAL9 watermat;
		SpecularColour (&watermat.Specular);
		watermat.Power = (microtex ? 30.0f : 35.0f);
	}

	TileManager::Render (dev, wmat, scale, level, viewap, bfog);
}

// ==============================================================

void SurfaceManager::RenderSimple(int level, int npatch, TILEDESC *tile, LPD3DXMATRIX mWrld)
{
	// render complete sphere (used at low LOD levels)
	HR(FX->SetTechnique(ePlanetTile));
	HR(FX->SetValue(eSun, gc->GetScene()->GetLight(-1), sizeof(D3D9Light)));
	HR(FX->SetMatrix(eW, mWrld));
	HR(FX->SetValue(eWater, &watermat, sizeof(D3DMATERIAL9)));
	HR(FX->SetValue(eMat, &def_mat, sizeof(D3DMATERIAL9)));
	HR(FX->SetValue(eColor, &D3DXCOLOR(cAmbient), sizeof(D3DXCOLOR)));
	HR(FX->SetFloat(eTime, float(fmod(oapiGetSimTime(),60.0))));
	HR(FX->SetVector(eTexOff, &D3DXVECTOR4(1.0f, 0.0f, 1.0f, 0.0f)));
	HR(FX->SetFloat(eMix, 0.0f));
	HR(FX->SetBool(eDebugHL, false));

	LPDIRECT3DDEVICE9 pDev = gc->GetDevice();
	pDev->SetVertexDeclaration(pPatchVertexDecl);

	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	for (int idx = 0; idx < npatch; idx++) {

		VBMESH &mesh = PATCH_TPL[level][idx]; // patch template

		gc->GetStats()->Vertices += mesh.nv;
		gc->GetStats()->Tiles[level]++;
		gc->GetStats()->Draw++;

		bool purespec = ((tile[idx].flag & 3) == 2);
		bool mixedspec = ((tile[idx].flag & 3) == 3);

		// step 1: render full patch, either completely diffuse or completely specular
		if (purespec) { // completely specular
			HR(FX->SetInt(eSpecularMode, 1));
		}
		else if (mixedspec) {
			HR(FX->SetInt(eSpecularMode, 2));
		}
		else {
			HR(FX->SetInt(eSpecularMode, 0));
		}

		LPDIRECT3DTEXTURE9 ltex = tile[idx].ltex;
		if (ltex==NULL) ltex = gc->GetDefaultTexture()->GetTexture();

		FX->SetTexture(eTex0, tile[idx].tex);
		FX->SetTexture(eTex1, ltex);
		FX->CommitChanges();

		pDev->SetStreamSource(0, mesh.pVB, 0, sizeof(VERTEX_2TEX));
		pDev->SetIndices(mesh.pIB);
		pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh.nv, 0, mesh.nf);
	}

	HR(FX->EndPass());
	HR(FX->End());	
}


void SurfaceManager::InitRenderTile()
{
	HR(FX->SetTechnique(ePlanetTile));
	HR(FX->SetValue(eSun, gc->GetScene()->GetLight(-1), sizeof(D3D9Light)));
	HR(FX->SetValue(eMat, &def_mat, sizeof(D3DMATERIAL9)));
	HR(FX->SetValue(eWater, &watermat, sizeof(D3DMATERIAL9)));
	HR(FX->SetValue(eColor, &D3DXCOLOR(cAmbient), sizeof(D3DXCOLOR)));
	HR(FX->SetFloat(eTime, float(fmod(oapiGetSimTime(),60.0))));
	HR(FX->SetBool(eDebugHL, false));

	LPDIRECT3DDEVICE9 pDev = gc->GetDevice();
	pDev->SetVertexDeclaration(pPatchVertexDecl);

	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
}

void SurfaceManager::EndRenderTile()
{
	HR(FX->EndPass());
	HR(FX->End());		
}


// =======================================================================

void SurfaceManager::RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, double sdist,
	TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag)
{
	LPDIRECT3DDEVICE9 pDev = gc->GetDevice();

	VBMESH &mesh = PATCH_TPL[lvl][ilat]; // patch template
	
	if (range.tumin == 0 && range.tumax == 1) {
		HR(FX->SetVector(eTexOff, &D3DXVECTOR4(1.0f, 0.0f, 1.0f, 0.0f)));
	}
	else {
		float tuscale = range.tumax-range.tumin, tuofs = range.tumin;
		float tvscale = range.tvmax-range.tvmin, tvofs = range.tvmin;
		HR(FX->SetVector(eTexOff, &D3DXVECTOR4(tuscale,tuofs,tvscale,tvofs)));
	}

	DWORD flags = *(DWORD*)gc->GetConfigParam(CFGPRM_GETDEBUGFLAGS);

	if (DebugControls::IsActive()) {
		if (flags&DBG_FLAGS_TILES) {
			float x = 0.6f;
			FX->SetBool(eDebugHL, true);
			switch(lvl) {
				case 14: FX->SetVector(eColor, &D3DXVECTOR4(x, 0, 0, 0)); break;
				case 13: FX->SetVector(eColor, &D3DXVECTOR4(0, x, 0, 0)); break;
				case 12: FX->SetVector(eColor, &D3DXVECTOR4(0, 0, x, 0)); break;
				case 11: FX->SetVector(eColor, &D3DXVECTOR4(x, x, 0, 0)); break;
				case 10: FX->SetVector(eColor, &D3DXVECTOR4(x, 0, x, 0)); break;
				case 9:  FX->SetVector(eColor, &D3DXVECTOR4(0, x, x, 0)); break;
				default: FX->SetVector(eColor, &D3DXVECTOR4(0, 0, 0, 0)); break;
			}
		}
	}

	bool purespec = ((flag & 3) == 2);
	bool mixedspec = ((flag & 3) == 3);
	
	if (ltex==NULL) ltex = gc->GetDefaultTexture()->GetTexture();

	HR(FX->SetMatrix(eW, &mWorld));
	HR(FX->SetTexture(eTex0, tex));	  // Base Texture
	HR(FX->SetTexture(eTex1, ltex));  // Specular Mask and Night Lights
	
	if (microtex) {
		HR(FX->SetTexture(eTex3, microtex->GetTexture())); 
		HR(FX->SetFloat(eMix, 1.0f));
	}
	else HR(FX->SetFloat(eMix, 0.0f));

	if (mixedspec)     FX->SetInt(eSpecularMode, 2);
	else if (purespec) FX->SetInt(eSpecularMode, 1);
	else			   FX->SetInt(eSpecularMode, 0);

	FX->CommitChanges();

	pDev->SetStreamSource(0, mesh.pVB, 0, sizeof(VERTEX_2TEX));
	pDev->SetIndices(mesh.pIB);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh.nv, 0, mesh.nf);
}
