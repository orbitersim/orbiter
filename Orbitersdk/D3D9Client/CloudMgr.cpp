// ==============================================================
// CloudMgr.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 - 2016 Martin Schweiger
// ==============================================================

// ==============================================================
// class CloudManager (implementation)
//
// Planetary rendering management for cloud layers, including a simple
// LOD (level-of-detail) algorithm for patch resolution.
// ==============================================================

#include "CloudMgr.h"
#include "VPlanet.h"
#include "Texture.h"
#include "D3D9Surface.h"

using namespace oapi;

// =======================================================================

CloudManager::CloudManager(D3D9Client *gclient, const vPlanet *vplanet)
: TileManager (gclient, vplanet)
{
	size_t len = strlen(objname)+7;
	char *texname = new char[len];
	strcpy_s(texname, len, objname);
	strcat_s(texname, len, "_cloud");
	delete []objname;
	objname = texname;

	maxlvl = min (*(int*)gc->GetConfigParam (CFGPRM_SURFACEMAXLEVEL),        // global setting
	              *(int*)oapiGetObjectParam (obj, OBJPRM_PLANET_SURFACEMAXLEVEL)); // planet-specific setting
	maxbaselvl = min (8, maxlvl);
	pcdir = _V(1,0,0);
	lightfac = *(double*)gc->GetConfigParam (CFGPRM_SURFACELIGHTBRT);
	nmask = 0;
	nhitex = nhispec = 0;

	atmc = oapiGetPlanetAtmConstants (obj);

	int maxidx = patchidx[maxbaselvl];
	tiledesc = new TILEDESC[maxidx];
	memset2 (tiledesc, 0, maxidx*sizeof(TILEDESC));

	for (int i = 0; i < patchidx[maxbaselvl]; i++)	tiledesc[i].flag = 1;
}

// =======================================================================

void CloudManager::LoadData()
{
	if (ntex!=0 || bNoTextures) return;
	LoadTextures();
}

// =======================================================================

void CloudManager::Render(LPDIRECT3DDEVICE9 dev, D3DXMATRIX &wmat, double scale, int level, double viewap)
{
	LoadData();
	if (bNoTextures) return;
	
	D3DMATERIAL9 def_mat = {{1,1,1,1},{1,1,1,1},{1,1,1,1},{0,0,0,1},0};

	HR(FX->SetTechnique(eCloudTech));
	HR(FX->SetValue(eMat, &def_mat, sizeof(D3DMATERIAL9)));
	HR(FX->SetValue(eSun, gc->GetScene()->GetSun(), sizeof(D3D9Sun)));
	HR(FX->SetInt(eSpecularMode, 0));

	if (microtex) {
		HR(FX->SetTexture(eTex3, microtex->GetTexture()));
	}

	bool do_micro = (microtex && microlvl > 0.01);

	if (do_micro) {	
		HR(FX->SetFloat(eMix, float(microlvl)));
	}
	else {
		HR(FX->SetFloat(eMix, 0.0f));
	}
	
	TileManager::Render(dev, wmat, scale, level, viewap);
}


void CloudManager::RenderShadow(LPDIRECT3DDEVICE9 dev, D3DXMATRIX &wmat, double scale, int level, double viewap, float shadowalpha)
{
	LoadData();
	if (bNoTextures) return;

	D3DMATERIAL9 cloudmat = {{0,0,0,1},{0,0,0,1},{0,0,0,0},{0,0,0,0},0};
	cloudmat.Diffuse.a = cloudmat.Ambient.a = shadowalpha;

	HR(FX->SetTechnique(eCloudShadow)) 
	HR(FX->SetValue(eMat, &cloudmat, sizeof(D3DMATERIAL9)));
	HR(FX->SetValue(eSun, gc->GetScene()->GetSun(), sizeof(D3D9Sun)));
	HR(FX->SetInt(eSpecularMode, 0));

	if (microtex) {
		HR(FX->SetTexture(eTex3, microtex->GetTexture()));
	}

	bool do_micro = (microtex && microlvl > 0.01);

	if (do_micro) {
		HR(FX->SetFloat(eMix, float(microlvl)));
	}
	else {
		HR(FX->SetFloat(eMix, 0.0f));
	}

	TileManager::Render(dev, wmat, scale, level, viewap);
}

// ==============================================================

void CloudManager::RenderSimple(int level, int npatch, TILEDESC *tile, LPD3DXMATRIX mWrld)
{
	LoadData();
	if (bNoTextures) return;

	// render complete sphere (used at low LOD levels)
	LPDIRECT3DDEVICE9 pDev = gc->GetDevice();
	pDev->SetVertexDeclaration(pPatchVertexDecl);

	HR(FX->SetMatrix(eW, mWrld));
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));

	for (int idx = 0; idx < npatch; idx++) {
		
		VBMESH &mesh = PATCH_TPL[level][idx]; // patch template

		D3D9Stats.Old.Tiles[level]++;
		
		HR(FX->SetTexture(eTex0, tile[idx].tex));
		FX->CommitChanges();

		pDev->SetStreamSource(0, mesh.pVB, 0, sizeof(VERTEX_2TEX));
		pDev->SetIndices(mesh.pIB);
		pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh.nv, 0, mesh.nf);
	}

	HR(FX->EndPass());
	HR(FX->End());	
}


void CloudManager::InitRenderTile()
{
	LPDIRECT3DDEVICE9 pDev = gc->GetDevice();
	pDev->SetVertexDeclaration(pPatchVertexDecl);

	HR(FX->SetFloat(eTime, float(fmod(oapiGetSimTime(),60.0))));
	UINT numPasses = 0;
	HR(FX->Begin(&numPasses, D3DXFX_DONOTSAVESTATE));
	HR(FX->BeginPass(0));
}

void CloudManager::EndRenderTile()
{
	HR(FX->EndPass());
	HR(FX->End());		
}


// =======================================================================

void CloudManager::RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, double sdist,
	TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag)
{
	VBMESH &mesh = PATCH_TPL[lvl][ilat]; // patch template

	if (range.tumin == 0 && range.tumax == 1) {
		HR(FX->SetVector(eTexOff, &D3DXVECTOR4(1.0f, 0.0f, 1.0f, 0.0f)));
	}
	else {
		float tuscale = range.tumax-range.tumin, tuofs = range.tumin;
		float tvscale = range.tvmax-range.tvmin, tvofs = range.tvmin;
		HR(FX->SetVector(eTexOff, &D3DXVECTOR4(tuscale,tuofs,tvscale,tvofs)));
	}
	HR(FX->SetMatrix(eW, &mWorld));
	HR(FX->SetTexture(eTex0, tex));	  // Diffuse Texture

	FX->CommitChanges();
	
	pDev->SetStreamSource(0, mesh.pVB, 0, sizeof(VERTEX_2TEX));
	pDev->SetIndices(mesh.pIB);
	pDev->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, mesh.nv, 0, mesh.nf);
}
