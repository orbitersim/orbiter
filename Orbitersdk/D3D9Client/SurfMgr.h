// ==============================================================
// SurfMgr.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
// ==============================================================

// ==============================================================
// class SurfaceManager (interface)
//
// Planetary surface rendering management, including a simple
// LOD (level-of-detail) algorithm for surface patch resolution.
// ==============================================================

#ifndef __SURFMGR_H
#define __SURFMGR_H

#include "TileMgr.h"

class SurfaceManager: public TileManager {
public:
	SurfaceManager(oapi::D3D9Client *gclient, const vPlanet *vplanet);
	void SetMicrotexture(const char *fname);
	void Render(LPDIRECT3DDEVICE9 dev, D3DXMATRIX &wmat, double scale, int level, double viewap = 0.0, bool bfog = false);
	void LoadData();

protected:

	void InitRenderTile();
	void EndRenderTile();
	void RenderSimple(int level, int npatch, TILEDESC *tile, LPD3DXMATRIX mWorld);

	void RenderTile(int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, double sdist,
		TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag);

};

#endif // !__SURFMGR_H