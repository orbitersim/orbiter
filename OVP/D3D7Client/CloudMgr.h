// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// CloudMgr.h
// class CloudManager (interface)
//
// Planetary rendering management for cloud layers, including a simple
// LOD (level-of-detail) algorithm for patch resolution.
// ==============================================================

#ifndef __CLOUDMGR_H
#define __CLOUDMGR_H

#include "TileMgr.h"

class CloudManager: public TileManager {
public:
	CloudManager (const oapi::D3D7Client *gclient, const vPlanet *vplanet);

	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double scale, int level, double viewap = 0.0);

protected:
	void RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, double sdist,
		TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag);

private:
	int cloudtexidx;
};

#endif // !__CLOUDMGR_H