// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// CSphereMgr: Rendering of the celestial sphere background at variable
// resolutions.
// =======================================================================

#ifndef __CSPHEREMGR_H
#define __CSPHEREMGR_H

#define STRICT 1
#include "TileMgr.h"

// =======================================================================
// Class CSphereManager

class CSphereManager {
public:
	CSphereManager ();
	~CSphereManager ();

	static void CreateDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev);
	static void DestroyDeviceObjects ();

	void Render (LPDIRECT3DDEVICE7 dev, int level, int bglvl);

protected:
	bool LoadPatchData ();
	bool LoadTileData ();
	void LoadTextures ();

	void ProcessTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, TILEDESC *tile,
		const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag,
		const TEXCRDRANGE &bkp_range, LPDIRECTDRAWSURFACE7 bkp_tex, LPDIRECTDRAWSURFACE7 bkp_ltex, DWORD bkp_flag);

	void RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng,
		TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag);

	MATRIX4 WorldMatrix (int ilng, int nlng, int ilat, int nlat);
	void SetWorldMatrix (const MATRIX4 &W);

	Vector TileCentre (int hemisp, int ilat, int nlat, int ilng, int nlng);
	// returns the direction of the tile centre from the planet centre in local
	// planet coordinates

	void TileExtents (int hemisp, int ilat, int nlat, int ilg, int nlng, double &lat1, double &lat2, double &lng1, double &lng2);

	bool TileInView (int lvl, int ilat);
	// Check if specified tile intersects viewport

private:
	char texname[64];
	float intensity;                 // opacity of background image
	bool disabled;                   // background image disabled?
	DWORD maxlvl;                    // max. patch resolution level
	DWORD maxbaselvl;                // max. resolution level, capped at 8
	DWORD ntex;                      // total number of loaded textures for levels <= 8
	DWORD nhitex;                    // number of textures for levels > 8
	DWORD nhispec;                   // number of specular reflection masks (level > 8)
	TILEDESC *tiledesc;              // tile descriptors for levels 1-8
	LPDIRECTDRAWSURFACE7 *texbuf;    // texture buffer for surface textures (level <= 8)
	bool bPreloadTile;               // preload high-resolution tile textures
	Matrix ecl2gal;                  // rotates from ecliptic to galactic frame
	MATRIX4 trans;                   // transformation from ecliptic to galactic frame

	TileBuffer *tilebuf;
	struct RENDERPARAM {
		LPDIRECT3DDEVICE7 dev;       // render device
		int tgtlvl;                  // target resolution level
		MATRIX4 transform;           // full transformation matrix (for frustum checks)
		MATRIX4 viewproj;            // view+projection matrix
		MATRIX4 dwmat;               // world matrix
		Vector camdir;               // camera direction in galactic frame
		double viewap;               // viewport aperture (semi-diagonal)
	} RenderParam;

	static DWORD vpX0, vpX1, vpY0, vpY1; // viewport boundaries
	static double diagscale;
};

#endif // !__CSPHEREMGR_H