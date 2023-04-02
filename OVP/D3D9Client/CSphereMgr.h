// =======================================================================
// CSphereMgr: Rendering of the celestial sphere background at variable
// resolutions.
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2011-2016 Jarmo Nikkanen (D3D9Client modification) 
// =======================================================================

#ifndef __CSPHEREMGR_H
#define __CSPHEREMGR_H

#define STRICT 1
#include "TileMgr.h"

class D3D9Config;

// =======================================================================
// Class CSphereManager

class CSphereManager
{
public:

#pragma pack(push, 4)
	struct CelDataStruct
	{
		float4x4 mWorld;
		float4x4 mViewProj;
		float	 fAlpha;
		float	 fBeta;
	} CelData;

	struct CelDataFlow
	{
		BOOL	 bAlpha;
		BOOL	 bBeta;
	} CelFlow;
#pragma pack(pop)

	/**
	 * \brief Constructs a new sphere manager object
	 * \param gclient client instance pointer
	 * \param scene scene to which the visual is added
	 */
	CSphereManager (oapi::D3D9Client *gclient, const Scene *scene);

	/**
	 * \brief Destroys the sphere manager object
	 */
	~CSphereManager ();

	/**
	 * \brief Set up global parameters shared by all instances
	 * \param gclient client instance pointer
	 */
	static void GlobalInit (oapi::D3D9Client *gclient);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit ();

	static void CreateDeviceObjects(LPDIRECT3D9 d3d, LPDIRECT3DDEVICE9 dev);
	static void DestroyDeviceObjects();

	/**
	 * \brief Set the visual brightness of the background image.
	 * \param val brightness value (0-1)
	 */
	void SetBgBrightness(double val);

	void Render (LPDIRECT3DDEVICE9 dev, int level, double bglvl);

protected:
	bool LoadPatchData ();
	bool LoadTileData ();
	void LoadTextures ();

	void ProcessTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, TILEDESC *tile,
		const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag,
		const TEXCRDRANGE &bkp_range, LPDIRECT3DTEXTURE9 bkp_tex, LPDIRECT3DTEXTURE9 bkp_ltex, DWORD bkp_flag);

	void RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng,
		TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag);

	void SetWorldMatrix (int ilng, int nlng, int ilat, int nlat);

	VECTOR3 TileCentre (int hemisp, int ilat, int nlat, int ilng, int nlng);
	// returns the direction of the tile centre from the planet centre in local
	// planet coordinates

	void TileExtents (int hemisp, int ilat, int nlat, int ilg, int nlng, double &lat1, double &lat2, double &lng1, double &lng2) const;

	bool TileInView (int lvl, int ilat);
	// Check if specified tile intersects viewport

	static const D3D9Config *cfg;    // configuration parameters
	const Scene *scn;
	static int *patchidx;            // texture offsets for different LOD levels
	static VBMESH PATCH_TPL_1;
	static VBMESH PATCH_TPL_2;
	static VBMESH PATCH_TPL_3;
	static VBMESH PATCH_TPL_4[2];
	static VBMESH PATCH_TPL_5;
	static VBMESH PATCH_TPL_6[2];
	static VBMESH PATCH_TPL_7[4];
	static VBMESH PATCH_TPL_8[8];
	static VBMESH PATCH_TPL_9[16];
	static VBMESH PATCH_TPL_10[32];
	static VBMESH PATCH_TPL_11[64];
	static VBMESH PATCH_TPL_12[128];
	static VBMESH PATCH_TPL_13[256];
	static VBMESH PATCH_TPL_14[512];
	static VBMESH *PATCH_TPL[15];
	static int **NLNG;
	static int *NLAT;

private:
	HANDLE hTexA, hTexB, hVSConst;
	ShaderClass* pShader;
	D3D9Client* gc;
	char texname[128];
	char starfieldname[128];
	float intensity;                 // opacity of background image
	bool m_bBkgImg;                  ///< background image available?
	bool m_bStarImg;                 ///< starfield image available?
	bool m_bDisabled;                ///< background disabled?
	DWORD maxlvl;                    // max. patch resolution level
	DWORD maxbaselvl;                // max. resolution level, capped at 8
	DWORD ntex;                      // total number of loaded textures for levels <= 8
	DWORD nhitex;                    // number of textures for levels > 8
	DWORD nhispec;                   // number of specular reflection masks (level > 8)
	TILEDESC *tiledesc;              // tile descriptors for levels 1-8
	std::vector<LPDIRECT3DTEXTURE9> m_texbuf; // texture buffer for surface textures (level <= 8)
	std::vector<LPDIRECT3DTEXTURE9> m_starbuf; // texture buffer for starfield textures (level <= 8)
	bool bPreloadTile;               // preload high-resolution tile textures
	MATRIX3 ecl2gal;                 // rotates from ecliptic to galactic frame
	D3DXMATRIX trans;                 // transformation from ecliptic to galactic frame
	D3DXMATRIX mWorld;

	TileBuffer *tilebuf;
	struct RENDERPARAM {
		LPDIRECT3DDEVICE9 dev;       // render device
		int tgtlvl;                  // target resolution level
		D3DXMATRIX wmat;             // world matrix
		VECTOR3 camdir;              // camera direction in galactic frame
		double viewap;               // viewport aperture (semi-diagonal)
	} RenderParam;

	static DWORD vpX0, vpX1, vpY0, vpY1; // viewport boundaries
	static double diagscale;
};

#endif // !__CSPHEREMGR_H
