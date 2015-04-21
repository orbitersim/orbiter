// ==============================================================
// TileMgr.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2007 Martin Schweiger
// ==============================================================

// ==============================================================
// class TileManager (interface)
//
// Planetary surface rendering management, including a simple
// LOD (level-of-detail) algorithm for surface patch resolution.
// ==============================================================

#ifndef __TILEMGR_H
#define __TILEMGR_H

#include "D3D9Effect.h"
#include "D3D9Util.h"
#include "Mesh.h"
#include "Spherepatch.h"

#define MAXQUEUE 10

#pragma pack(push,1)
	struct TILEFILESPEC {
		DWORD sidx;       // index for surface texture (-1: not present)
		DWORD midx;       // index for land-water mask texture (-1: not present)
		DWORD eidx;       // index for elevation data blocks (not used yet; always -1)
		DWORD flags;      // tile flags: bit 0: has diffuse component; bit 1: has specular component; bit 2: has city lights
		DWORD subidx[4];  // subtile indices
	};

struct LMASKFILEHEADER { // file header for contents file at level 1-8
	char id[8];          //    ID+version string
	DWORD hsize;         //    header size
	DWORD flag;          //    bitflag content information
	DWORD npatch;        //    number of patches
	BYTE minres;         //    min. resolution level
	BYTE maxres;         //    max. resolution level
};
#pragma pack(pop)

struct TILEDESC {
	LPDIRECT3DTEXTURE9 tex;      // diffuse surface texture
	LPDIRECT3DTEXTURE9 ltex;     // landmask texture, if applicable
	DWORD flag;
	struct TILEDESC *subtile[4];   // sub-tiles for the next resolution level
	DWORD ofs;                     // refers back to the master list entry for the tile
};

typedef struct {
	float tumin, tumax;
	float tvmin, tvmax;
} TEXCRDRANGE;

class D3D9Config;
class vPlanet;


class TileManager : public D3D9Effect {

	friend class TileBuffer;
	friend class CSphereManager;

public:
	/**
	 * \brief Constructs a new tile manager object
	 * \param gclient client instance pointer
	 * \param vPlanet planet instance pointer
	 */
	TileManager (oapi::D3D9Client *gclient, const vPlanet *vplanet);

	/**
	 * \brief Destroys the tile manager object
	 */
	virtual ~TileManager ();

	/**
	 * \brief Set up global parameters shared by all instances
	 * \param gclient client instance pointer
	 */
	static void GlobalInit(oapi::D3D9Client *gclient);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

	// One-time global initialisation/exit methods

	inline int GetMaxLevel () const { return maxlvl; }

	virtual void SetMicrotexture (const char *fname);
	virtual void SetMicrolevel (double lvl);

	virtual void Render(LPDIRECT3DDEVICE9 dev, D3DXMATRIX &wmat, double scale, int level, double viewap = 0.0, bool bfog = false);

	void SetAmbientColor(D3DCOLOR cAmbient);

protected:
	
	void ProcessTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, TILEDESC *tile,
	const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag,
	const TEXCRDRANGE &bkp_range, LPDIRECT3DTEXTURE9 bkp_tex, LPDIRECT3DTEXTURE9 bkp_ltex, DWORD bkp_flag);

	virtual void InitRenderTile() = 0;
	virtual void EndRenderTile() = 0;
	virtual void RenderSimple(int level, int npatch, TILEDESC *tile, LPD3DXMATRIX mWorld) = 0;

	virtual void RenderTile(int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, double sdist, TILEDESC *tile,
		const TEXCRDRANGE &range, LPDIRECT3DTEXTURE9 tex, LPDIRECT3DTEXTURE9 ltex, DWORD flag) = 0;

	bool LoadPatchData ();
	// load binary definition file for LOD levels 1-8

	bool LoadTileData ();
	// load binary definition file for LOD levels > 8

	bool AddSubtileData (TILEDESC &td, TILEFILESPEC *tfs, DWORD idx, DWORD sub, DWORD lvl);
	// add a high-resolution subtile specification to the tree

	void LoadTextures (char *modstr = 0);
	// load patch textures for all LOD levels

	void PreloadTileTextures (TILEDESC *tile8, DWORD ntex, DWORD nmask);
	// Pre-load high-resolution tile textures for the planet (level >= 9)

	void AddSubtileTextures (TILEDESC *td, LPDIRECT3DTEXTURE9 *tbuf, DWORD nt, LPDIRECT3DTEXTURE9 *mbuf, DWORD nm);
	// add a high-resolution subtile texture to the tree

	void LoadSpecularMasks ();
	// load specular and night light textures

	VECTOR3 TileCentre (int hemisp, int ilat, int nlat, int ilng, int nlng);
	// direction to tile centre from planet centre in planet frame

	void TileExtents (int hemisp, int ilat, int nlat, int ilg, int nlng, double &lat1, double &lat2, double &lng1, double &lng2) const;

	int IsTileInView(int lvl, int ilat, float scale);
	// checks if a given tile is observable from camera position

	void SetWorldMatrix (int ilng, int nlng, int ilat, int nlat);
	// set the world transformation for a particular tile

	bool SpecularColour (D3DCOLORVALUE *col);
	// adjust specular reflection through atmosphere


	D3DXMATRIX mWorld;
	D3DCOLOR cAmbient;
	oapi::D3D9Client *gc;			 // the client
	const vPlanet *vp;               // the planet visual
	OBJHANDLE obj;                   // the planet object
	char *objname;                   // the name of the planet (for identifying texture files)
	DWORD tilever;                   // file version for tile textures
	int maxlvl;                      // max LOD level
	int maxbaselvl;                  // max LOD level, capped at 8
	DWORD ntex;                      // total number of loaded textures for levels <= 8
	DWORD nhitex;                    // number of textures for levels > 8
	DWORD nhispec;                   // number of specular reflection masks (level > 8)
	double hipatchrad;               // angular aperture fraction at which to downgrade patch resolution
	double lightfac;                 // city light intensity factor
	double microlvl;                 // intensity of microtexture
	DWORD nmask;                     // number of specular reflection masks/light maps (level <= 8)
	VECTOR3 pcdir;                   // previous camera direction
	static D3DXMATRIX Rsouth;        // rotation matrix for mapping tiles to southern hemisphere
	float spec_base;                 // base intensity for specular reflections
	const ATMCONST *atmc;            // atmospheric parameters (used for specular colour modification)
	bool bPreloadTile;               // pre-load surface tile textures
	bool bNoTextures;				 // Textures failed to load
	TILEDESC *tiledesc;              // tile descriptors for levels 1-8
	static TileBuffer *tilebuf;      // subtile manager

	LPDIRECT3DTEXTURE9 *texbuf;		// texture buffer for surface textures (level <= 8)
	LPDIRECT3DTEXTURE9 *specbuf;	// texture buffer for specular masks (level <= 8);
	LPD3D9CLIENTSURFACE microtex;	// microtexture overlay

	// object-independent configuration data
	static bool bGlobalSpecular;     // user wants specular reflections
	static bool bGlobalRipple;       // user wants specular microtextures
	static bool bGlobalLights;       // user wants planet city lights

	// tile patch templates
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
	static int patchidx[9];          // texture offsets for different LOD levels
	static int NLAT[9];
	static int NLNG5[1], NLNG6[2], NLNG7[4], NLNG8[8], *NLNG[9];
	static DWORD vpX0, vpX1, vpY0, vpY1; // viewport boundaries

	static DWORD vbMemCaps;          // video/system memory flag for vertex buffers

	struct RENDERPARAM {
		LPDIRECT3DDEVICE9 dev;       // render device
		D3DXMATRIX wmat;              // world matrix
		D3DXMATRIX wmat_tmp;          // copy of world matrix used as work buffer
		int tgtlvl;                  // target resolution level
		MATRIX3 grot;                // planet rotation matrix
		VECTOR3 cpos;                // planet offset vector (in global frame)
		VECTOR3 sdir;                // sun direction from planet centre (in planet frame)
		VECTOR3 cdir;                // camera direction from planet centre (in planet frame)
		double cdist;                // camera distance from planet centre (in units of planet radii)
		double viewap;               // aperture of surface cap visible from camera pos
		double objsize;              // planet radius
		double horzdist;			 // distance to the horizon in meters
		bool bfog;                   // distance fog flag
		bool bCockpit;
	} RenderParam;
};


// =======================================================================
// Class TileBuffer: Global resource; holds a collection of
// tile specifications across all planets

class TileBuffer {
public:
	TileBuffer (const oapi::D3D9Client *gclient);
	~TileBuffer ();
	TILEDESC *AddTile ();
	void DeleteSubTiles (TILEDESC *tile);

	friend void ClearVertexBuffers (TILEDESC *td);
	// Recursively remove subrange vertex buffers from a tile tree with
	// root td. This is necessary when a new tile has been loaded, because
	// this can change the subrange extents for child tiles.

	bool LoadTileAsync (const char *name, TILEDESC *tile);
	// load the textures for a tile for planet 'name', given by descriptor
	// 'tile', using a separate thread.
	// Returns false if request can't be entered (queue full, or request
	// already present)

	static bool ShutDown();
	static void HoldThread(bool bHold);

	static HANDLE hQueueMutex;
	static HANDLE hLoadThread;

private:
	bool DeleteTile (TILEDESC *tile);

	static HRESULT ReadDDSSurface (LPDIRECT3DDEVICE9 pDev, const char *fname, long ofs, LPDIRECT3DTEXTURE9* pTex, bool bManaged);
	static DWORD WINAPI LoadTile_ThreadProc (void*);
	// the thread function loading tile textures on demand

	const oapi::D3D9Client *gc;      // the client
	bool bLoadMip;  // load mipmaps for tiles if available
	
	static bool bRunThread;
	static bool bHoldThread;
	static int nqueue, queue_in, queue_out;
	DWORD nbuf;     // buffer size;
	DWORD nused;    // number of active entries
	DWORD last;     // index of last activated entry
	TILEDESC **buf; // tile buffer

	static struct QUEUEDESC {
		const char *name;
		TILEDESC *td;
	} loadqueue[MAXQUEUE];
};

#endif // !__TILEMGR_H