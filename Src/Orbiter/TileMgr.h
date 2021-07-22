// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// TileManager: Rendering of planetary surfaces using texture tiles at
// variable resolutions.
// =======================================================================

#ifndef __TILEMGR_H
#define __TILEMGR_H

#define STRICT 1
#include "Spherepatch.h"

#define MAXQUEUE 10

// =======================================================================
// structure typedefs

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
	LPDIRECT3DVERTEXBUFFER7 vtx;
	LPDIRECTDRAWSURFACE7 tex;      // diffuse surface texture
	LPDIRECTDRAWSURFACE7 ltex;     // landmask texture, if applicable
	DWORD flag;
	struct TILEDESC *subtile[4];   // sub-tiles for the next resolution level
	DWORD ofs;                     // refers back to the master list entry for the tile
};

typedef struct {
	float tumin, tumax;
	float tvmin, tvmax;
} TEXCRDRANGE;

// =======================================================================
// Class TileBuffer: Global resource; holds a collection of
// tile specifications across all planets

class TileBuffer {
public:
	TileBuffer ();
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

	void StopLoadThread ();

	static HANDLE hQueueMutex;

private:
	bool DeleteTile (TILEDESC *tile);

	DWORD nbuf;     // buffer size;
	DWORD nused;    // number of active entries
	DWORD last;     // index of last activated entry
	TILEDESC **buf; // tile buffer

	bool bLoadMip;  // load mipmaps for tiles if available
	static bool bRunThread;
	static int nqueue, queue_in, queue_out;
	HANDLE hLoadThread;
	static DWORD WINAPI LoadTile_ThreadProc (void*);

	static struct QUEUEDESC {
		const char *name;
		TILEDESC *td;
	} loadqueue[MAXQUEUE];
};

// =======================================================================
// Class TileManager: Resolution levels 1..14

class TileManager {
	friend class CSphereManager;

public:
	TileManager (const Planet *_cbody);
	// Constructor. _cbody should be of type CelestialBody rather than Planet

	~TileManager ();

	static void CreateDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev);
	static void DestroyDeviceObjects ();
	static void Stop ();

	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double scale, VPlanet *vbody, int level, bool addambient, bool addfog);
	// Render planetary surface by looping over all 364 level-8 surface tiles
	// and recursively descend to required resolution

protected:
	bool LoadPatchData ();
	// Load patch specifications (flags for specular reflectance) from a binary file

	bool LoadTileData ();
	// Load hires tile specifications from a binary file

	bool AddSubtileData (TILEDESC &td, TILEFILESPEC *tfs, DWORD idx, DWORD sub, DWORD lvl);
	// Recursively adds tile information for tiles at resolution 9 and higher

	void LoadTextures ();
	// Pre-load the surface textures for the planet (level 1-8)

	void PreloadTileTextures (TILEDESC *tile8, DWORD ntex, DWORD nmask);
	// Pre-load high-resolution tile textures for the planet (level >= 9)

	void AddSubtileTextures (TILEDESC *td, LPDIRECTDRAWSURFACE7 *tbuf, DWORD nt, LPDIRECTDRAWSURFACE7 *mbuf, DWORD nm);
	// recursively copy textures from the provided arrays into the tile tree

	void LoadSpecularMasks ();
	// Load the mask textures for specular reflections

	void ProcessTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, TILEDESC *tile,
		const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag,
		const TEXCRDRANGE &bkp_range, LPDIRECTDRAWSURFACE7 bkp_tex, LPDIRECTDRAWSURFACE7 bkp_ltex, DWORD bkp_flag);
	// Render a single tile at resolution level 'lvl'
	// If lvl < tgtlvl, this recursively steps down to the requested level

	void RenderTile (int lvl, int hemisp, int ilat, int nlat, int ilng, int nlng, double sdist,
		TILEDESC *tile, const TEXCRDRANGE &range, LPDIRECTDRAWSURFACE7 tex, LPDIRECTDRAWSURFACE7 ltex, DWORD flag);
	// Render a tile at given resolution and position

	void RenderSimple (int lvl, TILEDESC *tile);
	// Render a full sphere (used for low resolutions)

	Vector TileCentre (int hemisp, int ilat, int nlat, int ilng, int nlng);
	// returns the direction of the tile centre from the planet centre in local
	// planet coordinates

	void TileExtents (int hemisp, int ilat, int nlat, int ilg, int nlng, double &lat1, double &lat2, double &lng1, double &lng2);

	bool TileInView (int lvl, int ilat);
	// Check if specified tile intersects viewport

	void SetWorldMatrix (int ilng, int nlng, int ilat, int nlat);
	// Set world transformation matrix for a given tile

	bool SpecularColour (D3DCOLORVALUE *col);
	// Adjusts the colour of the specular reflection component
	// according to atmospheric colour and relative sun position

private:
	const Planet *cbody;
	DWORD tilever;                   // file version for tile textures
	DWORD maxlvl;                    // max. patch resolution level
	DWORD maxbaselvl;                // max. resolution level, capped at 8
	DWORD ntex;                      // total number of loaded textures for levels <= 8
	DWORD nhitex;                    // number of textures for levels > 8
	DWORD nmask;                     // number of specular reflection masks/light maps (level <= 8)
	DWORD nhispec;                   // number of specular reflection masks (level > 8)
	LPDIRECTDRAWSURFACE7 *texbuf;    // texture buffer for surface textures (level <= 8)
	LPDIRECTDRAWSURFACE7 *specbuf;   // texture buffer for specular masks (level <= 8);
	LPDIRECTDRAWSURFACE7 wavetex;    // specular microtexture
	TILEDESC *tiledesc;              // tile descriptors for levels 1-8
	bool bRipple;                    // render ripples in specular reflection
	bool bPreloadTile;               // preload high-resolution tile textures
	double lightfac;                 // city light intensity factor
	Vector pcdir;                    // previous camera direction

	// object-independent configuration parameters
	static bool bSpecular;           // surface contains areas with specular reflection
	static bool bLights;             // render city lights on the dark side

	static TileBuffer *tilebuf;
	static D3DMATRIX Rsouth;
	static DWORD vpX0, vpX1, vpY0, vpY1; // viewport boundaries
	struct RENDERPARAM {
		LPDIRECT3DDEVICE7 dev;       // render device
		D3DMATRIX wmat;              // world matrix
		D3DMATRIX wmat_tmp;          // copy of world matrix used as work buffer
		Matrix grot;                 // planet rotation matrix
		Vector cpos;                 // planet offset vector (in global frame)
		VPlanet *vbody;              // pointer to visual
		int tgtlvl;                  // target resolution level
		Vector sdir;                 // sun direction from planet centre (in planet frame)
		Vector cdir;                 // camera direction from planet centre (in planet frame)
		double cdist;                // camera distance from planet centre (in units of planet radii)
		double viewap;               // aperture of surface cap visible from camera pos
		bool fog;                    // distance fog active?
	} RenderParam;
};

#endif // !__TILEMGR_H