// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __TILEMGR2_H
#define __TILEMGR2_H

#include "qtree.h"
#include "Planet.h"
#include "VPlanet.h"
#include "Spherepatch.h"
#include "ZTreeMgr.h"
#include "Log.h"

#define MAXQUEUE2 20

#define TILE_VALID  0x0001
#define TILE_ACTIVE 0x0002

#define TILE_FILERES 256
#define TILE_ELEVSTRIDE (TILE_FILERES+3)

// =======================================================================

typedef struct {
	float tumin, tumax;
	float tvmin, tvmax;
} TEXCRDRANGE2;

// =======================================================================

class Tile {
	friend class TileManager2Base;
	friend class TileLoader;

public:
	Tile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng);
	virtual ~Tile ();

	inline int Level() const { return lvl; }

	bool PreDelete();
	// Prepare tile for deletion. Return false if tile is locked

	bool InView (const MATRIX4 &transform);
	// tile in view of camera, given by transformation matrix 'transform'?

	void Extents (double *latmin, double *latmax, double *lngmin, double *lngmax) const;
	// Return the latitude/longitude extents of the tile

	virtual void MatchEdges () {}
	// Match edges with neighbour tiles

	inline const TEXCRDRANGE2 *GetTexRange () const { return &texrange; }
	// Returns the tile's texture coordinate range

	bool GetParentSubTexRange (TEXCRDRANGE2 *subrange);
	// Returns the texture range that allows to access the appropriate subregion of the
	// parent's texture

	// protected member access functions
	LPDIRECTDRAWSURFACE7 Tex() { return tex; }

	enum TileState {
		Invalid   = 0x0000,                            // tile data have not been loaded/created yet
		InQueue   = 0x0004,                            // queued for asynchronous load
		Loading   = 0x0008,                            // in the process of being loaded
		Inactive  = TILE_VALID,                        // valid data, but not in active part of quadtree (cached)
		Active    = TILE_VALID | TILE_ACTIVE,          // active, but not rendered itself (ancestor of rendered tiles)
		Invisible = TILE_VALID | TILE_ACTIVE | 0x0004, // active, but outside field of view
		ForRender = TILE_VALID | TILE_ACTIVE | 0x0008  // rendered tile
	};

	inline void SetState (TileState newstate) { state = newstate; }
	inline void SetEdgeState (bool valid) { edgeok = valid; }

protected:
	virtual Tile *getParent() = 0;

	virtual void Load () = 0;
	// Load tile data from file, or synthesise missing data from ancestors

	virtual void Render () {}

	Vector Centre () const;
	// Returns the direction of the tile centre from the planet centre in local planet coordinates

	VBMESH *CreateMesh_quadpatch (int grdlat, int grdlng, INT16 *elev=0, double elev_scale=1.0, double globelev=0.0,
		const TEXCRDRANGE2 *range=0, bool shift_origin=false, Vector *shift=0, double bb_excess=0.0);
	// Creates a quadrilateral patch mesh

	VBMESH *CreateMesh_tripatch (int grd, INT16 *elev=0, bool shift_origin=false, Vector *shift=0);
	// Creates a triangular patch mesh for north or south pole

	VBMESH *CreateMesh_hemisphere (int grd, INT16 *elev=0, double globelev=0.0);
	// Creates a hemisphere mesh for eastern or western hemisphere at resolution level 4

	TileManager2Base *mgr;		// the manager this tile is associated with
	int lvl;					// tile resolution level
	int ilat;					// latitude index
	int ilng;					// longitude index
	LPDIRECTDRAWSURFACE7 tex;	// diffuse surface texture
	bool owntex;				// true: tile owns the texture, false: tile uses ancestor subtexture
	TEXCRDRANGE2 texrange;		// texture coordinate subrange (if using ancestor subtexture)
	VBMESH *mesh;				// vertex-buffered tile mesh
	Vector cnt;					// tile centre in local planet coords
	Vector vtxshift;
	bool edgeok;				// edges have been checked in this frame
	TileState state;			// tile load/active/render state flags
	int lngnbr_lvl, latnbr_lvl, dianbr_lvl;	// neighbour levels to which edges have been adapted
	mutable double mean_elev;	// mean tile elevation [m]
};

// =======================================================================

class TileLoader {
	template<class T> friend class TileManager2;

public:
	TileLoader ();
	~TileLoader ();
	bool LoadTileAsync (Tile *tile);

	bool Unqueue (Tile *tile);
	// remove a tile from the load queue (caller must own hLoadMutex)

	inline static DWORD WaitForMutex() { return ::WaitForSingleObject (hLoadMutex, INFINITE); }
	inline static BOOL ReleaseMutex() { return ::ReleaseMutex (hLoadMutex); }

private:
	static struct QUEUEDESC {
		Tile *tile;
	} queue[MAXQUEUE2];

	static bool bRunThread;
	static int nqueue, queue_in, queue_out;
	HANDLE hLoadThread;
	static HANDLE hLoadMutex;
	static DWORD WINAPI Load_ThreadProc (void*);
};

// =======================================================================

class TileManager2Base {
	friend class Tile;
	friend class CsphereTile;

public:
	struct configPrm {				// global configuration parameters
		int elevMode;                   // elevation mode: 0=none, 1=lin interp., 2=cubic interp
		bool bSpecular;					// render specular surface reflections?
		bool bLights;					// render planet night lights?
		bool bCloudShadow;				// render cloud shadows?
		double lightfac;				// city light brightness factor
		DWORD tileLoadFlags;            // 0x0001: load tiles from directory tree
		                                // 0x0002: load tiles from compressed archive
	};

	struct RenderPrm {
		int maxlvl;        // max tile level
		MATRIX4 dwmat;     // planet world matrix, double precision
		MATRIX4 dwmat_tmp; // modifyable planet world matrix, double precision
		MATRIX4 dviewproj; // view+projection matrix, double precision
		Matrix grot;       // planet rotation matrix
		Vector cpos;       // planet offset vector (in global frame)
		Vector cdir;       // camera direction from planet centre (in planet frame)
		Vector sdir;       // sun direction in local planet coords
		double cdist;      // camera distance from planet centre (in units of planet radii)
		double viewap;     // aperture of surface cap visible from camera pos
		double scale;
		bool fog;          // apply distance fog?
		bool tint;         // apply atmospheric tint?
		float shadowcol;   // cloud shadow colour
		bool flatshadow;   // render cloud shadows onto sphere?
		Vector atm_tint;   // atmospheric RGB surface tint at high atmosphere
		TileManager2<CloudTile> *cloudmgr; // cloud manager (for shadow projection); NULL if no clouds
		double cloudrot;   // cloud layer rotation angle
	} prm;

	TileManager2Base (const Planet *_cbody, int _maxres, int _gridres);

	static void CreateDeviceObjects (LPDIRECT3D7 _d3d, LPDIRECT3DDEVICE7 _dev);
	static void DestroyDeviceObjects ();

	static LPDIRECT3DDEVICE7 Dev() { return dev; }
	static LPDIRECT3D7 D3d() { return d3d; }

	inline void SetRenderState (D3DRENDERSTATETYPE type, DWORD state)
	{ dVERIFY(dev->SetRenderState (type, state), "LPDIRECT3DDEVICE7::SetRenderState failed"); } // should check for return type

	inline void SetTextureStageState (DWORD stage, D3DTEXTURESTAGESTATETYPE type, DWORD state)
	{ dVERIFY(dev->SetTextureStageState (stage, type, state), "LPDIRECT3DDEVICE7::SetTextureStageState failed"); } // should check for return type

	inline void SetTexture (DWORD stage, LPDIRECTDRAWSURFACE7 tex)
	{ dVERIFY(dev->SetTexture (stage, tex), "LPDIRECT3DDEVICE7::SetTexture failed"); } // should check for return type

	inline void GetMaterial (LPD3DMATERIAL7 mat)
	{ dVERIFY(dev->GetMaterial (mat), "LPDIRECT3DDEVICE7::GetMaterial failed"); } // should check for return type

	inline void SetMaterial (LPD3DMATERIAL7 mat)
	{ dVERIFY(dev->SetMaterial (mat), "LPDIRECT3DDEVICE7::SetMaterial failed"); } // should check for return type

	inline void DrawIndexedPrimitiveVB (D3DPRIMITIVETYPE type, LPDIRECT3DVERTEXBUFFER7 vbuf, DWORD vtx0, DWORD nvtx, LPWORD idx, DWORD nidx, DWORD flags)
	{ dVERIFY(dev->DrawIndexedPrimitiveVB (type, vbuf, vtx0, nvtx, idx, nidx, flags), "LPDIRECT3DDEVICE7::DrawIndexedPrimitiveVB failed"); } // should check for return type

	static configPrm &Cprm() { return cprm; }

	inline const Planet *Cbody() const { return cbody; }
	// Private member const access functions

	/// \brief Return the root directory containing the body's texture data (surface, elevation, mask, cloud tiles)
	inline const std::string& DataRootDir() const { return m_dataRootDir; }

	template<class TileType>
	QuadTreeNode<TileType> *FindNode (QuadTreeNode<TileType> root[2], int lvl, int ilng, int ilat);
	// Returns the node at the specified position, or 0 if it doesn't exist

	template<class TileType>
	void ProcessNode (QuadTreeNode<TileType> *node);

	template<class TileType>
	void RenderNode (QuadTreeNode<TileType> *node);

	template<class TileType>
	void RenderNodeLabels (QuadTreeNode<TileType> *node, oapi::Sketchpad *skp, oapi::Font **labelfont, int *fontidx);

	void SetRenderPrm (MATRIX4 &dwmat, double prerot, VPlanet *vbody, bool use_zbuf, const VPlanet::RenderPrm &prm);

	inline const int GridRes() const { return gridRes; }

protected:
	virtual MATRIX4 WorldMatrix (int ilng, int nlng, int ilat, int nlat);
	void SetWorldMatrix (const MATRIX4 &W);

	template<class TileType>
	QuadTreeNode<TileType> *LoadChildNode (QuadTreeNode<TileType> *node, int idx);
	// loads one of the four subnodes of 'node', given by 'idx'

	const Planet *cbody;			// the planet we are rendering
	std::string m_dataRootDir;      // the root directory (usually ending in the cbody's name) for all tile data (textures, elevations, etc.)

	static TileLoader *loader;		// pointer to global tile loader
	static configPrm cprm;

private:
	static LPDIRECT3D7 d3d;			// D3D instance
	static LPDIRECT3DDEVICE7 dev;	// render device
	static double resolutionBias;
	static bool bTileLoadThread;	// load tiles on separate thread?
	int gridRes;                    // mesh grid resolution. must be multiple of 2. Default: 64 for surfaces, 32 for clouds
};

// =======================================================================

template<class TileType>
class TileManager2: public TileManager2Base {
	friend class Tile;

public:
	TileManager2 (const Planet *_cbody, int _maxres, int _gridres);
	TileManager2(const char *name, int _maxres, int _gridres);
	~TileManager2 ();

	const char *getName() const { return m_name; }

	void SetRenderPrm(MATRIX4 &dwmat, double prerot, VPlanet *vbody, bool use_zbuf, const VPlanet::RenderPrm &prm);

	void Render (LPDIRECT3DDEVICE7 dev, MATRIX4 &dwmat, VPlanet *vbody, bool use_zbuf, const VPlanet::RenderPrm &rprm);
	void RenderLabels (oapi::Sketchpad *skp, oapi::Font **labelfont, int *fontidx);
	void RenderFlatCloudShadows (LPDIRECT3DDEVICE7 dev, MATRIX4 &dwmat, VPlanet *vbody);

	void CreateLabels();
	void DeleteLabels();
	void SetSubtreeLabels(QuadTreeNode<TileType> *node, bool activate);

	inline QuadTreeNode<TileType> *FindNode (int lvl, int ilng, int ilat)
	{ return TileManager2Base::FindNode<TileType> (tiletree, lvl, ilng, ilat); }

	inline TileType *GlobalTile (int lvl)
	{ return (lvl >= -3 && lvl < 0 ? globtile[lvl+3] : 0); }

	int Coverage (double latmin, double latmax, double lngmin, double lngmax, int maxlvl, Tile **tbuf, int nt);
	// fills tbuf with a list of tiles up to maxlvl currently covering the area latmin,latmax,lngmin,lngmax
	// nt is the length of tbuf. Return value is the number of tiles copied into tbuf.
	// If the number of covering tiles is larger than nt, return value is -1. In that case, no tiles are copied.
	// The tile pointers are only valid for the current render pass. They are not guaranteed to exist any more
	// after the next call to Render.

	MATRIX4 WorldMatrix(int ilng, int nlng, int ilat, int nlat) { return TileManager2Base::WorldMatrix(ilng, nlng, ilat, nlat); }

public:
	ZTreeMgr **treeMgr;  // handle tiles in compressed archives
	int ntreeMgr;

protected:
	TileType *globtile[3];              // full-sphere tiles for resolution levels 1-3
	QuadTreeNode<TileType> tiletree[2]; // quadtrees for western and eastern hemisphere

	void CheckCoverage (QuadTreeNode<TileType> *node, double latmin, double latmax, double lngmin, double lngmax, int maxlvl, Tile **tbuf, int nt, int *nfound);

	void LoadZTrees();

private:
	char *m_name;  // tileset name (e.g. planet name)
};

#endif // !__TILEMGR2_H