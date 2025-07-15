// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// =======================================================================
// tilemgr2.h
// Rendering of planetary surfaces using texture tiles at
// variable resolutions (new version).
// =======================================================================

#ifndef __TILEMGR2_H
#define __TILEMGR2_H

#include "Client.h"
#include "Util.h"
#include "VPlanet.h"
#include "Spherepatch.h"
#include "Pad.h"
#include "Qtree.h"
#include "ZTreeMgr.h"
#include <stack>
#include <vector>
#include <list>

#define NPOOLS 32
#define MAXQUEUE2 20

#define TILE_VALID  0x0001
#define TILE_ACTIVE 0x0002

#define TILE_FILERES 256
#define TILE_ELEVSTRIDE (TILE_FILERES+3)

#ifdef _DEBUG
// Debugging helper
#define TILE_STATE_OK(t) (t->state == Tile::Invalid \
                       || t->state == Tile::InQueue  \
                       || t->state == Tile::Loading   \
                       || t->state == Tile::Inactive   \
                       || t->state == Tile::Active      \
                       || t->state == Tile::Invisible    \
                       || t->state == Tile::ForRender)
#endif // _DEBUG

// =======================================================================
// Type definitions


#pragma pack(push,1)

struct ELEVFILEHEADER { // file header for patch elevation data file
	char id[4];            // ID string + version ('E','L','E',1)
	int hdrsize;           // header size (76 expected)
	int dtype;             // data format (0=flat, no data block; 8=uint8; -8=int8; 16=uint16; -16=int16)
	int xgrd, ygrd;         // data grid size (259 x 259 expected)
	int xpad, ypad;         // horizontal, vertical padding width (1, 1 expected)
	double scale;          // data scaling factor (1.0 expected)
	double offset;         // data offset (elevation = raw value * scale + offset)
	double latmin, latmax; // latitude range [rad]
	double lngmin, lngmax; // longitude range [rad]
	double emin, emax, emean; // min, max, mean elevation [m]
};

#pragma pack(pop)


typedef struct {
	float tumin, tumax;
	float tvmin, tvmax;
} TEXCRDRANGE2;

typedef union {
	VECTOR4 vec;
	struct {
		double minlng;				///< Left    -
		double maxlat;				///< Top     +
		double maxlng;				///< Right   +
		double minlat;				///< Bottom  -
	};
} TILEBOUNDS;

bool FileExists(const char* path);

// =======================================================================

/**
 * \brief Surface tile texture class.
 *
 * A Tile is the basic (visual) representation of a "planetary" surface
 * (of a planet, moon or asteroid).
 * There should be no simple tile objects, but derived classes.
 */
class Tile
{
	friend class TileManager2Base;
	friend class TileLoader;

public:
	Tile (TileManager2Base *_mgr, int _lvl, int _ilat, int _ilng);
	virtual ~Tile ();

	inline int State() const { return state; }
	inline int Level() const { return lvl; }
	inline void GetIndex(int *lng, int *lat) const { *lng = ilng, *lat = ilat; }
	inline bool HasOwnTex() const { return owntex; }
	inline bool HasOwnElev() const { return has_elevfile; }
	inline void GetWorldMatrix(void *pOut) const { memcpy(pOut, &mWorld, sizeof(FMATRIX4)); }

	bool PreDelete();
	// Prepare tile for deletion. Return false if tile is locked

	bool InView (const MATRIX4 &transform);
	// tile in view of camera, given by transformation matrix 'transform'?

	VECTOR3 Centre() const;
	// Returns the direction of the tile centre from the planet centre in local planet coordinates

	void Extents (double *latmin, double *latmax, double *lngmin, double *lngmax) const;
	// Return the latitude/longitude extents of the tile

	bool IsPointInTile(double lng, double lat);

	virtual void MatchEdges () {}
	// Match edges with neighbour tiles

	float GetBoundingSphereRad() const;
	FVECTOR3 GetBoundingSpherePos() const;
	bool Pick(const FMATRIX4* pW, const FVECTOR3 *vDir, TILEPICK &result);
	FVECTOR4 GetTexRangeDX (const TEXCRDRANGE2 *subrange) const;
	inline const TEXCRDRANGE2 *GetTexRange () const { return &texrange; }
	// Returns the tile's texture coordinate range

	bool GetParentMicroTexRange(TEXCRDRANGE2 *subrange);
	bool GetParentSubTexRange(TEXCRDRANGE2 *subrange);
	bool GetParentOverlayRange(TEXCRDRANGE2 *subrange);

	// Returns the texture range that allows to access the appropriate subregion of the
	// parent's texture

	inline LPDIRECT3DTEXTURE9 Tex() { return tex; }
	inline const LPDIRECT3DTEXTURE9 Tex() const { return tex; }

	enum TileState {
		Invalid   = 0x0000,                            // tile data have not been loaded/created yet
		InQueue   = 0x0004,                            // queued for asynchronous load
		Loading   = 0x0008,                            // in the process of being loaded
		Inactive  = TILE_VALID,                        // valid data, but not in active part of quadtree (cached)
		Active    = TILE_VALID | TILE_ACTIVE,          // active, but not rendered itself (ancestor of rendered tiles)
		Invisible = TILE_VALID | TILE_ACTIVE | 0x0004, // active, but outside field of view
		ForRender = TILE_VALID | TILE_ACTIVE | 0x0008  // rendered tile
	};

protected:
	virtual Tile *getParent() const = 0;

	virtual void Render ();
	virtual void StepIn () {}

	virtual bool IsElevated() { return false; }

	virtual double GetMinElev() const = 0;
	virtual double GetMaxElev() const = 0;
	virtual double GetMeanElev() const = 0;
	

	/**
	 * \brief Preloads a surface tile data into a system memory from a tile loader thread
	 */
	virtual void PreLoad() = 0;

	/**
	 * \brief Construct a surface tile textures from a preloaded data /see virtual bool PreLoad()
	 */
	virtual void Load () = 0;

	bool	CreateTexture(LPDIRECT3DDEVICE9 pDev, LPDIRECT3DTEXTURE9 pPre, LPDIRECT3DTEXTURE9 *pTex);
	bool	LoadTextureFile(const char *path, LPDIRECT3DTEXTURE9 *pPre);
	bool	LoadTextureFromMemory(void *data, DWORD ndata, LPDIRECT3DTEXTURE9 *pPre);

	VBMESH *CreateMesh_quadpatch (int grdlat, int grdlng, float *elev=0, double elev_scale = 1.0, double globelev=0.0,
		const TEXCRDRANGE2 *range=0, bool shift_origin=false, VECTOR3 *shift=0, double bb_excess=0.0);
	// Creates a quadrilateral patch mesh

	VBMESH *CreateMesh_hemisphere (int grd, float *elev=0, double globelev=0.0);
	// Creates a hemisphere mesh for eastern or western hemisphere at resolution level 4

	TileManager2Base *mgr;     // the manager this tile is associated with
	int lvl;                   // tile resolution level
	int ilat;                  // latitude index
	int ilng;                  // longitude index
	int imicrolvl;			   // Micro texture level
	LPDIRECT3DTEXTURE9 tex;	   // diffuse surface texture
	LPDIRECT3DTEXTURE9 overlay;
	bool bMipmaps;			   // create mipmaps for the tile
	bool owntex;               // true: tile owns the texture, false: tile uses ancestor subtexture
	bool ownoverlay;
	bool has_elevfile;		   // true if the elevation data for this tile were read from file
	TEXCRDRANGE2 texrange;     // texture coordinate subrange (if using ancestor subtexture)
	TEXCRDRANGE2 microrange;   // texture coordinate subrange (if using ancestor subtexture)
	TEXCRDRANGE2 overlayrange;
	VBMESH *mesh;              // vertex-buffered tile mesh
	VECTOR3 cnt;               // tile centre in local planet coords
	VECTOR3 vtxshift;          // tile frame shift of origin from planet centre
	bool edgeok;               // edges have been checked in this frame
	TileState state;           // tile load/active/render state flags
	int lngnbr_lvl, latnbr_lvl, dianbr_lvl; // neighbour levels to which edges have been adapted
	double last_used;		   // time when this tile was last used as a part of active tile chain
	float width;			   // tile width [rad] (widest section i.e base)
	float height;			   // tile height [rad]
	float tgtscale;
	
public:
	FMATRIX4 mWorld;
	MATRIX4 dmWorld;
	TILEBOUNDS bnd;
};

// =======================================================================

/**
 * \brief Planetary surface tile loader template.
 *
 * Planetary surface tile loader template class.
 */
class TileLoader {
	template<class T> friend class TileManager2;

public:
	explicit TileLoader (const oapi::vkClient *gclient);
	~TileLoader ();
	bool LoadTileAsync (Tile *tile);
	bool ShutDown ();

	bool Unqueue (Tile *tile);
	// remove a tile from the load queue (caller must own hLoadMutex)

	void Unqueue (TileManager2Base *mgr);
	// removes all tiles of a manager from the load queue (caller must own hLoadMutex)

	inline static DWORD WaitForMutex() { return ::WaitForSingleObject (hLoadMutex, INFINITE); }
	inline static BOOL ReleaseMutex() { return ::ReleaseMutex (hLoadMutex); }

private:
	void TerminateLoadThread(); // Terminates the Load thread

	static struct QUEUEDESC {
		Tile *tile;
	} queue[MAXQUEUE2];

	const oapi::vkClient *gc; // the client
	static int nqueue, queue_in, queue_out;
	HANDLE hLoadThread; // Load ThreadProc handle
	HANDLE hStopThread; // Thread kill signal handle
	static HANDLE hLoadMutex;
	static DWORD WINAPI Load_ThreadProc (void*);
	int load_frequency;
};

// =======================================================================


namespace eElevMode {
	const int DontCare = 0x0;
	const int Elevated = 0x1;
	const int Spherical = 0x2;
	const int ForcedElevated = 0x3; // This can be really bad. Use only with small bodies
};


/**
 * \brief Base class for tile management classes.
 *
 * Rendering of planetary surfaces using texture tiles at
 * variable resolutions (new version).
 */
class TileManager2Base
{
	friend class Tile;

public:
	/**
	 * \brief Global configuration parameters
	 */
	static struct ConfigPrm {
		int elevMode;         ///< elevation mode (0=none, 1=linear, 2=cubic)
		bool bSpecular;       ///< render specular surface reflections?
		bool bLights;         ///< render planet night lights?
		bool bCloudShadow;    ///< render cloud shadows?
		double lightfac;      ///< city light brightness factor
		DWORD tileLoadFlags;  ///< 0x0001: load tiles from directory tree
		                      ///< 0x0002: load tiles from compressed archive
	} cprm;

	/**
	 * \brief Global rendering parameters
	 */
	struct RenderPrm {
		const vPlanet::RenderPrm *rprm;	///< render parameters inherited from the vPlanet object
		int maxlvl;						///< max tile level
		MATRIX4 dwmat;					///< planet world matrix, double precision
		MATRIX4 dwmat_tmp;				///< modifiable planet world matrix, double precision
		MATRIX4 dviewproj;				///< view+projection matrix, double precision
		MATRIX3 grot;					///< planet rotation matrix
		VECTOR3 cpos;					///< planet offset vector (in global frame)
		VECTOR3 cdir;					///< camera direction from planet centre (in planet frame)
		VECTOR3 sdir;					///< sun direction in local planet coordinates
		double cdist;					///< camera distance from planet centre (in units of planet radii)
		double viewap;					///< aperture of surface cap visible from camera pos
		double scale;					///< scale factor
	} prm;

	struct RenderStats {
		int	Elev;						///< Number of elevated tiles rendered
		int Sphe;						///< Number of spherical tiles rendered
	} elvstat, prevstat;

	int ElevMode, ElevModeLvl;
	int TilesLoaded;

	/**
	 * \brief Constructs a new tile manager object
	 * \param vplanet planet instance pointer
	 * \param _maxres maximum resolution
	 */
	TileManager2Base (vPlanet *vplanet, int _maxres, int _gridres);

	/**
	 * \brief Destroys the tile manager object
	 */
	~TileManager2Base ();

	/**
	 * \brief Set up global parameters shared by all instances
	 * \param gclient client instance pointer
	 */
	static void GlobalInit (class oapi::vkClient *gclient);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit ();

	/**
	 * \brief Shutdown on loader instance
	 */
	static bool ShutDown ();

	static LPDIRECT3DDEVICE9 Dev() { return pDev; }
	static HFONT GetDebugFont() { return hFont; }

	template<class TileType>
	QuadTreeNode<TileType> *FindNode (QuadTreeNode<TileType> root[2], int lvl, int ilat, int ilng);
	// Returns the node at the specified position, or 0 if it doesn't exist

	template<class TileType>
	void DebugDump(QuadTreeNode<TileType>* node);

	template<class TileType>
	void ProcessNode (QuadTreeNode<TileType> *node);

	template<class TileType>
	void RenderNode (QuadTreeNode<TileType> *node);

	template<class TileType>
	void QueryTiles(QuadTreeNode<TileType> *node, std::list<Tile*> &tiles);

	// v2 Labels interface -----------------------------------------------
	template<class TileType>
	void RenderNodeLabels(QuadTreeNode<TileType> *node, vkPad *skp, oapi::Font **labelfont, int *fontidx);

	void SetRenderPrm (MATRIX4 &dwmat, double prerot, bool use_zbuf, const vPlanet::RenderPrm &rprm);

	inline class Scene * GetScene() const { return gc->GetScene(); }
	inline oapi::vkClient *GetClient() const { return gc; }
	inline oapi::vkClient* Client() const { return gc; }
	inline vPlanet *GetPlanet() { return vp; }
	inline vPlanet* GetPlanet() const { return vp; }

	inline PlanetShader* GetShader() { return pShader; }
	inline bool IsUsingZBuf() const { return bUseZ; }
	inline const OBJHANDLE &Cbody() const { return obj; }
	inline const ConfigPrm &Cprm() const { return cprm; }
	inline const char *CbodyName() const { return cbody_name; }
	inline const double CbodySize() const { return obj_size; }
	inline const ELEVHANDLE ElevMgr() const { return emgr; }
	inline const int GridRes() const { return gridRes; }
	inline const double ElevRes() const { return elevRes; }
	inline OBJHANDLE GetHandle() const { return obj; }

	/// \brief Return the root directory containing the body's texture data (surface, elevation, mask, cloud tiles)
	inline const std::string& DataRootDir() const { return m_dataRootDir; }

protected:
	MATRIX4 WorldMatrix (int ilng, int nlng, int ilat, int nlat);
	MATRIX4 WorldMatrix(Tile *tile);

	template<class TileType>
	QuadTreeNode<TileType> *LoadChildNode (QuadTreeNode<TileType> *node, int idx);
	// loads one of the four subnodes of 'node', given by 'idx'

	double obj_size;                 // planet radius
	static TileLoader *loader;
	vPlanet *vp;					 // the planet visual
	PlanetShader* pShader;
	std::string m_dataRootDir;       // the root directory (usually ending in the cbody's name) for all tile data (textures, elevations, etc.)
	bool bUseZ;

private:
	OBJHANDLE obj;                   // the planet object
	char cbody_name[256];
	ELEVHANDLE emgr;                 // elevation data query handle
	int gridRes;                     // mesh grid resolution. must be multiple of 2. Default: 64 for surfaces, 32 for clouds
	double elevRes;                  // target elevation resolution

	static oapi::vkClient* gc;
	static LPDIRECT3DDEVICE9 pDev;
	static HFONT hFont;
	static double resolutionBias;
	static double resolutionScale;
	static bool bTileLoadThread;     // load tiles on separate thread
public:
	static LPDIRECT3DTEXTURE9 hOcean;
	static LPDIRECT3DTEXTURE9 hCloudMicro;
	static LPDIRECT3DTEXTURE9 hCloudMicroNorm;
};

// =======================================================================

template<class TileType>
class TileManager2: public TileManager2Base {
	friend class Tile;

public:
	TileManager2 (vPlanet *vplanet, int _maxres, int _gridres);
	~TileManager2 ();

	void Render (MATRIX4 &dwmat, bool use_zbuf, const vPlanet::RenderPrm &rprm);

	int GetElevation(double lng, double lat, double *elev, FVECTOR3 *nrm, SurfTile **cache);
	void Unload(int lvl);

	void Pick(FVECTOR3 &vRay, TILEPICK *pPick);

	// v2 Labels interface -----------------------------------------------
	void CreateLabels();
	void DeleteLabels();
	void RenderLabels(vkPad *skp, oapi::Font **labelfont, int *fontidx);
	void SetSubtreeLabels(QuadTreeNode<TileType> *node, bool activate);

	QuadTreeNode<TileType> *FindNode (int lvl, int ilat, int ilng)
	{ return TileManager2Base::FindNode<TileType> (tiletree, lvl, ilat, ilng); }
	// Returns the node at the specified position, or 0 if it doesn't exist

	Tile *SearchTile (double lng, double lat, int maxlvl, bool bOwntex) const;

	inline TileType *GlobalTile (int lvl) const {	return (lvl >= -3 && lvl < 0 ? globtile[lvl + 3] : NULL); }
//	{ return globtile[lvl]; } @todo: Is that ( above) OK for us?

	// Returns a low-res global tile

	int Coverage (double latmin, double latmax, double lngmin, double lngmax, int maxlvl, const Tile **tbuf, int nt) const;
	// fills tbuf with a list of tiles up to maxlvl currently covering the area latmin,latmax,lngmin,lngmax
	// nt is the length of tbuf. Return value is the number of tiles copied into tbuf.
	// If the number of covering tiles is larger than nt, return value is -1. In that case, no tiles are copied.
	// The tile pointers are only valid for the current render pass. They are not guaranteed to exist any more
	// after the next call to Render.

	inline ZTreeMgr *ZTreeManager (int i) const { return (i<ntreeMgr) ? treeMgr[i] : NULL; }
	inline bool DoLoadIndividualFiles (int i) const { return (i < ntreeMgr) ? hasIndividualFiles[i] : true; }

	float *BrowseElevationData(int lvl, int ilat, int ilng, int flags, ELEVFILEHEADER *hdr = NULL);

	// Load tile texture
	SURFHANDLE SeekTileTexture(int iLng, int iLat, int level, int flags = 3);
	//float *SeekTileElevation(int iLng, int iLat, int level, int flags = 3);

	// Check if tile texture exists
	bool HasTileData(int iLng, int iLat, int level, int flags = 3);

protected:
	TileType *globtile[3];              // full-sphere tiles for resolution levels 1-3
	QuadTreeNode<TileType> tiletree[2]; // quadtree roots for western and eastern hemisphere

	ZTreeMgr **treeMgr;  // access to tile layers in compressed archives
	int ntreeMgr;

	bool *hasIndividualFiles; // whether to check for individual texture files (per Layer-type) flags

	void CheckCoverage (const QuadTreeNode<TileType> *node, double latmin, double latmax, double lngmin, double lngmax, int maxlvl, const Tile **tbuf, int nt, int *nfound) const;

	void LoadZTrees();
	void InitHasIndividualFiles();
	Tile *SearchTileSub (const QuadTreeNode<TileType> *node, double lng, double lat, int maxlvl, bool bOwntex) const;
};

#endif // !__TILEMGR2_H
