// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __SPHEREPATCH_H
#define __SPHEREPATCH_H

// Classes for sphere patches at different resolutions.
// Building spheres from patches allows efficient rendering since
// invisible parts of the sphere can be culled up-front.
// Also overcomes limits in texture size since each single patch
// can use a relatively small texture.

// The following patch managers are defined:
// PatchManager1A : lores full circle, single texture (suggest 64x64)
// PatchManager1B : medres full circle, single texture (suggest 128x128)
// PatchManager1C : hires full circle, single texture (suggest 256x256)
// PatchManager2  : 2 hemispheres, 1 texture each (256x256 each)

#include "Mesh.h"
#include "Body.h"
#include "Planet.h"
#include "D3d7util.h"

#define MICROSTRUCT_NONE 0
#define MICROSTRUCT_CLOUD 1

class VPlanet;

struct VBMESH {
	VBMESH();
	~VBMESH();
	void MapVertices (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev, DWORD MemFlag); // copy vertices from vtx to vb
	LPDIRECT3DVERTEXBUFFER7 vb; // mesh vertex buffer
	LPDIRECT3DVERTEXBUFFER7 bb; // bounding box vertex buffer
	VERTEX_2TEX *vtx;           // separate storage of vertices (NULL if not available)
	VECTOR4 *bbvtx;             // bounding box vertices
	DWORD nv;                   // number of vertices
	LPWORD idx;                 // list of indices
	DWORD ni;                   // number of indices
};

// =======================================================================
// Nonmember functions

void CreatePatchDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev);
void DestroyPatchDeviceObjects ();
void RestorePatchDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev);

void CreateSphere (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev, VBMESH &mesh, DWORD nrings, bool hemisphere, int which_half, int texres);

void CreateSpherePatch (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev, VBMESH &mesh, int nlng, int nlat, int ilat, int res,
	int bseg = -1, bool reduce = true, bool outside = true, bool store_vtx = false, bool shift_origin = false);
// Creates a sphere patch in a vertex buffer with the specified parameters. If
// store_vtx==true, a copy of the vertex data is stored in mesh.vtx. This is useful if vertex
// data need to remain accessible with write-only or optimised vertex buffers.

void DestroyVBMesh (VBMESH &mesh);

// =======================================================================
// Class PatchManager

class PatchManager {
public:
	PatchManager (const char *_name, char _res_id, int _npatch,
		int _nlat, int *_nlng, VBMESH *_patch, D3DMATRIX *_trans,
		Vector *_patchcnt, double *_patchrad,
		LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0);

	virtual ~PatchManager();

	inline void SetRefObj (const CelestialBody *_ref) { ref = _ref; }
	//inline void SetTileManager (TileManagerOld *_tm) { tm = _tm; }
	inline void SetMicroStructure (int type) { microstruct = type; }
	inline void SetMicroLevel (double level) { microlevel = level; }
	inline double GetMicroLevel () const { return microlevel; }

	virtual void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad = 0, bool addbkg = false);
	// should only be used if nlat, nlng, trans, tex and patch have been
	// initialised
	// visrad is aperture of visibility sector from sphere centre
	// If 0, then visrad is calculated automatically
	// if addbkg == true, background colour is added (for rendering through atmospheres)

	void RenderSimple (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad, bool addbkg);
	void RenderNightlights (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad = 0);

	static void CreateDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev);
	static void RestoreDeviceObjects (LPDIRECT3D7 d3d, LPDIRECT3DDEVICE7 dev);
	static void DestroyDeviceObjects ();
	// device-dependent static initialisation

protected:
	void SetupPatchBands (D3DMATRIX *trans, Vector *pcnt, double *prad);
	// initialise all latitude bands

	void SetupPatchBand (int ilat, D3DMATRIX *trans, Vector *pcnt, double *prad);
	// generate transformation matrices for patches in latitude band
	// ilat (for northern hemisphere in ntrans, for southern in strans)

	bool SetReflectionColour (D3DCOLORVALUE *col);
	// calculate colour of specular reflections as function of incident angle and atmosphere

	char name[64];            // object name for textures
	char res_id;              // resolution id
	int npatch;               // number of patches in the mesh
	int nlat;                 // number of latitude bands per hemisphere
	int *nlng;                // number of patches in each latitude band
	VBMESH *vbpatch;            // sphere patches
	LPDIRECTDRAWSURFACE7 *tex;  // surface textures
	LPDIRECTDRAWSURFACE7 *ntex; // night textures
	LPDIRECTDRAWSURFACE7 *wtex; // water textures - OBSOLETE
	static LPDIRECTDRAWSURFACE7 lightstruct1;
	static LPDIRECTDRAWSURFACE7 cloudstruct;
	D3DMATRIX *trans;
	Vector *patchcnt;
	const CelestialBody *ref;   // global position of reference object
	double *patchrad;
	bool *vis;
	//TileManagerOld *tm;
	int microstruct;
	double microlevel;
	static DWORD vpX0, vpX1, vpY0, vpY1; // viewport boundaries
};

// =======================================================================
// Class PatchManager1: lowres fullsphere (64x64 resolution)

class PatchManager1: public PatchManager {
public:
	PatchManager1 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0);
	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad = 0, bool addbkg = false);
};

// =======================================================================
// Class PatchManager2: medres fullsphere (128x128 resolution)

class PatchManager2: public PatchManager {
public:
	PatchManager2 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0);
	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad = 0, bool addbkg = false);
};

// =======================================================================
// Class PatchManager3: highres fullsphere (256x256 resolution)

class PatchManager3: public PatchManager {
public:
	PatchManager3 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0);
	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad = 0, bool addbkg = false);
};

// =======================================================================
// Class PatchManager4: hemispheres (512x256 resolution)

class PatchManager4: public PatchManager {
public:
	PatchManager4 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0);
	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, double visrad = 0, bool addbkg = false);
};

// =======================================================================
// Class PatchManager5: 8 patches (1024x512 resolution)

class PatchManager5: public PatchManager {
public:
	PatchManager5 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0);

private:
	static bool needsetup;
	static D3DMATRIX TRANS[8];  // transformation matrices for the 8 patches
	static Vector PATCHCNT[8];
	static double PATCHRAD[8];
};

// =======================================================================
// Class PatchManager6: 24 patches (1024x1024 and 2048x1024 resolution)

class PatchManager6: public PatchManager {
public:
	PatchManager6 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0);

private:
	static bool needsetup;
	static D3DMATRIX TRANS[24]; // transformation matrices for the 8 patches
	static Vector PATCHCNT[24];
	static double PATCHRAD[24];
};

// =======================================================================
// Class PatchManager7: 100 patches (1536x2048, 3072x2048 and 4096x2048 resolution)

class PatchManager7: public PatchManager {
public:
	PatchManager7 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0);

private:
	static bool needsetup;
	static D3DMATRIX TRANS[100]; // transformation matrices for the 8 patches
	static Vector PATCHCNT[100];
	static double PATCHRAD[100];
};

// =======================================================================
// Class PatchManager8: 364 patches (8192x4096 max. resolution)

class PatchManager8: public PatchManager {
public:
	PatchManager8 (char *_name, LPDIRECTDRAWSURFACE7 *_tex, LPDIRECTDRAWSURFACE7 *_ntex = 0,
		bool *_vis = 0);

private:
	static bool needsetup;
	static D3DMATRIX TRANS[364]; // transformation matrices for patches
	static Vector PATCHCNT[364];
	static double PATCHRAD[364];
};

// =======================================================================
// =======================================================================

// Classes for rendering graded horizon haze

// =======================================================================
// Class HorizonManager

#define HORIZON_NSEG 128  // number of mesh segments

class HorizonManager {
public:
	HorizonManager (const Planet *_planet, const VPlanet *_vplanet);
	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, bool dual = false);

private:
	const Planet *planet;
	const VPlanet *vplanet;
	Vector basecol;
	float  hralt;  // relative horizon altitude
	float  dens0;  // atmosphere density factor
	float  hshift; // horizon reference shift factor
	float  hscale; // haze ring width (in planet radii)
	static bool need_setup;
	static WORD Idx[HORIZON_NSEG*2+2];
	static DWORD nIdx;
	static struct HVERTEX {
		D3DVALUE x,y,z;
		DWORD    dcol;
		D3DVALUE tu, tv; } Vtx[HORIZON_NSEG*2];
	static D3DVALUE CosP[HORIZON_NSEG], SinP[HORIZON_NSEG];
};

// =======================================================================
// =======================================================================

// Classes for rendering planetary ring systems at different resolutions

// =======================================================================
// Class RingManager

class RingManager {
public:
	RingManager (const char *_name, LPDIRECTDRAWSURFACE7 *_tex);
	virtual ~RingManager ();
	virtual void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat);

protected:
	LPDIRECTDRAWSURFACE7 *tex;
};

// =======================================================================
// Class RingManager8

class RingManager8: public RingManager {
public:
	RingManager8 (const char *_name, float irad, float orad, LPDIRECTDRAWSURFACE7 *_tex);
	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat);

private:
	Mesh mesh;
};

// =======================================================================
// Class RingManager12

class RingManager12: public RingManager {
public:
	RingManager12 (const char *_name, float irad, float orad, LPDIRECTDRAWSURFACE7 *_tex);
	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat);

private:
	Mesh mesh;
};

// =======================================================================
// Class RingManager16

class RingManager16: public RingManager {
public:
	RingManager16 (const char *_name, float irad, float orad, LPDIRECTDRAWSURFACE7 *_tex);
	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat);

private:
	Mesh mesh;
};

#endif // !__SPHEREPATCH_H