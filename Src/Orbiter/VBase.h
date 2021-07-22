// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class VBase
// Visual for surface base objects

#ifndef __VBASE_H
#define __VBASE_H

#include "Vobject.h"
#include "Base.h"
#include "Shadow.h"
#include "Planet.h"
#include "D3d7util.h"

// =======================================================================
// Class VBase

class VBase: public VObject {
public:
	VBase (const Base *_base);
	~VBase ();

	void Update (bool moving, bool force);
	void Timejump (bool moving);
	void CheckResolution (double iar);
	void Render (LPDIRECT3DDEVICE7 dev);
	void RenderSurfaceTiles (LPDIRECT3DDEVICE7 dev);
	void RenderSurfaceDecals (LPDIRECT3DDEVICE7 dev);
	void RenderShadows (LPDIRECT3DDEVICE7 dev);
	void RenderStructures (LPDIRECT3DDEVICE7 dev);
	void RenderGroundShadow (LPDIRECT3DDEVICE7 dev);
	void SetupRenderVectorList ();
	const Base *GetBase() const { return base; }
	Vector SunDir () const { return sundir; }

	double csun;          // cosine of sun's zenith distance

protected:
	bool ModLighting (LPD3DLIGHT7 light);
	// modification to diffuse and ambient lighting conditions due to
	// planet shadow and atmospheric scattering.

	void SetupShadowMeshes ();

private:
	const Base *base;     // logical object

	struct ShadowMeshOLD {   // collective shadow mesh
		DWORD nvtx, nidx;
		VERTEX_XYZ *vtx;
		WORD *idx;
	} shadow;

	struct ShadowMesh {
		LPDIRECT3DVERTEXBUFFER7 vbuf;
		WORD *idx;
		DWORD nvtx, nidx;
		double ecorr;
	} *shmesh;
	DWORD nshmesh;                  // number of shadow meshes

	struct SurfTile {		        // list of surface tiles
		Mesh *mesh;                 // tile mesh
		LPDIRECTDRAWSURFACE7 dtex;  // tile day texture
	} *tile;
	DWORD nsurftile;                // number of surface tiles

	Mesh **mesh_us, **mesh_os; // meshes for base structures (below/above shadows)
	DWORD nmesh_us, nmesh_os;  // list lenghts
	BaseObject **render_pre;   // list of objects rendering themselves before shadows
	BaseObject **render_post;  // list of objects rendering themselves after shadows
	BaseObject **render_shdw;  // list of objects rendering their own shadows
	DWORD nrender_pre, nrender_post, nrender_shdw; // list lengths

	static bool interpolate_textures; // use interpolation on base object textures?
	bool lights;
	double CheckLightT;   // time for next lighting check
	double BlinkT;        // counter for blinking landing lights
	Vector sundir;        // direction of sun in local coords
	bool enable_shadows;  // true if shadow rendering is enabled
	bool have_shadows;    // true if object shadows should be drawn
	bool surftile_alpha;  // surface tiles support alpha blending?
	bool mod_sunlight;    // true if lighting is modified
	bool padlight_on;     // blink flag for landing lights
	DWORD std_ambient;    // standard ambient level
	float shadowstrength; // shadow depth (only used for stencil version)
	D3DLIGHT7 lght;       // current local lighting parameters
};

#endif // !__VBASE_H