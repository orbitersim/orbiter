// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Class VVessel
// Visual for spacecraft

#ifndef __VVESSEL_H
#define __VVESSEL_H

#include "Vobject.h"
#include "Vessel.h"

class Mesh;

// =======================================================================
// Class VVessel

class VVessel: public VObject {
	friend class Vessel;
	friend class VESSEL;

public:
	VVessel (const Vessel *_vessel);
	~VVessel ();

	static void CreateDeviceObjects (LPDIRECT3DDEVICE7 dev);
	static void DestroyDeviceObjects ();

	DWORD GetCaps () const { return VOCAPS_HASENGINES; }
	const Vessel *GetVessel() const { return vessel; }

	void clbkEvent (DWORD msg, DWORD_PTR content);
	void RegisterMeshes ();
	void InsertMesh (UINT idx);
	void DelMesh (UINT idx);
	MESHHANDLE GetMesh (UINT idx);
	void UnregisterMeshes ();
	bool bRenderInternal () const;
	inline bool ExtRenderPass () const { return bExtRenderPass; }
	void CheckResolution (double iar);
	void Update (bool moving, bool force);
	void Timejump (bool moving);
	void Render (LPDIRECT3DDEVICE7 dev);
	void Render (LPDIRECT3DDEVICE7 dev, bool internalpass);
	void RenderBeacons (LPDIRECT3DDEVICE7 dev);
	void RenderExhaust (LPDIRECT3DDEVICE7 dev, LPDIRECTDRAWSURFACE7 defaulttex);
	void RenderGroundShadow (LPDIRECT3DDEVICE7 dev, const Planet *planet);
	void RenderAttachmentMarkers (LPDIRECT3DDEVICE7 dev, bool pa);
	void SetupRenderVectorList ();
	bool MeshgroupTransform (const MESHGROUP_TRANSFORM &mt) const;
	bool MeshgroupTransform (const MESHGROUP_TRANSFORM &mt, Mesh *mesh, UINT grp) const;
	bool MeshTransform (const MESHGROUP_TRANSFORM &mt, Mesh *mesh) const;

	void Animate (int seq); // OBSOLETE
	void Animate2 (UINT an, double state, UINT mshidx = (UINT)-1);
	void AnimateComponent (ANIMATIONCOMP *comp, const D3DMATRIX &T);

protected:
	bool ModLighting (LPD3DLIGHT7 light);
	// modification to diffuse and ambient lighting conditions due to
	// planet shadow and atmospheric scattering.

	void ScanMeshCaps ();
	// check out mesh parameters. Should be called after meshes are added or
	// deleted, or when visibility flags are changed

	void ShiftMesh (UINT idx);
	// register a mesh shift

	void CreateAnimation (UINT idx);
	// register a new animation after it has been generated in the logical vessel

	void UpdateAnimations (UINT mshidx = (UINT)-1);
	// update animations to their current logical state. If mshidx is set, only
	// the animation components for that mesh are updated.

	void ResetAnimations (UINT mshidx = (UINT)-1);
	// reset animation states to initial states. If mshidx is set, only animations
	// that contain components referring to that mesh are reset

	void ResetAnimation (UINT an);
	// reset a single animation state to initial state

	void ClearAnimations (bool reset);
	// Remove animations. If reset==true, all animations are reset to their
	// initial states

private:
	const Vessel *vessel;
	UINT nmesh;
	struct MeshList {
		Mesh *mesh;        // mesh pointer
		DWORD crc;         // mesh name identifier
		bool shifted;      // mesh is shifted against vessel origin
	} *meshlist;
	DWORD mesh_crc;
	int nanim;             // number of animation sequences // OBSOLETE
	double *vanim;         // visual animation sequence states // OBSOLETE
	double *vanim_state;   // current animation states of the visual
	bool renderpix;        // render distant vessel as pixel block?
	double tCheckLight;    // time for next lighting check
	bool bLocalLight;      // modified local lighting parameters?
	bool bExtRenderPass;   // any internally visible meshes rendered in external pass?
	D3DLIGHT7 lght;        // current local lighting parameters

	static LPDIRECTDRAWSURFACE7 mfdsurf; // render surface for MFD displays in virtual cockpits
};

#endif // !__VVESSEL_H
