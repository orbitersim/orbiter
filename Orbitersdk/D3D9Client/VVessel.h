// ==============================================================
// VVessel.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================

#ifndef __VVESSEL_H
#define __VVESSEL_H

#include "VObject.h"
#include "Mesh.h"


class oapi::D3D9Client;

// ==============================================================
// class vVessel (interface)
// ==============================================================
/**
 * \brief Visual representation of a vessel object.
 *
 * The vVessel instance is not persistent: It is created when the
 * object moves into visual range of the camera, and is destroyed
 * when it moves out of visual range.
 *
 * The vVessel contains a set of meshes and animations. At each
 * time step, it synchronises the visual with the logical animation
 * state, and renders the resulting meshes.
 */

class vVessel: public vObject {
public:
	friend class D3D9Client;
	/**
	 * \brief Creates a new vessel visual for a scene
	 * \param _hObj vessel object handle
	 * \param scene scene to which the visual is added
	 */
	vVessel (OBJHANDLE _hObj, const Scene *scene);

	~vVessel ();

	static void GlobalInit (oapi::D3D9Client *gc);
	static void GlobalExit ();

	void clbkEvent(DWORD evnt, UINT context);

	MESHHANDLE GetMesh (UINT idx);
	bool GetMinMaxDistance(float *zmin, float *zmax, float *dmin);
	void UpdateBoundingBox();
	inline DWORD GetMeshCount();

	void PreInitObject();
	
	/**
	 * \brief Per-frame object parameter updates
	 * \return \e true if update was performed, \e false if skipped.
	 * \action
	 *   - Calls vObject::Update
	 *   - Calls \ref UpdateAnimations
	 */
	bool Update ();

	/**
	 * \brief Object render call
	 * \param dev render device
	 * \return \e true if render operation was performed (object active),
	 *   \e false if skipped (object inactive)
	 * \action Calls Render(dev,false), i.e. performs the external render pass.
	 * \sa Render(LPDIRECT3DDEVICE9,bool)
	 */
	bool Render (LPDIRECT3DDEVICE9 dev);

	/**
	 * \brief Object render call
	 * \param dev render device
	 * \param internalpass flag for internal render pass
	 * \note This method renders either the external vessel meshes
	 *   (internalpass=false) or internal meshes (internalpass=true), e.g.
	 *   the virtual cockpit.
	 * \note The internal pass is only performed on the focus object, and only
	 *   in cockpit camera mode.
	 * \sa Render(LPDIRECT3DDEVICE9)
	 */
	bool Render (LPDIRECT3DDEVICE9 dev, bool internalpass);

	bool RenderExhaust();

	/**
	 * \brief Render the vessel's active light beacons
	 * \param dev render device
	 */
	void RenderBeacons (LPDIRECT3DDEVICE9 dev);
	void RenderReentry (LPDIRECT3DDEVICE9 dev);
	void RenderGrapplePoints (LPDIRECT3DDEVICE9 dev);
	void RenderGroundShadow (LPDIRECT3DDEVICE9 dev, OBJHANDLE hPlanet, float depth);
	void RenderAxis (LPDIRECT3DDEVICE9 dev, Sketchpad *pSkp);
	bool RenderENVMap (LPDIRECT3DDEVICE9 pDev, DWORD cnt=2, DWORD flags=0xFF);

	LPDIRECT3DCUBETEXTURE9 GetEnvMap(int idx);
	float GetExhaustLength() const { return ExhaustLength; }

	D3D9Pick Pick(const D3DXVECTOR3 *vDir);

	bool HasExtPass();
	bool const Playback() { return vessel->Playback(); }
	class MatMgr * GetMaterialManager() const { return pMatMgr; }
	const char *GetSkinName() const;
	void SetSkinName(const char *name);

protected:

	void LoadMeshes();
	void InsertMesh(UINT idx);
	void DisposeMeshes();
	void DelMesh(UINT idx);
	void InitAnimations();
	void InitAnimations(UINT meshidx);
	void InitNewAnimations();
	void DisposeAnimations();
	void DelAnimation(UINT idx);
	void ResetAnimations(UINT reset = 1);

	/**
	 * \brief Grow \ref animstate buffer (if needed)
	 *
	 * Increases the size of \ref animstate buffer (if needed).
	 * This method also updates the \ref nanim member.
	 * \param newSize new size of buffer (return value of VESSEL::GetAnimPtr())
	 * \return previous value of \ref nanim
	 */
	UINT GrowAnimstateBuffer (UINT newSize);


	/**
	 * \brief Update animations of the visual
	 *
	 * Synchronises the visual animation states with the logical
	 * animation states of the vessel object.
	 * \param mshidx mesh index
	 * \note If mshidx == (UINT)-1 (default), all meshes are updated.
	 */
	void UpdateAnimations (UINT mshidx = (UINT)-1);

	/**
	 * \brief Modify local lighting due to planet shadow or
	 *   atmospheric dispersion.
	 * \param light pointer to D3D9Light structure receiving modified parameters
	 * \return \e true if lighting modifications should be applied, \e false
	 *   if global lighting conditions apply.
	 */
	bool ModLighting (D3D9Light *light);

	void Animate (UINT an, double state, UINT mshidx);
	void AnimateComponent (ANIMATIONCOMP *comp, const D3DXMATRIX &T);


private:

	VESSEL *vessel;			// access instance for the vessel
	class MatMgr *pMatMgr;

	LPDIRECT3DCUBETEXTURE9 pEnv[4];
	
	int nEnv;				// Number of environmental maps
	int iFace;				// EnvMap Face index that is to be rendered next

	struct MESHREC {
		D3D9Mesh *mesh;		// DX9 mesh representation
		D3DXMATRIX *trans;	// mesh transformation matrix (rel. to vessel frame)
		WORD vismode;
	} *meshlist;			// list of associated meshes

	UINT nmesh;				// number of meshes
	bool bAMSO;
	ANIMATION *anim;		// list of animations (defined in the vessel object)
	double *animstate;		// list of visual animation states
	UINT nanim;				// number of animations
	double tCheckLight;		// time for next lighting check
	float ExhaustLength;
	char skinname[64];

	static class D3D9ClientSurface *mfdoff;
	static class D3D9ClientSurface *defreentrytex, *defexhausttex, *tHUD;
};

#endif // !__VVESSEL_H