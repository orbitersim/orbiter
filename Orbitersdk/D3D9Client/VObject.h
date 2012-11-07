// ==============================================================
// VObject.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2006 Martin Schweiger
//			     2011 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// class vObject (interface)
//
// A "vObject" is the visual representation of an Orbiter object
// (vessel, planet/moon/sun, surface base). vObjects usually have
// one or more meshes associated with them that define their visual
// appearance, but they can be arbitrarily complex (e.g. planets
// with clould layers, atmospheric haze, etc.)
// Visual objects don't persist like their "logical" counterparts,
// but are created and deleted as they pass in and out of the
// visual range of a camera. vObjects are therefore associated
// with a particular scene. In multi-scene environments, a single
// logical object may have multiple vObjects associated with it.
// ==============================================================

#ifndef __VOBJECT_H
#define __VOBJECT_H

#include "OrbiterAPI.h"
#include "GraphicsAPI.h"
#include "Scene.h"
#include "D3D9TK.h"
#include <d3d9.h> 
#include <d3dx9.h>

extern class D3D9Config *Config;

// ==============================================================
// class vObject (interface)
// ==============================================================
/**
 * \brief Visual object base class.
 *
 * A vObject is a render object representing a 'logical'
 * Orbiter object (identified by its OBJHANDLE) in a scene.
 */

class vObject: public oapi::VisObject {
public:

	/**
	 * \brief Constructs a new visual object for a scene
	 * \param _hObj object handle
	 * \param scene scene to which the visual is added
	 */
	vObject (OBJHANDLE _hObj, const Scene *scene);

	/**
	 * \brief Destroys the visual object
	 */
	virtual ~vObject () {}

	/**
	 * \brief Set up global parameters shared by all instances
	 * \param gclient client instance pointer
	 */
	static void GlobalInit (oapi::D3D9Client *gclient);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit ();

	/**
	 * \brief Creates a specific object from its handle
	 * \param _hObj object handle
	 * \param scene scene to which the visual is added
	 * \note Depending on the object type to which _hObj refers, this method
	 *   creates and returns an instance of the following classes:
	 *   - vVessel (vessel object)
	 *   - vPlanet (planet or moon object)
	 *   - vBase (surface base object)
	 */
	static vObject *Create (OBJHANDLE _hObj, const Scene *scene);

	/**
	 * \brief Activate or deactivate the object
	 * \param isactive \e true to activate, \e false to deactivate
	 * \note This method is only relevant for objects that persist when out
	 *   of visual range. They are activated when entering visual range, and
	 *   deactivated when moving out of visual range.
	 * \note Deactivated objects should skip their update and render methods
	 *   to improve performance.
	 * \sa IsActive
	 */
	virtual void Activate (bool isactive);

	/**
	 * \brief Returns activation state
	 * \return \e true for active, \e false for inactive objects.
	 * \sa Activate
	 */
	inline bool IsActive () const { return active; }

	inline const D3DXMATRIX * MWorld() const { return &mWorld; }

	/**
	 * \brief Returns the handle of the associated logical object
	 * \return object handle
	 */
	inline const OBJHANDLE Object() const { return hObj; }

	/**
	 * \brief Returns one of the visual's meshes, given by its index.
	 * \param idx mesh index (>= 0)
	 * \return Mesh handle
	 * \note Currently only vessel visuals return anything here.
	 */
	virtual MESHHANDLE GetMesh (UINT idx) { return NULL; }

	
	virtual bool GetMinMaxDistance(float *zmin, float *zmax, float *dmin) { return false; }
	
	virtual void UpdateBoundingBox();
	virtual bool IsVisible();
	virtual DWORD GetMeshCount();

	VECTOR3 GetBoundingSpherePos();
	float GetBoundingSphereRadius();
	const char *GetName();

	/**
	 * \brief Returns distance from camera
	 * \return camera distance [m]
	 * \sa PosFromCamera
	 */
	inline double CamDist() const { return cdist; }

	/**
	 * \brief Returns object position relative to camera
	 * \return relative position vector [<b>m</b>]
	 * \note The returned distance vector is expressed in the ecliptic frame.
	 * \sa CamDist
	 */
	inline const VECTOR3 &PosFromCamera() const { return cpos; }

	/**
	 * \brief Per-frame object parameter updates
	 * \return \e true if update was performed, \e false if skipped.
	 * \default Copies global and camera-relative position and rotation
	 *   parameters from the logical object. Updates the world matrix.
	 *   Calls CheckResolution.
	 * \note This method allows the visual to update any parameters in each
	 *   frame before the render call.
	 * \note Inactive objects skip this method.
	 * \sa Render, CheckResolution
	 */
	virtual bool Update ();

	/**
	 * \brief Level-of-detail check
	 * \default None.
	 * \note Derived classes can overload this method to select the level of
	 *   detail (e.g. mesh and texture resolution) with which the object is to
	 *   be rendered.
	 * \note Typically, the render detail will be a function of apparent size,
	 *   which depends on camera distance and camera aperture.
	 * \note Called by Update.
	 */
	virtual void CheckResolution () {}

	/**
	 * \brief Object render call
	 * \param dev Render device
	 * \return \e true if render operation was performed, \e false if skipped.
	 * \default None, returns \e false.
	 */
	virtual bool Render(LPDIRECT3DDEVICE9 dev) { return false; }

	/**
	 * \brief Render the vessel's active light beacons
	 * \param dev render device
	 * \default None.
	 */
	virtual void RenderBeacons (LPDIRECT3DDEVICE9 dev) {}

	 /**
     * \brief Render the vessel's grapple points when switched on (see oapiGetShowGrapplePoints)
     * \param dev render device
     * \default None.
     */
    virtual void RenderGrapplePoints (LPDIRECT3DDEVICE9 dev) {}  


	virtual void RenderAxis (LPDIRECT3DDEVICE9 dev, oapi::Sketchpad *pSkp) {} 

	
	void RenderDot(LPDIRECT3DDEVICE9 dev);
	

protected:

	void RenderSpot(LPDIRECT3DDEVICE9 dev, const VECTOR3 *ofs, float size, const VECTOR3 &col, bool lighting, int shape);
	void RenderAxisVector(oapi::Sketchpad *pSkp, LPD3DXCOLOR pColor, VECTOR3 vector, float lscale, float size, bool bLog=false);
	void RenderAxisLabel(oapi::Sketchpad *pSkp, LPD3DXCOLOR clr, VECTOR3 vector, float lscale, float size, const char *label, bool bLog=false);
	
	static oapi::D3D9Client *gc;			// graphics client instance pointer
	static D3D9ClientSurface *blobtex[3];  // beacon textures
	
	D3D9Light		sunLight;	// Local copy of sun light. (Can be freely edited)
	D9BBox			BBox;
	bool			bBSRecompute;

	bool active;		// visual is active (within camera range)
	OBJHANDLE hObj;		// handle for the "logical" object
	const Scene *scn;	// The scene to which the object belongs
	D3DXMATRIX mWorld;	// D3D world matrix for the object
	D3DXMATRIX mWorldInv;
	VECTOR3 cpos;		// camera-relative object position
	VECTOR3 albedo;
	double cdist;		// current camera distance
	char name[64];
};

#endif // !__VOBJECT_H