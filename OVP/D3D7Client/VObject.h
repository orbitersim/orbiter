// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VObject.h
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
#include <d3d.h>

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
	static void GlobalInit (const oapi::D3D7Client *gclient);

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

	/**
	 * \brief Returns the handle of the associated logical object
	 * \return object handle
	 */
	inline const OBJHANDLE Object() const { return hObj; }

	inline const D3DMATRIX &MWorld() const { return mWorld; }

	/**
	 * \brief Returns one of the visual's meshes, given by its index.
	 * \param idx mesh index (>= 0)
	 * \return Mesh handle
	 * \note Currently only vessel visuals return anything here.
	 */
	virtual MESHHANDLE GetMesh (UINT idx) { return NULL; }

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
	 * \brief Set up any vectors (forces, coordinate axes) to be drawn in the
	 *    next render pass.
	 * 
	 * Resets the vector list and adds object frame axes if requested for the
	 * given object type. Derived classes should extend this to add their own
	 * vector displays.
	 */
	virtual void UpdateRenderVectors();

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
	virtual bool Render (LPDIRECT3DDEVICE7 dev) { return false; }

	/**
	 * \brief Render the vessel's active light beacons
	 * \param dev render device
	 * \default None.
	 */
	virtual void RenderBeacons (LPDIRECT3DDEVICE7 dev) {}

	/**
	 * \brief structure for rendering vectors
	 */
	struct BodyVectorRec {
		VECTOR3 v;    ///< vector definition in local object frame
		VECTOR3 orig; ///< vector origin in local object frame
		double rad;   ///< vector arrow radius
		double dist;  ///< camera distance
		VECTOR3 col;  ///< colour components
		float alpha;  ///< alpha component
		std::string label; ///< vector label
		DWORD lcol;
		float lsize;  ///< length scale
	};

	virtual void RenderVectors(LPDIRECT3DDEVICE7 dev);

protected:
	void RenderSpot (LPDIRECT3DDEVICE7 dev, const VECTOR3 *ofs, float size, const VECTOR3 &col, bool lighting, int shape);

	void AddVector(const VECTOR3& v, const VECTOR3& orig, double rad, const std::string& label, const VECTOR3& col, float alpha = 1.0f, DWORD lcol = 0, float lsize = -1.0);

	bool DrawVector(LPDIRECT3DDEVICE7 dev, const VECTOR3& end, const VECTOR3& orig, double rad);

	static const oapi::D3D7Client *gc; // graphics client instance pointer
	static LPDIRECTDRAWSURFACE7 blobtex[3]; // beacon textures

	bool active;       // visual is active (within camera range)
	OBJHANDLE hObj;    // handle for the "logical" object
	const Scene *scn;  // The scene to which the object belongs
	D3DMATRIX mWorld;  // D3D world matrix for the object
	MATRIX4 dmWorld;   // world matrix in double precision
	double size;       // object radius [m]
	double cdist;      // current camera distance
	VECTOR3 cpos;      // camera-relative object position in global frame
	VECTOR3 campos;    // camera position in object frame
	std::vector<BodyVectorRec> veclist; ///< list of body vectors to be rendered
};

#endif // !__VOBJECT_H