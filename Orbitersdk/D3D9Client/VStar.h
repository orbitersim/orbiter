// ==============================================================
// VStar.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
// ==============================================================

#ifndef __VSTAR_H
#define __VSTAR_H

#include "VObject.h"

//class D3D9Mesh;


// ==============================================================
// class vStar (interface)
// ==============================================================

/**
 * \brief Visual representation of the (one) central star.
 *
 * Renders the central star as a billboard mesh.
 */
class vStar: public vObject {
public:
	/**
	 * \brief Constructs a new central star object for a scene
	 * \param _hObj object handle
	 * \param scene scene to which the visual is added
	 */
	vStar (OBJHANDLE _hObj, const Scene *scene);

	/**
	 * \brief Destroys the central star object
	 */
	~vStar ();

	/**
	 * \brief Set up global parameters shared by all instances
	 * \param gclient client instance pointer
	 */
	static void GlobalInit (oapi::D3D9Client *gc);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit ();

	bool Update (bool bMainScene);
	bool Render (LPDIRECT3DDEVICE9 dev);

private:
	double maxdist;                    ///< max render distance
	static LPD3D9CLIENTSURFACE deftex; ///< default texture
};

#endif // !__VSTAR_H