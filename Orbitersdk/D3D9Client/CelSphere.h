// ==============================================================
// CelSphere.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
// ==============================================================

#ifndef __CELSPHERE_H
#define __CELSPHERE_H

#include "D3D9Client.h"
#include "D3D9Util.h"

#define D3DMAXNUMVERTICES 32767		// This should be dynamic


// ==============================================================
// Class CelestialSphere (interface)
// ==============================================================

/**
 * \brief Rendering methods for the background celestial sphere.
 *
 * Loads star and constellation information from data bases and uses them to
 * render the celestial sphere background (stars, constellations, grids,
 * labels, etc.)
 */
class CelestialSphere {

public:
	/**
	 * \brief Create a new celestial sphere object.
	 * \param _gc pointer to graphics client
	 */
	explicit CelestialSphere(oapi::D3D9Client *_gc);

	/**
	 * \brief Destructor
	 */
	~CelestialSphere();

	/**
	 * \brief Render stars as pixels on the celestial sphere
	 * \param fx  render effect
	 * \param nmax  max. number of stars (default is all available stars)
	 * \param bgcol  pointer to background color (default is black)
	 * \note if a background colour is passed into this function, the rendering
	 *   of stars darker than the background is suppressed.
	 * \note All device parameters are assumed to be set correctly on call.
	 */
	void RenderStars(ID3DXEffect *fx, DWORD nmax = (DWORD)-1, const VECTOR3 *bgcol = 0);

	/**
	 * \brief Render constellation lines on the celestial sphere
	 * \param fx  render effect
	 * \note All device parameters are assumed to be set correctly on call.
	 * \note Suggestion: render additively onto background, so that the lines
	 *   are never darker than the background sky.
	 */
	void RenderConstellations(ID3DXEffect *fx);

	/**
	 * \brief Render a great circle on the celestial sphere in a given colour.
	 * \param fx  render effect
	 * \note By default (i.e. for identity world matrix), the circle is
	 *   drawn along the plane of the ecliptic. To render a circle in any
	 *   other orientation, the world matrix must be adjusted accordingly
	 *   before the call.
	 */
	void RenderGreatCircle(ID3DXEffect *fx);

	/**
	 * \brief Render grid lines on the celestial sphere in a given colour.
	 * \param fx  render effect
	 * \param eqline  indicates if the equator line should be drawn
	 * \note By default (i.e. for identity world matrix), this draws the
	 *   ecliptic grid. To render a grid for any other reference frame,
	 *   the world matrix must be adjusted accordingly before call.
	 * \note if eqline==false, then the latitude=0 line is not drawn.
	 *   this is useful if the line should be drawn in a different colour
	 *   with RenderGreatCircle().
	 */
	void RenderGrid(ID3DXEffect *fx, bool eqline = true);
	
	/**
	 * \brief Number of stars loaded from the data base
	 * \return Number of stars available
	 */
	inline DWORD NStar() const { return nsvtx; }

protected:
	/**
	 * \brief Load star coordinates from file
	 */
	void LoadStars ();

	/**
	 * \brief Load constellation line data from file
	 */
	void LoadConstellationLines ();

	/**
	 * \brief Allocate vertex list for rendering grid lines
	 *        (e.g. celestial or ecliptic)
	 */
	void AllocGrids ();

private:
	oapi::D3D9Client *gc; ///< pointer to grahics client
	float sphere_r;       ///< render radius for celestial sphere
	DWORD nsbuf;          ///< number of vertex buffers for star positions
//	DWORD nstar;          ///< total number of stars across all buffers
	DWORD nsvtx;          ///< total number of vertices over all buffers
	LPDIRECT3DVERTEXBUFFER9 *svtx; ///< star vertex buffers
	int lvlid[256];       ///< star brightness hash table
	DWORD ncline;         ///< number of constellation lines
	VERTEX_XYZ  *cnstvtx; ///< vertex list of constellation lines
	LPDIRECT3DVERTEXBUFFER9 grdlng, grdlat; ///< vertex buffers for grid lines
	D3DXMATRIX mWorld;    ///< world matrix
	LPDIRECT3DDEVICE9 pDevice; ///< DirectX9 device
};

#endif // !__CELSPHERE_H