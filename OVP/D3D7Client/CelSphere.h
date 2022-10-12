// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// CelSphere.h
// Class CelestialSphere (interface)
//
// This class is responsible for rendering the celestial sphere
// background (stars, constellations, grids, labels, etc.)
// ==============================================================

#ifndef __D3D7CELSPHERE_H
#define __D3D7CELSPHERE_H

#include "CelSphereAPI.h"
#include "D3D7Client.h"
#include "D3D7Util.h"

/**
 * \brief Rendering methods for the background celestial sphere.
 *
 * Loads star and constellation information from data bases and uses them to
 * celestial sphere background.
 */
class D3D7CelestialSphere: public oapi::CelestialSphere {
public:
	D3D7CelestialSphere (oapi::D3D7Client *gc, Scene *scene);
	~D3D7CelestialSphere ();

	/**
	 * \brief Render stars as pixels on the celestial sphere
	 * \param dev render device
	 * \param nmax max. number of stars (default is all available stars)
	 * \param bgcol pointer to background color (default is black)
	 * \note If a background colour is passed into this function, the rendering
	 *   of stars darker than the background is suppressed.
	 * \note All device parameters are assumed to be set correctly on call.
	 */
	void RenderStars (LPDIRECT3DDEVICE7 dev, DWORD nmax = (DWORD)-1, const VECTOR3* bgcol = 0);

	/**
	 * \brief Render constellation lines on the celestial sphere
	 * \param dev render device
	 * \param col line colour
	 * \note All device parameters are assumed to be set correctly on call.
	 * \note Suggestion: render additively onto background, so that the lines
	 *   are never darker than the background sky.
	 */
	void RenderConstellationLines (LPDIRECT3DDEVICE7 dev, VECTOR3& col);

	/**
	 * \brief Render a great circle on the celestial sphere in a given colour.
	 * \param dev render device
	 * \param col RGB line colour (0..1 for each component)
	 * \note By default (i.e. for identity world matrix), the circle is
	 *   drawn along the plane of the ecliptic. To render a circle in any
	 *   other orientation, the world matrix must be adjusted accordingly
	 *   before the call.
	 */
	void RenderGreatCircle (LPDIRECT3DDEVICE7 dev, VECTOR3 &col);

	/**
	 * \brief Render grid lines on the celestial sphere in a given colour.
	 * \param dev render device
	 * \param col RGB line colour (0..1 for each component)
	 * \param eqline indicates if the equator line should be drawn
	 * \note By default (i.e. for identity world matrix), this draws the
	 *   ecliptic grid. To render a grid for any other reference frame,
	 *   the world matrix must be adjusted accordingly before call.
	 * \note If eqline==false, then the latitude=0 line is not drawn.
	 *   This is useful if the line should be drawn in a different colour
	 *   with RenderGreatCircle().
	 */
	void RenderGrid (LPDIRECT3DDEVICE7 dev, VECTOR3 &col, bool eqline = true);

protected:
	/**
	 * \brief Prepare the star vertex list from the star database.
	 */
	void InitStars();

	/**
	 * \brief Load constellation line database from file.
	 */
	void InitConstellationLines();

	/**
	 * \brief Allocate vertex list for rendering grid lines
	 *    (e.g. celestial or ecliptic)
	 */
	void AllocGrids ();

	/**
	 * \brief Convert a direction into viewport coordinates
	 * \param dir direction in the ecliptic frame provided as a point on the
	 *    celestial sphere.
	 * \param x x-position in the viewport window [pixel]
	 * \param y y-position in the viewport window [pixel]
	 * \return true if point is visible in the viewport, false otherwise.
	 */
	virtual bool EclDir2WindowPos(const VECTOR3& dir, int& x, int& y) const;

private:
	oapi::D3D7Client *m_gc;          ///< pointer to graphics client
	Scene* m_scene;                  ///< pointer to scene object
	DWORD m_viewW;                   ///< render viewport width [pixel]
	DWORD m_viewH;                   ///< render viewport height [pixel]
	DWORD m_nsVtx;                   ///< total number of vertices over all buffers
	std::vector<LPDIRECT3DVERTEXBUFFER7> m_sVtx; ///< star vertex buffers
	int m_lvlIdx[256];               ///< star brightness hash table
	DWORD m_ncVtx;                   ///< number of constellation line vertices
	LPDIRECT3DVERTEXBUFFER7 m_cVtx;  ///< vertex buffer for constellation lines
	LPDIRECT3DVERTEXBUFFER7 m_grdLngVtx, m_grdLatVtx; ///< vertex buffers for grid lines
};

#endif // !__D3D7CELSPHERE_H