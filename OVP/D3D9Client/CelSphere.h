// ==============================================================
// CelSphere.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
// ==============================================================

#ifndef __D3D9CELSPHERE_H
#define __D3D9CELSPHERE_H

#include "CelSphereAPI.h"
#include "D3D9Client.h"
#include "D3D9Util.h"


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
class D3D9CelestialSphere : public oapi::CelestialSphere {

public:
	/**
	 * \brief Create a new celestial sphere object.
	 * \param gc pointer to graphics client
	 */
	explicit D3D9CelestialSphere(oapi::D3D9Client *gc, Scene* scene);

	/**
	 * \brief Destructor
	 */
	~D3D9CelestialSphere();

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
	void RenderConstellationLines(ID3DXEffect *fx);

	void RenderConstellationLabels(D3D9Pad* pSketch, bool fullName);

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
	 * \brief Render a background image on the celestial sphere.
	 * \param dev render device
	 * \param bglvl atmospheric background brightness
	 */
	void RenderBkgImage(LPDIRECT3DDEVICE9 dev, int bglvl);

protected:
	/**
	 * \brief Prepare the star vertex list from the star database.
	 */
	void InitStars ();

	/**
	 * \brief Load constellation line data from file
	 */
	void InitConstellationLines ();

	/**
	 * \brief Allocate vertex list for rendering grid lines
	 *        (e.g. celestial or ecliptic)
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
	oapi::D3D9Client *m_gc;         ///< pointer to graphics client
	CSphereManager* m_bkgImgMgr;    ///< background image manager
	Scene* m_scene;                 ///< pointer to scene object
	LPDIRECT3DDEVICE9 m_pDevice;    ///< DirectX9 device
	UINT maxNumVertices;            ///< number of vertices to use for one chunk at star-drawing
	DWORD m_nsVtx;                  ///< total number of vertices over all buffers
	std::vector<LPDIRECT3DVERTEXBUFFER9> m_sVtx; ///< star vertex buffers
	int m_lvlIdx[256];              ///< star brightness hash table
	DWORD m_ncVtx;                  ///< number of constellation line vertices
	LPDIRECT3DVERTEXBUFFER9 m_cVtx; ///< constellation line vertex buffer
	LPDIRECT3DVERTEXBUFFER9 m_grdLngVtx, m_grdLatVtx; ///< vertex buffers for grid lines
};

#endif // !__D3D9CELSPHERE_H
