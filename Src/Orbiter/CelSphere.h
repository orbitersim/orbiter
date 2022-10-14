// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __OGCELSPHERE_H
#define __OGCELSPHERE_H

#include "CelSphereAPI.h"
#include "OGraphics.h"
#include "cspheremgr2.h"
#include "D3D7Util.h"
#include "Vecmat.h"

#define MAXCONST 88      // max number of constellations

class CSphereManager;

class OGCelestialSphere : public oapi::CelestialSphere {
public:
	OGCelestialSphere(OrbiterGraphics* gc, Scene* scene);
	~OGCelestialSphere();

	void Render(LPDIRECT3DDEVICE7 dev, double bglvl);

	/**
	 * \brief Render stars as pixels on the celestial sphere
	 * \param dev render device
	 * \param nmax max. number of stars (default is all available stars)
	 * \param bglvl background brightness from atmospheric effects, mean of RGB channels (0-1)
	 * \note If a background colour is passed into this function, the rendering
	 *   of stars darker than the background is suppressed.
	 * \note All device parameters are assumed to be set correctly on call.
	 */
	void RenderStars(LPDIRECT3DDEVICE7 dev, DWORD nmax = (DWORD)-1, double bglvl = 0.0);

	/**
	 * \brief Render constellation lines on the celestial sphere
	 * \param dev render device
	 * \param col line colour
	 * \note All device parameters are assumed to be set correctly on call.
	 * \note Suggestion: render additively onto background, so that the lines
	 *   are never darker than the background sky.
	 */
	void RenderConstellationLines(LPDIRECT3DDEVICE7 dev, const Vector& col);

	/**
	 * \brief Render a great circle on the celestial sphere in a given colour.
	 * \param dev render device
	 * \param col RGB line colour (0..1 for each component)
	 * \note By default (i.e. for identity world matrix), the circle is
	 *   drawn along the plane of the ecliptic. To render a circle in any
	 *   other orientation, the world matrix must be adjusted accordingly
	 *   before the call.
	 */
	void RenderGreatCircle(LPDIRECT3DDEVICE7 dev, Vector& col);

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
	void RenderGrid(LPDIRECT3DDEVICE7 dev, Vector& col, bool eqline = true);

	/**
	 * \brief Render a background image on the celestial sphere.
	 * \param dev render device
	 * \param bglvl atmospheric background brightness
	 */
	void RenderBkgImage(LPDIRECT3DDEVICE7 dev, double bglvl);

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
	void AllocGrids();

	/**
	 * \brief Set up the render manager for showing a background image
	 *    on the celestial sphere.
	 */
	void InitBackgroundManager();

	void InitCelestialTransform();

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
	OrbiterGraphics* m_gc;          ///< pointer to graphics client
	CSphereManager* m_bkgImgMgr;    ///< background image manager (old version)
	CsphereManager* m_bkgImgMgr2;   ///< background image manager (new version)
	Scene* m_scene;                 ///< pointer to scene object
	DWORD m_viewW;                  ///< render viewport width [pixel]
	DWORD m_viewH;                  ///< render viewport height [pixel]
	DWORD m_nsVtx;                  ///< total number of vertices over all buffers
	std::vector<LPDIRECT3DVERTEXBUFFER7> m_sVtx; ///< star vertex buffers
	int m_lvlIdx[256];               ///< star brightness hash table
	DWORD m_ncVtx;                   ///< number of constellation line vertices
	LPDIRECT3DVERTEXBUFFER7 m_cVtx;  ///< vertex buffer for constellation lines
	LPDIRECT3DVERTEXBUFFER7 m_grdLngVtx, m_grdLatVtx; ///< vertex buffers for grid lines
	D3DMATRIX m_rotCelestial;        ///< rotation for celestial grid rendering
	MATRIX4 m_WMcsphere;
	double m_mjdPrecessionChecked;

	oapi::Font *m_cLabelFont;        ///< font for constellation labels
};

#endif // !__OGCELSPHERE_H