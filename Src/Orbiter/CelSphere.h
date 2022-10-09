// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __OGCELSPHERE_H
#define __OGCELSPHERE_H

#include "CelSphereAPI.h"
#include "OGraphics.h"
#include "D3D7Util.h"
#include "Vecmat.h"

#define MAXCONST 88      // max number of constellations

class OGCelestialSphere : public oapi::CelestialSphere {
public:
	OGCelestialSphere(OrbiterGraphics* og);
	~OGCelestialSphere();

	/**
	 * \brief Render stars as pixels on the celestial sphere
	 * \param dev render device
	 * \param nmax max. number of stars (default is all available stars)
	 * \param bgcol pointer to background color (default is black)
	 * \note If a background colour is passed into this function, the rendering
	 *   of stars darker than the background is suppressed.
	 * \note All device parameters are assumed to be set correctly on call.
	 */
	void RenderStars(LPDIRECT3DDEVICE7 dev, DWORD nmax = (DWORD)-1, const Vector* bgcol = 0);

	/**
	 * \brief Render constellation lines on the celestial sphere
	 * \param dev render device
	 * \param col line colour
	 * \note All device parameters are assumed to be set correctly on call.
	 * \note Suggestion: render additively onto background, so that the lines
	 *   are never darker than the background sky.
	 */
	void RenderConstellationLines(LPDIRECT3DDEVICE7 dev, const Vector& col);

	void RenderConstellationLabels(LPDIRECT3DDEVICE7 dev, bool full);

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
	 * \brief Load constellation label database from file. 
	 */
	void LoadConstellationLabels();

	/**
	 * \brief Allocate vertex list for rendering grid lines
	 *    (e.g. celestial or ecliptic)
	 */
	void AllocGrids();

private:
	OrbiterGraphics* m_gc;          ///< pointer to graphics client
	DWORD m_viewW;                  ///< render viewport width [pixel]
	DWORD m_viewH;                  ///< render viewport height [pixel]
	DWORD m_nsVtx;                  ///< total number of vertices over all buffers
	std::vector<LPDIRECT3DVERTEXBUFFER7> m_sVtx; ///< star vertex buffers
	int m_lvlIdx[256];               ///< star brightness hash table
	DWORD m_ncVtx;                   ///< number of constellation line vertices
	LPDIRECT3DVERTEXBUFFER7 m_cVtx;  ///< vertex buffer for constellation lines
	LPDIRECT3DVERTEXBUFFER7 m_grdLngVtx, m_grdLatVtx; ///< vertex buffers for grid lines

	std::vector<oapi::GraphicsClient::ConstLabelRenderRec> m_cLabel; ///< list of label records

	LPDIRECT3DVERTEXBUFFER7 vb_target, vb_cnstlabel;

	oapi::Font *m_cLabelFont;
};

#endif // !__OGCELSPHERE_H