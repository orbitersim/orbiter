// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __CELSPHERE_H
#define __CELSPHERE_H

#include "OGraphics.h"
#include "D3D7Util.h"
#include "Vecmat.h"

#define MAXCONST 88      // max number of constellations

class CelestialSphere {
public:
	CelestialSphere(OrbiterGraphics* og);
	~CelestialSphere();

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

protected:
	/**
	 * \brief Load star database from file.
	 */
	void LoadStars();

	/**
	 * \brief Load constellation line database from file.
	 */
	void LoadConstellationLines();

	void LoadConstellationLabels();

	void AllocGrids();

private:
	OrbiterGraphics* m_gc;          ///< pointer to graphics client
	DWORD m_nsVtx;                  ///< total number of vertices over all buffers
	std::vector<LPDIRECT3DVERTEXBUFFER7> m_sVtx; ///< star vertex buffers
	int m_lvlIdx[256];               ///< star brightness hash table
	DWORD m_ncVtx;                   ///< number of constellation line vertices
	LPDIRECT3DVERTEXBUFFER7 m_cVtx;  ///< vertex buffer for constellation lines
	LPDIRECT3DVERTEXBUFFER7 m_grdLngVtx, m_grdLatVtx; ///< vertex buffers for grid lines

	DWORD ncnstlabel;
	struct CnstLabel {
		char abbr[3];
		char* full;
		int len;
	} cnstlabel[MAXCONST];

	LPDIRECT3DVERTEXBUFFER7 vb_target, vb_cnstlabel;
};

#endif // !__CELSPHERE_H