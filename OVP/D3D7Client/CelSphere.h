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

class CSphereManager;

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
	 * \brief Notification of in-simulation user option change.
	 * \param cat option category, see \ref optcat
	 * \param item option item, see \ref optitem
	 */
	void OnOptionChanged(DWORD cat, DWORD item);

	/**
	 * \brief Render the celestial sphere background.
	 * \param pDevice pointer to graphics device
	 * \param skyCol sky background colour (atmospheric tint)
	 */
	void Render(LPDIRECT3DDEVICE7 dev, const VECTOR3 &skyCol);

	/**
	 * \brief Render stars as pixels on the celestial sphere
	 * \param dev render device
	 * \note If a background colour is passed into this function, the rendering
	 *   of stars darker than the background is suppressed.
	 * \note All device parameters are assumed to be set correctly on call.
	 */
	void RenderStars (LPDIRECT3DDEVICE7 dev);

	/**
	 * \brief Render constellation lines on the celestial sphere
	 * \param dev render device
	 * \note All device parameters are assumed to be set correctly on call.
	 * \note Suggestion: render additively onto background, so that the lines
	 *   are never darker than the background sky.
	 */
	void RenderConstellationLines (LPDIRECT3DDEVICE7 dev);

	/**
	 * \brief Render constellation boundaries on the celestial sphere
	 * \param dev render device
	 * \note All device parameters are assumed to be set correctly on call.
	 * \note Suggestion: render additively onto background, so that the lines
	 *   are never darker than the background sky.
	 */
	void RenderConstellationBoundaries(LPDIRECT3DDEVICE7 dev);

	/**
	 * \brief Render a great circle on the celestial sphere in a given colour.
	 * \param dev render device
	 * \param col RGB line colour (0..1 for each component)
	 * \note By default (i.e. for identity world matrix), the circle is
	 *   drawn along the plane of the ecliptic. To render a circle in any
	 *   other orientation, the world matrix must be adjusted accordingly
	 *   before the call.
	 */
	void RenderGreatCircle (LPDIRECT3DDEVICE7 dev, const oapi::FVECTOR4& baseCol);

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
	void RenderGrid (LPDIRECT3DDEVICE7 dev, const oapi::FVECTOR4& baseCol, bool eqline = true);

	void RenderGridLabels(LPDIRECT3DDEVICE7 dev, int az_idx, const oapi::FVECTOR4& baseCol, double dphi = 0.0);

	/**
	 * \brief Render a background image on the celestial sphere.
	 * \param dev render device
	 */
	void RenderBkgImage(LPDIRECT3DDEVICE7 dev);

protected:
	/**
	 * \brief Prepare the star vertex list from the star database.
	 */
	void InitStars();

	/**
	 * \brief Free the vertex buffers for star pixel rendering
	 */
	void ClearStars();

	/**
	 * \brief Load constellation line database from file.
	 */
	void InitConstellationLines();

	/**
	 * \brief Map constellation boundary database to vertex buffer.
	 */
	void InitConstellationBoundaries();

	/**
	 * \brief Allocate vertex list for rendering grid lines
	 *    (e.g. celestial or ecliptic)
	 */
	void AllocGrids ();

	void AllocGridLabels();

	void InitCelestialTransform();

	bool LocalHorizonTransform(MATRIX3& R, D3DMATRIX& T);

	/**
	 * \brief Convert a direction into viewport coordinates
	 * \param dir direction in the ecliptic frame provided as a point on the
	 *    celestial sphere.
	 * \param x x-position in the viewport window [pixel]
	 * \param y y-position in the viewport window [pixel]
	 * \return true if point is visible in the viewport, false otherwise.
	 */
	virtual bool EclDir2WindowPos(const VECTOR3& dir, int& x, int& y) const;

	int MapLineBuffer(const std::vector<VECTOR3>& lineVtx, LPDIRECT3DVERTEXBUFFER7& buf) const;

private:
	oapi::D3D7Client *m_gc;          ///< pointer to graphics client
	CSphereManager* m_bkgImgMgr;     ///< background image manager
	Scene* m_scene;                  ///< pointer to scene object
	DWORD m_viewW;                   ///< render viewport width [pixel]
	DWORD m_viewH;                   ///< render viewport height [pixel]
	DWORD m_nsVtx;                   ///< total number of vertices over all buffers
	std::vector<LPDIRECT3DVERTEXBUFFER7> m_sVtx; ///< star vertex buffers
	std::array<int, 256> m_starCutoffIdx;  ///< list of star render cutoff indices
	DWORD m_nclVtx;                  ///< number of constellation line vertices
	LPDIRECT3DVERTEXBUFFER7 m_clVtx; ///< vertex buffer for constellation lines
	DWORD m_ncbVtx;                  ///< number of constellation boundary vertices
	LPDIRECT3DVERTEXBUFFER7 m_cbVtx; ///< vertex buffer for constellation boundaries
	LPDIRECT3DVERTEXBUFFER7 m_grdLngVtx, m_grdLatVtx; ///< vertex buffers for grid lines
	LPDIRECTDRAWSURFACE7 m_GridLabelTex; ///< texture for grid labels
	std::array<LPDIRECT3DVERTEXBUFFER7, 3> m_azGridLabelVtx;  ///< vertex buffers for azimuth grid labels
	LPDIRECT3DVERTEXBUFFER7 m_elGridLabelVtx; ///< vertex buffer for elevation grid labels
	WORD* m_GridLabelIdx;            ///< index list for azimuth/elevation grid labels
	MATRIX3 m_rotCelestial;          ///< rotation matrix for celestial grid rendering
	D3DMATRIX m_transformCelestial;  ///< rotation for celestial grid rendering
	double m_mjdPrecessionChecked;
};

#endif // !__D3D7CELSPHERE_H