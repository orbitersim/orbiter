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
	void Render(LPDIRECT3DDEVICE9 pDevice, const VECTOR3& skyCol);

	/**
	 * \brief Render stars as pixels on the celestial sphere
	 * \param fx  render effect
	 * \param nmax  max. number of stars (default is all available stars)
	 * \note if a background colour is passed into this function, the rendering
	 *   of stars darker than the background is suppressed.
	 * \note All device parameters are assumed to be set correctly on call.
	 */
	void RenderStars(ID3DXEffect *fx);

	/**
	 * \brief Render constellation lines on the celestial sphere
	 * \param fx  render effect
	 * \note All device parameters are assumed to be set correctly on call.
	 * \note Suggestion: render additively onto background, so that the lines
	 *   are never darker than the background sky.
	 */
	void RenderConstellationLines(ID3DXEffect *fx);

	/**
	 * \brief Render constellation boundaries on the celestial sphere
	 * \param dev render device
	 * \note All device parameters are assumed to be set correctly on call.
	 * \note Suggestion: render additively onto background, so that the lines
	 *   are never darker than the background sky.
	 */
	void RenderConstellationBoundaries(ID3DXEffect* fx);

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

	void RenderGridLabels(ID3DXEffect* FX, int az_idx, const oapi::FVECTOR4& baseCol, const MATRIX3& R, double dphi);

	/**
	 * \brief Render a background image on the celestial sphere.
	 * \param dev render device
	 */
	void RenderBkgImage(LPDIRECT3DDEVICE9 dev);

	static void D3D9TechInit(ID3DXEffect* fx);

protected:
	/**
	 * \brief Prepare the star vertex list from the star database.
	 */
	void InitStars ();

	/**
	 * \brief Free the vertex buffers for star pixel rendering
	 */
	void ClearStars();

	/**
	 * \brief Load constellation line data from file
	 */
	void InitConstellationLines ();

	/**
	 * \brief Map constellation boundary database to vertex buffer.
	 */
	void InitConstellationBoundaries();

	/**
	 * \brief Allocate vertex list for rendering grid lines
	 *        (e.g. celestial or ecliptic)
	 */
	void AllocGrids();

	void AllocGridLabels();

	void InitCelestialTransform();

	bool LocalHorizonTransform(MATRIX3& R, D3DXMATRIX& T);

	/**
	 * \brief Convert a direction into viewport coordinates
	 * \param dir direction in the ecliptic frame provided as a point on the
	 *    celestial sphere.
	 * \param x x-position in the viewport window [pixel]
	 * \param y y-position in the viewport window [pixel]
	 * \return true if point is visible in the viewport, false otherwise.
	 */
	virtual bool EclDir2WindowPos(const VECTOR3& dir, int& x, int& y) const;

	int MapLineBuffer(const std::vector<VECTOR3>& lineVtx, LPDIRECT3DVERTEXBUFFER9& buf) const;

private:
	oapi::D3D9Client *m_gc;         ///< pointer to graphics client
	CSphereManager* m_bkgImgMgr;    ///< background image manager
	Scene* m_scene;                 ///< pointer to scene object
	LPDIRECT3DDEVICE9 m_pDevice;    ///< DirectX9 device
	UINT maxNumVertices;            ///< number of vertices to use for one chunk at star-drawing
	DWORD m_nsVtx;                  ///< total number of vertices over all buffers
	std::vector<LPDIRECT3DVERTEXBUFFER9> m_sVtx; ///< star vertex buffers
	std::array<int, 256> m_starCutoffIdx;  ///< list of star render cutoff indices
	DWORD m_nclVtx;                  ///< number of constellation line vertices
	LPDIRECT3DVERTEXBUFFER9 m_clVtx; ///< constellation line vertex buffer
	DWORD m_ncbVtx;                  ///< number of constellation boundary vertices
	LPDIRECT3DVERTEXBUFFER9 m_cbVtx; ///< vertex buffer for constellation boundaries
	LPDIRECT3DVERTEXBUFFER9 m_grdLngVtx, m_grdLatVtx; ///< vertex buffers for grid lines
	lpSurfNative m_GridLabelTex;     ///< texture for grid labels
	std::array<LPDIRECT3DVERTEXBUFFER9, 3> m_azGridLabelVtx;  ///< vertex buffers for azimuth grid labels
	LPDIRECT3DVERTEXBUFFER9 m_elGridLabelVtx; ///< vertex buffer for elevation grid labels
	LPDIRECT3DINDEXBUFFER9 m_GridLabelIdx; ///< index list for azimuth/elevation grid labels
	MATRIX3 m_rotCelestial;          ///< rotation matrix for celestial grid rendering
	D3DXMATRIX m_transformCelestial; ///< rotation for celestial grid rendering
	double m_mjdPrecessionChecked;

	static ID3DXEffect* s_FX;
	static D3DXHANDLE s_eStar;
	static D3DXHANDLE s_eLine;
	static D3DXHANDLE s_eLabel;
	static D3DXHANDLE s_eColor;
	static D3DXHANDLE s_eWVP;
};

#endif // !__D3D9CELSPHERE_H
