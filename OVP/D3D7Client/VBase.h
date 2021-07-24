// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VBase.h
// class vBase (interface)
//
// A vBase is the visual representation of a surface base
// object (a "spaceport" on the surface of a planet or moon,
// usually with runways or landing pads where vessels can
// land and take off.
// ==============================================================

#ifndef __VBASE_H
#define __VBASE_H

#include "VObject.h"
#include "Mesh.h"

class vBase: public vObject {
	friend class vPlanet;

public:
	vBase (OBJHANDLE _hObj, const Scene *scene);
	~vBase();

	bool Update ();

	bool RenderSurface (LPDIRECT3DDEVICE7 dev);
	bool RenderStructures (LPDIRECT3DDEVICE7 dev);
	void RenderGroundShadow (LPDIRECT3DDEVICE7 dev);

private:
	void SetupShadowMeshes ();

	/**
	 * \brief Modify local lighting due to planet shadow or
	 *   atmospheric dispersion.
	 * \param light pointer to D3DLIGHT7 structure receiving modified parameters
	 * \param nextcheck time interval until next lighting check [s]
	 * \return \e true if lighting modifications should be applied, \e false
	 *   if global lighting conditions apply.
	 */
	bool ModLighting (LPD3DLIGHT7 light, double &nextcheck);

	double Tchk;               // next update
	double Tlghtchk;           // next lighting update
	DWORD ntile;               // number of surface tiles
	const SurftileSpec *tspec; // list of tile specs
	struct SurfTile {
		D3D7Mesh *mesh;
	} *tile;
	D3D7Mesh **structure_bs;
	D3D7Mesh **structure_as;
	DWORD nstructure_bs, nstructure_as;
	bool lights;               // use nighttextures for base objects
	bool bLocalLight;          // true if lighting is modified
	D3DLIGHT7 localLight;      // current local lighting parameters

	struct ShadowMesh {
		LPDIRECT3DVERTEXBUFFER7 vbuf;
		WORD *idx;
		DWORD nvtx, nidx;
		double ecorr;
	} *shmesh;
	DWORD nshmesh;
};

#endif // !__VBASE_H