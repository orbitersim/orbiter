// ==============================================================
// VBase.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
// ==============================================================

#ifndef __VBASE_H
#define __VBASE_H

#include "VObject.h"
#include "Mesh.h"

class RunwayLights;
class TaxiLights;


// ==============================================================
// class vBase (interface)
// ==============================================================

/**
 * \brief Visual representation of a surface base.
 *
 * A vBase is the visual representation of a surface base object (a "spaceport"
 * on the surface of a planet or moon, usually with runways or landing pads
 * where vessels can land and take off.
 */
class vBase: public vObject {
	friend class vPlanet;

public:
	vBase (OBJHANDLE _hObj, const Scene *scene, vPlanet *vP=NULL);
	~vBase();

	virtual bool GetMinMaxDistance(float *zmin, float *zmax, float *dmin);
	virtual void UpdateBoundingBox();
	virtual DWORD GetMeshCount();
	
	bool Update ();

	void RenderRunwayLights (LPDIRECT3DDEVICE9 dev);
	bool RenderSurface (LPDIRECT3DDEVICE9 dev);
	bool RenderStructures (LPDIRECT3DDEVICE9 dev);
	void RenderGroundShadow (LPDIRECT3DDEVICE9 dev, float alpha);
	
	const SurftileSpec *GetTileDesc() { return tspec; }

private:
	void SetupShadowMeshes();
	void CreateRunwayLights();
	void CreateTaxiLights();

	/**
	 * \brief Modify local lighting due to planet shadow or
	 *   atmospheric dispersion.
	 * \param light pointer to D3DLIGHT7 structure receiving modified parameters
	 * \param nextcheck time interval until next lighting check [s]
	 * \return \e true if lighting modifications should be applied, \e false
	 *   if global lighting conditions apply.
	 */
	//bool ModLighting (D3D9Light *light, double &nextcheck);

	double Tchk;               // next update
	double Tlghtchk;           // next lighting update
	double csun_lights;
	DWORD ntile;               // number of surface tiles
	const SurftileSpec *tspec; // list of tile specs
	D3D9Mesh *tilemesh;
	D3D9Mesh **structure_bs;
	D3D9Mesh **structure_as;
	DWORD nstructure_bs, nstructure_as;
	bool lights;               // use nighttextures for base objects
	bool bLocalLight;          // true if lighting is modified
	D3D9Light localLight;      // current local lighting parameters
	class vPlanet *base_planet;
	
	int numRunwayLights;
	RunwayLights** runwayLights;

	int numTaxiLights;
	TaxiLights** taxiLights;
};

#endif // !__VBASE_H