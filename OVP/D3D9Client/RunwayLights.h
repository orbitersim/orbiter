// ==============================================================
// RunwayLights.h
// Part of the ORBITER VISUALISATION PROJECT (OVP) D3D9 Client
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2016 Émile "Bibi Uncle" Grégoire
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// class RunwayLights
//
// Defines runway lights used in vBase.
// ==============================================================

#ifndef __RUNWAYLIGHTS_H
#define __RUNWAYLIGHTS_H

#include "OrbiterAPI.h"
#include <d3d9.h>
#include <d3dx9.h>

class BeaconArray;

class RunwayLights
{
public:
	RunwayLights(class vBase *vB, const class Scene *snc);
	virtual ~RunwayLights();

	void SetEnd1(VECTOR3 pos);
	void SetEnd2(VECTOR3 pos);
	void SetWidth(double width);
	void SetTouchZoneDisplacement(double disp);
	void SetTouchZoneDisplacement2(double disp);
	void SetTouchZoneLength(double length);
	void SetDecisionDist(double dist);
	void SetApproachStart(double dist);
	void AddPAPI(VECTOR3 pos, float disp=0.0f, DWORD end=0);
	void AddVASI(VECTOR3 pos, DWORD end=0);
	void SetSignleEnded(bool bSingleEnded);
	void SetCategory(int cat);

	void Init();
	void Render(LPDIRECT3DDEVICE9 dev, LPD3DXMATRIX world, bool night);
	void Update(class vPlanet *vP);

	float GetWidth() const { return float(width); }

	static int CreateRunwayLights(class vBase *vB, const class Scene *scn, const char *file, RunwayLights**& out);

protected:

	void			   SetPAPIColors(BeaconArray *pPAPI, LPD3DXMATRIX world, int idx);

	class BeaconArray *BuildLights(VECTOR3 start, VECTOR3 end, double disp);
	class BeaconArray *BuildVASI(VECTOR3 start, VECTOR3 end, DWORD idx);
	class BeaconArray *BuildPAPI(VECTOR3 start, VECTOR3 end, DWORD idx);

	VECTOR3 end1;
	VECTOR3 end2;
	double width;
	double td_disp;
	double td_disp2;
	double td_length;
	double apr_length;
	double apr_start;
	bool   bSingleEnded;
	bool   bDisp2;
	int    iCategory;
	OBJHANDLE hObj;

	DWORD nPAPI;
	VECTOR3 PAPI_pos[12];
	float PAPI_disp[12];
	DWORD PAPI_end[12];

	DWORD nVASI;
	VECTOR3 VASI[2];
	DWORD VASI_end[2];

	BeaconArray* beacons1;
	BeaconArray* beacons2;
	BeaconArray* vasi[2];
	BeaconArray* papi[12];

	const class Scene * scene;
	class vBase *vB;
	float currentTime;
};





class TaxiLights
{
public:
	TaxiLights(OBJHANDLE handle, const class Scene *scn);
	virtual ~TaxiLights();

	void SetEnd1(VECTOR3 pos);
	void SetEnd2(VECTOR3 pos);
	void SetSize(double width);
	void SetCount(int count);
	void SetColor(VECTOR3 color);

	void Init();
	void Render(LPDIRECT3DDEVICE9 dev, LPD3DXMATRIX world, bool night);

	static int CreateTaxiLights(OBJHANDLE base, const class Scene *scn, const char *file, TaxiLights**& out);

protected:
	OBJHANDLE	hObj;
	VECTOR3		end1;
	VECTOR3		end2;
	VECTOR3		color;
	double		size;
	int			count;

	BeaconArray* beacons1;
	const class Scene * scene;
	float		 currentTime;
};




#endif // __RUNWAYLIGHTS_H
