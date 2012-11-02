// ==============================================================
// RunwayLights.h
// Part of the ORBITER VISUALISATION PROJECT (OVP) D3D9 Client
// Released under GNU General Public License
// Copyright (C) 2012 Émile "Bibi Uncle" Grégoire
//				 2012 Jarmo Nikkanen
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
	RunwayLights(OBJHANDLE handle);
	virtual ~RunwayLights();

	void SetEnd1(VECTOR3 pos);
	void SetEnd2(VECTOR3 pos);
	void SetWidth(double width);
	void SetTouchZoneDisplacement(double disp);
	void SetTouchZoneDisplacement2(double disp);
	void SetTouchZoneLength(double length);
	void SetDecisionDist(double dist);
	void SetApproachStart(double dist);
	void AddPAPI(VECTOR3 pos, float disp=0.0f);
	void AddVASI(VECTOR3 pos);
	void SetSignleEnded(bool bSingleEnded);
	void SetCategory(int cat);
	
	void Init();
	void Render(LPDIRECT3DDEVICE9 dev, LPD3DXMATRIX world, bool night);

	float GetWidth() { return float(width); }

	static int CreateRunwayLights(OBJHANDLE base, const char *file, RunwayLights**& out);

protected:

	void			   SetPAPIColors(BeaconArray *pPAPI, LPD3DXMATRIX world, int idx);

	class BeaconArray *BuildLights(VECTOR3 start, VECTOR3 end, double td_dist);
	class BeaconArray *BuildVASI(VECTOR3 start, VECTOR3 end);
	class BeaconArray *BuildPAPI(VECTOR3 start, VECTOR3 end, int idx);

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
	int	   iCategory;

	OBJHANDLE hObj;
	
	DWORD nPAPI;
	VECTOR3 PAPI_pos[6];
	float PAPI_disp[6];
	
	bool bVASI;
	VECTOR3 VASI;

	int numLights;
	BeaconArray* beacons1;
	BeaconArray* beacons2;
	BeaconArray* vasi1;
	BeaconArray* vasi2;
	BeaconArray* papi_f[6];
	BeaconArray* papi_b[6];
	const char * filename; 

	float currentTime;	
};





class TaxiLights
{
public:
	TaxiLights(OBJHANDLE handle);
	virtual ~TaxiLights();

	void SetEnd1(VECTOR3 pos);
	void SetEnd2(VECTOR3 pos);
	void SetSize(double width);
	void SetCount(int count);
	void SetColor(VECTOR3 color);
	
	void Init();
	void Render(LPDIRECT3DDEVICE9 dev, LPD3DXMATRIX world, bool night);

	static int CreateTaxiLights(OBJHANDLE base, const char *file, TaxiLights**& out);

protected:
	OBJHANDLE	hObj;
	VECTOR3		end1;
	VECTOR3		end2;
	VECTOR3		color;
	double		size;
	int			count;
	int			numLights;

	BeaconArray* beacons1;
	const char * filename; 
	float		 currentTime;	
};




#endif // __RUNWAYLIGHTS_H