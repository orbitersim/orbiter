// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// HazeMgr.h
// class HazeManager (interface)
//
// Planetary atmospheric haze rendering
// Implemented as transparent overlay on planetary disc
// ==============================================================

#ifndef __HAZEMGR_H
#define __HAZEMGR_H

#include "D3D7Client.h"

#define HORIZON_NSEG 128  // number of mesh segments

class vPlanet;

class HazeManager {
public:
	HazeManager (const oapi::D3D7Client *gclient, const vPlanet *vplanet);

	static void GlobalInit (oapi::D3D7Client *gclient);

	void Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &wmat, bool dual = false);

private:
	const oapi::D3D7Client *gc;
	OBJHANDLE obj;
	const vPlanet *vp;
	VECTOR3 basecol;
	double rad;    // planet radius
	float  hralt;  // relative horizon altitude
	float  dens0;  // atmosphere density factor
	double hshift; // horizon reference shift factor
	double cloudalt; // cloud layer altitude
	float  hscale; // inner haze ring radius (in planet radii)
	static WORD Idx[HORIZON_NSEG*2+2];
	static DWORD nIdx;
	static struct HVERTEX {
		D3DVALUE x,y,z;
		DWORD    dcol;
		D3DVALUE tu, tv; } Vtx[HORIZON_NSEG*2];
	static D3DVALUE CosP[HORIZON_NSEG], SinP[HORIZON_NSEG];
	static LPDIRECTDRAWSURFACE7 horizon;
};

#endif // !__HAZEMGR_H