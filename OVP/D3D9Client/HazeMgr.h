// ==============================================================
// HazeMgr.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

// ==============================================================
// class HazeManager (interface)
//
// Planetary atmospheric haze rendering
// Implemented as transparent overlay on planetary disc
// ==============================================================

#ifndef __HAZEMGR_H
#define __HAZEMGR_H

#include "D3D9Client.h"
#include "D3D9Effect.h"

#define HORIZON_NSEG 32		// number of mesh segments
#define HORIZON2_NSEG 200	// Horizon ring segments
#define HORIZON2_NRING 5	// Horizon ring ring count

class vPlanet;

class HazeManager : private D3D9Effect 
{
public:
	/**
	 * \brief Constructs a new haze manager object
	 * \param gclient pointer to graphics client
	 * \param vPlanet planet instance pointer
	 */
	HazeManager (const oapi::D3D9Client *gclient, const vPlanet *vplanet);

	/**
	 * \brief Set up global parameters shared by all instances
	 * \param gclient client instance pointer
	 */
	static void GlobalInit (oapi::D3D9Client *gclient);

	/**
	 * \brief Release global parameters
	 */
	static void GlobalExit();

	void Render (LPDIRECT3DDEVICE9 dev, FMATRIX4 &wmat, bool dual = false);
	
private:
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
		float x,y,z;
		DWORD    dcol;
		float tu, tv; } Vtx[HORIZON_NSEG*2];
	static float CosP[HORIZON_NSEG], SinP[HORIZON_NSEG];
	static SURFHANDLE horizon;
};



// ==============================================================
// class HazeManager2 (interface)
//
// Planetary atmospheric haze rendering with scattering technique
// HazeManager2 is used with TileManager2
// ==============================================================

class HazeManager2
{
public:
	HazeManager2 (vPlanet *vplanet);
	~HazeManager2();


	static	void GlobalInit (oapi::D3D9Client *gclient);
	static	void GlobalExit();
	static	void CreateSkydomeBuffers(int index);
	static	void CreateRingBuffers();

	void	Render(FMATRIX4 &wmat, float hz_aperture_deg);
	
private:

	void	RenderRing(VECTOR3 cpos, VECTOR3 cdir, double rad, double hralt);
	void	RenderSky(VECTOR3 cpos, VECTOR3 cdir, double rad, double apr);
	void	RenderSkySegment(FMATRIX4 &wmat, double rad, double dmin, double dmax, int index);

	OBJHANDLE obj;
	vPlanet *vp;
	double rad;

	static ShaderClass* pRing;
	static ShaderClass* pDome;
	static LPDIRECT3DDEVICE9 pDev;
	static LPDIRECT3DTEXTURE9 pNoise;
	static int xreslvl[6];
	static int yreslvl[6];
	static LPDIRECT3DVERTEXBUFFER9 pSkyVB[6];
	static LPDIRECT3DVERTEXBUFFER9 pRingVB;
};




#endif // !__HAZEMGR_H
