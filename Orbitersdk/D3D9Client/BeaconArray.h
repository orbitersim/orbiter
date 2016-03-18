// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================


#ifndef __BEACONARRAY_H
#define __BEACONARRAY_H

#include "D3D9Client.h"
#include "D3D9Effect.h"
#include <d3d9.h> 
#include <d3dx9.h>


/**
 * \brief BeaconArrayEntry structure describing one individual beacon light
 */
typedef struct {
	VECTOR3 pos;	///< Beacon position relative to visual origin (base origin)
	VECTOR3 dir;	///< Light direction. (1,0,0)=South (0,1,0)=Up (0,0,1)=East
	DWORD	color;	///< Light color
	float	size;	///< Size of the beacon in meters
	float   angle;	///< Light cone angle in degrees
	float	lon;	///< Light on time
	float   loff;	///< Light off time if (lon=0, loff=1) light always on
	float   bright;	///< Light brightness factor. 1.0 = default
	float   fall;	///< Spotlight falloff speed 2.0 to 0.1; 
} BeaconArrayEntry;

typedef struct {
	double lng, lat;
	VECTOR3 vLoc;
} BeaconPos;


/**
 * \brief BeaconArray object with D3D9-specific vertex buffer
 */
class BeaconArray : private D3D9Effect 
{

public:

	/**
	 * \brief Create a BeaconArray object for rendering multiple beacons at the same time
	 * \param pArray Pointer into a BeaconArrayEntry list.
	 * \param nArray Number of entries in the array
	 */
	BeaconArray(BeaconArrayEntry *pArray, DWORD nArray, class vBase *vP=NULL);
	~BeaconArray();
	
	void UnLockVertexBuffer();		///< Unlocks the vertex buffer after manipulation is finished
	BAVERTEX * LockVertexBuffer();	///< Locks the vertex buffer for manipulation

	/**
	 * \brief Render all beacons.
	 * \param dev Pointer to the Direct 3D 9 device
	 * \param pW 3DX matrix to operate on
	 * \param time Seconds-only part of the simulation elapsed time (0...1.0)
	 */
	void Render(LPDIRECT3DDEVICE9 dev, const LPD3DXMATRIX pW, float time=0.5f);

	void Update(DWORD nCount, vPlanet *vP);

private:

	DWORD nVert;					///< Number of beacons
	DWORD bidx;						///< Update index
	LPDIRECT3DVERTEXBUFFER9 pVB;	///< Vertex buffer pointer
	SURFHANDLE pBright;				///< D3D9RwyLight.dds texture handle
	OBJHANDLE hBase;
	class vBase *vB;
	BeaconPos *pBeaconPos;
	double base_elev;
};

#endif // !__BEACONARRAY_H