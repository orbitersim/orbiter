// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// RingMgr.h
// Render planetary ring systems
// ==============================================================

#ifndef __RINGMGR_H
#define __RINGMGR_H

#include "D3D7Client.h"
#include "VPlanet.h"
#include "Mesh.h"

#define MAXRINGRES 3

// ==============================================================
// class RingManager (interface)
// ==============================================================
/**
 * \brief Rendering of planet rings at different resolutions
 */
class RingManager {
public:
	RingManager (const vPlanet *vplanet, double inner_rad, double outer_rad);
	~RingManager ();

	static void GlobalInit (const oapi::D3D7Client *gclient);

	void SetMeshRes (DWORD res);

	inline double InnerRad() const { return irad; }
	inline double OuterRad() const { return orad; }

	bool Render (LPDIRECT3DDEVICE7 dev, D3DMATRIX &mWorld);

protected:
	D3D7Mesh *CreateRing (double irad, double orad, int nsect);
	DWORD LoadTextures ();

private:
	static const oapi::D3D7Client *gc;
	static DWORD vbMemCaps;
	const vPlanet *vp;
	D3D7Mesh *mesh[MAXRINGRES];
	LPDIRECTDRAWSURFACE7 tex[MAXRINGRES];
	DWORD rres, tres, ntex;
	double irad, orad;
};

#endif // !__RINGMGR_H
