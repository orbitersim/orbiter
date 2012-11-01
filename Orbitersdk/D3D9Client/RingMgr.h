// ==============================================================
// RingMgr.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
// ==============================================================

#ifndef __RINGMGR_H
#define __RINGMGR_H

#include "D3D9Client.h"
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

	static void GlobalInit (oapi::D3D9Client *gclient);

	void SetMeshRes (DWORD res);

	inline double InnerRad() const { return irad; }
	inline double OuterRad() const { return orad; }

	bool Render (LPDIRECT3DDEVICE9 dev, D3DXMATRIX &mWorld, bool zenable);

protected:
	D3D9Mesh *CreateRing (double irad, double orad, int nsect);
	DWORD RingManager::LoadTextures ();

private:
	static oapi::D3D9Client *gc;
	const vPlanet *vp;
	D3D9Mesh *mesh[MAXRINGRES];
	LPDIRECT3DTEXTURE9 tex[MAXRINGRES];
	DWORD rres, tres, ntex;
	double irad, orad;
};

#endif // !__RINGMGR_H