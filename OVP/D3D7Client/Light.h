// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
//   Copyright (C) 2006-2016 Martin Schweiger
//   Dual licensed under GPL v3 and LGPL v3
// ==============================================================

// ==============================================================
// Light.h
// class D3D7Light (interface)
//
// This class represents a light source in terms of DX7 interface
// (D3DLIGHT7)
// ==============================================================

#ifndef __LIGHT_H
#define __LIGHT_H

#include "D3D7Client.h"

class D3D7Light {
public:
	enum LTYPE { Point, Spot, Directional };
	D3D7Light (OBJHANDLE _hObj, LTYPE _ltype, const Scene *scene, DWORD _idx);

	void Update ();
	void SetLight (LPDIRECT3DDEVICE7 dev);

	inline const D3DLIGHT7 *GetLight() const { return &light; }

protected:
	void UpdateDirectional ();

private:
	OBJHANDLE hObj;   // object to which the light source is attached
	VECTOR3 rpos;     // light source position in the object frame
	LTYPE ltype;      // light type
	const Scene *scn; // The scene to which the light source belongs
	DWORD idx;        // light index
	D3DLIGHT7 light;  // DX7 light definition
};

#endif // !__LIGHT_H