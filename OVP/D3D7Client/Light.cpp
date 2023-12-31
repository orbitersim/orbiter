// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// Light.cpp
// class D3D7Light (implementation)
//
// This class represents a light source in terms of DX7 interface
// (D3DLIGHT7)
// ==============================================================

#include "Light.h"
#include "Scene.h"
#include "Camera.h"
#include "D3D7Util.h"

D3D7Light::D3D7Light (OBJHANDLE _hObj, LTYPE _ltype, const Scene *scene, DWORD _idx)
{
	hObj = _hObj;
	rpos = {0,0,0};
	ltype = _ltype;
	scn = scene;
	idx = _idx;

	memset (&light, 0, sizeof(D3DLIGHT7));

	switch (ltype) {
	case Point:       light.dltType = D3DLIGHT_POINT;       break;
	case Spot:        light.dltType = D3DLIGHT_SPOT;        break;
	case Directional: light.dltType = D3DLIGHT_DIRECTIONAL; break;
	}
	light.dcvDiffuse.r = light.dcvSpecular.r = 1.0f; // generalise (from light source specs)
	light.dcvDiffuse.g = light.dcvSpecular.g = 1.0f;
	light.dcvDiffuse.b = light.dcvSpecular.b = 1.0f;

	light.dvAttenuation0 = 1.0f; 
    light.dvRange = D3DLIGHT_RANGE_MAX;

	scn->GetClient()->GetDevice()->LightEnable (idx, TRUE);
}

void D3D7Light::Update ()
{
	switch (ltype) {
	case Point:
		// to be done
		break;
	case Spot:
		// to be done
		break;
	case Directional:
		UpdateDirectional();
		break;
	};
}

void D3D7Light::UpdateDirectional ()
{
	VECTOR3 rpos;
	oapiGetGlobalPos (hObj, &rpos);
	rpos -= *scn->GetCamera()->GetGPos(); // object position rel. to camera
	rpos /= -len(rpos); // normalise
	D3DVEC(rpos, light.dvDirection);
}

void D3D7Light::SetLight (LPDIRECT3DDEVICE7 dev)
{
	dev->SetLight (idx, &light);
}