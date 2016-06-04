// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 - 2016 Jarmo Nikkanen
// ==============================================================

#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Config.h"
#include "Scene.h"
#include "VVessel.h"

extern D3D9Client *g_client;


DLLCLBK bool gcWorldToScreenSpace(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, float clip = 1.0f)
{
	return g_client->GetScene()->WorldToScreenSpace(rdir, pt, clip);
}


DLLCLBK int gcSketchpadVersion(oapi::Sketchpad *pSkp)
{
	if (pSkp->GetTextWidth("_SkpVerInfo") == 2) return 2;
	return 1;
}


DLLCLBK SKETCHMESH gcLoadSketchMesh(const char *name)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	return new SketchMesh(name, pDev);
}


DLLCLBK void gcDeleteSketchMesh(SKETCHMESH hMesh)
{
	if (hMesh) delete ((SketchMesh*)hMesh);
}


DLLCLBK bool gcRegisterRenderProc(__gcRenderProc proc, DWORD flags, void *pParam)
{
	return g_client->RegisterRenderProc(proc, flags, pParam);
}


DLLCLBK DWORD gcClientID()
{
	return DWORD('D3D9');
}


DLLCLBK bool gcGenerateMipMaps(SURFHANDLE hSurface, DWORD Filter)
{
	return SURFACE(hSurface)->GenerateMipMaps();
}

			
DLLCLBK bool gcRegisterSkinName(const VISHANDLE hVisual, const char *name)
{
	VisObject *pVis = (VisObject *)hVisual;
	OBJHANDLE hObj = pVis->GetObject();
	if (hObj) if (oapiGetObjectType(hObj)==OBJTP_VESSEL) {
		vVessel *pVes = (vVessel *)hVisual;
		pVes->SetSkinName(name);
		return true;
	}
	return false;
}


DLLCLBK DWORD gcGetSurfaceAttribs(SURFHANDLE hSurf, bool bCreation)
{
	return SURFACE(hSurf)->GetAttribs(bCreation);
}


DLLCLBK void gcConvertSurface(SURFHANDLE hSurf, DWORD attrib)
{
	LogBlu("gcConvertSurface(0x%X) Attribs = 0x%X", hSurf, attrib);
	SURFACE(hSurf)->ConvertSurface(attrib);
}




// Custom Camera Interface ==================================================================
//
//
DLLCLBK CAMERAHANDLE gcSetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags)
{
	VECTOR3 x = crossp(up, dir);
	MATRIX3 mTake;
	mTake.m11 = x.x;	mTake.m21 = x.y;	mTake.m31 = x.z;
	mTake.m12 = up.x;	mTake.m22 = up.y;	mTake.m32 = up.z;
	mTake.m13 = dir.x;	mTake.m23 = dir.y;	mTake.m33 = dir.z;
	return g_client->GetScene()->SetupCustomCamera(hCam, hVessel, mTake, pos, fov, hSurf, flags);
}


DLLCLBK void gcCustomCameraOnOff(CAMERAHANDLE hCam, bool bOn)
{
	g_client->GetScene()->CustomCameraOnOff(hCam, bOn);
}


DLLCLBK int gcDeleteCustomCamera(CAMERAHANDLE hCam)
{
	return g_client->GetScene()->DeleteCustomCamera(hCam);
}

