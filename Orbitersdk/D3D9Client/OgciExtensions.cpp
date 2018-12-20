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

DLLCLBK SURFHANDLE gcLoadSurface(const char *fname, DWORD flags)
{
	return g_client->clbkLoadSurface(fname, flags);
}


DLLCLBK int gcMeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, int param, COLOUR4 *value, bool bSet)
{
	if (!hMesh) return -4;
	D3D9Mesh *pMesh = (D3D9Mesh *)hMesh;
	return pMesh->Material(idx, param, value, bSet);
}


DLLCLBK bool gcWorldToScreenSpace(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, FMATRIX4 *pVP, float clip = 1.0f)
{
	Scene *pScene = g_client->GetScene();
	return pScene ? pScene->WorldToScreenSpace(rdir, pt, (D3DXMATRIX *)pVP, clip) : false;
}


DLLCLBK int gcSketchpadVersion(oapi::Sketchpad *pSkp)
{
	if (pSkp->GetTextWidth("_SkpVerInfo") == 2) return 2;
	return 1;
}


DLLCLBK SKETCHMESH gcLoadSketchMesh(const char *name)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	SketchMesh *pSM = new SketchMesh(pDev);
	if (pSM->LoadMesh(name)) return pSM;
	delete pSM;
	return NULL;
}


DLLCLBK void gcDeleteSketchMesh(SKETCHMESH hMesh)
{
	if (hMesh) delete ((SketchMesh*)hMesh);
}


DLLCLBK HPOLY gcCreatePoly(HPOLY hPoly, const FVECTOR2 *pt, int npt, DWORD flags)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	if (!hPoly) return new D3D9PolyLine(pDev, pt, npt, (flags&PF_CONNECT) != 0);
	((D3D9PolyLine *)hPoly)->Update(pt, npt, (flags&PF_CONNECT) != 0);
	return hPoly;
}


DLLCLBK HPOLY gcCreateTriangles(HPOLY hPoly, const TriangleVtx *pt, int npt, DWORD flags)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	if (!hPoly) return new D3D9Triangle(pDev, pt, npt, flags);
	((D3D9Triangle *)hPoly)->Update(pt, npt);
	return hPoly;
}


DLLCLBK void gcDeletePoly(HPOLY hPoly)
{
	if (hPoly) {
		((D3D9PolyBase *)hPoly)->Release();
		delete ((D3D9PolyBase *)hPoly);
	}
}


DLLCLBK bool gcRegisterRenderProc(__gcRenderProc proc, DWORD flags, void *pParam)
{
	return g_client->RegisterRenderProc(proc, flags, pParam);
}


DLLCLBK bool gcRegisterGenericProc(__gcGenericProc proc, DWORD flags, void *pParam)
{
	return g_client->RegisterGenericProc(proc, flags, pParam);
}


DLLCLBK DWORD gcClientID()
{
	return DWORD('D3D9');
}


DLLCLBK bool gcGenerateMipMaps(SURFHANDLE hSurface, DWORD Filter)
{
	return SURFACE(hSurface)->GenerateMipMaps();
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


DLLCLBK HWND gcGetRenderWindow()
{
	return g_client->GetRenderWindow();
}


DLLCLBK DWORD gcGetTextLength(oapi::Font *hFont, const char *pText, int len)
{
	return DWORD((static_cast<D3D9PadFont *>(hFont))->GetTextLength(pText, len));
}


DLLCLBK DWORD gcGetCharIndexByPosition(oapi::Font *hFont, const char *pText, int pos, int len)
{
	return DWORD((static_cast<D3D9PadFont *>(hFont))->GetIndexByPosition(pText, pos, len));
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
	Scene *pScene = g_client->GetScene();
	return pScene ? pScene->SetupCustomCamera(hCam, hVessel, mTake, pos, fov, hSurf, flags) : NULL;
}


DLLCLBK void gcCustomCameraOnOff(CAMERAHANDLE hCam, bool bOn)
{
	Scene *pScene = g_client->GetScene();
	if (pScene) {
		pScene->CustomCameraOnOff(hCam, bOn);
	}
}


DLLCLBK int gcDeleteCustomCamera(CAMERAHANDLE hCam)
{
	Scene *pScene = g_client->GetScene();
	return pScene ? pScene->DeleteCustomCamera(hCam) : 0;
}

