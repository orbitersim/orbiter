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


DLLCLBK bool ogciRegisterRenderProc(__ogciRenderProc proc, DWORD flags)
{
	return g_client->RegisterRenderProc(proc, flags);
}


DLLCLBK DWORD ogciClientID()
{
	return DWORD('D3D9');
}

DLLCLBK bool ogciGenerateMipMaps(SURFHANDLE hSurface, DWORD Filter)
{
	return SURFACE(hSurface)->GenerateMipMaps();
}

			
DLLCLBK bool ogciRegisterSkinName(const VISHANDLE hVisual, const char *name)
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


DLLCLBK SURFHANDLE ogciCreateSurfaceEx(int w, int h, DWORD attrib)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	D3D9ClientSurface *surf = new D3D9ClientSurface(pDev, "oapiCreateSurfaceEx");
	surf->CreateSurface(w, h, attrib);
	return surf;
}


DLLCLBK DWORD ogciGetSurfaceAttribs(SURFHANDLE hSurf, bool bCreation)
{
	return SURFACE(hSurf)->GetAttribs(bCreation);
}


DLLCLBK void ogciConvertSurface(SURFHANDLE hSurf, DWORD attrib)
{
	LogBlu("ogciConvertSurface(0x%X) Attribs = 0x%X", hSurf, attrib);
	SURFACE(hSurf)->ConvertSurface(attrib);
}



// Sketchpad Extensions ==================================================================
//
//
DLLCLBK void ogciSketchBlt(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, int tx, int ty)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());

	if (pTgt->GetSketchPadMode() == SKETCHPAD_DIRECTX) {

		RECT src, tgt;

		DWORD w = SURFACE(hSrc)->GetWidth();
		DWORD h = SURFACE(hSrc)->GetHeight();

		src.left = 0; 
		src.top = 0;
		src.right = w; 
		src.bottom = h;
		tgt.left = tx; 
		tgt.top = ty;
		tgt.right = tx + w;
		tgt.bottom = ty + h;

		pTgt->SketchRect(hSrc, &src, &tgt);
		LogOk("ogciSketchBlt 0x%X (%s) -> 0x%X (%s) (%u,%u)", hSrc, SURFACE(hSrc)->GetName(), pTgt, pTgt->GetName(), w, h);
	}
}


DLLCLBK void ogciSketchBltEx(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, LPRECT t, float alpha, VECTOR3 *color)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());

	if (pTgt->GetSketchPadMode() == SKETCHPAD_DIRECTX) {
		pTgt->SketchRect(hSrc, s, t, alpha, color);
		LogOk("ogciSketchBltEx 0x%X (%s) -> 0x%X (%s)", hSrc, SURFACE(hSrc)->GetName(), pTgt, pTgt->GetName());
	}
}


DLLCLBK void ogciSketchRotateBlt(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, int tcx, int tcy, int w, int h, float angle, float alpha, VECTOR3 *color)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());

	if (pTgt->GetSketchPadMode() == SKETCHPAD_DIRECTX) {
		pTgt->SketchRotateRect(hSrc, s, tcx, tcy, w, h, angle, alpha, color);
		LogOk("ogciSketchRotateBlt 0x%X (%s) -> 0x%X (%s) (%u,%u)", hSrc, SURFACE(hSrc)->GetName(), pTgt, pTgt->GetName(), w, h);
	}
}


DLLCLBK int ogciSketchpadVersion(oapi::Sketchpad *pSkp)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());
	return pTgt->GetSketchPadMode();
}


// Custom Camera Interface ==================================================================
//
//
DLLCLBK CAMERAHANDLE ogciSetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags)
{
	VECTOR3 x = crossp(up, dir);
	MATRIX3 mTake;
	mTake.m11 = x.x;	mTake.m21 = x.y;	mTake.m31 = x.z;
	mTake.m12 = up.x;	mTake.m22 = up.y;	mTake.m32 = up.z;
	mTake.m13 = dir.x;	mTake.m23 = dir.y;	mTake.m33 = dir.z;
	return g_client->GetScene()->SetupCustomCamera(hCam, hVessel, mTake, pos, fov, hSurf, flags);
}

DLLCLBK void ogciCustomCameraOnOff(CAMERAHANDLE hCam, bool bOn)
{
	g_client->GetScene()->CustomCameraOnOff(hCam, bOn);
}

DLLCLBK int ogciDeleteCustomCamera(CAMERAHANDLE hCam)
{
	return g_client->GetScene()->DeleteCustomCamera(hCam);
}

