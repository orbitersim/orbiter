// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
// ==============================================================

#include "D3D9Client.h"
#include "D3D9Surface.h"
#include "D3D9Config.h"
#include "Scene.h"

extern D3D9Client *g_client;



DLLCLBK DWORD ogciClientID()
{
	return DWORD('D3D9');
}



DLLCLBK SURFHANDLE ogciCreateSurfaceEx(DWORD w, DWORD h, DWORD attrib)
{
	return oapiCreateSurfaceEx(w, h, attrib);
}



// Sketchpad Extensions ==================================================================
//
//
DLLCLBK void ogciSketchBlt(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, int tx, int ty)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());

	if (pTgt->GetSketchPadMode() == SKETCHPAD_DIRECTX) {

		RECT src, tgt;

		src.left = 0; src.top = 0;
		src.right = SURFACE(hSrc)->GetWidth();
		src.bottom = SURFACE(hSrc)->GetHeight();
		tgt.left = tx; tgt.top = ty;
		tgt.right = tx + SURFACE(hSrc)->GetWidth();
		tgt.bottom = ty + SURFACE(hSrc)->GetHeight();

		pTgt->SketchRect(hSrc, &src, &tgt);
	}
}


DLLCLBK void ogciSketchBltEx(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, LPRECT t, float alpha, VECTOR3 *color)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());

	if (pTgt->GetSketchPadMode() == SKETCHPAD_DIRECTX) {
		pTgt->SketchRect(hSrc, s, t, alpha, color);
	}
}


DLLCLBK void ogciSketchRotateBlt(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, int tcx, int tcy, int w, int h, float angle, float alpha, VECTOR3 *color)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());

	if (pTgt->GetSketchPadMode() == SKETCHPAD_DIRECTX) {
		pTgt->SketchRotateRect(hSrc, s, tcx, tcy, w, h, angle, alpha, color);
	}
}



DLLCLBK int ogciSketchpadVersion(oapi::Sketchpad *pSkp)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());
	return pTgt->GetSketchPadMode();
}



DLLCLBK void ogciGrabSketchWhenReleased(oapi::Sketchpad *pSkp, SURFHANDLE hTgt)
{
	D3D9ClientSurface *pTgt = SURFACE(pSkp->GetSurface());

	if (pTgt->GetSketchPadMode() == SKETCHPAD_DIRECTX) {
		D3D9Pad *pPad = (D3D9Pad *)pSkp;
		//pPad->GrabSketch(hTgt);
	}
}

	


// Acquire Camera Control ==================================================================
//
//
DLLCLBK void ogciTakeCamera(VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov)
{
	VECTOR3 x = crossp(up, dir);
	MATRIX3 mTake;
	mTake.m11 = x.x;	mTake.m21 = x.y;	mTake.m31 = x.z;
	mTake.m12 = up.x;	mTake.m22 = up.y;	mTake.m32 = up.z;
	mTake.m13 = dir.x;	mTake.m23 = dir.y;	mTake.m33 = dir.z;
	g_client->GetScene()->TakeCamera(mTake, pos, fov);
}

DLLCLBK void ogciReleaseCameraTake()
{
	g_client->GetScene()->ReleaseCameraTake();
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

