// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2014 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

#ifndef __OGCI_CPP
#define __OGCI_CPP

#include <windows.h>
#include "OGCI.h"

HMODULE ogciHandle = NULL;

__ogciClientID _ogciClientID = NULL;
__ogciCreateSurfaceEx _ogciCreateSurfaceEx = NULL;
__ogciTakeCamera _ogciTakeCamera = NULL;
__ogciReleaseCameraTake _ogciReleaseCameraTake = NULL;
__ogciDeleteCustomCamera _ogciDeleteCustomCamera = NULL;
__ogciCustomCameraOnOff _ogciCustomCameraOnOff = NULL;
__ogciSetupCustomCamera _ogciSetupCustomCamera = NULL;
__ogciSketchBlt _ogciSketchBlt = NULL;
__ogciSketchBltEx _ogciSketchBltEx = NULL;
__ogciSketchRotateBlt _ogciSketchRotateBlt = NULL;
__ogciSketchpadVersion _ogciSketchpadVersion = NULL;



bool PostInit(HMODULE hClient)
{
	if (!hClient) return false;

	_ogciClientID = (__ogciClientID)GetProcAddress(hClient, "ogciClientID");
	_ogciCreateSurfaceEx = (__ogciCreateSurfaceEx)GetProcAddress(hClient, "ogciCreateSurfaceEx");
	// -------------
	_ogciTakeCamera = (__ogciTakeCamera)GetProcAddress(hClient, "ogciTakeCamera");
	_ogciReleaseCameraTake = (__ogciReleaseCameraTake)GetProcAddress(hClient, "ogciReleaseCameraTake");
	// -------------
	_ogciDeleteCustomCamera = (__ogciDeleteCustomCamera)GetProcAddress(hClient, "ogciDeleteCustomCamera");
	_ogciCustomCameraOnOff = (__ogciCustomCameraOnOff)GetProcAddress(hClient, "ogciCustomCameraOnOff");
	_ogciSetupCustomCamera = (__ogciSetupCustomCamera)GetProcAddress(hClient, "ogciSetupCustomCamera");
	// -------------
	_ogciSketchBlt = (__ogciSketchBlt)GetProcAddress(hClient, "ogciSketchBlt");
	_ogciSketchBltEx = (__ogciSketchBltEx)GetProcAddress(hClient, "ogciSketchBltEx");
	_ogciSketchRotateBlt = (__ogciSketchRotateBlt)GetProcAddress(hClient, "ogciSketchRotateBlt");
	_ogciSketchpadVersion = (__ogciSketchpadVersion)GetProcAddress(hClient, "ogciSketchpadVersion");
	
	return true;
}

bool ogciEnabled()
{
	if (ogciHandle) return true;
	return false;
}

bool ogciInitialize()
{
	if (ogciHandle) return ogciEnabled(); // Already Initialized
	if (!ogciHandle) ogciHandle = GetModuleHandle("D3D9Client.dll");
	if (!ogciHandle) ogciHandle = GetModuleHandle("D3D9ClientDebug.dll");
	if (ogciHandle) return PostInit(ogciHandle);
	else oapiWriteLog("OGCI: [Graphics Client Not Found]");
	return false;
}


DWORD ogciClientID()
{
	if (_ogciClientID) return _ogciClientID();
}

int	ogciDeleteCustomCamera(CAMERAHANDLE hCam)
{
	if (_ogciDeleteCustomCamera) return _ogciDeleteCustomCamera(hCam);
	return 0;
}

void ogciCustomCameraOnOff(CAMERAHANDLE hCam, bool bOn)
{
	if (_ogciCustomCameraOnOff) _ogciCustomCameraOnOff(hCam, bOn);
}

CAMERAHANDLE ogciSetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags)
{
	if (_ogciSetupCustomCamera) return _ogciSetupCustomCamera(hCam, hVessel, pos, dir, up, fov, hSurf, flags);
	return NULL;
}

int ogciSketchpadVersion(oapi::Sketchpad *pSkp)
{
	if (_ogciSketchpadVersion) return _ogciSketchpadVersion(pSkp);
	return SKETCHPAD_NONE;
}

void ogciSketchBlt(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, int tx, int ty)
{
	if (_ogciSketchBlt) _ogciSketchBlt(pSkp, hSrc, tx, ty);
}

void ogciSketchBltEx(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, LPRECT t, float alpha, VECTOR3 *color)
{
	if (_ogciSketchBltEx) _ogciSketchBltEx(pSkp, hSrc, s, t, alpha, color);
}

void ogciSketchRotateBlt(oapi::Sketchpad *pSkp, SURFHANDLE hSrc, LPRECT s, int tcx, int tcy, int w, int h, float angle, float alpha, VECTOR3 *color)
{
	if (_ogciSketchRotateBlt) _ogciSketchRotateBlt(pSkp, hSrc, s, tcx, tcy, w, h, angle, alpha, color);
}

#endif
	