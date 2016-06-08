// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2014 - 2016 Jarmo Nikkanen
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

#include <windows.h>
#include "gcAPI.h"
#include "Sketchpad2.h"
#include "OrbiterAPI.h"

// Function Prototypes
// -------------------------------------------------------------------------------------------------------------------------------------

typedef DWORD (OGCIFN *__gcClientID)();
typedef DWORD (OGCIFN *__gcGetSurfaceAttribs)(SURFHANDLE hSurf, bool bCreation);
typedef void (OGCIFN *__gcConvertSurface)(SURFHANDLE hSurf, DWORD attrib);
typedef bool (OGCIFN *__gcGenerateMipMaps)(SURFHANDLE hSurface);
typedef bool (OGCIFN *__gcRegisterSkinName)(const VISHANDLE hVisual, const char *name);
typedef bool (OGCIFN *__gcRegisterRenderProc)(__gcRenderProc proc, DWORD id, void *pParam);

// Custom Camera Interface
typedef int   (OGCIFN *__gcDeleteCustomCamera)(CAMERAHANDLE hCam);
typedef void  (OGCIFN *__gcCustomCameraOnOff)(CAMERAHANDLE hCam, bool bOn);
typedef CAMERAHANDLE (OGCIFN *__gcSetupCustomCamera)(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags);

// Custom Sketchpad Goodies
typedef int   (OGCIFN *__gcSketchpadVersion)(oapi::Sketchpad *pSkp);
typedef SKETCHMESH (OGCIFN *__gcLoadSketchMesh)(const char *name);
typedef void (OGCIFN *__gcDeleteSketchMesh)(SKETCHMESH hMesh);
typedef HPOLY (OGCIFN *__gcCreatePoly)(HPOLY hPoly, const FVECTOR2 *pt, int npt, PolyFlags flags);
typedef void (OGCIFN *__gcDeletePoly)(HPOLY hPoly);

// Helper functiond
typedef bool (OGCIFN *__gcWorldToScreenSpace)(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, float clip);

bool bValid = false;

HMODULE gcHandle = NULL;

__gcClientID _gcClientID = NULL;
__gcGetSurfaceAttribs _gcGetSurfaceAttribs = NULL;
__gcConvertSurface _gcConvertSurface = NULL;
__gcDeleteCustomCamera _gcDeleteCustomCamera = NULL;
__gcCustomCameraOnOff _gcCustomCameraOnOff = NULL;
__gcSetupCustomCamera _gcSetupCustomCamera = NULL;
__gcSketchpadVersion _gcSketchpadVersion = NULL;
__gcGenerateMipMaps _gcGenerateMipMaps = NULL;
__gcRegisterRenderProc _gcRegisterRenderProc = NULL;
__gcLoadSketchMesh _gcLoadSketchMesh = NULL;
__gcDeleteSketchMesh _gcDeleteSketchMesh = NULL;
__gcWorldToScreenSpace _gcWorldToScreenSpace = NULL;
__gcCreatePoly _gcCreatePoly = NULL;
__gcDeletePoly _gcDeletePoly = NULL;


// ====================================================================================================
//
bool PostInit(HMODULE hClient)
{
	if (!hClient) return false;

	_gcClientID = (__gcClientID)GetProcAddress(hClient, "gcClientID");
	_gcGetSurfaceAttribs = (__gcGetSurfaceAttribs)GetProcAddress(hClient, "gcGetSurfaceAttribs");
	_gcConvertSurface = (__gcConvertSurface)GetProcAddress(hClient, "gcConvertSurface");
	_gcGenerateMipMaps = (__gcGenerateMipMaps)GetProcAddress(hClient, "gcGenerateMipMaps");
	_gcRegisterRenderProc = (__gcRegisterRenderProc)GetProcAddress(hClient, "gcRegisterRenderProc");
	// -------------
	_gcDeleteCustomCamera = (__gcDeleteCustomCamera)GetProcAddress(hClient, "gcDeleteCustomCamera");
	_gcCustomCameraOnOff = (__gcCustomCameraOnOff)GetProcAddress(hClient, "gcCustomCameraOnOff");
	_gcSetupCustomCamera = (__gcSetupCustomCamera)GetProcAddress(hClient, "gcSetupCustomCamera");
	// -------------
	_gcSketchpadVersion = (__gcSketchpadVersion)GetProcAddress(hClient, "gcSketchpadVersion");
	_gcLoadSketchMesh = (__gcLoadSketchMesh)GetProcAddress(hClient, "gcLoadSketchMesh");
	_gcDeleteSketchMesh = (__gcDeleteSketchMesh)GetProcAddress(hClient, "gcDeleteSketchMesh");
	_gcCreatePoly = (__gcCreatePoly)GetProcAddress(hClient, "gcCreatePoly");
	_gcDeletePoly = (__gcDeletePoly)GetProcAddress(hClient, "gcDeletePoly");
	// -------------
	_gcWorldToScreenSpace = (__gcWorldToScreenSpace)GetProcAddress(hClient, "gcWorldToScreenSpace");

	return (_gcClientID!=NULL);
}

// ====================================================================================================
//
bool gcEnabled()
{
	if (gcHandle && _gcClientID) return true;
	return false;
}

// ====================================================================================================
//
bool gcInitialize()
{
	if (gcHandle) return gcEnabled(); // Already Initialized
	if (!gcHandle) gcHandle = GetModuleHandle("D3D9Client.dll");
	if (!gcHandle) gcHandle = GetModuleHandle("D3D9ClientDebug.dll");
	if (!gcHandle) gcHandle = GetModuleHandle("D3D11Client.dll");
	if (gcHandle) return PostInit(gcHandle);
	else oapiWriteLog("gcAPI: [Graphics Client Not Found]");
	return false;
}

// ====================================================================================================
//
DWORD gcClientID()
{
	if (_gcClientID) return _gcClientID();
	return 0;
}

// ====================================================================================================
//
bool gcGenerateMipMaps(SURFHANDLE hSurface)
{
	if (_gcGenerateMipMaps) return _gcGenerateMipMaps(hSurface);	
	return false;	
}
		
// ====================================================================================================
//
bool gcRegisterRenderProc(__gcRenderProc proc, DWORD id, void *pParam)
{
	if (_gcRegisterRenderProc) return _gcRegisterRenderProc(proc, id, pParam);
	return false;
}

// ====================================================================================================
//
DWORD gcGetSurfaceAttribs(SURFHANDLE hSurf, bool bCreation)
{
	if (_gcGetSurfaceAttribs) return _gcGetSurfaceAttribs(hSurf, bCreation);
	return 0;
}

// ====================================================================================================
//
void gcConvertSurface(SURFHANDLE hSurf, DWORD attrib)
{
	if (_gcConvertSurface) return _gcConvertSurface(hSurf, attrib);	
}

// ====================================================================================================
//
int	gcDeleteCustomCamera(CAMERAHANDLE hCam)
{
	if (_gcDeleteCustomCamera) return _gcDeleteCustomCamera(hCam);
	return 0;
}

// ====================================================================================================
//
void gcCustomCameraOnOff(CAMERAHANDLE hCam, bool bOn)
{
	if (_gcCustomCameraOnOff) _gcCustomCameraOnOff(hCam, bOn);
}

// ====================================================================================================
//
CAMERAHANDLE gcSetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags)
{
	if (_gcSetupCustomCamera) return _gcSetupCustomCamera(hCam, hVessel, pos, dir, up, fov, hSurf, flags);
	return NULL;
}

// ====================================================================================================
//
int gcSketchpadVersion(oapi::Sketchpad *pSkp)
{
	if (_gcSketchpadVersion) return _gcSketchpadVersion(pSkp);
	return 1;
}

// ====================================================================================================
//
SKETCHMESH gcLoadSketchMesh(const char *name)
{
	if (_gcLoadSketchMesh) return _gcLoadSketchMesh(name);
	return NULL;
}

// ====================================================================================================
//
void gcDeleteSketchMesh(SKETCHMESH hMesh)
{
	if (_gcDeleteSketchMesh) _gcDeleteSketchMesh(hMesh);
}

// ====================================================================================================
//
HPOLY gcCreatePoly(HPOLY hPoly, const FVECTOR2 *pt, int npt, PolyFlags flags)
{
	if (_gcCreatePoly) return _gcCreatePoly(hPoly, pt, npt, flags);
	return NULL;
}

// ====================================================================================================
//
void gcDeletePoly(HPOLY hPoly)
{
	if (_gcDeletePoly) _gcDeletePoly(hPoly);
}



// ====================================================================================================
//
bool gcWorldToScreenSpace(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, float clip)
{
	if (_gcWorldToScreenSpace) return _gcWorldToScreenSpace(rdir, pt, clip);
	return false;
}





// =====================================================================================
// gcAPI Functions
// =====================================================================================

void gcSetTranslation(FMATRIX4 *mat, const VECTOR3 &pos)
{
	mat->m41 = float(pos.x);
	mat->m42 = float(pos.y);
	mat->m43 = float(pos.z);
}

void gcWorldMatrix(FMATRIX4 *mat, const VECTOR3 &pos, const VECTOR3 &x, const VECTOR3 &z, double scale)
{
	VECTOR3 y = crossp(x, z);

	mat->m11 = float(x.x * scale);
	mat->m12 = float(x.y * scale);
	mat->m13 = float(x.z * scale);
	mat->m14 = 0.0f;

	mat->m21 = float(y.x * scale);
	mat->m22 = float(y.y * scale);
	mat->m23 = float(y.z * scale);
	mat->m24 = 0.0f;

	mat->m31 = float(z.x * scale);
	mat->m32 = float(z.y * scale);
	mat->m33 = float(z.z * scale);
	mat->m34 = 0.0f;

	mat->m41 = float(pos.x);
	mat->m42 = float(pos.y);
	mat->m43 = float(pos.z);
	mat->m44 = 1.0f;
}