// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2014 - 2018 Jarmo Nikkanen
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
typedef bool (OGCIFN *__gcRegisterRenderProc)(__gcRenderProc proc, DWORD id, void *pParam);
typedef bool (OGCIFN *__gcRegisterGenericProc)(__gcGenericProc proc, DWORD id, void *pParam);
typedef HWND (OGCIFN *__gcGetRenderWindow)();

// Custom Camera Interface
typedef int   (OGCIFN *__gcDeleteCustomCamera)(CAMERAHANDLE hCam);
typedef void  (OGCIFN *__gcCustomCameraOnOff)(CAMERAHANDLE hCam, bool bOn);
typedef CAMERAHANDLE (OGCIFN *__gcSetupCustomCamera)(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags);

// Custom Sketchpad Goodies
typedef int   (OGCIFN *__gcSketchpadVersion)(oapi::Sketchpad *pSkp);
typedef SKETCHMESH (OGCIFN *__gcLoadSketchMesh)(const char *name);
typedef void (OGCIFN *__gcDeleteSketchMesh)(SKETCHMESH hMesh);
typedef HPOLY (OGCIFN *__gcCreatePoly)(HPOLY hPoly, const FVECTOR2 *pt, int npt, DWORD flags);
typedef void (OGCIFN *__gcDeletePoly)(HPOLY hPoly);
typedef DWORD (OGCIFN *__gcGetTextLength)(oapi::Font *hFont, const char *pText, int len);
typedef DWORD (OGCIFN *__gcGetCharIndexByPosition)(oapi::Font *hFont, const char *pText, int pos, int len);
typedef HPOLY (OGCIFN *__gcCreateTriangles)(HPOLY hPoly, const TriangleVtx *pt, int npt, DWORD flags);

// Mesh interface functions
typedef int	(OGCIFN *__gcMeshMaterial)(DEVMESHHANDLE hMesh, DWORD idx, int prop, COLOUR4 *value, bool bSet);

// Helper functiond
typedef bool (OGCIFN *__gcWorldToScreenSpace)(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, const oapi::FMATRIX4 *pVP, float clip);

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
__gcMeshMaterial _gcMeshMaterial = NULL;
__gcRegisterGenericProc _gcRegisterGenericProc = NULL;
__gcGetRenderWindow _gcGetRenderWindow = NULL;
__gcGetTextLength _gcGetTextLength = NULL;
__gcGetCharIndexByPosition _gcGetCharIndexByPosition = NULL;
__gcCreateTriangles _gcCreateTriangles = NULL;


// ====================================================================================================
//
bool PostInit(HMODULE hClient)
{
	if (!hClient) return false;

	_gcClientID = (__gcClientID)GetProcAddress(hClient, "gcClientID");
	_gcGetSurfaceAttribs = (__gcGetSurfaceAttribs)GetProcAddress(hClient, "gcGetSurfaceAttribs");
	_gcConvertSurface = (__gcConvertSurface)GetProcAddress(hClient, "gcConvertSurface");
	_gcGenerateMipMaps = (__gcGenerateMipMaps)GetProcAddress(hClient, "gcGenerateMipMaps");
	_gcGetRenderWindow = (__gcGetRenderWindow)GetProcAddress(hClient, "gcGetRenderWindow");
	_gcRegisterRenderProc = (__gcRegisterRenderProc)GetProcAddress(hClient, "gcRegisterRenderProc");
	_gcRegisterGenericProc = (__gcRegisterGenericProc)GetProcAddress(hClient, "gcRegisterGenericProc");
	// -------------
	_gcDeleteCustomCamera = (__gcDeleteCustomCamera)GetProcAddress(hClient, "gcDeleteCustomCamera");
	_gcCustomCameraOnOff = (__gcCustomCameraOnOff)GetProcAddress(hClient, "gcCustomCameraOnOff");
	_gcSetupCustomCamera = (__gcSetupCustomCamera)GetProcAddress(hClient, "gcSetupCustomCamera");
	// -------------
	_gcSketchpadVersion = (__gcSketchpadVersion)GetProcAddress(hClient, "gcSketchpadVersion");
	_gcLoadSketchMesh = (__gcLoadSketchMesh)GetProcAddress(hClient, "gcLoadSketchMesh");
	_gcDeleteSketchMesh = (__gcDeleteSketchMesh)GetProcAddress(hClient, "gcDeleteSketchMesh");
	_gcCreatePoly = (__gcCreatePoly)GetProcAddress(hClient, "gcCreatePoly");
	_gcCreateTriangles = (__gcCreateTriangles)GetProcAddress(hClient, "gcCreateTriangles");
	_gcDeletePoly = (__gcDeletePoly)GetProcAddress(hClient, "gcDeletePoly");
	_gcGetTextLength = (__gcGetTextLength)GetProcAddress(hClient, "gcGetTextLength");
	_gcGetCharIndexByPosition = (__gcGetCharIndexByPosition)GetProcAddress(hClient, "gcGetCharIndexByPosition");
	// -------------
	_gcWorldToScreenSpace = (__gcWorldToScreenSpace)GetProcAddress(hClient, "gcWorldToScreenSpace");
	// -------------
	_gcMeshMaterial = (__gcMeshMaterial)GetProcAddress(hClient, "gcMeshMaterial");

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
int	gcMeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, int prop, COLOUR4 *value, bool bSet)
{
	if (_gcMeshMaterial) return _gcMeshMaterial(hMesh, idx, prop, value, bSet);	
	return -5;	
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
bool gcRegisterGenericProc(__gcGenericProc proc, DWORD id, void *pParam)
{
	if (_gcRegisterGenericProc) return _gcRegisterGenericProc(proc, id, pParam);
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
HPOLY gcCreatePoly(HPOLY hPoly, const FVECTOR2 *pt, int npt, DWORD flags)
{
	if (_gcCreatePoly) return _gcCreatePoly(hPoly, pt, npt, flags);
	return NULL;
}

// ====================================================================================================
//
HPOLY gcCreateTriangles(HPOLY hPoly, const TriangleVtx *pt, int npt, DWORD flags)
{
	if (_gcCreateTriangles) return _gcCreateTriangles(hPoly, pt, npt, flags);
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
bool gcWorldToScreenSpace(const VECTOR3 &rdir, oapi::IVECTOR2 *pt, const oapi::FMATRIX4 *pVP, float clip)
{
	if (_gcWorldToScreenSpace) return _gcWorldToScreenSpace(rdir, pt, pVP, clip);
	return false;
}

// ====================================================================================================
//
DWORD gcGetTextLength(oapi::Font *hFont, const char *pText, int len)
{
	if (_gcGetTextLength) return _gcGetTextLength(hFont, pText, len);
	return 0;
}

// ====================================================================================================
//
DWORD gcGetCharIndexByPosition(oapi::Font *hFont, const char *pText, int pos, int len)
{
	if (_gcGetCharIndexByPosition) return _gcGetCharIndexByPosition(hFont, pText, pos, len);
	return 0;
}


// ====================================================================================================
//
HWND gcGetRenderWindow()
{
	if (_gcGetRenderWindow) return _gcGetRenderWindow();
	return NULL;
}




// =====================================================================================
// gcAPI Functions
// =====================================================================================

DWORD gcColor(const COLOUR4 *c)
{
	return gcColor((FVECTOR4 *)c);
}


DWORD gcColor(const FVECTOR4 *c)
{
	DWORD r = DWORD(c->r * 255.0f + 0.5f);
	DWORD g = DWORD(c->g * 255.0f + 0.5f);
	DWORD b = DWORD(c->b * 255.0f + 0.5f);
	DWORD a = DWORD(c->a * 255.0f + 0.5f);

	if (r > 0xFF) r = 0xFF;
	if (g > 0xFF) g = 0xFF;
	if (b > 0xFF) b = 0xFF;
	if (a > 0xFF) a = 0xFF;
	if (a == 0) a = 1;
	
	return (a << 24) | (b << 16) | (g << 8) | r;
}


COLOUR4	gcColour4(DWORD dwABGR)
{
	DWORD r = (dwABGR & 0xFF); dwABGR >>= 8;
	DWORD g = (dwABGR & 0xFF); dwABGR >>= 8;
	DWORD b = (dwABGR & 0xFF); dwABGR >>= 8;
	DWORD a = (dwABGR & 0xFF);
	if (a == 0) a = 255;
	COLOUR4 c;
	float q = 3.92156862e-3f;
	c.r = float(r) * q;
	c.g = float(g) * q;
	c.b = float(b) * q;
	c.a = float(a) * q;
	return c;
}


MATRIX4 gcMatrix4(const FMATRIX4 *M)
{
	MATRIX4 D;
	D.m11 = (double)M->m11;  D.m12 = (double)M->m12;  D.m13 = (double)M->m13;  D.m14 = (double)M->m14;
	D.m21 = (double)M->m21;  D.m22 = (double)M->m22;  D.m23 = (double)M->m23;  D.m24 = (double)M->m24;
	D.m31 = (double)M->m31;  D.m32 = (double)M->m32;  D.m33 = (double)M->m33;  D.m34 = (double)M->m34;
	D.m41 = (double)M->m41;  D.m42 = (double)M->m42;  D.m43 = (double)M->m43;  D.m44 = (double)M->m44;
	return D;
}


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