// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2013-2016 Jarmo Nikkanen
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


#include <d3d9.h>
#include <d3dx9.h>
#include <set>
#include "gcConst.h"
#include "D3D9Surface.h"
#include "Mesh.h"
#include "D3D9Client.h"

extern class D3D9Client *g_client;

// ===============================================================================================
// Custom SwapChain Interface
// ===============================================================================================
//
HSWAP gcConst::RegisterSwap(HWND hWnd, HSWAP hData, int AA) 
{
	return gcCore::RegisterSwap(hWnd, hData, AA);
}

// ===============================================================================================
//
void gcConst::FlipSwap(HSWAP hSwap)
{ 
	gcCore::FlipSwap(hSwap);
}

// ===============================================================================================
//
SURFHANDLE gcConst::GetRenderTarget(HSWAP hSwap)
{ 
	return gcCore::GetRenderTarget(hSwap);
}

// ===============================================================================================
//
void gcConst::ReleaseSwap(HSWAP hSwap)
{ 
	gcCore::ReleaseSwap(hSwap);
}




// ===============================================================================================
// Custom Camera Interface
// ===============================================================================================
//
CAMERAHANDLE gcConst::SetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags)
{
	return gcCore::SetupCustomCamera(hCam, hVessel, pos, dir, up, fov, hSurf, flags);
}

// ===============================================================================================
//
void gcConst::CustomCameraOnOff(CAMERAHANDLE hCam, bool bOn)
{
	gcCore::CustomCameraOnOff(hCam, bOn);
}

// ===============================================================================================
//
int gcConst::DeleteCustomCamera(CAMERAHANDLE hCam)
{
	return gcCore::DeleteCustomCamera(hCam);
}







// ===============================================================================================
// SketchPad Interface
// ===============================================================================================


// ===============================================================================================
//
int gcConst::SketchpadVersion(Sketchpad* pSkp)
{
	return gcCore::SketchpadVersion(pSkp);
}

// ===============================================================================================
//
oapi::Font* gcConst::CreateSketchpadFont(int height, char* face, int width, int weight, int gcFontStyle, float spacing)
{
	FontStyle sty = FontStyle::FONT_NORMAL;
	if (gcFontStyle & gcFont::ITALIC) sty = (FontStyle)(sty | FontStyle::FONT_ITALIC);
	if (gcFontStyle & gcFont::UNDERLINE) sty = (FontStyle)(sty | FontStyle::FONT_UNDERLINE);
	if (gcFontStyle & gcFont::STRIKEOUT) sty = (FontStyle)(sty | FontStyle::FONT_STRIKEOUT);
	if (gcFontStyle & gcFont::CRISP) sty = (FontStyle)(sty | FontStyle::FONT_CRISP);
	if (gcFontStyle & gcFont::ANTIALIAS) sty = (FontStyle)(sty | FontStyle::FONT_ANTIALIAS);
	return gcCore::CreateSketchpadFont(height, face, width, weight, sty, spacing);
}


// ===============================================================================================
//
HPOLY gcConst::CreatePoly(HPOLY hPoly, const FVECTOR2 *pt, int npt, DWORD flags)
{
	return gcCore::CreatePoly(hPoly, pt, npt, flags);
}


// ===============================================================================================
//
HPOLY gcConst::CreateTriangles(HPOLY hPoly, const gcCore::clrVtx *pt, int npt, DWORD flags)
{
	return gcCore::CreateTriangles(hPoly, pt, npt, flags);
}


// ===============================================================================================
//
void gcConst::DeletePoly(HPOLY hPoly)
{
	gcCore::DeletePoly(hPoly);
}


// ===============================================================================================
//
DWORD gcConst::GetTextLength(oapi::Font *hFont, const char *pText, int len)
{
	return gcCore::GetTextLength(hFont, pText, len);
}


// ===============================================================================================
//
DWORD gcConst::GetCharIndexByPosition(oapi::Font *hFont, const char *pText, int pos, int len)
{
	return gcCore::GetCharIndexByPosition(hFont, pText, pos, len);
}


// ===============================================================================================
//
bool gcConst::RegisterRenderProc(__gcRenderProc proc, DWORD flags, void *pParam)
{
	return g_client->RegisterRenderProc(proc, flags, pParam);
}




// ===============================================================================================
// Mesh interface functions
// ===============================================================================================
//
int gcConst::GetMatrix(int matrix_id, OBJHANDLE hVessel, DWORD mesh, DWORD group, FMATRIX4 *pMat)
{
	return gcCore::GetMatrix((gcCore::MatrixId)matrix_id, hVessel, mesh, group, pMat);
}


// ===============================================================================================
//
int gcConst::SetMatrix(int matrix_id, OBJHANDLE hVessel, DWORD mesh, DWORD group, const FMATRIX4 *pMat)
{
	return gcCore::SetMatrix((gcCore::MatrixId)matrix_id, hVessel, mesh, group, pMat);
}


// ===============================================================================================
//
int	gcConst::MeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, int prop, FVECTOR4* value, bool bSet)
{
	MatProp mat;
	if (prop == MESHM_DIFFUSE) mat = MatProp::Diffuse;
	if (prop == MESHM_AMBIENT) mat = MatProp::Ambient;
	if (prop == MESHM_SPECULAR) mat = MatProp::Specular;
	if (prop == MESHM_EMISSION) mat = MatProp::Light;
	if (prop == MESHM_EMISSION2) mat = MatProp::Emission;
	if (prop == MESHM_REFLECT) mat = MatProp::Reflect;
	if (prop == MESHM_ROUGHNESS) mat = MatProp::Smooth;
	if (prop == MESHM_FRESNEL) mat = MatProp::Fresnel;
	if (prop == MESHM_METALNESS) mat = MatProp::Metal;
	if (prop == MESHM_SPECIALFX) mat = MatProp::SpecialFX;

	if (bSet) return g_client->clbkSetMaterialEx(hMesh, idx, mat, value);
	return g_client->clbkMeshMaterialEx(hMesh, idx, mat, value);
}


// ===============================================================================================
//
DEVMESHHANDLE gcConst::GetDevMesh(MESHHANDLE hMesh)
{
	return g_client->GetDevMesh(hMesh);
}

// ===============================================================================================
//
DEVMESHHANDLE gcConst::LoadDevMeshGlobal(const char* file_name, bool bUseCache)
{
	MESHHANDLE hMesh = oapiLoadMeshGlobal(file_name);
	return g_client->GetDevMesh(hMesh);
}

// ===============================================================================================
//
void gcConst::ReleaseDevMesh(DEVMESHHANDLE hMesh)
{
	delete (D3D9Mesh*)(hMesh);
}

// ===============================================================================================
//
void gcConst::RenderMesh(DEVMESHHANDLE hMesh, const oapi::FMATRIX4* pWorld)
{
	gcCore::RenderMesh(hMesh, pWorld);
}

// ===============================================================================================
//
bool gcConst::PickMesh(PickMeshStruct* pm, DEVMESHHANDLE hMesh, const FMATRIX4* pWorld, short x, short y)
{
	return gcCore::PickMesh((gcCore::PickMeshStruct*)pm, hMesh, pWorld, x, y);
}







// ===============================================================================================
// Custom Render Interface
// ===============================================================================================
//
// ===============================================================================================
//
SURFHANDLE gcConst::LoadSurface(const char* fname, DWORD flags)
{
	return g_client->clbkLoadSurface(fname, flags);
}


// ===============================================================================================
//
void gcConst::RenderLines(const FVECTOR3* pVtx, const WORD* pIdx, int nVtx, int nIdx, const FMATRIX4* pWorld, DWORD color)
{
	gcCore::RenderLines(pVtx, pIdx, nVtx, nIdx, pWorld, color);
}




// ===============================================================================================
// Some Helper Functions
// ===============================================================================================
//
// ===============================================================================================
//
gcConst::PickGround gcConst::ScanScreen(int scr_x, int scr_y)
{
	gcCore::PickGround pg = gcCore::ScanScreen(scr_x, scr_y);
	gcConst::PickGround tg;

	tg.Bounds = pg.Bounds;
	tg.lat = pg.lat;
	tg.lng = pg.lng;
	tg.emax = pg.emax;
	tg.emin = pg.emin;
	tg.msg = pg.msg;
	tg.dist = pg.dist;
	tg.elev = pg.elev;
	tg.level = pg.level;
	tg.hTile = pg.hTile;
	tg.normal = pg.normal;
	tg.pos = pg.pos;
	return tg;
}


// ===============================================================================================
//
void gcConst::GetSystemSpecs(SystemSpecs* sp, int size)
{
	if (size == sizeof(SystemSpecs)) {
		sp->DisplayMode = g_client->GetFramework()->GetDisplayMode();
		sp->MaxTexSize = g_client->GetHardwareCaps()->MaxTextureWidth;
		sp->MaxTexRep = g_client->GetHardwareCaps()->MaxTextureRepeat;
		sp->gcAPIVer = BuildDate();
	}
}


// ===============================================================================================
//
bool gcConst::RegisterGenericProc(__gcGenericProc proc, DWORD id, void* pParam)
{
	return g_client->RegisterGenericProc(proc, id, pParam);
}


// ===============================================================================================
//
HBITMAP	gcConst::LoadBitmapFromFile(const char* fname)
{
	return g_client->gcReadImageFromFile(fname);
}


// ===============================================================================================
//
HWND gcConst::GetRenderWindow()
{
	return g_client->GetRenderWindow();
}


// ===============================================================================================
//
int gcConst::GetElevation(HTILE hTile, double lng, double lat, double *out_elev)
{
	return gcCore2::GetElevation(hTile, lng, lat, out_elev);
}

// ===============================================================================================
//
DWORD gcConst::Color(const COLOUR4* c)
{
	return FVECTOR4(*c).dword_abgr();
}

// ===============================================================================================
//
DWORD gcConst::Color(const oapi::FVECTOR4* c)
{
	return c->dword_abgr();
}

// ===============================================================================================
//
COLOUR4	gcConst::Colour4(DWORD dwABGR)
{
	return FVECTOR4(dwABGR).Colour4();
}
