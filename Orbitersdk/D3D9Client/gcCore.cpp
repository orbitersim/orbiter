// =================================================================================================================================
//
// Copyright (C) 2019 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense copies
// of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) If the Software is distributed in an object code form, it must inform that the source code is available and how to obtain it.
// d) You do not remove or alter any copyright notices contained within the Software.
// e) This copyright notice must be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================


#include <d3d9.h>
#include <d3dx9.h>
#include "gcConst.h"
#include "D3D9Surface.h"
#include "D3D9Client.h"
#include "Scene.h"
#include "VVessel.h"

extern D3D9Client *g_client;

class gcSwap
{
public:
	gcSwap() : pSwap(NULL), hSurf(NULL) { }
	~gcSwap() { Release(); }
	void Release() { SAFE_RELEASE(pSwap); if (hSurf) delete SURFACE(hSurf);	hSurf = NULL; }
	LPDIRECT3DSWAPCHAIN9 pSwap;
	SURFHANDLE hSurf;
};




// ===============================================================================================
// Custom SwapChain Interface
// ===============================================================================================
//
HSWAP gcCore::RegisterSwap(HWND hWnd, HSWAP hData, int AA) 
{ 
	gcSwap * pData = (gcSwap*)hData;

	D3DPRESENT_PARAMETERS pp;
	memset(&pp, 0, sizeof(D3DPRESENT_PARAMETERS));

	pp.BackBufferWidth = 0;
	pp.BackBufferHeight = 0;
	pp.BackBufferFormat = D3DFMT_X8R8G8B8;
	pp.BackBufferCount = 1;
	pp.MultiSampleType = D3DMULTISAMPLE_NONE;
	pp.MultiSampleQuality = 0;
	pp.SwapEffect = D3DSWAPEFFECT_FLIP;
	pp.hDeviceWindow = hWnd;
	pp.Windowed = true;
	pp.EnableAutoDepthStencil = false;
	pp.AutoDepthStencilFormat = D3DFMT_D24S8;
	pp.Flags = 0;
	pp.FullScreen_RefreshRateInHz = D3DPRESENT_RATE_DEFAULT;
	pp.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;

	D3D9Client *pClient = static_cast<D3D9Client *>(this);
	LPDIRECT3DDEVICE9 pDev = pClient->GetDevice();
	LPDIRECT3DSWAPCHAIN9 pSwap = NULL;

	if (pDev->CreateAdditionalSwapChain(&pp, &pSwap) == S_OK) {

		if (!pData) pData = new gcSwap();
		else pData->Release();
		
		LPDIRECT3DSURFACE9 pBack;
		HR(pSwap->GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, &pBack));

		D3D9ClientSurface *pSrf = new D3D9ClientSurface(pDev);
		pSrf->MakeBackBuffer(pBack);

		pData->hSurf = SURFHANDLE(pSrf);
		pData->pSwap = pSwap;

		return HSWAP(pData);
	}
	else {
		LogErr("Failed to create a swapchain. (Feature not supported in true-fullscreen mode)");
		return NULL;
	}
}

// ===============================================================================================
//
void gcCore::FlipSwap(HSWAP hSwap) 
{ 
	((gcSwap*)hSwap)->pSwap->Present(0, 0, 0, 0, 0);
}

// ===============================================================================================
//
SURFHANDLE gcCore::GetRenderTarget(HSWAP hSwap) 
{ 
	return ((gcSwap*)hSwap)->hSurf;
}

// ===============================================================================================
//
void gcCore::ReleaseSwap(HSWAP hSwap) 
{ 
	if (hSwap) delete ((gcSwap*)hSwap);
}






// ===============================================================================================
// Custom Camera Interface
// ===============================================================================================
//
CAMERAHANDLE gcCore::SetupCustomCamera(CAMERAHANDLE hCam, OBJHANDLE hVessel, VECTOR3 &pos, VECTOR3 &dir, VECTOR3 &up, double fov, SURFHANDLE hSurf, DWORD flags)
{
	VECTOR3 x = crossp(up, dir);
	MATRIX3 mTake;
	mTake.m11 = x.x;	mTake.m21 = x.y;	mTake.m31 = x.z;
	mTake.m12 = up.x;	mTake.m22 = up.y;	mTake.m32 = up.z;
	mTake.m13 = dir.x;	mTake.m23 = dir.y;	mTake.m33 = dir.z;
	Scene *pScene = g_client->GetScene();
	return pScene ? pScene->SetupCustomCamera(hCam, hVessel, mTake, pos, fov, hSurf, flags) : NULL;
}

// ===============================================================================================
//
void gcCore::CustomCameraOnOff(CAMERAHANDLE hCam, bool bOn)
{
	Scene *pScene = g_client->GetScene();
	if (pScene) {
		pScene->CustomCameraOnOff(hCam, bOn);
	}
}

// ===============================================================================================
//
int gcCore::DeleteCustomCamera(CAMERAHANDLE hCam)
{
	Scene *pScene = g_client->GetScene();
	return pScene ? pScene->DeleteCustomCamera(hCam) : 0;
}







// ===============================================================================================
// SketchPad Interface
// ===============================================================================================
//
int gcCore::SketchpadVersion(oapi::Sketchpad *pSkp)
{
	if (pSkp->GetTextWidth("_SkpVerInfo") == 2) return 2;
	return 1;
}

// ===============================================================================================
//
SKETCHMESH gcCore::LoadSketchMesh(const char *name)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	SketchMesh *pSM = new SketchMesh(pDev);
	if (pSM->LoadMesh(name)) return pSM;
	delete pSM;
	return NULL;
}

// ===============================================================================================
//
void gcCore::DeleteSketchMesh(SKETCHMESH hMesh)
{
	if (hMesh) delete ((SketchMesh*)hMesh);
}

// ===============================================================================================
//
HPOLY gcCore::CreatePoly(HPOLY hPoly, const FVECTOR2 *pt, int npt, DWORD flags)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	if (!hPoly) return new D3D9PolyLine(pDev, pt, npt, (flags&PF_CONNECT) != 0);
	((D3D9PolyLine *)hPoly)->Update(pt, npt, (flags&PF_CONNECT) != 0);
	return hPoly;
}

// ===============================================================================================
//
HPOLY gcCore::CreateTriangles(HPOLY hPoly, const TriangleVtx *pt, int npt, DWORD flags)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	if (!hPoly) return new D3D9Triangle(pDev, pt, npt, flags);
	((D3D9Triangle *)hPoly)->Update(pt, npt);
	return hPoly;
}

// ===============================================================================================
//
void gcCore::DeletePoly(HPOLY hPoly)
{
	if (hPoly) {
		((D3D9PolyBase *)hPoly)->Release();
		delete ((D3D9PolyBase *)hPoly);
	}
}

// ===============================================================================================
//
DWORD gcCore::GetTextLength(oapi::Font *hFont, const char *pText, int len)
{
	return DWORD((static_cast<D3D9PadFont *>(hFont))->GetTextLength(pText, len));
}

// ===============================================================================================
//
DWORD gcCore::GetCharIndexByPosition(oapi::Font *hFont, const char *pText, int pos, int len)
{
	return DWORD((static_cast<D3D9PadFont *>(hFont))->GetIndexByPosition(pText, pos, len));
}

// ===============================================================================================
//
bool gcCore::RegisterRenderProc(__gcRenderProc proc, DWORD flags, void *pParam)
{
	return g_client->RegisterRenderProc(proc, flags, pParam);
}







// ===============================================================================================
// Mesh interface functions
// ===============================================================================================
//
int gcCore::GetMatrix(int matrix_id, OBJHANDLE hVessel, DWORD mesh, DWORD group, FMATRIX4 *pMat)
{
	if (oapiGetObjectType(hVessel) != OBJTP_VESSEL) return -10;
	Scene *pScn = g_client->GetScene();
	vVessel *pVes = (vVessel *)pScn->GetVisObject(hVessel);
	if (pVes) return pVes->GetMatrixTransform(matrix_id, mesh, group, pMat);
	return -11;
}

// ===============================================================================================
//
int gcCore::SetMatrix(int matrix_id, OBJHANDLE hVessel, DWORD mesh, DWORD group, const FMATRIX4 *pMat)
{
	if (oapiGetObjectType(hVessel) != OBJTP_VESSEL) return -10;
	Scene *pScn = g_client->GetScene();
	vVessel *pVes = (vVessel *)pScn->GetVisObject(hVessel);
	if (pVes) return pVes->SetMatrixTransform(matrix_id, mesh, group, pMat);
	return -11;
}

// ===============================================================================================
//
int gcCore::MeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, int param, COLOUR4 *value, bool bSet)
{
	if (!hMesh) return -4;
	D3D9Mesh *pMesh = (D3D9Mesh *)hMesh;
	return pMesh->Material(idx, param, value, bSet);
}






// ===============================================================================================
// Some Helper Functions
// ===============================================================================================
//
void gcCore::ConvertSurface(SURFHANDLE hSurf, DWORD attrib)
{
	SURFACE(hSurf)->ConvertSurface(attrib);
}

// ===============================================================================================
//
DWORD gcCore::GetSurfaceAttribs(SURFHANDLE hSurf, bool bCreation)
{
	return SURFACE(hSurf)->GetAttribs(bCreation);
}

// ===============================================================================================
//
SURFHANDLE gcCore::LoadSurface(const char *fname, DWORD flags)
{
	return g_client->clbkLoadSurface(fname, flags);
}

// ===============================================================================================
//
bool gcCore::GenerateMipMaps(SURFHANDLE hSurface)
{
	return false;
}

// ===============================================================================================
//
HWND gcCore::GetRenderWindow()
{
	return g_client->GetRenderWindow();
}

// ===============================================================================================
//
DWORD gcCore::Color(const COLOUR4 *c)
{
	return Color((FVECTOR4 *)c);
}

// ===============================================================================================
//
DWORD gcCore::Color(const FVECTOR4 *c)
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

// ===============================================================================================
//
COLOUR4	gcCore::Colour4(DWORD dwABGR)
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

