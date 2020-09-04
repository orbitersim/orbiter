// =================================================================================================================================
//
// Copyright (C) 2020 Jarmo Nikkanen
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
#include "VPlanet.h"
#include "Surfmgr2.h"
#include "WindowMgr.h"

extern D3D9Client *g_client;
extern WindowManager *g_pWM;
extern std::set<Font *> g_fonts;


class gcSwap
{
	DWORD alloc_id;
public:
	gcSwap() : alloc_id('gcSW'), pSwap(NULL), hSurf(NULL) { }
	~gcSwap() { Release(); }
	void Release() { SAFE_RELEASE(pSwap); if (hSurf) delete SURFACE(hSurf);	hSurf = NULL; }
	LPDIRECT3DSWAPCHAIN9 pSwap;
	LPDIRECT3DSURFACE9 pSurf;
	SURFHANDLE hSurf;
};


struct TexEx
{
	static const int size = 4;
	TexEx() { for (int i = 0; i < size; i++) pArray[i] = NULL; }
	bool HasAny() { for (int i = 0; i < size; i++) if (pArray[i]) return true; return false; }
	void Release() { for (int i = 0; i < size; i++) SAFE_RELEASE(pArray[i]); }
	LPDIRECT3DTEXTURE9 pArray[size];
};


void DumpResource(LPDIRECT3DRESOURCE9 pResource)
{
	D3DSURFACE_DESC desc;

	if (pResource->GetType() == D3DRTYPE_SURFACE) {
		LPDIRECT3DSURFACE9 pSurf = static_cast<LPDIRECT3DSURFACE9>(pResource);
		pSurf->GetDesc(&desc);
	}
	else if (pResource->GetType() == D3DRTYPE_TEXTURE) {
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(pResource);
		pTex->GetLevelDesc(0, &desc);
		oapiWriteLogV("SRF_DUMP: Mips = %d", pTex->GetLevelCount());
	}

	oapiWriteLogV("SRF_DUMP: Usage = 0x%X", desc.Usage);
	oapiWriteLogV("SRF_DUMP: Pool = %u", desc.Pool);
	oapiWriteLogV("SRF_DUMP: Format = %u", desc.Format);
	oapiWriteLogV("SRF_DUMP: Multisample = %u", desc.MultiSampleType);
	oapiWriteLogV("SRF_DUMP: Size = (%u, %u)", desc.Width, desc.Height);
}


DWORD ConvertFormat_DX_to_OAPI(DWORD Format)
{
	DWORD Out = OAPISURFACE_NOALPHA;
	if (Format == D3DFMT_D24S8) return Out | OAPISURFACE_PF_DEPTH;
	if (Format == D3DFMT_X8R8G8B8) return Out | OAPISURFACE_PF_XRGB;
	if (Format == D3DFMT_R5G6B5) return Out | OAPISURFACE_PF_RGB565;
	if (Format == D3DFMT_L16) return Out | OAPISURFACE_PF_S16R;
	if (Format == D3DFMT_R16F) return Out | OAPISURFACE_PF_F16R;
	if (Format == D3DFMT_G16R16F) return Out | OAPISURFACE_PF_F16RG;
	if (Format == D3DFMT_R32F) return Out | OAPISURFACE_PF_F32R;
	if (Format == D3DFMT_G32R32F) return Out | OAPISURFACE_PF_F32RG;
	if (Format == D3DFMT_DXT1) return Out | OAPISURFACE_PF_DXT1;
	if (Format == D3DFMT_L8) return Out | OAPISURFACE_PF_GRAY;
	Out = OAPISURFACE_ALPHA;
	if (Format == D3DFMT_A8) return Out | OAPISURFACE_PF_ALPHA;
	if (Format == D3DFMT_A32B32G32R32F) return Out | OAPISURFACE_PF_F32RGBA;
	if (Format == D3DFMT_A16B16G16R16F) return Out | OAPISURFACE_PF_F16RGBA;
	if (Format == D3DFMT_A8R8G8B8) return Out | OAPISURFACE_PF_ARGB;
	if (Format == D3DFMT_DXT3) return Out | OAPISURFACE_PF_DXT3;
	if (Format == D3DFMT_DXT5) return Out | OAPISURFACE_PF_DXT5;
	return 0;
}


DWORD ConvertFormat_OAPI_to_DX(DWORD Format)
{
	if ((Format & OAPISURFACE_PF_MASK) == 0) {
		if (Format & OAPISURFACE_ALPHA) return D3DFMT_A8R8G8B8;
		if (Format & OAPISURFACE_NOALPHA) return D3DFMT_X8R8G8B8;
		return 0;
	}

	Format &= OAPISURFACE_PF_MASK;

	if (Format == OAPISURFACE_PF_DEPTH) return D3DFMT_D24S8;
	if (Format == OAPISURFACE_PF_XRGB) return D3DFMT_X8R8G8B8;
	if (Format == OAPISURFACE_PF_RGB565) return D3DFMT_R5G6B5;
	if (Format == OAPISURFACE_PF_S16R) return D3DFMT_L16;
	if (Format == OAPISURFACE_PF_F16R) return D3DFMT_R16F;
	if (Format == OAPISURFACE_PF_F16RG) return D3DFMT_G16R16F;
	if (Format == OAPISURFACE_PF_F32R) return D3DFMT_R32F;
	if (Format == OAPISURFACE_PF_F32RG) return D3DFMT_G32R32F;
	if (Format == OAPISURFACE_PF_DXT1) return D3DFMT_DXT1;
	if (Format == OAPISURFACE_PF_F32RGBA) return D3DFMT_A32B32G32R32F;
	if (Format == OAPISURFACE_PF_F16RGBA) return D3DFMT_A16B16G16R16F;
	if (Format == OAPISURFACE_PF_ARGB) return D3DFMT_A8R8G8B8;
	if (Format == OAPISURFACE_PF_DXT3) return D3DFMT_DXT3;
	if (Format == OAPISURFACE_PF_DXT5) return D3DFMT_DXT5;
	if (Format == OAPISURFACE_PF_GRAY) return D3DFMT_L8;
	if (Format == OAPISURFACE_PF_ALPHA) return D3DFMT_A8;
	return 0;
}




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
		pData->pSurf = pBack;

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
HSURFNATIVE gcCore::GetRenderTargetNative(HSWAP hSwap)
{
	return ((gcSwap*)hSwap)->pSurf;
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
Sketchpad* gcCore::GetSketchpadNative(HSURFNATIVE hSrf, HSURFNATIVE hDep)
{
	return g_client->GetSketchpadNative(hSrf, hDep);
}


// ===============================================================================================
//
void gcCore::ReleaseSketchpadNative(Sketchpad *pSkp)
{
	g_client->ReleaseSketchpadNative(pSkp);
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
oapi::Font *gcCore::CreateSketchpadFont(int height, char *face, int width, int weight, int style, float spacing)
{
	_TRACE;
	return *g_fonts.insert(new D3D9PadFont(height, face, width, weight, style, spacing)).first;
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
// Tile access interface functions
// ===============================================================================================
//

HPLANETMGR gcCore::GetPlanetManager(OBJHANDLE hPlanet)
{
	Scene *pScene = g_client->GetScene();
	vPlanet *vPl = (vPlanet *)pScene->GetVisObject(hPlanet);
	return HPLANETMGR(vPl);
}


// ===============================================================================================
//
HTILE gcCore::GetTile(HPLANETMGR vPl, double lng, double lat, int maxlevel)
{
	vPlanet *vP = static_cast<vPlanet *>(vPl);
	return HTILE(vP->FindTile(lng, lat, maxlevel));
}


// ===============================================================================================
//
gcCore::PickGround gcCore::GetTileData(HPLANETMGR vPl, double lng, double lat, int maxlevel)
{
	PickGround pg; memset(&pg, 0, sizeof(PickGround));
	if (!vPl) return pg;

	vPlanet *vP = static_cast<vPlanet *>(vPl);
	SurfTile *pTile = static_cast<SurfTile *>(vP->FindTile(lng, lat, maxlevel));

	if (!pTile) {
		oapiWriteLogV("gcCore::FindTile() Failed");
		return pg;
	}

	pTile->GetIndex(&pg.iLng, &pg.iLat);
	pTile->GetElevation(lng, lat, &pg.elev, &pg.normal, NULL, true, false);

	VECTOR3 pos = vP->GetUnitSurfacePos(lng, lat) * (vP->GetSize() + pg.elev);
	MATRIX3 mRot; oapiGetRotationMatrix(vP->GetObjectA(), &mRot);

	pos = mul(mRot, pos) + vP->PosFromCamera();

	pg.Bounds.left = pTile->bnd.minlng;
	pg.Bounds.right = pTile->bnd.maxlng;
	pg.Bounds.top = pTile->bnd.maxlat;
	pg.Bounds.bottom = pTile->bnd.minlat;

	pg.lat = lat;
	pg.lng = lng;

	pg.emax = float(pTile->GetMaxElev());
	pg.emin = float(pTile->GetMinElev());

	pg.msg = 0;
	pg.dist = length(pos);
	pg.level = pTile->Level();
	pg.hTile = HTILE(pTile);
	pg.pos = FVECTOR3(pos);

	return pg;
}


// ===============================================================================================
//
gcCore::PickGround gcCore::ScanScreen(int scr_x, int scr_y)
{
	PickGround pg; memset(&pg, 0, sizeof(PickGround));

	Scene *pScene = g_client->GetScene();
	TILEPICK tp = pScene->PickSurface(scr_x, scr_y);
	SurfTile *pTile = static_cast<SurfTile *>(tp.pTile);

	if (pTile) {

		pTile->GetIndex(&pg.iLng, &pg.iLat);

		pg.Bounds.left = pTile->bnd.minlng;
		pg.Bounds.right = pTile->bnd.maxlng;
		pg.Bounds.top = pTile->bnd.maxlat;
		pg.Bounds.bottom = pTile->bnd.minlat;

		pg.lat = tp.lat;
		pg.lng = tp.lng;

		pg.emax = float(pTile->GetMaxElev());
		pg.emin = float(pTile->GetMinElev());

		pg.msg = 0;
		pg.dist = tp.d;
		pg.elev = tp.elev;
		pg.level = pTile->Level();
		pg.hTile = HTILE(pTile);
		pg.normal = _FV(tp._n);
		pg.pos = _FV(tp._p);
	}
	return pg;
}


// ===============================================================================================
//
void * gcCore::SeekTileElevation(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags, ElevInfo *pInfo)
{
	return NULL;
}


// ===============================================================================================
//
HSURFNATIVE gcCore::SeekTileTexture(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags, void *reserved)
{
	if (!hMgr) return NULL;
	if (((vPlanet *)(hMgr))->SurfMgr2()) {
		return ((vPlanet *)(hMgr))->SurfMgr2()->SeekTileTexture(iLng, iLat, level, flags);
	}
	return NULL;
}


// ===============================================================================================
//
bool gcCore::HasTileData(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags)
{
	if (!hMgr) return false;
	if (((vPlanet *)(hMgr))->SurfMgr2()) {
		return ((vPlanet *)(hMgr))->SurfMgr2()->HasTileData(iLng, iLat, level, flags);
	}
	return false;
}


// ===============================================================================================
//
int gcCore::GetElevation(HTILE hTile, double lng, double lat, double *out_elev)
{
	SurfTile *pTile = static_cast<SurfTile *>(hTile);
	return pTile->GetElevation(lng, lat, out_elev, NULL, NULL, true, true);
}


// ===============================================================================================
//
HSURFNATIVE	gcCore::SetTileOverlay(HTILE hTile, const HSURFNATIVE hOverlay)
{
	//SurfTile *pTile = static_cast<SurfTile *>(hTile);
	//LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(hOverlay);
	//return HSURFNATIVE(pTile->SetOverlay(pTex, true));
	return NULL;
}


// ===============================================================================================
//
HOVERLAY gcCore::AddGlobalOverlay(HPLANETMGR hMgr, VECTOR4 mmll, const HSURFNATIVE hOverlay, HOVERLAY hOld)
{
	if (!hMgr) return NULL;
	vPlanet *vP = static_cast<vPlanet *>(hMgr);
	LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(hOverlay);
	vPlanet::sOverlay *oLay = static_cast<vPlanet::sOverlay *>(hOld);
	return vP->AddOverlaySurface(mmll, pTex, oLay);
}







// ===============================================================================================
// Custom Render Interface
// ===============================================================================================
//

HSURFNATIVE gcCore::LoadSurfaceNative(const char *file, DWORD flags)
{
	static const char *lbl[] = { "_norm", "_refl", "_spec", "_rghn" };

	LPDIRECT3DTEXTURE9 pTex = NULL;
	//TexEx tex;
	char path[MAX_PATH];

	/*if (!g_client->TexturePath(file, path)) {
		oapiWriteLogV("LoadSurfaceNative: File Not Found [%s]", path);
		return NULL;
	}*/

	if (flags == OAPISURFACE_TEXTURE) 
	{
		/*for (int i = 0; i < 4; i++) if (g_client->TexturePathEx(file, path, lbl[i])) 
			D3DXCreateTextureFromFileA(g_client->GetDevice(), path, &tex.pArray[i])	
		pTex->SetPrivateData(TextureArray, &tex, sizeof(tex), 0);*/

		HR(D3DXCreateTextureFromFileExA(g_client->GetDevice(), file, D3DX_DEFAULT_NONPOW2, D3DX_DEFAULT_NONPOW2, 0,
			0, D3DFMT_FROM_FILE, D3DPOOL_DEFAULT, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex));

		return HSURFNATIVE(pTex);
	}

	D3DXIMAGE_INFO info;
	HR(D3DXGetImageInfoFromFileA(path, &info));

	DWORD Mips = D3DX_FROM_FILE;
	DWORD Usage = 0;
	D3DFORMAT Format = D3DFMT_FROM_FILE;
	D3DPOOL Pool = D3DPOOL_DEFAULT;

	assert((flags & OAPISURFACE_RENDER3D) == 0);

	if (info.ImageFileFormat == D3DXIFF_JPG) Format = D3DFMT_X8R8G8B8;
	if (info.ImageFileFormat == D3DXIFF_PNG) Format = D3DFMT_X8R8G8B8;
	if (info.ImageFileFormat == D3DXIFF_BMP) Format = D3DFMT_X8R8G8B8;

	if (flags & OAPISURFACE_UNCOMPRESS) {
		if (info.Format == D3DFMT_DXT1) Format = D3DFMT_X8R8G8B8;
		if (info.Format == D3DFMT_DXT5) Format = D3DFMT_A8R8G8B8;
		if (info.Format == D3DFMT_DXT3) Format = D3DFMT_A8R8G8B8;
	}

	if (flags & OAPISURFACE_RENDERTARGET) Usage = D3DUSAGE_RENDERTARGET;
	if (flags & OAPISURFACE_GDI) Usage = D3DUSAGE_DYNAMIC;
	if (flags & OAPISURFACE_SYSMEM) Pool = D3DPOOL_SYSTEMMEM;
	if (flags & OAPISURFACE_NOMIPMAPS) Mips = 1;
	if (flags & OAPISURFACE_MIPMAPS) Mips = 0;
	
	D3DFORMAT Fmt = D3DFORMAT(ConvertFormat_OAPI_to_DX(flags));
	if (Fmt != 0) Format = Fmt;


	if (flags & OAPISURFACE_TEXTURE) {
		HR(D3DXCreateTextureFromFileExA(g_client->GetDevice(), path, D3DX_DEFAULT_NONPOW2, D3DX_DEFAULT_NONPOW2, Mips,
			Usage, Format, Pool, D3DX_DEFAULT, D3DX_DEFAULT, 0, NULL, NULL, &pTex));
		return HSURFNATIVE(pTex);
	}

	if (flags & OAPISURFACE_RENDERTARGET) {
		LPDIRECT3DSURFACE9 pSurf = NULL;
		HR(g_client->GetDevice()->CreateRenderTarget(info.Width, info.Height, Format, D3DMULTISAMPLE_NONE, 0, false, &pSurf, NULL));
		HR(D3DXLoadSurfaceFromFile(pSurf, NULL, NULL, path, NULL, D3DX_DEFAULT, 0, NULL));
		return HSURFNATIVE(pSurf);	
	}

	assert(false);
	return NULL;
}


// ===============================================================================================
//
bool gcCore::SaveSurfaceNative(const char *file, HSURFNATIVE hSrf)
{
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	LPDIRECT3DRESOURCE9 pResource = static_cast<LPDIRECT3DRESOURCE9>(hSrf);

	D3DXIMAGE_FILEFORMAT fmt = D3DXIMAGE_FILEFORMAT(0);

	if (contains(file, ".dds")) fmt = D3DXIFF_DDS;
	if (contains(file, ".bmp")) fmt = D3DXIFF_BMP;
	if (contains(file, ".jpg")) fmt = D3DXIFF_JPG;
	if (contains(file, ".png")) fmt = D3DXIFF_PNG;

	if (pResource->GetType() == D3DRTYPE_SURFACE) {
		LPDIRECT3DSURFACE9 pSurf = static_cast<LPDIRECT3DSURFACE9>(hSrf);
		if (D3DXSaveSurfaceToFileA(file, fmt, pSurf, NULL, NULL) == S_OK) return true;
		oapiWriteLog("gcCore::SaveSurfaceNative(SURF):");
		DumpResource(static_cast<LPDIRECT3DRESOURCE9>(hSrf));
		return false;
	}


	if (pResource->GetType() == D3DRTYPE_TEXTURE) {

		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(hSrf);
		D3DSURFACE_DESC desc;
		pTex->GetLevelDesc(0, &desc);

		if (desc.Pool == D3DPOOL_SYSTEMMEM || desc.Usage&D3DUSAGE_DYNAMIC) {
			if (D3DXSaveTextureToFileA(file, fmt, pTex, NULL) == S_OK) return true;
			oapiWriteLog("gcCore::SaveSurfaceNative(TEX):");
			DumpResource(static_cast<LPDIRECT3DRESOURCE9>(hSrf));
			return false;
		}

		if (desc.Usage&D3DUSAGE_RENDERTARGET) {
			LPDIRECT3DTEXTURE9 pSys = NULL;
			DWORD Mips = pTex->GetLevelCount();
			HR(D3DXCreateTexture(pDev, desc.Width, desc.Height, Mips, 0, desc.Format, D3DPOOL_SYSTEMMEM, &pSys));
			for (DWORD i = 0; i < Mips; i++) {
				LPDIRECT3DSURFACE9 pSrc, pTgt;
				HR(pTex->GetSurfaceLevel(i, &pSrc));
				HR(pSys->GetSurfaceLevel(i, &pTgt));
				HR(pDev->GetRenderTargetData(pSrc, pTgt));
				pSrc->Release();
				pTgt->Release();
			}
			if (D3DXSaveTextureToFile(file, fmt, pSys, NULL) == S_OK) {
				pSys->Release();
				return true;
			}
			pSys->Release();
		}

		if (D3DXSaveTextureToFileA(file, fmt, pTex, NULL) == S_OK) return true;
	}

	oapiWriteLog("gcCore::SaveSurfaceNative():");
	DumpResource(static_cast<LPDIRECT3DRESOURCE9>(hSrf));
	return false;
}


// ===============================================================================================
//
HSURFNATIVE gcCore::CreateSurfaceNative(int width, int height, DWORD flags)
{
	DWORD Mips = 1;
	DWORD Usage = 0;
	D3DPOOL Pool = D3DPOOL_DEFAULT;

	assert((flags & OAPISURFACE_RENDER3D) == 0);		// Currently not supported but valid otherwise
	assert((flags & OAPISURFACE_UNCOMPRESS) == 0);		// Makes no sense

	if (flags & OAPISURFACE_RENDERTARGET) {
		assert((flags & OAPISURFACE_GDI) == 0);			// Potentially invalid combo
		assert((flags & OAPISURFACE_SYSMEM) == 0);		// Potentially invalid combo
	}

	if (flags & OAPISURFACE_SKETCHPAD) {
		assert((flags & OAPISURFACE_GDI) == 0);			// Potentially invalid combo
		assert((flags & OAPISURFACE_SYSMEM) == 0);		// Potentially invalid combo
	}
	
	if (flags & OAPISURFACE_SKETCHPAD) Usage = D3DUSAGE_RENDERTARGET;
	if (flags & OAPISURFACE_RENDERTARGET) Usage = D3DUSAGE_RENDERTARGET;
	if (flags & OAPISURFACE_GDI) Usage = D3DUSAGE_DYNAMIC;
	if (flags & OAPISURFACE_SYSMEM) Pool = D3DPOOL_SYSTEMMEM;
	if (flags & OAPISURFACE_NOMIPMAPS) Mips = 1;
	if (flags & OAPISURFACE_MIPMAPS) Mips = 0;	// Fullchain

	D3DFORMAT Format = D3DFORMAT(ConvertFormat_OAPI_to_DX(flags));
	
	if (Format == 0) Format = D3DFMT_X8R8G8B8;

	if (flags & OAPISURFACE_TEXTURE) {
		LPDIRECT3DTEXTURE9 pTex = NULL;
		HR(D3DXCreateTexture(g_client->GetDevice(), width, height, Mips, Usage, Format, Pool, &pTex));
		return HSURFNATIVE(pTex);
	}

	if (flags & OAPISURFACE_RENDERTARGET) {
		LPDIRECT3DSURFACE9 pSurf = NULL;
		HR(g_client->GetDevice()->CreateRenderTarget(width, height, Format, D3DMULTISAMPLE_NONE, 0, false, &pSurf, NULL));
		return HSURFNATIVE(pSurf);
	}

	assert(false);
	return NULL;
}


// ===============================================================================================
//
HSURFNATIVE	gcCore::GetMipSublevel(HSURFNATIVE hSrf, int level)
{
	LPDIRECT3DRESOURCE9 pResource = static_cast<LPDIRECT3DRESOURCE9>(hSrf);
	if (pResource->GetType() == D3DRTYPE_TEXTURE) {
		LPDIRECT3DSURFACE9 pSurf = NULL;
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(hSrf);
		if (pTex->GetSurfaceLevel(level, &pSurf) == S_OK) return HSURFNATIVE(pSurf);
	}
	return NULL;
}


// ===============================================================================================
//
bool gcCore::GetSurfaceSpecs(HSURFNATIVE hSrf, SurfaceSpecs *pOut)
{
	LPDIRECT3DRESOURCE9 pResource = static_cast<LPDIRECT3DRESOURCE9>(hSrf);
	D3DSURFACE_DESC desc;

	pOut->Mips = 1;
	pOut->Flags = 0;

	if (pResource->GetType() == D3DRTYPE_SURFACE) {
		LPDIRECT3DSURFACE9 pSurf = static_cast<LPDIRECT3DSURFACE9>(hSrf);
		pSurf->GetDesc(&desc);
	}
	else if (pResource->GetType() == D3DRTYPE_TEXTURE) {
		LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(hSrf);
		pTex->GetLevelDesc(0, &desc);
		pOut->Mips = pTex->GetLevelCount();
		pOut->Flags |= OAPISURFACE_TEXTURE;
	}
	else return false;

	pOut->Width = desc.Width;
	pOut->Height = desc.Height;

	if (pOut->Mips > 1)  pOut->Flags |= OAPISURFACE_MIPMAPS;
	else pOut->Flags |= OAPISURFACE_NOMIPMAPS;

	if (desc.Usage & D3DUSAGE_RENDERTARGET) pOut->Flags |= OAPISURFACE_RENDERTARGET;
	if (desc.Usage & D3DUSAGE_DYNAMIC) pOut->Flags |= OAPISURFACE_GDI;
	if (desc.Pool == D3DPOOL_SYSTEMMEM) pOut->Flags |= OAPISURFACE_SYSMEM;

	pOut->Flags |= ConvertFormat_DX_to_OAPI(desc.Format);
	
	return true;
}


// ===============================================================================================
//
bool gcCore::GenerateMipMaps(HSURFNATIVE hSrf)
{
	LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(hSrf);
	DWORD nMip = pTex->GetLevelCount();
	if (nMip <= 1) return false;

	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	LPDIRECT3DSURFACE9 pHigh = NULL;
	LPDIRECT3DSURFACE9 pLow = NULL;

	HR(pTex->GetSurfaceLevel(0, &pHigh));

	if (pHigh) {
		for (DWORD i = 1; i < nMip; i++) {
			if (pTex->GetSurfaceLevel(i, &pLow) == S_OK) {
				HR(pDev->StretchRect(pHigh, NULL, pLow, NULL, D3DTEXF_LINEAR));
				pHigh->Release();
				pHigh = pLow;
			}
		}
		pHigh->Release();
		return true;
	}
	return false;
}


// ===============================================================================================
//
HSURFNATIVE	gcCore::CompressSurface(HSURFNATIVE hSurface, DWORD flags)
{
	D3DSURFACE_DESC desc;
	LPDIRECT3DSURFACE9 pDest = NULL;
	LPDIRECT3DTEXTURE9 pTex = NULL;
	LPDIRECT3DDEVICE9 pDev = g_client->GetDevice();
	LPDIRECT3DRESOURCE9 pResource = static_cast<LPDIRECT3DRESOURCE9>(hSurface);

	DWORD Mips = 1;
	D3DFORMAT Fmt = D3DFMT_DXT1;
	D3DPOOL Pool = D3DPOOL_DEFAULT;
	if (flags & OAPISURFACE_MIPMAPS) Mips = 0;
	if ((flags & OAPISURFACE_PF_MASK) == OAPISURFACE_PF_DXT3) Fmt = D3DFMT_DXT3;
	if ((flags & OAPISURFACE_PF_MASK) == OAPISURFACE_PF_DXT5) Fmt = D3DFMT_DXT5;
	if (flags & OAPISURFACE_SYSMEM) Pool = D3DPOOL_SYSTEMMEM;
	
	if (pResource->GetType() == D3DRTYPE_SURFACE) {
		LPDIRECT3DSURFACE9 pSurf = static_cast<LPDIRECT3DSURFACE9>(hSurface);
		HR(pSurf->GetDesc(&desc));
		HR(D3DXCreateTexture(pDev, desc.Width, desc.Height, Mips, 0, Fmt, Pool, &pTex));
		for (DWORD i = 0; i < pTex->GetLevelCount(); i++) {
			HR(pTex->GetSurfaceLevel(i, &pDest));
			HR(D3DXLoadSurfaceFromSurface(pDest, NULL, NULL, pSurf, NULL, NULL, D3DX_FILTER_BOX, 0));
			pDest->Release();
		}
		return HSURFNATIVE(pTex);
	}
	else if (pResource->GetType() == D3DRTYPE_TEXTURE) {
		LPDIRECT3DSURFACE9 pSurf = NULL;
		LPDIRECT3DTEXTURE9 pInp = static_cast<LPDIRECT3DTEXTURE9>(hSurface);
		HR(pInp->GetLevelDesc(0, &desc));
		HR(D3DXCreateTexture(pDev, desc.Width, desc.Height, Mips, 0, Fmt, Pool, &pTex));
		HR(pInp->GetSurfaceLevel(0, &pSurf));
		for (DWORD i = 0; i < pTex->GetLevelCount(); i++) {
			HR(pTex->GetSurfaceLevel(i, &pDest));
			HR(D3DXLoadSurfaceFromSurface(pDest, NULL, NULL, pSurf, NULL, NULL, D3DX_FILTER_BOX, 0));
			pDest->Release();
		}
		pSurf->Release();
		return HSURFNATIVE(pTex);
	}

	return NULL;
}

// ===============================================================================================
//
void gcCore::ReleaseSurface(HSURFNATIVE hSrf)
{
	LPDIRECT3DRESOURCE9 pResource = static_cast<LPDIRECT3DRESOURCE9>(hSrf);
	pResource->Release();
}


// ===============================================================================================
//
DEVMESHHANDLE gcCore::GetDevMesh(MESHHANDLE hMesh)
{
	return g_client->GetDevMesh(hMesh);
}


// ===============================================================================================
//
DEVMESHHANDLE gcCore::LoadDevMeshGlobal(const char *file_name, bool bUseCache)
{
	MESHHANDLE hMesh = oapiLoadMeshGlobal(file_name);
	return g_client->GetDevMesh(hMesh);
}


// ===============================================================================================
//
void gcCore::ReleaseDevMesh(DEVMESHHANDLE hMesh)
{
	delete (D3D9Mesh *)(hMesh);
}


// ===============================================================================================
//
void gcCore::RenderMesh(DEVMESHHANDLE hMesh, const oapi::FMATRIX4 *pWorld)
{
	Scene *pScene = g_client->GetScene();
	pScene->RenderMesh(hMesh, pWorld);
}


// ===============================================================================================
//
void gcCore::RenderMesh(DEVMESHHANDLE hMesh, HINSTBUF hInst)
{

}


// ===============================================================================================
//
bool gcCore::PickMesh(gcCore::PickMeshStruct *pm, DEVMESHHANDLE hMesh, const FMATRIX4 *pWorld, short x, short y)
{
	Scene *pScene = g_client->GetScene();
	D3D9Pick pk = pScene->PickMesh(hMesh, (const LPD3DXMATRIX)pWorld, x, y);
	if (pk.group >= 0) {
		if (pk.dist < pm->dist) {
			pm->pos = _FV(pk.pos);
			pm->normal = _FV(pk.normal);
			pm->grp_inst = pk.group;
			pm->dist = pk.dist;
			return true;
		}
	}
	return false;
}


// ===============================================================================================
//
bool gcCore::PickMesh(gcCore::PickMeshStruct *pm, DEVMESHHANDLE hMesh, HINSTBUF hInst)
{
	return false;
}


// ===============================================================================================
//
HINSTBUF gcCore::CreateInstanceBuffer(const oapi::FMATRIX4 *pData, int size, HINSTBUF hBuf)
{
	return NULL;
}


// ===============================================================================================
//
void gcCore::ReleaseInstanceBuffer(HINSTBUF hBuf)
{

}


// ===============================================================================================
//
void gcCore::RenderLines(const FVECTOR3 *pVtx, const WORD *pIdx, int nVtx, int nIdx, const FMATRIX4 *pWorld, DWORD color)
{
	D3D9Effect::RenderLines((const D3DXVECTOR3 *)pVtx, pIdx, nVtx, nIdx, (const D3DXMATRIX *)pWorld, color);
}








// ===============================================================================================
// Some Helper Functions
// ===============================================================================================
//

void gcCore::GetSystemSpecs(SystemSpecs *sp, int size)
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
bool gcCore::RegisterGenericProc(__gcGenericProc proc, DWORD id, void *pParam)
{
	return g_client->RegisterGenericProc(proc, id, pParam);
}

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
HBITMAP	gcCore::LoadBitmapFromFile(const char *fname)
{
	return g_client->gcReadImageFromFile(fname);
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
	return c->dword_abgr();
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

