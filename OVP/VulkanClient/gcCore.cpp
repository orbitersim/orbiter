// ===================================================
// Copyright (C) 2021-2026 Jarmo Nikkanen
// licensed under LGPL v2
// ===================================================


#include "D3DXMath.h" // d3d9.h/d3dx9.h
#include "gcCore.h"
#include "D3D9Surface.h"
#include "D3D9Client.h"
#include "Scene.h"
#include "VVessel.h"
#include "VPlanet.h"
#include "Surfmgr2.h"
#include "IProcess.h"
#include "VkTexFile.h"
#include <QWindow>
#include <QVulkanInstance>
#include <map>

extern D3D9Client *g_client;
extern std::set<Font *> g_fonts;

class gcSwap
{
public:
	gcSwap() : pSwap(VK_NULL_HANDLE), hSurf(NULL), pBack(NULL), iAcquire(0) { for (auto &s : acquireSem) s = VK_NULL_HANDLE; }
	~gcSwap() { Release(); }
	void Release() {
		DELETE_SURFACE(hSurf);
		SAFE_DELETE(pBack);
		VkDev *pDev = g_client->GetDevice(); // not upstream: the images and semaphores IDirect3DSwapChain9 kept inside
		if (pSwap) pDev->WaitIdle();
		for (auto s : pImage) { VkTex *t = s->tex; delete s; delete t; }
		pImage.clear();
		for (auto s : presentSem) vkDestroySemaphore(pDev->dev, s, NULL);
		presentSem.clear();
		for (auto &s : acquireSem) { if (s) vkDestroySemaphore(pDev->dev, s, NULL); s = VK_NULL_HANDLE; }
		if (pSwap) vkDestroySwapchainKHR(pDev->dev, pSwap, NULL); // SAFE_RELEASE(pSwap)
		pSwap = VK_NULL_HANDLE;
	}
	void Init(VkDev *pDev, VkFormat fmt, VkExtent2D ext); // not upstream: wraps the swapchain images, makes the semaphores
	void Present();                                       // not upstream: IDirect3DSwapChain9::Present
	VkSwapchainKHR pSwap;
	VkSurf *pBack;            // offscreen backbuffer, blitted to the window by Present
	SURFHANDLE hSurf;
	std::vector<VkSurf *> pImage;
	std::vector<VkSemaphore> presentSem;
	VkSemaphore acquireSem[VkDev::NFRAMES];
	int iAcquire;
};


// not upstream: IDirect3DDevice9::CreateAdditionalSwapChain's D3DPRESENT_PARAMETERS for a window (the old swapchain is retired)
static VkSwapchainKHR CreateAdditionalSwapChain(VkDev *pDev, QWindow *hWnd, VkSwapchainKHR old, VkFormat *pFmt, VkExtent2D *pExt)
{
	if (!hWnd->vulkanInstance()) hWnd->setVulkanInstance(pDev->qinst); // Qt makes the surface with the window's instance
	VkSurfaceKHR surface = QVulkanInstance::surfaceForWindow(hWnd);   // owned by the window
	if (!surface) return VK_NULL_HANDLE;
	VkBool32 present = VK_FALSE;
	vkGetPhysicalDeviceSurfaceSupportKHR(pDev->phys, pDev->queueFamily, surface, &present);
	if (!present) return VK_NULL_HANDLE;

	VkSurfaceCapabilitiesKHR sc;
	VKCHECK(vkGetPhysicalDeviceSurfaceCapabilitiesKHR(pDev->phys, surface, &sc));

	UINT n = 0;
	vkGetPhysicalDeviceSurfaceFormatsKHR(pDev->phys, surface, &n, NULL);
	std::vector<VkSurfaceFormatKHR> sf(n);
	vkGetPhysicalDeviceSurfaceFormatsKHR(pDev->phys, surface, &n, sf.data());
	if (!n) return VK_NULL_HANDLE;
	VkSurfaceFormatKHR fmt = sf[0];
	for (auto &f : sf) if (f.format == VK_FORMAT_B8G8R8A8_UNORM && f.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) { fmt = f; break; } // D3DFMT_X8R8G8B8

	vkGetPhysicalDeviceSurfacePresentModesKHR(pDev->phys, surface, &n, NULL);
	std::vector<VkPresentModeKHR> pm(n);
	vkGetPhysicalDeviceSurfacePresentModesKHR(pDev->phys, surface, &n, pm.data());
	VkPresentModeKHR mode = VK_PRESENT_MODE_FIFO_KHR;
	for (auto m : pm) if (m == VK_PRESENT_MODE_IMMEDIATE_KHR) mode = m; // D3DPRESENT_INTERVAL_IMMEDIATE

	VkExtent2D ext = sc.currentExtent; // BackBufferWidth/Height 0: the window's client size
	if (ext.width == 0xFFFFFFFF) {
		qreal dpr = hWnd->devicePixelRatio();
		ext.width = std::clamp(UINT(hWnd->width() * dpr), sc.minImageExtent.width, sc.maxImageExtent.width);
		ext.height = std::clamp(UINT(hWnd->height() * dpr), sc.minImageExtent.height, sc.maxImageExtent.height);
	}
	if (ext.width == 0 || ext.height == 0) return VK_NULL_HANDLE;

	UINT count = std::max(2u, sc.minImageCount); // BackBufferCount 1 + the front buffer
	if (sc.maxImageCount && count > sc.maxImageCount) count = sc.maxImageCount;

	VkSwapchainCreateInfoKHR ci = { VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR };
	ci.surface = surface;
	ci.minImageCount = count;
	ci.imageFormat = fmt.format;
	ci.imageColorSpace = fmt.colorSpace;
	ci.imageExtent = ext;
	ci.imageArrayLayers = 1;
	ci.imageUsage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
	ci.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
	ci.preTransform = sc.currentTransform;
	ci.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
	if (!(sc.supportedCompositeAlpha & ci.compositeAlpha)) ci.compositeAlpha = (VkCompositeAlphaFlagBitsKHR)(sc.supportedCompositeAlpha & -sc.supportedCompositeAlpha);
	ci.presentMode = mode;
	ci.clipped = VK_TRUE;
	ci.oldSwapchain = old;

	VkSwapchainKHR pSwap = VK_NULL_HANDLE;
	if (vkCreateSwapchainKHR(pDev->dev, &ci, NULL, &pSwap) != VK_SUCCESS) return VK_NULL_HANDLE;
	*pFmt = fmt.format;
	*pExt = ext;
	return pSwap;
}


void gcSwap::Init(VkDev *pDev, VkFormat fmt, VkExtent2D ext)
{
	UINT n = 0;
	vkGetSwapchainImagesKHR(pDev->dev, pSwap, &n, NULL);
	std::vector<VkImage> img(n);
	vkGetSwapchainImagesKHR(pDev->dev, pSwap, &n, img.data());
	VkSemaphoreCreateInfo si = { VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO };
	for (auto i : img) {
		pImage.push_back(new VkSurf(new VkTex(pDev, i, fmt, ext.width, ext.height)));
		VkSemaphore s;
		VKCHECK(vkCreateSemaphore(pDev->dev, &si, NULL, &s));
		presentSem.push_back(s);
	}
	for (auto &s : acquireSem) VKCHECK(vkCreateSemaphore(pDev->dev, &si, NULL, &s));
}


void gcSwap::Present()
{
	VkDev *pDev = g_client->GetDevice();
	if (!pSwap || !pDev->IsRecording()) return;
	UINT idx = 0;
	VkSemaphore acq = acquireSem[iAcquire];
	VkResult r = vkAcquireNextImageKHR(pDev->dev, pSwap, UINT64_MAX, acq, VK_NULL_HANDLE, &idx);
	if (r < 0) { LogErr("gcSwap: vkAcquireNextImageKHR() Failed (%d)", (int)r); return; } // out of date: the owner registers the window again
	iAcquire = (iAcquire + 1) % VkDev::NFRAMES;

	pDev->StretchRect(pBack, NULL, pImage[idx], NULL, VK_FILTER_LINEAR);
	pImage[idx]->tex->Transition(pDev->Cmd(), VK_IMAGE_LAYOUT_PRESENT_SRC_KHR);
	pDev->EndFrame(acq, presentSem[idx]); // the frame so far is submitted, as D3D9 flushed it at Present

	VkPresentInfoKHR pi = { VK_STRUCTURE_TYPE_PRESENT_INFO_KHR };
	pi.waitSemaphoreCount = 1;
	pi.pWaitSemaphores = &presentSem[idx];
	pi.swapchainCount = 1;
	pi.pSwapchains = &pSwap;
	pi.pImageIndices = &idx;
	r = pDev->QueuePresent(&pi);
	if (r < 0) LogErr("gcSwap: vkQueuePresentKHR() Failed (%d)", (int)r);
	pDev->BeginFrame();
}



DLLCLBK void gcBindCoreMethod(void** ppFnc, const char* name)
{
	*ppFnc = NULL; // (void*) casts below: g++ doesn't turn function pointers into void* implicitly
#define binder_start
	if (strcmp(name,"RegisterSwap")==0) *ppFnc = (void*)&gcCore2::RegisterSwap;
	if (strcmp(name,"FlipSwap")==0) *ppFnc = (void*)&gcCore2::FlipSwap;
	if (strcmp(name,"GetRenderTarget")==0) *ppFnc = (void*)&gcCore2::GetRenderTarget;
	if (strcmp(name,"ReleaseSwap")==0) *ppFnc = (void*)&gcCore2::ReleaseSwap;
	if (strcmp(name,"DeleteCustomCamera")==0) *ppFnc = (void*)&gcCore2::DeleteCustomCamera;
	if (strcmp(name,"CustomCameraOnOff")==0) *ppFnc = (void*)&gcCore2::CustomCameraOnOff;
	if (strcmp(name,"CustomCameraOverlay")==0) *ppFnc = (void*)&gcCore2::CustomCameraOverlay;
	if (strcmp(name,"SetCustomCameraSurfaceLabelScale")==0) *ppFnc = (void*)&gcCore2::SetCustomCameraSurfaceLabelScale;
	if (strcmp(name,"SetupCustomCamera")==0) *ppFnc = (void*)&gcCore2::SetupCustomCamera;
	if (strcmp(name,"SketchpadVersion")==0) *ppFnc = (void*)&gcCore2::SketchpadVersion;
	if (strcmp(name,"CreatePoly")==0) *ppFnc = (void*)&gcCore2::CreatePoly;
	if (strcmp(name,"CreateTriangles")==0) *ppFnc = (void*)&gcCore2::CreateTriangles;
	if (strcmp(name,"DeletePoly")==0) *ppFnc = (void*)&gcCore2::DeletePoly;
	if (strcmp(name,"GetTextLength")==0) *ppFnc = (void*)&gcCore2::GetTextLength;
	if (strcmp(name,"GetCharIndexByPosition")==0) *ppFnc = (void*)&gcCore2::GetCharIndexByPosition;
	if (strcmp(name,"RegisterRenderProc")==0) *ppFnc = (void*)&gcCore2::RegisterRenderProc;
	if (strcmp(name,"CreateSketchpadFont")==0) *ppFnc = (void*)&gcCore2::CreateSketchpadFont;
	if (strcmp(name,"GetMeshMaterial")==0) *ppFnc = (void*)&gcCore2::GetMeshMaterial;
	if (strcmp(name,"SetMeshMaterial")==0) *ppFnc = (void*)&gcCore2::SetMeshMaterial;
	if (strcmp(name,"GetMatrix")==0) *ppFnc = (void*)&gcCore2::GetMatrix;
	if (strcmp(name,"SetMatrix")==0) *ppFnc = (void*)&gcCore2::SetMatrix;
	if (strcmp(name,"GetDevMesh")==0) *ppFnc = (void*)&gcCore2::GetDevMesh;
	if (strcmp(name,"LoadDevMeshGlobal")==0) *ppFnc = (void*)&gcCore2::LoadDevMeshGlobal;
	if (strcmp(name,"ReleaseDevMesh")==0) *ppFnc = (void*)&gcCore2::ReleaseDevMesh;
	if (strcmp(name,"RenderMesh")==0) *ppFnc = (void*)&gcCore2::RenderMesh;
	if (strcmp(name,"PickMesh")==0) *ppFnc = (void*)&gcCore2::PickMesh;
	if (strcmp(name,"RenderLines")==0) *ppFnc = (void*)&gcCore2::RenderLines;
	if (strcmp(name,"GetSystemSpecs")==0) *ppFnc = (void*)&gcCore2::GetSystemSpecs;
	if (strcmp(name,"GetSurfaceSpecs")==0) *ppFnc = (void*)&gcCore2::GetSurfaceSpecs;
	if (strcmp(name,"LoadSurface")==0) *ppFnc = (void*)&gcCore2::LoadSurface;
	if (strcmp(name,"SaveSurface")==0) *ppFnc = (void*)&gcCore2::SaveSurface;
	if (strcmp(name,"GetMipSublevel")==0) *ppFnc = (void*)&gcCore2::GetMipSublevel;
	if (strcmp(name,"GenerateMipmaps")==0) *ppFnc = (void*)&gcCore2::GenerateMipmaps;
	if (strcmp(name,"CompressSurface")==0) *ppFnc = (void*)&gcCore2::CompressSurface;
	if (strcmp(name,"LoadBitmapFromFile")==0) *ppFnc = (void*)&gcCore2::LoadBitmapFromFile;
	if (strcmp(name,"GetRenderWindow")==0) *ppFnc = (void*)&gcCore2::GetRenderWindow;
	if (strcmp(name,"RegisterGenericProc")==0) *ppFnc = (void*)&gcCore2::RegisterGenericProc;
	if (strcmp(name,"StretchRectInScene")==0) *ppFnc = (void*)&gcCore2::StretchRectInScene;
	if (strcmp(name,"ClearSurfaceInScene")==0) *ppFnc = (void*)&gcCore2::ClearSurfaceInScene;
	if (strcmp(name,"ScanScreen")==0) *ppFnc = (void*)&gcCore2::ScanScreen;
	if (strcmp(name,"LockSurface")==0) *ppFnc = (void*)&gcCore2::LockSurface;
	if (strcmp(name,"ReleaseLock")==0) *ppFnc = (void*)&gcCore2::ReleaseLock;
	if (strcmp(name,"GetPlanetManager")==0) *ppFnc = (void*)&gcCore2::GetPlanetManager;
	if (strcmp(name,"SetTileOverlay")==0) *ppFnc = (void*)&gcCore2::SetTileOverlay;
	if (strcmp(name,"AddGlobalOverlay")==0) *ppFnc = (void*)&gcCore2::AddGlobalOverlay;
	if (strcmp(name,"GetTileData")==0) *ppFnc = (void*)&gcCore2::GetTileData;
	if (strcmp(name,"GetTile")==0) *ppFnc = (void*)&gcCore2::GetTile;
	if (strcmp(name,"HasTileData")==0) *ppFnc = (void*)&gcCore2::HasTileData;
	if (strcmp(name,"SeekTileTexture")==0) *ppFnc = (void*)&gcCore2::SeekTileTexture;
	if (strcmp(name,"SeekTileElevation")==0) *ppFnc = (void*)&gcCore2::SeekTileElevation;
	if (strcmp(name,"GetElevation")==0) *ppFnc = (void*)&gcCore2::GetElevation;
	if (strcmp(name,"CreateIPInterface")==0) *ppFnc = (void*)&gcCore2::CreateIPInterface;
	if (strcmp(name,"ReleaseIPInterface")==0) *ppFnc = (void*)&gcCore2::ReleaseIPInterface;
#define binder_end
	if (*ppFnc == NULL) oapiWriteLogV("ERROR:gcCoreAPI: Function [%s] failed to bind", name);
}


// ===============================================================================================
// Custom SwapChain Interface
// ===============================================================================================
//
HSWAP gcCore::RegisterSwap(QWindow *hWnd, HSWAP hData, int AA) 
{ 
	gcSwap * pData = (gcSwap*)hData;

	// D3DPRESENT_PARAMETERS (window size, X8R8G8B8, one backbuffer, no multisample, no depth, immediate): CreateAdditionalSwapChain above

	VkDev *pDev = g_client->GetDevice();
	VkFormat fmt = VK_FORMAT_UNDEFINED;
	VkExtent2D ext = {};
	VkSwapchainKHR pSwap = CreateAdditionalSwapChain(pDev, hWnd, pData ? pData->pSwap : VK_NULL_HANDLE, &fmt, &ext);

	if (pSwap)
	{
		if (!pData) pData = new gcSwap();
		else pData->Release();
		
		VkSurf *pBack = new VkSurf(pDev, ext.width, ext.height, VK_FORMAT_B8G8R8A8_UNORM, VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT); // GetBackBuffer

		SurfNative *pSrf = new SurfNative(pBack, OAPISURFACE_BACKBUFFER | OAPISURFACE_RENDER3D | OAPISURFACE_RENDERTARGET);
		pSrf->SetName("SwapChainBackBuffer");

		pData->hSurf = SURFHANDLE(pSrf);
		pData->pSwap = pSwap;
		pData->pBack = pBack;
		pData->Init(pDev, fmt, ext);

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
	((gcSwap*)hSwap)->Present();
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
void gcCore::CustomCameraOverlay(CAMERAHANDLE hCam, __gcRenderProc clbk, void *pUser)
{
	CAMERA(hCam)->pRenderProc = clbk;
	CAMERA(hCam)->pUser = pUser;
}


// ===============================================================================================
//
int gcCore::DeleteCustomCamera(CAMERAHANDLE hCam)
{
	Scene *pScene = g_client->GetScene();
	return pScene ? pScene->DeleteCustomCamera(hCam) : 0;
}

// ===============================================================================================
//
void gcCore::SetCustomCameraSurfaceLabelScale(CAMERAHANDLE hCam, float scale)
{
	Scene* pScene = g_client->GetScene();

	if(pScene) {
		pScene->SetCustomCameraSurfaceLabelScale(hCam, scale);
	}
}






// ===============================================================================================
// SketchPad Interface
// ===============================================================================================


// ===============================================================================================
//
int gcCore::SketchpadVersion(Sketchpad* pSkp)
{
	return ((D3D9Pad*)pSkp)->GetVersion();
}


// ===============================================================================================
//
oapi::Font* gcCore::CreateSketchpadFont(int height, char* face, int width, int weight, FontStyle Style, float spacing)
{
	return g_client->clbkCreateFontEx(height, face, width, weight, Style, spacing);
}


// ===============================================================================================
//
HPOLY gcCore::CreatePoly(HPOLY hPoly, const FVECTOR2 *pt, int npt, DWORD flags)
{
	VkDev *pDev = g_client->GetDevice();
	if (!hPoly) return new D3D9PolyLine(pDev, pt, npt, (flags&PF_CONNECT) != 0);
	((D3D9PolyLine *)hPoly)->Update(pt, npt, (flags&PF_CONNECT) != 0);
	return hPoly;
}


// ===============================================================================================
//
HPOLY gcCore::CreateTriangles(HPOLY hPoly, const gcCore::clrVtx *pt, int npt, DWORD flags)
{
	VkDev *pDev = g_client->GetDevice();
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
int gcCore::GetMatrix(MatrixId matrix_id, OBJHANDLE hVessel, DWORD mesh, DWORD group, FMATRIX4 *pMat)
{
	if (oapiGetObjectType(hVessel) != OBJTP_VESSEL) return -10;
	Scene *pScn = g_client->GetScene();
	vVessel *pVes = (vVessel *)pScn->GetVisObject(hVessel);
	if (pVes) return pVes->GetMatrixTransform(matrix_id, mesh, group, pMat);
	return -11;
}


// ===============================================================================================
//
int gcCore::SetMatrix(MatrixId matrix_id, OBJHANDLE hVessel, DWORD mesh, DWORD group, const FMATRIX4 *pMat)
{
	if (oapiGetObjectType(hVessel) != OBJTP_VESSEL) return -10;
	Scene *pScn = g_client->GetScene();
	vVessel *pVes = (vVessel *)pScn->GetVisObject(hVessel);
	if (pVes) return pVes->SetMatrixTransform(matrix_id, mesh, group, pMat);
	return -11;
}


// ===============================================================================================
//
int gcCore::GetMeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, MatProp prop, FVECTOR4* value)
{
	return g_client->clbkMeshMaterialEx(hMesh, idx, prop, value);
}


// ===============================================================================================
//
int gcCore::SetMeshMaterial(DEVMESHHANDLE hMesh, DWORD idx, MatProp prop, const FVECTOR4* value)
{
	return g_client->clbkSetMeshMaterialEx(hMesh, idx, prop, value);
}

// ===============================================================================================
//
DEVMESHHANDLE gcCore::GetDevMesh(MESHHANDLE hMesh)
{
	return g_client->GetDevMesh(hMesh);
}


// ===============================================================================================
//
DEVMESHHANDLE gcCore::LoadDevMeshGlobal(const char* file_name, bool bUseCache)
{
	MESHHANDLE hMesh = oapiLoadMeshGlobal(file_name);
	return g_client->GetDevMesh(hMesh);
}


// ===============================================================================================
//
void gcCore::ReleaseDevMesh(DEVMESHHANDLE hMesh)
{
	delete (D3D9Mesh*)(hMesh);
}


// ===============================================================================================
//
void gcCore::RenderMesh(DEVMESHHANDLE hMesh, const oapi::FMATRIX4* pWorld)
{
	Scene* pScene = g_client->GetScene();
	pScene->RenderMesh(hMesh, pWorld);
}


// ===============================================================================================
//
bool gcCore::PickMesh(PickMeshStruct* pm, DEVMESHHANDLE hMesh, const FMATRIX4* pWorld, short x, short y)
{
	Scene* pScene = g_client->GetScene();
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
// Custom Render Interface
// ===============================================================================================
//
// ===============================================================================================
//
SURFHANDLE gcCore::LoadSurface(const char* fname, DWORD flags)
{
	return g_client->clbkLoadSurface(fname, flags);
}

// ===============================================================================================
//
bool gcCore::SaveSurface(const char* file, SURFHANDLE hSrf)
{
	return NatSaveSurface(file, SURFACE(hSrf)->GetResource());
}

// ===============================================================================================
//
SURFHANDLE gcCore::GetMipSublevel(SURFHANDLE hSrf, int level)
{
	return NatGetMipSublevel(hSrf, level);
}

// ===============================================================================================
//
bool gcCore::GenerateMipmaps(SURFHANDLE hSurface)
{
	return NatGenerateMipmaps(hSurface);
}

// ===============================================================================================
//
SURFHANDLE gcCore::CompressSurface(SURFHANDLE hSurface, DWORD flags)
{
	return NatCompressSurface(hSurface, flags);
}


// ===============================================================================================
//
void gcCore::RenderLines(const FVECTOR3* pVtx, const WORD* pIdx, int nVtx, int nIdx, const FMATRIX4* pWorld, DWORD color)
{
	D3D9Effect::RenderLines((const D3DXVECTOR3*)pVtx, pIdx, nVtx, nIdx, (const D3DXMATRIX*)pWorld, color);
}


// ===============================================================================================
//
bool gcCore::StretchRectInScene(SURFHANDLE tgt, SURFHANDLE src, LPRECT tr, LPRECT sr)
{
	if (0 == g_client->BeginScene()) // S_OK
	{
		VkSurf *pss = SURFACE(src)->GetSurface();
		VkSurf *pts = SURFACE(tgt)->GetSurface();
		g_client->GetDevice()->StretchRect(pss, sr, pts, tr, VK_FILTER_LINEAR);
		g_client->EndScene();
		return (pss && pts); // hr == S_OK: StretchRect has no result, it skips missing surfaces
	}
	return false;
}

// ===============================================================================================
//
bool gcCore::ClearSurfaceInScene(SURFHANDLE tgt, DWORD color, LPRECT tr)
{
	if (0 == g_client->BeginScene()) // S_OK
	{
		VkSurf *pts = SURFACE(tgt)->GetSurface();
		g_client->GetDevice()->ColorFill(pts, tr, (D3DCOLOR)color);
		g_client->EndScene();
		return (pts != NULL); // hr == S_OK: ColorFill has no result
	}
	return false;
}





// ===============================================================================================
// Some Helper Functions
// ===============================================================================================
//
// ===============================================================================================
//
gcCore::PickGround gcCore::ScanScreen(int scr_x, int scr_y)
{
	PickGround pg; memset(&pg, 0, sizeof(PickGround));

	Scene* pScene = g_client->GetScene();
	TILEPICK tp = pScene->PickSurface(scr_x, scr_y);
	SurfTile* pTile = static_cast<SurfTile*>(tp.pTile);

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
void gcCore::GetSystemSpecs(SystemSpecs* sp, int size)
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
bool gcCore::GetSurfaceSpecs(SURFHANDLE hSrf, SurfaceSpecs* sp, int size)
{
	return SURFACE(hSrf)->GetSpecs(sp, size);
}


// ===============================================================================================
//
bool gcCore::RegisterGenericProc(__gcGenericProc proc, DWORD id, void* pParam)
{
	return g_client->RegisterGenericProc(proc, id, pParam);
}


// ===============================================================================================
//
QImage *	gcCore::LoadBitmapFromFile(const char* fname)
{
	return g_client->gcReadImageFromFile(fname);
}


// ===============================================================================================
//
QWindow *gcCore::GetRenderWindow()
{
	return g_client->GetRenderWindow();
}


// ===============================================================================================
// gcCore2 Interface --- Tile access interface functions
// ===============================================================================================
//

HPLANETMGR gcCore2::GetPlanetManager(OBJHANDLE hPlanet)
{
	Scene *pScene = g_client->GetScene();
	vPlanet *vPl = (vPlanet *)pScene->GetVisObject(hPlanet);
	return HPLANETMGR(vPl);
}


// ===============================================================================================
//
HTILE gcCore2::GetTile(HPLANETMGR vPl, double lng, double lat, int maxlevel)
{
	vPlanet *vP = static_cast<vPlanet *>(vPl);
	return HTILE(vP->FindTile(lng, lat, maxlevel));
}


// ===============================================================================================
//
gcCore::PickGround gcCore2::GetTileData(HPLANETMGR vPl, double lng, double lat, int maxlevel)
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
	MATRIX3 mRot; oapiGetRotationMatrix(vP->Object(), &mRot);

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
bool gcCore2::SeekTileElevation(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags, ElevInfo *pInfo)
{
	ELEVFILEHEADER hdr;
	if (!hMgr) return false;
	if (((vPlanet*)(hMgr))->SurfMgr2()) {
		float* pData = ((vPlanet*)(hMgr))->SurfMgr2()->BrowseElevationData(level, iLat, iLng, flags, &hdr);
		if (!pData) return false;
		pInfo->MaxElev = hdr.emax;
		pInfo->MinElev = hdr.emin;
		pInfo->MeanElev = hdr.emean;
		pInfo->Resolution = hdr.scale;
		pInfo->Offset = hdr.offset;
		pInfo->pElevData = pData;
		return true;
	}
	return false;
}


// ===============================================================================================
//
SURFHANDLE gcCore2::SeekTileTexture(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags, void *reserved)
{
	if (!hMgr) return NULL;
	if (((vPlanet *)(hMgr))->SurfMgr2()) {
		return ((vPlanet *)(hMgr))->SurfMgr2()->SeekTileTexture(iLng, iLat, level, flags);
	}
	return NULL;
}


// ===============================================================================================
//
bool gcCore2::HasTileData(HPLANETMGR hMgr, int iLng, int iLat, int level, int flags)
{
	if (!hMgr) return false;
	if (((vPlanet *)(hMgr))->SurfMgr2()) {
		return ((vPlanet *)(hMgr))->SurfMgr2()->HasTileData(iLng, iLat, level, flags);
	}
	return false;
}


// ===============================================================================================
//
int gcCore2::GetElevation(HTILE hTile, double lng, double lat, double *out_elev)
{
	SurfTile *pTile = static_cast<SurfTile *>(hTile);
	return pTile->GetElevation(lng, lat, out_elev, NULL, NULL, true, true);
}


// ===============================================================================================
//
SURFHANDLE gcCore2::SetTileOverlay(HTILE hTile, const SURFHANDLE hOverlay)
{
	//SurfTile *pTile = static_cast<SurfTile *>(hTile);
	//LPDIRECT3DTEXTURE9 pTex = static_cast<LPDIRECT3DTEXTURE9>(hOverlay);
	//return HSURFNATIVE(pTile->SetOverlay(pTex, true));
	return NULL;
}


// ===============================================================================================
//
HOVERLAY gcCore2::AddGlobalOverlay(HPLANETMGR hMgr, VECTOR4 mmll, OlayType type, const SURFHANDLE hOverlay, HOVERLAY hOld, const FVECTOR4* pBlend)
{
	if (!hMgr) return NULL;
	vPlanet *vP = static_cast<vPlanet *>(hMgr);
	vPlanet::sOverlay* oLay = static_cast<vPlanet::sOverlay*>(hOld);
	if (hOverlay) {
		VkTex *pTex = SURFACE(hOverlay)->GetTexture();
		return vP->AddOverlaySurface(mmll, type, pTex, oLay, pBlend);
	}
	return vP->AddOverlaySurface(mmll, type, NULL, oLay, pBlend);
}

// ===============================================================================================
//
static std::map<SURFHANDLE, VkPixels> g_locks; // not upstream: the CPU copies LockSurface hands out (LockRect's memory)

bool gcCore::LockSurface(SURFHANDLE hSrf, Lock* pOut, bool bWait)
{
	VkPixels &lock = g_locks[hSrf]; // D3DLOCKED_RECT: read back now, written back by ReleaseLock
	VkDev *pDev = g_client->GetDevice();
	// D3DLOCK_DONOTWAIT (bWait false) left out: the readback always waits for the GPU

	if (SURFACE(hSrf)->GetType() == NATTYPE_SURFACE) { // D3DRTYPE_SURFACE
		VkSurf *pSurf = SURFACE(hSrf)->GetSurface(); // (upstream cast hSrf itself to the surface)
		if (pSurf && VkReadPixels(pDev, pSurf->tex, lock, pSurf->level + 1)) {
			pOut->pData = lock.Level(pSurf->level, pSurf->layer).data();
			pOut->Pitch = DWORD(VkLevelSize(lock.fmt, pSurf->w, 1));
			return true;
		}
		g_locks.erase(hSrf);
		return false;
	}

	if (SURFACE(hSrf)->GetType() == NATTYPE_TEXTURE) { // D3DRTYPE_TEXTURE
		VkTex *pTex = SURFACE(hSrf)->GetResource(); // (upstream cast hSrf itself to the texture)
		if (pTex && VkReadPixels(pDev, pTex, lock, 1)) {
			pOut->pData = lock.Level(0).data();
			pOut->Pitch = DWORD(VkLevelSize(lock.fmt, pTex->w, 1));
			return true;
		}
		g_locks.erase(hSrf);
		return false;
	}

	g_locks.erase(hSrf);
	return false;
}


// ===============================================================================================
//
void gcCore::ReleaseLock(SURFHANDLE hSrf)
{
	auto it = g_locks.find(hSrf);
	if (it == g_locks.end()) return; // not upstream: nothing is locked
	VkPixels &lock = it->second;
	if (SURFACE(hSrf)->GetType() == NATTYPE_SURFACE) { // D3DRTYPE_SURFACE
		VkSurf *pSurf = SURFACE(hSrf)->GetSurface();
		std::vector<BYTE> &d = lock.Level(pSurf->level, pSurf->layer);
		pSurf->tex->Upload(pSurf->level, pSurf->layer, d.data(), d.size()); // UnlockRect
	}
	if (SURFACE(hSrf)->GetType() == NATTYPE_TEXTURE) { // D3DRTYPE_TEXTURE
		VkTex *pTex = SURFACE(hSrf)->GetResource();
		std::vector<BYTE> &d = lock.Level(0);
		pTex->Upload(0, 0, d.data(), d.size()); // UnlockRect(0)
	}
	g_locks.erase(it);
}

// ===============================================================================================
//
gcIPInterface* gcCore2::CreateIPInterface(const char* file, const char* PSEntry, const char* VSEntry, const char* ppf)
{
	ImageProcessing* pIPI = new ImageProcessing(g_client->GetDevice(), file, PSEntry, ppf);

	if (pIPI->IsOK() == false) {
		oapiWriteLogV("gcCore::CreateIPInterface() Failed !  File = [%s]", file);
		return NULL;
	}
	return new gcIPInterface(pIPI);
}

// ===============================================================================================
//
void gcCore2::ReleaseIPInterface(gcIPInterface* pIPI)
{
	if (!pIPI) return;
	if (pIPI->pIPI) delete pIPI->pIPI;
	delete pIPI;
}


