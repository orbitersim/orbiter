// ==============================================================
// File: D3D9frame.cpp
// Desc: Class functions to implement a Direct3D app framework.
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007-2026 Martin Schweiger
//				 2011 - 2016 Jarmo Nikkanen
// ==============================================================

// STRICT, _CRT_SECURE_NO_DEPRECATE and windows.h left out: Win32 build switches and headers

#include "GraphicsAPI.h"
#include "D3D9Frame.h"
#include "D3D9Util.h"
#include "AABBUtil.h"
#include "D3D9Surface.h"
#include "Log.h"
#include "D3D9Config.h"
#include "OapiExtension.h"
#include <QVulkanInstance>
#include <QWindow>
#include <QScreen>
#include <QGuiApplication>
#include <QMessageBox>

using namespace oapi;

VkVertexDecl	*pMeshVertexDecl = NULL;
VkVertexDecl	*pHazeVertexDecl = NULL;
VkVertexDecl	*pNTVertexDecl = NULL;
VkVertexDecl	*pBAVertexDecl = NULL;
VkVertexDecl	*pPosColorDecl = NULL;
VkVertexDecl	*pPositionDecl = NULL;
VkVertexDecl	*pVector4Decl  = NULL;
VkVertexDecl	*pPosTexDecl   = NULL;
VkVertexDecl	*pPatchVertexDecl = NULL;
VkVertexDecl	*pSketchpadDecl = NULL;
VkVertexDecl	*pLocalLightsDecl = NULL;

static const char *d3dmessage={"Required Vulkan version (1.4 or newer) not found\0"};

// not upstream: format support queries (CheckDeviceFormat, CheckDepthStencilMatch counterparts)
static bool FormatHas (VkPhysicalDevice p, VkFormat f, VkFormatFeatureFlags ff)
{
	VkFormatProperties fp;
	vkGetPhysicalDeviceFormatProperties (p, f, &fp);
	return (fp.optimalTilingFeatures & ff) == ff;
}

static bool VertexFormatHas (VkPhysicalDevice p, VkFormat f)
{
	VkFormatProperties fp;
	vkGetPhysicalDeviceFormatProperties (p, f, &fp);
	return (fp.bufferFeatures & VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT) != 0;
}

// not upstream: the adapters in vkEnumeratePhysicalDevices order (the Launchpad video tab lists them the same way)
static VkPhysicalDevice PickAdapter (DWORD idx)
{
	UINT n = 0;
	vkEnumeratePhysicalDevices (g_pD3DObject->vkInstance(), &n, NULL);
	if (n == 0) return VK_NULL_HANDLE;
	std::vector<VkPhysicalDevice> pd(n);
	vkEnumeratePhysicalDevices (g_pD3DObject->vkInstance(), &n, pd.data());
	return pd[idx < n ? idx : 0];
}

//-----------------------------------------------------------------------------
// Name: CD3DFramework9()
// Desc: The constructor. Clears static variables
//-----------------------------------------------------------------------------

CD3DFramework9::CD3DFramework9()
{
	Clear();

	if (g_pD3DObject == NULL) {
		LogErr("ERROR: [Vulkan Instance Creation Failed]");
		LogErr(d3dmessage);
		QMessageBox::critical(NULL, "VulkanClient Initialization Failed", d3dmessage);
	}
}

//-----------------------------------------------------------------------------
// Name: ~CD3DFramework9()
// Desc: The destructor. Deletes all objects
//-----------------------------------------------------------------------------
CD3DFramework9::~CD3DFramework9 ()
{
	LogAlw("Deleting Framework");
}

void CD3DFramework9::Clear()
{
	hWnd			  = NULL;
	bIsFullscreen	  = false;
	bVertexTexture    = false;
	bAAEnabled		  = false;
	bNoVSync		  = false;
	Alpha			  = false;
	SWVert			  = false;
	Pure			  = true;
	DDM				  = false;
	nvPerfHud		  = false;
	dwRenderWidth	  = 0;
	dwRenderHeight	  = 0;
	dwFSMode		  = 0;
	pDevice			  = NULL;
	dwZBufferBitDepth = 0;
	dwStencilBitDepth = 0;
	Adapter			  = 0;
	Mode			  = 0;
	MultiSample		  = 0;
	pRenderTarget	  = NULL;
	pBackBuffer		  = NULL;
	pDepthStencil	  = NULL;
	pResolve		  = NULL;
	surface			  = VK_NULL_HANDLE;
	swapchain		  = VK_NULL_HANDLE;
	swapFormat		  = VK_FORMAT_UNDEFINED;
	swapExtent		  = { 0, 0 };
	iAcquire		  = 0;
	for (int i = 0; i < VkDev::NFRAMES; i++) acquireSem[i] = VK_NULL_HANDLE;
	swapImages.clear();
	presentSem.clear();

	memset((void *)&rcScreenRect, 0, sizeof(RECT));
	// d3dPP (D3DPRESENT_PARAMETERS) left out: the swapchain members above
	memset((void *)&caps, 0, sizeof(VkDevCaps));
}

//-----------------------------------------------------------------------------
// Name: DestroyObjects()
// Desc: Objects created in Initialize() section are destroyed in here
//-----------------------------------------------------------------------------
int CD3DFramework9::DestroyObjects ()
{
	_TRACE;
	LogAlw("========== Destroying framework objects ==========");

	if (pDevice) pDevice->WaitIdle();

	SAFE_DELETE(pRenderTarget);
	SAFE_DELETE(pDepthStencil);
	SAFE_DELETE(pResolve);
	SAFE_DELETE(pNTVertexDecl);
	SAFE_DELETE(pBAVertexDecl);
	SAFE_DELETE(pPosColorDecl);
	SAFE_DELETE(pPositionDecl);
	SAFE_DELETE(pVector4Decl);
	SAFE_DELETE(pPosTexDecl);
	SAFE_DELETE(pHazeVertexDecl);
	SAFE_DELETE(pMeshVertexDecl);
	SAFE_DELETE(pPatchVertexDecl);
	SAFE_DELETE(pSketchpadDecl);
	SAFE_DELETE(pLocalLightsDecl);

	// EvictManagedResources, Sleep and Reset left out: Vulkan has no managed pool or lost devices to reset
	DestroySwapchain();

	SAFE_DELETE(pDevice);
	LogAlw("[Vulkan Device Destroyed]");

	return 0;
}

//-----------------------------------------------------------------------------
// Name: Initialize()
// Desc: Creates the internal objects for the framework
//-----------------------------------------------------------------------------
int CD3DFramework9::Initialize(QWindow *_hWnd, GraphicsClient::VIDEODATA *vData)
{
	_TRACE;

	Clear();

	DDM = (Config->DisableDriverManagement != 0);
	nvPerfHud = (Config->NVPerfHUD != 0);

	bool bFail = false;

	if (_hWnd==NULL || vData==NULL || g_pD3DObject==NULL) {
		LogErr("ERROR: Invalid input parameter in CD3DFramework9::Initialize()");
		return -1;
	}

	// Setup state for windowed/fullscreen mode
	//
	hWnd          = _hWnd;
	bAAEnabled    = (Config->SceneAntialias != 0);
	bIsFullscreen = vData->fullscreen;
	bNoVSync      = vData->novsync;
	dwFSMode	  = vData->style;
	Adapter		  = vData->deviceidx;
	Mode		  = vData->modeidx;

	LogAlw("[VideoConfiguration] Adapter=%u, ModeIndex=%u", Adapter, Mode);

	VkPhysicalDevice phys = PickAdapter(Adapter); // GetAdapterIdentifier
	if (phys == VK_NULL_HANDLE) {
		LogErr("[No Vulkan adapter found]");
		return -1;
	}
	VkPhysicalDeviceProperties info;
	vkGetPhysicalDeviceProperties(phys, &info);
	LogOapi("3D-Adapter.............. : %s",info.deviceName);
	LogAlw("dwFSMode................ : %u",dwFSMode);

	QScreen *scr = hWnd->screen() ? hWnd->screen() : QGuiApplication::primaryScreen();
	qreal dpr = hWnd->devicePixelRatio();

	// Get DisplayMode Resolution ---------------------------------------
	//
	if (bIsFullscreen) {

		switch(dwFSMode) {

			// True Fullscreen
			case 0:
			{
				// EnumAdapterModes left out: the display mode isn't switched, the window covers the screen at its mode
				dwDisplayMode = 0;
				if (scr) {
					dwRenderWidth = (DWORD)(scr->geometry().width() * scr->devicePixelRatio());
					dwRenderHeight = (DWORD)(scr->geometry().height() * scr->devicePixelRatio());
				}
			}
			break;

			// Fullscreen Window
			case 1:
			{
				dwDisplayMode = 1;
				QRect r = scr ? scr->geometry() : QRect(0, 0, 1024, 768); // SM_CXSCREEN, SM_CYSCREEN
				if (vData->pageflip && scr) r = scr->virtualGeometry(); // SM_CXVIRTUALSCREEN
				hWnd->setFlags(hWnd->flags() | Qt::FramelessWindowHint); // WS_CLIPCHILDREN | WS_VISIBLE, no frame
				hWnd->setGeometry(r);
				hWnd->show();
				bIsFullscreen = false;
			}
			break;

			// Fullscreen Window with Taskbar
			case 2:
			{
				dwDisplayMode = 1;
				QRect rect = scr ? scr->availableGeometry() : QRect(0, 0, 1024, 768); // SPI_GETWORKAREA
				hWnd->setFlags(hWnd->flags() | Qt::FramelessWindowHint);
				int x = scr ? scr->geometry().width() : rect.width();
				if (vData->pageflip) x = rect.width();
				hWnd->setGeometry(rect.left(), rect.top(), x, rect.height());
				hWnd->show();
				bIsFullscreen = false;
			}
			break;
		}
	}
	else {
		dwDisplayMode = 2;
		hWnd->show(); // WS_CLIPCHILDREN | WS_VISIBLE
		if (vData->trystencil)
			hWnd->resize((int)(vData->winw / dpr), (int)(vData->winh / dpr));
	}

	// Hardware CAPS Checks --------------------------------------------------
	// (GetDeviceCaps → the physical device limits)
	const VkPhysicalDeviceLimits &lim = info.limits;
	VkPhysicalDeviceFeatures feat;
	vkGetPhysicalDeviceFeatures(phys, &feat);

	caps.MaxTextureWidth = lim.maxImageDimension2D;
	caps.MaxTextureHeight = lim.maxImageDimension2D;
	caps.MaxTextureRepeat = 8192; // Vulkan has no repeat limit; the value upstream's users clamp against
	caps.MaxPrimitiveCount = std::min(lim.maxDrawIndexedIndexValue, 0xFFFFFu); // D3D9 drivers' 20-bit value: callers add it to counts (2^32-1 wrapped the star chunking)
	caps.MaxVertexIndex = std::min(lim.maxDrawIndexedIndexValue, 0xFFFFFFu); // D3D9 drivers' 24-bit value, same reason
	caps.MaxAnisotropy = (DWORD)lim.maxSamplerAnisotropy;

	// AA CAPS Checks --------------------------------------------------
	//
	DWORD aamax = 0;
	VkSampleCountFlags sc = lim.framebufferColorSampleCounts & lim.framebufferDepthSampleCounts;
	if (sc & VK_SAMPLE_COUNT_2_BIT) aamax=2;
	if (sc & VK_SAMPLE_COUNT_4_BIT) aamax=4;
	if (sc & VK_SAMPLE_COUNT_8_BIT) aamax=8;

	MultiSample = std::min(aamax, DWORD(Config->SceneAntialias));

	if (MultiSample==1) MultiSample = 0;

	// D3D9-only caps left out (blend stages, repeat, volume address, blend matrices, instruction count, decl/misc/dev caps)
	LogOapi("MaxTextureWidth......... : %u",caps.MaxTextureWidth);
	LogOapi("MaxTextureHeight........ : %u",caps.MaxTextureHeight);
	LogAlw("MaxVolumeExtent......... : %u",lim.maxImageDimension3D);
	LogAlw("MaxPrimitiveCount....... : %u",caps.MaxPrimitiveCount);
	LogAlw("MaxVertexIndex.......... : %u",caps.MaxVertexIndex);
	LogAlw("MaxAnisotropy........... : %u",caps.MaxAnisotropy);
	LogAlw("MaxSimultaneousTextures. : %u",lim.maxPerStageDescriptorSamplers);
	LogAlw("MaxStreams.............. : %u",lim.maxVertexInputBindings);
	LogAlw("MaxStreamStride......... : %u",lim.maxVertexInputBindingStride);
	LogAlw("MaxPointSize............ : %f",lim.pointSizeRange[1]);
	LogAlw("Vulkan API Version...... : %u.%u.%u",VK_API_VERSION_MAJOR(info.apiVersion),VK_API_VERSION_MINOR(info.apiVersion),VK_API_VERSION_PATCH(info.apiVersion));
	LogOapi("NumSimultaneousRTs...... : %u",lim.maxColorAttachments);

	// COLORWRITEENABLE and non-power of 2 textures are core Vulkan; pixel shader 3.0 → Vulkan 1.4 (VkDev checks features)
	if (info.apiVersion < VK_API_VERSION_1_4) {
		LogErr("[Vulkan 1.4 is required]");
		bFail=true;
	}

	// Do Some Additional Hardware Checks ===================================================================

	LogOapi("Separate AlphaBlend..... : Yes"); // core Vulkan blending

	VkFormat depthFmt = FormatHas(phys, VK_FORMAT_D24_UNORM_S8_UINT, VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT) ? VK_FORMAT_D24_UNORM_S8_UINT : VK_FORMAT_D32_SFLOAT_S8_UINT;
	const VkFormatFeatureFlags RT = VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT | VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT;

	// Check shadow mapping support
	//
	bool bShadowMap = true;
	if (!FormatHas(phys, VK_FORMAT_R32_SFLOAT, RT)) bShadowMap = false;
	if (!FormatHas(phys, depthFmt, VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT)) bShadowMap = false;

	if (bShadowMap) LogOapi("Shadow Mapping.......... : Yes");
	else			LogOapi("Shadow Mapping.......... : No");

	bool bFloat16BB = FormatHas(phys, VK_FORMAT_R16G16B16A16_SFLOAT, RT);

	if (bFloat16BB) LogOapi("D3DFMT_A16B16G16R16F.... : Yes");
	else		    LogOapi("D3DFMT_A16B16G16R16F.... : No");

	// vertex texture fetch: any sampled format can be read in the vertex stage
	bool VT16BB = FormatHas(phys, VK_FORMAT_R16G16B16A16_SFLOAT, VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT);
	bool VT32BB = FormatHas(phys, VK_FORMAT_R32G32B32A32_SFLOAT, VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT);
	bool VT16BC = FormatHas(phys, VK_FORMAT_R16_SFLOAT, VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT);
	bool VT32BC = FormatHas(phys, VK_FORMAT_R32_SFLOAT, VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT);

	if (VT16BB) LogOapi("Vertex_A16B16G16R16F.... : Yes");
	else 		LogOapi("Vertex_A16B16G16R16F.... : No");
	if (VT32BB) LogOapi("Vertex_A32B32G32R32F.... : Yes");
	else 		LogOapi("Vertex_A32B32G32R32F.... : No");
	if (VT16BC) LogOapi("Vertex_R16F............. : Yes");
	else 		LogOapi("Vertex_R16F............. : No");
	if (VT32BC) LogOapi("Vertex_R32F............. : Yes");
	else 		LogOapi("Vertex_R32F............. : No");

	if (!VT16BB || !VT32BB || !VT16BC || !VT32BC) bFail = true;

	bool bFloat32BB = FormatHas(phys, VK_FORMAT_R32G32B32A32_SFLOAT, RT);

	if (bFloat32BB) LogOapi("D3DFMT_A32B32G32R32F.... : Yes");
	else		    LogOapi("D3DFMT_A32B32G32R32F.... : No");

	bool D32F = FormatHas(phys, VK_FORMAT_D32_SFLOAT, VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT);

	if (D32F) LogOapi("D3DFMT_D32F_LOCKABLE.... : Yes");
	else	  LogOapi("D3DFMT_D32F_LOCKABLE.... : No");

	bool AR10 = FormatHas(phys, VK_FORMAT_A2R10G10B10_UNORM_PACK32, RT);

	if (AR10) LogOapi("D3DFMT_A2R10G10B10...... : Yes");
	else	  LogOapi("D3DFMT_A2R10G10B10...... : No");

	bool L8 = FormatHas(phys, VK_FORMAT_R8_UNORM, RT);

	if (L8) LogOapi("D3DFMT_L8............... : Yes");
	else	LogOapi("D3DFMT_L8............... : No");

	if (VertexFormatHas(phys, VK_FORMAT_A2B10G10R10_SNORM_PACK32)) LogOapi("D3DDTCAPS_DEC3N......... : Yes");
	else														   LogOapi("D3DDTCAPS_DEC3N......... : No");

	if (VertexFormatHas(phys, VK_FORMAT_R16G16_SFLOAT)) LogOapi("D3DDTCAPS_FLOAT16_2..... : Yes");
	else												LogOapi("D3DDTCAPS_FLOAT16_2..... : No");

	if (VertexFormatHas(phys, VK_FORMAT_R16G16B16A16_SFLOAT)) LogOapi("D3DDTCAPS_FLOAT16_4..... : Yes");
	else													  LogOapi("D3DDTCAPS_FLOAT16_4..... : No");

	// Check (Log) whether orbiter runs on WINE
	//
	LogOapi("Runs under WINE......... : %s", OapiExtension::RunsUnderWINE() ? "Yes" : "No");
	LogOapi("D3D9Build Date.......... : %u", BuildDate());

	// Log some locale information
	//
	//int size = max( GetLocaleInfoEx(LOCALE_NAME_USER_DEFAULT, LOCALE_SDECIMAL, NULL, 0),
	//                GetLocaleInfoEx(LOCALE_NAME_USER_DEFAULT, LOCALE_SENGLISHDISPLAYNAME, NULL, 0) );
	//auto buff = new WCHAR[size];

	//GetLocaleInfoEx(LOCALE_NAME_USER_DEFAULT, LOCALE_SDECIMAL, buff, size);
	//LogOapi("Decimal separator....... : %ls", buff);
	//GetLocaleInfoEx(LOCALE_NAME_USER_DEFAULT, LOCALE_SENGLISHDISPLAYNAME, buff, size);
	//LogOapi("Locale.................. : %ls", buff);

	//delete[] buff;

	// Check MipMap autogeneration (blits with linear filtering)
	//
	if (!FormatHas(phys, VK_FORMAT_B8G8R8A8_UNORM, VK_FORMAT_FEATURE_BLIT_SRC_BIT | VK_FORMAT_FEATURE_BLIT_DST_BIT | VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT)) {
		LogWrn("[No Hardware MipMap auto generation]");
		oapiWriteLog((char*)"D3D9: WARNING: [No Hardware MipMap auto generation]");
	}

	if (bFail) {
		oapiWriteLog((char*)"D3D9: FAIL: !! Graphics card doesn't meet the minimum requirements to run !!");
		QMessageBox::critical(NULL, "VulkanClient Error", "Graphics card doesn't meet the minimum requirements to run VulkanClient.");
		return -1;
	}

	dwZBufferBitDepth = (depthFmt == VK_FORMAT_D24_UNORM_S8_UINT) ? 24 : 32;
	dwStencilBitDepth = 8;

	int hr;
	if (bIsFullscreen) hr = CreateFullscreenMode();
	else			   hr = CreateWindowedMode();

	if (hr < 0) {
		LogErr("[Device Initialization Failed]");
		return hr;
	}

	VkPhysicalDeviceMemoryProperties mp; // GetAvailableTextureMem
	vkGetPhysicalDeviceMemoryProperties(phys, &mp);
	VkDeviceSize vram = 0;
	for (UINT i = 0; i < mp.memoryHeapCount; i++) if (mp.memoryHeaps[i].flags & VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) vram += mp.memoryHeaps[i].size;
	LogOapi("Available Texture Memory : %u MB", (DWORD)(vram >> 20));

	pDevice->BeginFrame(); // D3D9 takes device calls at any time: a frame's command buffer is always open
	pDevice->SetViewport(0.0f, 0.0f, (float)dwRenderWidth, (float)dwRenderHeight, 0.0f, 1.0f);

	pNTVertexDecl = new VkVertexDecl(NTVertexDecl, (UINT)std::size(NTVertexDecl)); // CreateVertexDeclaration
	pBAVertexDecl = new VkVertexDecl(BAVertexDecl, (UINT)std::size(BAVertexDecl));
	pPosColorDecl = new VkVertexDecl(PosColorDecl, (UINT)std::size(PosColorDecl));
	pPositionDecl = new VkVertexDecl(PositionDecl, (UINT)std::size(PositionDecl));
	pVector4Decl = new VkVertexDecl(Vector4Decl, (UINT)std::size(Vector4Decl));
	pPosTexDecl = new VkVertexDecl(PosTexDecl, (UINT)std::size(PosTexDecl));
	pHazeVertexDecl = new VkVertexDecl(HazeVertexDecl, (UINT)std::size(HazeVertexDecl));
	pMeshVertexDecl = new VkVertexDecl(MeshVertexDecl, (UINT)std::size(MeshVertexDecl));
	pPatchVertexDecl = new VkVertexDecl(PatchVertexDecl, (UINT)std::size(PatchVertexDecl));
	pSketchpadDecl = new VkVertexDecl(SketchpadDecl, (UINT)std::size(SketchpadDecl));
	pLocalLightsDecl = new VkVertexDecl(LocalLightsDecl, (UINT)std::size(LocalLightsDecl));

	// Setup some default fonts
	// (D3DXCreateFontIndirect left out: the client draws its labels with Sketchpad fonts)

	LogAlw("=== [3DDevice Initialized] ===");
	return 0;
}

//-----------------------------------------------------------------------------
// Name: CreateFullscreenBuffers()
// Desc: Creates the primary and (optional) backbuffer for rendering.
//       Windowed mode and fullscreen mode are handled differently.
//-----------------------------------------------------------------------------
int CD3DFramework9::CreateFullscreenMode()
{

	// Get the dimensions of the screen bounds
	// Store the rectangle which contains the renderer
	rcScreenRect = { 0, 0, (LONG)dwRenderWidth, (LONG)dwRenderHeight };

	LogAlw("[FULLSCREEN MODE] %u x %u,  hWindow=%s", dwRenderWidth, dwRenderHeight, _PTR(hWnd));

	// D3DMULTISAMPLE_NONE, D3DFMT_X8R8G8B8 backbuffer, D24S8 depth (d3dPP)
	pDevice = new VkDev(g_pD3DObject, PickAdapter(Adapter)); // CreateDevice
	if (!pDevice->IsOK()) {
		SAFE_DELETE(pDevice);
		return -1;
	}
	if (CreateSwapchain() < 0) return -1;

	// Get Backbuffer
	VkImageUsageFlags u = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;
	pRenderTarget = new VkSurf(pDevice, dwRenderWidth, dwRenderHeight, VK_FORMAT_B8G8R8A8_UNORM, u);
	pDepthStencil = new VkSurf(pDevice, dwRenderWidth, dwRenderHeight, dwZBufferBitDepth == 24 ? VK_FORMAT_D24_UNORM_S8_UINT : VK_FORMAT_D32_SFLOAT_S8_UINT, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT);
	pBackBuffer = (SURFHANDLE) new SurfNative(pRenderTarget, OAPISURFACE_BACKBUFFER | OAPISURFACE_RENDER3D | OAPISURFACE_RENDERTARGET, pDepthStencil);
	SURFACE(pBackBuffer)->SetName("BackBuffer");
	return 0;
}

//-----------------------------------------------------------------------------
// Name: CreateWindowedBuffers()
// Desc: Creates the primary and (optional) backbuffer for rendering.
//       Windowed mode and fullscreen mode are handled differently.
//-----------------------------------------------------------------------------
int CD3DFramework9::CreateWindowedMode()
{
	_TRACE;

	// Get the dimensions of the viewport and screen bounds
	qreal dpr = hWnd->devicePixelRatio();
	rcScreenRect = { 0, 0, (LONG)(hWnd->width() * dpr), (LONG)(hWnd->height() * dpr) }; // GetClientRect, device pixels

	// What is this ?!!
	//ClientToScreen(hWnd, (POINT*)&rcScreenRect.left);
	//ClientToScreen(hWnd, (POINT*)&rcScreenRect.right);

	dwRenderWidth	  = rcScreenRect.right  - rcScreenRect.left;
	dwRenderHeight	  = rcScreenRect.bottom - rcScreenRect.top;

	LogAlw("Window Size = [%u, %u]", dwRenderWidth, dwRenderHeight);
	LogAlw("Window LeftTop = [%d, %d]", rcScreenRect.left, rcScreenRect.top);

	// NVPerfHUD's D3DDEVTYPE_REF left out: no reference rasterizer (lavapipe can be picked as the adapter instead)
	if (nvPerfHud) LogErr("[WARNING] NVPerfHUD mode is not available (Disable from D3D9Client.cfg) [WARNING]");

	pDevice = new VkDev(g_pD3DObject, PickAdapter(Adapter)); // CreateDevice
	if (!pDevice->IsOK()) {
		LogErr("CreateDevice() Failed");
		SAFE_DELETE(pDevice);
		return -1;
	}
	if (CreateSwapchain() < 0) return -1;

	// Get Backbuffer (multisampled when MultiSample is set, resolved at Present)
	VkSampleCountFlagBits s = MultiSample ? (VkSampleCountFlagBits)MultiSample : VK_SAMPLE_COUNT_1_BIT;
	VkImageUsageFlags u = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;
	if (!MultiSample) u |= VK_IMAGE_USAGE_SAMPLED_BIT;
	pRenderTarget = new VkSurf(pDevice, dwRenderWidth, dwRenderHeight, VK_FORMAT_B8G8R8A8_UNORM, u, s);
	pRenderTarget->tex->SetSwizzle(VkSwizzleMap(SWZ_NOALPHA)); // D3DFMT_X8R8G8B8: same meaning as the surfaces clbkCreateSurfaceEx makes without alpha
	pDepthStencil = new VkSurf(pDevice, dwRenderWidth, dwRenderHeight, dwZBufferBitDepth == 24 ? VK_FORMAT_D24_UNORM_S8_UINT : VK_FORMAT_D32_SFLOAT_S8_UINT, VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, s);
	if (MultiSample) pResolve = new VkSurf(pDevice, dwRenderWidth, dwRenderHeight, VK_FORMAT_B8G8R8A8_UNORM, VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT);
	pBackBuffer = (SURFHANDLE) new SurfNative(pRenderTarget, OAPISURFACE_BACKBUFFER | OAPISURFACE_RENDER3D | OAPISURFACE_RENDERTARGET, pDepthStencil);
	SURFACE(pBackBuffer)->SetName("BackBuffer");
	return 0;
}

//-----------------------------------------------------------------------------
// not upstream: the swapchain D3D9 kept inside the device
//-----------------------------------------------------------------------------
int CD3DFramework9::CreateSwapchain()
{
	VkPhysicalDevice phys = pDevice->phys;
	if (!surface) surface = QVulkanInstance::surfaceForWindow(hWnd);
	if (!surface) {
		LogErr("[No Vulkan surface for the render window]");
		return -1;
	}
	VkBool32 present = VK_FALSE;
	vkGetPhysicalDeviceSurfaceSupportKHR(phys, pDevice->queueFamily, surface, &present);
	if (!present) {
		LogErr("[The graphics queue can't present to the render window]");
		return -1;
	}

	VkSurfaceCapabilitiesKHR sc;
	VKCHECK(vkGetPhysicalDeviceSurfaceCapabilitiesKHR(phys, surface, &sc));

	UINT n = 0;
	vkGetPhysicalDeviceSurfaceFormatsKHR(phys, surface, &n, NULL);
	std::vector<VkSurfaceFormatKHR> sf(n);
	vkGetPhysicalDeviceSurfaceFormatsKHR(phys, surface, &n, sf.data());
	VkSurfaceFormatKHR fmt = n ? sf[0] : VkSurfaceFormatKHR{ VK_FORMAT_B8G8R8A8_UNORM, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR };
	for (auto &f : sf) if (f.format == VK_FORMAT_B8G8R8A8_UNORM && f.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) { fmt = f; break; } // X8R8G8B8
	swapFormat = fmt.format;

	vkGetPhysicalDeviceSurfacePresentModesKHR(phys, surface, &n, NULL);
	std::vector<VkPresentModeKHR> pm(n);
	vkGetPhysicalDeviceSurfacePresentModesKHR(phys, surface, &n, pm.data());
	VkPresentModeKHR mode = VK_PRESENT_MODE_FIFO_KHR; // D3DPRESENT_INTERVAL_ONE
	if (bNoVSync) { // D3DPRESENT_INTERVAL_IMMEDIATE
		for (auto m : pm) if (m == VK_PRESENT_MODE_MAILBOX_KHR) mode = m;
		for (auto m : pm) if (m == VK_PRESENT_MODE_IMMEDIATE_KHR) mode = m;
	}

	swapExtent = sc.currentExtent;
	if (swapExtent.width == 0xFFFFFFFF) {
		swapExtent.width = std::clamp((UINT)dwRenderWidth, sc.minImageExtent.width, sc.maxImageExtent.width);
		swapExtent.height = std::clamp((UINT)dwRenderHeight, sc.minImageExtent.height, sc.maxImageExtent.height);
	}
	if (swapExtent.width == 0 || swapExtent.height == 0) return 0; // minimized: keep the old swapchain

	UINT count = sc.minImageCount + 1;
	if (sc.maxImageCount && count > sc.maxImageCount) count = sc.maxImageCount;

	VkSwapchainKHR old = swapchain;
	VkSwapchainCreateInfoKHR ci = { VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR };
	ci.surface = surface;
	ci.minImageCount = count;
	ci.imageFormat = fmt.format;
	ci.imageColorSpace = fmt.colorSpace;
	ci.imageExtent = swapExtent;
	ci.imageArrayLayers = 1;
	ci.imageUsage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
	ci.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
	ci.preTransform = sc.currentTransform;
	ci.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
	if (!(sc.supportedCompositeAlpha & ci.compositeAlpha)) ci.compositeAlpha = (VkCompositeAlphaFlagBitsKHR)(sc.supportedCompositeAlpha & -sc.supportedCompositeAlpha);
	ci.presentMode = mode;
	ci.clipped = VK_TRUE;
	ci.oldSwapchain = old;
	VkResult r = vkCreateSwapchainKHR(pDevice->dev, &ci, NULL, &swapchain);
	if (old) {
		pDevice->WaitIdle();
		vkDestroySwapchainKHR(pDevice->dev, old, NULL);
	}
	if (r < 0) {
		LogErr("vkCreateSwapchainKHR() Failed (%d)", (int)r);
		swapchain = VK_NULL_HANDLE;
		return -1;
	}

	vkGetSwapchainImagesKHR(pDevice->dev, swapchain, &n, NULL);
	swapImages.resize(n);
	vkGetSwapchainImagesKHR(pDevice->dev, swapchain, &n, swapImages.data());

	VkSemaphoreCreateInfo si = { VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO };
	while (presentSem.size() < n) {
		VkSemaphore s;
		VKCHECK(vkCreateSemaphore(pDevice->dev, &si, NULL, &s));
		presentSem.push_back(s);
	}
	for (int i = 0; i < VkDev::NFRAMES; i++)
		if (!acquireSem[i]) VKCHECK(vkCreateSemaphore(pDevice->dev, &si, NULL, &acquireSem[i]));

	LogAlw("[Swapchain] %u x %u, %u images, present mode %d", swapExtent.width, swapExtent.height, n, (int)mode);
	return 0;
}

void CD3DFramework9::DestroySwapchain()
{
	if (!pDevice) return;
	pDevice->WaitIdle();
	for (auto s : presentSem) vkDestroySemaphore(pDevice->dev, s, NULL);
	presentSem.clear();
	for (int i = 0; i < VkDev::NFRAMES; i++) {
		if (acquireSem[i]) vkDestroySemaphore(pDevice->dev, acquireSem[i], NULL);
		acquireSem[i] = VK_NULL_HANDLE;
	}
	if (swapchain) vkDestroySwapchainKHR(pDevice->dev, swapchain, NULL);
	swapchain = VK_NULL_HANDLE;
	swapImages.clear();
	surface = VK_NULL_HANDLE; // owned by the window's QVulkanInstance
}

//-----------------------------------------------------------------------------
// not upstream: IDirect3DDevice9::Present: blits the backbuffer to the next swapchain image, submits the frame
//-----------------------------------------------------------------------------
static void SwapBarrier(VkCommandBuffer cmd, VkImage img, VkImageLayout from, VkImageLayout to)
{
	VkImageMemoryBarrier2 b = { VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2 };
	b.srcStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	b.srcAccessMask = VK_ACCESS_2_MEMORY_WRITE_BIT;
	b.dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
	b.dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT;
	b.oldLayout = from;
	b.newLayout = to;
	b.srcQueueFamilyIndex = b.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
	b.image = img;
	b.subresourceRange = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1 };
	VkDependencyInfo di = { VK_STRUCTURE_TYPE_DEPENDENCY_INFO };
	di.imageMemoryBarrierCount = 1;
	di.pImageMemoryBarriers = &b;
	vkCmdPipelineBarrier2(cmd, &di);
}

int CD3DFramework9::Present()
{
	if (!pDevice || !pDevice->IsRecording()) return -1;
	pDevice->EndRendering();

	UINT idx = 0;
	VkSemaphore acq = acquireSem[iAcquire];
	VkResult r = swapchain ? vkAcquireNextImageKHR(pDevice->dev, swapchain, UINT64_MAX, acq, VK_NULL_HANDLE, &idx) : VK_ERROR_OUT_OF_DATE_KHR;
	if (r == VK_ERROR_OUT_OF_DATE_KHR || r < 0) { // run the frame without showing it, then follow the window size
		pDevice->EndFrame(VK_NULL_HANDLE, VK_NULL_HANDLE);
		CreateSwapchain();
		pDevice->BeginFrame();
		return 0;
	}
	iAcquire = (iAcquire + 1) % VkDev::NFRAMES;

	VkCommandBuffer cmd = pDevice->Cmd();
	VkSurf *src = pRenderTarget;
	if (pRenderTarget->tex->samples != VK_SAMPLE_COUNT_1_BIT && pResolve) { // D3D9 resolved the multisampled backbuffer at Present
		pRenderTarget->tex->Transition(cmd, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
		pResolve->tex->Transition(cmd, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
		VkImageResolve2 rr = { VK_STRUCTURE_TYPE_IMAGE_RESOLVE_2 };
		rr.srcSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 };
		rr.dstSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 };
		rr.extent = { dwRenderWidth, dwRenderHeight, 1 };
		VkResolveImageInfo2 ri = { VK_STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2 };
		ri.srcImage = pRenderTarget->tex->img;
		ri.srcImageLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
		ri.dstImage = pResolve->tex->img;
		ri.dstImageLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
		ri.regionCount = 1;
		ri.pRegions = &rr;
		vkCmdResolveImage2(cmd, &ri);
		src = pResolve;
	}
	src->tex->Transition(cmd, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);
	SwapBarrier(cmd, swapImages[idx], VK_IMAGE_LAYOUT_UNDEFINED, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
	VkImageBlit2 blit = { VK_STRUCTURE_TYPE_IMAGE_BLIT_2 };
	blit.srcSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 };
	blit.srcOffsets[1] = { (int)src->w, (int)src->h, 1 };
	blit.dstSubresource = { VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1 };
	blit.dstOffsets[1] = { (int)swapExtent.width, (int)swapExtent.height, 1 };
	VkBlitImageInfo2 bi = { VK_STRUCTURE_TYPE_BLIT_IMAGE_INFO_2 };
	bi.srcImage = src->tex->img;
	bi.srcImageLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
	bi.dstImage = swapImages[idx];
	bi.dstImageLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
	bi.regionCount = 1;
	bi.pRegions = &blit;
	bi.filter = VK_FILTER_LINEAR; // the window may have been resized since the backbuffer was made
	vkCmdBlitImage2(cmd, &bi);
	SwapBarrier(cmd, swapImages[idx], VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, VK_IMAGE_LAYOUT_PRESENT_SRC_KHR);

	pDevice->EndFrame(acq, presentSem[idx]);

	VkPresentInfoKHR pi = { VK_STRUCTURE_TYPE_PRESENT_INFO_KHR };
	pi.waitSemaphoreCount = 1;
	pi.pWaitSemaphores = &presentSem[idx];
	pi.swapchainCount = 1;
	pi.pSwapchains = &swapchain;
	pi.pImageIndices = &idx;
	r = pDevice->QueuePresent(&pi);
	if (r == VK_ERROR_OUT_OF_DATE_KHR || r == VK_SUBOPTIMAL_KHR) CreateSwapchain();
	else if (r < 0) LogErr("vkQueuePresentKHR() Failed (%d)", (int)r);

	pDevice->BeginFrame();
	return 0;
}
