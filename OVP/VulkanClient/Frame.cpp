// ==============================================================
// File: vkframe.cpp
// Desc: Class functions to implement a Direct3D app framework.
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 - 2016 Martin Schweiger
//				 2011 - 2016 Jarmo Nikkanen
// ==============================================================

#define STRICT

#define _CRT_SECURE_NO_DEPRECATE

#include <windows.h>
#include "GraphicsAPI.h"
#include "Frame.h"
#include "Util.h"
#include "AABBUtil.h"
#include "Surface.h"
#include "Log.h"
#include "Config.h"
#include "OapiExtension.h"

using namespace oapi;

IDirect3DVertexDeclaration9	*pMeshVertexDecl = NULL;
IDirect3DVertexDeclaration9	*pHazeVertexDecl = NULL;
IDirect3DVertexDeclaration9	*pNTVertexDecl = NULL;
IDirect3DVertexDeclaration9	*pBAVertexDecl = NULL;
IDirect3DVertexDeclaration9	*pPosColorDecl = NULL;
IDirect3DVertexDeclaration9	*pPositionDecl = NULL;
IDirect3DVertexDeclaration9	*pVector4Decl  = NULL;
IDirect3DVertexDeclaration9	*pPosTexDecl   = NULL;
IDirect3DVertexDeclaration9	*pPatchVertexDecl = NULL;
IDirect3DVertexDeclaration9 *pSketchpadDecl = NULL;
IDirect3DVertexDeclaration9 *pLocalLightsDecl = NULL;

static const char *d3dmessage={"Required DirectX version (June 2010 or newer) not found\0"};

//-----------------------------------------------------------------------------
// Name: CD3DFramework9()
// Desc: The constructor. Clears static variables
//-----------------------------------------------------------------------------

CD3DFramework9::CD3DFramework9()
{
	Clear();

	if (g_pD3DObject == NULL) {
		LogErr("ERROR: [Direct3D9 Creation Failed]");
		LogErr(d3dmessage);
		MessageBoxA(NULL, d3dmessage, "vkClient Initialization Failed",MB_OK);
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
	pLargeFont		  = NULL;
	pSmallFont		  = NULL;
	dwZBufferBitDepth = 0;
	dwStencilBitDepth = 0;
	Adapter			  = 0;
	Mode			  = 0;
	MultiSample		  = 0;
	pRenderTarget	  = NULL;
	pBackBuffer		  = NULL;
	pDepthStencil	  = NULL;
	
	memset((void *)&rcScreenRect, 0, sizeof(RECT));
	memset((void *)&d3dPP, 0, sizeof(D3DPRESENT_PARAMETERS));
	memset((void *)&caps, 0, sizeof(D3DCAPS9));
}

//-----------------------------------------------------------------------------
// Name: DestroyObjects()
// Desc: Objects created in Initialize() section are destroyed in here
//-----------------------------------------------------------------------------
HRESULT CD3DFramework9::DestroyObjects ()
{
	_TRACE;
	LogAlw("========== Destroying framework objects ==========");

	SAFE_RELEASE(pRenderTarget);
	SAFE_RELEASE(pDepthStencil);
	SAFE_RELEASE(pLargeFont);
	SAFE_RELEASE(pSmallFont);
	SAFE_RELEASE(pNTVertexDecl);
	SAFE_RELEASE(pBAVertexDecl);
	SAFE_RELEASE(pPosColorDecl);
	SAFE_RELEASE(pPositionDecl);
	SAFE_RELEASE(pVector4Decl);
	SAFE_RELEASE(pPosTexDecl);
	SAFE_RELEASE(pHazeVertexDecl);
	SAFE_RELEASE(pMeshVertexDecl);
	SAFE_RELEASE(pPatchVertexDecl);
	SAFE_RELEASE(pSketchpadDecl);
	SAFE_RELEASE(pLocalLightsDecl);

	HR(pDevice->EvictManagedResources());

	Sleep(200);

	if (pDevice->Reset(&d3dPP)==S_OK)	LogAlw("[DirectX Device Reset Succesfull]");
	else								LogAlw("[Failed to Reset DirectX Device] (Likely blocked by undeleted resources)");

	SAFE_RELEASE(pDevice);

	return S_OK;
}

//-----------------------------------------------------------------------------
// Name: Initialize()
// Desc: Creates the internal objects for the framework
//-----------------------------------------------------------------------------
HRESULT CD3DFramework9::Initialize(HWND _hWnd, GraphicsClient::VIDEODATA *vData)
{
	_TRACE;

	Clear();

	DDM = (Config->DisableDriverManagement != 0);
	nvPerfHud = (Config->NVPerfHUD != 0);

	bool bFail = false;

	if (_hWnd==NULL || vData==NULL) {
		LogErr("ERROR: Invalid input parameter in CD3DFramework9::Initialize()");
		return E_INVALIDARG;
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

	D3DADAPTER_IDENTIFIER9 info;
	g_pD3DObject->GetAdapterIdentifier(Adapter, 0, &info);
	LogOapi("3D-Adapter.............. : %s",info.Description);
	LogAlw("dwFSMode................ : %u",dwFSMode);

	// Get DisplayMode Resolution ---------------------------------------
	//
	if (bIsFullscreen) {

		switch(dwFSMode) {

			// True Fullscreen
			case 0:
			{
				dwDisplayMode = 0;
				D3DDISPLAYMODE mode;
				if (Adapter<g_pD3DObject->GetAdapterCount()) {
					if (Mode<g_pD3DObject->GetAdapterModeCount(Adapter, D3DFMT_X8R8G8B8)) {
						g_pD3DObject->EnumAdapterModes(Adapter, D3DFMT_X8R8G8B8, Mode, &mode);
						dwRenderWidth = mode.Width;
						dwRenderHeight = mode.Height;
					}
				}
			}
			break;

			// Fullscreen Window
			case 1:
			{
				dwDisplayMode = 1;
				int x = GetSystemMetrics(SM_CXSCREEN);
				int y = GetSystemMetrics(SM_CYSCREEN);
				if (vData->pageflip) x = GetSystemMetrics(SM_CXVIRTUALSCREEN);
				SetWindowLongA(hWnd, GWL_STYLE, WS_CLIPCHILDREN | WS_VISIBLE);
				SetWindowPos(hWnd,0, 0,0, x, y, SWP_SHOWWINDOW);
				bIsFullscreen = false;
			}
			break;

			// Fullscreen Window with Taskbar
			case 2:
			{
				dwDisplayMode = 1;
				RECT rect;
				SystemParametersInfo(SPI_GETWORKAREA,0,&rect,0);
				SetWindowLongA(hWnd, GWL_STYLE, WS_CLIPCHILDREN | WS_VISIBLE);
				int x = GetSystemMetrics(SM_CXSCREEN);
				if (vData->pageflip) x = rect.right-rect.left;
				SetWindowPos(hWnd,0,rect.left, rect.top, x, rect.bottom - rect.top, SWP_SHOWWINDOW);
				bIsFullscreen = false;
			}
			break;
		}
	}
	else {
		dwDisplayMode = 2;
		LONG x = GetWindowLongA(hWnd, GWL_STYLE);
		SetWindowLongA(hWnd, GWL_STYLE, x | WS_CLIPCHILDREN | WS_VISIBLE);
		if (vData->trystencil)
			SetWindowPos(hWnd, 0, 0, 0, vData->winw, vData->winh, SWP_SHOWWINDOW | SWP_NOSENDCHANGING);
	}

	// Hardware CAPS Checks --------------------------------------------------
	//
	HRESULT hr = g_pD3DObject->GetDeviceCaps(Adapter, D3DDEVTYPE_HAL, &caps);

	if (hr!=S_OK) {
		LogErr("g_pD3DObject->GetDeviceCaps(Adapter, D3DDEVTYPE_HAL, &caps)");
		return hr;
	}

	// AA CAPS Checks --------------------------------------------------
	//
	DWORD aamax = 0;
	if (g_pD3DObject->CheckDeviceMultiSampleType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_2_SAMPLES, NULL)==S_OK) aamax=2;
	if (g_pD3DObject->CheckDeviceMultiSampleType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_4_SAMPLES, NULL)==S_OK) aamax=4;
	if (g_pD3DObject->CheckDeviceMultiSampleType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_8_SAMPLES, NULL)==S_OK) aamax=8;

	MultiSample = min(aamax, DWORD(Config->SceneAntialias));

	if (MultiSample==1) MultiSample = 0;

	LogAlw("MaxTextureBlendStages... : %u",caps.MaxTextureBlendStages);
	LogOapi("MaxTextureWidth......... : %u",caps.MaxTextureWidth);
	LogOapi("MaxTextureHeight........ : %u",caps.MaxTextureHeight);
	LogOapi("MaxTextureRepeat........ : %u",caps.MaxTextureRepeat);
	LogOapi("VolTexAddressCaps....... : 0x%X",caps.VolumeTextureAddressCaps);
	LogAlw("MaxVolumeExtent......... : %u",caps.MaxVolumeExtent);
	LogAlw("MaxPrimitiveCount....... : %u",caps.MaxPrimitiveCount);
	LogAlw("MaxVertexIndex.......... : %u",caps.MaxVertexIndex);
	LogAlw("MaxAnisotropy........... : %u",caps.MaxAnisotropy);
	LogAlw("MaxSimultaneousTextures. : %u",caps.MaxSimultaneousTextures);
	LogAlw("MaxStreams.............. : %u",caps.MaxStreams);
	LogAlw("MaxStreamStride......... : %u",caps.MaxStreamStride);
	LogAlw("MaxVertexBlendMatrices.. : %u",caps.MaxVertexBlendMatrices);
	LogAlw("MaxVShaderInstrExecuted. : %u",caps.MaxVShaderInstructionsExecuted);
	LogAlw("MaxPointSize............ : %f",caps.MaxPointSize);
	LogAlw("VertexShaderVersion..... : 0x%hX",(caps.VertexShaderVersion&0xFFFF));
	LogAlw("PixelShaderVersion...... : 0x%hX",(caps.PixelShaderVersion&0xFFFF));
	LogOapi("NumSimultaneousRTs...... : %u",caps.NumSimultaneousRTs);
	LogAlw("D3DPTEXTURECAPS_POW2.... : %d",(caps.TextureCaps&D3DPTEXTURECAPS_POW2)>0);
	LogAlw("NONPOW2CONDITIONAL...... : %d",(caps.TextureCaps&D3DPTEXTURECAPS_NONPOW2CONDITIONAL)>0);
	LogOapi("VertexDeclCaps.......... : 0x%X",caps.DeclTypes);
	LogOapi("MiscCaps................ : 0x%X", caps.PrimitiveMiscCaps);
	LogAlw("DevCaps................. : 0x%X",caps.DevCaps);
	LogAlw("DevCaps2................ : 0x%X",caps.DevCaps2);
	

	// Check non power of two texture support
	if ((caps.PrimitiveMiscCaps&D3DPMISCCAPS_COLORWRITEENABLE)==0) {
		LogErr("D3DRS_COLORWRITEENABLE Not Suppoertd");
		bFail = true;
	}

	// Check non power of two texture support
	if ((caps.TextureCaps&D3DPTEXTURECAPS_POW2) && !(caps.TextureCaps&D3DPTEXTURECAPS_NONPOW2CONDITIONAL)) {
		LogErr("[Non-Power of 2 texture support required]");
		bFail=true;
	}

	if ((caps.TextureCaps&D3DPTEXTURECAPS_POW2) && (caps.TextureCaps&D3DPTEXTURECAPS_NONPOW2CONDITIONAL)) {
		LogWrn("[Hardware has only a limited non-power of 2 texture support]");
	}

	if ((caps.PixelShaderVersion&0xFFFF)<0x0300 || (caps.VertexShaderVersion&0xFFFF)<0x0300) {
		LogErr("[Pixel Shader Version 3.0 is required (i.e. DirectX 9.0c)]");
		bFail=true;
	}

	

	// Do Some Additional Hardware Checks ===================================================================
	
	if (caps.PrimitiveMiscCaps & D3DPMISCCAPS_SEPARATEALPHABLEND) {
		LogOapi("Separate AlphaBlend..... : Yes");
	}
	else {
		LogOapi("Separate AlphaBlend..... : No");
	}


	// Check shadow mapping support
	//
	bool bShadowMap = true;
	if (g_pD3DObject->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_RENDERTARGET, D3DRTYPE_TEXTURE, D3DFMT_R32F)!=S_OK) bShadowMap = false;
	if (g_pD3DObject->CheckDepthStencilMatch(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DFMT_R32F, D3DFMT_D24X8)!=S_OK) bShadowMap = false;

	if (bShadowMap) LogOapi("Shadow Mapping.......... : Yes");
	else			LogOapi("Shadow Mapping.......... : No");

	bool bFloat16BB = true;
	if (g_pD3DObject->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_RENDERTARGET, D3DRTYPE_TEXTURE, D3DFMT_A16B16G16R16F)!=S_OK) bFloat16BB = false;
	if (g_pD3DObject->CheckDepthStencilMatch(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DFMT_A16B16G16R16F, D3DFMT_D24X8)!=S_OK) bFloat16BB = false;
				  
	if (bFloat16BB) LogOapi("D3DFMT_A16B16G16R16F.... : Yes");
	else		    LogOapi("D3DFMT_A16B16G16R16F.... : No");

	HRESULT VT16BB = g_pD3DObject->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_QUERY_VERTEXTEXTURE, D3DRTYPE_TEXTURE, D3DFMT_A16B16G16R16F);
	HRESULT VT32BB = g_pD3DObject->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_QUERY_VERTEXTEXTURE, D3DRTYPE_TEXTURE, D3DFMT_A32B32G32R32F);
	HRESULT VT16BC = g_pD3DObject->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_QUERY_VERTEXTEXTURE, D3DRTYPE_TEXTURE, D3DFMT_R16F);
	HRESULT VT32BC = g_pD3DObject->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_QUERY_VERTEXTEXTURE, D3DRTYPE_TEXTURE, D3DFMT_R32F);

	if (VT16BB == S_OK) LogOapi("Vertex_A16B16G16R16F.... : Yes");
	else 				LogOapi("Vertex_A16B16G16R16F.... : No");
	if (VT32BB == S_OK) LogOapi("Vertex_A32B32G32R32F.... : Yes");
	else 				LogOapi("Vertex_A32B32G32R32F.... : No");
	if (VT16BC == S_OK) LogOapi("Vertex_R16F............. : Yes");
	else 				LogOapi("Vertex_R16F............. : No");
	if (VT32BC == S_OK) LogOapi("Vertex_R32F............. : Yes");
	else 				LogOapi("Vertex_R32F............. : No");

	if (VT16BB != S_OK || VT32BB != S_OK || VT16BC != S_OK || VT32BC != S_OK) bFail = true;
	

	bool bFloat32BB = true;
	if (g_pD3DObject->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_RENDERTARGET, D3DRTYPE_TEXTURE, D3DFMT_A32B32G32R32F)!=S_OK) bFloat32BB = false;
			  
	if (bFloat32BB) LogOapi("D3DFMT_A32B32G32R32F.... : Yes");
	else		    LogOapi("D3DFMT_A32B32G32R32F.... : No");

	HRESULT D32F = g_pD3DObject->CheckDeviceFormat(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, D3DFMT_D32F_LOCKABLE);

	if (D32F==S_OK) LogOapi("D3DFMT_D32F_LOCKABLE.... : Yes"); 
	else		    LogOapi("D3DFMT_D32F_LOCKABLE.... : No");

	HRESULT AR10 = g_pD3DObject->CheckDeviceFormat(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_RENDERTARGET, D3DRTYPE_TEXTURE, D3DFMT_A2R10G10B10);

	if (AR10==S_OK) LogOapi("D3DFMT_A2R10G10B10...... : Yes"); 
	else		    LogOapi("D3DFMT_A2R10G10B10...... : No");

	HRESULT L8 = g_pD3DObject->CheckDeviceFormat(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_RENDERTARGET, D3DRTYPE_TEXTURE, D3DFMT_L8);
	if (g_pD3DObject->CheckDepthStencilMatch(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DFMT_L8, D3DFMT_D24X8) != S_OK) L8 = S_FALSE;//-1;

	if (L8 == S_OK) LogOapi("D3DFMT_L8............... : Yes");
	else		    LogOapi("D3DFMT_L8............... : No");

	

	if (caps.DeclTypes&D3DDTCAPS_DEC3N) LogOapi("D3DDTCAPS_DEC3N......... : Yes");
	else								LogOapi("D3DDTCAPS_DEC3N......... : No");

	if (caps.DeclTypes&D3DDTCAPS_FLOAT16_2) LogOapi("D3DDTCAPS_FLOAT16_2..... : Yes");
	else								    LogOapi("D3DDTCAPS_FLOAT16_2..... : No");

	if (caps.DeclTypes&D3DDTCAPS_FLOAT16_4) LogOapi("D3DDTCAPS_FLOAT16_4..... : Yes");
	else								    LogOapi("D3DDTCAPS_FLOAT16_4..... : No");

	// Check (Log) whether orbiter runs on WINE
	//
	LogOapi("Runs under WINE......... : %s", OapiExtension::RunsUnderWINE() ? "Yes" : "No");
	LogOapi("vkBuild Date.......... : %u", BuildDate());

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

	// Check MipMap autogeneration
	//
	if ((caps.Caps2&D3DCAPS2_CANAUTOGENMIPMAP)==0) {
		LogWrn("[No Hardware MipMap auto generation]");
		oapiWriteLog((char*)"vk: WARNING: [No Hardware MipMap auto generation]");
	}

	if (bFail) {
		oapiWriteLog((char*)"vk: FAIL: !! Graphics card doesn't meet the minimum requirements to run !!");
		MessageBoxA(NULL, "Graphics card doesn't meet the minimum requirements to run vkClient.", "vkClient Error",MB_OK);
		return -1;
	}

	if (bIsFullscreen) hr = CreateFullscreenMode();
	else			   hr = CreateWindowedMode();

	if (FAILED(hr)) {
		LogErr("[Device Initialization Failed]");
		return hr;
	}

	LogOapi("Available Texture Memory : %u MB", pDevice->GetAvailableTextureMem() >> 20);

	D3DVIEWPORT9 vp;

	vp.X      = 0;
	vp.Y      = 0;
	vp.Width  = dwRenderWidth;
	vp.Height = dwRenderHeight;
	vp.MinZ   = 0.0f;
	vp.MaxZ   = 1.0f;

	pDevice->SetViewport(&vp);

	HR(pDevice->CreateVertexDeclaration(NTVertexDecl, &pNTVertexDecl));
	HR(pDevice->CreateVertexDeclaration(BAVertexDecl, &pBAVertexDecl));
	HR(pDevice->CreateVertexDeclaration(PosColorDecl, &pPosColorDecl));
	HR(pDevice->CreateVertexDeclaration(PositionDecl, &pPositionDecl));
	HR(pDevice->CreateVertexDeclaration(Vector4Decl,  &pVector4Decl));
	HR(pDevice->CreateVertexDeclaration(PosTexDecl,   &pPosTexDecl));
	HR(pDevice->CreateVertexDeclaration(HazeVertexDecl,  &pHazeVertexDecl));
	HR(pDevice->CreateVertexDeclaration(MeshVertexDecl,  &pMeshVertexDecl));
	HR(pDevice->CreateVertexDeclaration(PatchVertexDecl, &pPatchVertexDecl));
	HR(pDevice->CreateVertexDeclaration(SketchpadDecl, &pSketchpadDecl));
	HR(pDevice->CreateVertexDeclaration(LocalLightsDecl, &pLocalLightsDecl));

	// Setup some default fonts
	//
	D3DXFONT_DESC fontDesc;
	fontDesc.Height          = 30;
	fontDesc.Width           = 22;
	fontDesc.Weight          = FW_BOLD;
	fontDesc.MipLevels       = 0;
	fontDesc.Italic          = false;
	fontDesc.CharSet         = DEFAULT_CHARSET;
	fontDesc.OutputPrecision = OUT_DEFAULT_PRECIS;
	fontDesc.Quality         = ANTIALIASED_QUALITY;
	fontDesc.PitchAndFamily  = DEFAULT_PITCH | FF_DONTCARE;
	strcpy(fontDesc.FaceName, "Arial");

	HR(D3DXCreateFontIndirect(pDevice, &fontDesc, &pLargeFont));

	fontDesc.Height          = 16;
	fontDesc.Width           = 10;
	fontDesc.Weight          = FW_NORMAL;
	fontDesc.MipLevels       = 0;
	fontDesc.Italic          = false;
	fontDesc.CharSet         = DEFAULT_CHARSET;
	fontDesc.OutputPrecision = OUT_DEFAULT_PRECIS;
	fontDesc.Quality         = ANTIALIASED_QUALITY;
	fontDesc.PitchAndFamily  = DEFAULT_PITCH | FF_DONTCARE;
	strcpy(fontDesc.FaceName, "Arial");

	HR(D3DXCreateFontIndirect(pDevice, &fontDesc, &pSmallFont));

	LogAlw("=== [3DDevice Initialized] ===");
	return S_OK;
}

//-----------------------------------------------------------------------------
// Name: CreateFullscreenBuffers()
// Desc: Creates the primary and (optional) backbuffer for rendering.
//       Windowed mode and fullscreen mode are handled differently.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework9::CreateFullscreenMode()
{

	// Get the dimensions of the screen bounds
	// Store the rectangle which contains the renderer
	SetRect(&rcScreenRect, 0, 0, dwRenderWidth, dwRenderHeight);

	HR(g_pD3DObject->CheckDeviceType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DFMT_X8R8G8B8, false));

	LogAlw("[FULLSCREEN MODE] %u x %u,  hWindow=%s", dwRenderWidth, dwRenderHeight, _PTR(hWnd));

	dwZBufferBitDepth = 24;
	dwStencilBitDepth = 8;

	d3dPP.BackBufferWidth            = dwRenderWidth;
	d3dPP.BackBufferHeight           = dwRenderHeight;
	d3dPP.BackBufferFormat           = D3DFMT_X8R8G8B8;
	d3dPP.BackBufferCount            = 1;
	d3dPP.MultiSampleType            = D3DMULTISAMPLE_NONE;
	d3dPP.MultiSampleQuality         = 0;
	d3dPP.SwapEffect                 = D3DSWAPEFFECT_DISCARD;
	d3dPP.hDeviceWindow              = hWnd;
	d3dPP.Windowed                   = false;
	d3dPP.EnableAutoDepthStencil     = true;
	d3dPP.AutoDepthStencilFormat	 = D3DFMT_D24S8;
	d3dPP.Flags                      = D3DPRESENTFLAG_LOCKABLE_BACKBUFFER; // Must be lockable to allow GDI dialogs to work
	d3dPP.FullScreen_RefreshRateInHz = D3DPRESENT_RATE_DEFAULT;

	if (bNoVSync) d3dPP.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
	else		  d3dPP.PresentationInterval = D3DPRESENT_INTERVAL_ONE;

	DWORD devBehaviorFlags = 0;

	if (SWVert) devBehaviorFlags |= D3DCREATE_SOFTWARE_VERTEXPROCESSING;
	else		devBehaviorFlags |= D3DCREATE_HARDWARE_VERTEXPROCESSING;

	if (Pure)   devBehaviorFlags |= D3DCREATE_PUREDEVICE;

	if (DDM)	devBehaviorFlags |= D3DCREATE_DISABLE_DRIVER_MANAGEMENT;

	HR(g_pD3DObject->CreateDevice(
		Adapter,			// primary adapter
		D3DDEVTYPE_HAL,		// device type
		hWnd,				// window associated with device
		devBehaviorFlags|D3DCREATE_MULTITHREADED|D3DCREATE_FPU_PRESERVE,
		&d3dPP,				// present parameters
		&pDevice));		// return created device

	// Get Backbuffer
	if (pDevice) {
		pDevice->GetRenderTarget(0, &pRenderTarget);
		pDevice->GetDepthStencilSurface(&pDepthStencil);
		pBackBuffer = (SURFHANDLE) new SurfNative(pRenderTarget, OAPISURFACE_BACKBUFFER | OAPISURFACE_RENDER3D | OAPISURFACE_RENDERTARGET, pDepthStencil);
		SURFACE(pBackBuffer)->SetName("BackBuffer");
		return S_OK;
	}

	return -1;
}

//-----------------------------------------------------------------------------
// Name: CreateWindowedBuffers()
// Desc: Creates the primary and (optional) backbuffer for rendering.
//       Windowed mode and fullscreen mode are handled differently.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework9::CreateWindowedMode()
{
	_TRACE;

	// Get the dimensions of the viewport and screen bounds
	GetClientRect(hWnd, &rcScreenRect);
	
	// What is this ?!!
	//ClientToScreen(hWnd, (POINT*)&rcScreenRect.left);
	//ClientToScreen(hWnd, (POINT*)&rcScreenRect.right);

	dwRenderWidth	  = rcScreenRect.right  - rcScreenRect.left;
	dwRenderHeight	  = rcScreenRect.bottom - rcScreenRect.top;
	dwZBufferBitDepth = 24;
	dwStencilBitDepth = 8;

	LogAlw("Window Size = [%u, %u]", dwRenderWidth, dwRenderHeight);
	LogAlw("Window LeftTop = [%d, %d]", rcScreenRect.left, rcScreenRect.top);

	if (MultiSample) {

		DWORD level;
		HR(g_pD3DObject->CheckDeviceMultiSampleType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, (D3DMULTISAMPLE_TYPE)MultiSample, &level));

		d3dPP.BackBufferWidth            = dwRenderWidth;
		d3dPP.BackBufferHeight           = dwRenderHeight;
		d3dPP.BackBufferFormat           = D3DFMT_X8R8G8B8;
		d3dPP.BackBufferCount            = 1;
		d3dPP.MultiSampleType            = (D3DMULTISAMPLE_TYPE)MultiSample;
		d3dPP.MultiSampleQuality         = level - 1;
		d3dPP.SwapEffect                 = D3DSWAPEFFECT_DISCARD;
		d3dPP.hDeviceWindow              = hWnd;
		d3dPP.Windowed                   = true;
		d3dPP.EnableAutoDepthStencil     = true;
		d3dPP.AutoDepthStencilFormat	 = D3DFMT_D24S8;
		d3dPP.Flags                      = 0;
		d3dPP.FullScreen_RefreshRateInHz = D3DPRESENT_RATE_DEFAULT;
	}
	else {
		d3dPP.BackBufferWidth            = dwRenderWidth;
		d3dPP.BackBufferHeight           = dwRenderHeight;
		d3dPP.BackBufferFormat           = D3DFMT_X8R8G8B8;
		d3dPP.BackBufferCount            = 1;
		d3dPP.MultiSampleType            = D3DMULTISAMPLE_NONE;
		d3dPP.MultiSampleQuality         = 0;
		d3dPP.SwapEffect                 = D3DSWAPEFFECT_DISCARD;
		d3dPP.hDeviceWindow              = hWnd;
		d3dPP.Windowed                   = true;
		d3dPP.EnableAutoDepthStencil     = true;
		d3dPP.AutoDepthStencilFormat	 = D3DFMT_D24S8;
		d3dPP.Flags                      = 0;
		d3dPP.FullScreen_RefreshRateInHz = D3DPRESENT_RATE_DEFAULT;
	}

	if (bNoVSync) d3dPP.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
	else		  d3dPP.PresentationInterval = D3DPRESENT_INTERVAL_ONE;

	DWORD devBehaviorFlags = D3DCREATE_HARDWARE_VERTEXPROCESSING;

	if (Pure)   devBehaviorFlags |= D3DCREATE_PUREDEVICE;
	if (DDM)	devBehaviorFlags |= D3DCREATE_DISABLE_DRIVER_MANAGEMENT;

	HRESULT hr = 0;

	if (nvPerfHud && Adapter==1) {

		LogErr("[WARNING] NVPerfHUD mode is Active (Disable from vkClient.cfg) [WARNING]");

		hr = g_pD3DObject->CreateDevice(
			Adapter,				// primary adapter
			D3DDEVTYPE_REF,			// device type
			hWnd,					// window associated with device
			devBehaviorFlags|D3DCREATE_MULTITHREADED|D3DCREATE_FPU_PRESERVE,
			&d3dPP,					// present parameters
			&pDevice);			// return created device
	}
	else {

		hr = g_pD3DObject->CreateDevice(
			Adapter,				// primary adapter
			D3DDEVTYPE_HAL,			// device type
			hWnd,					// window associated with device
			devBehaviorFlags|D3DCREATE_MULTITHREADED|D3DCREATE_FPU_PRESERVE,
			&d3dPP,					// present parameters
			&pDevice);			// return created device
	}

	if (hr!=S_OK) LogErr("CreateDevice() Failed HR=0x%X",hr);

	// Get Backbuffer
	if (pDevice) {
		pDevice->GetRenderTarget(0, &pRenderTarget);
		pDevice->GetDepthStencilSurface(&pDepthStencil);
		pBackBuffer = (SURFHANDLE) new SurfNative(pRenderTarget, OAPISURFACE_BACKBUFFER | OAPISURFACE_RENDER3D | OAPISURFACE_RENDERTARGET, pDepthStencil);
		SURFACE(pBackBuffer)->SetName("BackBuffer");
		return S_OK;
	}

	return -1;
}

