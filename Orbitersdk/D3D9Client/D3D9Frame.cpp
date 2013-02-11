// ==============================================================
// File: D3D9frame.cpp
// Desc: Class functions to implement a Direct3D app framework.
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
//				 2011 Jarmo Nikkanen, All rights reserved. 
// ==============================================================




#define STRICT

#define _CRT_SECURE_NO_DEPRECATE

#include <windows.h>
#include <xnamath.h>
#include "GraphicsAPI.h"
#include "D3D9Frame.h"
#include "D3D9Util.h"
#include "D3D9Surface.h"
#include "Log.h"
#include "D3D9Config.h"

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
IDirect3DVertexDeclaration9	*pGPUBlitDecl = NULL;

static char *d3dmessage={"Required DirectX version (June 2010 or newer) not found\0"};

//-----------------------------------------------------------------------------
// Name: CD3DFramework9()
// Desc: The constructor. Clears static variables
//-----------------------------------------------------------------------------

CD3DFramework9::CD3DFramework9()
{
	pD3D = NULL;  
	Clear();

	// Create the Direct3D9 ---------------------------------------------
	//
    pD3D = Direct3DCreate9(D3D_SDK_VERSION);

	if (pD3D==NULL) {
		LogErr("ERROR: [Direct3D9 Creation Failed]");
		LogErr(d3dmessage);
		MessageBoxA(NULL, d3dmessage, "D3D9Client Initialization Failed",MB_OK);
	}
}

//-----------------------------------------------------------------------------
// Name: ~CD3DFramework9()
// Desc: The destructor. Deletes all objects
//-----------------------------------------------------------------------------
CD3DFramework9::~CD3DFramework9 ()
{
	LogAlw("Deleting Framework");
	SAFE_RELEASE(pD3D);
}

void CD3DFramework9::Clear()
{
	// NOTE: pD3D won't be cleared in here

	hWnd			  = NULL;
    bIsFullscreen	  = false;
	bAAEnabled		  = false;
	bNoVSync		  = false;
	Alpha			  = false;
	SWVert			  = false;
	Pure			  = false;
	DDM				  = false;
	bGDIBB			  = false;
	nvPerfHud		  = false;
	dwRenderWidth	  = 0;
    dwRenderHeight	  = 0;
	dwFSMode		  = 0;
	pd3dDevice		  = NULL;
	pLargeFont		  = NULL;
	pSmallFont		  = NULL;
	dwZBufferBitDepth = 0;
	dwStencilBitDepth = 0;
	Adapter			  = 0;
	Mode			  = 0;
	MultiSample		  = 0;
	pRenderTarget	  = NULL;
	pBackBuffer		  = NULL;
	pEnvDS			  = NULL;

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
	SAFE_RELEASE(pLargeFont);
	SAFE_RELEASE(pSmallFont);
	SAFE_RELEASE(pNTVertexDecl);
	SAFE_RELEASE(pPosColorDecl);
	SAFE_RELEASE(pPositionDecl);
	SAFE_RELEASE(pVector4Decl);
	SAFE_RELEASE(pPosTexDecl);
	SAFE_RELEASE(pHazeVertexDecl);
	SAFE_RELEASE(pMeshVertexDecl);
	SAFE_RELEASE(pPatchVertexDecl);
	SAFE_RELEASE(pGPUBlitDecl);
	SAFE_RELEASE(pEnvDS);
	
	if (pd3dDevice->Reset(&d3dPP)==S_OK)	LogAlw("[DirectX Device Reset Succesfull]");
	else									LogErr("[Failed to Reset DirectX Device]");				

	SAFE_RELEASE(pd3dDevice);

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
	dwFSMode	  = (vData->modeidx>>8)&0xFF;
	bGDIBB		  = vData->trystencil;
	Adapter		  = vData->deviceidx;
	Mode		  = vData->modeidx&0xFF;

	LogAlw("[VideoConfiguration] Adapter=%u, ModeIndex=%u", Adapter, Mode);
	LogAlw("dwFSMode = %u",dwFSMode);

	D3DADAPTER_IDENTIFIER9 info;
	pD3D->GetAdapterIdentifier(Adapter, 0, &info);
	LogAlw("Adapter = %s",info.Description);
	
	
	// Get DisplayMode Resolution ---------------------------------------
	//
	if (bIsFullscreen) {

		switch(dwFSMode) {

			case 0:	
			{
				D3DDISPLAYMODE mode;
				if (Adapter<pD3D->GetAdapterCount()) {
					if (Mode<pD3D->GetAdapterModeCount(Adapter, D3DFMT_X8R8G8B8)) {
						pD3D->EnumAdapterModes(Adapter, D3DFMT_X8R8G8B8, Mode, &mode); 
						dwRenderWidth = mode.Width;
						dwRenderHeight = mode.Height;
					}
				}
			}
			break;

			case 1:
			{
				int x = GetSystemMetrics(SM_CXVIRTUALSCREEN);
				int y = GetSystemMetrics(SM_CYVIRTUALSCREEN);
				SetWindowLongA(hWnd, GWL_STYLE, WS_CLIPSIBLINGS|WS_VISIBLE);
				SetWindowPos(hWnd,0, 0,0, x, y, SWP_SHOWWINDOW);
				bIsFullscreen = false;
			}
			break;

			case 2:
			{
				RECT rect;
				SystemParametersInfo(SPI_GETWORKAREA,0,&rect,0);	
				SetWindowLongA(hWnd, GWL_STYLE, WS_CLIPSIBLINGS|WS_VISIBLE);
				SetWindowPos(hWnd,0,rect.left, rect.top, rect.right-rect.left, rect.bottom - rect.top,SWP_SHOWWINDOW);
				bIsFullscreen = false;
			}
			break;
		}

		

	}

	// Hardware CAPS Checks --------------------------------------------------
	//	
	HRESULT hr = pD3D->GetDeviceCaps(Adapter, D3DDEVTYPE_HAL, &caps);

	if (hr!=S_OK) {
		LogErr("pD3D->GetDeviceCaps(Adapter, D3DDEVTYPE_HAL, &caps)");
		return hr;
	}

	// AA CAPS Checks --------------------------------------------------
	//
	DWORD aamax = 0;
	if (pD3D->CheckDeviceMultiSampleType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_2_SAMPLES, NULL)==S_OK) aamax=2;
	if (pD3D->CheckDeviceMultiSampleType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_4_SAMPLES, NULL)==S_OK) aamax=4;
	if (pD3D->CheckDeviceMultiSampleType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_8_SAMPLES, NULL)==S_OK) aamax=8;
	
	MultiSample = min(aamax, DWORD(Config->SceneAntialias));
	
	if (bGDIBB) MultiSample=0, bNoVSync = true;
	if (MultiSample==1) MultiSample = 0;

	LogAlw("MaxTextureBlendStages..: %u",caps.MaxTextureBlendStages);
	LogAlw("MaxTextureWidth........: %u",caps.MaxTextureWidth);
	LogAlw("MaxTextureHeight.......: %u",caps.MaxTextureHeight);
	LogAlw("MaxPrimitiveCount......: %u",caps.MaxPrimitiveCount);
	LogAlw("MaxVertexIndex.........: %u",caps.MaxVertexIndex);
	LogAlw("MaxAnisotropy..........: %u",caps.MaxAnisotropy);
	LogAlw("MaxSimultaneousTextures: %u",caps.MaxSimultaneousTextures);
	LogAlw("MaxStreams.............: %u",caps.MaxStreams);
	LogAlw("MaxStreamStride........: %u",caps.MaxStreamStride);
	LogAlw("MaxVertexBlendMatrices.: %u",caps.MaxVertexBlendMatrices);
	LogAlw("MaxVShaderInstrExecuted: %u",caps.MaxVShaderInstructionsExecuted);
	LogAlw("MaxPointSize...........: %f",caps.MaxPointSize);
	LogAlw("VertexShaderVersion....: 0x%hX",(caps.VertexShaderVersion&0xFFFF));
	LogAlw("PixelShaderVersion.....: 0x%hX",(caps.PixelShaderVersion&0xFFFF));
	LogAlw("NumSimultaneousRTs.....: %u",caps.NumSimultaneousRTs);
	LogAlw("D3DPTEXTURECAPS_POW2...: %d",(caps.TextureCaps&D3DPTEXTURECAPS_POW2)>0);
	LogAlw("NONPOW2CONDITIONAL.....: %d",(caps.TextureCaps&D3DPTEXTURECAPS_NONPOW2CONDITIONAL)>0);
	LogAlw("VertexDeclCaps.........: 0x%X",caps.DeclTypes);
	LogAlw("DevCaps................: 0x%X",caps.DevCaps);
	LogAlw("DevCaps2...............: 0x%X",caps.DevCaps2);
	
	BOOL bXna = XMVerifyCPUSupport();

	if (bXna) {
		LogAlw("XNA Math Support.......: Yes");
		oapiWriteLog("D3D9Client: Sytem has XNA math support");
	}
	else {
		LogErr("XNA Math Support.......: No");
		oapiWriteLog("D3D9Client:FAIL: Sytem has no XNA math support");
		bFail=true;
	}

	// Check non power of two texture support
	if ((caps.TextureCaps&D3DPTEXTURECAPS_POW2) && !(caps.TextureCaps&D3DPTEXTURECAPS_NONPOW2CONDITIONAL)) {
		LogErr("[Non-Power of 2 texture support required]");
		oapiWriteLog("D3D9Client:FAIL: [Non-Power of 2 texture support required]");
		bFail=true;
	}

	if ((caps.TextureCaps&D3DPTEXTURECAPS_POW2) && (caps.TextureCaps&D3DPTEXTURECAPS_NONPOW2CONDITIONAL)) {
		LogWrn("[Hardware has only a limited non-power of 2 texture support]");
		oapiWriteLog("D3D9Client:WARNING: [Hardware has only a limited non-power of 2 texture support]");
	}

	if ((caps.PixelShaderVersion&0xFFFF)<0x0200) {
		LogErr("[Pixel Shader Version 2.0 or better is required]");
		oapiWriteLog("D3D9Client:FAIL: [Pixel Shader Version 2.0 or better is required]");
		bFail=true;
	}

	if ((caps.VertexShaderVersion&0xFFFF)<0x0200) {
		LogErr("[Insufficient Vertex Shader. Attempting software vertex processing...]");
		oapiWriteLog("D3D9Client:WARNING: [Insufficient Vertex Shader. Attempting software vertex processing...]");
		SWVert = true;
	}

	if ((caps.DevCaps&D3DDEVCAPS_PUREDEVICE)==0) {
		Pure = false;	
		LogWrn("[Not a pure device]");
		oapiWriteLog("D3D9Client:WARNING: [Not a pure device]");
	}

	if ((caps.DevCaps&D3DDEVCAPS_HWTRANSFORMANDLIGHT)==0) {
		SWVert = true;
		LogWrn("[No Hardware T&L]");
		oapiWriteLog("D3D9Client:WARNING: [No Hardware T&L]");
	}


	// ==============================================================


	if ((caps.Caps2&D3DCAPS2_CANAUTOGENMIPMAP)==0) {
		LogWrn("[No Hardware MipMap auto generation]");
		oapiWriteLog("D3D9Client:WARNING: [No Hardware MipMap auto generation]");
	}
	else {
		HR(pD3D->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_AUTOGENMIPMAP, D3DRTYPE_TEXTURE, D3DFMT_DXT5));
		HR(pD3D->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_AUTOGENMIPMAP, D3DRTYPE_TEXTURE, D3DFMT_DXT3));
		HR(pD3D->CheckDeviceFormat(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_AUTOGENMIPMAP, D3DRTYPE_TEXTURE, D3DFMT_DXT1));
	}

	if (bFail) {
		MessageBoxA(NULL, "Graphics card doesn't meet the minimum requirements to run D3D9Client.", "D3D9Client Error",MB_OK);	
		return -1;
	}

	if (bIsFullscreen) hr = CreateFullscreenMode();
	else			   hr = CreateWindowedMode();

	if (FAILED(hr)) {
		LogErr("[Device Initialization Failed]");
		oapiWriteLog("D3D9Client:FAIL: [Device Initialization Failed]");	
		return hr;
	}

	LogAlw("Available Texture Memory = %u MB",pd3dDevice->GetAvailableTextureMem()>>20);

	D3DVIEWPORT9 vp;

	vp.X      = 0;
	vp.Y      = 0;
	vp.Width  = dwRenderWidth;
	vp.Height = dwRenderHeight;
	vp.MinZ   = 0.0f;
	vp.MaxZ   = 1.0f;

	pd3dDevice->SetViewport(&vp);

	HR(pd3dDevice->CreateVertexDeclaration(NTVertexDecl, &pNTVertexDecl));
	HR(pd3dDevice->CreateVertexDeclaration(BAVertexDecl, &pBAVertexDecl));
	HR(pd3dDevice->CreateVertexDeclaration(PosColorDecl, &pPosColorDecl));
	HR(pd3dDevice->CreateVertexDeclaration(PositionDecl, &pPositionDecl));
	HR(pd3dDevice->CreateVertexDeclaration(Vector4Decl,  &pVector4Decl));
	HR(pd3dDevice->CreateVertexDeclaration(PosTexDecl,   &pPosTexDecl));
	HR(pd3dDevice->CreateVertexDeclaration(HazeVertexDecl,  &pHazeVertexDecl));
	HR(pd3dDevice->CreateVertexDeclaration(MeshVertexDecl,  &pMeshVertexDecl));
	HR(pd3dDevice->CreateVertexDeclaration(PatchVertexDecl, &pPatchVertexDecl));
	HR(pd3dDevice->CreateVertexDeclaration(GPUBlitDecl, &pGPUBlitDecl));

	// Setup some default fomts
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

	HR(D3DXCreateFontIndirect(pd3dDevice, &fontDesc, &pLargeFont));

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

	HR(D3DXCreateFontIndirect(pd3dDevice, &fontDesc, &pSmallFont));

	DWORD EnvMapSize = Config->EnvMapSize;

	if (Config->EnvMapMode) {
		HR(pd3dDevice->CreateDepthStencilSurface(EnvMapSize, EnvMapSize, D3DFMT_D24S8, D3DMULTISAMPLE_NONE, 0, true, &pEnvDS, NULL));
	}
	else pEnvDS = NULL;

	LogOk("[3DDevice Initialized]");
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

	HR(pD3D->CheckDeviceType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DFMT_X8R8G8B8, false));

	LogMsg("[FULLSCREEN MODE] %u x %u,  hWindow=0x%X", dwRenderWidth, dwRenderHeight, hWnd);

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

	HR(pD3D->CreateDevice(
		Adapter,			// primary adapter
		D3DDEVTYPE_HAL,		// device type
		hWnd,				// window associated with device
		devBehaviorFlags|D3DCREATE_MULTITHREADED|D3DCREATE_FPU_PRESERVE,
	    &d3dPP,				// present parameters
	    &pd3dDevice));		// return created device

	// Get Backbuffer 
	if (pd3dDevice) {
		pd3dDevice->GetRenderTarget(0, &pRenderTarget);
		pBackBuffer = new D3D9ClientSurface(pd3dDevice);
		pBackBuffer->MakeBackBuffer(pRenderTarget);
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
    ClientToScreen(hWnd, (POINT*)&rcScreenRect.left);
    ClientToScreen(hWnd, (POINT*)&rcScreenRect.right);

	if (0==pD3D->CheckDeviceFormat(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, D3DFMT_D32F_LOCKABLE)) LogAlw("D3DFMT_D32F_LOCKABLE");
	if (0==pD3D->CheckDeviceFormat(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, D3DFMT_D32)) LogAlw("D3DFMT_D32");
	if (0==pD3D->CheckDeviceFormat(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, D3DFMT_D24S8)) LogAlw("D3DFMT_D24S8");
	if (0==pD3D->CheckDeviceFormat(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, D3DFMT_D24X8)) LogAlw("D3DFMT_D24X8");
	if (0==pD3D->CheckDeviceFormat(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, D3DFMT_D24FS8)) LogAlw("D3DFMT_D24FS8");
	
	
    dwRenderWidth	  = rcScreenRect.right  - rcScreenRect.left;
    dwRenderHeight	  = rcScreenRect.bottom - rcScreenRect.top;
	dwZBufferBitDepth = 24;
	dwStencilBitDepth = 8;

	if (MultiSample) {

		DWORD level;
		HR(pD3D->CheckDeviceMultiSampleType(Adapter, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, (D3DMULTISAMPLE_TYPE)MultiSample, &level));

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

	if (bGDIBB) d3dPP.Flags |= D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;
	
	if (bNoVSync) d3dPP.PresentationInterval = D3DPRESENT_INTERVAL_IMMEDIATE;
	else		  d3dPP.PresentationInterval = D3DPRESENT_INTERVAL_ONE;

	DWORD devBehaviorFlags = 0;

	if (SWVert) devBehaviorFlags |= D3DCREATE_SOFTWARE_VERTEXPROCESSING;
	else		devBehaviorFlags |= D3DCREATE_HARDWARE_VERTEXPROCESSING;
	if (Pure)   devBehaviorFlags |= D3DCREATE_PUREDEVICE;
	if (DDM)	devBehaviorFlags |= D3DCREATE_DISABLE_DRIVER_MANAGEMENT;

	HRESULT hr = 0;

	if (nvPerfHud && Adapter==1) {

		LogErr("[WARNING] NVPerfHUD mode is Active (Disable from D3D9Client.cfg) [WARNING]");
		
		hr = pD3D->CreateDevice(
			Adapter,				// primary adapter
			D3DDEVTYPE_REF,			// device type
			hWnd,					// window associated with device
			devBehaviorFlags|D3DCREATE_MULTITHREADED|D3DCREATE_FPU_PRESERVE,
			&d3dPP,					// present parameters
			&pd3dDevice);			// return created device
	}
	else {
	
		hr = pD3D->CreateDevice(
			Adapter,				// primary adapter
			D3DDEVTYPE_HAL,			// device type
			hWnd,					// window associated with device
			devBehaviorFlags|D3DCREATE_MULTITHREADED|D3DCREATE_FPU_PRESERVE,
			&d3dPP,					// present parameters
			&pd3dDevice);			// return created device
	}
	
	if (hr!=S_OK) LogErr("CreateDevice() Failed HR=0x%X",hr);
	
	// Get Backbuffer 
	if (pd3dDevice) {
		pd3dDevice->GetRenderTarget(0, &pRenderTarget);
		pBackBuffer = new D3D9ClientSurface(pd3dDevice);
		pBackBuffer->MakeBackBuffer(pRenderTarget);
		return S_OK;
	}

	return -1;
}


