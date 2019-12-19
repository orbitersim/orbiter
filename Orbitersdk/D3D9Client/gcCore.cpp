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
HWND gcCore::GetRenderWindow()
{
	return g_client->GetRenderWindow();	
}