// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2026 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

// ============================================================================
// File: D3D9frame.h
// Desc: Class to manage the Direct3D environment objects
//
//       The class is initialized with the Initialize() function, after which
//       the Get????() functions can be used to access the objects needed for
//       rendering. If the device or display needs to be changed, the
//       ChangeDevice() function can be called. If the display window is moved
//       the changes need to be reported with the Move() function.
//
//       After rendering a frame, the ShowFrame() function flips or blits the
//       backbuffer contents to the primary. If surfaces are lost, they can be
//       restored with the RestoreSurfaces() function. Finally, if normal
//       Windows output is needed, the FlipToGDISurface() provides a GDI
//       surface to draw on.
// ============================================================================

#ifndef D3D9FRAME_H
#define D3D9FRAME_H

// d3d9.h/d3dx9.h left out: VkCore.h (via D3D9Client.h)
#include "Orbitersdk.h"
#include "D3D9Client.h"

class SurfNative;

// not upstream: the device limits the client reads (D3DCAPS9 counterpart)
struct VkDevCaps {
	DWORD MaxTextureWidth;
	DWORD MaxTextureHeight;
	DWORD MaxTextureRepeat;
	DWORD MaxPrimitiveCount;
	DWORD MaxVertexIndex;
	DWORD MaxAnisotropy;
};

//-----------------------------------------------------------------------------
// Name: CD3DFramework9
// Desc: The Direct3D sample framework class for DX9. Maintains the D3D
//       surfaces and device used for 3D rendering.
//-----------------------------------------------------------------------------
class CD3DFramework9
{

private:

    // Internal variables for the framework class
    QWindow *              hWnd;               // The window object
    BOOL                   bIsFullscreen;      // Fullscreen vs. windowed
    BOOL                   bVertexTexture;
    BOOL                   bAAEnabled;
    BOOL                   bNoVSync;           // don't use vertical sync in fullscreen
    BOOL                   Alpha;
    BOOL                   SWVert;
    BOOL                   Pure;
    BOOL                   DDM;
    BOOL                   nvPerfHud;
    DWORD                  dwRenderWidth;      // Dimensions of the render target
    DWORD                  dwRenderHeight;     // Dimensions of the render target
    DWORD                  dwFSMode;
    VkDev *                pDevice;            // The D3D device
    // pLargeFont/pSmallFont (ID3DXFont) left out: the client draws its labels with its own fonts
    DWORD                  dwZBufferBitDepth;  // Bit depth of z-buffer
    DWORD                  dwStencilBitDepth;  // Bit depth of stencil buffer (0 if none)
    DWORD                  Adapter;
    DWORD                  Mode;
    DWORD                  MultiSample;
	DWORD				   dwDisplayMode;
    VkSurf *               pRenderTarget;      // offscreen backbuffer, blitted to the swapchain by Present()
	VkSurf *               pDepthStencil;
	VkSurf *               pResolve;           // single sample copy of a multisampled backbuffer
    SURFHANDLE			   pBackBuffer;
	// d3dPP (D3DPRESENT_PARAMETERS) → the swapchain below
	VkSurfaceKHR           surface;
	VkSwapchainKHR         swapchain;
	VkFormat               swapFormat;
	VkExtent2D             swapExtent;
	std::vector<VkImage>   swapImages;
	std::vector<VkSemaphore> presentSem;       // one per swapchain image
	VkSemaphore            acquireSem[VkDev::NFRAMES];
	int                    iAcquire;
	VkDevCaps              caps;
    RECT                   rcScreenRect;       // Screen rect for window

    // Internal functions for the framework class

    int     CreateFullscreenMode();
    int     CreateWindowedMode();
    int     CreateSwapchain();                 // not upstream
    void    DestroySwapchain();                // not upstream
    void    Clear();

public:

    // Access functions for DirectX objects
    inline QWindow *           GetRenderWindow() const          { return hWnd; }
    inline VkDev *             GetD3DDevice() const             { return pDevice; }
    inline DWORD               GetZBufferBitDepth() const       { return dwZBufferBitDepth; }
    inline DWORD               GetStencilBitDepth() const       { return dwStencilBitDepth; }
    inline VkFormat            GetDepthFormat() const           { return dwZBufferBitDepth == 24 ? VK_FORMAT_D24_UNORM_S8_UINT : VK_FORMAT_D32_SFLOAT_S8_UINT; } // not upstream: D3DFMT_D24S8
    inline DWORD               GetWidth() const                 { return dwRenderWidth; }  // Dimensions of the render target
    inline DWORD               GetHeight() const                { return dwRenderHeight; } // Dimensions of the render target
    inline const RECT          GetScreenRect() const            { return rcScreenRect; }
    inline VkSurf *            GetBackBuffer() const            { return pRenderTarget; }
    inline SURFHANDLE          GetBackBufferHandle() const      { return pBackBuffer; }
    inline BOOL                IsFullscreen() const             { return bIsFullscreen; }
    inline BOOL                IsAAEnabled() const              { return bAAEnabled; }
    inline const VkDevCaps *   GetCaps() const                  { return &caps; }
    inline BOOL                HasVertexTextureSup() const      { return bVertexTexture; }
    inline BOOL                GetVSync() const                 { return (bNoVSync==FALSE); }

	// GetDisplayMode 0=True Fullscreen, 1=Fullscreen Window, 2=Windowed
	inline DWORD			   GetDisplayMode() const			{ return dwDisplayMode; }

    // Creates the Framework
    int Initialize(QWindow *hWnd, struct oapi::GraphicsClient::VIDEODATA *vData);

    int DestroyObjects();

    int Present();                             // not upstream: IDirect3DDevice9::Present

            CD3DFramework9();
           ~CD3DFramework9();
};


//-----------------------------------------------------------------------------
// Flags used for the Initialize() method of a CD3DFramework object
//-----------------------------------------------------------------------------
#define D3DFW_FULLSCREEN    0x00000001 // Use fullscreen mode
#define D3DFW_STEREO        0x00000002 // Use stereo-scopic viewing
#define D3DFW_ZBUFFER       0x00000004 // Create and use a zbuffer
#define D3DFW_NO_FPUSETUP   0x00000008 // Don't use default DDSCL_FPUSETUP flag
#define D3DFW_NOVSYNC       0x00000010 // Don't use vertical sync in fullscreen
#define D3DFW_PAGEFLIP      0x00000020 // Allow page flipping in fullscreen


//-----------------------------------------------------------------------------
// Errors that the Initialize() and ChangeDriver() calls may return
//-----------------------------------------------------------------------------
#define D3DFWERR_INITIALIZATIONFAILED 0x82000000
#define D3DFWERR_NODIRECTDRAW         0x82000001
#define D3DFWERR_COULDNTSETCOOPLEVEL  0x82000002
#define D3DFWERR_NODIRECT3D           0x82000003
#define D3DFWERR_NO3DDEVICE           0x82000004
#define D3DFWERR_NOZBUFFER            0x82000005
#define D3DFWERR_INVALIDZBUFFERDEPTH  0x82000006
#define D3DFWERR_NOVIEWPORT           0x82000007
#define D3DFWERR_NOPRIMARY            0x82000008
#define D3DFWERR_NOCLIPPER            0x82000009
#define D3DFWERR_BADDISPLAYMODE       0x8200000a
#define D3DFWERR_NOBACKBUFFER         0x8200000b
#define D3DFWERR_NONZEROREFCOUNT      0x8200000c
#define D3DFWERR_NORENDERTARGET       0x8200000d
#define D3DFWERR_INVALIDMODE          0x8200000e
#define D3DFWERR_NOTINITIALIZED       0x8200000f

#endif // !D3D9FRAME_H

