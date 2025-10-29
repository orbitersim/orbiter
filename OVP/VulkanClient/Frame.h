// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

// ============================================================================
// File: vkframe.h
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

#ifndef vkFRAME_H
#define vkFRAME_H

#include <d3d9.h>
#include "MathAPI.h"
#include "Orbitersdk.h"
#include "Client.h"

class SurfNative;

//-----------------------------------------------------------------------------
// Name: CD3DFramework9
// Desc: The Direct3D sample framework class for DX9. Maintains the D3D
//       surfaces and device used for 3D rendering.
//-----------------------------------------------------------------------------
class CD3DFramework9
{

private:

    // Internal variables for the framework class
    HWND                   hWnd;               // The window object
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
    LPDIRECT3DDEVICE9      pDevice;            // The D3D device
    LPD3DXFONT             pLargeFont;
    LPD3DXFONT             pSmallFont;
    DWORD                  dwZBufferBitDepth;  // Bit depth of z-buffer
    DWORD                  dwStencilBitDepth;  // Bit depth of stencil buffer (0 if none)
    DWORD                  Adapter;
    DWORD                  Mode;
    DWORD                  MultiSample;
	DWORD				   dwDisplayMode;
    LPDIRECT3DSURFACE9     pRenderTarget;
	LPDIRECT3DSURFACE9     pDepthStencil;
    SURFHANDLE			   pBackBuffer;
	D3DPRESENT_PARAMETERS  d3dPP;
	D3DCAPS9               caps;
    RECT                   rcScreenRect;       // Screen rect for window

    // Internal functions for the framework class

    HRESULT CreateFullscreenMode();
    HRESULT CreateWindowedMode();
    void    Clear();

public:

    // Access functions for DirectX objects
    inline HWND                GetRenderWindow() const          { return hWnd; }
    inline LPDIRECT3DDEVICE9   GetD3DDevice() const             { return pDevice; }
    inline DWORD               GetZBufferBitDepth() const       { return dwZBufferBitDepth; }
    inline DWORD               GetStencilBitDepth() const       { return dwStencilBitDepth; }
    inline DWORD               GetWidth() const                 { return dwRenderWidth; }  // Dimensions of the render target
    inline DWORD               GetHeight() const                { return dwRenderHeight; } // Dimensions of the render target
    inline const RECT          GetScreenRect() const            { return rcScreenRect; }
    inline LPDIRECT3DSURFACE9  GetBackBuffer() const            { return pRenderTarget; }
    inline SURFHANDLE          GetBackBufferHandle() const      { return pBackBuffer; }
    inline LPD3DXFONT          GetLargeFont() const             { return pLargeFont; }
    inline LPD3DXFONT          GetSmallFont() const             { return pSmallFont; }
    inline BOOL                IsFullscreen() const             { return bIsFullscreen; }
    inline BOOL                IsAAEnabled() const              { return bAAEnabled; }
    inline const D3DCAPS9 *    GetCaps() const                  { return &caps; }
    inline BOOL                HasVertexTextureSup() const      { return bVertexTexture; }
    inline BOOL                GetVSync() const                 { return (bNoVSync==FALSE); }

	// GetDisplayMode 0=True Fullscreen, 1=Fullscreen Window, 2=Windowed
	inline DWORD			   GetDisplayMode() const			{ return dwDisplayMode; }

    // Creates the Framework
    HRESULT Initialize(HWND hWnd, struct oapi::GraphicsClient::VIDEODATA *vData);

    HRESULT DestroyObjects();

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

#endif // !vkFRAME_H
