// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
//               2012 Jarmo Nikkanen
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
//       After rendering a frame, the ShowFrame() function filps or blits the
//       backbuffer contents to the primary. If surfaces are lost, they can be
//       restored with the RestoreSurfaces() function. Finally, if normal
//       Windows output is needed, the FlipToGDISurface() provides a GDI
//       surface to draw on.
// ============================================================================

#ifndef D3D9FRAME_H
#define D3D9FRAME_H

#include <d3d9.h>
#include <d3dx9.h>
#include "orbitersdk.h"
#include "D3D9Client.h"

class D3D9ClientSurface;

//-----------------------------------------------------------------------------
// Name: CD3DFramework9
// Desc: The Direct3D sample framework class for DX9. Maintains the D3D
//       surfaces and device used for 3D rendering.
//-----------------------------------------------------------------------------
class CD3DFramework9
{

private:

    // Internal variables for the framework class
    HWND                 hWnd;               // The window object
    BOOL                 bIsFullscreen;      // Fullscreen vs. windowed
    BOOL                 bVertexTexture;
    BOOL                 bAAEnabled;
    BOOL                 bNoVSync;           // don't use vertical sync in fullscreen
    BOOL                 Alpha;
    BOOL                 SWVert;
    BOOL                 Pure;
    BOOL                 DDM;
    BOOL                 bGDIBB;
    BOOL                 nvPerfHud;
    DWORD                dwRenderWidth;      // Dimensions of the render target
    DWORD                dwRenderHeight;
    DWORD                dwFSMode;
    LPDIRECT3D9          pD3D;               // The Direct3D object
    LPDIRECT3DDEVICE9    pd3dDevice;         // The D3D device
    LPD3DXFONT           pLargeFont;
    LPD3DXFONT           pSmallFont;
    DWORD                dwZBufferBitDepth;  // Bit depth of z-buffer
    DWORD                dwStencilBitDepth;  // Bit depth of stencil buffer (0 if none)
    DWORD                Adapter;
    DWORD                Mode;
    DWORD                MultiSample;
    LPDIRECT3DSURFACE9   pRenderTarget;
    LPDIRECT3DSURFACE9   pEnvDS;
    LPDIRECT3DSURFACE9   pShmDS;
    LPDIRECT3DTEXTURE9   pShmRT;
    D3D9ClientSurface *  pBackBuffer;
//  class D3D9Config  *  cfg;

    RECT                 rcScreenRect;       // Screen rect for window

    // Internal functions for the framework class

    HRESULT CreateFullscreenMode();
    HRESULT CreateWindowedMode();
    void    Clear();

public:

    D3DPRESENT_PARAMETERS   d3dPP;
    D3DCAPS9                caps;

    // Access functions for DirectX objects
    inline HWND                GetRenderWindow() const          { return hWnd; }
    inline LPDIRECT3D9         GetDirect3D() const              { return pD3D; }
    inline LPDIRECT3DDEVICE9   GetD3DDevice() const             { return pd3dDevice; }
    inline DWORD               GetRenderWidth() const           { return dwRenderWidth; }      // Dimensions of the render target
    inline DWORD               GetRenderHeight() const          { return dwRenderHeight; }     // Dimensions of the render target
    inline DWORD               GetZBufferBitDepth() const       { return dwZBufferBitDepth; }
    inline DWORD               GetStencilBitDepth() const       { return dwStencilBitDepth; }
    inline DWORD               GetWidth() const                 { return dwRenderWidth; }
    inline DWORD               GetHeight() const                { return dwRenderHeight; }
    inline const RECT          GetScreenRect() const            { return rcScreenRect; }
    inline LPDIRECT3DSURFACE9  GetBackBuffer() const            { return pRenderTarget; }
    inline LPDIRECT3DSURFACE9  GetEnvDepthStencil() const       { return pEnvDS; }
    inline LPDIRECT3DSURFACE9  GetShadowMapDepthStencil() const { return pShmDS; }
    inline LPDIRECT3DTEXTURE9  GetShadowMapRenderTarget() const { return pShmRT; }
    inline SURFHANDLE          GetBackBufferHandle() const      { return pBackBuffer; }
    inline LPD3DXFONT          GetLargeFont() const             { return pLargeFont; }
    inline LPD3DXFONT          GetSmallFont() const             { return pSmallFont; }
    inline BOOL                IsFullscreen() const             { return bIsFullscreen; }
    inline BOOL                IsGDIBB() const                  { return bGDIBB; }
    inline BOOL                IsAAEnabled() const              { return bAAEnabled; }
    inline const D3DCAPS9 *    GetCaps() const                  { return &caps; }
    inline BOOL                HasVertexTextureSup() const      { return bVertexTexture; }
    inline BOOL                GetVSync() const                 { return (bNoVSync==FALSE); }

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

#endif // !D3D9FRAME_H