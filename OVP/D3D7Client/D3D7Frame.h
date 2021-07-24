// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// D3d7frame.h
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
// ==============================================================

#ifndef D3D7FRAME_H
#define D3D7FRAME_H
#include <ddraw.h>
#include <d3d.h>

//-----------------------------------------------------------------------------
// Name: CD3DFramework7
// Desc: The Direct3D sample framework class for DX7. Maintains the D3D
//       surfaces and device used for 3D rendering.
//-----------------------------------------------------------------------------
class CD3DFramework7
{
    // Internal variables for the framework class
    HWND                 m_hWnd;               // The window object
    BOOL                 m_bIsFullscreen;      // Fullscreen vs. windowed
	BOOL				 m_bIsTLDevice;        // device supports hardware transform & lighting
    BOOL                 m_bIsStereo;          // Stereo view mode
	BOOL				 m_bNoVSync;           // don't use vertical sync in fullscreen
	BOOL                 m_bPageflip;          // allow page flipping in fullscreen
	BOOL                 m_bTryStencil;        // try to find a device that supports stencil buffers
	BOOL                 m_bSupportsMipmaps;
    DWORD                m_dwRenderWidth;      // Dimensions of the render target
    DWORD                m_dwRenderHeight;
    RECT                 m_rcScreenRect;       // Screen rect for window
    LPDIRECTDRAW7        m_pDD;                // The DirectDraw object
    LPDIRECT3D7          m_pD3D;               // The Direct3D object
    LPDIRECT3DDEVICE7    m_pd3dDevice;         // The D3D device
    LPDIRECTDRAWSURFACE7 m_pddsFrontBuffer;    // The primary surface
    LPDIRECTDRAWSURFACE7 m_pddsBackBuffer;     // The backbuffer surface
    LPDIRECTDRAWSURFACE7 m_pddsBackBufferLeft; // For stereo modes
    LPDIRECTDRAWSURFACE7 m_pddsZBuffer;        // The zbuffer surface
    DWORD                m_dwDeviceMemType;
	DDPIXELFORMAT		 m_ddpfBackBufferPixelFormat;
	DWORD				 m_dwZBufferBitDepth;  // Bit depth of z-buffer
	DWORD                m_dwStencilBitDepth;  // Bit depth of stencil buffer (0 if none)
	DWORD				 m_dwMaxLights;        // max number of light sources supported
	DWORD                m_ddFlipFlag;

    // Internal functions for the framework class
    HRESULT CreateZBuffer (GUID*);
    HRESULT CreateFullscreenBuffers (DDSURFACEDESC2*);
    HRESULT CreateWindowedBuffers ();
    HRESULT CreateDirectDraw (GUID*, DWORD);
    HRESULT CreateDirect3D (GUID*);
    HRESULT CreateEnvironment (GUID*, GUID*, DDSURFACEDESC2*, DWORD);

public:
    // Access functions for DirectX objects
    inline LPDIRECTDRAW7        GetDirectDraw() const        { return m_pDD; }
    inline LPDIRECT3D7          GetDirect3D() const          { return m_pD3D; }
    inline LPDIRECT3DDEVICE7    GetD3DDevice() const         { return m_pd3dDevice; }
    inline LPDIRECTDRAWSURFACE7 GetFrontBuffer() const       { return m_pddsFrontBuffer; }
    inline LPDIRECTDRAWSURFACE7 GetBackBuffer() const        { return m_pddsBackBuffer; }
    inline LPDIRECTDRAWSURFACE7 GetRenderSurface() const     { return m_pddsBackBuffer; }
    inline LPDIRECTDRAWSURFACE7 GetRenderSurfaceLeft() const { return m_pddsBackBufferLeft; }
    inline DWORD                GetRenderWidth() const       { return m_dwRenderWidth; }      // Dimensions of the render target
    inline DWORD                GetRenderHeight() const	     { return m_dwRenderHeight; }      // Dimensions of the render target
	inline DWORD                GetDeviceMemType() const     { return m_dwDeviceMemType; }
	inline DWORD                GetZBufferBitDepth() const   { return m_dwZBufferBitDepth; }
	inline DWORD                GetStencilBitDepth() const   { return m_dwStencilBitDepth; }
	inline DWORD				GetMaxLights() const         { return m_dwMaxLights; }
	inline BOOL                 SupportsMipmaps() const      { return m_bSupportsMipmaps; }
	inline const RECT          &GetScreenRect() const        { return m_rcScreenRect; }
	DWORD                       RequireTexPow2() const;

    // Functions to aid rendering
    HRESULT RestoreSurfaces();
    HRESULT ShowFrame();
    HRESULT FlipToGDISurface( BOOL bDrawFrame = FALSE );

    // Functions for managing screen and viewport bounds
    inline BOOL    IsFullscreen() const         { return m_bIsFullscreen; }
	inline BOOL    IsTLDevice() const           { return m_bIsTLDevice; }
    inline BOOL    IsStereo() const             { return m_bIsStereo; }
    VOID    Move( INT x, INT y );

    // Creates the Framework
    HRESULT Initialize( HWND hWnd, GUID* pDriverGUID, GUID* pDeviceGUID,
                        DDSURFACEDESC2* pddsd, DWORD dwFlags );
    HRESULT DestroyObjects();

            CD3DFramework7();
           ~CD3DFramework7();
};

//-----------------------------------------------------------------------------
// Flags used for the Initialize() method of a CD3DFramework object
//-----------------------------------------------------------------------------
#define D3DFW_FULLSCREEN    0x00000001 // Use fullscreen mode
#define D3DFW_STEREO        0x00000002 // Use stereo-scopic viewing
#define D3DFW_ZBUFFER       0x00000004 // Create and use a zbuffer
#define D3DFW_NO_FPUSETUP   0x00000008 // Don't use default DDSCL_FPUSETUP flag
#define D3DFW_NOVSYNC		0x00000010 // Don't use vertical sync in fullscreen
#define D3DFW_PAGEFLIP      0x00000020 // Allow page flipping in fullscreen
#define D3DFW_TRYSTENCIL    0x00000040 // Try for stencil buffer support

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

#endif // !D3D7FRAME_H