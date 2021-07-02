//-----------------------------------------------------------------------------
// File: D3DFrame.h
//
// Desc: Class to manage the Direct3D environment objects such as buffers,
//       viewports, and 3D devices.
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
//
//
// Copyright (C) 1997 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------

//#define DIRECTINPUT_VERSION 0x0600
// use DirectX6 interface for DirectInput

#ifndef D3DFRAME_H
#define D3DFRAME_H

#include <ddraw.h>
#include <d3d.h>
#include <dinput.h>



//-----------------------------------------------------------------------------
// Name: CD3DFramework
// Desc: The Direct3D sample framework class. Maintains the D3D surfaces,
//       device, and viewport used for 3D rendering.
//-----------------------------------------------------------------------------
class CD3DFramework
{
    // Internal variables for the framework class
	HINSTANCE            m_hInst;             // The application instance
    HWND                 m_hWnd;              // The window object
    BOOL                 m_bIsFullscreen;     // Fullscreen vs. windowed
	BOOL                 m_bSupportsMipmaps;  // Device supports mipmapped textures?
    DWORD                m_dwRenderWidth;     // Dimensions of the render target
    DWORD                m_dwRenderHeight;
    RECT                 m_rcScreenRect;      // Screen rect for window
    RECT                 m_rcViewportRect;    // Offscreen rect for VPort
    LPDIRECTDRAWSURFACE4 m_pddsFrontBuffer;   // The primary surface
    LPDIRECTDRAWSURFACE4 m_pddsBackBuffer;    // The backbuffer surface
    LPDIRECTDRAWSURFACE4 m_pddsRenderTarget;  // The render target surface
    LPDIRECTDRAWSURFACE4 m_pddsZBuffer;       // The zbuffer surface
    LPDIRECT3DDEVICE3    m_pd3dDevice;        // The D3D device
    LPDIRECT3DVIEWPORT3  m_pvViewport;        // The D3D viewport
    LPDIRECTDRAW4        m_pDD;               // The DirectDraw object
    LPDIRECT3D3          m_pD3D;              // The Direct3D object
    D3DDEVICEDESC        m_ddDeviceDesc;
    DWORD                m_dwDeviceMemType;
    DDPIXELFORMAT        m_ddpfZBuffer;       // Enumerated zbuffer format
	LPDIRECTINPUT		 m_pDI;				  // DirectInput object
	LPDIRECTINPUTDEVICE	 m_pDIKeyboard;		  // DirectInput Keyboard device
	LPDIRECTINPUTDEVICE2 m_pDIJoystick;       // DirectInput Joystick device
	BOOL                 m_JoystickHasRudder; // flag for rudder control available
	BOOL                 m_bJoystickPresent;  // flag for joystick detected

    // Internal functions for the framework class
    HRESULT CreateViewport();
    HRESULT Create3DDevice( GUID* );
    HRESULT CreateZBuffer();
    HRESULT CreateBuffers( DDSURFACEDESC2*, DWORD );
    HRESULT CreateDirectDraw( GUID*, DWORD );
    HRESULT CreateDirect3D( GUID*, DWORD );
    HRESULT CreateEnvironment( GUID*, GUID*, DDSURFACEDESC2*, DWORD );

	HRESULT CreateDirectInput ();
	BOOL SetJoystickProperties (LPDIRECTINPUTDEVICE lpDev);

public:
    // Access functions for DirectX objects
	HWND                 GetWindow()         { return m_hWnd; }
    LPDIRECTDRAW4        GetDirectDraw()     { return m_pDD; }
    LPDIRECT3D3          GetDirect3D()       { return m_pD3D; }
    LPDIRECT3DDEVICE3    GetD3DDevice()      { return m_pd3dDevice; }
    LPDIRECT3DVIEWPORT3  GetViewport()       { return m_pvViewport; }
    LPDIRECTDRAWSURFACE4 GetFrontBuffer()    { return m_pddsFrontBuffer; }
    LPDIRECTDRAWSURFACE4 GetBackBuffer()     { return m_pddsBackBuffer; }
    LPDIRECTDRAWSURFACE4 GetRenderSurface()  { return m_pddsRenderTarget; }
	LPDIRECTINPUTDEVICE  GetKeyboard()       { return m_pDIKeyboard; }
	LPDIRECTINPUTDEVICE2 GetJoystick()       { return m_pDIJoystick; }

    // Functions to aid rendering
    HRESULT RestoreSurfaces();
    HRESULT ShowFrame();
    HRESULT FlipToGDISurface( BOOL bDrawFrame );

	// Device caps
	BOOL    SupportsMipmaps() const         { return m_bSupportsMipmaps; }
	DWORD   DeviceMemType() const           { return m_dwDeviceMemType; }

    // Functions for managing screen and viewport bounds
    BOOL    IsFullscreen() const            { return m_bIsFullscreen; }
    RECT*   GetViewportRect()               { return &m_rcViewportRect; }
	RECT*   GetScreenRect()                 { return &m_rcScreenRect; }
    VOID    Move( INT x, INT y );

    // Functions to support sprite-based rendering
    HRESULT ChangeRenderTarget( LPDIRECTDRAWSURFACE4 pddsNewTarget );

    // Creates the Framework
    HRESULT Initialize (HINSTANCE hInst, HWND hWnd, GUID *pDriverGUID,
		GUID *pDeviceGUID, DDSURFACEDESC2* pddsd, DWORD dwFlags );
    HRESULT DestroyObjects();

    CD3DFramework();
    ~CD3DFramework();

	BOOL    JoystickPresent() const { return m_bJoystickPresent; }
	BOOL    JoystickHasRudder() const { return m_JoystickHasRudder; }
	HRESULT GetJoystickStatus (DIJOYSTATE2 *js);

	friend BOOL FAR PASCAL DIEnumJoystickCallback (LPCDIDEVICEINSTANCE lpdinst, LPVOID pvRef);
};




//-----------------------------------------------------------------------------
// Flags used for the Initialize() method of a CD3DFramework object
//-----------------------------------------------------------------------------
#define D3DFW_FULLSCREEN    0x00000001 // Use fullscreen mode
#define D3DFW_BACKBUFFER    0x00000002 // Create and use a backbuffer
#define D3DFW_ZBUFFER       0x00000004 // Create and use a zbuffer
#define D3DFW_STENCILBUFFER 0x00000008 // Use a z-buffer w/stenciling
#define D3DFW_NO_FPUSETUP   0x00000010 // Don't use default DDSCL_FPUSETUP flag



//-----------------------------------------------------------------------------
// Errors that the Initialize() and ChangeDriver() calls may return
//-----------------------------------------------------------------------------
#define D3DFWERR_INITIALIZATIONFAILED 0x82000000
#define D3DFWERR_NODIRECTDRAW         0x82000001
#define D3DFWERR_COULDNTSETCOOPLEVEL  0x82000002
#define D3DFWERR_NODIRECT3D           0x82000003
#define D3DFWERR_NO3DDEVICE           0x82000004
#define D3DFWERR_NOZBUFFER            0x82000005
#define D3DFWERR_NOVIEWPORT           0x82000006
#define D3DFWERR_NOPRIMARY            0x82000007
#define D3DFWERR_NOCLIPPER            0x82000008
#define D3DFWERR_BADDISPLAYMODE       0x82000009
#define D3DFWERR_NOBACKBUFFER         0x8200000a
#define D3DFWERR_NONZEROREFCOUNT      0x8200000b
#define D3DFWERR_NORENDERTARGET       0x8200000c
#define D3DFWERR_INVALIDMODE          0x8200000d
#define D3DFWERR_NOTINITIALIZED       0x8200000e
#define D3DFWERR_NODIRECTINPUT        0x8200000f

#endif // D3DFRAME_H

