//-----------------------------------------------------------------------------
// File: D3DFrame.cpp
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
// Copyright (c) 1995-1998 by Microsoft, all rights reserved
//-----------------------------------------------------------------------------

#define STRICT
#include <windows.h>
#include "D3DFrame.h"
#include "D3DUtil.h"
#include "Config.h"
#include "Log.h"

#include "D3d7app.h"
extern CD3DApplication App;

extern DWORD requestZDepth;

//-----------------------------------------------------------------------------
// Name: EnumZBufferFormatsCallback()
// Desc: Enumeration function to report valid pixel formats for z-buffers.
//-----------------------------------------------------------------------------
static HRESULT WINAPI EnumZBufferFormatsCallback (DDPIXELFORMAT* pddpf,
                                                  VOID* data)
{
	DDPIXELFORMAT *pddpfStore = (DDPIXELFORMAT*)data;

    if (NULL == pddpf || NULL == pddpfStore)
        return D3DENUMRET_CANCEL;

	// Store this pixel format if the z-buffer depth is higher than what we
	// have so far. If the depth is less than requested keep going.
    if (pddpf->dwFlags == pddpfStore->dwFlags &&
		pddpf->dwZBufferBitDepth > pddpfStore->dwZBufferBitDepth) {
        memcpy (pddpfStore, pddpf, sizeof(DDPIXELFORMAT));
		if (pddpf->dwZBufferBitDepth >= requestZDepth) return D3DENUMRET_CANCEL;
    }
    return D3DENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: CD3DFramework()
// Desc: The constructor. Clears static variables
//-----------------------------------------------------------------------------
CD3DFramework::CD3DFramework ()
{
	m_hInst            = NULL;
    m_hWnd             = NULL;
    m_bIsFullscreen    = FALSE;
	m_bSupportsMipmaps = FALSE;
    m_dwRenderWidth    = 0L;
    m_dwRenderHeight   = 0L;
    m_pddsFrontBuffer  = NULL;
    m_pddsBackBuffer   = NULL;
    m_pddsRenderTarget = NULL;
    m_pddsZBuffer      = NULL;
    m_pd3dDevice       = NULL;
    m_pvViewport       = NULL;
    m_pDD              = NULL;
    m_pD3D             = NULL;
    m_dwDeviceMemType  = NULL;
	m_pDI              = NULL;
	m_pDIKeyboard      = NULL;
	m_pDIJoystick      = NULL;
}




//-----------------------------------------------------------------------------
// Name: ~CD3DFramework()
// Desc: The destructor. Deletes all objects
//-----------------------------------------------------------------------------
CD3DFramework::~CD3DFramework()
{
    DestroyObjects();
}




//-----------------------------------------------------------------------------
// Name: DestroyObjects()
// Desc: Cleans everything up upon deletion. This code returns an error
//       if any of the objects have remaining reference counts.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::DestroyObjects()
{
    LONG nDD  = 0L; // Number of outstanding DDraw references
    LONG nD3D = 0L; // Number of outstanding D3DDevice references
	LONG nDI  = 0L; // Number of outstanding DInput references

    SAFE_RELEASE( m_pvViewport );

	// Destroy the DirectInput object and devices
	if (m_pDI) {
		if (m_pDIKeyboard) {
			m_pDIKeyboard->Unacquire ();
			if (m_pDIKeyboard->Release () > 0)
				DEBUG_MSG (TEXT ("Error: Keyboard device is still referenced!"));
			m_pDIKeyboard = NULL;
		}
		if (0 < (nDI = m_pDI->Release()))
			DEBUG_MSG (TEXT ("Error: DInput object is still referenced!"));
		m_pDI = NULL;
	}

    // Do a safe check for releasing the D3DDEVICE. RefCount must be zero.
    if( m_pd3dDevice )
        if( 0 < ( nD3D = m_pd3dDevice->Release() ) )
            DEBUG_MSG( TEXT("Error: D3DDevice object is still referenced!") );
	m_pd3dDevice = NULL;

    // In windowed mode, release the explicity created backbuffer.
    if( FALSE == m_bIsFullscreen )
        SAFE_RELEASE( m_pddsBackBuffer );
    SAFE_RELEASE( m_pddsRenderTarget ); //Note: release before frontbuffer
    SAFE_RELEASE( m_pddsZBuffer );
    SAFE_RELEASE( m_pddsFrontBuffer );
    SAFE_RELEASE( m_pD3D );

    // Do a safe check for releasing DDRAW. RefCount must be zero.
    if( m_pDD )
	{
	    m_pDD->SetCooperativeLevel( m_hWnd, DDSCL_NORMAL );

        if( 0 < ( nDD = m_pDD->Release() ) )
            DEBUG_MSG( TEXT("Error: DDraw object is still referenced!") );
	}
	m_pDD = NULL;

    // Return successful, unless there are outstanding DD or D3DDevice refs.
    return ( nDD==0 && nD3D==0 && nDI==0) ? S_OK : D3DFWERR_NONZEROREFCOUNT;
}




//-----------------------------------------------------------------------------
// Name: Initialize()
// Desc: Creates the internal objects for the framework
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::Initialize( HINSTANCE hInst, HWND hWnd, GUID* pDriverGUID, 
                                   GUID* pDeviceGUID, DDSURFACEDESC2* pMode,
								   DWORD dwFlags )
{
	HRESULT hr;

    // Check params. A NULL mode is valid for windowed modes only. A NULL 
	// device GUID is legal for apps that only want 2D support.
    if( NULL==hInst || NULL==hWnd || ( NULL==pMode && (dwFlags&D3DFW_FULLSCREEN) ) )
        return E_INVALIDARG;

    // Setup state for windowed/fullscreen mode
	m_hInst         = hInst;
    m_hWnd          = hWnd;
    m_bIsFullscreen = ( dwFlags & D3DFW_FULLSCREEN ) ? TRUE : FALSE;

    // Create the D3D rendering environment (surfaces, device, viewport, etc.)
    if( FAILED( hr = CreateEnvironment( pDriverGUID, pDeviceGUID, pMode,
		                                dwFlags ) ) )
	{
		LOGOUT("CreateEnvironment: failed");
		DestroyObjects();
		if( E_FAIL == hr )
			hr = D3DFWERR_INITIALIZATIONFAILED;
	}
	return hr;
}

    


//-----------------------------------------------------------------------------
// Name: CreateEnvironment()
// Desc: Creates the internal objects for the framework
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::CreateEnvironment( GUID* pDriverGUID, GUID* pDeviceGUID,
										  DDSURFACEDESC2* pMode, DWORD dwFlags )
{
    HRESULT hr;

	// Create the DDraw object
	if( FAILED( hr = CreateDirectDraw( pDriverGUID, dwFlags ) ) ) {
		LOGOUT("CreateDirectDraw: failed");
		return hr;
	}

	// Create the Direct3D object
	if( pDeviceGUID )
		if( FAILED( hr = CreateDirect3D( pDeviceGUID, dwFlags ) ) ) {
			LOGOUT("CreateDirect3D: failed");
			return hr;
		}

	// Create the front and back buffers, and attach a clipper
	if( FAILED( hr = CreateBuffers( pMode, dwFlags ) ) ) {
		LOGOUT("CreateBuffers: failed");
		return hr;
	}

	// If there is no device GUID, then the app only wants 2D, so we're done
	if( NULL == pDeviceGUID )
		return S_OK;

	// Create and attach the zbuffer
	if( dwFlags & D3DFW_ZBUFFER )
		if( FAILED( hr = CreateZBuffer() ) ) {
			LOGOUT("CreateZBuffer: failed");
			return hr;
		}
	
	// Query the render buffer for the 3ddevice
	if( FAILED( hr = Create3DDevice( pDeviceGUID ) ) ) {
		LOGOUT("Create3DDevice: failed");
		return hr;
	}

	// Create and set the viewport
	if( FAILED( hr = CreateViewport() ) ) {
		LOGOUT("CreateViewport: failed");
		return hr;
	}

	// Create DirectInput devices
	if (FAILED (hr = CreateDirectInput ())) {
		LOGOUT("CreateDirectInput: failed");
		return hr;
	}

	return S_OK;
}

    


//-----------------------------------------------------------------------------
// Name: CreateDirectDraw()
// Desc: Create the DirectDraw interface
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::CreateDirectDraw( GUID* pDriverGUID, DWORD dwFlags )
{
    // Create the DirectDraw interface, and query for the DD4 interface
    LPDIRECTDRAW pDD;
    if( FAILED( DirectDrawCreate( pDriverGUID, &pDD, NULL ) ) )
    {
        DEBUG_MSG( TEXT("Could not create DirectDraw") );
        return D3DFWERR_NODIRECTDRAW;
    }

    if( FAILED( pDD->QueryInterface( IID_IDirectDraw4, (VOID**)&m_pDD ) ) )
    {
        pDD->Release();
        DEBUG_MSG( TEXT("Couldn't query for DirectDraw4") );
        return D3DFWERR_NODIRECTDRAW;
    }
    pDD->Release();

    // Set the Windows cooperative level
    DWORD dwCoopFlags = DDSCL_NORMAL;
    if( m_bIsFullscreen )
        dwCoopFlags = DDSCL_ALLOWREBOOT|DDSCL_EXCLUSIVE|DDSCL_FULLSCREEN;

    // By defualt, set the flag to allow D3D to optimize floating point calcs
    // if( 0L == ( dwFlags & D3DFW_NO_FPUSETUP ) )
    //     dwCoopFlags |= DDSCL_FPUSETUP;
	// *** MS: This forces FPU to single precision even when working
	//     with double precision variables! Definitely not what I need ...

    if( FAILED( m_pDD->SetCooperativeLevel( m_hWnd, dwCoopFlags ) ) )
    {
        DEBUG_MSG( TEXT("Couldn't set coop level") );
        return D3DFWERR_COULDNTSETCOOPLEVEL;
    }

	return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateDirect3D()
// Desc: Create the Direct3D interface
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::CreateDirect3D( GUID* pDeviceGUID, DWORD dwFlags )
{
    // Query DirectDraw for access to Direct3D
    if( FAILED( m_pDD->QueryInterface( IID_IDirect3D3, (VOID**)&m_pD3D ) ) )
    {
        LOGOUT("Couldn't query the Direct3D interface");
        return D3DFWERR_NODIRECT3D;
    }

    // Use the FindDevice() method to test if the requested device
    // exists, and if so, take note of what memory type it takes.
    D3DFINDDEVICERESULT  devResult;
    D3DFINDDEVICESEARCH  devSearch;
    ZeroMemory( &devResult, sizeof(D3DFINDDEVICERESULT) );
    ZeroMemory( &devSearch, sizeof(D3DFINDDEVICESEARCH) );
    devResult.dwSize  = sizeof(D3DFINDDEVICERESULT);
    devSearch.dwSize  = sizeof(D3DFINDDEVICESEARCH);
    devSearch.dwFlags = D3DFDS_GUID;
    CopyMemory( &devSearch.guid, pDeviceGUID, sizeof(GUID) );

    if( FAILED( m_pD3D->FindDevice( &devSearch, &devResult ) ) )
    {
        DEBUG_MSG( TEXT("Couldn't find the specified device") );
        return D3DFWERR_NODIRECT3D;
    }

    // Whether device is SW or HW, get the devicedesc, and the defualt memtype
    if( 0L == devResult.ddHwDesc.dwFlags )
    {
        m_dwDeviceMemType = DDSCAPS_SYSTEMMEMORY;
        memcpy( &m_ddDeviceDesc, &devResult.ddSwDesc, sizeof(D3DDEVICEDESC) );
    }
    else
    {
        m_dwDeviceMemType = DDSCAPS_VIDEOMEMORY;
        memcpy( &m_ddDeviceDesc, &devResult.ddHwDesc, sizeof(D3DDEVICEDESC) );
    }

    // Using the device GUID, let's enumerate a format for our z-buffer, in
    // case we later decide to create one.
	ZeroMemory( &m_ddpfZBuffer, sizeof(DDPIXELFORMAT) );

    if( dwFlags & D3DFW_STENCILBUFFER )
        m_ddpfZBuffer.dwFlags = DDPF_ZBUFFER | DDPF_STENCILBUFFER;
    else
        m_ddpfZBuffer.dwFlags = DDPF_ZBUFFER;
	m_ddpfZBuffer.dwZBufferBitDepth = 0; // "undefined"

    // Get an appropiate pixel format from enumeration of the formats.
    m_pD3D->EnumZBufferFormats( *pDeviceGUID, EnumZBufferFormatsCallback,
                                (VOID*)&m_ddpfZBuffer );

    if( sizeof(DDPIXELFORMAT) != m_ddpfZBuffer.dwSize )
    {
        DEBUG_MSG( TEXT("Device doesn't support requested zbuffer format") );
        return D3DFWERR_NOZBUFFER;
    } else {
		LOGOUT1P("z-buffer bit depth requested: %d", requestZDepth);
		LOGOUT1P("z-buffer bit depth used:      %d", m_ddpfZBuffer.dwZBufferBitDepth);
	}

    return S_OK;
}   
    

    
    
//-----------------------------------------------------------------------------
// Name: CreateBuffers()
// Desc: Creates the primary and (optional) backbuffer for rendering.
//       Windowed mode and fullscreen mode are handled differently.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::CreateBuffers( DDSURFACEDESC2* pddsd, DWORD dwFlags )
{
    HRESULT hr;

	if( dwFlags & D3DFW_FULLSCREEN )
	{
		// Get the dimensions of the viewport and screen bounds
		// Store the rectangle which contains the renderer
		SetRect( &m_rcViewportRect, 0, 0, pddsd->dwWidth, pddsd->dwHeight );
		memcpy( &m_rcScreenRect, &m_rcViewportRect, sizeof(RECT) );
		m_dwRenderWidth  = m_rcViewportRect.right;
		m_dwRenderHeight = m_rcViewportRect.bottom;
    
		// Set the display mode to the requested dimensions. Check for
		// 320x200x8 modes, and set flag to avoid using ModeX
		DWORD dwModeFlags = 0;

		if( (320==m_dwRenderWidth) && (200==m_dwRenderHeight) && 
			(8==pddsd->ddpfPixelFormat.dwRGBBitCount) )
			dwModeFlags |= DDSDM_STANDARDVGAMODE;

		if( FAILED( m_pDD->SetDisplayMode( m_dwRenderWidth, m_dwRenderHeight,
									pddsd->ddpfPixelFormat.dwRGBBitCount,
									pddsd->dwRefreshRate, dwModeFlags ) ) )
		{
			DEBUG_MSG( TEXT("Can't set display mode") );
			return D3DFWERR_BADDISPLAYMODE;
		}

		// Create the primary surface
		DDSURFACEDESC2 ddsd;
		D3DUtil_InitSurfaceDesc( ddsd, DDSD_CAPS );
		ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE | DDSCAPS_3DDEVICE;

		// With no backbuffer, the primary becomes the render target
		if( dwFlags & D3DFW_BACKBUFFER )
		{
			ddsd.dwFlags          |= DDSD_BACKBUFFERCOUNT;
			ddsd.ddsCaps.dwCaps   |= DDSCAPS_FLIP | DDSCAPS_COMPLEX;
			ddsd.dwBackBufferCount = 1;
		}

		if( FAILED( hr = m_pDD->CreateSurface( &ddsd, &m_pddsFrontBuffer, NULL ) ) )
		{
			DEBUG_MSG( TEXT("Error: Can't create primary surface") );
			if( hr != DDERR_OUTOFVIDEOMEMORY )
				return D3DFWERR_NOPRIMARY;
			DEBUG_MSG( TEXT("Error: Out of video memory") );
			return DDERR_OUTOFVIDEOMEMORY;
		}

		// Get the backbuffer. For fullscreen mode, the backbuffer was created
		// along with the primary, but windowed mode still needs to create one.
		if( dwFlags & D3DFW_BACKBUFFER )
		{
			// Get a ptr to the back buffer, which will be our render target
			DDSCAPS2 ddscaps;
			ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
			if( FAILED( hr = m_pddsFrontBuffer->GetAttachedSurface( &ddscaps, 
														&m_pddsBackBuffer ) ) )
			{
				DEBUG_MSG( TEXT("Error: Can't get/create the backbuffer") );
				if( hr != DDERR_OUTOFVIDEOMEMORY )
					return D3DFWERR_NOBACKBUFFER;
				DEBUG_MSG( TEXT("Error: Out of video memory") );
				return DDERR_OUTOFVIDEOMEMORY;
			}
		}
	}
	else // Set up buffers for windowed rendering
	{
		// Get the dimensions of the viewport and screen bounds
		GetClientRect( m_hWnd, &m_rcViewportRect );
		GetClientRect( m_hWnd, &m_rcScreenRect );
		ClientToScreen( m_hWnd, (POINT*)&m_rcScreenRect.left );
		ClientToScreen( m_hWnd, (POINT*)&m_rcScreenRect.right );
		m_dwRenderWidth  = m_rcViewportRect.right;
		m_dwRenderHeight = m_rcViewportRect.bottom;
    
		// Create the primary surface
		DDSURFACEDESC2 ddsd;
		D3DUtil_InitSurfaceDesc( ddsd, DDSD_CAPS );
		ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;

		// With no backbuffer, the primary becomes the render target
		if( 0L == ( dwFlags & D3DFW_BACKBUFFER ) )
			ddsd.ddsCaps.dwCaps |= DDSCAPS_3DDEVICE;
    
		if( FAILED( hr = m_pDD->CreateSurface( &ddsd, &m_pddsFrontBuffer, NULL ) ) )
		{
			DEBUG_MSG( TEXT("Error: Can't create primary surface") );
			if( hr != DDERR_OUTOFVIDEOMEMORY )
				return D3DFWERR_NOPRIMARY;
			DEBUG_MSG( TEXT("Error: Out of video memory") );
			return DDERR_OUTOFVIDEOMEMORY;
		}

		// If in windowed-mode, create a clipper object
		LPDIRECTDRAWCLIPPER pcClipper;
		if( FAILED( hr = m_pDD->CreateClipper( 0, &pcClipper, NULL ) ) )
		{
			DEBUG_MSG( TEXT("Error: Couldn't create clipper") );
			return D3DFWERR_NOCLIPPER;
		}

		// Associate the clipper with the window
		pcClipper->SetHWnd( 0, m_hWnd );
		m_pddsFrontBuffer->SetClipper( pcClipper );
		SAFE_RELEASE( pcClipper );

		// Get the backbuffer. For fullscreen mode, the backbuffer was created
		// along with the primary, but windowed mode still needs to create one.
		if( dwFlags & D3DFW_BACKBUFFER )
		{
			// Create the back buffer (the render target)   
			ddsd.dwFlags        = DDSD_WIDTH | DDSD_HEIGHT | DDSD_CAPS;
			ddsd.dwWidth        = m_dwRenderWidth;
			ddsd.dwHeight       = m_dwRenderHeight;
			ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN | DDSCAPS_3DDEVICE;

			if( FAILED( hr = m_pDD->CreateSurface( &ddsd, &m_pddsBackBuffer, NULL ) ) )
			{
				DEBUG_MSG( TEXT("Error: Couldn't create the backbuffer") );
				if( hr != DDERR_OUTOFVIDEOMEMORY )
					return D3DFWERR_NOBACKBUFFER;
				DEBUG_MSG( TEXT("Error: Out of video memory") );
				return DDERR_OUTOFVIDEOMEMORY;
			}
		}
		else // For rendering without a backbuffer
		{
			ClientToScreen( m_hWnd, (POINT*)&m_rcViewportRect.left );
			ClientToScreen( m_hWnd, (POINT*)&m_rcViewportRect.right );
		}
	}

	// Set up backbuffer ptr and ref counts
	if( dwFlags & D3DFW_BACKBUFFER )
		m_pddsRenderTarget = m_pddsBackBuffer;
	else
		m_pddsRenderTarget = m_pddsFrontBuffer;
	m_pddsRenderTarget->AddRef();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateZBuffer()
// Desc: Internal function called by Create() to make and attach a zbuffer
//       to the renderer
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::CreateZBuffer()
{
	HRESULT hr;

    // Check if the device supports z-bufferless hidden surface removal. If so,
    // we don't really need a z-buffer
    DWORD dwRasterCaps = m_ddDeviceDesc.dpcTriCaps.dwRasterCaps;
    if( dwRasterCaps & D3DPRASTERCAPS_ZBUFFERLESSHSR )
        return S_OK;

    // Get z-buffer dimensions from the render target
    // Setup the surface desc for the z-buffer.
    DDSURFACEDESC2 ddsd;
    D3DUtil_InitSurfaceDesc( ddsd, DDSD_WIDTH | DDSD_HEIGHT | DDSD_CAPS |
                             DDSD_PIXELFORMAT );
    ddsd.ddsCaps.dwCaps = DDSCAPS_ZBUFFER | m_dwDeviceMemType;
    ddsd.dwWidth        = m_dwRenderWidth;
    ddsd.dwHeight       = m_dwRenderHeight;
    memcpy( &ddsd.ddpfPixelFormat, &m_ddpfZBuffer, sizeof(DDPIXELFORMAT) );

    // Create and attach a z-buffer
    if( FAILED( hr = m_pDD->CreateSurface( &ddsd, &m_pddsZBuffer, NULL ) ) )
    {
        DEBUG_MSG( TEXT("Error: Couldn't create a ZBuffer surface") );
		if( hr != DDERR_OUTOFVIDEOMEMORY )
			return D3DFWERR_NOZBUFFER;
		DEBUG_MSG( TEXT("Error: Out of video memory") );
		return DDERR_OUTOFVIDEOMEMORY;
    }

    if( FAILED( m_pddsRenderTarget->AddAttachedSurface( m_pddsZBuffer ) ) )
    {
        DEBUG_MSG( TEXT("Error: Couldn't attach zbuffer to render surface") );
        return D3DFWERR_NOZBUFFER;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: Create3DDevice()
// Desc: Creates the 3D device for the render target
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::Create3DDevice( GUID* pDeviceGUID )
{
	// Check that we are NOT in a palettized display. That case will fail,
	// since the framework doesn't use palettes.
	DDSURFACEDESC2 ddsd;
	ddsd.dwSize = sizeof(DDSURFACEDESC2);
	m_pDD->GetDisplayMode( &ddsd );
	if( ddsd.ddpfPixelFormat.dwRGBBitCount <= 8 )
		return D3DFWERR_INVALIDMODE;

	// Create the device
    if( FAILED( m_pD3D->CreateDevice( *pDeviceGUID, m_pddsRenderTarget,
                                      &m_pd3dDevice, NULL ) ) )
    {
        DEBUG_MSG( TEXT("Couldn't create the D3DDevice") );
        return D3DFWERR_NO3DDEVICE;
    }

	// The following code checks whether the device supports mipmapped textures
    D3DDEVICEDESC d3dHALDesc;
    D3DDEVICEDESC d3dHELDesc;
    LPD3DPRIMCAPS pdpcTriCaps;

    ZeroMemory (&d3dHALDesc, sizeof(D3DDEVICEDESC));
    ZeroMemory (&d3dHELDesc, sizeof(D3DDEVICEDESC));

    d3dHALDesc.dwSize = sizeof (D3DDEVICEDESC);
    d3dHELDesc.dwSize = sizeof (D3DDEVICEDESC);

    m_pd3dDevice->GetCaps (&d3dHALDesc, &d3dHELDesc);

    // get triangle caps from hardware or software description 
    if (0 != d3dHALDesc.dwFlags) pdpcTriCaps = &d3dHALDesc.dpcTriCaps;
    else                         pdpcTriCaps = &d3dHELDesc.dpcTriCaps;

	// check if device supports mipmaping 
	m_bSupportsMipmaps = 
		((pdpcTriCaps->dwTextureFilterCaps & D3DPTFILTERCAPS_MIPNEAREST | 
		  pdpcTriCaps->dwTextureFilterCaps & D3DPTFILTERCAPS_MIPLINEAR) ? TRUE : FALSE);

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateViewport()
// Desc: Create the D3D viewport used by the renderer.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::CreateViewport()
{
    // Set up the viewport data parameters
    HRESULT      hr;
    D3DVIEWPORT2 vdData;
    D3DUtil_InitViewport( vdData, m_dwRenderWidth, m_dwRenderHeight );

    // Create the viewport
    if( FAILED( m_pD3D->CreateViewport( &m_pvViewport, NULL ) ) )
    {
        DEBUG_MSG( TEXT("Error: Couldn't create a viewport") );
        return D3DFWERR_NOVIEWPORT;
    }

    // Associate the viewport with the D3DDEVICE object
    if( FAILED( hr = m_pd3dDevice->AddViewport( m_pvViewport ) ) )
    {
        DEBUG_MSG( TEXT("Error: Couldn't add the viewport") );
        return D3DFWERR_NOVIEWPORT;
    }

    // Set the parameters to the new viewport
    if( FAILED( m_pvViewport->SetViewport2( &vdData ) ) )
    {
        DEBUG_MSG( TEXT("Error: Couldn't set the viewport data") );
        return D3DFWERR_NOVIEWPORT;
    }

    // Finally, set the current viewport for the current device
    if( FAILED( m_pd3dDevice->SetCurrentViewport( m_pvViewport ) ) )
    {
        DEBUG_MSG( TEXT("Error: Couldn't set current viewport to device") );
        return D3DFWERR_NOVIEWPORT;
    }

    return S_OK;
}

// =======================================================================
// Name: CreateDirectInput()
// Desc: Enumerate and create input devices for keyboard, mouse and joystick

HRESULT CD3DFramework::CreateDirectInput ()
{
	HRESULT hr;

	// Create the DirectInput object
	if (FAILED (hr = DirectInputCreate (m_hInst, DIRECTINPUT_VERSION, &m_pDI, NULL))) {
		LOGOUT("DirectInputCreate: failed");
		LOGOUT(DI_ErrorMsg (hr));
		return D3DFWERR_NODIRECTINPUT;
	}
	// Create the keyboard device
	if (FAILED (m_pDI->CreateDevice (GUID_SysKeyboard, &m_pDIKeyboard, NULL))) {
		LOGOUT("CreateDevice_Keyboard: failed");
		return D3DFWERR_NODIRECTINPUT;
	}
	if (FAILED (m_pDIKeyboard->SetDataFormat (&c_dfDIKeyboard))) {
		LOGOUT("SetDataFormat_Keyboard: failed");
		return D3DFWERR_NODIRECTINPUT;
	}
    DWORD dwCoopFlags = DISCL_FOREGROUND | DISCL_NONEXCLUSIVE; // maybe need to change for fullscreen
	if (FAILED (m_pDIKeyboard->SetCooperativeLevel (m_hWnd, dwCoopFlags))) {
		LOGOUT ("SetCooperativeLevel_Keyboard: failed");
		return D3DFWERR_NODIRECTINPUT;
	}
	if (FAILED (m_pDIKeyboard->Acquire ())) {
		LOGOUT("Acquire_Keyboard: failed");
		return D3DFWERR_NODIRECTINPUT;
	}

	// enumerate joysticks
	m_pDI->EnumDevices (DIDEVTYPE_JOYSTICK, DIEnumJoystickCallback, this, DIEDFL_ATTACHEDONLY);
	if (!m_pDIJoystick || !SetJoystickProperties (m_pDIJoystick) ||
		m_pDIJoystick->Acquire() != DI_OK) {
		m_bJoystickPresent = false;
		LOGOUT("No joystick detected");
	} else {
		// store some joystick capabilities
		DIDEVCAPS didevcaps;
	    ZeroMemory (&didevcaps, sizeof (DIDEVCAPS));
		didevcaps.dwSize = sizeof (DIDEVCAPS);
		m_pDIJoystick->GetCapabilities (&didevcaps);
		// retrieve data from didevcaps ...
		m_bJoystickPresent = true;
		LOGOUT("Joystick registered");
	}

	return S_OK;
}

// =====================================================================================
// DIEnumJoystickCallback()
// DirectInput joystick enumeration callback function

BOOL FAR PASCAL DIEnumJoystickCallback (LPCDIDEVICEINSTANCE lpdinst, LPVOID pvRef)
{
	CD3DFramework        *frame = (CD3DFramework*)pvRef;
	HWND                 hWnd = frame->m_hWnd;
	LPDIRECTINPUT        m_pDI = frame->m_pDI;
	LPDIRECTINPUTDEVICE	 lpDev;

	if (m_pDI->CreateDevice (lpdinst->guidInstance, &lpDev, NULL) != DI_OK)
		return DIENUM_CONTINUE;

	if (lpDev->QueryInterface (IID_IDirectInputDevice2, (void**)&frame->m_pDIJoystick) != S_OK) {
		lpDev->Release();
		return DIENUM_CONTINUE;
	}
	lpDev->Release();

	if (frame->m_pDIJoystick->SetDataFormat (&c_dfDIJoystick2) != DI_OK) {
		frame->m_pDIJoystick->Release ();
		frame->m_pDIJoystick = NULL;
		return DIENUM_CONTINUE;
	}

	if (frame->m_pDIJoystick->SetCooperativeLevel (hWnd,
		frame->m_bIsFullscreen ? DISCL_EXCLUSIVE | DISCL_BACKGROUND : DISCL_NONEXCLUSIVE | DISCL_FOREGROUND)
		!= DI_OK) {
		frame->m_pDIJoystick->Release ();
		frame->m_pDIJoystick = NULL;
		return DIENUM_CONTINUE;
	}

	return DIENUM_STOP;
}

// =====================================================================================
// SetJoystickProperties()
// set the properties of the joystick (range, deadzone)

BOOL CD3DFramework::SetJoystickProperties (LPDIRECTINPUTDEVICE lpDev)
{
	DIPROPRANGE diprg;
	DIPROPDWORD diprw;

	// x-axis range
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_X;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if (lpDev->SetProperty (DIPROP_RANGE, &diprg.diph) != DI_OK)
		return FALSE;

	// x-axis deadzone
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_X;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = g_config.JoyDeadzone;
	if (lpDev->SetProperty (DIPROP_DEADZONE, &diprw.diph) != DI_OK)
		return FALSE;

	// y-axis range
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_Y;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if (lpDev->SetProperty (DIPROP_RANGE, &diprg.diph) != DI_OK)
		return FALSE;

	// y-axis deadzone
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_Y;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = g_config.JoyDeadzone;
	if (lpDev->SetProperty (DIPROP_DEADZONE, &diprw.diph) != DI_OK)
		return FALSE;

	// initialise rudder control
	m_JoystickHasRudder = TRUE;

	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_RZ;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = +1000;
	if (lpDev->SetProperty (DIPROP_RANGE, &diprg.diph) != DI_OK)
		m_JoystickHasRudder = FALSE;

	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_RZ;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = g_config.JoyDeadzone;
	if (lpDev->SetProperty (DIPROP_DEADZONE, &diprw.diph) != DI_OK)
		m_JoystickHasRudder = FALSE;

	// z-axis range (throttle)
	diprg.diph.dwSize       = sizeof (diprg);
	diprg.diph.dwHeaderSize = sizeof (diprg.diph);
	diprg.diph.dwObj        = DIJOFS_Z;
	diprg.diph.dwHow        = DIPH_BYOFFSET;
	diprg.lMin              = -1000;
	diprg.lMax              = 0;
	if (lpDev->SetProperty (DIPROP_RANGE, &diprg.diph) != DI_OK)
		return FALSE;

	// throttle saturation at extreme ends
	diprw.diph.dwSize       = sizeof (diprw);
	diprw.diph.dwHeaderSize = sizeof (diprw.diph);
	diprw.diph.dwObj        = DIJOFS_Z;
	diprw.diph.dwHow        = DIPH_BYOFFSET;
	diprw.dwData            = g_config.JoyThrottleSaturation;
	if (lpDev->SetProperty (DIPROP_SATURATION, &diprw.diph) != DI_OK) {
		LOGOUT("Setting joystick throttle saturation failed");
	} else {
		LOGOUT1P("Joystick throttle saturation: %d", g_config.JoyThrottleSaturation);
	}

	LOGOUT1P("Joystick deadzone: %d", g_config.JoyDeadzone);

	return TRUE;
}

HRESULT CD3DFramework::GetJoystickStatus (DIJOYSTATE2 *js)
{
	HRESULT hr = m_pDIJoystick->Poll ();
	if (hr != DI_OK && hr != DI_NOEFFECT) return hr;

	hr = m_pDIJoystick->GetDeviceState (sizeof (DIJOYSTATE2), js);
	return hr;
}

//-----------------------------------------------------------------------------
// Name: ShowFrame()
// Desc: Show the frame on the primary surface, via a blt or a flip.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::ShowFrame()
{
	if( NULL == m_pddsFrontBuffer )
		return D3DFWERR_NOTINITIALIZED;

    // Check for a backbuffer. If no backbuffer exists, then we have nothing to
    // do. However, to be consistent let's check for lost surfaces
    if( NULL == m_pddsBackBuffer )
        return m_pddsFrontBuffer->IsLost();

    // If we are in fullscreen mode perform a flip.
    if( m_bIsFullscreen )
        return m_pddsFrontBuffer->Flip( NULL, DDFLIP_WAIT );

    // Else, we are in windowed mode, so perform a blit.
    return m_pddsFrontBuffer->Blt( &m_rcScreenRect, m_pddsBackBuffer, 
                                   &m_rcViewportRect, DDBLT_WAIT, NULL );
}

//-----------------------------------------------------------------------------
// Name: FlipToGDISurface()
// Desc: Puts the GDI surface in front of the primary, so that dialog
//       boxes and other windows drawing funcs may happen.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::FlipToGDISurface( BOOL bDrawFrame )
{
    if( m_pDD && m_bIsFullscreen )
    {
        m_pDD->FlipToGDISurface();

        if( bDrawFrame )
        {
            DrawMenuBar( m_hWnd );
            RedrawWindow( m_hWnd, NULL, NULL, RDW_FRAME );
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RestoreSurfaces()
// Desc: Checks for lost surfaces and restores them if lost. Note: Don't
//       restore render surface, since it's just a duplicate ptr.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::RestoreSurfaces()
{
    // Check/restore the primary surface
    if( m_pddsFrontBuffer )
        if( m_pddsFrontBuffer->IsLost() )
            m_pddsFrontBuffer->Restore();
    
    // Check/restore the back buffer
    if( m_pddsBackBuffer )
        if( m_pddsBackBuffer->IsLost() )
            m_pddsBackBuffer->Restore();

    // Check/restore the z-buffer surface
    if( m_pddsZBuffer )
        if( m_pddsZBuffer->IsLost() )
            m_pddsZBuffer->Restore();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: Move()
// Desc: Moves the screen rect for windowed renderers
//-----------------------------------------------------------------------------
VOID CD3DFramework::Move( INT x, INT y )
{
    if( FALSE == m_bIsFullscreen )
    {
        SetRect( &m_rcScreenRect, x, y, 
			     x + m_dwRenderWidth, y + m_dwRenderHeight );

		// If we have no backbuffer, then update viewport rect as well
		if( NULL == m_pddsBackBuffer )
			CopyMemory( &m_rcViewportRect, &m_rcScreenRect, sizeof(RECT) );
    }
}




//-----------------------------------------------------------------------------
// Name: ChangeRenderTarget()
// Desc: Wrapper for the IDirect3DDevice::SetRenderTarget() function, which
//       adds functionality to handle an attached z-buffer. Note that this
//       function does NOT alter the current viewport
//-----------------------------------------------------------------------------
HRESULT CD3DFramework::ChangeRenderTarget( LPDIRECTDRAWSURFACE4 pddsNewTarget )
{
    if( NULL == pddsNewTarget )
        return E_INVALIDARG;

    // Get the new render target dimensions
    DDSURFACEDESC2 ddsd;
    D3DUtil_InitSurfaceDesc( ddsd );
    pddsNewTarget->GetSurfaceDesc( &ddsd );
    m_dwRenderWidth  = ddsd.dwWidth;
    m_dwRenderHeight = ddsd.dwHeight;

    // If a z-buffer is attached, delete and recreate it
    if( NULL != m_pddsZBuffer )
    {
        // Remove the old z-buffer
        m_pddsRenderTarget->DeleteAttachedSurface( 0, m_pddsZBuffer );
        SAFE_RELEASE( m_pddsZBuffer );

        // Keep track of reference counts
        SAFE_RELEASE( m_pddsRenderTarget );
        m_pddsRenderTarget = pddsNewTarget;
        m_pddsRenderTarget->AddRef();

        // Create the new z-buffer
        if( FAILED( CreateZBuffer() ) )
        {
            DEBUG_MSG( TEXT("ChangeRenderTarget() - zbuffer create failed") );
            return D3DFWERR_NOZBUFFER;
        }
    }
    else
    {
        // With no z-buffer, we just do accounting on the reference counts
        SAFE_RELEASE( m_pddsRenderTarget );
        m_pddsRenderTarget = pddsNewTarget;
        m_pddsRenderTarget->AddRef();
    }

    // Finally, perform the set render target call
    if( FAILED( m_pd3dDevice->SetRenderTarget( m_pddsRenderTarget, 0 ) ) )
    {
        return D3DFWERR_NORENDERTARGET;
    }

    return S_OK;
}





