// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ====================================================================================
// File: D3d7frame.cpp
// Desc: Class functions to implement a Direct3D app framework.
// ====================================================================================

#define STRICT 1
#include <windows.h>
#include "D3d7frame.h"
#include "D3d7util.h"
#include "Log.h"
#include "util.h"

//-----------------------------------------------------------------------------
// Name: CD3DFramework7()
// Desc: The constructor. Clears static variables
//-----------------------------------------------------------------------------
CD3DFramework7::CD3DFramework7 ()
{
	m_hWnd               = NULL;
	m_bIsFullscreen      = FALSE;
	m_bIsTLDevice        = FALSE;
	m_bIsStereo          = FALSE;
	m_bNoVSync           = FALSE;
	m_bSupportsMipmaps   = FALSE;
	m_dwRenderWidth      = 0L;
	m_dwRenderHeight     = 0L;
	m_dwZBufferBitDepth  = 0L;
	m_dwStencilBitDepth  = 0L;
	m_dwMaxLights        = 8L;

	m_pddsFrontBuffer    = NULL;
	m_pddsBackBuffer     = NULL;
	m_pddsBackBufferLeft = NULL;
     
	m_pddsZBuffer        = NULL;
	m_pd3dDevice         = NULL;
	m_pDD                = NULL;
	m_pD3D               = NULL;
	m_dwDeviceMemType    = NULL;

	m_ddFlipFlag         = DDFLIP_WAIT;
}

//-----------------------------------------------------------------------------
// Name: ~CD3DFramework7()
// Desc: The destructor. Deletes all objects
//-----------------------------------------------------------------------------
CD3DFramework7::~CD3DFramework7 ()
{
    DestroyObjects ();
}

//-----------------------------------------------------------------------------
// Name: DestroyObjects()
// Desc: Cleans everything up upon deletion. This code returns an error
//       if any of the objects have remaining reference counts.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::DestroyObjects (bool warnref)
{
    LONG nDD  = 0L; // Number of outstanding DDraw references
    LONG nD3D = 0L; // Number of outstanding D3DDevice references
	HRESULT hr;

    if (m_pDD) m_pDD->SetCooperativeLevel(m_hWnd, DDSCL_NORMAL);

    // Do a safe check for releasing the D3DDEVICE. RefCount must be zero.
    if (m_pd3dDevice) {
		if (FAILED (hr = m_pd3dDevice->SetTexture(0,0))) // make sure last texture is released
			LOGOUT_DDERR(hr);
        if (0 < (nD3D = m_pd3dDevice->Release()))
			if (warnref)
	            LOGOUT_ERR("D3DDevice object is still referenced!");
	}
    m_pd3dDevice = NULL;

    SAFE_RELEASE (m_pddsBackBuffer);
    SAFE_RELEASE (m_pddsBackBufferLeft);
    SAFE_RELEASE (m_pddsZBuffer);
    SAFE_RELEASE (m_pddsFrontBuffer);
    SAFE_RELEASE (m_pD3D);

	if (m_pDD) {
        // Do a safe check for releasing DDRAW. RefCount must be zero.
        if (0 < (nDD = m_pDD->Release()))
			if (warnref)
				LOGOUT_WARN("On simulation shutdown %d graphics objects were not released", nDD);
	}
    m_pDD = NULL;

    // Return successful, unless there are outstanding DD or D3DDevice refs.
    return (nDD==0 && nD3D==0) ? S_OK : D3DFWERR_NONZEROREFCOUNT;
}

//-----------------------------------------------------------------------------
// Name: Initialize()
// Desc: Creates the internal objects for the framework
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::Initialize (HWND hWnd, GUID* pDriverGUID,
                                    GUID* pDeviceGUID, DDSURFACEDESC2* pMode,
                                    DWORD dwFlags)
{
    HRESULT hr;

    // Check params. Note: A NULL mode is valid for windowed modes only.
    if ((NULL==hWnd ) || (NULL==pDeviceGUID) || 
        (NULL==pMode && (dwFlags&D3DFW_FULLSCREEN)))
        return E_INVALIDARG;

    // Setup state for windowed/fullscreen mode
    m_hWnd          = hWnd;
	m_bIsStereo     = FALSE;
    m_bIsFullscreen = (dwFlags & D3DFW_FULLSCREEN) ? TRUE : FALSE;
	m_bIsTLDevice   = ( *pDeviceGUID == IID_IDirect3DTnLHalDevice );
	m_bNoVSync      = (dwFlags & D3DFW_NOVSYNC) ? TRUE : FALSE;
	m_bPageflip     = (dwFlags & D3DFW_PAGEFLIP) ? TRUE : FALSE;
	m_bTryStencil   = (dwFlags & D3DFW_TRYSTENCIL) ? TRUE : FALSE;

    // Support stereoscopic viewing for fullscreen modes which support it
	if ((dwFlags & D3DFW_STEREO) && (dwFlags & D3DFW_FULLSCREEN))
		if (pMode->ddsCaps.dwCaps2 & DDSCAPS2_STEREOSURFACELEFT)
			m_bIsStereo = TRUE;

    // Create the D3D rendering environment (surfaces, device, viewport, and so forth.)
    if (FAILED (hr = CreateEnvironment (pDriverGUID, pDeviceGUID, pMode, dwFlags))) {
        DestroyObjects ();
        return hr;
    }

	m_ddFlipFlag = DDFLIP_WAIT;
	if (m_bIsStereo) m_ddFlipFlag |= DDFLIP_STEREO;
	if (m_bNoVSync)  m_ddFlipFlag |= DDFLIP_NOVSYNC;

    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: CreateEnvironment()
// Desc: Creates the internal objects for the framework
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::CreateEnvironment (GUID* pDriverGUID, GUID* pDeviceGUID,
                                           DDSURFACEDESC2* pMode, DWORD dwFlags)
{
    HRESULT hr;

    // Select the default memory type, for whether the device is HW or SW
    if (IsEqualIID (*pDeviceGUID, IID_IDirect3DHALDevice))
        m_dwDeviceMemType = DDSCAPS_VIDEOMEMORY;
    else if (IsEqualIID (*pDeviceGUID, IID_IDirect3DTnLHalDevice))
        m_dwDeviceMemType = DDSCAPS_VIDEOMEMORY;
    else
        m_dwDeviceMemType = DDSCAPS_SYSTEMMEMORY;

    // Create the DDraw object
    hr = CreateDirectDraw (pDriverGUID, dwFlags);
    if (FAILED (hr)) return hr;

    // Create the front and back buffers, and attach a clipper
    if (dwFlags & D3DFW_FULLSCREEN) hr = CreateFullscreenBuffers (pMode);
    else                            hr = CreateWindowedBuffers ();
    if (FAILED (hr)) return hr;

    // Create the Direct3D object and the Direct3DDevice object
    hr = CreateDirect3D (pDeviceGUID);
    if (FAILED (hr)) return hr;

    // Create and attach the zbuffer
    if (dwFlags & D3DFW_ZBUFFER)
        hr = CreateZBuffer( pDeviceGUID );
    if (FAILED (hr)) return hr;

    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: EnumZBufferFormatsCallback()
// Desc: Simply returns the first matching enumerated z-buffer format
//-----------------------------------------------------------------------------
static HRESULT WINAPI EnumZBufferFormatsCallback (DDPIXELFORMAT* pddpf, VOID* pContext)
{
    DDPIXELFORMAT* pddpfOut = (DDPIXELFORMAT*)pContext;

    if (pddpfOut->dwRGBBitCount == pddpf->dwRGBBitCount && pddpfOut->dwStencilBitDepth <= pddpf->dwStencilBitDepth) {
        (*pddpfOut) = (*pddpf);
        return D3DENUMRET_CANCEL;
    }

    return D3DENUMRET_OK;
}

//-----------------------------------------------------------------------------
// Name: CreateDirectDraw()
// Desc: Create the DirectDraw interface
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::CreateDirectDraw (GUID* pDriverGUID, DWORD dwFlags)
{
	HRESULT hr;
    // Create the DirectDraw interface
    if (FAILED (hr = DirectDrawCreateEx (pDriverGUID, (VOID**)&m_pDD,
                                    IID_IDirectDraw7, NULL))) {
        LOGOUT("ERROR: Could not create DirectDraw");
		LOGOUT_DDERR(hr);
        return D3DFWERR_NODIRECTDRAW;
    }

    // Set the Windows cooperative level
    DWORD dwCoopFlags = DDSCL_NORMAL;
    if (m_bIsFullscreen)
        dwCoopFlags = DDSCL_ALLOWREBOOT | DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN;
	dwCoopFlags |= DDSCL_MULTITHREADED; // set for both modes

    // floating point optimisation flag
    if (dwFlags & D3DFW_NO_FPUSETUP) dwCoopFlags |= DDSCL_FPUPRESERVE;
	else                             dwCoopFlags |= DDSCL_FPUSETUP;

    if (FAILED (hr = m_pDD->SetCooperativeLevel (m_hWnd, dwCoopFlags))) {
        LOGOUT("ERROR: Couldn't set coop level");
		LOGOUT_DDERR(hr);
        return D3DFWERR_COULDNTSETCOOPLEVEL;
    }

    // Check that we are NOT in a palettized display. That case will fail,
    // since the Direct3D framework doesn't use palettes.
    DDSURFACEDESC2 ddsd;
    ddsd.dwSize = sizeof(ddsd);
    m_pDD->GetDisplayMode (&ddsd);
    if (ddsd.ddpfPixelFormat.dwRGBBitCount <= 8) {
		LOGOUT("ERROR: Display mode bpp <= 8");
        return D3DFWERR_INVALIDMODE;
	}

	LOGOUT("DirectDraw interface OK");
    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: CreateFullscreenBuffers()
// Desc: Creates the primary and (optional) backbuffer for rendering.
//       Windowed mode and fullscreen mode are handled differently.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::CreateFullscreenBuffers (DDSURFACEDESC2* pddsd)
{
    HRESULT hr;

    // Get the dimensions of the screen bounds
    // Store the rectangle which contains the renderer
    SetRect (&m_rcScreenRect, 0, 0, pddsd->dwWidth, pddsd->dwHeight);
    m_dwRenderWidth  = m_rcScreenRect.right  - m_rcScreenRect.left;
    m_dwRenderHeight = m_rcScreenRect.bottom - m_rcScreenRect.top;

    // Set the display mode to the requested dimensions. Check for
    // 320x200x8 modes, and set flag to avoid using ModeX
    DWORD dwModeFlags = 0;

    if ((320==m_dwRenderWidth) && (200==m_dwRenderHeight) &&
        (8==pddsd->ddpfPixelFormat.dwRGBBitCount))
        dwModeFlags |= DDSDM_STANDARDVGAMODE;

    if (FAILED (m_pDD->SetDisplayMode (m_dwRenderWidth, m_dwRenderHeight,
                                pddsd->ddpfPixelFormat.dwRGBBitCount,
                                pddsd->dwRefreshRate, dwModeFlags))) {
        LOGOUT("Can't set display mode");
        return D3DFWERR_BADDISPLAYMODE;
    }

    // Setup to create the primary surface w/backbuffer
    DDSURFACEDESC2 ddsd;
    ZeroMemory (&ddsd, sizeof(ddsd));
    ddsd.dwSize            = sizeof (ddsd);
    ddsd.dwFlags           = DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
    ddsd.ddsCaps.dwCaps    = DDSCAPS_PRIMARYSURFACE | DDSCAPS_3DDEVICE |
                             DDSCAPS_FLIP | DDSCAPS_COMPLEX;
    ddsd.dwBackBufferCount = 1;

    // Support for stereoscopic viewing
    if (m_bIsStereo) {
        ddsd.ddsCaps.dwCaps  |= DDSCAPS_VIDEOMEMORY;
        ddsd.ddsCaps.dwCaps2 |= DDSCAPS2_STEREOSURFACELEFT;
    }

    // Create the primary surface
    if (FAILED (hr = m_pDD->CreateSurface (&ddsd, &m_pddsFrontBuffer, NULL))) {
        LOGOUT("Error: Can't create primary surface");
        if (hr != DDERR_OUTOFVIDEOMEMORY) return D3DFWERR_NOPRIMARY;
        LOGOUT("Error: Out of video memory");
        return DDERR_OUTOFVIDEOMEMORY;
    }

    // Get the backbuffer, which was created along with the primary.
    DDSCAPS2 ddscaps = { DDSCAPS_BACKBUFFER, 0, 0, 0 };
    if (FAILED (hr = m_pddsFrontBuffer->GetAttachedSurface (&ddscaps, &m_pddsBackBuffer))) {
        LOGOUT("Error: Can't get the backbuffer");
        return D3DFWERR_NOBACKBUFFER;
	}

    // Increment the backbuffer count (for consistency with windowed mode)
    m_pddsBackBuffer->AddRef ();

    // Support for stereoscopic viewing
    if (m_bIsStereo) {
        // Get the left back buffer, which was created along with the primary.
        DDSCAPS2 ddscaps = { 0, DDSCAPS2_STEREOSURFACELEFT, 0, 0 };
        if (FAILED (hr = m_pddsBackBuffer->GetAttachedSurface (&ddscaps, &m_pddsBackBufferLeft))) {
            LOGOUT("Error: Can't get the left back buffer");
            return D3DFWERR_NOBACKBUFFER;
        }
        m_pddsBackBufferLeft->AddRef ();
    }

	ZeroMemory (&m_ddpfBackBufferPixelFormat, sizeof (DDPIXELFORMAT));
	m_ddpfBackBufferPixelFormat.dwSize = sizeof (DDPIXELFORMAT);
	if (FAILED (hr = m_pddsBackBuffer->GetPixelFormat (&m_ddpfBackBufferPixelFormat))) {
		LOGOUT("ERROR: Check on backbuffer pixel format failed");
		LOGOUT_DDERR(hr);
	}

	hr = m_pddsBackBuffer->GetSurfaceDesc (&ddsd);
	
    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: CreateWindowedBuffers()
// Desc: Creates the primary and (optional) backbuffer for rendering.
//       Windowed mode and fullscreen mode are handled differently.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::CreateWindowedBuffers ()
{
    HRESULT hr;

    // Get the dimensions of the viewport and screen bounds
    GetClientRect (m_hWnd, &m_rcScreenRect);
    ClientToScreen (m_hWnd, (POINT*)&m_rcScreenRect.left);
    ClientToScreen (m_hWnd, (POINT*)&m_rcScreenRect.right);
    m_dwRenderWidth  = m_rcScreenRect.right  - m_rcScreenRect.left;
    m_dwRenderHeight = m_rcScreenRect.bottom - m_rcScreenRect.top;

    // Create the primary surface
    DDSURFACEDESC2 ddsd;
    ZeroMemory (&ddsd, sizeof(ddsd));
    ddsd.dwSize         = sizeof(ddsd);
    ddsd.dwFlags        = DDSD_CAPS;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;

    if (FAILED (hr = m_pDD->CreateSurface (&ddsd, &m_pddsFrontBuffer, NULL))) {
        LOGOUT("Error: Can't create primary surface");
        if (hr != DDERR_OUTOFVIDEOMEMORY) return D3DFWERR_NOPRIMARY;
        LOGOUT("Error: Out of video memory");
        return DDERR_OUTOFVIDEOMEMORY;
    }

    // If in windowed-mode, create a clipper object
    LPDIRECTDRAWCLIPPER pcClipper;
    if (FAILED (hr = m_pDD->CreateClipper (0, &pcClipper, NULL))) {
        LOGOUT("Error: Couldn't create clipper");
        return D3DFWERR_NOCLIPPER;
    }

    // Associate the clipper with the window
    pcClipper->SetHWnd (0, m_hWnd);
    m_pddsFrontBuffer->SetClipper (pcClipper);
    SAFE_RELEASE (pcClipper);

    // Create a backbuffer
    ddsd.dwFlags        = DDSD_WIDTH | DDSD_HEIGHT | DDSD_CAPS;
    ddsd.dwWidth        = m_dwRenderWidth;
    ddsd.dwHeight       = m_dwRenderHeight;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN | DDSCAPS_3DDEVICE;

    if (FAILED (hr = m_pDD->CreateSurface (&ddsd, &m_pddsBackBuffer, NULL))) {
        LOGOUT("Error: Couldn't create the backbuffer");
        if (hr != DDERR_OUTOFVIDEOMEMORY)
            return D3DFWERR_NOBACKBUFFER;
        LOGOUT("Error: Out of video memory");
        return DDERR_OUTOFVIDEOMEMORY;
    }

	ZeroMemory (&m_ddpfBackBufferPixelFormat, sizeof (DDPIXELFORMAT));
	m_ddpfBackBufferPixelFormat.dwSize = sizeof (DDPIXELFORMAT);
	hr = m_pddsBackBuffer->GetPixelFormat (&m_ddpfBackBufferPixelFormat);
	hr = m_pddsBackBuffer->GetSurfaceDesc (&ddsd);

    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: CreateDirect3D()
// Desc: Create the Direct3D interface
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::CreateDirect3D (GUID* pDeviceGUID)
{
    // Query DirectDraw for access to Direct3D.
    if (FAILED (m_pDD->QueryInterface (IID_IDirect3D7, (VOID**)&m_pD3D))) {
		LOGOUT("ERROR: Could not get Direct3D interface");
        return D3DFWERR_NODIRECT3D;
    }

    // Create the device
    if (FAILED (m_pD3D->CreateDevice (*pDeviceGUID, m_pddsBackBuffer, &m_pd3dDevice))) {
		LOGOUT("ERROR: Could not create D3DDevice");
        return D3DFWERR_NO3DDEVICE;
    }

	// The following code checks whether the device supports mipmapped textures
    D3DDEVICEDESC7 d3dDesc;
    LPD3DPRIMCAPS pdpcTriCaps;
    ZeroMemory (&d3dDesc, sizeof(D3DDEVICEDESC7));
    m_pd3dDevice->GetCaps (&d3dDesc);
	pdpcTriCaps = &d3dDesc.dpcTriCaps;

	// check if device supports mipmaping 
	m_bSupportsMipmaps = 
		((pdpcTriCaps->dwTextureFilterCaps & D3DPTFILTERCAPS_MIPNEAREST | 
		  pdpcTriCaps->dwTextureFilterCaps & D3DPTFILTERCAPS_MIPLINEAR) ? TRUE : FALSE);

	// max number of simultaneously active light sources
	m_dwMaxLights = d3dDesc.dwMaxActiveLights;

	// Finally, set the viewport for the newly created device
    D3DVIEWPORT7 vp = { 0, 0, m_dwRenderWidth, m_dwRenderHeight, 0.0f, 1.0f };

    if (FAILED (m_pd3dDevice->SetViewport (&vp))) {
        LOGOUT("ERROR: Could not set current viewport to device");
        return D3DFWERR_NOVIEWPORT;
    }

	LOGOUT("Direct3D interface OK");
    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: CreateZBuffer()
// Desc: Internal function called by Create() to make and attach a zbuffer
//       to the renderer
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::CreateZBuffer (GUID* pDeviceGUID)
{
    HRESULT hr;

    // Check if the device supports z-bufferless hidden surface removal. If so,
    // we don't really need a z-buffer
    D3DDEVICEDESC7 ddDesc;
    m_pd3dDevice->GetCaps (&ddDesc);
    if (ddDesc.dpcTriCaps.dwRasterCaps & D3DPRASTERCAPS_ZBUFFERLESSHSR) {
		LOGOUT("Zbuffer: not required");
        return S_OK;
	}

    // Get z-buffer dimensions from the render target
    DDSURFACEDESC2 ddsd;
    ddsd.dwSize = sizeof(ddsd);
    m_pddsBackBuffer->GetSurfaceDesc (&ddsd);

    // Setup the surface desc for the z-buffer.
    ddsd.dwFlags        = DDSD_WIDTH | DDSD_HEIGHT | DDSD_CAPS | DDSD_PIXELFORMAT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_ZBUFFER | m_dwDeviceMemType;
    ddsd.ddpfPixelFormat.dwSize = 0;  // Tag the pixel format as unitialized.
	ddsd.ddpfPixelFormat.dwStencilBitDepth = (m_bTryStencil ? 1:0);

	// Get an appropiate pixel format from enumeration of the formats. On the
    // first pass, we look for a zbuffer dpeth which is equal to the frame
    // buffer depth (as some cards unfornately require this).
    m_pD3D->EnumZBufferFormats (*pDeviceGUID, EnumZBufferFormatsCallback,
                                (VOID*)&ddsd.ddpfPixelFormat);

	// If we were looking for stencil support and no match was found,
	// try again without stencil
	if (0 == ddsd.ddpfPixelFormat.dwSize && m_bTryStencil) {
		ddsd.ddpfPixelFormat.dwStencilBitDepth = 0;
	    m_pD3D->EnumZBufferFormats (*pDeviceGUID, EnumZBufferFormatsCallback,
		     (VOID*)&ddsd.ddpfPixelFormat);
	}

	// Still no luck: Try again, just accepting any 16-bit zbuffer mode
    if (0 == ddsd.ddpfPixelFormat.dwSize) {
        ddsd.ddpfPixelFormat.dwRGBBitCount = 16;
        m_pD3D->EnumZBufferFormats (*pDeviceGUID, EnumZBufferFormatsCallback,
                                    (VOID*)&ddsd.ddpfPixelFormat);
            
        if (0 == ddsd.ddpfPixelFormat.dwSize) {
			LOGOUT("ERROR: Device does not support requested zbuffer format");
            return D3DFWERR_NOZBUFFER;
        }
    }

	m_dwZBufferBitDepth = ddsd.ddpfPixelFormat.dwZBufferBitDepth;
	m_dwStencilBitDepth = ddsd.ddpfPixelFormat.dwStencilBitDepth;

    // Create and attach a z-buffer
    if (FAILED (hr = m_pDD->CreateSurface (&ddsd, &m_pddsZBuffer, NULL))) {
        LOGOUT("ERROR: Could not create a ZBuffer surface");
        if (hr != DDERR_OUTOFVIDEOMEMORY)
            return D3DFWERR_NOZBUFFER;
        LOGOUT("ERROR: Out of video memory");
        return DDERR_OUTOFVIDEOMEMORY;
    }

    if (FAILED (m_pddsBackBuffer->AddAttachedSurface (m_pddsZBuffer))) {
        LOGOUT("ERROR: Could not attach zbuffer to render surface");
        return D3DFWERR_NOZBUFFER;
    }

    // For stereoscopic viewing, attach zbuffer to left surface as well
    if (m_bIsStereo) {
        if (FAILED (m_pddsBackBufferLeft->AddAttachedSurface (m_pddsZBuffer))) {
            LOGOUT("ERROR: Could not attach zbuffer to left render surface");
            return D3DFWERR_NOZBUFFER;
        }
    }

    // Finally, this call rebuilds internal structures
    if (FAILED (m_pd3dDevice->SetRenderTarget (m_pddsBackBuffer, 0L))) {
        LOGOUT("ERROR: SetRenderTarget() failed after attaching zbuffer!");
        return D3DFWERR_NOZBUFFER;
    }

    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: RestoreSurfaces()
// Desc: Checks for lost surfaces and restores them if lost. Note: Don't
//       restore render surface, since it's just a duplicate ptr.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::RestoreSurfaces ()
{
	// Restore all surfaces (including video memory vertex buffers)
	m_pDD->RestoreAllSurfaces ();

    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: Move()
// Desc: Moves the screen rect for windowed renderers
//-----------------------------------------------------------------------------
VOID CD3DFramework7::Move (INT x, INT y)
{
    if (TRUE == m_bIsFullscreen) return;
    SetRect (&m_rcScreenRect, x, y, x + m_dwRenderWidth, y + m_dwRenderHeight);
}

//-----------------------------------------------------------------------------
// Name: FlipToGDISurface()
// Desc: Puts the GDI surface in front of the primary, so that dialog
//       boxes and other windows drawing funcs may happen.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::FlipToGDISurface (BOOL bDrawFrame)
{
    if (m_pDD && m_bIsFullscreen) {
        m_pDD->FlipToGDISurface ();
        if (bDrawFrame) {
            DrawMenuBar (m_hWnd);
            RedrawWindow (m_hWnd, NULL, NULL, RDW_FRAME);
        }
    }

    return S_OK;
}

//-----------------------------------------------------------------------------
// Name: ShowFrame()
// Desc: Show the frame on the primary surface, via a blt or a flip.
//-----------------------------------------------------------------------------
HRESULT CD3DFramework7::ShowFrame ()
{
	if (NULL == m_pddsFrontBuffer) {
        return D3DFWERR_NOTINITIALIZED;
	}

    if (m_bIsFullscreen && m_bPageflip) {
        // We are in fullscreen mode, so perform a flip.
		HRESULT hres = m_pddsFrontBuffer->Flip (NULL, m_ddFlipFlag);
		return hres;
    } else {
        // We are in windowed mode, so perform a blit.
        HRESULT hres = m_pddsFrontBuffer->Blt (&m_rcScreenRect, m_pddsBackBuffer,
                                       NULL, DDBLT_WAIT, NULL);
		return hres;
    }
}

DWORD CD3DFramework7::RequireTexPow2 () const
{
	D3DDEVICEDESC7 ddsd;
	m_pd3dDevice->GetCaps (&ddsd);
	if (ddsd.dpcTriCaps.dwTextureCaps & D3DPTEXTURECAPS_POW2)
		return 1;
	else if (ddsd.dpcTriCaps.dwTextureCaps & D3DPTEXTURECAPS_NONPOW2CONDITIONAL)
		return 2;
	else
		return 0;
}