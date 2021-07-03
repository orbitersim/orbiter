/*
**-----------------------------------------------------------------------------
**  Name:       D3DWin.cpp
**  Purpose:    
**
**	Basic Initialization proceeds as follows:
**
**	1.  Enumerate all Driver, modes, D3D devices (see DrvMgr.cpp for details)
**	2.  Choose a starting driver, mode, D3D device
**			- default driver = primary display driver (lpGuidDD = NULL)
**			- default mode   = current desktop
**			- default device = D3D device compatible with desktop mode
**	3.  Validate driver, mode, D3D device
**	4.  Create interfaces (from DD driver)
**  5.  Set window (from associated window handle)
**  6.  Create DD/D3D interfaces (lpDD, lpDD2, lpD3D)
**  7.  Create Primary surface (primary palette, if necessary)
**			- Attach a clipper to primary surface
**  8.  Create Render surface 
**			- Render surface (and associated Z-buffer)
**			- D3D Device 
**			- D3D Viewport
**
**  After initialization is complete, we have the
**	following objects necessary for rendering:
**
**		lpDD2 - DirectDraw interface, used for creating texture surfaces
**		lpD3D - Direct3D interface, used for creating materials, lights, viewports
**		lpD3DDevice - D3D device (current material, current viewport, etc.)
**		lpViewport - current viewport
**		lpPrimary  - front buffer
**		lpRender   - render target
**
**  Copyright (c) 1995-1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
** Includes
**-----------------------------------------------------------------------------
*/

#include "D3DWin.h"
#include "WinProc.h"
#include "D3DScene.h"
#include "Debug.h"



/*
**-----------------------------------------------------------------------------
**	D3DWindow Methods
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::D3DWindow
** Purpose: Default Constructor
**-----------------------------------------------------------------------------
*/

D3DWindow::D3DWindow (void)
{
    ZeroMemory (this, sizeof(D3DWindow));
    this->dwSize = sizeof(D3DWindow);

	// Default to creating a z-buffer
	createZBufferOn ();	
} // End D3DWindow::D3DWindow ()


  
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::~D3DWindow
** Purpose: Destructor
**-----------------------------------------------------------------------------
*/

D3DWindow::~D3DWindow (void)
{
	// Destroy all objects
    Fini ();

	// Mark all other pointers as invalid
	// In case user tries to reuse this object
	lpCurrDriver = NULL;
	lpCurrMode	 = NULL;
	lpCurrDevice = NULL;
	hWindow		 = NULL;
	lpd3dScene	 = NULL;

} // End D3DWindow::~D3DWindow



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::Create
** Purpose: Creates a D3DWindow 
**
** Basic Algorithm:
**	- Validate parameters
**	- Choose (and validate choices) for driver, mode, device
**	- Create Interfaces
**	- Set Window
**	- Set Mode
**	- Create Primary surface (and palette)
**	- Create Render surface (and D3D device)
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::Create (
	HWND   hWnd,		/* In:  Window */
	LPGUID lpGuidDD,	/* In:  Requested DirectDraw Device */
	DWORD  dwW,			/* In:	Requested Mode */
	DWORD  dwH,			
	DWORD  dwBPP,
	DWORD  dwRefresh,
	LPGUID lpGuidD3D,	/* In:  Requested D3D device */
	BOOL   fUseZBuffer) /* In:  Create Z-Buffer */
{
    HRESULT         hResult;

    // Check parameters
	if ((! hWnd) || (! IsWindow (hWnd)))
	{
		// Error, Invalid parameters
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
		return hResult;
	}

	// Set Current Window
	hWindow = hWnd;

    // Set Use Z-Buffer On/Off
    if (fUseZBuffer)
        createZBufferOn ();
    else
        createZBufferOff ();

    // Choose Default Driver, Mode, device 
    hResult = ChooseDriverDefaults (lpGuidDD, 
									dwW, dwH, dwBPP, dwRefresh,
									lpGuidD3D,
									TRUE, // HEREITIS!
									&lpCurrDriver,
									&lpCurrMode,
									&lpCurrDevice);
    if (FAILED (hResult))
        return hResult;
    
    // Create DD/D3D Interface objects
    hResult = InitInterfaces ();
    if (FAILED (hResult))
        return hResult;

	// Attach window to DD interface
	hResult = InitWindow ();
	if (FAILED (hResult))
		goto lblCLEANUP;

	// Set Fullscreen Mode
	hResult = InitFullscreenMode ();
	if (FAILED (hResult))
		goto lblCLEANUP;

    // Create Primary Surface (and palette)
    hResult = InitPrimary ();
    if (FAILED (hResult))
        goto lblCLEANUP;

	// Create the Render Surface (and D3D Device)
    hResult = InitRender ();
    if (FAILED (hResult))
        goto lblCLEANUP;

	// Notify the window of a successful initialization
	SendMessage (hWindow, D3DWIN_INIT, 0, (LPARAM)(void *)this);

    // Success
    return DD_OK;

lblCLEANUP:
	// Failure

    // Cleanup
    Fini ();

    return hResult;
} // End D3DWindow::Create



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::Init
** Purpose: 
**
** Basic Algorithm:
**	- Validate driver, mode, device
**	- Create Interfaces
**	- Attach Window
**	- Set Fullscreen Mode
**	- Create Primary surface (and palette)
**	- Create Render surface (and D3D device)
**
** Notes:
**		1.  Assumes that a valid window handle has already
**			been associated with this D3DWindow
**		2.  Assumes that driver, mode, device already choosen
**			- however if not, reasonable defaults will be choosen
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::Init (void)
{
    HRESULT         hResult;

    // Check parameters
	if ((! hWindow) || (! IsWindow (hWindow)))
	{
		// Error, Invalid Initialization
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
		return hResult;
	}

    // Validate Curr Driver, mode, device
	hResult = ValidateDefaults ();
	if (FAILED (hResult))
		return hResult;

    // Create DD/D3D Interface objects
    hResult = InitInterfaces ();
    if (FAILED (hResult))
		goto lblCLEANUP;

	// Attach the window to the DD interface
	hResult = InitWindow ();
	if (FAILED (hResult))
		goto lblCLEANUP;

	// Set the Mode
	hResult = InitFullscreenMode ();
	if (FAILED (hResult))
		goto lblCLEANUP;

    // Create Primary Surface (and palette)
    hResult = InitPrimary ();
    if (FAILED (hResult))
        goto lblCLEANUP;

	// Create Render surface (and D3D device)
    hResult = InitRender ();
    if (FAILED (hResult))
        goto lblCLEANUP;

	// Notify the window of a successful initialization
	SendMessage (hWindow, D3DWIN_INIT, 0, (LPARAM)(void *)this);

    // Success
    return DD_OK;

lblCLEANUP:
	// Failure 

    // Cleanup
    Fini ();
    return hResult;
} // End D3DWindow::Init



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::Fini
** Purpose: Destroys a D3DWindow
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::Fini (void)
{
	// Notify the window that we are cleaning up
	SendMessage (hWindow, D3DWIN_FINI, 0, (LPARAM)(void *)this);

	// Cleanup
    FiniRender ();
    FiniPrimary ();
	FiniFullscreenMode ();
//	FiniWindow ();
    FiniInterfaces ();

	// Success
	return DD_OK;
} // End D3DWindow::Fini


  
  
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::ValidateDefaults
** Purpose: Verify's current driver, mode, and device
** Notes:   
**
**	1.  Rather than fail completely, this will pick new defaults
**      if the current defaults don't work.
**
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::ValidateDefaults (void)
{
	LPGUID			lpGuidDD, lpGuidD3D;
	HRESULT			hResult;
    LPDDDrvInfo		lpDrvNew;
	LPDDModeInfo	lpModeNew;
	LPD3DDevInfo	lpDevNew;

	// Initialize Driver Manager, if necessary
	if (! DDDrvMgr::isInitialized ())
	{
		hResult = DDDrvMgr::Init ();
		if (FAILED (hResult))
			return hResult;
	}
    
    // Get DD Guid
	if (lpCurrDriver)
		lpGuidDD = lpCurrDriver->GetGuid ();
	else
		lpGuidDD = NULL;

	// Get D3D Guid
	if (lpCurrDevice)
		lpGuidD3D = &(lpCurrDevice->guid);
	else
		lpGuidD3D = NULL;

	// Get Driver corresponding to DD Guid
    lpDrvNew = ValidateDriver (lpGuidDD);
	if (! lpDrvNew)
    {
        // Error, invalid DD Guid
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
        return hResult;
    }

	DWORD w, h, bpp, refresh;

	// Get Current mode info
	if (lpCurrMode)
		lpCurrMode->GetMode (w, h, bpp, refresh);
	else
	{
		w = h = bpp = refresh = 0;
	}

	// Get Fullscreen D3D device and compatible mode
	if (! GetFullscreenMode (lpDrvNew, lpGuidD3D,
							 w, h, bpp, refresh,
							 &lpModeNew, &lpDevNew))
	{
		// Couldn't find a valid mode and/or device
		hResult = APPERR_GENERIC;
		REPORTERR (hResult);
		return APPERR_GENERIC;
	}

	// Note:  Instead of complaining let's go ahead
	//		  and use the new defaults
	// Save new defaults
	lpCurrDriver = lpDrvNew;
	lpCurrMode	 = lpModeNew;
	lpCurrDevice = lpDevNew;

    // Success
    return DD_OK;
} // End D3DWindow::ValidateDefaults



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::CreateInterfaces
** Purpose: Creates DD/D3D interfaces from specified Guid
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::CreateInterfaces (LPGUID lpDDGuid)
{
	LPDDDrvInfo lpDrvNew;
	HRESULT		hResult;

	// Verify Guid
	lpDrvNew = ValidateDriver (lpDDGuid);
	if (! lpDrvNew)
	{
		// Invalid Params
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
		return hResult;
	}

	lpCurrDriver = lpDrvNew;

	hResult = D3DWindow::InitInterfaces ();
	if (FAILED (hResult))
		return hResult;

	// Success
	return DD_OK;
} // End D3DWindow::CreateInterfaces

  

/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::InitInterfaces
** Purpose: Creates DD/D3D interfaces
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::InitInterfaces (void)
{
    HRESULT         hResult;
    LPGUID          lpGuid;

    // Do we have a current DD Driver
    if (! lpCurrDriver)
    {
		// So, Grab the Primary DD driver
		lpCurrDriver = ValidateDriver (NULL);
		if (! lpCurrDriver)
		{
			// Error, No current Driver
			hResult = APPERR_NOTINITIALIZED;
			REPORTERR (hResult);
			return hResult;
		}
    }

    // Get DD Guid
    lpGuid = lpCurrDriver->GetGuid ();
    
    // Create DD interface
    hResult = DirectDrawCreate (lpGuid, &lpDD, NULL);
    if (FAILED (hResult))
    {
        // Error
		REPORTERR (hResult);
		goto lblCLEANUP;
    }

    // Get DD2 interface
    hResult = lpDD->QueryInterface ((REFIID)IID_IDirectDraw2, (void **)&lpDD2);
    if (FAILED (hResult))
    {
        // Error
		REPORTERR (hResult);

		// Inform User that they Need DX 5.0 installed
		MessageBox(hWindow, "This application requires DirectX 5 or later.", NULL, MB_OK);
        goto lblCLEANUP;
    }

    // Get D3D interface
    hResult = lpDD2->QueryInterface ((REFIID)IID_IDirect3D2, (void **)&lpD3D);
    if (FAILED (hResult))
    {
        // Error
		REPORTERR (hResult);
        goto lblCLEANUP;
    }

	// Mark this stage as done
	turnValidInterfaceOn ();

    // Success
    return DD_OK;

lblCLEANUP:
    // Failure
    FiniInterfaces ();

    return hResult;
} // End InitInterfaces



  
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::FiniInterfaces
** Purpose: Destroys DD/D3D interfaces
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::FiniInterfaces (void)
{
	// Mark this stage as invalid
	turnValidInterfaceOff ();

    // Release Direct3D Interface
    if (lpD3D)
    {
        lpD3D->Release ();
        lpD3D = NULL;
    }

    // Release DirectDraw2 Interface
    if (lpDD2)
    {
        lpDD2->Release ();
        lpDD2 = NULL;
    }

    // Release DirectDraw Interface
    if (lpDD)
    {
        lpDD->Release ();
        lpDD = NULL;
    }

	// Success
	return DD_OK;
} // End D3DWindow::FiniInterfaces


/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::InitWindow
** Purpose: Attaches Window to Direct Draw Interface
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::InitWindow (void)
{
	HRESULT hResult;
	DWORD	dwFlags;

	// Check Initialization
	if ((! hWindow) || (! IsWindow (hWindow)))
	{
		// Error, we need a valid window to continue
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
		return hResult;
	}

    // Get Cooperative Flags
    dwFlags = DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN;

    // Set Cooperative Level
    hResult = lpDD2->SetCooperativeLevel (hWindow, dwFlags);
    if (FAILED (hResult))
    {
        // Error
        REPORTERR (hResult);
		return hResult;
    }

	// Success
	return DD_OK;
} // End D3DWindow::InitWindow


  
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::FiniWindow
** Purpose: Cleanups window
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::FiniWindow (void)
{
	// Currently does nothing

	// Success
	return DD_OK;
} // End D3DWindow::FiniWindow


  
/*
**-----------------------------------------------------------------------------
**	Name:		D3DWindow::InitFullscreenMode
**	Purpose:	Switches to requested fullscreen mode
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::InitFullscreenMode (void)
{
	HRESULT hResult;
	DWORD   dwFlags = 0;

	// Check Initialization
	if ((! lpCurrMode) || (! lpDD2))
	{
		// Error, we need a valid mode and DirectDraw 2 interface to proceed
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
		return hResult;
	}
        
	// Calculate Mode info
	DWORD w, h, bpp, refresh;
	lpCurrMode->GetMode (w, h, bpp, refresh);

	// Special check for mode 320 x 200 x 8
	if ((w == 320) && (h == 200) && (bpp == 8))
    {
		// Make sure we use Mode 13 instead of Mode X
		dwFlags = DDSDM_STANDARDVGAMODE;
    } 
		
    // Set Requested Fullscreen mode
    hResult = lpDD2->SetDisplayMode (w, h, bpp, refresh, dwFlags);
    if (SUCCEEDED (hResult))
	{
		// Save Surface Rectangle
		rSurf.left   = 0;
		rSurf.top    = 0;
		rSurf.right  = w;
		rSurf.bottom = h;

		// Success
		turnValidFullscreenOn ();
		return hResult;
	}

	DPF (DEBUG_ERROR, "SetDisplayMode failed (%d x %d x %d), trying (640 x 480 x %d)", w, h, bpp, bpp);

	// Don't give up!
	// Try 640 x 480 x bpp mode instead
	if ((w != 640 || h != 480))
    {
		w = 640;
		h = 480;

		lpCurrMode = ValidateMode (lpCurrDriver, w, h, bpp, 0, lpCurrDevice);
		if (lpCurrMode)
		{
			hResult = lpDD2->SetDisplayMode (w, h, bpp, 0, 0);
			if (SUCCEEDED (hResult))
			{
				// Save Surface Rectangle
				rSurf.left   = 0;
				rSurf.top    = 0;
				rSurf.right  = w;
				rSurf.bottom = h;

				// Success
				turnValidFullscreenOn ();
				return hResult;
			}
		}
	}

	// Keep trying
	// Try 640 x 480 x 16 mode instead
	if (bpp != 16)
    {
		DPF (DEBUG_ERROR, "SetDisplayMode failed (640 x 480 x %d), trying (640 x 480 x 16)", bpp);
		bpp = 16;

		lpCurrMode = ValidateMode (lpCurrDriver, w, h, bpp, 0, lpCurrDevice);
		if (lpCurrMode)
		{
			hResult = lpDD2->SetDisplayMode (w, h, bpp, 0, 0);
			if (SUCCEEDED (hResult))
			{
				// Save Surface Rectangle
				rSurf.left   = 0;
				rSurf.top    = 0;
				rSurf.right  = w;
				rSurf.bottom = h;

				// Success
				turnValidFullscreenOn ();
				return hResult;
			}
		}
	}

	// Failure
	REPORTERR (hResult);
	return hResult;
} // End D3DWindow::InitFullscreenMode



/*
**-----------------------------------------------------------------------------
**	Name:		D3DWindow::FiniFullscreenMode
**	Purpose:	Restores mode to original desktop
**	Notes:		This does nothing if we are windowed
**				I.E. the user already is restored
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::FiniFullscreenMode (void)
{
	turnValidFullscreenOff ();

	// Restore original desktop mode
	if (lpDD2)
		lpDD2->RestoreDisplayMode();

	// Success
	return DD_OK;
} // End D3DWindow::FiniFullscreenMode



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::InitPrimary
** Purpose: Creates a primary surface (desktop surface)
** Notes:   Also creates and attaches palette, if necessary
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::InitPrimary (void)
{
    HRESULT			hResult;
    DDSURFACEDESC	ddsd;

    // Check Initialization
    if ((! lpCurrMode) || (! lpDD2))
    {
        // Error, Need a valid mode and DD interface to proceed
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
        return hResult;
    }

	// Note:  There is no need to fill in width, height, bpp, etc.
	//        This was taken care of in the SetDisplayMode call.

	// Setup Surfaces caps for a front buffer and back buffer
    ddsd.dwSize				= sizeof(ddsd);
    ddsd.dwFlags			= DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
    ddsd.ddsCaps.dwCaps		= DDSCAPS_PRIMARYSURFACE | 
							  DDSCAPS_FLIP | 
							  DDSCAPS_COMPLEX |
							  DDSCAPS_3DDEVICE;		// Create a D3D compatible surface
    ddsd.dwBackBufferCount	= 1;			

	// Create Primary surface
    hResult = lpDD2->CreateSurface (&ddsd, &lpddsPrimary, NULL);
    if (FAILED (hResult))
    {
        // Error
        REPORTERR (hResult);
        return hResult;
    }

	// Create and attach palette, if necessary
	hResult = InitPalette ();
	if (FAILED (hResult))
		return hResult;

	// Mark as Valid
	turnValidPrimaryOn ();

    // Success
    return DD_OK;
} // End D3DWindow::InitPrimary



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::FiniPrimary
** Purpose: Destroys the Primary Surface
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::FiniPrimary (void)
{
	// Mark as Invalid
	turnValidPrimaryOff ();

	// Cleanup palette
	FiniPalette ();

    // Release Primary Surface Object
    if (lpddsPrimary)
    {
        lpddsPrimary->Release ();
        lpddsPrimary = NULL;
    }

	// Success
	return DD_OK;
} // End D3DWindow::FiniPrimary



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::InitPalette
** Purpose: Creates a primary palette if necessary
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::InitPalette ()
{
    HRESULT             hResult;
    HDC                 hdc;
    DWORD               ii;
    DWORD               cbSize;
    DWORD               dwFlags;
    DDSURFACEDESC       ddsd;

    // Destroy old palette
    FiniPalette ();

    // Make sure we are properly intialized 
    // for this to work
    if ((! lpDD2) || (! lpddsPrimary))
    {
        // Error, need a valid DD interfac and a primary surface to continue
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
        return hResult;
    }

    // Get primary surface caps
    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    hResult = lpddsPrimary->GetSurfaceDesc(&ddsd);
    if (FAILED (hResult))
    {
        // Error
        REPORTERR (hResult);
        return hResult;
    }

    // Make sure it is a palettized surface
    if (! isPalettized (&(ddsd.ddpfPixelFormat)))
    {
        // Success, primary isn't palettized
		// So we don't need to create a palette
        return DD_OK;
    }

    // Create and save System palette
    hdc = GetDC (NULL);
    cPalette = GetDeviceCaps (hdc, SIZEPALETTE);
    if (cPalette)
    {
        if (cPalette > 256)
            cPalette = 256;

        // Get memory for system palette
        lppeSystem = new PALETTEENTRY[cPalette];
        if (! lppeSystem)
        {
			ReleaseDC (NULL, hdc);

            // Error, not enough memory
			hResult = APPERR_OUTOFMEMORY;
			REPORTERR (hResult);
			goto lblCLEANUP;
        }

		// Get Memory for current palette
		lppeCurr = new PALETTEENTRY[cPalette];
		if (! lppeCurr)
		{
			ReleaseDC (NULL, hdc);

            // Error, not enough memory
			hResult = APPERR_OUTOFMEMORY;
			REPORTERR (hResult);
			goto lblCLEANUP;

		}

        // Save system palette
        GetSystemPaletteEntries (hdc, 0, cPalette, lppeSystem);

        // Copy system palette to temporary values
        cbSize = cPalette * sizeof (PALETTEENTRY);
        CopyMemory (lppeCurr, lppeSystem, cbSize);
    }
	ReleaseDC (NULL, hdc);

    if (ddsd.ddpfPixelFormat.dwFlags & DDPF_PALETTEINDEXED1)
    {
        dwFlags = DDPCAPS_1BIT;

        // Only 2 palette entries, we need them all
        for (ii = 0; ii < 2; ii++)
            lppeCurr[ii].peFlags = D3DPAL_FREE | PC_RESERVED;

    }
    else if (ddsd.ddpfPixelFormat.dwFlags & DDPF_PALETTEINDEXED2)
    {
        // Only 4 palette entries, we need them all
        for (ii = 0; ii < 4; ii++)
            lppeCurr[ii].peFlags = D3DPAL_FREE | PC_RESERVED;

        dwFlags = DDPCAPS_2BIT;
    }
    else if (ddsd.ddpfPixelFormat.dwFlags & DDPF_PALETTEINDEXED4)
    {
        // Only 16 palette entries, we will save black and white
        // and keep the rest for ourselves.

        lppeCurr[0].peFlags = D3DPAL_READONLY;
        lppeCurr[15].peFlags = D3DPAL_READONLY;

        for (ii = 1; ii < 15; ii++)
            lppeCurr[ii].peFlags = D3DPAL_FREE | PC_RESERVED;

        dwFlags = DDPCAPS_4BIT;
    }
    else if (ddsd.ddpfPixelFormat.dwFlags & DDPF_PALETTEINDEXED8)
    {
        // 256 palette entries, we can afford to be nice
        // and save the first 10 and last 10 palette entries
        // for system use 
        for (ii = 0; ii < 10; ii++)
        {
            lppeCurr[ii].peFlags = D3DPAL_READONLY;
            lppeCurr[246+ii].peFlags = D3DPAL_READONLY;
        }

        for (ii = 10; ii < 246; ii++)
            lppeCurr[ii].peFlags = D3DPAL_FREE | PC_RESERVED;

        dwFlags = DDPCAPS_8BIT;        
    }
    else
    {
        // Error, programming (unknown palette type)
		hResult = APPERR_GENERIC;
		REPORTERR (hResult);
		goto lblCLEANUP;
    }

    // Create Primary Palette
    hResult = lpDD2->CreatePalette (dwFlags,
                                    lppeCurr,
                                    &lpddpPalette,
                                    NULL);
    if (FAILED (hResult))
    {
        REPORTERR (hResult);
        goto lblCLEANUP;
    }

    // Attach palette to primary surface
    hResult = lpddsPrimary->SetPalette (lpddpPalette);
    if (FAILED (hResult))
    {
        // Error
        REPORTERR (hResult);
        goto lblCLEANUP;
    }

    // Success
    return DD_OK;

lblCLEANUP:
	// Failure

	// Cleanup
	FiniPalette ();
	return hResult;
} // D3DWindow::InitPalette



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::FiniPalette
** Purpose: Destroys primary palette
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::FiniPalette (void)
{
    // Note:  Should we Detach Palette object from surfaces
    // No way to do this that I know of...
    
    // Cleanup up DD Palette object
    if (lpddpPalette)
    {
        lpddpPalette->Release ();
        lpddpPalette = NULL;
    }

	// Cleanup Current Palette
	if (lppeCurr)
	{
		delete [] lppeCurr;
		lppeCurr = NULL;
	}

    // Cleanup System Palette
    if (lppeSystem)
    {
        // Note:  Should we try and restore system palette here ?!?

        // Destroy system palette
        delete [] lppeSystem;
        lppeSystem = NULL;
    }

	cPalette = 0;

	// Success
	return DD_OK;
} // End FiniPalette


    
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::CreateRender
** Purpose: Creates the rendering surface and D3D device
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::CreateRender (LPGUID lpD3DGuid)
{
	HRESULT		 hResult;
	LPD3DDevInfo lpDevNew;
	LPDDModeInfo lpModeNew;

	// Check Initialization
	if ((! lpCurrDriver) || (! lpCurrMode))
	{
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
		return hResult;
	}

	// Validate D3D Device
	lpDevNew = ValidateDevice (lpCurrDriver, lpD3DGuid, NULL);
	if (! lpDevNew)
	{
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
		return hResult;
	}

	DWORD w, h, bpp, refresh;
	lpCurrMode->GetMode (w, h, bpp, refresh);

	// Validate Mode with this D3D Device
	lpModeNew = ValidateMode (lpCurrDriver, w, h, bpp, 0, lpDevNew);
	if (! lpModeNew)
	{
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
		return hResult;
	}

	// Do we need to Change the Mode as well
	// to stay in synch with the new D3D device ?!?
	if (lpModeNew != lpCurrMode)
	{
		// Save old mode
		LPDDModeInfo lpModeOld = lpCurrMode;

		// Try to switch modes
		lpCurrMode = lpModeNew;
		hResult = InitPrimary ();
		if (FAILED (hResult))
		{
			// Try to restore old mode and exit with error
			lpCurrMode = lpModeOld;
			InitPrimary ();
			return hResult;
		}

		// Successfully switched modes
	}
	
	// Save new D3D device
	lpCurrDevice = lpDevNew;

	// Create new Render surface (using new Device)
	hResult = InitRender ();
	if (FAILED (hResult))
		return hResult;

	// Success
	return DD_OK;
}



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::InitRender
** Purpose: Creates the rendering surface and D3D device
** Notes:
**
**	1.  Catch 22:  To creating a D3D device you need it's render surface
**		First.  But in order to create a render surface properly you need
**      to know the D3D device desc characteristics first.   Fortunately we 
**		already have this information.  Because we store it awy in the 
**		Driver Manager.
**
**  2.  This same catch 22 prevented us from enumerating Texture formats
**      easily in the Driver Manager.  So, we do it here once we have a 
**		valid D3D device.
**
** Basic Algorithm:
**	1. Get pointer to back buffer and use as render surface
**  2. Create Z-buffer (optional)
**	3. Create D3D Device (enumerate Texture Formats)
**	4. Create Viewport
**	5. InitScene (optional)
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::InitRender (void)
{
    HRESULT         hResult;
    DWORD           dwMemType;
    LPD3DDEVICEDESC lpDeviceDesc;
    DDSURFACEDESC   ddsd;
    DDSCAPS			ddscaps;
    DWORD           dwWidth, dwHeight, dwBPP;

	// Check Initialization
    if ((! hWindow) || (! IsWindow (hWindow)) ||
		(! lpCurrDevice) || (! lpCurrMode) || 
		(! lpDD2) || (! lpD3D) || (! lpddsPrimary))
    {
        // Error, Not initialized properly before calling this method
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
        return hResult;
    }

	// 
	// Step 1.  Grab the Render surface (back buffer) 
	//			from the Primary surface (front buffer)
	//
   	ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
	hResult = lpddsPrimary->GetAttachedSurface (&ddscaps, &lpddsRender);
   	if (FAILED (hResult))
	{
		REPORTERR (hResult);
		return hResult;
	}

	
	//
	// Step 2.	Create and attach Z-buffer (optional)
	//
	
	// Get D3D Device description
	if (lpCurrDevice->isHardware ())
	{
		// Hardware device
		// Z-buffer has to be in Video memory
		lpDeviceDesc = &(lpCurrDevice->d3dHalDesc);
		dwMemType = DDSCAPS_VIDEOMEMORY;
	}
	else
	{
		// Software device 
		// Z-Buffer has to be in System memory
		lpDeviceDesc = &(lpCurrDevice->d3dHelDesc);
		dwMemType = DDSCAPS_SYSTEMMEMORY;
	}

	// Should we create a Z-buffer ?!?
    if ((isCreateZBuffer ()) && (lpDeviceDesc) &&
        (0L != lpDeviceDesc->dwDeviceZBufferBitDepth))
    {
		// Get Z-buffer surface info (w, h, bpp, video vs. system memory)
		ddsd.dwSize = sizeof(ddsd);
		hResult = lpddsPrimary->GetSurfaceDesc (&ddsd);
		if (FAILED (hResult))
		{
			REPORTERR (hResult);
			return hResult;
		}
		dwWidth   = ddsd.dwWidth;
		dwHeight  = ddsd.dwHeight;
		dwBPP	  = FlagsToBitDepth (lpDeviceDesc->dwDeviceZBufferBitDepth);

        // Create the z-buffer.
        ZeroMemory (&ddsd, sizeof(ddsd));
        ddsd.dwSize            = sizeof(ddsd);
        ddsd.dwFlags           = DDSD_CAPS   |
                                 DDSD_WIDTH  |
                                 DDSD_HEIGHT |
                                 DDSD_ZBUFFERBITDEPTH;
        ddsd.ddsCaps.dwCaps    = DDSCAPS_ZBUFFER | dwMemType;
        ddsd.dwWidth           = dwWidth;
        ddsd.dwHeight          = dwHeight;
        ddsd.dwZBufferBitDepth = dwBPP;
        hResult = lpDD2->CreateSurface (&ddsd, &lpddsZBuff, NULL);
        if (FAILED(hResult))
        {
            REPORTERR (hResult);

            // Note: we may be able to continue without a z buffer
			// So don't exit
        }
		else
		{
			// Attach Z-buffer to rendering surface
			hResult = lpddsRender->AddAttachedSurface (lpddsZBuff);
			if (FAILED (hResult))
			{
				REPORTERR (hResult);

				if (lpddsZBuff)
				{
					lpddsZBuff->Release ();
					lpddsZBuff = NULL;
				}

				// Note: we may be able to continue without a z buffer
				// So don't exit
			}
			//else
			//{
			// Note:  We could actually release the reference to the
			//        Z-Buffer here, if we don't need a pointer to it anymore.
			//        The back-buffer has an attachment to it now and it
			//        won't go away until the Back-buffer does.
			// lpddsZBuff->Release ();
			// lpddsZBuff = NULL;
			//}
		}
    }


    //
    // Step 4.  Create the D3D device interface
	//
	hResult = lpD3D->CreateDevice (lpCurrDevice->guid,
				 				   lpddsRender, 
							 	   &lpd3dDevice);
    if (FAILED (hResult))
    {
        REPORTERR (hResult);
        return hResult;
    }

	// Enumerate all Texture formats associated with this D3D device
	hResult = lpCurrDevice->LoadFormats (lpd3dDevice);
	if (FAILED (hResult))
	{
		// Error, no texture formats
		// Hope we can run OK without textures
	}

	// Mark as valid
	turnValidRenderOn ();


	//
	// Step 5.  Create the viewport
	//
	hResult = InitViewport ();
	if (FAILED (hResult))
		return hResult;
	

	// 
	// Step 6.	Allow Scene to create all objects dependent on us
	//
	if (lpd3dScene)
		hResult = lpd3dScene->Attach ();

    // Success
    return DD_OK;
} // End D3DWindow::InitRender



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::FiniRender
** Purpose: Destroys the Rendering surface
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::FiniRender (void)
{
	// Allow Scene to cleanup all objects dependent on us
	if (lpd3dScene)
		lpd3dScene->Detach ();

	// Cleanup viewport
	FiniViewport ();

	// Mark as invalid
	turnValidRenderOff ();

    // Release D3D Device
    if (lpd3dDevice)
    {
        lpd3dDevice->Release ();
        lpd3dDevice = NULL;
    }

    // Release Z Buffer
    if (lpddsZBuff)
    {
		// Detach Z-Buffer from back buffer
		if (lpddsRender)
			lpddsRender->DeleteAttachedSurface (0L, lpddsZBuff);

		// Release Z-Buffer
        lpddsZBuff->Release ();
        lpddsZBuff = NULL;
    }

    // Release rendering surface
    if (lpddsRender)
    {
        lpddsRender->Release ();
        lpddsRender = NULL;
    }

	// Success
	return DD_OK;
} // End D3DWindow::FiniRender



/*
**-----------------------------------------------------------------------------
**  Name:       D3DWindow::InitViewport
**  Purpose:    
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::InitViewport (void)
{
	HRESULT hResult;

	// Check Initialization
	if ((! lpD3D) || (! lpd3dDevice))
	{
		// Error, Not properly initialized before calling this method
		hResult = APPERR_NOTINITIALIZED;
        REPORTERR (hResult);
        return hResult;
	}

	// Create Viewport
    hResult = lpD3D->CreateViewport (&lpd3dViewport, NULL);
    if (FAILED (hResult))
    {
        REPORTERR (hResult);
        return hResult;
    }

	// Attach viewport to D3D device
    hResult = lpd3dDevice->AddViewport (lpd3dViewport);
    if (FAILED (hResult))
    {
		lpd3dViewport->Release ();
		lpd3dViewport = NULL;

        REPORTERR (hResult);
        return hResult;
    }

	// Set up Initial Viewport parameters
	hResult = UpdateViewport ();
    if (FAILED (hResult))
	{
		lpd3dDevice->DeleteViewport (lpd3dViewport);
		lpd3dViewport->Release ();
		lpd3dViewport = NULL;

        return hResult;
	}

	// Mark as valid
	turnValidViewportOn ();

	/// Success
	return DD_OK;
} // End D3DWindow::InitViewport
  


/*
**-----------------------------------------------------------------------------
**  Name:       D3DWindow::FiniViewport
**  Purpose:    Cleanup viewport
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::FiniViewport (void)
{
	// Mark as invalid
	turnValidViewportOn ();

	// Release D3D viewport
    if (lpd3dViewport)
    {
		lpd3dDevice->DeleteViewport (lpd3dViewport);
        lpd3dViewport->Release ();
        lpd3dViewport = NULL;
    }

	// Success
	return DD_OK;
} // End D3DWindow::FiniViewport

  

/*
**-----------------------------------------------------------------------------
**  Name:       D3DWindow::UpdateViewport
**  Purpose:    Keeps viewport updated with current window size
**  Notes:		
**
**	1. The viewport construction here assumes that you are rendering 
**		Triangles using the D3DVERTEX and D3DIM is doing Transform,
**		lighting, and rasterization for you.
**
**	2. If you are rendering triangles using D3DTLVERTEX and doing your
**	   own transform and lighting then you need to setup the viewport
**	   differently.   As follows:
**
**      // Replace the following values below:
**		dvClipX			= 0.0f;
**		dvClipY			= 0.0f;
**		dvClipWidth		= dwSurfW;
**		dvClipHeight	= dvSurfH;
**
**  3. This perserves the aspect ratio.  If you don't need or want to
**     perserve the aspect ratio then set inv_aspect = 1.0 below and
**     work this constant through the rest of the viewport setup.
**
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::UpdateViewport (void)
{
    HRESULT			hResult;
    D3DVIEWPORT2	d3dViewport;
	DWORD			dwSurfW, dwSurfH;

    // Check Parameters
	if ((! lpd3dDevice) || (! lpd3dViewport))
	{
		// Not properly initialized before calling this method
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
		return hResult;
	}

	// Get Surface Width and Height
	dwSurfW = abs (rSurf.right - rSurf.left);
	dwSurfH = abs (rSurf.bottom - rSurf.top);

	float inv_aspect;
	
	if (dwSurfW) {
		inv_aspect = (float)dwSurfH/(float)dwSurfW;
	} else {
		inv_aspect = 1.0f;
	}

    // Update Viewport
    ZeroMemory (&d3dViewport, sizeof(d3dViewport));
    d3dViewport.dwSize		= sizeof(d3dViewport);     // Always set size of structure!!!
    d3dViewport.dwX			= 0UL;
    d3dViewport.dwY			= 0UL;
    d3dViewport.dwWidth		= dwSurfW;
    d3dViewport.dwHeight	= dwSurfH;
    d3dViewport.dvClipX		= -1.0f;
    d3dViewport.dvClipY		= inv_aspect;
    d3dViewport.dvClipWidth	= 2.0f;
    d3dViewport.dvClipHeight = 2.0f * inv_aspect;
    d3dViewport.dvMinZ		= 0.0f;
    d3dViewport.dvMaxZ		= 1.0f;

	// Update Viewport
    hResult = lpd3dViewport->SetViewport2 (&d3dViewport);
    if (FAILED (hResult))
    {
        REPORTERR (hResult);
        return hResult;
    }

	// Update D3D device to use this viewport
    hResult = lpd3dDevice->SetCurrentViewport (lpd3dViewport);
    if (FAILED (hResult))
    {
        REPORTERR (hResult);
        return hResult;
    }

    // Success
    return DD_OK;
} // End D3DWindow::UpdateViewport

  

/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::DrawFrame
** Purpose: Paints current surface to window
** Notes:   
**
**	1. Full screen mode - we render to the back buffer
**	   and then flip the front and back buffers to make the
**	   changes visible
**
**  2. Windowed mode - We blt the render surface onto the primary surface
**
**  3. This routine is not written for optimal performance
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::DrawFrame (void)
{
    HRESULT hResult = DD_OK;

	// Check Initialization
    if (! isValid ())
    {
        // Error, not properly initialized
		hResult = APPERR_NOTINITIALIZED;
		//REPORTERR (hResult);
        return hResult;
    }

    if (isPaused ())
    {
        // Don't draw, if paused
        return DD_OK;
    }
    
    // Paint until we truly succeed or error out
    while (TRUE)
    {
		// Render D3D Scene
		if (lpd3dScene)
			hResult = lpd3dScene->Render ();

		if (SUCCEEDED (hResult))
		{
			// Flip Front and Back buffers (Primary, Render)
		    hResult =lpddsPrimary->Flip (lpddsRender, 1);

			// Did it work ?!?  
			if (SUCCEEDED (hResult))
			{
				// Success, exit
				return hResult;
			}
		}

        // Check if busy or drawing
        if ((DDERR_SURFACEBUSY == hResult) ||
            (DDERR_WASSTILLDRAWING == hResult))
        {
            // Try again
            continue;
        }

        // Check for lost surfaces
        while (DDERR_SURFACELOST == hResult)
        {
            // Restore surfaces
            hResult = Restore ();
        }

        // Check for real error
        if (FAILED (hResult))
        {
            // Error,
            REPORTERR (hResult);
            return hResult;
        }
    }

    // Success
    return DD_OK;
} // End D3DWindow::DrawFrame



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::Move
** Purpose: Moving full-screen windows not supported
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::Move (long x, long y)
{
	// Do nothing

    // Success
    return DD_OK;
} // D3DWindow::Move



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::Resize
** Purpose: Resizes window
** Notes:	Resizing Fullscreen windows not supported.
**			I.E. The Back buffer needs to stay the same size as the front
**			Buffer.
**
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::Resize (DWORD dwWidth, DWORD dwHeight)
{
	// Do nothing

	// Success
	return DD_OK;
} // End D3DWindow::Resize 



/*
**-----------------------------------------------------------------------------
**  Name:       D3DWindow::RealizePalette
**  Purpose:    
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::RealizePalette (void)
{
	HRESULT hResult;

	//
    // Realizing the palette using DirectDraw is quite different
    // from GDI. To realize the palette we call SetPalette()
    // each time our application is activated.
    //
    // NOTE: DirectDraw spots the fact that the new palette is the
    // same as the old one and so does not increase the reference
    // count of the palette.
    //

	if ((lpddsPrimary) && (lpddpPalette))
	{
		hResult = lpddsPrimary->SetPalette (lpddpPalette);
		if (FAILED (hResult))
        {
            REPORTERR (hResult);
            return hResult;
        }
	}

	// Success
	return DD_OK;
} // End D3DWindow::RealizePalette



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::toGDI
** Purpose: Setups for drawing to GDI surface
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::toGDI (void)
{
	HRESULT hResult;

	// Step 1. Restore system palette (optional)
	if (lpddpPalette) 
	{
		// Save the current palette
		hResult = lpddpPalette->GetEntries (0, 0, cPalette, lppeCurr);
		if (FAILED (hResult))
		{
			REPORTERR (hResult);
			return hResult;
		}

		// Restore the system palette into our device
		hResult = lpddpPalette->SetEntries (0, 0, cPalette, lppeSystem);
		if (FAILED (hResult))
		{
			REPORTERR (hResult);
			return hResult;
		}
	}

	// Step 2. Flip to GDI Surface
	if (lpDD2) 
	{
		hResult = lpDD2->FlipToGDISurface ();
		if (FAILED (hResult))
		{
			REPORTERR (hResult);
			return hResult;
		}
	}

	// Step 3.  Force window to redraw itself (on GDI surface)
	if ((hWindow) && (IsWindow (hWindow)))
	{
		DrawMenuBar (hWindow);
		RedrawWindow (hWindow, NULL, NULL, RDW_FRAME);
	}

	// Success
	return DD_OK;
} // End D3DWindow::toGDI



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::fromGDI
** Purpose: Restores from GDI
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::fromGDI (void)
{
	HRESULT hResult;

	// Restore current palette
	if (lpddpPalette)
	{
		hResult = lpddpPalette->SetEntries (0, 0, cPalette, lppeCurr);
		if (FAILED (hResult)) 
			return hResult;
	}

	// Success
	return DD_OK;
} // End D3DWindow::fromGDI



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::Pause
** Purpose: Pause any work on DD/D3D resources
** Notes:	
**
** For Fullscreen apps we need to setup the 
**
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::Pause (BOOL fPause)
{
	HRESULT hResult;

    // Turning pausing on/off ?!?
    if (fPause)
    {
		// Increment pause semaphore
        dwPaused++;

		// Are we pausing for the first time
        if (dwPaused == 1L)
        {
			// Flip to GDI surface
			hResult = toGDI ();
			if (FAILED (hResult))
				return hResult;
        }

    }
    else
    {
        if (dwPaused == 0L)
        {
            // Programmer Error, already unpaused
			hResult = APPERR_GENERIC;
			REPORTERR (hResult);
            return hResult;
        }

		// Are we unpausing for the last time ?!?
        if (dwPaused == 1L)
        {
			// Restore from GDI surface
			hResult = fromGDI ();
			if (FAILED (hResult))
				return hResult;
        }

        // Decrement pause semaphore
        dwPaused--;
    }

    // Success
    return DD_OK;
} // D3DWindow::Pause
  


/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::AttachScene
** Purpose: 
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::AttachScene (LPD3DScene lpNewScene)
{
	// Check parameters
	if (! lpNewScene)
	{
		return DDERR_INVALIDPARAMS;
	}

	// Save Scene pointer
	lpd3dScene = lpNewScene;

	// Inform scene of our existence
	lpd3dScene->Init (this);

	// Allow scene to create any objects dependent on us
	if (isValid ())
	{
		lpd3dScene->Attach ();
	}

	// Success
	return DD_OK;
} // End D3DWindow::AttachScene



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::DetachScene
** Purpose: 
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::DetachScene (void)
{
	// Cleanup Scene
	if (lpd3dScene)
	{
		lpd3dScene->Detach ();
		lpd3dScene->Fini ();
		lpd3dScene = NULL;
	}

	// Success
	return DD_OK;
} // End D3DWindow::DetachScene


  
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::Restore
** Purpose: Restores lost surfaces
** Note:    Eventually we should inform the user somehow that
**          they need to redraw the surface but for now punt
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::Restore (void)
{
    HRESULT hResult;

	// Check Initialization
    if (! isValid ())
	{
		// Error, not properly initialized before calling this method
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
        return hResult;
	}

    // Restore Primary Surface
    if (lpddsPrimary)
    {
		hResult = lpddsPrimary->IsLost ();
		if (hResult != DD_OK)
		{
			hResult = lpddsPrimary->Restore ();
			if (FAILED (hResult))
				return hResult;
		}
    }

    // Restore Z Buffer
    if (lpddsZBuff)
    {
		hResult = lpddsZBuff->IsLost ();
		if (hResult != DD_OK)
		{
	        hResult = lpddsZBuff->Restore ();
		    if (FAILED (hResult))
			    return hResult;
		}
    }

    // Restore Rendering surface
    if (lpddsRender)
    {
		hResult = lpddsRender->IsLost ();
		if (hResult != DD_OK)
		{
	        hResult = lpddsRender->Restore ();
		    if (FAILED (hResult))
			    return hResult;
		}
    }

	// Allow D3D Scene to restore any surfaces
	if (lpd3dScene)
	{
		hResult = lpd3dScene->Restore ();
		if (FAILED (hResult))
			return hResult;
	}

    // Success
    return DD_OK;
} // End D3DWindow::Restore


  
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::GetSurfaceRect
** Purpose: Get bounding rectangle of surface
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::GetSurfaceRect (RECT & rSurface)
{
	HRESULT hResult;

    if (! isValid ())
    {
		// Error, not properly initialized before calling this method
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
        return hResult;
    }

    // Return Surface rectangle
    rSurface = rSurf;

    // Success
    return DD_OK;
} // D3DWindow::GetSurfaceRect



/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::GetPrimaryRect
** Purpose: Get bounding rectangle of primary surface
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::GetPrimaryRect (RECT & rPrimary)
{
	HRESULT hResult;

    if (! isValid ())
    {
		// Error, not properly initialized before calling this method
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
        return hResult;
    }

    // Return Primary rectangle
    rPrimary = rPrim;

    // Success
    return DD_OK;
} // GetPrimaryRect
  

  
/*
**-----------------------------------------------------------------------------
**  Name:       D3DWindow::ChangeDesktop
**  Purpose:    The Primary Desktop has changed Modes
**	Notes:		
**
**	1.  Since we are a fullscreen app, this shouldn't effect us
**		until we restore the desktop mode.  Since DirectDraw should
**		be tracking this anyway.  We can just ignore doing anything
**
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::ChangeDesktop (void)
{
	// Do nothing

	// Success 
	return DD_OK;
} // D3DInfo::ChangeDesktop
  


  
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::ChangeDriver
** Purpose: 
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::ChangeDriver (
	LPGUID		 lpGuidDD,
	LPD3DDevInfo lpDevHint,
	LPDDModeInfo lpModeHint)
{
	HRESULT		 hResult;
	LPGUID		 lpGuidD3D;
	LPDDDrvInfo  lpDrvNew,  lpDrvOld;
	LPDDModeInfo lpModeNew, lpModeOld;
	LPD3DDevInfo lpDevNew, lpDevOld;
	DWORD		 w, h, bpp, refresh;

	// Get New Driver
	lpDrvNew = ValidateDriver (lpGuidDD);
	if (! lpDrvNew)
	{
		// Error, invalid DD Guid
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
        return hResult;
	}

	// Get requested D3D device
	if (lpDevHint)
		lpGuidD3D = &(lpDevHint->guid);
	else if (lpCurrDevice)
		lpGuidD3D = &(lpCurrDevice->guid);
	else
		lpGuidD3D = NULL;

	// Get requested mode
	if (lpModeHint)
		lpModeHint->GetMode (w, h, bpp, refresh);
	else
	{
		// Default to 640 x 480 x 16
		w		= 640;
		h		= 480;
		bpp		= 16;
		refresh = 0;
	}

	// Get new device and mode compatible with this driver
	if (! GetFullscreenMode (lpDrvNew, lpGuidD3D, w, h, bpp, refresh,
							 &lpModeNew, &lpDevNew))
	{
		// Error, unable to find a valid D3D Device
		// and Mode that work with this driver.
		hResult = APPERR_GENERIC;
		REPORTERR (hResult);
		return hResult;
	}
	
	// Save old defaults
	lpDrvOld	= lpCurrDriver;
	lpModeOld	= lpCurrMode;
	lpDevOld	= lpCurrDevice;

	// Destroy almost everything
	Fini ();

	// Set new defaults
	lpCurrDriver = lpDrvNew;
	lpCurrMode   = lpModeNew;
	lpCurrDevice = lpDevNew;

	// Re-create almost everything based on new driver, device, and mode
	hResult = Init ();
	if (FAILED (hResult))
	{
		// Try to restore old defaults
		Fini ();

		lpCurrDriver = lpDrvOld;
		lpCurrMode   = lpModeOld;
		lpCurrDevice = lpDevOld;

		Init ();
		return hResult;
	}

	// Notify the window of a successful change in Driver
	SendMessage (hWindow, D3DWIN_CHANGED_DRIVER, 0, 0);

	// Success
    return DD_OK;
} // End D3DWindow::ChangeDriver


  
/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::ChangeMode
** Purpose: Change the mode
**
**	Basic Algorithm:
**
**  1.  Validate new mode request
**  2.  Validate that the D3D device is compatible
**  3.  Destroy old mode
**  4.  Create new mode
**
**	- This can be complicated by the fact that the new mode is not
**    compatible with the current D3D device, in which case we need
**    to choose a new D3D device that will work with this mode.
**
**  - We don't normally allow mode changes for windowed mode.
**		- However the desktop mode could have changed underneath us,
**		  So make sure the desktop mode and our current mode match.
**		  If not, then change to the current desktop mode.
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::ChangeMode (
	DWORD w,			// Mode Width
	DWORD h,			// Mode Height
	DWORD bpp,			// Mode Bits Per Pixel
	DWORD refresh)		// Optional:  Mode refresh rate
{
	HRESULT		 hResult;
	LPDDDrvInfo  lpOldDrv;
	LPDDModeInfo lpOldMode, lpNewMode;
	LPD3DDevInfo lpOldDev, lpNewDev;

	// Check Initialization
	if ((! hWindow) || (! IsWindow (hWindow)))
	{
		// Error, Not properly initialized
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
		return hResult;
	}

	lpOldDrv  = lpCurrDriver;
	lpOldMode = lpCurrMode;
	lpOldDev  = lpCurrDevice;


	//
	// Step 1. Get New Mode
	//

	// Find new mode corresponding to w, h, bpp
	lpNewMode = lpOldDrv->FindMode (w, h, bpp, 0, NULL);
	if (! lpNewMode)
	{
		// Error, Invalid Mode parameters
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
		return hResult;
	}

	// 
	// Step 2.   Check if Device needs to be changed as well
	//
	if (lpNewMode->ModeSupported (lpOldDev))
	{
		lpNewDev = NULL;
	}
	else
	{
		LPD3DDevInfo lpNextBest;
		lpNewDev = lpOldDrv->FindDeviceSupportsMode (&lpOldDev->guid,
													 lpNewMode,
													 &lpNextBest);
		if (! lpNewDev)
		{
			if (! lpNextBest)
			{
				// No D3D device is compatible with this new mode
				hResult = APPERR_GENERIC;
				REPORTERR (hResult);
				return hResult;
			}
			lpNewDev = lpNextBest;
		}
	}

	// 
	// Step 3.	Destroy current Mode
	//
	FiniRender ();
	FiniPrimary ();
//  FiniFullscreenMode ();		// Don't do this => unnecessary mode switch

	//
	// Step 4.  Create new mode
	//
	lpCurrMode = lpNewMode;
	if (lpNewDev)
		lpCurrDevice = lpNewDev;

	// Change Mode
	hResult = InitFullscreenMode ();
	if (FAILED (hResult))
		return hResult;

	// Create Primary Surface
	hResult = InitPrimary ();
	if (FAILED (hResult))
	{
		// Try to restore old mode
		lpCurrMode	 = lpOldMode;
		lpCurrDevice = lpOldDev;

		InitFullscreenMode ();
		InitPrimary ();
		InitRender ();

		return hResult;
	}

	// Create Render surface
	hResult = InitRender ();
	if (FAILED (hResult))
	{
		FiniPrimary ();
	//  FiniFullscreenMode ();		// Unnecessary mode switch

		// Try to restore old mode
		lpCurrMode	 = lpOldMode;
		lpCurrDevice = lpOldDev;

		InitFullscreenMode ();
		InitPrimary ();
		InitRender ();

		return hResult;
	}

	// Notify the window of a successful change in Mode
	SendMessage (hWindow, D3DWIN_CHANGED_MODE, 0, 0);

	// Success
    return DD_OK;
} // End D3DWindow::ChangeMode

  

/*
**-----------------------------------------------------------------------------
** Name:    D3DWindow::ChangeDevice
** Purpose: Change to a new D3D device (RAMP, RGB, Hardware, etc.)
** Notes:
**
**  Algorithm:
**		- Destroy the current D3D Device (and associated surfaces)
**		- Recreate a new D3D device from the new GUID
**
**  1.	The new D3D Device may not be supported by the current DD Device.
**  2.  The new D3D Device may not be compatible with the current Mode
**		- Since we are fullscreen, just pick a new mode that is compatible
**
**-----------------------------------------------------------------------------
*/

HRESULT D3DWindow::ChangeDevice (
	LPGUID		 lpD3DGuid,
	LPDDModeInfo lpModeHint)
{
    HRESULT         hResult;
	LPDDDrvInfo		lpDrvOld;
	LPDDModeInfo    lpModeNew, lpModeOld;
	LPD3DDevInfo	lpDevNew, lpDevOld;

	// Check Parameters
	if (! lpD3DGuid)
	{
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
        return hResult;
	}
	
	// Check Initialization
    if (! isValid () || (! lpddsRender))
	{
		hResult = APPERR_NOTINITIALIZED;
		REPORTERR (hResult);
        return hResult;
	}

	// Save Original State
	lpDrvOld	= lpCurrDriver;
	lpModeOld   = lpCurrMode;
	lpDevOld	= lpCurrDevice;

	// Verify new D3D device belongs to current DD driver
	lpDevNew = lpDrvOld->FindDevice (lpD3DGuid, NULL);
	if (! lpDevNew)
	{
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
        return hResult;
	}

	//
	//	Step 1. Verify new D3D device is supported with current mode
	//
	if (lpModeHint)
		lpModeNew = lpModeHint;
	else
		lpModeNew = lpModeOld;
	if (! lpModeNew->ModeSupported (lpDevNew))
	{
		// We are a full screen app, so we can do what we want
		// Pick a new mode that is compatible with this device
		LPDDModeInfo lpNextBest;
		DWORD w, h, bpp, refresh;

		lpModeNew->GetMode (w, h, bpp, refresh);

		lpModeNew = lpDrvOld->FindModeSupportsDevice (w, h, bpp, 0,
													  lpDevNew,
													  &lpNextBest);
		if (! lpModeNew)
		{
			if (! lpNextBest)
			{
				// Error , no compatible mode found!!!
				hResult = APPERR_GENERIC;
				REPORTERR (hResult);
				return hResult;
			}
			lpModeNew = lpNextBest;
		}
	}
	if (lpModeNew == lpModeOld)
		lpModeNew = NULL;


	//
	//	Step 2.  Destroy Old D3D Device (and mode)
	//
	FiniRender ();
	if (lpModeNew)
	{
		FiniPrimary ();
		// FiniFullscreenMode ();	// Unnecessary mode switch
	}


	//
	//	Step 3. Create new D3D Device (new mode optional)
	//

	// Set new D3D device (and mode)
	if (lpModeNew)
		lpCurrMode = lpModeNew;
	lpCurrDevice = lpDevNew;

	// Create new mode, if necessary
	if (lpModeNew)
	{
		// Change Mode
		hResult = InitFullscreenMode ();
		if (FAILED (hResult))
		{
			// Try to restore original mode and device
			lpCurrDevice = lpDevOld;
			lpCurrMode   = lpModeOld;

			InitFullscreenMode ();
			InitPrimary ();
			InitRender ();

			return hResult;
		}

		// Create Primary
		hResult = InitPrimary ();
		if (FAILED (hResult))
		{
			// Try to restore original mode and device
			lpCurrDevice = lpDevOld;
			lpCurrMode   = lpModeOld;

			InitFullscreenMode ();
			InitPrimary ();
			InitRender ();

			return hResult;
		}
	}

	// Create new D3D Device
	hResult = InitRender ();
	if (FAILED (hResult))
	{
		// Try to restore original mode and device
		if (lpModeNew)
			lpCurrMode = lpModeOld;
		lpCurrDevice = lpDevOld;

		if (lpModeNew)
		{
			FiniPrimary ();
			InitFullscreenMode ();
			InitPrimary ();
		}
		
		InitRender ();

		// Return Error
		REPORTERR (hResult);
		return hResult;
	}

	// Notify the window of a successful change in device
	SendMessage (hWindow, D3DWIN_CHANGED_DEVICE, 0, 0);

    // Success
    return DD_OK;
} // End D3DWindow::ChangeDevice



/*
**-----------------------------------------------------------------------------
** End of File
**-----------------------------------------------------------------------------
*/
