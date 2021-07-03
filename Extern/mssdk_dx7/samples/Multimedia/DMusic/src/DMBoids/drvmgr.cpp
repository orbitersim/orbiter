/*
**-----------------------------------------------------------------------------
**  Name:       DrvMgr.cpp
**  Purpose:    Implements DD/D3D Driver/Device enumeration
**
**  Copyright (c) 1995-1999 by Microsoft, all rights reserved.
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Includes
**-----------------------------------------------------------------------------
*/

#include "DrvMgr.h"
#include "Debug.h"



/*
**-----------------------------------------------------------------------------
**  Typedefs
**-----------------------------------------------------------------------------
*/

// Used with Enumeration Callbacks
typedef struct tagDD_CB_INFO 
{
    BOOL	  fResult;	    // Success/Failure
    DWORD	  cCount;       // Current count
    void *    lpExtra;		// Current Driver/Device/Etc.
} DD_CB_INFO;
typedef DD_CB_INFO * LPDD_CB_INFO;



/*
**-----------------------------------------------------------------------------
**  Global Variables
**-----------------------------------------------------------------------------
*/

// Global DDDrvMgr data
DWORD       DDDrvMgr::g_fFlags			= 0L;
DWORD		DDDrvMgr::g_cDrivers		= 0L;
LPDDDrvInfo DDDrvMgr::g_lpDriverRoot	= NULL;
LPDDDrvInfo DDDrvMgr::g_lpDriverTail	= NULL;



/*
**-----------------------------------------------------------------------------
**  Local Prototypes
**-----------------------------------------------------------------------------
*/

BOOL WINAPI DriverEnumCallback (LPGUID lpGuid, LPTSTR lpDesc, 
                                LPTSTR lpName, LPVOID lpExtra);

HRESULT WINAPI ModeEnumCallback (LPDDSURFACEDESC lpDDSurfDesc,
                                 LPVOID lpExtra);

HRESULT WINAPI TextureFormatEnumCallback (LPDDSURFACEDESC lpTexFormatDesc,
										  LPVOID lpExtra);

HRESULT WINAPI DeviceEnumCallback (LPGUID lpGuid, 
								   LPTSTR lpName,  
								   LPTSTR lpDesc,
                                   LPD3DDEVICEDESC lpHalDevice,
                                   LPD3DDEVICEDESC lpHelDevice, 
                                   LPVOID lpExtra);

    
/*
**-----------------------------------------------------------------------------
**  Functions
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
** Name:    ValidateDriver
** Purpose: Find DD driver matching user choice
**			or returns the primary driver on failure
**-----------------------------------------------------------------------------
*/

LPDDDrvInfo ValidateDriver (LPGUID lpDDGuid)
{
	LPDDDrvInfo lpDrvNew, lpDrvNext;

    // Find Driver matching specified GUID
    lpDrvNew = DDDrvMgr::FindDriver (lpDDGuid, &lpDrvNext);
    if (lpDrvNew)
	{
		// Exact match
		return lpDrvNew;
	}

	// Return next best match (or failure)
	return lpDrvNext;
} // End ValidateDriver

  

/*
**-----------------------------------------------------------------------------
** Name:    ValidateMode
** Purpose: Find DD mode matching user choice (w,h,bpp)
** Notes:	filters modes against device, if specified.
**-----------------------------------------------------------------------------
*/

LPDDModeInfo ValidateMode (
	LPDDDrvInfo	 lpDriver,  /* In:  DD Driver */
	DWORD		 w,			/* In:  width, height, bpp */
	DWORD		 h,
	DWORD		 bpp,
	DWORD		 refresh,
	LPD3DDevInfo lpFilter)	/* In:	Device used as filter */
{
	LPDDModeInfo lpModeNew, lpModeNext;

	// Check Parameters
	if (! lpDriver)
		return FALSE;

	if (! lpFilter)
		lpModeNew = lpDriver->FindMode (w, h, bpp, refresh, &lpModeNext);
	else
	{
		// Filter mode against D3D device compatiblity
		lpModeNew = lpDriver->FindModeSupportsDevice (w, h, bpp, refresh,
													  lpFilter,
													  &lpModeNext);
	}

    if (lpModeNew)
	{
		// Exact match
		return lpModeNew;
	}

	// Return next best match (or failure)
	return lpModeNext;
} // End ValidateMode


  
  
/*
**-----------------------------------------------------------------------------
** Name:    ValidateDevice
** Purpose: Find D3D device matching user choice
** Notes:	Filters devices against mode, if specified.
**-----------------------------------------------------------------------------
*/

LPD3DDevInfo ValidateDevice(
	LPDDDrvInfo	 lpDriver,
	LPGUID		 lpD3DGuid,
	LPDDModeInfo lpFilter)
{
	LPD3DDevInfo lpDevNew, lpDevNext;

	// Check Parameters
	if (! lpDriver)
		return FALSE;

	if (! lpFilter)
	{
		lpDevNew = lpDriver->FindDevice (lpD3DGuid, &lpDevNext);
	}
	else
	{
		// Filter device against mode
		lpDevNew = lpDriver->FindDeviceSupportsMode (lpD3DGuid, 
			  									     lpFilter,
													 &lpDevNext);
	}

    if (lpDevNew)
	{
		// Exact match
		return lpDevNew;
	}

	// Return next best match (or failure)
	return lpDevNext;
} // End ValidateDevice

  

  
/*
**-----------------------------------------------------------------------------
** Name:    ValidateFormat
** Purpose: Find texture format matching user choice (ddpfPixelformat)
** Notes:	filters modes against device, if specified.
**-----------------------------------------------------------------------------
*/

LPDDModeInfo ValidateFormat (
	LPD3DDevInfo lpDevice,  /* In:  D3D Device */
	DWORD		 bpp)
{
	LPDDModeInfo lpFormatNew, lpFormatNext;

	// Check Parameters
	if (! lpDevice)
		return FALSE;

	lpFormatNew = lpDevice->FindFormat (bpp, &lpFormatNext);

    if (lpFormatNew)
	{
		// Exact match
		return lpFormatNew;
	}

	// Return next best match (or failure)
	return lpFormatNext;
} // End ValidateFormat


  
/*
**-----------------------------------------------------------------------------
** Name:    ValidateFormat
** Purpose: Find texture format matching user choice (ddpfPixelformat)
** Notes:	filters modes against device, if specified.
**-----------------------------------------------------------------------------
*/

LPDDModeInfo ValidateFormat (
	LPD3DDevInfo lpDevice,  /* In:  D3D Device */
	LPDDPIXELFORMAT lpddpf)
{
	LPDDModeInfo lpFormatNew, lpFormatNext;

	// Check Parameters
	if (! lpDevice)
		return FALSE;

	lpFormatNew = lpDevice->FindFormat (lpddpf, &lpFormatNext);

    if (lpFormatNew)
	{
		// Exact match
		return lpFormatNew;
	}

	// Return next best match (or failure)
	return lpFormatNext;
} // End ValidateFormat
  


/*
**-----------------------------------------------------------------------------
** Name:    GetDesktopMode
** Purpose: Find DD mode corresponding to desktop 
**			and a compatible D3D device.
** Notes:	
**
**	1.  We have no choice, we are stuck with the current desktop mode.
**  2.  Since we can't change the mode, we must choose a D3D device
**		that is compatible with the current mode.
**
**-----------------------------------------------------------------------------
*/

BOOL GetDesktopMode (
	LPDDDrvInfo	 lpDriver,	/* In:   Driver */
	LPGUID		 lpD3DGuid, /* In:   Requested D3D device guid */
	LPDDModeInfo * lpMode,	/* Out:  Desktop Mode */
	LPD3DDevInfo * lpDev)	/* Out:  D3D device compatible with mode */
{
	HWND		 hDesktop;
	HDC			 hdc;
	DWORD		 w, h, bpp;
	LPDDModeInfo lpModeNew;
	LPD3DDevInfo lpDevNew, lpDevNext;

	// Check Parameters
	if ((! lpDriver) || (! lpMode) || (! lpDev))
		return FALSE;

	// Get Desktop Mode info
	hDesktop = GetDesktopWindow ();
	hdc = GetDC (hDesktop);

	w	= GetDeviceCaps (hdc, HORZRES);
	h	= GetDeviceCaps (hdc, VERTRES);
	bpp	= GetDeviceCaps(hdc, PLANES) * GetDeviceCaps (hdc, BITSPIXEL);

	ReleaseDC (hDesktop, hdc);

	// Get Mode
	lpModeNew = lpDriver->FindMode (w, h, bpp, 0, NULL);
	if (! lpModeNew)
		return FALSE;

	// Get Compatible Device
	lpDevNew = lpDriver->FindDeviceSupportsMode (lpD3DGuid,
		 								         lpModeNew,
												 &lpDevNext);
	if (! lpDevNew)
	{
		if (! lpDevNext)
			return FALSE;
		lpDevNew = lpDevNext;
	}

	// Save results
	*lpMode = lpModeNew;
	*lpDev  = lpDevNew;

	// Success
	return TRUE;
} // End GetDesktopMode


  
/*
**-----------------------------------------------------------------------------
** Name:    GetFullscreenMode
** Purpose: Find D3D Device and a compatible mode
** Notes:	
**
**  1. Pick the D3D device first
**  2. Pick a DD mode that is compatible with our D3D device choice.
**-----------------------------------------------------------------------------
*/

BOOL GetFullscreenMode (
	LPDDDrvInfo	 lpDriver,	/* In:   Driver */
	LPGUID		 lpD3DGuid, /* In:   requested D3D device guid */
	DWORD		 w,			/* In:	 requested mode */
	DWORD		 h,
	DWORD		 bpp,
	DWORD        refresh,
	LPDDModeInfo * lpMode,	/* Out:  Valid Desktop Mode compatible with device */
	LPD3DDevInfo * lpDev)	/* Out:  Valid D3D device */
{
	LPDDModeInfo lpModeNew, lpModeNext;
	LPD3DDevInfo lpDevNew, lpDevNext;

	// Check Parameters
	if ((! lpDriver) || (! lpMode) || (! lpDev))
		return FALSE;

	// Get D3D Device
	lpDevNew = lpDriver->FindDevice (lpD3DGuid, &lpDevNext);
	if (! lpDevNew)
	{
		if (! lpDevNext)
			return FALSE;
		lpDevNew = lpDevNext;
	}

	// Double check requested mode parameters
	if ((w == 0) || (h == 0) || (bpp == 0))
	{
		// Pick a reasonable full screen default
		// Most Hardware devices support 16 bpp,
		// many don't support 8 bpp, so pick 16
		w   = 640;
		h	= 480;
		bpp = 16;	
	}

	// Get Compatible Mode
	lpModeNew = lpDriver->FindModeSupportsDevice (w, h, bpp, refresh,
											      lpDevNew,
												  &lpModeNext);
	if (! lpModeNew)
	{
		if (! lpModeNext)
			return FALSE;
		lpModeNew = lpModeNext;
	}

	// Save results
	*lpMode = lpModeNew;
	*lpDev  = lpDevNew;

	// Success
	return TRUE;
} // End GetFullscreenMode


  
/*
**-----------------------------------------------------------------------------
** Name:    ChooseDriverDefaults
** Purpose: Chooses default driver, mode, and device
** Notes:
**
**	Windowed:	The mode comes from the desktop and can't be changed
**				so derive the device from the mode.
**  FullScreen:	Any mode is fine.  Hardware is more interesting than
**				software, so derive the mode from the device
** 
**-----------------------------------------------------------------------------
*/

HRESULT ChooseDriverDefaults (
    LPGUID lpGuidDD,			// In:	Requested DD Guid
    DWORD  dwW,					// In:	Requested Mode (w,h,bpp,refresh)
	DWORD  dwH,
	DWORD  dwBPP, 
	DWORD  dwRefresh,
    LPGUID lpGuidD3D,			// In:	Requested D3D Guid
	BOOL   fFullScreen,			// In:  Fullscreen or Windowed mode
	LPDDDrvInfo * lpDriver,		// Out: Valid Driver
	LPDDModeInfo * lpMode,		// Out: Valid Mode
	LPD3DDevInfo * lpDevice)	// Out: Valid Device
{
	HRESULT			hResult;
    LPDDDrvInfo		lpDrvNew;
	LPDDModeInfo	lpModeNew;
	LPD3DDevInfo	lpDevNew;

	// Check Parameters
	if ((! lpDriver) || (! lpMode) || (! lpDevice))
	{
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
		return hResult;
	}

	// Initialize Driver Manager, if necessary
	if (! DDDrvMgr::isInitialized ())
	{
		hResult = DDDrvMgr::Init ();
		if (FAILED (hResult))
			return hResult;
	}
    
    // Find Driver matching specified GUID
    lpDrvNew = ValidateDriver (lpGuidDD);
	if (! lpDrvNew)
    {
        // Error, invalid DD Guid
		hResult = APPERR_INVALIDPARAMS;
		REPORTERR (hResult);
        return hResult;
    }

	if (fFullScreen)
	{
		// Get D3D device and compatible mode
	    if (! GetFullscreenMode (lpDrvNew, lpGuidD3D, 
								 dwW, dwH, dwBPP, dwRefresh,
								 &lpModeNew, &lpDevNew))
		{
			hResult = APPERR_GENERIC;
			REPORTERR (hResult);
			return hResult;
		}
	}
	else
	{
		// Get Desktop mode and compatible D3D device
		if (! GetDesktopMode (lpDrvNew, lpGuidD3D, &lpModeNew, &lpDevNew))
		{
			hResult = APPERR_GENERIC;
			REPORTERR (hResult);
			return hResult;
		}
	}

	// Return results
	*lpDriver = lpDrvNew;
	*lpMode   = lpModeNew;
	*lpDevice = lpDevNew;

    // Success
    return DD_OK;
} // End ChooseDriverDefaults


  
/*
**-----------------------------------------------------------------------------
**  Name:       FlagsToBitDepth
**  Purpose:    Gets Bit Depth from DDPF Flags
**-----------------------------------------------------------------------------
*/

DWORD FlagsToBitDepth (DWORD dwFlags)
{
    if (dwFlags & DDBD_1)
        return 1L;
    else if (dwFlags & DDBD_2)
        return 2L;
    else if (dwFlags & DDBD_4)
        return 4L;
    else if (dwFlags & DDBD_8)
        return 8L;
    else if (dwFlags & DDBD_16)
        return 16L;
    else if (dwFlags & DDBD_24)
        return 24L;
    else if (dwFlags & DDBD_32)
        return 32L;
    else
        return 0L; 
} // End FlagsToBitDepth



/*
**-----------------------------------------------------------------------------
**  Name:       BitDepthToFlags
**  Purpose:    Converts BPP to corresponding DDPF flag
**-----------------------------------------------------------------------------
*/

DWORD BitDepthToFlags (DWORD dwBPP)
{
    switch (dwBPP) 
	{
	case 1:
	    return DDBD_1;
	case 2:
	    return DDBD_2;
	case 4:
	    return DDBD_4;
	case 8:
	    return DDBD_8;
	case 16:
	    return DDBD_16;
	case 24:
	    return DDBD_24;
	case 32:
	    return DDBD_32;
	default:
		// Error
	    return (DWORD)0L;
    }
} // End BitDepthToFlags


  
/*
**-----------------------------------------------------------------------------
** Name:    isPalettized
**-----------------------------------------------------------------------------
*/

BOOL isPalettized (LPDDPIXELFORMAT lpddpf)
{
    if (! lpddpf)
    {
        // Error, 
        return FALSE;
    }

    if (lpddpf->dwFlags & DDPF_PALETTEINDEXED1)
        return TRUE;

    if (lpddpf->dwFlags & DDPF_PALETTEINDEXED2)
        return TRUE;

    if (lpddpf->dwFlags & DDPF_PALETTEINDEXED4)
        return TRUE;

    if (lpddpf->dwFlags & DDPF_PALETTEINDEXED8)
        return TRUE;

    // Not palettized
    return FALSE;
} // End IsPalettized






/*
**-----------------------------------------------------------------------------
**  Local Functions
**-----------------------------------------------------------------------------
*/
  
/*
**-----------------------------------------------------------------------------
**  Name:       DriverEnumCallback
**  Purpose:    Add this DD driver to global driver list
**-----------------------------------------------------------------------------
*/

BOOL WINAPI DriverEnumCallback(
    GUID FAR *  lpGuid,
    LPTSTR      lpDesc, 
    LPTSTR      lpName,
    LPVOID      lpExtra)
{
    HRESULT         hResult;
    LPDD_CB_INFO lpInfo;
    LPDDDrvInfo     lpDriver;
    DWORD           dwIndex;

    if (! lpExtra)
    {
        // Programming error, invalid pointer
        return DDENUMRET_OK;
    }

    lpInfo = (LPDD_CB_INFO)lpExtra;
    dwIndex = lpInfo->cCount;

    // Get Pointer to driver info
    lpDriver = new DDDrvInfo;
    if (! lpDriver)
    {
        // Error, Not enough memory to create driver
        return DDENUMRET_OK;
    }

    // Initialize driver 
	// Enumerate modes, D3D devices
    hResult = lpDriver->Create (lpGuid, lpName, lpDesc);
    if (FAILED (hResult))
    {
        // Error, Driver Create failed
        return DDENUMRET_OK;
    }

	// Add To Global Driver List
	hResult = DDDrvMgr::AddDriver (lpDriver);	
	if (FAILED (hResult))
	{
		// Error, Driver Create Failed
		return DDENUMRET_OK;
	}

    // Increment driver count
    lpInfo->cCount++;

    // Success
    return DDENUMRET_OK;
} // End DriverEnumCallback



/*
**-----------------------------------------------------------------------------
**  Name:       ModeEnumCallback
**  Purpose:    Add this mode to the driver
**-----------------------------------------------------------------------------
*/

HRESULT WINAPI ModeEnumCallback (
    LPDDSURFACEDESC lpDDSurfDesc,
    LPVOID          lpExtra)
{
	HRESULT			hResult;
	LPDDModeInfo	lpMode;
    LPDD_CB_INFO lpInfo;
    LPDDDrvInfo		lpDriver;
    DWORD           dwIndex;

    if (! lpExtra)
    {
        // Programming error, invalid pointer
        return DDENUMRET_OK;
    }

    lpInfo   = (LPDD_CB_INFO)lpExtra;
    lpDriver = (LPDDDrvInfo) lpInfo->lpExtra;
    dwIndex  = lpInfo->cCount;

    if (! lpDriver)
    {
        // Programming Error, invalid pointer
        return DDENUMRET_OK;
    }

    if (! lpDDSurfDesc)
    {
        // Error, invalid pointer
        return DDENUMRET_CANCEL;
    }

    // Double check structure size
    if (lpDDSurfDesc->dwSize != sizeof(DDSURFACEDESC))
    {
        // Error, structure is wrong size
        return DDENUMRET_CANCEL;
    }

	// Create Mode node
	lpMode = new DDModeInfo;
	if (! lpMode)
	{
        // Error, not enough memory to store mode info
        return DDENUMRET_OK;
	}

    // Copy surface description
    lpMode->ddSurfDesc = *lpDDSurfDesc;

	// Add Mode to Driver Mode List
	hResult = lpDriver->AddMode (lpMode);
	if (FAILED (hResult))
	{
        // Error, not enough memory to store mode info
        return DDENUMRET_OK;
	}

    // Update mode count
    lpInfo->cCount++;
        
    return DDENUMRET_OK;
} // End ModeEnumCallback



/*
**-----------------------------------------------------------------------------
**  Name:       TextureFormatEnumCallback
**  Purpose:    Add this mode to the driver
**-----------------------------------------------------------------------------
*/

HRESULT WINAPI TextureFormatEnumCallback (
    LPDDSURFACEDESC lpTextureFormat,
    LPVOID          lpExtra)
{
	HRESULT			hResult;
	LPDDModeInfo	lpFormat;
    LPDD_CB_INFO lpInfo;
    LPD3DDevInfo	lpDevice;
    DWORD           dwIndex;

    if (! lpExtra)
    {
        // Programming error, invalid pointer
        return DDENUMRET_OK;
    }

    lpInfo   = (LPDD_CB_INFO)lpExtra;
    lpDevice = (LPD3DDevInfo)lpInfo->lpExtra;
    dwIndex  = lpInfo->cCount;

    if (! lpDevice)
    {
        // Programming error, invalid pointer
        return DDENUMRET_OK;
    }

    if (! lpTextureFormat)
    {
        // Error, invalid pointer
        return DDENUMRET_CANCEL;
    }

    // Double check structure size
    if (lpTextureFormat->dwSize != sizeof(DDSURFACEDESC))
    {
        // Error, structure is wrong size
        return DDENUMRET_CANCEL;
    }

	// Create format node
	lpFormat = new DDModeInfo;
	if (! lpFormat)
	{
        // Error, not enough memory to store format info
        return DDENUMRET_OK;
	}

    // Copy texture format description
    lpFormat->ddSurfDesc = *lpTextureFormat;

	// Add format to D3D device format list
	hResult = lpDevice->AddFormat (lpFormat);
	if (FAILED (hResult))
	{
        // Error, not enough memory to store mode info
        return DDENUMRET_OK;
	}

    // Update format count
    lpInfo->cCount++;
        
    return DDENUMRET_OK;
} // End TextureFormatEnumCallback
  

  
/*
**-----------------------------------------------------------------------------
**  Name:       DeviceEnumCallback
**  Purpose:    Add this D3D Device Driver info to the driver
**-----------------------------------------------------------------------------
*/

HRESULT WINAPI DeviceEnumCallback (
    LPGUID          lpGuid,
    LPTSTR          lpName,
    LPTSTR          lpDesc,
    LPD3DDEVICEDESC lpHalDevice,
    LPD3DDEVICEDESC lpHelDevice,
    LPVOID          lpExtra)
{
    HRESULT         hResult;
    LPDD_CB_INFO	lpInfo;
    LPDDDrvInfo     lpDriver;
	LPD3DDevInfo	lpDevice;
    DWORD           dwIndex;

    if (! lpExtra)
    {
        // Programming error, invalid pointer
        return DDENUMRET_OK;
    }

    lpInfo   = (LPDD_CB_INFO) lpExtra;
    lpDriver = (LPDDDrvInfo) lpInfo->lpExtra;
    dwIndex  = lpInfo->cCount;

    if (! lpDriver)
    {
        // Programming Error, invalid pointer
        return DDENUMRET_OK;
    }

	// Create D3D Device node
	lpDevice = new D3DDevInfo;
	if (! lpDevice)
	{
        // Not Enough memory to create D3D device node
        return DDENUMRET_OK;
	}

    // Initialize D3D Device info
    hResult = lpDevice->Create (lpGuid, lpName, lpDesc,
                                lpHalDevice, lpHelDevice);
    if (FAILED (hResult))
    {
        // Error
        return DDENUMRET_OK;
    }

	// Add to Driver D3D Device list
	hResult = lpDriver->AddDevice (lpDevice);
	if (FAILED (hResult))
	{
        // Error
        return DDENUMRET_OK;
	}

    // Update D3D device Driver count
    lpInfo->cCount++;
        
    return DDENUMRET_OK;
} // End DeviceEnumCallback


  
/*
**-----------------------------------------------------------------------------
**  DDModeInfo Methods
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Name:       DDModeInfo::GetWidth
**  Purpose:    Gets the Width for this mode
**-----------------------------------------------------------------------------
*/

DWORD DDModeInfo::GetWidth (void)
{
    // Check parameters
    if (ddSurfDesc.dwSize != sizeof (DDSURFACEDESC))
        return 0L;

	// Check that Pixel format is valid
	if (! (ddSurfDesc.dwFlags & DDSD_WIDTH))
		return 0L;

	// Get Bits Per Pixel
	return ddSurfDesc.dwWidth;
} // DDModeInfo::GetWidth


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDModeInfo::GetHeight
**  Purpose:    Gets the Height for this mode
**-----------------------------------------------------------------------------
*/

DWORD DDModeInfo::GetHeight (void)
{
    // Check parameters
    if (ddSurfDesc.dwSize != sizeof (DDSURFACEDESC))
        return 0L;

	// Check that Pixel format is valid
	if (! (ddSurfDesc.dwFlags & DDSD_HEIGHT))
		return 0L;

	// Get Bits Per Pixel
	return ddSurfDesc.dwHeight;
} // DDModeInfo::GetHeight



/*
**-----------------------------------------------------------------------------
**  Name:       DDModeInfo::GetBPP
**  Purpose:    Gets the Bits per pixel for this mode
**-----------------------------------------------------------------------------
*/

DWORD DDModeInfo::GetBPP (void)
{
    // Check parameters
    if (ddSurfDesc.dwSize != sizeof (DDSURFACEDESC))
        return 0L;

	// Check that Pixel format is valid
	if (! (ddSurfDesc.dwFlags & DDSD_PIXELFORMAT))
		return 0L;
	if (ddSurfDesc.ddpfPixelFormat.dwSize != sizeof(DDPIXELFORMAT))
		return 0L;

	// Assume it is RGB
	return ddSurfDesc.ddpfPixelFormat.dwRGBBitCount;
} // DDModeInfo::GetBPP


/*
**-----------------------------------------------------------------------------
**  Name:       DDModeInfo::GetMode
**  Purpose:    Gets the Mode info (w,h,bpp) for this mode
**-----------------------------------------------------------------------------
*/

HRESULT DDModeInfo::GetMode (
	DWORD & dwW, 
	DWORD & dwH, 
	DWORD & dwBPP,
	DWORD & dwRefresh)
{
    // Check parameters
    if (ddSurfDesc.dwSize != sizeof (DDSURFACEDESC))
        return DDERR_GENERIC;

	// Check that width is valid
	if (! (ddSurfDesc.dwFlags & DDSD_WIDTH))
		return DDERR_GENERIC;

	// Check that height is valid
	if (! (ddSurfDesc.dwFlags & DDSD_HEIGHT))
		return DDERR_GENERIC;

	// Check that Pixel format is valid
	if (! (ddSurfDesc.dwFlags & DDSD_PIXELFORMAT))
		return DDERR_GENERIC;
	if (ddSurfDesc.ddpfPixelFormat.dwSize != sizeof(DDPIXELFORMAT))
		return DDERR_GENERIC;

	// Get Width, height, BPP
	dwW			= ddSurfDesc.dwWidth;
	dwH			= ddSurfDesc.dwHeight;
	dwBPP		= ddSurfDesc.ddpfPixelFormat.dwRGBBitCount;
	dwRefresh	= 0L;

	// Success
	return DD_OK;
} // DDModeInfo::GetMode



/*
**-----------------------------------------------------------------------------
**  Name:       DDModeInfo::ModeSupportedByDevice
**  Purpose:    
**-----------------------------------------------------------------------------
*/

BOOL DDModeInfo::ModeSupported (LPD3DDevInfo lpDevice)
{
	// Check Parameters
	if (! lpDevice)
		return FALSE;

	// Make sure D3D device supports this mode
	DWORD dwBPP    = GetBPP ();
	DWORD dwFlag   = BitDepthToFlags (dwBPP);
	DWORD dwDepths = 0L;

	// Get Supported Bit Depths for this D3D device
	if (lpDevice->isHardware ())
		dwDepths = lpDevice->d3dHalDesc.dwDeviceRenderBitDepth;
	else
		dwDepths = lpDevice->d3dHelDesc.dwDeviceRenderBitDepth;

	if (dwDepths & dwFlag)
	{
		// Supported !!!
		return TRUE;
	}

	// Not Supported !!!
	return FALSE;
} // End DDModeInfo::ModeSupported
  

  
  
/*
**-----------------------------------------------------------------------------
**  Name:       DDModeInfo::Match
**  Purpose:    Checks if this mode matches (w,h,bpp)
**-----------------------------------------------------------------------------
*/

BOOL DDModeInfo::Match (DWORD dwW, DWORD dwH, DWORD dwBPP)
{
    // Check parameters
    if (ddSurfDesc.dwSize != sizeof (DDSURFACEDESC))
        return FALSE;

	// Check for Match
    if ((ddSurfDesc.dwWidth  == dwW) &&
        (ddSurfDesc.dwHeight == dwH))
	{
		if (ddSurfDesc.ddpfPixelFormat.dwRGBBitCount == dwBPP)
			return TRUE;
    }

    return FALSE;
} // DDModeInfo::Match


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::Match
**  Purpose:    Checks if this mode matches with this surface desc
**-----------------------------------------------------------------------------
*/

BOOL DDModeInfo::Match (const DDSURFACEDESC & ddsd)
{
    // Check parameters
    if (ddsd.dwSize != sizeof (DDSURFACEDESC))
        return FALSE;

    // Check Height
    if (ddsd.dwFlags & DDSD_HEIGHT)
    {
        if (ddsd.dwHeight != ddSurfDesc.dwHeight)
            return FALSE;
    }

    // Check Width
    if (ddsd.dwFlags & DDSD_WIDTH)
    {
        if (ddsd.dwWidth != ddSurfDesc.dwWidth)
            return FALSE;
    }

    // Check Pitch
    if (ddsd.dwFlags & DDSD_PITCH)
    {
        if (ddsd.lPitch != ddSurfDesc.lPitch)
            return FALSE;
    }

    // Check Back Buffer count
    if (ddsd.dwFlags & DDSD_BACKBUFFERCOUNT)
    {
        if (ddsd.dwBackBufferCount != ddSurfDesc.dwBackBufferCount)
            return FALSE;
    }

    // Check MipMap count
    if (ddsd.dwFlags & DDSD_MIPMAPCOUNT)
    {
        if (ddsd.dwMipMapCount != ddSurfDesc.dwMipMapCount)
            return FALSE;
    }

    // Check ZBufferBitDepth
    if (ddsd.dwFlags & DDSD_ZBUFFERBITDEPTH)
    {
        if (ddsd.dwZBufferBitDepth != ddSurfDesc.dwZBufferBitDepth)
            return FALSE;
    }

    // Check Refresh Rate
    if (ddsd.dwFlags & DDSD_REFRESHRATE)
    {
        if (ddsd.dwRefreshRate != ddSurfDesc.dwRefreshRate)
            return FALSE;
    }

    // Check Alpha Bit Depth
    if (ddsd.dwFlags & DDSD_ALPHABITDEPTH)
    {
        if (ddsd.dwAlphaBitDepth != ddSurfDesc.dwAlphaBitDepth)
            return FALSE;
    }

    // Check ColorKey Dest Blt
    if (ddsd.dwFlags & DDSD_CKDESTBLT)
    {

        if (ddsd.ddckCKDestBlt.dwColorSpaceLowValue != 
            ddSurfDesc.ddckCKDestBlt.dwColorSpaceLowValue)
            return FALSE;

        if (ddsd.ddckCKDestBlt.dwColorSpaceHighValue != 
            ddSurfDesc.ddckCKDestBlt.dwColorSpaceHighValue)
            return FALSE;
    }

    // Check ColorKey Dest Overlay
    if (ddsd.dwFlags & DDSD_CKDESTBLT)
    {

        if (ddsd.ddckCKDestOverlay.dwColorSpaceLowValue != 
            ddSurfDesc.ddckCKDestOverlay.dwColorSpaceLowValue)
            return FALSE;

        if (ddsd.ddckCKDestOverlay.dwColorSpaceHighValue != 
            ddSurfDesc.ddckCKDestOverlay.dwColorSpaceHighValue)
            return FALSE;
    }

    
    // Check ColorKey Src Blt
    if (ddsd.dwFlags & DDSD_CKSRCBLT)
    {

        if (ddsd.ddckCKSrcBlt.dwColorSpaceLowValue != 
            ddSurfDesc.ddckCKSrcBlt.dwColorSpaceLowValue)
            return FALSE;

        if (ddsd.ddckCKSrcBlt.dwColorSpaceHighValue != 
            ddSurfDesc.ddckCKSrcBlt.dwColorSpaceHighValue)
            return FALSE;
    }

    // Check ColorKey Src Overlay
    if (ddsd.dwFlags & DDSD_CKSRCOVERLAY)
    {

        if (ddsd.ddckCKSrcOverlay.dwColorSpaceLowValue != 
            ddSurfDesc.ddckCKSrcOverlay.dwColorSpaceLowValue)
            return FALSE;

        if (ddsd.ddckCKSrcOverlay.dwColorSpaceHighValue != 
            ddSurfDesc.ddckCKSrcOverlay.dwColorSpaceHighValue)
            return FALSE;
    }


    // Check Pixel Format
    if (ddsd.dwFlags & DDSD_PIXELFORMAT)
    {
		if (! Match (ddsd.ddpfPixelFormat))
			return FALSE;
    }

    // Check Caps
    if (ddsd.dwFlags & DDSD_CAPS)
    {
        // Superset is OK
        if ((ddsd.ddsCaps.dwCaps & ddSurfDesc.ddsCaps.dwCaps) !=
            ddsd.ddsCaps.dwCaps)
            return FALSE;
    }

    // Success, we have a match
    return TRUE;
} // DDModeInfo::Match
  


/*
**-----------------------------------------------------------------------------
**  Name:       DDModeInfo::Match
**  Purpose:    Checks if this mode matches (bpp)
**-----------------------------------------------------------------------------
*/

BOOL DDModeInfo::Match (DWORD dwBPP)
{
    // Check parameters
    if (ddSurfDesc.dwSize != sizeof (DDSURFACEDESC))
        return FALSE;

	// Check for Match
	if (ddSurfDesc.ddpfPixelFormat.dwRGBBitCount == dwBPP)
		return TRUE;

    return FALSE;
} // DDModeInfo::Match
  


/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::Match
**  Purpose:    Checks if this mode matches the requested pixel format
**-----------------------------------------------------------------------------
*/

BOOL DDModeInfo::Match (const DDPIXELFORMAT & ddpf)
{
	DWORD dwCheck;

    // Check parameters
	if (ddpf.dwSize != sizeof (DDPIXELFORMAT))
		return FALSE;

	// Check Alpha-Only flag
    if (ddpf.dwFlags & DDPF_ALPHA)
    {
		// Is this mode alpha only too ?!?
        if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_ALPHA))
            return FALSE;

		// Do Alpha bit depths match ?!?
		if (ddpf.dwAlphaBitDepth != ddSurfDesc.ddpfPixelFormat.dwAlphaBitDepth)
			return FALSE;
    }

	// Check ZBuffer-Only flag
    if (ddpf.dwFlags & DDPF_ZBUFFER)
    {
		// Is this mode Z-buffer only too ?!?
        if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_ZBUFFER))
            return FALSE;

		// Do Z-depths match ?!?
		if (ddpf.dwZBufferBitDepth != ddSurfDesc.ddpfPixelFormat.dwZBufferBitDepth)
			return FALSE;
    }

	// Check Compressed flag
	if (ddpf.dwFlags & DDPF_COMPRESSED)
	{
		// Is this mode compressed too ?!?
        if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_COMPRESSED))
            return FALSE;
	}

	// Check FourCC flag
	if (ddpf.dwFlags & DDPF_FOURCC)
	{
		// Is this mode a fourCC code too ?!?
		if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_FOURCC))
			return FALSE;

		// Do fourCC codes match ?!?
		if (ddSurfDesc.ddpfPixelFormat.dwFourCC != ddpf.dwFourCC)
			return FALSE;
	}

    // Check Palette flags
	dwCheck = DDPF_PALETTEINDEXED1 | DDPF_PALETTEINDEXED2 |
		      DDPF_PALETTEINDEXED4 | DDPF_PALETTEINDEXED8 |
			  DDPF_PALETTEINDEXEDTO8;
	if (ddpf.dwFlags & dwCheck)
	{
		DWORD dwOne = ddpf.dwFlags & dwCheck;
		DWORD dwTwo = ddSurfDesc.ddpfPixelFormat.dwFlags & dwCheck;

		// Do palette formats match ?!?
		if (dwOne != dwTwo)
			return FALSE;
	}

	// Check RGB flags
	if (ddpf.dwFlags & DDPF_RGB)
	{
		// Does this mode support RGB too ???
		if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_RGB))
			return FALSE;

		// Check RGBTOYUV flag
		if (ddpf.dwFlags & DDPF_RGBTOYUV)
		{
			// Does this mode support RGBTOYUV too???
			if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_RGBTOYUV))
				return FALSE;
		}

		// Do BPP's match?
		if (ddpf.dwRGBBitCount != ddSurfDesc.ddpfPixelFormat.dwRGBBitCount)
			return FALSE;

		// Do Masks match
		if (ddpf.dwRBitMask != ddSurfDesc.ddpfPixelFormat.dwRBitMask)
			return FALSE;

		if (ddpf.dwGBitMask != ddSurfDesc.ddpfPixelFormat.dwGBitMask)
			return FALSE;

		if (ddpf.dwBBitMask != ddSurfDesc.ddpfPixelFormat.dwBBitMask)
			return FALSE;

		// Check Alpha Channel
		if (ddpf.dwFlags & DDPF_ALPHAPIXELS)
		{
			// Does this mode support Alphachannel as well ?!?
			if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_ALPHAPIXELS))
				return FALSE;

			// Do Alpha channel masks match
			if (ddpf.dwRGBAlphaBitMask != ddSurfDesc.ddpfPixelFormat.dwRGBAlphaBitMask)
				return FALSE;
		}

		// Check Z channel
		if (ddpf.dwFlags & DDPF_ZPIXELS)
		{
			// Does this mode support Z channel as well ?!?
			if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_ZPIXELS))
				return FALSE;

			// Do Z channel masks match
			if (ddpf.dwRGBZBitMask != ddSurfDesc.ddpfPixelFormat.dwRGBZBitMask)
				return FALSE;
		}
	}

	// Check YUV flags
	if (ddpf.dwFlags & DDPF_YUV)
	{
		// Does this mode support YUV too ???
		if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_YUV))
			return FALSE;

		// Do BPP's match?
		if (ddpf.dwYUVBitCount != ddSurfDesc.ddpfPixelFormat.dwYUVBitCount)
			return FALSE;

		// Do Masks match
		if (ddpf.dwYBitMask != ddSurfDesc.ddpfPixelFormat.dwYBitMask)
			return FALSE;

		if (ddpf.dwUBitMask != ddSurfDesc.ddpfPixelFormat.dwUBitMask)
			return FALSE;

		if (ddpf.dwVBitMask != ddSurfDesc.ddpfPixelFormat.dwVBitMask)
			return FALSE;

		// Check Alpha channel
		if (ddpf.dwFlags & DDPF_ALPHAPIXELS)
		{
			// Does this mode support Alphachannel as well ?!?
			if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_ALPHAPIXELS))
				return FALSE;

			// Do Alpha channel masks match
			if (ddpf.dwYUVAlphaBitMask != ddSurfDesc.ddpfPixelFormat.dwYUVAlphaBitMask)
				return FALSE;
		}

		// Check Z channel
		if (ddpf.dwFlags & DDPF_ZPIXELS)
		{
			// Does this mode support Z channel as well ?!?
			if (! (ddSurfDesc.ddpfPixelFormat.dwFlags & DDPF_ZPIXELS))
				return FALSE;

			// Do Z channel masks match
			if (ddpf.dwRGBZBitMask != ddSurfDesc.ddpfPixelFormat.dwRGBZBitMask)
				return FALSE;
		}
	}

    // Success, we have a match
    return TRUE;
} // DDModeInfo::Match
  

  
/*
**-----------------------------------------------------------------------------
**  D3DDevInfo Methods
**-----------------------------------------------------------------------------
*/


/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::isHardware
**  Purpose:    Checks if this D3D Device is a hardware driver
**-----------------------------------------------------------------------------
*/

BOOL D3DDevInfo::isHardware (void)
{
    // No correct way of doing this,
    // but if the hardware caps don't specify a shading
    // model then it probably isn't hardware accelerated
    DWORD dwColorModel = d3dHalDesc.dcmColorModel;
    if (dwColorModel)
        return TRUE;
    return FALSE;
} // End D3DDevInfo::isHardware



/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::Match
**  Purpose:    Checks if this D3D Device matches this guid
**-----------------------------------------------------------------------------
*/

BOOL D3DDevInfo::Match (LPGUID lpGuid)
{
    if (lpGuid == NULL)
        return FALSE;

    if (! isValid ())
        return FALSE;
    
    if (guid != *lpGuid)
        return FALSE;

    // Success
    return TRUE;
} // End D3DDevInfo::Match



/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::Match
**  Purpose:    Checks if this D3D Device matches the specified
**              hardware and/or software description
**-----------------------------------------------------------------------------
*/

BOOL D3DDevInfo::Match (LPD3DDEVICEDESC lpHal, LPD3DDEVICEDESC lpHel)
{
    if (! isValid ())
        return FALSE;

    // Check Parameters
    if ((! lpHal) && (! lpHel))
        return FALSE;

    //
    // Compare Hal description's
    //
    if (lpHal)
    {
        if (lpHal->dwSize != sizeof (D3DDEVICEDESC))
            return FALSE;

        // Check ColorModel
        if (lpHal->dwFlags & D3DDD_COLORMODEL)
        {
            // If it does more than we are interested in that's OK
            if ((lpHal->dcmColorModel & d3dHalDesc.dcmColorModel) != 
                lpHal->dcmColorModel)
                return FALSE;
        }

        // Check Device Caps
        if (lpHal->dwFlags & D3DDD_DEVCAPS)
        {
            // If it does more than we are asking, that's OK
            if ((lpHal->dwDevCaps & d3dHalDesc.dwDevCaps) !=
                lpHal->dwDevCaps)
                return FALSE;
        }

        // Check transform caps
        if (lpHal->dwFlags & D3DDD_TRANSFORMCAPS)
        {
            // Note: Flesh this out later
        }

        // Check Clipping 
        if (lpHal->dwFlags & D3DDD_BCLIPPING)
        {
            if (lpHal->bClipping != d3dHalDesc.bClipping)
                return FALSE;
        }

        // Check lighting caps
        if (lpHal->dwFlags & D3DDD_LIGHTINGCAPS)
        {
            // Note: Flesh this out later
        }
        
        // Check line caps
        if (lpHal->dwFlags & D3DDD_LINECAPS)
        {
            // Note: Flesh this out later
        }

        // Check triangle caps
        if (lpHal->dwFlags & D3DDD_TRICAPS)
        {
            // Note: Flesh this out later
        }

        // Check Render Surface bit depth
        if (lpHal->dwDeviceRenderBitDepth)
        {
             if (lpHal->dwDeviceRenderBitDepth != d3dHalDesc.dwDeviceRenderBitDepth)
                return FALSE;
        }

        // Check Z-Buffer surface bit depth
        if (lpHal->dwDeviceZBufferBitDepth)
        {
            if (lpHal->dwDeviceZBufferBitDepth != d3dHalDesc.dwDeviceZBufferBitDepth)
                return FALSE;
        }

        // Check Max buffer size
        if (lpHal->dwFlags & D3DDD_MAXBUFFERSIZE)
        {
            // Only worry, if it is smaller than what we want
            if (lpHal->dwMaxBufferSize > d3dHalDesc.dwMaxBufferSize)
                return FALSE;
        }

        // Check Max Vertex count
        if (lpHal->dwFlags & D3DDD_MAXVERTEXCOUNT)
        {
            // Only worry, if it is smaller than what we want
            if (lpHal->dwMaxVertexCount > d3dHalDesc.dwMaxVertexCount)
                return FALSE;
        }
    }

    
    //
    // Compare Hel description's
    //
    if (lpHel)
    {
        if (lpHel->dwSize != sizeof (D3DDEVICEDESC))
            return FALSE;

        // Check ColorModel
        if (lpHel->dwFlags & D3DDD_COLORMODEL)
        {
            // If it does more than we are interested in that's OK
            if ((lpHel->dcmColorModel & d3dHelDesc.dcmColorModel) != 
                lpHel->dcmColorModel)
                return FALSE;
        }

        // Check Device Caps
        if (lpHel->dwFlags & D3DDD_DEVCAPS)
        {
            // If it does more than we are asking, that's OK
            if ((lpHel->dwDevCaps & d3dHelDesc.dwDevCaps) !=
                lpHel->dwDevCaps)
                return FALSE;
        }

        // Check transform caps
        if (lpHel->dwFlags & D3DDD_TRANSFORMCAPS)
        {
            // Note: Flesh this out later
        }

        // Check Clipping 
        if (lpHel->dwFlags & D3DDD_BCLIPPING)
        {
            if (lpHel->bClipping != d3dHelDesc.bClipping)
                return FALSE;
        }

        // Check lighting caps
        if (lpHel->dwFlags & D3DDD_LIGHTINGCAPS)
        {
            // Note: Flesh this out later
        }
        
        // Check line caps
        if (lpHel->dwFlags & D3DDD_LINECAPS)
        {
            // Note: Flesh this out later
        }

        // Check triangle caps
        if (lpHel->dwFlags & D3DDD_TRICAPS)
        {
            // Note: Flesh this out later
        }

        // Check Render Surface bit depth
        if (lpHel->dwDeviceRenderBitDepth)
        {
             if (lpHel->dwDeviceRenderBitDepth != d3dHelDesc.dwDeviceRenderBitDepth)
                return FALSE;
        }

        // Check Z-Buffer surface bit depth
        if (lpHel->dwDeviceZBufferBitDepth)
        {
            if (lpHel->dwDeviceZBufferBitDepth != d3dHelDesc.dwDeviceZBufferBitDepth)
                return FALSE;
        }

        // Check Max buffer size
        if (lpHel->dwFlags & D3DDD_MAXBUFFERSIZE)
        {
            // Only worry, if it is smaller than what we want
            if (lpHel->dwMaxBufferSize > d3dHelDesc.dwMaxBufferSize)
                return FALSE;
        }

        // Check Max Vertex count
        if (lpHel->dwFlags & D3DDD_MAXVERTEXCOUNT)
        {
            // Only worry, if it is smaller than what we want
            if (lpHel->dwMaxVertexCount > d3dHelDesc.dwMaxVertexCount)
                return FALSE;
        }
    }

    // Success
    return TRUE;
} // End D3DDevInfo::Match
  

  
/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::Create
**  Purpose:    Creates a new D3D Device 
**-----------------------------------------------------------------------------
*/

HRESULT D3DDevInfo::Create (
    LPGUID          lpD3DGuid,
    LPTSTR          lpD3DName, 
    LPTSTR          lpD3DDesc, 
    LPD3DDEVICEDESC lpD3DHal, 
    LPD3DDEVICEDESC lpD3DHel)
{
    // Copy GUID
    if (! lpD3DGuid)
    {
        // Error, Invalid device
        return DDERR_INVALIDPARAMS;
    }
    guid = *lpD3DGuid;

    
	// Copy Name
	LPTSTR szTemp;
    if (! lpD3DName)
    {
		szTemp = TEXT("UNKNOWN");
    }
    else
    {
        szTemp = lpD3DName;
    }

    DWORD cLen = _tcslen (szTemp);
	DWORD cbSize = (cLen + 1) * sizeof(TCHAR);
	szName = (LPTSTR) malloc (cbSize);
	if (szName)
	{
		_tcsncpy (szName, szTemp, cLen);
		szName[cLen] = 0;
	}

    
    // Copy Description
    if (! lpD3DDesc)
    {
		szTemp = TEXT("UNKNOWN");
    }
    else
    {
        szTemp = lpD3DDesc;
    }

    cLen = _tcslen (szTemp);
	cbSize = (cLen + 1) * sizeof(TCHAR);
	szDesc = (LPTSTR) malloc (cbSize);
	if (szDesc)
	{
		_tcsncpy (szDesc, szTemp, cLen);
		szDesc[cLen] = 0;
	}


    // Copy D3D info
    if (lpD3DHal)
    {
        d3dHalDesc = *lpD3DHal;
    }

    if (lpD3DHel)
    {
        d3dHelDesc = *lpD3DHel;
    }

	// Mark Texture format list as not loaded
	cFormats = 0L;
	turnFormatsLoadedOff ();

	// Mark as valid
    validOn ();

    return DD_OK;
} // End D3DDevInfo::Create



/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::Destroy
**  Purpose:    Cleanup any memory or interfaces
**-----------------------------------------------------------------------------
*/

void D3DDevInfo::Destroy (void)
{
	// Destroy Texture Formats
	DestroyFormats ();

	// Clean up strings
	if (szDesc)
	{
		free (szDesc);
		szDesc = NULL;
	}

	if (szName)
	{
		free (szName);
		szName = NULL;
	}

	lpPrev = NULL;
	lpNext = NULL;
} // End D3DDevInfo::Destroy


  
/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::LoadFormats
**  Purpose:    Loads texture formats
**  Notes:
**
**	In order to load texture formats, we need to have a valid D3D Device
**	Getting a D3D Device directly complicates the Driver Manager code.
**
**	So we will defer loading texture formats until we have created a valid 
**  D3D device in D3DWindow::InitRender and then call this function to 
**  Load all the texture formats.
**
**-----------------------------------------------------------------------------
*/

HRESULT D3DDevInfo::LoadFormats (LPDIRECT3DDEVICE2 lpD3DDevice)
{
	// Have we already loaded the texture formats
	if (! formatsLoaded ())
	{
		HRESULT hResult;
	    DD_CB_INFO		cbInfo;

		// Check Parameters
		if (! lpD3DDevice)
		{
			hResult = APPERR_INVALIDPARAMS;
			REPORTERR (hResult);
			return hResult;
		}
		
		// Enumerate all Texture Formats for this device
		cbInfo.fResult  = TRUE;
		cbInfo.lpExtra  = (void *)this;
		cbInfo.cCount   = 0L;

		hResult = lpD3DDevice->EnumTextureFormats (TextureFormatEnumCallback, 
												   (void *)&cbInfo);
		if (FAILED(hResult))
		{
			// Error
			REPORTERR (hResult);
			return hResult;
		}
	
		// Double check count
		if ((! cbInfo.fResult) || (cbInfo.cCount == 0) || (cFormats != cbInfo.cCount))
		{
			hResult = APPERR_GENERIC;
			REPORTERR (hResult);
			return hResult;
		}

		// Mark texture formats as loaded
		turnFormatsLoadedOn ();
	}

	// Success
	return DD_OK;
} // D3DDevInfo::LoadFormats



/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::DestroyFormats
**  Purpose:    Destroys texture formats
**-----------------------------------------------------------------------------
*/
  
HRESULT D3DDevInfo::DestroyFormats (void)
{
	if (formatsLoaded ())
	{
		LPDDModeInfo lpCurr, lpNext, lpPrev;

		lpCurr = lpFormatRoot;

		// Walk linked list and destroy all Format nodes
		while (lpCurr)
		{
			lpNext = lpCurr->lpNext;
			lpPrev = lpCurr->lpPrev;

			// Remove node from List
			if (lpPrev)
				lpPrev->lpNext = lpNext;
			else
				lpFormatRoot = lpNext;

			if (lpNext)
				lpNext->lpPrev = lpPrev;
			else
				lpFormatTail = lpPrev;

			// Destroy this node
			lpCurr->lpNext = NULL;
			lpCurr->lpPrev = NULL;
			delete lpCurr;
		
			// Move to next node in list
			lpCurr = lpNext;
		}

		cFormats	 = 0L;
		lpFormatRoot = NULL;
		lpFormatTail = NULL;

		// Mark as unloaded
		turnFormatsLoadedOff ();
	}

	// Success
	return DD_OK;
} // End D3DDevInfo::DestroyFormats 


  
/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::AddFormat
**  Purpose:    add new texture format to format list
**-----------------------------------------------------------------------------
*/

HRESULT D3DDevInfo::AddFormat (LPDDModeInfo lpFormatNew)
{
	// Check Parameters
	if (! lpFormatNew)
	{
		// Error, Invalid parameters
		return DDERR_INVALIDPARAMS;
	}

	// Add to tail of Format List
	lpFormatNew->lpPrev = lpFormatTail;
	lpFormatNew->lpNext = NULL;
	
	// Update tail
	if (lpFormatTail)
		lpFormatTail->lpNext = lpFormatNew;
	lpFormatTail = lpFormatNew;

	// Update Root
	if (! lpFormatRoot)
		lpFormatRoot = lpFormatNew;

	// Update count
	cFormats++;

	// Success
	return DD_OK;
} // End D3DDevInfo::AddFormat


  
/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::DelFormat
**  Purpose:    removes texture format from format list
**-----------------------------------------------------------------------------
*/

HRESULT D3DDevInfo::DelFormat (LPDDModeInfo lpFormatDel)
{
	// Check parameters
	if (lpFormatDel)
	{
		// Error
		return DDERR_INVALIDPARAMS;
	}

	// Remove this Mode From mode list
    LPDDModeInfo lpPrevMode = lpFormatDel->lpPrev;
    LPDDModeInfo lpNextMode = lpFormatDel->lpNext;

    if (lpPrevMode)
        lpPrevMode->lpNext = lpNextMode;
    else
        lpFormatRoot = lpNextMode;

    if (lpNextMode)
		lpNextMode->lpPrev = lpPrevMode;
	else
		lpFormatTail = lpPrevMode;

    // Destroy format node
	lpFormatDel->lpPrev = NULL;
    lpFormatDel->lpNext = NULL;
    delete lpFormatDel;

	// Update count
	cFormats--;

	// Success
	return DD_OK;
} // End D3DDevInfo::DelFormat
  

/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::FindFormat
**  Purpose:    finds specified texture format
**-----------------------------------------------------------------------------
*/

LPDDModeInfo D3DDevInfo::FindFormat (
	LPDDPIXELFORMAT lpddpf,			/* In:	Texture Format Desc */
	LPDDModeInfo *  lpNextBest,		/* Out:	Second best match */
	LPDDModeInfo    lpStart)		/* In:	start from this node */
{
	LPDDModeInfo	lpCurrFormat, lpNextFormat;

	// Get Starting node
	if (! lpStart)
		lpCurrFormat = lpFormatRoot;
	else
		lpCurrFormat = lpStart;

	if (lpNextBest)
		*lpNextBest = lpCurrFormat;

	// Search format list for best match
    while (lpCurrFormat)
	{
		lpNextFormat = lpCurrFormat->lpNext;
        if (lpCurrFormat->Match (*lpddpf))
        {
            return lpCurrFormat;
        }

		lpCurrFormat = lpNextFormat;
    }

    // Failure, user may use lpNextBest instead
    return NULL;
} // End D3DDevInfo::FindFormat


  
/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::FindFormat
**  Purpose:    finds specified texture format from requested BPP
**-----------------------------------------------------------------------------
*/

LPDDModeInfo D3DDevInfo::FindFormat (
	DWORD			bpp,			/* In:	requested BPP */
	LPDDModeInfo *  lpNextBest,		/* Out:	Second best match */
	LPDDModeInfo    lpStart)		/* In:	start from this node */
{
	LPDDModeInfo	lpCurrFormat, lpNextFormat;

	// Get Starting node
	if (! lpStart)
		lpCurrFormat = lpFormatRoot;
	else
		lpCurrFormat = lpStart;

	if (lpNextBest)
		*lpNextBest = lpCurrFormat;

	// Search format list for best match
    while (lpCurrFormat)
	{
		lpNextFormat = lpCurrFormat->lpNext;
        if (lpCurrFormat->Match (bpp))
        {
            return lpCurrFormat;
        }

		lpCurrFormat = lpNextFormat;
    }

    // Failure, user may use lpNextBest instead
    return NULL;
} // End D3DDevInfo::FindFormat


  
/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo::EnumFormats
**  Purpose:    Enumerate Formats in this Device
**-----------------------------------------------------------------------------
*/

DWORD D3DDevInfo::EnumFormats (const D3DDEV_ENUMINFO & eiInfo)
{
    DWORD           dwResult = ENUM_FAILURE;
	LPDDModeInfo	lpCurrFormat, lpNextFormat;

    // Check Match Callback function
    if (! eiInfo.fpcbEnum)
    {
        // Error, invalid callback
        return ENUM_ERROR;
    }
	
	// Get Starting node
	if (eiInfo.lpStart)
		lpCurrFormat = (LPDDModeInfo)(eiInfo.lpStart);
	else
		lpCurrFormat = lpFormatRoot;

	// Do callback on each node in list
	while (lpCurrFormat)
	{
		lpNextFormat = lpCurrFormat->lpNext;

        // Call Enum Mode Callback
        dwResult = (*eiInfo.fpcbEnum)((LPVOID)this, lpCurrFormat, eiInfo.dwExtra);

        // Check for early exit
        if (dwResult & ENUM_STOP)
        {
            // Return result code
            return (dwResult & ~ENUM_STOP);
        }
   }

    // Failure
    return (dwResult & ~ENUM_STOP);
} // End D3DDevInfo::EnumFormats



/*
**-----------------------------------------------------------------------------
**  DDDrvInfo Methods
**-----------------------------------------------------------------------------
*/
    
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::Create
**  Purpose:    Create a new Driver description
**  Notes:		
**		1.  If the driver doesn't support D3D we treat it as an invalid driver.
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvInfo::Create (
	GUID FAR *      lpGuid,
    LPTSTR          lpDriverName, 
    LPTSTR          lpDriverDesc)
{
    HRESULT         hResult;
    LPDIRECTDRAW    lpDD  = NULL;
    LPDIRECTDRAW2   lpDD2 = NULL;
    LPDIRECT3D2     lpD3D = NULL;

    if (isValid ())
    {
        // Programmer Error, already valid, call Fini to cleanup
        return FALSE;
    }


    // Copy GUID
    if (! lpGuid)
    {
        primaryOn ();
    }
    else
    {
        guid = *lpGuid;
    }

    // Copy Name
	LPTSTR	szTemp;
    if (! lpDriverName)
	{
		szTemp = TEXT("UNKNOWN");
	}
    else
    {
		szTemp = lpDriverName;
    }

	DWORD cLen = _tcslen (szTemp);
	DWORD cbSize = (cLen + 1) * sizeof(TCHAR);
	szName = (LPTSTR) malloc (cbSize);
	if (szName)
	{
		_tcsncpy (szName, szTemp, cLen);
		szName[cLen] = 0;
	}

    // Copy Desc
    if (! lpDriverDesc)
	{
		szTemp = TEXT("UNKNOWN");
	}
    else
    {
		szTemp = lpDriverDesc;
    }

	cLen = _tcslen (szTemp);
	cbSize = (cLen + 1) * sizeof(TCHAR);
	szDesc = (LPTSTR) malloc (cbSize);
	if (szDesc)
	{
		_tcsncpy (szDesc, szTemp, cLen);
		szDesc[cLen] = 0;
	}

    // Create DirectDraw Object
    hResult = DirectDrawCreate (lpGuid, &lpDD, NULL);
    if FAILED (hResult)
    {
        // Error
        REPORTERR (hResult);
        goto lblCLEANUP;
    }
    
    // Get The DirectDraw2 Interface
    hResult = lpDD->QueryInterface ((REFIID)IID_IDirectDraw2, (void **)&lpDD2);
    if (FAILED(hResult))
    {
        // Error
        REPORTERR (hResult);
        goto lblCLEANUP;
    }

    // Get The Direct3D Interface
    hResult = lpDD->QueryInterface ((REFIID)IID_IDirect3D2, (void **)&lpD3D);
    if (FAILED(hResult))
    {
        // Error
        REPORTERR (hResult);
        goto lblCLEANUP;
    }

    // Get The Driver Caps
    ddHalCaps.dwSize = sizeof(DDCAPS);
    ddHelCaps.dwSize = sizeof(DDCAPS);
    hResult = lpDD2->GetCaps (&ddHalCaps, &ddHelCaps);
    if (FAILED(hResult))
    {
        // Error
        REPORTERR (hResult);
        goto lblCLEANUP;
    }


	// Enumerate all Modes for this DD Driver
	cModes = 0L;
	turnModesLoadedOff ();
	
	hResult = LoadModes (lpDD2);
	if (FAILED (hResult))
		goto lblCLEANUP;

	
	// Enumerate all D3D Devices for this driver
	cDevices = 0L;
	turnDevicesLoadedOff ();

	hResult = LoadDevices (lpD3D);
	if (FAILED (hResult))
		goto lblCLEANUP;
        

    // Mark as Valid Driver
    validOn ();

    // Success
    hResult = DD_OK;

lblCLEANUP:
    // Cleanup the Interfaces before leaving
    if (lpD3D) 
    {
        lpD3D->Release ();
        lpD3D = NULL;
    }

    if (lpDD2) 
    {
        lpDD2->Release ();
        lpDD2 = NULL;
    }

    if (lpDD)
    {
        lpDD->Release ();
        lpDD = NULL;
    }

    return hResult;
} // End DDDrvInfo::Create


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::Destroy
**  Purpose:    Cleanup any memory or interfaces
**-----------------------------------------------------------------------------
*/

void DDDrvInfo::Destroy (void)
{
	// Destroy all Modes and Devices
	DestroyDevices ();
	DestroyModes ();

	// Clean up strings
	if (szDesc)
	{
		free (szDesc);
		szDesc = NULL;
	}

	if (szName)
	{
		free (szName);
		szName = NULL;
	}

	lpPrev = NULL;
	lpNext = NULL;

    // Mark as an invalid driver
    validOff ();
} // End DDDrvInfo::Destroy

  

/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::Match
**  Purpose:    checks for match with specified guid
**-----------------------------------------------------------------------------
*/

BOOL DDDrvInfo::Match (LPGUID lpGuid)
{
    if (! isValid())
        return FALSE;

    if (! lpGuid)
    {
        if (isPrimary())
            return TRUE;
    }
    else
    {
        if (*lpGuid == guid)
            return TRUE;
    }

    return FALSE;
} // End DDDrvInfo::Match


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::Match
**  Purpose:    checks for match with specified hal,hel caps
**-----------------------------------------------------------------------------
*/

BOOL DDDrvInfo::Match (LPDDCAPS lpHal, LPDDCAPS lpHel)
{
    if (! isValid())
        return FALSE;

    if ((! lpHal) && (! lpHel))
        return FALSE;

    // Check for match with hal caps
    if (lpHal)
    {
        // Flesh out later
    }

    if (lpHal)
    {
        // Flesh out later
    }

    // Success
    return TRUE;
} // End DDDrvInfo::Match



  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::LoadModes
**  Purpose:    Load all modes associated with this DD Driver
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvInfo::LoadModes (LPDIRECTDRAW2 lpDD2)
{
	// Have we already loaded the modes
	if (! modesLoaded ())
	{
		HRESULT hResult;
	    DD_CB_INFO		cbInfo;

		// Check Parameters
		if (! lpDD2)
		{
			hResult = APPERR_INVALIDPARAMS;
			REPORTERR (hResult);
			return hResult;
		}
		
	    // Enumerate all modes for this driver
		cbInfo.fResult  = TRUE;
		cbInfo.lpExtra  = (void *)this;
		cbInfo.cCount   = 0L;

		hResult = lpDD2->EnumDisplayModes (0L, NULL, &cbInfo, 
                                           ModeEnumCallback);
		if (FAILED(hResult))
		{
			// Error
			REPORTERR (hResult);
			return hResult;
		}

		// Double check count
		if ((! cbInfo.fResult) || (cbInfo.cCount == 0) || (cModes != cbInfo.cCount))
		{
			hResult = APPERR_GENERIC;
			REPORTERR (hResult);
			return hResult;
		}

		// Mark Modes as loaded
		turnModesLoadedOn ();
	}

	// Success
	return DD_OK;
} // D3DDevInfo::LoadModes




/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::DestroyModes
**  Purpose:    Destroys all Modes in Mode List
**-----------------------------------------------------------------------------
*/
  
HRESULT DDDrvInfo::DestroyModes (void)
{
	LPDDModeInfo lpCurr, lpNext, lpPrev;

	lpCurr = lpModeRoot;

	// Walk linked list and destroy all Mode nodes
	while (lpCurr)
	{
		lpNext = lpCurr->lpNext;
		lpPrev = lpCurr->lpPrev;

		// Remove node from List
		if (lpPrev)
			lpPrev->lpNext = lpNext;
		else
			lpModeRoot = lpNext;

		if (lpNext)
			lpNext->lpPrev = lpPrev;
		else
			lpModeTail = lpPrev;

		// Destroy this node
		lpCurr->lpNext = NULL;
		lpCurr->lpPrev = NULL;
		delete lpCurr;
	
		// Move to next node in list
		lpCurr = lpNext;
	}

	cModes	   = 0L;
	lpModeRoot = NULL;
	lpModeTail = NULL;

	turnModesLoadedOff ();

	// Success
	return DD_OK;
} // End DDDrvInfo::DestroyModes
  

  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::AddMode
**  Purpose:    add new mode to mode list
**  Notes:		1
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvInfo::AddMode (LPDDModeInfo lpModeNew)
{
	// Check Parameters
	if (! lpModeNew)
	{
		// Error, Invalid parameters
		return DDERR_INVALIDPARAMS;
	}

	// Add to tail of Mode List
	lpModeNew->lpPrev = lpModeTail;
	lpModeNew->lpNext = NULL;
	
	// Update tail
	if (lpModeTail)
		lpModeTail->lpNext = lpModeNew;
	lpModeTail = lpModeNew;

	// Update Root
	if (! lpModeRoot)
		lpModeRoot = lpModeNew;

	// Update count
	cModes++;

	// Success
	return DD_OK;
} // End DDDrvInfo::AddMode


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::DelMode
**  Purpose:    removes mode from mode list
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvInfo::DelMode (LPDDModeInfo lpModeDel)
{
	// Check parameters
	if (lpModeDel)
	{
		// Error
		return DDERR_INVALIDPARAMS;
	}

	// Remove this Mode From mode list
    LPDDModeInfo lpPrevMode = lpModeDel->lpPrev;
    LPDDModeInfo lpNextMode = lpModeDel->lpNext;

    if (lpPrevMode)
        lpPrevMode->lpNext = lpNextMode;
    else
        lpModeRoot = lpNextMode;

    if (lpNextMode)
		lpNextMode->lpPrev = lpPrevMode;
	else
		lpModeTail = lpPrevMode;

	lpModeDel->lpPrev = NULL;
    lpModeDel->lpNext = NULL;

    // Destroy mode node
    delete lpModeDel;

	// Update count
	cModes--;

	// Success
	return DD_OK;
} // End DDDrvInfo::DelMode
  


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::FindMode
**  Purpose:    finds specified mode
**-----------------------------------------------------------------------------
*/

LPDDModeInfo DDDrvInfo::FindMode (
	DWORD			dwW,			/* In:  Match this width */
	DWORD			dwH,			/* In:  Match this Height */
	DWORD			dwBPP,			/* In:  Match this Bits Per Pixel */
	DWORD           dwRefresh,		/* In:  Not supported yet... */
	LPDDModeInfo *	lpNextBest,		/* Out: Return next best match */
	LPDDModeInfo    lpStart) 		/* In:  Start search from this mode */
{
	LPDDModeInfo	lpCurrMode, lpNextMode;

	// Get Starting node
	if (! lpStart)
		lpCurrMode = lpModeRoot;
	else
		lpCurrMode = lpStart;

	if (lpNextBest)
		*lpNextBest = lpCurrMode;

	// Search mode list for best match
    while (lpCurrMode)
	{
		lpNextMode = lpCurrMode->lpNext;
        if (lpCurrMode->Match (dwW, dwH, dwBPP))
        {
			return lpCurrMode;
        }
        else if (lpCurrMode->Match (640, 480, 8))
        {
			if (lpNextBest)
				*lpNextBest = lpCurrMode;
        }
		lpCurrMode = lpNextMode;
    }

    // Failure, user may use lpNextBest instead
    return NULL;
} // End DDDrvInfo::FindMode 


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::FindMode
**  Purpose:    finds specified mode
**-----------------------------------------------------------------------------
*/

LPDDModeInfo DDDrvInfo::FindMode (
	LPDDSURFACEDESC lpddsd,			/* In:	Mode Desc */
	LPDDModeInfo *  lpNextBest,		/* Out:	Second best match */
	LPDDModeInfo    lpStart)		/* In:	start from this node */
{
	LPDDModeInfo	lpCurrMode, lpNextMode;

	// Get Starting node
	if (! lpStart)
		lpCurrMode = lpModeRoot;
	else
		lpCurrMode = lpStart;

	if (lpNextBest)
		*lpNextBest = lpCurrMode;

	// Search mode list for best match
    while (lpCurrMode)
	{
		lpNextMode = lpCurrMode->lpNext;
        if (lpCurrMode->Match (*lpddsd))
        {
            return lpCurrMode;
        }
        else if (lpCurrMode->Match (640, 480, 8))
        {
			if (lpNextBest)
				*lpNextBest = lpCurrMode;
        }
    }

    // Failure, user may use lpNextBest instead
    return NULL;
} // End DDDrvInfo::FindMode 


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::EnumModes
**  Purpose:    Enumerate modes in this Driver
**-----------------------------------------------------------------------------
*/

DWORD DDDrvInfo::EnumModes (const DDDRV_ENUMINFO & eiInfo)
{
    DWORD           dwResult = ENUM_FAILURE;
	LPDDModeInfo	lpCurrMode, lpNextMode;

    // Check Match Callback function
    if (! eiInfo.fpcbEnum)
    {
        // Error, invalid callback
        return ENUM_ERROR;
    }

	// Get Starting node
	if (eiInfo.lpStart)
		lpCurrMode = (LPDDModeInfo)(eiInfo.lpStart);
	else
		lpCurrMode = lpModeRoot;

	// Do callback on each node in list
	while (lpCurrMode)
	{
		lpNextMode = lpCurrMode->lpNext;

        // Call Enum Mode Callback
        dwResult = (*eiInfo.fpcbEnum)((LPVOID)this, lpCurrMode, eiInfo.dwExtra);

        // Check for early exit
        if (dwResult & ENUM_STOP)
        {
            // Return result code
            return (dwResult & ~ENUM_STOP);
        }
   }

    // Failure
    return (dwResult & ~ENUM_STOP);
} // End DDDrvInfo::EnumModes




/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::LoadDevices
**  Purpose:    Load all Devices associated with this DD Driver
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvInfo::LoadDevices (LPDIRECT3D2 lpD3D2)
{
	// Have we already loaded the D3D Devices for this driver
	if (! devicesLoaded ())
	{
		HRESULT hResult;
	    DD_CB_INFO		cbInfo;

		// Check Parameters
		if (! lpD3D2)
		{
			hResult = APPERR_INVALIDPARAMS;
			REPORTERR (hResult);
			return hResult;
		}
		
	    cbInfo.fResult  = TRUE;
	    cbInfo.lpExtra  = (void *)this;
		cbInfo.cCount   = 0L;

		hResult = lpD3D2->EnumDevices (DeviceEnumCallback, &cbInfo);
		if (FAILED(hResult))
		{
			REPORTERR (hResult);
			return hResult;
		}

		// Double check count
		if ((! cbInfo.fResult) || (cbInfo.cCount == 0) || (cDevices != cbInfo.cCount))
		{
			hResult = APPERR_GENERIC;
			REPORTERR (hResult);
			return hResult;
		}

		// Mark Devices as loaded
		turnDevicesLoadedOn ();
	}

	// Success
	return DD_OK;
} // DDDrvInfo::LoadDevices
  

  

/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::DestroyDevices
**  Purpose:    Destroys all Devices in Device List
**-----------------------------------------------------------------------------
*/
  
HRESULT DDDrvInfo::DestroyDevices (void)
{
	LPD3DDevInfo lpCurr, lpNext, lpPrev;

	lpCurr = lpDeviceRoot;

	// Walk linked list and destroy all D3D Device nodes
	while (lpCurr)
	{
		lpNext = lpCurr->lpNext;
		lpPrev = lpCurr->lpPrev;

		// Remove node from List
		if (lpPrev)
			lpPrev->lpNext = lpNext;
		else
			lpDeviceRoot = lpNext;

		if (lpNext)
			lpNext->lpPrev = lpPrev;
		else
			lpDeviceTail = lpPrev;

		// Destroy this node
		lpCurr->lpNext = NULL;
		lpCurr->lpPrev = NULL;
		delete lpCurr;
	
		// Move to next node in list
		lpCurr = lpNext;
	}

	cDevices	   = 0L;
	lpDeviceRoot = NULL;
	lpDeviceTail = NULL;

	turnDevicesLoadedOff ();

	// Success
	return DD_OK;
} // End DDDrvInfo::DestroyDevices
  

  

/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::AddDevice
**  Purpose:    add new D3D device to device list
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvInfo::AddDevice (LPD3DDevInfo lpDevNew)
{
	// Check Parameters
	if (! lpDevNew)
	{
		// Error, Invalid parameters
		return DDERR_INVALIDPARAMS;
	}

	// Add to tail of Mode List
	lpDevNew->lpPrev = lpDeviceTail;
	lpDevNew->lpNext = NULL;
	
	// Update tail
	if (lpDeviceTail)
		lpDeviceTail->lpNext = lpDevNew;
	lpDeviceTail = lpDevNew;

	// Update Root
	if (! lpDeviceRoot)
		lpDeviceRoot = lpDevNew;

	// Update count
	cDevices++;

	// Success
	return DD_OK;
} // End DDDrvInfo::AddDevice


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::DelDevice
**  Purpose:    removes D3D device from Device list
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvInfo::DelDevice (LPD3DDevInfo lpDevDel)
{
	// Check parameters
	if (lpDevDel)
	{
		// Error
		return DDERR_INVALIDPARAMS;
	}

	// Remove this Mode From mode list
    LPD3DDevInfo lpPrevDev = lpDevDel->lpPrev;
    LPD3DDevInfo lpNextDev = lpDevDel->lpNext;

    if (lpPrevDev)
        lpPrevDev->lpNext = lpNextDev;
    else
        lpDeviceRoot = lpNextDev;

    if (lpNextDev)
		lpNextDev->lpPrev = lpPrevDev;
	else
		lpDeviceTail = lpPrevDev;

	lpDevDel->lpPrev = NULL;
    lpDevDel->lpNext = NULL;

    // Destroy mode node
    delete lpDevDel;

	// Update count
	cDevices--;

	// Success
	return DD_OK;
} // End DDDrvInfo::DelDevice
  

  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::FindDevice
**  Purpose:    finds specified D3D Device
**-----------------------------------------------------------------------------
*/

LPD3DDevInfo DDDrvInfo::FindDevice (
	LPGUID			lpGuid, 
	LPD3DDevInfo *  lpNextBest,
	LPD3DDevInfo	lpStart)
{
	LPD3DDevInfo	lpCurrDev, lpNextDev;
	LPD3DDevInfo    lpHardware = NULL;
	LPD3DDevInfo	lpMMX = NULL;
	LPD3DDevInfo    lpRGB = NULL;
	LPD3DDevInfo    lpFirst = NULL;

	if (lpNextBest)
		*lpNextBest = NULL;

	// Get Root
	if (! lpStart)
		lpCurrDev = lpDeviceRoot;
	else
		lpCurrDev = lpStart;

	lpFirst = lpCurrDev;

	// Search mode list for best match
    while (lpCurrDev)
	{
		lpNextDev = lpCurrDev->lpNext;
        if (lpCurrDev->Match (lpGuid))
        {
			return lpCurrDev;
        }
        
		if (lpCurrDev->isHardware ())
        {
			if (! lpHardware)
				lpHardware = lpCurrDev;
        }
		
		if (lpCurrDev->guid == IID_IDirect3DRGBDevice)
		{
			if (! lpRGB)
				lpRGB = lpCurrDev;
		}

		if (lpCurrDev->guid == IID_IDirect3DMMXDevice)
		{
			if (! lpMMX)
				lpMMX = lpCurrDev;
		}

		lpCurrDev = lpNextDev;
    }

	if (lpNextBest)
	{
		if (lpHardware)
			*lpNextBest = lpHardware;
		else if (lpRGB)
			*lpNextBest = lpRGB;
		else if (lpMMX)
			*lpNextBest = lpMMX;
		else if (lpFirst)
			*lpNextBest = lpFirst;
	}

    // Failure, user may use lpNextBest instead
    return NULL;
} // End DDDrvInfo::FindDevice



/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::FindDevice
**  Purpose:    finds specified D3D Device
**-----------------------------------------------------------------------------
*/

LPD3DDevInfo DDDrvInfo::FindDevice (
    LPD3DDEVICEDESC lpHal, 
    LPD3DDEVICEDESC lpHel,
    LPD3DDevInfo *	lpNextBest,
	LPD3DDevInfo	lpStart)
{
	LPD3DDevInfo	lpCurrDev, lpNextDev;
	LPD3DDevInfo    lpHardware = NULL;
	LPD3DDevInfo	lpMMX = NULL;
	LPD3DDevInfo    lpRGB = NULL;
	LPD3DDevInfo    lpFirst = NULL;

	if (lpNextBest)
		*lpNextBest = NULL;

	// Get Root
	if (! lpStart)
		lpCurrDev = lpDeviceRoot;
	else
		lpCurrDev = lpStart;

	lpFirst = lpCurrDev;

	// Search mode list for best match
    while (lpCurrDev)
	{
		lpNextDev = lpCurrDev->lpNext;
        if (lpCurrDev->Match (lpHal, lpHel))
        {
			return lpCurrDev;
        }
        
		if (lpCurrDev->isHardware ())
        {
			if (! lpHardware)
				lpHardware = lpCurrDev;
        }

		if (lpCurrDev->guid == IID_IDirect3DRGBDevice)
		{
			if (! lpRGB)
				lpRGB = lpCurrDev;
		}

		if (lpCurrDev->guid == IID_IDirect3DMMXDevice)
		{
			if (! lpMMX)
				lpMMX = lpCurrDev;
		}

		lpCurrDev = lpNextDev;
    }

	if (lpNextBest)
	{
		if (lpHardware)
			*lpNextBest = lpHardware;
		else if (lpRGB)
			*lpNextBest = lpRGB;
		else if (lpMMX)
			*lpNextBest = lpMMX;
		else if (lpFirst)
			*lpNextBest = lpFirst;
	}

    // Failure
    return NULL;
} // End DDDrvInfo::FindDevice



  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::FindDeviceSupportsMode
**  Purpose:    finds specified D3D Device that is compatible
**				with specified mode
**  Notes;
**
**	1.  First looks for an exact match that is compatible
**  2.  Looks for a hardware device that is compatible
**	3.  Looks for any device that is compatible
**
**-----------------------------------------------------------------------------
*/

LPD3DDevInfo DDDrvInfo::FindDeviceSupportsMode (
	LPGUID			lpGuid, 
    LPDDModeInfo	lpMode,
	LPD3DDevInfo *  lpNextBest,
	LPD3DDevInfo	lpStart)
{
	LPD3DDevInfo	lpCurrDev, lpNextDev;


	// Check parameters
	if (! lpMode)
	{
		// Error, Invalid parameters
		if (lpNextBest)
			*lpNextBest = NULL;
		return NULL;
	}

	// Get Root
	if (! lpStart)
		lpCurrDev = lpDeviceRoot;
	else
		lpCurrDev = lpStart;

	if (lpNextBest)
	{
		if (lpMode->ModeSupported (lpCurrDev))
			*lpNextBest = lpCurrDev;
	}

	// Search mode list for best match
    while (lpCurrDev)
	{
		lpNextDev = lpCurrDev->lpNext;
        if (lpCurrDev->Match (lpGuid))
        {
				if (lpMode->ModeSupported (lpCurrDev))
					return lpCurrDev;
        }
        else if (lpCurrDev->isHardware ())
        {
			if (lpNextBest)
			{
				if (lpMode->ModeSupported (lpCurrDev))
					*lpNextBest = lpCurrDev;
			}
        }
		else if (lpMode->ModeSupported (lpCurrDev))
		{
			if (lpNextBest)
				*lpNextBest = lpCurrDev;
		}
		lpCurrDev = lpNextDev;
    }

    // Failure, user may use lpNextBest instead
    return NULL;
} // End DDDrvInfo::FindDeviceSupportsMode


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::FindModeSupportsDevice
**  Purpose:    finds specified mode that is compatible
**				with specified D3D device
**
**	1.  First trys for an exact mode, if it is compatible
**	2.  Then looks for 640, 480, 16, if it is compatible
**  3.  Then looks for any mode that is compatible
**
**-----------------------------------------------------------------------------
*/

LPDDModeInfo DDDrvInfo::FindModeSupportsDevice (
	DWORD			dwW, 
	DWORD			dwH, 
	DWORD			dwBPP,
	DWORD			dwRefresh,	/* In: Not supported yet ... */
	LPD3DDevInfo	lpDevice,
	LPDDModeInfo *	lpNextBest,
	LPDDModeInfo	lpStart)
{
	LPDDModeInfo	lpCurrMode, lpNextMode;

	// Check parameters
	if (! lpDevice)
	{
		// Error, Invalid parameters
		if (lpNextBest)
			*lpNextBest = NULL;
		return NULL;
	}

	// Get Root
	if (! lpStart)
		lpCurrMode = lpModeRoot;
	else
		lpCurrMode = lpStart;

	if (lpNextBest)
	{
		if (lpCurrMode->ModeSupported (lpDevice))
			*lpNextBest = lpCurrMode;
	}

	// Search mode list for best match
    while (lpCurrMode)
	{
		lpNextMode = lpCurrMode->lpNext;
        if (lpCurrMode->Match (dwW, dwH, dwBPP))
        {
				if (lpCurrMode->ModeSupported (lpDevice))
					return lpCurrMode;
        }
        else if (lpCurrMode->Match (640, 480, 16))
        {
			if (lpNextBest)
			{
				if (lpCurrMode->ModeSupported (lpDevice))
					*lpNextBest = lpCurrMode;
			}
        }
		else if (lpCurrMode->ModeSupported (lpDevice))
		{
			if (lpNextBest)
				*lpNextBest = lpCurrMode;
		}
		lpCurrMode = lpNextMode;
    }

    // Failure, user may use lpNextBest instead
    return NULL;
} // End DDDrvInfo::FindModeSupportsDevice



  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::EnumDevices
**  Purpose:    Enumerate D3D devices in driver
**-----------------------------------------------------------------------------
*/

DWORD DDDrvInfo::EnumDevices (const DDDRV_ENUMINFO & eiInfo)
{
    DWORD           dwResult = ENUM_FAILURE;
	LPD3DDevInfo	lpCurrDev, lpNextDev;

    // Check Match Callback function
    if (! eiInfo.fpcbEnum)
    {
        // Error, invalid callback
        return ENUM_ERROR;
    }

	// Get Starting node
	if (eiInfo.lpStart)
		lpCurrDev = (LPD3DDevInfo)(eiInfo.lpStart);
	else
		lpCurrDev = lpDeviceRoot;

	// Do callback on each node in list
	while (lpCurrDev)
	{
		lpNextDev = lpCurrDev->lpNext;

		dwResult = (*eiInfo.fpcbEnum)((LPVOID)this, lpCurrDev, eiInfo.dwExtra);
		
        // Check for early exit
        if (dwResult & ENUM_STOP)
		{
            // Return result code
            return (dwResult & ~ENUM_STOP);
		}

		lpCurrDev = lpNextDev;
	}

    // Failure
    return (dwResult & ~ENUM_STOP);
} // End DDDrvInfo::EnumDevices

  

/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo::GetGuid
**	Purpose:
**-----------------------------------------------------------------------------
*/

LPGUID DDDrvInfo::GetGuid (void)
{
	if (isPrimary ())
		return NULL;
	else
		return &guid;
} // End DDDrvInfo::GetGuid

  
/*
**-----------------------------------------------------------------------------
**  DDDrvMgr Methods
**-----------------------------------------------------------------------------
*/
  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::Init
**  Purpose:    Grovel all DD driver info in system 
**              and store in global DDInfo structures
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvMgr::Init (void)
{
    if (! isInitialized ())
    {        
        HRESULT         hResult;

		g_cDrivers = 0L;

		// Load all drivers in system
		hResult = LoadDrivers ();
		if (FAILED (hResult))
			return hResult;

        // Mark as initialized
        initOn ();
    }

	// Success
    return DD_OK;
} // End DDDrvMgr::Init


  
  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::Fini
**  Purpose:    Cleanup all global DDInfo structures
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvMgr::Fini (void)
{
    if (isInitialized ())
    {
		DestroyDrivers ();

        // Mark as not initialized
        initOff ();
    }

	// Success
    return DD_OK;
} // End DDDrvMgr::Fini



/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::LoadDrivers
**  Purpose:    Load all DD Drivers in system
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvMgr::LoadDrivers (void)
{
	HRESULT hResult;
    DD_CB_INFO		cbInfo;
		
    // Initialize all valid drivers in system
    cbInfo.fResult  = TRUE;
    cbInfo.cCount   = 0L;
    cbInfo.lpExtra  = (void *)NULL;

    hResult = DirectDrawEnumerate (DriverEnumCallback, &cbInfo);
	if (FAILED(hResult))
	{
		REPORTERR (hResult);
		return hResult;
	}

	// Double check count
	if ((! cbInfo.fResult) || (cbInfo.cCount == 0) || (g_cDrivers != cbInfo.cCount))
	{
		hResult = APPERR_GENERIC;
		REPORTERR (hResult);
		return hResult;
	}

	// Success
	return DD_OK;
} // DDDrvMgr::LoadDrivers
  

  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::DestroyDrivers
**  Purpose:    Destroys all Drivers in Driver List
**-----------------------------------------------------------------------------
*/
  
HRESULT DDDrvMgr::DestroyDrivers (void)
{
	LPDDDrvInfo lpCurr, lpNext, lpPrev;

	lpCurr = g_lpDriverRoot;

	// Walk linked list and destroy all D3D Device nodes
	while (lpCurr)
	{
		lpNext = lpCurr->lpNext;
		lpPrev = lpCurr->lpPrev;

		// Remove node from List
		if (lpPrev)
			lpPrev->lpNext = lpNext;
		else
			g_lpDriverRoot = lpNext;

		if (lpNext)
			lpNext->lpPrev = lpPrev;
		else
			g_lpDriverTail = lpPrev;

		// Destroy this node
		lpCurr->lpNext = NULL;
		lpCurr->lpPrev = NULL;
		delete lpCurr;
	
		// Move to next node in list
		lpCurr = lpNext;
	}

	g_cDrivers	 = 0L;
	g_lpDriverRoot = NULL;
	g_lpDriverTail = NULL;

	// Success
	return DD_OK;
} // End DDDrvMgr::DestroyDrivers

  

/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::DDDrvMgr
**  Purpose:    Default constructor
**-----------------------------------------------------------------------------
*/

DDDrvMgr::DDDrvMgr (void)
{
    lpCurrDriver    = NULL;
    lpCurrMode		= NULL;
    lpCurrDevice	= NULL;
} // End DDDrvMgr::DDDrvMgr


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::~DDDrvMgr
**-----------------------------------------------------------------------------
*/

DDDrvMgr::~DDDrvMgr (void)
{
} // End DDDrvMgr::~DDDrvMgr




  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::AddDriver
**  Purpose:    add new driver to driver list
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvMgr::AddDriver (LPDDDrvInfo lpDrvNew)
{
	// Check Parameters
	if (! lpDrvNew)
	{
		// Error, Invalid parameters
		return DDERR_INVALIDPARAMS;
	}

	// Add to tail of Mode List
	lpDrvNew->lpPrev = DDDrvMgr::g_lpDriverTail;
	lpDrvNew->lpNext = NULL;
	
	// Update tail
	if (DDDrvMgr::g_lpDriverTail)
		DDDrvMgr::g_lpDriverTail->lpNext = lpDrvNew;
	DDDrvMgr::g_lpDriverTail = lpDrvNew;

	// Update Root
	if (! DDDrvMgr::g_lpDriverRoot)
		DDDrvMgr::g_lpDriverRoot = lpDrvNew;

	// Update count
	g_cDrivers++;

	// Success
	return DD_OK;
} // End DDDrvMgr::AddDriver


  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::DelDriver
**  Purpose:    removes driver from driver list
**-----------------------------------------------------------------------------
*/

HRESULT DDDrvMgr::DelDriver (LPDDDrvInfo lpDrvDel)
{
	// Check parameters
	if (lpDrvDel)
	{
		// Error
		return DDERR_INVALIDPARAMS;
	}

	// Remove this Mode From mode list
    LPDDDrvInfo lpPrevDrv = lpDrvDel->lpPrev;
    LPDDDrvInfo lpNextDrv = lpDrvDel->lpNext;

    if (lpPrevDrv)
        lpPrevDrv->lpNext = lpNextDrv;
    else
		DDDrvMgr::g_lpDriverRoot = lpNextDrv;

    if (lpNextDrv)
		lpNextDrv->lpPrev = lpPrevDrv;
	else
		DDDrvMgr::g_lpDriverTail = lpPrevDrv;

	lpDrvDel->lpPrev = NULL;
    lpDrvDel->lpNext = NULL;

    // Destroy mode node
    delete lpDrvDel;

	// Update count
	g_cDrivers--;

	// Success
	return DD_OK;
} // End DDDrvMgr::DelDriver
  

  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::FindDriver
**  Purpose:    Finds driver corresponding to guid
**-----------------------------------------------------------------------------
*/

LPDDDrvInfo DDDrvMgr::FindDriver (
	LPGUID			lpGuid, 
	LPDDDrvInfo *	lpNextBest,
	LPDDDrvInfo		lpStart)
{
    LPDDDrvInfo lpCurrDrv, lpNextDrv;
    
#ifdef DEBUG
    if (! isInitialized ())
    {
        // Error, not initialized
        return NULL;
    }
#endif

    // Get Start node
	if (! lpStart)
		lpCurrDrv = DDDrvMgr::g_lpDriverRoot;
	else
		lpCurrDrv = lpStart;

	if (lpNextBest)
		*lpNextBest = lpCurrDrv;

	while (lpCurrDrv)
	{
		lpNextDrv = lpCurrDrv->lpNext;
		if (lpCurrDrv->Match (lpGuid))
		{
			// Success
			return lpCurrDrv;
		}
		else if (lpCurrDrv->isPrimary ())
		{
			if (lpNextBest)
				*lpNextBest = lpCurrDrv;
		}

		lpCurrDrv = lpNextDrv;
	}
    
    // Failure, user could use next best instead
    return NULL;
} // End DDDrvMgr::FindDriver
        

  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::FindDriver
**  Purpose:    Finds driver corresponding to specified device caps
**-----------------------------------------------------------------------------
*/

LPDDDrvInfo DDDrvMgr::FindDriver (
    LPDDCAPS	  lpHal, 
    LPDDCAPS	  lpHel, 
    LPDDDrvInfo * lpNextBest,
	LPDDDrvInfo   lpStart)
{
    LPDDDrvInfo lpCurrDrv, lpNextDrv;
    
    // Get Start node
	if (! lpStart)
		lpCurrDrv = DDDrvMgr::g_lpDriverRoot;
	else
		lpCurrDrv = lpStart;

	if (lpNextBest)
		*lpNextBest = lpCurrDrv;

	while (lpCurrDrv)
	{
		lpNextDrv = lpCurrDrv->lpNext;

		if (lpCurrDrv->Match (lpHal, lpHel))
		{
			// Success
			return lpCurrDrv;
		}
		else if (lpCurrDrv->isPrimary ())
		{
			if (lpNextBest)
				*lpNextBest = lpCurrDrv;
		}

		lpCurrDrv = lpNextDrv;
	}
    
    // Failure, user could use next best instead
    return NULL;
} // End DDDrvMgr::FindDriver

  
  
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr::EnumDrivers
**  Purpose:    Enumerate drivers
**-----------------------------------------------------------------------------
*/

DWORD DDDrvMgr::EnumDrivers (const DDDRV_ENUMINFO & eiInfo)
{
    DWORD		dwResult = ENUM_FAILURE;
	LPDDDrvInfo lpCurrDrv, lpNextDrv;

    // Check Match Callback function
    if (! eiInfo.fpcbEnum)
    {
        // Error, invalid callback
        return ENUM_ERROR;
    }

	// Get starting node
    if (eiInfo.lpStart)
		lpCurrDrv = (LPDDDrvInfo)(eiInfo.lpStart);
	else
		lpCurrDrv = DDDrvMgr::g_lpDriverRoot;

    // for each driver, call user defined match callback function
	while (lpCurrDrv)
	{
        lpNextDrv = lpCurrDrv->lpNext;

        // Call Enum Drivers Callback
        dwResult = (*eiInfo.fpcbEnum)((LPVOID)lpCurrDrv, NULL, eiInfo.dwExtra);

        // Check for early exit
        if (dwResult & ENUM_STOP)
        {
            // Return result code
            return (dwResult & ~ENUM_STOP);
        }
   }

    // Failure
    return (dwResult & ~ENUM_STOP);
} // End DDDrvMgr::EnumDrivers


/*
**-----------------------------------------------------------------------------
**  End of File
**-----------------------------------------------------------------------------
*/


