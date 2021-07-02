//-----------------------------------------------------------------------------
// File: D3DEnum.cpp
//
// Desc: Class enumerate through the DirectDraw drivers, Direct3D devices,
//       and the display modes available to each device.
//
//
// Copyright (c) 1997-1998 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------

#define STRICT
#include <stdio.h>

#include "D3DEnum.h"
#include "D3DUtil.h"
#include "Config.h"
#include "Log.h"

extern DWORD requestDriver;
extern DWORD requestFullscreen;
extern DWORD requestSoftware;
extern DWORD requestScreenW;
extern DWORD requestScreenH;

//-----------------------------------------------------------------------------
// Constants and function prototypes for the user select driver dialog
//-----------------------------------------------------------------------------
DLGTEMPLATE*  _BuildDriverSelectTemplate();
BOOL CALLBACK _DriverSelectProc( HWND, UINT, WPARAM, LPARAM );




//-----------------------------------------------------------------------------
// Global data for the enumerator functions
//-----------------------------------------------------------------------------
static LPDIRECTDRAW4 g_pDD                = NULL;  // Used for callbacks
static BOOL          g_bRefRastEnumerated = FALSE; // For the reference rast
static BOOL          g_bDevicesEnumerated = FALSE; // Used during enumeration

D3DEnum_DriverInfo*  g_pFirstDriver   = NULL; // List of DD drivers
D3DEnum_DriverInfo*  g_pDefaultDriver = NULL; // Default driver
D3DEnum_DriverInfo*  g_pCurrentDriver = NULL; // The selected DD driver

static HRESULT (*g_fnAppConfirmFn)(DDCAPS*, D3DDEVICEDESC*) = NULL;




//-----------------------------------------------------------------------------
// Local callback functions used during enumeration
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Name: EnumDisplayModesCallback()
// Desc: Callback function called for each display mode. Each available
//       display mode is added to a list for further choosing from the app.
//-----------------------------------------------------------------------------
static HRESULT WINAPI EnumDisplayModesCallback( DDSURFACEDESC2* pddsd,
                                                VOID* pvContext )
{
    // Check parameters
    if( NULL==pddsd || NULL==pvContext )
        return DDENUMRET_CANCEL;

    D3DEnum_DeviceInfo* pDevice = (D3DEnum_DeviceInfo*)pvContext;
    D3DEnum_ModeInfo*   pNewMode;
    DWORD dwBitDepth  = pDevice->ddDesc.dwDeviceRenderBitDepth;
    DWORD dwModeDepth = pddsd->ddpfPixelFormat.dwRGBBitCount;

	// Check mode for compatability with device. Skip 8-bit modes.
    if( (32==dwModeDepth) && (0==(dwBitDepth&DDBD_32)) ) return DDENUMRET_OK;
    if( (24==dwModeDepth) && (0==(dwBitDepth&DDBD_24)) ) return DDENUMRET_OK;
    if( (16==dwModeDepth) && (0==(dwBitDepth&DDBD_16)) ) return DDENUMRET_OK;
	if( ( 8==dwModeDepth) ) return DDENUMRET_OK;

    // Create a new mode structure
    if( NULL == ( pNewMode = new D3DEnum_ModeInfo ) )
        return DDENUMRET_CANCEL;

    // Initialize the new mode structure
    ZeroMemory( pNewMode, sizeof(D3DEnum_ModeInfo) );
    memcpy( &pNewMode->ddsd, pddsd, sizeof(DDSURFACEDESC2) );
    sprintf( pNewMode->strDesc, TEXT("%ld x %ld x %ld"), pddsd->dwWidth,
                                pddsd->dwHeight, dwModeDepth );

    // Link the new mode struct in the list of other display modes
	D3DEnum_ModeInfo** pMode = &pDevice->pFirstMode;
	while( *pMode )
		pMode = &((*pMode)->pNext);
	(*pMode) = pNewMode;
	
    // If this is a requestWidth x requestHeight x 16 mode, save it as the default mode
    if( ( requestScreenW == pddsd->dwWidth ) && ( requestScreenH == pddsd->dwHeight ) &&
        ( g_config.requestRGBbits == pddsd->ddpfPixelFormat.dwRGBBitCount ) )
        pDevice->pCurrentMode = pNewMode;
	
	if( NULL == pDevice->pCurrentMode )
        pDevice->pCurrentMode = pNewMode;

    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: Enum3DDevicesCallback()
// Desc: Callback function called for each DirectX 3D device. The driver's
//       attributes are added to the list of available drivers.
//-----------------------------------------------------------------------------
static HRESULT WINAPI Enum3DDevicesCallback( GUID* pGUID, LPSTR strDesc, 
                                LPSTR strName, LPD3DDEVICEDESC pHALDesc, 
                                LPD3DDEVICEDESC pHELDesc, LPVOID pvContext )
{
    D3DEnum_DriverInfo* pDriver = (D3DEnum_DriverInfo*)pvContext;
    D3DEnum_DeviceInfo* pNewDevice;

    // Check params
    if( NULL==pGUID || NULL==pHALDesc || NULL==pHELDesc || NULL==pDriver )
        return D3DENUMRET_CANCEL;

    // Handle specific device GUIDs. NullDevice renders nothing
    if( IsEqualGUID( *pGUID, IID_IDirect3DNullDevice ) )
        return D3DENUMRET_OK;

	// Set a flag so we know enumeration is working. This is just a feature to
	// help return more informative return codes later on.
    g_bDevicesEnumerated = TRUE;

	// Get info about this device.
    BOOL           bIsHardware = ( 0 != pHALDesc->dwFlags );
	D3DDEVICEDESC* pDesc       = bIsHardware ? pHALDesc : pHELDesc;

    // Only enumerate software rasterizers for the primary device (which has
	// a NULL GUID). This is to avoid duplicates
    if( pDriver->pGUID != NULL )
	    if( FALSE == bIsHardware )
			return D3DENUMRET_OK;


	// Give the app a chance to accept or reject this device, based on
	// what feature set it supports
	if( g_fnAppConfirmFn )
		if( FAILED( g_fnAppConfirmFn( &pDriver->ddDriverCaps, pDesc ) ) )
			return D3DENUMRET_OK;

    // Create a new D3D Driver struct
    if( NULL == ( pNewDevice = new D3DEnum_DeviceInfo ) )
        return D3DENUMRET_CANCEL;
    ZeroMemory( pNewDevice, sizeof(D3DEnum_DeviceInfo) );

    // Copy remaining device attributes
    memcpy( &pNewDevice->guid, pGUID, sizeof(GUID) );
    pNewDevice->pGUID = &pNewDevice->guid;
    strncpy( pNewDevice->strDesc, strDesc, 39 );
    memcpy( &pNewDevice->ddDesc, pDesc, sizeof(D3DDEVICEDESC) );
    pNewDevice->bIsHardware = bIsHardware;

    if( pNewDevice->bIsHardware )
        pDriver->pCurrentDevice = pNewDevice;
    else
	{
		if( NULL == pDriver->pCurrentDevice )
			if( D3DCOLOR_RGB & pHELDesc->dcmColorModel )
				pDriver->pCurrentDevice = pNewDevice;
	}

    // Enumerate the display modes
    g_pDD->EnumDisplayModes( 0, NULL, pNewDevice, EnumDisplayModesCallback );

	// Get the display mode's depth
	DDSURFACEDESC2 ddsd;
	ddsd.dwSize = sizeof(DDSURFACEDESC2);
	g_pDD->GetDisplayMode( &ddsd );
	DWORD dwDisplayBPP = ddsd.ddpfPixelFormat.dwRGBBitCount;

	// Set the initial bWindowed flag if the device can render in a window and
	// supports the current display mode
	if( pDriver->ddDriverCaps.dwCaps2 & DDCAPS2_CANRENDERWINDOWED )
	{
		for( D3DEnum_ModeInfo* pMode=pNewDevice->pFirstMode; pMode;
			 pMode = pMode->pNext )
		{
			if( pMode->ddsd.ddpfPixelFormat.dwRGBBitCount == dwDisplayBPP )
			{
				pNewDevice->bCompatbileWithDesktop = TRUE;
	            if( NULL == pDriver->pGUID )
				   	pNewDevice->bWindowed = TRUE;
			}
		}
	}

	if( pNewDevice->pFirstMode )
    {
		// Link it with the other D3D drivers in the DD Driver struct
		D3DEnum_DeviceInfo** pDevice = &pDriver->pFirstDevice;
		while( *pDevice )
			pDevice = &((*pDevice)->pNext);
		(*pDevice) = pNewDevice;
	}
	else
		// Device has no modes, so delete it
		delete pNewDevice;

    if( IsEqualGUID( *pGUID, IID_IDirect3DRefDevice ) )
        g_bRefRastEnumerated = TRUE;

    return D3DENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: DirectDrawEnumCallbackEx()
// Desc: Callback function called for each DirectDraw driver. Unless we have
//       multimon or a type of card which uses a separate 2D card in
//       conjunction with the 3D card, this is only done once.
//-----------------------------------------------------------------------------
static BOOL WINAPI DirectDrawEnumCallbackEx( GUID FAR* pGUID, LPSTR strDesc,
											 LPSTR strName, VOID*,
											 HMONITOR hMonitor )
{
    // Use the GUID to create the DirectDraw object, so that information
    // can be extracted from it.
	LPDIRECTDRAW pDD;
    if( FAILED( DirectDrawCreate( pGUID, &pDD, 0L ) ) )
    {
		DEBUG_MSG( TEXT("Can't create DDraw during enumeration!") );
        return D3DENUMRET_OK;
	}

    // Query the DirectDraw driver for access to Direct3D.
    if( FAILED( pDD->QueryInterface( IID_IDirectDraw4, (VOID**)&g_pDD ) ) )
	{
        DEBUG_MSG( TEXT("Can't query IDirectDraw4 during enumeration!") );
		pDD->Release();
		return D3DENUMRET_OK;
	}
	pDD->Release();

    // Query the DirectDraw driver for access to Direct3D.
    LPDIRECT3D3 pD3D;
    if( FAILED( g_pDD->QueryInterface( IID_IDirect3D3, (VOID**)&pD3D ) ) )
	{
        DEBUG_MSG( TEXT("Can't query IDirect3D3 during enumeration!") );
		g_pDD->Release();
		return D3DENUMRET_OK;
	}

    // Copy the DDDriver info into a new DriverInfo struct
    D3DEnum_DriverInfo* pNewDriver = new D3DEnum_DriverInfo;
    if( NULL == pNewDriver )
		return D3DENUMRET_CANCEL;

    ZeroMemory( pNewDriver, sizeof(D3DEnum_DriverInfo) );

    // Copy the GUID (if specified) and the driver name
    if( NULL != pGUID  )
    {
        memcpy( &pNewDriver->guid, pGUID, sizeof(GUID) );
        pNewDriver->pGUID = &pNewDriver->guid;
    }
    strncpy( pNewDriver->strDesc, strDesc, 39 );
    strncpy( pNewDriver->strName, strName, 39 );
	pNewDriver->hMonitor = hMonitor;

    // Get the caps bits for the driver
    pNewDriver->ddDriverCaps.dwSize = sizeof(DDCAPS);
    pNewDriver->ddHELCaps.dwSize    = sizeof(DDCAPS);
    g_pDD->GetCaps( &pNewDriver->ddDriverCaps, &pNewDriver->ddHELCaps );

	// Now, enumerate all the 3D devices
    pD3D->EnumDevices( Enum3DDevicesCallback, pNewDriver );

	if( pNewDriver->pFirstDevice )
	{
	    // Link the new DDDriver with the global list
		D3DEnum_DriverInfo** pDriver = &g_pFirstDriver;
		while( *pDriver )
			pDriver = &((*pDriver)->pNext);
		(*pDriver) = pNewDriver;

		// Decide if this is a good default driver
		if( NULL == pGUID )
			g_pCurrentDriver = pNewDriver;
	}
	else
		// Driver has no devices, so delete it
		delete pNewDriver;

    pD3D->Release();
    g_pDD->Release();
    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: DirectDrawEnumCallback()
// Desc: Non-mulitmon version of the ddraw enumeration callback
//-----------------------------------------------------------------------------
static BOOL WINAPI DirectDrawEnumCallback( GUID FAR* pGUID, LPSTR strDesc,
                                           LPSTR strName, VOID* )
{
	return DirectDrawEnumCallbackEx( pGUID, strDesc, strName, NULL, NULL );
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_FreeResources()
// Desc: Frees all resources used for driver enumeration
//-----------------------------------------------------------------------------
VOID D3DEnum_FreeResources()
{
    // Loop through each driver, and delete everything
    while( g_pFirstDriver )
    {
        D3DEnum_DriverInfo* pDriverVictim = g_pFirstDriver;
        g_pFirstDriver = g_pFirstDriver->pNext;

        while( pDriverVictim->pFirstDevice )
        {
            D3DEnum_DeviceInfo* pDeviceVictim = pDriverVictim->pFirstDevice;
            pDriverVictim->pFirstDevice = pDeviceVictim->pNext;
   
            while( pDeviceVictim->pFirstMode )
            {
                D3DEnum_ModeInfo* pModeVictim = pDeviceVictim->pFirstMode;
                pDeviceVictim->pFirstMode = pModeVictim->pNext;
                delete pModeVictim;
            }
            delete pDeviceVictim;
        }
        delete pDriverVictim;
    }
}




//-----------------------------------------------------------------------------
// Name: RefreshListForDesktopCompatibility()
// Desc: Loops through list of devices, and marks a flag for whether the device
//       is compatible with the desktop bit depth.
//-----------------------------------------------------------------------------
static VOID RefreshListForDesktopCompatibility()
{
	// Get the currect display mode description
	LPDIRECTDRAW  pDD;
	DDSURFACEDESC ddsd;
	if( FAILED( DirectDrawCreate( NULL, &pDD, NULL ) ) )
		return;
	ddsd.dwSize = sizeof(DDSURFACEDESC);
	pDD->GetDisplayMode( &ddsd );
	pDD->Release();

	// Get the display mode's depth
	DWORD dwDisplayBPP = ddsd.ddpfPixelFormat.dwRGBBitCount;

	// Loop through the devices, and check if any modes works with the current
	// display setting
	for( D3DEnum_DriverInfo* pDriver = g_pFirstDriver; pDriver;
	     pDriver = pDriver->pNext )
	{
		for( D3DEnum_DeviceInfo* pDevice=pDriver->pFirstDevice; pDevice;
			 pDevice = pDevice->pNext )
		{
			pDevice->bCompatbileWithDesktop = FALSE;

			for( D3DEnum_ModeInfo* pMode=pDevice->pFirstMode; pMode;
				 pMode = pMode->pNext )
			{
				if( pMode->ddsd.ddpfPixelFormat.dwRGBBitCount == dwDisplayBPP )
					pDevice->bCompatbileWithDesktop = TRUE;
			}
		}
	}
}



//-----------------------------------------------------------------------------
// Name: D3DEnum_EnumerateDevices()
// Desc: Enumerates all drivers, devices, and modes. The optional app-supplied
//       callback is called for each enumerated device, to confirm that the
//       device supports the feature set required by the app.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_EnumerateDevices( 
							HRESULT (*AppConfirmFn)(DDCAPS*, D3DDEVICEDESC*) )
{
	g_fnAppConfirmFn     = AppConfirmFn;
    g_bRefRastEnumerated = FALSE;

	// We need to manually get the procedure address for the DDrawEnumEx()
	// function.
	HMODULE hDDrawDLL = GetModuleHandle("DDRAW.DLL");
	if( NULL == hDDrawDLL )
	{
		DEBUG_MSG( TEXT("Can't load DDRAW.DLL!") );
		return D3DENUMERR_NODIRECTDRAW;
	}

	// Find the DDraw enumeration function, and call it
	LPDIRECTDRAWENUMERATEEX pDDrawEnumFn = (LPDIRECTDRAWENUMERATEEX)
		             GetProcAddress( hDDrawDLL, "DirectDrawEnumerateExA" );

	if( pDDrawEnumFn )
		pDDrawEnumFn( DirectDrawEnumCallbackEx, NULL,
					  DDENUM_ATTACHEDSECONDARYDEVICES |
					  DDENUM_DETACHEDSECONDARYDEVICES |
					  DDENUM_NONDISPLAYDEVICES );
	else
        DirectDrawEnumerate( DirectDrawEnumCallback, NULL );

	// Select a driver. Ask for a hardware device that renders in a window
	return D3DEnum_SelectDefaultDriver( requestDriver | requestFullscreen |
		requestSoftware);
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_SelectDefaultDriver()
// Desc: Picks a default driver according to the passed in flags.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_SelectDefaultDriver( DWORD dwFlags )
{
	// Refresh the list of devices to mark which devices (if any) are
	// compatible with the current desktop (ie. can render in a window).
	RefreshListForDesktopCompatibility();

	// If a specific driver was requested, perform that search here
	if( dwFlags & 0x0000003c )
	{
		for( D3DEnum_DriverInfo* pDriver = g_pFirstDriver; pDriver;
			 pDriver = pDriver->pNext )
		{
			for( D3DEnum_DeviceInfo* pDevice = pDriver->pFirstDevice; pDevice;
				 pDevice = pDevice->pNext )
			{
				BOOL bFound = FALSE;

			    if( IsEqualGUID( *pDevice->pGUID, IID_IDirect3DRGBDevice ) )
				{
					if( dwFlags & D3DENUM_RGBEMULATION )
						bFound = TRUE;
				}
			    else if( IsEqualGUID( *pDevice->pGUID, IID_IDirect3DRefDevice ) )
				{
					if( dwFlags & D3DENUM_REFERENCERAST )
						bFound = TRUE;
				}
				else
				{
					if( dwFlags & D3DENUM_PRIMARYHAL )
						if( pDriver == g_pFirstDriver )
							bFound = TRUE;
					if( dwFlags & D3DENUM_SECONDARYHAL )
						if( pDriver != g_pFirstDriver )
							bFound = TRUE;
				}
				
				if( bFound )
				{
					g_pCurrentDriver = pDriver;
					g_pCurrentDriver->pCurrentDevice = pDevice;
					return S_OK;
				}
			}
		}
		LOGOUT("Requested D3D driver not found");
		return D3DENUMERR_NOTFOUND;
	}

	// Do 4 passes, looping through drivers, devices and modes. The 1st pass
	// searches for hardware. The 2nd pass looks for software devices. The
	// final two passes allow fullscreen modes.
	for( WORD pass=0; pass<4; pass++ )
	{
		BOOL bSeekHardware = ( pass & 0x1 ) ? FALSE : TRUE;
		BOOL bSeekWindowed = ( pass & 0x2 ) ? FALSE : TRUE;
		
		// Skip the passes we aren't allowing
		if( (TRUE==bSeekHardware) && (dwFlags&D3DENUM_SOFTWAREONLY) )			
			continue;
		if( (TRUE==bSeekWindowed) && (dwFlags&D3DENUM_FULLSCREENONLY) )			
			continue;

		for( D3DEnum_DriverInfo* pDriver = g_pFirstDriver; pDriver;
		     pDriver = pDriver->pNext )
		{
			DDCAPS* pCaps = &pDriver->ddDriverCaps;

			if( bSeekWindowed )
				if( 0 == ( pCaps->dwCaps2 & DDCAPS2_CANRENDERWINDOWED ) )
					continue;

			for( D3DEnum_DeviceInfo* pDevice = pDriver->pFirstDevice; pDevice;
			     pDevice = pDevice->pNext )
			{
				if( bSeekHardware != pDevice->bIsHardware )
					continue;
				if( bSeekWindowed && FALSE == pDevice->bCompatbileWithDesktop )
					continue;

				pDevice->bWindowed = bSeekWindowed;
				g_pCurrentDriver   = pDriver;
				g_pCurrentDriver->pCurrentDevice = pDevice;

				return S_OK;
			}
		}
	}

	// No compatible devices were found. Return an error code
    if( FALSE == g_bDevicesEnumerated ) {
		LOGOUT("D3D driver enumeration failed");
		return D3DENUMERR_ENUMERATIONFAILED; // Enumeration really did fail
	}
    if( FALSE == g_bRefRastEnumerated ) {
		LOGOUT("Only available D3D driver is RefRast");
		return D3DENUMERR_SUGGESTREFRAST;    // Suggest enabling the RefRast
	}

	LOGOUT("No compatible D3D devices");
	return D3DENUMERR_NOCOMPATIBLEDEVICES;
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_UserDlgSelectDriver()
// Desc: Displays a dialog box for the user to select a driver/device/mode.
//       The return values are akin to the Windows DialogBox() function.
//-----------------------------------------------------------------------------
INT D3DEnum_UserDlgSelectDriver( HWND hwndParent, BOOL bCurrentlyWindowed )
{
	INT nResult = -1;

    // Check in case drivers weren't properly enumerated beforehand.
    if( NULL == g_pCurrentDriver )
        return -1;

	// Refresh the list of devices to mark which devices (if any) are
	// compatible with the current desktop (ie. can render in a window).
	RefreshListForDesktopCompatibility();

	// Match the current windowed-vs-fullscreen state
	g_pCurrentDriver->pCurrentDevice->bWindowed = bCurrentlyWindowed;

    // Pop up a dialog box for the user's choice of driver/device/mode
    HINSTANCE hInstance = (HINSTANCE)GetWindowLong( hwndParent, 
                                                    GWL_HINSTANCE );

    // Create dynamic dialog template
    DLGTEMPLATE* pDlgSelect = _BuildDriverSelectTemplate();
    if( pDlgSelect )
	{
	    // Create dialog box from template
		nResult = DialogBoxIndirectParam( hInstance, pDlgSelect, hwndParent,
                                          (DLGPROC)_DriverSelectProc, 0L );
	    delete pDlgSelect;
	}

    return nResult;
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_GetSelectedDriver()
// Desc: Returns the currently selected driver, device, and display mode
//-----------------------------------------------------------------------------
HRESULT D3DEnum_GetSelectedDriver( LPGUID* ppDriverGUID, LPGUID* ppDeviceGUID, 
                                   LPDDSURFACEDESC2* ppddsd, BOOL* pbWindowed,
								   BOOL* pbIsHardware )

{
    // Check parans
    if( (!ppDriverGUID) || (!ppDeviceGUID) )
        return E_INVALIDARG;

    // Abort if things weren't setup correctly
    if( NULL == g_pCurrentDriver )
        return D3DENUMERR_ENUMERATIONFAILED;

    // Copy the driver and device GUID ptrs
    (*ppDriverGUID) = g_pCurrentDriver->pGUID;
    (*ppDeviceGUID) = g_pCurrentDriver->pCurrentDevice->pGUID;
	
	if( ppddsd )
		(*ppddsd) = &g_pCurrentDriver->pCurrentDevice->pCurrentMode->ddsd;
	if( pbWindowed )
		(*pbWindowed) = g_pCurrentDriver->pCurrentDevice->bWindowed;
	if( pbIsHardware )
		(*pbIsHardware) = g_pCurrentDriver->pCurrentDevice->bIsHardware;

	return S_OK;
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_GetSelectedDriver()
// Desc: Returns the currently selected driver, device, and display mode
//-----------------------------------------------------------------------------
HRESULT D3DEnum_GetSelectedDriver( D3DEnum_DriverInfo** ppDriverInfo,
								   D3DEnum_DeviceInfo** ppDeviceInfo )

{
    // Abort if things weren't setup correctly
    if( NULL == g_pCurrentDriver )
        return D3DENUMERR_ENUMERATIONFAILED;

    // Copy the driver and device info ptrs
    if( ppDriverInfo ) *ppDriverInfo = g_pCurrentDriver;
    if( ppDeviceInfo ) *ppDeviceInfo = g_pCurrentDriver->pCurrentDevice;
	
	return S_OK;
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_GetFirstDriver()
// Desc: Returns a ptr to the first DriverInfo structure in the list.
//-----------------------------------------------------------------------------
D3DEnum_DriverInfo* D3DEnum_GetFirstDriver()
{
    return g_pFirstDriver;
}





