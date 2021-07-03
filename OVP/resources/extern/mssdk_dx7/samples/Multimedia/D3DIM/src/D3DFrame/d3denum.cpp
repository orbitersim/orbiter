//-----------------------------------------------------------------------------
// File: D3DEnum.cpp
//
// Desc: Functions to enumerate DDraw/D3D drivers, devices, and modes.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------
#define STRICT
#include <windowsx.h>
#include <stdio.h>
#include <tchar.h>
#include "D3DEnum.h"
#include "D3DUtil.h" // For DEBUG_MSG
#include "D3DRes.h"  // For dialog controls




//-----------------------------------------------------------------------------
// Global data for the enumerator functions
//-----------------------------------------------------------------------------
static HRESULT (*g_fnAppConfirmFn)(DDCAPS*, D3DDEVICEDESC7*) = NULL;

static D3DEnum_DeviceInfo g_pDeviceList[20];
static DWORD g_dwNumDevicesEnumerated = 0L;
static DWORD g_dwNumDevices           = 0L;




//-----------------------------------------------------------------------------
// Name: SortModesCallback()
// Desc: Callback function for sorting display modes.
//-----------------------------------------------------------------------------
int SortModesCallback( const VOID* arg1, const VOID* arg2 )
{
    DDSURFACEDESC2* p1 = (DDSURFACEDESC2*)arg1;
    DDSURFACEDESC2* p2 = (DDSURFACEDESC2*)arg2;

    if( p1->dwWidth < p2->dwWidth )
        return -1;
    if( p1->dwWidth > p2->dwWidth )
        return +1;

    if( p1->dwHeight < p2->dwHeight )
        return -1;
    if( p1->dwHeight > p2->dwHeight )
        return +1;

    if( p1->ddpfPixelFormat.dwRGBBitCount < p2->ddpfPixelFormat.dwRGBBitCount )
        return -1;
    if( p1->ddpfPixelFormat.dwRGBBitCount > p2->ddpfPixelFormat.dwRGBBitCount )
        return +1;

    return 0;
}




//-----------------------------------------------------------------------------
// Name: ModeEnumCallback()
// Desc: Callback function for enumerating display modes.
//-----------------------------------------------------------------------------
static HRESULT WINAPI ModeEnumCallback( DDSURFACEDESC2* pddsd,
                                        VOID* pParentInfo )
{
    D3DEnum_DeviceInfo* pDevice = (D3DEnum_DeviceInfo*)pParentInfo;

    // Reallocate storage for the modes
    DDSURFACEDESC2* pddsdNewModes = new DDSURFACEDESC2[pDevice->dwNumModes+1];
    memcpy( pddsdNewModes, pDevice->pddsdModes,
            pDevice->dwNumModes * sizeof(DDSURFACEDESC2) );
    delete pDevice->pddsdModes;
    pDevice->pddsdModes = pddsdNewModes;

    // Add the new mode
    pDevice->pddsdModes[pDevice->dwNumModes++] = (*pddsd);

    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: DeviceEnumCallback()
// Desc: Callback function for enumerating devices
//-----------------------------------------------------------------------------
static HRESULT WINAPI DeviceEnumCallback( TCHAR* strDesc, TCHAR* strName,
                                          D3DDEVICEDESC7* pDesc,
                                          VOID* pParentInfo )
{
    // Keep track of # of devices that were enumerated
    g_dwNumDevicesEnumerated++;

    D3DEnum_DeviceInfo* pDriverInfo = (D3DEnum_DeviceInfo*)pParentInfo;
    D3DEnum_DeviceInfo* pDeviceInfo = &g_pDeviceList[g_dwNumDevices];
    ZeroMemory( pDeviceInfo, sizeof(D3DEnum_DeviceInfo) );

    // Select either the HAL or HEL device desc:
    pDeviceInfo->bHardware = pDesc->dwDevCaps & D3DDEVCAPS_HWRASTERIZATION;
    memcpy( &pDeviceInfo->ddDeviceDesc, pDesc, sizeof(D3DDEVICEDESC7) );

    // Set up device info for this device
    pDeviceInfo->bDesktopCompatible = pDriverInfo->bDesktopCompatible;
    pDeviceInfo->ddDriverCaps       = pDriverInfo->ddDriverCaps;
    pDeviceInfo->ddHELCaps          = pDriverInfo->ddHELCaps;
    pDeviceInfo->guidDevice         = pDesc->deviceGUID;
    pDeviceInfo->pDeviceGUID        = &pDeviceInfo->guidDevice;
    pDeviceInfo->pddsdModes         = new DDSURFACEDESC2[pDriverInfo->dwNumModes];

    // Copy the driver GUID and description for the device
    if( pDriverInfo->pDriverGUID )
    {
        pDeviceInfo->guidDriver  = pDriverInfo->guidDriver;
        pDeviceInfo->pDriverGUID = &pDeviceInfo->guidDriver;
        lstrcpyn( pDeviceInfo->strDesc, pDriverInfo->strDesc, 39 );
    }
    else
    {
        pDeviceInfo->pDriverGUID = NULL;
        lstrcpyn( pDeviceInfo->strDesc, strName, 39 );
    }

    // Avoid duplicates: only enum HW devices for secondary DDraw drivers.
    if( NULL != pDeviceInfo->pDriverGUID && FALSE == pDeviceInfo->bHardware )
            return D3DENUMRET_OK;

    // Give the app a chance to accept or reject this device.
    if( g_fnAppConfirmFn )
        if( FAILED( g_fnAppConfirmFn( &pDeviceInfo->ddDriverCaps,
                                      &pDeviceInfo->ddDeviceDesc ) ) )
            return D3DENUMRET_OK;

    // Build list of supported modes for the device
    for( DWORD i=0; i<pDriverInfo->dwNumModes; i++ )
    {
        DDSURFACEDESC2 ddsdMode = pDriverInfo->pddsdModes[i];
        DWORD dwRenderDepths    = pDeviceInfo->ddDeviceDesc.dwDeviceRenderBitDepth;
        DWORD dwDepth           = ddsdMode.ddpfPixelFormat.dwRGBBitCount;

        // Accept modes that are compatable with the device
        if( ( ( dwDepth == 32 ) && ( dwRenderDepths & DDBD_32 ) ) ||
            ( ( dwDepth == 24 ) && ( dwRenderDepths & DDBD_24 ) ) ||
            ( ( dwDepth == 16 ) && ( dwRenderDepths & DDBD_16 ) ) )
        {
            // Copy compatible modes to the list of device-supported modes
            pDeviceInfo->pddsdModes[pDeviceInfo->dwNumModes++] = ddsdMode;

            // Record whether the device has any stereo modes
            if( ddsdMode.ddsCaps.dwCaps2 & DDSCAPS2_STEREOSURFACELEFT )
                pDeviceInfo->bStereoCompatible = TRUE;
        }
    }

    // Bail if the device has no supported modes
    if( 0 == pDeviceInfo->dwNumModes )
        return D3DENUMRET_OK;

    // Find a 640x480x16 mode for the default fullscreen mode
    for( i=0; i<pDeviceInfo->dwNumModes; i++ )
    {
        if( ( pDeviceInfo->pddsdModes[i].dwWidth == 640 ) &&
            ( pDeviceInfo->pddsdModes[i].dwHeight == 480 ) &&
            ( pDeviceInfo->pddsdModes[i].ddpfPixelFormat.dwRGBBitCount == 16 ) )
        {
            pDeviceInfo->ddsdFullscreenMode = pDeviceInfo->pddsdModes[i];
            pDeviceInfo->dwCurrentMode      = i;
        }
    }

    // Select whether the device is initially windowed
    pDeviceInfo->bWindowed = pDeviceInfo->bDesktopCompatible;

    // Accept the device and return
    g_dwNumDevices++;

    return D3DENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: DriverEnumCallback()
// Desc: Callback function for enumerating drivers.
//-----------------------------------------------------------------------------
static BOOL WINAPI DriverEnumCallback( GUID* pGUID, TCHAR* strDesc,
                                       TCHAR* strName, VOID*, HMONITOR )
{
    D3DEnum_DeviceInfo d3dDeviceInfo;
    LPDIRECTDRAW7      pDD;
    LPDIRECT3D7        pD3D;
    HRESULT            hr;
    
    // Use the GUID to create the DirectDraw object
    hr = DirectDrawCreateEx( pGUID, (VOID**)&pDD, IID_IDirectDraw7, NULL );
    if( FAILED(hr) )
    {
        DEBUG_MSG( _T("Can't create DDraw during enumeration!") );
        return D3DENUMRET_OK;
    }

    // Create a D3D object, to enumerate the d3d devices
    hr = pDD->QueryInterface( IID_IDirect3D7, (VOID**)&pD3D );
    if( FAILED(hr) )
    {
        pDD->Release();
        DEBUG_MSG( _T("Can't query IDirect3D7 during enumeration!") );
        return D3DENUMRET_OK;
    }

    // Copy data to a device info structure
    ZeroMemory( &d3dDeviceInfo, sizeof(d3dDeviceInfo) );
    lstrcpyn( d3dDeviceInfo.strDesc, strDesc, 39 );
    d3dDeviceInfo.ddDriverCaps.dwSize = sizeof(DDCAPS);
    d3dDeviceInfo.ddHELCaps.dwSize    = sizeof(DDCAPS);
    pDD->GetCaps( &d3dDeviceInfo.ddDriverCaps, &d3dDeviceInfo.ddHELCaps );
    if( pGUID )
    {
        d3dDeviceInfo.guidDriver = (*pGUID);
        d3dDeviceInfo.pDriverGUID = &d3dDeviceInfo.guidDriver;
    }

    // Record whether the device can render into a desktop window
    if( d3dDeviceInfo.ddDriverCaps.dwCaps2 & DDCAPS2_CANRENDERWINDOWED )
        if( NULL == d3dDeviceInfo.pDriverGUID )
            d3dDeviceInfo.bDesktopCompatible = TRUE;

    // Enumerate the fullscreen display modes.
    pDD->EnumDisplayModes( 0, NULL, &d3dDeviceInfo, ModeEnumCallback );

    // Sort list of display modes
    qsort( d3dDeviceInfo.pddsdModes, d3dDeviceInfo.dwNumModes,
           sizeof(DDSURFACEDESC2), SortModesCallback );

    // Now, enumerate all the 3D devices
    pD3D->EnumDevices( DeviceEnumCallback, &d3dDeviceInfo );

    // Clean up and return
    SAFE_DELETE( d3dDeviceInfo.pddsdModes );
    pD3D->Release();
    pDD->Release();

    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_EnumerateDevices()
// Desc: Enumerates all drivers, devices, and modes. The callback function is
//       called each device, to confirm that the device supports the feature
//       set required by the app.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_EnumerateDevices( HRESULT (*AppConfirmFn)(DDCAPS*, D3DDEVICEDESC7*) )
{
    // Store the device enumeration callback function
    g_fnAppConfirmFn = AppConfirmFn;

    // Enumerate drivers, devices, and modes
    DirectDrawEnumerateEx( DriverEnumCallback, NULL, 
                           DDENUM_ATTACHEDSECONDARYDEVICES |
                           DDENUM_DETACHEDSECONDARYDEVICES |
                           DDENUM_NONDISPLAYDEVICES );

    // Make sure devices were actually enumerated
    if( 0 == g_dwNumDevicesEnumerated )
    {
        DEBUG_MSG( _T("No devices and/or modes were enumerated!") );
        return D3DENUMERR_ENUMERATIONFAILED;
    }
    if( 0 == g_dwNumDevices )
    {
        DEBUG_MSG( _T("No enumerated devices were accepted!") );
        DEBUG_MSG( _T("Try enabling the D3D Reference Rasterizer.") );
        return D3DENUMERR_SUGGESTREFRAST;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_FreeResources()
// Desc: Cleans up any memory allocated during device enumeration
//-----------------------------------------------------------------------------
VOID D3DEnum_FreeResources()
{
    for( DWORD i=0; i<g_dwNumDevices; i++ )
    {
        SAFE_DELETE( g_pDeviceList[i].pddsdModes );
    }
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_GetDevices()
// Desc: Returns a ptr to the array of D3DEnum_DeviceInfo structures.
//-----------------------------------------------------------------------------
VOID D3DEnum_GetDevices( D3DEnum_DeviceInfo** ppDevices, DWORD* pdwCount )
{
    if( ppDevices )
        (*ppDevices) = g_pDeviceList;
    if( pdwCount )
        (*pdwCount)  = g_dwNumDevices;
}




//-----------------------------------------------------------------------------
// Name: UpdateDialogControls()
// Desc: Builds the list of devices and modes for the combo boxes in the device
//       select dialog box.
//-----------------------------------------------------------------------------
static VOID UpdateDialogControls( HWND hDlg, D3DEnum_DeviceInfo* pCurrentDevice,
                                  DWORD dwCurrentMode, BOOL bWindowed,
                                  BOOL bStereo )
{
    // Get access to the enumerated device list
    D3DEnum_DeviceInfo* pDeviceList;
    DWORD               dwNumDevices;
    D3DEnum_GetDevices( &pDeviceList, &dwNumDevices );

    // Access to UI controls
    HWND hwndDevice         = GetDlgItem( hDlg, IDC_DEVICE_COMBO );
    HWND hwndMode           = GetDlgItem( hDlg, IDC_MODE_COMBO );
    HWND hwndWindowed       = GetDlgItem( hDlg, IDC_WINDOWED_CHECKBOX );
    HWND hwndStereo         = GetDlgItem( hDlg, IDC_STEREO_CHECKBOX );
    HWND hwndFullscreenText = GetDlgItem( hDlg, IDC_FULLSCREEN_TEXT );

    // Reset the content in each of the combo boxes
    ComboBox_ResetContent( hwndDevice );
    ComboBox_ResetContent( hwndMode );

    // Don't let non-GDI devices be windowed
    if( FALSE == pCurrentDevice->bDesktopCompatible )
        bWindowed = FALSE;

    // Add a list of devices to the device combo box
    for( DWORD device = 0; device < dwNumDevices; device++ )
    {
        D3DEnum_DeviceInfo* pDevice = &pDeviceList[device];

        // Add device name to the combo box
        DWORD dwItem = ComboBox_AddString( hwndDevice, pDevice->strDesc );
        
        // Set the remaining UI states for the current device
        if( pDevice == pCurrentDevice )
        {
            // Set the combobox selection on the current device
            ComboBox_SetCurSel( hwndDevice, dwItem );

            // Enable/set the fullscreen checkbox, as appropriate
            if( hwndWindowed )
            {
                EnableWindow( hwndWindowed, pDevice->bDesktopCompatible );
                Button_SetCheck( hwndWindowed, bWindowed );
            }
            
            // Enable/set the stereo checkbox, as appropriate
            if( hwndStereo )
            {
                EnableWindow( hwndStereo, pDevice->bStereoCompatible && !bWindowed );
                Button_SetCheck( hwndStereo, bStereo );
            }

            // Enable/set the fullscreen modes combo, as appropriate
            EnableWindow( hwndMode, !bWindowed );
            EnableWindow( hwndFullscreenText, !bWindowed );

            // Build the list of fullscreen modes
            for( DWORD mode = 0; mode < pDevice->dwNumModes; mode++ )
            {
                DDSURFACEDESC2* pddsdMode = &pDevice->pddsdModes[mode];

                // Skip non-stereo modes, if the device is in stereo mode
                if( 0 == (pddsdMode->ddsCaps.dwCaps2&DDSCAPS2_STEREOSURFACELEFT) )
                    if( bStereo )
                        continue;

                TCHAR strMode[80];
                wsprintf( strMode, _T("%ld x %ld x %ld"),
                          pddsdMode->dwWidth, pddsdMode->dwHeight,
                          pddsdMode->ddpfPixelFormat.dwRGBBitCount );

                // Add mode desc to the combo box
                DWORD dwItem = ComboBox_AddString( hwndMode, strMode );

                // Set the item data to identify this mode
                ComboBox_SetItemData( hwndMode, dwItem, mode );

                // Set the combobox selection on the current mode
                if( mode == dwCurrentMode )
                    ComboBox_SetCurSel( hwndMode, dwItem );

                // Since not all modes support stereo, select a default mode in
                // case none was chosen yet.
                if( bStereo && ( CB_ERR == ComboBox_GetCurSel( hwndMode ) ) )
                    ComboBox_SetCurSel( hwndMode, dwItem );
            }
        }
    }
}




//-----------------------------------------------------------------------------
// Name: ChangeDeviceProc()
// Desc: Windows message handling function for the device select dialog
//-----------------------------------------------------------------------------
static BOOL CALLBACK ChangeDeviceProc( HWND hDlg, UINT uiMsg, WPARAM wParam, 
                                       LPARAM lParam )
{
    static D3DEnum_DeviceInfo** ppDeviceArg;
    static D3DEnum_DeviceInfo* pCurrentDevice;
    static DWORD dwCurrentMode;
    static BOOL  bCurrentWindowed;
    static BOOL  bCurrentStereo;

    // Get access to the enumerated device list
    D3DEnum_DeviceInfo* pDeviceList;
    DWORD               dwNumDevices;
    D3DEnum_GetDevices( &pDeviceList, &dwNumDevices );

    // Handle the initialization message
    if( WM_INITDIALOG == uiMsg )
    {
        // Get the app's current device, passed in as an lParam argument        
        ppDeviceArg = (D3DEnum_DeviceInfo**)lParam;
        if( NULL == ppDeviceArg )
            return FALSE;

        // Setup temp storage pointers for dialog
        pCurrentDevice = (*ppDeviceArg);
        dwCurrentMode    = pCurrentDevice->dwCurrentMode;
        bCurrentWindowed = pCurrentDevice->bWindowed;
        bCurrentStereo   = pCurrentDevice->bStereo;

        UpdateDialogControls( hDlg, pCurrentDevice, dwCurrentMode,
                              bCurrentWindowed, bCurrentStereo );

        return TRUE;
    }
    else if( WM_COMMAND == uiMsg )
    {
        HWND hwndDevice   = GetDlgItem( hDlg, IDC_DEVICE_COMBO );
        HWND hwndMode     = GetDlgItem( hDlg, IDC_MODE_COMBO );
        HWND hwndWindowed = GetDlgItem( hDlg, IDC_WINDOWED_CHECKBOX );
        HWND hwndStereo   = GetDlgItem( hDlg, IDC_STEREO_CHECKBOX );

        // Get current UI state
        DWORD dwDevice   = ComboBox_GetCurSel( hwndDevice );
        DWORD dwModeItem = ComboBox_GetCurSel( hwndMode );
        DWORD dwMode     = ComboBox_GetItemData( hwndMode, dwModeItem );
        BOOL  bWindowed  = hwndWindowed ? Button_GetCheck( hwndWindowed ) : 0;
        BOOL  bStereo    = hwndStereo   ? Button_GetCheck( hwndStereo )   : 0;

        D3DEnum_DeviceInfo* pDevice = &pDeviceList[dwDevice];
        
        if( IDOK == LOWORD(wParam) )
        {
            // Handle the case when the user hits the OK button. Check if any
            // of the options were changed
            if( pDevice != pCurrentDevice || dwMode != dwCurrentMode ||
                bWindowed != bCurrentWindowed || bStereo != bCurrentStereo )
            {
                // Return the newly selected device and its new properties
                (*ppDeviceArg)              = pDevice;
                pDevice->bWindowed          = bWindowed;
                pDevice->bStereo            = bStereo;
                pDevice->dwCurrentMode      = dwMode;
                pDevice->ddsdFullscreenMode = pDevice->pddsdModes[dwMode];

                EndDialog( hDlg, IDOK );
            }
            else
                EndDialog( hDlg, IDCANCEL );

            return TRUE;
        }
        else if( IDCANCEL == LOWORD(wParam) )
        {
            // Handle the case when the user hits the Cancel button
            EndDialog( hDlg, IDCANCEL );
            return TRUE;
        }
        else if( CBN_SELENDOK == HIWORD(wParam) )
        {
            if( LOWORD(wParam) == IDC_DEVICE_COMBO )
            {
                // Handle the case when the user chooses the device combo
                dwMode    = pDeviceList[dwDevice].dwCurrentMode;
                bWindowed = pDeviceList[dwDevice].bWindowed;
                bStereo   = pDeviceList[dwDevice].bStereo;
            }
        }

        // Keep the UI current
        UpdateDialogControls( hDlg, &pDeviceList[dwDevice], dwMode, bWindowed, bStereo );
        return TRUE;
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_UserChangeDevice()
// Desc: Pops up a dialog which allows the user to select a new device.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_UserChangeDevice( D3DEnum_DeviceInfo** ppDevice )
{
    if( IDOK == DialogBoxParam( (HINSTANCE)GetModuleHandle(NULL),
                                MAKEINTRESOURCE(IDD_CHANGEDEVICE),
                                GetForegroundWindow(),
                                ChangeDeviceProc, (LPARAM)ppDevice ) )
        return S_OK;

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: D3DEnum_SelectDefaultDevice()
// Desc: Pick a default device, preferably hardware and desktop compatible.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_SelectDefaultDevice( D3DEnum_DeviceInfo** ppDevice,
                                     DWORD dwFlags )
{
    // Check arguments
    if( NULL == ppDevice )
        return E_INVALIDARG;

    // Get access to the enumerated device list
    D3DEnum_DeviceInfo* pDeviceList;
    DWORD               dwNumDevices;
    D3DEnum_GetDevices( &pDeviceList, &dwNumDevices );

    // Look for windowable software, hardware, and hardware TnL devices
    D3DEnum_DeviceInfo* pRefRastDevice     = NULL;
    D3DEnum_DeviceInfo* pSoftwareDevice    = NULL;
    D3DEnum_DeviceInfo* pHardwareDevice    = NULL;
    D3DEnum_DeviceInfo* pHardwareTnLDevice = NULL;

    for( DWORD i=0; i<dwNumDevices; i++ )
    {
        if( pDeviceList[i].bDesktopCompatible )
        {
            if( pDeviceList[i].bHardware )
            {
                if( (*pDeviceList[i].pDeviceGUID) == IID_IDirect3DTnLHalDevice )
                    pHardwareTnLDevice = &pDeviceList[i];
                else
                    pHardwareDevice = &pDeviceList[i];
            }
            else
            {
                if( (*pDeviceList[i].pDeviceGUID) == IID_IDirect3DRefDevice )
                    pRefRastDevice = &pDeviceList[i];
                else
                    pSoftwareDevice = &pDeviceList[i];
            }
        }
    }

    // Prefer a hardware TnL device first, then a non-TnL hardware device, and
    // finally, a software device.
    if( 0 == ( dwFlags & D3DENUM_SOFTWAREONLY ) && pHardwareTnLDevice )
        (*ppDevice) = pHardwareTnLDevice;
    else if( 0 == ( dwFlags & D3DENUM_SOFTWAREONLY ) && pHardwareDevice )
        (*ppDevice) = pHardwareDevice;
    else if( pSoftwareDevice )
        (*ppDevice) = pSoftwareDevice;
    else if( pRefRastDevice )
        (*ppDevice) = pRefRastDevice;
    else
        return D3DENUMERR_NOCOMPATIBLEDEVICES;

    // Set the windowed state of the newly selected device  
    (*ppDevice)->bWindowed = TRUE;

    return S_OK;
}





