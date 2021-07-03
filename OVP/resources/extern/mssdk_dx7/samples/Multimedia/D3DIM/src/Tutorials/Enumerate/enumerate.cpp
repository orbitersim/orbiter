//-----------------------------------------------------------------------------
// File: Enumerate.cpp
//
// Desc: Code to enumerate DirectDraw/Direct3D drivers, devices, and modes.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------

#define STRICT
#include <stdio.h>
#include "enumerate.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Name: struct D3DDEVICEINFO
// Desc: Structure to hold info about the enumerated Direct3D devices.
//-----------------------------------------------------------------------------
struct D3DDEVICEINFO
{
    // D3D Device info
    CHAR           strDesc[40];
    GUID*          pDeviceGUID;
    D3DDEVICEDESC7 ddDeviceDesc;
    BOOL           bHardware;

    // DDraw Driver info
    GUID*          pDriverGUID;
    DDCAPS         ddDriverCaps;
    DDCAPS         ddHELCaps;

    // DDraw Mode Info
    DDSURFACEDESC2 ddsdMode;
    BOOL           bFullscreen;

    // For internal use (apps should not need these)
    GUID           guidDevice;
    GUID           guidDriver;
    DDSURFACEDESC2 ddsdModes[100];
    DWORD          dwNumModes;
    DWORD          dwCurrentMode;
    BOOL           bDesktopCompatible;
};




//-----------------------------------------------------------------------------
// Global data for the enumerator functions
//-----------------------------------------------------------------------------
D3DDEVICEINFO g_d3dDevices[20];
DWORD         g_dwNumDevices    = 0L;
DWORD         g_dwCurrentDevice = 0L;




//-----------------------------------------------------------------------------
// Local callback functions used during enumeration
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// Name: ModeEnumCallback()
// Desc: Callback function for enumerating display modes.
//-----------------------------------------------------------------------------
static HRESULT WINAPI ModeEnumCallback( DDSURFACEDESC2* pddsd,
                                        VOID* pParentInfo )
{
    // Copy the mode into the driver's list of supported modes
    D3DDEVICEINFO* pDriverInfo = (D3DDEVICEINFO*)pParentInfo;
    pDriverInfo->ddsdModes[pDriverInfo->dwNumModes++] = (*pddsd);

    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: DeviceEnumCallback()
// Desc: Callback function for enumerating devices
//-----------------------------------------------------------------------------
static HRESULT WINAPI DeviceEnumCallback(LPSTR strDesc,
                                  LPSTR strName, D3DDEVICEDESC7* pDesc,
                                  VOID* pParentInfo )
{
    // Set up device info for this device
    D3DDEVICEINFO* pDriverInfo = (D3DDEVICEINFO*)pParentInfo;
    D3DDEVICEINFO* pDeviceInfo = &g_d3dDevices[g_dwNumDevices];
    memcpy( &pDeviceInfo->ddDeviceDesc, pDesc, sizeof(D3DDEVICEDESC7) );
    pDeviceInfo->guidDevice   = pDesc->deviceGUID;
    pDeviceInfo->pDeviceGUID  = &pDeviceInfo->guidDevice;
    pDeviceInfo->bHardware    = pDesc->dwDevCaps & D3DDEVCAPS_HWRASTERIZATION;
    pDeviceInfo->dwNumModes   = 0;
    pDeviceInfo->ddDriverCaps = pDriverInfo->ddDriverCaps;
    pDeviceInfo->ddHELCaps    = pDriverInfo->ddHELCaps;

    // Copy the driver GUID and description for the device
    if( pDriverInfo->pDriverGUID )
    {
        pDeviceInfo->guidDriver  = pDriverInfo->guidDriver;
        pDeviceInfo->pDriverGUID = &pDeviceInfo->guidDriver;
        strncpy( pDeviceInfo->strDesc, pDriverInfo->strDesc, 39 );
    }
    else
    {
        pDeviceInfo->pDriverGUID = NULL;
        strncpy( pDeviceInfo->strDesc, strName, 39 );
    }

    // Avoid duplicates: only enum HW devices for secondary DDraw drivers.
    if( NULL != pDeviceInfo->pDriverGUID && FALSE == pDeviceInfo->bHardware )
            return D3DENUMRET_OK;

    // This is a good place to give the app a chance to accept or reject this
    // device via a callback function. This simple tutorial does not do this,
    // though.

    // Build list of supported modes for the device
    for( DWORD i=0; i<pDriverInfo->dwNumModes; i++ )
    {
        DDSURFACEDESC2 ddsdMode = pDriverInfo->ddsdModes[i];
        DWORD dwModeDepth = ddsdMode.ddpfPixelFormat.dwRGBBitCount;
        DWORD dwBitDepth  = pDeviceInfo->ddDeviceDesc.dwDeviceRenderBitDepth;
        BOOL  bCompatible = FALSE;

        // Check mode for compatability with device. Skip 8-bit modes.
        if( (32==dwModeDepth) && (dwBitDepth&DDBD_32) ) bCompatible = TRUE;
        if( (24==dwModeDepth) && (dwBitDepth&DDBD_24) ) bCompatible = TRUE;
        if( (16==dwModeDepth) && (dwBitDepth&DDBD_16) ) bCompatible = TRUE;

        // Copy compatible modes to the list of device-supported modes
        if( bCompatible )
            pDeviceInfo->ddsdModes[pDeviceInfo->dwNumModes++] = ddsdMode;
    }

    // Make sure the device has supported modes
    if( 0 == pDeviceInfo->dwNumModes )
        return D3DENUMRET_OK;

    // Mark whether the device can render into a desktop window
    if( 0 == pDeviceInfo->ddsdModes[0].dwWidth )
    {
        pDeviceInfo->bDesktopCompatible = TRUE;
        pDeviceInfo->ddsdMode = pDeviceInfo->ddsdModes[1];
    }
    else
    {
        pDeviceInfo->bDesktopCompatible = FALSE;
        pDeviceInfo->ddsdMode = pDeviceInfo->ddsdModes[0];
    }

    // Accept the device and return
    g_dwNumDevices++;
    return D3DENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: DriverEnumCallback()
// Desc: Callback function for enumerating drivers.
//-----------------------------------------------------------------------------
static BOOL WINAPI DriverEnumCallback( GUID* pGUID, LPSTR strDesc,
                                       LPSTR strName, VOID*, HMONITOR )
{
    D3DDEVICEINFO d3dDeviceInfo;
    LPDIRECT3D7   pD3D;
    LPDIRECTDRAW7 pDD;
    HRESULT       hr;
    
    // Use the GUID to create the DirectDraw object
    hr = DirectDrawCreateEx( pGUID, (VOID**)&pDD, IID_IDirectDraw7, NULL );
    if( FAILED(hr) )
    {
        return D3DENUMRET_OK;
    }

    // Create a D3D object, to enumerate the d3d devices
    hr = pDD->QueryInterface( IID_IDirect3D7, (VOID**)&pD3D );
    if( FAILED(hr) )
    {
        pDD->Release();
        return D3DENUMRET_OK;
    }

    // Copy data to a device info structure
    ZeroMemory( &d3dDeviceInfo, sizeof(D3DDEVICEINFO) );
    strncpy( d3dDeviceInfo.strDesc, strDesc, 39 );
    d3dDeviceInfo.ddDriverCaps.dwSize = sizeof(DDCAPS);
    d3dDeviceInfo.ddHELCaps.dwSize    = sizeof(DDCAPS);
    pDD->GetCaps( &d3dDeviceInfo.ddDriverCaps, &d3dDeviceInfo.ddHELCaps );
    d3dDeviceInfo.pDriverGUID = pGUID;
    if( pGUID )
        d3dDeviceInfo.guidDriver = (*pGUID);

    // Add a display mode for rendering into a desktop window
    if( ( NULL == d3dDeviceInfo.pDriverGUID ) &&
        ( d3dDeviceInfo.ddDriverCaps.dwCaps2 & DDCAPS2_CANRENDERWINDOWED ) )
    {
        // Get the current display depth
        DEVMODE devmode;
        devmode.dmSize = sizeof(DEVMODE);
        EnumDisplaySettings( NULL, ENUM_CURRENT_SETTINGS, &devmode );

        // Set up the mode info
        DDSURFACEDESC2* pMode = &d3dDeviceInfo.ddsdModes[0];
        pMode->ddpfPixelFormat.dwRGBBitCount = devmode.dmBitsPerPel;
        pMode->dwWidth  = 0;
        pMode->dwHeight = 0;

        d3dDeviceInfo.dwNumModes = 1;
    }
    
    // Enumerate the fullscreen display modes.
    pDD->EnumDisplayModes( 0, NULL, &d3dDeviceInfo, ModeEnumCallback );

    // Now, enumerate all the 3D devices
    pD3D->EnumDevices( DeviceEnumCallback, &d3dDeviceInfo );

    // Clean up and return
    pD3D->Release();
    pDD->Release();

    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: UpdateComboBoxesContent()
// Desc: Builds the list of devices and modes for the combo boxes in the device
//       select dialog box.
//-----------------------------------------------------------------------------
static VOID UpdateComboBoxesContent( HWND hDlg, DWORD dwCurrentDevice,
                                     DWORD dwCurrentMode )
{
    // Reset the content in each of the combo boxes
    SendDlgItemMessage( hDlg, IDC_DEVICE_COMBO, CB_RESETCONTENT, 0, 0 );
    SendDlgItemMessage( hDlg, IDC_MODE_COMBO,   CB_RESETCONTENT, 0, 0 );

    for( DWORD device = 0; device < g_dwNumDevices; device++ )
    {
        D3DDEVICEINFO* pDevice = &g_d3dDevices[device];

        // Add device name to the combo box
        DWORD dwItem = SendDlgItemMessage( hDlg, IDC_DEVICE_COMBO, 
                                           CB_ADDSTRING, 0,
                                           (LPARAM)pDevice->strDesc );

        // Associate DeviceInfo ptr with the item in the combo box
        SendDlgItemMessage( hDlg, IDC_DEVICE_COMBO, CB_SETITEMDATA,
                            (WPARAM)dwItem, (LPARAM)device );

        // Set the combobox selection on the current device
        if( device == dwCurrentDevice )
            SendDlgItemMessage( hDlg, IDC_DEVICE_COMBO, CB_SETCURSEL, 
                                (WPARAM)dwItem, 0L );

        // Build the list of modes for the current device
        if( device == dwCurrentDevice )
        {
            D3DDEVICEINFO* pDevice = &g_d3dDevices[dwCurrentDevice];

            for( DWORD mode = 0; mode < pDevice->dwNumModes; mode++ )
            {
                CHAR            strMode[80];
                DDSURFACEDESC2* pMode = &pDevice->ddsdModes[mode];
                
                if( 0 == pMode->dwWidth && 0 == pMode->dwHeight )
                    sprintf( strMode, "%ld-bit Desktop Window",
                             pMode->ddpfPixelFormat.dwRGBBitCount );
                else
                    sprintf( strMode, "%ld x %ld x %ld",
                             pMode->dwWidth, pMode->dwHeight,
                             pMode->ddpfPixelFormat.dwRGBBitCount );
                
                // Add mode desc to the combo box
                DWORD dwItem = SendDlgItemMessage( hDlg, IDC_MODE_COMBO, 
                                                   CB_ADDSTRING, 0,
                                                   (LPARAM)strMode );

                // Associate ModeInfo ptr with the item in the combo box
                SendDlgItemMessage( hDlg, IDC_MODE_COMBO, CB_SETITEMDATA, 
                                    (WPARAM)dwItem, (LPARAM)mode );

                // If this is the current mode, set is as the current selection
                if( mode == dwCurrentMode )
                    SendDlgItemMessage( hDlg, IDC_MODE_COMBO, CB_SETCURSEL, 
                                        (WPARAM)dwItem, 0L );
            }   
        }
    }
}




//-----------------------------------------------------------------------------
// Name: DeviceSelectProc()
// Desc: Windows message handling function for the device select dialog
//-----------------------------------------------------------------------------
BOOL CALLBACK DeviceSelectProc( HWND hDlg, UINT uiMsg, WPARAM wParam, 
                                LPARAM lParam )
{
    static DWORD dwOldDevice,    dwNewDevice;
    static DWORD dwOldMode,      dwNewMode;

    // Handle the initialization message
    if( WM_INITDIALOG == uiMsg )
    {
        // Make sure the required controls exist
        if( NULL == GetDlgItem( hDlg, IDC_DEVICE_COMBO ) )
            return FALSE;
        if( NULL == GetDlgItem( hDlg, IDC_MODE_COMBO ) )
            return FALSE;

        // Setup temp storage pointers for dialog
        dwNewDevice = dwOldDevice = g_dwCurrentDevice;
        dwNewMode   = dwOldMode   = g_d3dDevices[dwNewDevice].dwCurrentMode;

        UpdateComboBoxesContent( hDlg, dwNewDevice, dwNewMode );

        return TRUE;
    }
    
    if( WM_COMMAND == uiMsg )
    {
        // Handle the case when the user hits the OK button
        if( IDOK == LOWORD(wParam) )
        {
            // Check if any of the options were changed
            if( dwNewDevice != dwOldDevice || dwNewMode != dwOldMode )
            {
                D3DDEVICEINFO* pDeviceInfo = &g_d3dDevices[dwNewDevice];
                pDeviceInfo->dwCurrentMode = dwNewMode;

                if( 0 == dwNewMode && pDeviceInfo->bDesktopCompatible )
                    pDeviceInfo->bFullscreen = FALSE;
                else
                {
                    pDeviceInfo->bFullscreen = TRUE;
                    pDeviceInfo->ddsdMode = pDeviceInfo->ddsdModes[dwNewMode];
                }

                g_dwCurrentDevice = dwNewDevice;

                EndDialog( hDlg, IDOK );
            }
            else
                EndDialog( hDlg, IDCANCEL );

            return TRUE;
        }

        // Handle the case when the user hits the Cancel button
        else if( IDCANCEL == LOWORD(wParam) )
        {
            EndDialog( hDlg, IDCANCEL );
            return TRUE;
        }

        // Handle the case when the user chooses an item in the combo boxes.
        else if( CBN_SELENDOK == HIWORD(wParam) )
        {
            DWORD dwIndex = SendMessage( (HWND)lParam, CB_GETCURSEL, 0, 0 );
            DWORD dwData  = SendMessage( (HWND)lParam, CB_GETITEMDATA,
                                         dwIndex, 0 );
            if( CB_ERR == dwIndex )
                return TRUE;

            // Handle the case where one of these may have changed. The
            // combo boxes will need to be updated to reflect the changes.
            switch( LOWORD( wParam ) )
            {
                case IDC_DEVICE_COMBO:
                    if( dwData != dwNewDevice )
                    {
                        dwNewDevice = dwData;
                        dwNewMode   = g_d3dDevices[dwNewDevice].dwCurrentMode;
                    }
                    break;
                case IDC_MODE_COMBO:
                    dwNewMode = dwData;
                    break;
            }

            UpdateComboBoxesContent( hDlg, dwNewDevice, dwNewMode  );

            return TRUE;
        }
    }
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: UserDlgSelectDriver()
// Desc: Prompts the user with a dialog box, from which to choose a DD driver,
//       D3D device, and compatible display mode. The function will return 
//       IDOK if a new driver/device/mode was selected. Any error will result
//       in a -1 for a return code.
//-----------------------------------------------------------------------------
INT UserDlgSelectDriver( HWND hwndParent )
{
    // Enumerate drivers, devices, and modes
    DirectDrawEnumerateEx( DriverEnumCallback, NULL, 
                           DDENUM_ATTACHEDSECONDARYDEVICES |
                           DDENUM_DETACHEDSECONDARYDEVICES |
                           DDENUM_NONDISPLAYDEVICES );

    // Make sure devices were actually enumerated
    if( 0 == g_dwNumDevices )
        return -1;

    // Display a dialog box, asking user to pick a device
    return DialogBox( (HINSTANCE)GetWindowLong( hwndParent, GWL_HINSTANCE ),
                      MAKEINTRESOURCE(IDD_CHANGEDEVICE), hwndParent,
                      DeviceSelectProc );
}




//-----------------------------------------------------------------------------
// Name: GetSelectedDriver()
// Desc: Returns information about the currently selected driver, device, and
//       display mode.
//-----------------------------------------------------------------------------
HRESULT GetSelectedDriver( GUID** ppDriverGUID, GUID** ppDeviceGUID,
                           DDSURFACEDESC2** pddsdDisplayMode,
                           BOOL* pbWindowed, BOOL* pbIsHardware )
{
    (*ppDriverGUID)     = g_d3dDevices[g_dwCurrentDevice].pDriverGUID;
    (*ppDeviceGUID)     = g_d3dDevices[g_dwCurrentDevice].pDeviceGUID;
    (*pddsdDisplayMode) = &g_d3dDevices[g_dwCurrentDevice].ddsdMode;
    (*pbWindowed)       = !g_d3dDevices[g_dwCurrentDevice].bFullscreen;
    (*pbIsHardware)     = g_d3dDevices[g_dwCurrentDevice].bHardware;

    return S_OK;
};




