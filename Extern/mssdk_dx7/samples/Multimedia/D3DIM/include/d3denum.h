//-----------------------------------------------------------------------------
// File: D3DEnum.h
//
// Desc: Functions to enumerate DDraw/D3D drivers, devices, and modes.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------
#ifndef D3DENUM_H
#define D3DENUM_H
#include <d3d.h>


//-----------------------------------------------------------------------------
// Flag and error definitions
//-----------------------------------------------------------------------------
#define D3DENUM_SOFTWAREONLY           0x00000001 // Software-devices only flag

#define D3DENUMERR_NODIRECTDRAW        0x81000001 // Could not create DDraw
#define D3DENUMERR_ENUMERATIONFAILED   0x81000002 // Enumeration failed
#define D3DENUMERR_SUGGESTREFRAST      0x81000003 // Suggest using the RefRast
#define D3DENUMERR_NOCOMPATIBLEDEVICES 0x81000004 // No devices were found that
                                                  // meet the app's desired
                                                  // capabilities


//-----------------------------------------------------------------------------
// Name: struct D3DEnum_DeviceInfo
// Desc: Structure to hold info about the enumerated Direct3D devices.
//-----------------------------------------------------------------------------
struct D3DEnum_DeviceInfo
{
    // D3D Device info
    TCHAR          strDesc[40];
    GUID*          pDeviceGUID;
    D3DDEVICEDESC7 ddDeviceDesc;
    BOOL           bHardware;

    // DDraw Driver info
    GUID*          pDriverGUID;
    DDCAPS         ddDriverCaps;
    DDCAPS         ddHELCaps;

    // DDraw Mode Info
    DDSURFACEDESC2 ddsdFullscreenMode;
    BOOL           bWindowed;
    BOOL           bStereo;

    // For internal use (apps should not need to use these)
    GUID            guidDevice;
    GUID            guidDriver;
    DDSURFACEDESC2* pddsdModes;
    DWORD           dwNumModes;
    DWORD           dwCurrentMode;
    BOOL            bDesktopCompatible;
    BOOL            bStereoCompatible;
};


// For code not yet switched to new struct name
typedef D3DEnum_DeviceInfo D3DDEVICEINFO;




//-----------------------------------------------------------------------------
// Name: D3DEnum_EnumerateDevices()
// Desc: Enumerates all drivers, devices, and modes. The callback function is
//       called each device, to confirm that the device supports the feature
//       set required by the app.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_EnumerateDevices( HRESULT (*fn)(DDCAPS*, D3DDEVICEDESC7*) );




//-----------------------------------------------------------------------------
// Name: D3DEnum_FreeResources()
// Desc: Cleans up any memory allocated during device enumeration
//-----------------------------------------------------------------------------
VOID D3DEnum_FreeResources();




//-----------------------------------------------------------------------------
// Name: D3DEnum_GetDevices()
// Desc: Returns a ptr to the array of enumerated D3DDEVICEINFO structures.
//-----------------------------------------------------------------------------
VOID D3DEnum_GetDevices( D3DEnum_DeviceInfo** ppDevices, DWORD* pdwCount );




//-----------------------------------------------------------------------------
// Name: D3DEnum_SelectDefaultDevice()
// Desc: Picks a driver based on a set of passed in criteria. The
//       D3DENUM_SOFTWAREONLY flag can be used to pick a software device.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_SelectDefaultDevice( D3DEnum_DeviceInfo** pDevice,
                                     DWORD dwFlags = 0L );




//-----------------------------------------------------------------------------
// Name: D3DEnum_UserChangeDevice()
// Desc: Pops up a dialog which allows the user to select a new device.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_UserChangeDevice( D3DEnum_DeviceInfo** ppDevice );




#endif // D3DENUM_H



