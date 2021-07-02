//-----------------------------------------------------------------------------
// File: D3DEnum.h
//
// Desc: Functions which enumerate through the DirectDraw drivers, Direct3D
//       devices, and the display modes available to each device.
//
//
// Copyright (C) 1997 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------


#ifndef D3DENUM_H
#define D3DENUM_H

#include <ddraw.h>
#include <d3d.h>




//-----------------------------------------------------------------------------
// Name: D3DEnum_ModeInfo
// Desc: Linked-list structure to hold information about a display mode. This
//       info is stored as a width, height, bpp, and pixelformat within a 
//       DDSURFACEDESC2.
//-----------------------------------------------------------------------------
struct D3DEnum_ModeInfo
{
    DDSURFACEDESC2    ddsd;
	CHAR              strDesc[40];
    D3DEnum_ModeInfo* pNext;
};




//-----------------------------------------------------------------------------
// Name: D3DEnum_DeviceInfo
// Desc: Linked-list structure to hold information about a Direct3D device. The
//       primary information recorded here is the D3DDEVICEDESC and a ptr to a
//       linked-list of valid display modes.
//-----------------------------------------------------------------------------
struct D3DEnum_DeviceInfo
{
    CHAR                strDesc[40];
    GUID                guid;
    GUID*               pGUID;
    D3DDEVICEDESC       ddDesc;
    BOOL                bIsHardware;
	BOOL                bCompatbileWithDesktop;
	BOOL                bWindowed;

    D3DEnum_ModeInfo*   pCurrentMode;
    D3DEnum_ModeInfo*   pFirstMode;
    D3DEnum_DeviceInfo* pNext;
};




//-----------------------------------------------------------------------------
// Name: D3DEnum_DriverInfo
// Desc: Linked-list structure to hold information about a DirectX driver. The
//       info stored is the capability bits for the driver plus a linked-list
//       of valid Direct3D devices for the driver. Note: most systems will only
//       have one driver. The exception are multi-monitor systems, and systems
//       with non-GDI 3D video cards.
//-----------------------------------------------------------------------------
struct D3DEnum_DriverInfo
{
    GUID                 guid;
    GUID*                pGUID;
    CHAR                 strDesc[40];
    CHAR                 strName[40];
    DDCAPS               ddDriverCaps;
    DDCAPS               ddHELCaps;
	HANDLE               hMonitor;

    D3DEnum_DeviceInfo*  pCurrentDevice;
    D3DEnum_DeviceInfo*  pFirstDevice;
    D3DEnum_DriverInfo*  pNext;
};




//-----------------------------------------------------------------------------
// Name: D3DEnum_EnumerateDevices()
// Desc: Enumerates all drivers, devices, and modes. The optional app-supplied
//       callback is called for each enumerated device, to confirm that the
//       device supports the feature set required by the app.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_EnumerateDevices(
						HRESULT (*AppConfirmFn)(DDCAPS*, D3DDEVICEDESC*) );




//-----------------------------------------------------------------------------
// Name: D3DEnum_FreeResources()
// Desc: Frees all resources used for driver enumeration
//-----------------------------------------------------------------------------
VOID D3DEnum_FreeResources();




//-----------------------------------------------------------------------------
// Name: D3DEnum_SelectDefaultDriver()
// Desc: Picks a driver based on a set of passed in criteria.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_SelectDefaultDriver( DWORD dwFlags );

#define D3DENUM_SOFTWAREONLY   0x00000001
#define D3DENUM_FULLSCREENONLY 0x00000002
#define D3DENUM_RGBEMULATION   0x00000004
#define D3DENUM_REFERENCERAST  0x00000008
#define D3DENUM_PRIMARYHAL     0x00000010
#define D3DENUM_SECONDARYHAL   0x00000020




//-----------------------------------------------------------------------------
// Name: D3DEnum_UserDlgSelectDriver()
// Desc: Prompts the user with a dialog box, from which to choose a DD driver,
//       D3D device, and compatible display mode. The function will return 
//       IDOK if a new driver/device/mode was selected, or IDCANCEL if not.
//       Any error will result in a -1 for a return code.
//-----------------------------------------------------------------------------
INT D3DEnum_UserDlgSelectDriver( HWND hwndParent, BOOL bCurrentlyWindowed );




//-----------------------------------------------------------------------------
// Name: D3DEnum_GetSelectedDriver()
// Desc: Returns the currently selected driver, device, and display mode.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_GetSelectedDriver( LPGUID* ppDriverGUID, LPGUID* ppDeviceGuid,
                                   LPDDSURFACEDESC2* pddsdDisplayMode = NULL,
								   BOOL* pbWindowed = NULL,
								   BOOL* pbIsHardware = NULL );




//-----------------------------------------------------------------------------
// Name: D3DEnum_GetSelectedDriver()
// Desc: Returns the currently selected driver, device, and display mode.
//-----------------------------------------------------------------------------
HRESULT D3DEnum_GetSelectedDriver( D3DEnum_DriverInfo** ppDriverInfo,
								   D3DEnum_DeviceInfo** ppDeviceInfo );




//-----------------------------------------------------------------------------
// Name: D3DEnum_GetFirstDriver()
// Desc: Returns a ptr to the first DriverInfo structure in the tree holding
//       the device/driver/mode enumeration information.
//-----------------------------------------------------------------------------
D3DEnum_DriverInfo* D3DEnum_GetFirstDriver();




//-----------------------------------------------------------------------------
// Error codes
//-----------------------------------------------------------------------------
#define D3DENUMERR_ENUMERATIONFAILED   0x81000001 // Enumeration failed
#define D3DENUMERR_SUGGESTREFRAST      0x81000002 // Suggest using the RefRast
#define D3DENUMERR_NOCOMPATIBLEDEVICES 0x81000003 // No devices were found that
                                                  // meet the app's desired
                                                  // capabilities
#define D3DENUMERR_NODIRECTDRAW        0x81000004 // DDraw couldn't initialize
#define D3DENUMERR_NOTFOUND            0x81000005 // Requested device not found

#endif // D3DENUM_H

