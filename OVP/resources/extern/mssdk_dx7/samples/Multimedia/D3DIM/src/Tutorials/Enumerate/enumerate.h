//-----------------------------------------------------------------------------
// File: Enumerate.h
//
// Desc: Functions which enumerate through the DirectDraw drivers, Direct3D
//       devices, and the display modes available to each device.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------
#ifndef ENUMERATE_H
#define ENUMERATE_H


#include <d3d.h>
#include <windows.h>




//-----------------------------------------------------------------------------
// Name: UserDlgSelectDriver()
// Desc: Prompts the user with a dialog box, from which to choose a DD driver,
//       D3D device, and compatible display mode. The function will return 
//       IDOK if a new driver/device/mode was selected. Any error will result
//       in a -1 for a return code.
//-----------------------------------------------------------------------------
INT UserDlgSelectDriver( HWND hwndParent );




//-----------------------------------------------------------------------------
// Name: GetSelectedDriver()
// Desc: Returns information about the currently selected driver, device, and
//       display mode.
//-----------------------------------------------------------------------------
HRESULT GetSelectedDriver( GUID** ppDriverGUID, GUID** ppDeviceGUID,
                           DDSURFACEDESC2** pddsdDisplayMode,
						   BOOL* pbWindowed, BOOL* pbIsHardware );




#endif // ENUMERATE_H
