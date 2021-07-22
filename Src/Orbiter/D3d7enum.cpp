// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ====================================================================================
// DD and D3D device enumeration (DX7)
// ====================================================================================

#define STRICT

#include "D3d7enum.h"
#include "D3d7util.h"
#include "Log.h"

char D3Ddevicename[256];

// ------------------------------------------------------------------------------------
// Global data for the enumerator functions
// ------------------------------------------------------------------------------------
static HRESULT (*g_fnAppConfirmFn)(DDCAPS*, D3DDEVICEDESC7*) = NULL;
static D3D7Enum_DeviceInfo g_pDeviceList[20];
static DWORD g_dwNumDevicesEnumerated = 0L;
static DWORD g_dwNumDevices           = 0L;

//-------------------------------------------------------------------------------------
// Name: SortModesCallback()
// Desc: Callback function for sorting display modes.
//-------------------------------------------------------------------------------------
int SortModesCallback (const VOID* arg1, const VOID* arg2)
{
    DDSURFACEDESC2* p1 = (DDSURFACEDESC2*)arg1;
    DDSURFACEDESC2* p2 = (DDSURFACEDESC2*)arg2;

    if (p1->dwWidth < p2->dwWidth)
        return -1;
    if (p1->dwWidth > p2->dwWidth)
        return +1;

    if (p1->dwHeight < p2->dwHeight)
        return -1;
    if (p1->dwHeight > p2->dwHeight)
        return +1;

    if (p1->ddpfPixelFormat.dwRGBBitCount < p2->ddpfPixelFormat.dwRGBBitCount)
        return -1;
    if (p1->ddpfPixelFormat.dwRGBBitCount > p2->ddpfPixelFormat.dwRGBBitCount)
        return +1;

    return 0;
}

//-------------------------------------------------------------------------------------
// Name: ModeEnumCallback()
// Desc: Callback function for enumerating display modes.
//-------------------------------------------------------------------------------------
static HRESULT WINAPI ModeEnumCallback (DDSURFACEDESC2* pddsd, VOID* pParentInfo)
{
    D3D7Enum_DeviceInfo* pDevice = static_cast<D3D7Enum_DeviceInfo*>(pParentInfo);

	// Check valid mode
	if (pddsd->dwWidth < 640) return DDENUMRET_OK;
	if (pddsd->ddpfPixelFormat.dwRGBBitCount < 16) return DDENUMRET_OK;

    // Reallocate storage for the modes
    DDSURFACEDESC2* pddsdNewModes = new DDSURFACEDESC2[pDevice->dwNumModes+1]; TRACENEW
    memcpy (pddsdNewModes, pDevice->pddsdModes, pDevice->dwNumModes * sizeof(DDSURFACEDESC2));
    delete pDevice->pddsdModes;
    pDevice->pddsdModes = pddsdNewModes;

    // Add the new mode
    pDevice->pddsdModes[pDevice->dwNumModes++] = (*pddsd);

    return DDENUMRET_OK;
}

//-------------------------------------------------------------------------------------
// Name: DeviceEnumCallback()
// Desc: Callback function for enumerating devices
//-------------------------------------------------------------------------------------
static HRESULT WINAPI DeviceEnumCallback (TCHAR* strDesc, TCHAR* strName,
                                          D3DDEVICEDESC7* pDesc, VOID* pParentInfo)
{
	HRESULT hr;
	DWORD i;

    // Keep track of # of devices that were enumerated
    g_dwNumDevicesEnumerated++;

    D3D7Enum_DeviceInfo* pDriverInfo = (D3D7Enum_DeviceInfo*)pParentInfo;
    D3D7Enum_DeviceInfo* pDeviceInfo = &g_pDeviceList[g_dwNumDevices];
    ZeroMemory (pDeviceInfo, sizeof(D3D7Enum_DeviceInfo));

    // Select either the HAL or the HEL device desc:
    pDeviceInfo->bHardware = pDesc->dwDevCaps & D3DDEVCAPS_HWRASTERIZATION;
    memcpy (&pDeviceInfo->ddDeviceDesc, pDesc, sizeof(D3DDEVICEDESC7));

    // Set up device info for this device
    pDeviceInfo->bDesktopCompatible = pDriverInfo->bDesktopCompatible;
    pDeviceInfo->ddDriverCaps       = pDriverInfo->ddDriverCaps;
    pDeviceInfo->ddHELCaps          = pDriverInfo->ddHELCaps;
    pDeviceInfo->guidDevice         = pDesc->deviceGUID;
    pDeviceInfo->pDeviceGUID        = &pDeviceInfo->guidDevice;
    pDeviceInfo->pddsdModes         = new DDSURFACEDESC2[pDriverInfo->dwNumModes]; TRACENEW

    // Copy the driver GUID and description for the device
    if (pDriverInfo->pDriverGUID) {
        pDeviceInfo->guidDriver  = pDriverInfo->guidDriver;
        pDeviceInfo->pDriverGUID = &pDeviceInfo->guidDriver;
		char cbuf[256];
		sprintf (cbuf, "%s (%s)", strName, pDriverInfo->strDesc);

        // For clarity, handle multiple monitors here by preventing duplicate entries in the device description list.
        char tempBuf[256];
        int dupeIDCount = 1;
        for (i=0; i < 20; i++) {  // 20-loop-limit is for a sanity check in case of a bug
            strcpy(tempBuf, cbuf);  // working copy
            if (dupeIDCount > 1) {
                char idStr[4];
                // this is the newest device name
                strcat(cbuf, " #");
                strcat(cbuf, _itoa(dupeIDCount, idStr, 10));
            }
            // check whether we have a duplicate of this name
            UINT j;  // we need this outside the for loop
            for (j=0; j < g_dwNumDevices; j++) {
                const D3D7Enum_DeviceInfo* pExistingDeviceInfo = g_pDeviceList + j;
                if (strcmp(cbuf, pExistingDeviceInfo->strDesc) == 0) {
                    // found a device with the exact same name and description
                    dupeIDCount++;  
                    break;  // return to outer loop and try again
                }
            }
            if (j >= g_dwNumDevices)
                break;  // name is unique
        }

        // now save the unique name
        lstrcpyn (pDeviceInfo->strDesc, cbuf, 79);
    } else {
        pDeviceInfo->pDriverGUID = NULL;
        lstrcpyn (pDeviceInfo->strDesc, strName, 79);
    }

    // Avoid duplicates: only enum HW devices for secondary DDraw drivers.
    if (NULL != pDeviceInfo->pDriverGUID && FALSE == pDeviceInfo->bHardware)
		return D3DENUMRET_OK;

    // Give the app a chance to accept or reject this device.
    if (g_fnAppConfirmFn) {
		hr = g_fnAppConfirmFn (&pDeviceInfo->ddDriverCaps, &pDeviceInfo->ddDeviceDesc);
		if (hr < 0) { // macro FAILED doesn't work here for some strange reason!
			return D3DENUMRET_OK;
		}
	}

    // Build list of supported modes for the device
    for (i = 0; i < pDriverInfo->dwNumModes; i++) {
        DDSURFACEDESC2 ddsdMode = pDriverInfo->pddsdModes[i];
        DWORD dwRenderDepths    = pDeviceInfo->ddDeviceDesc.dwDeviceRenderBitDepth;
        DWORD dwDepth           = ddsdMode.ddpfPixelFormat.dwRGBBitCount;

        // Accept modes that are compatable with the device
        if (((dwDepth == 32) && (dwRenderDepths & DDBD_32)) ||
            ((dwDepth == 24) && (dwRenderDepths & DDBD_24)) ||
            ((dwDepth == 16) && (dwRenderDepths & DDBD_16))) {
            // Copy compatible modes to the list of device-supported modes
            pDeviceInfo->pddsdModes[pDeviceInfo->dwNumModes++] = ddsdMode;

            // Record whether the device has any stereo modes
            if (ddsdMode.ddsCaps.dwCaps2 & DDSCAPS2_STEREOSURFACELEFT)
                pDeviceInfo->bStereoCompatible = TRUE;
        }
    }

    // Bail if the device has no supported modes
    if (0 == pDeviceInfo->dwNumModes)
        return D3DENUMRET_OK;

	// Try to find a 1024x768 default mode. If not available, fall back to
	// 800x600 or even 640x480.
    for (i = 0; i < pDeviceInfo->dwNumModes; i++) {
		DDSURFACEDESC2* mode = pDeviceInfo->pddsdModes+i;
		if (mode->ddpfPixelFormat.dwRGBBitCount < 16) continue;
		if (mode->dwWidth == 1024 && mode->dwHeight == 768) {
            pDeviceInfo->ddsdFullscreenMode = *mode;
            pDeviceInfo->dwCurrentMode      = i;
			break;
		}
		if (mode->dwWidth == 800 && mode->dwHeight == 600) {
            pDeviceInfo->ddsdFullscreenMode = *mode;
            pDeviceInfo->dwCurrentMode      = i;
		}
		if (mode->dwWidth == 640 && mode->dwHeight == 480 && pDeviceInfo->ddsdFullscreenMode.dwWidth < 800) {
            pDeviceInfo->ddsdFullscreenMode = *mode;
            pDeviceInfo->dwCurrentMode      = i;
		}
	}

    // Select windowed mode by default
    pDeviceInfo->bWindowed = TRUE;

    // Accept the device and return
    g_dwNumDevices++;
    return D3DENUMRET_OK;
}

// ------------------------------------------------------------------------------------
// Name: DriverEnumCallback
// Desc: Callback function for enumerating drivers
// ------------------------------------------------------------------------------------
static BOOL WINAPI DriverEnumCallback (GUID* pGUID, TCHAR* strDesc, TCHAR* strName,
									   VOID*, HMONITOR)
{
	D3D7Enum_DeviceInfo d3dDeviceInfo;
	LPDIRECTDRAW7       pDD;
	LPDIRECT3D7         pD3D;
	HRESULT             hr;

	// STEP 1
	// Use the GUID to create the DirectDraw object
	hr = DirectDrawCreateEx (pGUID, (VOID**)&pDD, IID_IDirectDraw7, NULL);
	if (FAILED(hr)) {
		LOGOUT("Can't create DDraw during enumeration!");
		return D3DENUMRET_OK;
	}

	// STEP 2
	// Create a Direct3D object to enumerate the d3d devices
	hr = pDD->QueryInterface (IID_IDirect3D7, (VOID**)&pD3D);
	if (FAILED(hr)) {
		pDD->Release();
		LOGOUT("Can't query IDirect3D7 during enumeration!");
		return D3DENUMRET_OK;
	}

	// STEP 3
	// Copy data to a device information structure
	ZeroMemory (&d3dDeviceInfo, sizeof (d3dDeviceInfo));
	lstrcpyn (d3dDeviceInfo.strDesc, strDesc, 79);
	d3dDeviceInfo.ddDriverCaps.dwSize = sizeof(DDCAPS);
	d3dDeviceInfo.ddHELCaps.dwSize    = sizeof(DDCAPS);
	pDD->GetCaps (&d3dDeviceInfo.ddDriverCaps, &d3dDeviceInfo.ddHELCaps);
	if (pGUID) {
		d3dDeviceInfo.guidDriver = (*pGUID);
		d3dDeviceInfo.pDriverGUID = &d3dDeviceInfo.guidDriver;
	}
	strcpy (D3Ddevicename, d3dDeviceInfo.strDesc);

	// Record whether the device can render into a desktop window
	if (d3dDeviceInfo.ddDriverCaps.dwCaps2 & DDCAPS2_CANRENDERWINDOWED)
		//if (NULL == d3dDeviceInfo.pDriverGUID)  // check removed 040116
			d3dDeviceInfo.bDesktopCompatible = TRUE;

	// STEP 4
	// Enumerate the fullscreen display modes
	pDD->EnumDisplayModes (0, NULL, &d3dDeviceInfo, ModeEnumCallback);

	// Sort the list of display modes
	qsort (d3dDeviceInfo.pddsdModes, d3dDeviceInfo.dwNumModes,
		sizeof (DDSURFACEDESC2), SortModesCallback);

	// Now enumerate all the 3D devices
	pD3D->EnumDevices (DeviceEnumCallback, &d3dDeviceInfo);

	// Clean up and return
	SAFE_DELETE (d3dDeviceInfo.pddsdModes);
	pD3D->Release();
	pDD->Release();

	return DDENUMRET_OK;
}

// ------------------------------------------------------------------------------------
// Name: D3D7Enum_ReadDeviceList()
// Read device information from file
// ------------------------------------------------------------------------------------
int D3D7Enum_ReadDeviceList (CHAR *fname)
{
	int i;
	FILE *ifs = fopen (fname, "rb");
	if (!ifs) return 0;
	for (i = 0; i < 20; i++) {
		D3D7Enum_DeviceInfo *pInfo = g_pDeviceList+i;
		if (!fread (pInfo, sizeof(D3D7Enum_DeviceInfo), 1, ifs)) break;
		pInfo->pDeviceGUID = &pInfo->guidDevice;
		pInfo->pDriverGUID = &pInfo->guidDriver;
		pInfo->pddsdModes = new DDSURFACEDESC2[pInfo->dwNumModes]; TRACENEW
		if (fread (pInfo->pddsdModes, sizeof(DDSURFACEDESC2), pInfo->dwNumModes, ifs)
			< pInfo->dwNumModes) break;
	}
	fclose (ifs);
	g_dwNumDevices = i;
	return i;
}

// ------------------------------------------------------------------------------------
// Name: D3D7Enum_WriteDeviceList()
// Write the device information stored in the device list to a file (binary format)
// ------------------------------------------------------------------------------------
VOID D3D7Enum_WriteDeviceList (CHAR *fname)
{
	FILE *ofs = fopen (fname, "wb");
	DWORD i, n = g_dwNumDevices;
	for (i = 0; i < n; i++) {
		D3D7Enum_DeviceInfo *pInfo = g_pDeviceList+i;
		fwrite (pInfo, sizeof(D3D7Enum_DeviceInfo), 1, ofs);
		fwrite (pInfo->pddsdModes, sizeof(DDSURFACEDESC2), pInfo->dwNumModes, ofs);
	}
	fclose (ofs);
}

// ------------------------------------------------------------------------------------
// Name: D3D7Enum_EnumerateDevices()
// Desc: Enumerates all drivers, devices, and modes. The callback function is called
//       for each device to confirm that the device supports the feature set the
//       application requires.
// ------------------------------------------------------------------------------------
HRESULT D3D7Enum_EnumerateDevices (HRESULT (*AppConfirmFn)(DDCAPS*, D3DDEVICEDESC7*))
{
	// Store the device enumeration callback function
	g_fnAppConfirmFn = AppConfirmFn;

	// Enumerate drivers, devices, and modes
	DirectDrawEnumerateEx (DriverEnumCallback, NULL,
		DDENUM_ATTACHEDSECONDARYDEVICES |
		DDENUM_DETACHEDSECONDARYDEVICES |
		DDENUM_NONDISPLAYDEVICES);

	// Make sure that devices were enumerated
	if (0 == g_dwNumDevicesEnumerated) {
		LOGOUT("No devices and/or modes were enumerated!");
		return D3DENUMERR_ENUMERATIONFAILED;
	}
	if (0 == g_dwNumDevices) {
		LOGOUT("No enumerated devices were accepted!");
		LOGOUT("Try enabling the D3D Reference Rasterizer.");
		return D3DENUMERR_SUGGESTREFRAST;
	}
	LOGOUT("Devices enumerated: %d", g_dwNumDevicesEnumerated);
	LOGOUT("Devices accepted: %d", g_dwNumDevices);

	return S_OK;
}

//-----------------------------------------------------------------------------
// Name: D3D7Enum_FreeResources ()
// Desc: Cleans up any memory allocated during device enumeration
//-----------------------------------------------------------------------------
VOID D3D7Enum_FreeResources ()
{
    for (DWORD i = 0; i < g_dwNumDevices; i++) {
        SAFE_DELETE (g_pDeviceList[i].pddsdModes);
    }
	g_dwNumDevicesEnumerated = 0L;
	g_dwNumDevices           = 0L;
}

// ------------------------------------------------------------------------------------
// Name: D3D7Enum_SelectDefaultDevice()
// Desc: Pick a device from the enumeration list based on required caps
// ------------------------------------------------------------------------------------
HRESULT D3D7Enum_SelectDefaultDevice (D3D7Enum_DeviceInfo **ppDevice, DWORD dwFlags)
{
	// Check arguments
	if (NULL == ppDevice) return E_INVALIDARG;

	// Get access to the enumerated device list
	D3D7Enum_DeviceInfo *pDeviceList;
	DWORD                dwNumDevices;
	D3D7Enum_GetDevices (&pDeviceList, &dwNumDevices);

	// Pick TnL, hardware, software and reference device
	// If given a choice, use a windowable device
    D3D7Enum_DeviceInfo* pRefRastDevice     = NULL;
    D3D7Enum_DeviceInfo* pSoftwareDevice    = NULL;
    D3D7Enum_DeviceInfo* pHardwareDevice    = NULL;
    D3D7Enum_DeviceInfo* pHardwareTnLDevice = NULL;

	for (DWORD i = 0; i < dwNumDevices; i++) {
		if (pDeviceList[i].bHardware) {
			if ((*pDeviceList[i].pDeviceGUID) == IID_IDirect3DTnLHalDevice) {
		        if (!pHardwareTnLDevice || pDeviceList[i].bDesktopCompatible)
                    pHardwareTnLDevice = &pDeviceList[i];
			} else {
				if (!pHardwareDevice || pDeviceList[i].bDesktopCompatible)
                    pHardwareDevice = &pDeviceList[i];
			}
		} else {
			if ((*pDeviceList[i].pDeviceGUID) == IID_IDirect3DRefDevice) {
				if (!pRefRastDevice || pDeviceList[i].bDesktopCompatible)
                    pRefRastDevice = &pDeviceList[i];
			} else {
				if (!pSoftwareDevice || pDeviceList[i].bDesktopCompatible)
                    pSoftwareDevice = &pDeviceList[i];
			}
		}
	}

	// Pick a device in this order: TnL, hardware, software, reference
    if (0 == (dwFlags & D3DENUM_SOFTWAREONLY) && pHardwareTnLDevice)
        (*ppDevice) = pHardwareTnLDevice;
    else if (0 == (dwFlags & D3DENUM_SOFTWAREONLY) && pHardwareDevice)
        (*ppDevice) = pHardwareDevice;
    else if (pSoftwareDevice)
        (*ppDevice) = pSoftwareDevice;
    else if (pRefRastDevice)
        (*ppDevice) = pRefRastDevice;
    else
        return D3DENUMERR_NOCOMPATIBLEDEVICES;

    // Set fullscreen mode by default
#ifdef INLINEGRAPHICS
    (*ppDevice)->bWindowed = TRUE;
#else
    (*ppDevice)->bWindowed = TRUE;
#endif // INLINEGRAPHICS

	return S_OK;
}

// ------------------------------------------------------------------------------------
// Name: D3D7Enum_GetDevices()
// Desc: Returns a ptr to the array of D3D7Enum_DeviceInfo structures.
// ------------------------------------------------------------------------------------
VOID D3D7Enum_GetDevices (D3D7Enum_DeviceInfo **ppDevices, DWORD *pdwCount)
{
	if (ppDevices) (*ppDevices) = g_pDeviceList;
	if (pdwCount)  (*pdwCount)  = g_dwNumDevices;
}

// ------------------------------------------------------------------------------------
// Name: D3D7Enum_GetDevice()
// Desc: Returns a ptr to device idx or NULL if i is out of range
// ------------------------------------------------------------------------------------
D3D7Enum_DeviceInfo *D3D7Enum_GetDevice (DWORD idx)
{
	return (idx < g_dwNumDevices ? g_pDeviceList+idx : NULL);
}

