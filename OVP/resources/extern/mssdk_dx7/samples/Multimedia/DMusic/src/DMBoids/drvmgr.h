#ifndef DRVMGR_H
#define DRVMGR_H
/*
**-----------------------------------------------------------------------------
**  Name:       DrvMgr.h
**  Purpose:    Creates and manages DD/D3D drivers static info
**
**  Copyright (c) 1995-1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Includes
**-----------------------------------------------------------------------------
*/

#include "Common.h"



/*
**-----------------------------------------------------------------------------
**  Defines
**-----------------------------------------------------------------------------
*/

// DD Driver Flags
#define DDDRV_INIT				0x00000001L
	
#define DDDRV_VALID				0x00000001L
#define DDDRV_PRIMARY			0x00000002L
#define DDDRV_D3D				0x00000004L

#define DDDRV_MODES_LOADED		0x00000008L
#define DDDRV_DEVICES_LOADED	0x00000010L


// D3D Device Flags
#define D3DDEV_VALID			0x00000001L
#define D3DDEV_FORMATS_LOADED	0x00000002L



/*
**-----------------------------------------------------------------------------
**  Typedefs
**-----------------------------------------------------------------------------
*/

// Forward declarations
class DDModeInfo;
typedef DDModeInfo * LPDDModeInfo;

class D3DDevInfo;
typedef D3DDevInfo * LPD3DDevInfo;

class DDDrvInfo;
typedef DDDrvInfo * LPDDDrvInfo;

class DDDrvMgr;
typedef DDDrvMgr * LPDDDrvMgr;


// Enumeration Callback Info
#define ENUM_STOP  0x80000000L

typedef enum {
    ENUM_ERROR    = 0,
    ENUM_FAILURE  = 1,
    ENUM_SUCCESS  = 2
} ENUMERRORS;


// Callback function prototypes
typedef DWORD (*ENUMDDDRVCALLBACK)(LPVOID lpDriver, LPVOID lpData, DWORD dwExtra);
typedef DWORD (*ENUMD3DDEVCALLBACK)(LPVOID lpDevice, LPVOID lpData, DWORD dwExtra);

// Enum Driver Callback Info
typedef struct tagDDDRV_ENUMINFO
{
    void *              lpStart;        // Starting Node
    ENUMDDDRVCALLBACK   fpcbEnum;       // Enumeration Callback function
    DWORD               dwExtra;        // User defined extra data for callback
} DDDRV_ENUMINFO;
typedef DDDRV_ENUMINFO * LPDDDRV_ENUMINFO;

// Enum D3D Device Callback Info
typedef struct tagD3DDEV_ENUMINFO
{
    void *              lpStart;        // Starting Node
    ENUMD3DDEVCALLBACK  fpcbEnum;       // Enumeration Callback function
    DWORD               dwExtra;        // User defined extra data for callback
} D3DDEV_ENUMINFO;
typedef D3DDEV_ENUMINFO * LPD3DDEV_ENUMINFO;



/*
**-----------------------------------------------------------------------------
**  Functions
**-----------------------------------------------------------------------------
*/

LPDDDrvInfo ValidateDriver (LPGUID lpDDGuid);
LPDDModeInfo ValidateMode (LPDDDrvInfo lpDriver, DWORD w, DWORD h,
						   DWORD bpp, DWORD refresh, LPD3DDevInfo lpFilter);
LPD3DDevInfo ValidateDevice (LPDDDrvInfo lpDriver, LPGUID lpD3DGuid,
						     LPDDModeInfo lpFilter);

BOOL GetDesktopMode (LPDDDrvInfo lpDriver, LPGUID lpD3DGuid,
					 LPDDModeInfo * lpMode,	LPD3DDevInfo * lpDev);

BOOL GetFullscreenMode (LPDDDrvInfo lpDriver,  LPGUID lpD3DGuid,
						DWORD w, DWORD h, DWORD bpp, DWORD refresh,
					    LPDDModeInfo * lpMode,	LPD3DDevInfo * lpDev);

HRESULT ChooseDriverDefaults (LPGUID lpGuidDD, DWORD w, DWORD h,
							  DWORD bpp, DWORD refresh, LPGUID lpGuidD3D,
							  BOOL fFullScreen, LPDDDrvInfo * lpDriver,
							  LPDDModeInfo * lpMode, LPD3DDevInfo * lpDevice);

DWORD FlagsToBitDepth (DWORD dwFlags);
DWORD BitDepthToFlags (DWORD dwBPP);
BOOL  isPalettized (LPDDPIXELFORMAT lpddpf);



/*
**-----------------------------------------------------------------------------
**  Classes
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Name:       DDModeInfo
**  Purpose:    DirectDraw Mode info 
**-----------------------------------------------------------------------------
*/

class DDModeInfo
{
private:
protected:
public:
    // Data
    DDSURFACEDESC   ddSurfDesc;     // Complete Surface Description
	LPDDModeInfo	lpPrev;			// Prev Node in List
	LPDDModeInfo    lpNext;			// Next Node In List

    // Constructors
    DDModeInfo (void)
        { 
            ZeroMemory (&ddSurfDesc, sizeof (ddSurfDesc)); 
            ddSurfDesc.dwSize = sizeof (DDSURFACEDESC);

			lpPrev = NULL;
			lpNext = NULL;
        }

    DDModeInfo (const DDSURFACEDESC & ddDesc)
        { 
            CopyMemory (&ddSurfDesc, (const void *)&ddDesc, sizeof (ddSurfDesc)); 
            ddSurfDesc.dwSize = sizeof (DDSURFACEDESC);

			lpPrev = NULL;
			lpNext = NULL;
        }
    ~DDModeInfo (void) 
		{
			lpPrev = NULL;
			lpNext = NULL;
		}

    // Methods
	DWORD GetWidth (void);
	DWORD GetHeight (void);
	DWORD GetBPP (void);
	HRESULT GetMode (DWORD & dwWidth, DWORD & dwHeight, 
					 DWORD & dwBPP, DWORD & dwRefresh);

	BOOL ModeSupported (LPD3DDevInfo lpDevice);

    BOOL Match (DWORD dwW, DWORD dwH, DWORD dwBPP);
    BOOL Match (const DDSURFACEDESC & ddsd);

	BOOL Match (DWORD dwBPP);
	BOOL Match (const DDPIXELFORMAT & ddpf);
}; // DDModeInfo



/*
**-----------------------------------------------------------------------------
**  Name:       D3DDevInfo
**  Purpose:    D3D Device/driver info for D3D Driver Manager
**	Notes:		
**
**	- The Texture Format is a Mode (both are DDSURFACEDESC wrappers)
**
**-----------------------------------------------------------------------------
*/

class D3DDevInfo
{
private:
protected:
public:
    // D3D Device Driver info
    DWORD           fFlags;                 // Flags

    // D3D Info
    GUID            guid;                   // GUID
    LPTSTR          szName;					// Driver Name
    LPTSTR          szDesc;					// Driver Description
    D3DDEVICEDESC   d3dHalDesc;             // HAL info
    D3DDEVICEDESC   d3dHelDesc;             // HEL info

    // Texture Formats
	DWORD			cFormats;				// Count of Texture Formats
    LPDDModeInfo    lpFormatRoot;			// List of Texture Formats (root)
	LPDDModeInfo	lpFormatTail;			// List of Texture Formats (tail)

	// Node Info
	LPD3DDevInfo	lpPrev;					// Prev Node
	LPD3DDevInfo	lpNext;					// Next Node

    // Flags
    BOOL isValid (void)     { return (fFlags & DDDRV_VALID); }
    void validOn (void)     { fFlags |= DDDRV_VALID; }
    void validOff (void)    { fFlags &= ~DDDRV_VALID; }

    // Methods
    HRESULT Create (LPGUID lpGuid, LPTSTR lpName, LPTSTR lpDesc,
                    LPD3DDEVICEDESC lpHalDevice,
                    LPD3DDEVICEDESC lpHelDevice);
    void Destroy (void);

    // Constructors 
    D3DDevInfo (void) 
        { 
		validOff ();
		turnFormatsLoadedOff ();

		szName = NULL;
		szDesc = NULL;

        ZeroMemory (&d3dHalDesc, sizeof(D3DDEVICEDESC)); 
        d3dHalDesc.dwSize = sizeof (D3DDEVICEDESC);

        ZeroMemory (&d3dHelDesc, sizeof(D3DDEVICEDESC)); 
        d3dHelDesc.dwSize = sizeof (D3DDEVICEDESC);

		cFormats = 0L;
		lpFormatRoot = NULL;
		lpFormatTail = NULL;

		lpPrev = NULL;
		lpNext = NULL;
        }

    ~D3DDevInfo (void) 
		{ 
		Destroy (); 
		}

	
	// Texure Format Methods
	BOOL formatsLoaded (void)			{ return ((fFlags & D3DDEV_FORMATS_LOADED) ? TRUE : FALSE); }	
	void turnFormatsLoadedOn (void)		{ fFlags |= D3DDEV_FORMATS_LOADED; }
	void turnFormatsLoadedOff (void)	{ fFlags &= ~D3DDEV_FORMATS_LOADED; }

	HRESULT LoadFormats (LPDIRECT3DDEVICE2 lpd3dDevice);
	HRESULT DestroyFormats (void);

	HRESULT AddFormat (LPDDModeInfo lpFormatNew);
	HRESULT DelFormat (LPDDModeInfo lpFormatDel);
	DWORD   countFormats (void) { return cFormats; }

    LPDDModeInfo FindFormat (DWORD dwBPP, 
						     LPDDModeInfo * lpNextBest,
						     LPDDModeInfo lpStartFormat = NULL);
    LPDDModeInfo FindFormat (LPDDPIXELFORMAT lpddsd, 
						     LPDDModeInfo * lpNextBest,
						     LPDDModeInfo lpStartFormat = NULL);

    DWORD EnumFormats (const D3DDEV_ENUMINFO & eiInfo);


    // Methods
    BOOL isHardware (void);
    BOOL Match (LPGUID lpGuid);
    BOOL Match (LPD3DDEVICEDESC lpHalDesc, LPD3DDEVICEDESC lpHelDesc);

}; // End D3DDevInfo




/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvInfo
**  Purpose:    Encapsulates DD/D3D driver info
**-----------------------------------------------------------------------------
*/

class DDDrvInfo
{
private:
protected:
public:

    // Structure info
    DWORD           fFlags;                         // D3D Driver flags

    // Driver Info
    GUID            guid;                           // guid, if any
    LPTSTR          szName;							// name
    LPTSTR          szDesc;							// description

    // Driver Caps
    DDCAPS          ddHalCaps;                      // Hardware caps
    DDCAPS          ddHelCaps;                      // Emulation caps

    // Mode Info
	DWORD			cModes;							// Count of Modes
    LPDDModeInfo    lpModeRoot;						// List of Modes (root)
	LPDDModeInfo	lpModeTail;						// List of Modes (tail)

    // D3D Info
	DWORD			cDevices;						// Count of D3D devices
    LPD3DDevInfo    lpDeviceRoot;					// List of D3D Devices (root)
	LPD3DDevInfo	lpDeviceTail;					// List of D3D Devices (tail)

	// Node Info
	LPDDDrvInfo		lpPrev;							// Previous node
	LPDDDrvInfo		lpNext;							// Next node


    //
    // Flag Methods
    //
    BOOL isValid (void)     { return (fFlags & DDDRV_VALID); }
    void validOn (void)     { fFlags |= DDDRV_VALID; }
    void validOff (void)    { fFlags &= ~DDDRV_VALID; }

    BOOL isPrimary (void)   { return (fFlags & DDDRV_PRIMARY); }
    void primaryOn (void)   { fFlags |= DDDRV_PRIMARY; }
    void primaryOff (void)  { fFlags &= ~DDDRV_PRIMARY; }

    BOOL isD3D (void)     { return (fFlags & DDDRV_D3D); }
    void d3dOn (void)     { fFlags |= DDDRV_D3D; }
    void d3dOff (void)    { fFlags &= ~DDDRV_D3D; }


    // Methods
    HRESULT Create (LPGUID lpGuid, LPTSTR lpszName, LPTSTR lpszDesc);
    void    Destroy (void);

    // Constructors
    DDDrvInfo (void)
        { 
		validOff ();
        
		ZeroMemory (&ddHalCaps, sizeof (DDCAPS)); 
		ddHalCaps.dwSize = sizeof (DDCAPS);

		ZeroMemory (&ddHelCaps, sizeof (DDCAPS)); 
		ddHelCaps.dwSize = sizeof (DDCAPS);

		cModes  = 0L;
		lpModeRoot = NULL;
		lpModeTail = NULL;

		cDevices = 0L;
		lpDeviceRoot = NULL;
		lpDeviceTail = NULL;

		lpPrev = NULL;
		lpNext = NULL;
        }

    ~DDDrvInfo (void) 
		{ 
		Destroy (); 
		}


	// Mode Methods
	BOOL modesLoaded (void)				{ return ((fFlags & DDDRV_MODES_LOADED) ? TRUE : FALSE); }
	void turnModesLoadedOn (void)		{ fFlags |= DDDRV_MODES_LOADED; }
	void turnModesLoadedOff (void)		{ fFlags &= ~DDDRV_MODES_LOADED; }

	HRESULT LoadModes (LPDIRECTDRAW2 lpDD2);
	HRESULT DestroyModes (void);

	HRESULT AddMode (LPDDModeInfo lpModeNew);
	HRESULT DelMode (LPDDModeInfo lpModeDel);
	DWORD countModes (void) { return cModes; }

    LPDDModeInfo FindMode (DWORD dwW, DWORD dwH, 
						   DWORD dwBPP, DWORD dwRefresh,
						   LPDDModeInfo * lpNextBest,
						   LPDDModeInfo lpStartMode = NULL);
    LPDDModeInfo FindMode (LPDDSURFACEDESC lpddsd, 
						   LPDDModeInfo * lpNextBest,
						   LPDDModeInfo lpStartMode = NULL);

    DWORD EnumModes (const DDDRV_ENUMINFO & eiInfo);

	// D3D Device Methods
	BOOL devicesLoaded (void)				{ return ((fFlags & DDDRV_DEVICES_LOADED) ? TRUE : FALSE); }
	void turnDevicesLoadedOn (void)			{ fFlags |= DDDRV_DEVICES_LOADED; }
	void turnDevicesLoadedOff (void)		{ fFlags &= ~DDDRV_DEVICES_LOADED; }

	HRESULT LoadDevices (LPDIRECT3D2 lpD3D2);
	HRESULT DestroyDevices (void);

	HRESULT AddDevice (LPD3DDevInfo lpModeNew);
	HRESULT DelDevice (LPD3DDevInfo lpModeDel);
	DWORD countDevices (void) { return cDevices; }

    LPD3DDevInfo FindDevice (LPGUID lpGuid, LPD3DDevInfo * lpNextBest,
							 LPD3DDevInfo lpStart = NULL);
    LPD3DDevInfo FindDevice (LPD3DDEVICEDESC lpHal, LPD3DDEVICEDESC lpHel, 
							 LPD3DDevInfo * lpNextBest,
							 LPD3DDevInfo lpStart = NULL);

    DWORD EnumDevices (const DDDRV_ENUMINFO & eiInfo);

	// Mode <=> Device compatible find functions 
    LPD3DDevInfo FindDeviceSupportsMode (LPGUID lpGuid, 
										 LPDDModeInfo lpMode,
									     LPD3DDevInfo * lpNextBest,
									     LPD3DDevInfo lpStart = NULL);
	LPDDModeInfo FindModeSupportsDevice (DWORD dwW, DWORD dwH, 
										 DWORD dwBPP, DWORD dwRefresh,
										 LPD3DDevInfo lpDevice,
										 LPDDModeInfo * lpNextBest,
										 LPDDModeInfo lpStart = NULL);
										 

    // Methods
    BOOL  Match (LPGUID lpGuid);
    BOOL  Match (LPDDCAPS lpHal, LPDDCAPS lpHel);

	LPGUID GetGuid (void);

}; // End class DDDrvInfo


   
/*
**-----------------------------------------------------------------------------
**  Name:       DDDrvMgr
**  Purpose:    Creates and Manages DD/D3D driver info
**  Notes:
**          1.  Modes and D3D Drivers may be incompatible with each other
**              code needs to be added to detect and mark this
**          2.  Need to add code for finding exact and nearest mode matches
**              from requested width, height, pixel format, and refresh rate
**          3.  Need to add code for finding D3D devices from
**-----------------------------------------------------------------------------
*/

class DDDrvMgr
{
private:
protected:
public:
    // Static Data
    static DWORD         g_fFlags;                  // Global flags
	static DWORD		 g_cDrivers;				// Count of DD Drivers
    static LPDDDrvInfo	 g_lpDriverRoot;			// Global list of DD Device Drivers (root)
    static LPDDDrvInfo	 g_lpDriverTail;			// Global list of DD Device Drivers (tail)

            
    // Data
    LPDDDrvInfo		lpCurrDriver;					// Pointer to current DD Device driver
    LPDDModeInfo	lpCurrMode;						// Pointer to current mode in DD driver
    LPD3DDevInfo	lpCurrDevice;					// Pointer to current D3D device
	LPDDModeInfo	lpCurrTextureFormat;			// Pointer to current Texture Format

    // Static Methods
    static HRESULT Init (void);
    static HRESULT Fini (void);

    static BOOL isInitialized (void)    { return (g_fFlags & DDDRV_INIT); }
    static void initOn (void)           { g_fFlags |= DDDRV_INIT; }
    static void initOff (void)          { g_fFlags &= ~DDDRV_INIT; }

	// Driver Methods
	static HRESULT LoadDrivers (void);
	static HRESULT DestroyDrivers (void);

	static HRESULT AddDriver (LPDDDrvInfo lpDrvNew);
	static HRESULT DelDriver (LPDDDrvInfo lpDrvDel);
	static DWORD countDrivers (void) { return g_cDrivers; }

    static DWORD EnumDrivers (const DDDRV_ENUMINFO & eiInfo);

    static LPDDDrvInfo FindDriver (LPGUID lpGuidDD, LPDDDrvInfo * dwNextBest,
								   LPDDDrvInfo lpStart = NULL);
    static LPDDDrvInfo FindDriver (LPDDCAPS lpHalCaps, LPDDCAPS lpHelCaps, 
								   LPDDDrvInfo * dwNextBest,
								   LPDDDrvInfo lpStart = NULL);

    // Constructors
    DDDrvMgr (void);
    ~DDDrvMgr (void);

	// DD info methods
    LPDDDrvInfo GetCurrDriver (void)             
		{
		return lpCurrDriver;
		}

    // Mode Methods
    DWORD countModes (void)              
		{ 
		if (lpCurrDriver)
			return lpCurrDriver->countModes ();
		return 0L;
		}

    LPDDModeInfo GetCurrMode (void)             
		{
		return lpCurrMode;
		}

    // D3D info methods
    DWORD countDevices (void)
		{ 
		if (lpCurrDriver)
			return lpCurrDriver->countDevices ();
		return 0L;
		}    

    LPD3DDevInfo GetCurrDevice (void)
		{ 
		return lpCurrDevice;
		}

	// Texture Format methods
	DWORD countFormats (void)
		{
		if (lpCurrDevice)
			return lpCurrDevice->countFormats ();
		return 0L;
		}

	LPDDModeInfo GetCurrTextureFormat (void)
		{
		return lpCurrTextureFormat;
		}
        
}; // End class DDDrvMgr


/*
**-----------------------------------------------------------------------------
**  End of File
**-----------------------------------------------------------------------------
*/
#endif // DRVMGR_H


