#ifndef D3DWIN_H
#define D3DWIN_H
/*
**-----------------------------------------------------------------------------
**  Name:       D3DWin.h
**  Purpose:    Sample D3D framework
**
**	Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Includes
**-----------------------------------------------------------------------------
*/
#include "Common.h"
#include "DrvMgr.h"


/*
**-----------------------------------------------------------------------------
**  Defines
**-----------------------------------------------------------------------------
*/

// Windows messages
#define D3DWIN_GET_VALID    (WM_USER+1000)
#define D3DWIN_GET_POINTER  (WM_USER+1001)
#define D3DWIN_GET_SURFACE  (WM_USER+1002)

#define D3DWIN_INIT				  (WM_USER+2000)
#define D3DWIN_FINI				  (WM_USER+2001)
#define D3DWIN_CHANGED_DRIVER	  (WM_USER+2002)
#define D3DWIN_CHANGED_MODE		  (WM_USER+2003)
#define D3DWIN_CHANGED_DEVICE	  (WM_USER+2004)

#define D3DWIN_MIN_SIZE     64



/*
**-----------------------------------------------------------------------------
**  Typedefs
**-----------------------------------------------------------------------------
*/

class D3DScene;						// Prevent having to include D3DScene.h
typedef D3DScene * LPD3DScene;

class D3DWindow;                    // Forward declaration
typedef D3DWindow * LPD3DWindow;



/*
**-----------------------------------------------------------------------------
**  Classes
**-----------------------------------------------------------------------------
*/

/*
**-----------------------------------------------------------------------------
**  Name:       D3DWindow
**  Purpose:    Encapsulates D3D rendering info for a window
**-----------------------------------------------------------------------------
*/

class D3DWindow {
protected:
	//
    // Flags
	//
    enum {
		DWF_FULLSCREEN		= 0x00000001,
        DWF_VISIBLE			= 0x00000002,
        DWF_ZBUFFER			= 0x00000004,
		DWF_ACTIVE			= 0x00000008,
    } Attributes;

    enum {
        DWF_VALID_INTERFACE = 0x00000001,
		DWF_VALID_FULLSCREEN= 0x00000002,
		DWF_VALID_PRIMARY	= 0x00000004,
		DWF_VALID_RENDER    = 0x00000008,
		DWF_VALID_VIEWPORT  = 0x00000010,
		DWF_VALID_SCENE		= 0x00000020,

		DWF_VALID		    = 0x0000001F, // INTERFACE | FULLSCREEN | PRIMARY | RENDER | VIEWPORT
    } Validates;

	
public:

	//
    // Creation Methods
	//
    D3DWindow (void);
    ~D3DWindow (void);

	HRESULT D3DWindow::Create (
		HWND   hWnd,			
		LPGUID lpDDGuid		= NULL,	
		DWORD  dwW			= 0L,
		DWORD  dwH			= 0L,			
		DWORD  dwBPP		= 0L,
		DWORD  dwRefresh	= 0L,
		LPGUID lpGuidD3D	= NULL,
		BOOL   fUseZBuffer	= TRUE);
    HRESULT Init (void);
	HRESULT Fini (void);

	//
    // Window Methods
    //
    HRESULT DrawFrame (void);

    HRESULT Move (long x, long y);
    HRESULT Resize (DWORD w, DWORD h);
	
	HRESULT RealizePalette (void);

	HRESULT toGDI (void);
	HRESULT fromGDI (void);

    BOOL    isPaused (void)             { return (dwPaused != 0); }
    HRESULT Pause (BOOL fOn);      

	BOOL    isFullscreen (void)			{ return ((fValid & DWF_VALID_FULLSCREEN) ? TRUE : FALSE); }

    HRESULT Restore (void );

	// Window State
	BOOL    isActive (void)				{ return ((fAttribs & DWF_ACTIVE) ? TRUE : FALSE); }
    void	turnActiveOn (void)			{ fAttribs |= DWF_ACTIVE; }
    void	turnActiveOff (void)		{ fAttribs &= ~DWF_ACTIVE; }


	//
	// Scene Methods
	//
	HRESULT	   AttachScene (LPD3DScene lpNewScene);
	HRESULT    DetachScene (void);

    
	//
    // Driver Methods
    //
	HRESULT ChangeDesktop (void);
    HRESULT ChangeDriver (LPGUID lpGuid, LPD3DDevInfo lpDevHint = NULL,
						  LPDDModeInfo lpModeHint = NULL);
    HRESULT ChangeDevice (LPGUID lpGuid, LPDDModeInfo lpModeHint = NULL);
    HRESULT ChangeMode (DWORD w, DWORD h, DWORD bpp, DWORD refresh);


	//
	// Misc. Methods
	//
	BOOL isValid (void)         { return (((fValid & DWF_VALID) == DWF_VALID) ? TRUE : FALSE); }

    BOOL isCreateZBuffer (void) { return (fAttribs & DWF_ZBUFFER); }
    void createZBufferOn (void) { fAttribs |= DWF_ZBUFFER; }
    void createZBufferOff (void){ fAttribs &= ~DWF_ZBUFFER; }
    

	//
	// Member methods
	//
	HWND		  GetWindow (void)			{ return hWindow; }
	HCURSOR		  GetCursor (void)			{ return hCursorOld; }
	void		  SetCursor (HCURSOR hNew)	{ hCursorOld = hNew; }

	LPDDDrvInfo	  GetDriverInfo (void)	{ return lpCurrDriver; }
	LPDDModeInfo  GetModeInfo (void)	{ return lpCurrMode; }
	LPD3DDevInfo  GetDeviceInfo (void)	{ return lpCurrDevice; }

	LPGUID		  GetDDGuid (void)		{ return ((lpCurrDriver) ? lpCurrDriver->GetGuid () : NULL); }
	BOOL		  GetModeInfo (DWORD & w, DWORD & h, 
							   DWORD & bpp, DWORD & refresh)
					{
						if (! lpCurrMode)
							return FALSE;

						lpCurrMode->GetMode (w, h , bpp, refresh);
						return TRUE;
					}
	LPGUID		  GetD3DGuid (void)		{ return ((lpCurrDevice) ? &(lpCurrDevice->guid) : NULL); }

	LPDIRECTDRAW  GetDD (void)			{ return lpDD; }
	LPDIRECTDRAW2 GetDD2 (void)			{ return lpDD2; }
	LPDIRECT3D2   GetD3D (void)			{ return lpD3D; }

	LPDIRECTDRAWSURFACE GetPrimary (void)		{ return lpddsPrimary; }
	LPDIRECTDRAWSURFACE GetFrontBuffer (void)	{ return lpddsPrimary; }
	LPDIRECTDRAWSURFACE GetBackBuffer (void)	{ return lpddsRender; }
	LPDIRECTDRAWPALETTE	GetPalette (void)		{ return lpddpPalette; }

	LPDIRECTDRAWSURFACE GetRender (void)		{ return lpddsRender; }
	LPDIRECTDRAWSURFACE GetZBuffer (void)		{ return lpddsZBuff; }
	LPDIRECT3DDEVICE2   GetD3DDevice (void)		{ return lpd3dDevice; }
	LPDIRECT3DVIEWPORT2	GetViewport (void)		{ return lpd3dViewport; }

	HRESULT GetSurfaceRect (RECT & rSurface);
    HRESULT GetPrimaryRect (RECT & rPrimary);

	LPD3DScene GetScene (void)					{ return lpd3dScene; }

protected:

	//
	// Protected Methods
	//

    HRESULT ValidateDefaults (void);

	HRESULT CreateInterfaces (LPGUID lpGuidDD);
    HRESULT InitInterfaces (void);
    HRESULT FiniInterfaces (void);

	HRESULT InitWindow (void);
	HRESULT FiniWindow (void);

	HRESULT InitFullscreenMode (void);
	HRESULT FiniFullscreenMode (void);

	HRESULT InitPrimary (void);
    HRESULT FiniPrimary (void);

    HRESULT InitPalette (void);
    HRESULT FiniPalette (void);

    HRESULT CreateRender (LPGUID lpD3DGuid);
	HRESULT InitRender (void);
    HRESULT FiniRender (void);

	HRESULT InitViewport (void);
	HRESULT FiniViewport (void);
	HRESULT UpdateViewport (void);

	// Track initialization process
    void turnValidInterfaceOn (void)	{ fValid |= DWF_VALID_INTERFACE; }
    void turnValidInterfaceOff(void)	{ fValid &= ~DWF_VALID_INTERFACE; }

    void turnValidPrimaryOn (void)		{ fValid |= DWF_VALID_PRIMARY; }
    void turnValidPrimaryOff (void)		{ fValid &= ~DWF_VALID_PRIMARY; }

    void turnValidFullscreenOn (void)	{ fValid |= DWF_VALID_FULLSCREEN; }
    void turnValidFullscreenOff (void)	{ fValid &= ~DWF_VALID_FULLSCREEN; }

    void turnValidRenderOn (void)		{ fValid |= DWF_VALID_RENDER; }
    void turnValidRenderOff(void)		{ fValid &= ~DWF_VALID_RENDER; }

    void turnValidViewportOn (void)		{ fValid |= DWF_VALID_VIEWPORT; }
    void turnValidViewportOff(void)		{ fValid &= ~DWF_VALID_VIEWPORT; }

	void turnValidSceneOn (void)		{ fValid |= DWF_VALID_SCENE; }
	void turnValidSceneOff (void)		{ fValid &= ~DWF_VALID_SCENE;}

    // Flags
    BOOL isValidDefaults (void)		{ return ((lpCurrDriver && lpCurrMode && lpCurrDevice) ? TRUE : FALSE); }
    BOOL isValidInterface (void)	{ return ((fValid & DWF_VALID_INTERFACE) ? TRUE : FALSE); }
    BOOL isValidFullscreen (void)	{ return ((fValid & DWF_VALID_FULLSCREEN) ? TRUE : FALSE); }
    BOOL isValidPrimary (void)		{ return ((fValid & DWF_VALID_PRIMARY) ? TRUE : FALSE); }
    BOOL isValidRender (void)		{ return ((fValid & DWF_VALID_RENDER) ? TRUE : FALSE); }
	BOOL isValidViewport (void)		{ return ((fValid & DWF_VALID_VIEWPORT) ? TRUE : FALSE); }
	BOOL isValidScene (void)		{ return ((fValid & DWF_VALID_SCENE) ? TRUE : FALSE); }

    HRESULT CalcRects (void);


	//
	// Protected Data
	//
    DWORD           dwSize;             // Structure Size
    DWORD           fAttribs;           // Attribute Flags
    DWORD           fChecks;            // Programming flags
	DWORD			fValid;				// Validation flags

    // Window Data
	HWND            hWindow;            // Window handle
    DWORD           dwPaused;           // Paused Semaphore
	HCURSOR			hCursorOld;			// Old Cursor

    // Driver Defaults
	LPDDDrvInfo		lpCurrDriver;		// Current Driver
	LPDDModeInfo	lpCurrMode;			// Current Mode
	LPD3DDevInfo	lpCurrDevice;		// Current Device
    
	// Interfaces
    LPDIRECTDRAW    lpDD;               // DirectDraw Interface
    LPDIRECTDRAW2   lpDD2;              // DirectDraw2 Interface
    LPDIRECT3D2     lpD3D;              // Direct3D2 Interface
    
    // Primary Surface
    LPDIRECTDRAWSURFACE  lpddsPrimary;  // Primary Surface
    LPDIRECTDRAWPALETTE  lpddpPalette;  // Primary Palette
	DWORD				 cPalette;		// Count of palette entries
    PALETTEENTRY *       lppeSystem;    // Saved System palette entries
	PALETTEENTRY *       lppeCurr;		// Current palette entries

    // Render Surface
    LPDIRECTDRAWSURFACE  lpddsRender;   // Rendering surface
    LPDIRECTDRAWSURFACE  lpddsZBuff;    // Z-Buffer surface
    LPDIRECT3DDEVICE2    lpd3dDevice;	// D3D Device
	LPDIRECT3DVIEWPORT2  lpd3dViewport;	// D3D Viewport

    // Misc.
    RECT                rPrim;          // Current primary pos,size
    RECT                rSurf;          // Current surface pos,size

	// D3D Scene
	LPD3DScene			lpd3dScene;		// D3D Scene

	// Friends
	friend	class D3DScene;

}; // End D3DWindow


   
/*
**-----------------------------------------------------------------------------
**  End of File
**-----------------------------------------------------------------------------
*/
#endif // D3DWIN_H


