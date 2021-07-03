//-----------------------------------------------------------------------------
// File: rmmain.cpp
//
// Desc: Main file for retained-mode samples. Contains Windows UI and D3DRM
//       initialization/cleanup code.
//
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID
#include <windows.h>
#include <string.h>
#include <stdarg.h>
#include <d3drmwin.h>
#include "resource.h"


#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(msg) MessageBox( NULL, msg, "Application Message", MB_OK )


// Functions to build the customize the app for each sample
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM,
				 LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2 view,
				 LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera );
VOID OverrideDefaults( BOOL* bNoTextures, BOOL* pbResizingDisabled, 
					   BOOL* pbConstRenderQuality, CHAR** pstrName );




//-----------------------------------------------------------------------------
// GLOBAL VARIABLES
//-----------------------------------------------------------------------------
LPDIRECT3DRM3         g_pD3DRM      = NULL; // D3DRM object
LPDIRECTDRAWCLIPPER  g_pDDClipper  = NULL; // DDrawClipper object
LPDIRECT3DRMDEVICE3   g_pDevice     = NULL; // D3DRM device 
LPDIRECT3DRMVIEWPORT2 g_pViewport   = NULL; // D3DRM viewport
LPDIRECT3DRMFRAME3    g_pScene      = NULL; // Root frame for the scene
LPDIRECT3DRMFRAME3    g_pCamera     = NULL; // Camera's frame

#define MAX_DRIVERS 5               // Max # of D3D drivers to enumerate
GUID g_DriverGUID[MAX_DRIVERS];     // GUIDs of the available D3D drivers
CHAR g_DriverName[MAX_DRIVERS][50]; // names of the available D3D drivers
WORD g_wNumDrivers = 0;             // number of available D3D drivers
WORD g_wCurrDriver = 0;             // D3D driver currently being used

D3DRMRENDERQUALITY  g_eRenderQuality;        // Shade, fill and light state
D3DRMTEXTUREQUALITY g_eTextureQuality;       // Texture interpolation
BOOL                g_bDithering    = FALSE; // Dithering enable flag
BOOL                g_bAntialiasing = FALSE; // Antialiasing enable flag

BOOL g_bQuit               = FALSE; // Program is about to terminate
BOOL g_bInitialized        = FALSE; // Objects have been initialized
BOOL g_bMinimized          = FALSE; // Window is minimized
BOOL g_bSingleStepMode     = FALSE; // Render one frame at a time
BOOL g_bDrawAFrame         = FALSE; // Render the scene
BOOL g_bNoTextures         = FALSE; // This sample doesn't use any textures
BOOL g_bConstRenderQuality = FALSE; // Whether sample is not constructed
                                    // w/MeshBuilders and so the RenderQuality
                                    // cannot be changed

DWORD g_wBPP;                 // bit depth of the current display mode

WORD  g_wMouseButtons;         // mouse button state
WORD  g_wMouseX;               // mouse cursor x position
WORD  g_wMouseY;               // mouse cursor y position




//-----------------------------------------------------------------------------
// PROTOTYPES
//-----------------------------------------------------------------------------
LONG FAR PASCAL WndProc( HWND, UINT, WPARAM, LPARAM );
extern VOID     ReadMouse( WORD*, WORD*, WORD* );

HWND     InitApp( HINSTANCE, int );
VOID     InitGlobals();
HRESULT  CreateDevAndView( LPDIRECTDRAWCLIPPER lpDDClipper, WORD wDriver,
						   DWORD dwWidth, DWORD dwHeight );
HRESULT  RenderScene();
VOID     CleanUpAndPostQuit();
VOID     SetRenderState();
BOOL     BuildDeviceMenu( HMENU hmenu );









//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Initializes the application then enters a message loop which renders
//       the scene until a quit message is received.
//-----------------------------------------------------------------------------
int PASCAL WinMain( HINSTANCE hInst, HINSTANCE, LPSTR cmdline, int nCmdShow )
{
    HWND hWnd;
    MSG  msg;
    WORD wFailCount = 0;  // number of times RenderScene() has failed

    // Create the window and initialize all objects needed to begin rendering
    if( !( hWnd = InitApp( hInst, nCmdShow ) ) )
        return 1;

    HACCEL accel = LoadAccelerators( hInst, "AppAccel" );

    while( !g_bQuit )
	{
        // Monitor the message queue until there are no pressing messages
        while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
		{
            if( msg.message == WM_QUIT )
			{
                CleanUpAndPostQuit();
                break;
            }
            if( !TranslateAccelerator( msg.hwnd, accel, &msg ) )
			{
                TranslateMessage( &msg );
                DispatchMessage( &msg );
            }
        }
		if( g_bQuit )
			break;

		// If the app is not minimized, not about to quit and D3DRM has
        // been initialized, we can render
        if( !g_bMinimized && !g_bQuit && g_bInitialized )
		{
            // If were are not in single step mode or if we are and the
            // bDrawAFrame flag is set, render one frame
            if( !( g_bSingleStepMode && !g_bDrawAFrame ) ) 
			{
                // Attempt to render a frame, if it fails, take a note.  If
                // rendering fails more than twice, abort execution.
                if( FAILED( RenderScene() ) )
				{
	                if( ++wFailCount > 2 )
					{
			            MSG( "Render failed. Aborting.\n" );
				        CleanUpAndPostQuit();
					    break;
					}
				}
            }

            // Reset the g_bDrawAFrame flag if we are in single step mode
            if( g_bSingleStepMode )
                g_bDrawAFrame = FALSE;
        }
		else
			WaitMessage();
    }

    DestroyWindow( hWnd );
    return msg.wParam;
}




//-----------------------------------------------------------------------------
// Name: AddMediaPath()
// Desc: Looks in the system registry to determine the media path for the
//       sample. Then, it adds that path to search path for D3DRM.
//-----------------------------------------------------------------------------
VOID AddMediaPath( LPDIRECT3DRM3 pD3DRM )
{
	HKEY   key;
	LONG   result;
	TCHAR  strPath[512];
	DWORD  type, size = 512;

	// Open the registry
	result = RegOpenKeyEx( HKEY_LOCAL_MACHINE, "Software\\Microsoft\\DirectX",
						   0, KEY_READ, &key );
	if( ERROR_SUCCESS != result )
		return;

	// Search for the desired registry value, and close the registry
        result = RegQueryValueEx( key, "DXSDK Samples Path", NULL, &type, 
		                      (BYTE*)strPath, &size );
	RegCloseKey( key );

	if( ERROR_SUCCESS != result )
		return;

	strcat( strPath, "\\D3DRM\\Media" );

	pD3DRM->AddSearchPath( strPath );

	return;
}




//-----------------------------------------------------------------------------
// Name: InitApp()
// Desc: Creates window and initializes all objects neccessary to begin
//       rendering.
//-----------------------------------------------------------------------------
HWND InitApp( HINSTANCE hInst, int nCmdShow )
{
    DWORD    dwFlags;
    HRESULT  hr;
    RECT     rc;

	// Set up and registers the window class
    WNDCLASS wndClass = { CS_HREDRAW | CS_VREDRAW, WndProc, 0, sizeof(DWORD),
		                  hInst, LoadIcon( hInst, TEXT("AppIcon") ),
						  LoadCursor(NULL, IDC_ARROW), 
						  (HBRUSH)GetStockObject(BLACK_BRUSH), 
						  TEXT("AppMenu"), TEXT("D3DRM Window") };

    if (!RegisterClass( &wndClass ))
        return FALSE;

	// Initialize the global variables and allow the sample code to override
    // some of these default settings.
    g_eRenderQuality  = D3DRMLIGHT_ON | D3DRMFILL_SOLID | D3DRMSHADE_GOURAUD;
    g_eTextureQuality = D3DRMTEXTURE_NEAREST;

    BOOL  bResizingDisabled = FALSE;
    CHAR* strWindowTitle    = TEXT("D3DRM Example");
    OverrideDefaults( &g_bNoTextures, &bResizingDisabled, 
		              &g_bConstRenderQuality, &strWindowTitle );

    // Create the window
    if( bResizingDisabled )
        dwFlags =  WS_VISIBLE | WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU |
                   WS_MINIMIZEBOX | WS_MAXIMIZEBOX;
    else
        dwFlags = WS_OVERLAPPEDWINDOW;
    
	HWND hWnd = CreateWindow( "D3DRM Window", strWindowTitle, dwFlags,
		                      CW_USEDEFAULT, CW_USEDEFAULT, 300, 300,
						      NULL, NULL, hInst, NULL );
    if( !hWnd )
        return FALSE;

    // Record the current display BPP
    HDC hdc = GetDC( hWnd);
    g_wBPP = GetDeviceCaps( hdc, BITSPIXEL );
    ReleaseDC( hWnd, hdc );

	// Enumerate the D3D drivers into a menu and select one
    if( !BuildDeviceMenu( GetSubMenu( GetMenu(hWnd), 0 ) ) )
        return FALSE;

	// Create the D3DRM object and the D3DRM window object
	LPDIRECT3DRM pD3DRM1;
	if( FAILED( Direct3DRMCreate( &pD3DRM1 ) ) )
	{
        MSG( "Failed to create Direct3DRM.\n" );
        return FALSE;
    }
	if ( FAILED( pD3DRM1->QueryInterface(IID_IDirect3DRM3, (void **)&g_pD3DRM) ) )
	{
        MSG( "Failed to QI for Direct3DRM3.\n" );
        return FALSE;
	}

	// Create a clipper and associate the window with it
    if( FAILED( DirectDrawCreateClipper(0, &g_pDDClipper, NULL) ) )
	{
        MSG( "Failed to create DirectDrawClipper");
        return FALSE;
    }
    if( FAILED( g_pDDClipper->SetHWnd( 0, hWnd ) ) )
	{
        MSG( "Failed to set hwnd on the clipper");
        return FALSE;
    }


	// Create the master scene frame and camera frame
    if( FAILED( g_pD3DRM->CreateFrame( NULL, &g_pScene ) ) )
	{
        MSG( "Failed to create the master scene frame.\n" );
        return FALSE;
    }
    if( FAILED( g_pD3DRM->CreateFrame(g_pScene, &g_pCamera) ) )
	{
        MSG( "Failed to create the camera's frame.\n" );
        return FALSE;
    }
    if( FAILED( g_pCamera->SetPosition( g_pScene, 0.0f, 0.0f, 0.0f ) ) )
	{
        MSG( "Failed to position the camera in the frame.\n" );
        return FALSE;
    }

	// Created the D3DRM device with the selected D3D driver
    GetClientRect( hWnd, &rc );
    if( hr = CreateDevAndView( g_pDDClipper, g_wCurrDriver, rc.right,
		                       rc.bottom ) )
	{
		g_wCurrDriver = 0;
		if( hr = CreateDevAndView( g_pDDClipper, g_wCurrDriver, rc.right,
			                       rc.bottom ) )
		{
			MSG( "Failed to create the D3DRM device.\n" );
			return FALSE;
		}
    }


	AddMediaPath( g_pD3DRM );
	
	// Create the scene to be rendered by calling this sample's BuildScene
    if( !BuildScene( g_pD3DRM, g_pDevice, g_pViewport, g_pScene, g_pCamera ) )
        return FALSE;

	// Display the window
    ShowWindow( hWnd, nCmdShow );
    UpdateWindow( hWnd );

	// Now we are ready to render
    g_bInitialized = TRUE;

    return hWnd;
}




//-----------------------------------------------------------------------------
// Name: CreateDevAndView()
// Desc: Create the D3DRM device and viewport with the given D3D driver and of
//       the specified size.
//-----------------------------------------------------------------------------
HRESULT CreateDevAndView( LPDIRECTDRAWCLIPPER pDDClipper, WORD wDriver,
						  DWORD dwWidth, DWORD dwHeight )
{
    HRESULT hr;

    if( !dwWidth || !dwHeight)
        return D3DRMERR_BADVALUE;

	// Create the D3DRM device from this window and using the specified D3D
    // driver.
    if( FAILED( hr = g_pD3DRM->CreateDeviceFromClipper( pDDClipper,
		                                &g_DriverGUID[wDriver],
                                        dwWidth, dwHeight, &g_pDevice ) ) )
        return hr;

	// Create the D3DRM viewport using the camera frame.  Set the background
    // depth to a large number.  The width and height may be slightly
    // adjusted, so get them from the device to be sure.
    dwWidth  = g_pDevice->GetWidth();
    dwHeight = g_pDevice->GetHeight();
    
	if( FAILED( hr = g_pD3DRM->CreateViewport( g_pDevice, g_pCamera, 0, 0,
		                                       dwWidth, dwHeight,
											   &g_pViewport ) ) )
	{
		g_bInitialized = FALSE;
        SAFE_RELEASE(g_pDevice);
        return hr;
    }

    g_pViewport->SetBack( 5000.0f );

	// Set the render quality, fill mode, lighting state and color shade info
    SetRenderState();

    return D3DRM_OK;
}




//-----------------------------------------------------------------------------
// Name: enumDeviceFunc()
// Desc: Callback function which records each usable D3D driver's name and GUID
//       Chooses a driver to begin with and sets *lpContext to this starting
//       driver
//-----------------------------------------------------------------------------
HRESULT WINAPI enumDeviceFunc( LPGUID pGuid, PSTR pDeviceDescription,
							   LPSTR pDeviceName, LPD3DDEVICEDESC pHWDesc, 
							   LPD3DDEVICEDESC pHELDesc, VOID* pContext )
{
    WORD* pStartDriver = (WORD*)pContext;

    static BOOL bHardware = FALSE; // Current start driver is hardware
    static BOOL bMono = FALSE;     // Current start driver is mono light

	// Decide which device description we should consult
    LPD3DDEVICEDESC pDesc = pHWDesc->dcmColorModel ? pHWDesc : pHELDesc;

	// If this driver cannot render in the current display bit depth skip
    // it and continue with the enumeration.

	DWORD dwDisplayBitDepth = ( (g_wBPP==4)  ? DDBD_4 : 
	                          ( (g_wBPP==8)  ? DDBD_8 : 
							  ( (g_wBPP==16) ? DDBD_16 : 
							  ( (g_wBPP==24) ? DDBD_24 : 
							  ( (g_wBPP==32) ? DDBD_32 : 0 ) ) ) ) );

	if( !(pDesc->dwDeviceRenderBitDepth & dwDisplayBitDepth ) )
		return D3DENUMRET_OK;

	// Record this driver's info
    memcpy( &g_DriverGUID[g_wNumDrivers], pGuid, sizeof(GUID) );
    lstrcpy( &g_DriverName[g_wNumDrivers][0], pDeviceName );

	// Choose hardware over software, RGB lights over mono lights
    if( 0 == g_wNumDrivers )
	{
        // This is the first valid driver
        *pStartDriver = g_wNumDrivers;
    }
	else if( pDesc == pHWDesc && !bHardware )
	{
        // This driver is hardware and start driver is not
        *pStartDriver = g_wNumDrivers;
    }
	else if( (pDesc==pHWDesc && bHardware) || (pDesc==pHELDesc && !bHardware) )
	{
        if( pDesc->dcmColorModel == D3DCOLOR_MONO && !bMono )
		{
            // This driver and start driver are the same type and this
            // driver is mono while start driver is not
            *pStartDriver = g_wNumDrivers;
        }
    }

    bHardware = ( pDesc == pHWDesc ? TRUE : FALSE );
    bMono     = ( pDesc->dcmColorModel & D3DCOLOR_MONO ? TRUE : FALSE );

    if( ++g_wNumDrivers >= MAX_DRIVERS )
        return D3DENUMRET_CANCEL;
    return D3DENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: BuildDeviceMenu()
// Desc: Enumerate the available D3D drivers, add them to the file menu, and
//       choose one to use.
//-----------------------------------------------------------------------------
BOOL BuildDeviceMenu( HMENU hmenu )
{
    LPDIRECTDRAW pDD;
    LPDIRECT3D   pD3D;

    // Create a DirectDraw object and query for the Direct3D interface to use
    // to enumerate the drivers.
    if( SUCCEEDED( DirectDrawCreate( NULL, &pDD, NULL ) ) )
	{
	    if( SUCCEEDED( pDD->QueryInterface( IID_IDirect3D, (VOID**)&pD3D ) ) )
		{
			// Enumerate the drivers
			pD3D->EnumDevices( enumDeviceFunc, &g_wCurrDriver );
			
			// Make sure we found at least one valid driver
			if( g_wNumDrivers > 0 )
			{
				// Add the driver names to the File menu
				for( int i=0; i<g_wNumDrivers; i++ )
					InsertMenu( hmenu, 5+i, MF_BYPOSITION | MF_STRING,
					            MENU_FIRST_DRIVER+i, g_DriverName[i] );
			}
			else
				MSG( "Can't find a driver compatible w/this display depth" );
			pD3D->Release();
		}
		else
	        MSG( "Creation of Direct3D interface failed.\n" );
		pDD->Release();
	}
	else
		MSG( "Creation of DirectDraw HEL failed.\n" );

	return (g_wNumDrivers > 0 ) ? TRUE: FALSE;
}




//-----------------------------------------------------------------------------
// Name: RenderScene()
// Desc: Clear the viewport, render the next frame and update the window
//-----------------------------------------------------------------------------
HRESULT RenderScene()
{
	// Tick the scene
    if( SUCCEEDED( g_pScene->Move( 1.0f ) ) )
	{
		// Clear the viewport
		if( SUCCEEDED( g_pViewport->Clear( D3DRMCLEAR_ALL ) ) )
		{
			// Render the scene to the viewport
			if( SUCCEEDED( g_pViewport->Render( g_pScene ) ) )
			{
				// Update the window
				if( SUCCEEDED( g_pDevice->Update() ) )
					return S_OK;
			}
		}
	}

	MSG("Could not render the scene.\n" );
    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: WndProc()
// Desc: Main window message handler
//-----------------------------------------------------------------------------
LONG FAR PASCAL WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    WORD    i;
    HRESULT hr;
    RECT    rc;
	HMENU   hMenu = (HMENU)wParam; // For menu commands only

	if( !g_bInitialized )
		return DefWindowProc( hWnd, msg, wParam, lParam );

    switch( msg )
	{
        case WM_LBUTTONDOWN:
        case WM_LBUTTONUP:
        case WM_RBUTTONDOWN:
        case WM_RBUTTONUP:
        case WM_MOUSEMOVE:
            // Record the mouse state for ReadMouse
            g_wMouseButtons = wParam;
            g_wMouseX       = LOWORD(lParam);
            g_wMouseY       = HIWORD(lParam);
            break;
        
		case WM_INITMENUPOPUP:
            // Check and enable the appropriate menu items
			for( i=0; i<g_wNumDrivers; i++ )
                CheckMenuItem(  hMenu, MENU_FIRST_DRIVER+i,
                               (i==g_wCurrDriver)?MF_CHECKED:MF_UNCHECKED );

            CheckMenuItem( hMenu, MENU_STEP, g_bSingleStepMode ? MF_CHECKED : MF_UNCHECKED );
            EnableMenuItem(hMenu, MENU_GO,   g_bSingleStepMode ? MF_ENABLED : MF_GRAYED );
            if( !g_bConstRenderQuality )
			{
                CheckMenuItem( hMenu, MENU_LIGHTING,  (g_eRenderQuality & D3DRMLIGHT_MASK) == D3DRMLIGHT_ON       ? MF_CHECKED : MF_GRAYED );
                CheckMenuItem( hMenu, MENU_FLAT,      (g_eRenderQuality & D3DRMSHADE_MASK) == D3DRMSHADE_FLAT     ? MF_CHECKED : MF_UNCHECKED );
                CheckMenuItem( hMenu, MENU_GOURAUD,   (g_eRenderQuality & D3DRMSHADE_MASK) == D3DRMSHADE_GOURAUD  ? MF_CHECKED : MF_UNCHECKED );
                CheckMenuItem( hMenu, MENU_PHONG,     (g_eRenderQuality & D3DRMSHADE_MASK) == D3DRMSHADE_PHONG    ? MF_CHECKED : MF_UNCHECKED );
                CheckMenuItem( hMenu, MENU_POINT,     (g_eRenderQuality & D3DRMFILL_MASK)  == D3DRMFILL_POINTS    ? MF_CHECKED : MF_UNCHECKED );
                CheckMenuItem( hMenu, MENU_WIREFRAME, (g_eRenderQuality & D3DRMFILL_MASK)  == D3DRMFILL_WIREFRAME ? MF_CHECKED : MF_UNCHECKED );
                CheckMenuItem( hMenu, MENU_SOLID,     (g_eRenderQuality & D3DRMFILL_MASK)  == D3DRMFILL_SOLID     ? MF_CHECKED : MF_UNCHECKED );
            } 
			else
			{
                EnableMenuItem( hMenu, MENU_LIGHTING,  MF_GRAYED );
                EnableMenuItem( hMenu, MENU_FLAT,      MF_GRAYED );
                EnableMenuItem( hMenu, MENU_GOURAUD,   MF_GRAYED );
                EnableMenuItem( hMenu, MENU_PHONG,     MF_GRAYED );
                EnableMenuItem( hMenu, MENU_POINT,     MF_GRAYED );
                EnableMenuItem( hMenu, MENU_WIREFRAME, MF_GRAYED );
                EnableMenuItem( hMenu, MENU_SOLID,     MF_GRAYED );
            }
            if( !g_bNoTextures )
			{
                CheckMenuItem( hMenu, MENU_POINT_FILTER,  (g_eTextureQuality == D3DRMTEXTURE_NEAREST) ? MF_CHECKED : MF_UNCHECKED);
                CheckMenuItem( hMenu, MENU_LINEAR_FILTER, (g_eTextureQuality == D3DRMTEXTURE_LINEAR) ? MF_CHECKED : MF_UNCHECKED);
            } 
			else 
			{
                EnableMenuItem( hMenu, MENU_POINT_FILTER, MF_GRAYED);
                EnableMenuItem( hMenu, MENU_LINEAR_FILTER, MF_GRAYED);
            }
            CheckMenuItem(  hMenu, MENU_DITHERING, g_bDithering    ? MF_CHECKED : MF_UNCHECKED );
            CheckMenuItem(  hMenu, MENU_ANTIALIAS, g_bAntialiasing ? MF_CHECKED : MF_UNCHECKED );
			
            EnableMenuItem( hMenu, MENU_PHONG,     MF_GRAYED );
            EnableMenuItem( hMenu, MENU_ANTIALIAS, MF_GRAYED);
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
			{
                case MENU_EXIT:
                    CleanUpAndPostQuit();
                    break;
                
				case MENU_STEP:
                    // Go into single step mode and draw a frame
                    g_bSingleStepMode = TRUE;
                    g_bDrawAFrame     = TRUE;
                    break;

                case MENU_GO:
                    // Exit single step mode
                    g_bSingleStepMode = FALSE;
                    break;
				
				default:
					switch( LOWORD(wParam) )
					{
		                case MENU_LIGHTING:
							g_eRenderQuality ^= D3DRMLIGHT_ON;
				            break;
						case MENU_POINT:
							g_eRenderQuality = (g_eRenderQuality & ~D3DRMFILL_MASK) | D3DRMFILL_POINTS;
		                    break;
						case MENU_WIREFRAME:
							g_eRenderQuality = (g_eRenderQuality & ~D3DRMFILL_MASK) | D3DRMFILL_WIREFRAME;
							break;
						case MENU_SOLID:
							g_eRenderQuality = (g_eRenderQuality & ~D3DRMFILL_MASK) | D3DRMFILL_SOLID;
							break;
						case MENU_FLAT:
							g_eRenderQuality = (g_eRenderQuality & ~D3DRMSHADE_MASK) | D3DRMSHADE_FLAT;
							break;
						case MENU_GOURAUD:
							g_eRenderQuality = (g_eRenderQuality & ~D3DRMSHADE_MASK) | D3DRMSHADE_GOURAUD;
							break;
						case MENU_PHONG:
							g_eRenderQuality = (g_eRenderQuality & ~D3DRMSHADE_MASK) | D3DRMSHADE_PHONG;
							break;
						case MENU_DITHERING:
							g_bDithering = !g_bDithering;
							break;
						case MENU_ANTIALIAS:
							g_bAntialiasing = !g_bAntialiasing;
							break;
		                case MENU_POINT_FILTER:
				            g_eTextureQuality = D3DRMTEXTURE_NEAREST;
						    break;
		                case MENU_LINEAR_FILTER:
				            g_eTextureQuality = D3DRMTEXTURE_LINEAR;
						    break;
					}
					SetRenderState();
					break;
            }
            
            // Changing the D3D Driver
			if( LOWORD(wParam) >= MENU_FIRST_DRIVER &&
                    LOWORD(wParam) < MENU_FIRST_DRIVER + MAX_DRIVERS &&
                    g_wCurrDriver != LOWORD(wParam) - MENU_FIRST_DRIVER )
			{
				// Release the current viewport and device and create
                // the new one
			    WORD wLastDriver = g_wCurrDriver;
			    g_bInitialized = FALSE;
                SAFE_RELEASE(g_pViewport);
                SAFE_RELEASE(g_pDevice);
                g_wCurrDriver = LOWORD(wParam)-MENU_FIRST_DRIVER;
                GetClientRect( hWnd, &rc );
                if( hr = CreateDevAndView(g_pDDClipper, g_wCurrDriver, rc.right, rc.bottom) )
				{
					g_wCurrDriver = wLastDriver;
					if( hr = CreateDevAndView(g_pDDClipper, g_wCurrDriver,
						    rc.right, rc.bottom))
					{
						MSG("Failed to create the D3DRM device.\n" );
						CleanUpAndPostQuit();
					}
					else
					{
						MSG("Not enough vidmem to use the 3D hardware device.\nRestoring software device.");
						g_bInitialized = TRUE;
					}
                }
				else
					g_bInitialized = TRUE;
            }

			// Draw a frame in single step mode after every command
            g_bDrawAFrame = TRUE;
            break;

		case WM_DESTROY:
			CleanUpAndPostQuit();
			break;
		
		case WM_SIZE:
			// Handle resizing of the window
			{
				int width = LOWORD(lParam);
				int height = HIWORD(lParam);
				if (width && height && g_pViewport && g_pDevice)
				{
					int view_width  = g_pViewport->GetWidth();
					int view_height = g_pViewport->GetHeight();
					int dev_width   = g_pDevice->GetWidth();
					int dev_height  = g_pDevice->GetHeight();
					// If the window hasn't changed size and we aren't returning from
					// a minimize, there is nothing to do
					if (view_width == width && view_height == height &&
						!g_bMinimized)
						break;
					if (width <= dev_width && height <= dev_height)
					{
						// If the window has shrunk, we can use the same device with a
						// new viewport
						g_bInitialized = FALSE;
						SAFE_RELEASE(g_pViewport);
						
						if( FAILED( g_pD3DRM->CreateViewport( g_pDevice, g_pCamera,
													          0, 0, width, height,
													          &g_pViewport) ) )
						{
							MSG("Failed to resize the viewport.\n" );
							CleanUpAndPostQuit();
							break;
						}
						g_pViewport->SetBack( 5000.0f );

						g_bInitialized = TRUE;
					} 
					else
					{
					 	// If the window got larger than the current device, create a
						// new device.
						g_bInitialized = FALSE;
						SAFE_RELEASE(g_pViewport);
						SAFE_RELEASE(g_pDevice);
						if( hr = CreateDevAndView( g_pDDClipper, g_wCurrDriver,
								                   width, height ) )
						{
							g_wCurrDriver = 0;
							if( hr = CreateDevAndView( g_pDDClipper, g_wCurrDriver,
													   width, height ) )
							{
								MSG("Failed to create the D3DRM device.\n" );
								CleanUpAndPostQuit();
							}
							else
								MSG("Not enough vidmem to use the 3D hardware device.\nUsing software rendering.");
						}

						g_bInitialized = TRUE;
					}
					// We must not longer be minimized
					g_bMinimized = FALSE;
				} 
				else
					// This is a minimize message
					g_bMinimized = TRUE;
			}
			g_bDrawAFrame = TRUE;
			break;
		case WM_ACTIVATE:
			{
				 // Create a Windows specific D3DRM window device to handle this
				 // message
				LPDIRECT3DRMWINDEVICE windev;
				if( !g_pDevice )
					break;
				if( SUCCEEDED( g_pDevice->QueryInterface( IID_IDirect3DRMWinDevice,
													      (VOID**) &windev ) ) )
				{
					if( FAILED(windev->HandleActivate(wParam) ) )
						MSG("Failed to handle WM_ACTIVATE.\n");
					windev->Release();
				}
				else
					MSG("Failed to create Windows device to handle WM_ACTIVATE.\n");
			}
			break;
		case WM_PAINT:
			if (!g_bInitialized || !g_pDevice)
				return DefWindowProc( hWnd, msg, wParam, lParam );

			// Create a Windows specific D3DRM window device to handle this
			// message
			RECT r;
			PAINTSTRUCT ps;
			LPDIRECT3DRMWINDEVICE windev;

			if( GetUpdateRect( hWnd, &r, FALSE ) )
			{
				BeginPaint( hWnd, &ps );
				if( SUCCEEDED( g_pDevice->QueryInterface( IID_IDirect3DRMWinDevice,
					                                      (VOID**)&windev ) ) )
				{
					if( FAILED(windev->HandlePaint( ps.hdc ) ) )
						MSG("Failed to handle WM_PAINT.\n");
					windev->Release();
				}
				else
					MSG("Failed to create Windows device to handle WM_PAINT.\n");
				EndPaint( hWnd, &ps );
			}
			break;
		case WM_DISPLAYCHANGE:
			{
				// If this display change message was generated because another application
				// has gone exclusive, ignore it.
				LPDIRECTDRAW        pDD;
				LPDIRECTDRAWSURFACE pDDS;
				DDSURFACEDESC       ddsd;

				if( DirectDrawCreate( NULL, &pDD, NULL ) )
					break;
				if( FAILED( pDD->SetCooperativeLevel( hWnd, DDSCL_NORMAL ) ) )
				{
					pDD->Release();
					break;
				}
				ZeroMemory( &ddsd, sizeof(ddsd) );
				ddsd.dwSize         = sizeof(ddsd);
				ddsd.dwFlags        = DDSD_CAPS;
				ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;
				hr = pDD->CreateSurface( &ddsd, &pDDS, NULL );
				if( hr == DDERR_NOEXCLUSIVEMODE )
				{
					// This exclusive mode generated WM_DISPLAYCHANGE, ignoring
					pDD->Release();
					break;
				}
				if( SUCCEEDED(hr) )
					pDDS->Release();
				pDD->Release();	    
			}
			GetClientRect( hWnd, &rc );
			g_bInitialized = FALSE;
			if( hr = CreateDevAndView( g_pDDClipper, g_wCurrDriver,
									   rc.right, rc.bottom ) )
			{
				g_wCurrDriver = 0;
				if( hr = CreateDevAndView( g_pDDClipper, g_wCurrDriver,
							               rc.right, rc.bottom ) )
				{
					MSG("Failed to create the D3DRM device.\n" );
					CleanUpAndPostQuit();
				}
				else
					g_bInitialized = TRUE;
			}
			else
				g_bInitialized = TRUE;
			break;
		default:
			return DefWindowProc( hWnd, msg, wParam, lParam );
    }

    return 0L;
}




//-----------------------------------------------------------------------------
// Name: SetRenderState()
// Desc: Set the render quality, dither toggle and shade info if any of them
//       has changed
//-----------------------------------------------------------------------------
VOID SetRenderState()
{
	// Set the render states
    g_pDevice->SetQuality( g_eRenderQuality );
    g_pDevice->SetDither( g_bDithering );
    g_pDevice->SetTextureQuality( g_eTextureQuality );

	// Set shade info based on current bits per pixel
    switch( g_wBPP )
	{
		case 1:
			g_pDevice->SetShades(4);
			g_pD3DRM->SetDefaultTextureShades(4);
			break;
		case 16:
			g_pDevice->SetShades(32);
			g_pD3DRM->SetDefaultTextureColors(64);
			g_pD3DRM->SetDefaultTextureShades(32);
			break;
		case 24:
		case 32:
			g_pDevice->SetShades(256);
			g_pD3DRM->SetDefaultTextureColors(64);
			g_pD3DRM->SetDefaultTextureShades(256);
			break;
    }
}




//-----------------------------------------------------------------------------
// Name: ReadMouse()
// Desc: Returns the mouse status for interaction with sample code
//-----------------------------------------------------------------------------
VOID ReadMouse( WORD* pButtons, WORD* pX, WORD* pY )
{
    (*pButtons) = g_wMouseButtons;
    (*pX)       = g_wMouseX;
    (*pY)       = g_wMouseY;
}




//-----------------------------------------------------------------------------
// Name: CleanUpAndPostQuit()
// Desc: Release all D3DRM objects, post a quit message and set the bQuit flag
//-----------------------------------------------------------------------------
VOID CleanUpAndPostQuit()
{
    SAFE_RELEASE( g_pScene );
    SAFE_RELEASE( g_pCamera );
    SAFE_RELEASE( g_pViewport );
    SAFE_RELEASE( g_pDevice );
    SAFE_RELEASE( g_pD3DRM );
    SAFE_RELEASE( g_pDDClipper );

    g_bInitialized = FALSE;
    g_bQuit        = TRUE;
}




