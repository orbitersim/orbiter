//-----------------------------------------------------------------------------
// File: ScreenSaver.cpp
//
// Desc: Windows code for making Direct3D screen savers.
//
//       This code uses the Direct3D sample framework.
//
// Copyright (c) 1996-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define STRICT
#include <windows.h>
#include <mmsystem.h>
#include <scrnsave.h>
#include <stdio.h>
#include <regstr.h> 
#include "D3DFrame.h"
#include "D3DEnum.h"
#include "D3DUtil.h"
#include "resource.h"
#include "ScreenSaver.h"




//-----------------------------------------------------------------------------
// Global variables for using the D3D sample framework class
//-----------------------------------------------------------------------------
CD3DFramework7* g_pFramework           = NULL;
BOOL            g_bReady               = FALSE;
extern BOOL     g_bAppUseZBuffer;
extern BOOL     g_bAppUseBackBuffer;




//-----------------------------------------------------------------------------
// Local function-prototypes
//-----------------------------------------------------------------------------
HRESULT Initialize3DEnvironment( HWND );
HRESULT Render3DEnvironment();
VOID    Cleanup3DEnvironment( HWND );
VOID    AppOutputText( LPDIRECT3DDEVICE7, DWORD, DWORD, CHAR* );




//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
HRESULT App_ConfirmDevice( DDCAPS*, D3DDEVICEDESC* );
HRESULT App_OneTimeSceneInit();
VOID    App_DeleteDeviceObjects( HWND, LPDIRECT3DDEVICE7 );
HRESULT App_InitDeviceObjects( HWND, LPDIRECT3DDEVICE7 );
HRESULT App_FrameMove( LPDIRECT3DDEVICE7, FLOAT );
HRESULT App_Render( LPDIRECT3DDEVICE7 );
HRESULT App_RestoreSurfaces();
HRESULT App_FinalCleanup();




//-----------------------------------------------------------------------------
// Name: ReadSettings()
// Desc: 
//-----------------------------------------------------------------------------
VOID ReadSettings( ScreenSaverOptions* pOptions )
{
    HKEY  hKey; 
	DWORD dwType = REG_BINARY;
	DWORD dwSize = sizeof(ScreenSaverOptions);

	TCHAR strRegPath[80] = "";
	strcat( strRegPath, REGSTR_PATH_SCREENSAVE );
	strcat( strRegPath, TEXT("\\ScreenSaver.") );
	strcat( strRegPath, g_strScreenSaverName );

	// Read data from the registry
	if( ERROR_SUCCESS == RegOpenKeyEx( HKEY_CURRENT_USER, strRegPath,
		                               KEY_READ, NULL, &hKey ) )
	{
		RegQueryValueEx( hKey, "Options",  0, &dwType, (BYTE*)pOptions, &dwSize );
		RegCloseKey( hKey ); 
		return;
	}

	// Else, keep current options w/their default values
}




//-----------------------------------------------------------------------------
// Name: WriteSettings()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT WriteSettings( ScreenSaverOptions* pOptions )
{
    HKEY  hKey; 
	DWORD dwDisposition;
	DWORD dwType = REG_BINARY;
	DWORD dwSize = sizeof(ScreenSaverOptions);

	// Build the registry path
	TCHAR strRegPath[80] = "";
	strcat( strRegPath, REGSTR_PATH_SCREENSAVE );
	strcat( strRegPath, TEXT("\\ScreenSaver.") );
	strcat( strRegPath, g_strScreenSaverName );

	// Open the registry
	if( ERROR_SUCCESS != RegCreateKeyEx( HKEY_CURRENT_USER, strRegPath, 0, "",
		                                 REG_OPTION_NON_VOLATILE, 
										 KEY_ALL_ACCESS, NULL, &hKey,
										 &dwDisposition ) )
		return E_FAIL;

	RegSetValueEx( hKey, "Options",  0, dwType, (BYTE*)pOptions, dwSize );
	RegFlushKey( hKey );
    RegCloseKey( hKey ); 

	return S_OK;
}




//-----------------------------------------------------------------------------
// Name: ScreenSaverProc()
// Desc: 
//-----------------------------------------------------------------------------
LRESULT CALLBACK ScreenSaverProc( HWND hWnd, UINT uMsg, WPARAM wParam,
								  LPARAM lParam )
{
	static UINT  uTimer = 0;
	static DWORD dwFakeMouseMoveMsgAllowed = 5;
	HRESULT      hr;

    switch( uMsg )
    {
		case WM_CREATE:
			// Read data from the registry
			ReadSettings( &g_CurrentOptions );
			
			// Initialize the 3D environment for the app
			if( FAILED( hr = Initialize3DEnvironment( hWnd ) ) )
				return 0;

			g_bReady = TRUE;

			// Create a timer
			uTimer = SetTimer( hWnd, 1, 0, NULL );

			return 0;
		
		case WM_MOUSEMOVE:
			// Ignore any fake msgs triggered by DDraw::SetDisplayMode()
			if( dwFakeMouseMoveMsgAllowed )
			{
				dwFakeMouseMoveMsgAllowed--;
				return 0;
			}
			break;

		case WM_ERASEBKGND:
			break;
		
		case WM_TIMER:
			// Draw the scene
            if( g_bReady )
                Render3DEnvironment();
			return 0;
		
		case WM_DESTROY:
			// Clean everything up and exit
			KillTimer( hWnd, 1 );
			Cleanup3DEnvironment( hWnd );
			break;
	}

	// Pass all unhandled msgs to the default msg handler
    return DefScreenSaverProc( hWnd, uMsg, wParam, lParam );
}
            



//-----------------------------------------------------------------------------
// Name: RegisterDialogClasses()
// Desc: 
//-----------------------------------------------------------------------------
BOOL WINAPI RegisterDialogClasses( HANDLE hInstance )
{
	return TRUE;
}




//-----------------------------------------------------------------------------
// Name: ScreenSaverConfigureDialog()
// Desc: 
//-----------------------------------------------------------------------------
BOOL WINAPI ScreenSaverConfigureDialog( HWND hDlg, UINT uMsg, WPARAM wParam,
							            LPARAM lParam )
{
    if( WM_INITDIALOG == uMsg )
    {
	    // Handle the initialization message
		ReadSettings( &g_CurrentOptions );

		if( TRUE == g_CurrentOptions.bUse640x480Mode )
			SendDlgItemMessage( hDlg, IDC_640x480_MODE, BM_SETCHECK, BST_CHECKED, 0 ); 
		else
			SendDlgItemMessage( hDlg, IDC_DEFAULT_MODE, BM_SETCHECK, BST_CHECKED, 0 ); 

		if( TRUE == g_CurrentOptions.bUseHardware )
			SendDlgItemMessage( hDlg, IDC_HARDWARE, BM_SETCHECK, BST_CHECKED, 0 ); 
		else
			SendDlgItemMessage( hDlg, IDC_SOFTWARE, BM_SETCHECK, BST_CHECKED, 0 ); 

        return TRUE;
    }
    
    if( WM_COMMAND == uMsg )
    {
        if( IDOK == LOWORD(wParam) )
        {
	        // Handle the case when the user hits the OK button
			g_CurrentOptions.bUse640x480Mode = SendDlgItemMessage( hDlg, IDC_640x480_MODE, BM_GETCHECK, 0, 0 ); 
			g_CurrentOptions.bUseHardware = SendDlgItemMessage( hDlg, IDC_HARDWARE, BM_GETCHECK, 0, 0 ); 
			WriteSettings( &g_CurrentOptions );
    
			EndDialog( hDlg, IDOK );
            return TRUE;
        }
        else if( IDCANCEL == LOWORD(wParam) )
        {
	        // Handle the case when the user hits the Cancel button
            EndDialog( hDlg, IDCANCEL );
            return TRUE;
        }
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Note: From this point on, the code is DirectX specific support for the app.
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// Name: AppInitialize()
// Desc: Initializes the sample framework, then calls the app-specific function
//       to initialize device specific objects. This code is structured to
//       handled any errors that may occur duing initialization
//-----------------------------------------------------------------------------
HRESULT AppInitialize( HWND hWnd, GUID guidDevice, DDSURFACEDESC2* pddsd )
{
    HRESULT hr;

	// Setup framework flags
    DWORD dwFlags = 0L;
	if( TRUE == g_bAppUseZBuffer )
		dwFlags |= D3DFW_ZBUFFER;

	// IMPORTANT: we need to determine whether the hWnd we got is the
	// child preview window or the real fullscreen screensaver window.
	if( 0 == ( GetWindowLong( hWnd, GWL_STYLE ) & WS_CHILD ) )
	    dwFlags |= D3DFW_FULLSCREEN;
		
    // Initialize the D3D framework
    if( SUCCEEDED( hr = g_pFramework->Initialize( hWnd, NULL, &guidDevice,
		                                          pddsd, dwFlags ) ) )
    {
        // Let the app run its startup code which creates the 3d scene.
        if( SUCCEEDED( hr = App_InitDeviceObjects( hWnd, 
			                                 g_pFramework->GetD3DDevice() ) ) )
		{
            return S_OK;
		}
        else
        {
            App_DeleteDeviceObjects( hWnd, g_pFramework->GetD3DDevice() );
            g_pFramework->DestroyObjects();
        }
    }

	return hr;
}




//-----------------------------------------------------------------------------
// Name: Initialize3DEnvironment()
// Desc: Called when the app window is initially created, this triggers
//       creation of the remaining portion (the 3D stuff) of the app.
//-----------------------------------------------------------------------------
HRESULT Initialize3DEnvironment( HWND hWnd )
{
    HRESULT        hr;
	DDSURFACEDESC2 ddsd;
	ZeroMemory( &ddsd, sizeof(DDSURFACEDESC2) );

    // Initialize the app
    if( FAILED( hr = App_OneTimeSceneInit() ) )
        return E_FAIL;

    // Create a new CD3DFramework class. This class does all of our D3D
    // initialization and manages the common D3D objects.
    if( NULL == ( g_pFramework = new CD3DFramework7() ) )
        return E_OUTOFMEMORY;

	if( g_CurrentOptions.bUse640x480Mode )
	{
		ddsd.dwWidth  = 640;
		ddsd.dwHeight = 480;
		ddsd.ddpfPixelFormat.dwRGBBitCount = 16;
	}
	else
	{
		DEVMODE dm;
		dm.dmSize = sizeof(DEVMODE);
		EnumDisplaySettings( NULL, ENUM_CURRENT_SETTINGS, &dm );
		ddsd.dwWidth  = dm.dmPelsWidth;
		ddsd.dwHeight = dm.dmPelsHeight;
		ddsd.ddpfPixelFormat.dwRGBBitCount = dm.dmBitsPerPel;
	}

    // Initialize the framework and scene. Try hardware first
	if( TRUE == g_CurrentOptions.bUseHardware )
	{
		if( SUCCEEDED( AppInitialize( hWnd, IID_IDirect3DHALDevice, &ddsd ) ) )
			return S_OK;

		// If that failed, try forcing a 16-bit depth
		ddsd.ddpfPixelFormat.dwRGBBitCount = 16;
		if( SUCCEEDED( AppInitialize( hWnd, IID_IDirect3DHALDevice, &ddsd ) ) )
			return S_OK;

		// If that failed too, try 640x480 mode
		ddsd.dwWidth  = 640;
		ddsd.dwHeight = 480;
		if( SUCCEEDED( AppInitialize( hWnd, IID_IDirect3DHALDevice, &ddsd ) ) )
			return S_OK;
	}

	// Resort to a software rasterizer
	if( SUCCEEDED( AppInitialize( hWnd, IID_IDirect3DRGBDevice, &ddsd ) ) )
		return S_OK;

	// All attempts failed, so return an error
	return E_FAIL;
}

    


//-----------------------------------------------------------------------------
// Name: Render3DEnvironment()
// Desc: Draws the scene
//-----------------------------------------------------------------------------
HRESULT Render3DEnvironment()
{
    // Check the cooperative level before rendering
    if( FAILED( g_pFramework->GetDirectDraw()->TestCooperativeLevel() ) )
        return S_OK;

    // Get the relative time, in seconds
    FLOAT fTime = timeGetTime() * 0.001f;

    // FrameMove (animate) the scene
    if( FAILED( App_FrameMove( g_pFramework->GetD3DDevice(), fTime ) ) )
		return E_FAIL;

    //Render the scene
    if( FAILED( App_Render( g_pFramework->GetD3DDevice() ) ) )
        return E_FAIL;
    
    // Show the frame on the primary surface.
    if( DDERR_SURFACELOST == g_pFramework->ShowFrame() )
    {
        g_pFramework->RestoreSurfaces();
        App_RestoreSurfaces();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: Cleanup3DEnvironment()
// Desc: Cleanup scene objects
//-----------------------------------------------------------------------------
VOID Cleanup3DEnvironment( HWND hWnd )
{
    if( g_pFramework )
    {
		App_DeleteDeviceObjects( hWnd, g_pFramework->GetD3DDevice() );
        App_FinalCleanup();

        SAFE_DELETE( g_pFramework );
    }

    g_bReady  = FALSE;
}


  

//-----------------------------------------------------------------------------
// Name: AppOutputText()
// Desc: Draws text on the window.
//-----------------------------------------------------------------------------
VOID AppOutputText( LPDIRECT3DDEVICE3 pd3dDevice, DWORD x, DWORD y, CHAR* str )
{
    LPDIRECTDRAWSURFACE4 pddsRenderSurface;
    if( FAILED( pd3dDevice->GetRenderTarget( &pddsRenderSurface ) ) )
        return;

    // Get a DC for the surface. Then, write out the buffer
    HDC hDC;
    if( SUCCEEDED( pddsRenderSurface->GetDC(&hDC) ) )
    {
        SetTextColor( hDC, RGB(255,255,0) );
        SetBkMode( hDC, TRANSPARENT );
        ExtTextOut( hDC, x, y, 0, NULL, str, strlen(str), NULL );
    
        pddsRenderSurface->ReleaseDC(hDC);
    }
    pddsRenderSurface->Release();
}




