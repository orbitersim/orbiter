//-----------------------------------------------------------------------------
// File: gfx.cpp
//
// Desc: Graphics routines
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include "duel.h"
#include "gfx.h"
#include "diutil.h"
#include "ddutil.h"


//-----------------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------------
extern BOOL             g_bFullscreen;   // Window or fullscreen mode ?
extern HWND             g_hwndMain;      // Main window
extern DWORD            g_dwKeys;               
extern int              g_nProgramState; // Current state of program
extern RECT             g_rcWindow;      // Client window rectangle
extern HINSTANCE        g_hInst;         // Application instance handle


LPDIRECTDRAW            g_pDD             = NULL; // DirectDraw interface
LPDIRECTDRAWPALETTE     g_pArtPalette     = NULL; // Game screen palette
LPDIRECTDRAWPALETTE     g_pSplashPalette  = NULL; // Splash screen palette
LPDIRECTDRAWSURFACE     g_pddsFrontBuffer = NULL; // primary surface
LPDIRECTDRAWSURFACE     g_pddsBackBuffer  = NULL; // back buffer for animation
LPDIRECTDRAWSURFACE     g_pddsShip[4];            // ship bitmaps
LPDIRECTDRAWSURFACE     g_pddsNumbers;            // Numbers bitmap
DWORD                   g_dwFillColor;




//-----------------------------------------------------------------------------
// Name: InitGraphics()
// Desc:
//-----------------------------------------------------------------------------
HRESULT InitGraphics()
{
    DDCAPS          ddcaps;
    HRESULT         hr;
    DDSURFACEDESC   ddsd;
    DDSCAPS         ddscaps;

    // Create a window
    g_hwndMain = CreateWindowEx( WS_EX_APPWINDOW, TEXT("DuelClass"),
                                 TEXT("Duel"), WS_POPUP | WS_SYSMENU, 0, 0, 
                                 GetSystemMetrics(SM_CXSCREEN),
                                 GetSystemMetrics(SM_CYSCREEN),
                                 NULL, NULL, g_hInst, NULL );
    if( NULL == g_hwndMain )
        return E_FAIL;

    UpdateWindow( g_hwndMain );
    SetFocus( g_hwndMain );

    // DDraw stuff begins here
    if( FAILED( hr = DirectDrawCreate( NULL, &g_pDD, NULL ) ) )
    {
        ShowError(IDS_DDRAW_ERROR_DDC);
        return E_FAIL;
    }

    // Set access mode based on fullscreen/window
    if( g_bFullscreen ) 
    {
        hr = g_pDD->SetCooperativeLevel( g_hwndMain,
                            DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN );
    }
    else
    {
        hr = g_pDD->SetCooperativeLevel( g_hwndMain,
                            DDSCL_NORMAL);
    }

    if( FAILED(hr) )
    {
        ShowError(IDS_DDRAW_ERROR_SCL);
        return E_FAIL;
    }

    if( g_bFullscreen )
    {
        // Set the mode to 640 by 480 by 8
        if( FAILED( g_pDD->SetDisplayMode( 640, 480, 8 ) ) )
        {
            ShowError(IDS_DDRAW_ERROR_SDM);
            return E_FAIL;
        }
    }
    else
    {
        RECT  rcWork;
        RECT  rc;
        DWORD dwStyle;

        // If we are still a WS_POPUP window we should convert to a
        // normal app window so we look like a windows app.
        dwStyle  = GetWindowStyle(g_hwndMain);
        dwStyle &= ~WS_POPUP;
        dwStyle |= WS_OVERLAPPED | WS_CAPTION | WS_THICKFRAME | WS_MINIMIZEBOX;
        SetWindowLong( g_hwndMain, GWL_STYLE, dwStyle );

        // Aet window size
        SetRect( &rc, 0, 0, MAX_DEFWIN_X, MAX_DEFWIN_Y );

        AdjustWindowRectEx( &rc, GetWindowStyle(g_hwndMain),
                            GetMenu(g_hwndMain) != NULL,
                            GetWindowExStyle(g_hwndMain) );

        SetWindowPos( g_hwndMain, NULL, 0, 0, rc.right-rc.left,
                      rc.bottom-rc.top,
                      SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);

        SetWindowPos( g_hwndMain, HWND_NOTOPMOST, 0, 0, 0, 0,
                      SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

        //  Make sure our window does not hang outside of the work area
        SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWork, 0 );
        GetWindowRect( g_hwndMain, &rc );
        if( rc.left < rcWork.left ) rc.left = rcWork.left;
        if( rc.top  < rcWork.top )  rc.top  = rcWork.top;
        SetWindowPos( g_hwndMain, NULL, rc.left, rc.top, 0, 0,
                      SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE );
    }

    // Check the color key hardware capabilities
    ddcaps.dwSize = sizeof( ddcaps );
    memset( &ddsd, 0, sizeof( ddsd ) );
    ddsd.dwSize = sizeof( ddsd );

    if( g_bFullscreen )
    {
        // Create surfaces
        ddsd.dwFlags = DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
        ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE |
                              DDSCAPS_FLIP |
                              DDSCAPS_COMPLEX;
        ddsd.dwBackBufferCount = 1;
        if( FAILED( hr = g_pDD->CreateSurface( &ddsd, &g_pddsFrontBuffer,
                                               NULL ) ) )
        {
            ShowError(IDS_DDRAW_ERROR_CREATESURFACE);
            return E_FAIL;
        }

        // Get a pointer to the back buffer
        ddscaps.dwCaps = DDSCAPS_BACKBUFFER;
        if( FAILED( hr = g_pddsFrontBuffer->GetAttachedSurface( &ddscaps,
                                                         &g_pddsBackBuffer ) ) )
        {
            ShowError(IDS_DDRAW_ERROR_GAS);
            return E_FAIL;
        }
    }
    else
    {
        LPDIRECTDRAWCLIPPER pcClipper;
        
        // Window case, create the primary surface
        // and create a backbuffer in offscreen memory
        ddsd.dwFlags = DDSD_CAPS;
        ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE;

        if( FAILED( g_pDD->CreateSurface( &ddsd, &g_pddsFrontBuffer, NULL ) ) )
        {
            ShowError(IDS_DDRAW_ERROR_CREATESURFACE);
            return E_FAIL;
        }

        ddsd.dwFlags = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT;    
        ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
        ddsd.dwWidth = MAX_DEFWIN_X;
        ddsd.dwHeight = MAX_DEFWIN_Y;
        if( FAILED( hr = g_pDD->CreateSurface( &ddsd, &g_pddsBackBuffer, NULL ) ) )
        {
            ShowError(IDS_DDRAW_ERROR_CREATESURFACE);
            return E_FAIL;
        }

        if( FAILED( hr = g_pDD->CreateClipper( 0, &pcClipper, NULL) ) )
        {
            ShowError(IDS_DDRAW_ERROR_CC);
            return E_FAIL;
        }

        if( FAILED( hr = pcClipper->SetHWnd( 0, g_hwndMain) ) )
        {
            pcClipper->Release();
            ShowError(IDS_DDRAW_ERROR_SH);
            return E_FAIL;
        }

        if( FAILED( hr = g_pddsFrontBuffer->SetClipper( pcClipper) ) )
        {
            pcClipper->Release();
            ShowError(IDS_DDRAW_ERROR_SC);
            return E_FAIL;
        }

        // Done with clipper
        pcClipper->Release();
    }

    ddsd.dwFlags        = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT; 
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;

    ddsd.dwWidth  = 320;
    ddsd.dwHeight = 128;
    
    for( DWORD i=0; i<4; i++ )
    {
        if( FAILED( hr = g_pDD->CreateSurface( &ddsd, &g_pddsShip[i], NULL ) ) )
        {
            ShowError(IDS_DDRAW_ERROR_CREATESURFACE);
            return E_FAIL;
        }   
    }

    ddsd.dwHeight = 16;
    if( FAILED( hr = g_pDD->CreateSurface( &ddsd, &g_pddsNumbers, NULL ) ) )
    {
        ShowError(IDS_DDRAW_ERROR_CREATESURFACE);
        return E_FAIL;
    }

    if( FAILED( RestoreSurfaces() ) )
    {
        ShowError(IDS_DDRAW_ERROR_RS);
        return E_FAIL;
    }
    
    g_dwKeys = 0;
    ShowWindow( g_hwndMain, SW_SHOW);
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CleanupGraphics()
// Desc:
//-----------------------------------------------------------------------------
VOID CleanupGraphics()
{
    for( DWORD i=0; i<4; i++ )
        if( g_pddsShip[i] )
            g_pddsShip[i]->Release();
    if( g_pddsNumbers )
        g_pddsNumbers->Release();
    if( g_pddsFrontBuffer )
        g_pddsFrontBuffer->Release();
    if( g_pArtPalette )
        g_pArtPalette->Release();
    if( g_pSplashPalette )
        g_pSplashPalette->Release();
    if( !g_bFullscreen && g_pddsBackBuffer )
        g_pddsBackBuffer->Release();
    if( g_pDD )
        g_pDD->Release();
}




//-----------------------------------------------------------------------------
// Name: BltSplashScreen()
// Desc:
//-----------------------------------------------------------------------------
HRESULT BltSplashScreen( RECT* prc )
{
    HRESULT hr;
    HBITMAP hbm;

    if( ( g_pddsFrontBuffer == NULL ) || ( g_pSplashPalette == NULL ) ||
        ( g_pddsBackBuffer == NULL ) )
        return E_FAIL;

    // Set the palette before loading the splash screen
    g_pddsFrontBuffer->SetPalette( g_pSplashPalette );

    hbm = (HBITMAP)LoadImage( GetModuleHandle( NULL ), TEXT("SPLASH"),
                              IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
    if( NULL == hbm )
        return E_FAIL;
    
    // If the surface is lost, DDUtil_CopyBitmap will fail and the surface will
    // be restored below.
    hr = DDUtil_CopyBitmap( g_pddsBackBuffer, hbm, 0, 0, 0, 0 );

    DeleteObject( hbm );

    while( 1 )
    {
        hr = g_pddsFrontBuffer->Blt( &g_rcWindow, g_pddsBackBuffer,
                                     prc, DDBLT_WAIT, NULL);
        if( SUCCEEDED(hr) )
            return S_OK;
        if( hr == DDERR_SURFACELOST )
            if( FAILED( RestoreSurfaces() ) )
                return E_FAIL;
        if( hr != DDERR_WASSTILLDRAWING )
            return E_FAIL;
    }
}




//-----------------------------------------------------------------------------
// Name: BltNumber()
// Desc:
//-----------------------------------------------------------------------------
HRESULT BltNumber( CHAR* strScore, int x, int y )
{
    while( *strScore )
    {
        RECT src;
        src.left   = ((*strScore++)-'0')*16;
        src.top    = 0;
        src.right  = src.left + 16;
        src.bottom = src.top + 16;

        BltObject( x, y, g_pddsNumbers, &src, DDBLTFAST_SRCCOLORKEY );
        x += 16;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: BltObject()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT BltObject( int x, int y, LPDIRECTDRAWSURFACE pdds, RECT* prc,
                   DWORD flags )
{
	if( NULL == pdds )
		return E_FAIL;
    
	while( 1 )
    {
        HRESULT hr = g_pddsBackBuffer->BltFast( x, y, pdds, prc, flags );
        if( FAILED(hr) )
        {
            if( hr == DDERR_WASSTILLDRAWING )
                continue;

            if( hr == DDERR_SURFACELOST )
                if( SUCCEEDED( RestoreSurfaces() ) )
                    continue;
            return E_FAIL;
        }

        return S_OK;
    }
}




//-----------------------------------------------------------------------------
// Name: EraseScreen()
// Desc: 
//-----------------------------------------------------------------------------
VOID EraseScreen()
{
    // Erase the background
    DDBLTFX ddbltfx;
    ZeroMemory( &ddbltfx, sizeof(ddbltfx) );
    ddbltfx.dwSize = sizeof(ddbltfx);
#ifdef NONAMELESSUNION
    ddbltfx.u5.dwFillColor = g_dwFillColor;
#else
    ddbltfx.dwFillColor = g_dwFillColor;
#endif

    while( 1 )
    {
        HRESULT hr = g_pddsBackBuffer->Blt( NULL, NULL, NULL,
                                            DDBLT_COLORFILL, &ddbltfx );
        if( SUCCEEDED(hr) )
            return;
        
        if( hr == DDERR_SURFACELOST )
        {
            if( FAILED( RestoreSurfaces() ) )
                return;
        }

        if( hr != DDERR_WASSTILLDRAWING )
            return;
    }
}




//-----------------------------------------------------------------------------
// Name: FlipScreen()
// Desc: 
//-----------------------------------------------------------------------------
VOID FlipScreen()
{
    // Flip the surfaces
    if( g_bFullscreen )
    {
        while( 1 )
        {
            HRESULT hr = g_pddsFrontBuffer->Flip( NULL, 0 );

            if( hr == DDERR_SURFACELOST )
            {
                if( FAILED( RestoreSurfaces() ) )
                    return;
            }
            if( hr != DDERR_WASSTILLDRAWING )
                    return;
        }
    }
    else
    {
        g_pddsFrontBuffer->Blt( &g_rcWindow, g_pddsBackBuffer, NULL,
                                DDBLT_WAIT, NULL );
    }
}





//-----------------------------------------------------------------------------
// Name: RestoreSurfaces()
// Desc:
//-----------------------------------------------------------------------------
HRESULT RestoreSurfaces()
{
    HRESULT hr;
    HBITMAP hbm;

    if( FAILED( hr = g_pddsFrontBuffer->Restore() ) )
        return hr;
    if( FAILED( hr = g_pddsBackBuffer->Restore() ) )
        return hr;

    for( DWORD i=0; i<4; i++ )
        if( FAILED( hr = g_pddsShip[i]->Restore() ) )
            return hr;

    // Create and set the palette for the splash bitmap
    g_pSplashPalette = DDUtil_LoadPalette( g_pDD, TEXT("SPLASH") );
    if( NULL == g_pSplashPalette )
        return E_FAIL;

    // Create and set the palette for the art bitmap
    g_pArtPalette = DDUtil_LoadPalette( g_pDD, TEXT("Duel8") );
    if( NULL == g_pArtPalette )
        return E_FAIL;

    // set the palette before loading the art
    g_pddsFrontBuffer->SetPalette( g_pArtPalette );

    hbm = (HBITMAP)LoadImage( GetModuleHandle(NULL), TEXT("Duel8"),
                              IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
    if( NULL == hbm )
        return E_FAIL;

    if( FAILED( hr = DDUtil_CopyBitmap( g_pddsShip[0], hbm, 0, 0, 320, 128 ) ) )
    {
        DeleteObject( hbm );
        return E_FAIL;
    }

    if( FAILED( hr = DDUtil_CopyBitmap( g_pddsShip[1], hbm, 0, 128, 320, 128 ) ) )
    {
        DeleteObject( hbm );
        return E_FAIL;
    }

    if( FAILED( hr = DDUtil_CopyBitmap( g_pddsShip[2], hbm, 0, 256, 320, 128 ) ) )
    {
        DeleteObject( hbm );
        return E_FAIL;
    }

    if( FAILED( hr = DDUtil_CopyBitmap( g_pddsShip[3], hbm, 0, 384, 320, 128 ) ) )
    {
        DeleteObject( hbm );
        return E_FAIL;
    }

    if( FAILED( hr = DDUtil_CopyBitmap( g_pddsNumbers, hbm, 0, 512, 320, 16 ) ) )
    {
        DeleteObject( hbm );
        return E_FAIL;
    }

    DeleteObject( hbm );

    // set colorfill colors and colorkeys according to bitmap contents
    g_dwFillColor = DDUtil_ColorMatch( g_pddsShip[0], CLR_INVALID );
    
    DDUtil_SetColorKey( g_pddsShip[0], CLR_INVALID );
    DDUtil_SetColorKey( g_pddsShip[1], CLR_INVALID );
    DDUtil_SetColorKey( g_pddsShip[2], CLR_INVALID );
    DDUtil_SetColorKey( g_pddsShip[3], CLR_INVALID );
    DDUtil_SetColorKey( g_pddsNumbers, CLR_INVALID );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetGamePalette()
// Desc:
//-----------------------------------------------------------------------------
VOID SetGamePalette()
{
    if( g_pddsFrontBuffer )
        g_pddsFrontBuffer->SetPalette( g_pArtPalette );
}




