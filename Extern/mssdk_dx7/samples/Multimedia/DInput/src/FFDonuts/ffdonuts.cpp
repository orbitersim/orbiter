//-----------------------------------------------------------------------------
// File: ffdonuts.cpp
//
// Desc: DirectInput ForceFeedback version of Donuts game
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define INITGUID
#include <windows.h>
#include <cguid.h>
#include <mmsystem.h>
#include <ddraw.h>
#include <dsound.h>
#include <stdio.h>
#include "DDUtil.h"
#include "DIUtil.h"
#include "DSUtil.h"
#include "resource.h"
#include "ffdonuts.h"


// DirectDraw objects
LPDIRECTDRAW4        g_pDD             = NULL;
LPDIRECTDRAWPALETTE  g_pArtPalette     = NULL;
LPDIRECTDRAWPALETTE  g_pSplashPalette  = NULL;
LPDIRECTDRAWSURFACE4 g_pddsFrontBuffer = NULL;
LPDIRECTDRAWSURFACE4 g_pddsBackBuffer  = NULL;
LPDIRECTDRAWSURFACE4 g_pddsDonut       = NULL;
LPDIRECTDRAWSURFACE4 g_pddsPyramid     = NULL;
LPDIRECTDRAWSURFACE4 g_pddsCube        = NULL;
LPDIRECTDRAWSURFACE4 g_pddsSphere      = NULL;
LPDIRECTDRAWSURFACE4 g_pddsShip        = NULL;
LPDIRECTDRAWSURFACE4 g_pddsNumbers     = NULL;

// DirectSound and DSUtil sound objects
LPDIRECTSOUND g_pDS                  = NULL;
SoundObject*  g_pBeginLevelSound     = NULL;
SoundObject*  g_pEngineIdleSound     = NULL;
SoundObject*  g_pEngineRevSound      = NULL;
SoundObject*  g_pSkidToStopSound     = NULL;
SoundObject*  g_pShieldBuzzSound     = NULL;
SoundObject*  g_pShipExplodeSound    = NULL;
SoundObject*  g_pFireBulletSound     = NULL;
SoundObject*  g_pShipBounceSound     = NULL;
SoundObject*  g_pDonutExplodeSound   = NULL;
SoundObject*  g_pPyramidExplodeSound = NULL;
SoundObject*  g_pCubeExplodeSound    = NULL;
SoundObject*  g_pSphereExplodeSound  = NULL;

// DirectInput objects
LPDIRECTINPUTDEVICE2 g_pdidJoystick        = NULL;
FFEffectObject*      g_pBounceFFEffect     = NULL;
FFEffectObject*      g_pExplodeFFEffect    = NULL;
FFEffectObject*      g_pFireFFEffect       = NULL;
DWORD                g_dwForceFeedbackGain = 75L;

BOOL    g_bEngineIdle = FALSE;
BOOL    g_bShieldsOn  = FALSE;
BOOL    g_bThrusting  = FALSE;

DWORD   lastInput        = 0;
BOOL    lastThrust       = FALSE;
BOOL    g_bLastShield    = FALSE;
FLOAT   g_fShowDelay     = 0.0f;
HWND    g_hWndMain;
BOOL    g_bIsActive;
BOOL    g_bIsReady       = FALSE;
BOOL    g_bMouseVisible;
DWORD   g_dwLastTickCount;
DWORD   g_dwScore        = 0;
int     g_ProgramState;
DWORD   g_dwLevel        = 0;
DWORD   g_dwFillColor;
BOOL    g_bLeaveTrails   = FALSE;
DWORD   g_dwScreenWidth  = 640;
DWORD   g_dwScreenHeight = 480;
DWORD   g_dwScreenDepth  = 8;
BOOL    g_bSoundEnabled  = FALSE;

DisplayObject* g_pDisplayList = NULL; // Display List
DisplayObject* g_pShip        = NULL; // Object for player's ship



BOOL CALLBACK ForceLevelDlgProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                 LPARAM lParam );
BOOL HasForceFeedbackJoystick( HINSTANCE hInst );



//-----------------------------------------------------------------------------
// Name: SetupGame()
// Desc:
//-----------------------------------------------------------------------------
VOID SetupGame()
{
    AdvanceLevel();
    
    // Set the palette
    g_pddsFrontBuffer->SetPalette( g_pArtPalette );
}




//-----------------------------------------------------------------------------
// Name: MainWndproc()
// Desc: Callback for all Windows messages
//-----------------------------------------------------------------------------
long FAR PASCAL MainWndproc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    PAINTSTRUCT ps;
    HDC         hdc;

    switch( msg )
    {
        case WM_ACTIVATEAPP:
            g_bIsActive = (BOOL) wParam;
            if( g_bIsActive )
            {
                g_bMouseVisible   = FALSE;
                g_dwLastTickCount = GetTickCount();
                g_bLeaveTrails    = FALSE;

                // Need to reacquire the devices
                if( g_pdidJoystick )
                    g_pdidJoystick->Acquire();
            }
            else
            {
                g_bMouseVisible = TRUE;
            }
            break;

        case WM_CREATE:
            break;

        case WM_SETCURSOR:
            if( !g_bMouseVisible && g_bIsReady )
                SetCursor(NULL);
            else
                SetCursor(LoadCursor( NULL, IDC_ARROW ));
            return TRUE;

        case WM_KEYDOWN:
            switch( wParam )
            {
                case VK_F3:
                    if( g_bSoundEnabled )
                        DestroySound();
                    else
                        InitializeSound( hWnd );

                    break;

                case VK_RETURN:
                    if( g_ProgramState == PS_SPLASH )
                    {
                        g_ProgramState = PS_BEGINREST;
                        SetupGame();
                    }
                    break;

                case VK_ESCAPE:
                case VK_F12:
                    PostMessage( hWnd, WM_CLOSE, 0, 0 );
                    return 0;

                case VK_F1:
                    g_bLeaveTrails = !g_bLeaveTrails;
                    break;
            }
            break;

        case WM_KEYUP:
            switch( wParam )
            {
                case VK_NUMPAD7:
                    lastInput &= ~KEY_SHIELD;
                    break;
                case VK_NUMPAD5:
                    lastInput &= ~KEY_STOP;
                    break;
                case VK_DOWN:
                case VK_NUMPAD2:
                    lastInput &= ~KEY_DOWN;
                    break;
                case VK_LEFT:
                case VK_NUMPAD4:
                    lastInput &= ~KEY_LEFT;
                    break;
                case VK_RIGHT:
                case VK_NUMPAD6:
                    lastInput &= ~KEY_RIGHT;
                    break;
                case VK_UP:
                case VK_NUMPAD8:
                    lastInput &= ~KEY_UP;
                    break;
                case VK_NUMPAD3:
                    lastInput &= ~KEY_THROW;
                    break;
                case VK_SPACE:
                    lastInput &= ~KEY_FIRE;
                    break;
            }
            break;

        case WM_ERASEBKGND:
            return 1;

        case WM_PAINT:
            hdc = BeginPaint( hWnd, &ps );
            EndPaint( hWnd, &ps );
            return 1;

        case WM_DESTROY:
            lastInput=0;
            DestroyGame();
            PostQuitMessage( 0 );
            break;
    }

    return DefWindowProc( hWnd, msg, wParam, lParam );
}




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Application entry point
//-----------------------------------------------------------------------------
int PASCAL WinMain( HINSTANCE hInstance, HINSTANCE, LPSTR, int nCmdShow )
{
    // Confirm that a force feedback joystick is attached
    if( FALSE == HasForceFeedbackJoystick( hInstance ) )
        return 0;

    // Get the ForceFeedback gain level from the user
    DialogBox( hInstance, MAKEINTRESOURCE(IDD_FORCE), NULL,
               ForceLevelDlgProc );

    // Register the window class
    WNDCLASS wndClass = { CS_DBLCLKS, MainWndproc, 0, 0, hInstance,
                          LoadIcon( hInstance, MAKEINTRESOURCE(DONUTS_ICON) ),
                          LoadCursor( NULL, IDC_ARROW ), 
                          (HBRUSH)GetStockObject( BLACK_BRUSH ),
                          NULL, TEXT("DonutsClass") };
    RegisterClass( &wndClass );

    // Create our main window
    g_hWndMain = CreateWindowEx( 0, TEXT("DonutsClass"), TEXT("Donuts"),
                                 WS_VISIBLE|WS_POPUP|WS_CAPTION|WS_SYSMENU,
                                 0, 0, GetSystemMetrics(SM_CXSCREEN),
                                 GetSystemMetrics(SM_CYSCREEN), NULL, NULL,
                                 hInstance, NULL );
    if( NULL == g_hWndMain )
        return FALSE;
    UpdateWindow( g_hWndMain );

    g_bIsReady  = TRUE;

    if( FAILED( InitializeGame( g_hWndMain ) ) )
    {
        DestroyWindow( g_hWndMain );
        return FALSE;
    }

    while( 1 )
    {
        MSG msg;

        if( PeekMessage( &msg, NULL, 0, 0, PM_NOREMOVE ) )
        {
            if( FALSE == GetMessage( &msg, NULL, 0, 0 ) )
                return msg.wParam;

            TranslateMessage( &msg );
            DispatchMessage( &msg );
        }
        else if( g_bIsActive )
        {
            UpdateFrame();
        }
        else
        {
            WaitMessage();
        }
    }
}




//-----------------------------------------------------------------------------
// Name: DestroyGame()
// Desc:
//-----------------------------------------------------------------------------
VOID DestroyGame()
{
    DestroySound();
    DestroyInput();
}




//-----------------------------------------------------------------------------
// Name: InitializeGame()
// Desc:
//-----------------------------------------------------------------------------
HRESULT InitializeGame( HWND hWnd )
{
    LPDIRECTDRAW    pDD;
    HRESULT         hr;
    DDSURFACEDESC2  ddsd;

    // Create DirectDraw
    if( SUCCEEDED( hr = DirectDrawCreate( NULL, &pDD, NULL ) ) )
    {
        // Get DirectDraw4 interface
        hr = pDD->QueryInterface( IID_IDirectDraw4, (VOID**)&g_pDD );
        pDD->Release();
    }
    if( FAILED(hr) )
    {
        CleanupAndDisplayError("DirectDrawCreate Failed!");
        return E_FAIL;
    }

    // Set the cooperative level for fullscreen mode
    hr = g_pDD->SetCooperativeLevel( hWnd,
                            DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN );
    if( FAILED(hr) )
    {
        CleanupAndDisplayError("SetCooperativeLevel Failed");
        return E_FAIL;
    }

    // Set the display mode
    hr = g_pDD->SetDisplayMode( g_dwScreenWidth, g_dwScreenHeight,
                                g_dwScreenDepth, 0, 0 );
    if( FAILED(hr) )
    {
        CleanupAndDisplayError("SetDisplayMode Failed!");
        return E_FAIL;
    }

    // Create surfaces
    ZeroMemory( &ddsd, sizeof( ddsd ) );
    ddsd.dwSize         = sizeof( ddsd );
    ddsd.dwFlags        = DDSD_CAPS | DDSD_BACKBUFFERCOUNT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE | DDSCAPS_FLIP |
                          DDSCAPS_COMPLEX;
    ddsd.dwBackBufferCount = 1;

    // Create the front buffer
    hr = g_pDD->CreateSurface( &ddsd, &g_pddsFrontBuffer, NULL );
    if( FAILED(hr) )
    {
        CleanupAndDisplayError("CreateSurface FrontBuffer Failed!");
        return E_FAIL;
    }

    // Get a pointer to the back buffer
    DDSCAPS2 ddscaps;
    ddscaps.dwCaps  = DDSCAPS_BACKBUFFER;
    ddscaps.dwCaps2 = 0;
    ddscaps.dwCaps3 = 0;
    ddscaps.dwCaps4 = 0;
    hr = g_pddsFrontBuffer->GetAttachedSurface( &ddscaps, &g_pddsBackBuffer );
    if( FAILED(hr) )
    {
        CleanupAndDisplayError("GetAttachedSurface Failed!");
        return E_FAIL;
    }

    ddsd.dwFlags        = DDSD_CAPS | DDSD_WIDTH | DDSD_HEIGHT;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    ddsd.dwWidth        = 320;

    // Creaate the surfaces for the object images. (We're OR'ing the HRESULTs
    // together, so we only need to check for failure once).
    ddsd.dwHeight = 384;
    hr |= g_pDD->CreateSurface( &ddsd, &g_pddsDonut, NULL );

    ddsd.dwHeight = 128;
    hr |= g_pDD->CreateSurface( &ddsd, &g_pddsPyramid, NULL );

    ddsd.dwHeight = 32;
    hr |= g_pDD->CreateSurface( &ddsd, &g_pddsCube, NULL );

    ddsd.dwHeight = 32;
    hr |= g_pDD->CreateSurface( &ddsd, &g_pddsSphere, NULL );
    
    ddsd.dwHeight = 256;
    hr |= g_pDD->CreateSurface( &ddsd, &g_pddsShip, NULL );

    ddsd.dwHeight = 16;
    hr |= g_pDD->CreateSurface( &ddsd, &g_pddsNumbers, NULL );

    if( FAILED(hr) )
    {
        CleanupAndDisplayError("Could not create surfaces!");
        return E_FAIL;
    }

    if( FAILED( RestoreSurfaces() ) )
    {
        CleanupAndDisplayError("RestoreSurfaces Failed!");
        return E_FAIL;
    }

    // Add a ship to the displaylist
    g_pDisplayList = new DisplayObject;
    g_pDisplayList->next        = NULL;
    g_pDisplayList->prev        = NULL;
    g_pDisplayList->type        = OBJ_SHIP;
    g_pDisplayList->pddsSurface = g_pddsShip;
    g_pShip = g_pDisplayList;

    g_dwLastTickCount = GetTickCount();

    if( FAILED( InitializeInput( hWnd ) ) )
        return E_FAIL;
    if( FAILED( InitializeSound( hWnd ) ) )
        return E_FAIL;

    g_ProgramState = PS_SPLASH;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CleanupAndDisplayError()
// Desc:
//-----------------------------------------------------------------------------
VOID CleanupAndDisplayError( CHAR* strError )
{
    CHAR buffer[80];
    sprintf( buffer, "Error: %s\n", strError );
    OutputDebugString( buffer );

    // Make the cursor visible
    SetCursor( LoadCursor( NULL, IDC_ARROW ) );
    g_bMouseVisible = TRUE;

    if( g_pddsDonut )
        g_pddsDonut->Release();

    if( g_pddsPyramid )
        g_pddsPyramid->Release();

    if( g_pddsCube )
        g_pddsCube->Release();

    if( g_pddsSphere )
        g_pddsSphere->Release();

    if( g_pddsShip )
        g_pddsShip->Release();

    if( g_pddsNumbers )
        g_pddsNumbers->Release();

    if( g_pddsFrontBuffer )
        g_pddsFrontBuffer->Release();

    if( g_pArtPalette )
        g_pArtPalette->Release();

    if( g_pSplashPalette )
        g_pSplashPalette->Release();

    if( g_pDD )
        g_pDD->Release();

    MessageBox( g_hWndMain, strError, "ERROR", MB_OK );
}




//-----------------------------------------------------------------------------
// Name: DisplaySplashScreen()
// Desc:
//-----------------------------------------------------------------------------
HRESULT DisplaySplashScreen()
{
    HBITMAP hbm;
    hbm = (HBITMAP)LoadImage( GetModuleHandle( NULL ), "SPLASH", IMAGE_BITMAP,
                              0, 0, LR_CREATEDIBSECTION );
    if( NULL == hbm )
        return E_FAIL;

    // Set the palette before loading the splash screen
    g_pddsFrontBuffer->SetPalette( g_pSplashPalette );

    // If the surface is lost, DDCopyBitmap will fail and the surface will
    // be restored in FlipScreen().
    DDUtil_CopyBitmap( g_pddsBackBuffer, hbm, 0, 0, 0, 0 );

    // Done with the bitmap
    DeleteObject( hbm );

    // Show the backbuffer contents on the frontbuffer
    if( FAILED( FlipScreen() ) )
        return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: PlayPannedSound()
// Desc: Play a sound, but first set the panning according to where the
//       object is on the screen.
//-----------------------------------------------------------------------------
VOID PlayPannedSound( SoundObject* pSound, DisplayObject* pObject )
{
    if( NULL == pObject )
        return;

    DWORD dwCenter = ( pObject->rcDst.right + pObject->rcDst.left ) / 2;
    FLOAT fCenter  = ( 2.0f * ((FLOAT)dwCenter)/g_dwScreenWidth ) - 1.0f;

    DSUtil_PlayPannedSound( pSound, fCenter );
}




//-----------------------------------------------------------------------------
// Name: UpdateFrame()
// Desc:
//-----------------------------------------------------------------------------
VOID UpdateFrame()
{
    static FLOAT fLevelIntroTime;

    switch( g_ProgramState )
    {
        case PS_SPLASH:
            // Display the splash screen
            DisplaySplashScreen();
            break;

        case PS_ACTIVE:
            UpdateDisplayList();
            CheckForHits();
            DrawDisplayList();

            if( IsDisplayListEmpty() )
            {
                DSUtil_StopSound( g_pEngineIdleSound );
                DSUtil_StopSound( g_pEngineRevSound );

                g_bEngineIdle  = FALSE;
                g_bThrusting   = FALSE;
                lastThrust     = FALSE;
                g_bLastShield  = FALSE;
                g_ProgramState = PS_BEGINREST;
                AdvanceLevel();
            }
            break;

        case PS_BEGINREST:
            DSUtil_PlaySound(g_pBeginLevelSound, 0);
            fLevelIntroTime = GetTickCount()/1000.0f;
            g_ProgramState  = PS_REST;

            // Fall through to PS_REST state

        case PS_REST:
            // Show the Level intro screen for 3 seconds
            if( ( GetTickCount()/1000.0f - fLevelIntroTime ) > 3.0f )
            {
                DSUtil_PlaySound( g_pEngineIdleSound, DSBPLAY_LOOPING );
                g_bEngineIdle     = TRUE;
                g_dwLastTickCount = GetTickCount();
                g_ProgramState    = PS_ACTIVE;
            }
            else
            {
                DisplayLevelIntroScreen( g_dwLevel );
            }
            break;
    }
}




//-----------------------------------------------------------------------------
// Name: DisplayLevelIntroScreen()
// Desc:
//-----------------------------------------------------------------------------
VOID DisplayLevelIntroScreen( DWORD dwLevel )
{
    // Erase the screen
    EraseScreen();

    // Output the bitmapped letters for the word "Level"
    CHAR buffer[10];
    buffer[0] = 10 + '0';
    buffer[1] = 11 + '0';
    buffer[2] = 12 + '0';
    buffer[3] = 11 + '0';
    buffer[4] = 10 + '0';
    buffer[5] = '\0';
    BltBitmappedText( buffer, g_dwScreenWidth/2-64, g_dwScreenHeight/2-8 );

    // Output the bitmapped numbers for the level number
    buffer[0] = (CHAR)(dwLevel/100) + '0';
    buffer[1] = (CHAR)(dwLevel/ 10) + '0';
    buffer[2] = (CHAR)(dwLevel% 10) + '0';
    buffer[3] = '\0';
    BltBitmappedText( buffer, g_dwScreenWidth/2+22, g_dwScreenHeight/2-8 );

    // Show the backbuffer screen on the front buffer
    FlipScreen();
}




//-----------------------------------------------------------------------------
// Name: BltBitmappedText()
// Desc:
//-----------------------------------------------------------------------------
VOID BltBitmappedText( CHAR* num, int x, int y )
{
    for( CHAR* c = num; *c != '\0'; c++ )
    {
        HRESULT hr = DDERR_WASSTILLDRAWING;
        RECT    rcSrc;
    
        rcSrc.left   = (*c - '0')*16;
        rcSrc.top    = 0;
        rcSrc.right  = rcSrc.left + 16;
        rcSrc.bottom = rcSrc.top + 16;

        while( hr == DDERR_WASSTILLDRAWING )
        {
            hr = g_pddsBackBuffer->BltFast( x, y, g_pddsNumbers, &rcSrc,
                                            DDBLTFAST_SRCCOLORKEY );
        }

        x += 16;
    }
}




//-----------------------------------------------------------------------------
// Name: CheckForHits()
// Desc:
//-----------------------------------------------------------------------------
VOID CheckForHits()
{
    DisplayObject* pObject;
    DisplayObject* bullet;

    // Update screen rects
    for( pObject = g_pDisplayList; pObject; pObject = pObject->next )
    {
        int frame = (DWORD)pObject->frame;
        
        switch( pObject->type )
        {
            case OBJ_DONUT:
                pObject->rcDst.left   = (DWORD)pObject->posx;
                pObject->rcDst.top    = (DWORD)pObject->posy;
                pObject->rcDst.right  = pObject->rcDst.left + DONUT_WIDTH;
                pObject->rcDst.bottom = pObject->rcDst.top + DONUT_HEIGHT;
                pObject->rcSrc.left   = DONUT_WIDTH * (frame % 5);
                pObject->rcSrc.top    = DONUT_HEIGHT * (frame /5);
                pObject->rcSrc.right  = pObject->rcSrc.left + DONUT_WIDTH;
                pObject->rcSrc.bottom = pObject->rcSrc.top + DONUT_HEIGHT;
                break;

            case OBJ_PYRAMID:
                pObject->rcDst.left   = (DWORD)pObject->posx;
                pObject->rcDst.top    = (DWORD)pObject->posy;
                pObject->rcDst.right  = pObject->rcDst.left + PYRAMID_WIDTH;
                pObject->rcDst.bottom = pObject->rcDst.top + PYRAMID_HEIGHT;
                pObject->rcSrc.left   = PYRAMID_WIDTH * (frame % 10);
                pObject->rcSrc.top    = PYRAMID_HEIGHT * (frame /10);
                pObject->rcSrc.right  = pObject->rcSrc.left + PYRAMID_WIDTH;
                pObject->rcSrc.bottom = pObject->rcSrc.top + PYRAMID_HEIGHT;
                break;

            case OBJ_SPHERE:
                pObject->rcDst.left   = (DWORD)pObject->posx;
                pObject->rcDst.top    = (DWORD)pObject->posy;
                pObject->rcDst.right  = pObject->rcDst.left + SPHERE_WIDTH;
                pObject->rcDst.bottom = pObject->rcDst.top + SPHERE_HEIGHT;
                pObject->rcSrc.left   = SPHERE_WIDTH * (frame % 20);
                pObject->rcSrc.top    = SPHERE_HEIGHT * (frame /20);
                pObject->rcSrc.right  = pObject->rcSrc.left + SPHERE_WIDTH;
                pObject->rcSrc.bottom = pObject->rcSrc.top + SPHERE_HEIGHT;
                break;
            case OBJ_CUBE:
                pObject->rcDst.left   = (DWORD)pObject->posx;
                pObject->rcDst.top    = (DWORD)pObject->posy;
                pObject->rcDst.right  = pObject->rcDst.left + CUBE_WIDTH;
                pObject->rcDst.bottom = pObject->rcDst.top + CUBE_HEIGHT;
                pObject->rcSrc.left   = CUBE_WIDTH * (frame % 20);
                pObject->rcSrc.top    = CUBE_HEIGHT * (frame /20);
                pObject->rcSrc.right  = pObject->rcSrc.left + CUBE_WIDTH;
                pObject->rcSrc.bottom = pObject->rcSrc.top + CUBE_HEIGHT;
                break;

            case OBJ_SHIP:
                pObject->rcDst.left   = (DWORD)pObject->posx;
                pObject->rcDst.top    = (DWORD)pObject->posy;
                pObject->rcDst.right  = pObject->rcDst.left + SHIP_WIDTH;
                pObject->rcDst.bottom = pObject->rcDst.top + SHIP_HEIGHT;
                if( g_bLastShield )
                    pObject->rcSrc.top = SHIP_HEIGHT * (frame / 10) + 128;
                else
                    pObject->rcSrc.top = SHIP_HEIGHT * (frame /10);
                pObject->rcSrc.left   = SHIP_WIDTH * (frame % 10);
                pObject->rcSrc.right  = pObject->rcSrc.left + SHIP_WIDTH;
                pObject->rcSrc.bottom = pObject->rcSrc.top + SHIP_HEIGHT;
                break;

            case OBJ_BULLET:
                frame = (DWORD)pObject->frame/20 % 4;
                pObject->rcDst.left   = (DWORD)pObject->posx;
                pObject->rcDst.top    = (DWORD)pObject->posy;
                pObject->rcDst.right  = pObject->rcDst.left + BULLET_WIDTH;
                pObject->rcDst.bottom = pObject->rcDst.top + BULLET_HEIGHT;
                pObject->rcSrc.left   = BULLET_XOFFSET + frame*4;
                pObject->rcSrc.top    = BULLET_YOFFSET;
                pObject->rcSrc.right  = pObject->rcSrc.left + BULLET_WIDTH;
                pObject->rcSrc.bottom = pObject->rcSrc.top + BULLET_HEIGHT;
                break;
        }
    }
    for( bullet = g_pDisplayList; bullet; bullet = bullet->next )
    {
        // Only bullet objects and the ship (if shieleds are on) can hit
        // other objects. Skip all others.
        if( (bullet->type != OBJ_BULLET) && (bullet->type != OBJ_SHIP) )
            continue;

        // The ship cannot hit anything with shields on
        if( bullet->type == OBJ_SHIP && TRUE == g_bLastShield )
            continue;

        BOOL bBulletHit = FALSE;
        WORD wBulletX   = (bullet->rcDst.left + bullet->rcDst.right) / 2;
        WORD wBulletY   = (bullet->rcDst.top + bullet->rcDst.bottom) / 2;

        for( pObject = g_pDisplayList->next; pObject; pObject = pObject->next )
        {
            FLOAT left   = (FLOAT)pObject->rcDst.left;
            FLOAT right  = (FLOAT)pObject->rcDst.right;
            FLOAT top    = (FLOAT)pObject->rcDst.top;
            FLOAT bottom = (FLOAT)pObject->rcDst.bottom;

            // Only trying to hit explodable targets
            if( ( pObject->type != OBJ_DONUT ) && 
                ( pObject->type != OBJ_PYRAMID ) &&
                ( pObject->type != OBJ_SPHERE ) &&
                ( pObject->type != OBJ_CUBE ) )
                continue;

            if( ( wBulletX >= left ) && ( wBulletX < right ) &&
                ( wBulletY >= top ) && ( wBulletY < bottom ) )
            {
                // The object was hit
                switch( pObject->type )
                {
                    case OBJ_DONUT:
                        PlayPannedSound( g_pDonutExplodeSound, pObject );
                        CreateObject( OBJ_PYRAMID, left, top, -1.0f, -1.0f );
                        CreateObject( OBJ_PYRAMID, left, top, -1.0f, -1.0f );
                        CreateObject( OBJ_PYRAMID, left, top, -1.0f, -1.0f );
                        g_dwScore += 10;
                        break;

                    case OBJ_PYRAMID:
                        PlayPannedSound( g_pPyramidExplodeSound, pObject );
                        CreateObject( OBJ_SPHERE, left, top, -1.0f, -1.0f );
                        CreateObject( OBJ_CUBE,   left, top, -1.0f, -1.0f );
                        CreateObject( OBJ_CUBE,   left, top, -1.0f, -1.0f );
                        g_dwScore += 20;
                        break;

                    case OBJ_CUBE:
                        PlayPannedSound( g_pCubeExplodeSound, pObject );
                        CreateObject( OBJ_SPHERE, left, top, -1.0f, -1.0f );
                        CreateObject( OBJ_SPHERE, left, top, -1.0f, -1.0f );
                        g_dwScore += 40;
                        break;

                    case OBJ_SPHERE:
                        PlayPannedSound( g_pSphereExplodeSound, pObject );
                        g_dwScore += 20;
                        break;
                }

                bBulletHit = TRUE;

                DisplayObject* pVictim = pObject;
                pObject = pObject->prev;
                DeleteFromList( pVictim );
            }

            if( bBulletHit )
            {
                if( bullet->type == OBJ_SHIP )
                {
                    bBulletHit = FALSE;

                    if( !g_bLastShield && g_pShip->bVisible )
                    {
                        // Ship has exploded
                        PlayPannedSound( g_pShipExplodeSound, bullet);
                        DIUtil_PlayEffect( g_pExplodeFFEffect );

                        if( g_dwScore < 150 )
                            g_dwScore = 0;
                        else
                            g_dwScore -= 150;

                        FLOAT x = g_pShip->posx;
                        FLOAT y = g_pShip->posy;

                        CreateObject( OBJ_SPHERE, x, y, -1.0f, -1.0f );
                        CreateObject( OBJ_SPHERE, x, y, -1.0f, -1.0f );
                        CreateObject( OBJ_SPHERE, x, y, -1.0f, -1.0f );
                        CreateObject( OBJ_SPHERE, x, y, -1.0f, -1.0f );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );
                        CreateObject( OBJ_BULLET, x, y, rnd()/2, rnd()/2 );

                        // Delay for 2 seconds before displaying ship
                        InitializeShip();
                        g_fShowDelay = 2.0f;
                        g_pShip->bVisible = FALSE;
                    }
                }

                break;
            }
        }

        if( bBulletHit )
        {
            DisplayObject* pLastBullet = bullet;
            bullet = bullet->prev;
            DeleteFromList( pLastBullet );
        }
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
    ddbltfx.dwSize      = sizeof(ddbltfx);
    ddbltfx.dwFillColor = g_dwFillColor;
    
    while( 1 )
    {
        HRESULT hr = g_pddsBackBuffer->Blt( NULL, NULL, NULL, DDBLT_COLORFILL,
                                            &ddbltfx );
        if( SUCCEEDED(hr) )
            break;

        if( hr == DDERR_SURFACELOST )
            if( FAILED( RestoreSurfaces() ) )
                return;

        if( hr != DDERR_WASSTILLDRAWING )
            return;
    }
}




//-----------------------------------------------------------------------------
// Name: FlipScreen()
// Desc:
//-----------------------------------------------------------------------------
HRESULT FlipScreen()
{
    HRESULT hr = DDERR_WASSTILLDRAWING;

    // Flip the surfaces
    while( DDERR_WASSTILLDRAWING == hr )
    {
        hr = g_pddsFrontBuffer->Flip( NULL, 0 );

        if( hr == DDERR_SURFACELOST )
            if( FAILED( RestoreSurfaces() ) )
                return E_FAIL;
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DrawDisplayList()
// Desc:
//-----------------------------------------------------------------------------
VOID DrawDisplayList()
{
    if( FALSE == g_bLeaveTrails )
        EraseScreen();

    char scorebuf[11];
    int  rem;

    rem = g_dwScore;
    scorebuf[0] = rem/10000000 + '0';
    rem = g_dwScore % 10000000;
    scorebuf[1] = rem/1000000 + '0';
    rem = g_dwScore % 1000000;
    scorebuf[2] = rem/100000 + '0';
    rem = g_dwScore % 100000;
    scorebuf[3] = rem/10000 + '0';
    rem = g_dwScore % 10000;
    scorebuf[4] = rem/1000 + '0';
    rem = g_dwScore % 1000;
    scorebuf[5] = rem/100 + '0';
    rem = g_dwScore % 100;
    scorebuf[6] = rem/10 + '0';
    rem = g_dwScore % 10;
    scorebuf[7] = rem + '0';
    scorebuf[8] = '\0';

    // Draw the sound icon, if sound is enabled
    if( g_bSoundEnabled )
    {
        scorebuf[8]  = 14 + '0';
        scorebuf[9]  = 13 + '0';
        scorebuf[10] = '\0';
    }

    // Display the score
    BltBitmappedText( scorebuf, 10, g_dwScreenHeight-26 );

    // Display all visible objects in the display list
    for( DisplayObject* pObject = g_pDisplayList; pObject;
         pObject = pObject->next )
    {
        if( !pObject->bVisible )
            continue;

        HRESULT hr = DDERR_WASSTILLDRAWING;

        if( pObject->rcSrc.right - pObject->rcSrc.left == 0 )
            continue; // New objects do not have a src rect yet.

        while( hr == DDERR_WASSTILLDRAWING )
        {
            hr = g_pddsBackBuffer->BltFast( pObject->rcDst.left,
                                   pObject->rcDst.top, pObject->pddsSurface,
                                   &pObject->rcSrc, DDBLTFAST_SRCCOLORKEY );

        }
    }

    // Flip the backbuffer contents to the front buffer
    FlipScreen();
}




//-----------------------------------------------------------------------------
// Name: DeleteFromList()
// Desc:
//-----------------------------------------------------------------------------
VOID DeleteFromList( DisplayObject* pObject )
{
    if( pObject->next )
        pObject->next->prev = pObject->prev;
    if( pObject->prev )
        pObject->prev->next = pObject->next;
    delete( pObject );
}




//-----------------------------------------------------------------------------
// Name: UpdateDisplayList()
// Desc:
//-----------------------------------------------------------------------------
VOID UpdateDisplayList()
{
    DisplayObject* thisnode;
    FLOAT          maxx, maxy;
    FLOAT          maxframe;
    DWORD          input = lastInput;

    BOOL bShieldState = FALSE;
    BOOL bThrustState = FALSE;
    BOOL bBounce      = FALSE;
    LONG lAngle       = 0;

    // Update the game clock parameters
    DWORD dwTickCount = GetTickCount();
    FLOAT fTickDiff   = ( dwTickCount - g_dwLastTickCount )/1000.0f;
    g_dwLastTickCount = dwTickCount;

    // Read input from the jostick/keyboard/etc
    input = GetDeviceInput();

    if( g_fShowDelay > 0.0f )
    {
        g_fShowDelay -= fTickDiff;

        if( g_fShowDelay < 0.0f )
        {
            InitializeShip();
            g_bLastShield = FALSE;
            g_fShowDelay  = 0.0f;
            g_pShip->bVisible = TRUE;
        }
    }

    // Update the ship
    if( g_pShip->bVisible )
    {
        g_pShip->posx += g_pShip->velx * fTickDiff;
        g_pShip->posy += g_pShip->vely * fTickDiff;
    }
    if( g_pShip->posx > (FLOAT)(MAX_SCREEN_X - SHIP_WIDTH) )
    {
        g_pShip->posx = (FLOAT)(MAX_SCREEN_X - SHIP_WIDTH);
        g_pShip->velx = -g_pShip->velx;

        // Bounce off the right edge of the screen
        bBounce = TRUE;
        lAngle  = 90;
    }
    else if( g_pShip->posx < 0 )
    {
        g_pShip->posx = 0;
        g_pShip->velx = -g_pShip->velx;
        
        // Bounce off the left edge of the screen
        bBounce = TRUE;
        lAngle  = 270;
    }

    if( g_pShip->posy > (FLOAT)(MAX_SCREEN_Y - SHIP_HEIGHT) )
    {
        g_pShip->posy = (FLOAT)(MAX_SCREEN_Y - SHIP_HEIGHT);
        g_pShip->vely = -g_pShip->vely;

        // Bounce off the bottom edge of the screen
        bBounce = TRUE;
        lAngle  = 180;
    }
    else if( g_pShip->posy < 0 )
    {
        g_pShip->posy =0;
        g_pShip->vely = -g_pShip->vely;

        // Bounce off the top edge of the screen
        bBounce = TRUE;
        lAngle  = 0;
    }

    if( bBounce )
    {
        // "Bounce" the ship
        PlayPannedSound( g_pShipBounceSound, g_pShip );
        DIUtil_PlayDirectionalEffect( g_pBounceFFEffect, lAngle );
    }

    bShieldState = ( !g_pShip->bVisible || ( input & KEY_SHIELD ) );
    
    if( bShieldState != g_bLastShield )
    {
        if( bShieldState && g_pShip->bVisible )
        {
            DSUtil_PlaySound( g_pShieldBuzzSound, DSBPLAY_LOOPING );
            g_bShieldsOn = TRUE;
        }
        else
        {
            DSUtil_StopSound( g_pShieldBuzzSound );
            g_bShieldsOn = FALSE;
        }

        g_bLastShield = bShieldState;
    }
    if( bShieldState )
    {
        input &= ~(KEY_FIRE);
    }

    if( input & KEY_FIRE )
    {
        // Add a bullet to the scene
        if( g_pShip->bVisible )
        {
            // Bullets cost one score point
            if( g_dwScore )
                g_dwScore--;

            // Play the "fire" sound
            DSUtil_PlaySound( g_pFireBulletSound, 0);

            // Play the "fire" forcefeedback effect
            DIUtil_PlayEffect( g_pFireFFEffect );

            // Add a bullet to the display list
            CreateObject( OBJ_BULLET,
                         Dirx[(int)g_pShip->frame]*6.0f + 16.0f + g_pShip->posx,
                         Diry[(int)g_pShip->frame]*6.0f + 16.0f + g_pShip->posy,
                         Dirx[(int)g_pShip->frame]*500.0f,
                         Diry[(int)g_pShip->frame]*500.0f );
        }
    }

    if( input & KEY_LEFT )
    {
        g_pShip->frame -= 1.0;
        if( g_pShip->frame < 0.0 )
            g_pShip->frame += MAX_SHIP_FRAME;
    }
    if( input & KEY_RIGHT )
    {
        g_pShip->frame += 1.0;
        if( g_pShip->frame >= MAX_SHIP_FRAME )
            g_pShip->frame -= MAX_SHIP_FRAME;
    }
    if( input & KEY_UP )
    {
        g_pShip->velx += Dirx[(int)g_pShip->frame] * 10.0f;
        g_pShip->vely += Diry[(int)g_pShip->frame] * 10.0f;
        bThrustState = TRUE;
    }
    if( input & KEY_DOWN )
    {
        g_pShip->velx -= Dirx[(int)g_pShip->frame] * 10.0f;
        g_pShip->vely -= Diry[(int)g_pShip->frame] * 10.0f;
        bThrustState = TRUE;
    }

    if( bThrustState != lastThrust )
    {
        if( bThrustState )
        {
            input &= ~KEY_STOP;
            DSUtil_StopSound( g_pSkidToStopSound );
            DSUtil_PlaySound( g_pEngineRevSound, DSBPLAY_LOOPING );

            g_bThrusting = TRUE;
        }
        else
        {
            DSUtil_StopSound( g_pEngineRevSound );
            g_bThrusting = FALSE;
        }

        lastThrust = bThrustState;
    }

    if( input & KEY_STOP )
    {
        if( g_pShip->velx || g_pShip->vely )
            PlayPannedSound( g_pSkidToStopSound, g_pShip );

        g_pShip->velx = 0.0f;
        g_pShip->vely = 0.0f;
    }

    for( thisnode = g_pDisplayList->next; thisnode;  )
    {
        thisnode->posx  += thisnode->velx  * fTickDiff;
        thisnode->posy  += thisnode->vely  * fTickDiff;
        thisnode->frame += thisnode->delay * fTickDiff;
        switch( thisnode->type )
        {
            case OBJ_DONUT:
                maxx     = (FLOAT)(MAX_SCREEN_X - DONUT_WIDTH);
                maxy     = (FLOAT)(MAX_SCREEN_Y - DONUT_HEIGHT);
                maxframe = MAX_DONUT_FRAME;
                break;
            case OBJ_PYRAMID:
                maxx     = (FLOAT)(MAX_SCREEN_X - PYRAMID_WIDTH);
                maxy     = (FLOAT)(MAX_SCREEN_Y - PYRAMID_HEIGHT);
                maxframe = MAX_PYRAMID_FRAME;
                break;
            case OBJ_SPHERE:
                maxx     = (FLOAT)(MAX_SCREEN_X - SPHERE_WIDTH);
                maxy     = (FLOAT)(MAX_SCREEN_Y - SPHERE_HEIGHT);
                maxframe = MAX_SPHERE_FRAME;
                break;
            case OBJ_CUBE:
                maxx     = (FLOAT)(MAX_SCREEN_X - CUBE_WIDTH);
                maxy     = (FLOAT)(MAX_SCREEN_Y - CUBE_HEIGHT);
                maxframe = MAX_CUBE_FRAME;
                break;
            case OBJ_BULLET:
                maxx     = (FLOAT)(MAX_SCREEN_X - BULLET_WIDTH);
                maxy     = (FLOAT)(MAX_SCREEN_Y - BULLET_HEIGHT);
                maxframe = MAX_BULLET_FRAME;

                // If bullet "expired", removed it from list
                if( thisnode->frame >= MAX_BULLET_FRAME )
                {
                    DisplayObject* pVictim = thisnode;
                    thisnode = thisnode->next;
                    DeleteFromList( pVictim );
                    continue;
                }
                break;
        }
        if( thisnode != g_pDisplayList )
        {
            if( thisnode->posx > maxx )
            {
                thisnode->posx = maxx;
                thisnode->velx = -thisnode->velx;
            }
            else if ( thisnode->posx < 0 )
            {
                thisnode->posx =0;
                thisnode->velx = -thisnode->velx;
            }
            if( thisnode->posy > maxy )
            {
                thisnode->posy = maxy;
                thisnode->vely = -thisnode->vely;
            }
            else if ( thisnode->posy < 0 )
            {
                thisnode->posy =0;
                thisnode->vely = -thisnode->vely;
            }
            if( thisnode->frame >= maxframe )
            {
                thisnode->frame -= maxframe;
            }
            thisnode = thisnode->next;
        }
    }
}




//-----------------------------------------------------------------------------
// Name: IsDisplayListEmpty()
// Desc:
//-----------------------------------------------------------------------------
BOOL IsDisplayListEmpty()
{
    DisplayObject* pObject = g_pDisplayList->next;

    while( pObject )
    {
        if( pObject->type != OBJ_BULLET )
            return FALSE;
        
        pObject = pObject->next;
    }
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: InitializeShip()
// Desc:
//-----------------------------------------------------------------------------
VOID InitializeShip()
{
    g_pShip->posx  = (FLOAT)(g_dwScreenWidth/2-16); // Center the ship
    g_pShip->posy  = (FLOAT)(g_dwScreenHeight/2-16);
    g_pShip->frame = 0.0f;
    g_pShip->velx  = 0.0f;                          // Not moving
    g_pShip->vely  = 0.0f;
}




//-----------------------------------------------------------------------------
// Name: AdvanceLevel()
// Desc:
//-----------------------------------------------------------------------------
VOID AdvanceLevel()
{
    // Up the level
    g_dwLevel++;

    // Clear any stray objects (anything but the ship) out of the display list
    while( g_pShip->next )
    {
        DeleteFromList( g_pShip->next );
    }

    // Create donuts for the new level
    for( WORD i=0; i<(2*g_dwLevel-1); i++ )
    {
        FLOAT x = rnd( 0.0f, (FLOAT)(MAX_SCREEN_X - DONUT_WIDTH) );
        FLOAT y = rnd( 0.0f, (FLOAT)(MAX_SCREEN_Y - DONUT_HEIGHT) );
       
        CreateObject( OBJ_DONUT, x, y, 0.0f, 0.0f );
    }

    // Delay for 2 seconds before displaying ship
    InitializeShip();
    g_pShip->bVisible = FALSE;
    g_fShowDelay = 2.0f;
}




//-----------------------------------------------------------------------------
// Name: CreateObject()
// Desc:
//-----------------------------------------------------------------------------
DisplayObject* CreateObject( SHORT type, FLOAT x, FLOAT y, FLOAT vx, FLOAT vy )
{
    // Create a new display object
    DisplayObject* pObject = new DisplayObject;
    if( NULL == pObject )
        return NULL;

    // Add the object to the global display list
    AddToList( pObject );

    // Set object attributes
    pObject->bVisible = TRUE;
    pObject->type = type;
    pObject->posx = x;
    pObject->posy = y;
    pObject->velx = vx;
    pObject->vely = vy;

    switch( type )
    {
        case OBJ_DONUT:
            pObject->velx  = rnd( -50.0f, 50.0f );
            pObject->vely  = rnd( -50.0f, 50.0f );
            pObject->frame = rnd( 0.0f, 30.0f );
            pObject->delay = rnd( 3.0f, 12.0f );
            pObject->pddsSurface = g_pddsDonut;
            break;

        case OBJ_PYRAMID:
            pObject->velx  = rnd( -75.0f, 75.0f );
            pObject->vely  = rnd( -75.0f, 75.0f );
            pObject->frame = rnd( 0.0f, 30.0f );
            pObject->delay = rnd( 12.0f, 40.0f );
            pObject->pddsSurface = g_pddsPyramid;
            break;

        case OBJ_SPHERE:
            pObject->velx  = rnd( -150.0f, 150.0f );
            pObject->vely  = rnd( -150.0f, 150.0f );
            pObject->frame = rnd( 0.0f, 30.0f );
            pObject->delay = rnd( 60.0f, 80.0f );
            pObject->pddsSurface = g_pddsSphere;
            break;

        case OBJ_CUBE:
            pObject->velx  = rnd( -200.0f, 200.0f );
            pObject->vely  = rnd( -200.0f, 200.0f );
            pObject->frame = rnd( 0.0f, 30.0f );
            pObject->delay = rnd( 32.0f, 80.0f );
            pObject->pddsSurface = g_pddsCube;
            break;

        case OBJ_BULLET:
            pObject->frame =    0.0f;
            pObject->delay = 1000.0f;
            pObject->pddsSurface = g_pddsNumbers;
            break;
    }

    return pObject;
};




//-----------------------------------------------------------------------------
// Name: AddToList()
// Desc:
//-----------------------------------------------------------------------------
VOID AddToList( DisplayObject* pObject )
{
    pObject->next = g_pDisplayList->next;
    pObject->prev = g_pDisplayList;
    
    if( g_pDisplayList->next )
        g_pDisplayList->next->prev = pObject;
    g_pDisplayList->next = pObject;
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
        return E_FAIL;
    if( FAILED( hr = g_pddsDonut->Restore() ) )
        return E_FAIL;
    if( FAILED( hr = g_pddsPyramid->Restore() ) )
        return E_FAIL;
    if( FAILED( hr = g_pddsCube->Restore() ) )
        return E_FAIL;
    if( FAILED( hr = g_pddsSphere->Restore() ) )
        return E_FAIL;
    if( FAILED( hr = g_pddsShip->Restore() ) )
        return E_FAIL;
    if( FAILED( hr = g_pddsNumbers->Restore() ) )
        return E_FAIL;

    // Create and set the palette for the splash bitmap
    g_pSplashPalette = DDUtil_LoadPalette( g_pDD, "SPLASH" );

    // Create and set the palette for the art bitmap
    g_pArtPalette = DDUtil_LoadPalette( g_pDD, "DONUTS8" );

    if( NULL == g_pSplashPalette || NULL == g_pArtPalette )
    {
        CleanupAndDisplayError("Could not load palettes");
        return E_FAIL;
    }

    // Set the palette before loading the art
    g_pddsFrontBuffer->SetPalette( g_pArtPalette );

    hbm = (HBITMAP)LoadImage( GetModuleHandle(NULL), "DONUTS8", IMAGE_BITMAP, 0, 0,
                              LR_CREATEDIBSECTION );
    if( NULL == hbm )
    {
        CleanupAndDisplayError("Could not load bitmap");
        return E_FAIL;
    }

    // Copy the bitmaps. (We're OR'ing the hresults together so we only have to
    // do one check for a failure case.)
    hr |= DDUtil_CopyBitmap( g_pddsDonut,   hbm, 0,   0, 320, 384 );
    hr |= DDUtil_CopyBitmap( g_pddsPyramid, hbm, 0, 384, 320, 128 );
    hr |= DDUtil_CopyBitmap( g_pddsSphere,  hbm, 0, 512, 320,  32 );
    hr |= DDUtil_CopyBitmap( g_pddsCube,    hbm, 0, 544, 320,  32 );
    hr |= DDUtil_CopyBitmap( g_pddsShip,    hbm, 0, 576, 320, 256 );
    hr |= DDUtil_CopyBitmap( g_pddsNumbers, hbm, 0, 832, 320,  16 );

    if( FAILED(hr) )
    {
        CleanupAndDisplayError("Could not copy bitmaps to surfaces");
        DeleteObject( hbm );
        return E_FAIL;
    }

    // Done with the bitmap
    DeleteObject( hbm );

    // Set colorfill colors and color keys according to bitmap contents
    g_dwFillColor = DDUtil_ColorMatch( g_pddsDonut, CLR_INVALID );

    DDUtil_SetColorKey( g_pddsDonut,   CLR_INVALID );
    DDUtil_SetColorKey( g_pddsPyramid, CLR_INVALID );
    DDUtil_SetColorKey( g_pddsCube,    CLR_INVALID );
    DDUtil_SetColorKey( g_pddsSphere,  CLR_INVALID );
    DDUtil_SetColorKey( g_pddsShip,    CLR_INVALID );
    DDUtil_SetColorKey( g_pddsNumbers, CLR_INVALID );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: rnd()
// Desc:
//-----------------------------------------------------------------------------
FLOAT rnd( FLOAT low, FLOAT high )
{
    return low + ( high - low ) * ((FLOAT)rand())/RAND_MAX;
}




//-----------------------------------------------------------------------------
// Name: InitializeSound()
// Desc:
//-----------------------------------------------------------------------------
HRESULT InitializeSound( HWND hWnd )
{
    g_bSoundEnabled = FALSE;

    if( FAILED( DirectSoundCreate( NULL, &g_pDS, NULL ) ) )
        return E_FAIL;

    if( FAILED( g_pDS->SetCooperativeLevel( hWnd, DSSCL_NORMAL ) ) )
    {
        g_pDS->Release();
        g_pDS = NULL;

        return E_FAIL;
    }

    g_pBeginLevelSound     = DSUtil_CreateSound( g_pDS, "BeginLevel",      1 );
    g_pEngineIdleSound     = DSUtil_CreateSound( g_pDS, "EngineIdle",      1 );
    g_pEngineRevSound      = DSUtil_CreateSound( g_pDS, "EngineRev",       1 );
    g_pSkidToStopSound     = DSUtil_CreateSound( g_pDS, "SkidToStop",      1 );
    g_pShieldBuzzSound     = DSUtil_CreateSound( g_pDS, "ShieldBuzz",      1 );
    g_pShipExplodeSound    = DSUtil_CreateSound( g_pDS, "ShipExplode",     1 );
    g_pFireBulletSound     = DSUtil_CreateSound( g_pDS, "Gunfire",        25 );
    g_pShipBounceSound     = DSUtil_CreateSound( g_pDS, "ShipBounce",      4 );
    g_pDonutExplodeSound   = DSUtil_CreateSound( g_pDS, "DonutExplode",   10 );
    g_pPyramidExplodeSound = DSUtil_CreateSound( g_pDS, "PyramidExplode", 12 );
    g_pCubeExplodeSound    = DSUtil_CreateSound( g_pDS, "CubeExplode",    15 );
    g_pSphereExplodeSound  = DSUtil_CreateSound( g_pDS, "SphereExplode",  10 );

    g_bSoundEnabled = TRUE;

    // Start sounds that should be playing
    if( g_bEngineIdle ) DSUtil_PlaySound( g_pEngineIdleSound, DSBPLAY_LOOPING );
    if( g_bShieldsOn )  DSUtil_PlaySound( g_pShieldBuzzSound, DSBPLAY_LOOPING );
    if( g_bThrusting )  DSUtil_PlaySound( g_pEngineRevSound,  DSBPLAY_LOOPING );

    return S_OK;    
}




//-----------------------------------------------------------------------------
// Name: DestroySound()
// Desc:
//-----------------------------------------------------------------------------
VOID DestroySound()
{
    if( g_pDS )
    {
        DSUtil_DestroySound( g_pBeginLevelSound );
        DSUtil_DestroySound( g_pEngineIdleSound );
        DSUtil_DestroySound( g_pEngineRevSound );
        DSUtil_DestroySound( g_pSkidToStopSound );
        DSUtil_DestroySound( g_pShieldBuzzSound );
        DSUtil_DestroySound( g_pShipExplodeSound );
        DSUtil_DestroySound( g_pFireBulletSound );
        DSUtil_DestroySound( g_pShipBounceSound );
        DSUtil_DestroySound( g_pDonutExplodeSound );
        DSUtil_DestroySound( g_pPyramidExplodeSound );
        DSUtil_DestroySound( g_pCubeExplodeSound );
        DSUtil_DestroySound( g_pSphereExplodeSound );

        g_pDS->Release();
    }

    g_pBeginLevelSound     = NULL;
    g_pEngineIdleSound     = NULL;
    g_pEngineRevSound      = NULL;
    g_pSkidToStopSound     = NULL;
    g_pShieldBuzzSound     = NULL;
    g_pShipExplodeSound    = NULL;
    g_pFireBulletSound     = NULL;
    g_pShipBounceSound     = NULL;
    g_pDonutExplodeSound   = NULL;
    g_pPyramidExplodeSound = NULL;
    g_pCubeExplodeSound    = NULL;
    g_pSphereExplodeSound  = NULL;

    g_pDS           = NULL;
    g_bSoundEnabled = FALSE;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL CALLBACK EnumFFJoysticksCallback( LPCDIDEVICEINSTANCE pInst, 
                                       LPVOID pbHasFFDevice )
{

    if( FALSE == IsEqualIID( pInst->guidInstance, GUID_NULL ) )
        *((BOOL*)pbHasFFDevice) = TRUE;

    return DIENUM_CONTINUE;
}




//-----------------------------------------------------------------------------
// Name: HasForceFeedbackJoystick()
// Desc:
//-----------------------------------------------------------------------------
BOOL HasForceFeedbackJoystick( HINSTANCE hInst )
{
    LPDIRECTINPUT pDI;
    BOOL    bHasFFDevice = FALSE;
    HRESULT hr;

    // Initialize DirectInput
    if( SUCCEEDED( DirectInputCreate( hInst,
                                      DIRECTINPUT_VERSION, 
                                      &pDI, NULL ) ) )
    {
        // Enumerate all DirectInput devices
        hr = pDI->EnumDevices( DIDEVTYPE_JOYSTICK,
                               EnumFFJoysticksCallback,
                               &bHasFFDevice,
                               DIEDFL_ATTACHEDONLY | DIEDFL_FORCEFEEDBACK );

        pDI->Release();
    }

    if( FAILED(hr) )
        return FALSE;

    if( FALSE == bHasFFDevice )
        MessageBox( NULL, "This sample requires a force feedback\n"
                    "input device.\n\nThis sample will now exit.", 
                    "FFDonuts", MB_ICONERROR );

    return bHasFFDevice;
}




//-----------------------------------------------------------------------------
// Name: InitializeInput()
// Desc:
//-----------------------------------------------------------------------------
HRESULT InitializeInput( HWND hWnd )
{
    // Initialize DirectInpu
    if( FAILED( DIUtil_Initialize( hWnd ) ) )
    {
        MessageBox( hWnd, "Can't Initialize DirectInput", "FFDonuts", MB_OK );
        return E_FAIL;
    }

    // Get the list of DirectInput devices
    DIDEVICEINSTANCE* pDevices;
    DWORD             dwNumDevices;
    DIUtil_GetDevices( &pDevices, &dwNumDevices );

    // Search through the list, looking for a joystick
    for( DWORD i=0; i< dwNumDevices; i++ )
    {
        DWORD dwDevType = pDevices[i].dwDevType & 0x000000ff;

        if( dwDevType == DIDEVTYPE_JOYSTICK )
        {
            g_pdidJoystick = DIUtil_CreateDevice( hWnd, &pDevices[i] );
            break;
        }
    }

    // This app requires a joystick
    if( g_pdidJoystick == NULL )
    {
        MessageBox( hWnd, "Need a joystick", "FFDonuts", MB_OK );
        return E_FAIL;
    }

    // If the joystick is force feedback, ask user for options and setup effects
    if( DIUtil_IsForceFeedback( g_pdidJoystick ) )
    {
        // Create force feedback effects
        g_pBounceFFEffect  = DIUtil_CreateEffect();
        g_pFireFFEffect    = DIUtil_CreateEffect();
        g_pExplodeFFEffect = DIUtil_CreateEffect();

        g_pBounceFFEffect->diEffect.dwGain  = ( g_dwForceFeedbackGain * 100L );
        g_pFireFFEffect->diEffect.dwGain    = ( g_dwForceFeedbackGain * 100L );
        g_pExplodeFFEffect->diEffect.dwGain = ( g_dwForceFeedbackGain * 100L );

        DIUtil_SetupConstantForceEffect( g_pBounceFFEffect, g_pdidJoystick, TRUE );
        DIUtil_SetupConstantForceEffect( g_pFireFFEffect, g_pdidJoystick, FALSE );
        DIUtil_SetupPeriodicEffect( g_pExplodeFFEffect, g_pdidJoystick );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DestroyInput()
// Desc:
//-----------------------------------------------------------------------------
VOID DestroyInput()
{
    // Delete effect objects
    DIUtil_DeleteEffect( g_pBounceFFEffect );
    DIUtil_DeleteEffect( g_pFireFFEffect );
    DIUtil_DeleteEffect( g_pExplodeFFEffect );
    
    // Unacquire and release input devices
    if( g_pdidJoystick )
    {
        g_pdidJoystick->Unacquire();
        g_pdidJoystick->Release();
    }
    
    // Release DirectInput
    DIUtil_CleanupDirectInput();
}




//-----------------------------------------------------------------------------
// Name: DIUtil_ForceLevelDlgProc()
// Desc: Dialog proceedure for handling the Force Level dialog box.
//-----------------------------------------------------------------------------
BOOL CALLBACK ForceLevelDlgProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                 LPARAM lParam )
{
    switch( uMsg )
    {
        case WM_INITDIALOG:
            // select default / current setting
            CheckRadioButton(hWnd, IDC_CHILD, IDC_BODYBUILDER, IDC_ADULT );

            return TRUE;

        case WM_COMMAND:
            if( IDOK == LOWORD(wParam) )
            {
                if( IsDlgButtonChecked( hWnd, IDC_CHILD ) )
                    g_dwForceFeedbackGain = 50L;
                else if(IsDlgButtonChecked(hWnd, IDC_BODYBUILDER))
                    g_dwForceFeedbackGain = 100L;
                else
                    g_dwForceFeedbackGain = 75L;

                EndDialog( hWnd, 0 );
            }
            break;
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: GetDeviceInput()
// Desc: Processes data from the input device.  Uses GetDeviceState().
//-----------------------------------------------------------------------------
DWORD GetDeviceInput()
{
    HRESULT     hr;
    DIJOYSTATE  dijs;
    DWORD       dwInput = 0;

    // Poll the joystick to read the current state
    hr = g_pdidJoystick->Poll();

    // Read the device state
    hr = g_pdidJoystick->GetDeviceState( sizeof(DIJOYSTATE), &dijs );
    if( FAILED(hr) )
    {
        if( hr == DIERR_INPUTLOST )
        {
            // Reacquire the device
            g_pdidJoystick->Acquire();
        }
        // We did not read anything, return no motion
        return 0;
    }

    // Process device state
    // x-axis (left)
    if( dijs.lX < 0 )
        dwInput |= KEY_LEFT;

    // x-axis (right)
    if( dijs.lX > 0 )
        dwInput |= KEY_RIGHT;

    // y-axis (forward)
    if( dijs.lY < 0 )
        dwInput |= KEY_UP;

    // y-axis (backward)
    if( dijs.lY > 0 )
        dwInput |= KEY_DOWN;

    // button 0 (fire)
    if( dijs.rgbButtons[0] & 0x80 )
        dwInput |= KEY_FIRE;

    // button 1 (shield)
    if( dijs.rgbButtons[1] & 0x80 )
        dwInput |= KEY_SHIELD;

    // button 2 (stop) - requires a joystick with more than 2 buttons
    if( dijs.rgbButtons[2] & 0x80 )
        dwInput |= KEY_STOP;

    // return the new device state
    return dwInput;
}




