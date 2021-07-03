/*==========================================================================
 *
 *  Copyright (C) 1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File:       3dsound.cpp
 *  Content:    Windows setup and procs for DirectSound sample 
 *
 ***************************************************************************/


#include <windows.h>
#include "resource.h"
#include "sound.h"

#define NAME "3DMusic Sample"
#define WINWD 300
#define WINHT 300

HWND        g_hwnd;
HCURSOR     g_hcurCross;
HCURSOR     g_hcurSpeaker;
DWORD       g_dxCrossHot;
DWORD       g_dyCrossHot;
LONG        g_cxCross;
LONG        g_cyCross;
DWORD       g_dxSpeakerHot;
DWORD       g_dySpeakerHot;
LONG        g_cxSpeaker;
LONG        g_cySpeaker;
int         g_x = (int) (WINWD / 2);  
int         g_y = (int) (WINHT / 2);
BOOL        PlayDrip;


/* --------------------------------------------------------

    InvalidateCursorRect

    Invalidate the rectangle that contains the speaker cursor.
    The coordinates are client coordinates.

   -------------------------------------------------------- */

void InvalidateCursorRect( HWND hwnd )
{
    RECT rc = { g_x - g_dxSpeakerHot,g_y - g_dySpeakerHot,
                g_x - g_dxSpeakerHot + g_cxSpeaker, g_y - g_dySpeakerHot + g_cySpeaker };
    InvalidateRect( hwnd, &rc, TRUE );
}

/* --------------------------------------------------------

      UpdateCursorPosition

      Move our private cursor in the requested direction.

   -------------------------------------------------------- */

void UpdateCursorPosition( int dx, int dy )
{    
    // Make sure cursor is erased at old position.
    InvalidateCursorRect( g_hwnd );

    g_x += dx * 10;
    g_y += dy * 10;

    // Clip the cursor to client area.
    if ( g_x < 0 ) 
        g_x = 0;
    if ( g_x >= WINWD - 2 ) 
        g_x = WINWD - 3;
    if ( g_y < 0 ) 
        g_y = 0;
    if ( g_y >= WINHT - 2 ) 
        g_y = WINHT - 3;

    // Make sure cursor is painted at new position.
    InvalidateCursorRect( g_hwnd );
}  // UpdateCursorPosition()


/* --------------------------------------------------------

   WindowProc()
   Main message handler.

   -------------------------------------------------------- */

LRESULT CALLBACK WindowProc( HWND hwnd, UINT message, 
                             WPARAM wParam, LPARAM lParam )
{
    static HINSTANCE hinst;
    HDC              hdc;
    PAINTSTRUCT      ps;
    UINT             DripCheck;

  
    switch ( message )
    {
        case WM_CREATE:
            hinst = ( ( LPCREATESTRUCT )lParam )->hInstance;
            return 0;

        case WM_COMMAND:
            switch ( LOWORD( wParam ) )
            {
                case ID_FILE_PLAYDRIP:
                    PlayDrip = !PlayDrip;
                    DripCheck = PlayDrip ? MF_CHECKED : MF_UNCHECKED;
                    CheckMenuItem( GetMenu( hwnd ), ID_FILE_PLAYDRIP,
                            MF_BYCOMMAND | DripCheck );
                    break;

                case ID_FILE_EXIT:
                    SendMessage( hwnd, WM_CLOSE, 0, 0L );
                    break;
            }
            break;

        case WM_PAINT:
            hdc = BeginPaint( hwnd, &ps );
            if ( hdc )
            {
                // Draw speaker in new location.
                DrawIcon( hdc, g_x - g_dxSpeakerHot,
                          g_y - g_dySpeakerHot, g_hcurSpeaker );
                // Restore listener.
                DrawIcon( hdc, WINWD/2 - g_dxCrossHot,
                          WINHT/2 - g_dyCrossHot, g_hcurCross );
            }
            EndPaint( hwnd, &ps );
            ReleaseDC( hwnd, hdc );
            break;

        case WM_KEYDOWN:
            switch ( wParam )
            {
                case VK_NUMPAD1: 
                    TurnListener( -0.5, 0, -0.5 );
                    break;
                case VK_NUMPAD2: 
                    TurnListener( 0, 0, -0.5 );
                    break;
                case VK_NUMPAD3: 
                    TurnListener( 0.5, 0, -0.5 );
                    break;
                case VK_NUMPAD4: 
                    TurnListener( -0.5, 0, 0 );
                    break;
                case VK_NUMPAD6: 
                    TurnListener( 0.5, 0, 0 );
                    break;
                case VK_NUMPAD7: 
                    TurnListener( -0.5, 0, 0.5 );
                    break;
                case VK_NUMPAD8: 
                    TurnListener( 0, 0, 0.5 );
                    break;
                case VK_NUMPAD9: 
                    TurnListener( 0.5, 0, 0.5 );
                    break;

                case VK_LEFT:
                    UpdateCursorPosition( -1, 0 );
                    MoveSound( g_x, g_y );
                    break;
                case VK_RIGHT:
                    UpdateCursorPosition( 1, 0 );
                    MoveSound( g_x, g_y );
                    break;
                case VK_UP:
                    UpdateCursorPosition( 0, -1 );
                    MoveSound( g_x, g_y );
                    break;
                case VK_DOWN:
                    UpdateCursorPosition( 0, 1 );
                    MoveSound( g_x, g_y );
                    break;
            }
            break;


        case WM_DESTROY:
            PostQuitMessage( 0 );
            break;
            
    } // switch ( message )

    return DefWindowProc( hwnd, message, wParam, lParam );
} // WindowProc 



/* --------------------------------------------------------

   WinMain()
   Windows initialization

   -------------------------------------------------------- */

int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR lpCmdLine, int nCmdShow )
{
    MSG         msg;
    WNDCLASS    wc;

    lpCmdLine = lpCmdLine;
    hPrevInstance = hPrevInstance;

    // Set up and register window class.
    wc.style = 0;
    wc.lpfnWndProc = WindowProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInstance;
    wc.hIcon = LoadIcon( hInstance, MAKEINTRESOURCE( ID_ICON_SPEAKER ) );
    wc.hCursor = LoadCursor( NULL, IDC_ARROW );
    wc.hbrBackground = (HBRUSH)GetStockObject( WHITE_BRUSH );
    wc.lpszMenuName = "MENU";
    wc.lpszClassName = NAME;
    RegisterClass( &wc );

    
    // Set up speaker cursor.
    g_hcurSpeaker = LoadIcon( hInstance, MAKEINTRESOURCE( ID_ICON_SPEAKER ) );

    ICONINFO ii;
    GetIconInfo( g_hcurSpeaker, &ii );

    BITMAP bm;
    GetObject( ii.hbmMask, sizeof( BITMAP ), &bm );
    if ( ii.hbmMask )  DeleteObject( ii.hbmMask );
    if ( ii.hbmColor ) DeleteObject( ii.hbmColor );

    g_dxSpeakerHot = ii.xHotspot;
    g_dySpeakerHot = ii.yHotspot;

    g_cxSpeaker = bm.bmWidth;
    g_cySpeaker = bm.bmHeight;

    // Set up listener cursor.
    g_hcurCross = LoadCursor( NULL, IDC_CROSS );

    GetIconInfo( g_hcurCross, &ii );

    GetObject( ii.hbmMask, sizeof( BITMAP ), &bm );
    if ( ii.hbmMask )  DeleteObject( ii.hbmMask );
    if ( ii.hbmColor ) DeleteObject( ii.hbmColor );

    g_dxCrossHot = ii.xHotspot;
    g_dyCrossHot = ii.yHotspot;

    g_cxCross = bm.bmWidth;
    g_cyCross = bm.bmHeight / 2;

    // Calculate window size from desired client size.
    DWORD dwStyle = WS_POPUPWINDOW | WS_CAPTION | WS_VISIBLE;
    RECT rc = { 0, 0, WINWD, WINHT };
    AdjustWindowRect( &rc, dwStyle, TRUE );

    // Create the window.
    g_hwnd = CreateWindow(
            NAME,
            NAME,
            dwStyle,
            CW_USEDEFAULT, CW_USEDEFAULT,  // position
            rc.right - rc.left,            // width
            rc.bottom - rc.top,            // height
            HWND_DESKTOP,
            NULL,
            hInstance,
            NULL );

    if ( !g_hwnd ) return FALSE;
    ShowWindow( g_hwnd, nCmdShow );
    UpdateWindow( g_hwnd );

    if ( !Init3DMusic( g_hwnd, hInstance ) )
    {
        Cleanup3DMusic();
        DestroyWindow( g_hwnd );
        return 0;
    };

 
    // The message loop.
    BOOL Done = FALSE;
    while ( !Done )
    {
        while ( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) ) 
		{ 
            if ( msg.message == WM_QUIT ) 
            {
                Done = TRUE;
            } 
            else
            {
                TranslateMessage( &msg );
                DispatchMessage( &msg );
            }
        }
        if ( PlayDrip ) Drip();
    } // while ( !Done )

    Cleanup3DMusic();
    return msg.wParam;
} // WinMain 


