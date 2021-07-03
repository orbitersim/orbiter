//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Windows management for the Scrawl sample
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include <windowsx.h>
#include "Scrawl.h"

#define SCRAWL_CXBITMAP             512
#define SCRAWL_CYBITMAP             300

const char c_szAppName[] = "Scrawl";    // app name

//-----------------------------------------------------------------------------
// Local function prototypes 
//-----------------------------------------------------------------------------
HRESULT InitVariables();
HWND RegisterWindowClass( HINSTANCE hInst, int nCmdShow );
LRESULT CALLBACK MainWndProc( HWND, UINT, WPARAM, LPARAM );

HRESULT OnClear( HWND hWnd );
void InvalidateCursorRect(HWND hWnd);

//-----------------------------------------------------------------------------
// Message handlers
//-----------------------------------------------------------------------------
void OnPaint( HWND hWnd );
BOOL OnCreate( HWND hWnd, LPCREATESTRUCT lpCreateStruct );
void OnInitMenuPopup( HWND hWnd, HMENU hMenu, UINT item, BOOL fSystemMenu );
void OnKeyDown( HWND hWnd, UINT vk, BOOL fDown, int cRepeat, UINT flags );

//-----------------------------------------------------------------------------
// Global varibles
//-----------------------------------------------------------------------------
HDC     g_hDC           = NULL; // Memory DC our picture lives in 
HBITMAP g_hBitmap       = NULL; // Our picture 
HBITMAP g_hbmpDeselect  = NULL; // Stock bitmap for deselecting 

HCURSOR g_hCursorCross  = NULL; // cross hair
int     g_cxCross;              // Width of crosshairs cursor 
int     g_cyCross;              // Height of crosshairs cursor 
int     g_dxCrossHot;           // Hotspot location of crosshairs 
int     g_dyCrossHot;           // Hotspot location of crosshairs 
BOOL    g_bShowCursor   = TRUE; // Should the cursor be shown? 

int     g_x;                    // Virtual x-coordinate 
int     g_y;                    // Virtual y-coordinate 

int     g_dxFuzz;               // Leftover x-fuzz from scaling 
int     g_dyFuzz;               // Leftover y-fuzz from scaling 
int     g_iSensitivity;         // Mouse sensitivity 




//-----------------------------------------------------------------------------
// Function: WinMain(HANDLE, HANDLE, LPSTR, int)
//
// Description: 
//     Entry point for the application.  Since we use a simple dialog for 
//     user interaction we don't need to pump messages.
//
//-----------------------------------------------------------------------------
int APIENTRY WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, 
                      LPSTR lpCmdLine, int nCmdShow )
{
    HRESULT hr;
    HWND    hWnd;
    BOOL    bDone;
    DWORD   dwResult;
    MSG     msg;

    // DirectInputCreate needs the instance handle
    g_hInst = hInstance;

    // initialize global varibles
    hr = InitVariables();
    if ( FAILED(hr) )
    {
        MessageBox( NULL, 
            "Error Initializing Variables", 
            "DirectInput Sample", 
            MB_ICONERROR | MB_OK );
        return TRUE;
    }

    // Display the main dialog box.
    hWnd = RegisterWindowClass( hInstance, nCmdShow );
    if ( NULL == hWnd )
    {
        MessageBox( NULL, 
            "Error Creating Window", 
            "DirectInput Sample", 
            MB_ICONERROR | MB_OK );
        return TRUE;
    }

    // start message pump
   if ( NULL != hWnd ) 
   {
        //  Since we use notification handles, we need to use
        //  MsgWaitForMultipleObjects to wait for the event or
        //  a message, whichever comes first.

        bDone = FALSE;
        while ( !bDone ) 
        {
            dwResult = MsgWaitForMultipleObjects( 
                            1,
                            &g_hMouseEvent, 
                            FALSE, 
                            INFINITE,
                            QS_ALLINPUT );

            switch (dwResult) 
            {
                // WAIT_OBJECT_0 + 0 means that g_hevtMouse was signalled 
                case WAIT_OBJECT_0 + 0:
                    OnMouseInput( hWnd );
                    break;

                // WAIT_OBJECT_0 + 1 means that we have messages to process 
                case WAIT_OBJECT_0 + 1:
                    while ( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) ) 
                    {
                        if (msg.message == WM_QUIT) 
                        {
                            // stop loop if it's a quit message
                            bDone = TRUE;
                        } 
                        else 
                        {
                            TranslateMessage(&msg);
                            DispatchMessage(&msg);
                        }
                    }
                    break;
            }
        }
    }

    // clean up
    FreeDirectInput();

    // delete bitmaps
    if ( NULL != g_hDC ) 
    {
        if ( NULL != g_hbmpDeselect ) 
        {
            SelectObject( g_hDC, g_hbmpDeselect );
        }

        DeleteDC( g_hDC );
    }

    if ( NULL != g_hBitmap ) 
    {
        DeleteObject( g_hBitmap );
    }

    return TRUE;
}




//-----------------------------------------------------------------------------
// Function: InitVariables
//
// Description: 
//      Initialize global varibles 
//
//-----------------------------------------------------------------------------
HRESULT InitVariables()
{
    ICONINFO iconInfo;
    BITMAP   bitmap;
    HDC      hDC;

    // get our crosshairs cursor and extract the the width and
    // hotspot location so we can draw it manually.
    g_hCursorCross = LoadCursor( NULL, IDC_CROSS );

    GetIconInfo( g_hCursorCross, &iconInfo );
    GetObject( iconInfo.hbmMask, sizeof(BITMAP), &bitmap );

    // delete un-needed handles
    if (NULL != iconInfo.hbmMask)  
        DeleteObject( iconInfo.hbmMask );
    if (NULL != iconInfo.hbmColor) 
        DeleteObject( iconInfo.hbmColor );

    // save x-y info 
    g_dxCrossHot = iconInfo.xHotspot;
    g_dyCrossHot = iconInfo.yHotspot;

    g_cxCross = bitmap.bmWidth;
    g_cyCross = bitmap.bmHeight;

    // create and setup our scrawl bitmap.
    hDC = GetDC( NULL );
    g_hDC = CreateCompatibleDC( hDC );  
    ReleaseDC( NULL, hDC );

    if ( NULL == g_hDC ) 
        return E_FAIL;

    g_hBitmap = CreateBitmap( SCRAWL_CXBITMAP, 
                              SCRAWL_CYBITMAP, 
                              1, 1, 0 );

    if ( NULL == g_hBitmap ) 
        return E_FAIL;

    g_hbmpDeselect = (HBITMAP) SelectObject( g_hDC, g_hBitmap );

    // clear bitmap
    OnClear( NULL );  

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: RegisterWindowClass
//
// Description: 
//      Set up the window class.
//
//-----------------------------------------------------------------------------
HWND RegisterWindowClass( HINSTANCE hInst, int nCmdShow )
{
    HWND     hWnd = NULL;
    WNDCLASS wc;
    RECT     rc;
    DWORD    dwStyle;
    DWORD    dwExStyle;

    wc.hCursor        = LoadCursor( 0, IDC_ARROW );
    wc.hIcon          = LoadIcon( hInst, MAKEINTRESOURCE(IDI_MAIN) );
    wc.lpszMenuName   = NULL;
    wc.lpszClassName  = c_szAppName;
    wc.hbrBackground  = NULL;
    wc.hInstance      = hInst;
    wc.style          = 0;
    wc.lpfnWndProc    = MainWndProc;
    wc.cbClsExtra     = 0;
    wc.cbWndExtra     = 0;

    if ( FALSE == RegisterClass(&wc) ) 
    {
        return NULL;
    }

    dwStyle = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX;
    dwExStyle = WS_EX_APPWINDOW;

    rc.left     = 0;
    rc.top      = 0;
    rc.right    = SCRAWL_CXBITMAP;
    rc.bottom   = SCRAWL_CYBITMAP;

    AdjustWindowRectEx( &rc, dwStyle, FALSE, dwExStyle );

    hWnd = CreateWindowEx(
                dwExStyle,          // dwExStyle
                c_szAppName,        // lpClassName
                c_szAppName,        // lpWindowName
                dwStyle,            // dwStyle
                CW_USEDEFAULT,      // x
                CW_USEDEFAULT,      // y
                rc.right - rc.left, // nWidth
                rc.bottom - rc.top, // nHeight
                NULL,               // hWndParent
                NULL,               // hMenu 
                g_hInst,            // hInstance 
                NULL                // lpParam
                );

    ShowWindow( hWnd, nCmdShow );

    return hWnd;
}




//-----------------------------------------------------------------------------
// Function: MainWndProc
//
// Description: 
//      Handles window messages
//
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainWndProc( HWND hWnd, UINT message, WPARAM wParam, 
                              LPARAM lParam )
{
    LRESULT lr = 0;

    switch (message) 
    {
            // pass these messages to user defined functions
            HANDLE_MSG( hWnd, WM_CREATE,        OnCreate );
            HANDLE_MSG( hWnd, WM_PAINT,         OnPaint );
            HANDLE_MSG( hWnd, WM_INITMENUPOPUP, OnInitMenuPopup );
            HANDLE_MSG( hWnd, WM_KEYDOWN,       OnKeyDown );

        case WM_ACTIVATE:   // sent when window changes active state
            if ( WA_INACTIVE == wParam )
            {
                g_bActive = FALSE;
            }
            else
            {
                g_bActive = TRUE;
            }

            // Set exclusive mode access to the mouse based on active state
            SetAcquire();
            return 0;
            break;

        case WM_ENTERMENULOOP:
        case WM_ENTERSIZEMOVE:
            // un-acquire device when entering menu or re-sizing
            // this will show the mouse cursor again
            g_bActive = FALSE;
            SetAcquire();
            return 0;
            break;

        case WM_EXITMENULOOP:
            // If we aren't returning from the popup menu, let the user continue
            // to be in non-exclusive mode (to move the window for example)
            if ((BOOL )wParam == FALSE)
                return 0;
        case WM_EXITSIZEMOVE:
            // re-acquire device when leaving menu or re-sizing
            // this will show the mouse cursor again

            // even though the menu is going away, the app
            // might have lost focus or be an icon
            if ( GetActiveWindow() == hWnd ||
                 !IsIconic( hWnd ) )
            {
                g_bActive = TRUE;
            }
            else
            {
                g_bActive = FALSE;
            }

            SetAcquire();
            return 0;
            break;
        
        case WM_SYSCOMMAND:
            lr = 0;
            switch ( LOWORD(wParam) ) 
            {
            case IDC_CLEAR:
                OnClear( hWnd );
                break;
            
            case IDC_ABOUT:
                MessageBox( hWnd, 
                    "Scrawl DirectInput Sample v1.0",
                    c_szAppName, 
                    MB_OK );
                break;
            
            case SC_SCREENSAVE:
                // eat the screen-saver notification.
                break;

            case IDC_SENSITIVITY_LOW:
                g_iSensitivity = -1;
                break;

            case IDC_SENSITIVITY_NORMAL:
                g_iSensitivity = 0;
                break;

            case IDC_SENSITIVITY_HIGH:
                g_iSensitivity = 1;
                break;
            
            default:
                lr = DefWindowProc( hWnd, message, wParam, lParam );
                break;
            }
        
            // The WM_SYSCOMMAND might've been a WM_CLOSE, 
            // in which case our window no longer exists.  
            if ( IsWindow(hWnd) ) 
            {
                SetAcquire();
            }
            return lr;  // 0 by default
            break;

       case WM_DESTROY:
            PostQuitMessage(0);
            break;
    }

    return DefWindowProc( hWnd, message, wParam, lParam );
}




//-----------------------------------------------------------------------------
// Function: OnCreate
//
// Description: 
//      Handles the WM_CREATE window message
//
//-----------------------------------------------------------------------------
BOOL OnCreate( HWND hWnd, LPCREATESTRUCT lpCreateStruct )
{
    HRESULT hr;
    HMENU   hMenu;

    // initialize direct input
    hr = InitDirectInput( hWnd );
    if ( FAILED(hr) )
    {
        MessageBox( NULL, 
            "Error Initializing DirectInput", 
            "DirectInput Sample", 
            MB_ICONERROR | MB_OK );
        return FALSE; // creation failed
    }

    // fix up the popup system menu with custom commands
    hMenu = GetSystemMenu( hWnd, FALSE );

    EnableMenuItem( hMenu, SC_SIZE,     MF_BYCOMMAND | MF_DISABLED | MF_GRAYED );
    EnableMenuItem( hMenu, SC_MAXIMIZE, MF_BYCOMMAND | MF_DISABLED | MF_GRAYED );

    AppendMenu( hMenu, 
        MF_ENABLED | MF_STRING, 
        IDC_CLEAR, 
        "C&lear\tDel" );

    AppendMenu( hMenu, 
        MF_ENABLED | MF_STRING, 
        IDC_ABOUT, 
        "&About\tF1" );

    AppendMenu( hMenu, 
        MF_ENABLED | MF_STRING | MF_POPUP,
        (UINT)LoadMenu( g_hInst, MAKEINTRESOURCE(IDM_SENSITIVITY) ),
        "Sensitivit&y" );

    return TRUE;    
}




//-----------------------------------------------------------------------------
// Function: OnPaint
//
// Description: 
//      Handles the WM_PAINT window message
//
//-----------------------------------------------------------------------------
void OnPaint( HWND hWnd )
{
    PAINTSTRUCT ps;
    HDC         hDC;
    
    hDC = BeginPaint( hWnd, &ps );

    if ( NULL != hDC ) 
    {
        BitBlt( hDC,
                ps.rcPaint.left,
                ps.rcPaint.top,
                ps.rcPaint.right - ps.rcPaint.left,
                ps.rcPaint.bottom - ps.rcPaint.top,
                g_hDC,
                ps.rcPaint.left,
                ps.rcPaint.top,
                SRCCOPY );

        if ( g_bActive && g_bShowCursor ) 
        {
            DrawIcon( hDC, 
                g_x - g_dxCrossHot,
                g_y - g_dyCrossHot, 
                g_hCursorCross );
        }

        EndPaint( hWnd, &ps );
    }
}




//-----------------------------------------------------------------------------
// Function: OnInitMenuPopup
//
// Description: 
//      Handles the WM_INITMENUPOPUP window message
//
//-----------------------------------------------------------------------------
void OnInitMenuPopup( HWND hWnd, HMENU hMenu, UINT item, BOOL fSystemMenu )
{
    int iSensitivity;

    for (iSensitivity = -1; iSensitivity <= 1; iSensitivity++) 
    {
        if ( g_iSensitivity == iSensitivity ) 
        {
            CheckMenuItem( hMenu, 
                IDC_SENSITIVITY_NORMAL + iSensitivity,
                MF_BYCOMMAND | MF_CHECKED );
        } 
        else 
        {
            CheckMenuItem( hMenu, 
                IDC_SENSITIVITY_NORMAL + iSensitivity,
                MF_BYCOMMAND | MF_UNCHECKED );
        }
    }
}




//-----------------------------------------------------------------------------
// Function: OnKeyDown
//
// Description: 
//      Handles the WM_KEYDOWN window message
//
//-----------------------------------------------------------------------------
void OnKeyDown( HWND hWnd, UINT vk, BOOL fDown, int cRepeat, UINT flags )
{
    switch (vk) 
    {
    case '1':
    case '2':
    case '3':
        PostMessage( hWnd, WM_SYSCOMMAND, IDC_SENSITIVITY_NORMAL + vk - '2', 0 );
        break;

    case VK_DELETE:
        PostMessage( hWnd, WM_SYSCOMMAND, IDC_CLEAR, 0 );
        break;

    case VK_F1:
        PostMessage( hWnd, WM_SYSCOMMAND, IDC_ABOUT, 0 );
        break;
    }
}




//-----------------------------------------------------------------------------
// Function: OnClear
//
// Description: 
//      Makes the bitmap white
//
//-----------------------------------------------------------------------------
HRESULT OnClear( HWND hWnd )
{
    PatBlt( g_hDC, 
            0, 0, 
            SCRAWL_CXBITMAP, SCRAWL_CYBITMAP, 
            WHITENESS );

    if ( NULL != hWnd ) 
    {
        InvalidateRect( hWnd, 0, 0 );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: InvalidateCursorRect
//
// Description: 
//      Invalidate the rectangle that contains the cursor.
//      The coordinates are in client coordinates.
//
//-----------------------------------------------------------------------------
void InvalidateCursorRect(HWND hWnd)
{
    RECT rc = { g_x - g_dxCrossHot,             g_y - g_dyCrossHot,
                g_x - g_dxCrossHot + g_cxCross, g_y - g_dyCrossHot + g_cyCross };

    InvalidateRect( hWnd, &rc, 0 );
}




//-----------------------------------------------------------------------------
// Function: UpdateCursorPosition
//
// Description: 
//      Move our private cursor in the requested direction, subject
//      to clipping, scaling, and all that other stuff.
//
//      This does not redraw the cursor.  You need to do that yourself.
//
//-----------------------------------------------------------------------------
void UpdateCursorPosition( int dx, int dy )
{   
    // Pick up any leftover fuzz from last time.  This is important
    // when scaling down mouse motions.  Otherwise, the user can
    // drag to the right extremely slow for the length of the table
    // and not get anywhere.
    dx += g_dxFuzz;     
    g_dxFuzz = 0;

    dy += g_dyFuzz;     
    g_dyFuzz = 0;

    switch (g_iSensitivity) 
    {

    case 1:     // High sensitivity: Magnify! 
        dx *= 2;
        dy *= 2;
        break;

    case -1:    // Low sensitivity: Scale down 
        g_dxFuzz = dx % 2;  // remember the fuzz for next time 
        g_dyFuzz = dy % 2;
        dx /= 2;
        dy /= 2;
        break;

    case 0:     // normal sensitivity 
        // No adjustments needed 
        break;
    }

    g_x += dx;
    g_y += dy;

    // clip the cursor to our client area
    if (g_x < 0)                
        g_x = 0;

    if (g_x >= SCRAWL_CXBITMAP) 
        g_x = SCRAWL_CXBITMAP - 1;

    if (g_y < 0)                
        g_y = 0;

    if (g_y >= SCRAWL_CYBITMAP) 
        g_y = SCRAWL_CYBITMAP - 1;
}




//-----------------------------------------------------------------------------
// Function: StartPenDraw
//
// Description: 
//      Called when starting pen draw.
//
//-----------------------------------------------------------------------------
void StartPenDraw( HWND hWnd, LEFTBUTTONINFO* plbInfo )
{
    // Hide the cursor while scrawling 
    g_bShowCursor = FALSE;

    plbInfo->hdcWindow = GetDC( hWnd );
    MoveToEx( plbInfo->hdcWindow, g_x, g_y, 0 );
    MoveToEx( g_hDC, g_x, g_y, 0 );

    SelectObject( plbInfo->hdcWindow, GetStockObject(BLACK_PEN) );
    SelectObject( g_hDC, GetStockObject(BLACK_PEN) );

    plbInfo->bMoved = FALSE;
    plbInfo->dwSeqLastSeen = 0;
}

//-----------------------------------------------------------------------------
// Function: FinishPenDraw
//
// Description: 
//      Called when ending pen draw.
//
//-----------------------------------------------------------------------------
void FinishPenDraw( HANDLE hWnd )
{
    g_bShowCursor = TRUE;
}




//-----------------------------------------------------------------------------
// Function: OnLeftButtonDown_FlushMotion
//
// Description: 
//      Flush out any motion that we are holding.
//
//-----------------------------------------------------------------------------
void OnLeftButtonDown_FlushMotion( LEFTBUTTONINFO* plbInfo )
{
    if ( plbInfo->bMoved ) 
    {
        plbInfo->bMoved = FALSE;
        plbInfo->dwSeqLastSeen = 0;
        LineTo( plbInfo->hdcWindow, g_x, g_y );
        LineTo( g_hDC, g_x, g_y );
    }
}




//-----------------------------------------------------------------------------
// Function: OnRightButtonUp
//
// Description: 
//      Pop up a context menu.
//
//-----------------------------------------------------------------------------
void OnRightButtonUp( HWND hWnd )
{
    HMENU hMenuPopup;
    UINT  iMenuID;
    POINT pt;

    // place a popup menu where the mouse curent is
    pt.x = g_x;
    pt.y = g_y;

    ClientToScreen( hWnd, &pt );
    hMenuPopup = GetSystemMenu( hWnd, FALSE );

    // hide the cursor while moving it so you don't get annoying flicker.
    ShowCursor( FALSE );
    InvalidateCursorRect( hWnd );

    // unacquire the devices so the user can interact with the menu.
    g_bActive = FALSE;
    SetAcquire();

    // put the Windows cursor at the same location as our virtual cursor.
    SetCursorPos( pt.x, pt.y );

    // show the cursor now that it is moved 
    ShowCursor( TRUE );
    InvalidateCursorRect( hWnd );

    // track the popup menu and return the menu item selected
    iMenuID = TrackPopupMenuEx( hMenuPopup,
                                 TPM_RIGHTBUTTON | TPM_RETURNCMD,
                                 pt.x, 
                                 pt.y, 
                                 hWnd, 
                                 0 );

    if ( 0 != iMenuID ) // if a menu item was selected
    {
        PostMessage(hWnd, WM_SYSCOMMAND, iMenuID, 0L);
    }
}

