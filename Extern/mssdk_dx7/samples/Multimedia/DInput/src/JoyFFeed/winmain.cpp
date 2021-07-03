//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Windows management for DirectInput sample
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include <windowsx.h>
#include "JoyFFeed.h"

#define FEEDBACK_WINDOW_X       20
#define FEEDBACK_WINDOW_Y       60
#define FEEDBACK_WINDOW_WIDTH   200


//-----------------------------------------------------------------------------
// Function prototypes 
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainDialogProc( HWND, UINT, WPARAM, LPARAM );
VOID    OnPaint( HWND hWnd );
VOID    OnMouseMove( HWND hWnd, int x, int y, UINT keyFlags );
VOID    OnLeftButtonDown( HWND hWnd, int x, int y, UINT keyFlags );
VOID    OnLeftButtonUp( HWND hWnd, int x, int y, UINT keyFlags );
int     CoordToForce( int x );




//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
DWORD g_dwLastEffectSet; // Time of the previous force feedback effect set




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
//-----------------------------------------------------------------------------
int APIENTRY WinMain( HINSTANCE hInstance, HINSTANCE, LPSTR, int )
{
    // DirectInputCreate needs the instance handle
    g_hInst = hInstance;

    // Display the main dialog box.
    DialogBox( hInstance, MAKEINTRESOURCE(IDD_JOY_FEEDBACK), NULL, 
               (DLGPROC)MainDialogProc );

    FreeDirectInput();

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: MainDialogProc()
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainDialogProc( HWND hDlg, UINT msg, WPARAM wParam, 
                                 LPARAM lParam )
{
    HRESULT hr;

    switch( msg ) 
    {
        case WM_INITDIALOG:
            hr = InitDirectInput( hDlg );
            if( FAILED(hr) )
            {
                MessageBox( NULL, "Error Initializing DirectInput", 
                            "DirectInput Sample", MB_ICONERROR | MB_OK );
                EndDialog( hDlg, 0 );
            }

            // Init the time of the last force feedback effect
            g_dwLastEffectSet = timeGetTime();

            return TRUE;

        case WM_MOUSEMOVE:
            OnMouseMove( hDlg, LOWORD(lParam), HIWORD(lParam), wParam );
            break;

        case WM_LBUTTONDOWN:
            OnLeftButtonDown( hDlg, LOWORD(lParam), HIWORD(lParam), wParam );
            break;

        case WM_LBUTTONUP:
            OnLeftButtonUp( hDlg, LOWORD(lParam), HIWORD(lParam), wParam );
            break;

        case WM_PAINT:
            OnPaint( hDlg );
            break;

        case WM_ACTIVATE:
			// Sent when window changes active state
            if( WA_INACTIVE == wParam )
                g_bActive = FALSE;
            else
                g_bActive = TRUE;

            // Set exclusive mode access to the mouse based on active state
            SetAcquire( hDlg );

            return TRUE;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
	            case IDC_CLOSE:
		            PostQuitMessage( 0 );
            }
            return TRUE;

        case WM_CLOSE:
            EndDialog( hDlg, TRUE ); 
            return TRUE;
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: OnPaint()
// Desc: Handles the WM_PAINT window message
//-----------------------------------------------------------------------------
VOID OnPaint( HWND hWnd )
{
    PAINTSTRUCT ps;
    HDC         hDC;
    HPEN        hpenOld;
    HPEN        hpenBlack;
    HBRUSH      hbrOld;
    HBRUSH      hbrBlack;
    int         x;
    int         y;
    
    hDC = BeginPaint( hWnd, &ps );

    if( hDC ) 
    {
        // Everything is scaled to the size of the window.
        hpenBlack = GetStockPen( BLACK_PEN );
        hpenOld   = SelectPen( hDC, hpenBlack );

        // Draw force feedback bounding rect
        MoveToEx( hDC, FEEDBACK_WINDOW_X, FEEDBACK_WINDOW_Y, NULL );

        LineTo( hDC, FEEDBACK_WINDOW_X, 
                     FEEDBACK_WINDOW_Y + FEEDBACK_WINDOW_WIDTH );
        LineTo( hDC, FEEDBACK_WINDOW_X + FEEDBACK_WINDOW_WIDTH, 
                     FEEDBACK_WINDOW_Y + FEEDBACK_WINDOW_WIDTH );
        LineTo( hDC, FEEDBACK_WINDOW_X + FEEDBACK_WINDOW_WIDTH, 
                     FEEDBACK_WINDOW_Y );
        LineTo( hDC, FEEDBACK_WINDOW_X, 
                     FEEDBACK_WINDOW_Y );

        // Calculate center of feedback window for center marker
        x = FEEDBACK_WINDOW_X + FEEDBACK_WINDOW_WIDTH / 2;
        y = FEEDBACK_WINDOW_Y + FEEDBACK_WINDOW_WIDTH / 2;

        // Draw center marker
        MoveToEx( hDC, x, y - 10, NULL );
        LineTo(   hDC, x, y + 10 + 1 );
        MoveToEx( hDC, x - 10, y, NULL );
        LineTo(   hDC, x + 10 + 1, y );

        hbrBlack = GetStockBrush( BLACK_BRUSH );
        hbrOld   = SelectBrush( hDC, hbrBlack );

        x = MulDiv( FEEDBACK_WINDOW_WIDTH,
                    g_nXForce + DI_FFNOMINALMAX, 
                    2 * DI_FFNOMINALMAX );

        y = MulDiv( FEEDBACK_WINDOW_WIDTH, 
                    g_nYForce + DI_FFNOMINALMAX, 
                    2 * DI_FFNOMINALMAX );

        x += FEEDBACK_WINDOW_X;
        y += FEEDBACK_WINDOW_Y;

        Ellipse( hDC, x-5, y-5, x+6, y+6 );

        SelectBrush( hDC, hbrOld );
        SelectPen( hDC, hpenOld );

        EndPaint( hWnd, &ps );
    }
}





//-----------------------------------------------------------------------------
// Name: OnMouseMove()
// Desc: If the mouse button is down, then change the direction of
//       the force to match the new location.
//-----------------------------------------------------------------------------
VOID OnMouseMove( HWND hWnd, int x, int y, UINT keyFlags )
{
    DWORD dwCurrentTime;

    if( keyFlags & MK_LBUTTON ) 
    {
        dwCurrentTime = timeGetTime();
        
        if( dwCurrentTime - g_dwLastEffectSet < 100 )
        {
            // Don't allow setting effect more often than
            // 100ms since every time an effect is set, the
            // joystick will jerk.
            //
            // Note: This is not neccessary, and is specific to this sample
            return;
        }

        g_dwLastEffectSet = dwCurrentTime;

        x -= FEEDBACK_WINDOW_X;
        y -= FEEDBACK_WINDOW_Y;

        g_nXForce = CoordToForce( x );
        g_nYForce = CoordToForce( y );

        InvalidateRect( hWnd, 0, TRUE );
        UpdateWindow( hWnd );

        SetJoyForcesXY();
    }
}




//-----------------------------------------------------------------------------
// Name: OnLeftButtonDown()
// Desc: Capture the mouse so we can follow it, and start updating the
//       force information.
//-----------------------------------------------------------------------------
VOID OnLeftButtonDown( HWND hWnd, int x, int y, UINT keyFlags )
{
    SetCapture( hWnd );
    OnMouseMove( hWnd, x, y, MK_LBUTTON );
}




//-----------------------------------------------------------------------------
// Name: OnLeftButtonUp()
// Desc: Stop capturing the mouse when the button goes up.
//-----------------------------------------------------------------------------
VOID OnLeftButtonUp( HWND hWnd, int x, int y, UINT keyFlags )
{
    ReleaseCapture();
}




//-----------------------------------------------------------------------------
// Name: CoordToForce()
// Desc: Convert a coordinate 0 <= nCoord <= FEEDBACK_WINDOW_WIDTH 
//       to a force value in the range -DI_FFNOMINALMAX to +DI_FFNOMINALMAX.
//-----------------------------------------------------------------------------
int CoordToForce( int nCoord )
{
    int nForce = MulDiv( nCoord, 2 * DI_FFNOMINALMAX, FEEDBACK_WINDOW_WIDTH )
                 - DI_FFNOMINALMAX;

    // Keep force within bounds
    if( nForce < -DI_FFNOMINALMAX ) 
        nForce = -DI_FFNOMINALMAX;

    if( nForce > +DI_FFNOMINALMAX ) 
        nForce = +DI_FFNOMINALMAX;

    return nForce;
}

