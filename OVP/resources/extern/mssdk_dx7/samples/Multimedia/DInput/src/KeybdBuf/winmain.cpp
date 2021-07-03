//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Windows management for DirectInput sample
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include "KeybdBuf.h"

//-----------------------------------------------------------------------------
// Function prototypes 
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainDialogProc( HWND, UINT, WPARAM, LPARAM );




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
    // DirectInputCreate needs the instance handle
    g_hInst = hInstance;

    // Display the main dialog box.
    DialogBox( hInstance, 
            MAKEINTRESOURCE( IDD_KEYBD_BUF ), 
            NULL, 
            (DLGPROC) MainDialogProc );

    FreeDirectInput();

    return TRUE;
}




//-----------------------------------------------------------------------------
// Function: MainDialogProc
//
// Description: 
//     Handles dialog messages
//
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainDialogProc( HWND hDlg, UINT message, WPARAM wParam, 
                                 LPARAM lParam )
{
    HRESULT hr;

    switch (message) 
    {
        case WM_INITDIALOG:
            hr = InitDirectInput( hDlg );
            if ( FAILED(hr) )
            {
                MessageBox( NULL, 
                    "Error Initializing DirectInput", 
                    "DirectInput Sample", 
                    MB_ICONERROR | MB_OK );

                EndDialog( hDlg, 0 );
            }

            // set a timer to go off once a second at every timer 
            // message the input device will be read
            SetTimer( hDlg, 0, 1000 / 1, NULL );

            return TRUE;

            break;

        case WM_ACTIVATE:   // sent when window changes active state
            if ( WA_INACTIVE == wParam )
            {
                g_bActive = FALSE;
            }
            else
            {
                g_bActive = TRUE;
            }

            // Set exclusive mode access to the keyboard based on active state
            SetAcquire( hDlg );

            return TRUE;
            break;

        case WM_TIMER:
            // update the input device every timer message

            if ( g_bActive )  // update only when active
            {
                hr = UpdateInputState( hDlg );
                if ( FAILED(hr) )
                {
                    KillTimer( hDlg, 0 );    
                    MessageBox( NULL, 
                        "Error Reading Input State", 
                        "DirectInput Sample", 
                        MB_ICONERROR | MB_OK );

                    EndDialog( hDlg, TRUE ); 
                }
            }
            break;

        case WM_COMMAND:
            {
                switch ( LOWORD(wParam) )
                {
                case IDC_CLOSE:
                    PostQuitMessage( 0 );
                    break;
                }

                return TRUE;
            }
            break;


        case WM_CLOSE:
            KillTimer( hDlg, 0 );
            EndDialog( hDlg, TRUE );

            return TRUE;
            break;
    }

    return FALSE;
}




