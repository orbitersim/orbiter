//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Windows management for DirectInput sample
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <dinput.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Function prototypes and global variables
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainDialogProc( HWND, UINT, WPARAM, LPARAM );
extern HRESULT InitDirectInput( HWND hDlg );
extern HRESULT SetAcquire( HWND hDlg );
extern HRESULT FreeDirectInput();
extern HRESULT UpdateInputState( HWND hDlg );

HINSTANCE g_hInst   = NULL;
BOOL      g_bActive = TRUE;     



//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.
//-----------------------------------------------------------------------------
int APIENTRY WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, 
                      LPSTR pCmdLine, int nCmdShow )
{
    // DirectInputCreate needs the instance handle
    g_hInst = hInstance;

    // Display the main dialog box.
    DialogBox( hInstance, MAKEINTRESOURCE( IDD_JOYST_IMM ), NULL,
		       (DLGPROC)MainDialogProc );

    FreeDirectInput();

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: MainDialogProc
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

            // Set a timer to go off 30 times a second. At every timer message
			// the input device will be read
            SetTimer( hDlg, 0, 1000 / 30, NULL );

            return TRUE;

        case WM_ACTIVATE:
            if( WA_INACTIVE == wParam )
                g_bActive = FALSE;
            else
                g_bActive = TRUE;

            // Set exclusive mode access to the joystick based on active state
            SetAcquire( hDlg );

            return TRUE;

        case WM_TIMER:
            // Update the input device every timer message
            if( g_bActive )
            {
                hr = UpdateInputState( hDlg );
                if( FAILED(hr) )
                {
                    KillTimer( hDlg, 0 );    
                    MessageBox( NULL, "Error Reading Input State", 
                                "DirectInput Sample", MB_ICONERROR | MB_OK );

                    EndDialog( hDlg, TRUE ); 
                }
            }
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
	            case IDC_CLOSE:
		            PostQuitMessage( 0 );
			        break;
            }

            return TRUE;

        case WM_CLOSE:
            KillTimer( hDlg, 0 );    
            EndDialog( hDlg, TRUE ); 

            return TRUE;
    }

    return FALSE;
}




