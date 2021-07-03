//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Windows management for DirectInput sample
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include "KeybdImm.h"


//-----------------------------------------------------------------------------
// Name: MainDialogProc()
// Desc: Handles dialog messages
//
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

            // Set a timer to go off 12 times a second, to read input
            // Note: Typically an application would poll the keyboard
            //       much faster than this, but this slow rate is simply 
            //       for the purposes of demostration
            SetTimer( hDlg, 0, 1000 / 12, NULL );
            return TRUE;

        case WM_ACTIVATE:
            if( WA_INACTIVE == wParam )
                g_bActive = FALSE;
            else
                g_bActive = TRUE;

            // Set exclusive mode access to the mouse based on active state
            SetAcquire( hDlg );
            return TRUE;

        case WM_TIMER:
            // Update the input device every timer message
            if ( g_bActive )
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
            switch ( LOWORD(wParam) )
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




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
//-----------------------------------------------------------------------------
int APIENTRY WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, 
                      LPSTR strCmdLine, int nCmdShow )
{
    // Display the main dialog box.
    DialogBox( hInstance, MAKEINTRESOURCE( IDD_KEYBD_IMM ), 
               NULL, (DLGPROC)MainDialogProc );

    FreeDirectInput();

    return TRUE;
}




