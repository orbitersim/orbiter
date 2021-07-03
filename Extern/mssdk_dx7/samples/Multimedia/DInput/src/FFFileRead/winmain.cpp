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
BOOL CALLBACK MainDialogProc( HWND, UINT, WPARAM, LPARAM );
extern HRESULT InitDirectInput( HWND hDlg );
extern HRESULT FreeDirectInput();
extern HRESULT OnReadFile( HWND hDlg );
extern HRESULT OnPlayEffects( HWND hDlg );




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.
//-----------------------------------------------------------------------------
int APIENTRY WinMain( HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR pCmdLine, int nCmdShow )
{
    // Display the main dialog box.
    DialogBox( hInst, MAKEINTRESOURCE( IDD_MAIN ), NULL, MainDialogProc );

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: MainDialogProc
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
BOOL CALLBACK MainDialogProc( HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam )
{
    HRESULT hr;

    switch( msg ) 
    {
        case WM_INITDIALOG:
            {
                // Set the icon for this dialog.
                HINSTANCE hInst = (HINSTANCE) GetWindowLong( hDlg, GWL_HINSTANCE );
                HICON hIcon = LoadIcon( hInst, MAKEINTRESOURCE( IDI_ICON ) );
                PostMessage( hDlg, WM_SETICON, ICON_BIG,   (LPARAM) hIcon );  // Set big icon
                PostMessage( hDlg, WM_SETICON, ICON_SMALL, (LPARAM) hIcon );  // Set small icon

                EnableWindow( GetDlgItem( hDlg, IDC_PLAY_EFFECTS ), FALSE );

                hr = InitDirectInput( hDlg );
                if( FAILED(hr) )
                {
                    MessageBox( NULL, "Error Initializing DirectInput. "
                                "The sample will now exit.", 
                                "DirectInput Sample", MB_ICONERROR | MB_OK );                
                    EndDialog( hDlg, 1 );
                    break;
                }
            }
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDCANCEL:
                    EndDialog( hDlg, 0 ); 
                    break;

                case IDC_READ_FILE:
                    if( FAILED( hr = OnReadFile( hDlg ) ) )
                    {
                        MessageBox( NULL, "Error reading effects file.",
                                    "DirectInput Sample", MB_ICONERROR | MB_OK );
                        EnableWindow( GetDlgItem( hDlg, IDC_PLAY_EFFECTS ), FALSE );
                        break;
                    }
                    break;

                case IDC_PLAY_EFFECTS:
                    if( FAILED( hr = OnPlayEffects( hDlg ) ) )
                    {
                        MessageBox( NULL, "Error playing DirectInput effects. "
                                    "The sample will now exit.", 
                                    "DirectInput Sample", MB_ICONERROR | MB_OK );
                        EndDialog( hDlg, 1 );
                        break;
                    }
                    break;

                default: 
                    return FALSE; // Message not handled
            }
            break;

        case WM_DESTROY:
            FreeDirectInput();   
            break;

        default: 
            return FALSE; // Message not handled
    }

    return TRUE; // Message handled
}

