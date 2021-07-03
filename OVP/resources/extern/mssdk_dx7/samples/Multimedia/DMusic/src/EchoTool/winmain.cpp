//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Plays a Primary Segment using DirectMusic
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------


#define STRICT
#include <windows.h>
#include "resource.h"
#include "Helper.h"




//-----------------------------------------------------------------------------
// Function: MainDialogProc
//
// Description: 
//      Handles dialog messages
//
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainDialogProc( HWND hDlg, UINT message, 
                                 WPARAM wParam, LPARAM lParam )
{
    HWND hPlaying = GetDlgItem( hDlg, IDC_NOW_PLAYING );

    switch (message) 
    {
    case WM_INITDIALOG:
        // default to no echo
        PostMessage( GetDlgItem( hDlg, IDC_NO_ECHO ), BM_SETCHECK, BST_CHECKED, 0 );
        PostMessage( hDlg, WM_COMMAND, IDC_NO_ECHO, 0 );
        break;

    case WM_COMMAND:
        switch ( LOWORD(wParam) )
        {
        case IDC_NO_ECHO:
            g_pEchoTool->SetEchoNum(0);
            SetWindowText( hPlaying, "No Echo" );
            break;

        case IDC_4TH_ECHO:
            g_pEchoTool->SetEchoNum(3);
            g_pEchoTool->SetDelay(DMUS_PPQ / 1);
            SetWindowText( hPlaying, "1/4th Note Echo" );
            break;

        case IDC_8TH_ECHO:
            g_pEchoTool->SetEchoNum(3);
            g_pEchoTool->SetDelay(DMUS_PPQ / 2);
            SetWindowText( hPlaying, "1/8th Note Echo" );
            break;

        case IDC_16TH_ECHO:
            g_pEchoTool->SetEchoNum(3);
            g_pEchoTool->SetDelay(DMUS_PPQ / 4);
            SetWindowText( hPlaying, "1/16th Note Echo" );
            break;

        case IDC_FAST_ECHO:
            g_pEchoTool->SetEchoNum(3);
            g_pEchoTool->SetDelay(48); // really fast
            SetWindowText( hPlaying, "Fast Echo" );
            break;

        case IDC_CLOSE:
            PostQuitMessage( 0 );
            break;
        }
        break;

    case WM_CLOSE:
        EndDialog(hDlg, TRUE);
        return (TRUE);
    }

    return FALSE;
}




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

    hr = InitDirectMusic( lpCmdLine );
    if ( FAILED(hr) )
    {
        FreeDirectMusic();
        return FALSE;
    }

    // Display the main dialog box.
    DialogBox( hInstance, 
        MAKEINTRESOURCE(IDD_ECHOTOOL), 
        NULL, 
        (DLGPROC)MainDialogProc );

    FreeDirectMusic();

    return TRUE;
}

