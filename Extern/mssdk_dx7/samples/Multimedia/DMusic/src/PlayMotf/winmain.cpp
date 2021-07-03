//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Plays a Primary Segment and a Motif using DirectMusic
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <stdlib.h>
#include "resource.h"
#include "playmotf.h"


//-----------------------------------------------------------------------------
// Extern global variables 
//-----------------------------------------------------------------------------
extern WCHAR g_awstrMotifName[9][MAX_PATH];




//-----------------------------------------------------------------------------
// Name: MainDialogProc()
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainDialogProc( HWND hDlg, UINT msg, WPARAM wParam, 
                                 LPARAM lParam )
{
    DWORD dwIndex;

    switch( msg )
    {
		case WM_INITDIALOG:
			// Update buttons with names of the motifs
			for( dwIndex = 0; dwIndex < 9; dwIndex++ )
			{
			    CHAR strButton[255];
				CHAR strMotif[255];
				HWND hWnd = GetDlgItem( hDlg, IDC_MOTIF_1 + dwIndex );

				// If this motif exists, set up the corresponding dialog button
				if( g_awstrMotifName[ dwIndex ][0] )
				{
					// Make the button visible
					LONG style = GetWindowLong( hWnd, GWL_STYLE );
					SetWindowLong( hWnd, GWL_STYLE, style|WS_VISIBLE );

					// Set the button's text
					GetWindowText( hWnd, strButton, 255 );
					wcstombs( strMotif, g_awstrMotifName[ dwIndex ], 255 );
					strcat( strButton, ": " );
					strcat( strButton, strMotif );
					SetWindowText( hWnd, strButton );
				}
			}
			break;

		case WM_COMMAND:
			switch( LOWORD(wParam) )
			{
				case IDC_MOTIF_1:
				case IDC_MOTIF_2:
				case IDC_MOTIF_3:
				case IDC_MOTIF_4:
				case IDC_MOTIF_5:
				case IDC_MOTIF_6:
				case IDC_MOTIF_7:
				case IDC_MOTIF_8:
				case IDC_MOTIF_9:
					// Play selected motif
					dwIndex = LOWORD(wParam) - IDC_MOTIF_1;
					PlayMotif( g_awstrMotifName[ dwIndex ] );
					break;

				case IDC_CLOSE:
					PostQuitMessage( 0 );
					break;
			}
			break;

		case WM_CLOSE:
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
int APIENTRY WinMain( HINSTANCE hInstance, HINSTANCE, 
                      LPSTR strCmdLine, int nCmdShow )
{
	// Initialize all DMusic objects
    if( FAILED( InitDirectMusic( strCmdLine ) ) )
    {
        FreeDirectMusic();
        return FALSE;
    }

    // Display the main dialog box.
    DialogBox( hInstance, MAKEINTRESOURCE(IDD_PLAY_MOTIF), NULL, 
               (DLGPROC)MainDialogProc );

	// Free the DMusic objects
    FreeDirectMusic();

    return TRUE;
}




