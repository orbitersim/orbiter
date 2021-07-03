//-----------------------------------------------------------------------------
// File: WinMain.cpp
//
// Desc: Windows management for DirectMusic sample
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define STRICT
#include <windows.h>
#include "resource.h"
#include "PlayPri.h"


//-----------------------------------------------------------------------------
// Name: MainDialogProc()
// Desc: Handles dialog messages
//-----------------------------------------------------------------------------
LRESULT CALLBACK MainDialogProc( HWND hDlg, UINT message, 
                                 WPARAM wParam, LPARAM lParam )
{
    switch (message) 
    {
		case WM_COMMAND:
			switch ( LOWORD(wParam) )
			{
				case IDC_CLOSE:
					PostQuitMessage( 0 );
					break;
			}
			break;

		case WM_CLOSE:
			EndDialog( hDlg, TRUE );
			return (TRUE);
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point for the application.  Since we use a simple dialog for 
//       user interaction we don't need to pump messages.
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
    DialogBox( hInstance, MAKEINTRESOURCE(IDD_PLAY_PRI), NULL, 
               (DLGPROC)MainDialogProc );

    FreeDirectMusic();

    return TRUE;
}

