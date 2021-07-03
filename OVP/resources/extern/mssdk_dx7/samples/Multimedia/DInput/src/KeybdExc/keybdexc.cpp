//-----------------------------------------------------------------------------
// File: KeybdExc.cpp
//
// Desc: Demonstrates an application which receives immediate 
//       keyboard data in exclusive mode. Here's how exclusive vs.
//       non-exclusive cooperative levels are explained in the DirectX
//       documentation:
//           "The fact that your application is using a device at the exclusive
//         level does not mean that other applications cannot get data from
//         the device. However, it does mean that no other application can
//         also acquire the device exclusively."
//           "When an application has exclusive access to the keyboard, it
//         suppresses all keyboard messages except CTRL+ALT+DEL and, on Windows
//         95 and Windows 98, ALT+TAB."
//           "DirectInput does not allow any application to have background 
//         exclusive access to the keyboard. If it did, Windows would not have
//         access to the keyboard and the user would not even be able to use
//         CTRL+ALT+DELETE to restart the system."
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <dinput.h>
#include <stdio.h>
#include "resource.h"


//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
LPDIRECTINPUT       g_pDI       = NULL;         
LPDIRECTINPUTDEVICE g_pKeyboard = NULL;     
BOOL                g_bActive   = TRUE;     




//-----------------------------------------------------------------------------
// Name: InitDirectInput()
// Desc: Initialize the DirectInput variables.
//-----------------------------------------------------------------------------
HRESULT InitDirectInput( HWND hWnd )
{
    HRESULT hr;

    // Register with the DirectInput subsystem and get a pointer
    // to a IDirectInput interface we can use.
    hr = DirectInputCreate( (HINSTANCE)GetWindowLong( hWnd, GWL_HINSTANCE ),
                            DIRECTINPUT_VERSION, &g_pDI, NULL );
    if( FAILED(hr) ) 
        return hr;

    // Obtain an interface to the system keyboard device.
    hr = g_pDI->CreateDevice( GUID_SysKeyboard, &g_pKeyboard, NULL );
    if( FAILED(hr) ) 
        return hr;

    // Set the data format to "keyboard format" - a predefined data format 
    //
    // A data format specifies which controls on a device we
    // are interested in, and how they should be reported.
    //
    // This tells DirectInput that we will be passing an array
    // of 256 bytes to IDirectInputDevice::GetDeviceState.
    hr = g_pKeyboard->SetDataFormat( &c_dfDIKeyboard );
    if( FAILED(hr) ) 
        return hr;

    // Set the cooperativity level to let DirectInput know how
    // this device should interact with the system and with other
    // DirectInput applications.
    hr = g_pKeyboard->SetCooperativeLevel( hWnd, 
                                     DISCL_EXCLUSIVE | DISCL_FOREGROUND );
    if( FAILED(hr) ) 
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetAcquire()
// Desc: Acquire or unacquire the keyboard, depending on if the app is active
//       Input device must be acquired before the GetDeviceState is called
//-----------------------------------------------------------------------------
HRESULT SetAcquire( HWND hDlg )
{
    // Nothing to do if g_pKeyboard is NULL
    if( NULL == g_pKeyboard )
        return S_FALSE;

    if( g_bActive ) 
    {
        // Acquire the input device 
        g_pKeyboard->Acquire();
    } 
    else 
    {
        // Update the dialog text 
        SetWindowText( GetDlgItem( hDlg, IDC_KEYBD_STATE ), "Unacquired" );

        // Unacquire the input device 
        g_pKeyboard->Unacquire();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: UpdateInputState()
// Desc: Get the input device's state and display it.
//-----------------------------------------------------------------------------
HRESULT UpdateInputState( HWND hDlg )
{
    if( g_pKeyboard ) 
    {
        BYTE    diks[256];   // DirectInput keyboard state buffer 
        HRESULT hr;

        // Get the input's device state, and put the state in dims
        hr = g_pKeyboard->GetDeviceState( sizeof(diks), &diks );
        if( FAILED(hr) )  
        {
            // DirectInput is telling us that the input stream has been
            // interrupted.  We aren't tracking any state between polls, so
            // we don't have any special reset that needs to be done.
            // We just re-acquire and try again.

            // If input is lost then acquire and keep trying 
            hr = g_pKeyboard->Acquire();
            while( hr == DIERR_INPUTLOST || 
                   hr == E_ACCESSDENIED  ) 
            {
                hr = g_pKeyboard->Acquire();
            }

            if( FAILED(hr) )  
                return hr;
        }
        
        // Make a string of the index values of the keys that are down
        CHAR strNewText[256*3 + 1] = "";
        CHAR strLetter[4];

        for( int i = 0; i < 256; i++ ) 
        {
              if( diks[i] & 0x80 ) 
              {
                  sprintf( strLetter, "%02x ", i );
                  strcat( strNewText, strLetter );
              }
        }

        // Get the old text in the text box
        CHAR strOldText[128];
        GetWindowText( GetDlgItem( hDlg, IDC_KEYBD_STATE ), strOldText, 127 );

        // If anything changed then repaint - avoid flicker
        if( 0 != lstrcmp( strOldText, strNewText ) ) 
            SetWindowText( GetDlgItem( hDlg, IDC_KEYBD_STATE ), strNewText );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FreeDirectInput()
// Desc: Initialize the DirectInput variables.
//-----------------------------------------------------------------------------
HRESULT FreeDirectInput()
{
    // Unacquire and release any DirectInputDevice objects.
    if( g_pKeyboard ) 
    {
        // Unacquire the device one last time just in case 
        // the app tried to exit while the device is still acquired.
        g_pKeyboard->Unacquire();
        g_pKeyboard->Release();
        g_pKeyboard = NULL;
    }

    // Release any DirectInput objects.
    if( g_pDI ) 
    {
        g_pDI->Release();
        g_pDI = NULL;
    }

    return S_OK;
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
            if( wParam == IDC_CLOSE )
                PostQuitMessage( 0 );

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
    DialogBox( hInstance, MAKEINTRESOURCE(IDD_KEYBD_EXC), NULL,
               (DLGPROC)MainDialogProc );

    FreeDirectInput();

    return TRUE;
}





