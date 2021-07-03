//-----------------------------------------------------------------------------
// File: KeybdImm.cpp
//
// Desc: Demonstrates an application which receives immediate 
//       keyboard data in non-exclusive mode via a dialog timer.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include "KeybdImm.h"
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
                                     DISCL_NONEXCLUSIVE | DISCL_FOREGROUND );
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
    CHAR strOldText[128];   // Previous keyboard state text
    CHAR strNewText[128];   // Current keyboard state text

    if( g_pKeyboard ) 
    {
        BYTE    diks[256];   // DirectInput keyboard state buffer 
        HRESULT hr;

        hr = DIERR_INPUTLOST;

        // If input is lost then acquire and keep trying 
        while( DIERR_INPUTLOST == hr ) 
        {
            // Get the input's device state, and put the state in dims
            hr = g_pKeyboard->GetDeviceState( sizeof(diks), &diks );

            if( hr == DIERR_INPUTLOST )
            {
                // DirectInput is telling us that the input stream has been
				// interrupted.  We aren't tracking any state between polls, so
				// we don't have any special reset that needs to be done.
				// We just re-acquire and try again.
                hr = g_pKeyboard->Acquire();
            }
        }

        if( FAILED(hr) )  
            return hr;

        // Display the index values of the keys that are down
        CHAR* str = strNewText;
        for( int i = 0; i < 256; i++ ) 
        {
            if( diks[i] & 0x80 ) 
                str += wsprintf( str, "%02x ", i );
        }
        *str = 0;   // Terminate the string 

        // If anything changed then repaint - avoid flicker
        GetWindowText( GetDlgItem( hDlg, IDC_KEYBD_STATE ), strOldText, 255 );

        // Set the text on the dialog
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

