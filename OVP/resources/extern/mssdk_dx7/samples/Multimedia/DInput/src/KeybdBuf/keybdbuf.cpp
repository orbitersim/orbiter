//-----------------------------------------------------------------------------
// File: KeybdBuf.cpp
//
// Desc: Demonstrates an application which receives keyboard data
//       in non-exclusive mode via a dialog timer.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define INITGUID
#include "KeybdBuf.h"
#include "resource.h"

#define BUFFER_SIZE 16  // number of buffer elements

//-----------------------------------------------------------------------------
// Global variables for the DirectMusic sample 
//-----------------------------------------------------------------------------
IDirectInput*           g_pDI       = NULL;         
IDirectInputDevice*     g_pKeyboard = NULL;     
HINSTANCE               g_hInst     = NULL;
BOOL                    g_bActive   = TRUE;     




//-----------------------------------------------------------------------------
// Function: InitDirectInput
//
// Description: 
//      Initialize the DirectInput variables.
//
//-----------------------------------------------------------------------------
HRESULT InitDirectInput( HWND hDlg )
{
    HRESULT hr;

    // Register with the DirectInput subsystem and get a pointer
    // to a IDirectInput interface we can use.
    hr = DirectInputCreate( g_hInst, DIRECTINPUT_VERSION, &g_pDI, NULL );
    if ( FAILED(hr) ) 
        return hr;

    // Obtain an interface to the system keyboard device.
    hr = g_pDI->CreateDevice( GUID_SysKeyboard, &g_pKeyboard, NULL );
    if ( FAILED(hr) ) 
        return hr;

    // Set the data format to "keyboard format" - a predefined data format 
    //
    // A data format specifies which controls on a device we
    // are interested in, and how they should be reported.
    //
    // This tells DirectInput that we will be passing an array
    // of 256 bytes to IDirectInputDevice::GetDeviceState.
    hr = g_pKeyboard->SetDataFormat( &c_dfDIKeyboard );
    if ( FAILED(hr) ) 
        return hr;

    // Set the cooperativity level to let DirectInput know how
    // this device should interact with the system and with other
    // DirectInput applications.
    hr = g_pKeyboard->SetCooperativeLevel( hDlg, 
                                        DISCL_NONEXCLUSIVE | DISCL_FOREGROUND);
    if ( FAILED(hr) ) 
        return hr;

    // IMPORTANT STEP TO USE BUFFERED DEVICE DATA!
    //
    // DirectInput uses unbuffered I/O (buffer size = 0) by default.
    // If you want to read buffered data, you need to set a nonzero
    // buffer size.
    //
    // Set the buffer size to DINPUT_BUFFERSIZE (defined above) elements.
    //
    // The buffer size is a DWORD property associated with the device.
    DIPROPDWORD dipdw;

    dipdw.diph.dwSize = sizeof(DIPROPDWORD);
    dipdw.diph.dwHeaderSize = sizeof(DIPROPHEADER);
    dipdw.diph.dwObj = 0;
    dipdw.diph.dwHow = DIPH_DEVICE;
    dipdw.dwData = BUFFER_SIZE;

    hr = g_pKeyboard->SetProperty( DIPROP_BUFFERSIZE, &dipdw.diph );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: SetAcquire
//
// Description: 
//      Acquire or unacquire the keyboard, depending on if the app is active
//      Input device must be acquired before the GetDeviceState is called
//
//-----------------------------------------------------------------------------
HRESULT SetAcquire( HWND hDlg )
{
    char szText[128];
    HWND hDlgText;

    // nothing to do if g_pKeyboard is NULL
    if (NULL == g_pKeyboard)
        return S_FALSE;

    if (g_bActive) 
    {
        // acquire the input device 
        g_pKeyboard->Acquire();
    } 
    else 
    {
        // update the dialog text 
        strcpy( szText, "Unacquired" );
        hDlgText = GetDlgItem( hDlg, IDC_KEYBD_STATE );
        SetWindowText( hDlgText, szText );

        // unacquire the input device 
        g_pKeyboard->Unacquire();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: UpdateInputState
//
// Description: 
//      Get the input device's state and display it.
//
//-----------------------------------------------------------------------------
HRESULT UpdateInputState( HWND hDlg )
{
    char  szOldText[128];    // previous keyboard state text
    char  szNewText[128];    // current  keyboard state text
    HWND  hDlgText;          // handle to static text box
    DWORD i;

    if (NULL != g_pKeyboard) 
    {
        DIDEVICEOBJECTDATA didod[ BUFFER_SIZE ];  // Receives buffered data 
        DWORD dwElements;
        HRESULT hr;
        hr = DIERR_INPUTLOST;

        while ( DI_OK != hr )
        {

            dwElements = BUFFER_SIZE;
            hr = g_pKeyboard->GetDeviceData( sizeof(DIDEVICEOBJECTDATA),
                                            didod, 
                                            &dwElements, 
                                            0 );
            if (hr != DI_OK) 
            {
                 // We got an error or we got DI_BUFFEROVERFLOW.
                 //
                 // Either way, it means that continuous contact with the
                 // device has been lost, either due to an external
                 // interruption, or because the buffer overflowed
                 // and some events were lost.
                 //
                 // Consequently, if a button was pressed at the time
                 // the buffer overflowed or the connection was broken,
                 // the corresponding "up" message might have been lost.
                 //
                 // But since our simple sample doesn't actually have
                 // any state associated with button up or down events,
                 // there is no state to reset.  (In a real game, ignoring
                 // the buffer overflow would result in the game thinking
                 // a key was held down when in fact it isn't; it's just
                 // that the "up" event got lost because the buffer
                 // overflowed.)
                 //
                 // If we want to be cleverer, we could do a
                 // GetDeviceState() and compare the current state
                 // against the state we think the device is in,
                 // and process all the states that are currently
                 // different from our private state.
                hr = g_pKeyboard->Acquire();
                if ( FAILED(hr) )  
                    return hr;

            }
        }

        if ( FAILED(hr) )  
            return hr;

        // Study each of the buffer elements and process them.
        //
        // Since we really don't do anything, our "processing"
        // consists merely of squirting the name into our
        // local buffer.
        char *psz = szNewText;
        for (i = 0; i < dwElements; i++) 
        {
            // this will display then scan code of the key
            // plus a 'D' - meaning the key was pressed 
            //   or a 'U' - meaning the key was released
            psz += wsprintf(psz, "%02x%s ",
                      didod[ i ].dwOfs,
                     (didod[ i ].dwData & 0x80) ? "D" : "U");
        }
        *psz = 0;   // Terminate the string 


        // if anything changed then repaint - avoid flicker
        hDlgText = GetDlgItem( hDlg, IDC_KEYBD_STATE );
        GetWindowText( hDlgText, szOldText, 255 );

        if ( 0 != lstrcmp( szOldText, szNewText ) ) 
        {
            // set the text on the dialog
            SetWindowText( hDlgText, szNewText );
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: FreeDirectInput
//
// Description: 
//      Initialize the DirectInput variables.
//
//-----------------------------------------------------------------------------
HRESULT FreeDirectInput()
{
    // Unacquire and release any DirectInputDevice objects.
    if (NULL != g_pKeyboard) 
    {
        // Unacquire the device one last time just in case 
        // the app tried to exit while the device is still acquired.
        g_pKeyboard->Unacquire();

        g_pKeyboard->Release();
        g_pKeyboard = NULL;
    }

    // Release any DirectInput objects.
    if (NULL != g_pDI) 
    {
        g_pDI->Release();
        g_pDI = NULL;
    }

    return S_OK;
}

