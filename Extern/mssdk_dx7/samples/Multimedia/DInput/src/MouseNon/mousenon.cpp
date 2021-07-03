//-----------------------------------------------------------------------------
// File: MouseNon.cpp
//
// Desc: Demonstrates an application which receives relative mouse data
//       in non-exclusive mode via a dialog timer.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define INITGUID
#include "MouseNon.h"
#include "resource.h"

//-----------------------------------------------------------------------------
// Global variables for the DirectMusic sample 
//-----------------------------------------------------------------------------
IDirectInput*           g_pDI       = NULL;
IDirectInputDevice*     g_pMouse    = NULL;
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

    // Obtain an interface to the system mouse device.
    hr = g_pDI->CreateDevice( GUID_SysMouse, &g_pMouse, NULL );
    if ( FAILED(hr) ) 
        return hr;

    // Set the data format to "mouse format" - a predefined data format 
    //
    // A data format specifies which controls on a device we
    // are interested in, and how they should be reported.
    //
    // This tells DirectInput that we will be passing a
    // DIMOUSESTATE structure to IDirectInputDevice::GetDeviceState.
    hr = g_pMouse->SetDataFormat( &c_dfDIMouse );
    if ( FAILED(hr) ) 
        return hr;

    // Set the cooperativity level to let DirectInput know how
    // this device should interact with the system and with other
    // DirectInput applications.
    hr = g_pMouse->SetCooperativeLevel( hDlg, 
                                        DISCL_NONEXCLUSIVE | DISCL_FOREGROUND);
    if ( FAILED(hr) ) 
        return hr;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: SetAcquire
//
// Description: 
//      Acquire or unacquire the mouse, depending on if the app is active
//       Input device must be acquired before the GetDeviceState is called
//
//-----------------------------------------------------------------------------
HRESULT SetAcquire( HWND hDlg )
{
    char szText[128];
    HWND hDlgText;

    // nothing to do if g_pMouse is NULL
    if (NULL == g_pMouse)
        return S_FALSE;

    if (g_bActive) 
    {
        // acquire the input device 
        g_pMouse->Acquire();
    } 
    else 
    {
        // update the dialog text 
        strcpy( szText, "Unacquired" );
        hDlgText = GetDlgItem( hDlg, IDC_MOUSE_STATE );
        SetWindowText( hDlgText, szText );

        // unacquire the input device 
        g_pMouse->Unacquire();
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
    char szOldText[128];    // previous mouse state text
    char szNewText[128];    // current  mouse state text
    HWND hDlgText;          // handle to static text box

    if (NULL != g_pMouse) 
    {
        DIMOUSESTATE dims;      // DirectInput mouse state structure
        HRESULT hr;

        hr = DIERR_INPUTLOST;

        // if input is lost then acquire and keep trying 
        while ( DIERR_INPUTLOST == hr ) 
        {
            // get the input's device state, and put the state in dims
            hr = g_pMouse->GetDeviceState( sizeof(DIMOUSESTATE), &dims );

            if ( hr == DIERR_INPUTLOST )
            {
                // DirectInput is telling us that the input stream has
                // been interrupted.  We aren't tracking any state
                // between polls, so we don't have any special reset
                // that needs to be done.  We just re-acquire and
                // try again.
                hr = g_pMouse->Acquire();
                if ( FAILED(hr) )  
                    return hr;
            }
        }

        if ( FAILED(hr) )  
            return hr;

        // The dims structure now has the state of the mouse, so 
        // display mouse coordinates (x, y, z) and buttons.
        wsprintf( szNewText, "(%d, %d, %d) %c %c %c %c",
                     dims.lX, dims.lY, dims.lZ,
                     (dims.rgbButtons[0] & 0x80) ? '0' : ' ',
                     (dims.rgbButtons[1] & 0x80) ? '1' : ' ',
                     (dims.rgbButtons[2] & 0x80) ? '2' : ' ',
                     (dims.rgbButtons[3] & 0x80) ? '3' : ' ');

        // if anything changed then repaint - avoid flicker
        hDlgText = GetDlgItem( hDlg, IDC_MOUSE_STATE );
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
    if (NULL != g_pMouse) 
    {
        // Unacquire the device one last time just in case 
        // the app tried to exit while the device is still acquired.
        g_pMouse->Unacquire();

        g_pMouse->Release();
        g_pMouse = NULL;
    }

    // Release any DirectInput objects.
    if (NULL != g_pDI) 
    {
        g_pDI->Release();
        g_pDI = NULL;
    }

    return S_OK;
}

