//-----------------------------------------------------------------------------
// File: JoystImm.cpp
//
// Desc: Demonstrates an application which receives immediate 
//       joystick data in exclusive mode via a dialog timer.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define INITGUID
#include <windows.h>
#include <dinput.h>
#include <stdio.h>
#include "resource.h"


//-----------------------------------------------------------------------------
// Function prototypes and global variables
//-----------------------------------------------------------------------------
LPDIRECTINPUT7       g_pDI              = NULL;         
LPDIRECTINPUTDEVICE2 g_pJoystick        = NULL;     
DIDEVCAPS            g_diDevCaps;

extern BOOL      g_bActive;
extern HINSTANCE g_hInst;





//-----------------------------------------------------------------------------
// Name: EnumJoysticksCallback()
// Desc: Called once for each enumerated joystick. If we find one, create a
//       device interface on it so we can play with it.
//-----------------------------------------------------------------------------
BOOL CALLBACK EnumJoysticksCallback( const DIDEVICEINSTANCE* pdidInstance,
                                     VOID* pContext )
{
    HRESULT hr;

    // Obtain an interface to the enumerated joystick.
    hr = g_pDI->CreateDeviceEx( pdidInstance->guidInstance, IID_IDirectInputDevice2,
		                        (VOID**)&g_pJoystick, NULL );

    // If it failed, then we can't use this joystick. (Maybe the user unplugged
    // it while we were in the middle of enumerating it.)
    if( FAILED(hr) ) 
        return DIENUM_CONTINUE;


    // Stop enumeration. Note: we're just taking the first joystick we get. You
    // could store all the enumerated joysticks and let the user pick.
    return DIENUM_STOP;
}




//-----------------------------------------------------------------------------
// Name: EnumAxesCallback()
// Desc: Callback function for enumerating the axes on a joystick
//-----------------------------------------------------------------------------
BOOL CALLBACK EnumAxesCallback( const DIDEVICEOBJECTINSTANCE* pdidoi,
                                VOID* pContext )
{
    HWND hDlg = (HWND)pContext;

    DIPROPRANGE diprg; 
    diprg.diph.dwSize       = sizeof(DIPROPRANGE); 
    diprg.diph.dwHeaderSize = sizeof(DIPROPHEADER); 
    diprg.diph.dwHow        = DIPH_BYOFFSET; 
    diprg.diph.dwObj        = pdidoi->dwOfs; // Specify the enumerated axis
    diprg.lMin              = -1000; 
    diprg.lMax              = +1000; 
    
    // Set the range for the axis
    if( FAILED( g_pJoystick->SetProperty( DIPROP_RANGE, &diprg.diph ) ) )
        return DIENUM_STOP;

    // Set the UI to reflect what axes the joystick supports
    switch( pdidoi->dwOfs )
    {
        case DIJOFS_X:
            EnableWindow( GetDlgItem( hDlg, IDC_X_AXIS ), TRUE );
            EnableWindow( GetDlgItem( hDlg, IDC_X_AXIS_TEXT ), TRUE );
            break;
        case DIJOFS_Y:
            EnableWindow( GetDlgItem( hDlg, IDC_Y_AXIS ), TRUE );
            EnableWindow( GetDlgItem( hDlg, IDC_Y_AXIS_TEXT ), TRUE );
            break;
        case DIJOFS_Z:
            EnableWindow( GetDlgItem( hDlg, IDC_Z_AXIS ), TRUE );
            EnableWindow( GetDlgItem( hDlg, IDC_Z_AXIS_TEXT ), TRUE );
            break;
        case DIJOFS_RX:
            EnableWindow( GetDlgItem( hDlg, IDC_X_ROT ), TRUE );
            EnableWindow( GetDlgItem( hDlg, IDC_X_ROT_TEXT ), TRUE );
            break;
        case DIJOFS_RY:
            EnableWindow( GetDlgItem( hDlg, IDC_Y_ROT ), TRUE );
            EnableWindow( GetDlgItem( hDlg, IDC_Y_ROT_TEXT ), TRUE );
            break;
        case DIJOFS_RZ:
            EnableWindow( GetDlgItem( hDlg, IDC_Z_ROT ), TRUE );
            EnableWindow( GetDlgItem( hDlg, IDC_Z_ROT_TEXT ), TRUE );
            break;
        case DIJOFS_SLIDER(0):
            EnableWindow( GetDlgItem( hDlg, IDC_SLIDER0 ), TRUE );
            EnableWindow( GetDlgItem( hDlg, IDC_SLIDER0_TEXT ), TRUE );
            break;
        case DIJOFS_SLIDER(1):
            EnableWindow( GetDlgItem( hDlg, IDC_SLIDER1 ), TRUE );
            EnableWindow( GetDlgItem( hDlg, IDC_SLIDER1_TEXT ), TRUE );
            break;
    }

    return DIENUM_CONTINUE;
}




//-----------------------------------------------------------------------------
// Name: InitDirectInput()
// Desc: Initialize the DirectInput variables.
//-----------------------------------------------------------------------------
HRESULT InitDirectInput( HWND hDlg )
{
    HRESULT hr;

    // Register with the DirectInput subsystem and get a pointer
    // to a IDirectInput interface we can use.
    hr = DirectInputCreateEx( g_hInst, DIRECTINPUT_VERSION,IID_IDirectInput7, (LPVOID*)&g_pDI, NULL );
    if( FAILED(hr) ) 
        return hr;

    // Look for a simple joystick we can use for this sample program.
    hr = g_pDI->EnumDevices( DIDEVTYPE_JOYSTICK, EnumJoysticksCallback,
                             NULL, DIEDFL_ATTACHEDONLY );
    if( FAILED(hr) ) 
        return hr;

    // Make sure we got a joystick
    if( NULL == g_pJoystick )
    {
        MessageBox( NULL, "Joystick not found", "DInput Sample", 
                    MB_ICONERROR | MB_OK );
        return E_FAIL;
    }

    // Set the data format to "simple joystick" - a predefined data format 
    //
    // A data format specifies which controls on a device we are interested in,
    // and how they should be reported. This tells DInput that we will be
    // passing a DIJOYSTATE structure to IDirectInputDevice::GetDeviceState().
    hr = g_pJoystick->SetDataFormat( &c_dfDIJoystick );
    if( FAILED(hr) ) 
        return hr;

    // Set the cooperative level to let DInput know how this device should
    // interact with the system and with other DInput applications.
    hr = g_pJoystick->SetCooperativeLevel( hDlg, DISCL_EXCLUSIVE|DISCL_FOREGROUND );
    if( FAILED(hr) ) 
        return hr;

    // Determine how many axis the joystick has (so we don't error out setting
    // properties for unavailable axis)
    g_diDevCaps.dwSize = sizeof(DIDEVCAPS);
    hr = g_pJoystick->GetCapabilities(&g_diDevCaps);
    if ( FAILED(hr) ) 
        return hr;


    // Enumerate the axes of the joyctick and set the range of each axis. Note:
    // we could just use the defaults, but we're just trying to show an example
    // of enumerating device objects (axes, buttons, etc.).

//    g_pJoystick->EnumObjects( EnumAxesCallback, (VOID*)g_pJoystick, DIDFT_AXIS );
    g_pJoystick->EnumObjects( EnumAxesCallback, (VOID*)hDlg, DIDFT_AXIS );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetAcquire()
// Desc: Acquire or unacquire the keyboard, depending on if the app is active
//       Input device must be acquired before the GetDeviceState is called
//-----------------------------------------------------------------------------
HRESULT SetAcquire( HWND hWnd )
{
    if( g_pJoystick )
    {
        if( g_bActive ) 
            g_pJoystick->Acquire();
        else 
            g_pJoystick->Unacquire();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: UpdateInputState()
// Desc: Get the input device's state and display it.
//-----------------------------------------------------------------------------
HRESULT UpdateInputState( HWND hDlg )
{
    HRESULT     hr;
    CHAR        strText[128]; // Device state text
    DIJOYSTATE  js;           // DInput joystick state 
    CHAR*       str;

    if( g_pJoystick ) 
    {
        do
        {
            // Poll the device to read the current state
            hr = g_pJoystick->Poll();
            if( FAILED(hr) )
                return hr;

            // Get the input's device state
            hr = g_pJoystick->GetDeviceState( sizeof(DIJOYSTATE), &js );

            if( hr == DIERR_INPUTLOST )
            {
                // DInput is telling us that the input stream has been
                // interrupted. We aren't tracking any state between polls, so
                // we don't have any special reset that needs to be done. We
                // just re-acquire and try again.
                hr = g_pJoystick->Acquire();
                if( FAILED(hr) )  
                    return hr;
            }
        }
        while( DIERR_INPUTLOST == hr );

        if( FAILED(hr) )  
            return hr;

        // Display joystick state to dialog
        sprintf( strText, "%ld", js.lX ); 
        SetWindowText( GetDlgItem( hDlg, IDC_X_AXIS ), strText );
        sprintf( strText, "%ld", js.lY ); 
        SetWindowText( GetDlgItem( hDlg, IDC_Y_AXIS ), strText );
        sprintf( strText, "%ld", js.lZ ); 
        SetWindowText( GetDlgItem( hDlg, IDC_Z_AXIS ), strText );

        // For steering wheels
        sprintf( strText, "%ld", js.lRx ); 
        SetWindowText( GetDlgItem( hDlg, IDC_X_ROT ), strText );
        sprintf( strText, "%ld", js.lRy ); 
        SetWindowText( GetDlgItem( hDlg, IDC_Y_ROT ), strText );
        sprintf( strText, "%ld", js.lRz ); 
        SetWindowText( GetDlgItem( hDlg, IDC_Z_ROT ), strText );

        // For slider controls
        sprintf( strText, "%ld", js.rglSlider[0] ); 
        SetWindowText( GetDlgItem( hDlg, IDC_SLIDER0 ), strText );
        sprintf( strText, "%ld", js.rglSlider[1] ); 
        SetWindowText( GetDlgItem( hDlg, IDC_SLIDER1 ), strText );

        // Point of view
        if( g_diDevCaps.dwPOVs >= 1 )
        {
            wsprintf( strText, "%ld", js.rgdwPOV[0] ); 
            SetWindowText( GetDlgItem( hDlg, IDC_POV ), strText );
        }

        // Fill up text with which buttons are pressed
        str = strText;
        for( int i = 0; i < 32; i++ )
        {
            if ( js.rgbButtons[i] & 0x80 )
                str += sprintf( str, "%02d ", i );
        }
        *str = 0;   // Terminate the string 

        SetWindowText( GetDlgItem( hDlg, IDC_BUTTONS ), strText );
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
    if( NULL != g_pJoystick ) 
    {
        // Unacquire the device one last time just in case 
        // the app tried to exit while the device is still acquired.
        g_pJoystick->Unacquire();
        g_pJoystick->Release();
        g_pJoystick = NULL;
    }


    // Release any DirectInput objects.
    if( g_pDI ) 
    {
        g_pDI->Release();
        g_pDI = NULL;
    }

    return S_OK;
}



