//-----------------------------------------------------------------------------
// Name: DIEx3.cpp
//
// Desc: DirectInput simple sample 3
//       Demonstrates an application which receives keyboard data
//       in non-exclusive mode via a game loop.
//
// Copyright (C) 1997-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <dinput.h>




//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
#define IDI_MAIN_ICON 101

LPDIRECTINPUT       g_pDI;
LPDIRECTINPUTDEVICE g_pdidKeyboard;
CHAR                g_strText[1024];  // What we display in client area
BOOL                g_bPaused = TRUE; // Should I be paused?


                                      
                                      
//-----------------------------------------------------------------------------
// Name: DisplayError()
// Desc: Displays an error in a message box
//-----------------------------------------------------------------------------
VOID DisplayError( HRESULT hr, CHAR* strMessage )
{
    MessageBox( NULL, strMessage, "DirectInput Sample", MB_OK );
}




//-----------------------------------------------------------------------------
// Name: CreateDInput()
// Desc: Initialize the DirectInput variables using:
//           DirectInputCreate
//           IDirectInput::CreateDevice
//           IDirectInputDevice::SetDataFormat
//           IDirectInputDevice::SetCooperativeLevel
//-----------------------------------------------------------------------------
BOOL CreateDInput( HWND hWnd )
{
    HINSTANCE hInst = (HINSTANCE)GetWindowLong( hWnd, GWL_HINSTANCE );
    HRESULT   hr;

    // Register with the DirectInput subsystem and get a pointer
    // to a IDirectInput interface we can use
    if( FAILED( hr = DirectInputCreate( hInst, DIRECTINPUT_VERSION, &g_pDI,
                                        NULL) ) )
    {
        DisplayError( hr, "DirectInputCreate() failed." );
        return hr;
    }

    // Obtain an interface to the keybord device
    if( FAILED( hr = g_pDI->CreateDevice( GUID_SysKeyboard, &g_pdidKeyboard,
                                          NULL ) ) )
    {
        DisplayError( hr, "CreateDevice() failed" );
        return hr;
    }

    // Set the data format to "keyboard format". A data format specifies which
    // controls on a device we are interested in, and how they should be
    // reported. This tells DirectInput that we will be passing an array of 256
    // bytes to IDirectInputDevice::GetDeviceState().
    if( FAILED( hr = g_pdidKeyboard->SetDataFormat( &c_dfDIKeyboard ) ) )
    {
        DisplayError( hr, "SetDataFormat() failed" );
        return hr;
    }

    // Set the cooperative level to let DirectInput know how this device
    // should interact with the system and with other DirectInput applications.
    // Use DISCL_NONEXCLUSIVE to retrieve device data when acquired, not
    // interfering with any other applications which are reading mouse data.
    // Use DISCL_FOREGROUND so that if the user switches away from our app,
    // automatically release the device back to the system.
    if( FAILED( hr = g_pdidKeyboard->SetCooperativeLevel( hWnd,
                                    DISCL_NONEXCLUSIVE | DISCL_FOREGROUND ) ) )
    {
        DisplayError( hr, "SetCooperativeLevel() failed" );
        return hr;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DestroyDInput()
// Desc: Terminate our usage of DirectInput
//-----------------------------------------------------------------------------
VOID DestroyDInput()
{
    // Destroy the keyboard object
    if( g_pdidKeyboard )
    {
        // Unacquire the device (just in case) before exitting.
        g_pdidKeyboard->Unacquire();

        g_pdidKeyboard->Release();
        g_pdidKeyboard = NULL;
    }

    // Destroy the DInput object
    if( g_pDI )
    {
        g_pDI->Release();
        g_pDI = NULL;
    }
}




//-----------------------------------------------------------------------------
// Name: UpdateFrame()
// Desc: The game plays here. Read keyboard data and displaying it.
//-----------------------------------------------------------------------------
VOID UpdateFrame( HWND hWnd )
{
    if( g_pdidKeyboard )
    {
        BYTE    diks[256]; // DirectInput keyboard state buffer
        HRESULT hr;

        hr = g_pdidKeyboard->GetDeviceState( sizeof(diks), &diks );
        if( hr == DIERR_INPUTLOST )
        {
            // DirectInput is telling us that the input stream has been
            // interrupted. Re-acquire and try again.
            hr = g_pdidKeyboard->Acquire();
            if( SUCCEEDED(hr) )
                UpdateFrame( hWnd );
        }

        if( SUCCEEDED(hr) )
        {
            CHAR  strBuf[1024];
            CHAR* str = strBuf;

            // Display the scan codes of the keys that are down.
            for( int i=0; i<256; i++ )
            {
                if( diks[i] & 0x80 )
                    str += wsprintf( str, "%02x ", i );
            }
            *str = 0;

            // Trigger a repaint if the status string changed.
            if( lstrcmp( g_strText, strBuf ) )
            {
                lstrcpy( g_strText, strBuf );
                InvalidateRect( hWnd, NULL, TRUE );
            }
        }
    }

    // Sleep for a few milliseconds to simulate a 30fps frame rate.
    Sleep( 1000/30 );
}




//-----------------------------------------------------------------------------
// Name: SyncAcquire()
// Desc: Acquire or unacquire the keyboard, depending on the g_bPaused flag.
//       This syncs the device with our internal view of the world.
//-----------------------------------------------------------------------------
VOID SyncAcquire( HWND hWnd )
{
    if( g_pdidKeyboard )
    {
        if( g_bPaused )
            g_pdidKeyboard->Unacquire();
        else
            g_pdidKeyboard->Acquire();
    }
}




//-----------------------------------------------------------------------------
// Name: WndProc()
// Desc: Window procedure for simple sample.
//-----------------------------------------------------------------------------
LRESULT CALLBACK WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg )
    {
        case WM_PAINT:
            // Paint the window with the text string of the mouse status
            {
                PAINTSTRUCT ps;
                HDC hDC = BeginPaint( hWnd, &ps );

                if( hDC )
                {
                    ExtTextOut( hDC, 0, 0, ETO_OPAQUE, &ps.rcPaint, g_strText,
                                lstrlen(g_strText), NULL );
                    EndPaint( hWnd, &ps );
                }
            }
            return 0;

        case WM_ACTIVATE:
            // Pause or unpause the app based on whether we are losing or
            // gaining activation. Tell DInput that we don't (if paused) or do
            // (if unpaused) want non-exclusive access to the keyboard.
            g_bPaused = (wParam == WA_INACTIVE) ? TRUE : FALSE;
            SyncAcquire( hWnd );
            break;

        case WM_DESTROY:
            PostQuitMessage(0);
            break;
    }

    return DefWindowProc( hWnd, msg, wParam, lParam );
}




//-----------------------------------------------------------------------------
// WinMain()
// Desc: Application entry point.
//-----------------------------------------------------------------------------
int PASCAL WinMain( HINSTANCE hInst, HINSTANCE, LPSTR, int nCmdShow )
{
    // Set up the window class
    WNDCLASS wc = { CS_HREDRAW | CS_VREDRAW, WndProc, 0, 0, hInst,
                    LoadIcon( hInst, MAKEINTRESOURCE(IDI_MAIN_ICON)),
                    LoadCursor(NULL, IDC_ARROW), NULL, NULL, 
                    TEXT("DI Window") };

    if( !RegisterClass( &wc ) )
        return FALSE;

    // Create the main window
    HWND hWnd = CreateWindow( "DI Window", "DIEx3 - Accessing the keyboard",
                              WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
                              300, 100, NULL, NULL, hInst, 0 );

    if( FAILED( CreateDInput( hWnd ) ) )
    {
        DestroyWindow( hWnd );
        return FALSE;
    }

    ShowWindow( hWnd, nCmdShow );

    // Standard game loop.
    while( TRUE )
    {
        MSG msg;

        if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
        {
            // Check for a quit message
            if( msg.message == WM_QUIT )
                break;

            TranslateMessage( &msg );
            DispatchMessage( &msg );
        }
        else if( g_bPaused )
        {
            WaitMessage();
        }
        else
        {
            UpdateFrame( hWnd );
        }
    }

    DestroyDInput();

    return TRUE;
}



