//-----------------------------------------------------------------------------
// Name: DIEx1.cpp
//
// Desc: DirectInput simple sample 1
//       Demonstrates an application which receives relative mouse data
//       in non-exclusive mode via a game loop.
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------

#include <windows.h>
#include <dinput.h>


//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
#define IDI_MAIN_ICON 101

LPDIRECTINPUT       g_pDI;
LPDIRECTINPUTDEVICE g_pdidMouse;
CHAR                g_strText[1024]; // What we display in client area
BOOL                g_bPaused = TRUE;




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

    // Obtain an interface to the system mouse device
    if( FAILED( hr = g_pDI->CreateDevice( GUID_SysMouse, &g_pdidMouse,
                                          NULL ) ) )
    {
        DisplayError( hr, "CreateDevice() failed" );
        return hr;
    }

    // Set the data format to "mouse format". A data format specifies which
    // controls on a device we are interested in, and how they should be
    // reported. This tells DirectInput that we will be passing a
    // DIMOUSESTATE structure to IDirectInputDevice::GetDeviceState.
    if( FAILED( hr = g_pdidMouse->SetDataFormat( &c_dfDIMouse ) ) )
    {
        DisplayError( hr, "SetDataFormat() failed" );
        return hr;
    }


    // Set the cooperativity level to let DirectInput know how this device
    // should interact with the system and with other DirectInput applications.
    // Use DISCL_NONEXCLUSIVE to Retrieve mouse data when acquired, not
    // interfering with any other applications which are reading mouse data.
    // Use DISCL_FOREGROUND so that if the user switches away from our app,
    // automatically release the mouse back to the system.
    if( FAILED( hr = g_pdidMouse->SetCooperativeLevel( hWnd,
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
    // Destroy the mouse object
    if( g_pdidMouse )
    {
        // Unacquire the device (just in case) before exitting.
        g_pdidMouse->Unacquire();

        g_pdidMouse->Release();
        g_pdidMouse = NULL;
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
// Desc: The game plays here. Read mouse data and displaying it.
//-----------------------------------------------------------------------------
VOID UpdateFrame( HWND hWnd )
{
    if( g_pdidMouse )
    {
        DIMOUSESTATE dims;          /* DirectInput mouse state structure */
        HRESULT hr;

        hr = g_pdidMouse->GetDeviceState( sizeof(DIMOUSESTATE), &dims );
        
        if( hr == DIERR_INPUTLOST )
        {
            // DInput is telling us that the input stream has been interrupted.
            // We aren't tracking any state between polls, so just re-acquire
            // and try again.
            hr = g_pdidMouse->Acquire();
            if( SUCCEEDED(hr) ) 
                UpdateFrame( hWnd );
        }

        if( SUCCEEDED(hr) )
        {
            CHAR strBuf[1024];

            // Build status string of mouse coordinates and button status
            wsprintf( strBuf, "Mouse state: (%d, %d, %d) %c %c %c %c",
                      dims.lX, dims.lY, dims.lZ,
                      (dims.rgbButtons[0] & 0x80) ? '0' : ' ',
                      (dims.rgbButtons[1] & 0x80) ? '1' : ' ',
                      (dims.rgbButtons[2] & 0x80) ? '2' : ' ',
                      (dims.rgbButtons[3] & 0x80) ? '3' : ' ' );

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
            // (if unpaused) want non-exclusive access to the mouse.
            g_bPaused = ( wParam == WA_INACTIVE ) ? TRUE : FALSE;

            if( g_pdidMouse )
            {
                if( g_bPaused )
                    g_pdidMouse->Unacquire();
                else
                    g_pdidMouse->Acquire();
            }
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
    HWND hWnd = CreateWindow( "DI Window", "DIEx1 - Non-exclusive mouse access",
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
