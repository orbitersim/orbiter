//-----------------------------------------------------------------------------
// Name: DIEx4.cpp
//
// Desc: DirectInput simple sample 3
//       Demonstrates an application which receives buffered keyboard data
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
#define DINPUT_BUFFERSIZE       16    // Number of buffer elements

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
    // reported. This tells DirectInput that we are interested in all keys on
    // the device, and they should be reported as DirectInput DIK_* codes.
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

    // IMPORTANT STEP IF YOU WANT TO USE BUFFERED DEVICE DATA!
    //
    // DirectInput uses unbuffered I/O (buffer size = 0) by default. If you
    // want to read buffered data, you need to set a nonzero buffer size. Set
    // the buffer size to DINPUT_BUFFERSIZE (defined above) elements.
    DIPROPDWORD dipdw = 
    { 
        {
            sizeof(DIPROPDWORD),  // diph.dwSize
            sizeof(DIPROPHEADER), // diph.dwHeaderSize
            0,                    // diph.dwObj
            DIPH_DEVICE,          // diph.dwHow
        },
        DINPUT_BUFFERSIZE,        // dwData
    };

    if( FAILED( hr = g_pdidKeyboard->SetProperty( DIPROP_BUFFERSIZE,
                                                  &dipdw.diph ) ) )
    {
        DisplayError( hr, "SetProperty() failed" );
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
        DIDEVICEOBJECTDATA rgod[DINPUT_BUFFERSIZE]; // Receives buffered data
        DWORD   cod;
        HRESULT hr;

        cod = DINPUT_BUFFERSIZE;
        hr = g_pdidKeyboard->GetDeviceData( sizeof(DIDEVICEOBJECTDATA),
                                            rgod, &cod, 0 );
        if( FAILED( hr ) )
        {
            // We got an error or we got DI_BUFFEROVERFLOW. Either way, it
            // means that continuous contact with the device has been lost,
            // either due to an external interruption, or because the buffer
            // overflowed and some events were lost.
            //
            // Consequently, if a button was pressed at the time the buffer
            // overflowed or the connection was broken, the corresponding "up"
            // message might have been lost.
            //
            // But since our simple sample doesn't actually have any state
            // associated with button up or down events, there is no state to
            // reset. (In a real game, ignoring the buffer overflow would
            // result in the game thinking a key was held down.)
            //
            // A more clever approach would be to call GetDeviceState() and
            // compare the current state against the state we think the device
            // is in, and process all the states that are currently different
            // from our private state.

            if( hr == DIERR_INPUTLOST )
            {
                hr = g_pdidKeyboard->Acquire();
                if( SUCCEEDED(hr) )
                    UpdateFrame( hWnd );
            }
        }

        // In order for it to be worth our while to parse the buffer elements,
        // the GetDeviceData() call must have succeeded, and we must have
        // received some data.
        if( SUCCEEDED(hr) && cod > 0 )
        {
            CHAR  strBuf[1024];

            // Process each of the buffer elements
            for( DWORD iod=0; iod<cod; iod++ )
            {
                wsprintf( strBuf, "%02x was %s", rgod[iod].dwOfs,
                          (rgod[iod].dwData & 0x80) ? "pressed" : "released" );
            }

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
    HWND hWnd = CreateWindow( "DI Window", "DIEx4 - Buffering keyboard data",
                              WS_OVERLAPPEDWINDOW, CW_USEDEFAULT,
                              CW_USEDEFAULT, 300, 100, NULL, NULL, hInst, 0 );

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



