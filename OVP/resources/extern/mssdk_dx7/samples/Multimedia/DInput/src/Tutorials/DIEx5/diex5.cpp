//-----------------------------------------------------------------------------
// Name: DIEx5.cpp
//
// Desc: DirectInput simple sample 5
//       Demonstrates an application which retrieves buffered HID data
//       in non-exclusive mode via a blocking loop.
//
// Copyright (C) 1997-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <dinput.h>

#define DIMAKEUSAGEDWORD(UsagePage, Usage) (DWORD)MAKELONG(Usage, UsagePage)
#define DIDOI_GUIDISUSAGE       0x00010000




//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
#define DINPUT_BUFFERSIZE       16    // Number of buffer elements
LPDIRECTINPUT        g_pDI;
LPDIRECTINPUTDEVICE  g_pDIDevice;
HANDLE               g_hEvent;
CHAR                 g_strText[1024]; // What we display in client area




//-----------------------------------------------------------------------------
// Name: struct VCRDATA
// Desc: DInput custom data format used to read VCR data. It merely consists of
// four buttons. Note: the size of this structure must be a multiple of four
// (which it is...otherwise, padding would be necessary.)
//-----------------------------------------------------------------------------

#define VCROFS_PLAY     FIELD_OFFSET( VCRDATA, bPlay )
#define VCROFS_STOP     FIELD_OFFSET( VCRDATA, bStop )
#define VCROFS_REWIND   FIELD_OFFSET( VCRDATA, bRewind )
#define VCROFS_FFORWARD FIELD_OFFSET( VCRDATA, bFForward )

struct VCRDATA
{
    BYTE   bPlay;
    BYTE   bStop;
    BYTE   bRewind;
    BYTE   bFForward;
};




//-----------------------------------------------------------------------------
// Name: c_rgodfVCR
// Desc: Array of DirectInput custom data formats, describing each field in our
//       VCRDATA structure. Each row in the array consists of four fields:
//       1. (const GUID *)DIMAKEUSAGEDWORD(UsagePage, Usage)
//          The usage page and usage of the control we desire.
//          The DIDOI_GUIDISUSAGE flag in the dwFlags indicates that
//          the first field is really a DIMAKEUSAGEDWORD instead
//          of a pointer to a GUID.
//       2. VCROFS_*
//          The data offset of the item.  This information is used
//          to identify the control after the data format is selected.
//       3. DIDFT_BUTTON | DIDFT_ANYINSTANCE
//          The data format object type filter.  Here, we say that
//          we want a button, but we don't care which button.  The
//          usage page and usage information will choose the right
//          button for us.
//       4. DIDOI_GUIDISUSAGE
//          The first field of the DIOBJECTDATAFORMAT should be
//          interpreted as a usage page and usage, rather than a GUID.
//-----------------------------------------------------------------------------

// These magic numbers come from the USB HID Specification.
#define HID_USAGE_PAGE_CONSUMER     0x000C // Consumer controls
#define HID_USAGE_CONSUMER_PLAY     0x00B0 // Play
#define HID_USAGE_CONSUMER_STOP     0x00B7 // Stop
#define HID_USAGE_CONSUMER_REWIND   0x00B4 // Rewind
#define HID_USAGE_CONSUMER_FFORWARD 0x00B3 // Fast Forward

DIOBJECTDATAFORMAT c_rgodfVCR[4] =
{
    // Play button
    {
        (const GUID *)DIMAKEUSAGEDWORD(HID_USAGE_PAGE_CONSUMER,
                                       HID_USAGE_CONSUMER_PLAY),
        VCROFS_PLAY,
        DIDFT_BUTTON | DIDFT_ANYINSTANCE,
        DIDOI_GUIDISUSAGE,
    },

    // Stop button
    {
        (const GUID *)DIMAKEUSAGEDWORD(HID_USAGE_PAGE_CONSUMER,
                                       HID_USAGE_CONSUMER_STOP),
        VCROFS_STOP,
        DIDFT_BUTTON | DIDFT_ANYINSTANCE,
        DIDOI_GUIDISUSAGE,
    },

    // Rewind button
    {
        (const GUID *)DIMAKEUSAGEDWORD(HID_USAGE_PAGE_CONSUMER,
                                       HID_USAGE_CONSUMER_REWIND),
        VCROFS_REWIND,
        DIDFT_BUTTON | DIDFT_ANYINSTANCE,
        DIDOI_GUIDISUSAGE,
    },

    // Fast Forward button
    {
        (const GUID *)DIMAKEUSAGEDWORD(HID_USAGE_PAGE_CONSUMER,
                                       HID_USAGE_CONSUMER_FFORWARD),
        VCROFS_FFORWARD,
        DIDFT_BUTTON | DIDFT_ANYINSTANCE,
        DIDOI_GUIDISUSAGE,
    },
};




//-----------------------------------------------------------------------------
// Name: c_dfVCR
// Desc: DirectInput custom data format which ties all this information
//       together into a DirectInput data format.
//-----------------------------------------------------------------------------
DIDATAFORMAT c_dfVCR =
{
    sizeof(DIDATAFORMAT),           // dwSize
    sizeof(DIOBJECTDATAFORMAT),     // dwObjSize
    0,                              // No special flags
    sizeof(VCRDATA),                // Size of data structure
    4,                              // Number of objects in data format
    c_rgodfVCR,                     // The objects themselves
};




//-----------------------------------------------------------------------------
// Name: DisplayError()
// Desc: Displays an error in a message box
//-----------------------------------------------------------------------------
VOID DisplayError( HRESULT hr, CHAR* strMessage )
{
    MessageBox( NULL, strMessage, "DirectInput Sample", MB_OK );
}




//-----------------------------------------------------------------------------
// Name: DITryCreatedDevice()
// Desc: Check if a created device meets our needs (we are looking for a VCR
//       device.)
//-----------------------------------------------------------------------------
BOOL DITryCreatedDevice( LPDIRECTINPUTDEVICE pDIDevice, HWND hWnd )
{
    HRESULT   hr;
    DIDEVCAPS caps;

    // Select our data format to see if this device supports the things we
    // request. A data format specifies which controls on a device we are
    // interested in, and how they should be reported. In this case, the
    // c_dfVCR data format specifies what we are looking for.
    if( FAILED( hr = pDIDevice->SetDataFormat( &c_dfVCR ) ) )
        return FALSE;

    // Query the device capabilities to see if it requires polling.
    // This appliction isn't written to support polling.  So, if the
    // device require it, then skip polling.
    //
    // Note that we set the caps.dwSize to sizeof(DIDEVCAPS_DX3)
    // because we don't particularly care about force feedback.
    //
    // Note that we check the DIDC_POLLEDDATAFORMAT flag instead of
    // the DIDC_POLLEDDEVICE flag.  We don't care whether the device
    // requires polling, we only care if the VCR controls require
    // polling.
    caps.dwSize = sizeof(DIDEVCAPS_DX3);

    if( FAILED( hr = pDIDevice->GetCapabilities( &caps ) ) )
        return FALSE;

    if( caps.dwFlags & DIDC_POLLEDDATAFORMAT )
        return FALSE;

    // Set the cooperative level to let DirectInput know how
    // this device should interact with the system and with other
    // DirectInput applications.
    if( FAILED( hr = pDIDevice->SetCooperativeLevel( hWnd,
                                       DISCL_BACKGROUND | DISCL_EXCLUSIVE ) ) )
        return FALSE;

    // DirectInput uses unbuffered I/O (buffer size = 0) by default.
    // If you want to read buffered data, you need to set a nonzero
    // buffer size.
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

    if( FAILED( hr = pDIDevice->SetProperty( DIPROP_BUFFERSIZE,
                                             &dipdw.diph ) ) )
        return FALSE;

    // We are not a game, we are just a little VCR app.  So
    // instead of going into a polling loop on the device,
    // use event notifications so we don't use any CPU until
    // the user actually presses a button.
    pDIDevice->SetEventNotification( g_hEvent);

    // The device met all the criteria. Return TRUE.
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: EnumDevicesCallback()
// Desc: Callback function called by EnumDevices(). Checks each device to see
//       if it meets our criteria (we are looking for a VCR device.)
//-----------------------------------------------------------------------------
BOOL CALLBACK EnumDevicesCallback( LPCDIDEVICEINSTANCE pInst, LPVOID pvRef )
{
    LPDIRECTINPUTDEVICE pDIDevice;
    HRESULT hr;
    HWND    hWnd = (HWND)pvRef; // Our refdata is the coop window

    // Create the device we enumerated so we can snoop at it to decide
    // if we like it or not.
    hr = g_pDI->CreateDevice( pInst->guidInstance, &pDIDevice, NULL );

    if( SUCCEEDED(hr) )
    {
        // Use our helper function to see if this is a device
        // that is useful to us.
        if( DITryCreatedDevice( pDIDevice, hWnd ) )
        {
            // Okay, we found a VCR control. Save the device pointer and bump
            // its refcount so it won't go away -- see below for the Release()
            // that this AddRef() counteracts. We've found a device so we can
            // stop enumerating. (Alternatively, we could keep enumerating and
            // put all accepted devices in a listbox.)
            g_pDIDevice = pDIDevice;
            return DIENUM_STOP;
        }

        // We didn't like the device, so destroy it
        pDIDevice->Release();
    }

    return DIENUM_CONTINUE;
}




//-----------------------------------------------------------------------------
// Name: CreateDInput()
// Desc: Initialize the DirectInput globals
//-----------------------------------------------------------------------------
HRESULT CreateDInput( HWND hWnd )
{
    HINSTANCE hInst = (HINSTANCE)GetWindowLong( hWnd, GWL_HINSTANCE );
    HRESULT   hr;

    // We use event notifications, so we'll need an event.
    if( NULL == ( g_hEvent = CreateEvent( NULL, FALSE, FALSE, NULL ) ) )
    {
        DisplayError( 0, "CreateEvent() failed" );
        return E_FAIL;
    }

    //  Register with the DirectInput subsystem and get a pointer to a
    // IDirectInput interface we can use.
    if( FAILED( hr = DirectInputCreate( hInst, DIRECTINPUT_VERSION, &g_pDI,
                                        NULL ) ) )
    {
        DisplayError( hr, "DirectInputCreate() failed");
        return hr;
    }

    // Enumerate all the devices in the system looking for one that we
    // like.
    if( FAILED( hr = g_pDI->EnumDevices( 0, EnumDevicesCallback, hWnd,
                                         DIEDFL_ATTACHEDONLY ) ) )
    {
        DisplayError( hr, "EnumDevices() failed");
        return hr;
    }

    // Complain if we couldn't find a VCR control.
    if( NULL == g_pDIDevice )
    {
        DisplayError( 0, "Couldn't find a VCR control" );
        return E_FAIL;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DestroyDInput()
// Desc: Terminate our usage of DirectInput
//-----------------------------------------------------------------------------
VOID DestroyDInput()
{
    // Destroy any lingering IDirectInputDevice object.
    if( g_pDIDevice )
    {
        // Unacquire the device
        g_pDIDevice->Unacquire();

        g_pDIDevice->Release();
        g_pDIDevice = NULL;
    }

    // Destroy any lingering IDirectInput object.
    if( g_pDI )
    {
        g_pDI->Release();
        g_pDI = NULL;
    }

    // Destroy the event we were using.
    if( g_hEvent )
    {
        CloseHandle( g_hEvent );
        g_hEvent = NULL;
    }
}




//-----------------------------------------------------------------------------
// Name: ReadVCRData()
// Desc: Read and display status of the VCR buttons
//-----------------------------------------------------------------------------
VOID ReadVCRData( HWND hWnd )
{
    DIDEVICEOBJECTDATA rgod[DINPUT_BUFFERSIZE]; // Receives buffered data
    DWORD   cod;
    HRESULT hr;

    cod = DINPUT_BUFFERSIZE;
    if( FAILED( hr = g_pDIDevice->GetDeviceData( sizeof(DIDEVICEOBJECTDATA),
                                                 rgod, &cod, 0 ) ) )
    {
        // We got an error or we got DI_BUFFEROVERFLOW. Either way, it means
        // that continuous contact with the device has been lost, either due
        // to an external interruption, or because the buffer overflowed and
        // some events were lost.
        //
        // Consequently, if a button was pressed at the time the buffer
        // overflowed or the connection was broken, the corresponding "up"
        // message might have been lost.
        //
        // But since our simple sample doesn't actually have any state
        // associated with button up or down events, there is no state to
        // reset. (In a real app, ignoring the buffer overflow would result in
        // the app thinking a key was held down.)
        //
        // It would be more clever to call GetDeviceState() and compare the
        // current state against the state we think the device is in, and
        // process all the states that are currently different from our
        // private state.

        if( hr == DIERR_INPUTLOST )
        {
            hr = g_pDIDevice->Acquire();
            if( SUCCEEDED(hr) ) 
                ReadVCRData( hWnd );
        }
    }

    // In order for it to be worth our while to parse the buffer elements, the
    // GetDeviceData() call must have succeeded, and we must have received some
    // data at all.
    if( SUCCEEDED(hr) && cod > 0 )
    {
        CHAR  strBuf[1024];

        // Process each of the buffer elements
        for( DWORD iod = 0; iod < cod; iod++ ) 
        {
            CHAR* strObj = NULL;

            switch( rgod[iod].dwOfs )
            {
                case VCROFS_PLAY:     strObj = "Play"; break;
                case VCROFS_STOP:     strObj = "Stop"; break;
                case VCROFS_REWIND:   strObj = "FF";   break;
                case VCROFS_FFORWARD: strObj = "Rew";  break;
            }

            if( strObj )
                wsprintf( strBuf, "%s was %s", strObj,
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




//-----------------------------------------------------------------------------
// Name: WndProc()
// Desc: Application's message handler
//-----------------------------------------------------------------------------
LRESULT CALLBACK WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg )
    {
        case WM_PAINT:
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

        case WM_DESTROY:
            PostQuitMessage(0);
            break;
    }

    return DefWindowProc( hWnd, msg, wParam, lParam );
}




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Application entry point.
//-----------------------------------------------------------------------------
int PASCAL WinMain( HINSTANCE hInst, HINSTANCE, LPSTR, int nCmdShow )
{
    // Set up the window class
    WNDCLASS wc = { CS_HREDRAW | CS_VREDRAW, WndProc, 0, 0, hInst,
                    LoadIcon( hInst, MAKEINTRESOURCE(IDI_APPLICATION)),
                    LoadCursor(NULL, IDC_ARROW), NULL, NULL, 
                    TEXT("DI Window") };

    if( !RegisterClass( &wc ) )
        return FALSE;

    // Create the main window
    HWND hWnd = CreateWindow( "DI Window", "DIEx5 - Accessing a HID",
                              WS_OVERLAPPEDWINDOW, CW_USEDEFAULT,
                              CW_USEDEFAULT, 300, 100, NULL, NULL, hInst, 0 );

    if( FAILED( CreateDInput( hWnd ) ) )
    {
        DestroyWindow( hWnd );
        return FALSE;
    }

    ShowWindow( hWnd, nCmdShow );

    // Everything is all set up.  Acquire the device and have at it.
    if( FAILED( g_pDIDevice->Acquire() ) )
    {
        // Unable to obtain access to the device.  Some other
        // application probably has it in exclusive mode.
        DisplayError( E_FAIL, "Cannot acquire device" );
        DestroyDInput();
        return FALSE;
    }

    // Standard game loop.
    while( TRUE )
    {
        MSG msg;

        if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
        {
            // If it's a quit message, then we're done.
            if( msg.message == WM_QUIT )
                break;

            // Otherwise translate and dispatch it
            TranslateMessage( &msg );
            DispatchMessage( &msg );
        }

        // Wait for a message or for our event.
        DWORD dwRc = MsgWaitForMultipleObjects( 1, &g_hEvent, FALSE,
                                                INFINITE, QS_ALLINPUT );
        
        // dwRc is WAIT_OBJECT_0 if we have new input.
        if( dwRc == WAIT_OBJECT_0 )
            ReadVCRData( hWnd );
    }

    DestroyDInput();

    return TRUE;
}



