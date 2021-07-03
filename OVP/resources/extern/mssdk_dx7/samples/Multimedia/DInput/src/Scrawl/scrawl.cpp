//-----------------------------------------------------------------------------
// File: Scrawl.cpp
//
// Desc: Demonstrates an application which receives relative mouse data
//       in non-exclusive mode via a dialog timer.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define INITGUID
#include "Scrawl.h"
#include "resource.h"

#define BUFFER_SIZE           16

//-----------------------------------------------------------------------------
// Local function prototypes 
//-----------------------------------------------------------------------------
HRESULT InitDirectInput( HWND hWnd );
HRESULT SetAcquire();
HRESULT FreeDirectInput();
void OnMouseInput( HWND hWnd );
void OnLeftButtonDown( HWND hWnd );
void OnRightButtonUp( HWND hWnd );

//-----------------------------------------------------------------------------
// Global variables for the DirectMusic sample 
//-----------------------------------------------------------------------------
IDirectInput*           g_pDI           = NULL;
IDirectInputDevice*     g_pMouse        = NULL;
HANDLE                  g_hMouseEvent   = NULL;

HINSTANCE               g_hInst         = NULL;
BOOL                    g_bActive       = TRUE;
BOOL                    g_bSwapMouseButtons;




//-----------------------------------------------------------------------------
// Function: InitDirectInput
//
// Description: 
//      Initialize the DirectInput variables.
//
//-----------------------------------------------------------------------------
HRESULT InitDirectInput( HWND hWnd )
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
    hr = g_pMouse->SetCooperativeLevel( hWnd, 
                                        DISCL_EXCLUSIVE | DISCL_FOREGROUND);
    if ( FAILED(hr) ) 
        return hr;

    // create a win32 event which is signaled when mouse data is availible
    g_hMouseEvent = CreateEvent( NULL, FALSE, FALSE, NULL );
    if ( NULL == g_hMouseEvent )
        return E_FAIL;

    // give the event to the mouse device
    hr = g_pMouse->SetEventNotification( g_hMouseEvent );
    if ( FAILED(hr) ) 
        return hr;

    // setup the buffer size for the mouse data
    DIPROPDWORD dipdw;

    dipdw.diph.dwSize = sizeof(DIPROPDWORD);
    dipdw.diph.dwHeaderSize = sizeof(DIPROPHEADER);
    dipdw.diph.dwObj = 0;
    dipdw.diph.dwHow = DIPH_DEVICE;
    dipdw.dwData = BUFFER_SIZE;

    hr = g_pMouse->SetProperty( DIPROP_BUFFERSIZE, &dipdw.diph );
    if ( FAILED(hr) ) 
        return hr;

    // not necessary, but nice for left handed users that have
    // their swapped mouse buttons
    g_bSwapMouseButtons = GetSystemMetrics( SM_SWAPBUTTON );

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
HRESULT SetAcquire()
{
    // nothing to do if g_pMouse is NULL
    if (NULL == g_pMouse)
        return S_FALSE;

    if (g_bActive) 
    {
        g_pMouse->Acquire();
    } 
    else 
    {
        g_pMouse->Unacquire();
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
    if ( NULL != g_pMouse ) 
    {
        // Unacquire the device one last time just in case 
        // the app tried to exit while the device is still acquired.
        g_pMouse->Unacquire();

        g_pMouse->Release();
        g_pMouse = NULL;
    }

    // Release any DirectInput objects.
    if ( NULL != g_pDI )  
    {
        g_pDI->Release();
        g_pDI = NULL;
    }

    // Close event handle
    if ( NULL != g_hMouseEvent )
    {
        CloseHandle( g_hMouseEvent );
        g_hMouseEvent = NULL;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Function: OnMouseInput
//
// Description: 
//      Handles responding to any mouse input that is generated from
//      the mouse event being triggered.
//
//-----------------------------------------------------------------------------
void OnMouseInput( HWND hWnd )
{
    BOOL                bDone;
    DIDEVICEOBJECTDATA  od;
    DWORD               dwElements;
    HRESULT             hr;

    // Invalidate the old cursor so it will be erased 
    InvalidateCursorRect( hWnd );

    // Attempt to read one data element.  Continue as long as
    // device data is available.
    bDone = FALSE;

    while ( !bDone ) 
    {
        dwElements = 1;
        hr = g_pMouse->GetDeviceData( 
                            sizeof(DIDEVICEOBJECTDATA), 
                            &od,
                            &dwElements, 
                            0 );

        if (hr == DIERR_INPUTLOST) 
        {
            SetAcquire();
            break;
        }

        // Unable to read data or no data available
        if ( FAILED(hr) || dwElements == 0) 
        {
            break;
        }

        // look at the element to see what happened
        switch (od.dwOfs) 
        {     
            case DIMOFS_X:       // Mouse horizontal motion 
                UpdateCursorPosition( od.dwData, 0 ); 
                break;

            case DIMOFS_Y:       // Mouse vertical motion 
                UpdateCursorPosition( 0, od.dwData ); 
                break;

        
            case DIMOFS_BUTTON0: // Right button pressed or released 
            case DIMOFS_BUTTON1: // Left button pressed or released 

                // is the right or a swapped left button down?
                if ( ( g_bSwapMouseButtons  && DIMOFS_BUTTON1 == od.dwOfs ) ||
                     ( !g_bSwapMouseButtons && DIMOFS_BUTTON0 == od.dwOfs ) )
                {
                    if ( od.dwData & 0x80 ) 
                    { 
                        // left button pressed, so go into button-down mode 
                        bDone = TRUE;
                        OnLeftButtonDown( hWnd ); 
                    }
                }

                // is the left or a swapped right button down?
                if ( ( g_bSwapMouseButtons  && DIMOFS_BUTTON0 == od.dwOfs ) ||
                     ( !g_bSwapMouseButtons && DIMOFS_BUTTON1 == od.dwOfs ) )
                {
                    if ( !(od.dwData & 0x80) ) 
                    {  
                        // button released, so check context menu 
                        bDone = TRUE;
                        OnRightButtonUp( hWnd ); 
                    }
                }
                break;
        }
    }

    // invalidate the new cursor so it will be drawn 
    InvalidateCursorRect( hWnd );
}



//-----------------------------------------------------------------------------
// Function: OnLeftButtonDown
//
// Description: 
//      If we are drawing a curve, then read buffered data and draw
//      lines from point to point.  By reading buffered data, we can
//      track the motion of the mouse accurately without coalescing.
//
//      This function illustrates how a non-message-based program can
//      process buffered data directly from a device, processing
//      messages only occasionally (as required by Windows).
//
//      This function also illustrates how an application can piece
//      together buffered data elements based on the sequence number.
//      A single mouse action (e.g., moving diagonally) is reported
//      as a series of events, all with the same sequence number.
//      Zero is never a valid DirectInput sequence number, so it is
//      safe to use it as a sentinel value.
//
//-----------------------------------------------------------------------------
void OnLeftButtonDown( HWND hWnd )
{
    HRESULT             hr;
    LEFTBUTTONINFO      lbInfo;
    BOOL                bDone;
    DIDEVICEOBJECTDATA  od;
    DWORD               dwElements;
    MSG                 msg;

    // For performance, draw directly onto the window's DC instead of
    // invalidating and waiting for the WM_PAINT message.  Of course,
    // we always draw onto our bitmap, too, since that's what really
    // counts.

    // hide cursor and initialize button info with cursor position
    StartPenDraw( hWnd, &lbInfo );
    InvalidateCursorRect( hWnd );
    UpdateWindow( hWnd );

    // keep reading data elements until we see a "mouse button up" event.
    bDone = FALSE;
    while ( !bDone ) 
    {
        dwElements = 1;
        hr = g_pMouse->GetDeviceData(
                            sizeof(DIDEVICEOBJECTDATA), 
                            &od,
                            &dwElements, 
                            0 );
        if ( FAILED(hr) )       
        {
            break; // unable to read data, so break out
        }

        // if theres no data available, finish the element 
        // we have been collecting, and then process our message 
        // queue so the system doesn't think the app has hung.
        if ( dwElements == 0 ) 
        {
            // if there is a partial motion, flush it out 
            OnLeftButtonDown_FlushMotion( &lbInfo );

            while ( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) ) 
            {
                // if it's a quit message, we're outta here 
                if ( msg.message == WM_QUIT ) 
                {
                    // Re-post the quit message so the
                    // outer loop will see it and exit.
                    PostQuitMessage( msg.wParam );
                    bDone = TRUE;                    
                    break;
                } 
                else 
                {
                    TranslateMessage( &msg );
                    DispatchMessage( &msg );
                }
            }
            continue;
        }

        // if this is the start of a new event, flush out the old one 
        if ( od.dwSequence != lbInfo.dwSeqLastSeen ) 
        {
            OnLeftButtonDown_FlushMotion( &lbInfo );
            lbInfo.dwSeqLastSeen = od.dwSequence;
        }

        // look at the element to see what happened 
        switch ( od.dwOfs ) 
        {
            case DIMOFS_X:      // Mouse horizontal motion 
                UpdateCursorPosition( od.dwData, 0 );
                lbInfo.bMoved = TRUE;
                break;

            case DIMOFS_Y:      // Mouse vertical motion 
                UpdateCursorPosition( 0, od.dwData );
                lbInfo.bMoved = TRUE;
                break;

            case DIMOFS_BUTTON0: // Button 0 pressed or released 
            case DIMOFS_BUTTON1: // Button 1 pressed or released 
                if ( ( g_bSwapMouseButtons  && DIMOFS_BUTTON1 == od.dwOfs ) ||
                     ( !g_bSwapMouseButtons && DIMOFS_BUTTON0 == od.dwOfs ) )
                {
                    if ( !(od.dwData & 0x80) ) 
                    { 
                        // button released, so flush out dregs 
                        bDone = TRUE;
                        OnLeftButtonDown_FlushMotion( &lbInfo ); 
                    }
                }
                break;
        }
    }

    ReleaseDC( hWnd, lbInfo.hdcWindow );

    // re-show the cursor now that scrawling is finished 
    FinishPenDraw( hWnd );
    InvalidateCursorRect( hWnd );
}

