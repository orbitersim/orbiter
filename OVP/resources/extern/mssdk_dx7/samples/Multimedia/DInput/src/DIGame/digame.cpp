//-----------------------------------------------------------------------------
// Name: DIGame.cpp
//
// Desc: Demonstrates a little game which uses DirectInput. This file contains
//       all the DInput code. The other file contains all the game play code.
//
// Copyright (c) 1998-1999 Microsoft Corp.  All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <dinput.h>
#include <mmsystem.h>
#include "resource.h"




//-----------------------------------------------------------------------------
// Global variables and functions
//-----------------------------------------------------------------------------
#define NUM_ROWS       8
#define WINDOW_WIDTH   320
#define WINDOW_HEIGHT  ((NUM_ROWS+1)*40+25)
#define WM_GAMEOVER    (WM_USER+1)
#define WM_SYNCACQUIRE (WM_USER+2)

LPDIRECTINPUT7       g_pDI;            // The DInput object
LPDIRECTINPUTDEVICE2 g_pdidDevice2;    // The DIDevice2 interface
GUID                 g_guidJoystick;   // The GUID for the joystick
BOOL                 g_bActive;        // Whether app is active
BOOL                 g_bGamePlaying;   // Whether app is playing
FLOAT                g_fCurrentTime;

BOOL g_bUseKeyboard = TRUE;
BOOL g_bUseMouse    = FALSE;
BOOL g_bUseJoystick = FALSE;

VOID InitializeGameObjects( HINSTANCE hInst, HWND hWnd );
VOID DeleteGameObjects();
VOID StartNewGame();
VOID PauseGame( BOOL );
VOID MovePlayer( BOOL bLeft, BOOL bRight, BOOL bTop, BOOL bDown, BOOL bFire );
VOID AnimateScene( HWND hWnd );
VOID RenderScene( HWND hWnd );




//-----------------------------------------------------------------------------
// Name: DisplayError()
// Desc: Displays an error message.
//-----------------------------------------------------------------------------
VOID DisplayError( CHAR* strMessage )
{
    MessageBox( NULL, strMessage, "DInput Sample Game", MB_OK );
}




//-----------------------------------------------------------------------------
// Name: EnumJoysticksCallback()
// Desc: Called once for each enumerated joystick. If we find one, 
//       create a device interface on it so we can play with it.
//-----------------------------------------------------------------------------
BOOL CALLBACK EnumJoysticksCallback( LPCDIDEVICEINSTANCE pInst, 
                                     VOID* pvContext )
{
    memcpy( pvContext, &pInst->guidInstance, sizeof(GUID) );

    return DIENUM_STOP;
}




//-----------------------------------------------------------------------------
// Name: CreateDInput()
// Desc: Initialize the DirectInput objects.
//-----------------------------------------------------------------------------
HRESULT CreateDInput( HWND hWnd )
{
    // Create the main DirectInput object
    if( FAILED( DirectInputCreateEx( (HINSTANCE)GetWindowLong( hWnd, GWL_HINSTANCE ),
                                   DIRECTINPUT_VERSION, IID_IDirectInput7, (LPVOID*) &g_pDI, NULL) ) )
    {
        DisplayError( "DirectInputCreate() failed." );
        return E_FAIL;
    }

    // Check to see if a joystick is present. If so, the enumeration callback
    // will save the joystick's GUID, so we can create it later.
    ZeroMemory( &g_guidJoystick, sizeof(GUID) );
    
    g_pDI->EnumDevices( DIDEVTYPE_JOYSTICK, EnumJoysticksCallback,
                        &g_guidJoystick, DIEDFL_ATTACHEDONLY );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateInputDevice()
// Desc: Create a DirectInput device.
//-----------------------------------------------------------------------------
HRESULT CreateInputDevice( HWND hWnd, GUID guidDevice,
                           const DIDATAFORMAT* pdidDataFormat, DWORD dwFlags )
{
    // Obtain an interface to the input device
    if( FAILED( g_pDI->CreateDeviceEx( guidDevice, IID_IDirectInputDevice2,
		                               (VOID**)&g_pdidDevice2, NULL ) ) )
    {
        DisplayError( "CreateDeviceEx() failed" );
        return E_FAIL;
    }


    // Set the device data format. Note: a data format specifies which
    // controls on a device we are interested in, and how they should be
    // reported.
    if( FAILED( g_pdidDevice2->SetDataFormat( pdidDataFormat ) ) )
    {
        DisplayError( "SetDataFormat() failed" );
        return E_FAIL;
    }

    // Set the cooperativity level to let DirectInput know how this device
    // should interact with the system and with other DirectInput applications.
    if( FAILED( g_pdidDevice2->SetCooperativeLevel( hWnd, dwFlags ) ) )
    {
        DisplayError( "SetCooperativeLevel() failed" );
        return E_FAIL;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DestroyInputDevice()
// Desc: Release the DirectInput device
//-----------------------------------------------------------------------------
VOID DestroyInputDevice()
{
    // Unacquire and release the device's interfaces
    if( g_pdidDevice2 )
    {
        g_pdidDevice2->Unacquire();
        g_pdidDevice2->Release();
        g_pdidDevice2 = NULL;
    }
        
}




//-----------------------------------------------------------------------------
// Name: DestroyDInput()
// Desc: Terminate our usage of DirectInput
//-----------------------------------------------------------------------------
VOID DestroyDInput()
{
    // Destroy the DInput object
    if( g_pDI )
        g_pDI->Release();
    g_pDI = NULL;
}




//-----------------------------------------------------------------------------
// Name: UpdateFrame()
// Desc: The game plays here. Use DInput to get input. Move the player's ship
//       accordingly, update the weapons' positions and handle collisions.
//-----------------------------------------------------------------------------
VOID UpdateFrame( HWND hWnd )
{
    g_fCurrentTime = timeGetTime() * 0.001f;

    if( g_pdidDevice2 ) 
    {
        HRESULT      hr;
        BYTE         diks[256]; // DInput keyboard state buffer
        DIMOUSESTATE dims;      // DInput mouse state structure
        DIJOYSTATE   dijs;      // DInput joystick state structure

        // Poll the device before reading the current state. This is required
        // for some devices (joysticks) but has no effect for others (keyboard
        // and mice). Note: this uses a DIDevice2 interface for the device.
        if( FAILED( g_pdidDevice2->Poll() ) )
            return;

        if( g_bUseKeyboard ) // Get the keyboard state
            hr = g_pdidDevice2->GetDeviceState( sizeof(diks), &diks );
        
        if( g_bUseMouse )    // Else, get the mouse state
            hr = g_pdidDevice2->GetDeviceState( sizeof(DIMOUSESTATE), &dims );
        
        if( g_bUseJoystick ) // Else, get the joystick state
            hr = g_pdidDevice2->GetDeviceState( sizeof(DIJOYSTATE), &dijs );

        // Check whether the input stream has been interrupted. If so,
        // re-acquire and try again.
        if( hr == DIERR_INPUTLOST )
        {
            hr = g_pdidDevice2->Acquire();
            if( SUCCEEDED(hr) )
                return;
        }

        // Read keyboard input
        if( g_bUseKeyboard )
        {
            // Update the variables for the player's ship
            MovePlayer( diks[DIK_LEFT] & 0x80, diks[DIK_RIGHT] & 0x80,
                        diks[DIK_UP] & 0x80,   diks[DIK_DOWN] & 0x80,
                        diks[DIK_SPACE] & 0x80 );
        }

        // Read mouse input
        if( g_bUseMouse )
        {
            // Note: on really fast computers, the mouse will appear to be 
            // still most of the time, and move in jumps. To combat this, we'll
            // keep 0.1 seconds of persistence for any up/down values we read.
            static FLOAT fUpTime = 0.0f;
            static FLOAT fDnTime = 0.0f;
            if( dims.lY < 0 ) fDnTime = 0.0f, fUpTime = g_fCurrentTime+0.1f;
            if( dims.lY > 0 ) fUpTime = 0.0f, fDnTime = g_fCurrentTime+0.1f;

            MovePlayer( dims.lX<0, dims.lX>0, fUpTime-g_fCurrentTime > 0.0f, 
                        fDnTime-g_fCurrentTime > 0.0f, dims.rgbButtons[0] & 0x80 );
        }

        // Read joystick input
        if( g_bUseJoystick )
        {
            // Update the variables for the player's ship
            MovePlayer( dijs.lX<0, dijs.lX>0, dijs.lY<0, dijs.lY>0,
                        dijs.rgbButtons[0] & 0x80 );
        }
    }

    AnimateScene( hWnd );
    RenderScene( hWnd );
    return;
}




//-----------------------------------------------------------------------------
// Name: InputDeviceSelectProc()
// Desc: Dialog procedure for selecting an input device.
//-----------------------------------------------------------------------------
BOOL CALLBACK InputDeviceSelectProc( HWND hWnd, UINT msg, WPARAM wParam,
                                     LPARAM lParam )
{
    HWND hwndKeyboardButton = GetDlgItem( hWnd, IDC_KEYBOARD );
    HWND hwndMouseButton    = GetDlgItem( hWnd, IDC_MOUSE );
    HWND hwndJoystickButton = GetDlgItem( hWnd, IDC_JOYSTICK );

    if( WM_INITDIALOG == msg )
    {
        SendMessage( hwndKeyboardButton, BM_SETCHECK, g_bUseKeyboard, 0L);
        SendMessage( hwndMouseButton,    BM_SETCHECK, g_bUseMouse,    0L);
        SendMessage( hwndJoystickButton, BM_SETCHECK, g_bUseJoystick, 0L);
        
        EnableWindow( hwndJoystickButton, (g_guidJoystick != GUID_NULL) );

        return TRUE;
    }

    if( WM_COMMAND == msg && IDOK == LOWORD(wParam) )
    {
        // Destroy the old device
        DestroyInputDevice();

        // Check the dialog controls to see which device type to create
        g_bUseKeyboard = SendMessage( hwndKeyboardButton, BM_GETCHECK, 0, 0L);
        g_bUseMouse    = SendMessage( hwndMouseButton,    BM_GETCHECK, 0, 0L);
        g_bUseJoystick = SendMessage( hwndJoystickButton, BM_GETCHECK, 0, 0L);
    
        if( g_bUseKeyboard )
        {
            CreateInputDevice( GetParent(hWnd), GUID_SysKeyboard, &c_dfDIKeyboard,
                               DISCL_NONEXCLUSIVE|DISCL_FOREGROUND );
        }
        else if( g_bUseMouse )
        {
            CreateInputDevice( GetParent(hWnd), GUID_SysMouse, &c_dfDIMouse,
                               DISCL_EXCLUSIVE|DISCL_FOREGROUND );
        }
        else if( g_bUseJoystick )
        {
            CreateInputDevice( GetParent(hWnd), g_guidJoystick, &c_dfDIJoystick,
                               DISCL_EXCLUSIVE|DISCL_FOREGROUND );

            // Set the range of the joystick axes tp [-1000,+1000]
            DIPROPRANGE diprg; 
            diprg.diph.dwSize       = sizeof(DIPROPRANGE); 
            diprg.diph.dwHeaderSize = sizeof(DIPROPHEADER); 
            diprg.diph.dwHow        = DIPH_BYOFFSET; 
            diprg.lMin              = -10; 
            diprg.lMax              = +10; 

            diprg.diph.dwObj = DIJOFS_X;    // Set the x-axis range
            g_pdidDevice2->SetProperty( DIPROP_RANGE, &diprg.diph );

            diprg.diph.dwObj = DIJOFS_Y;    // Set the y-axis range
            g_pdidDevice2->SetProperty( DIPROP_RANGE, &diprg.diph );

            // Set the dead zone for the joystick axes (because many joysticks
            // aren't perfectly calibrated to be zero when centered).
            DIPROPDWORD dipdw; 
            dipdw.diph.dwSize       = sizeof(DIPROPDWORD); 
            dipdw.diph.dwHeaderSize = sizeof(DIPROPHEADER); 
            dipdw.diph.dwHow        = DIPH_DEVICE; 
            dipdw.dwData            = 1000; // Here, 1000 = 10%

            dipdw.diph.dwObj = DIJOFS_X;    // Set the x-axis deadzone
            g_pdidDevice2->SetProperty( DIPROP_DEADZONE, &dipdw.diph );

            dipdw.diph.dwObj = DIJOFS_Y;    // Set the y-axis deadzone
            g_pdidDevice2->SetProperty( DIPROP_RANGE, &dipdw.diph );
        }

        EndDialog( hWnd, IDOK );
        return TRUE;
    }

    if( WM_COMMAND == msg && IDCANCEL == LOWORD(wParam) )
    {
        EndDialog( hWnd, IDCANCEL );
        return TRUE;
    }

    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: WndProc()
// Desc: Window procedure for the game window
//-----------------------------------------------------------------------------
LRESULT CALLBACK WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg )
    {
        case WM_ACTIVATE:
            // Pause or unpause the app (and acquire or unacquire the device)
            // based on whether we are losing or gaining activation.
            g_bActive = ( WA_INACTIVE != wParam );
            PauseGame( !g_bActive );
            PostMessage( hWnd, WM_SYNCACQUIRE, 0, 0 );
            break;

        case WM_ENTERMENULOOP:
            g_bActive = FALSE;
            PauseGame( TRUE );
            PostMessage( hWnd, WM_SYNCACQUIRE, 0, 0 );
            break;

        case WM_EXITMENULOOP:
            if( ( GetActiveWindow() == hWnd ) && ( FALSE == IsIconic( hWnd ) ) )
                g_bActive = TRUE;
            PauseGame( FALSE );
            PostMessage( hWnd, WM_SYNCACQUIRE, 0, 0 );
            break;

        case WM_PAINT:
            RenderScene( hWnd );
            break;

        case WM_SYNCACQUIRE:
            if( g_pdidDevice2 )
            {
                if( g_bActive && g_bGamePlaying)
                    g_pdidDevice2->Acquire();
                else
                    g_pdidDevice2->Unacquire();
            }
            break;

        case WM_GAMEOVER:
            g_bGamePlaying = FALSE;
            PostMessage( hWnd, WM_SYNCACQUIRE, 0, 0 );
            break;

        case WM_COMMAND:
            switch( LOWORD(wParam) )
            {
                case IDM_NEWGAME:
                    StartNewGame();
                    g_bGamePlaying = TRUE;
                    PostMessage( hWnd, WM_SYNCACQUIRE, 0, 0 );
                    break;

                case IDM_SELECTINPUTDEVICE:
                    DialogBox( (HINSTANCE)GetWindowLong( hWnd, GWL_HINSTANCE ),
                               MAKEINTRESOURCE(IDD_SELECTINPUTDEVICE), hWnd,
                               (DLGPROC)InputDeviceSelectProc );
                    break;
                
                case IDM_EXIT:
                    SendMessage( hWnd, WM_CLOSE, 0, 0 );
                    return 0;
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
                    LoadIcon( hInst, MAKEINTRESOURCE(IDI_DIRECTX_ICON)),
                    LoadCursor(NULL, IDC_ARROW), NULL, MAKEINTRESOURCE(IDR_MENU), 
                    TEXT("DI Window") };
    if( !RegisterClass( &wc ) )
        return FALSE;

    // Create the main window
    HWND hWnd = CreateWindow( "DI Window", "GameInput - Simple Game Using DInput",
                              WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX,
                              CW_USEDEFAULT, CW_USEDEFAULT,
                              WINDOW_WIDTH, WINDOW_HEIGHT, NULL, NULL, hInst, 0 );

    // Create the DInput object
    if( FAILED( CreateDInput( hWnd ) ) )
    {
        DestroyWindow( hWnd );
        return FALSE;
    }

    // Create a keyboard device
    if( FAILED( CreateInputDevice( hWnd, GUID_SysKeyboard, &c_dfDIKeyboard,
                                   DISCL_NONEXCLUSIVE | DISCL_FOREGROUND ) ) )
    {
        DestroyWindow( hWnd );
        return FALSE;
    }

    // Initialize and start a new game
    ShowWindow( hWnd, nCmdShow );
    InitializeGameObjects( hInst, hWnd );
    StartNewGame();
    g_bGamePlaying = TRUE;

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
        else if( g_bActive && g_bGamePlaying )
        {
            // Play the game (check user input and update the window)
            UpdateFrame( hWnd );
        }
        else
            WaitMessage();
    }

    // Cleanup before exitting
    DeleteGameObjects();
    DestroyInputDevice();
    DestroyDInput();

    return TRUE;
}



