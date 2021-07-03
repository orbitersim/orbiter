//-----------------------------------------------------------------------------
// File: DIUtil.cpp
//
// Desc: Input routines
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include "duel.h"
#include "diutil.h"
#include "gameproc.h"


//-----------------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------------
static LPDIRECTINPUT       g_pDI;               // DirectInput interface
static LPDIRECTINPUTDEVICE g_pdidKeyboard;      // Keyboard device interface
static BOOL                g_bKeyboardAcquired; // Whether eyboard is acquired




//-----------------------------------------------------------------------------
// Name: DIUtil_InitInput()
// Desc: Initialize DirectInput objects & devices
//-----------------------------------------------------------------------------
HRESULT DIUtil_InitInput( HWND hWnd )
{
    // Create DI object
    if( FAILED( DirectInputCreate( (HINSTANCE)GetWindowLong( hWnd, GWL_HINSTANCE ),
                                   DIRECTINPUT_VERSION, &g_pDI, NULL ) ) )
    {
        ShowError(IDS_DINPUT_ERROR_DIC);
        return E_FAIL;
    }

    // Create keyboard device
    if( FAILED( g_pDI->CreateDevice( GUID_SysKeyboard, &g_pdidKeyboard, NULL ) ) )
    {
        ShowError(IDS_DINPUT_ERROR_CD);
        return E_FAIL;
    }

    // Tell DirectInput that we want to receive data in keyboard format
    if( FAILED( g_pdidKeyboard->SetDataFormat( &c_dfDIKeyboard) ) )
    {
        ShowError(IDS_DINPUT_ERROR_DF);
        return E_FAIL;
    }

    // Set cooperative level
    if( FAILED( g_pdidKeyboard->SetCooperativeLevel( hWnd,
                                  DISCL_NONEXCLUSIVE | DISCL_FOREGROUND ) ) )
    {
        ShowError(IDS_DINPUT_ERROR_SP);
        return E_FAIL;
    }

    // try to acquire the keyboard
    if( SUCCEEDED( g_pdidKeyboard->Acquire() ) )
        g_bKeyboardAcquired = TRUE;
    else
        g_bKeyboardAcquired = FALSE;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DIUtil_ReadKeys()
// Desc: Use DirectInput to read game-play keys
//-----------------------------------------------------------------------------
VOID DIUtil_ReadKeys( DWORD* pdwKeys )
{
    BYTE    rgbKeybd[256];
    DWORD   dwKeys = 0L;
    HRESULT hr;

    hr = g_pdidKeyboard->GetDeviceState( sizeof(rgbKeybd), rgbKeybd );
    if( FAILED(hr) )
    {
        if( hr == DIERR_INPUTLOST )
        {
            // We lost control of the keyboard, reacquire
            if( SUCCEEDED( g_pdidKeyboard->Acquire() ) )
                g_bKeyboardAcquired = TRUE;
            else
                g_bKeyboardAcquired = FALSE;
        }

        // Failed to read the keyboard, just return
        return;
    }

    // check & update key states
    if( rgbKeybd[DIK_NUMPAD5] & 0x80 )
        dwKeys |= KEY_STOP;

    if( (rgbKeybd[DIK_NUMPAD2] & 0x80) || (rgbKeybd[DIK_DOWN] & 0x80) )
        dwKeys |= KEY_DOWN;

    if( (rgbKeybd[DIK_NUMPAD4] & 0x80) || (rgbKeybd[DIK_LEFT] & 0x80) )
        dwKeys |= KEY_LEFT;

    if( (rgbKeybd[DIK_NUMPAD6] & 0x80) || (rgbKeybd[DIK_RIGHT] & 0x80) )
        dwKeys |= KEY_RIGHT;

    if( (rgbKeybd[DIK_NUMPAD8] & 0x80) || (rgbKeybd[DIK_UP] & 0x80) )
        dwKeys |= KEY_UP;

    if( rgbKeybd[DIK_SPACE] & 0x80 )
        dwKeys |= KEY_FIRE;

    // Return the keys
    (*pdwKeys) = dwKeys;
}




//-----------------------------------------------------------------------------
// Name: DIUtil_CleanupInput()
// Desc: Cleans up DirectInput objects
//-----------------------------------------------------------------------------
VOID DIUtil_CleanupInput()
{
    if(g_bKeyboardAcquired)
    {
        g_pdidKeyboard->Unacquire();
        g_bKeyboardAcquired = FALSE;
    }

    if( g_pdidKeyboard )
        g_pdidKeyboard->Release();

    if( g_pDI )
        g_pDI->Release();
}




//-----------------------------------------------------------------------------
// Name: DIUtil_ReacquireInputDevices()
// Desc: Reacquires DirectInput devices as needed
//-----------------------------------------------------------------------------
HRESULT DIUtil_ReacquireInputDevices()
{
    g_bKeyboardAcquired = FALSE;

    if( NULL == g_pdidKeyboard )
        return E_FAIL;

    g_pdidKeyboard->Acquire();
    g_bKeyboardAcquired = TRUE;
    
    return S_OK;
}



