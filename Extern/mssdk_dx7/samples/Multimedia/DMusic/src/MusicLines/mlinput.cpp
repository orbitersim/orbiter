//
// MLInput.cpp
//
// Input abstraction layer for MusicLines
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <dinput.h>
#include "Debug.h"
#include "MLInput.h"

const int MetaActionTag = -1;       

const struct KeyboardMapEntry
{
    DWORD               DIKey;              // DirectInput name for key
    int                 PlayerNo;           // Player this key corresponds to, and
    Input::GameAction   Action;             // action for that player it represents
} KeyboardMap[] =
{
    // Player 1 uses the AWDS diamond
    //
    { DIK_A,        0,      Input::PlayerLeft },
    { DIK_D,        0,      Input::PlayerRight },
    { DIK_W,        0,      Input::PlayerUp },
    { DIK_S,        0,      Input::PlayerDown },

    // Player 2 uses the arrow keys
    //
    { DIK_LEFT,     1,      Input::PlayerLeft },
    { DIK_RIGHT,    1,      Input::PlayerRight },
    { DIK_UP,       1,      Input::PlayerUp },
    { DIK_DOWN,     1,      Input::PlayerDown },

    // NUMPAD aliases for the arrow keys work as well
    //
    { DIK_NUMPAD4,  1,      Input::PlayerLeft },
    { DIK_NUMPAD6,  1,      Input::PlayerRight },
    { DIK_NUMPAD8,  1,      Input::PlayerUp },
    { DIK_NUMPAD2,  1,      Input::PlayerDown },

    // Meta actions use player -1
    //
    { DIK_SPACE,    MetaActionTag,     Input::MetaPause },
    { DIK_ESCAPE,   MetaActionTag,     Input::MetaQuit },
#ifdef _DEBUG
    { DIK_F1,       MetaActionTag,     Input::MetaRenderTest },
#endif
};

const KeyboardMapEntry * const KeyboardMapEnd = KeyboardMap + (sizeof(KeyboardMap) / sizeof(KeyboardMap[0]));

// Buffer for the keyboard data
//
const int KeyboardBufferSize = 16;
DIPROPDWORD KeyboardBufferProp =
{
    {
    	sizeof(DIPROPDWORD),        // diph.dwSize
        sizeof(DIPROPHEADER),       // diph.dwHeaderSize
        0,                          // diph.dwObj
        DIPH_DEVICE,                // diph.dwHow
    },
    KeyboardBufferSize,             // dwData
};

// Input::Input
//
Input::Input()
{
    m_lpDirectInput = NULL;
    m_lpDiKeyboard  = NULL;
    m_fAcquired = FALSE;
}

// Input::~Input()
//
Input::~Input()
{
    Disable();
}

// Input::Enable
//
// Get the DirectInputCreate interface, create a keyboard object, and acquire it.
// Must eventually be matched by a call to Input::Disable.
// 
BOOL Input::Enable(
    HINSTANCE   hInstance, 
    HWND        hWnd)
{
    HRESULT hr;

    // Get the DirectInput object
    //
    hr = DirectInputCreate(hInstance,
                           DIRECTINPUT_VERSION,
                           &m_lpDirectInput,
                           NULL);
    if (FAILED(hr))
    {
        TraceMsg("Input::Enable(): DirectInputCreate", hr); 
        m_lpDirectInput = NULL;
        return FALSE;
    }
    
    // Get the keyboard object
    //
    hr = m_lpDirectInput->CreateDevice(GUID_SysKeyboard,
                                       &m_lpDiKeyboard,
                                       NULL);
    if (FAILED(hr))
    {
        TraceMsg("Input::Enable(): CreateDevice(GUID_SysKeyboard)", hr);
        m_lpDiKeyboard = NULL;
        Disable();
        return FALSE;
    }

    // Use the keyboard as a keyboard
    //
    hr = m_lpDiKeyboard->SetDataFormat(&c_dfDIKeyboard);
    if (FAILED(hr))
    {
        TraceMsg("Input::Enable(): SetDataFormat", hr);
        Disable();
        return FALSE;
    } 
    
    // Set up the cooperative level. We don't want exclusive access
    //
    hr = m_lpDiKeyboard->SetCooperativeLevel(hWnd, 
                                             DISCL_NONEXCLUSIVE | DISCL_FOREGROUND);
    if (FAILED(hr))
    {
        TraceMsg("Input::Enable(): SetCooperativeLevel", hr);
        Disable();
        return FALSE;
    }

    // We want to use the keyboard as a buffered device
    //
    hr = m_lpDiKeyboard->SetProperty(DIPROP_BUFFERSIZE, 
                                     (DIPROPHEADER*)&KeyboardBufferProp);
    if (FAILED(hr))
    {
        TraceMsg("Input::Enable(): SetProperty", hr);
        Disable();
        return FALSE;
    }

    // Clear and set initial state
    Reset();

    return TRUE;
}

// Input::Disable
//
// Release any resources held from DirectInput. Matches a call to Input::Enable.
//
void Input::Disable()
{
    if (m_fAcquired)
    {
        m_lpDiKeyboard->Unacquire();
        m_fAcquired = FALSE;
    }

    if (m_lpDiKeyboard)
    {
        m_lpDiKeyboard->Release();
        m_lpDiKeyboard = NULL;
    }

    if (m_lpDirectInput)
    {
        m_lpDirectInput->Release();
        m_lpDirectInput = NULL;
    }
}

// Input::GetPlayerAction
//
// Player actions are processed from queued keyboard events in Input::ProcessBufferedData. 
//
// Returns FALSE if we have lost keyboard focus and could not get it back.
//
BOOL Input::GetPlayerAction(
    int PlayerNo,
    GameAction *Action)
{
    if (!ProcessBufferedData())
    {
        return FALSE;
    }

    *Action = m_gaPlayerActions[PlayerNo];
    return TRUE; 
}    

// Input::GetMetaAction
//
// Meta actions are actions which are not associate with a player, like pausing or stopping
// the game. 
//
// Returns FALSE if we have lost keyboard focus and could not get it back.
//
BOOL Input::GetMetaAction(
    GameAction *Action)
{
    if (!ProcessBufferedData())
    {
        return FALSE;
    }

    *Action = m_aLastMetaAction;
    m_aLastMetaAction = None;

    return TRUE;
}

// Input::ProcessBufferedData
//
// The workhorse of the abstraction layer. Reads queued keystrokes and translates them into
// game events based on the keyboard map.
//
// Make sure we can get the keyboard; if we're lost focus and can't reacquire yet, return FALSE.
// Otherwise, read the queued data if any and translate into game events. Since we read the same
// number of elements we set the keyboard buffer to on create, we're guaranteed of getting them 
// all at once.
//
// For player events, we take the last pressed key as the current action (since directions aren't 
// cumulative in this game - up+left does not go diagonally up and left). Release of the current 
// action key returns to no action. 
//
// Only one meta action is queued; if there is a meta action queued but not read (by a call to 
// Input::GetMetaAction) then future meta actions are ignored. 
//
// Returns TRUE on success or FALSE if focus was lost.
//
BOOL Input::ProcessBufferedData()
{
    DIDEVICEOBJECTDATA Data[KeyboardBufferSize];
    DWORD BufferElements;
    HRESULT hr;

	if (!m_fAcquired)
	{
		hr = m_lpDiKeyboard->Acquire();
		if (FAILED(hr))
		{
	        Trace(0, "Could not acquire keyboard");
			return FALSE;
		}

		m_fAcquired = TRUE;
	}


    // Try to get keyboard data, checking to make sure we haven't lost
    // focus.
    //
    BufferElements = KeyboardBufferSize;
    hr = m_lpDiKeyboard->GetDeviceData(sizeof(DIDEVICEOBJECTDATA),
                                       Data,
                                       &BufferElements,
                                       0);  

    if (hr == DIERR_INPUTLOST)
    {
		m_fAcquired = FALSE;
        hr = m_lpDiKeyboard->Acquire();
        if (FAILED(hr))
        {
            TraceMsg("Keyboard focus lost; Acquire()", hr);
            return FALSE;
        }

		m_fAcquired = TRUE;
        BufferElements = KeyboardBufferSize;
        hr = m_lpDiKeyboard->GetDeviceData(sizeof(DIDEVICEOBJECTDATA),
                                           Data,
                                           &BufferElements,
                                           0);  
    }

    if (FAILED(hr))
    {
        TraceMsg("Keyboard->GetDeviceData()", hr);
        return FALSE;
    }

    // We got it. Now step through looking for keys we recognize
    //
    DIDEVICEOBJECTDATA *DataEnd = Data + BufferElements;
    DIDEVICEOBJECTDATA *Event;
    for (Event = Data; Event != DataEnd; Event++)
    {
        if (Event->dwData & 0x80)
        {
            m_fKeyPressed = TRUE;
        }
        const KeyboardMapEntry *Entry;            
        for (Entry = KeyboardMap; Entry != KeyboardMapEnd; Entry++)
        {
            if (Entry->DIKey == Event->dwOfs)
            {
                Trace(0, "Recognized key!");
                break;
            }
        }

        if (Entry == KeyboardMapEnd)
        {
            continue;
        }

        // If a key press, overrides anything that is already pressed for that player. If meta action,
        // save it if no other meta action pending.
        //
        if (Event->dwData & 0x80)
        {
            if (Entry->PlayerNo == MetaActionTag)
            {
                if (m_aLastMetaAction == None)
                {
                    m_aLastMetaAction = Entry->Action;
                }
            }
            else
            {
               m_gaPlayerActions[Entry->PlayerNo] = Entry->Action;
            }
        }
    }

    return TRUE;
}

void Input::Reset()
{
    m_fKeyPressed = FALSE;

    for (int PlayerNo = 0; PlayerNo < 2; PlayerNo++)
    {
        m_gaPlayerActions[PlayerNo] = None;
    }

    m_aLastMetaAction = None;
}
