//
// MLInput.h
//
// Input abstraction layer for MusicLines
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef _MLInput_
#define _MLInput_

#include <dinput.h>

class Input
{
public:
    // Game input actions
    //
    enum GameAction
    {
        None,               // Returned for player or meta

        PlayerUp,           // Returned per-player
        PlayerDown,
        PlayerLeft,
        PlayerRight,

        MetaPause,          // Returned only a meta-action
        MetaQuit,
#ifdef _DEBUG
        MetaRenderTest,     // Render speed test
#endif
    };

    Input();
    ~Input();

    // Enable attempts to create the DirectInput object(s) and initialize. Returns TRUE on success
    // Disable frees all resources
    //
    BOOL Enable(HINSTANCE hInstance, HWND hWnd);
    void Disable();
    
    // Gets the current action for a given player. The rules are:
    // 1. When no key is pressed, the action is None
    // 2. An action sticks for as long as the key is held down, *except*
    // 3. If a new control is activated while the old one is still down, the newest control
    //    has precedence.
    //
    // Returns TRUE on success, FALSE if keyboard FOCUS has been lost (should go into pause)
    //
    BOOL GetPlayerAction(int PlayerNo, GameAction *Action);

    // Only one meta action is queued at a time. Subsequent meta actions are ignored until the queued
    // action has been read.
    //
    // Returns TRUE on success, FALSE if keyboard FOCUS has been lost (should go into pause)
    // 
    BOOL GetMetaAction(GameAction *Action);

    // Reset clears all key state to no input action. 
    //
    void Reset();

    // Returns TRUE if a key has been pressed since reset. Does NOT neccesarily
    // mean a key is currently held down.
    //
    inline BOOL KeyPressed() { return m_fKeyPressed; }
private:
    LPDIRECTINPUT           m_lpDirectInput;
    LPDIRECTINPUTDEVICE     m_lpDiKeyboard;
    BOOL                    m_fAcquired;
    BOOL                    m_fKeyPressed;

    // Only support two players on the local keyboard
    //
    GameAction              m_gaPlayerActions[2];
    GameAction              m_aLastMetaAction;

private:
    BOOL ProcessBufferedData();
};

extern Input *theInput;

#endif // _MLInput_

