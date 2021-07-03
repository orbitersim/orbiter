// main.h : header file
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
/////////////////////////////////////////////////////////////////////////////

#ifndef _MAIN_H
#define _MAIN_H


#include <windows.h>
#include "events.h"
#include "dmplayer.h"
#include "scheme.h"
#include "resource.h"
#include <shellapi.h>

#define MENU_FIELD_LENGTH   256
#define ITEM_LENGTH         128
#define INVALID_PORT        0
#define VALID_PORT          1
#define SELECTED_PORT       2
#define NO_PORT             3


class CMain
{
public:
    CMain() {};
    virtual ~CMain() {};
    LRESULT WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
    int WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow);

protected:
    BOOL InitInstance(HINSTANCE hInstance);
    BOOL InitApplication(HINSTANCE hInstance);
    void ExitApplication();
    BOOL DecodeMenuSelection(WORD wID, HWND hWnd);
    void WideToByte(PWSTR pwzInput, PSTR pszOutput, WORD wStrLength);
    void CreatePopupMenu();

    CDMPlayer* m_pDMPlayer;
    HWND m_hWnd;                // hWnd for the target (invisible) window
    HINSTANCE m_hInst;          // current instance
    HHOOK m_hhkGetMessage;      // handle to GetMsgProc hook
    HHOOK m_hhkCallWndProc;     // handle to CallWndProc hook
    HINSTANCE m_hDLL;           // a handle to the DMHOOK.DLL
    UINT m_uHookEvent;          // the registered message sent from DMHOOK.DLL
    UINT m_uUIEvent;            // the registered message sent from the popup
                                // menu on the system tray
    WORD m_wActiveScheme;       // the index of the active scheme
    BOOL m_bAboutBoxActive;     // the about box is active
};

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow);
LRESULT CALLBACK AboutBox(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);


#endif  //_MAIN.H

