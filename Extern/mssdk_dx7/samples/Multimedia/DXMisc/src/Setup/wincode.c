//-----------------------------------------------------------------------------
// File: WinCode.c
//
// Desc: All of the Windows specific code needed for the DSetup sample
//
//      The code in this file includes the main Windows entry point
//      as well as code to handle messages and our modeless version of
//      MessageBox().
//
// Call Tree:
//      WinMain                             Main Windows Entry Point
//          DirectXInstallInit              Initializes & registers window class
//              DirectXInstallWndProc       Processes windows messages
//                  DirectXInstall          SEE DINSTALL.C
//                  DirectXGetVersion       SEE DINSTALL.C
//      DirectXInstall                      SEE DINSTALL.C
//          DlgProc                         Handles all messages for our modeless MessageBox()
//              SetButtons                  Initializes the text of the dialog buttons to mimmic MessageBox()
//                  ShowButton              Helper function to get and set the text of a button from resource strings
//
// Copyright (c) 1998 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <dsetup.h>
#include "resource.h"
#include "dinstall.h"

//-----------------------------------------------------------------------------
// Global variables
//-----------------------------------------------------------------------------
DWORD       g_fStatus = SHOW_ALL;   // filter setting for messages from DirectXSetup
HINSTANCE   g_hInstance;            // global instance handle
HWND        g_hDlg = NULL;          // window handle to dialog proc
char        g_szAppTitle[256];      // application title
int         g_wReply = -1;          // global value for dialog return
BOOL        g_bCheckOlder = FALSE;  // Whether or not to check for older installs

//-----------------------------------------------------------------------------
// Name: ShowButton(HWND , int , int )
// Desc: Helper function to get and set the text of a button from the
//       resource strings.
//-----------------------------------------------------------------------------
void
ShowButton(HWND hDlg, int Id, int strid)
{
    HWND btnHwd = GetDlgItem(hDlg, Id);
    char buf[20];
    LoadString(g_hInstance, strid, buf, 20);
    SetWindowText(btnHwd, buf);
    ShowWindow(btnHwd, SW_NORMAL);
}




//-----------------------------------------------------------------------------
// Name: SetButtons(HWND , DWORD )
// Desc: Initializes the text of the dialog buttons to mimmic MessageBox()
//-----------------------------------------------------------------------------
void
SetButtons(HWND hDlg, DWORD wMsgType)
{
    LONG dwStyle;
    switch (wMsgType & 0x0000000F)
    {
        case MB_OKCANCEL:
            ShowButton(hDlg, IDBUT1, STR_OK);
            ShowButton(hDlg, IDBUT2, STR_CANCEL);
            ShowWindow(GetDlgItem(hDlg, IDBUT3), SW_HIDE);
            break;
        case MB_OK:
            ShowButton(hDlg, IDBUT3, STR_OK);
            break;
        case MB_RETRYCANCEL:
            ShowButton(hDlg, IDBUT1, STR_RETRY);
            ShowButton(hDlg, IDBUT2, STR_CANCEL);
            ShowWindow(GetDlgItem(hDlg, IDBUT3), SW_HIDE);
            break;
        case MB_ABORTRETRYIGNORE:
            ShowButton(hDlg, IDBUT1, STR_ABORT);
            ShowButton(hDlg, IDBUT3, STR_RETRY);
            ShowButton(hDlg, IDBUT2, STR_IGNORE);
            break;
        case MB_YESNOCANCEL:
            ShowButton(hDlg, IDBUT1, STR_YES);
            ShowButton(hDlg, IDBUT3, STR_NO);
            ShowButton(hDlg, IDBUT2, STR_CANCEL);
            break;
        case MB_YESNO:
            ShowButton(hDlg, IDBUT1, STR_YES);
            ShowButton(hDlg, IDBUT2, STR_NO);
            ShowWindow(GetDlgItem(hDlg, IDBUT3), SW_HIDE);
            break;
        default:
            ShowWindow(GetDlgItem(hDlg, IDBUT1), SW_HIDE);
            ShowWindow(GetDlgItem(hDlg, IDBUT2), SW_HIDE);
            ShowWindow(GetDlgItem(hDlg, IDBUT3), SW_HIDE);
    }
    if (!(wMsgType & MB_DEFBUTTON2))
    {
        dwStyle = GetWindowLong(GetDlgItem(hDlg, IDBUT2), GWL_STYLE);
        SendMessage(GetDlgItem(hDlg, IDBUT2), BM_SETSTYLE, dwStyle & ~BS_DEFPUSHBUTTON,0);
    }
    else
    {
        dwStyle = GetWindowLong(GetDlgItem(hDlg, IDBUT2), GWL_STYLE);
        SendMessage(GetDlgItem(hDlg, IDBUT2), BM_SETSTYLE, dwStyle | BS_DEFPUSHBUTTON,0);
    }

    if (!(wMsgType & MB_DEFBUTTON3))
    {
        dwStyle = GetWindowLong(GetDlgItem(hDlg, IDBUT3), GWL_STYLE);
        SendMessage(GetDlgItem(hDlg, IDBUT3), BM_SETSTYLE, dwStyle & ~BS_DEFPUSHBUTTON,0);
    }
    else
    {
        dwStyle = GetWindowLong(GetDlgItem(hDlg, IDBUT3), GWL_STYLE);
        SendMessage(GetDlgItem(hDlg, IDBUT3), BM_SETSTYLE, dwStyle | BS_DEFPUSHBUTTON,0);
    }

    if (!(wMsgType & MB_DEFBUTTON3) && !(wMsgType & MB_DEFBUTTON2))
    {
        dwStyle = GetWindowLong(GetDlgItem(hDlg, IDBUT1), GWL_STYLE);
        SendMessage(GetDlgItem(hDlg, IDBUT1), BM_SETSTYLE, dwStyle | BS_DEFPUSHBUTTON,0);
    }
    else
    {
        dwStyle = GetWindowLong(GetDlgItem(hDlg, IDBUT1), GWL_STYLE);
        SendMessage(GetDlgItem(hDlg, IDBUT1), BM_SETSTYLE, dwStyle & ~BS_DEFPUSHBUTTON,0);
    }
}




//-----------------------------------------------------------------------------
// Name: DlgProc(HWND , WORD , WPARAM , LPARAM )
// Desc: Message proc for our modeless version of MessageBox()
//       This function sets g_wReply for GetReply()
//-----------------------------------------------------------------------------
DLGPROC
DlgProc(HWND hDlg, WORD message, WPARAM wParam, LPARAM lParam)
{
    switch (message)
    {
        case WM_INITDIALOG:
            SetButtons(hDlg, -1);
            break;
        case WM_COMMAND:
            switch (LOWORD(wParam))
            {
                case IDBUT1:
                case IDBUT2:
                case IDBUT3:
                    // Let GetReply() know the user clicked on a button
                    g_wReply = LOWORD(wParam);
                    break;
            }
            break;
        case WM_ACTIVATE:
            {
                if (LOWORD(wParam) == WA_INACTIVE)
                {
                    if ((HWND) lParam == GetParent(hDlg))
                    {
                        SetForegroundWindow(hDlg);
                    }
                }
            }
            break;
    }
    return (0);
}




//-----------------------------------------------------------------------------
// Name: SetStatusChecks(HWND )
// Desc: Helper function to set checkmarks by status menu items
//-----------------------------------------------------------------------------
void
SetStatusChecks(HWND hWnd)
{
    struct
    {
        WORD    wfStatus;
        WORD    wID;
    } trans_array[] =
    {
        {SHOW_ALL,IDSHOWALL},
        {SHOW_UPGRADES,IDSHOWUPGRADES},
        {SHOW_PROBLEMS,IDSHOWPROBLEMS},
        {SHOW_NONE,IDSHOWNOTHING}
    };
    HMENU   hMenu = GetMenu(hWnd);
    int     i;

    for (i=0; i < 4; i++)
        if (g_fStatus == trans_array[i].wfStatus)
            CheckMenuItem(hMenu, trans_array[i].wID, MF_BYCOMMAND | MF_CHECKED);
        else
            CheckMenuItem(hMenu, trans_array[i].wID, MF_BYCOMMAND | MF_UNCHECKED);
}




//-----------------------------------------------------------------------------
// Name: DirectXInstallWndProc(HWND, unsigned, WORD, LONG)
// Desc: Processes windows messages
//-----------------------------------------------------------------------------
long WINAPI
DirectXInstallWndProc(HWND hWnd, WORD message, WPARAM wParam, LPARAM lParam)
{
    switch (message)
    {
        case WM_COMMAND:
            {
                // Process menu items
                switch (LOWORD(wParam))
                {
                    case IDINSTALL:
                        DirectXInstall(hWnd);
                        break;
                    case IDGETVERSION:
                        DirectXGetVersion();
                        break;
                    case IDEXIT:
                        DestroyWindow(hWnd);
                        break;
                    case IDSHOWALL:
                        g_fStatus = SHOW_ALL;
                        SetStatusChecks(hWnd);
                        break;
                    case IDSHOWUPGRADES:
                        g_fStatus = SHOW_UPGRADES;
                        SetStatusChecks(hWnd);
                        break;
                    case IDSHOWPROBLEMS:
                        g_fStatus = SHOW_PROBLEMS;
                        SetStatusChecks(hWnd);
                        break;
                    case IDSHOWNOTHING:
                        g_fStatus = SHOW_NONE;
                        SetStatusChecks(hWnd);
                        break;
                    case IDCHECKOLDERINSTALL:
						g_bCheckOlder = !g_bCheckOlder;
			            CheckMenuItem( GetMenu(hWnd), IDCHECKOLDERINSTALL,
							           MF_BYCOMMAND | MF_CHECKED );
						break;

                    default:
                        break;
                }
                break;
            }
        case WM_DESTROY:       // Window being destroyed
            PostQuitMessage(0);
            break;
        default:               // Passes it on if unproccessed
            return (DefWindowProc(hWnd, message, wParam, lParam));
    }
    return (0);
}




//-----------------------------------------------------------------------------
// Name: DirectXInstallInit(HANDLE)
// Desc: Initializes window data and registers window class
//       Sets up a structure to register the window class.  Structure includes
//       such information as what function will process messages, what cursor
//       and icon to use, etc.
//-----------------------------------------------------------------------------
BOOL
DirectXInstallInit(HINSTANCE hInstance)
{
    HANDLE      hMemory;        // handle to allocated memory
    PWNDCLASS   pWndClass;      // structure pointer
    BOOL        bSuccess;       // RegisterClass() result

    hMemory = LocalAlloc(LPTR, sizeof(WNDCLASS));
    pWndClass = (PWNDCLASS) LocalLock(hMemory);

    pWndClass->style = CS_GLOBALCLASS;
    pWndClass->lpfnWndProc = (WNDPROC) DirectXInstallWndProc;
    pWndClass->hInstance = hInstance;
    pWndClass->hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_MAIN_ICON));
    pWndClass->hCursor = LoadCursor(NULL, IDC_ARROW);
    pWndClass->hbrBackground = GetStockObject(WHITE_BRUSH);
    pWndClass->lpszMenuName = "MainMenu";
    pWndClass->lpszClassName = (LPSTR) "DirectXInstall";

    bSuccess = RegisterClass(pWndClass);
    LocalUnlock(hMemory);       // Unlocks the memory
    LocalFree(hMemory);         // Returns it to Windows

    return (bSuccess);          // Returns result of registering the window
}




//-----------------------------------------------------------------------------
// Name: WinMain(HANDLE, HANDLE, LPSTR, int)
// Desc: Calls initialization function, processes message loop
//       This will initialize the window class if it is the first time this
//       application is run.  It then creates the window, and processes the
//       message loop until a PostQuitMessage is received.  It exits the
//       application by returning the value passed by the PostQuitMessage.
//-----------------------------------------------------------------------------
int WINAPI 
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    HWND    hWnd;
    MSG     msg;

    // Has application been initialized?
    if (!hPrevInstance)
        if (!DirectXInstallInit(hInstance))
            return (0);     // Exits if unable to initialize     */

    g_hInstance = hInstance;

    hWnd = CreateWindow("DirectXInstall",
                        "DirectX Install",
                        WS_OVERLAPPEDWINDOW,
                        CW_USEDEFAULT,
                        CW_USEDEFAULT,
                        CW_USEDEFAULT,
                        CW_USEDEFAULT,
                        NULL,
                        NULL,
                        g_hInstance,
                        NULL);

    if (!hWnd)              // Was the window created?
        return (0);
    LoadString(g_hInstance, STR_TITLE, g_szAppTitle, 200);
    ShowWindow(hWnd, SW_NORMAL);
    UpdateWindow(hWnd);     // Send a WM_PAINT message
    SetStatusChecks(hWnd);  // Check the default message menu item
    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return (msg.wParam);    // Returns the value from PostQuitMessage
}
