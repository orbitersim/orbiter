//-----------------------------------------------------------------------------
// File: DInstall.c
//
// Desc: Example code showing how to use DirectXSetup.
//
//      This file contains code that will handle all messages sent to the
//      DirectXSetupCallbackFunction, with the filtering level set at what the
//      user wants.  This way you can test to see which messages you want to
//      handle automatically or pass on to the user.
//
// Call Tree:
//      DirectXInstallWndProc               SEE WINCODE.C
//          DirectXInstall                  Set up the callback and handle return codes
//              GetReply                    SEE WINCODE.C
//          DirectXGetVersion               Display the results of DirectXSetupGetVersion()
//      DirectXSetupCallbackFunction        Called from DirectXSetup
//          GetReply                        SEE WINCODE.C
//          SetButtons                      SEE WINCODE.C
//              ShowButton                  SEE WINCODE.C
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
// Externs for global variables
//-----------------------------------------------------------------------------
extern DWORD        g_fStatus;          // filter setting for messages from DirectXSetup
extern HINSTANCE    g_hInstance;        // global instance handle
extern HWND         g_hDlg;             // window handle to dialog proc
extern char         g_szAppTitle[256];  // application title
extern int          g_wReply;           // global value for dialog return
extern BOOL         g_bCheckOlder;      // Whether or not to check for older installs


//-----------------------------------------------------------------------------
// Name: GetReply(DWORD )
// Desc: Waits for the user to click on a button on our simulated message box
//       See DlgProc for the code that sets g_wReply
//-----------------------------------------------------------------------------
int
GetReply(DWORD dwMsgType)
{
    WORD    wDefaultButton = 0;

    // Wait until DlgProc() lets us know that the user clicked on a button
    while (g_wReply == -1)
    {
        MSG     msg;

        // Forward my messages...
        while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
        {
            if (msg.message == WM_QUIT ||
                msg.message == WM_CLOSE ||
                msg.message == WM_SYSCOMMAND ||
                msg.message == WM_DESTROY)
            {
                // Put the message back on the queue and get out of here.
                PostMessage(msg.hwnd, msg.message, msg.wParam, msg.lParam);
                break;
            }
            if (!IsDialogMessage(msg.hwnd, &msg))
            {
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
        }
    }

    // Return the proper ID value for the button the user clicked on
    // This code simulates what MessageBox() would return
    switch (dwMsgType & 0x0000000F)
    {
        case MB_OKCANCEL:
            wDefaultButton = (g_wReply == IDBUT1) ? IDOK : IDCANCEL;
            break;
        case MB_OK:
            wDefaultButton = IDOK;
            break;
        case MB_RETRYCANCEL:
            wDefaultButton = (g_wReply == IDBUT1) ? IDRETRY : IDCANCEL;
            break;
        case MB_ABORTRETRYIGNORE:
            if (g_wReply == IDBUT1)
                wDefaultButton = IDABORT;
            else if (g_wReply == IDBUT2)
                wDefaultButton = IDRETRY;
            else
                wDefaultButton = IDIGNORE;
            break;
        case MB_YESNOCANCEL:
            if (g_wReply == IDBUT1)
                wDefaultButton = IDYES;
            else if (g_wReply == IDBUT2)
                wDefaultButton = IDNO;
            else
                wDefaultButton = IDCANCEL;
            break;
        case MB_YESNO:
            wDefaultButton = (g_wReply == IDBUT1) ? IDYES : IDNO;
            break;
        default:
            wDefaultButton = IDOK;
    }
    g_wReply = -1;
    return wDefaultButton;
}




//-----------------------------------------------------------------------------
// Name: DirectXSetupCallbackFunction(DWORD , DWORD , LPSTR , LPSTR , void *)
// Desc: Handle each reason for why the callback was called, filtering each
//       message by what the current state of g_fStatus is.
//-----------------------------------------------------------------------------
DWORD WINAPI
DirectXSetupCallbackFunction(DWORD dwReason, DWORD dwMsgType, LPSTR szMessage, LPSTR
                             szName, void *pInfo)
{
    if (szMessage == NULL && szName == NULL)
        return IDOK;
    SetButtons(g_hDlg, -1);
    if (g_fStatus == SHOW_ALL)
    {
        //---------------------------------------------------------------------
        //              Show all messages from DirectSetup
        //---------------------------------------------------------------------
        SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), szMessage);
        // When dwMsgType is equal to zero we can display status information but
        // should not wait for input from the user. 
        if (dwMsgType == 0)
        {
            Sleep(1000);
            return IDOK;
        }
        else
            SetButtons(g_hDlg, dwMsgType);
    }
    else if (g_fStatus == SHOW_UPGRADES)
    {
        //---------------------------------------------------------------------
        //              Show only upgrade messages
        //---------------------------------------------------------------------
        switch (dwReason)
        {
            case DSETUP_CB_MSG_SETUP_INIT_FAILED:
            case DSETUP_CB_MSG_INTERNAL_ERROR:
            case DSETUP_CB_MSG_OUTOFDISKSPACE:
            case DSETUP_CB_MSG_FILECOPYERROR:
                SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), szMessage);
                SetButtons(g_hDlg, dwMsgType);
                break;
            case DSETUP_CB_MSG_CHECK_DRIVER_UPGRADE:
                {
                    // pInfo points to a structure containing flags that summarize
                    // the DirectXSetup functions recommendation on how the upgrade
                    // of DirectX components and drivers should be preformed
                    DWORD dwUpgrade = ((DSETUP_CB_UPGRADEINFO *) pInfo)->UpgradeFlags
                    & DSETUP_CB_UPGRADE_TYPE_MASK;
                    switch (dwUpgrade)
                    {
                        case DSETUP_CB_UPGRADE_FORCE:
                        case DSETUP_CB_UPGRADE_KEEP:
                        case DSETUP_CB_UPGRADE_SAFE:
                        case DSETUP_CB_UPGRADE_UNKNOWN:
                            {
                                SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), szMessage);
                                SetButtons(g_hDlg, dwMsgType);
                            }
                            break;
                    }
                }
                break;
            default:
                return IDOK;
        }
    }
    else if (g_fStatus == SHOW_PROBLEMS)
    {
        //---------------------------------------------------------------------
        //              Show only problem messages
        //---------------------------------------------------------------------
        switch (dwReason)
        {
            case DSETUP_CB_MSG_CANTINSTALL_UNKNOWNOS:
            case DSETUP_CB_MSG_CANTINSTALL_NT:
            case DSETUP_CB_MSG_CANTINSTALL_BETA:
            case DSETUP_CB_MSG_CANTINSTALL_NOTWIN32:
            case DSETUP_CB_MSG_CANTINSTALL_WRONGLANGUAGE:
            case DSETUP_CB_MSG_CANTINSTALL_WRONGPLATFORM:
            case DSETUP_CB_MSG_PREINSTALL_NT:
            case DSETUP_CB_MSG_NOTPREINSTALLEDONNT:
            case DSETUP_CB_MSG_SETUP_INIT_FAILED:
            case DSETUP_CB_MSG_INTERNAL_ERROR:
            case DSETUP_CB_MSG_OUTOFDISKSPACE:
            case DSETUP_CB_MSG_FILECOPYERROR:
                SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), szMessage);
                SetButtons(g_hDlg, dwMsgType);
                break;
            case DSETUP_CB_MSG_CHECK_DRIVER_UPGRADE:
                {
                    DWORD dwUpgrade = ((DSETUP_CB_UPGRADEINFO *) pInfo)->UpgradeFlags
                    & DSETUP_CB_UPGRADE_TYPE_MASK;
                    switch (dwUpgrade)
                    {
                        case DSETUP_CB_UPGRADE_FORCE:
                        case (DSETUP_CB_UPGRADE_SAFE & DSETUP_CB_UPGRADE_HASWARNINGS):
                        case DSETUP_CB_UPGRADE_UNKNOWN:
                            {
                                SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), szMessage);
                                SetButtons(g_hDlg, dwMsgType);
                                break;
                            }
                        case DSETUP_CB_UPGRADE_KEEP:
                            return IDOK;
                        case DSETUP_CB_UPGRADE_SAFE:
                            switch (dwMsgType & 0x0000000F)
                            {
                                case MB_YESNO:
                                case MB_YESNOCANCEL:
                                    return IDYES;
                                case MB_OKCANCEL:
                                case MB_OK:
                                default:
                                    return IDOK;
                            }
                            break;
                    }
                }
                break;
            default:
                return IDOK;
        }
    }
    else if (g_fStatus == SHOW_NONE)
    {
        //---------------------------------------------------------------------
        //              Don't show any messages
        //---------------------------------------------------------------------
        return IDOK;
    }
    return GetReply(dwMsgType);
}




//-----------------------------------------------------------------------------
// Name: DirectXGetVersion(void )
// Desc: Shows the results of a call to DirectXSetupGetVersion()
//-----------------------------------------------------------------------------
void
DirectXGetVersion(void )
{
    INT     iret;
    DWORD   dwVersion;
    DWORD   dwRevision;
    char    szBuf[128];

    iret = DirectXSetupGetVersion(&dwVersion, &dwRevision);
    sprintf(szBuf, "Version 0x%08lX\nRevision %ld", dwVersion, dwRevision);
    // Use HIWORD(dwVersion); to get the DirectX major version
    // Use LOWORD(dwVersion); to get the DirectX minor version
    // For example:
    //                      dwVersion ==
    //      DirectX 1:      0x00040001
    //      DirectX 2:      0x00040002
    //      DirectX 3:      0x00040003
    //      DirectX 5:      0x00040005
    //      DirectX 6:      0x00040006
    MessageBox(NULL, szBuf, "Results:", MB_OK | MB_ICONINFORMATION);
}




//-----------------------------------------------------------------------------
// Name: DirectXInstall(HWND )
// Desc: Set up the callback function for DirectXSetup and handle the return
//       results.  This function starts a modeless version of MessageBox() so
//       that the user can see the progress of the DirectX installation.
//-----------------------------------------------------------------------------
BOOL
DirectXInstall(HWND hWnd)
{
    char    szSource[_MAX_PATH];
    char    string[256];
    int     retc;
	DWORD   dwFlags;

    // The DSETUP DLLs should be at the current path, along with the DirectX
    // redist directory so that it can be found and set up.  Otherwise, change
    // the code below to reflect the real path.
    GetCurrentDirectory(_MAX_PATH, szSource);

    // If the user wants any messages, bring up the simulated MessageBox
    // dialog
    if (g_fStatus != SHOW_NONE)
    {
        // Create a modeless dialog box so we can show messages that don't
        // need user input
        g_hDlg = CreateDialog(g_hInstance, "INSTDX", hWnd, (DLGPROC) DlgProc);
        if (g_hDlg == NULL)
        {
            char    buf[200];

            LoadString(g_hInstance, STR_NODIALOG, buf, 200);
            MessageBox(hWnd, string, g_szAppTitle, MB_ICONSTOP | MB_OK);
            return FALSE;
        }
        ShowWindow(GetDlgItem(g_hDlg, IDBUT1), SW_HIDE);
        ShowWindow(GetDlgItem(g_hDlg, IDBUT2), SW_HIDE);
        ShowWindow(GetDlgItem(g_hDlg, IDBUT3), SW_HIDE);
        ShowWindow(g_hDlg, SW_NORMAL);
    }

    // Set the callback function up before calling DirectXSetup
    DirectXSetupSetCallback((DSETUP_CALLBACK) DirectXSetupCallbackFunction);
    if (g_fStatus != SHOW_NONE)
    {
        LoadString(g_hInstance, STR_STARTSETUP, string, 256);
        SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), string);
        SetButtons(g_hDlg, -1);
        Sleep(1000);
    }
    //-------------------------------------------------------------------------
    //
    //                          *** IMPORTANT NOTE: ***
    //
    // Do the DirectXSetup, but only in test mode
    // In the real world, you would remove the DSETUP_TESTINSTALL
    //-------------------------------------------------------------------------
	dwFlags = DSETUP_DIRECTX | DSETUP_TESTINSTALL;
	if( g_bCheckOlder == TRUE )
		 dwFlags |= DSETUP_USEROLDERFLAG;

	retc = DirectXSetup( hWnd, szSource, dwFlags );

    // If the user didn't want any message, we now need to bring up the dialog
    // to reflect the return message from DirectXSetup
    if (g_fStatus == SHOW_NONE)
    {
        g_hDlg = CreateDialog(g_hInstance, "INSTDX", hWnd, (DLGPROC) DlgProc);
        if (g_hDlg == NULL)
        {
            char    buf[200];

            LoadString(g_hInstance, STR_NODIALOG, buf, 200);
            MessageBox(hWnd, string, g_szAppTitle, MB_ICONSTOP | MB_OK);
            return FALSE;
        }
        ShowWindow(GetDlgItem(g_hDlg, IDBUT1), SW_HIDE);
        ShowWindow(GetDlgItem(g_hDlg, IDBUT2), SW_HIDE);
        ShowWindow(GetDlgItem(g_hDlg, IDBUT3), SW_HIDE);
        ShowWindow(g_hDlg, SW_NORMAL);
    }

    switch (retc)
    {
        // Since our MessageBox dialog is still up, display the results in it
		case DSETUPERR_NEWERVERSION:
			MessageBox( NULL, "Installation is newer than one being installed",
				        "DirectX Setup", MB_OK);
			break;

        case DSETUPERR_SUCCESS_RESTART:
            LoadString(g_hInstance, STR_RESTART, string, 256);
            SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), string);
            SetButtons(g_hDlg, -1);
            SetButtons(g_hDlg, MB_YESNO);
            if (GetReply(MB_YESNO) == IDYES)
            {
                // Restart Windows
                ExitWindowsEx(EWX_REBOOT, 0);
            }
            break;
        case DSETUPERR_SUCCESS:
            LoadString(g_hInstance, STR_SUCCESS, string, 256);
            SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), string);
            SetButtons(g_hDlg, -1);
            SetButtons(g_hDlg, MB_OK);
            GetReply(MB_OK);
            break;
        case DSETUPERR_BADWINDOWSVERSION:
        case DSETUPERR_SOURCEFILENOTFOUND:
        case DSETUPERR_BADSOURCESIZE:
        case DSETUPERR_BADSOURCETIME:
        case DSETUPERR_NOCOPY:
        case DSETUPERR_OUTOFDISKSPACE:
        case DSETUPERR_CANTFINDINF:
        case DSETUPERR_CANTFINDDIR:
        case DSETUPERR_INTERNAL:
        case DSETUPERR_UNKNOWNOS:
        case DSETUPERR_USERHITCANCEL:
        case DSETUPERR_NOTPREINSTALLEDONNT:
            LoadString(g_hInstance, STR_ERRORRETURN + (retc * -1) - 1, string, 256);
            SetWindowText(GetDlgItem(g_hDlg, ID_MYMESSAGE), string);
            SetButtons(g_hDlg, -1);
            SetButtons(g_hDlg, MB_OK);
            GetReply(MB_OK);
            break;
    }
    DestroyWindow(g_hDlg);
    g_hDlg = NULL;
    return (TRUE);
}
