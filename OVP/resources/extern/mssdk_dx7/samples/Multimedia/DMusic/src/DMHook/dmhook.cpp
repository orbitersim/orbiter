/*****************************************************************************\
*  dmhook.cpp - DLL for hooking the system messages
*
*  Since we need to hook messages from all applications, this module must
*  be a DLL which can attach to any thread.
*
*  DMHOOK.DLL indentifies the events listed in EVENTS.H and posts a message
*  to the DMShell window when they occur. Some examples include application open
*  window minimize, etc.
*
*  DMSHELL.EXE is abstracted from the ugliness of indentifying events.
*
*  Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
\*****************************************************************************/

#include <windows.h>
#include "..\dmshell\events.h"

BOOL CALLBACK FindStartButton( HWND hWnd, LPARAM lParam);

HWND hwndDMShell = NULL;		// the window handle of the DMShell Application

UINT uShellHook;				// a registered message that the system sends
								// to indicate a change on the task bar. DMHOOK
								// uses this to indentify when applications
								// open and close.

UINT uCMBPopupOpen;				// a registered message that occurs when a
								// popup menu is created

UINT uDMSRegEvent;				// Contains the registered message # used
								// to post events to DMShell

HWND hwndStartButton = NULL;	// the Window Handle of the "Start" button


/*****************************************************************************\
* DllMain (hModule,cbHeap,lpchCmdLine)
*
* Called when the libary is loaded
*
* Arguments:
*    PVOID hModule - Module handle for the libary.
*    ULONG ulReason - DLL purpose
*    PCONTEXT pctx - not used
*
* Returns:
*    TRUE - Everything is ok
*    FALSE- Error.
\*****************************************************************************/

BOOL APIENTRY DllMain(PVOID hModule, ULONG ulReason, PCONTEXT pctx )
{
	HWND hShellTray;

    UNREFERENCED_PARAMETER(hModule);
    UNREFERENCED_PARAMETER(pctx);

    if ( ulReason == DLL_PROCESS_ATTACH )
	{
		// This function is called for every instance of the DLL. We must find
		// and store the handle to the DMShell window every time an instance of the
		// DLL is instantiated.
		
		hwndDMShell = FindWindow(CLASSNAME, WINDOWNAME);

		uShellHook = RegisterWindowMessage( "SHELLHOOK" );
		uCMBPopupOpen = RegisterWindowMessage( "CMBPopupOpen" );
		uDMSRegEvent = RegisterWindowMessage ( DME_REGISTEREDEVENT );

		// Find the hWnd for the "Start" button. It will be used later to detect
		// when the user clicks the "start" button.
		//
		// Since the start button is a fairly generic window style and class,
		// we will first find the parent window, which is unique in description.
		// Then we'll ask the children to enumerate themselves. From there, we
		// can reliably identify the "start" button without confusion.
		
		hShellTray = FindWindow( "Shell_TrayWnd", NULL);
		EnumChildWindows (hShellTray, (WNDENUMPROC)FindStartButton , 0);
	}
	return TRUE;
}

/*****************************************************************************\
*  FindStartButton( ...)
*
*		A spefic use of EnumChildProc to find the hWnd for the start button
*
*  This is called by each child window of the "Shell_TrayWnd" window.
*  One of the children is the "start" button.
*
\*****************************************************************************/

BOOL CALLBACK FindStartButton( HWND hWnd, LPARAM lParam)
{
	// Windows style for the start button is: 54000D80
	// Windows extended style for the start button is: 0
	
	if ( GetWindowLong( hWnd, GWL_STYLE ) == 0x54000D80 &&
		GetWindowLong( hWnd, GWL_EXSTYLE ) == 0)
	{
		// this is the button
		hwndStartButton = hWnd;
	}
	return TRUE;
}

/*****************************************************************************\
* DecodeMessage( hWnd, uiMessage, wParam, lParam )
*
* Checks the incoming messages to determine if they signify any meaningful
* events as described in EVENTS.H. Posts a message to the DMShell application
* if appropriate.
*
* Arguments:
*    HWND hWnd - window handle for the parent window
*    UINT uiMessage - message number
*    WPARAM wParam - message-dependent
*    LPARAM lParam - message-dependent
*
\*****************************************************************************/

VOID DecodeMessage(HWND hWnd, UINT uiMessage, WPARAM wParam, LPARAM lParam)
{
	WPARAM wpEvent = 0xFFFF;
	LPARAM lpEvent = 0;

	if (hwndDMShell == NULL || !IsWindow(hwndDMShell))
		return;

	if (uiMessage == WM_SYSCOMMAND)
	{
		switch ( (wParam & 0xFFF0) )
		{
			case SC_VSCROLL:		// Scrolls vertically.
			case SC_HSCROLL:		// Scrolls horizontally. 
				wpEvent = DME_SCROLL;
				break;

			case SC_MAXIMIZE:		// Maximizes the window. 
				wpEvent = DME_MAXIMIZE;
				break;

			case SC_MINIMIZE:		// Minimizes the window. 
				wpEvent = DME_MINIMIZE;
				break;

			case SC_RESTORE:		// Restores the window to its normal position and size. 
				wpEvent = DME_RESTORE;
				break;

			case SC_MOVE:			// Moves the window. 
				wpEvent = DME_WINDOWMOVE;
				break;
			
			case SC_CLOSE:			// Closes the window.
				wpEvent = DME_WINDOWCLOSE;
				break;
		}
	}

	if (uiMessage == WM_KEYDOWN)
	{
		switch (wParam)
		{
			case VK_BACK:
				wpEvent = DME_KEYBACKSPACE;
				break;
			case VK_SPACE:
				wpEvent = DME_KEYSPACE;
				break;
			case VK_RETURN:
				wpEvent = DME_KEYRETURN;
				break;
			default:
				wpEvent = DME_KEYGENERIC;
				break;
		}
	}
	
	if (uiMessage == uShellHook)
	{
		if (wParam == 1)
		{
			wpEvent = DME_APPOPEN;      // lParam = HWND of application that opened
			lpEvent = lParam;
		}
		if (wParam == 2)				// application (lParam = HWND) closed
		{
			wpEvent = DME_APPCLOSE;
			lpEvent = lParam;
		}
		if (wParam == 4)				// switched to app with lParam = HWND
		{								// lParam = 0 for the desktop
			wpEvent = DME_APPSWITCH;
			lpEvent = lParam;			
		}
	}
	if (uiMessage == WM_INITMENUPOPUP	// INITMENUPOPUP is for standard menus and right clicks
		|| uiMessage == uCMBPopupOpen)	// CMBPopup is for start bar mini windows

		wpEvent = DME_MENUPOPUP;

	if (uiMessage == WM_CAPTURECHANGED
		&& lParam != 0)

		wpEvent = DME_MENUPOPUP;		// This is for menu selection in Office applications

	if (hWnd == hwndStartButton			// The start button
		&& uiMessage == BM_SETSTATE		// When the button state changes
		&& wParam )						// When the button is "pressed"

		wpEvent = DME_STARTBUTTON;

    if (wpEvent != 0xFFFF)
		PostMessage(hwndDMShell, uDMSRegEvent, wpEvent, lpEvent);
}


/*****************************************************************************\
* HookGetMsgProc
*
*		Arguments defined by GetMsgProc
*
* This hooks queued messages that are "Posted" through PostMessage
\*****************************************************************************/

LRESULT CALLBACK HookGetMsgProc(INT hc, WPARAM wParam, LPARAM lParam)
{
	if (hc == HC_ACTION)
	{
		PMSG pmsg;
		UINT uMes;

		pmsg = (PMSG)lParam;
		uMes = pmsg->message;

		DecodeMessage(pmsg->hwnd, uMes, pmsg->wParam, pmsg->lParam);
	}
	// Note that CallNextHookEx ignores the first parameter (hhook) so
	// it is acceptable (barely) to pass in a NULL.
	return CallNextHookEx(NULL, hc, wParam, lParam);
}

/*****************************************************************************\
* HookCallWndProc
*
*		Arguments defined by CallWndProc
*
* The Call Window Proc (Send Message) hook function.
\*****************************************************************************/

LRESULT CALLBACK HookCallWndProc(INT hc, WPARAM wParam, LPARAM lParam)
{
	if (hc == HC_ACTION)
	{
		PCWPSTRUCT pcwps;
		UINT uMes;

		pcwps = (PCWPSTRUCT)lParam;
		uMes = pcwps->message;
		
		DecodeMessage(pcwps->hwnd, uMes, pcwps->wParam, pcwps->lParam);
	}
	// Note that CallNextHookEx ignores the first parameter (hhook) so
	// it is acceptable (barely) to pass in a NULL.
	return CallNextHookEx(NULL, hc, wParam, lParam);
}
