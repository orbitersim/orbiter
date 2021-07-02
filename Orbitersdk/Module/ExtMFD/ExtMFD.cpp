// ==============================================================
//                  ORBITER MODULE: ExtMFD
//                  Part of the ORBITER SDK
//            Copyright (C) 2006 Martin Schweiger
//                   All rights reserved
//
// ExtMFD.cpp
//
// Open multifunctional displays (MFD) in external windows
// ==============================================================

#define STRICT 1
#define ORBITER_MODULE
#include <windows.h>
#include "MFDWindow.h"
#include "orbitersdk.h"
#include "resource.h"
#include <stdio.h>

// ==============================================================
// Global variables
// ==============================================================

HINSTANCE g_hInst;    // module instance handle
HBITMAP g_hPin;       // "pin" button bitmap
DWORD g_dwCmd;        // custom function identifier

// ==============================================================
// Local prototypes
// ==============================================================

void OpenDlgClbk (void *context);
BOOL CALLBACK MsgProc (HWND, UINT, WPARAM, LPARAM);
extern long FAR PASCAL MFD_WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
extern long FAR PASCAL MFD_BtnProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// This function is called when Orbiter starts or when the module
// is activated.

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	g_hInst = hDLL; // remember the instance handle

	// To allow the user to open our new dialog box, we create
	// an entry in the "Custom Functions" list which is accessed
	// in Orbiter via Ctrl-F4.
	g_dwCmd = oapiRegisterCustomCmd ("External MFD",
		"Opens a multifunctional display in an external window",
		OpenDlgClbk, NULL);

	// Load the bitmap for the "pin" title button
	g_hPin = (HBITMAP)LoadImage (g_hInst, MAKEINTRESOURCE(IDB_PIN), IMAGE_BITMAP, 15, 30, 0);

	// Register a window classes for the MFD display and buttons
	WNDCLASS wndClass;
	wndClass.style = CS_HREDRAW | CS_VREDRAW;
	wndClass.lpfnWndProc   = MFD_WndProc;
	wndClass.cbClsExtra    = 0;
	wndClass.cbWndExtra    = 0;
	wndClass.hInstance     = hDLL;
	wndClass.hIcon         = NULL;
	wndClass.hCursor       = LoadCursor (NULL, MAKEINTRESOURCE(IDC_ARROW));
	wndClass.hbrBackground = (HBRUSH)GetStockObject (BLACK_BRUSH);
	wndClass.lpszMenuName  = NULL;
	wndClass.lpszClassName = "ExtMFD_Display";
	RegisterClass (&wndClass);

	wndClass.lpfnWndProc   = MFD_BtnProc;
	wndClass.hbrBackground = (HBRUSH)GetStockObject (LTGRAY_BRUSH);
	wndClass.lpszClassName = "ExtMFD_Button";
	RegisterClass (&wndClass);
}

// ==============================================================
// This function is called when Orbiter shuts down or when the
// module is deactivated

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister window classes
	UnregisterClass ("ExtMFD_Display", g_hInst);
	UnregisterClass ("ExtMFD_Button", g_hInst);

	// Free bitmap resources
	DeleteObject (g_hPin);

	// Unregister the custom function in Orbiter
	oapiUnregisterCustomCmd (g_dwCmd);
}


// ==============================================================
// Write some parameters to the scenario file

DLLCLBK void opcSaveState (FILEHANDLE scn)
{
	//oapiWriteScenario_int (scn, "Param", myprm);
}

// ==============================================================
// Read custom parameters from scenario

DLLCLBK void opcLoadState (FILEHANDLE scn)
{
	//char *line;
	//while (oapiReadScenario_nextline (scn, line)) {
	//	if (!strnicmp (line, "Param", 5)) {
	//		sscanf (line+5, "%d", &myprm);
	//	}
	//}
}

// ==============================================================
// Open the dialog window

void OpenDlgClbk (void *context)
{
	MFDSPEC spec = {{0,0,100,100},6,6,10,10};
	oapiRegisterExternMFD (new MFDWindow (g_hInst, spec), spec);
}

// ==============================================================
// Close the dialog

//void CloseDlg (HWND hDlg)
//{
//	oapiCloseDialog (hDlg);
//}

