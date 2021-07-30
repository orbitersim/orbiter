// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: DialogTemplate
//                    Part of the ORBITER SDK
//
// DialogTemplate.cpp
//
// This module demonstrates how to build an Orbiter plugin which
// opens a Windows dialog box. This is a good starting point for
// your own dialog-based addons.
// ==============================================================

#define STRICT
#define ORBITER_MODULE
#include "windows.h"
#include "orbitersdk.h"
#include "resource.h"
#include <stdio.h>

// ==============================================================
// Global variables
// ==============================================================

HINSTANCE g_hInst;  // module instance handle
DWORD g_dwCmd;      // custom function identifier
int myprm = 0;

// ==============================================================
// Local prototypes
// ==============================================================

void OpenDlgClbk (void *context);
LRESULT CALLBACK MsgProc (HWND, UINT, WPARAM, LPARAM);

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
	g_dwCmd = oapiRegisterCustomCmd ("My dialog",
		"Opens a test dialog box which doesn't do much.",
		OpenDlgClbk, NULL);
}

// ==============================================================
// This function is called when Orbiter shuts down or when the
// module is deactivated

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister the custom function in Orbiter
	oapiUnregisterCustomCmd (g_dwCmd);
}


// ==============================================================
// Write some parameters to the scenario file

DLLCLBK void opcSaveState (FILEHANDLE scn)
{
	oapiWriteScenario_int (scn, "Param", myprm);
}

// ==============================================================
// Read custom parameters from scenario

DLLCLBK void opcLoadState (FILEHANDLE scn)
{
	char *line;
	while (oapiReadScenario_nextline (scn, line)) {
		if (!_strnicmp (line, "Param", 5)) {
			sscanf (line+5, "%d", &myprm);
		}
	}
}

// ==============================================================
// Open the dialog window

void OpenDlgClbk (void *context)
{
	HWND hDlg = oapiOpenDialog (g_hInst, IDD_MYDIALOG, MsgProc);
	// Don't use a standard Windows function like CreateWindow to
	// open the dialog box, because it won't work in fullscreen mode
}

// ==============================================================
// Close the dialog

void CloseDlg (HWND hDlg)
{
	oapiCloseDialog (hDlg);
}

// ==============================================================
// Windows message handler for the dialog box

LRESULT CALLBACK MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	char name[256];

	switch (uMsg) {
	case WM_INITDIALOG:
		sprintf (name, "%d", myprm);
		SetWindowText (GetDlgItem (hDlg, IDC_REMEMBER), name);
		return TRUE;

	case WM_DESTROY:
		GetWindowText (GetDlgItem (hDlg, IDC_REMEMBER), name, 256);
		sscanf (name, "%d", &myprm);
		return TRUE;

	case WM_COMMAND:
		switch (LOWORD (wParam)) {

		case IDC_WHOAMI:  // user pressed dialog button
			// display the focus vessel name
			oapiGetObjectName (oapiGetFocusObject(), name, 256);
			SetWindowText (GetDlgItem (hDlg, IDC_IAM), name);
			return TRUE;

		case IDCANCEL: // dialog closed by user
			CloseDlg (hDlg);
			return TRUE;
		}
		break;
	}
	return oapiDefDialogProc (hDlg, uMsg, wParam, lParam);
}
