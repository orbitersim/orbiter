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

#ifndef __linux__
#define STRICT
#else // __linux__
// STRICT left out: windows.h handle type-checking switch
#endif // __linux__
#define ORBITER_MODULE
#ifndef __linux__
#include "windows.h"
#include "orbitersdk.h"
#else // __linux__
// windows.h left out: the dialog controls are Qt widgets
#include "Orbitersdk.h"
#include "OrbiterResource.h"
#endif // __linux__
#include "resource.h"
#include <stdio.h>
#ifdef __linux__
#include <strings.h>
#include <functional>
#include <QEvent>
#endif // __linux__

// ==============================================================
// Global variables
// ==============================================================

#ifndef __linux__
HINSTANCE g_hInst;  // module instance handle
#else // __linux__
void *g_hInst;      // module instance handle
#endif // __linux__
DWORD g_dwCmd;      // custom function identifier
int myprm = 0;

// ==============================================================
// Local prototypes
// ==============================================================

void OpenDlgClbk (void *context);
#ifndef __linux__
INT_PTR CALLBACK MsgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
void MsgProc (QWidget*, void*);
#endif // __linux__

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// This function is called when Orbiter starts or when the module
// is activated.

#ifndef __linux__
DLLCLBK void InitModule (HINSTANCE hDLL)
#else // __linux__
DLLCLBK void InitModule (void *hDLL)
#endif // __linux__
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

#ifndef __linux__
DLLCLBK void ExitModule (HINSTANCE hDLL)
#else // __linux__
DLLCLBK void ExitModule (void *hDLL)
#endif // __linux__
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
#ifndef __linux__
		if (!_strnicmp (line, "Param", 5)) {
#else // __linux__
		if (!strncasecmp (line, "Param", 5)) {
#endif // __linux__
			sscanf (line+5, "%d", &myprm);
		}
	}
}

// ==============================================================
// Open the dialog window

void OpenDlgClbk (void *context)
{
#ifndef __linux__
	HWND hDlg = oapiOpenDialog (g_hInst, IDD_MYDIALOG, MsgProc);
#else // __linux__
	QWidget *hDlg = oapiOpenDialog (g_hInst, IDD_MYDIALOG, MsgProc);
#endif // __linux__
	// Don't use a standard Windows function like CreateWindow to
	// open the dialog box, because it won't work in fullscreen mode
}

// ==============================================================
// Close the dialog

#ifndef __linux__
void CloseDlg (HWND hDlg)
#else // __linux__
void CloseDlg (QWidget *hDlg)
#endif // __linux__
{
	oapiCloseDialog (hDlg);
}

#ifdef __linux__
// not upstream: WM_DESTROY comes while the controls still exist; here that is the dialog's deferred delete
class DestroyHook: public QObject {
public:
	DestroyHook (QWidget *hWnd, std::function<void()> f): QObject (hWnd), onDestroy (f) { hWnd->installEventFilter (this); }
	bool eventFilter (QObject *o, QEvent *e) override {
		if (e->type() == QEvent::DeferredDelete) onDestroy();
		return false;
	}
private:
	std::function<void()> onDestroy;
};

#endif // __linux__
// ==============================================================
// Windows message handler for the dialog box

#ifndef __linux__
INT_PTR CALLBACK MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void MsgProc (QWidget *hDlg, void *context)
#endif // __linux__
{
	char name[256];

#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG:
#else // __linux__
	// WM_INITDIALOG
#endif // __linux__
		sprintf (name, "%d", myprm);
#ifndef __linux__
		SetWindowText (GetDlgItem (hDlg, IDC_REMEMBER), name);
		return TRUE;
#else // __linux__
		oapiSetDlgItemText (hDlg, IDC_REMEMBER, name);
#endif // __linux__

#ifndef __linux__
	case WM_DESTROY:
		GetWindowText (GetDlgItem (hDlg, IDC_REMEMBER), name, 256);
		sscanf (name, "%d", &myprm);
		return TRUE;
#else // __linux__
	// WM_DESTROY
	new DestroyHook (hDlg, [hDlg]() {
		char name[256];
		oapiGetDlgItemText (hDlg, IDC_REMEMBER, name, 256);
		sscanf (name, "%d", &myprm);
	});
#endif // __linux__

#ifndef __linux__
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
#else // __linux__
	// WM_COMMAND
	oapiConnectDlgCommands (hDlg, [hDlg](int id, int code, QWidget *hCtrl) {
		char name[256];
		switch (id) {

		case IDC_WHOAMI:  // user pressed dialog button
			// display the focus vessel name
			oapiGetObjectName (oapiGetFocusObject(), name, 256);
			oapiSetDlgItemText (hDlg, IDC_IAM, name);
			return;

		case IDCANCEL: // dialog closed by user
			CloseDlg (hDlg);
			return;
		}
	});
	// oapiDefDialogProc left out: oapiOpenDialog wires the default dialog behaviour
#endif // __linux__
}
