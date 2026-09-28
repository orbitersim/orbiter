// ==============================================================          
// Copyright (C) 2006-2026 Martin Schweiger
// Licensed under the MIT License
// ==============================================================
//
// ExtMFD.cpp
//
// Open multifunctional displays (MFD) in external windows
// ==============================================================

// STRICT left out: windows.h handle type-checking switch
#define ORBITER_MODULE
// windows.h left out: the Win32 types come from OrbiterPlatform.h
#include "MFDWindow.h"
#include "Orbitersdk.h"
#include "OrbiterResource.h"
#include "resource.h"
#include <QImage>
#include <stdio.h>

// ==============================================================
// Global variables
// ==============================================================

void *g_hInst;        // module instance handle
QImage *g_hPin;       // "pin" button bitmap
DWORD g_dwCmd;        // custom function identifier

// ==============================================================
// Local prototypes
// ==============================================================

void OpenDlgClbk (void *context);
void MsgProc (QWidget*, void*);
extern QWidget *MFD_DisplayCtrl (const RESCONTROL *ctrl, QWidget *parent); // window class "ExtMFD_Display" (MFD_WndProc)
extern QWidget *MFD_ButtonCtrl (const RESCONTROL *ctrl, QWidget *parent);  // window class "ExtMFD_Button" (MFD_BtnProc)

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// This function is called when Orbiter starts or when the module
// is activated.

DLLCLBK void InitModule (void *hDLL)
{
	g_hInst = hDLL; // remember the instance handle

	// To allow the user to open our new dialog box, we create
	// an entry in the "Custom Functions" list which is accessed
	// in Orbiter via Ctrl-F4.
	g_dwCmd = oapiRegisterCustomCmd ((char*)"Vulkan External MFD", // not upstream: Vulkan in place of DX9
		(char*)"Opens a multifunctional display in an external window",
		OpenDlgClbk, NULL);

	// Load the bitmap for the "pin" title button
	g_hPin = oapiLoadResImage (g_hInst, IDB_PIN);
	if (g_hPin) *g_hPin = g_hPin->scaled (15, 30); // LoadImage size

	// Register a window classes for the MFD display and buttons
	// WNDCLASS: procedure and background brush are in the widgets the factories make; the cursor is Qt's arrow
	oapiRegisterResControl (hDLL, "ExtMFD_Display", MFD_DisplayCtrl);
	oapiRegisterResControl (hDLL, "ExtMFD_Button", MFD_ButtonCtrl);
}

// ==============================================================
// This function is called when Orbiter shuts down or when the
// module is deactivated

DLLCLBK void ExitModule (void *hDLL)
{
	// Unregister window classes
	oapiUnregisterResControl (g_hInst, "ExtMFD_Display");
	oapiUnregisterResControl (g_hInst, "ExtMFD_Button");

	// Free bitmap resources
	delete g_hPin;

	// Unregister the custom function in Orbiter
	oapiUnregisterCustomCmd (g_dwCmd);
}


// ==============================================================
// Write some parameters to the scenario file

//DLLCLBK void opcSaveState (FILEHANDLE scn)
//{
//	oapiWriteScenario_int (scn, "Param", myprm);
//}

// ==============================================================
// Read custom parameters from scenario

//DLLCLBK void opcLoadState (FILEHANDLE scn)
//{
//	char *line;
//	while (oapiReadScenario_nextline (scn, line)) {
//		if (!strnicmp (line, "Param", 5)) {
//			sscanf (line+5, "%d", &myprm);
//		}
//	}
//}

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


