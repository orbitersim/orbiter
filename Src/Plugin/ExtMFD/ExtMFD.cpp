// Copyright (c) Martin Schweiger
// Licensed under the MIT License

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

#define ORBITER_MODULE
#include "MFDWindow.h"
#include "orbitersdk.h"

// ==============================================================
// Global variables
// ==============================================================

DWORD g_dwCmd;        // custom function identifier

// ==============================================================
// Local prototypes
// ==============================================================

void OpenDlgClbk (void *context);

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// This function is called when Orbiter starts or when the module
// is activated.

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	// To allow the user to open our new dialog box, we create
	// an entry in the "Custom Functions" list which is accessed
	// in Orbiter via Ctrl-F4.
	g_dwCmd = oapiRegisterCustomCmd ((char*)"External MFD",
		(char*)"Opens a multifunctional display in an external window",
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
	oapiRegisterExternMFD (new MFDWindow (spec), spec);
}

// ==============================================================
// Close the dialog

//void CloseDlg (HWND hDlg)
//{
//	oapiCloseDialog (hDlg);
//}

