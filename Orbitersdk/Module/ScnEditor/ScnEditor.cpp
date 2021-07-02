// ==============================================================
//              ORBITER MODULE: Scenario Editor
//                  Part of the ORBITER SDK
//            Copyright (C) 2006 Martin Schweiger
//                   All rights reserved
//
// ScnEditor.cpp
//
// A plugin module to edit a scenario during the simulation.
// This allows creation, deleting and configuration of vessels.
// ==============================================================

#define STRICT 1
#define ORBITER_MODULE
#include "orbitersdk.h"
#include "resource.h"
#include "Editor.h"
#include "DlgCtrl.h"

// ==============================================================
// Global variables and constants
// ==============================================================

ScnEditor *g_editor = 0;   // scenario editor instance pointer
HBITMAP g_hPause;          // "pause" button bitmap

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// Initialise module

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	INITCOMMONCONTROLSEX cc = {sizeof(INITCOMMONCONTROLSEX),ICC_TREEVIEW_CLASSES};
	InitCommonControlsEx(&cc);
	// Windows tree view control registration

	// Create editor instance
	g_editor = new ScnEditor (hDLL);

	// Register custom dialog controls
	oapiRegisterCustomControls (hDLL);

	// Load the bitmap for the "pause" title button
	g_hPause = (HBITMAP)LoadImage (hDLL, MAKEINTRESOURCE (IDB_PAUSE), IMAGE_BITMAP, 15, 30, 0);
}

// ==============================================================
// Clean up module

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Delete editor instance
	delete g_editor;
	g_editor = 0;

	// Unregister custom dialog controls
	oapiUnregisterCustomControls (hDLL);

	// Free bitmap resources
	DeleteObject (g_hPause);
}

// ==============================================================
// Vessel destruction notification

DLLCLBK void opcDeleteVessel (OBJHANDLE hVessel)
{
	g_editor->VesselDeleted (hVessel);
}

// ==============================================================
// Pause state change notification

DLLCLBK void opcPause (bool pause)
{
	g_editor->Pause (pause);
}