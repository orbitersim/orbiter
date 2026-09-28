// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//              ORBITER MODULE: Scenario Editor
//                  Part of the ORBITER SDK
//
// ScnEditor.cpp
//
// A plugin module to edit a scenario during the simulation.
// This allows creation, deleting and configuration of vessels.
// ==============================================================

#ifndef __linux__
#define STRICT 1
#else // __linux__
// STRICT left out: windows.h handle type-checking switch
#endif // __linux__
#define ORBITER_MODULE
#ifndef __linux__
#include "orbitersdk.h"
#else // __linux__
#include "Orbitersdk.h"
#include "OrbiterResource.h"
#endif // __linux__
#include "resource.h"
#include "Editor.h"
#include "DlgCtrl.h"
#ifdef __linux__
#include <QImage>
#endif // __linux__

// ==============================================================
// Global variables and constants
// ==============================================================

ScnEditor *g_editor = 0;   // scenario editor instance pointer
#ifndef __linux__
HBITMAP g_hPause;          // "pause" button bitmap
#else // __linux__
QImage *g_hPause;          // "pause" button bitmap
#endif // __linux__

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// Initialise module

#ifndef __linux__
DLLCLBK void InitModule (HINSTANCE hDLL)
#else // __linux__
DLLCLBK void InitModule (void *hDLL)
#endif // __linux__
{
#ifndef __linux__
	INITCOMMONCONTROLSEX cc = {sizeof(INITCOMMONCONTROLSEX),ICC_TREEVIEW_CLASSES};
	InitCommonControlsEx(&cc);
#else // __linux__
	// InitCommonControlsEx left out: the tree view is a Qt widget
#endif // __linux__
	// Windows tree view control registration

	// Create editor instance
	g_editor = new ScnEditor (hDLL);

	// Register custom dialog controls
	oapiRegisterCustomControls (hDLL);

	// Load the bitmap for the "pause" title button
#ifndef __linux__
	g_hPause = (HBITMAP)LoadImage (hDLL, MAKEINTRESOURCE (IDB_PAUSE), IMAGE_BITMAP, 15, 30, 0);
#else // __linux__
	g_hPause = oapiLoadResImage (hDLL, IDB_PAUSE);
	if (g_hPause) *g_hPause = g_hPause->scaled (15, 30); // LoadImage size
#endif // __linux__
}

// ==============================================================
// Clean up module

#ifndef __linux__
DLLCLBK void ExitModule (HINSTANCE hDLL)
#else // __linux__
DLLCLBK void ExitModule (void *hDLL)
#endif // __linux__
{
	// Delete editor instance
	delete g_editor;
	g_editor = 0;

	// Unregister custom dialog controls
	oapiUnregisterCustomControls (hDLL);

	// Free bitmap resources
#ifndef __linux__
	DeleteObject (g_hPause);
#else // __linux__
	delete g_hPause;
#endif // __linux__
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
