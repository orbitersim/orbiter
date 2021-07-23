// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: DialogTemplate
//                  Part of the ORBITER SDK
//
// MFDTemplate.cpp
//
// This module demonstrates how to build an Orbiter plugin which
// inserts a new MFD (multi-functional display) mode. The code
// is not very useful in itself, but it can be used as a starting
// point for your own MFD developments.
// ==============================================================

#define STRICT
#define ORBITER_MODULE
#include "windows.h"
#include "orbitersdk.h"
#include "MFDTemplate.h"

// ==============================================================
// Global variables

int g_MFDmode; // identifier for new MFD mode

// ==============================================================
// API interface

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	static char *name = "MFD Template";   // MFD mode name
	MFDMODESPECEX spec;
	spec.name = name;
	spec.key = OAPI_KEY_T;                // MFD mode selection key
	spec.context = NULL;
	spec.msgproc = MFDTemplate::MsgProc;  // MFD mode callback function

	// Register the new MFD mode with Orbiter
	g_MFDmode = oapiRegisterMFDMode (spec);
}

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister the custom MFD mode when the module is unloaded
	oapiUnregisterMFDMode (g_MFDmode);
}

// ==============================================================
// MFD class implementation

// Constructor
MFDTemplate::MFDTemplate (DWORD w, DWORD h, VESSEL *vessel)
: MFD2 (w, h, vessel)
{
	font = oapiCreateFont (w/20, true, "Arial", FONT_NORMAL, 450);
	// Add MFD initialisation here
}

// Destructor
MFDTemplate::~MFDTemplate ()
{
	oapiReleaseFont (font);
	// Add MFD cleanup code here
}

// Return button labels
char *MFDTemplate::ButtonLabel (int bt)
{
	// The labels for the two buttons used by our MFD mode
	static char *label[2] = {"UP", "DN"};
	return (bt < 2 ? label[bt] : 0);
}

// Return button menus
int MFDTemplate::ButtonMenu (const MFDBUTTONMENU **menu) const
{
	// The menu descriptions for the two buttons
	static const MFDBUTTONMENU mnu[2] = {
		{"Move up", 0, '['},
		{"Move down", 0, ']'}
	};
	if (menu) *menu = mnu;
	return 2; // return the number of buttons used
}


// Repaint the MFD
bool MFDTemplate::Update (oapi::Sketchpad *skp)
{
	Title (skp, "MFD Template");
	// Draws the MFD title

	skp->SetFont (font);
	skp->SetTextAlign (oapi::Sketchpad::CENTER, oapi::Sketchpad::BASELINE);
	skp->SetTextColor (0x00FFFF);
	skp->Text (W/2, H/2,"Display area", 12);
	skp->Rectangle (W/4, H/4, (3*W)/4, (3*H)/4);

	// Add MFD display routines here.
	// Use the device context (hDC) for Windows GDI paint functions.

	return true;
}

// MFD message parser
int MFDTemplate::MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam)
{
	switch (msg) {
	case OAPI_MSG_MFD_OPENED:
		// Our new MFD mode has been selected, so we create the MFD and
		// return a pointer to it.
		return (int)(new MFDTemplate (LOWORD(wparam), HIWORD(wparam), (VESSEL*)lparam));
	}
	return 0;
}

