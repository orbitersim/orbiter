// ==============================================================
//                 ORBITER MODULE: GC API Info MFD
//                  Part of the gcAPI SDK
//            Copyright (C) 2017 Peter Schneider
//                   All rights reserved
//
// GCInfoMFD.cpp
//
// This module demonstrates how to access the gcAPI functionality.
// The MFD code itself just shows information, but it can be used as a
// starting point for own developments.
// ==============================================================

#define STRICT
#define ORBITER_MODULE
#include "windows.h"
#include "orbitersdk.h"
#include "gcAPI.h" // <= what it's all about ;)

#include "GCInfoMFD.h"

// ==============================================================
// Global variables

int g_MFDmode; // identifier for new MFD mode

// ==============================================================
// API interface

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	static char *name = "GC Info MFD";   // MFD mode name
	MFDMODESPECEX spec;
	spec.name = name;
	spec.key = OAPI_KEY_T;               // MFD mode selection key
	spec.context = NULL;
	spec.msgproc = GCInfoMFD::MsgProc;   // MFD mode callback function

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
GCInfoMFD::GCInfoMFD (DWORD w, DWORD h, VESSEL *vessel)
	: MFD2 (w, h, vessel)
{
	info.sketchpadVersion = -1;

	// Init gcAPI and gather some info
	info.initialized = gcInitialize();
	info.enabled     = gcEnabled();
	info.clientId    = gcClientID();

	font = oapiCreateFont(w/20, true, "Arial", FONT_NORMAL);
}

// Destructor
GCInfoMFD::~GCInfoMFD ()
{
	oapiReleaseFont(font);
}

// clientId to string helper
static const char *clientId2String (DWORD clientId) {
	return clientId ? (clientId==DWORD('D3D9')?"D3D9":"???") : "Inline Client";
}

// Repaint the MFD
bool GCInfoMFD::Update (oapi::Sketchpad *skp)
{
	// Get the Sketchpad version info if not yet done (via gcAPI)
	if (info.sketchpadVersion == -1) {
		info.sketchpadVersion = gcSketchpadVersion(skp);
	}

	Title(skp, "GC Info MFD"); // Draw the MFD title

	skp->SetFont(font);
	skp->SetTextAlign(oapi::Sketchpad::LEFT, oapi::Sketchpad::BASELINE);
	skp->SetTextColor(0x00FF00);

	int top = 60, left = 10, XX = (W*2/5);
	skp->Text(left, top+20, "Initialized", 11);      skp->Text(left+XX, top+20, ":", 1);
	skp->Text(left, top+40, "Enabled", 7);           skp->Text(left+XX, top+40, ":", 1);
	skp->Text(left, top+60, "Client ID", 9);         skp->Text(left+XX, top+60, ":", 1);
	skp->Text(left, top+80, "Sketchpad Version", 17);skp->Text(left+XX, top+80, ":", 1);

	char buffer[128] = "";
	sprintf_s(buffer, _ARRAYSIZE(buffer), "0x%x (%4s)", info.clientId, clientId2String(info.clientId));

	skp->Text(XX+2*left, top+20, info.initialized ? "true" : "false", 5);
	skp->Text(XX+2*left, top+40, info.enabled     ? "true" : "false", 5);
	skp->Text(XX+2*left, top+60, buffer, strlen(buffer));
	skp->Text(XX+2*left, top+80, _itoa(info.sketchpadVersion, buffer, 10), 1);

	return true;
}

// MFD message parser
int GCInfoMFD::MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam)
{
	switch (msg) {
	case OAPI_MSG_MFD_OPENED:
		// Our new MFD mode has been selected, so we create the MFD and
		// return a pointer to it.
		return (int)(new GCInfoMFD(LOWORD(wparam), HIWORD(wparam), (VESSEL*)lparam));
	}
	return 0;
}
