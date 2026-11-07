// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: DialogTemplate
//                    Part of the ORBITER SDK
//
// DialogTemplate.cpp
//
// This module demonstrates how to build an Orbiter plugin which
// opens an ImGui dialog box. This is a good starting point for
// your own dialog-based addons.
// ==============================================================

#define ORBITER_MODULE
#include "orbitersdk.h"
#include "imgui.h"
#include <stdio.h>

ORBITER_MODULE_INFO("SDK sample plugins",
	"DIALOG TEMPLATE:\r\n\r\nA simple SDK example which provides a dialog box that can be opened during a simulation session via the \"My dialog\" entry in the Custom Functions list (Ctrl-F4).\r\n\r\nThis addon is not very useful - it is intended as a starting point for developers.");

// ==============================================================
// Global variables
// ==============================================================

DWORD g_dwCmd;      // custom function identifier
int myprm = 0;      // a parameter stored in the scenario file

// ==============================================================
// The dialog
// ==============================================================

// Derive from ImGuiDialog and draw the contents in OnDraw.
// Orbiter calls ImGui::Begin/End around OnDraw and provides
// the close button in the title bar.

class MyDialog: public ImGuiDialog {
public:
	MyDialog (): ImGuiDialog ("Orbiter dialog template", {300.0f, 150.0f}) {}

protected:
	void OnDraw () override
	{
		if (ImGui::Button ("Who am I?")) {
			// display the focus vessel name
			oapiGetObjectName (oapiGetFocusObject(), iam, 256);
		}
		ImGui::Text ("%s", iam);
		ImGui::Separator ();
		ImGui::SetNextItemWidth (ImGui::GetFontSize() * 8.0f);
		ImGui::InputInt ("Remember this", &myprm);
		if (ImGui::Button ("Close"))
			oapiCloseDialog (this);
	}

private:
	char iam[256] = "";
};

MyDialog *g_dlg = 0;

void OpenDlgClbk (void *context)
{
	oapiOpenDialog (g_dlg);
}

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// This function is called when Orbiter starts or when the module
// is activated.

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	g_dlg = new MyDialog;

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
	delete g_dlg;
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
