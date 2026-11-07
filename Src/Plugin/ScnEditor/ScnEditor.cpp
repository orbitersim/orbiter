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

#define ORBITER_MODULE
#include "orbitersdk.h"
#include "Editor.h"

ORBITER_MODULE_INFO("Tools and dialogs",
	"SCENARIO EDITOR:\r\n\r\nCreate and delete spacecraft during the simulation - Edit position, orientation, orbital elements, surface location, fuel status and docking connections - Reset the simulation time.\r\n\r\nThe scenario editor can be opened by selecting the \"Scenario editor\" entry in the Custom Functions dialog (Ctrl-F4).");

ScnEditor *g_editor = 0;

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	g_editor = new ScnEditor;
}

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	delete g_editor;
	g_editor = 0;
}

DLLCLBK void opcDeleteVessel (OBJHANDLE hVessel)
{
	if (g_editor) g_editor->VesselDeleted (hVessel);
}
