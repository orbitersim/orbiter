// ==============================================================
// Copyright (C) 2006-2026 Martin Schweiger
// Licensed under the MIT License
// ==============================================================
//
// ExtMFD.cpp
//
// Open multifunctional displays (MFD) in external windows
// ==============================================================

#define ORBITER_MODULE
#include "MFDWindow.h"
#include "orbitersdk.h"

ORBITER_MODULE_INFO("Tools and dialogs",
	"EXTERNAL MFD:\r\n\r\nProvides support for displaying MFD (multi-functional display) instruments in separate windows.\r\n\r\nExternal MFD windows can be opened by selecting the \"DX9 External MFD\" entry in the Custom Functions dialog (Ctrl-F4).");

DWORD g_dwCmd;

void OpenDlgClbk (void *context);

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	g_dwCmd = oapiRegisterCustomCmd ((char*)"DX9 External MFD",
		(char*)"Opens a multifunctional display in an external window",
		OpenDlgClbk, NULL);
}

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	oapiUnregisterCustomCmd (g_dwCmd);
}

void OpenDlgClbk (void *context)
{
	MFDSPEC spec = {{0,0,100,100},6,6,10,10};
	oapiRegisterExternMFD (new MFDWindow (spec), spec);
}
