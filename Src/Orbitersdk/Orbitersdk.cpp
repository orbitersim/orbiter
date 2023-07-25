// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ========================================================================
// To be linked into all Orbiter addon modules.
// Contains standard module entry point and version information.
// ========================================================================

#include <windows.h>
#include <fstream>
#include <stdio.h>

#define DLLCLBK extern "C" __declspec(dllexport)
#define OAPIFUNC __declspec(dllimport)

BOOL WINAPI DllMain (HINSTANCE hModule,
					 DWORD ul_reason_for_call,
					 LPVOID lpReserved)
{
	OAPIFUNC void InitLib (HINSTANCE hModule);
	typedef void (*DLLEXIT)(HINSTANCE);
	static DLLEXIT DLLExit;

	switch (ul_reason_for_call) {
	case DLL_PROCESS_ATTACH:
		InitLib (hModule);
		DLLExit = (DLLEXIT)GetProcAddress (hModule, "ExitModule");
		if (!DLLExit) DLLExit = (DLLEXIT)GetProcAddress (hModule, "opcDLLExit");
		break;
	case DLL_PROCESS_DETACH:
		if (DLLExit) (*DLLExit)(hModule);
		break;
	}
	return TRUE;
}

int oapiGetModuleVersion ()
{
	static int v = 0;
	if (!v) {
		OAPIFUNC int Date2Int (char *date);
		v = Date2Int ((char*)__DATE__);
	}
	return v;
}

DLLCLBK int GetModuleVersion (void)
{
	return oapiGetModuleVersion();
}

void dummy () {}

