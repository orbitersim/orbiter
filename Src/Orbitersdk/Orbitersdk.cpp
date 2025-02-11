// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ========================================================================
// To be linked into all Orbiter addon modules.
// Contains standard module entry point and version information.
// ========================================================================

typedef struct ModHandle ModHandle;

#define DLLCLBK extern "C" __declspec(dllexport)
#define OAPIFUNC __declspec(dllimport)

extern OAPIFUNC void InitLib (ModHandle*);
typedef void (*DLLEXIT)(ModHandle*);
extern "C" OAPIFUNC DLLEXIT SDL_LoadFunction(ModHandle*, const char*);

DLLCLBK void OrbitersdkModuleEntry (ModHandle* hModule, bool detach)
{
	static DLLEXIT DLLExit;

	if (!detach) {
		InitLib (hModule);
		DLLExit = SDL_LoadFunction (hModule, "ExitModule");
		if (!DLLExit) DLLExit = SDL_LoadFunction (hModule, "opcDLLExit");
	} else {
		if (DLLExit) (*DLLExit)(hModule);
	}
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

