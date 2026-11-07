// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ========================================================================
// To be linked into all Orbiter addon modules.
// Contains standard module entry point and version information.
// ========================================================================

#include <windows.h>
#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <fstream>
#include <stdio.h>

#define DLLCLBK extern "C" __attribute__((visibility("default")))

extern void InitLib (HINSTANCE hModule);
extern void ExitLib (HINSTANCE hModule);
extern int Date2Int (char *date);

namespace {

HINSTANCE g_hThisModule = NULL;

HINSTANCE ThisModuleHandle ()
{
	Dl_info info;
	if (!dladdr((const void*)&ThisModuleHandle, &info) || !info.dli_fname)
		return NULL;
	void *h = dlopen(info.dli_fname, RTLD_NOLOAD | RTLD_LAZY);
	if (h) dlclose(h);
	return (HINSTANCE)h;
}

bool IsMainExecutable ()
{
	Dl_info info;
	if (!dladdr((const void*)&ThisModuleHandle, &info))
		return false;
	return info.dli_fbase == (const void*)_dyld_get_image_header(0);
}

bool g_attached = false;

}

__attribute__((constructor)) static void OrbiterModuleAttach ()
{
	if (IsMainExecutable())
		return;
	g_hThisModule = ThisModuleHandle();
	if (g_hThisModule) {
		g_attached = true;
		InitLib(g_hThisModule);
	}
}

DLLCLBK void OrbiterModuleDetach ()
{
	if (g_attached) {
		g_attached = false;
		ExitLib(g_hThisModule);
	}
}

int oapiGetModuleVersion ()
{
	static int v = 0;
	if (!v)
		v = Date2Int ((char*)__DATE__);
	return v;
}

DLLCLBK int GetModuleVersion (void)
{
	return oapiGetModuleVersion();
}

void dummy () {}
