// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __linux__
#define STRICT 1
#endif // !__linux__
#define OAPI_IMPLEMENTATION
#include "Orbitersdk.h"
#include "Orbiter.h"
#ifdef __linux__
#include "Util.h"
#endif // __linux__

extern Orbiter *g_pOrbiter;
extern TimeData td;

using namespace oapi;

// ======================================================================
// class ModuleNV
// ======================================================================

#ifndef __linux__
ModuleNV::ModuleNV (HINSTANCE hDLL)
#else // __linux__
ModuleNV::ModuleNV (void *hDLL)
#endif // __linux__
{
	version = 0;
	hModule = hDLL;
}

// ======================================================================

double ModuleNV::GetSimTime () const
{
	return td.SimT0;
}

// ======================================================================

double ModuleNV::GetSimStep () const
{
	return td.SimDT;
}

// ======================================================================

double ModuleNV::GetSimMJD () const
{
	return td.MJD0;
}

// ======================================================================
// class Module
// ======================================================================

#ifndef __linux__
Module::Module (HINSTANCE hDLL): ModuleNV (hDLL)
#else // __linux__
Module::Module (void *hDLL): ModuleNV (hDLL)
#endif // __linux__
{
	version = 1;
}

// ======================================================================

Module::~Module ()
{
}

// ======================================================================

void Module::clbkSimulationStart (RenderMode mode)
{
	// backward compatibility call (deprecated)
#ifndef __linux__
	void (*opcOpenRenderViewport)(HWND,DWORD,DWORD,BOOL) = (void(*)(HWND,DWORD,DWORD,BOOL))GetProcAddress (hModule, "opcOpenRenderViewport");
#else // __linux__
	void (*opcOpenRenderViewport)(QWindow*,DWORD,DWORD,BOOL) = (void(*)(QWindow*,DWORD,DWORD,BOOL))ModuleProc (hModule, "opcOpenRenderViewport");
#endif // __linux__
	if (opcOpenRenderViewport) opcOpenRenderViewport (g_pOrbiter->GetRenderWnd(), g_pOrbiter->ViewW(), g_pOrbiter->ViewH(), g_pOrbiter->IsFullscreen()?TRUE:FALSE);
}

// ======================================================================

void Module::clbkSimulationEnd ()
{
	// backward compatibility call (deprecated)
#ifndef __linux__
	void (*opcCloseRenderViewport)() = (void(*)())GetProcAddress (hModule, "opcCloseRenderViewport");
#else // __linux__
	void (*opcCloseRenderViewport)() = (void(*)())ModuleProc (hModule, "opcCloseRenderViewport");
#endif // __linux__
	if (opcCloseRenderViewport) opcCloseRenderViewport();
}

// ======================================================================

void Module::clbkPreStep (double simt, double simdt, double mjd)
{
	// backward compatibility call (deprecated)
#ifndef __linux__
	void (*opcPreStep)(double,double,double) = (void(*)(double,double,double))GetProcAddress (hModule, "opcPreStep");
#else // __linux__
	void (*opcPreStep)(double,double,double) = (void(*)(double,double,double))ModuleProc (hModule, "opcPreStep");
#endif // __linux__
	if (opcPreStep) opcPreStep (simt, simdt, mjd);
}

// ======================================================================

void Module::clbkPostStep (double simt, double simdt, double mjd)
{
	// backward compatibility call (deprecated)
#ifndef __linux__
	void (*opcPostStep)(double,double,double) = (void(*)(double,double,double))GetProcAddress (hModule, "opcPostStep");
#else // __linux__
	void (*opcPostStep)(double,double,double) = (void(*)(double,double,double))ModuleProc (hModule, "opcPostStep");
#endif // __linux__
	if (opcPostStep) opcPostStep (simt, simdt, mjd);
}

// ======================================================================

void Module::clbkFocusChanged (OBJHANDLE new_focus, OBJHANDLE old_focus)
{
	// backward compatibility call (deprecated)
#ifndef __linux__
	void (*opcFocusChanged)(OBJHANDLE,OBJHANDLE) = (void(*)(OBJHANDLE,OBJHANDLE))GetProcAddress (hModule, "opcFocusChanged");
#else // __linux__
	void (*opcFocusChanged)(OBJHANDLE,OBJHANDLE) = (void(*)(OBJHANDLE,OBJHANDLE))ModuleProc (hModule, "opcFocusChanged");
#endif // __linux__
	if (opcFocusChanged) opcFocusChanged (new_focus, old_focus);

}

// ======================================================================

void Module::clbkTimeAccChanged (double new_warp, double old_warp)
{
	// backward compatibility call (deprecated)
#ifndef __linux__
	void (*opcTimeAccChanged)(double,double) = (void(*)(double,double))GetProcAddress (hModule, "opcTimeAccChanged");
#else // __linux__
	void (*opcTimeAccChanged)(double,double) = (void(*)(double,double))ModuleProc (hModule, "opcTimeAccChanged");
#endif // __linux__
	if (opcTimeAccChanged) opcTimeAccChanged (new_warp, old_warp);
}

// ======================================================================

void Module::clbkDeleteVessel (OBJHANDLE hVessel)
{
	// backward compatibility call (deprecated)
#ifndef __linux__
	void (*opcDelVessel)(OBJHANDLE) = (void(*)(OBJHANDLE))GetProcAddress (hModule, "opcDeleteVessel");
#else // __linux__
	void (*opcDelVessel)(OBJHANDLE) = (void(*)(OBJHANDLE))ModuleProc (hModule, "opcDeleteVessel");
#endif // __linux__
	if (opcDelVessel) opcDelVessel (hVessel);
}

// ======================================================================

void Module::clbkPause (bool pause)
{
	// backward compatibility call (deprecated)
#ifndef __linux__
	void (*opcPause)(bool) = (void(*)(bool))GetProcAddress (hModule, "opcPause");
#else // __linux__
	void (*opcPause)(bool) = (void(*)(bool))ModuleProc (hModule, "opcPause");
#endif // __linux__
	if (opcPause) opcPause (pause);
}
