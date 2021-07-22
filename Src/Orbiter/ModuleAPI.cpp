// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION
#include "Orbitersdk.h"
#include "Orbiter.h"

extern Orbiter *g_pOrbiter;
extern TimeData td;

using namespace oapi;

// ======================================================================
// class ModuleNV
// ======================================================================

ModuleNV::ModuleNV (HINSTANCE hDLL)
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

Module::Module (HINSTANCE hDLL): ModuleNV (hDLL)
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
	void (*opcOpenRenderViewport)(HWND,DWORD,DWORD,BOOL) = (void(*)(HWND,DWORD,DWORD,BOOL))GetProcAddress (hModule, "opcOpenRenderViewport");
	if (opcOpenRenderViewport) opcOpenRenderViewport (g_pOrbiter->GetRenderWnd(), g_pOrbiter->ViewW(), g_pOrbiter->ViewH(), g_pOrbiter->IsFullscreen()?TRUE:FALSE);
}

// ======================================================================

void Module::clbkSimulationEnd ()
{
	// backward compatibility call (deprecated)
	void (*opcCloseRenderViewport)() = (void(*)())GetProcAddress (hModule, "opcCloseRenderViewport");
	if (opcCloseRenderViewport) opcCloseRenderViewport();
}

// ======================================================================

void Module::clbkPreStep (double simt, double simdt, double mjd)
{
	// backward compatibility call (deprecated)
	void (*opcPreStep)(double,double,double) = (void(*)(double,double,double))GetProcAddress (hModule, "opcPreStep");
	if (opcPreStep) opcPreStep (simt, simdt, mjd);
}

// ======================================================================

void Module::clbkPostStep (double simt, double simdt, double mjd)
{
	// backward compatibility call (deprecated)
	void (*opcPostStep)(double,double,double) = (void(*)(double,double,double))GetProcAddress (hModule, "opcPostStep");
	if (opcPostStep) opcPostStep (simt, simdt, mjd);
}

// ======================================================================

void Module::clbkFocusChanged (OBJHANDLE new_focus, OBJHANDLE old_focus)
{
	// backward compatibility call (deprecated)
	void (*opcFocusChanged)(OBJHANDLE,OBJHANDLE) = (void(*)(OBJHANDLE,OBJHANDLE))GetProcAddress (hModule, "opcFocusChanged");
	if (opcFocusChanged) opcFocusChanged (new_focus, old_focus);

}

// ======================================================================

void Module::clbkTimeAccChanged (double new_warp, double old_warp)
{
	// backward compatibility call (deprecated)
	void (*opcTimeAccChanged)(double,double) = (void(*)(double,double))GetProcAddress (hModule, "opcTimeAccChanged");
	if (opcTimeAccChanged) opcTimeAccChanged (new_warp, old_warp);
}

// ======================================================================

void Module::clbkDeleteVessel (OBJHANDLE hVessel)
{
	// backward compatibility call (deprecated)
	void (*opcDelVessel)(OBJHANDLE) = (void(*)(OBJHANDLE))GetProcAddress (hModule, "opcDeleteVessel");
	if (opcDelVessel) opcDelVessel (hVessel);
}

// ======================================================================

void Module::clbkPause (bool pause)
{
	// backward compatibility call (deprecated)
	void (*opcPause)(bool) = (void(*)(bool))GetProcAddress (hModule, "opcPause");
	if (opcPause) opcPause (pause);
}
