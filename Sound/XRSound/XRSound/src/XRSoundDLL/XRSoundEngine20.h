// ==============================================================
// XRSoundEngine20.h : Defines the XRSoundEngine version 2.x interface.
//
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include "XRSoundEngine10.h"
#include "Orbitersdk.h"  // for OBJHANDLE

// NOTE: do not change the order of these methods!  We need to maintain compatibility with 
// previous versions of the static client library (XRSound.lib) that uses this engine object.

class XRSoundEngine20 : public XRSoundEngine10
{
public:
    // these are not exposed in the XRSound.h public API; however, we use them internally on the client side
    enum class EngineType { Unknown = -1, Vessel, Module };   // `Unknown` can only occur with XRSound 2.x+ vessels communicating with XRSound.dll version 1.0
    virtual EngineType GetEngineType() = 0;
    virtual const char *GetLogID() = 0;  // e.g., vessel or module name
};

// These are used to dynamically bind to DLL-exported methods.  Note that DLLCLBK specifies 'extern "C"', which uses the __cdecl
// calling convention, NOT the normal __stdcall that C++ uses.
extern "C" typedef XRSoundEngine20 *(__cdecl *VesselXRSoundEngineInstanceFuncPtr)(OBJHANDLE hVessel);
extern "C" typedef XRSoundEngine20 *(__cdecl *ModuleXRSoundEngineInstanceFuncPtr)(const char *pUniqueModuleName);

// {XXX} UPDATE THIS FOR THE CURRENT BUILD VERSION; DO NOT REMOVE THIS {XXX} COMMENT
// Note: set XRSOUND_BETA to empty string if this is not a beta build.
// If beta build, remember to add a trailing space to the string; e.g., "Beta-1 "; otherwise, make it "".
#define XRSOUND_ENGINE_VERSION 3.0f
#define XRSOUND_BETA_STR "RC1 "

// for use by build version strings
#ifdef _WIN64
#define ARCH_TYPE "64-bit"
#else
#define ARCH_TYPE "32-bit"
#endif

#ifdef _DEBUG
#define BUILD_TYPE "Debug"
#else
#define BUILD_TYPE "Release"
#endif
