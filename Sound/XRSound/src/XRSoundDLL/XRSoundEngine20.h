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
