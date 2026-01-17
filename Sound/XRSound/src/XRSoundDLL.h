// ==============================================================
// Defines the XRSound API engine implementation; this file is not distributed with XRSound.
// The code for this exists in XRSound.dll.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include "OrbiterSDK.h"
#include "XRSoundEngine.h"
#include "CStringHasher.h"

// Defines the master map of all known Orbiter vessels (handles) -> XRSoundEngine * for it.
// key = vessel handle (void *), value = XRSoundEngine * for that vessel (may be nullptr).
typedef unordered_map<OBJHANDLE, VesselXRSoundEngine *> HASHMAP_VESSELHANDLE_XRSOUNDENGINEPTR;
typedef pair<OBJHANDLE, VesselXRSoundEngine *> vesselHandle_XRSoundEnginePtr_Pair;

// Defines the master map of all known Orbiter vessels (handles) -> XRSoundEngine * for it.
// key = vessel handle (void *), value = XRSoundEngine * for that vessel (may be nullptr).
typedef unordered_map<CString, ModuleXRSoundEngine *, CStringHasher, CStringHasher> HASHMAP_CSTRING_XRSOUNDENGINEPTR;
typedef pair<CString, ModuleXRSoundEngine *> CString_XRSoundEnginePtr_Pair;


// Orbiter module that maintains state data and methods for our DLL singleton object
class XRSoundDLL : public oapi::Module
{
public:
    XRSoundDLL(HINSTANCE hDLL);
    virtual ~XRSoundDLL();

    static XRSoundDLL *s_pInstance;  // our singleton DLL object
    static VesselXRSoundEngine *GetXRSoundEngineInstance(const OBJHANDLE hVessel, const bool bInvokedByClientVessel);  // factory method for vessel-tied engines
    static ModuleXRSoundEngine *GetXRSoundEngineInstance(const char *pUniqueModuleName);  // factory method for module-tied engines

    // key = OBJHANDLE (for Orbiter vessel), value = VesselXRSoundEngine * for that vessel
    HASHMAP_VESSELHANDLE_XRSOUNDENGINEPTR m_allVesselsMap;

    // key = CString (for unique module name), value = ModuleXRSoundEngine * for that vessel
    HASHMAP_CSTRING_XRSOUNDENGINEPTR m_allModulesMap;

    // Orbiter callbacks
    virtual void clbkSimulationStart(RenderMode mode) override;
    virtual void clbkSimulationEnd() override;
    virtual void clbkPreStep(double simt, double simdt, double mjd) override;
    virtual void clbkPause(bool paused) override;

    // Returns the linear simulation time since simulation start, ignoring any MJD changes (edits).
    // This is the same principle as oapiGetSimTime except that it always returns a value >= the previous frame's value.
    static double GetAbsoluteSimTime() 
    { 
        _ASSERTE(s_pInstance);
        return s_pInstance->m_absoluteSimTime; 
    }

    // Returns the number of seconds since the system booted (realtime); typically has 10-16 millisecond accuracy (16 ms = 1/60th second),
    // which should suffice for normal realtime deltas.
    // Note: it is OK for this method to be static without a mutex because Orbiter is single-threaded
    static double GetSystemUptime();

    // worker methods
    void UpdateAllVesselsMap();
    VesselXRSoundEngine *FindXRSoundEngineForVessel(const OBJHANDLE hVessel);
    ModuleXRSoundEngine *FindXRSoundEngineForModule(const char *pUniqueModuleName);

    static vector<OBJHANDLE> GetAllActiveVesselsFromOrbiter();
    static XRSoundConfigFileParser &GetGlobalConfig() { return XRSoundEngine::s_globalConfig;  }
    static void WriteLog(const char *pMsg);
    static void ParseGlobalConfigFile();

protected:
    HINSTANCE m_hDLL;  // our DLL's handle
    double m_nextSoundEnginesRefreshSimt;
    double m_absoluteSimTime;   // replaces simt and oapiGetSimTime(), both of which are unreliable!  See note in XRSoundDLL::clbkPreStep.
};
