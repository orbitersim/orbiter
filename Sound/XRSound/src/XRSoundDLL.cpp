// ==============================================================
// XRSoundDLL.cpp : Main class file for XRSound Orbiter module.
//
// Copyright (c) 2018-2026 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#define ORBITER_MODULE
#ifndef __linux__
#include "OrbiterSDK.h"
#else // __linux__
#include "Orbitersdk.h"
#endif // __linux__
#include "XRSoundDLL.h"
#include "VesselXRSoundEngine.h"
#include "ModuleXRSoundEngine.h"
#ifdef __linux__
#include <chrono>
#endif // __linux__

namespace
{
// These SEH guards were inline in XRSoundDLL::clbkPreStep. The VS 2026 Debug
// build reports C2712 there because that function also owns a debug STL
// iterator requiring destruction. Keep the guards in these helpers without
// changing their behavior: false makes clbkPreStep write the original error
// message, then continue the update.
// TODO: Investigate the CTD that prompted the catch-all handlers and whether
// they can be removed. Continuing after an access violation may leave Orbiter
// in an invalid state; changing that policy is outside this build fix.
#ifdef __linux__
// __try/__except -> try/catch: C++ exceptions are caught; access violations have no catchable counterpart on Linux
#endif // __linux__
bool PreStepVesselWithSEH(OBJHANDLE hVessel, VesselXRSoundEngine *pEngine,
    double simt, double simdt, double mjd)
{
#ifndef __linux__
    __try
#else // __linux__
    try
#endif // __linux__
    {
#ifndef __linux__
        _ASSERTE(oapiIsVessel(hVessel));
        _ASSERTE(pEngine);
#else // __linux__
        assert(oapiIsVessel(hVessel));
        assert(pEngine);
#endif // __linux__
        pEngine->clbkPreStep(simt, simdt, mjd);
        return true;
    }
#ifndef __linux__
    __except(EXCEPTION_EXECUTE_HANDLER)
#else // __linux__
    catch (...)
#endif // __linux__
    {
        return false;
    }
}

bool UpdateIrrKlangWithSEH()
{
#ifndef __linux__
    __try
#else // __linux__
    try
#endif // __linux__
    {
        XRSoundEngine::UpdateIrrKlangEngine();
        return true;
    }
#ifndef __linux__
    __except(EXCEPTION_EXECUTE_HANDLER)
#else // __linux__
    catch (...)
#endif // __linux__
    {
        return false;
    }
}
}

// Exported DLL method; invoked only by our static XRSound.lib to obtain an XRSoundEngine object for a given vessel.
// Added in API version 1.0
DLLCLBK XRSoundEngine *GetXRSoundEngineInstance(OBJHANDLE hVessel)
{
    // Note: the XRSoundEngine vtable has not changed between XRSound version 1.x and 2.0, so we don't need a bridge object here.
    return XRSoundDLL::GetXRSoundEngineInstance(hVessel, true);
}

// Exported DLL method; invoked only by our static XRSound.lib to obtain an XRSoundEngine object for a given unique module name.
// Added in API version 2.0
DLLCLBK XRSoundEngine *GetModuleXRSoundEngineInstance(const char *pUniqueModuleName)
{
    return XRSoundDLL::GetXRSoundEngineInstance(pUniqueModuleName);
}

// This is the single entry point for acquiring an XRSoundEngine instance for a given *vessel*.
// It returns the existing XRSoundEngine instance for the supplied vessel if one already exists, otherwise it creates 
// a new XREngine instance and returns that *if* the vessel should have any default sounds *or* it was invoked via a vessel's custom code. 
// Otherwise, it returns nullptr.
//
// You must use XRSoundEngine::DestroyInstance to destroy all XRSoundEngine objects created here; they are not 
// reference-counted, and will be destroyed when the owning vessel is destroyed.
//
// Params:
//   hVessel: vessel to which this engine instance pertains
//   bInvokedByClientVessel: true if this was invoked from XRSound.lib, false otherwise
//
// Returns: new engine instance on success, or nullptr if hVessel is invalid
VesselXRSoundEngine *XRSoundDLL::GetXRSoundEngineInstance(const OBJHANDLE hVessel, const bool bInvokedByClientVessel)
{
    const bool bIsValidVessel = oapiIsVessel(hVessel);
#ifndef __linux__
    _ASSERTE(bIsValidVessel);
#else // __linux__
    assert(bIsValidVessel);
#endif // __linux__
    if (!bIsValidVessel)
    {
        // should never happen!
        char msg[256];
        snprintf(msg, 256, "XRSoundDLL::GetXRSoundEngineInstance ERROR: vessel handle passed is invalid! bInvokedByClientVessel = %d", bInvokedByClientVessel);
        WriteLog(msg);
        return nullptr;
    }

    VESSEL *pVessel = oapiGetVesselInterface(hVessel);
#ifndef __linux__
    _ASSERTE(pVessel);
#else // __linux__
    assert(pVessel);
#endif // __linux__
    
    // sanity check: bail out now if this is not a valid vessel for unknown reason
    if (!pVessel)
        return nullptr;

    VesselXRSoundEngine *pEngine = s_pInstance->FindXRSoundEngineForVessel(hVessel);
    if (!pEngine)   // no existing engine found for this vessel
    {
        // if this ship has thrusters; it should have default sounds
        const DWORD dwThrusterCount = pVessel->GetThrusterCount();
        const bool bShouldHaveDefaultSounds = (dwThrusterCount > 0);
        char csVesselDesc[256];
        const char *pName = pVessel->GetName();
        const char *pClassName = pVessel->GetClassName();
        snprintf(csVesselDesc, 256, "'%s' [class name '%s'], bInvokedByClientVessel = %d, dwThrusterCount = %u, bShouldHaveDefaultSounds = %d",
            pName ? pName : "<unknown>", pClassName ? pClassName : "<unknown>", bInvokedByClientVessel, dwThrusterCount, bShouldHaveDefaultSounds);

        // if the request for an engine came from a vessel, it should *always* succeed, even if it would not normally have default sounds
        if (bInvokedByClientVessel || bShouldHaveDefaultSounds)
        {
            char msg[256];
            snprintf(msg, 256, "XRSoundDLL::GetXRSoundEngineInstance: creating new XRSoundEngine instance for vessel %s", static_cast<const char *>(csVesselDesc));

            pEngine = VesselXRSoundEngine::CreateInstance(hVessel);
            if (pEngine)
                s_pInstance->m_allVesselsMap.insert(vesselHandle_XRSoundEnginePtr_Pair(hVessel, pEngine));  // add the new engine to our master map for vessels
            s_pInstance->WriteLog(msg);
        }
    }
    return pEngine;   // may be nullptr
}

// This is the single entry point for acquiring an XRSoundEngine instance for a given *module*.
// It returns the existing instance for the supplied module if one already exists, otherwise it creates 
// a new XREngine instance and returns that.
//
// Note that unlike the vessel-bound version of this method, there are no default sounds for modules: sounds are
// only playable manually via the XRSound C++ API.
//
// You must use XRSoundEngine::DestroyInstance to destroy all XRSoundEngine objects created here; they are not 
// destroyed automatically when Orbiter exits to the launch pad.
//
// Params:
//   pUniqueModuleName: module to which this engine instance pertains
//
// Returns: new engine instance on success, or nullptr if pUniqueModuleName is nullptr or empty, or if another module is
// already using an XRSoundEngine instance with pUniqueModuleName.
ModuleXRSoundEngine *XRSoundDLL::GetXRSoundEngineInstance(const char *pUniqueModuleName)
{
    const bool bIsValidModuleID = (pUniqueModuleName && *pUniqueModuleName);
#ifndef __linux__
    _ASSERTE(bIsValidModuleID);
#else // __linux__
    assert(bIsValidModuleID);
#endif // __linux__
    if (!bIsValidModuleID)
    {
        // should never happen!
        char msg[256];
        snprintf(msg, 256, "XRSoundDLL::GetXRSoundEngineInstance ERROR: pUniqueModuleName passed is nullptr or empty!");
        WriteLog(msg);
        return nullptr;
    }

    ModuleXRSoundEngine *pEngine = s_pInstance->FindXRSoundEngineForModule(pUniqueModuleName);
    if (!pEngine)   // no existing engine found for this module
    {
        char csMsg[256];
        snprintf(csMsg, 256, "XRSoundDLL::GetXRSoundEngineInstance: creating new XRSoundEngine instance for module '%s'", pUniqueModuleName);
        pEngine = ModuleXRSoundEngine::CreateInstance(pUniqueModuleName);
        if (pEngine)
        {
            // add the new engine to our master map for modules
            s_pInstance->m_allModulesMap.insert(CString_XRSoundEnginePtr_Pair(pUniqueModuleName, pEngine));  
        }
        else
        {
            snprintf(csMsg, 256, "XRSoundDLL::GetXRSoundEngineInstance: WARNING: nullptr or empty module name supplied; returning nullptr.");
        }
        s_pInstance->WriteLog(csMsg);
    }
    
    return pEngine;   // may be nullptr
}

//==============================================================
// This function is called when Orbiter starts or when the module
// is activated.
//==============================================================
#ifndef __linux__
DLLCLBK void InitModule(HINSTANCE hDLL)
#else // __linux__
DLLCLBK void InitModule(void *hDLL)
#endif // __linux__
{
#ifdef _DEBUG
    // Enable Visual Studio's runtime heap checking for debug builds
    // NOTE: _CRTDBG_CHECK_ALWAYS_DF is too slow; only enable that flag if chasing a difficult memory corruption bug
#ifndef __linux__
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF |
        _CRTDBG_CHECK_CRT_DF |
        _CRTDBG_LEAK_CHECK_DF);
#else // __linux__
    // _CrtSetDbgFlag left out: MSVC debug heap; the asan preset (ORBITER_SANITIZER) checks the heap on Linux
#endif // __linux__
#endif

    XRSoundDLL::s_pInstance = new XRSoundDLL(hDLL);
    oapiRegisterModule(XRSoundDLL::s_pInstance);
    
    // parse this immediately so we can read it from the start
    XRSoundDLL::ParseGlobalConfigFile();

    // NOTE: the initial implementation was to call XRSoundEngine::InitializeIrrKlangEngine from here, but
    // it hung indefinitely when calling createIrrKlangDevice, so I had to move that logic into XRSoundDLL::GetXRSoundEngineInstance.
}

// parse (or re-parse) our global XRSound.cfg file (this parse is vessel-independent)
void XRSoundDLL::ParseGlobalConfigFile()
{
    // parse XRSound.log
    XRSoundConfigFileParser &globalConfig = XRSoundDLL::GetGlobalConfig();
    globalConfig.ParseFile();
    if (globalConfig.ParseFailed())
    {
        char msg[256];
        snprintf(msg, 256, "Error parsing configuration file '%s' -- see above error messages for details.", globalConfig.GetDefaultFilename());
        globalConfig.WriteLog(msg);
    }
}

//==============================================================
// This function is called when Orbiter shuts down or when the
// module is deactivated.
//==============================================================
#ifndef __linux__
DLLCLBK void ExitModule(HINSTANCE hDLL)
#else // __linux__
DLLCLBK void ExitModule(void *hDLL)
#endif // __linux__
{
    // Note: do not delete XRSoundDLL::s_pInstance here; per the Orbiter docs, the Orbiter core automatically
    // destroys all modules when required.
}

// static data
XRSoundDLL *XRSoundDLL::s_pInstance;

// Constructor
#ifndef __linux__
XRSoundDLL::XRSoundDLL(HINSTANCE hDLL) :
#else // __linux__
XRSoundDLL::XRSoundDLL(void *hDLL) :
#endif // __linux__
    Module(hDLL), m_hDLL(hDLL), m_nextSoundEnginesRefreshSimt(0), m_absoluteSimTime(0), m_nextIrrKlangUpdateRealtime(0)
{
}

// Destructor
XRSoundDLL::~XRSoundDLL()
{
    // Note: we already freed all XRSoundEngine objects in clbkSimulationEnd()
#ifdef _DEBUG
    // sanity checks to make sure things were already cleaned up as expected
#ifndef __linux__
    _ASSERTE(m_allVesselsMap.size() == 0);
    _ASSERTE(m_allModulesMap.size() == 0);
    _ASSERTE(!XRSoundEngine::IsKlangEngineInitialized());
#else // __linux__
    assert(m_allVesselsMap.size() == 0);
    assert(m_allModulesMap.size() == 0);
    assert(!XRSoundEngine::IsKlangEngineInitialized());
#endif // __linux__
#endif
}

// This method is called immediately after a simulation session has been set up (i.e. all objects created and their
// states set according to the scenario data) and the render window has been opened (if applicable).
void XRSoundDLL::clbkSimulationStart(RenderMode mode)
{
    // reparse our global (i.e., vessel-independent) config file in case the user dropped to the launchpad and restarted
    XRSoundDLL::ParseGlobalConfigFile();

    // NOTE: this is too late to invoke XRSoundEngine::InitializeIrrKlangEngine, since vessels need the engine to be available
    // before clbkSetClassCaps.
    // Instead, the first call to XRSoundDLL::GetXRSoundEngineInstance handles initializing the engine, since that
    // is the first call that must be made before any other XRSoundEngine calls can be made.

    m_nextSoundEnginesRefreshSimt = 0;  // reset so our clbkPreStep runs immediately on startup
    m_absoluteSimTime = 0;              // reset since sim is restarting            
    XRSoundEngine::ResetStaticSimulationData();
}

// This method is called immediately before a simulation session is terminated, and before the render window is closed.
void XRSoundDLL::clbkSimulationEnd()
{
    s_pInstance->UpdateAllVesselsMap();  // in case any vessels were added or deleted since the last time we checked

    // Note: *modules* are not loaded or unloaded when the simulation starts or ends: they are only loaded or unloaded via
    // the Orbiter launch pad, either when 1) the launch pad loads or exits, or 2) when a module is manually loaded or unloaded via its checkbox
    // in the Modules tab.  Each module that uses XRSound must initialize its own sounds in clbkSimulationStart and delete its XRSoundImpl proxy object 
    // in its clbkSimulationEnd handler.  However, since we own each engine object's lifecycle, we must free up any XRSoundEngine objects for each module
    // here as well.  (See comments in XRSoundImpl::~XRSoundImpl() for details about why this is so.)
    for (auto it = m_allModulesMap.begin(); it != m_allModulesMap.end(); it++)
        XRSoundEngine::DestroyInstance(it->second);
    m_allModulesMap.clear();

    // loop through all vessels and call DestroyInstance for each vessel's XRSoundEngine to free it
    // NOTE: these should have already been destroyed via the UpdateAllVesselsMap() call above, but it won't hurt to make sure here (it->second will be nullptr for any destroyed instances).
    for (auto it = m_allVesselsMap.begin(); it != m_allVesselsMap.end(); it++)
        XRSoundEngine::DestroyInstance(it->second);
    m_allVesselsMap.clear();

    XRSoundEngine::DestroyIrrKlangEngine();
}

// Returns a list of all Orbiter vessel handles that exist during this frame
vector<OBJHANDLE> XRSoundDLL::GetAllActiveVesselsFromOrbiter()
{
    vector<OBJHANDLE> vector;

    for (unsigned int i = 0; i < oapiGetVesselCount(); i++)
        vector.push_back(oapiGetVesselByIndex(i));

    return vector;
}

// Returns the XRSoundEngine for the supplied vessel handle, or nullptr if that vessel has no sound engine associated with it
// (i,e., it has no default sounds nor custom sounds).
VesselXRSoundEngine *XRSoundDLL::FindXRSoundEngineForVessel(const OBJHANDLE hVessel)
{
    VesselXRSoundEngine *pResult = nullptr;

    if (hVessel)
    {
        auto it = m_allVesselsMap.find(hVessel);
        if (it != m_allVesselsMap.end())
            pResult = it->second;
    }

    return pResult;
}

// Returns the XRSoundEngine for the supplied module ID, or nullptr if there is no sound engine associated with that module ID.
ModuleXRSoundEngine *XRSoundDLL::FindXRSoundEngineForModule(const char *pUniqueModuleName)
{
    ModuleXRSoundEngine *pResult = nullptr;

    if (pUniqueModuleName && *pUniqueModuleName)
    {
        auto it = m_allModulesMap.find(pUniqueModuleName);
        if (it != m_allModulesMap.end())
            pResult = it->second;
    }

    return pResult;
}

// Should be called in a given timestep before m_allVesselsMap is accessed; walks through through m_allVesselsMap and:
//    1) removes any vessels that no longer exist (and terminating their sounds), and 
//    2) adds any new vessels, creating a default XRSoundEngine for each
void XRSoundDLL::UpdateAllVesselsMap()
{
    const int startingListSize = static_cast<int>(m_allVesselsMap.size());  // for debug logging later

    // 1a) build a list of vessels that no longer exist
    vector<OBJHANDLE> deletedVesselHandles;
    for (auto it = m_allVesselsMap.begin(); it != m_allVesselsMap.end(); it++)
    {
        const OBJHANDLE hVessel = it->first;
        if (!oapiIsVessel(hVessel))
            deletedVesselHandles.push_back(hVessel);   // we can't modify m_allVesselsMap while we are iterating over it, so we have to make a new list

#if 0   // DEV DEBUGGING ONLY        
        if (oapiIsVessel(hVessel))
        {
            VESSEL *pVessel = oapiGetVesselInterface(hVessel);
            if (strcmp(pVessel->GetClassName(), "XR2PayloadCHM") == 0)
            {
                sprintf(oapiDebugString(), "Simt = %lf, XR2PayloadCHM GetThrusterCount() = %d", XRSoundDLL::GetAbsoluteSimTime(), pVessel->GetThrusterCount());
            }
        }
#endif
    }

    // 1b) free XRSoundEngine instances for any deleted vessels, and remove them from our m_allVesselsMap
    for (unsigned int i = 0; i < deletedVesselHandles.size(); i++)
    {
        const OBJHANDLE hVessel = deletedVesselHandles[i];

        // Can't just use FindXRSoundEngineForVessel here because we need remove the VesselXRSoundEngine * pair in the map itself, too.
        auto it = m_allVesselsMap.find(hVessel);
#ifndef __linux__
        _ASSERTE(it != m_allVesselsMap.end());  // sanity check
#else // __linux__
        assert(it != m_allVesselsMap.end());  // sanity check
#endif // __linux__
        if (it != m_allVesselsMap.end())
        {
            VesselXRSoundEngine *pEngine = it->second;  // may be nullptr, in which case it was already previously freed or never added in the first place
            XRSoundEngine::DestroyInstance(pEngine);    // this also stops all sounds and frees all irrLKlang resources for this engine instance
            m_allVesselsMap.erase(it);
        }
    }

    // 2) add any new vessels to our m_allVesselsMap, creating a new (default) XRSoundEngine instance for each 
    vector<OBJHANDLE> allVesselHandles = GetAllActiveVesselsFromOrbiter();
    for (unsigned int i = 0; i < allVesselHandles.size(); i++)
    {
        OBJHANDLE hVessel = allVesselHandles[i];
#ifndef __linux__
        _ASSERTE(oapiIsVessel(hVessel));  // should always be a valid vessel handle
#else // __linux__
        assert(oapiIsVessel(hVessel));  // should always be a valid vessel handle
#endif // __linux__

        auto it = m_allVesselsMap.find(hVessel);
        if (it == m_allVesselsMap.end())
        {
            // this is a new vessel; create a new, default XRSoundEngine instance for it so we can play default sounds on it
#ifndef __linux__
            _ASSERTE(oapiIsVessel(hVessel));
#else // __linux__
            assert(oapiIsVessel(hVessel));
#endif // __linux__
            VesselXRSoundEngine *pEngine = GetXRSoundEngineInstance(hVessel, false);  // Note: will be nullptr if this vessel should not have any default sounds AND if it wasn't created by a client API call
            if (pEngine)
            {
                // add the new engine to our master map
                m_allVesselsMap.insert(vesselHandle_XRSoundEnginePtr_Pair(hVessel, pEngine));
            }
        }
    }

    // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "UpdateAllVesselsMap: startingListSize=%d, endingListSize=%d", startingListSize, m_allVesselsMap.size());
}


// Invoked every timestep before Orbiter state update.  This is not called if the simulation is paused, even if 
// the user moves the camera.
// Params:
//   simt simulation time after the currently processed step  [DO NOT USE -- SEE NOTE]
//   simdt length of the currently processed step
//   mjd simulation time after the currently processed step into modified Julian Date format[days]
void XRSoundDLL::clbkPreStep(double simtDoNotUse, double simdt, double mjd)
{
    // Note: currently simdt never appears to go negative, but we're being defensive here anyway
    if (simdt > 0)
        m_absoluteSimTime += simdt;

    // use our simt that is not affected by MDJ changes
    const double simt = GetAbsoluteSimTime();

    // for performance reasons, only update all the sound engines for all vessels n times per second, NOT every frame
    if (simt >= m_nextSoundEnginesRefreshSimt)
    {
        UpdateAllVesselsMap();

        // loop through each sound-enabled vessel and update the volume / playback state of each
        for (auto it = m_allVesselsMap.begin(); it != m_allVesselsMap.end(); it++)
        {
            const OBJHANDLE hVessel = it->first;
            // The vessel should still be valid: UpdateAllVesselsMap removes deleted vessels.
            if (!PreStepVesselWithSEH(hVessel, it->second, simt, simdt, mjd))
            {
                char csMsg[256];
                snprintf(csMsg, 256, "XRSoundDLL::clbkPreStep ERROR: Access Violation in VesselXRSoundEngine::clbkPreStep for vessel %s", oapiIsVessel(hVessel) ? oapiGetVesselInterface(hVessel)->GetName() : "<invalid>");
                WriteLog(csMsg);
            }
        }
        m_nextSoundEnginesRefreshSimt = simt + GetGlobalConfig().UpdateInterval;
    }

    // NOTE: we need to give irrKlang ~20 timeslices a second in *realtime*, not *sim time*, which can be slowed down to 1/10 realtime.
    // give irrKlang a timeslice to update the state of the sound output
    const double systemUptime = GetSystemUptime();
    if (systemUptime >= m_nextIrrKlangUpdateRealtime)
    {
        if (!UpdateIrrKlangWithSEH())
        {
            WriteLog("XRSoundDLL::clbkPreStep ERROR: Access Violation in XRSoundEngine::UpdateIrrKlangEngine");
        }
        m_nextIrrKlangUpdateRealtime = systemUptime + 0.05;     // 20 updates per second in realtime
    }
}

// Returns the number of seconds since the system booted (realtime); typically has 10-16 millisecond accuracy (16 ms = 1/60th second),
// which should suffice for normal realtime deltas.
// Note: it is OK for this method to be static without a mutex because Orbiter is single-threaded
// This code was cloned from Vessel3Ext.h.
double XRSoundDLL::GetSystemUptime()
{
    // Even though we lose some precision going from 2^64 max down to 2^53 (53 bits mantissia in a double), that's still enough
    // precision to track 104,249,991.37 days, or 285,616 years of uptime right down to the millisecond.  
    // See https://stackoverflow.com/questions/1848700/biggest-integer-that-can-be-stored-in-a-double
#ifndef __linux__
    const double uptimeMilli = static_cast<double>(GetTickCount64());  // GetTickCount64 requires Vista or higher, but that is our minimum target OS anyway
#else // __linux__
    // GetTickCount64 -> steady_clock (CLOCK_MONOTONIC, time since boot)
    const double uptimeMilli = static_cast<double>(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now().time_since_epoch()).count());
#endif // __linux__
    return (uptimeMilli / 1000);  // convert to seconds
}

// Invoked when the pause/resume state of the simulation has changed.
void XRSoundDLL::clbkPause(bool paused)
{
    if (paused)
    {
        // Walk through all vessels and pause all the sounds for each; all normal vessel sounds will automatically 
        // be unpaused by our UpdateAllSoundStates later when the simulation resumes.
        for (auto it = m_allVesselsMap.begin(); it != m_allVesselsMap.end(); it++)
        {
            XRSoundEngine *pEngine = it->second;   // will be nullptr if this vessel has already been freed
#ifndef __linux__
            _ASSERTE(pEngine);
#else // __linux__
            assert(pEngine);
#endif // __linux__
            pEngine->SetAllWavPaused(true);
        }
    }

    // Walk through our *module sounds* and pause or unpause each.
    for (auto it = m_allModulesMap.begin(); it != m_allModulesMap.end(); it++)
    {
        XRSoundEngine *pEngine = it->second;
#ifndef __linux__
        _ASSERTE(pEngine);
#else // __linux__
        assert(pEngine);
#endif // __linux__
        pEngine->SetAllWavPaused(paused);
    }

    // pause or resume our global music slot; we have to do this separately because no vessel "owns" it.
    XRSoundEngine::PauseOrReumseMusic(paused);
}

// static method to write to XRSound.log
void XRSoundDLL::WriteLog(const char *pMsg)
{
    GetGlobalConfig().WriteLog(pMsg);
}
