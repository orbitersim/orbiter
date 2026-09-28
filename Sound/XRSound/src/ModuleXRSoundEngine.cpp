// ==============================================================
// XRSound engine class bound to an Orbiter module (i.e., to a unique ID).
// 
// Copyright (c) 2018-2026 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#include "ModuleXRSoundEngine.h"
#include "XRSoundConfigFileParser.h"
#include "XRSoundDLL.h"   // for XRSoundDLL::GetAbsoluteSimTime()

// Static method to create a new instance of an XRSoundEngine for a module.  This is the ONLY place where new 
// XRSoundEngine instances for modules are constructed.
//
// This also handles static one-time initialization of our singleton irrKlang engine.
ModuleXRSoundEngine *ModuleXRSoundEngine::CreateInstance(const char *pUniqueModuleName)
{
#ifndef __linux__
    _ASSERTE(pUniqueModuleName);
    _ASSERTE(*pUniqueModuleName);
#else // __linux__
    assert(pUniqueModuleName);
    assert(*pUniqueModuleName);
#endif // __linux__

    if (!pUniqueModuleName || !*pUniqueModuleName)
        return nullptr;

    // Must handle initializing the irrKlang engine here since clbkSimulationStart is too late: it needs to be done
    // before the first call to LoadWav.
    if (s_bIrrKlangEngineNeedsInitialization)
    {
        s_bIrrKlangEngineNeedsInitialization = false;
        InitializeIrrKlangEngine();
    }

    return new ModuleXRSoundEngine(pUniqueModuleName);
}

// Constructor
ModuleXRSoundEngine::ModuleXRSoundEngine(const char *pUniqueModuleName) :
    XRSoundEngine(),
    m_csModuleName(pUniqueModuleName)
{
#ifndef __linux__
    _ASSERTE(pUniqueModuleName);
    _ASSERTE(*pUniqueModuleName);
#else // __linux__
    assert(pUniqueModuleName);
    assert(*pUniqueModuleName);
#endif // __linux__

    // Note: there are no "overrides" applicable to modules, so there is no need to parse module configuration override .cfg files
    m_pConfig = new XRSoundConfigFileParser();  // for [SYSTEM] settings and logging
    m_pConfig->ParseModuleSoundConfig(pUniqueModuleName);
}

// Destructor
ModuleXRSoundEngine::~ModuleXRSoundEngine()
{
    // our base class cleans up m_pConfig
}

// Only invoked by our base class's static DestroyInstance method
void ModuleXRSoundEngine::FreeResources()
{
    char msg[256];
    snprintf(msg, 256, "ModuleXRSoundEngine::FreeResources: freeing XRSound engine resources for module '%s'",
        m_csModuleName.c_str());
    s_globalConfig.WriteLog(msg);

    // stop all of this module's sounds and free all irrKlang resources for them
    StopAllWav();
}

// Default sound groups are not supported for modules
bool ModuleXRSoundEngine::SetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID, const bool bEnabled)
{
    return false;
}

// Default sound groups are not supported for modules
bool ModuleXRSoundEngine::GetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID)
{
    return false;
}

// Default sound groups are not supported for modules
bool ModuleXRSoundEngine::SetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID, const char *pSubfolderPath)
{
    return false;
}

// Default sound groups are not supported for modules
const char *ModuleXRSoundEngine::GetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID) const
{
    return nullptr;
}

// Update the playback state & volume of a single sound based.  Unlike vessel-played sounds, all module sounds play as "global"
// sounds, and are not affected by any vessel's camera distance or atmosphere around it.
void ModuleXRSoundEngine::UpdateSoundState(WavContext &context)
{
    // NOTE: If you update this method, check/update the same method in VesselXRSoundEngine as well.

#ifndef __linux__
    ISound *pISound = context.pISound;  // will be nullptr if sound was never played yet, or was stopped before finishing
#else // __linux__
    AudioVoice *pISound = context.pISound;  // will be nullptr if sound was never played yet, or was stopped before finishing
#endif // __linux__
    if (pISound)    // sound was marked to play or is playing now?
    {
#ifndef __linux__
        if (!pISound->isFinished())
#else // __linux__
        if (!pISound->IsFinished())
#endif // __linux__
        {
            // update the irrKlang state for this sound
#ifndef __linux__
            pISound->setVolume(context.volume);  // Note: context.volume has already been adjusted for MasterVolume setting in config
            pISound->setIsLooped(context.bLoop);
            pISound->setIsPaused(context.bPaused);
#else // __linux__
            pISound->SetVolume(context.volume);  // Note: context.volume has already been adjusted for MasterVolume setting in config
            pISound->SetLooped(context.bLoop);
            pISound->SetPaused(context.bPaused);
#endif // __linux__
        }
        else
        {
            // sound has finished, so release its resources
#ifndef __linux__
            pISound->drop();
#else // __linux__
            pISound->Release();
#endif // __linux__
            context.pISound = nullptr;
        }
    }
}

