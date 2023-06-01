// ==============================================================
// XRSound engine class bound to an Orbiter module (i.e., to a unique ID).
// 
// Copyright (c) 2018-2021 Douglas Beachy
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
    _ASSERTE(pUniqueModuleName);
    _ASSERTE(*pUniqueModuleName);

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
    _ASSERTE(pUniqueModuleName);
    _ASSERTE(*pUniqueModuleName);

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
    CString msg;
    msg.Format("ModuleXRSoundEngine::FreeResources: freeing XRSound engine resources for module '%s'",
        static_cast<const char *>(m_csModuleName));
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

    ISound *pISound = context.pISound;  // will be nullptr if sound was never played yet, or was stopped before finishing
    if (pISound)    // sound was marked to play or is playing now?
    {
        if (!pISound->isFinished())
        {
            // update the irrKlang state for this sound
            pISound->setVolume(context.volume);  // Note: context.volume has already been adjusted for MasterVolume setting in config
            pISound->setIsLooped(context.bLoop);
            pISound->setIsPaused(context.bPaused);
        }
        else
        {
            // sound has finished, so release its resources
            pISound->drop();
            context.pISound = nullptr;
        }
    }
}
