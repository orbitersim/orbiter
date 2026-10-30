// ==============================================================
// XRSound engine class bound to an Orbiter module (i.e., to a unique ID).
// 
// Copyright (c) 2018-2026 Douglas Beachy and contributors
// Licensed under the MIT License
// ==============================================================

#include "ModuleXRSoundEngine.h"
#include "XRSoundConfigFileParser.h"
#include "XRSoundDLL.h"   // for XRSoundDLL::GetAbsoluteSimTime()
#include "ISound.h"

// Static method to create a new instance of an XRSoundEngine for a module.  This is the ONLY place where new 
// XRSoundEngine instances for modules are constructed.
//
// This also handles static one-time initialization of our singleton sound engine.
ModuleXRSoundEngine *ModuleXRSoundEngine::CreateInstance(const char *pUniqueModuleName)
{
    _ASSERTE(pUniqueModuleName);
    _ASSERTE(*pUniqueModuleName);

    if (!pUniqueModuleName || !*pUniqueModuleName)
        return nullptr;

    // Must handle initializing the sound engine here since clbkSimulationStart is too late: it needs to be done
    // before the first call to LoadWav.
    if (s_bSoundEngineNeedsInitialization)
    {
        s_bSoundEngineNeedsInitialization = false;
        InitializeSoundEngine();
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

    // This module's active volume levels and settings start from its own configuration (just parsed above); from here on the XRSound API changes only the active values.
    // A module accepts and stores every setting (the vessel-related ones have no effect on a module).
    InitializeActiveValues();
}

// Destructor
ModuleXRSoundEngine::~ModuleXRSoundEngine()
{
    // our base class cleans up m_pConfig
}

// Only invoked by our base class's static DestroyInstance method
void ModuleXRSoundEngine::FreeResources()
{
    s_globalConfig.WriteLog(FormatString("ModuleXRSoundEngine::FreeResources: freeing XRSound engine resources for module '%s'",
        m_csModuleName.c_str()).c_str());

    // stop all of this module's sounds and free all sound resources for them
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

    // Modules never play the shared music slot (PlaysMusicSlot is always false for them), so a module must never apply its levels or paused state to it.
    if (IsMusicSlotContext(context) && !PlaysMusicSlot())
        return;

    ISound *pISound = context.pISound;  // will be nullptr if sound was never played yet, or was stopped before finishing
    if (pISound)    // sound was marked to play or is playing now?
    {
        if (!pISound->isFinished())
        {
            // update the state for this sound
            // Note: ComputeFinalVolume applies the requested / item / category levels and this module's main volume.
            pISound->setVolume(ComputeFinalVolume(context));
            pISound->setIsLooped(context.bLoop);

            // While the simulation is paused, never un-pause a sound that already exists: clbkPause pauses each ISound directly and leaves context.bPaused alone, so
            // setting it from the context here would resume the sound during the pause (e.g., from SetPaused or PlayWav).  Each sound is restored to
            // its own context.bPaused state when the sim resumes (see XRSoundEngine::SetAllWavPaused).  Pausing a sound is always allowed.
            if (context.bPaused || !oapiGetPause())
                pISound->setIsPaused(context.bPaused);
        }
        else
        {
            // sound has finished, so release its resources
			s_SoundEngine->Release(pISound);
            context.pISound = nullptr;
        }
    }
}

