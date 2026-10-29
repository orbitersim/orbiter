// ==============================================================
// XRSound engine implementation.
// 
// Copyright (c) 2018-2026 Douglas Beachy and contributors
// Licensed under the MIT License
// ==============================================================

#include <sstream>
#include <cmath>

#include "XRSoundEngine.h"
#include "XRSoundConfigFileParser.h"
#include "SoundPreSteps.h"
#include "DefaultSoundGroupPreSteps.h"
#include "AnimationState.h"
#include "XRSoundDLL.h"   // for XRSoundDLL::GetAbsoluteSimTime()
#include "ISound.h"

// static data and methods

ISoundEngine *XRSoundEngine::s_SoundEngine = nullptr;
XRSoundConfigFileParser XRSoundEngine::s_globalConfig;
bool XRSoundEngine::s_bSoundEngineNeedsInitialization = true;
WavContext *XRSoundEngine::s_pMusicFolderWavContext = nullptr;  // this global, vessel-independent context will exist until the irrKlang engine is terminated
std::string XRSoundEngine::s_csVersion;

// Returns the global configuration (the configured defaults; written only by the parser).
XRSoundConfigFileParser &XRSoundEngine::GetGlobalConfig()
{
    return s_globalConfig;
}

// Parse (or re-parse) our global XRSound.cfg file (this parse is vessel-independent).
// This is the one implementation of the global parse.  It resets the [SYSTEM] fields first, so a line that was removed from the file
// since the last parse reverts to its built-in default instead of keeping the value parsed the last time.
void XRSoundEngine::ParseGlobalConfigFile()
{
    s_globalConfig.ResetSystemSettingsToDefaults();
    s_globalConfig.ParseFile();
    if (s_globalConfig.ParseFailed())
    {
        char msg[256];
        snprintf(msg, 256, "Error parsing configuration file '%s' -- see above error messages for details.", s_globalConfig.GetDefaultFilename());
        s_globalConfig.WriteLog(msg);
    }
}

static bool equalsIgnoreCase(const std::string& a, const std::string& b) {
    if (a.size() != b.size()) return false;

    for (size_t i = 0; i < a.size(); ++i) {
        if (std::tolower(static_cast<unsigned char>(a[i])) !=
            std::tolower(static_cast<unsigned char>(b[i]))) {
            return false;
        }
    }
    return true;
}

// Perform one-time initialization of our ISoundEngine singleton.
// Returns: true on success, false on error (which means no sounds will play).
bool XRSoundEngine::InitializeSoundEngine()
{
    _ASSERTE(!XRSoundEngine::IsSoundEngineInitialized());

    if (!XRSoundEngine::IsSoundEngineInitialized())
    {
        // This runs exactly once per session (at the first vessel or module engine creation), after the user has left the launchpad.  The XRSound API never
        // changes the global configuration (runtime values live in each engine's ActiveValues, which die with the engine and start again from the configuration
        // in the next session), so no runtime change can leak into a new session from here.  This re-parse is parser-side: it picks up edits to XRSound.cfg made since
        // the last session (clbkSimulationStart runs too late for the first engine creation).  ParseGlobalConfigFile resets the [SYSTEM] fields before parsing.
        ParseGlobalConfigFile();

        s_SoundEngine = new ISoundEngine();     // the sound engine's own output volume stays at its default of 1.0; the main volume of each XRSound engine is applied per sound

        char logMsg[256];   // can't use CString easily here b/c Orbiter's oapiWriteLog takes a char * instead of const char * for some bizarre reason.
        if (s_SoundEngine)
            sprintf_s(logMsg, "%s initialized using sound driver %s. XRSound UpdateInterval = %.03lf (%.1lf updates per second), MasterVolume = %.2f, MusicVolume = %.2f",
                GetVersionStr(), XRSoundEngine::GetSoundDriverName(),
                s_globalConfig.UpdateInterval, (1.0 / s_globalConfig.UpdateInterval), s_globalConfig.MasterVolume, s_globalConfig.MusicVolume);
        else
            sprintf_s(logMsg, "%s ERROR: could not initialize default sound device.", GetVersionStr());

        oapiWriteLog(logMsg);
        s_globalConfig.WriteLog("----------------------------------------------------------------------------");
        s_globalConfig.WriteLog(logMsg);

        if (!XRSoundEngine::IsSoundEngineInitialized())
            return false;
    }
    else
    {
        s_globalConfig.WriteLog("WARNING: XRSoundEngine::InitializeSoundEngine() called, but engine was already initialized.");
    }

    // Sound engine (was) initialized successfully!
    return true;
}

// Perform one-time static cleanup of our ISoundEngine singleton.
void XRSoundEngine::DestroySoundEngine()
{
    char logMsg[256];   // can't use CString easily here b/c Orbiter's oapiWriteLog takes a char * instead of const char * for some bizarre reason.
    sprintf_s(logMsg, "%s terminating.", GetVersionStr());

    oapiWriteLog(logMsg);
    s_globalConfig.WriteLog(logMsg);

    // It is not an error to call this if the engine was never initialized, so we don't check for that here.
    if (XRSoundEngine::IsSoundEngineInitialized())
    {
        // need to stop and free our static MusicFolder sound if any vessel started it
        if (s_pMusicFolderWavContext)
        {
            StopWavImpl(s_pMusicFolderWavContext, nullptr);
            delete s_pMusicFolderWavContext;
            s_pMusicFolderWavContext = nullptr;        // so we will recreate it when the Orbiter relaunches
        }

        // free and reset the engine
        delete s_SoundEngine;
        s_SoundEngine = nullptr;
        s_bSoundEngineNeedsInitialization = true;    // need to reinitialize the engine on next LoadWav call
    }
}

bool XRSoundEngine::IsSoundEngineInitialized()
{ 
    return (s_SoundEngine != nullptr); 
}

// Returns the name of the sound driver, like 'ALSA' for the alsa device.
// Possible returned strings are "nullptr", "ALSA", "CoreAudio", "winMM", "DirectSound" and "DirectSound8".
//
// Returns nullptr if Klang engine not initialized
const char *XRSoundEngine::GetSoundDriverName()
{
    const char *pDriverName = nullptr;
    if (IsSoundEngineInitialized())
        pDriverName = s_SoundEngine->getDriverName();

    return pDriverName;
}

// ------------------------------------------------------------------------

// Static method to free any resources associated with pInst and then free it.
// WARNING: is is possible that Orbiter already deleted our vessel associated with pInst, so do not reference any vessel fields 
// in this method chain.
void XRSoundEngine::DestroyInstance(XRSoundEngine *pInst)
{
    if (pInst)  // not already freed?
    {
        pInst->FreeResources();  // free resources for this engine + log it
        delete pInst;
    }
}

// Constructor
XRSoundEngine::XRSoundEngine() :
    m_nextDuplicateLogLineSimt(0), m_pConfig(nullptr)
{
    // each subclass will set up m_pConfig and then invoke InitializeActiveValues()
}

// Fills the volume levels and settings of 'a' with the configured values from 'config'.  This is the ONE place that maps a configuration field to the public value of a
// volume level or setting (including the conversions to the public MusicOrder / MusicPlay enums): InitializeActiveValues uses it for the engine's initial active values, and
// GetDefaultVolumeByType / GetDefaultSettingNumber / GetDefaultSettingString (XRSound::GetDefaultVolume / GetDefaultSetting) use it to report the configured defaults, so a
// default and its initial active value can never drift apart.  The configuration is only read here: nothing ever assigns to it outside the parser.
// Only the volume levels and the settings are touched; the other ActiveValues fields (item level, playback type override, music enabled flag) are not configuration-derived.
void XRSoundEngine::FillConfiguredValues(const XRSoundConfigFileParser &config, ActiveValues &a)
{
    // volume levels: absolute levels; Main, ATC, and Music start at their configured values and all other types at 1.0
    for (float &level : a.volumeLevels)
        level = 1.0f;
    a.volumeLevels[static_cast<int>(XRSound::VolumeType::Main)] = config.MasterVolume;    // the XRSound.cfg key is still named MasterVolume
    a.volumeLevels[static_cast<int>(XRSound::VolumeType::ATC)] = config.ATCVolume;
    a.volumeLevels[static_cast<int>(XRSound::VolumeType::Music)] = config.MusicVolume;

    // [SYSTEM] settings
    a.SilenceOfSpace = config.SilenceOfSpace;
    a.MusicOrder = static_cast<XRSound::MusicOrder>(XRSoundConfigFileParser::MusicOrderToPublic(config.MusicOrder));
    a.UpdateInterval = config.UpdateInterval;
    a.EnableVerboseLogging = config.EnableVerboseLogging;
    a.LogVesselAnimations = config.LogVesselAnimations;
    a.LogThrusterData = config.LogThrusterData;
    a.DisableAutopilotsTimeAccThreshold = config.DisableAutopilotsTimeAccThreshold;
    a.MinThrusterLevelForRCSSoundEffects = config.MinThrusterLevelForRCSSoundEffects;

    // vessel-related settings
    a.ATCMinDelay = config.ATCMinDelay;
    a.ATCMaxDelay = config.ATCMaxDelay;
    a.ATCAllowWhileLanded = config.ATCAllowWhileLanded;
    a.ATCAllowInAtmosphere = config.ATCAllowInAtmosphere;
    a.ATCAllowDuringReentry = config.ATCAllowDuringReentry;
    a.ATCDelayPlanetDistance = config.ATCDelayPlanetDistance;
    a.ATCDelayPlanetMultiplier = config.ATCDelayPlanetMultiplier;
    a.CabinAmbienceMin = config.CabinAmbienceMin;
    a.CabinAmbienceMax = config.CabinAmbienceMax;
    a.MusicPlayInternal = static_cast<XRSound::MusicPlay>(XRSoundConfigFileParser::MusicPlayToPublic(config.MusicPlayInternal));
    a.MusicPlayExternal = static_cast<XRSound::MusicPlay>(XRSoundConfigFileParser::MusicPlayToPublic(config.MusicPlayExternal));
    a.WarningGearIsUpAltitude = config.WarningGearIsUpAltitude;
    a.CustomEnginesThrusterIndexes = config.CustomEnginesThrusterIndexes;
    a.LandingGearAnimationID = config.LandingGearAnimationID;
}

// Initializes this engine's active volume levels and settings from its own configuration (m_pConfig, already parsed by the subclass constructor).
// This is the only place the active values are initialized, and the configuration is only read here: nothing ever assigns to it outside the parser.
void XRSoundEngine::InitializeActiveValues()
{
    _ASSERTE(m_pConfig);
    ActiveValues &a = m_active;

    FillConfiguredValues(*m_pConfig, a);    // the volume levels and settings (the same function supplies the XRSound::GetDefaultVolume / GetDefaultSetting values)
    a.musicItemLevel = 1.0f;
    a.bHasMusicPlaybackTypeOverride = false;
    a.bMusicEnabled = true;     // music is on for every engine until that engine calls SetDefaultSoundEnabled(MusicFolder, false)

    if (VerboseLogging())
    {
        char csMsg[256];
        snprintf(csMsg, sizeof(csMsg), "XRSoundEngine::InitializeActiveValues: initialized from config: main volume = %.3f, ATC volume = %.3f, music volume = %.3f, UpdateInterval = %.3f, SilenceOfSpace = %d, file types = [%s]",
            a.volumeLevels[static_cast<int>(XRSound::VolumeType::Main)], a.volumeLevels[static_cast<int>(XRSound::VolumeType::ATC)], a.volumeLevels[static_cast<int>(XRSound::VolumeType::Music)],
            a.UpdateInterval, a.SilenceOfSpace, m_pConfig->SupportedSoundFileTypes.c_str());
        WriteLog(csMsg);
    }
}

// The base class has no side effects to apply when a setting changes; VesselXRSoundEngine overrides this.
void XRSoundEngine::OnSettingChanged(const XRSound::Setting setting)
{
}

// Destructor
XRSoundEngine::~XRSoundEngine()
{
    delete m_pConfig;
}

// Returns the engine's version
float XRSoundEngine::GetVersion() const
{ 
    return XRSOUND_ENGINE_VERSION; 
}

// Loads the specified wav file and assigns the supplied sound ID to it.
// If you load a different wav file with the same ID as the previous wav file, the previous wav file is unloaded.
//   soundID: sound ID to be assigned to this wav file
//   pSoundFilename: path under $ORBITER_ROOT\XRSound of wav file to load
//   playbackType: denotes how sound will be faded (has no effect for sounds bound to a module; i.e., it is treated as XRSound::PlaybackType::Global)
//
// Returns true on success, false if file not found or pSoundFileName is nullptr or empty
bool XRSoundEngine::LoadWav(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType)
{
    if (!IsSoundEngineInitialized())
        return false;

    if (!pSoundFilename || !*pSoundFilename)
    {
        VERBOSE_LOG(this, "XRSoundEngine::LoadWav INFO: sound ID %d is disabled", soundID);
        return false;
    }

    // If the file is unchanged, leave the sound playing and apply the supplied playback type without reloading the file.
    bool bEnabled = true;
    WavContext slotLevelValues(soundID, "", playbackType, true);    // slot-level values to carry over into the new context (see below); defaults if no sound in this slot yet
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        // already have a sound in this slot
        if (equalsIgnoreCase(pContext->csSoundFilename, pSoundFilename))
        {
            // File is unchanged, so nothing to load; however, apply the playback type that this call supplied.  An override set via SetPlaybackType is cleared for
            // sound IDs that only a client loads, so that the client's explicit type wins; it persists for default and animation sounds (XRSound itself reloads those).
            // The global MusicFolder slot is excluded: it always plays as Global, regardless of the type its prestep passes, so that music continues across vessel switches.
            const XRSound::PlaybackType oldEffectiveType = pContext->EffectivePlaybackType();
            if (soundID != XRSound::MusicFolder)
            {
                pContext->playbackType = playbackType;
                if (!IsItemLevelSoundID(soundID))
                    pContext->bHasPlaybackTypeOverride = false;
            }
            if (pContext->EffectivePlaybackType() != oldEffectiveType)
            {
                VERBOSE_LOG(this, "XRSoundEngine::LoadWav: same file already loaded; applying new playback type %s (was %s) to %s",
                    WavContext::PlaybackTypeToStr(pContext->EffectivePlaybackType()), WavContext::PlaybackTypeToStr(oldEffectiveType), pContext->ToStr(false).c_str());
                ReapplyVolume(*pContext);   // the type affects only the volume (fades); the sound is not restarted
            }
            return true;
        }
        bEnabled = pContext->bEnabled;   // preserve the existing bEnabled flag
        slotLevelValues = *pContext;     // remember the other slot-level values, too (copied here because the pContext in the map is replaced below)

        // SPECIAL CHECK: if this is our special radio sound, we need to update that object directly and not add it to this vessel's m_allWavsMap.
        if (soundID == XRSound::MusicFolder)
        {
            StopWav(soundID);   // in case something is still playing
            // only the filename changes here; other slot-level fields in the WavContext do not.
            pContext->csSoundFilename = pSoundFilename;
            // ...but everything that belongs to the previously loaded file does: this is a different file now.
            pContext->requestedVolume = 1.0f;
            pContext->bDurationCached = false;
            pContext->cachedDurationMillis = 0;
            pContext->bHasPendingPan = pContext->bHasPendingSpeed = pContext->bHasPendingPosition = false;
            VERBOSE_LOG(this, "XRSoundEngine::LoadWav for global MusicFolder success: %s", pContext->ToStr(false).c_str());
            return true;
        }
    }

    // We are loading a new or different wav file into this slot
    WavContext context(soundID, pSoundFilename, playbackType, bEnabled);
    context.category = CategoryForSoundID(soundID);     // a client file loaded into a default sound ID keeps that ID's category

    // Slot-level values survive the file being replaced, just as bEnabled does.  Everything else about the old file (requested volume,
    // cached duration, pending pan/speed/position) is dropped: the new context starts with fresh defaults for those.
    context.itemLevel = slotLevelValues.itemLevel;
    if (IsItemLevelSoundID(soundID))
    {
        // default and animation sounds: the playback type override persists across XRSound's own reloads of the slot
        context.bHasPlaybackTypeOverride = slotLevelValues.bHasPlaybackTypeOverride;
        context.playbackTypeOverride = slotLevelValues.playbackTypeOverride;
    }
    // else: only clients load other sound IDs, so the client's explicit playback type wins and any override is cleared (left at the default of "no override")

    // Note: "LoadWav" doesn't actually load the sound file data into memory; however, it does verify that
    // the sound file actually exists.
    if (!ConfigFileParser::IsFileReadable(pSoundFilename))
    {
        VERBOSE_LOG(this, "XRSoundEngine::LoadWav WARNING: Unable to open sound file for %s", context.ToStr(false).c_str());
        return false;
    }

    // stop any existing sound is this slot and remove it from our master map if it's already defined before adding in the new one
    StopWav(soundID);
    m_allWavsMap.erase(soundID);
    m_allWavsMap.insert(soundID_WavContext_Pair(soundID, context));   // copied by value

    VERBOSE_LOG(this, "XRSoundEngine::LoadWav success: %s", context.ToStr(false).c_str());
    return true;
}

// Play (or update the bLoop and volume of) the wav file with the specified ID *if* this vessel has focus.
//   soundID: unique sound ID originally passed  to LoadWav
//   bLoop: true to loop sound continuously until StopWav called, false to play only once
//   volume: 0 (muted) to 1.0 (loudest), or exactly XRSound::UseStoredVolume to play at the volume stored in the sound's context (the stored volume is 1.0 until changed
//           by SetSoundVolume or a previous PlayWav).  Any other negative volume is clamped to 0.  The volume played (and stored) is NOT adjusted for the main volume here: the main volume,
//           item level, and category level are applied by ComputeMixVolume, and the playback type fades by ComputeFinalVolume.
//
// Returns true on success (or vessel does not have focus), false if invalid sound ID
bool XRSoundEngine::PlayWav(const int soundID, const bool bLoop, const float volume)
{
    if (!IsSoundEngineInitialized())
        return false;

    // Note: sound should *always* be marked to start playing, even if it is far away from the camera: the user may switch
    // vessels and/or camera modes at any time, and so the sound must be "in progress" as normal if and when that occurs.

    WavContext *pContext = FindWavContext(soundID);
    if (pContext == nullptr)
    {
        VERBOSE_LOG(this, "XRSoundEngine::PlayWav ERROR: no wav loaded for soundID %d (did you forget to call LoadWav for it?)", soundID);
        return false;
    }

    // If this (default) sound is *disabled*, nothing more to do
    if (!pContext->bEnabled)
        return false;

    // The shared music slot may be started, and have its volume, loop, and paused state changed, only by the engine that plays it (the focus vessel's, see PlaysMusicSlot).
    // Another engine (a non-focus vessel or a module) would otherwise apply its own levels to the slot, or leave a new track paused because UpdateSoundState refuses to touch the slot for it.
    if ((soundID == XRSound::MusicFolder) && !PlaysMusicSlot())
    {
        VERBOSE_LOG(this, "XRSoundEngine::PlayWav WARNING: this engine does not play the shared music slot (not the focus vessel); ignored for %s", pContext->ToStr(false).c_str());
        return false;
    }

    // A placeholder context (created by SetVolume, SetPlaybackType, or SetDefaultSoundEnabled for a default sound that has no file loaded, e.g., because it is
    // disabled in the configuration) has no file to play: stay silent.  Without this, each call (some presteps call PlayWav at every update) would try to
    // open an empty filename and log an error to Orbiter.log.
    if (pContext->csSoundFilename.empty())
        return false;

    // Determine the volume to play at: only the exact UseStoredVolume sentinel means "use the stored volume"; any other negative value is clamped to zero.
    // A NaN or infinite volume is invalid and is treated like a negative one (clamped to zero).  Values above 1.0 keep their permissive pre-4.1 behavior.
    float requestedVolume = volume;
    if (volume == XRSound::UseStoredVolume)
        requestedVolume = pContext->requestedVolume;
    else if (!std::isfinite(volume))
    {
        VERBOSE_LOG(this, "XRSoundEngine::PlayWav WARNING: non-finite volume (%f) for soundID %d treated as 0", volume, soundID);
        requestedVolume = 0;
    }
    else if (volume < 0)
        requestedVolume = 0;

    // we always update the volume and loop parameter even if the sound was already playing
    const bool bLogVolumeChange = (fabs(pContext->requestedVolume - requestedVolume) > 0.01f);
    const bool bLoopChanged = (bLoop != pContext->bLoop);
    pContext->bLoop = bLoop;
    pContext->requestedVolume = requestedVolume;
    pContext->bPaused = false;      // PlayWav *always* unpauses the sound

    ISound *pISound = pContext->pISound;   // will be nullptr if this sound is not currently playing
    if (pISound)
    {
        if (pISound->isFinished())
        {
            // this means the previous sound in this slot has finished, but its pISound interface was not freed yet
            s_SoundEngine->Release(pISound);
            pISound = nullptr;     // we'll obtain a new pISound below
        }
        // else sound is already playing, so we will just update its volume and loop settings later
    }

    if (pISound == nullptr)
    {
        // sound is not playing, so let's start immediately and track it via its pISound interface
        // NOTE: we start this paused so that we can set the proper volume level before starting it via UpdateSoundState
        pISound = pContext->pISound = s_SoundEngine->play2D(pContext->csSoundFilename.c_str(), bLoop, true);
        if (pISound == nullptr)   // this means the sound could not be played; e.g., corrupt file, etc.
        {
            VERBOSE_LOG(this, "XRSoundEngine::PlayWav ERROR: could not play sound %s", pContext->ToStr().c_str());
            return false;
        }
        ApplyPendingStartValues(*pContext); // apply (and clear) any pan, speed, and position that were set before this start; the sound is still paused here, so it starts at those values
        UpdateSoundState(*pContext);        // update volume immediately and unpause it w/o waiting for the next timestep
        if (oapiGetPause() && pContext->pISound)
        {
            // While Orbiter is paused, UpdateSoundState never un-pauses a sound (so that nothing resumes during a pause), but a sound that PlayWav
            // newly starts still starts (e.g., a switch click while the sim is paused), so un-pause it here; its context is not paused because PlayWav always unpauses.
            pContext->pISound->setIsPaused(false);
        }
        VERBOSE_LOG(this, "XRSoundEngine::PlayWav playing sound %s", pContext->ToStr().c_str());
    }
    else
    {
        // to prevent spamming the log each frame, don't re-log PlayWav message for a sound that's already playing unless its volume or bLoop has changed
        if (bLogVolumeChange)
            VERBOSE_LOG(this, "XRSoundEngine::PlayWav adjusting volume for playing sound %s", pContext->ToStr().c_str());

        if (bLoopChanged)
            VERBOSE_LOG(this, "XRSoundEngine::PlayWav adjusting bLoop setting for playing sound %s", pContext->ToStr().c_str());

        // Apply the new volume, loop, and un-paused state now instead of waiting for the next update because modules have no periodic update.
        // The sim-pause rule in UpdateSoundState keeps a sound paused by a sim pause paused until the sim resumes.
        UpdateSoundState(*pContext);
    }

    // Note: the sound will be have its bLoop & volumed adjusted for distance each frame, be freed, etc. by UpdateStateOfAllSounds in the next clbkPreStep.

    return true;
}

// Stop all wav sounds currently playing for this vessel.
// WARNING: is is possible that Orbiter already deleted our vessel associated with pInst, so do not reference any vessel fields 
// in this method chain.
void XRSoundEngine::StopAllWav()
{
    if (!IsSoundEngineInitialized())
        return;

    for (auto it = m_allWavsMap.begin(); it != m_allWavsMap.end(); it++)
        StopWav(it->first);
}

// Pause or resume all wav sounds currently playing for this engine.
void XRSoundEngine::SetAllWavPaused(const bool bPaused)
{
    if (!IsSoundEngineInitialized())
        return;

    for (auto it = m_allWavsMap.begin(); it != m_allWavsMap.end(); it++)
    {
        WavContext &context = it->second;
        ISound *pISound = context.pISound;
        if (pISound && !pISound->isFinished())      // a finished sound has nothing to pause or restore (and would be pointlessly "paused" otherwise)
        {
            // Don't change the state in the WavContext of the sounds here: 1) we don't need to because Orbiter will no longer call our PreStep while it
            // is paused, and 2) We want to preserve the current state of each sounds's bPaused flag anyway so that each will be unpaused or remain paused
            // as desired when Orbiter unpauses.
            // When *resuming*, restore each sound to its own bPaused state instead of un-pausing everything: a sound that the client paused (via SetPaused)
            // must stay paused after the sim resumes.
            pISound->setIsPaused(bPaused ? true : context.bPaused);
        }
    }
}

// Stop playing the wav file with the specified ID and free its audio resources (ISound interface).
//   soundID: unique sound ID originally passed to LoadWav
//
// Returns true on success, false if invalid sound ID 
bool XRSoundEngine::StopWav(const int soundID)
{
    if (!IsSoundEngineInitialized())
        return false;

    bool retVal = false;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        XRSoundEngine::StopWavImpl(pContext, this);
        retVal = true;
    }
    return retVal;
}

// Static method to stop the sound for the supplied WavContext; invoked from StopWav and UpdateSoundState.
//   pEngine: used only for VERBOSE_LOG call
// Returns: true if wav stopped, false if wav was not playing to begin with.
bool XRSoundEngine::StopWavImpl(WavContext *pContext, XRSoundEngine *pEngine)
{
    _ASSERTE(pContext);

    bool bStopped = false;
    ISound *pISound = pContext->pISound;
    if (pISound)   // was sound ever started via PlayWav?
    {
        if (!pISound->isFinished())
        {
            bStopped = true;
            if (pEngine)
                VERBOSE_LOG(pEngine, "XRSoundEngine::StopWavImpl: stopping sound %s", pContext->ToStr().c_str());
            pISound->stop();
        }
        s_SoundEngine->Release(pISound);    // free resources for this sound
        pContext->ResetPlaybackFields();  // reset all playback fields to their initial state, indicating the context is not in use
    }
    return bStopped;
}

// Returns false if the specified sound is not playing
bool XRSoundEngine::IsWavPlaying(const int soundID)
{
    if (!IsSoundEngineInitialized())
        return false;

    bool bIsPlaying = false;
    const WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        ISound *pISound = pContext->pISound;
        if (pISound)   // was sound ever started via PlayWav?
            bIsPlaying = !pISound->isFinished();
    }
    return bIsPlaying;
}

// Returns pointer to the sound filename loaded for soundID, or nullptr if no wav file loaded for that sound ID.
// NOTE: the pointer returned is only guaranteed to be valid for the remainder of this Orbiter frame, since LoadWav may replace it
// at any frame.
const char *XRSoundEngine::GetWavFilename(const int soundID)
{
    if (!IsSoundEngineInitialized())
        return nullptr;

    const char *pFilename = nullptr;
    const WavContext *pContext = FindWavContext(soundID);
    if (pContext)
        pFilename = pContext->csSoundFilename.c_str();

    return pFilename;
}

// Pause or unpause a sound.
//   soundID: unique sound ID originally passed to LoadWav.
//   bPause: true to pause sound, false to unpause it
// Returns true on success, false if invalid sound ID or XRSound.dll not present.
bool XRSoundEngine::SetPaused(const int soundID, const bool bPause)
{
    if (!IsSoundEngineInitialized())
        return false;

    bool bSuccess = false;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        // If this (default) sound is *disabled*, nothing more to do
        if (!pContext->bEnabled)
            return false;

        // only the engine that plays the shared music slot may change its paused state (see PlayWav)
        if ((soundID == XRSound::MusicFolder) && !PlaysMusicSlot())
        {
            VERBOSE_LOG(this, "XRSoundEngine::SetPaused WARNING: this engine does not play the shared music slot (not the focus vessel); ignored for %s", pContext->ToStr(false).c_str());
            return false;
        }

        bSuccess = true;    // sound ID is valid

        // Log the change only if we are changing the paused state
        if (pContext->bPaused != bPause)   // are we changing the paused state?
            VERBOSE_LOG(this, "XRSoundEngine::SetPaused: setting sound %s paused state = %d", pContext->ToStr().c_str(), bPause);

        pContext->bPaused = bPause;
        UpdateSoundState(*pContext);    // pause or unpause immediately without waiting for the next sound step
    }
    return bSuccess;
}

// Detect if a sound is paused.
//   soundID: unique sound ID originally passed to LoadWav.
// Returns true if sound is paused, or false if not paused, invalid sound ID, or XRSound.dll not present.
bool XRSoundEngine::IsPaused(const int soundID)
{
    if (!IsSoundEngineInitialized())
        return false;

    bool bIsPaused = false;
    const WavContext *pContext = FindWavContext(soundID);

    // we only need to check the WavContext here because we set the paused/unpaused state of every sound each timestep based on its WavContext
    if (pContext)
        bIsPaused = pContext->bPaused;

    return bIsPaused;
}

// ========================================================================

bool XRSoundEngine::IsDefaultSound(const int soundID) const
{
    // NOTE: verify that AirConditioning is still the FIRST group defined and LastDefaultSound is the LAST; if not, update the code here to match
    return ((soundID >= XRSound::AirConditioning) && (soundID < XRSound::LastDefaultSound));
}

bool XRSoundEngine::IsDefaultSoundGroup(const int soundID) const
{
    // NOTE: verify that RadioATCGroup is still the FIRST group defined and DockingDistanceCalloutsGroup is the LAST; if not, update the code here to match
    return ((soundID >= XRSound::RadioATCGroup) && (soundID < XRSound::LastDefaultGroup));
}

bool XRSoundEngine::IsDefaultSoundOrGroup(const int soundID) const
{
    return (IsDefaultSound(soundID) || IsDefaultSoundGroup(soundID));
}

// Writes to our XRSound.log
void XRSoundEngine::WriteLog(const char *pMsg)
{
    _ASSERTE(m_pConfig);

    // prefix the vessel ID to the log message if we can get it
    char msg[2048];
    snprintf(msg, sizeof(msg), "[%s][%.03lf] %s", GetLogID(), XRSoundDLL::GetAbsoluteSimTime(), pMsg);

    // Hopefully we won't need this check, but it's here as a sanity check in case something unexpected happens in the field.
    // reduce log verbosity by not re-logging the exact same message (ever) 
    const double simt = XRSoundDLL::GetAbsoluteSimTime();
    if ((m_lastLogLine != msg) /* || (simt >= m_nextDuplicateLogLineSimt) */)
    {
        m_lastLogLine = msg;
        // m_nextDuplicateLogLineSimt = simt + 1.0;
        m_pConfig->WriteLog(msg);
    }
}

// Returns true if verbose logging is enabled
bool XRSoundEngine::VerboseLogging() const
{
    return m_active.EnableVerboseLogging;     // this engine's own active value
}

// Returns the WavContext for the supplied soundID, or nullptr if no sound loaded with that ID.
// The returned pointers points to an object in the master soundID -> WavContext map.
WavContext *XRSoundEngine::FindWavContext(const int soundID)
{
    // SPECIAL CASE: check if this is our singleton, global MusicFolder sound, which is vessel-independent
    if (soundID == XRSound::DefaultSoundID::MusicFolder)
    {
        if (!s_pMusicFolderWavContext)
        {
            // first time requesting music folder; initialize it from our settings (these are all [SYSTEM] settings, and so match for all vessels)
            s_pMusicFolderWavContext = new WavContext(soundID, "", XRSound::PlaybackType::Global, true);
            s_pMusicFolderWavContext->category = XRSound::VolumeType::Music;    // music is the one category with a global level and no vessel volume
        }
        return s_pMusicFolderWavContext;
    }

    WavContext *pResult = nullptr;

    auto it = m_allWavsMap.find(soundID);
    if (it != m_allWavsMap.end())
        pResult = &(it->second);

    return pResult;
}

// Static method that returns variable volume based on a level (0..1).
// Note: level may be outside range of 0..1; this is not an error, but it will be limited to between 0 and 1.
float XRSoundEngine::ComputeVariableVolume(const double minVolume, const double maxVolume, double level)
{
    _ASSERTE(minVolume >= 0);
    _ASSERTE(maxVolume <= 1.0);
    _ASSERTE(minVolume <= maxVolume);

    if (level < 0)
        level = 0;
    else if (level > 1.0)
        level = 1.0;

    const float variableVolume = static_cast<float>(minVolume) + ((static_cast<float>(maxVolume) - static_cast<float>(minVolume)) * static_cast<float>(level));
    return (variableVolume >= 0.02) ? variableVolume : 0;  // don't bother playing sounds at extremely low volumes
}

// Static method that returns a variable value between an arbitrary minimum and maximum value, based on a level from 0..1.
// Note: level may be outside range of 0..1; this is not an error, but it will be limited to between 0 and 1.
double XRSoundEngine::ComputeVariableLevel(const double min, const double max, double level)
{
    if (level < 0)
        level = 0;
    else if (level > 1.0)
        level = 1.0;

    return min + ((max - min) * level);
}

// e.g., "XRSound 1.00 Beta-1 (Build Date Jan 17, 2018)"
const char *XRSoundEngine::GetVersionStr()
{
    if (s_csVersion.empty()) {
		char cbuf[256];
		snprintf(cbuf, 256, "XRSound %.2f %s[%s %s], Build Date : %s", XRSOUND_ENGINE_VERSION, XRSOUND_BETA_STR, ARCH_TYPE, BUILD_TYPE, __DATE__);
        s_csVersion = cbuf;
	}
    return s_csVersion.c_str();
}

// Returns vector of valid sound filename extensions; e.g., ".flac", ".mp3", etc.
vector<std::string> XRSoundEngine::GetValidSoundFileExtensions()
{
    _ASSERTE(m_pConfig);
    return m_pConfig->SupportedSoundFileTypesAsVector();
}

// Reset any static data for a simulation restart (e.g., one-shot timers, etc.)
void XRSoundEngine::ResetStaticSimulationData()
{
    AudioGreetingDefaultSoundPreStep::s_bPlayedGreeting = false;        // reset so the greeting replays
}

// Static method to pause or resume our global MusicFolder slot; invoked by XRSoundDLL whenever we are pausing or resuming the simulation
//  bPause: true = pause music if currently playing, false = unpause music IF it was playing before it was paused
// Returns: true if music paused or resumed, false if music is not playing or was not playing
bool XRSoundEngine::PauseOrReumseMusic(const bool bPause)
{
    bool bRetVal = false;

    // has any vessel played music yet since simulation start AND should music be currently playing?
    if (s_pMusicFolderWavContext && !s_pMusicFolderWavContext->bPaused)  
    {
        ISound *pISound = s_pMusicFolderWavContext->pISound;
        if (pISound)
        {
            bRetVal = true;

            // If *pausing*, don't change the state in the WavContext of the sound here: 1) we don't need to because Orbiter will no longer call our PreStep while it
            // is paused, and 2) We want to preserve the current state of each sounds's bPaused flag anyway so that each will be unpaused or remain paused 
            // as desired when Orbiter unpauses and this method is called again.
            char msg[256];
            if (bPause)
            {
                snprintf(msg, 256, "Pausing global music slot %s", static_cast<const char *>(s_pMusicFolderWavContext->ToStr().c_str()));
                pISound->setIsPaused(true);
            }
            else
            {
                snprintf(msg, 256, "Unpausing global music slot %s", static_cast<const char*>(s_pMusicFolderWavContext->ToStr().c_str()));
                pISound->setIsPaused(false);
            }
            XRSoundDLL::WriteLog(msg);
        }
    }

    return bRetVal;
}

//===========================================================================================
// Functionality added in XRSound version 4.1
//===========================================================================================

// Returns the volume category for the supplied sound ID.  This is the one function that owns the sound ID -> category mapping.
XRSound::VolumeType XRSoundEngine::CategoryForSoundID(const int soundID) const
{
    switch (soundID)
    {
    case XRSound::MainEngines:
    case XRSound::RetroEngines:
    case XRSound::HoverEngines:
    case XRSound::CustomEngines:
        return XRSound::VolumeType::Engines;

    case XRSound::RCSSustain:
    case XRSound::RCSAttackPlusX:
    case XRSound::RCSAttackPlusY:
    case XRSound::RCSAttackPlusZ:
    case XRSound::RCSAttackMinusX:
    case XRSound::RCSAttackMinusY:
    case XRSound::RCSAttackMinusZ:
    case XRSound::RCSClosePlusX:
    case XRSound::RCSClosePlusY:
    case XRSound::RCSClosePlusZ:
    case XRSound::RCSCloseMinusX:
    case XRSound::RCSCloseMinusY:
    case XRSound::RCSCloseMinusZ:
        return XRSound::VolumeType::RCS;

    case XRSound::AudioGreeting:
    case XRSound::Rotation:
    case XRSound::Translation:
    case XRSound::Off:
    case XRSound::AFPitch:
    case XRSound::AFOn:
    case XRSound::AFOff:
    case XRSound::Touchdown:
    case XRSound::WheelStop:
    case XRSound::OneHundredKnots:
    case XRSound::Liftoff:
    case XRSound::WarningGearIsUp:
    case XRSound::YouAreClearedToLand:
    case XRSound::DockingCallout:
    case XRSound::UndockingCallout:
    case XRSound::SubsonicCallout:
    case XRSound::MachCalloutsGroup:
    case XRSound::AltitudeCalloutsGroup:
    case XRSound::DockingDistanceCalloutsGroup:
        return XRSound::VolumeType::Callouts;

    case XRSound::LandedWind:
    case XRSound::SwitchOn:
    case XRSound::SwitchOff:
    case XRSound::Crash:
    case XRSound::MetalCrunch:
    case XRSound::WheelChirp:
    case XRSound::TiresRolling:
    case XRSound::Docking:
    case XRSound::Undocking:
    case XRSound::Wheekbrakes:
    case XRSound::DockingRadarBeep:
    case XRSound::FlightWind:
    case XRSound::ReentryPlasma:
    case XRSound::SonicBoom:
    case XRSound::AutopilotOn:
    case XRSound::AutopilotOff:
        return XRSound::VolumeType::Effects;

    case XRSound::CabinAmbienceGroup:
    case XRSound::AirConditioning:
        return XRSound::VolumeType::CabinAmbience;

    case XRSound::RadioATCGroup:
        return XRSound::VolumeType::ATC;

    case XRSound::MusicFolder:
        return XRSound::VolumeType::Music;

    default:
        break;
    }

    // not a default sound ID: animation (door) sounds are effects
    if (IsAnimationSoundID(soundID))
        return XRSound::VolumeType::Effects;

    return XRSound::VolumeType::Custom;     // every other sound ID is a client sound
}

// Modules have no animations; VesselXRSoundEngine overrides this.
bool XRSoundEngine::IsAnimationSoundID(const int soundID) const
{
    return false;
}

// Returns true if soundID is a default sound, default group, or animation sound ID
bool XRSoundEngine::IsItemLevelSoundID(const int soundID) const
{
    return (IsDefaultSoundOrGroup(soundID) || IsAnimationSoundID(soundID));
}

// Returns the "mix" volume for a sound: requested * item level * this engine's category level * this engine's main volume.
// The playback type fades (ComputeFinalVolume in VesselXRSoundEngine) are NOT included.
// Every level used here is this engine's own active level (see ActiveValues): a vessel or module only ever hears its own levels.
float XRSoundEngine::ComputeMixVolume(const WavContext &context) const
{
    const int categoryIndex = static_cast<int>(context.category);
    float categoryLevel = 1.0f;
    if ((categoryIndex > 0) && (categoryIndex < static_cast<int>(sizeof(m_active.volumeLevels) / sizeof(m_active.volumeLevels[0]))))
        categoryLevel = m_active.volumeLevels[categoryIndex];

    // The shared music slot's item level is kept per engine (a vessel may change only its own levels); every other slot keeps its item level in its own context.
    const float itemLevel = ((context.category == XRSound::VolumeType::Music) ? m_active.musicItemLevel : context.itemLevel);
    const float mainLevel = m_active.volumeLevels[static_cast<int>(XRSound::VolumeType::Main)];

    return context.requestedVolume * itemLevel * categoryLevel * mainLevel;
}

// The base class (module engines) has no playback type fades, so the final volume is the mix volume.
float XRSoundEngine::ComputeFinalVolume(const WavContext &context)
{
    return ComputeMixVolume(context);
}

// Returns the playback type in effect for the context as heard by this engine.
XRSound::PlaybackType XRSoundEngine::EffectivePlaybackTypeFor(const WavContext &context) const
{
    if ((context.category == XRSound::VolumeType::Music) && m_active.bHasMusicPlaybackTypeOverride)
        return m_active.musicPlaybackTypeOverride;

    return context.EffectivePlaybackType();
}

// Modules never play music; VesselXRSoundEngine overrides this.
bool XRSoundEngine::PlaysMusicSlot() const
{
    return false;
}

// Returns true if the context is the shared music slot (see header).
bool XRSoundEngine::IsMusicSlotContext(const WavContext &context)
{
    return (&context == s_pMusicFolderWavContext);
}

// Re-applies the volume of the shared music slot using this engine's levels, but only if this engine is the one playing the slot right now (see header).
void XRSoundEngine::ReapplyMusicVolume()
{
    if (s_pMusicFolderWavContext && PlaysMusicSlot())      // do not create the music context just to apply a volume
        ReapplyVolume(*s_pMusicFolderWavContext);
}

// Immediately re-applies the volume to the sound if it is playing or paused.  Never touches pause, loop, or position.
void XRSoundEngine::ReapplyVolume(WavContext &context)
{
    ISound *pISound = GetActiveISound(context);
    if (pISound)
        pISound->setVolume(ComputeFinalVolume(context));
}

// Re-applies the volume to every sound in this engine (the global music slot is handled separately by the callers that change it).
void XRSoundEngine::ReapplyAllVolumes()
{
    for (auto it = m_allWavsMap.begin(); it != m_allWavsMap.end(); it++)
        ReapplyVolume(it->second);
}

// Returns the context's ISound if it is playing or paused (exists and has not finished), otherwise nullptr.
ISound *XRSoundEngine::GetActiveISound(const WavContext &context)
{
    ISound *pISound = context.pISound;
    if (pISound && !pISound->isFinished())
        return pISound;

    return nullptr;
}

// Returns the context for soundID, creating an empty placeholder in our map if none exists yet.
WavContext *XRSoundEngine::FindOrCreateSlotContext(const int soundID)
{
    WavContext *pContext = FindWavContext(soundID);     // always succeeds for the global MusicFolder slot
    if (!pContext)
    {
        // no sound loaded yet: create an empty WavContext to hold the slot-level values; these will be propagated later when the slot is filled by a real LoadWav
        WavContext placeholder(soundID, "", XRSound::PlaybackType::InternalOnly, true);
        placeholder.category = CategoryForSoundID(soundID);
        m_allWavsMap.insert(soundID_WavContext_Pair(soundID, placeholder));   // copied by value
        pContext = FindWavContext(soundID);     // must get the persisted copy in our map; should always succeed now
        VERBOSE_LOG(this, "XRSoundEngine::FindOrCreateSlotContext: created placeholder context for sound ID %d (category %d)", soundID, static_cast<int>(placeholder.category));
    }
    return pContext;
}

// Applies the pending pan, speed, and position (one-shot values set while the sound was not playing) to a newly created ISound that has not been started yet.
void XRSoundEngine::ApplyPendingStartValues(WavContext &context)
{
    ISound *pISound = context.pISound;
    if (!pISound)
        return;

    if (context.bHasPendingPan)
    {
        pISound->setPan(-context.pendingPan);   // miniaudio has pan direction inverted with Orbiter's X coordinate system, so flip it (see SetPan)
        VERBOSE_LOG(this, "XRSoundEngine::ApplyPendingStartValues: applied pending pan %.2f to %s", context.pendingPan, context.ToStr(false).c_str());
        context.bHasPendingPan = false;
    }

    if (context.bHasPendingSpeed)
    {
        pISound->setPlaybackSpeed(context.pendingSpeed);
        VERBOSE_LOG(this, "XRSoundEngine::ApplyPendingStartValues: applied pending playback speed %.2f to %s", context.pendingSpeed, context.ToStr(false).c_str());
        context.bHasPendingSpeed = false;
    }

    if (context.bHasPendingPosition)
    {
        if (pISound->setPlayPosition(context.pendingPositionMillis))
        {
            VERBOSE_LOG(this, "XRSoundEngine::ApplyPendingStartValues: applied pending play position %u ms to %s", context.pendingPositionMillis, context.ToStr(false).c_str());
        }
        else
        {
            // The seek failed (e.g., position is beyond the end of the file); play from the beginning.  Always log this since it means a client's request was not honored.
            char msg[256];
            snprintf(msg, 256, "XRSoundEngine::ApplyPendingStartValues WARNING: could not seek to pending play position %u ms for %s; playing from the beginning",
                context.pendingPositionMillis, context.ToStr(false).c_str());
            WriteLog(msg);
        }
        context.bHasPendingPosition = false;
    }
}

// Returns true if type is one of the VolumeType values (Main..Custom)
bool XRSoundEngine::IsValidVolumeType(const XRSound::VolumeType type)
{
    const int value = static_cast<int>(type);
    return ((value >= static_cast<int>(XRSound::VolumeType::Main)) && (value <= static_cast<int>(XRSound::VolumeType::Custom)));
}

// Implements XRSound::SetVolume(const VolumeType, const float)
// Sets THIS engine's active level for the volume type (an absolute level, not a multiplier); no other vessel, module, or configuration value changes.
bool XRSoundEngine::SetVolumeByType(const XRSound::VolumeType type, const float level)
{
    if (!IsSoundEngineInitialized())
        return false;

    // also rejects NaN
    if (!((level >= 0.0f) && (level <= 1.0f)))
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetVolumeByType WARNING: invalid level %.3f for volume type %d; ignored", level, static_cast<int>(type));
        return false;
    }

    if (!IsValidVolumeType(type))
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetVolumeByType WARNING: unknown volume type %d; ignored (level = %.3f)", static_cast<int>(type), level);
        return false;
    }

    const float oldLevel = m_active.volumeLevels[static_cast<int>(type)];
    m_active.volumeLevels[static_cast<int>(type)] = level;

    // Re-apply to this engine's sounds now.  A change to the Music level affects only the shared music slot (not this engine's own sounds), and the main level affects both.
    if (type != XRSound::VolumeType::Music)
        ReapplyAllVolumes();
    if ((type == XRSound::VolumeType::Main) || (type == XRSound::VolumeType::Music))
        ReapplyMusicVolume();       // only does anything if this engine is the one playing the music right now

    VERBOSE_LOG(this, "XRSoundEngine::SetVolumeByType: volume type %d changed from %.3f to %.3f", static_cast<int>(type), oldLevel, level);
    return true;
}

// Implements XRSound::GetVolume(const VolumeType, float &): this engine's active level
bool XRSoundEngine::GetVolumeByType(const XRSound::VolumeType type, float &level)
{
    if (!IsSoundEngineInitialized() || !IsValidVolumeType(type))
        return false;

    level = m_active.volumeLevels[static_cast<int>(type)];
    return true;
}

// Implements XRSound::SetVolume(const int soundID, const float)
bool XRSoundEngine::SetSoundVolume(const int soundID, const float volume)
{
    if (!IsSoundEngineInitialized())
        return false;

    // also rejects NaN
    if (!((volume >= 0.0f) && (volume <= 1.0f)))
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetSoundVolume WARNING: invalid volume %.3f for sound ID %d; ignored", volume, soundID);
        return false;
    }

    if (soundID == XRSound::MusicFolder)
    {
        // The music slot is shared by all vessels, so the item level is this engine's own (it is applied when this engine's vessel has focus and plays the music).
        m_active.musicItemLevel = volume;
        ReapplyMusicVolume();
        VERBOSE_LOG(this, "XRSoundEngine::SetSoundVolume: set this engine's MusicFolder item level to %.3f", volume);
        return true;
    }

    if (IsItemLevelSoundID(soundID))
    {
        // Default sounds, groups, and animation sounds: XRSound sets their playback volume itself, so this sets the slot's item level (a multiplier
        // on XRSound's automatic volume). It may be set before the sound has been loaded.
        WavContext *pContext = FindOrCreateSlotContext(soundID);
        _ASSERTE(pContext);
        pContext->itemLevel = volume;
        ReapplyVolume(*pContext);
        VERBOSE_LOG(this, "XRSoundEngine::SetSoundVolume: set item level to %.3f for %s", volume, pContext->ToStr(false).c_str());
        return true;
    }

    // client sound
    WavContext *pContext = FindWavContext(soundID);
    if (!pContext)
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetSoundVolume WARNING: no wav loaded for soundID %d; ignored (volume = %.3f)", soundID, volume);
        return false;
    }

    pContext->requestedVolume = volume;
    ReapplyVolume(*pContext);   // takes effect at once if the sound is playing or paused
    VERBOSE_LOG(this, "XRSoundEngine::SetSoundVolume: set stored volume to %.3f for %s", volume, pContext->ToStr(false).c_str());
    return true;
}

// Implements XRSound::GetVolume(const int soundID, float &)
bool XRSoundEngine::GetSoundVolume(const int soundID, float &volume)
{
    if (!IsSoundEngineInitialized())
        return false;

    if (soundID == XRSound::MusicFolder)
    {
        volume = m_active.musicItemLevel;       // this engine's own item level for the shared music slot
        return true;
    }

    const WavContext *pContext = FindWavContext(soundID);
    if (IsItemLevelSoundID(soundID))
    {
        volume = (pContext ? pContext->itemLevel : 1.0f);   // the item level defaults to 1.0 even before the sound is loaded
        return true;
    }

    if (!pContext)
        return false;

    volume = pContext->requestedVolume;
    return true;
}

// Implements XRSound::SetPlaybackType
bool XRSoundEngine::SetSoundPlaybackType(const int soundID, const XRSound::PlaybackType playbackType)
{
    if (!IsSoundEngineInitialized())
        return false;

    const int typeValue = static_cast<int>(playbackType);
    if ((typeValue < static_cast<int>(XRSound::PlaybackType::InternalOnly)) || (typeValue > static_cast<int>(XRSound::PlaybackType::Global)))
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetSoundPlaybackType WARNING: invalid playback type %d for sound ID %d; ignored", typeValue, soundID);
        return false;
    }

    if (soundID == XRSound::MusicFolder)
    {
        // the music slot is shared by all vessels, so the override is this engine's own (see ActiveValues)
        m_active.bHasMusicPlaybackTypeOverride = true;
        m_active.musicPlaybackTypeOverride = playbackType;
        ReapplyMusicVolume();
        VERBOSE_LOG(this, "XRSoundEngine::SetSoundPlaybackType: set this engine's MusicFolder playback type override to %s", WavContext::PlaybackTypeToStr(playbackType));
        return true;
    }

    WavContext *pContext = nullptr;
    if (IsItemLevelSoundID(soundID))
        pContext = FindOrCreateSlotContext(soundID);    // may be set before XRSound has loaded the sound
    else
        pContext = FindWavContext(soundID);

    if (!pContext)
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetSoundPlaybackType WARNING: no wav loaded for soundID %d; ignored (playback type = %d)", soundID, typeValue);
        return false;
    }

    pContext->bHasPlaybackTypeOverride = true;
    pContext->playbackTypeOverride = playbackType;
    ReapplyVolume(*pContext);   // the type affects only the volume (fades); the sound is not reloaded or restarted
    VERBOSE_LOG(this, "XRSoundEngine::SetSoundPlaybackType: set playback type override to %s for %s", WavContext::PlaybackTypeToStr(playbackType), pContext->ToStr(false).c_str());
    return true;
}

// Implements XRSound::GetPlaybackType
bool XRSoundEngine::GetSoundPlaybackType(const int soundID, XRSound::PlaybackType &playbackType)
{
    if (!IsSoundEngineInitialized())
        return false;

    if ((soundID == XRSound::MusicFolder) && m_active.bHasMusicPlaybackTypeOverride)
    {
        playbackType = m_active.musicPlaybackTypeOverride;
        return true;
    }

    const WavContext *pContext = FindWavContext(soundID);
    if (!pContext)
        return false;

    // a placeholder context (no file loaded yet) has no playback type unless SetPlaybackType set an override (see XRSound.h)
    if (pContext->csSoundFilename.empty() && !pContext->bHasPlaybackTypeOverride)
        return false;

    playbackType = pContext->EffectivePlaybackType();
    return true;
}

// Implements XRSound::GetDuration: returns the length of the file loaded for soundID in milliseconds without playing it.
// The value is measured on the first call (which can take a moment for a long compressed file) and then cached in the WavContext until a different file is loaded.
bool XRSoundEngine::GetSoundDuration(const int soundID, unsigned int &durationMillis)
{
    if (!IsSoundEngineInitialized())
        return false;

    WavContext *pContext = FindWavContext(soundID);
    if (!pContext || pContext->csSoundFilename.empty())
        return false;

    if (!pContext->bDurationCached)
    {
        const ULONGLONG startTick = GetTickCount64();
        unsigned int lengthMillis = 0;
        if (!s_SoundEngine->getSoundLengthMillis(pContext->csSoundFilename.c_str(), lengthMillis) || (lengthMillis == 0))
        {
            char msg[256];
            snprintf(msg, 256, "XRSoundEngine::GetSoundDuration WARNING: could not determine the length of %s (lengthMillis = %u)", pContext->ToStr(false).c_str(), lengthMillis);
            WriteLog(msg);
            return false;
        }

        pContext->cachedDurationMillis = lengthMillis;
        pContext->bDurationCached = true;
        VERBOSE_LOG(this, "XRSoundEngine::GetSoundDuration: measured %u ms in %llu ms for %s", lengthMillis, static_cast<unsigned long long>(GetTickCount64() - startTick), pContext->ToStr(false).c_str());
    }

    durationMillis = pContext->cachedDurationMillis;
    return true;
}

