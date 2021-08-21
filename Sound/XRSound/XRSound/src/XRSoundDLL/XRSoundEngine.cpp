// ==============================================================
// XRSound engine implementation.
// 
// Copyright (c) 2017-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#include "XRSoundEngine.h"
#include "XRSoundConfigFileParser.h"
#include "SoundPreSteps.h"
#include "DefaultSoundGroupPreSteps.h"
#include "AnimationState.h"
#include "XRSoundDLL.h"   // for XRSoundDLL::GetAbsoluteSimTime()

// static data and methods

ISoundEngine *XRSoundEngine::s_pKlangEngine = nullptr;
XRSoundConfigFileParser XRSoundEngine::s_globalConfig;
bool XRSoundEngine::s_bIrrKlangEngineNeedsInitialization = true;
WavContext *XRSoundEngine::s_pMusicFolderWavContext = nullptr;  // this global, vessel-independent context will exist until the irrKlang engine is terminated
CString XRSoundEngine::s_csVersion;

// Perform one-time initialization of our irrKlang singleton.
// Returns: true on success, false on error (which means no sounds will play).
bool XRSoundEngine::InitializeIrrKlangEngine()
{
    _ASSERTE(!XRSoundEngine::IsKlangEngineInitialized());

    if (!XRSoundEngine::IsKlangEngineInitialized())
    {
        // Note: we do NOT want to use multi-threading here: that opens up possible timing gaps / race conditions between the time 
        // we query a given sound's state in our thread and when the OTHER thread updates that state.
        // TODO: if and when we want to support 3D sounds, will need to add ESEO_USE_3D_BUFFERS flag below as well
        s_pKlangEngine = createIrrKlangDevice(
            ESOD_AUTO_DETECT,
            ESEO_LOAD_PLUGINS | ESEO_PRINT_DEBUG_INFO_TO_DEBUGGER
        );

        char logMsg[256];   // can't use CString easily here b/c Orbiter's oapiWriteLog takes a char * instead of const char * for some bizarre reason.
        if (s_pKlangEngine)
            sprintf_s(logMsg, "%s initialized using sound driver %s; irrKlang version = %s.  XRSound UpdateInterval = %.03lf (%.1lf updates per second)", 
                GetVersionStr(), XRSoundEngine::GetSoundDriverName(), IRR_KLANG_VERSION, 
                s_globalConfig.UpdateInterval, (1.0 / s_globalConfig.UpdateInterval));
        else
            sprintf_s(logMsg, "%s ERROR: could not initialize default sound device.", GetVersionStr());

        oapiWriteLog(logMsg);
        s_globalConfig.WriteLog("----------------------------------------------------------------------------");
        s_globalConfig.WriteLog(logMsg);

        if (!XRSoundEngine::IsKlangEngineInitialized())
            return false;
    }
    else
    {
        s_globalConfig.WriteLog("WARNING: XRSoundEngine::InitializeIrrKlangEngine() called, but engine was already initialized.");
    }

    // irrKlang (was) initialized successfully!
    return true;
}

// Perform one-time static cleanup of our irrKlang singleton.
void XRSoundEngine::DestroyIrrKlangEngine()
{
    char logMsg[256];   // can't use CString easily here b/c Orbiter's oapiWriteLog takes a char * instead of const char * for some bizarre reason.
    sprintf_s(logMsg, "%s terminating.", GetVersionStr());

    oapiWriteLog(logMsg);
    s_globalConfig.WriteLog(logMsg);

    // It is not an error to call this if the engine was never initialized, so we don't check for that here.
    if (XRSoundEngine::IsKlangEngineInitialized())
    {
        // need to stop and free our static MusicFolder sound if any vessel started it
        if (s_pMusicFolderWavContext)
        {
            StopWavImpl(s_pMusicFolderWavContext, nullptr);
            s_pMusicFolderWavContext = nullptr;        // so we will recreate it when the Orbiter relaunches
        }

        // free and reset the engine
        s_pKlangEngine->drop();
        s_pKlangEngine = nullptr;
        s_bIrrKlangEngineNeedsInitialization = true;    // need to reinitialize the engine on next LoadWav call
    }
}

bool XRSoundEngine::IsKlangEngineInitialized()
{ 
    return (s_pKlangEngine != nullptr); 
}

// Returns the name of the sound driver, like 'ALSA' for the alsa device.
// Possible returned strings are "nullptr", "ALSA", "CoreAudio", "winMM", "DirectSound" and "DirectSound8".
//
// Returns nullptr if Klang engine not initialized
const char *XRSoundEngine::GetSoundDriverName()
{
    const char *pDriverName = nullptr;
    if (IsKlangEngineInitialized())
        pDriverName = s_pKlangEngine->getDriverName();

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
    // each subclass will set up m_pConfig
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
    if (!IsKlangEngineInitialized())
        return false;

    if (!pSoundFilename || !*pSoundFilename)
    {
        VERBOSE_LOG(this, "XRSoundEngine::LoadWav INFO: sound ID %d is disabled", soundID);
        return false;
    }

    // if the wav file has not changed from what is currently in the slot, do not stop it and load it: it's a no-op instead.
    bool bEnabled = true;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        // already have a sound in this slot
        if (pContext->csSoundFilename.CompareNoCase(pSoundFilename) == 0)
            return true;    // file is unchanged, so nothing to load
        bEnabled = pContext->bEnabled;   // preserve the existing bEnabled flag

        // SPECIAL CHECK: if this is our special radio sound, we need to update that object directly and not add it to this vessel's m_allWavsMap.
        if (soundID == XRSound::MusicFolder)
        {
            StopWav(soundID);   // in case something is still playing
            // only the filename changes here; other fields in the WavContext do not.
            pContext->csSoundFilename = pSoundFilename;
            VERBOSE_LOG(this, "XRSoundEngine::LoadWav for global MusicFolder success: %s", static_cast<const char *>(pContext->ToStr(false)));
            return true;
        }
    }

    // We are loading a new or different wav file into this slot
    WavContext context(soundID, pSoundFilename, playbackType, bEnabled);

    // Note: "LoadWav" doesn't actually load the sound file data into memory; however, it does verify that
    // the sound file actually exists.
    if (!ConfigFileParser::IsFileReadable(pSoundFilename))
    {
        VERBOSE_LOG(this, "XRSoundEngine::LoadWav WARNING: Unable to open sound file for %s", static_cast<const char*>(context.ToStr(false)));
        return false;
    }
       
    // stop any existing sound is this slot and remove it from our master map if it's already defined before adding in the new one
    StopWav(soundID);
    m_allWavsMap.erase(soundID);
    m_allWavsMap.insert(soundID_WavContext_Pair(soundID, context));   // copied by value

    VERBOSE_LOG(this, "XRSoundEngine::LoadWav success: %s", static_cast<const char*>(context.ToStr(false)));
    return true;
}

// Play (or update the bLoop and volume of) the wav file with the specified ID *if* this vessel has focus.
//   soundID: unique sound ID originally passed  to LoadWav
//   bLoop: true to loop sound continuously until StopWav called, false to play only once
//   volume: 0 (muted) to 1.0 (loudest)
//
// Returns true on success (or vessel does not have focus), false if invalid sound ID 
bool XRSoundEngine::PlayWav(const int soundID, const bool bLoop, const float volume)
{
    if (!IsKlangEngineInitialized())
        return false;

    // Adjust the volume per the MasterVolume: all sounds to be played come through here, so we only need to adjust it once in this place.
    const float adjustedVolume = volume * m_pConfig->MasterVolume;

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

    // we always update the volume and loop parameter even if the sound was already playing
    const bool bVolumeChanged = (adjustedVolume != pContext->volume);
    const bool bLogVolumeChange = (fabs(pContext->volume - adjustedVolume) > 0.01f);
    const bool bLoopChanged = (bLoop != pContext->bLoop);
    pContext->bLoop = bLoop;
    pContext->volume = adjustedVolume;
    pContext->bPaused = false;      // PlayWav *always* unpauses the sound

    ISound *pISound = pContext->pISound;   // will be nullptr if this sound is not currently playing
    if (pISound)
    {
        if (pISound->isFinished())
        {
            // this means the previous sound in this slot has finished, but its pISound interface was not freed yet
            pISound->drop();
            pISound = nullptr;     // we'll obtain a new pISound below
        }
        // else sound is already playing, so we will just update its volume and loop settings later
    }

    if (pISound == nullptr)
    {
        // sound is not playing, so let's start immediately and track it via its pISound interface
        // NOTE: we start this paused so that we can set the proper volume level before starting it via UpdateSoundState
        pISound = pContext->pISound = s_pKlangEngine->play2D(pContext->csSoundFilename, bLoop, true, true);
        if (pISound == nullptr)   // this means the sound could not be played; e.g., corrupt file, etc.
        {
            VERBOSE_LOG(this, "XRSoundEngine::PlayWav ERROR: could not play sound %s", static_cast<const char*>(pContext->ToStr()));
            return false;
        }
        UpdateSoundState(*pContext);        // update volume immediately and unpause it w/o waiting for the next timestep
        VERBOSE_LOG(this, "XRSoundEngine::PlayWav playing sound %s", static_cast<const char*>(pContext->ToStr()));
    }
    else
    {
        // to prevent spamming the log each frame, don't re-log PlayWav message for a sound that's already playing unless its volume or bLoop has changed
        if (bLogVolumeChange)
            VERBOSE_LOG(this, "XRSoundEngine::PlayWav adjusting volume for playing sound %s", static_cast<const char*>(pContext->ToStr()));

        if (bLoopChanged)
            VERBOSE_LOG(this, "XRSoundEngine::PlayWav adjusting bLoop setting for playing sound %s", static_cast<const char*>(pContext->ToStr()));
    }

    // Note: the sound will be have its bLoop & volumed adjusted for distance each frame, be freed, etc. by UpdateStateOfAllSounds in the next clbkPreStep.

    return true;
}

// Stop all wav sounds currently playing for this vessel.
// WARNING: is is possible that Orbiter already deleted our vessel associated with pInst, so do not reference any vessel fields 
// in this method chain.
void XRSoundEngine::StopAllWav()
{
    if (!IsKlangEngineInitialized())
        return;

    for (auto it = m_allWavsMap.begin(); it != m_allWavsMap.end(); it++)
        StopWav(it->first);
}

// Pause or resume all wav sounds currently playing for this engine.
void XRSoundEngine::SetAllWavPaused(const bool bPaused)
{
    if (!IsKlangEngineInitialized())
        return;

    for (auto it = m_allWavsMap.begin(); it != m_allWavsMap.end(); it++)
    {
        WavContext &context = it->second;
        ISound *pISound = context.pISound;
        if (pISound)
        {
            // Don't change the state in the WavContext of the sounds here: 1) we don't need to because Orbiter will no longer call our PreStep while it
            // is paused, and 2) We want to preserve the current state of each sounds's bPaused flag anyway so that each will be unpaused or remain paused 
            // as desired when Orbiter unpauses.
            pISound->setIsPaused(bPaused);
        }
    }
}

// Stop playing the wav file with the specified ID and free its irrKlang resources (ISound interface).
//   soundID: unique sound ID originally passed to LoadWav
//
// Returns true on success, false if invalid sound ID 
bool XRSoundEngine::StopWav(const int soundID)
{
    if (!IsKlangEngineInitialized())
        return false;

    bool retVal = false;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        StopWavImpl(pContext, this);
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
    if (pISound)   // was sound ever started via StartWav?
    {
        if (!pISound->isFinished())
        {
            bStopped = true;
            if (pEngine)
                VERBOSE_LOG(pEngine, "XRSoundEngine::StopWavImpl: stopping sound %s", static_cast<const char*>(pContext->ToStr()));
            pISound->stop();
        }
        pISound->drop();    // free irrKlang resources for this sound
        pContext->ResetPlaybackFields();  // reset all playback fields to their initial state, indicating the context is not in use
    }
    return bStopped;
}

// Returns false if the specified sound is not playing
bool XRSoundEngine::IsWavPlaying(const int soundID)
{
    if (!IsKlangEngineInitialized())
        return false;

    bool bIsPlaying = false;
    const WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        ISound *pISound = pContext->pISound;
        if (pISound)   // was sound ever started via StartWav?
            bIsPlaying = !pISound->isFinished();
    }
    return bIsPlaying;
}

// Returns pointer to the sound filename loaded for soundID, or nullptr if no wav file loaded for that sound ID.
// NOTE: the pointer returned is only guaranteed to be valid for the remainder of this Orbiter frame, since LoadWav may replace it
// at any frame.
const char *XRSoundEngine::GetWavFilename(const int soundID)
{
    if (!IsKlangEngineInitialized())
        return nullptr;

    const char *pFilename = nullptr;
    const WavContext *pContext = FindWavContext(soundID);
    if (pContext)
        pFilename = pContext->csSoundFilename;

    return pFilename;
}

// Pause or unpause a sound.
//   soundID: unique sound ID originally passed to LoadWav.
//   bPause: true to pause sound, false to unpause it
// Returns true on success, false if invalid sound ID or XRSound.dll not present.
bool XRSoundEngine::SetPaused(const int soundID, const bool bPause)
{
    if (!IsKlangEngineInitialized())
        return false;

    bool bSuccess = false;
    WavContext *pContext = FindWavContext(soundID);
    if (pContext)
    {
        // If this (default) sound is *disabled*, nothing more to do
        if (!pContext->bEnabled)
            return false;

        bSuccess = true;    // sound ID is valid

        // Log the change only if we are changing the paused state
        if (pContext->bPaused != bPause)   // are we changing the paused state?
            VERBOSE_LOG(this, "XRSoundEngine::SetPaused: setting sound %s paused state = %d", static_cast<const char*>(pContext->ToStr()), bPause);

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
    if (!IsKlangEngineInitialized())
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
    CString msg;
    msg.Format("[%s][%.03lf] %s", GetLogID(), XRSoundDLL::GetAbsoluteSimTime(), pMsg);

    // Hopefully we won't need this check, but it's here as a sanity check in case something unexpected happens in the field.
    // reduce log verbosity by not re-logging the exact same message (ever) 
    const double simt = XRSoundDLL::GetAbsoluteSimTime();
    if ((msg != m_lastLogLine) /* || (simt >= m_nextDuplicateLogLineSimt) */)
    {
        m_lastLogLine = msg;
        // m_nextDuplicateLogLineSimt = simt + 1.0;
        m_pConfig->WriteLog(msg);
    }
}

// Returns true if verbose logging is enabled
bool XRSoundEngine::VerboseLogging() const 
{ 
    return m_pConfig->EnableVerboseLogging;
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
    if (s_csVersion.IsEmpty())
        s_csVersion.Format("XRSound %.2f %s[%s %s], Build Date : %s", XRSOUND_ENGINE_VERSION, XRSOUND_BETA_STR, ARCH_TYPE, BUILD_TYPE, __DATE__);

    return static_cast<const char *>(s_csVersion);
}

// Returns vector of valid sound filename extensions; e.g., ".flac", ".mp3", etc.
vector<CString> XRSoundEngine::GetValidSoundFileExtensions()
{
    _ASSERTE(m_pConfig);
    return m_pConfig->SupportedSoundFileTypesAsVector();
}

// Give irrKlang a timeslice to update the state of the sound output.
// Must be invoked several times per second, but a minimum of 3.
void XRSoundEngine::UpdateIrrKlangEngine()
{
    if (!XRSoundEngine::IsKlangEngineInitialized())
        return;     // edge case: there are no sound-enabled vessels in Orbiter yet, so nothing to do

    s_pKlangEngine->update();
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
            CString msg;
            if (bPause)
            {
                msg.Format("Pausing global music slot %s", static_cast<const char *>(s_pMusicFolderWavContext->ToStr()));
                pISound->setIsPaused(true);
            }
            else
            {
                msg.Format("Unpausing global music slot %s", static_cast<const char*>(s_pMusicFolderWavContext->ToStr()));
                pISound->setIsPaused(false);
            }
            XRSoundDLL::WriteLog(msg);
        }
    }

    return bRetVal;
}
