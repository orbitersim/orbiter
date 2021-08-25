// ==============================================================
// Implements default sounds handlers for XRSound.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#include "SoundPreSteps.h"
#include "XRSoundDLL.h"     // for GetSystemUptime

// Plays a switch sound: On (high click), or Off (low click)
bool SoundPreStep::PlaySwitch(const bool bOn, const float volume)
{
    bool bRetVal;

    // these switch sounds are global sounds loaded for each engine instance (see XRSoundEngine::LoadGlobalSounds()).
    if (bOn)
        bRetVal = PlayWav(XRSound::SwitchOn, false, volume);
    else
        bRetVal = PlayWav(XRSound::SwitchOff, false, volume);

    return bRetVal;
}

// Load wav file
bool SoundPreStep::LoadWav(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType)
{
    return m_pEngine->LoadWav(soundID, pSoundFilename, playbackType);
}

// Play a wav that was already loaded previously (e.g., in Initialize)
bool SoundPreStep::PlayWav(const int soundID, const bool bLoop, const float volume)
{
    return m_pEngine->PlayWav(soundID, bLoop, volume);
}

// Stop a wav (convenience method)
bool SoundPreStep::StopWav(const int soundID)
{
    return m_pEngine->StopWav(soundID);
}

// Load and plays a wav file in the specified sound slot.
//   soundID: must be >= 0 (meaning, initialized)
//   pWaveFile: may be nullptr or empty (meaning, no sound set).  In that case, returns true since this is not an error.
//   bLoop: true to loop, false to not loop
//   playbackType: how to play or fade the sound
//   volume 0..1.0
// Returns true on success, or false if load or play failed
bool SoundPreStep::LoadAndPlayWavUsingID(const int soundID, const char *pWavFile, const bool bLoop, const XRSound::PlaybackType playbackType, const float volume)
{
    bool bSuccess = true;

    if ((soundID >= 0) && pWavFile && *pWavFile)
    {
        bSuccess = m_pEngine->LoadWav(soundID, pWavFile, playbackType);
        if (bSuccess)
            bSuccess = m_pEngine->PlayWav(soundID, bLoop, volume);
    }
    return bSuccess;
}

// ------------------------------------------------------------------------

// Constructor
DefaultSoundPreStep::DefaultSoundPreStep(VesselXRSoundEngine *pEngine) : 
    SoundPreStep(pEngine),
    m_soundID(-1), m_playbackType(XRSound::PlaybackType::InternalOnly), m_bWavPresent(false)
{
    _ASSERTE(pEngine);
}

// Destructor
DefaultSoundPreStep::~DefaultSoundPreStep()
{
    if (IsInitialized())
        StopWav();
}

// Default implementation; loads a single wav file from pSoundFilename
//   soundID: unique sound ID
//   pSoundfilename: may be nullptr or empty
//   playbackType: type of sound (fade, etc.)
// 
// Returns true if initialization successful, false if pSoundFilename is present bu the sound file load failed: the sound will not play
bool DefaultSoundPreStep::Initialize(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType)
{
    m_soundID = soundID;
    m_playbackType = playbackType;

    if (pSoundFilename && *pSoundFilename)
    {
        if (LoadWav(pSoundFilename))
            m_bWavPresent = true;       // we have a valid wav file loaded
    }

    return IsInitialized();
}

// Load a new sound into our sound ID slot; this will stop any existing sound in this slot that is currently playing, as well.
bool DefaultSoundPreStep::LoadWav(const char *pSoundFilename)
{
    _ASSERTE(IsInitialized());
    // Note: this is called by Initialize(), so don't check for m_bWavPresent here
    return m_pEngine->LoadWav(m_soundID, pSoundFilename, m_playbackType);  // this logs a message on success or failure
}

// plays non-looped at max volume
bool DefaultSoundPreStep::LoadAndPlayWav(const char *pSoundFilename, float volume)
{
    bool bRetVal = false;
    m_bWavPresent = false;      // reset

    if (LoadWav(pSoundFilename))
    {
        m_bWavPresent = true;
        bRetVal = PlayWav(false, volume);
    }

    return bRetVal;
}

// Play our loaded sound ID (even if it's already playing -- we may be changing the volume or bLoop settings)
//   volume: 0..1.0
// Returns true on success, false if invalid sound ID
bool DefaultSoundPreStep::PlayWav(const bool bLoop, float volume)
{
    _ASSERTE(IsInitialized());
    bool bSuccess = false;

    if (m_bWavPresent)
        bSuccess = m_pEngine->PlayWav(m_soundID, bLoop, volume);  // this logs a message on success or failure

    return bSuccess;
}

// Stop our wav playing (the StopWav call in the XRSoundEngine already only actually stops the sound
// if it was already playing, so we don't need to do that here).
// Returns true on success, false if invalid sound ID (should never happen)
bool DefaultSoundPreStep::StopWav()
{
    _ASSERTE(IsInitialized());
    bool bSuccess = false;

    if (m_bWavPresent)
        bSuccess = m_pEngine->StopWav(m_soundID);   // this logs a message on success or failure

    return bSuccess;
}

// Returns true if our wav is playing, false if not.
bool DefaultSoundPreStep::IsWavPlaying() const
{
    _ASSERTE(IsInitialized());
    bool bSuccess = false;

    if (m_bWavPresent)
        bSuccess = m_pEngine->IsWavPlaying(m_soundID);

    return bSuccess;
}

// Returns true if successfully paused, false if not playing and so it cannot be paused.
//   bPause: true to pause sound, false to unpause it
bool DefaultSoundPreStep::SetPaused(const bool bPause)
{
    _ASSERTE(IsInitialized());
    bool bSuccess = false;

    if (m_bWavPresent)
        bSuccess = m_pEngine->SetPaused(m_soundID, bPause);

    return bSuccess;
}

bool DefaultSoundPreStep::IsPaused() const
{
    _ASSERTE(IsInitialized());
    bool bSuccess = false;

    if (m_bWavPresent)
        bSuccess = m_pEngine->IsPaused(m_soundID);   // this logs a message on success or failure

    return bSuccess;
}

// ------------------------------------------------------------------------

// Subclass that handles playing all animation default sounds via properties read from vessel class-override .cfg files.

// Constructor
//  pEngine: engine/vessel to which these sounds pertain
//  animationSounds: animation ID + group of sounds to play for that animation when it moves or stops
AnimationSoundPreStep::AnimationSoundPreStep(VesselXRSoundEngine *pEngine, const AnimationSounds &animationSounds) :
    SoundPreStep(pEngine), m_animationSounds(animationSounds)
{ 
}

// Invoked every n frames from XRSoundDLL
void AnimationSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // play the corresponding sound for this animation state
    const AnimationState *pAnimationState = m_pEngine->GetAnimationState(m_animationSounds.GetAnimationID());
    
    // this will be nullptr if the vessel does not have an animation ID in Orbiter matching the animation ID we have from the vessel's XRSound-<vessel class>.cfg file
    if (pAnimationState)   
    {
        // Note: for all states except Idle, it means that the animation just *transitioned* to that state in this frame.  
        // In the frame that follows, the state will either be Moving or Idle.
        const AnimationState::StateType stateType = pAnimationState->State;
        switch (stateType)
        {
        case AnimationState::StateType::Unknown:
            break;  // nothing to do yet -- sim is just starting

        case AnimationState::StateType::Idle:
            // only stop the MOVING sound here: the other sound, which does not loop, needs to finish.
            // Note: it is OK to call StopWave repeatedly each from on a wav that is already stopped; it is a lightweight call and will not spam the logs
            m_pEngine->StopWav(m_animationSounds.GetMovingSoundID());
            break;

        case AnimationState::StateType::Opening:
        case AnimationState::StateType::Open:
        case AnimationState::StateType::Closing:
        case AnimationState::StateType::Closed:
            LoadAndPlayWavUsingID(m_animationSounds.GetOpenCloseSoundID(), m_animationSounds.GetWavForAnimationState(stateType), false, m_animationSounds.GetPlaybackType());
            break;

        case AnimationState::StateType::Moving:
            LoadAndPlayWavUsingID(m_animationSounds.GetMovingSoundID(), m_animationSounds.GetWavForAnimationState(stateType), true, m_animationSounds.GetPlaybackType());
            break;

        default:
            // this means we missed an enum case
            _ASSERTE(false);
            break;      // no-op
        }
    }
}

// ------------------------------------------------------------------------

// Each of these callbacks are invoked every n frames from XRSoundDLL *if* the sound is enabled

void AirConditioningDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // We technically wouldn't need this check since the playbackType of this sound is InternalOnly (i.e., we could just call PlayWav every frame, 
    // but we don't want to spam the log with confusing PlayWav messages when the sound won't actaully play.
    if (m_pEngine->InCockpitView())
        PlayWav(true);
    else
        StopWav();
}

void LandedWindDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // if ship is not in focus, the sound should not play
    if (!HasFocus())
    {
        StopWav();
        return;
    }

    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check 

    // We don't want to constantly keep replaying the song from the beginning each time the pilot switches out to an external view, so we pause / unpause the song
    // if it's already playing *until* the ship is no longer landed, at which point we stop the sound playing.
    if (!pVessel->GroundContact())
    {
        StopWav();
    }
    else  // ship is on the ground
    {
        if (!m_pEngine->InCockpitView())    // in external view?
        {
            if (!IsWavPlaying())
            {
                PlayWav(true);
            }
            else
            {
                SetPaused(false);  // already playing, so resume it
            }
        }
        else  // back in cockpit view, so pause the sound in case it was already playing
        {
            SetPaused(true);
        }
    }
}

bool AudioGreetingDefaultSoundPreStep::s_bPlayedGreeting = false;

void AudioGreetingDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // only ONE of these greetings plays per simulation run
    // plays in any view for any vessel that has this configured
    if (!s_bPlayedGreeting && (simt > 1.0) && HasFocus())
    {
        if (PlayWav(false))
            s_bPlayedGreeting = true;
    }
}

// ------------------------------------------------------------------------

// Class handling engine thrust sounds (main / hover / retro).  RCS is handled by another class.

// Constructor
//  tghType: THGROUP_MAIN, THGROUP_RETRO, or THGROUP_HOVER
EngineDefaultSoundPreStep::EngineDefaultSoundPreStep(VesselXRSoundEngine *pEngine, const THGROUP_TYPE thgType, const float minVolume, const float maxVolume) :
    DefaultSoundPreStep(pEngine), m_thgType(thgType), m_minVolume(minVolume), m_maxVolume(maxVolume)
{
}

// this is invoked at every timestep.  This code was moved here / ported from the EngineSoundsPreStep class in the XR codebase.
void EngineDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // these sounds play even if the ship is not in focus
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check 

    const double totalThrustLevel = pVessel->GetThrusterGroupLevel(m_thgType);
    if (totalThrustLevel > 0)   // should sound be playing?
    {
        const float volume = XRSoundEngine::ComputeVariableVolume(m_minVolume, m_maxVolume, totalThrustLevel);

        // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "Engine thrust level = %lf, volume = %f", totalThrustLevel, volume);

        // OK if sound already playing here; if so, this call will merely change the volume
        PlayWav(true, volume);
    }
    else
        StopWav();
}

// ------------------------------------------------------------------------

// Class handling RCS thrust sound.

const float RCSDefaultSoundPreStep::s_minThrustLevelForSound = 0.20f;

// Constructor
RCSDefaultSoundPreStep::RCSDefaultSoundPreStep(VesselXRSoundEngine *pEngine) :
    DefaultSoundPreStep(pEngine)
{
    m_thrustVectorsROT = m_thrustVectorsLIN = _V(0, 0, 0);  // set by value

    // we have six total axes, but need to check for both rotation and translation: rotation and translation for a given axis share the same sound slot
    const XRSoundConfigFileParser &config = m_pEngine->GetConfig();
    m_pRCSAttackForAxisSoundArray[0]  = new RCSAttackForAxisSound(m_thrustVectorsROT.x, XRSound::RCSAttackPlusX, pEngine, false, config.RCSAttackPlusX);
    m_pRCSAttackForAxisSoundArray[1]  = new RCSAttackForAxisSound(m_thrustVectorsLIN.x, XRSound::RCSAttackPlusX, pEngine, false, config.RCSAttackPlusX);
    m_pRCSAttackForAxisSoundArray[2]  = new RCSAttackForAxisSound(m_thrustVectorsROT.y, XRSound::RCSAttackPlusY, pEngine, false, config.RCSAttackPlusY);
    m_pRCSAttackForAxisSoundArray[3]  = new RCSAttackForAxisSound(m_thrustVectorsLIN.y, XRSound::RCSAttackPlusY, pEngine, false, config.RCSAttackPlusY);
    m_pRCSAttackForAxisSoundArray[4]  = new RCSAttackForAxisSound(m_thrustVectorsROT.z, XRSound::RCSAttackPlusZ, pEngine, false, config.RCSAttackPlusZ);
    m_pRCSAttackForAxisSoundArray[5]  = new RCSAttackForAxisSound(m_thrustVectorsLIN.z, XRSound::RCSAttackPlusZ, pEngine, false, config.RCSAttackPlusZ);

    m_pRCSAttackForAxisSoundArray[6]  = new RCSAttackForAxisSound(m_thrustVectorsROT.x, XRSound::RCSAttackMinusX, pEngine, true, config.RCSAttackMinusX);
    m_pRCSAttackForAxisSoundArray[7]  = new RCSAttackForAxisSound(m_thrustVectorsLIN.x, XRSound::RCSAttackMinusX, pEngine, true, config.RCSAttackMinusX);
    m_pRCSAttackForAxisSoundArray[8]  = new RCSAttackForAxisSound(m_thrustVectorsROT.y, XRSound::RCSAttackMinusY, pEngine, true, config.RCSAttackMinusY);
    m_pRCSAttackForAxisSoundArray[9]  = new RCSAttackForAxisSound(m_thrustVectorsLIN.y, XRSound::RCSAttackMinusY, pEngine, true, config.RCSAttackMinusY);
    m_pRCSAttackForAxisSoundArray[10] = new RCSAttackForAxisSound(m_thrustVectorsROT.z, XRSound::RCSAttackMinusZ, pEngine, true, config.RCSAttackMinusZ);
    m_pRCSAttackForAxisSoundArray[11] = new RCSAttackForAxisSound(m_thrustVectorsLIN.z, XRSound::RCSAttackMinusZ, pEngine, true, config.RCSAttackMinusZ);
}

// Destructor
RCSDefaultSoundPreStep::~RCSDefaultSoundPreStep()
{
    for (int i = 0; i < (sizeof(m_pRCSAttackForAxisSoundArray) / sizeof(RCSAttackForAxisSound *)); i++)
        delete m_pRCSAttackForAxisSoundArray[i];
}

// Overrides our base class method so we can load our RCSAttack sound.
// Note: caller should pass the RCSSustain sound parameters this method.
bool RCSDefaultSoundPreStep::Initialize(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType)
{
    return DefaultSoundPreStep::Initialize(soundID, pSoundFilename, playbackType);   // load sustain sound
}

// this is invoked at every timestep.  This code was moved here / ported from the EngineSoundsPreStep class in the XR codebase.
void RCSDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // these sounds play even if the ship is not in focus
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check 

    // Add up all the RCS thrusters for *both* modes, since an autopilot can fire in any mode even if the pilot has them in another mode 
    // or switched off. if TOTAL thrust >= 1.0, use full volume on RCS sustain.
    pVessel->GetAttitudeRotLevel(m_thrustVectorsROT);
    pVessel->GetAttitudeLinLevel(m_thrustVectorsLIN);

    // Handle RCSAttack sounds for each of our axes
    for (int i = 0; i < (sizeof(m_pRCSAttackForAxisSoundArray) / sizeof(RCSAttackForAxisSound *)); i++)
        m_pRCSAttackForAxisSoundArray[i]->clbkPreStep();

    // Compute the total thrust of all thrusters
    double totalThrustLevel =
        fabs(m_thrustVectorsROT.x) + fabs(m_thrustVectorsROT.y) + fabs(m_thrustVectorsROT.z) +
        fabs(m_thrustVectorsLIN.x) + fabs(m_thrustVectorsLIN.y) + fabs(m_thrustVectorsLIN.z);

    // TODO: could set this to 2.0 if we wanted to vary the RCS sustain volume to show more than 1 thruster engaged
    const double maxTotalThrustLevel = 1.0;  
    if (totalThrustLevel > maxTotalThrustLevel)
        totalThrustLevel = maxTotalThrustLevel;

    // Play or halt the RCS sustain sound
    // tweaked per user feedback: do not play thruster sound if total thrust level is under a certain minimum threshold; 
    // e.g., when Prograde or Retrograde autopilot is enabled and firing thrusters at very low levels.
    if (totalThrustLevel > GetConfig().MinThrusterLevelForRCSSoundEffects)   // should sound be playing?
    {
        const float volume = XRSoundEngine::ComputeVariableVolume(s_minThrustLevelForSound, 1.0, totalThrustLevel / maxTotalThrustLevel);

        // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "RCS thrust level = %lf, volume = %f", totalThrustLevel, volume);
        // if sound already playing here, this call will merely change the volume
        PlayWav(true, volume);   // loop
    }
    else    // sound should NOT be playing
    {
        // Note: we never terminate the RCS Attack sounds: it always completes
        StopWav();
    }
}

// Child class of RCSDefaultSoundPreStep that handles RCS thrust sound for a single axis.

// Constructor
//   pWavFilePath: may be nullptr or empty; if so, this sound will not play
RCSDefaultSoundPreStep::RCSAttackForAxisSound::RCSAttackForAxisSound(const double &axisThrustLevel, const int soundID, VesselXRSoundEngine *pEngine, const bool bNegativeAxis, const char *pWavFilePath) :
    m_axisThrustLevel(axisThrustLevel), m_soundID(soundID), m_bAttackSoundMayPlay(true), m_pEngine(pEngine), m_bNegativeAxis(bNegativeAxis)
{
    // if load fails or sound file path is not set, RCSAttack sound will not play
    m_pEngine->LoadWav(soundID, pWavFilePath, XRSound::PlaybackType::BothViewClose);  
}

// Destructor
RCSDefaultSoundPreStep::RCSAttackForAxisSound::~RCSAttackForAxisSound()
{
    m_pEngine->StopWav(m_soundID);
}

// Invoked at every PreStep by our owning RCSDefaultSoundPreStep
void RCSDefaultSoundPreStep::RCSAttackForAxisSound::clbkPreStep()
{
    // m_axisThrustLevel is a reference to a specific axis's thrust level that resides in a VECTOR3 in our parent class; thrust levels are from -1.0 to 1.0, but
    // we ignore negative thrust values because that means it is being handled by another axis
    const double thrustLevel = (m_bNegativeAxis ? -m_axisThrustLevel : m_axisThrustLevel);
    if (thrustLevel >= s_minThrustLevelForSound)
    {
        if (m_bAttackSoundMayPlay)
        {
            const float volume = XRSoundEngine::ComputeVariableVolume(s_minThrustLevelForSound, 1.0, thrustLevel);
            m_pEngine->PlayWav(m_soundID, false, volume);  // Note: if RCSAttack is already playing, this call will have no effect since the volume is not changing.
            m_bAttackSoundMayPlay = false;  // attack should not play again until thrust level drops below minimum level for sound
            // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "RCSAttack sound playing for axis ref 0x%X, volume %f", reinterpret_cast<unsigned int>(&m_axisThrustLevel), volume);
        }
    }
    else 
    {
        // Reset, since thrust fell below the minimum threshold for sound to play for it.  
        // However, do not stop the RCSAttack sound currently playing, if any: RCSAttack sounds always finish playing.
        m_bAttackSoundMayPlay = true;
    }
}

// ------------------------------------------------------------------------

// Constructor
RCSModeDefaultSoundPreStep::RCSModeDefaultSoundPreStep(VesselXRSoundEngine *pEngine) : 
    SoundPreStep(pEngine), m_previousRCSMode(-1)
{
}

void RCSModeDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // if ship is not in focus, the sound should not play
    if (!HasFocus())
        return;

    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    const int rcsMode = pVessel->GetAttitudeMode();
    if (m_previousRCSMode < 0)
    {
        // first time through here, so initialize the state
        m_previousRCSMode = rcsMode;
        return;
    }

    if (rcsMode != m_previousRCSMode)
    {
        // Note: LoadAndPlayWav handles nullptr or empty (i.e., disabled) file paths and returs false
        switch (rcsMode)
        {
        // Note: we use XRSound::Radio mode for these so the pilot can hear them even in external view (e.g., when they switch modes via the keyboard)
        case RCS_ROT:
            PlaySwitch(true);  // "On" click sound
            LoadAndPlayWavUsingID(XRSound::Rotation, GetConfig().RCSRotation, false, XRSound::PlaybackType::Radio);
            break;

        case RCS_LIN:
            PlaySwitch(true);  // "On" click sound
            LoadAndPlayWavUsingID(XRSound::Translation, GetConfig().RCSTranslation, false, XRSound::PlaybackType::Radio);
            break;

        case RCS_NONE:
            PlaySwitch(false);  // "Off" click sound
            LoadAndPlayWavUsingID(XRSound::Off, GetConfig().RCSOff, false, XRSound::PlaybackType::Radio);
            break;

        default:
            _ASSERTE(false);    // should never happen!
            break;
        }
        m_previousRCSMode = rcsMode;  // remember for next time

        // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "RCS Mode=%d, AFCtrlMode=%d", rcsMode, min(pVessel->GetADCtrlMode(), 2));
    }
}

// ------------------------------------------------------------------------

// Constructor
AFCtrlModeDefaultSoundPreStep::AFCtrlModeDefaultSoundPreStep(VesselXRSoundEngine *pEngine) :
    SoundPreStep(pEngine), m_previousAFCtrlMode(-1)
{
}

void AFCtrlModeDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // if ship is not in focus, the sound should not play
    if (!HasFocus())
        return;

    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    // GetADCtrlMode()
    // - bit 0: elevator enabled/disabled 
    // - bit 1: rudder enabled / disabled
    // - bit 2: ailerons enabled / disabled
    const int afCtrlMode = min(pVessel->GetADCtrlMode(), 2);  // 0 = OFF, 1 = PITCH (elevators), 2 = ON (more than just pitch)

    if (m_previousAFCtrlMode < 0)
    {
        // first time through here, so initialize the state
        m_previousAFCtrlMode = afCtrlMode;
        return;
    }

    if (afCtrlMode != m_previousAFCtrlMode)
    {
        // Note: LoadAndPlayWav handles nullptr or empty (i.e., disabled) file paths and returs false
        switch (afCtrlMode)
        {
        // Note: we use XRSound::Radio mode for these so the pilot can hear them even in external view (e.g., when he switches modes via the keyboard)
        case 0:
            PlaySwitch(false);  // "Off" click sound
            LoadAndPlayWavUsingID(XRSound::AFOff, GetConfig().AFOff, false, XRSound::PlaybackType::Radio);
            break;

        case 1:
            PlaySwitch(true);  // "On" click sound
            LoadAndPlayWavUsingID(XRSound::AFPitch, GetConfig().AFPitch, false, XRSound::PlaybackType::Radio);
            break;

        case 2:
            PlaySwitch(true);  // "On" click sound
            LoadAndPlayWavUsingID(XRSound::AFOn, GetConfig().AFOn, false, XRSound::PlaybackType::Radio);
            break;

        default:
            _ASSERTE(false);    // should never happen!
            break;
        }
        m_previousAFCtrlMode = afCtrlMode;  // remember for next time
    }
}

// ------------------------------------------------------------------------

// Constructor
LogThrusterDataPreStep::LogThrusterDataPreStep(VesselXRSoundEngine *pEngine) 
    : SoundPreStep(pEngine), m_simtNextLog(1)
{
}

// Invoked n times per second 
void LogThrusterDataPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    if (GetConfig().LogThrusterData && simt >= m_simtNextLog)
    {
        m_simtNextLog = simt + 1.0;     // log once per second

        const VESSEL *pVessel = GetVessel();
        if (!pVessel)
            return;         // sanity check

        // loop through all thrusters and log data for any that are active
        for (DWORD i = 0; i < pVessel->GetThrusterCount(); i++)
        {
            const THRUSTER_HANDLE thHandle = pVessel->GetThrusterHandleByIndex(i);
            const double thLevel = pVessel->GetThrusterLevel(thHandle);
            if (thLevel > 0)
            {
                double thrusterMax = pVessel->GetThrusterMax(thHandle, 0);
                const double thrust = thrusterMax * thLevel / 1000;  // in kilonewtons

                CString msg;
                msg.Format("LogThrusterData: [thruster index %u] thrust level = %lf, thrust = %lf kN", i, thLevel, thrust);
                WriteLog(msg);
            }
        }
    }
}

// ------------------------------------------------------------------------

// Handles one custom engine sound per vessel; e.g., SCRAM engines.
// Constructor
CustomEnginesDefaultSoundPreStep::CustomEnginesDefaultSoundPreStep(VesselXRSoundEngine *pEngine) :
    DefaultSoundPreStep(pEngine), m_bFirstRun(true)
{
}

// Invoked n times per second 
void CustomEnginesDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    _ASSERTE(IsInitialized());      // wav should be present if we were invoked

    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    const int soundID = XRSound::CustomEngines;
    vector<int> customThrusterIndexes = GetConfig().CustomEnginesThrusterIndexesAsVector();

    if (m_bFirstRun)
    {
        m_bFirstRun = false;

        // we want to show the actual integers here, not SupportedSoundFileTypes string, in case of parse errors resulting in unexpected 0 values
        CString thrusterIdxStr;
        for (unsigned int i = 0; i < customThrusterIndexes.size(); i++)
        {
            if (i > 0)
                thrusterIdxStr += " ";

            CString intVal;
            intVal.Format("%d", customThrusterIndexes[i]);
            thrusterIdxStr += intVal;
        }

        // log which custom thruster IDs we are using
        CString msg;
        msg.Format("CustomEnginesDefaultSoundPreStep: using custom engine sound '%s' for thrusters w/indexes [%s]", static_cast<const char*>(m_pEngine->GetWavFilename(soundID)), static_cast<const char *>(thrusterIdxStr));
        WriteLog(msg);
    }

    // figure out total engine thrust for all the thrusters in customThrusterIndexes
    double totalThrustLevel = 0;
    const int thrusterCount = static_cast<int>(customThrusterIndexes.size());
    for (int i = 0; i < thrusterCount; i++)
    {
        const THRUSTER_HANDLE th = pVessel->GetThrusterHandleByIndex(i);
        totalThrustLevel += pVessel->GetThrusterLevel(th);
    }

    if (totalThrustLevel > 0)   // should sound be playing?
    {
        // divide total thrust level by # of thrusters so we get an accurate thrust level for all thrusters in the group
        const double level = totalThrustLevel / thrusterCount;
        const float volume = XRSoundEngine::ComputeVariableVolume(0.40f, 1.0f, level);

        // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "Custom Engine thrust level = %lf, volume = %f", totalThrustLevel, volume);

        // OK if sound already playing here; if so, this call will merely change the volume
        PlayWav(true, volume);
    }
    else
        StopWav();
}

//-------------------------------------------------------------------------

// Handles liftoff / touchdown / wheel chirp / crash / gear rolling / 100 Knots sounds.
// Based on class of the same name in DeltaGliderXR1\XR1Lib\XR1PreSteps.cpp; however, we do not play "V1" or "Rotate"
// callouts, since cwe have no way of knowing what those values should be for a given vessel.

// Constructor
TakeoffAndLandingCalloutsAndCrashPreStep::TakeoffAndLandingCalloutsAndCrashPreStep(VesselXRSoundEngine *pEngine) :
    SoundPreStep(pEngine), 
    m_takeoffTime(0), m_previousFrameVerticalSpeed(-1), m_airborneTargetTime(0), m_touchdownTime(0),
    m_previousFrameAirspeed(-1)
{
#define LOAD_SOUND(soundID, pbType)  \
    if (*GetConfig().soundID) LoadWav(XRSound::soundID, GetConfig().soundID, XRSound::PlaybackType::pbType)

    LOAD_SOUND(Crash,           BothViewFar);
    LOAD_SOUND(MetalCrunch,     BothViewFar);
    LOAD_SOUND(Touchdown,       Radio);
    LOAD_SOUND(OneHundredKnots, Radio);
    LOAD_SOUND(Liftoff,         Radio);

    // slight optimization: only load these sounds if this vessel has gear
    if (pEngine->HasLandingGear())
    {
        LOAD_SOUND(WheelChirp,   BothViewMedium);
        LOAD_SOUND(WheelStop,    Radio);
        LOAD_SOUND(TiresRolling, BothViewClose);
    }
}

void TakeoffAndLandingCalloutsAndCrashPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    static const double airborneTriggerTime = 0.5;          // assume airborne 1/2-second after wheels-up
    const double airspeed = pVessel->GetAirspeed();
    const double groundspeed = pVessel->GetGroundspeed();
    const double altitude = m_pEngine->GetGearAdjustedAltitude();  // adjusts for extended gear or hull size
    const bool bGearDeployed = (m_pEngine->GetLandingGearAnimationState() == 1.0);
    const bool bGearStateUnknown = (m_pEngine->GetLandingGearAnimationState() < 0);  // we don't know if this vessel has gear or not

    VECTOR3 asVector;
    pVessel->GetAirspeedVector(FRAME_HORIZON, asVector);
    double verticalSpeed = -(asVector.y);  // in m/s  (asVector.y will be negative if descending, so we reverse it to mean higher == faster descent)

    if (m_previousFrameAirspeed < 0)
        goto exit;      // set the data for next time

    // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "altitude = %lf", altitude);

    // check whether in contact with the ground
    if ((pVessel->GroundContact() || altitude <= 0.0))
    {
        // check whether we just touched down (i.e., were flying before)
        if (m_takeoffTime > 0)   // m_takeoffTime will remain > 0 until the vessel comes to a complete stop, either after a crash or after a landing
        {
            double touchdownVerticalSpeed = verticalSpeed;

            // As a scenario editor fix, if our previous frame's altitude was > 100 meters, assume this was a scenario editor "instant touchdown" and prevent 
            // any bogus damage/hard landing checks.  However, we cannot just check the previous frame's altitude because that can change rapidly between frames under time acc.
            // Therefore, we check for IsLanded() and touchdownVerticalSpeed == 0.0 here.
            if (m_pEngine->IsLanded() && (touchdownVerticalSpeed == 0.0))
                m_previousFrameVerticalSpeed = verticalSpeed = 0.0;  // the scenario editor moved us

            // NOTE: if touchdownVerticalSpeed < m_previousFrameVerticalSpeed (meaning, the impact was SOFTER than the previous frame's value), use
            // the PREVIOUS frame as the impact velocity because Orbiter just "bounced" us!
            if (touchdownVerticalSpeed < m_previousFrameVerticalSpeed)
            {
                // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "touchdownVecticalSpeed=%lf, previousFrameVerticalSpeed=%lf", touchdownVerticalSpeed, m_previousFrameVerticalSpeed);
                touchdownVerticalSpeed = m_previousFrameVerticalSpeed;    // use the harder impact, which is the true impact velocity
            }

            // we just touched down (or crashed!)
            // Note: the sounds below are loaded globally
            if (touchdownVerticalSpeed >= 39.0)  // matches CREW_IMPACT_DEATH_THRESHOLD in the XR code
            {
                PlayWav(XRSound::Crash);        // already loaded as a global sound
                goto exit;      // do not reset for ground mode until we have a gentler impact on some upcoming frame
            }
            else if (touchdownVerticalSpeed >= 4.0)   // the XR1 has a max touchdown rate of 3.0 m/s, and the XR2's max is -3.7.  So we'll be a bit generous here.
            {
                // this is a hard landing or a "thump" as the ship rolls after crashing; i
                PlayWav(XRSound::MetalCrunch);
                goto exit;      // do not reset for ground mode until we have a gentler impact on some upcoming frame
            }

            // if we reach here, vessel touched down without a crash or gear collapse
            if (!bGearStateUnknown)
            {
                // this vessel has landing gear, so we can check for a belly landing or wheel chirp
                if (bGearDeployed)
                {
                    if (groundspeed >= 45.0)   // 45 m/s == ~100 MPH
                        PlayWav(XRSound::WheelChirp);

                    PlayWav(XRSound::Touchdown);        // "Touchdown" (or "Wheels Down" if DG, etc.)
                }
                else
                {
                    // belly landing!
                    PlayWav(XRSound::MetalCrunch);
                }
            }
            else
            {
                // gear state unknown, so just play "Touchdown"
                PlayWav(XRSound::Touchdown);
            }

            //
            // Reset the ship for ground mode; i.e., ready for rolling to stop and then takeoff again
            //
            m_takeoffTime = 0;
            m_touchdownTime = simt;
            m_airborneTargetTime = 0;   // reset timer

            // m_takeoffTime will remain zero until the ship remains airborne long enough again
            goto exit;
        }  // if (m_takeoffTime > 0)

        // 
        // Vessel is rolling on ground for either takeoff or landing (m_takeoffTime == 0).
        //

        // reset airborne timer in case we are bouncing during takeoff, or if we hovered just enough to bounce
        m_airborneTargetTime = 0;  // reset timer

        // check whether we reached wheel-stop
        if (m_pEngine->IsLanded())
        {
            // wheel-stop, so we are ready to launch!  Reset everything.
            if ((m_touchdownTime > 0) && bGearDeployed)
                PlayWav(XRSound::WheelStop);    // this will only play if we had *lifted off* (b/c of `m_touchdownTime > 0` above)

            StopWav(XRSound::TiresRolling);
            m_takeoffTime = m_touchdownTime = 0;
        }
        else if (m_takeoffTime == 0)
        {
            // we're rolling along the ground for takeoff or landing
            if (bGearDeployed)
            {
                // max volume reached at 100 knots
                const double level = groundspeed / KNOTS_TO_MPS(100);
                const float volume = XRSoundEngine::ComputeVariableVolume(0.1, 1.0, level);
                PlayWav(XRSound::TiresRolling, true, volume);   // loop this
            }

            // play the "100 Knots" callout if cross that threshold
            double mpsKnots = KNOTS_TO_MPS(100);
            if ((airspeed >= mpsKnots) && (m_previousFrameAirspeed < mpsKnots) ||
                (airspeed <= mpsKnots) && (m_previousFrameAirspeed > mpsKnots))
            {
                PlayWav(XRSound::OneHundredKnots);
            }
        }
    }  // if ((pVessel->GroundContact() || altitude <= 0.0))
    else  // we're airborne -- disarm the takeoff callouts IF we've been airborne long enough to be sure it's not just a bounce
    {
        if (m_takeoffTime == 0)     // are we still taking off?
        {
            if (m_airborneTargetTime == 0)      // did we just become airborne?
            {
                m_airborneTargetTime = simt + airborneTriggerTime;  // start the timer running
            }
            else    // timer already running, so we went airborne recently
            {
                if (simt >= m_airborneTargetTime)
                {
                    // timer expired -- we're airborne!
                    StopWav(XRSound::TiresRolling);

                    // since we can't save the state of this, don't play this if the sim just started
                    if (simt > 3.0)
                        PlayWav(XRSound::Liftoff);

                    m_takeoffTime = simt;
                    m_touchdownTime = 0;      // reset: we are in flight now
                }
            }
        }  // if (m_takeoffTime == 0)
    }  // we're airborne
    
exit:
    // common exit point
    m_previousFrameVerticalSpeed = verticalSpeed;
    m_previousFrameAirspeed = airspeed;
}

//-------------------------------------------------------------------------

void WheelbrakeDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    const double level = (pVessel->GetWheelbrakeLevel(1) + pVessel->GetWheelbrakeLevel(2)) / 2;
    const double groundSpeed = pVessel->GetGroundspeed();
    const float volume = XRSoundEngine::ComputeVariableVolume(0.1, 1.0, level);

    if ((level > 0) && (groundSpeed >= 1.0))
        PlayWav(true, volume);
    else
        StopWav();
}

//-------------------------------------------------------------------------

FlightWindAndPlasmaSoundPreStep::FlightWindAndPlasmaSoundPreStep(VesselXRSoundEngine *pEngine) :
    SoundPreStep(pEngine)
{
    LoadWav(XRSound::FlightWind, GetConfig().FlightWind, XRSound::PlaybackType::BothViewFar);
    LoadWav(XRSound::ReentryPlasma, GetConfig().ReentryPlasma, XRSound::PlaybackType::BothViewFar);
}

void FlightWindAndPlasmaSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    const double dynamicPressure = pVessel->GetDynPressure();
    const double plasmaLevel = m_pEngine->GetPlasmaLevel();  // 0..1.0

    // Note: dynamicPressure is ~20,000 at ~200 m/s in lower Earth atmosphere (which is ~450 mph).
    double windLevel = dynamicPressure / 40e3;      // max out the volume at ~900 mph in lower atmosphere
    if (windLevel > 1.0)
        windLevel = 1.0;

    float windVolume = XRSoundEngine::ComputeVariableVolume(0, 1.0, windLevel);
    float plasmaVolume = XRSoundEngine::ComputeVariableVolume(0, 1.0, plasmaLevel);

    // Note: during testing, the wind volume tends to drown out the plasma volume, so we adjust wind volume down here as plasma volume goes up
    float windVolAdjustment = 1.0f - (windVolume + plasmaVolume);      // e.g., -0.5 if wind + plasma == 1.5
    if (windVolAdjustment < 0)
        windVolume += windVolAdjustment;    // adjust so that plasma + wind = 1.0

    if (windVolume > 0)
        PlayWav(XRSound::FlightWind, true, windVolume);
    else
        StopWav(XRSound::FlightWind);

    if (plasmaVolume > 0)
        PlayWav(XRSound::ReentryPlasma, true, plasmaVolume);
    else
        StopWav(XRSound::ReentryPlasma);

    // DEV DEBUGGING ONLY: if (m_pEngine->IsDG()) sprintf(oapiDebugString(), "windVolume = %lf, plasmaVolume = %lf, plasmaLevel = %lf", windVolume, plasmaVolume, plasmaLevel);
}

//-------------------------------------------------------------------------

AutopilotOnOffSoundPreStep::AutopilotOnOffSoundPreStep(VesselXRSoundEngine *pEngine) :
    SoundPreStep(pEngine), m_prevNavmode(-1)
{
    LoadWav(XRSound::AutopilotOn, GetConfig().AutopilotOn, XRSound::PlaybackType::InternalOnly);
    LoadWav(XRSound::AutopilotOff, GetConfig().AutopilotOff, XRSound::PlaybackType::InternalOnly);
}

void AutopilotOnOffSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    // Optmization: only process this if this vessel has focus
    if (!m_pEngine->HasFocus())
        return;

    // check if any navmode (autopilot) *besides killrot* is engaged; only ONE of these other nav modes can be engaged at any one time
    /*
    • #define NAVMODE_KILLROT 1
    "Kill rotation" mode
    
    • #define NAVMODE_HLEVEL 2
    "Hold level with horizon" mode
    
    • #define NAVMODE_PROGRADE 3
    "Prograde" mode
    
    • #define NAVMODE_RETROGRADE 4
    "Retrograde" mode
    
    • #define NAVMODE_NORMAL 5
    "Normal to orbital plane" mode
    
    • #define NAVMODE_ANTINORMAL 6
    "Anti-normal to orbital plane" mode
    
    • #define NAVMODE_HOLDALT 7
    "Hold altitude" mode
    */
    int currentNavmode = 0;   // assume no autopilot engaged
    for (int i = 2; i <= 7; i++)
    {
        if (pVessel->GetNavmodeState(i))
        {
            currentNavmode = i;
            break;
        }
    }

    if (m_prevNavmode >= 0)   // not first frame of the simulation?
    {
        static const float volume = 0.86f;     // matches XR's volume for these sounds
        // we have valid data in m_bAutopilotEngaged -- play a sound if the active autopilot changed
        // Note: ideally we would just use a single slot for both sounds, but that would mean that users could not 
        // override or disable just one of the sounds.
        if (currentNavmode != m_prevNavmode)
        {
            if (currentNavmode)   // any autopilot active?
            {
                StopWav(XRSound::AutopilotOff);
                PlayWav(XRSound::AutopilotOn, false, volume);
            }
            else   // no autopilot active
            {
                StopWav(XRSound::AutopilotOn);
                PlayWav(XRSound::AutopilotOff, false, volume);
            }
        }
    }

    // save for next time
    m_prevNavmode = currentNavmode;
}

//-------------------------------------------------------------------------

// Constructor
DockingRadarDefaultSoundPreStep::DockingRadarDefaultSoundPreStep(VesselXRSoundEngine *pEngine) : 
    DefaultSoundPreStep(pEngine), m_realtimeNextBeep(-1)
{
}

// this is invoked at every timestep
void DockingRadarDefaultSoundPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // these sounds do not play unless the ship is in focus
    if (!m_pEngine->HasFocus())
    {
        StopWav();
        m_realtimeNextBeep = -1;  // reset
        return;
    }

    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check 

    static const double maxBeepDistance = 500;  // matches distance at which Orbiter starts rendering its docking rectangles

    const double dockingDistance = m_pEngine->GetDockingDistance();

    if (m_pEngine->IsDocked() || (dockingDistance <= 0) || (dockingDistance > maxBeepDistance))
    {
        StopWav();
        m_realtimeNextBeep = -1;  // reset so we beep immediately when we come into range
    }
    else  // we're in range of radar!  Figure out when to beep.
    {
        const double uptime = XRSoundDLL::GetSystemUptime();
        if (uptime >= m_realtimeNextBeep)  // either just came into range *or* we are in range but it's time for the next beep
        {
            StopWav();    // in case it's still playing
            PlayWav(false, 0.4f);       // play this at lower volume since it should really never be too loud
            // beep linearly from 4 seconds at max range to 0.5 second at zero range
            const double distanceFraction = dockingDistance / maxBeepDistance;
            const double interval = XRSoundEngine::ComputeVariableLevel(0.5, 4.0, distanceFraction);  // smaller distance fraction == shorter interval
            m_realtimeNextBeep = uptime + interval;
            // DEV DEBUGGING ONLY: if (m_pEngine->IsDG()) sprintf(oapiDebugString(), "distanceFraction = %lf, interval = %lf", distanceFraction, interval);
        }
    }
}

//-------------------------------------------------------------------------

// Constructor
DisableAutopilotsForTimeAccPreStep::DisableAutopilotsForTimeAccPreStep(VesselXRSoundEngine *pEngine) :
    SoundPreStep(pEngine)
{
}

// this is invoked at every timestep
void DisableAutopilotsForTimeAccPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    const double maxTimeAccForAP = GetConfig().DisableAutopilotsTimeAccThreshold;
    if (maxTimeAccForAP <= 0)
        return;     // feature disabled

    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;

    const double currentTimeACC = oapiGetTimeAcceleration();
    if (currentTimeACC > maxTimeAccForAP)
    {
        // disable any engaged default auotpilots, including KillRot
        for (int i = 1; i <= 7; i++)
        {
            if (pVessel->GetNavmodeState(i))
            {
                CString msg;
                msg.Format("DisableAutopilotsForTimeAccPreStep: auto-disabling autopilot w/navmode %d due to time acceleration (%dx) [configured max time acc threshold for autopilots is %dx]",
                    i, static_cast<int>(currentTimeACC), static_cast<int>(maxTimeAccForAP));
                WriteLog(msg);
                pVessel->DeactivateNavmode(i);
                // Note: either this frame or the next will play our AutopilotOff sound, so we don't need to do that here.
            }
        }
    }
}
