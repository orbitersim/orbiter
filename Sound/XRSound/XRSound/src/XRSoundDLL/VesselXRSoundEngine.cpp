// ==============================================================
// XRSound engine class bound to a vessel.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#include "VesselXRSoundEngine.h"
#include "SoundPreSteps.h"
#include "DefaultSoundGroupPreSteps.h"
#include "AnimationState.h"
#include "XRSoundDLL.h"   // for XRSoundDLL::GetAbsoluteSimTime()

// static pressure threshold at which OAT and Mach values are valid; matches the XR constant value
// (APPROX) AS SEEN ON SURFACE MFD, BUT TOO RISKY TO USE IN PRODUCTION: const double OAT_VALID_STATICP_THRESHOLD = 0.014;  // in pascals
#define OAT_VALID_STATICP_THRESHOLD 0.02
// ------------------------------------------------------------------------

// Only invoked by our base class's static DestroyInstance method
void VesselXRSoundEngine::FreeResources()
{
    CString msg;
    msg.Format("XRSoundEngine::FreeResources: freeing XRSound engine resources for vessel '%s'",
        static_cast<const char *>(GetVesselName()));
    s_globalConfig.WriteLog(msg);

    // stop all of this vessel's sounds and free all irrKlang resources for them
    StopAllWav();

    // free all our DefaultSoundPreStep objects
    FreeDefaultSounds();
}

// Static method to create a new instance of an XRSoundEngine for a vessel.  This is the ONLY place where new 
// XRSoundEngine instances for vessels are constructed.
//
// This also handles static one-time initialization of our singleton irrKlang engine.
VesselXRSoundEngine *VesselXRSoundEngine::CreateInstance(const OBJHANDLE hVessel)
{
    _ASSERTE(oapiIsVessel(hVessel));
    if (!oapiIsVessel(hVessel))
        return nullptr;

    // Must handle initializing the irrKlang engine here since clbkSimulationStart is too late: it needs to be done
    // before the first call to LoadWav.
    if (s_bIrrKlangEngineNeedsInitialization)
    {
        s_bIrrKlangEngineNeedsInitialization = false;
        InitializeIrrKlangEngine();
    }

    VesselXRSoundEngine *pEngine = new VesselXRSoundEngine(hVessel);
    pEngine->LoadDefaultSounds();  // these sounds are enabled for all vessels by default
    return pEngine;
}

// Constructor
VesselXRSoundEngine::VesselXRSoundEngine(const OBJHANDLE hVessel) :
    m_hVessel(hVessel)
{
    m_pConfig = new XRSoundConfigFileParser();
    VESSEL *pVessel = GetVessel();  // should never be nullptr at this point (XRSoundDLL::GetXRSoundEngineInstance already validated hVessel).
    _ASSERTE(pVessel);

    // parse XRSound.log + any vessel class-specific override file
    m_pConfig->ParseVesselSoundConfig(pVessel);
    if (m_pConfig->ParseFailed())
    {
        CString msg;
        msg.Format("Error parsing configuration file(s) '%s' -- see above error messages for details.", m_pConfig->GetConfigFilenames());
        m_pConfig->WriteLog(msg);
    }

    InitializeCachedData();
    LoadGlobalSounds();
}

// Destructor
VesselXRSoundEngine::~VesselXRSoundEngine()
{
    // our base class cleans up m_pConfig
}

// Enable or disable a default sound or group.  This can also be done globally via the configuration file; however, this option 
// is useful if you want to disable some sound effects at runtime.
//   soundID: which default sound to enable or disable
//   bEnabled: true to enable default sound, false to disable it
//
// Returns true on success, false if XRSound.dll not present this default sound is disabled via config file (i.e., this default sound is not loaded).
bool VesselXRSoundEngine::SetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID, const bool bEnabled)
{
    if (!IsKlangEngineInitialized())
        return false;

    bool bSuccess = false;

    // this works for both default sounds and defaults groups (which still only have one slot per group)
    if (IsDefaultSoundOrGroup(soundID))
    {
        WavContext *pContext = FindWavContext(soundID);

        // NOTE: it is possible for a vessel to disable a default sound before we have loaded it, so we have to handle that here
        if (!pContext)
        {
            // create an empty WavContext to hold the enabled status; this will be propagated later as the WavContext is updated with a real LoadWav
            WavContext wavContext(soundID, "", XRSound::PlaybackType::InternalOnly, bEnabled);
            m_allWavsMap.insert(soundID_WavContext_Pair(soundID, wavContext));   // copied by value
            pContext = FindWavContext(soundID);     // must get the persisted copy in our map; should always succeed now
        }

        _ASSERTE(pContext);

        // TOO VERBOSE -- keeps alternating when two or more XR1s are in the scenario: VERBOSE_LOG(this, "XRSoundEngine::SetDefaultSoundEnabled: setting default sound %s bEnabled = %d", pContext->ToStr(), bEnabled);

        pContext->bEnabled = bEnabled;
        UpdateSoundState(*pContext);    // disable or enable it immediately instead of waiting for the next timestep
    }

    return bSuccess;
}

// Returns true if the specified default sound is enabled, false if sound is disabled or XRSound.dll not present.
//   option: which default sound ID to check
bool VesselXRSoundEngine::GetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID)
{
    if (!IsKlangEngineInitialized())
        return false;

    bool bEnabled = false;

    // this works for both default sounds and default groups (which still only have one slot per group)
    if (IsDefaultSoundOrGroup(soundID))
    {
        const WavContext *pContext = FindWavContext(soundID);
        if (pContext)
            bEnabled = pContext->bEnabled;
    }

    return bEnabled;
}

// Set the default subfolder path for a default sound group, relative to $ORBITER_ROOT.
//   defaultSoundID: which default XRSound group to update (only DefaultSoundIDs that end in "Group" are valid for this call)
//   pSubfolderPath: subfolder path relative to $ORBITER_ROOT; may not be nullptr or empty.
//
// Returns true on success, false if XRSound.dll not present or defaultSoundID is not a valid group sound ID.
bool VesselXRSoundEngine::SetDefaultSoundGroupFolder(const XRSound::DefaultSoundID defaultSoundID, const char *pSubfolderPath)
{
    if (!IsKlangEngineInitialized())
        return false;

    bool bSuccess = false;

    if (IsDefaultSoundGroup(defaultSoundID))
    {
        // safe to cast here because we verified the defaultSoundID above
        DefaultSoundGroupPreStep *pPreStep = static_cast<DefaultSoundGroupPreStep *>(FindDefaultSoundGroupPreStep(defaultSoundID));
        if (!pPreStep)
        {
            VERBOSE_LOG(this, "XRSoundEngine::SetDefaultSoundGroupFolder WARNING: no default sounds loaded for sound ID %d; cannot set default sound folder '%s'",
                defaultSoundID, pSubfolderPath);
            return false;
        }

        // this logs a message on success or failure
        bSuccess = pPreStep->SetFolder(pSubfolderPath);
    }
    return bSuccess;
}

// Returns the default subfolder path for a default sound group, relative to $ORBITER_ROOT, 
// or nullptr if no default sounds loaded for the supplied defaultSoundID or XRSoundDLL not present.
//   groupdefaultSoundID: which default XRSound group to update (only DefaultSoundIDs that end in "Group" are valid for this call)
const char *VesselXRSoundEngine::GetDefaultSoundGroupFolder(const XRSound::DefaultSoundID defaultSoundID) const
{
    if (!IsKlangEngineInitialized())
        return false;

    const char *pSoundFolder = nullptr;

    // safe to cast here because we verified the defaultSoundID above
    const DefaultSoundGroupPreStep *pPreStep = static_cast<DefaultSoundGroupPreStep *>(FindDefaultSoundGroupPreStep(defaultSoundID));
    if (pPreStep)
        pSoundFolder = pPreStep->GetFolder();   // will never be nullptr here

    return pSoundFolder;
}

// Callback invoked several times per second so that all the sounds for this vessel can update their states.
//
// Params:
//   simt simulation time after the currently processed step
//   simdt length of the currently processed step
//   mjd simulation time afte the currently processed step into modified Julian Date format[days]
void VesselXRSoundEngine::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    if (IsKlangEngineInitialized())
    {
        // Update our map of all animation (door) IDs -> animation states.  This data is used by our DefaultSoundPreStep 
        // handlers to determine when a given door is changing states.  In addition, this method logs animation states to
        // XRSound.log if LogVesselAnimations is enabled.
        PollAllAnimationStates();

        // Invoke all our sound clbkPreSteps so they can each update the state of their sound for this frame.
        // This includes all our DefaultSoundPreStep objects as well as any custom ones defined by users.
        for (vector<SoundPreStep *>::iterator it = m_allSoundPreSteps.begin(); it != m_allSoundPreSteps.end(); it++)
            (*it)->clbkPreStep(simt, simdt, mjd);

        // Handle volume changes for all sounds in this engine based on camera mode, 
        // distance from camera to this vessel, atmospheric pressure, etc.
        UpdateAllSoundStates();
    }
}

// Goes through each playing sound and adjusts each sound's volume as necessary based on the camera mode, 
// the distance from the camera to this vessel (i.e., sounds generated by this vessel), and atmospheric pressure
// for external sounds.  If there is no atmosphere around this vessel, external sound volume will be zero.
// This is invoked by our clbkPreStep, so all sounds are updated based on Orbiter's state each frame.
void VesselXRSoundEngine::UpdateAllSoundStates()
{
    for (auto it = m_allWavsMap.begin(); it != m_allWavsMap.end(); it++)
        UpdateSoundState(it->second);
}

// Update the playback state & volume of a single sound based on the camera mode, 
// the distance from the camera to this vessel (i.e., the sound is generated by its vessel), and atmospheric pressure
// if the sound is external.  If there is no atmosphere around this vessel, external sound volume will be zero.
void VesselXRSoundEngine::UpdateSoundState(WavContext &context)
{
    // NOTE: If you update this method, check/update the same method in ModuleXRSoundEngine as well.

    ISound *pISound = context.pISound;  // will be nullptr if sound was never played yet, or was stopped before finishing
    if (pISound)    // sound was marked to play or is playing now?
    {
        if (!pISound->isFinished())
        {
            // sound is currently playing or paused
            if (!context.bEnabled)  // NOTE: we only use this field for *default* sounds, nothing else!
            {
                // (default!) sound is disabled, so it should not play
                StopWavImpl(&context, this);
                return;
            }

            float volume = context.volume;  // Note: context.volume has already been adjusted for MasterVolume setting in config
            switch (context.playbackType)
            {
            case XRSound::PlaybackType::InternalOnly:     // full volume, but in cockpit view only
                if (!InCockpitView())
                    volume = 0;     // should not play
                break;

            case XRSound::PlaybackType::Radio:  // full volume in both internal and external view; requires focus
                if (!HasFocus())
                    volume = 0;
                break;

            case XRSound::PlaybackType::BothViewFar:
                FadeVolumeForDistanceAndPressure(volume, 125.0);
                break;

            case XRSound::PlaybackType::BothViewMedium:
                FadeVolumeForDistanceAndPressure(volume, 50.0);
                break;

            case XRSound::PlaybackType::BothViewClose:
                FadeVolumeForDistanceAndPressure(volume, 20.0);
                break;

            case XRSound::PlaybackType::Wind:
                FadeVolumeForPressure(volume);
                break;

            case XRSound::PlaybackType::Global:
                break;      // not faded by distance or pressure, and does not require focus

            default:
                _ASSERTE(false);    // unknown sound type -- should never happen!
                goto release_sound;
            }

            // update the irrKlang state for this sound
            pISound->setVolume(volume);
            pISound->setIsLooped(context.bLoop);
            pISound->setIsPaused(context.bPaused);
        }
        else
        {
        release_sound:
            // sound has finished, so release its resources
            pISound->drop();
            context.pISound = nullptr;
        }
    }
}

// Invoked every n frames to update our map of all animation (door) IDs -> animation states in m_animIDStateMap.  
// This data is used by our DefaultSoundPreStep  handlers to determine when a given door is changing states by 
// calling m_pEngine->GetAnimationState(animID).  
//
// In addition, this method logs animation states to XRSound.log if LogVesselAnimations is enabled.
void VesselXRSoundEngine::PollAllAnimationStates()
{
    ANIMATION *pAllAnimations;
    const VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check in case Orbiter deleted it out from under us as the other checks missed it

    const int animationCount = pVessel->GetAnimPtr(&pAllAnimations);

    auto prevAnimIDStateMap = m_animIDStateMap;  // deep-clone map of values from previous frame since we will rebuild the active map below.
    m_animIDStateMap.clear();   // we rebuild this every frame

                                // loop through all animations defined for this vessel during this frame
    for (int animID = 0; animID < animationCount; animID++)
    {
        const double thisFrameProc = pAllAnimations[animID].state;      // 0..1.0

        double prevFrameProc = -1;  // no previous value
        AnimationState::StateType prevState = AnimationState::StateType::Unknown;
        const AnimationState *pPrevAnimationState = nullptr;

        auto it = prevAnimIDStateMap.find(animID);
        if (it != prevAnimIDStateMap.end())
        {
            // we have valid data from the previous frame
            pPrevAnimationState = &(it->second);
            prevFrameProc = pPrevAnimationState->Proc;      // 0..1.0
            prevState = pPrevAnimationState->State;         // "Idle", "Opening", etc.
        }

        // calculate a corresponding AnimationState based on prevFrameProc and currentFrameProc
        AnimationState::StateType state = AnimationState::StateType::Unknown;
        if ((prevFrameProc < 0) || (prevFrameProc == thisFrameProc))
            state = AnimationState::StateType::Idle;  // this is the first time we've seen this animation OR it hasn't changed
        else if (prevFrameProc == 0)
            state = AnimationState::StateType::Opening; // 0 moving up
        else if (prevFrameProc == 1.0)
            state = AnimationState::StateType::Closing; // 1.0 moving down
        else if (thisFrameProc == 0)
            state = AnimationState::StateType::Closed;  // reached 0.0
        else if (thisFrameProc == 1.0)
            state = AnimationState::StateType::Open;    // reached 1.0
        else
            state = AnimationState::StateType::Moving;  // between 0.0 and 1.0: still moving, but neither fully open nor fully closed yet

        _ASSERTE(state != AnimationState::StateType::Unknown);  // in case we screw up the above logic at some point

        // add to the map of all known animations for this vessel (we rebuild this every frame since they can change at any time).
        AnimationState animationState(state, thisFrameProc);
        m_animIDStateMap.insert(ANIMID_ANIMATIONSTATE_Pair(animID, animationState));

        // log this animation state if requested *and* if the state changed
        bool bStateTransitioned = (
            pPrevAnimationState &&   // any valid data? (if so, its State will never == Unknown)
            (pPrevAnimationState->State != state)
            );

        if (bStateTransitioned && GetConfig().LogVesselAnimations)
        {
            CString csMsg;
            csMsg.Format("  >> LogVesselAnimations: [%s][animation ID = %d, state = %s]", static_cast<const char *>(GetVesselClassName()), animID, animationState.ToStr());
            WriteLog(csMsg);
        }
    }
}

// Adjust the supplied external sound's volume for distance from Orbiter's camera, also adjusting for static pressure (i.e., atmosphere density)
//   volume: 0..1.0 volume to be adjusted
//   maxLoudnessDistance: distance in meters that the sound reaches max loudness (i.e., does not get any lounder by approaching the source).
//                        Smaller values denote a quieter sound.
void VesselXRSoundEngine::FadeVolumeForDistanceAndPressure(float &volume, const double maxLoudnessDistance)
{
    if (!InCockpitView())   // should be full volume in internal view, so don't change volume
    {
        if (InAtmosphere() || !GetConfig().SilenceOfSpace)
        {
            const float orgVolume = volume;

            // fade for distance first; the sound is 1/2 has loud for every 'maxLoudnessDistance' the observer (camera) is from the sound (ship).
            double soundDistance = GetCameraDistance();
            if (soundDistance == 0)     // sanity check to prevent division-by-zero
                soundDistance = 0.1;

            // not already at max volume for distance?
            if (soundDistance > maxLoudnessDistance)
            {
                // use inverse square law to reduce sound volume over distance:
                // intensity2 = (intensity1 x distance1^2) / distance2^2
                // intensity1 is volume (0..1.0), and distance1 is maxLoudnessDistance.  Distance2 is the distance of the camera (the "listener").
                volume = static_cast<float>((volume * (maxLoudnessDistance * maxLoudnessDistance)) / (soundDistance * soundDistance));
                if (volume > orgVolume)     // sanity check: never *increase* the volume here
                    volume = orgVolume;
            }

            const double distanceFadedVolume = volume;
            // now fade for pressure
            FadeVolumeForPressure(volume);

            // if volume falls below 4%, drop it to zero since the sound shouldn't carry any farther
            if (volume < 0.04f)
                volume = 0;

            // DEV DEBUGGING ONLY: if (IsDG()) sprintf_s(oapiDebugString(), 256, "orgVolume=%.4lf, distanceFadedVolume=%.4lf, pressureFadedVolume=%.4lf", orgVolume, distanceFadedVolume, volume);
        }
        else
            volume = 0.0f;  // in vacuum, so should be silent
    }
}

// Adjust the supplied external sound's volume for static pressure (i.e., atmosphere density) 
// UNLESS the user has `SilenceOfSpace` *disabled* via their XRSound.cfg file.
//   volume: 0..1.0 volume to be adjusted
void VesselXRSoundEngine::FadeVolumeForPressure(float &volume)
{
    if (!GetConfig().SilenceOfSpace)
        return;

    const float orgVolume = volume;

    // we assume full volume at 1 ATM static pressure
    VESSEL *pVessel = GetVessel();
    if (pVessel)
    {
        const double staticPressure = pVessel->GetAtmPressure();
        const double pressureForMaxVolume = 101325.0 / 10;   // was 1 ATM, but was then too quiet in upper atmosphere
        const double adjustedPressureFraction = staticPressure / pressureForMaxVolume;

        volume *= static_cast<float>(adjustedPressureFraction);
        if (volume > orgVolume)     //  never *increase* the volume here
            volume = orgVolume;
    }
}

// Returns distance from Orbiter's camera to our vessel (i.e., the source of the sound).
double VesselXRSoundEngine::GetCameraDistance()
{
    VESSEL *pVessel = GetVessel();
    if (pVessel == nullptr)        // vessel deleted out from under us?
        return 10.0;            // just assume the camera is close

    const VECTOR3 &zero = _V(0, 0, 0);
    VECTOR3 vesselGlobalCoords;
    pVessel->Local2Global(zero, vesselGlobalCoords);

    return dist(vesselGlobalCoords, GetCameraCoordinates());
}

// Returns true if this vessels is in an atmosphere
bool VesselXRSoundEngine::InAtmosphere()
{
    bool bInAtm = false;

    const VESSEL *pVessel = GetVessel();
    if (pVessel)
        bInAtm = (pVessel->GetAtmPressure() > 0.1);  // matches the corresponding method in the XR vessel code

    return bInAtm;
}

// Returns true if our vessel is landed; this code matches what is in the XR codebase
bool VesselXRSoundEngine::IsLanded()
{
    bool bIsLanded = false;

    const VESSEL *pVessel = GetVessel();
    if (pVessel)
    {
        // check below is for max meters-per-second the ship can be moving and still be considered wheel-stop (vessels never stop moving in Orbiter 2016 [until restart] due to core bug)
        bIsLanded = (pVessel->GroundContact() && (pVessel->GetGroundspeed() < 0.04));
        // NOTE: used to compare speed to 0, but Orbiter 2016 causes a very slight airspeed bump on startup when landed because of gear compression physics in the core
    }

    return bIsLanded;
}

// Returns true if our vessel is reentering
bool VesselXRSoundEngine::InReentry()
{
    bool bInReentry = false;

    const VESSEL *pVessel = GetVessel();
    if (pVessel)
        bInReentry = (pVessel->GetDynPressure() > 75000);    // 75 kpa in Earth ATM is where the Orbiter core starts to render plasma on the nose

    return bInReentry;
}

// Retrieves the effective "gear down" altitude; i.e., this is "altitude to touchdown".
double VesselXRSoundEngine::GetGearAdjustedAltitude()
{
    double altitude = 0;
    VESSEL *pVessel = GetVessel();
    if (pVessel)    // should never be nullptr *unless* Orbiter deleted the vessel out from under us
    {
        altitude = pVessel->GetAltitude(ALTMODE_GROUND);

        if (pVessel->GroundContact() || altitude <= 0)
        {
            altitude = 0;
        }
        else   // adjust for gear-down / hull touchdown points if we can
        {
            // A vessel's touchdown points vary based on if the gear is up or down, so we can reliably use that.
            // Checking the second  touchdown point should suffice; per Orbiter docs, the first three touchdown points defined
            // refer to the landing gear: pt1 = front, pt2 = left main, pt3 = right main.
            TOUCHDOWNVTX vtx;
            const bool bSuccess = pVessel->GetTouchdownPoint(vtx, 1);
            if (bSuccess)
            {
                // Y coordinate of the aft gear or hull touchdown point is what we're interested in here;
                // e.g., the DG has y = -2.57 meters for extended gear.
                altitude += vtx.pos.y;  // e.g., altitude += -2.57.
            }
        }
    }

    return max(altitude, 0);
}

// Returns animation (door) state of this vessel's landing gear (0..1), or -1 if none is defined or data is unavailable
double VesselXRSoundEngine::GetLandingGearAnimationState()
{
    double doorState = -1;
    VESSEL *pVessel = GetVessel();
    if (pVessel)
    {
        const int gearAnimID = GetConfig().LandingGearAnimationID;
        if (gearAnimID >= 0)
            doorState = pVessel->GetAnimation(gearAnimID);
    }
    return doorState;
}

// Returns true if this vessel has landing gear, or false if landing gear state is undefined (i.e., .cfg file for this vessel does not specify any gear)
bool VesselXRSoundEngine::HasLandingGear()
{
    return (GetConfig().LandingGearAnimationID >= 0);
}

// returns true if the ship is docked
bool VesselXRSoundEngine::IsDocked()
{
    bool bDocked = false;

    VESSEL *pVessel = GetVessel();
    if (pVessel)
        bDocked = ((pVessel->GetFlightStatus() & 0x2) != 0);

    return bDocked;
}

// returns true if the outside air temperature is in enough of an atmosphere for it to be valid (and not just be a stray particle)
bool VesselXRSoundEngine::IsOATValid()
{
    bool bValid = false;

    VESSEL *pVessel = GetVessel();
    if (pVessel)
        bValid = (pVessel->GetAtmPressure() >= OAT_VALID_STATICP_THRESHOLD);

    return bValid;
}

// load sounds global to the system; invoked during engine creation
void VesselXRSoundEngine::LoadGlobalSounds()
{
#define LOAD_GLOBAL_SOUND(soundID, pbType)  \
    if (*GetConfig().soundID) LoadWav(XRSound::soundID, GetConfig().soundID, XRSound::PlaybackType::pbType)

    // load the global sounds: (these are used by multiple SoundPreSteps)
    LOAD_GLOBAL_SOUND(SwitchOn, InternalOnly);
    LOAD_GLOBAL_SOUND(SwitchOff, InternalOnly);
}

// Load (LoadWav) all default sounds; these are present for all initial instances for XRSoundEngine.
void VesselXRSoundEngine::LoadDefaultSounds()
{
    // add default sounds
    AddDefaultSound(new AirConditioningDefaultSoundPreStep(this), XRSound::AirConditioning, GetConfig().AirConditioning, XRSound::PlaybackType::InternalOnly);
    AddDefaultSound(new LandedWindDefaultSoundPreStep(this), XRSound::LandedWind, GetConfig().LandedWind, XRSound::PlaybackType::Wind);
    AddDefaultSound(new AudioGreetingDefaultSoundPreStep(this), XRSound::AudioGreeting, GetConfig().AudioGreeting, XRSound::PlaybackType::Radio);
    AddDefaultSound(new CustomEnginesDefaultSoundPreStep(this), XRSound::CustomEngines, GetConfig().CustomEngines, XRSound::PlaybackType::BothViewFar);
    AddDefaultSound(new WheelbrakeDefaultSoundPreStep(this), XRSound::Wheekbrakes, GetConfig().Wheelbrakes, XRSound::PlaybackType::BothViewClose);
    AddDefaultSound(new DockingRadarDefaultSoundPreStep(this), XRSound::DockingRadarBeep, GetConfig().DockingRadarBeep, XRSound::PlaybackType::InternalOnly);

    AddDefaultSound(new EngineDefaultSoundPreStep(this, THGROUP_MAIN, 0.20f, 1.0f), XRSound::MainEngines, GetConfig().MainEngines, XRSound::PlaybackType::BothViewFar);
    AddDefaultSound(new EngineDefaultSoundPreStep(this, THGROUP_USER, 0.20f, 1.0f), XRSound::MainUserEngines, GetConfig().MainEngines, XRSound::PlaybackType::BothViewFar);
    AddDefaultSound(new EngineDefaultSoundPreStep(this, THGROUP_RETRO, 0.10f, 0.60f), XRSound::RetroEngines, GetConfig().RetroEngines, XRSound::PlaybackType::BothViewFar);
    AddDefaultSound(new EngineDefaultSoundPreStep(this, THGROUP_HOVER, 0.20f, 1.0f), XRSound::HoverEngines, GetConfig().HoverEngines, XRSound::PlaybackType::BothViewFar);
    AddDefaultSound(new RCSDefaultSoundPreStep(this), XRSound::RCSSustain, GetConfig().RCSSustain, XRSound::PlaybackType::BothViewMedium);

    AddSoundPreStep(new RCSModeDefaultSoundPreStep(this));
    AddSoundPreStep(new AFCtrlModeDefaultSoundPreStep(this));
    AddSoundPreStep(new LogThrusterDataPreStep(this));
    AddSoundPreStep(new TakeoffAndLandingCalloutsAndCrashPreStep(this));
    AddSoundPreStep(new FlightWindAndPlasmaSoundPreStep(this));
    AddSoundPreStep(new AutopilotOnOffSoundPreStep(this));

    // This is a special, user-requested PreStep that does not play a sound, but auto-disables Orbiter's stock autopilots above a configured threshold.  
    AddSoundPreStep(new DisableAutopilotsForTimeAccPreStep(this));


    // add default sound groups
    AddDefaultSound(new AmbientDefaultSoundGroupPreStep(this), XRSound::CabinAmbienceGroup, GetConfig().CabinAmbienceGroup, XRSound::PlaybackType::InternalOnly);
    AddDefaultSound(new ATCDefaultSoundGroupPreStep(this), XRSound::RadioATCGroup, GetConfig().ATCFolder, XRSound::PlaybackType::Radio);
    AddDefaultSound(new AltitudeCalloutsDefaultSoundGroupPreStep(this), XRSound::AltitudeCalloutsGroup, GetConfig().AltitudeCalloutsGroup, XRSound::PlaybackType::Radio);
    AddDefaultSound(new DockingCalloutsDefaultSoundGroupPreStep(this), XRSound::DockingDistanceCalloutsGroup, GetConfig().DockingDistanceCalloutsGroup, XRSound::PlaybackType::Radio);
    AddDefaultSound(new MachCalloutsDefaultSoundGroupPreStep(this), XRSound::MachCalloutsGroup, GetConfig().MachCalloutsGroup, XRSound::PlaybackType::Radio);
    AddDefaultSound(new MusicDefaultSoundGroupPreStep(this), XRSound::MusicFolder, GetConfig().MusicFolder, XRSound::PlaybackType::Radio);

    // Loop through each AnimationSounds object defined for this vessel and add an AnimationSoundPreStep for each.
    // NOTE: this includes any custom animation sounds defined by users, too, not just default animation sounds that are already in the default 
    // vessel .cfg override files.
    vector<const AnimationSounds *> allAnimationSounds = m_pConfig->GetAllAnimationSounds();
    for (unsigned int i = 0; i < allAnimationSounds.size(); i++)
    {
        // This is a borrowed reference to data that resides in the XRSoundConfigFileParser for this vessel; it remains valid until the vessel is destroyed.
        const AnimationSounds *pSounds = allAnimationSounds[i];

        // Note: all the wav files referenced in our pSounds object here were already checked when the object was created, so we don't need to validate 
        // that they exist here.
        m_allSoundPreSteps.push_back(new AnimationSoundPreStep(this, *pSounds));
    }
}

// Adds a new default sound or sound group (a subclass of DefaultSoundPreStep) to our master vector of DefaultSound objects.
//
// Returns true on success, false if the supplied pWavFilename is empty (i.e., sound is disabled) or could not be loaded.
void VesselXRSoundEngine::AddSoundPreStep(SoundPreStep *pPreStep)
{
    _ASSERTE(pPreStep);
    m_allSoundPreSteps.push_back(pPreStep);
}

// Adds a new default sound or sound group (a subclass of DefaultSoundPreStep) to our master vector of DefaultSound objects.
//   soundID: unique sound ID
//   pSoundfilename: may not be nullptr, but may be empty.  Relative to $ORBITER_ROOT.  If empty or contains the special value of "none", a message is logged and the default sound is NOT loaded.
//   playbackType: type of sound (fade, etc.)
//
// Returns true on success, false if the supplied pWavFilename is empty (i.e., sound is disabled) or could not be loaded.
bool VesselXRSoundEngine::AddDefaultSound(DefaultSoundPreStep *pPreStep, const int soundID, const char *pSoundFileOrFolderName, const XRSound::PlaybackType playbackType)
{
    _ASSERTE(pSoundFileOrFolderName);

    // see if this sound is disabled
    if (!(*pSoundFileOrFolderName) || (_stricmp(pSoundFileOrFolderName, "none") == 0))
    {
        // no sound filename set, so sound was disabled by the user in XRSound.cfg (or vessel class .cfg override)
        VERBOSE_LOG(this, "XRSoundEngine::AddDefaultSound INFO: default sound ID %d disabled via config file.", soundID);
        return false;
    }

    const bool bSuccess = pPreStep->Initialize(soundID, pSoundFileOrFolderName, playbackType);
    if (bSuccess)
        AddSoundPreStep(pPreStep);
    else
        delete pPreStep;    // can't use this sound

    return bSuccess;
}

// Free all our default sounds (sound groups, which are subclasses); invoked just before this engine is destroyed
void VesselXRSoundEngine::FreeDefaultSounds()
{
    for (vector<SoundPreStep *>::iterator it = m_allSoundPreSteps.begin(); it != m_allSoundPreSteps.end(); it++)
        delete *it;
}

// returns distance to target docking port in meters, or -1 if no port set; cloned from the XR code
double VesselXRSoundEngine::GetDockingDistance()
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)       // sanity check
        return -1;

    double distance = -1;  // assume no target

                           // obtain the global position of our docking port, if any
    DOCKHANDLE hOurDock = pVessel->GetDockHandle(0);
    if (!hOurDock)
        return -1;      // no docking port on this vessel

    VECTOR3 ourDockingPortLocalCoord;
    VECTOR3 temp;   // reused
    pVessel->GetDockParams(hOurDock, ourDockingPortLocalCoord, temp, temp);

    VECTOR3 ourPos;
    pVessel->Local2Global(ourDockingPortLocalCoord, ourPos);

    // NOTE: Orbiter does not provide a way for us to determine which NAV radio is marked "active" by the radio MFD,
    // so we have to just make a "best guess" by walking through all four of our nav radios and choosing a frequency
    // based on two criteria: 1) the closest TRANSMITTER_IDS in range, or 2) the closest TRANSMITTER_XPDR in range.
    double closestIDS = -1;      // out-of-range

                                 // find the closest TRANSMITTER_IDS values
    for (int i = 0; i < 4; i++)
    {
        NAVHANDLE hNav = pVessel->GetNavSource(i);
        if (hNav != nullptr)  // tuned and in range?
        {
            NAVDATA navdata;
            oapiGetNavData(hNav, &navdata);
            if ((navdata.type == TRANSMITTER_IDS) || (navdata.type == TRANSMITTER_XPDR))
            {
                // obtain target position (will either be the vessel itself (XPDR) or the docking port (IDS)
                VECTOR3 targetPos;
                oapiGetNavPos(hNav, &targetPos);

                // compute the distance between our docking port and the IDS or XPDR target
                VECTOR3 dp = ourPos - targetPos;       // delta position
                distance = sqrt((dp.x * dp.x) + (dp.y * dp.y) + (dp.z * dp.z));

                if (navdata.type == TRANSMITTER_IDS)
                {
                    // TODO: not sure how to deal with this with an arbitary vessel where we cannot interrogate its payload bay
                    // verify that the vessel is NOT attached in our cargo bay
                    // if ((GetXR1().m_pPayloadBay != nullptr) && !(GetXR1().m_pPayloadBay->IsChildVesselAttached(navdata.ids.hVessel)))
                    {
                        if ((closestIDS == -1) || (distance < closestIDS))
                        {
                            closestIDS = distance;     // best IDS match so far
                        }
                    }
                }
            }
        }
    }

    // if any IDS found in range, use the closest one; otherwise, use the closest XPDR 
    if (closestIDS != -1)
    {
        distance = closestIDS;
        // sprintf(oapiDebugString(), "Docking distance [IDS] = %f", distance);
    }
    /*    else if (closestXPDR != -1)
    {
    distance = closestXPDR;
    // sprintf(oapiDebugString(), "Docking distance [XPDR] = %f", distance);
    }
    */
    return distance;
}

// used to detect when Orbiter core is rendering plasma (approx.); used for certain sound volume calculations 
// Returns 0..1.0, where 0 = no plasma and 1 = max plasma.
double VesselXRSoundEngine::GetPlasmaLevel()
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)       // sanity check
        return 0;

    // This code is lifted from SetHullTempsPostStep::AddHeat in XR1PostSteps.cpp.  It does not need to be precise; it is just to get an approximate 
    // level of plasma for the vessel in at atmosphere.
    const double HULL_HEATING_FACTOR = 3.1034e-10;
    const double workingHullHeatingFactor = HULL_HEATING_FACTOR * 0.642; // tweaked very carefully…

    const double atmPressure = pVessel->GetAtmPressure();
    const double airspeed = pVessel->GetAirspeed();   // check *airspeed* here, not ground speed

    const double tweakedAtmPressure = atmPressure / 2;       // atmospheric pressure in pascals
    const double tweakedAirspeed = (airspeed*airspeed*airspeed);
    const double speedTimesPressure = tweakedAirspeed * tweakedAtmPressure;

    const double degreesK = speedTimesPressure * workingHullHeatingFactor;

    const double extTemp = GetExternalTemperature();
    const double noseconeTemp = extTemp + degreesK;

    const double noseconeTempLimit = (double)2840 + (double)273;  // convert to kelvin
    const double minVisibilityTemp = noseconeTempLimit * 0.387;  // tweaked to coincide with Orbiter visual plasma; org in XR code was 0.387
    const double maxVisibilityTemp = noseconeTempLimit * 0.80;  // was 0.80 in XR code; also tweaked to coincide with Orbiter visual plasma

                                                                // Given .....|......|... for visibility range, figure out where the nosecone temp falls in that range (could be out-of-bounds):
                                                                // noseconeTemp ^   << example value
    const double visibilityRange = maxVisibilityTemp - minVisibilityTemp;
    const double noseconeTempIntoVisibilityRange = noseconeTemp - minVisibilityTemp;
    double visibilityFrac = noseconeTempIntoVisibilityRange / visibilityRange;        // could be < 0 or > 1.0

    if (visibilityFrac < 0)
        visibilityFrac = 0;
    else if (visibilityFrac > 1.0)
        visibilityFrac = 1.0;

    // DEV DEBUGGING ONLY: if (IsXR1()) sprintf(oapiDebugString(), "noseconeTemp=%.1lf, visibilityFrac=%lf, minVisibilityTemp=%.1lf, maxVisibilityTemp=%.1lf", noseconeTemp, visibilityFrac, minVisibilityTemp, maxVisibilityTemp);

    return visibilityFrac;
}

// Returns the *effective* external temperature of the air molecules, taking static pressure into account.
// Cloned from the XR1 code.
double VesselXRSoundEngine::GetExternalTemperature()
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)       // sanity check
        return 1;       // should never happen

    /* Orbiter 2009 new atmosphere models fix: we cannot just take OAT as a baseline anymore because the temperature
    reported by the core is very high in the upper atmosphere, even though dynamic pressure is practically non-existant.
    Empirical testing with the surface MFD shows that mach and temperature are valid at DYNAMIC PRESSURE 2.78 pascals and STATIC PRESSURE of about 0.014 pascal.

    100% OAT at staticPressure >= OAT_VALID_STATICP_THRESHOLD (0.02)
    ...tapering smoothly down to...
    10% OAT at 0 kpa  (lower figure is arbitrary)
    */

    double effectiveOATFraction = (pVessel->GetAtmPressure() / 0.02);

    // keep in range
    if (effectiveOATFraction > 1.0)
        effectiveOATFraction = 1.0;   // baseline temp is never greater than OAT
    else if (effectiveOATFraction < 0.1)
        effectiveOATFraction = 0.1;   // baseline temp is never less than 10% of OAT

                                      // WARNING: THIS SHOULD BE THE *ONLY* PLACE IN THE CODE WHERE GetAtmTemperature() IS INVOKED!  All other code should invoke GetExternalTemperature() instead.
    const double extTemp = pVessel->GetAtmTemperature() * effectiveOATFraction;  // this is in Kelvin, so it is never negative
    return extTemp;
}

// Returns DefaultSoundGroupPreStep matching the supplied soundID, or nullptr if none found with that default group sound ID.
DefaultSoundGroupPreStep *VesselXRSoundEngine::FindDefaultSoundGroupPreStep(const int soundID) const
{
    DefaultSoundGroupPreStep *pFound = nullptr;

    if (IsDefaultSoundGroup(soundID))
    {
        for (vector<SoundPreStep *>::const_iterator it = m_allSoundPreSteps.begin(); it != m_allSoundPreSteps.end(); it++)
        {
            if ((*it)->GetSoundID() == soundID)
            {
                // safe to cast this because we validated that the soundID is a DefaultSoundGroup ID
                pFound = static_cast<DefaultSoundGroupPreStep *>(*it);
                break;
            }
        }
    }

    return pFound;
}
