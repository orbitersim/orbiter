// ==============================================================
// XRSound.h : Defines the XRSound 2.0 public API.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

class VESSEL;    // defined in Orbiter.h

class XRSound
{
public:
    // determines how a given sound will be played back (i.e., where it will be audible)
    // Note: if the `SilenceOfSpace` configuration setting is disabled via the user's XRSound.cfg, sounds will not be faded by pressure.
    enum class PlaybackType
    {
        InternalOnly = 1, // 1: In cockpit view only, always full volume.  Implies the vessel has focus, as well.
        BothViewFar,      // 2: Full volume in internal view, faded (less) by distance and pressure in external view (this will be silent in vacuum).  Vessel does not need focus.
        BothViewMedium,   // 3: Full volume in internal view, faded (middle) by distance and pressure in external view (this will be silent in vacuum).  Vessel does not need focus.
        BothViewClose,    // 4: Full volume in internal view, faded (more) by distance and pressure in external view (this will be silent in vacuum).  Vessel does not need focus.
        Radio,            // 5: Full volume in both internal and external view; vessel must have focus.
        Wind,             // 6: External view only, faded by static pressure but not distance.
        Global            // 7: Full volume in both internal and external views, not faded, and does *not* require focus.  This is designed for cross-vessel sounds like music playback; normally, vessels should not use this playback type.
    };

    // Defines vessel-unique sound IDs for each default sound; you can disable, enable, or change each of these default
    // sounds either by editing your XRSound.cfg file or editing your vessel's XRSound-<vesselClass>.cfg file.
    // You can also replace or disable/re-enable any default sound by calling SetDefaultSoundEnabled(...) from your
    // custom vessel code, and you can call LoadWav with any default sound ID here to replace a default sound.
    //
    // Refer to each sound ID's comments in $ORBITER_ROOT\XRSound\XRSound.cfg for details about each sound.
    //
    // Note: when you are writing vessel code, do not use any custom sound IDs that are >= 10000 and < 12000; they are reserved for use (and future use) by XRSound's default sounds.
    // Note: we cannot make this enum a class because it must be converted to/from integer sound IDs.
    enum DefaultSoundID
    {
        AirConditioning = 10000,    // 10000
        LandedWind,                 // 10001
        AudioGreeting,              // 10002
        MainEngines = 10004,        // 10004
        RetroEngines,               // 10005
        HoverEngines,               // 10006
        RCSSustain = 10008,         // 10008
        SwitchOn,                   // 10009
        SwitchOff,                  // 10010
        Rotation,                   // 10011
        Translation,                // 10012
        Off,                        // 10013
        CustomEngines,              // 10014
        AFPitch,                    // 10015
        AFOn,                       // 10016
        AFOff,                      // 10017
        Crash,                      // 10018
        MetalCrunch,                // 10019
        WheelChirp,                 // 10020
        Touchdown,                  // 10021
        WheelStop,                  // 10022
        TiresRolling,               // 10023
        OneHundredKnots,            // 10024
        Liftoff,                    // 10025
        WarningGearIsUp,            // 10026
        YouAreClearedToLand,        // 10027
        Docking,                    // 10028
        DockingCallout,             // 10029
        Undocking,                  // 10030
        UndockingCallout,           // 10031
        Wheekbrakes,                // 10032
        DockingRadarBeep,           // 10033
        FlightWind,                 // 10034
        ReentryPlasma,              // 10035
        SonicBoom,                  // 10036
        AutopilotOn,                // 10037
        AutopilotOff,               // 10038
        SubsonicCallout,            // 10039
        RCSAttackPlusX,             // 10040
        RCSAttackPlusY,             // 10041
        RCSAttackPlusZ,             // 10042
        RCSAttackMinusX,            // 10043
        RCSAttackMinusY,            // 10044
        RCSAttackMinusZ,            // 10045
        LastDefaultSound,

        // these are sound IDs that play from a group of files, each in a configured folder
        RadioATCGroup = 11000,          // 11000
        CabinAmbienceGroup,             // 11001
        MachCalloutsGroup,              // 11002
        AltitudeCalloutsGroup,          // 11003
        DockingDistanceCalloutsGroup,   // 11004
        MusicFolder,                    // 11005; this is a special, global (i.e., vessel-independent) sound ID.
        LastDefaultGroup
    };

    // Create an instance of an XRSound proxy object for the supplied vessel; invoke this from your vessel's clbkPostCreation method.
    // Normally you will only invoke this method from your own vessel's code; however, If you call this method from a *module*, 
    // you will be able to use the returned proxy object to control the sounds for the supplied vessel.
    //   pVessel: Orbiter vessel to which this XRSound proxy object is tied
    //
    // Returns: instance of XRSound proxy to use with the specified pVessel.
    // NOTE: do not forget to delete the returned proxy object when you no longer need it (e.g., in your vessel's destructor).
    static XRSound *CreateInstance(VESSEL *pVessel);

    // Create an instance of an XRSound proxy object for the named *module*; invoke this from your module's clbkSimulationStart method.
    //   pUniqueModuleName: arbitrary name uniquely identifying the calling module; typically, this should match the filename of your Orbiter module DLL, excluding the ".dll" extension.
    //
    // Returns: instance of XRSound proxy to use with the specified pUniqueModuleName.
    // NOTE: do not forget to delete the returned proxy object when you no longer need it (e.g., in your module's clbkSimulationEnd method).
    static XRSound *CreateInstance(const char *pUniqueModuleName);

    // Returns true if XRSound.dll is present, false if not.
    virtual bool IsPresent() const = 0;

    // Returns the version of XRSound.dll, or 0 if DLL not present.
    virtual float GetVersion() const = 0;

    // Loads the specified sound file and assigns the supplied sound ID to it.  Note: it does not need to be a .wav file: it may be any supported sound file format
    // supported by XRSound.  Note that this call is lightweight and does not actually load the sound file data into memory: 
    // the sound data is not loaded until it is needed by PlayWav later.
    // If you load a different sound file with the same ID as an existing loaded sound file, the previous sound file is
    // automatically stopped if it is playing and replaced by this sound.
    //   soundID: sound ID to be assigned to this sound file (vessel-instance unique)
    //   pSoundFilename: path relatve to $ORBITER_ROOT of sound file to load; may not be nullptr or empty.
    //   playbackType: denotes how sound will be faded.  Note: for module sounds, you should pass XRSound::PlaybackType::Global for this since other values have no effect when playing sounds from an Orbiter module (as opposed to an Orbiter vessel).
    //
    // Returns true on success, false if file not found or XRSound.dll not present.
    virtual bool LoadWav(const int soundID, const char *pSoundFilename, const PlaybackType playbackType) = 0;
    
    // Play the sound file with the specified ID.  If the sound is already playing, this call will only alter its loop or volume settings.
    //   soundID: vessel-instance-unique sound ID originally passed to LoadWav
    //   bLoop: true to loop sound continuously until StopWav called, false to play only once
    //   volume: 0 (muted) - 1.0 (loudest)
    //
    // Returns true on success, false if invalid sound ID or if XRSound.dll not present.
    virtual bool PlayWav(const int soundID, const bool bLoop = false, const float volume = 1.0) = 0;

    // Stop playing the sound file with the specified ID.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    // Returns true on success, false if invalid sound ID or if XRSound.dll not present.
    virtual bool StopWav(const int soundID) = 0;

    // Returns false if the specified sound is not playing or XRSound.dll is not present.
    virtual bool IsWavPlaying(const int soundID) const = 0;

    // Pause or unpause a sound.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   bPause: true to pause sound, false to unpause it
    // Returns true on success, false if invalid sound ID or XRSound.dll not present.
    virtual bool SetPaused(const int soundID, const bool bPause) = 0;

    // Detect if a sound is paused.
    //   soundID: unique sound ID originally passed to LoadWav.
    // Returns true if sound is paused, or false if not paused, invalid sound ID, or XRSound.dll not present.
    virtual bool IsPaused(const int soundID) const = 0;

    // Enable or disable a default sound or group.  This can also be done globally via the configuration file; however, this option 
    // is useful if you want to disable some sound effects at runtime for a specific vessel class.
    // Not supported for module sounds.
    //   soundID: which default sound to enable or disable.
    //   bEnabled: true to enable default sound, false to disable it
    // Returns true on success, false if XRSound.dll not present or this default sound is disabled via config file (i.e., no sound file specified for this).
    virtual bool SetDefaultSoundEnabled(const DefaultSoundID soundID, const bool bEnabled) = 0;

    // Returns true if the specified default sound or group is enabled, false if sound or group is disabled or XRSound.dll not present.
    // Not supported for module sounds.
    //   option: which default sound ID to check.
    virtual bool GetDefaultSoundEnabled(const DefaultSoundID soundID) const = 0;

    // Set the default subfolder path for a default sound group, relative to $ORBITER_ROOT.
    // Not supported for module sounds.
    //   defaultSoundID: which default XRSound group to update (only DefaultSoundIDs that end in "Group" are valid for this call).
    //   pSubfolderPath: subfolder path relative to $ORBITER_ROOT; may not be nullptr or empty.
    // Returns true on success, false if defaultSoundID is not a valid default group sound ID, no default sounds loaded for the supplied defaultSoundID, or XRSound.dll not present.
    virtual bool SetDefaultSoundGroupFolder(const DefaultSoundID defaultSoundID, const char *pSubfolderPath) = 0;

    // Returns the default subfolder path for a default sound group, relative to $ORBITER_ROOT, 
    // or nullptr if no default sounds loaded for the supplied defaultSoundID or XRSoundDLL not present.
    // Not supported for module sounds.
    //   groupdefaultSoundID: which default XRSound group to update (only DefaultSoundIDs that end in "Group" are valid for this call).
    virtual const char *GetDefaultSoundGroupFolder(const DefaultSoundID defaultSoundID) const = 0;

    // Sets the pan (left/right balance) of the sound with the specified  ID.
    // Note: the sound must have already started playing via PlayWav for this to have any effect.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   pan: range is from -1.0 (full left) to 1.0 (full right). Zero is the center.
    // Returns true on  success, false if sound ID is invalid or is not currently playing.
    virtual bool SetPan(const int soundID, const float pan) = 0;

    // Returns the pan of the sound with the specified ID, from -1.0 (full left) to 1.0 (full right). Zero is the center.
    // Note: the sound must have already started playing via PlayWav.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   pan: range is from -1.0 (full left) to 1.0 (full right). Zero is the center.
    // 
    // Returns -100 if sound ID is invalid or is not currently playing.
    virtual float GetPan(const int soundID) = 0;

    // Sets the playback speed of the sound with the specified ID.
    // Note: the sound must have already started playing via PlayWav for this to have any effect.
    // Plays the sound at a higher or lower speed, increasing or decreasing its frequency, which makes it sound lower or higher.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   speed: factor of the speed increase or decrease; 2 is twice as fast, 0.5 is only half as fast. The default is 1.0.
    // 
    // Returns true on success, false if sound ID is invalid or is not currently playing.
    virtual bool SetPlaybackSpeed(const int soundID, const float speed = 1.0) = 0;

    // Returns the playback speed of the sound with the specified ID.
    // Note: the sound must have already started playing via PlayWav.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    // 
    // Returns factor of the speed increase or decrease; 2 is twice as fast, 0.5 is only half as fast. The default is 1.0.
    // Returns 0 if sound ID is invalid or is not currently playing.
    virtual float GetPlaybackSpeed(const int soundID) = 0;

    // Sets the playback position of the sound with the specified ID.
    // Note: the sound must have already started playing via PlayWav for this to have any effect.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   positionMillis: must be between zero (the start of the file) and the length of the file, in milliseconds
    // 
    // Returns true on success, false if positionMillis is invalid or if sound ID is invalid or is not currently playing.
    virtual bool SetPlayPosition(const int soundID, const unsigned int positionMillis) = 0;

    // Returns the playback position of the sound in milliseconds with the specified ID.
    // Note: the sound must have already started playing via PlayWav.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    // 
    // Returns between zero (the start of the file) and the length of the file, in milliseconds.
    // Returns < 0 if sound ID is invalid or is not currently playing.
    virtual int GetPlayPosition(const int soundID) = 0;

    virtual ~XRSound() { };

protected:
    // this has protected access to prevent incorrect instantiation by vessel code: always use the static XRSound::CreateInstance method to create an instance of XRSound
    XRSound() { }
};
