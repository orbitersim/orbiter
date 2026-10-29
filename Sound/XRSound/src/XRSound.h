// ==============================================================
// XRSound.h : Defines the XRSound 4.1 public API.
// 
// Copyright (c) 2018-2026 Douglas Beachy and contributors
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
        RCSClosePlusX,              // 10046
        RCSClosePlusY,              // 10047
        RCSClosePlusZ,              // 10048
        RCSCloseMinusX,             // 10049
        RCSCloseMinusY,             // 10050
        RCSCloseMinusZ,             // 10051
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
    // If the same file is already loaded with this sound ID, it is not reloaded or restarted; playbackType is still applied.
    // For a sound ID that is not a default or animation sound ID, this also replaces any override set via SetPlaybackType.
    // For a default sound ID, XRSound loads its configured file only if the slot is empty. Once you load a file into that slot,
    // XRSound keeps playing your file with the supplied playback type, unless you override it via SetPlaybackType.
    //
    // Returns true on success, false if file not found or XRSound.dll not present.
    virtual bool LoadWav(const int soundID, const char *pSoundFilename, const PlaybackType playbackType) = 0;
    
    // Play the sound file with the specified ID.  If the sound is already playing, this call will only alter its loop or volume settings.
    //   soundID: vessel-instance-unique sound ID originally passed to LoadWav
    //   bLoop: true to loop sound continuously until StopWav called, false to play only once
    //   volume: 0 (muted) - 1.0 (loudest), or XRSound::UseStoredVolume (the default) to use this sound ID's stored volume.
    //
    // The stored volume starts at 1.0 and changes when you call SetVolume(soundID, volume) or pass an explicit volume to PlayWav.
    // Later PlayWav calls without a volume use that stored value. Negative volumes other than UseStoredVolume, and NaN or infinite volumes, are treated as 0.
    // This volume is further scaled by the vessel's or module's volume levels, including its main volume (see SetVolume(VolumeType, level)).
    //
    // Returns true on success, false if invalid sound ID or if XRSound.dll not present.
    virtual bool PlayWav(const int soundID, const bool bLoop = false, const float volume = UseStoredVolume) = 0;

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
    // Returns true if the enabled flag was applied, false if XRSound.dll not present or soundID is not a default sound or group.
    // Returns true even for a default sound disabled in the config file; the flag applies once a file is supplied via LoadWav.
    // MusicFolder is per vessel. The music is one track shared by all vessels and played by whichever vessel has focus, but disabling MusicFolder here turns music off only for
    // THIS vessel: if this vessel has focus, the music stops now; if another vessel has focus, its music keeps playing, and the music stops whenever focus moves to this vessel while it is disabled
    // (and starts again when focus moves to a vessel whose music is enabled).  It always returns true for MusicFolder, and it never affects another vessel's music setting.
    virtual bool SetDefaultSoundEnabled(const DefaultSoundID soundID, const bool bEnabled) = 0;

    // Returns true if the specified default sound or group is enabled, false if sound or group is disabled or XRSound.dll not present.
    // Not supported for module sounds.
    // For MusicFolder this returns THIS vessel's music on/off flag (see SetDefaultSoundEnabled); it starts out true.
    //   option: which default sound ID to check.
    virtual bool GetDefaultSoundEnabled(const DefaultSoundID soundID) const = 0;

    // Set the default subfolder path for a default sound group, relative to $ORBITER_ROOT.  This changes only this vessel's folder, for every group including MusicFolder
    // (this vessel's music plays from this vessel's own MusicFolder while it has focus); it never changes the configured folder, and it never affects another vessel.
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
    // If the sound is loaded but not currently playing, the pan is remembered and applied when PlayWav next starts it, once only.
    // Subsequent starts use the center position again.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   pan: range is from -1.0 (full left) to 1.0 (full right). Zero is the center.
    // Returns true on success, false if sound ID is invalid, pan is out of range, or the sound engine is unavailable.
    // Note: for MusicFolder (the one music track shared by all vessels) this works only from the vessel that has focus and only while the music is playing; otherwise it returns false, and
    // no pan is remembered for a later track (so nothing one vessel sets can carry into a track that another vessel starts).
    virtual bool SetPan(const int soundID, const float pan) = 0;

    // Returns the pan of the sound with the specified ID, from -1.0 (full left) to 1.0 (full right). Zero is the center.
    // Note: the sound must have already started playing via PlayWav.  A pan remembered by SetPan for a sound that is not playing yet is not
    // readable here: this still returns -100 until the sound is playing.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   pan: range is from -1.0 (full left) to 1.0 (full right). Zero is the center.
    // 
    // Returns -100 if sound ID is invalid or is not currently playing.
    virtual float GetPan(const int soundID) = 0;

    // Sets the playback speed of the sound with the specified ID.
    // If the sound is loaded but not currently playing, the speed is remembered and applied when PlayWav next starts it, once only.
    // Subsequent starts use speed 1.0 again.
    // Plays the sound at a higher or lower speed, increasing or decreasing its frequency, which makes it sound lower or higher.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   speed: factor of the speed increase or decrease; 2 is twice as fast, 0.5 is only half as fast. The default is 1.0.
    //
    // Returns true on success, false if sound ID is invalid or the sound engine is unavailable.
    // For a sound that is not playing, speed must be greater than zero.
    // Note: for MusicFolder (the one music track shared by all vessels) this works only from the vessel that has focus and only while the music is playing; otherwise it returns false, and
    // no speed is remembered for a later track.
    virtual bool SetPlaybackSpeed(const int soundID, const float speed = 1.0) = 0;

    // Returns the playback speed of the sound with the specified ID.
    // Note: the sound must have already started playing via PlayWav.  A speed remembered by SetPlaybackSpeed for a sound that is not
    // playing yet is not readable here: this still returns 0 until the sound is playing.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    // 
    // Returns factor of the speed increase or decrease; 2 is twice as fast, 0.5 is only half as fast. The default is 1.0.
    // Returns 0 if sound ID is invalid or is not currently playing.
    virtual float GetPlaybackSpeed(const int soundID) = 0;

    // Sets the playback position of the sound with the specified ID.
    // If the sound is loaded but not currently playing, the position is remembered and applied when PlayWav next starts it, once only.
    // Subsequent starts begin at the start of the file again. If the remembered seek fails, the sound plays from the beginning.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    //   positionMillis: must be between zero (the start of the file) and the length of the file, in milliseconds
    //
    // Returns true on success, false if positionMillis or sound ID is invalid, or the sound engine is unavailable.
    // When the sound is not currently playing, positionMillis is checked against the sound's length only if GetDuration has already measured it.
    // Note: for MusicFolder (the one music track shared by all vessels) this works only from the vessel that has focus and only while the music is playing; otherwise it returns false, and
    // no position is remembered for a later track.
    virtual bool SetPlayPosition(const int soundID, const unsigned int positionMillis) = 0;

    // Returns the playback position of the sound in milliseconds with the specified ID.
    // Note: the sound must have already started playing via PlayWav.  A position remembered by SetPlayPosition for a sound that is not
    // playing yet is not readable here: this still returns a value < 0 until the sound is playing.
    //   soundID: unique sound ID originally passed to LoadWav and PlayWav
    // 
    // Returns between zero (the start of the file) and the length of the file, in milliseconds.
    // Returns < 0 if sound ID is invalid or is not currently playing.
    virtual int GetPlayPosition(const int soundID) = 0;

    // ===========================================================================================================================
    // Volume levels and runtime settings
    //
    // General rules for the calls below:
    //   * Unavailable calls return false and leave output parameters unchanged. XRSound.dll must be present and its sound engine must be running.
    //   * Every vessel proxy and every module proxy has its OWN active level for every VolumeType and its OWN active value for every Setting.  They are initialized from the
    //     configuration (XRSound.cfg and, for a vessel, its class override file) when the proxy's engine is created.  The calls below get and set only the calling proxy's
    //     own active values: they never change the configured defaults, never write to any XRSound configuration file, and never affect another vessel or module.
    //     The next session (i.e., after the user returns to the Orbiter launchpad and starts again) starts again from the configured values.
    //   * A setter that is given an out-of-range or wrong-type value returns false and leaves the current value unchanged.
    //   * A "changes immediately" setter also changes sounds that are already playing; the sound is never restarted, paused, or repositioned.
    // ===========================================================================================================================

    // Identifies a volume level that you can get via GetVolume(VolumeType, level) and set via SetVolume(VolumeType, level).  Every vessel or module proxy has its own
    // active level for each type, as an absolute level from 0 to 1.0 (not a multiplier on the configured level); setting it changes only this proxy.
    // A sound's final volume in a proxy is the volume the sound is played at x its item level (see SetVolume(soundID, volume)) x the proxy's level for the sound's type x the proxy's
    // Main level, before XRSound's distance, pressure, and view fades.
    enum class VolumeType
    {
        Main = 1,        // This proxy's overall level: scales everything this vessel or module plays, including the music it plays while its vessel has focus.  Starts at the configured MasterVolume value (XRSound.cfg's key is still named MasterVolume).
        Engines,         // This proxy's engine sounds: MainEngines, RetroEngines, HoverEngines, and CustomEngines.  Starts at 1.0.
        RCS,             // This proxy's RCS sounds: RCSSustain and all RCSAttack*/RCSClose* sounds.  Starts at 1.0.
        Effects,         // This proxy's sound effects: LandedWind, SwitchOn, SwitchOff, Crash, MetalCrunch, WheelChirp, TiresRolling, Docking, Undocking, Wheekbrakes,
                         // DockingRadarBeep, FlightWind, ReentryPlasma, SonicBoom, AutopilotOn, AutopilotOff, and the vessel's animation (door) sounds.  Starts at 1.0.
        Callouts,        // This proxy's spoken callouts: AudioGreeting, Rotation, Translation, Off, AFPitch, AFOn, AFOff, Touchdown, WheelStop, OneHundredKnots, Liftoff,
                         // WarningGearIsUp, YouAreClearedToLand, DockingCallout, UndockingCallout, SubsonicCallout, MachCalloutsGroup, AltitudeCalloutsGroup, DockingDistanceCalloutsGroup.  Starts at 1.0.
        CabinAmbience,   // This proxy's cabin ambience: CabinAmbienceGroup and AirConditioning.  Starts at 1.0.
        ATC,             // This proxy's ATC chatter (RadioATCGroup).  Starts at the configured ATC Volume value.
        Music,           // The music level used while this proxy's vessel has focus (music is one shared track that plays with the levels of the vessel that has focus; a vessel changes only its own
                         // Music level).  Starts at the configured MusicVolume value.  A module proxy accepts and stores this level, but it has no effect for a module (modules do not play music).
        Custom           // The sounds that this proxy's client code loaded with its own sound IDs (i.e., not a default sound ID, group ID, or animation sound ID).  Starts at 1.0.
    };

    // Identifies a setting that you can get via GetSetting and set via SetSetting.  Each corresponds to a setting in XRSound.cfg
    // (or a vessel class override file) of the same name, with the same valid values.  EVERY setting is per proxy: a vessel or module proxy has its own active value for each one,
    // initialized from the configuration when its engine is created (for a vessel, including its class override file where that file is allowed to set it), and
    // setting it changes only the calling vessel or module.  This is true even for the settings that XRSound.cfg keeps in its [SYSTEM] section (they are global in the
    // configuration file, but a running vessel or module changes only its own copy).  A module proxy accepts and stores every setting too, but the vessel-related ones
    // (everything not in the [SYSTEM] group below) have no effect for a module because nothing in a module reads them.
    //
    // Numeric settings use the double form of SetSetting/GetSetting: a bool setting is 0 (false) or 1 (true) (any other value fails); an integer setting
    // fails if given a non-integer value; MusicOrder and MusicPlayInternal/MusicPlayExternal use the integer value of the XRSound::MusicOrder and XRSound::MusicPlay enums.
    // CustomEnginesThrusterIndexes is a string setting and uses the const char * form; each form fails for the other form's settings.
    enum class Setting
    {
        // ----- settings from the [SYSTEM] section of XRSound.cfg (per proxy at runtime; see above) -----
        SilenceOfSpace = 1,                 // bool.  Takes effect at this vessel's next XRSound update (within its UpdateInterval).
        MusicOrder,                         // XRSound::MusicOrder (0 or 1).  Used when this vessel's music selects a track: takes effect at the next track selection; the current track keeps playing, and Sequential continues from its current position.
        UpdateInterval,                     // double seconds, 0.02 to 1.0.  How often XRSound updates this vessel's sounds; takes effect at this vessel's next refresh.  Has no effect for a module proxy (modules have no periodic update).
        EnableVerboseLogging,               // bool.  Verbose XRSound.log lines for this vessel or module only.  Takes effect immediately.
        LogVesselAnimations,                // bool.  Takes effect immediately.
        LogThrusterData,                    // bool.  Takes effect immediately.
        DisableAutopilotsTimeAccThreshold,  // double, 0 to 10000.  Takes effect at this vessel's next update.
        MinThrusterLevelForRCSSoundEffects, // double, 0 to 1.0.  Takes effect at this vessel's next update.

        // ----- settings from the [SOUND_FILES], [ATC], [VESSEL], and [animation_*] sections of XRSound.cfg or a vessel class override file -----
        ATCMinDelay,                        // integer seconds, 10 to 900.  Reschedules this vessel's next ATC clip now with the new range (measured from now); a clip already playing finishes.
        ATCMaxDelay,                        // integer seconds, 10 to 900.  Same as ATCMinDelay.  (A maximum below the minimum is handled the same way as in the configuration file.)
        ATCAllowWhileLanded,                // bool.  Checked when the next ATC clip is due; a clip already playing finishes.
        ATCAllowInAtmosphere,               // bool.  Same as ATCAllowWhileLanded.
        ATCAllowDuringReentry,              // bool.  Same as ATCAllowWhileLanded.
        ATCDelayPlanetDistance,             // double km, 0 to 1e6.  Reschedules this vessel's next ATC clip now.
        ATCDelayPlanetMultiplier,           // double, 1 to 100.  Reschedules this vessel's next ATC clip now.
        CabinAmbienceMin,                   // integer seconds, 7 to 300.  Reschedules this vessel's next cabin-ambience clip now (measured from now).
        CabinAmbienceMax,                   // integer seconds, 7 to 300.  Same as CabinAmbienceMin.
        MusicPlayInternal,                  // XRSound::MusicPlay (0 to 2).  Music follows the focus vessel's values: takes effect at the next update, when the current track pauses or resumes under the new rule.
        MusicPlayExternal,                  // XRSound::MusicPlay (0 to 2).  Same as MusicPlayInternal.
        WarningGearIsUpAltitude,            // double meters, 0 to 1000.  Takes effect at the next update.
        CustomEnginesThrusterIndexes,       // string: space-separated non-negative integers (e.g., "0 1 4"), or an empty string for "no custom-engine thrusters" (the configured value for a vessel with no custom engines is empty).  Takes effect at the next update.
        LandingGearAnimationID              // integer: -1 (no landing gear) or an animation ID >= 0.  Takes effect at the next update.
    };

    // Values for the MusicOrder setting (SetSetting/GetSetting with Setting::MusicOrder).
    enum class MusicOrder { Sequential = 0, Random = 1 };

    // Values for the MusicPlayInternal and MusicPlayExternal settings: Off = never play music, Space = play music only when in space (i.e., not in an atmosphere), On = always play music.
    enum class MusicPlay { Off = 0, Space = 1, On = 2 };

    // PlayWav's default volume: play at the volume stored for the sound ID (see PlayWav).  The value is exactly -1.0f.
    static constexpr float UseStoredVolume = -1.0f;

    // Sets one of THIS proxy's active volume levels (see VolumeType).  This changes only the calling vessel or module: not the configured default, and not any other vessel or module.
    //   type: which volume level to change (see VolumeType for what each one covers and where it starts)
    //   level: the new absolute level, from 0 (muted) to 1.0 (full), inclusive.  (It replaces the current level; it is not a multiplier on the configured level.)
    // Takes effect immediately, including for sounds already playing.  The next session starts again from the configured values.
    // Returns true on success, false if level is out of range (or not a number) or type is not valid.
    virtual bool SetVolume(const VolumeType type, const float level) = 0;

    // Gets one of THIS proxy's active volume levels (see VolumeType): the value it started with from the configuration, or the last value set via SetVolume(VolumeType, level).
    //   type: which volume level to read
    //   level: receives the current level (0..1.0)
    // Returns true on success, false if type is not valid. On false, level is unchanged.
    virtual bool GetVolume(const VolumeType type, float &level) const = 0;

    // Sets the volume of one sound.  This is a different overload than SetVolume(VolumeType, level): a plain sound ID (including a DefaultSoundID) selects this one.
    //   soundID: ID of a sound that you loaded with LoadWav, or a default sound ID, a default group sound ID (including MusicFolder), or an animation sound ID for this vessel.
    //   volume: 0 (muted) to 1.0, inclusive.
    // For a sound that you loaded: this sets the volume that PlayWav plays at when you call it without a volume (or with UseStoredVolume), and if the sound is currently playing or paused,
    // the change takes effect at once.  A newly loaded sound starts at 1.0, and loading a different file into the same sound ID resets it to 1.0.
    // For a default sound, default group sound, or animation sound: XRSound sets those sounds' playback volume itself (e.g., by thrust level; for an animation sound, the animation's configured Volume, 1.0 if not set), so this sets an "item level" multiplier
    // that scales XRSound's automatic volume instead (default 1.0); it takes effect immediately and may be set before the sound has been loaded.  The item level is kept when the
    // sound's file is replaced via LoadWav.  This changes only this vessel or module: the item level for MusicFolder is this vessel's own and applies while this vessel has focus and plays the music.
    // You can use this for default sounds too, even though you cannot play or stop them yourself.
    // Returns true on success, false if volume is out of range (or not a number) or soundID is not a loaded sound (and is not a default or animation sound ID).
    virtual bool SetVolume(const int soundID, const float volume) = 0;

    // Gets the volume set via SetVolume(soundID, volume) or PlayWav: for a sound that you loaded, the volume stored for the sound (before the VolumeType levels, the main volume, distance, and
    // atmosphere adjustments are applied); for a default sound, default group sound, or animation sound, its item level.
    // Returns true on success, false if soundID is not a loaded sound (and is not a default or animation sound ID). On false, volume is unchanged.
    virtual bool GetVolume(const int soundID, float &volume) const = 0;

    // Overrides the playback type of one sound, which controls how the sound is faded (see PlaybackType).
    //   soundID: ID of a loaded sound, a default sound ID, a default group sound ID, or an animation sound ID for this vessel.
    //   playbackType: the new playback type
    // Takes effect immediately for a sound that is playing; it never restarts, reloads, or repositions the sound.  For a default or animation sound, this may be set before the sound has been loaded, and it
    // is kept when XRSound reloads the sound's file.  For any other sound, a later LoadWav of that sound ID replaces this override with the playback type that you pass to LoadWav.
    // Note: sounds played by a *module* proxy always play as PlaybackType::Global, so for module sounds the type is stored but has no audible effect.
    // Returns true on success, false if playbackType is not valid or soundID is not a loaded sound (and is not a default or animation sound ID).
    virtual bool SetPlaybackType(const int soundID, const PlaybackType playbackType) = 0;

    // Gets the playback type currently in effect for one sound (the override set via SetPlaybackType if any, otherwise the type most recently passed to LoadWav).
    // Returns true on success, false if soundID is not a loaded sound (and has no override). On false, playbackType is unchanged.
    virtual bool GetPlaybackType(const int soundID, PlaybackType &playbackType) const = 0;

    // Gets the length of a loaded sound's file, in milliseconds, without playing it.
    // Note: the first call for a sound measures the file, which can take a moment for a long compressed file (e.g., a long .mp3); the result is then remembered until a different file is loaded into this sound ID.
    //   soundID: ID of a loaded sound (including default sounds and the current MusicFolder track)
    //   durationMillis: receives the length in milliseconds
    // Returns true on success, false if soundID is not a loaded sound, the file could not be read, or its length is unknown or zero. On false, durationMillis is unchanged.
    virtual bool GetDuration(const int soundID, unsigned int &durationMillis) = 0;

    // Changes a numeric (or enum or bool) setting for THIS vessel or module only (never the configured default, and never another vessel or module).  See the Setting enum for each
    // setting's type, valid values, and when the change takes effect.  The next session starts again from the configured values.
    //   setting: which setting to change; must be a numeric setting
    //   value: the new value; bools are 0 or 1, and enum settings use the integer value of the enum (see Setting)
    // Returns true on success, false if setting is not a numeric setting or value is invalid for that setting.
    virtual bool SetSetting(const Setting setting, const double value) = 0;

    // Gets the current value of a numeric (or enum or bool) setting for THIS vessel or module: the value it started with from the configuration, or the last value set via SetSetting.
    // Returns true on success, false if setting is not a numeric setting. On false, value is unchanged.
    virtual bool GetSetting(const Setting setting, double &value) const = 0;

    // Changes CustomEnginesThrusterIndexes for THIS vessel or module only.  See the Setting enum for the valid values and effect.
    //   setting: which setting to change; must be a string setting
    //   pValue: the new value; may not be nullptr.  It may be empty to mean no custom-engine thrusters (what the configuration holds for a vessel with no custom engines).  It is copied: you keep ownership of the string.
    // Returns true on success, false if setting is not a string setting or pValue is nullptr or invalid.
    virtual bool SetSetting(const Setting setting, const char *pValue) = 0;

    // Gets the current value of a string setting for THIS vessel or module; see SetSetting.
    //   pBuffer: buffer you own that receives the value as a null-terminated string
    //   bufferLength: size of pBuffer in bytes, including the terminating null
    // Returns true on success, false if setting is not a string setting, pBuffer is nullptr, or bufferLength cannot hold the value and its terminating null.
    // Nothing is copied on failure.
    virtual bool GetSetting(const Setting setting, char *pBuffer, const int bufferLength) const = 0;

    // Gets the configured starting level of one of THIS proxy's volume levels (see VolumeType): the value that GetVolume(VolumeType, level) returned before any SetVolume call.
    // These are the configured values for this vessel or module, from XRSound.cfg (plus the vessel class's override file, for a vessel) (Main = MasterVolume, ATC = ATC Volume, Music = MusicVolume,
    // and every other type 1.0).  They never change during the session and are read-only: SetVolume does not change them, and nothing at runtime can.
    // Example uses: an XR ship's "100% = configured level" volume slider, or a "reset to default" button (SetVolume(type, defaultLevel)).
    //   type: which volume level to read the default of
    //   level: receives the configured starting level (0..1.0)
    // Returns true on success, false if type is not valid. On false, level is unchanged.
    virtual bool GetDefaultVolume(const VolumeType type, float &level) const = 0;

    // Gets the configured starting value of a numeric (or enum or bool) setting for THIS vessel or module: the value that GetSetting(setting, value) returned before any SetSetting call.
    // This is the configured value from XRSound.cfg (plus the vessel class's override file, for a vessel); it never changes during the session and is read-only (SetSetting does not change it).
    // Example use: a "reset to default" button (SetSetting(setting, defaultValue)).  Same value forms as GetSetting (bools are 0 or 1, enum settings use the enum's integer value).
    // Returns true on success, false if setting is not a numeric setting. On false, value is unchanged.
    virtual bool GetDefaultSetting(const Setting setting, double &value) const = 0;

    // Gets the configured starting value of CustomEnginesThrusterIndexes for THIS vessel or module; see GetDefaultSetting above.
    //   pBuffer: buffer you own that receives the value as a null-terminated string
    //   bufferLength: size of pBuffer in bytes, including the terminating null
    // Returns true on success, false if setting is not a string setting, pBuffer is nullptr, or bufferLength cannot hold the value and its terminating null.
    // Nothing is copied on failure.
    virtual bool GetDefaultSetting(const Setting setting, char *pBuffer, const int bufferLength) const = 0;

    // Convenience overload so that SetSetting(setting, 0), SetSetting(setting, 5), and SetSetting(setting, true) are not ambiguous between the double and const char * overloads
    // (an integer 0 would otherwise convert to a null const char *).  It is the same as passing the value as a double.  Inline only because it forwards to the double form; this allocates nothing.
    bool SetSetting(const Setting setting, const int value) { return SetSetting(setting, static_cast<double>(value)); }

    // ---------------------------------------------------------------------------------------------------------------------------
    // Settings covered by existing calls (they change only this vessel or module, never the configuration or any other vessel or module):
    //   * Default sound group folders (cabin ambience, ATC, callouts, and music): SetDefaultSoundGroupFolder, which changes only this vessel's folder for every group, MusicFolder included.
    //   * Default sound files (and the CustomEngines sound file): LoadWav with the default sound ID.
    //   * Enabling or disabling default sounds: SetDefaultSoundEnabled.
    //   * The MasterVolume, MusicVolume, and ATC Volume configuration settings: the Main, Music, and ATC levels of SetVolume(VolumeType, level), which start at those configured values.
    // ---------------------------------------------------------------------------------------------------------------------------

    virtual ~XRSound() { };

protected:
    // this has protected access to prevent incorrect instantiation by vessel code: always use the static XRSound::CreateInstance method to create an instance of XRSound
    XRSound() { }
};

