// ==============================================================
// XRSoundConfigFileParser.h
// Parses XRSound.cfg
//
// Copyright (c) 2018-2026 Douglas Beachy and contributors
// Licensed under the MIT License
// ==============================================================

#pragma once

#include <vector>
#include "ConfigFileParser.h"
#include "Orbitersdk.h"   // for VESSEL
#include "AnimationState.h"
#include "XRSound.h"    // for PlaybackType
#include <unordered_map>
#include <sstream>

static const char *XRSOUND_CONFIG_FILE = "XRSound\\XRSound.cfg";

// XRSound.log always resides in the Orbiter root folder, alongside Orbiter.log and the XR vessel log files
static const char *XRSOUND_LOG_FILE = "XRSound.log";

// A vessel has multiple animation IDs, and each animation has multiple sounds.

// Defines map of animation state -> wav file path.
// key = animation state, value = path to wav file for that state
typedef std::unordered_map<AnimationState::StateType, std::string> HASHMAP_ANIMATIONSTATE_WAVFILEPATH;
typedef std::pair<AnimationState::StateType, std::string> animationState_wavFilePath_Pair;

// Class encapsulating a single vessel animation's sounds.  So if a vessel defines custom 
// sounds for four animations, there will be four instances of VesselAnimationSounds, too.
// This class can safely be copied by value.
// Also, the AnimationSoundPreStep class maintains a borrowed reference to this class, so any changes 
// made to these values via code while the simulation is running will take effect immediately.
class AnimationSounds
{
public:
    AnimationSounds(const int animationID);
    int GetAnimationID() const { return m_animationID; }

    void SetPlaybackType(XRSound::PlaybackType playbackType) { m_playbackType = playbackType; }
    XRSound::PlaybackType GetPlaybackType() const { return m_playbackType; }

    // Volume this animation's sounds (both the open/close sound and the looping moving sound) are played at: 0 (silent) to 1.0 (full, the default).
    // This is XRSound's automatic volume for the animation's sounds; the per-sound item level (XRSound::SetVolume(soundID, volume)) scales it further.
    void SetVolume(float volume) { m_volume = volume; }
    float GetVolume() const { return m_volume; }

    void SetOpenCloseSoundID(int soundID) { m_openCloseSoundID = soundID; }
    int GetOpenCloseSoundID() const { return m_openCloseSoundID; }

    void SetMovingSoundID(int soundID) { m_movingSoundID = soundID; }
    int GetMovingSoundID() const { return m_movingSoundID; }

    bool SetWavForAnimationState(AnimationState::StateType state, const char *pWavFilePath);
    const char *GetWavForAnimationState(AnimationState::StateType state) const;

    int GetSoundIDForAnimationState(AnimationState::StateType state) const;

protected:
    int m_animationID;
    XRSound::PlaybackType m_playbackType;
    float m_volume;     // configured per-animation play volume; 1.0f if not set
    int m_openCloseSoundID;
    int m_movingSoundID;
    HASHMAP_ANIMATIONSTATE_WAVFILEPATH m_soundStateWavFileMap;
};

// Defines map of animation ID -> AnimationSounds object.
// key = animation ID, value = AnimationSounds object for that animation
typedef std::unordered_map<int, AnimationSounds> HASHMAP_ANIMATIONID_ANIMATIONSOUNDS;
typedef std::pair<int, AnimationSounds &> animationID_AnimationSounds_pair;

// Parses our global XRSound.cfg file + any vessel class-specific override file.  
// Each instance is tied to exactly one vessel instance (and, therefore, one vessel class instance).
class XRSoundConfigFileParser : public ConfigFileParser
{
public:
    XRSoundConfigFileParser(const char *pConfigFile = XRSOUND_CONFIG_FILE);
    virtual ~XRSoundConfigFileParser();

    // parse the global XRSound.cfg for a module
    bool ParseModuleSoundConfig(const char *pUniqueModuleName);

    // parse the global XRSound.cfg + any vessel class-specific override file for this vessel
    bool ParseVesselSoundConfig(VESSEL *pVessel);

    // base class calls this as it parses lines in the file
    virtual bool ParseLine(const char *pSection, const char *pPropertyName, const char *pValue, const bool bParsingOverrideFile) override;

    // Resets every [SYSTEM] setting to its built-in default (including forced verbose logging in _DEBUG builds). The constructor and XRSoundEngine::InitializeSoundEngine
    // call this before parsing XRSound.cfg so each session starts from the same defaults. SupportedSoundFileTypes and MusicFolder default to empty.
    void ResetSystemSettingsToDefaults();

    // ---------------------------------------------------------------------------------------------------------------------------
    // Valid ranges and defaults for the numeric settings.  These are the *single authority*: they are used both by ParseLine (the configuration file)
    // and by the runtime setters (XRSound::SetSetting), so the two can never disagree.  An invalid value in the configuration file is replaced by the default.
    // ---------------------------------------------------------------------------------------------------------------------------
    // [SYSTEM]
    static constexpr float  MASTER_VOLUME_MIN = 0.0f;
    static constexpr float  MASTER_VOLUME_MAX = 1.0f;
    static constexpr float  MASTER_VOLUME_DEFAULT = 1.0f;

    static constexpr float  MUSIC_VOLUME_MIN = 0.0f;
    static constexpr float  MUSIC_VOLUME_MAX = 1.0f;
    static constexpr float  MUSIC_VOLUME_DEFAULT = 1.0f;

    static constexpr double UPDATE_INTERVAL_MIN = 0.02;
    static constexpr double UPDATE_INTERVAL_MAX = 1.0;
    static constexpr double UPDATE_INTERVAL_DEFAULT = 0.05;

    static constexpr double DISABLE_AUTOPILOTS_TIME_ACC_THRESHOLD_MIN = 0;
    static constexpr double DISABLE_AUTOPILOTS_TIME_ACC_THRESHOLD_MAX = 10000;
    static constexpr double DISABLE_AUTOPILOTS_TIME_ACC_THRESHOLD_DEFAULT = 100;

    static constexpr double MIN_THRUSTER_LEVEL_FOR_RCS_SOUND_EFFECTS_MIN = 0;
    static constexpr double MIN_THRUSTER_LEVEL_FOR_RCS_SOUND_EFFECTS_MAX = 1.0;
    static constexpr double MIN_THRUSTER_LEVEL_FOR_RCS_SOUND_EFFECTS_DEFAULT = 0.05;

    // [SOUND_FILES]
    static constexpr double WARNING_GEAR_IS_UP_ALTITUDE_MIN = 0;
    static constexpr double WARNING_GEAR_IS_UP_ALTITUDE_MAX = 1000;
    static constexpr double WARNING_GEAR_IS_UP_ALTITUDE_DEFAULT = 275;

    // [SOUND_GROUPS]
    // range shared by CabinAmbienceMin and CabinAmbienceMax
    static constexpr int    CABIN_AMBIENCE_DELAY_MIN = 7;
    static constexpr int    CABIN_AMBIENCE_DELAY_MAX = 300;
    static constexpr int    CABIN_AMBIENCE_MIN_DEFAULT = 7;
    static constexpr int    CABIN_AMBIENCE_MAX_DEFAULT = 35;

    // [ATC]
    static constexpr float  ATC_VOLUME_MIN = 0.0f;
    static constexpr float  ATC_VOLUME_MAX = 1.0f;
    static constexpr float  ATC_VOLUME_DEFAULT = 1.0f;

    // range shared by ATCMinDelay and ATCMaxDelay
    static constexpr int    ATC_DELAY_MIN = 10;
    static constexpr int    ATC_DELAY_MAX = 900;
    static constexpr int    ATC_MIN_DELAY_DEFAULT = 15;
    static constexpr int    ATC_MAX_DELAY_DEFAULT = 120;

    static constexpr double ATC_DELAY_PLANET_DISTANCE_MIN = 0;
    static constexpr double ATC_DELAY_PLANET_DISTANCE_MAX = 1e6;
    static constexpr double ATC_DELAY_PLANET_DISTANCE_DEFAULT = 400;

    static constexpr double ATC_DELAY_PLANET_MULTIPLIER_MIN = 1;
    static constexpr double ATC_DELAY_PLANET_MULTIPLIER_MAX = 100;
    static constexpr double ATC_DELAY_PLANET_MULTIPLIER_DEFAULT = 5;

    // [animation_#]
    static constexpr float  ANIMATION_VOLUME_MIN = 0.0f;
    static constexpr float  ANIMATION_VOLUME_MAX = 1.0f;
    static constexpr float  ANIMATION_VOLUME_DEFAULT = 1.0f;

    // ---------------------------------------------------------------------------------------------------------------------------
    // state data read from the config file
    // Note: if a std::string filename / file path is empty, it means that default sound is disabled.
    // Therefore, we do no initialize these std::string values to a default value in the constructor.
    //
    // The objects of this class hold the CONFIGURED DEFAULTS and are written only by this class's parser. Nothing else assigns to a field:
    // not the XRSound API, and not the engines.  Each engine copies the values it needs into its own "active" values (XRSoundEngine::ActiveValues) right after its
    // configuration is parsed, and the API reads and changes only those.  A vessel's or module's configuration already contains the [SYSTEM] settings from XRSound.cfg
    // (a vessel class override file cannot override them).
    // The [SYSTEM] settings are: SupportedSoundFileTypes, MusicFolder, MasterVolume, MusicVolume, MusicOrder, EnableVerboseLogging, LogVesselAnimations,
    // LogThrusterData, SilenceOfSpace, UpdateInterval, DisableAutopilotsTimeAccThreshold, and MinThrusterLevelForRCSSoundEffects.
    // ---------------------------------------------------------------------------------------------------------------------------
    std::string SupportedSoundFileTypes;
    std::vector<std::string> SupportedSoundFileTypesAsVector() const
    {
        static const char *pTokenStr = " ";
        std::vector<std::string> vec;
		std::istringstream iss(SupportedSoundFileTypes);
		std::string token;

		while (iss >> token)
		{
			vec.push_back(token); // identical semantics
		}

		return vec;
    }
    
    std::string CustomEngines;      // wav file to play for these engines
    std::string CustomEnginesThrusterIndexes;
    std::vector<int> CustomEnginesThrusterIndexesAsVector() const
    {
        static const char *pTokenStr = " ";
        std::vector<int> vec;
		std::istringstream iss(CustomEnginesThrusterIndexes);
		std::string token;

		while (iss >> token)
		{
			vec.push_back(std::atoi(token.c_str())); // identical semantics
		}
        return vec;
    }

    float MasterVolume;
    bool EnableVerboseLogging;
    bool LogVesselAnimations;
    bool LogThrusterData;
    bool SilenceOfSpace;
    int LandingGearAnimationID;   // set manually via [animation] section logic whenever 'IsLandingGear = 1' is set for a given animation

    int CabinAmbienceMin;
    int CabinAmbienceMax;

    // ATC settings
    float ATCVolume;
    int ATCMinDelay;
    int ATCMaxDelay;
    bool ATCAllowWhileLanded;
    bool ATCAllowDuringReentry;
    bool ATCAllowInAtmosphere;
    double ATCDelayPlanetDistance;
    double ATCDelayPlanetMultiplier;

    // These are sound file (relative) paths; however, if they hold the special value of "none", it means the sound is disabled
    std::string AirConditioning;
    std::string LandedWind;
    float MusicVolume;
    double UpdateInterval;
    double WarningGearIsUpAltitude;
    double DisableAutopilotsTimeAccThreshold;
    
    enum class SeqRandom { Sequential, Random };
    enum class MusicPlay{ Off, Space, On};
    SeqRandom MusicOrder;
    MusicPlay MusicPlayInternal;
    MusicPlay MusicPlayExternal;

    // Explicit mapping between these internal enums and the public XRSound::MusicOrder / XRSound::MusicPlay enums, so that nothing relies on the two
    // sets of enums happening to share the same underlying values.  The ToInternal methods return false (leaving the output unchanged) for a value that is not a valid public enum value.
    static bool MusicOrderToInternal(const int publicValue, SeqRandom &internalValue);
    static int  MusicOrderToPublic(const SeqRandom internalValue);
    static bool MusicPlayToInternal(const int publicValue, MusicPlay &internalValue);
    static int  MusicPlayToPublic(const MusicPlay internalValue);

    double MinThrusterLevelForRCSSoundEffects;
    
    std::string AudioGreeting;

    std::string MainEngines;
    std::string HoverEngines;
    std::string RetroEngines;

    std::string RCSAttackPlusX;
    std::string RCSAttackPlusY;
    std::string RCSAttackPlusZ;
    std::string RCSAttackMinusX;
    std::string RCSAttackMinusY;
    std::string RCSAttackMinusZ;

    std::string RCSClosePlusX;
    std::string RCSClosePlusY;
    std::string RCSClosePlusZ;
    std::string RCSCloseMinusX;
    std::string RCSCloseMinusY;
    std::string RCSCloseMinusZ;

    std::string RCSSustain;

    std::string SwitchOn;
    std::string SwitchOff;

    std::string RCSRotation;
    std::string RCSTranslation;
    std::string RCSOff;

    std::string AFPitch;
    std::string AFOn;
    std::string AFOff;

    std::string Crash;
    std::string MetalCrunch;
    std::string WheelChirp;
    std::string Touchdown;
    std::string WheelStop;
    std::string TiresRolling;
    std::string OneHundredKnots;
    std::string Liftoff;
    std::string WarningGearIsUp;
    std::string YouAreClearedToLand;

    std::string Docking;
    std::string DockingCallout;
    std::string Undocking;
    std::string UndockingCallout;

    std::string Wheelbrakes;
    std::string DockingRadarBeep;
    std::string FlightWind;
    std::string ReentryPlasma;
    std::string SonicBoom;
    std::string AutopilotOn;
    std::string AutopilotOff;
    std::string SubsonicCallout;

    // Group (folder) paths;  however, if they hold the special value of "none", it means the sound group is disabled
    std::string CabinAmbienceGroup;
    std::string ATCFolder;
    std::string AltitudeCalloutsGroup;
    std::string DockingDistanceCalloutsGroup;
    std::string MachCalloutsGroup;
    std::string MusicFolder;

    // Returns AnimationSounds object for a given animation ID, or nullptr if not found
    AnimationSounds *GetAnimationSounds(const int animationID);

    // Returns vector of all AnimationSounds objects for this vessel (e.g., default sounds + any custom ones defined by the end user via .cfg file)
    std::vector<const AnimationSounds *> GetAllAnimationSounds() const;

protected:
    bool AddOrUpdateAnimationState(const int animationID, const char *pPropertyName, const char *pValue);

    // key = animation ID, value = AnimationSounds for that animation
    HASHMAP_ANIMATIONID_ANIMATIONSOUNDS m_animationSoundsMap;
};

