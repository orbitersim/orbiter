// ==============================================================
// XRSoundConfigFileParser.h
// Parses XRSound.cfg
//
// Copyright (c) 2018-2021 Douglas Beachy
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

    // state data read from the config file
    // Note: if a std::string filename / file path is empty, it means that default sound is disabled.
    // Therefore, we do no initialize these std::string values to a default value in the constructor.
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
