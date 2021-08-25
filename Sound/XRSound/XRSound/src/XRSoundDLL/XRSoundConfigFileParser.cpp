// ==============================================================
// XRSoundConfigFileParser.cpp : Parses XRSound.cfg
//
// Copyright (c) 2018 Douglas Beachy
// Licensed under the MIT License
//
// This software is FREEWARE and may not be sold!e
// ==============================================================

#include "XRSoundConfigFileParser.h"
#include "ConfigFileParserMacros.h"

// Constructor
XRSoundConfigFileParser::XRSoundConfigFileParser(const char *pConfigFile) :
    ConfigFileParser(pConfigFile, XRSOUND_LOG_FILE),
    EnableVerboseLogging(false), LogVesselAnimations(false), LogThrusterData(false), MasterVolume(1.0), CabinAmbienceMin(7), CabinAmbienceMax(35), MusicVolume(1.0),
    ATCVolume(1.0), ATCMinDelay(15), ATCMaxDelay(120), ATCAllowWhileLanded(true), ATCAllowDuringReentry(false), ATCAllowInAtmosphere(true),
    ATCDelayPlanetDistance(400.0), ATCDelayPlanetMultiplier(5.0), LandingGearAnimationID(-1),
    MusicOrder(SeqRandom::Random), MusicPlayInternal(MusicPlay::Off), MusicPlayExternal(MusicPlay::Space), UpdateInterval(0.05), SilenceOfSpace(true),
    WarningGearIsUpAltitude(275), DisableAutopilotsTimeAccThreshold(100), MinThrusterLevelForRCSSoundEffects(0.05)
{
    // No prefix: SetLogPrefix(XRSOUND_CONFIG_FILE);  // will show "<timestamp> [Sound\XRSound.cfg] <log message>" in log file
#ifdef _DEBUG
    EnableVerboseLogging = true;        // always default to true for debug builds
#endif
}

// Destructor
XRSoundConfigFileParser::~XRSoundConfigFileParser()
{
}

// Parse our global XRSound.cfg for the supplied module ID; there is no override .cfg file for modules, since those settings are all
// vessel-related.
bool XRSoundConfigFileParser::ParseModuleSoundConfig(const char *pUniqueModuleName)
{
    m_csConfigFilenames = GetDefaultFilename();

    CString csTemp;
    csTemp.Format("[%s] Using configuration file: %s", pUniqueModuleName, GetConfigFilenames());
    WriteLog(csTemp);

    return ParseFile();  // ignore error here (any errors were already logged)
}

// Parse configuration for the supplied vessel; first parses our global XRSound.cfg, then parses the vessel class-specific 
// override .cfg file, if present.
//
//   pVessel: will never be nullptr
// Return: true on success, false if at least one warning or error exists in the .cfg file(s).
bool XRSoundConfigFileParser::ParseVesselSoundConfig(VESSEL *pVessel)
{
    _ASSERTE(pVessel);

    bool bOverrideFileExists = false;
    const char *pVesselClassName = pVessel->GetClassName();

    // NOTE: some vessel class names have slashes or other illegal filename characters, so we have to handle that here
    static const CString s_csIllegalCharacters("\\/:? \"<>|");
    CString csSanitizedClassName(pVesselClassName);
    for (int i = 0; i < csSanitizedClassName.GetLength(); i++)
    {
        if (s_csIllegalCharacters.Find(csSanitizedClassName[i]) >= 0)
            csSanitizedClassName.SetAt(i, '_');   // replace invalid character
    }

    m_csOverrideFilename.Format("XRSound\\XRSound-%s.cfg", static_cast<const char *>(csSanitizedClassName));  // e.g., "XRSound\XRSound-XR2Ravenstar.cfg"
    bOverrideFileExists = (PathFileExists(m_csOverrideFilename) != FALSE);

    if (bOverrideFileExists)
        m_csConfigFilenames.Format("%s + %s", GetDefaultFilename(), GetOverrideFilename());
    else
    {
        m_csConfigFilenames.Format("%s (no override found [%s])", GetDefaultFilename(), GetOverrideFilename());
        m_csOverrideFilename.Empty();  // empty the filename to indicate it does not exist
    }

    CString csTemp;
    csTemp.Format("[%s][class %s] Using configuration file(s): %s", pVessel->GetName(), pVesselClassName, GetConfigFilenames());
    WriteLog(csTemp);

    // parse the default config file first
    bool retVal = ParseFile();  // ignore error here (any errors were already logged)

    // now parse the override file if any exists
    if (bOverrideFileExists)
        retVal &= ParseFile(GetOverrideFilename());  // clear flag if error occurs

    return retVal;
}

#define PARSE_STRING_PROPERTY(propName)             \
if (PNAME_MATCHES(TO_STR(propName)))                \
{                                                   \
    CSTRING_CPY(propName);                          \
    return true;                                    \
}

// Invoked by our superclass as each line of the config file is read.
// pP
// Returns: true if line OK, false if error
bool XRSoundConfigFileParser::ParseLine(const char *pSection, const char *pPropertyName, const char *pValue, const bool bParsingOverrideFile)
{
    // sanity checks; our base class should already validate that each of these values are not nullptr or empty.
    _ASSERTE(pSection);
    _ASSERTE(*pSection);

    _ASSERTE(pPropertyName);
    _ASSERTE(*pPropertyName);

    _ASSERTE(pValue);
    _ASSERTE(*pValue);

    // TODO: if and when vessel-class-specific configuration overrides are implemented, look into caching the default 
    // config file's XRSoundConfigFileParser object (and copying its state via a copy constructor?)
    // so that we don't unnecessarily re-parse the config file for every single vessel that is created.
    // Still, it should be inexpensive to parse the config file, given its size, so I'll have to see.

    // TODO: once parser is complete, delete any of these that we d on't need
    int len;        // used by SECTION_STARTSWITH and PNAME_STARTSWITH macros
    bool processed = false;     // set to 'true' by macros if parameter processed; primarily used by subclasses, and the macros expect this variable to exist
    CString csMsg;      // used for constructing log messages

    // parse [SYSTEM] settings
    if (SECTION_MATCHES("SYSTEM"))
    {
        // cannot override [SYSTEM] settings via vessel class override file, so check that here
        if (bParsingOverrideFile)
        {
            csMsg.Format("WARNING parsing file [%s]: cannot override [SYSTEM] settings via a vessel class override file.", GetCurrentFilename(bParsingOverrideFile));
            WriteLog(csMsg);
            return false;
        }

        if (PNAME_MATCHES("EnableVerboseLogging"))
        {
            // always default to true for debug builds, so ignore this setting in that case
#ifndef _DEBUG
            SSCANF_BOOL("%c", &EnableVerboseLogging);
#endif
        }
        else if (PNAME_MATCHES("MasterVolume"))
        {
            SSCANF1("%f", &MasterVolume);
            VALIDATE_FLOAT(&MasterVolume, 0, 1.0f, 1.0f);
        }
        else if (PNAME_MATCHES("LogVesselAnimations"))
        {
            SSCANF_BOOL("%c", &LogVesselAnimations);
        }
        else if (PNAME_MATCHES("LogThrusterData"))
        {
            SSCANF_BOOL("%c", &LogThrusterData);
        }
        else if (PNAME_MATCHES("SilenceOfSpace"))
        {
            SSCANF_BOOL("%c", &SilenceOfSpace);
        }
        else if (PNAME_MATCHES("UpdateInterval"))
        {
            SSCANF1("%lf", &UpdateInterval);
            VALIDATE_DOUBLE(&UpdateInterval, 0.02, 1.0, 0.05);
        }
        else if (PNAME_MATCHES("DisableAutopilotsTimeAccThreshold"))
        {
            SSCANF1("%lf", &DisableAutopilotsTimeAccThreshold);
            VALIDATE_DOUBLE(&DisableAutopilotsTimeAccThreshold, 0, 10000, 100);
        }
        else PARSE_STRING_PROPERTY(SupportedSoundFileTypes)
        else PARSE_STRING_PROPERTY(MusicFolder)
        else if (PNAME_MATCHES("MusicVolume"))
        {
            SSCANF1("%f", &MusicVolume);
            VALIDATE_FLOAT(&MusicVolume, 0, 1.0f, 1.0f);
        }
        else if (PNAME_MATCHES("MusicOrder"))
        {
            if (_stricmp(pValue, "random") == 0)
            {
                MusicOrder = SeqRandom::Random;
                return true;
            }
            else if (_stricmp(pValue, "sequential") == 0)
            {
                MusicOrder = SeqRandom::Sequential;
                return true;
            }
            else
            {
                WriteLog("Invalid value for parameter 'MusicOrder'; valid values are 'random' or 'sequential'");
                return false;
            }
        }
        else if (PNAME_MATCHES("MinThrusterLevelForRCSSoundEffects"))
        {
            SSCANF1("%lf", &MinThrusterLevelForRCSSoundEffects);
            VALIDATE_DOUBLE(&MinThrusterLevelForRCSSoundEffects, 0, 1.0, 0);
        }
        else
        {
            goto invalid_property_name;
        }
    }
    else if(SECTION_MATCHES("SOUND_FILES"))
    {
        // the PARSE_STRING_PROPERTY macro evaluates out an if block, so don't use a semicolon on the end of each line
        PARSE_STRING_PROPERTY(AirConditioning)
        else PARSE_STRING_PROPERTY(LandedWind)
        else if (PNAME_MATCHES("MusicPlayInternal") || PNAME_MATCHES("MusicPlayExternal"))
        {
            MusicPlay &playVar = ((PNAME_MATCHES("MusicPlayInternal") ? MusicPlayInternal : MusicPlayExternal));
            if (_stricmp(pValue, "off") == 0)
            {
                playVar = MusicPlay::Off;
                return true;
            }
            else if (_stricmp(pValue, "space") == 0)
            {
                playVar = MusicPlay::Space;
                return true;
            }
            else if (_stricmp(pValue, "on") == 0)
            {
                playVar = MusicPlay::On;
                return true;
            }
            else
            {
                WriteLog("Invalid value for parameter; valid values are 'off', 'space', or 'on'");
                return false;
            }
        }
        else PARSE_STRING_PROPERTY(LandedWind)
        else PARSE_STRING_PROPERTY(AudioGreeting)
        else PARSE_STRING_PROPERTY(MainEngines)
        else PARSE_STRING_PROPERTY(HoverEngines)
        else PARSE_STRING_PROPERTY(RetroEngines)
        else PARSE_STRING_PROPERTY(RCSAttackPlusX)
        else PARSE_STRING_PROPERTY(RCSAttackPlusY)
        else PARSE_STRING_PROPERTY(RCSAttackPlusZ)
        else PARSE_STRING_PROPERTY(RCSAttackMinusX)
        else PARSE_STRING_PROPERTY(RCSAttackMinusY)
        else PARSE_STRING_PROPERTY(RCSAttackMinusZ)
        else PARSE_STRING_PROPERTY(RCSSustain)
        else PARSE_STRING_PROPERTY(SwitchOn)
        else PARSE_STRING_PROPERTY(SwitchOff)
        else PARSE_STRING_PROPERTY(RCSRotation)
        else PARSE_STRING_PROPERTY(RCSTranslation)
        else PARSE_STRING_PROPERTY(RCSOff)
        else PARSE_STRING_PROPERTY(AFPitch)
        else PARSE_STRING_PROPERTY(AFOn)
        else PARSE_STRING_PROPERTY(AFOff)
        else PARSE_STRING_PROPERTY(Crash)
        else PARSE_STRING_PROPERTY(MetalCrunch)
        else PARSE_STRING_PROPERTY(WheelChirp)
        else PARSE_STRING_PROPERTY(Touchdown)
        else PARSE_STRING_PROPERTY(WheelStop)
        else PARSE_STRING_PROPERTY(TiresRolling)
        else PARSE_STRING_PROPERTY(OneHundredKnots)
        else PARSE_STRING_PROPERTY(Liftoff)
        else PARSE_STRING_PROPERTY(WarningGearIsUp)
        else PARSE_STRING_PROPERTY(YouAreClearedToLand)
        else PARSE_STRING_PROPERTY(Docking)
        else PARSE_STRING_PROPERTY(DockingCallout)
        else PARSE_STRING_PROPERTY(Undocking)
        else PARSE_STRING_PROPERTY(UndockingCallout)
        else PARSE_STRING_PROPERTY(Wheelbrakes)
        else PARSE_STRING_PROPERTY(DockingRadarBeep)
        else PARSE_STRING_PROPERTY(FlightWind)
        else PARSE_STRING_PROPERTY(ReentryPlasma)
        else PARSE_STRING_PROPERTY(SonicBoom)
        else PARSE_STRING_PROPERTY(AutopilotOn)
        else PARSE_STRING_PROPERTY(AutopilotOff)
        else PARSE_STRING_PROPERTY(SubsonicCallout)
        else if (PNAME_MATCHES("WarningGearIsUpAltitude"))
        {
            SSCANF1("%lf", &WarningGearIsUpAltitude);
            VALIDATE_DOUBLE(&WarningGearIsUpAltitude, 0, 1000, 275);
        }
        else
        {
            goto invalid_property_name;
        }
    }
    else if (SECTION_MATCHES("SOUND_GROUPS"))
    {
        // the PARSE_SINGLE_SOUND_FILE macro evaluates out an if block, so don't use a semicolon on the end of each line
        PARSE_STRING_PROPERTY(CabinAmbienceGroup)
        else if (PNAME_MATCHES("CabinAmbienceMin"))
        {
            SSCANF1("%d", &CabinAmbienceMin);
            VALIDATE_INT(&CabinAmbienceMin, 7, 300, 7);
        }
        else if (PNAME_MATCHES("CabinAmbienceMax"))
        {
            SSCANF1("%d", &CabinAmbienceMax);
            VALIDATE_INT(&CabinAmbienceMax, 7, 300, 35);
        }
        else PARSE_STRING_PROPERTY(AltitudeCalloutsGroup)
        else PARSE_STRING_PROPERTY(DockingDistanceCalloutsGroup)
        else PARSE_STRING_PROPERTY(MachCalloutsGroup)
        else
        {
            goto invalid_property_name;
        }
    }
    else if (SECTION_MATCHES("ATC"))
    {
        if (PNAME_MATCHES("Folder"))
        {                                   
            CSTRING_CPY(ATCFolder);
            return true;                    
        }
        else if (PNAME_MATCHES("Volume"))
        {
            SSCANF1("%f", &ATCVolume);
            VALIDATE_FLOAT(&ATCVolume, 0, 1.0f, 1.0f);
        }
        else if (PNAME_MATCHES("MinDelay"))
        {
            SSCANF1("%d", &ATCMinDelay);
            VALIDATE_INT(&ATCMinDelay, 10, 900, 15);
        }
        else if (PNAME_MATCHES("MaxDelay"))
        {
            SSCANF1("%d", &ATCMaxDelay);
            VALIDATE_INT(&ATCMaxDelay, 10, 900, 120);
        }
        else if (PNAME_MATCHES("AllowWhileLanded"))
        {
            SSCANF_BOOL("%c", &ATCAllowWhileLanded);
        }
        else if (PNAME_MATCHES("AllowDuringReentry"))
        {
            SSCANF_BOOL("%c", &ATCAllowDuringReentry);
        }
        else if (PNAME_MATCHES("AllowInAtmosphere"))
        {
            SSCANF_BOOL("%c", &ATCAllowInAtmosphere);
        }
        else if (PNAME_MATCHES("DelayPlanetDistance"))
        {
            SSCANF1("%lf", &ATCDelayPlanetDistance);
            VALIDATE_DOUBLE(&ATCDelayPlanetDistance, 0, 1e6, 400);
        }
        else if (PNAME_MATCHES("DelayPlanetMultiplier"))
        {
            SSCANF1("%lf", &ATCDelayPlanetMultiplier);
            VALIDATE_DOUBLE(&ATCDelayPlanetMultiplier, 1, 100, 5);
        }
        else
        {
            goto invalid_property_name;
        }
    }
    else if (SECTION_MATCHES("VESSEL"))
    {
        // cannot override [VESSEL] settings via the global XRSound.cfg; that makes no sense
        if (!bParsingOverrideFile)
        {
            csMsg.Format("WARNING parsing file [%s]: [VESSEL] properties are inherently vessel class-specific and therefore have no meaning in the global .cfg file.", GetCurrentFilename(bParsingOverrideFile));
            WriteLog(csMsg);
            return false;
        }
        
        PARSE_STRING_PROPERTY(CustomEnginesThrusterIndexes)
        else PARSE_STRING_PROPERTY(CustomEngines)
        else
        {
            goto invalid_property_name;
        }
    }
    else if (SECTION_STARTSWITH("animation_"))
    {
        // these are only valid for vessel override .cfg files, NOT the master file
        if (!bParsingOverrideFile)
        {
            csMsg.Format("WARNING parsing file [%s]: [animation_*] properties are inherently vessel class-specific and therefore have no meaning in the global .cfg file.", GetCurrentFilename(bParsingOverrideFile));
            WriteLog(csMsg);
            return false;
        }

        // e.g., "[animation_7]"
        int animationID = -1;
        sscanf(pSection + 10, "%d", &animationID); 
        if (animationID < 0)
        { 
            csMsg.Format("ERROR: Invalid [animation] section name ('%s'): missing or invalid animation ID.  Format should be [animation_#]; e.g., [animation_7].",
                pSection);
            WriteLog(csMsg);
            return false;
        }

        // If we reach here, this line is in a valid animation section. Validate the supplied sound state and supplied file path, also
        // making sure the file exists.  If this fails, it will log a message and return false, so we don't need to log an additional error message here
        // if the call fails.
        return AddOrUpdateAnimationState(animationID, pPropertyName, pValue);
    }
    else  // invalid section!
        goto invalid_section;

    // success!
    return true;

    // invalid section handler
invalid_section:
    // our base class will provide more details in the line following this one
    if (*pSection == 0)
        csMsg = "Missing [section] line (e.g., '[GENERAL]')";
    else
    {
        csMsg.Format("Invalid [section] value: '%s'", pSection);
    }
    WriteLog(csMsg);
    return false;

    // invalid property name handler
invalid_property_name:
    // our base class will provide more details in the line following this one
    csMsg.Format("Invalid property name: '%s' in section [%s]", pPropertyName, pSection);
    WriteLog(csMsg);
    return false;
}

// Update our m_animationSoundsMap for the supplied property name and value.
//   animationID: unique animationID; 0..n
//   pName: e.g., "PlaybackType", "Opening", "Open", "Moving", "etc.  Will never be nullptr or empty.
//   pValue: e.g., path to custom wav file for this sound; e.g., "Default\Gear Down.flac".  Will never be nullptr or empty.
// Returns: true on success, or false on error (e.g., animationID < 0, pName is unknown, or sound file does not exist)
bool XRSoundConfigFileParser::AddOrUpdateAnimationState(const int animationID, const char *pName, const char *pValue)
{
    bool processed = false;     // set to 'true' by macros if parameter processed; the macros expect this variable to exist

    _ASSERTE(animationID >= 0);
    if (animationID < 0)
        return false;

    bool bSuccess = true;

    // retrieve or create our AnimationSounds object for this animationID
    AnimationSounds *pAnimationSounds = GetAnimationSounds(animationID);
    if (!pAnimationSounds)
    {
        // Don't have any custom sounds for this animation yet, so create a new object to hold them and add them to our master m_animationSoundsMap.
        // This object will be copied by and persisted by value.
        AnimationSounds animationSounds(animationID);
        m_animationSoundsMap.insert(animationID_AnimationSounds_pair(animationID, animationSounds));

        // NOTE: we need to retrieve the *persisted* object we just added to the map, not the animationSounds object that's about to go out-of-scope.
        pAnimationSounds = GetAnimationSounds(animationID); // will always succeed now.  
        bSuccess = true;
    }
    _ASSERTE(pAnimationSounds);

    // parse non-wav file path paraemters
    if (_stricmp(pName, "OpenCloseSoundID") == 0)
    {
        int soundID = -1;
        SSCANF1("%d", &soundID);
        VALIDATE_INT(&soundID, 0, MAXINT32, -1);
        if (soundID >= -0)
            pAnimationSounds->SetOpenCloseSoundID(soundID);
    }
    else if (_stricmp(pName, "MovingSoundID") == 0)
    {
        int soundID = -1;
        SSCANF1("%d", &soundID);
        VALIDATE_INT(&soundID, 0, MAXINT32, -1);
        if (soundID >= -0)
            pAnimationSounds->SetMovingSoundID(soundID);
    }
    else if (_stricmp(pName, "PlaybackType") == 0)
    {
        // this sets the playbacktype for each default sound in this animation
        XRSound::PlaybackType type;
#define PARSE_PLAYBACK_TYPE(pbt) else if (_stricmp(pValue, TO_STR(pbt)) == 0) type = XRSound::PlaybackType::pbt;

        if (false);   // deliberate empty statement here so the macro works
        PARSE_PLAYBACK_TYPE(InternalOnly)
        PARSE_PLAYBACK_TYPE(BothViewFar)
        PARSE_PLAYBACK_TYPE(BothViewMedium)
        PARSE_PLAYBACK_TYPE(BothViewClose)
        PARSE_PLAYBACK_TYPE(Radio)
        PARSE_PLAYBACK_TYPE(Wind)
        else
        {
            CString msg;
            msg.Format("ERROR: Invalid playback type: '%s'.  Valid playback types are InternalOnly, BothViewFar, BothViewMedium, BothViewClose, Radio, and Wind.", pName);
            WriteLog(msg);
            return false;
        }

        // playback type is valid!
        pAnimationSounds->SetPlaybackType(type);
    }
    else if (_stricmp(pName, "IsLandingGear") == 0)
    { 
        bool bIsLandingGear = false;
        SSCANF_BOOL("%c", &bIsLandingGear);
        if (bIsLandingGear)
        {
            LandingGearAnimationID = animationID;
            CString msg;
            msg.Format("XRSoundConfigFileParser: using landing gear animation ID %d", animationID);
            WriteLog(msg);
        }
    }
    else   // let's see if it's a door sound + wav file 
    {
        // parse soundType = <wav file>
        // if we reach here, parse it as an animation state + wav path
        AnimationState::StateType state;

#define PARSE_STATE_TYPE(type) else if (_stricmp(pName, TO_STR(type)) == 0) state = AnimationState::StateType::type;

        // Note: if a sound goes idle, then by definition it is silent, so you cannot specify a custom sound for the Idle state.
        if (false);   // deliberate empty statement here so the macro works
        PARSE_STATE_TYPE(Opening)
        PARSE_STATE_TYPE(Open)
        PARSE_STATE_TYPE(Moving)
        PARSE_STATE_TYPE(Closing)
        PARSE_STATE_TYPE(Closed)
        else
        {
            CString msg;
            msg.Format("ERROR: Invalid sound event type: '%s'.  Valid sound event types are Opening, Open, Moving, Closing, and Closed.", pName);
            WriteLog(msg);
            return false;
        }

        // Now validate that the file exists: although LoadWav will also do that later, we want this to "fail-fast" so
        // the user knows immediately on simulation start if his vessel override .cfg file is wrong.
        if (!IsFileReadable(pValue))
        {
            CString msg;
            msg.Format("ERROR: Sound file does not exist or is not readable: '%s' (remember that sound paths are relative to $ORBITER_ROOT)", pValue);
            WriteLog(msg);
            return false;
        }

        // We have a valid sound event type and file specfied!  Plug it into our master custom default sounds map for this vessel.

        // add or replace the wav file for this animation type (Opening, Closing, etc.).
        bSuccess = pAnimationSounds->SetWavForAnimationState(state, pValue);
        _ASSERTE(bSuccess);     // if false, it means that state == Unknown, which you cannot assign a custom sound to (and we should have previously validated that)
    }

    return bSuccess;
}

// Returns a pointer to the AnimationSounds object in our master m_animationSoundsMap for a given animation ID, or nullptr if not found.
// Any changes made to the returned object will be reflected and persisted in our master m_animationSoundsMap.
AnimationSounds *XRSoundConfigFileParser::GetAnimationSounds(const int animationID)
{
    AnimationSounds *pRetVal = nullptr;

    auto it = m_animationSoundsMap.find(animationID);
    if (it != m_animationSoundsMap.end())
        pRetVal = &(it->second);

    return pRetVal;
}

// Returns vector of all AnimationSounds objects for this vessel (e.g., default sounds + any custom ones defined by the end user via .cfg file)
std::vector<const AnimationSounds *> XRSoundConfigFileParser::GetAllAnimationSounds() const
{
    std::vector<const AnimationSounds *> allAnimationSounds;
    for (auto it = m_animationSoundsMap.begin(); it != m_animationSoundsMap.end(); it++)
        allAnimationSounds.push_back(&(it->second));

    return allAnimationSounds;
}

// =========================================================================
// AnimationSounds class
// =========================================================================

// Constructor
AnimationSounds::AnimationSounds(const int animationID) :
    m_animationID(animationID), m_playbackType(XRSound::PlaybackType::BothViewClose),
    m_openCloseSoundID(-1), m_movingSoundID(-1)
{
}

// state: may not be of type Unknown.
// pWavFilePath: if nullptr or empty, removes the sound for the supplied animation state
// Returns: true on success, false if state is invalid (i.e., "Unknown"
bool AnimationSounds::SetWavForAnimationState(AnimationState::StateType state, const char *pWavFilePath)
{
    if (state == AnimationState::StateType::Unknown)
    {
        // can't set a sound for "Unknown" state
        _ASSERTE(false);
        return false;
    }

    if (!pWavFilePath || !*pWavFilePath)
    {
        // caller wants to remove sound for this animation state
        m_soundStateWavFileMap.erase(state);
    }
    else
    {
        // define or redfine the sound for this animation state
        // Note: state and pWavFilePath are copied by value
        m_soundStateWavFileMap.insert(animationState_wavFilePath_Pair(state, pWavFilePath));
    }

    return true;
}

// state: *may* be of type Unknown (in which case this returns nullptr)..
// Returns nullptr if no sound defined for the supplied animation state OR if the required 
// sound ID that is uses has not been set.
const char *AnimationSounds::GetWavForAnimationState(AnimationState::StateType state) const
{
    const char *pcsSoundPath = nullptr;

    auto it = m_soundStateWavFileMap.find(state);
    if (it != m_soundStateWavFileMap.end())
    {
        // verify that the required sound ID for this sound has also been set; if not, the sound cannot play.
        int soundID = GetSoundIDForAnimationState(state);
        if (soundID >= 0)
            pcsSoundPath = it->second;   // CString lives in our map, so it's OK to return a pointer to it
    }

    return pcsSoundPath;
}

// Returns the sound ID for the supplied state, or -1 if no sound ID for that state set.
int AnimationSounds::GetSoundIDForAnimationState(AnimationState::StateType state) const
{
    int animationID = -1;
    switch (state)
    {
    case AnimationState::StateType::Opening:
    case AnimationState::StateType::Open:
    case AnimationState::StateType::Closing:
    case AnimationState::StateType::Closed:
        animationID = GetOpenCloseSoundID();
        break;

    case AnimationState::StateType::Moving:
        animationID = GetOpenCloseSoundID();
        break;

    default:
        // fall through with -1; no sound ID for Unknown or Idle
        break;
    }
    return animationID;
}
