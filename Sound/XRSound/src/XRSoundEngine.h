// ==============================================================
// Base class for the XRSound API engine implementation; this file is not distributed with XRSound.
// The code for this exists in XRSound.dll.
// 
// Copyright (c) 2018-2026 Douglas Beachy and contributors
// Licensed under the MIT License
// ==============================================================

#pragma once

#include <unordered_map>

#include "OrbiterSDK.h"
#include "XRSoundEngine41.h"   // latest interface version

using namespace std;

class XRSoundConfigFileParser;
class VesselXRSoundEngine;
class ModuleXRSoundEngine;

// Redefine oapiGetSimTime as a compile-time error to prevent XRSound code from accidentally invoking it
// Note: ideally I would like to have this invoke '#error foo' to generate an elegant message, but there's no way to declare that
#define oapiGetSimTime ERROR! "Do not invoke oapiGetSimTime: see comment block in XRSoundDLL::clbkPreStep for details"

class ISound;
class ISoundEngine;
#ifdef XRSOUND_DLL_BUILD
#include "XRSoundConfigFileParser.h"
#include "Utils/FormatString.h"     // for VERBOSE_LOG; XRSound.lib never formats log text, so it never sees this header
#else
// we're compiling XRSoundLib, so all we need is a forward reference (and we don't want to include the full class definition on the .lib side!)
class XRSoundConfigFileParser;
#endif

// Data object that contains all the state necessary to play back a single sound file (sound effect).
// This maintains state for a given sound across multiple frames. It persists until the vessel is destroyed
// or the vessel loads another wav file using the same sound ID (i.e., "sound slot") as this context uses.
struct WavContext
{
    // Normal constructor
    WavContext(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType, const bool bEnabled) :
        soundID(soundID), csSoundFilename(pSoundFilename), playbackType(playbackType),
        bEnabled(bEnabled)
    {
        ResetPlaybackFields();  // initializes any playback fields not already initialized by their default member initializers below
    }

    // Copy constructor: the contexts are stored in a map by copy, so it must copy *every* field.  Defaulting it guarantees that
    // (a hand-written field list would silently drop any field added later); every field has a default member initializer.
    WavContext(const WavContext &that) = default;

    // reset all playback-related fields to their initial state, indicating the context is not (or not longer) in use
    // NOTE: requestedVolume is deliberately NOT reset here: it survives StopWav so that PlayWav(..., UseStoredVolume) plays at the stored volume again.
    // The pending pan/speed/position values are not reset here either: they are one-shot values for the *next* start of the sound.
    void ResetPlaybackFields()
    {
        bLoop = false;
        pISound = nullptr;  // we will allocate a new ISound interface when (and if) the sound is played again
    }

    // Returns the playback type in effect: the SetPlaybackType override if one is set, otherwise the type supplied by the latest LoadWav.
    XRSound::PlaybackType EffectivePlaybackType() const
    {
        return (bHasPlaybackTypeOverride ? playbackTypeOverride : playbackType);
    }

    // used for logging; defined in XRSoundEngine.cpp because XRSound.lib never formats log text
    std::string ToStr(const bool bShowLoopAndVolume = true) const;

    // Used for logging
    static const char* PlaybackTypeToStr(const XRSound::PlaybackType state)
    {
        switch (state) {
        case XRSound::PlaybackType::InternalOnly:
            return "InternalOnly";
        case XRSound::PlaybackType::BothViewFar:
            return "BothViewFar";
        case XRSound::PlaybackType::BothViewMedium:
            return "BothViewMedium";
        case XRSound::PlaybackType::BothViewClose:
            return "BothViewClose";
        case XRSound::PlaybackType::Radio:
            return "Radio";
        case XRSound::PlaybackType::Wind:
            return "Wind";
        case XRSound::PlaybackType::Global:
            return "Global";
        default:
            return "ERROR (unknown type)";
        }
    }

    // data added via LoadWav
    int soundID;
    std::string csSoundFilename;
    XRSound::PlaybackType playbackType;   // InternalOnly, BothViewFar, Radio, etc.; the type most recently supplied by LoadWav (see EffectivePlaybackType)

    // data added via PlayWav and SetPaused
    bool bLoop = false;
    float requestedVolume = 1.0f;   // 0..1.0 (client values above 1.0 are permitted); this is the volume requested via PlayWav / SetVolume (or XRSound's automatic volume for default sounds),
                                    // NOT adjusted for the item level, the category level, the engine's main volume, or distance.  It survives StopWav; see XRSoundEngine::ComputeMixVolume.
    bool bPaused = false;
    bool bEnabled = true;           // NOTE this is *only* used to enable or disable default sounds, NEVER for other sounds
    ISound *pISound = nullptr;      // used to control and retrieve the state of the sound effect

    // Slot-level values (itemLevel, category, the playback type override, and bEnabled) survive LoadWav replacing the file in this slot; all the
    // other values below this line belong to the file that is loaded and are reset when a different file is loaded into the slot (LoadWav builds a new WavContext).
    float itemLevel = 1.0f;         // default sounds, default groups, and animation sounds only: multiplier on XRSound's automatic volume (set via XRSound::SetVolume(soundID, volume)); always 1.0 for client sounds
                                    // (the global music slot ignores this field: each engine keeps its own item level for MusicFolder in ActiveValues::musicItemLevel, since music is
                                    // played by whichever vessel has focus and a vessel may change only its own levels)
    XRSound::VolumeType category = XRSound::VolumeType::Custom;     // Engines..Custom; set when the context is created (see XRSoundEngine::CategoryForSoundID)
    bool bHasPlaybackTypeOverride = false;                          // true if SetPlaybackType set an override (never set on the global music slot: see ActiveValues::bHasMusicPlaybackTypeOverride)
    XRSound::PlaybackType playbackTypeOverride = XRSound::PlaybackType::Global;   // only valid if bHasPlaybackTypeOverride

    // duration cache for XRSound::GetDuration; computed only when called
    bool bDurationCached = false;
    unsigned int cachedDurationMillis = 0;

    // One-shot values from SetPan / SetPlaybackSpeed / SetPlayPosition on a sound that is not playing; PlayWav applies them (and clears them) when it creates the ISound.
    bool bHasPendingPan = false;
    float pendingPan = 0.0f;
    bool bHasPendingSpeed = false;
    float pendingSpeed = 1.0f;
    bool bHasPendingPosition = false;
    unsigned int pendingPositionMillis = 0;
};

// Abstract base class for all XRSoundEngine objects; Implements the XRSoundEngine 2.0/3.0/4.1 interface.
//
// WARNING: if using static linking, do not allocate or free any memory in any inline methods here: XRSound.dll uses its own heap, so any
// code that allocates or frees memory must reside in XRSound.dll, NOT in the calling DLL via non-virtual inline code
// in this header file.

// NOTE: these methods must all be *virtual* so that they can be located via the object pointer from inside 
// the caller's DLL: by design, there is no import library for XRSound.dll.
// Do NOT use any inline code for any methods invoked by XRSoundImpl, or it will reside in the caller's DLL instead of XRSound.dll.
//
// NOTE: this class is not thread-safe!  However, Orbiter is not multi-threaded, so that should not be an issue.
class XRSoundEngine : public XRSoundEngine41
{
public:
    // Note: constructor and destructor are private by design; callers should always use CreateInstance (defined in each subclass) 
    // and DestroyInstance to create and destroy instances of XRSoundEngine.
    static void DestroyInstance(XRSoundEngine *pInst);

    //============================================================================================
    // Public abstract methods: these implement the XRSoundEngine interfaces
    //============================================================================================
    // XRSound version 1.x 
    virtual float GetVersion() const override;
    virtual bool LoadWav(const int soundID, const char *pSoundFilename, const XRSound::PlaybackType playbackType) override;
    virtual bool PlayWav(const int soundID, const bool bLoop = false, const float volume = 1.0) override;
    virtual bool StopWav(const int soundID) override;
    virtual bool IsWavPlaying(const int soundID) override;
    virtual bool SetPaused(const int soundID, const bool bPause) override;
    virtual bool IsPaused(const int soundID) override;

    virtual bool SetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID, const bool bEnabled) override = 0;
    virtual bool GetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID) override = 0;
    
    virtual bool SetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID, const char *pSubfolderPath) override = 0;
    virtual const char *GetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID) const override = 0;

    // these are not part of the public XRSound API, but are exposed via the XRSoundEngine interface so the XRSoundImpl side can call them
    virtual bool IsDefaultSound(const int soundID) const override;
    virtual bool IsDefaultSoundGroup(const int soundID) const override;
    virtual bool IsDefaultSoundOrGroup(const int soundID) const override;

protected:
    // these are protected to prevent incorrect creation or deletion outside of our subclasses' static CreateInstance methods and our DestroyInstance method
    XRSoundEngine();
    virtual ~XRSoundEngine();

public:
    // XRSound version 2.x 
    virtual EngineType GetEngineType() override = 0;
    virtual const char *GetLogID() override = 0;  // e.g., vessel or module name

    // XRSound version 3.x
    virtual bool  SetPan(const int soundID, const float pan);
    virtual float GetPan(const int soundID);

    virtual bool  SetPlaybackSpeed(const int soundID, const float speed = 1.0);
    virtual float GetPlaybackSpeed(const int soundID);

    virtual bool  SetPlayPosition(const int soundID, const unsigned int positionMillis);
    virtual int   GetPlayPosition(const int soundID);

    // XRSound version 4.1
    // The volume, playback type, and duration methods are implemented in the base class; the settings methods are in XRSoundEngine41.cpp.
    // Every one of these changes or reads only THIS engine's active values (see ActiveValues); none of them ever changes the configuration objects.
    virtual bool SetVolumeByType(const XRSound::VolumeType type, const float level) override;
    virtual bool GetVolumeByType(const XRSound::VolumeType type, float &level) override;
    virtual bool SetSoundVolume(const int soundID, const float volume) override;
    virtual bool GetSoundVolume(const int soundID, float &volume) override;
    virtual bool SetSoundPlaybackType(const int soundID, const XRSound::PlaybackType playbackType) override;
    virtual bool GetSoundPlaybackType(const int soundID, XRSound::PlaybackType &playbackType) override;
    virtual bool GetSoundDuration(const int soundID, unsigned int &durationMillis) override;
    virtual bool SetSettingNumber(const XRSound::Setting setting, const double value) override;
    virtual bool GetSettingNumber(const XRSound::Setting setting, double &value) override;
    virtual bool SetSettingString(const XRSound::Setting setting, const char *pValue) override;
    virtual bool GetSettingString(const XRSound::Setting setting, char *pBuffer, const int bufferLength) override;
    // The configured starting values (read from this engine's own m_pConfig through FillConfiguredValues, never from the active values); implemented in XRSoundEngine41.cpp.
    virtual bool GetDefaultVolumeByType(const XRSound::VolumeType type, float &level) override;
    virtual bool GetDefaultSettingNumber(const XRSound::Setting setting, double &value) override;
    virtual bool GetDefaultSettingString(const XRSound::Setting setting, char *pBuffer, const int bufferLength) override;

    // --------------------------------------------------------------------

    // Methods only invoked by XRSoundDLL (our Orbiter module class).  Because these are non-virtual, it is impossible for XRSoundLib to invoke them
    // because it cannot link with them when the Orbiter vessel using XRSoundLib tries to link.
    static void DestroySoundEngine();     // performs one-time destruction of our sound engine
    static bool IsSoundEngineInitialized();
    static const char *GetSoundDriverName();
    static const char *GetVersionStr();
    static void ResetStaticSimulationData();
    static float ComputeVariableVolume(const double minVolume, const double maxVolume, double level);
    static double ComputeVariableLevel(const double min, const double max, double level);

    // Returns the global (vessel-independent) configuration, which holds the configured defaults parsed from XRSound.cfg.  It is written ONLY by the parser: nothing
    // at runtime (including every XRSound API call) ever assigns a field of it.  It is used for logging and for the DLL-level update scheduling; every other value an
    // engine needs at runtime comes from that engine's own ActiveValues, which are initialized from the engine's configuration when the engine is created.
    // Not inline on purpose: XRSoundConfigFileParser is only a forward declaration on the XRSound.lib side.
    static XRSoundConfigFileParser &GetGlobalConfig();

    // Parses (or re-parses) our global XRSound.cfg into s_globalConfig and logs an error if the parse fails.  This is the one implementation of the global parse;
    // XRSoundDLL::ParseGlobalConfigFile and InitializeSoundEngine both use it.
    static void ParseGlobalConfigFile();

    // The "active" values of one engine instance (one vessel or one module proxy): its own level for every VolumeType and its own value for every Setting.
    // They are initialized once, from the engine's own configuration (m_pConfig, which already contains the [SYSTEM] values from XRSound.cfg because [SYSTEM] cannot be
    // overridden per vessel), right after that configuration is parsed (see InitializeActiveValues).  The XRSound API reads and changes ONLY the calling engine's
    // active values: it never changes the configuration and never affects another vessel or module.  They die with the engine, so each new session starts from the
    // configuration again.  This struct holds only plain data and public enums so that this header can still be compiled on the XRSound.lib side.
    struct ActiveValues
    {
        // Volume levels indexed by XRSound::VolumeType (index 0 is unused).  These are absolute levels (0..1.0), not multipliers on the configured levels:
        // Main starts at the configured MasterVolume, ATC at the configured ATC Volume, Music at the configured MusicVolume, and all the others at 1.0.
        float volumeLevels[static_cast<int>(XRSound::VolumeType::Custom) + 1] = { };

        // This engine's item level and playback type override for the shared (global) MusicFolder slot.  They live here instead of in the slot's WavContext
        // because the slot is shared by all vessels, and a vessel may change only its own values; the music prestep of the vessel that has focus applies its own.
        float musicItemLevel = 1.0f;
        bool bHasMusicPlaybackTypeOverride = false;
        XRSound::PlaybackType musicPlaybackTypeOverride = XRSound::PlaybackType::Global;

        // This engine's music on/off flag (SetDefaultSoundEnabled / GetDefaultSoundEnabled for MusicFolder). Like the levels above it, this is per engine and never stored in the shared
        // music slot's WavContext (bEnabled), so a vessel can disable music only for itself. The focus vessel's music prestep consults this flag and stops the track while it is false.
        bool bMusicEnabled = true;

        // Settings initialized from XRSound.cfg's [SYSTEM] section; each engine keeps and updates its own active values.
        bool SilenceOfSpace = true;
        XRSound::MusicOrder MusicOrder = XRSound::MusicOrder::Random;
        double UpdateInterval = 0.05;
        bool EnableVerboseLogging = false;
        bool LogVesselAnimations = false;
        bool LogThrusterData = false;
        double DisableAutopilotsTimeAccThreshold = 100;
        double MinThrusterLevelForRCSSoundEffects = 0.05;

        // Settings from the vessel-related sections ([SOUND_FILES], [ATC], [VESSEL], [animation_*]); a module proxy accepts and stores them too, but nothing reads them there
        int ATCMinDelay = 15;
        int ATCMaxDelay = 120;
        bool ATCAllowWhileLanded = true;
        bool ATCAllowInAtmosphere = true;
        bool ATCAllowDuringReentry = false;
        double ATCDelayPlanetDistance = 400;
        double ATCDelayPlanetMultiplier = 5;
        int CabinAmbienceMin = 7;
        int CabinAmbienceMax = 35;
        XRSound::MusicPlay MusicPlayInternal = XRSound::MusicPlay::Off;
        XRSound::MusicPlay MusicPlayExternal = XRSound::MusicPlay::Space;
        double WarningGearIsUpAltitude = 275;
        std::string CustomEnginesThrusterIndexes;
        int LandingGearAnimationID = -1;
    };

    // Returns this engine's active values (see ActiveValues).  Every live read of a volume level or setting goes through here.
    ActiveValues &GetActive() { return m_active; }
    const ActiveValues &GetActive() const { return m_active; }

    // Returns this engine's current level for the supplied volume type (the caller guarantees the type is valid).
    float GetActiveVolumeLevel(const XRSound::VolumeType type) const { return m_active.volumeLevels[static_cast<int>(type)]; }

    //=====================================================================
    // non-static methods (which are therefore not callable by XRSoundLib)
    //=====================================================================
    void WriteLog(const char *pMsg);   // can't be const b/c we need to call oapiGetVesselInterface
    void StopAllWav();     
    void SetAllWavPaused(const bool bPaused);     
    
    XRSoundConfigFileParser &GetConfig() { _ASSERTE(m_pConfig);  return *m_pConfig; }
    vector<std::string> GetValidSoundFileExtensions();    // e.g., ".flac", ".wav", ".mp3", etc.
    const char *GetWavFilename(const int soundID);

    // Added in XRSound 4.1 (non-virtual unless noted; these are never called by XRSound.lib)

    // Returns the volume category (Engines..Custom, or Music) for the supplied sound ID; this is the one function that owns the sound ID -> category mapping.
    // Sound IDs that are not default sounds or animation sounds are Custom.
    XRSound::VolumeType CategoryForSoundID(const int soundID) const;

    // Returns true if soundID is one of this engine's animation ("door") sound IDs; the base class (modules) has no animations.  VesselXRSoundEngine overrides this.
    virtual bool IsAnimationSoundID(const int soundID) const;

    // Returns the "mix" volume for a sound: requestedVolume * itemLevel * this engine's category level * this engine's main volume.  This does not include the playback type fades.
    // For the shared music slot, the item level and the category level (Music) are this engine's own, so the result is the volume as the engine playing the music hears it.
    float ComputeMixVolume(const WavContext &context) const;

    // Returns the volume to set on the sound's ISound: by default (modules) the mix volume; VesselXRSoundEngine overrides this to also apply its playback type fades
    // (distance, pressure, view, and focus).
    virtual float ComputeFinalVolume(const WavContext &context);

    // Returns the playback type in effect for the context as heard by THIS engine: the same as context.EffectivePlaybackType(), except that the shared music slot
    // uses this engine's own playback type override for MusicFolder (see ActiveValues).
    XRSound::PlaybackType EffectivePlaybackTypeFor(const WavContext &context) const;

    // Sets the sound's volume to ComputeFinalVolume(context).  With verbose logging on, logs the volume to XRSound.log when the sound starts, and when it changes
    // if bLogChange is true, so that a test or a developer can see the exact volume given to the audio library.  ReapplyVolume passes true (a level, item level,
    // or playback type change), and so does ReapplyMusicVolume when the music changes hands to another vessel; the per-update paths pass false, since the fades
    // and the engine sounds' thrust-following volumes change at every update.  (A PlayWav call that changes the volume of a sound already playing is applied by
    // the per-update path, so it is not logged.)
    void ApplyFinalVolume(const WavContext &context, ISound *pISound, const bool bLogChange);

    // If the context has an ISound that has not finished, sets its volume to ComputeFinalVolume(context).  Never changes the paused, loop, or position state of the sound.
    void ReapplyVolume(WavContext &context);

    // Calls ReapplyVolume for every sound in this engine (not the global music slot).
    void ReapplyAllVolumes();

    // If this engine is the one currently playing the shared music slot (see PlaysMusicSlot), re-applies the volume of the music slot using THIS engine's levels.
    // Invoked with bLogChange true when this engine changes a level or playback type that affects music, and with bLogChange false at every update by the music
    // prestep, so that the slot always plays at the levels of the vessel that has focus (the music's fades can change at every update).  Either way, a volume change
    // is logged when the music changes hands to this engine (a focus change), so that the handover shows in the log.
    void ReapplyMusicVolume(const bool bLogChange);

    // Returns true if this engine is the one that plays the shared (global) music slot right now: a vessel engine whose vessel has focus.  Module engines never play music.
    virtual bool PlaysMusicSlot() const;

    // Returns true if verbose logging is enabled for this engine (its active EnableVerboseLogging value)
    bool VerboseLogging() const;

    // returns Orbiter's camera's global coordinates
    static VECTOR3 GetCameraCoordinates()
    {
        VECTOR3 cameraGlobalCoords;
        oapiCameraGlobalPos(&cameraGlobalCoords);
        return cameraGlobalCoords;
    }

    static bool PauseOrReumseMusic(const bool bPause);

    // static data
    // This is parsed at DLL initialization time so we can write to the log file and read configuration data early-on from our static methods.
    // Holds the configured defaults and is written only by the parser (see GetGlobalConfig()); runtime values live in each engine's ActiveValues.
    static XRSoundConfigFileParser s_globalConfig;

protected:
    static bool InitializeSoundEngine();  // performs one-time static initialization of our sound singleton; also where the global configuration is re-parsed for the new session

    // Initializes this engine's active values (volume levels and settings) from its own m_pConfig.  Each subclass constructor invokes this right after it parses m_pConfig.
    void InitializeActiveValues();

    // Fills the volume levels and settings of 'values' with the configured values from 'config'.  The one source of those values: InitializeActiveValues uses it for the initial
    // active values and GetDefaultVolumeByType / GetDefaultSettingNumber / GetDefaultSettingString use it for the defaults, so the two can never drift apart.  Only reads 'config'.
    static void FillConfiguredValues(const XRSoundConfigFileParser &config, ActiveValues &values);

    // Returns true if type is one of the VolumeType values (Main..Custom); shared by the volume getters, setter, and default getter.
    static bool IsValidVolumeType(const XRSound::VolumeType type);

    // Invoked after a setting changes in this engine's active values, so a subclass can apply side effects such as rescheduling a sound. The base class does nothing.
    virtual void OnSettingChanged(const XRSound::Setting setting);

    //=====================================================================================
    // Protected abstract methods -- these are not exposed via the XRSoundEngine interface.
    //=====================================================================================
    virtual void FreeResources() = 0;  // NOTE: *always* use this method to release XRSoundEngine resources before deleting it; invoked from our static DestroyInstance method.
    virtual void UpdateSoundState(WavContext &context) = 0;

    WavContext *FindWavContext(const int soundID);
    static bool StopWavImpl(WavContext *pContext, XRSoundEngine *pEngine);

    // Returns the WavContext for soundID, creating an empty placeholder (no file loaded, enabled) in this engine's map if none exists yet.
    // This is the "placeholder context" pattern that lets a slot-level value (item level, playback type override, enabled flag) be set before
    // the sound has been loaded; LoadWav carries those slot-level values over into the real context.  The placeholder gets the correct category.
    // Note: the global MusicFolder slot is never placed in the map; FindWavContext always returns the global context for it.
    WavContext *FindOrCreateSlotContext(const int soundID);

    // Returns true if soundID is a default sound, default group, or animation sound ID: these get an item level instead of a stored requested volume,
    // and a playback type override that persists across XRSound's own reloads of the sound.
    bool IsItemLevelSoundID(const int soundID) const;

    // Returns the ISound for the context if it is currently playing or paused (i.e., it exists and has not finished), otherwise nullptr.
    static ISound *GetActiveISound(const WavContext &context);

    // Applies (and clears) the pending pan, speed, and position values to a newly created, not yet started ISound.
    void ApplyPendingStartValues(WavContext &context);

    // Used by SetPan, SetPlaybackSpeed, and SetPlayPosition for MusicFolder: returns the shared music slot's ISound if THIS engine may change it (PlaysMusicSlot() is true, i.e., this is the
    // focus vessel's engine) AND the music is actually playing (or paused), otherwise nullptr.  Those setters never store pending start values for the music slot, so a value set by
    // one vessel can never carry into a track that another vessel starts.  pCaller is only for the log line when the change is refused.
    ISound *GetControllableMusicISound(const char *pCaller);

    // Returns true if the supplied context is the shared (global) music slot, whose static pointer is private to XRSoundEngine.  UpdateSoundState uses this to refuse to touch the slot unless this engine plays it.
    static bool IsMusicSlotContext(const WavContext &context);

    // data
    static ISoundEngine *s_SoundEngine;        // initialized by InitializeSoundEngine
    static bool s_bSoundEngineNeedsInitialization; // used to handle one-time startup items

    // This is per-engine instance instead of static so that we can per-vessel or per-module configuration overrides if we want to.
    // This holds this engine's configured defaults (XRSound.cfg + any vessel class override file), including the [SYSTEM] values.  It is written ONLY by the parser;
    // at runtime read the engine's m_active values instead of the fields of this object (they are initialized from it by InitializeActiveValues).
    XRSoundConfigFileParser *m_pConfig;

    // This engine's active volume levels and settings; the only values the XRSound API changes. See ActiveValues.
    ActiveValues m_active;

    // Defines the master map of all sounds known by this engine instance (i.e., known to this vessel or module).
    // key = soundID, value = WavContext for that soundID
    typedef unordered_map<int, WavContext> HASHMAP_SOUNDID_WAVCONTEXT;
    typedef pair<int, WavContext &> soundID_WavContext_Pair;

    // map of all sound ID -> wav context pairs known by this vessel
    HASHMAP_SOUNDID_WAVCONTEXT m_allWavsMap;

private:
    static WavContext *s_pMusicFolderWavContext;  // our special, global MusicFolder sound
    static const XRSoundEngine *s_pMusicVolumeEngine;   // the engine that applied the music slot's volume last (see ReapplyMusicVolume); only compared, never dereferenced
    static std::string s_csVersion;
    std::string m_lastLogLine;  // last log message written to XRSound.log
    double m_nextDuplicateLogLineSimt;
};

// this macro is designed to be use with an XRSoundEngine instance (reference)
// The format string is part of the variadic arguments so that a message with no arguments forwards cleanly to FormatString.
#define VERBOSE_LOG(pEngine, ...)                   \
{                                                   \
    if (pEngine->VerboseLogging())  /* this engine's active EnableVerboseLogging value */ \
    {                                               \
        /* FormatString sizes the result to fit, so long file paths / sound descriptions are never truncated */ \
        const std::string verboseLogMsg = FormatString(__VA_ARGS__); \
        pEngine->WriteLog(verboseLogMsg.c_str());   \
    }                                               \
}

// utility macros
#define MPS_TO_KNOTS(mps)   (mps / 0.5148)    /* meters-per-second to knots */
#define KNOTS_TO_MPS(knots) (knots * 0.5148)  /* knots to meters-per-second */

// These are used to dynamically bind to DLL-exported methods.  Note that DLLCLBK specifies 'extern "C"', which uses the __cdecl
// calling convention, NOT the normal __stdcall that C++ uses.
extern "C" typedef XRSoundEngine* (__cdecl* VesselXRSoundEngineInstanceFuncPtr)(OBJHANDLE hVessel);
extern "C" typedef XRSoundEngine* (__cdecl* ModuleXRSoundEngineInstanceFuncPtr)(const char* pUniqueModuleName);

