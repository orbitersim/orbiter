// ==============================================================
// XRSoundApiTest.h : the test module class, and the helpers that the XRSoundApiTest source files share.  See XRSoundApiTest.cpp.
// ==============================================================

#pragma once

#define STRICT
#include <cmath>
#include <functional>
#include <limits>
#include <string>
#include <vector>
#include "Orbitersdk.h"
#include "XRSound.h"

using VT = XRSound::VolumeType;
using SS = XRSound::Setting;
using PT = XRSound::PlaybackType;

static const char *MODULE_NAME = "XRSoundApiTest";     // also this module's XRSound proxy name, which XRSound.log shows as its log ID
static const char *SECOND_VESSEL_NAME = "GL-02";        // the scenario's second XRSound vessel
static const char *THIRD_VESSEL_NAME = "GL-03";         // the scenario's third XRSound vessel, which the ATC and gear-up warning checks move around
static const double THIRD_VESSEL_SITE_LNG = -80.6946;  // where the third vessel lands (degrees): the Shuttle Landing Facility at Cape Canaveral
static const double THIRD_VESSEL_SITE_LAT = 28.6150;
static const int DG_GEAR_MOVING_SOUND_ID = 12003;      // XRSound-DeltaGlider.cfg, [animation_7] MovingSoundID: the landing gear sound while it moves
static const char *TEST_FOLDER = "XRSound\\ApiTest";   // the test tones; also used as a sound group folder
static const char *FILE_WAV = "XRSound\\ApiTest\\tone12.wav";
static const char *FILE_OGG = "XRSound\\ApiTest\\tone12.ogg";
static const char *FILE_MP3 = "XRSound\\ApiTest\\tone12.mp3";
static const char *FILE_FLAC = "XRSound\\ApiTest\\tone12.flac";
static const char *XRSOUND_LOG = "XRSound.log";
static const char *XRSOUND_CFG = "XRSound\\XRSound.cfg";

extern int g_pass, g_fail, g_skip;     // the check counts for the summary (TestSupport.cpp)

// TestSupport.cpp
std::string Fmt(const char *format, ...);
void Log(const std::string &line);
void Report(const bool ok, const std::string &name, const std::string &details = "");
void Skip(const std::string &name, const char *reason);
bool Near(const double a, const double b, const double tol);
const char *VolName(const VT t);
int WavHeaderMillis(const char *path);
double ReadXRSoundCfgLevel(const char *section, const char *name, const double defaultLevel);
std::string VolStr(const double v);
bool LoggedNear(const double logged, const double expected);

static const double NaNd = std::numeric_limits<double>::quiet_NaN();
static const float NaNf = std::numeric_limits<float>::quiet_NaN();

static const VT ALL_VOLUME_TYPES[] = { VT::Main, VT::Engines, VT::RCS, VT::Effects, VT::Callouts, VT::CabinAmbience, VT::ATC, VT::Music, VT::Custom };

// ---------------------------------------------------------------------------------------------------------
// XRSound.log reader for the volume checks
// ---------------------------------------------------------------------------------------------------------
class XRSoundLogReader
{
public:
    void SkipToEnd();
    void Read();
    double LastVolume(const char *logID, const int soundID) const;
    std::string LastStartedFile(const char *logID, const int soundID) const;
    bool AnyLineContains(const std::string &a, const std::string &b) const;

private:
    std::streamoff m_offset = 0;
    std::vector<std::string> m_lines;
};

// ---------------------------------------------------------------------------------------------------------
// Settings tables
// ---------------------------------------------------------------------------------------------------------
struct NumSetting { SS s; const char *name; std::vector<double> valid; std::vector<double> invalid; };

std::vector<NumSetting> MakeNumSettings();     // TestSupport.cpp

// The proxies under test: [0] the focus vessel, [1] this module, [2] the second XRSound vessel (null if there is none).
extern XRSound *g_proxies[3];
extern const char *g_proxyLabels[3];

// ---------------------------------------------------------------------------------------------------------
// The test module
// ---------------------------------------------------------------------------------------------------------
enum Stage
{
    ST_WARMUP, ST_SYNC, ST_VOL_WAIT, ST_STEPS_BEFORE_PAUSE, ST_MODULE_SOUND, ST_MOD_WAIT,
    ST_PAUSE_WAIT, ST_IN_PAUSE, ST_RESUME_FIRST, ST_RESUME_WAIT, ST_MUSIC, ST_STEPS_AFTER_MUSIC, ST_LEGACY_WAIT, ST_DONE
};

// a default sound for the routing stage, and the volume type it must belong to
struct RoutingTarget { int id; const char *name; VT type; PT origType; bool done = false; };

// a check that runs over several frames: start is called once, then tick every frame (including the first) until it returns true
struct Step
{
    const char *name;
    std::function<void(ULONGLONG)> start;
    std::function<bool(ULONGLONG)> tick;
};

// sound IDs this test loads (any IDs that are not XRSound default sound IDs)
static const int SND_VOL = 100;         // vessel: per-sound volume and loop position
static const int SND_PRE = 101;         // vessel: pre-start pan, speed, and position
static const int SND_DUR_WAV = 110, SND_DUR_OGG = 111, SND_DUR_MP3 = 112, SND_DUR_FLAC = 113;
static const int SND_PAUSE = 120;       // vessel: simulation pause
static const int SND_MISSING = 130;     // vessel: a file that does not exist
static const int SND_MIX = 140;         // both vessels: the volume mix checks
static const int SND_MOD = 200;         // module sound
static const int SND_FADE = 150;        // both vessels: the playback type fade checks
static const int SND_MOD_MIX = 201;     // module: the volume mix checks

class TestModule : public oapi::Module
{
public:
    TestModule(HINSTANCE h) : oapi::Module(h) {}
    ~TestModule() override { DeleteProxies(); }

    void clbkSimulationStart(RenderMode) override;
    void clbkSimulationEnd() override;
    void clbkPause(bool pause) override;
    bool clbkProcessKeyboardImmediate(char *, bool) override;
    void clbkPreStep(double simt, double, double) override;

private:
    // the ATC conditions step's cases (BuiltInSounds.cpp)
    enum class Situation { Landed, Atmosphere, Reentry };

    struct ATCCase { const char *name; Situation situation; SS allowSetting; double allow; double planetDistance; double multiplier; double minMs; double maxMs; bool expectPlay; };

    // the music in the atmosphere step's cases (Music.cpp)
    struct MusicAtmosphereCase { const char *name; bool cockpit; SS setting; XRSound::MusicPlay value; bool expectMusic; };

    XRSound *m_ves = nullptr;           // proxy for the focus vessel
    XRSound *m_mod = nullptr;           // proxy for this module
    XRSound *m_ves2 = nullptr;          // proxy for the second XRSound vessel (not the focus vessel)
    VESSEL *m_vessel = nullptr;         // the focus vessel
    VESSEL *m_vessel2 = nullptr;        // the second vessel
    XRSound *m_ves3 = nullptr;          // proxy for the third XRSound vessel (null if there is none)
    VESSEL *m_vessel3 = nullptr;        // the third vessel
    bool m_legacy = false;
    bool m_summaryDone = false;
    Stage m_stage = ST_WARMUP;
    ULONGLONG m_tStage = 0;
    XRSoundLogReader m_log;
    XRSoundLogReader m_stepLog;         // a second reader, for the steps' own lines (m_log is used by the volume checks inside them)
    XRSoundLogReader m_offLog;          // a third reader, for the logging-off checks, which span the ambient and update interval steps
    double m_gearAnimStart = -1;
    // steps
    std::vector<Step> m_steps;
    size_t m_stepIndex = 0;
    bool m_stepStarted = false;
    ULONGLONG m_tStep = 0;
    // per-sound volume stage
    ULONGLONG m_tVol1 = 0, m_tVol2 = 0;
    int m_pVol1 = 0, m_pVol2 = 0;
    // default sound routing stage
    std::vector<RoutingTarget> m_routing;
    int m_origAttitudeMode = RCS_ROT;
    bool m_rcsGateOpen = false, m_rcsPlayedBelowThreshold = false;
    // ambient step
    std::string m_origCabinFolder, m_origATCFolder;
    bool m_ambientCabinDone = false, m_ambientATCDone = false, m_ambientMusicDone = false, m_ambientAirDone = false;
    // update interval and time acceleration steps
    int m_updatePhase = 0;
    std::vector<ULONGLONG> m_updateTimes;
    bool m_customPlayed = false;
    bool m_timeAccRaised = false;
    ULONGLONG m_tTimeAcc = 0;
    // ATC conditions and gear-up warning steps (the third vessel)
    size_t m_atcCase = 0;
    int m_atcPhase = 0;
    ULONGLONG m_tATCCase = 0;
    std::string m_origATCFolder3;
    VECTOR3 m_siteR = { 0, 0, 0 }, m_siteV = { 0, 0, 0 };   // the third vessel's landed site: position and velocity relative to Earth
    bool m_holdThird = false;
    VESSELSTATUS2 m_holdStatus = {};
    int m_gearPhase = 0;
    double m_gearAnimID = -1;
    bool m_gearWarned = false;
    // music settings steps
    int m_musicPhase = 0;
    std::string m_origMusicFolder, m_origMusicFolder2;
    std::vector<std::string> m_musicPicks;
    // module sound
    int m_pMod0 = 0;
    // pause stage
    int m_p0 = 0, m_pA = 0, m_pB = 0, m_pMid = 0, m_pC = 0, m_pR = 0;
    ULONGLONG m_tPauseReq = 0, m_tPauseHook = 0, m_tResumeReq = 0, m_tR = 0;
    bool m_pauseHookStarted = false, m_pauseActionsDone = false, m_pauseMidDone = false;
    int m_hookPausedCalls = 0;
    // music stage
    int m_musicState = 0;
    ULONGLONG m_tMusic = 0;
    int m_musicPos0 = 0;
    bool m_musicStoppedSetsRefused = false;
    std::string m_musicStoppedSets;
    double m_musicRequested = 0;        // the music's own requested volume, measured with every level at 1.0
    // legacy
    int m_legP0 = 0;

    // XRSoundApiTest.cpp: the callbacks, the proxies, the summary, the step runner, and legacy mode
    void DeleteProxies();
    bool CreateProxies();
    void Summary();

    bool RunSteps(const ULONGLONG now);
    void SetSteps(std::vector<Step> steps);
    std::vector<Step> StepsBeforePause();
    std::vector<Step> StepsAfterMusic();
    void PrepareLevels();
    static void RestoreSetting(XRSound *p, const SS s);
    static void RestoreGroupFolder(XRSound *p, const XRSound::DefaultSoundID group, const std::string &origFolder);
    void RestoreDefaultLevels();

    void RunLegacyStart();
    void FinishLegacy();

    // LevelsAndSettings.cpp: version, volume types, settings, default sound volumes, sound group folders, and the volume mix
    void RunSyncChecks();
    void TestVolumeTypes();
    void TestSettings();
    void TestIntOverload();
    void TestSoundVolumeSync();
    void TestGroupFolders();

    void RunMixChecks();

    // Playback.cpp: per-sound volume, pan, speed, and position, durations, playback type fades, the module sound, and simulation pause
    void StartVolumeStage();
    void FinishVolumeStage(const ULONGLONG now);

    void RunPreStart();

    void RunDuration();

    double CameraDistance(VESSEL *pVessel) const;
    static double ExpectedFade(const PT type, const bool silenceOfSpace, const double cameraDistance, const double pressure);
    double StartedVolume(XRSound *p, const char *logID, const int soundID);
    void CheckFades(XRSound *p, VESSEL *pVessel, const char *situation);
    void RunFadeChecks();

    void StartAtmosphereFadeStep();
    bool AtmosphereFadeTick(const ULONGLONG now);

    void StartModuleSound();
    void FinishModuleSound();

    void StartPauseStage();
    void RequestPause(const ULONGLONG now);
    void PauseTick(const ULONGLONG now);

    // BuiltInSounds.cpp: the default sounds: routing to volume types, cabin ambience and ATC, the update interval, time acceleration, ATC conditions, and the gear-up warning
    void StartRoutingStage();
    bool RoutingTick(const ULONGLONG now);
    void RestoreCustomEngines();
    void StartLoggingOffChecks();
    void FinishLoggingOffChecks(const bool mainEngineFired);
    static double GearAnimationState(VESSEL *pVessel, XRSound *p);
    bool RoutingTargetDone(const int soundID) const;
    void CheckRouting(const RoutingTarget &r);

    void StartAmbientStep();
    bool AmbientTick(const ULONGLONG now);

    void StartUpdateIntervalStep();
    bool UpdateIntervalTick(const ULONGLONG now);

    void StartTimeAccStep();
    bool TimeAccTick(const ULONGLONG now);
    bool FinishTimeAccStep();

    static const std::vector<ATCCase> &ATCCases();
    void StartATCConditionsStep();
    bool ATCConditionsTick(const ULONGLONG now);
    bool FinishATCConditionsStep();
    void PlaceThirdVessel(const Situation situation, const bool landedAtSite);
    VESSELSTATUS2 AirborneStatus(const double altitude, const double airspeedEast, const double verticalSpeed) const;

    void StartGearWarningStep();
    bool GearWarningTick(const ULONGLONG now);

    // Music.cpp: the shared music track, and the music settings
    // True if the shared music is playing and not paused.  Where MusicPlayInternal or MusicPlayExternal says no music, XRSound pauses the
    // track (to resume it later) rather than stopping it, and IsWavPlaying is true for a paused sound.
    bool MusicAudible() const { return MusicAudible(m_ves); }
    static bool MusicAudible(XRSound *p);
    void StartMusicExternalStep();
    bool MusicExternalTick(const ULONGLONG now);
    void StartMusicOrderStep(const ULONGLONG now);
    void NextMusicTrack(const ULONGLONG now);
    bool MusicOrderTick(const ULONGLONG now);
    bool FinishMusicOrderStep();

    void StartMusicFolderStep(const ULONGLONG now);
    bool MusicFolderTick(const ULONGLONG now);

    static const std::vector<MusicAtmosphereCase> &MusicAtmosphereCases();
    void StartMusicAtmosphereStep(const ULONGLONG now);
    void StartMusicAtmosphereCase(const ULONGLONG now);
    bool MusicAtmosphereTick(const ULONGLONG now);

    void MusicTick(const ULONGLONG now);
    void StartMusicLevelStage(const ULONGLONG now);
    void FinishMusicStage();
    void StartMusicLevelChecks();
    void CheckMusicLevelsAfterFocusChange(const ULONGLONG now, VESSEL *pFocus, const double levelProduct, const char *checkName, const int nextState);
};
