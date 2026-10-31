// ==============================================================
// XRSoundApiTest.cpp : manual developer test for the XRSound 4.1 API.
//
// This is an Orbiter plugin module that calls the XRSound API the way an add-on does, from a running Orbiter: it needs a real
// sound engine, real vessels, and real simulation frames, so it cannot run as a unit test.  See README.md for how to run it.
//
// This file has the callbacks, the stage machine, the step runner, and legacy mode.  The checks are in LevelsAndSettings.cpp,
// Playback.cpp, BuiltInSounds.cpp, and Music.cpp, and the helpers they share are in TestSupport.cpp.
//
// Every check writes one line to Orbiter.log:
//     XRSOUNDTEST PASS|FAIL|SKIP <check> <details>
// and the run ends with one "XRSOUNDTEST SUMMARY mode=... pass=... fail=... skip=..." line.  The checks run from a small state
// machine driven by clbkPreStep (no sleeps), plus clbkProcessKeyboardImmediate, which Orbiter calls every frame even while the
// simulation is paused, but only while its window is active (for the pause checks).  Elapsed real time is measured with GetTickCount64.
//
// The volume checks read XRSound.log: with EnableVerboseLogging on, XRSound logs the volume that it gives to the audio library when a
// sound starts, and when a level, item level, playback type, or music focus change changes it ("ApplyFinalVolume: volume = ...").
// That is the only way to see the actual volume, since the API returns only the levels that went into it.
//
// With an XRSound.dll older than 4.1 (e.g., 4.0), the test runs in "legacy" mode instead: every 4.1 call must return false
// without crashing, and the older calls must still work.
// ==============================================================

#define ORBITER_MODULE
#include <cstring>
#include "XRSoundApiTest.h"

void TestModule::clbkSimulationStart(RenderMode)
{
    Log("XRSOUNDTEST INFO clbkSimulationStart");
    if (m_stage != ST_WARMUP)
    {
        // the checks assume fresh XRSound state, so they run once per Orbiter start
        Log("XRSOUNDTEST INFO the test already ran in this Orbiter session; exit and restart Orbiter to run it again");
        m_stage = ST_DONE;
    }
}

void TestModule::clbkSimulationEnd()
{
    if (!m_summaryDone)
        Log(Fmt("XRSOUNDTEST SUMMARY INCOMPLETE mode=%s stage=%d pass=%d fail=%d skip=%d", m_legacy ? "legacy" : "full", static_cast<int>(m_stage), g_pass, g_fail, g_skip));
    DeleteProxies();
}

void TestModule::clbkPause(bool pause)
{
    Log(Fmt("XRSOUNDTEST INFO clbkPause(%d) stage=%d tick=%llu", pause ? 1 : 0, static_cast<int>(m_stage), static_cast<unsigned long long>(GetTickCount64())));
}

// Orbiter calls this every frame while its window is active, even while the simulation is paused, so it drives the pause stage.  If the
// window is not active, the pause stage waits; un-pausing by hand (Ctrl+P) makes clbkPreStep report the pause checks as failed and go on.
bool TestModule::clbkProcessKeyboardImmediate(char *, bool)
{
    if ((m_stage == ST_IN_PAUSE) && oapiGetPause())
    {
        ++m_hookPausedCalls;
        PauseTick(GetTickCount64());
    }
    return false;
}

void TestModule::clbkPreStep(double simt, double, double)
{
    const ULONGLONG now = GetTickCount64();
    if (m_holdThird && m_vessel3)
        m_vessel3->DefSetStateEx(&m_holdStatus);    // hold the third vessel in its airborne situation (see PlaceThirdVessel)
    for (bool again = true; again;)
    {
        again = false;
        switch (m_stage)
        {
        case ST_WARMUP:
            if (simt < 2.0) return;     // let XRSound load every vessel's sounds first
            if (!CreateProxies()) { m_stage = ST_DONE; Summary(); return; }
            m_legacy = (m_ves->GetVersion() < 4.05f);
            Log(Fmt("XRSOUNDTEST MODE %s version=%.3f", m_legacy ? "legacy" : "full", m_ves->GetVersion()));
            if (m_legacy) { RunLegacyStart(); m_stage = ST_LEGACY_WAIT; m_tStage = now; return; }
            m_stage = ST_SYNC; again = true; break;
        case ST_SYNC:
            RunSyncChecks();
            StartVolumeStage();
            m_stage = ST_VOL_WAIT; m_tStage = GetTickCount64(); return;     // after the synchronous checks, which can take a while: the volume stage measures its wait from here
        case ST_VOL_WAIT:
            if (now - m_tStage < 700) return;
            FinishVolumeStage(now);
            RunPreStart();
            RunDuration();
            RunMixChecks();
            SetSteps(StepsBeforePause());
            m_stage = ST_STEPS_BEFORE_PAUSE; return;
        case ST_STEPS_BEFORE_PAUSE:
            if (!RunSteps(now)) return;
            m_stage = ST_MODULE_SOUND; again = true; break;
        case ST_MODULE_SOUND:
            StartModuleSound();
            m_stage = ST_MOD_WAIT; m_tStage = now; return;
        case ST_MOD_WAIT:
            if (now - m_tStage < 600) return;
            FinishModuleSound();
            StartPauseStage();
            m_stage = ST_PAUSE_WAIT; m_tStage = now; return;
        case ST_PAUSE_WAIT:
            if (now - m_tStage < 600) return;
            RequestPause(now);
            m_stage = ST_IN_PAUSE; m_pauseHookStarted = false; m_pauseActionsDone = false; m_pauseMidDone = false;
            return;
        case ST_IN_PAUSE:
            // The keyboard hook drives this stage while the simulation is paused.  Not paused here means that the pause never took effect, or that
            // it was ended by hand (Ctrl+P) before the hook could finish the pause checks (the window was not active): report that, and carry on.
            if (!oapiGetPause() && (now - m_tPauseReq > 3000))
            {
                Report(false, "pause.checks_ran", Fmt("the simulation is not paused %llu ms after oapiSetPause(true), and the pause checks did not finish (keyboard-hook calls while paused: %d); "
                                                      "if the Orbiter window was not active, the pause was ended by hand", static_cast<unsigned long long>(now - m_tPauseReq), m_hookPausedCalls));
                m_ves->StopWav(SND_PAUSE);
                m_stage = ST_MUSIC; m_musicState = 0; m_tStage = now;
            }
            return;
        case ST_RESUME_FIRST:
        {
            const int pR = m_ves->GetPlayPosition(SND_PAUSE);
            const bool playing = m_ves->IsWavPlaying(SND_PAUSE);
            m_pR = pR; m_tR = now;
            Report(playing && (pR >= m_pC - 50) && (pR - m_pC <= 400), "pause.resume_no_jump",
                   Fmt("posAtPauseEnd=%d posAtFirstPrestepAfterResume=%d playing=%d wallSinceResumeRequest=%llu ms", m_pC, pR, playing, static_cast<unsigned long long>(now - m_tResumeReq)));
            m_stage = ST_RESUME_WAIT; m_tStage = now; return;
        }
        case ST_RESUME_WAIT:
            if (now - m_tStage < 600) return;
            {
                const int pF = m_ves->GetPlayPosition(SND_PAUSE);
                Report(m_ves->IsWavPlaying(SND_PAUSE) && (pF - m_pR >= 300) && (pF - m_pR <= 1500), "pause.resume_advances",
                       Fmt("posFirst=%d posAfter=%d wallMs=%llu", m_pR, pF, static_cast<unsigned long long>(now - m_tR)));
                m_ves->StopWav(SND_PAUSE);
            }
            m_stage = ST_MUSIC; m_musicState = 0; m_tStage = now; return;
        case ST_MUSIC:
            MusicTick(now);
            return;
        case ST_STEPS_AFTER_MUSIC:
            if (!RunSteps(now)) return;
            m_stage = ST_DONE; Summary(); return;
        case ST_LEGACY_WAIT:
            if (now - m_tStage < 600) return;
            FinishLegacy();
            m_stage = ST_DONE; Summary(); return;
        case ST_DONE:
            return;
        }
    }
}

void TestModule::DeleteProxies()
{
    delete m_ves; m_ves = nullptr;
    delete m_mod; m_mod = nullptr;
    delete m_ves2; m_ves2 = nullptr;
    delete m_ves3; m_ves3 = nullptr;
    m_holdThird = false;
    g_proxies[0] = g_proxies[1] = g_proxies[2] = nullptr;
}

bool TestModule::CreateProxies()
{
    m_vessel = oapiGetFocusInterface();
    if (!m_vessel) { Report(false, "init.focus_vessel", "no focus vessel"); return false; }
    m_ves = XRSound::CreateInstance(m_vessel);
    m_mod = XRSound::CreateInstance(MODULE_NAME);
    // the second vessel: the scenario's GL-02, or else any other vessel
    OBJHANDLE h2 = oapiGetVesselByName(const_cast<char *>(SECOND_VESSEL_NAME));
    if (h2) m_vessel2 = oapiGetVesselInterface(h2);
    for (DWORD i = 0; !m_vessel2 && (i < oapiGetVesselCount()); ++i)
    {
        VESSEL *v = oapiGetVesselInterface(oapiGetVesselByIndex(i));
        if (v && (v != m_vessel)) m_vessel2 = v;
    }
    if (m_vessel2 == m_vessel) m_vessel2 = nullptr;
    if (m_vessel2)
    {
        m_ves2 = XRSound::CreateInstance(m_vessel2);
        Log(Fmt("XRSOUNDTEST INFO second vessel '%s' class '%s' proxy present=%d", m_vessel2->GetName(), m_vessel2->GetClassName(), m_ves2 && m_ves2->IsPresent()));
    }
    else Log("XRSOUNDTEST INFO no second vessel found");
    OBJHANDLE h3 = oapiGetVesselByName(const_cast<char *>(THIRD_VESSEL_NAME));
    m_vessel3 = h3 ? oapiGetVesselInterface(h3) : nullptr;
    if (m_vessel3 && ((m_vessel3 == m_vessel) || (m_vessel3 == m_vessel2))) m_vessel3 = nullptr;
    if (m_vessel3)
    {
        m_ves3 = XRSound::CreateInstance(m_vessel3);
        if (!m_ves3->IsPresent()) { delete m_ves3; m_ves3 = nullptr; }
    }
    Log(Fmt("XRSOUNDTEST INFO third vessel '%s' proxy present=%d", THIRD_VESSEL_NAME, m_ves3 != nullptr));
    g_proxies[0] = m_ves; g_proxies[1] = m_mod; g_proxies[2] = (m_ves2 && m_ves2->IsPresent()) ? m_ves2 : nullptr;
    Log(Fmt("XRSOUNDTEST INFO focus vessel '%s' class '%s'", m_vessel->GetName(), m_vessel->GetClassName()));
    Report(m_ves && m_ves->IsPresent(), "init.vessel_proxy_present");
    Report(m_mod && m_mod->IsPresent(), "init.module_proxy_present");
    return m_ves && m_mod && m_ves->IsPresent() && m_mod->IsPresent();
}

void TestModule::Summary()
{
    if (m_summaryDone) return;
    m_summaryDone = true;
    Log(Fmt("XRSOUNDTEST SUMMARY mode=%s pass=%d fail=%d skip=%d", m_legacy ? "legacy" : "full", g_pass, g_fail, g_skip));
}

// =====================================================================================================
// Steps: the checks that need XRSound to react to the simulation over several frames.  Each step has a start function, called once,
// and a tick function, called every frame (including the frame it starts) until it returns true.
// =====================================================================================================
// Runs the current step list; returns true when every step is done
bool TestModule::RunSteps(const ULONGLONG now)
{
    while (m_stepIndex < m_steps.size())
    {
        Step &step = m_steps[m_stepIndex];
        if (!m_stepStarted)
        {
            Log(Fmt("XRSOUNDTEST INFO step '%s' starting", step.name));
            m_stepStarted = true;
            m_tStep = now;
            step.start(now);
        }
        if (!step.tick(now))
            return false;
        ++m_stepIndex;
        m_stepStarted = false;
    }
    return true;
}

void TestModule::SetSteps(std::vector<Step> steps)
{
    m_steps = std::move(steps);
    m_stepIndex = 0;
    m_stepStarted = false;
}

// The steps that run before the module sound and pause stages
std::vector<Step> TestModule::StepsBeforePause()
{
    return {
        { "routing", [this](ULONGLONG) { StartRoutingStage(); }, [this](ULONGLONG now) { return RoutingTick(now); } },
        { "ambient", [this](ULONGLONG) { StartAmbientStep(); }, [this](ULONGLONG now) { return AmbientTick(now); } },
        { "update interval", [this](ULONGLONG) { StartUpdateIntervalStep(); }, [this](ULONGLONG now) { return UpdateIntervalTick(now); } },
        { "time acceleration", [this](ULONGLONG) { StartTimeAccStep(); }, [this](ULONGLONG now) { return TimeAccTick(now); } },
        { "fades", [this](ULONGLONG) { RunFadeChecks(); }, [](ULONGLONG) { return true; } },
        { "ATC conditions", [this](ULONGLONG) { StartATCConditionsStep(); }, [this](ULONGLONG now) { return ATCConditionsTick(now); } },
        { "gear-up warning", [this](ULONGLONG) { StartGearWarningStep(); }, [this](ULONGLONG now) { return GearWarningTick(now); } },
    };
}

// The steps that run after the music stage
std::vector<Step> TestModule::StepsAfterMusic()
{
    return {
        { "music external", [this](ULONGLONG) { StartMusicExternalStep(); }, [this](ULONGLONG now) { return MusicExternalTick(now); } },
        { "music order", [this](ULONGLONG now) { StartMusicOrderStep(now); }, [this](ULONGLONG now) { return MusicOrderTick(now); } },
        { "music folder per vessel", [this](ULONGLONG now) { StartMusicFolderStep(now); }, [this](ULONGLONG now) { return MusicFolderTick(now); } },
        { "music in the atmosphere", [this](ULONGLONG now) { StartMusicAtmosphereStep(now); }, [this](ULONGLONG now) { return MusicAtmosphereTick(now); } },
        { "fades in the atmosphere", [this](ULONGLONG) { StartAtmosphereFadeStep(); }, [this](ULONGLONG now) { return AtmosphereFadeTick(now); } },
    };
}

// Turns verbose logging on and sets every level to 1.0 on both vessels and the module, for a check that reads the volumes from XRSound.log
void TestModule::PrepareLevels()
{
    for (int idx = 0; idx < 3; ++idx)
    {
        if (!g_proxies[idx]) continue;
        g_proxies[idx]->SetSetting(SS::EnableVerboseLogging, 1);
        for (const VT t : ALL_VOLUME_TYPES)
            g_proxies[idx]->SetVolume(t, 1.0f);
    }
}

// Sets a numeric setting of proxy p back to its configured value
void TestModule::RestoreSetting(XRSound *p, const SS s)
{
    double d = 0;
    if (p->GetDefaultSetting(s, d))
        p->SetSetting(s, d);
}

// Sets a sound group's folder back.  A group that had no folder (disabled in the configuration) keeps the test folder: the API cannot
// remove a folder.
void TestModule::RestoreGroupFolder(XRSound *p, const XRSound::DefaultSoundID group, const std::string &origFolder)
{
    if (!origFolder.empty())
        p->SetDefaultSoundGroupFolder(group, origFolder.c_str());
}

// Puts every volume type of both vessels and the module back to its configured level, and EnableVerboseLogging back to its configured value
void TestModule::RestoreDefaultLevels()
{
    for (int idx = 0; idx < 3; ++idx)
    {
        if (!g_proxies[idx]) continue;
        for (const VT t : ALL_VOLUME_TYPES)
        {
            float def = 1.0f;
            if (g_proxies[idx]->GetDefaultVolume(t, def))
                g_proxies[idx]->SetVolume(t, def);
        }
        double verbose = 0;
        g_proxies[idx]->GetDefaultSetting(SS::EnableVerboseLogging, verbose);
        g_proxies[idx]->SetSetting(SS::EnableVerboseLogging, verbose);
    }
}

// =====================================================================================================
// Legacy mode (an XRSound.dll older than 4.1): every 4.1 call returns false without crashing, and the older calls still work
// =====================================================================================================
void TestModule::RunLegacyStart()
{
    const float v = m_ves->GetVersion();
    Report((v < 4.05f) && (v > 0), "legacy.version_is_old", Fmt("GetVersion=%.3f", v));
    float f = -9;
    double d = -9;
    unsigned u = 0xDEADBEEF;
    PT pt = PT::Wind;
    char buf[16] = "keep";
    Report(!m_ves->SetVolume(VT::Main, 0.5f) && !m_ves->GetVolume(VT::Main, f) && !m_ves->GetDefaultVolume(VT::Main, f) && (f == -9), "legacy.volume_type_false", Fmt("f=%g", f));
    Report(!m_mod->SetVolume(VT::Main, 0.5f) && !m_mod->GetVolume(VT::Custom, f) && !m_mod->GetDefaultVolume(VT::Music, f) && (f == -9), "legacy.volume_type_false_module", Fmt("f=%g", f));
    Report(!m_ves->SetVolume(SND_VOL, 0.5f) && !m_ves->GetVolume(SND_VOL, f) && (f == -9), "legacy.sound_volume_false", Fmt("f=%g", f));
    Report(!m_ves->SetVolume(XRSound::MainEngines, 0.5f) && !m_ves->GetVolume(XRSound::MainEngines, f) && (f == -9), "legacy.default_sound_volume_false", Fmt("f=%g", f));
    Report(!m_ves->SetPlaybackType(SND_VOL, PT::Radio) && !m_ves->GetPlaybackType(SND_VOL, pt) && (pt == PT::Wind), "legacy.playback_type_false", Fmt("pt=%d", static_cast<int>(pt)));
    Report(!m_ves->GetDuration(SND_VOL, u) && (u == 0xDEADBEEF), "legacy.duration_false", Fmt("u=0x%X", u));
    bool anyNum = false, anyGet = false, anyStr = false, anyGetStr = false;
    const int settingCount = static_cast<int>(MakeNumSettings().size()) + 2;    // the numeric settings, CustomEnginesThrusterIndexes, and one past the last
    for (int i = 1; i <= settingCount; ++i)
    {
        const SS s = static_cast<SS>(i);
        anyNum |= m_ves->SetSetting(s, 1.0) || m_mod->SetSetting(s, 1.0) || m_ves->SetSetting(s, 1);
        anyGet |= m_ves->GetSetting(s, d) || m_mod->GetSetting(s, d);
        anyStr |= m_ves->SetSetting(s, "0") || m_mod->SetSetting(s, ".wav");
        anyGetStr |= m_ves->GetSetting(s, buf, sizeof(buf)) || m_mod->GetSetting(s, buf, sizeof(buf));
        anyGet |= m_ves->GetDefaultSetting(s, d) || m_mod->GetDefaultSetting(s, d);
        anyGetStr |= m_ves->GetDefaultSetting(s, buf, sizeof(buf)) || m_mod->GetDefaultSetting(s, buf, sizeof(buf));
    }
    Report(!anyNum && !anyGet && !anyStr && !anyGetStr && (d == -9) && !strcmp(buf, "keep"), "legacy.settings_false_all",
           Fmt("settings=%d num=%d get=%d str=%d getStr=%d d=%g buf='%s'", settingCount, anyNum, anyGet, anyStr, anyGetStr, d, buf));
    // older calls
    const bool ld = m_ves->LoadWav(SND_VOL, FILE_WAV, PT::Global);
    const bool pl = m_ves->PlayWav(SND_VOL, true);        // XRSound.h turns UseStoredVolume into 1.0 for an old DLL
    Report(ld && pl && m_ves->IsWavPlaying(SND_VOL), "legacy.loadwav_playwav_default_volume", Fmt("load=%d play=%d playing=%d", ld, pl, m_ves->IsWavPlaying(SND_VOL)));
    m_legP0 = m_ves->GetPlayPosition(SND_VOL);
    const bool sp = m_ves->SetPan(SND_VOL, -0.5f), ss = m_ves->SetPlaybackSpeed(SND_VOL, 1.25f);
    Report(sp && ss && Near(m_ves->GetPan(SND_VOL), -0.5, 0.02) && Near(m_ves->GetPlaybackSpeed(SND_VOL), 1.25, 0.02), "legacy.pan_speed_while_playing",
           Fmt("pan set=%d get=%g speed set=%d get=%g", sp, m_ves->GetPan(SND_VOL), ss, m_ves->GetPlaybackSpeed(SND_VOL)));
    const bool ldb = m_ves->LoadWav(SND_PRE, FILE_WAV, PT::Global);
    const bool stopped = m_ves->SetPan(SND_PRE, 0.5f);
    Report(ldb && !stopped, "legacy.setpan_while_stopped_false", Fmt("load=%d set=%d (old behavior: false for a sound that is not playing)", ldb, stopped));
    Report((m_ves->GetPan(SND_PRE) == -100.0f) && (m_ves->GetPlaybackSpeed(SND_PRE) == 0.0f) && (m_ves->GetPlayPosition(SND_PRE) < 0), "legacy.sentinels_while_stopped",
           Fmt("pan=%g speed=%g pos=%d", m_ves->GetPan(SND_PRE), m_ves->GetPlaybackSpeed(SND_PRE), m_ves->GetPlayPosition(SND_PRE)));
    Report(m_ves->SetPaused(SND_VOL, true) && m_ves->IsPaused(SND_VOL) && m_ves->SetPaused(SND_VOL, false) && !m_ves->IsPaused(SND_VOL), "legacy.pause_resume_sound");
    const bool mld = m_mod->LoadWav(SND_MOD, FILE_WAV, PT::Global);
    const bool mpl = m_mod->PlayWav(SND_MOD, true);
    Report(mld && mpl && m_mod->IsWavPlaying(SND_MOD), "legacy.module_sound_plays", Fmt("load=%d play=%d", mld, mpl));
    const char *g = m_ves->GetDefaultSoundGroupFolder(XRSound::CabinAmbienceGroup);
    Log(Fmt("XRSOUNDTEST INFO legacy GetDefaultSoundGroupFolder(CabinAmbienceGroup)=%s", g ? g : "(null)"));
    Log(Fmt("XRSOUNDTEST INFO legacy GetDefaultSoundEnabled(AirConditioning)=%d", m_ves->GetDefaultSoundEnabled(XRSound::AirConditioning)));
}

void TestModule::FinishLegacy()
{
    const int p = m_ves->GetPlayPosition(SND_VOL);
    Report(m_ves->IsWavPlaying(SND_VOL) && (p - m_legP0 >= 300), "legacy.position_advances", Fmt("pos0=%d pos1=%d", m_legP0, p));
    const int mp = m_mod->GetPlayPosition(SND_MOD);
    Report(mp >= 300, "legacy.module_position_advances", Fmt("pos=%d", mp));
    m_ves->StopWav(SND_VOL);
    m_mod->StopWav(SND_MOD);
}

static TestModule *g_pModule = nullptr;

DLLCLBK void InitModule(HINSTANCE hDLL)
{
    g_pModule = new TestModule(hDLL);
    oapiRegisterModule(g_pModule);
    Log("XRSOUNDTEST INFO module loaded");
}

DLLCLBK void ExitModule(HINSTANCE)
{
    delete g_pModule;       // Orbiter deletes only the modules that it allocated itself
    g_pModule = nullptr;
}
