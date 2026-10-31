// ==============================================================
// Music.cpp : XRSoundApiTest checks of the shared music track, and the music settings
// ==============================================================

#include "XRSoundApiTest.h"

// =====================================================================================================
// Music settings (after the music stage, with the focus vessel's music playing): MusicPlayExternal off silences the music in the external
// view; MusicOrder sequential plays every file of the music folder in turn, and random plays files from it.
// =====================================================================================================
bool TestModule::MusicAudible(XRSound *p)
{
    return p->IsWavPlaying(XRSound::MusicFolder) && !p->IsPaused(XRSound::MusicFolder);
}

void TestModule::StartMusicExternalStep()
{
    m_ves->SetSetting(SS::MusicPlayExternal, static_cast<int>(XRSound::MusicPlay::Off));
    m_musicPhase = 0;
}

bool TestModule::MusicExternalTick(const ULONGLONG now)
{
    const bool playing = MusicAudible();
    if (m_musicPhase == 0)
    {
        if (playing && (now - m_tStep < 4000)) return false;
        Report(!playing, "setting.effect.MusicPlayExternal.off", Fmt("music %s %llu ms after MusicPlayExternal off", playing ? "STILL PLAYING" : "paused", static_cast<unsigned long long>(now - m_tStep)));
        RestoreSetting(m_ves, SS::MusicPlayExternal);
        m_musicPhase = 1;
        m_tMusic = now;
        return false;
    }
    if (!playing && (now - m_tMusic < 12000)) return false;
    Report(playing, "setting.effect.MusicPlayExternal.space", Fmt("music %s %llu ms after MusicPlayExternal went back to its configured value", playing ? "playing again" : "NOT playing", static_cast<unsigned long long>(now - m_tMusic)));
    return true;
}

void TestModule::StartMusicOrderStep(const ULONGLONG now)
{
    const char *folder = m_ves->GetDefaultSoundGroupFolder(XRSound::MusicFolder);
    m_origMusicFolder = folder ? folder : "";
    m_ves->SetSetting(SS::EnableVerboseLogging, 1);
    m_ves->SetDefaultSoundGroupFolder(XRSound::MusicFolder, TEST_FOLDER);
    m_ves->SetSetting(SS::MusicOrder, static_cast<int>(XRSound::MusicOrder::Sequential));
    m_musicPicks.clear();
    m_musicPhase = 0;
    NextMusicTrack(now);
}

// Switches the focus vessel's music off and on, which starts the next track after a short pause
void TestModule::NextMusicTrack(const ULONGLONG now)
{
    m_stepLog.SkipToEnd();
    m_ves->SetDefaultSoundEnabled(XRSound::MusicFolder, false);
    m_ves->SetDefaultSoundEnabled(XRSound::MusicFolder, true);
    m_tMusic = now;     // the frame's time, as the tick compares with it (a later GetTickCount64 reading could be past the frame's time)
}

bool TestModule::MusicOrderTick(const ULONGLONG now)
{
    // The test folder has 4 files.  Random never picks the file it picked last, so each random pick follows the sequential order (the next
    // file in turn) with a chance of 1 in 3: 8 picks (7 steps) all in sequential order would happen by chance once in 2187 runs.
    static const size_t SEQUENTIAL_PICKS = 5, RANDOM_PICKS = 8;
    if (m_ves->IsWavPlaying(XRSound::MusicFolder))
    {
        m_stepLog.Read();
        const std::string file = m_stepLog.LastStartedFile(m_vessel->GetName(), XRSound::MusicFolder);
        if (!file.empty())
        {
            m_musicPicks.push_back(file);
            if (m_musicPicks.size() == SEQUENTIAL_PICKS)
            {
                const bool firstFourDifferent = (m_musicPicks[0] != m_musicPicks[1]) && (m_musicPicks[0] != m_musicPicks[2]) && (m_musicPicks[0] != m_musicPicks[3])
                                             && (m_musicPicks[1] != m_musicPicks[2]) && (m_musicPicks[1] != m_musicPicks[3]) && (m_musicPicks[2] != m_musicPicks[3]);
                std::string detail;
                for (const std::string &f : m_musicPicks) detail += "'" + f + "' ";
                Report(firstFourDifferent && (m_musicPicks[4] == m_musicPicks[0]), "setting.effect.MusicOrder.sequential", "tracks: " + detail + "(expected the 4 files in turn, then the first again)");
                m_ves->SetSetting(SS::MusicOrder, static_cast<int>(XRSound::MusicOrder::Random));
            }
            if (m_musicPicks.size() == SEQUENTIAL_PICKS + RANDOM_PICKS)
            {
                // the sequential picks give the folder's order: the file after file f is the one that followed it there
                auto nextInOrder = [this](const std::string &f) -> std::string
                {
                    for (size_t i = 0; i < 4; ++i)
                        if (m_musicPicks[i] == f) return m_musicPicks[i + 1];
                    return std::string();
                };
                bool fromFolder = true, noRepeat = true, allInOrder = true;
                std::string detail;
                for (size_t i = SEQUENTIAL_PICKS; i < m_musicPicks.size(); ++i)
                {
                    const std::string &f = m_musicPicks[i];
                    detail += "'" + f + "' ";
                    if (f.find(Fmt("%s\\", TEST_FOLDER)) != 0) fromFolder = false;
                    if (i > SEQUENTIAL_PICKS)
                    {
                        if (f == m_musicPicks[i - 1]) noRepeat = false;
                        if (f != nextInOrder(m_musicPicks[i - 1])) allInOrder = false;
                    }
                }
                Report(fromFolder && noRepeat && !allInOrder, "setting.effect.MusicOrder.random",
                       "tracks: " + detail + Fmt("(expected files from %s; never the same file twice in a row: %s; not all in the sequential order: %s)", TEST_FOLDER, noRepeat ? "yes" : "NO", allInOrder ? "NO" : "yes"));
                return FinishMusicOrderStep();
            }
            NextMusicTrack(now);
            return false;
        }
    }
    if (now - m_tMusic < 12000) return false;
    Report(false, "setting.effect.MusicOrder", Fmt("no music track started within 12 s (after %zu tracks)", m_musicPicks.size()));
    return FinishMusicOrderStep();
}

bool TestModule::FinishMusicOrderStep()
{
    RestoreGroupFolder(m_ves, XRSound::MusicFolder, m_origMusicFolder);
    RestoreSetting(m_ves, SS::MusicOrder);
    RestoreSetting(m_ves, SS::EnableVerboseLogging);
    return true;
}

// =====================================================================================================
// MusicFolder per vessel: each vessel's music plays from its own MusicFolder while it has focus.  The second vessel's folder is set to the
// test folder; with focus there, its next track comes from the test folder, and back on the first vessel, from the first vessel's folder.
// (The music order step has already shown the first vessel's own folder change taking effect.)
// =====================================================================================================
void TestModule::StartMusicFolderStep(const ULONGLONG now)
{
    m_musicPhase = 0;
    if (!g_proxies[2])
    {
        Skip("group.MusicFolder.per_vessel", "no second XRSound vessel in the scenario");
        return;
    }
    const char *f1 = m_ves->GetDefaultSoundGroupFolder(XRSound::MusicFolder);
    const char *f2 = g_proxies[2]->GetDefaultSoundGroupFolder(XRSound::MusicFolder);
    m_origMusicFolder = f1 ? f1 : "";
    m_origMusicFolder2 = f2 ? f2 : "";
    const bool s = g_proxies[2]->SetDefaultSoundGroupFolder(XRSound::MusicFolder, TEST_FOLDER);
    m_ves->SetSetting(SS::EnableVerboseLogging, 1);
    g_proxies[2]->SetSetting(SS::EnableVerboseLogging, 1);
    Log(Fmt("XRSOUNDTEST INFO music folder step: first vessel's folder '%s'; second vessel's folder set to '%s': %d", m_origMusicFolder.c_str(), TEST_FOLDER, s));
    oapiSetFocusObject(m_vessel2->GetHandle());
    m_tMusic = now;
}

bool TestModule::MusicFolderTick(const ULONGLONG now)
{
    if (!g_proxies[2]) return true;
    // phases 0 and 2: a second after a focus change, the vessel with focus switches its music off and on, which starts its next track
    // phases 1 and 3: wait for that track, and check where it came from
    XRSound *pFocus = (m_musicPhase < 2) ? g_proxies[2] : m_ves;
    VESSEL *pFocusVessel = (m_musicPhase < 2) ? m_vessel2 : m_vessel;
    if ((m_musicPhase == 0) || (m_musicPhase == 2))
    {
        if (now - m_tMusic < 1000) return false;
        m_stepLog.SkipToEnd();
        pFocus->SetDefaultSoundEnabled(XRSound::MusicFolder, false);
        pFocus->SetDefaultSoundEnabled(XRSound::MusicFolder, true);
        m_tMusic = now;
        ++m_musicPhase;
        return false;
    }
    m_stepLog.Read();
    const std::string file = m_stepLog.LastStartedFile(pFocusVessel->GetName(), XRSound::MusicFolder);
    if (file.empty() && (now - m_tMusic < 12000)) return false;
    if (m_musicPhase == 1)
    {
        Report(file.find(Fmt("%s\\", TEST_FOLDER)) == 0, "group.MusicFolder.second_vessel_plays_its_own_folder",
               Fmt("'%s' has focus: its next track was '%s' (expected a file from its own folder, %s)", m_vessel2->GetName(), file.c_str(), TEST_FOLDER));
        oapiSetFocusObject(m_vessel->GetHandle());
        m_tMusic = now;
        m_musicPhase = 2;
        return false;
    }
    const bool fromFirst = !m_origMusicFolder.empty() && (file.find(m_origMusicFolder) == 0) && (file.find(Fmt("%s\\", TEST_FOLDER)) != 0);
    Report(fromFirst, "group.MusicFolder.first_vessel_plays_its_own_folder",
           Fmt("'%s' has focus again: its next track was '%s' (expected a file from its own folder, %s, not the second vessel's)", m_vessel->GetName(), file.c_str(), m_origMusicFolder.c_str()));
    RestoreGroupFolder(g_proxies[2], XRSound::MusicFolder, m_origMusicFolder2);
    RestoreSetting(m_ves, SS::EnableVerboseLogging);
    RestoreSetting(g_proxies[2], SS::EnableVerboseLogging);
    return true;
}

// =====================================================================================================
// Music in the atmosphere, with the third vessel (landed at its site) in focus: MusicPlay Space means no music in the atmosphere, and On means
// music, in the external view (MusicPlayExternal) and in the cockpit (MusicPlayInternal).  (The other steps check Off and Space in space.)
// =====================================================================================================
const std::vector<TestModule::MusicAtmosphereCase> &TestModule::MusicAtmosphereCases()
{
    static const std::vector<MusicAtmosphereCase> cases = {
        { "MusicPlayExternal.space_in_atmosphere", false, SS::MusicPlayExternal, XRSound::MusicPlay::Space, false },
        { "MusicPlayExternal.on_in_atmosphere", false, SS::MusicPlayExternal, XRSound::MusicPlay::On, true },
        { "MusicPlayInternal.space_in_atmosphere", true, SS::MusicPlayInternal, XRSound::MusicPlay::Space, false },
        { "MusicPlayInternal.on_in_atmosphere", true, SS::MusicPlayInternal, XRSound::MusicPlay::On, true },
    };
    return cases;
}

void TestModule::StartMusicAtmosphereStep(const ULONGLONG now)
{
    m_musicPhase = 0;
    if (!m_ves3)
    {
        Skip("setting.effect.MusicPlay.in_atmosphere", "no third XRSound vessel in the scenario");
        return;
    }
    PlaceThirdVessel(Situation::Landed, true);
    oapiSetFocusObject(m_vessel3->GetHandle());
    StartMusicAtmosphereCase(now);
}

void TestModule::StartMusicAtmosphereCase(const ULONGLONG now)
{
    const MusicAtmosphereCase &c = MusicAtmosphereCases()[m_musicPhase];
    oapiCameraAttach(m_vessel3->GetHandle(), c.cockpit ? 0 : 1);
    m_ves3->SetSetting(c.setting, static_cast<int>(c.value));
    m_tMusic = now;
}

// No music: it must be silent 3 s after the change (XRSound pauses it at the vessel's next update).  Music: it must play within 12 s
// (it resumes at once, or the next track starts after XRSound's 2-second gap).
bool TestModule::MusicAtmosphereTick(const ULONGLONG now)
{
    if (!m_ves3) return true;
    const std::vector<MusicAtmosphereCase> &cases = MusicAtmosphereCases();
    const MusicAtmosphereCase &c = cases[m_musicPhase];
    const ULONGLONG elapsed = now - m_tMusic;
    const bool audible = MusicAudible(m_ves3);
    if (c.expectMusic ? (!audible && (elapsed < 12000)) : (elapsed < 3000))
        return false;
    Report(audible == c.expectMusic, Fmt("setting.effect.%s", c.name),
           Fmt("'%s' in focus, landed (static pressure %.0f Pa), %s view: music %s after %llu ms (expected %s)", m_vessel3->GetName(), m_vessel3->GetAtmPressure(),
               c.cockpit ? "cockpit" : "external", audible ? "playing" : "silent", static_cast<unsigned long long>(elapsed), c.expectMusic ? "playing" : "silent"));
    if (++m_musicPhase < static_cast<int>(cases.size()))
    {
        StartMusicAtmosphereCase(now);
        return false;
    }
    RestoreSetting(m_ves3, SS::MusicPlayExternal);
    RestoreSetting(m_ves3, SS::MusicPlayInternal);
    oapiCameraAttach(m_vessel3->GetHandle(), 1);    // the external view, for the atmosphere fade step
    return true;
}

// =====================================================================================================
// Music: one shared track, played at the levels of the vessel that has focus.  Each vessel switches music on and off for itself only.
// =====================================================================================================
void TestModule::MusicTick(const ULONGLONG now)
{
    const int MUSIC = XRSound::MusicFolder;
    const XRSound::DefaultSoundID MUSICID = XRSound::MusicFolder;
    const bool playing = m_ves->IsWavPlaying(MUSIC);
    switch (m_musicState)
    {
    case 0:     // wait for the focus vessel's music to be playing
        if (playing)
        {
            m_musicPos0 = m_ves->GetPlayPosition(MUSIC);
            Log(Fmt("XRSOUNDTEST INFO music is playing %llu ms into this stage; position %d", static_cast<unsigned long long>(now - m_tStage), m_musicPos0));
            if (!g_proxies[2])
            {
                Skip("music.nonfocus_isolation", "no second XRSound vessel in the scenario");
                m_musicState = 2; m_tMusic = 0; return;
            }
            // a NON-focus vessel switches its music off and changes its own music levels and settings
            float focusMusic = -1, focusMusicDefault = -1;
            const bool e0 = g_proxies[2]->GetDefaultSoundEnabled(MUSICID);
            const bool e0f = m_ves->GetDefaultSoundEnabled(MUSICID);
            const bool off = g_proxies[2]->SetDefaultSoundEnabled(MUSICID, false);
            const bool l1 = g_proxies[2]->SetVolume(VT::Music, 0.0f), l2 = g_proxies[2]->SetVolume(VT::Main, 0.0f);
            const bool l3 = g_proxies[2]->SetSetting(SS::MusicPlayInternal, 0), l4 = g_proxies[2]->SetSetting(SS::MusicPlayExternal, 0);
            const bool l5 = g_proxies[2]->SetVolume(MUSIC, 0.0f);         // the second vessel's music item level
            const bool e1 = g_proxies[2]->GetDefaultSoundEnabled(MUSICID);
            const bool e1f = m_ves->GetDefaultSoundEnabled(MUSICID);
            m_ves->GetVolume(VT::Music, focusMusic);
            m_ves->GetDefaultVolume(VT::Music, focusMusicDefault);
            float focusItem = -1;
            m_ves->GetVolume(MUSIC, focusItem);
            double fp = -1;
            m_ves->GetSetting(SS::MusicPlayExternal, fp);
            Report(e0 && e0f && off && !e1 && e1f && l1 && l2 && l3 && l4 && l5 && Near(focusMusic, focusMusicDefault, 1e-5) && Near(focusItem, 1.0, 1e-5), "music.nonfocus_off_isolated_values",
                   Fmt("second vessel music enabled %d -> %d (set returned %d); focus vessel music enabled %d -> %d; focus Music level=%g (configured %g), focus music item level=%g, focus MusicPlayExternal=%g",
                       e0, e1, off, e0f, e1f, focusMusic, focusMusicDefault, focusItem, fp));
            m_musicState = 1; m_tMusic = now;
        }
        else if (now - m_tStage > 12000)
        {
            Skip("music.stage", "the focus vessel's music was not playing within 12 s of the end of the pause stage, so the music stage's checks could not be arranged");
            FinishMusicStage();
        }
        return;
    case 1:     // 1500 ms later: the focus vessel's music must still be playing and not restarted
        if (now - m_tMusic < 1500) return;
        {
            const int pos = m_ves->GetPlayPosition(MUSIC);
            Report(playing && (pos >= m_musicPos0 + 1000), "music.nonfocus_off_keeps_focus_music_playing", Fmt("playing=%d position %d -> %d after %llu ms", playing, m_musicPos0, pos, static_cast<unsigned long long>(now - m_tMusic)));
            // pan, speed, and position of the shared track: refused from a vessel without focus, and applied from the focus vessel while the music plays
            const bool nPan = g_proxies[2]->SetPan(MUSIC, 0.3f), nSpeed = g_proxies[2]->SetPlaybackSpeed(MUSIC, 1.0f), nPos = g_proxies[2]->SetPlayPosition(MUSIC, 0);
            const bool fPan = m_ves->SetPan(MUSIC, 0.3f);
            const float panGot = m_ves->GetPan(MUSIC);
            const bool fPan0 = m_ves->SetPan(MUSIC, 0.0f);
            const bool fSpeed = m_ves->SetPlaybackSpeed(MUSIC, 1.0f);
            const int posNow = m_ves->GetPlayPosition(MUSIC);
            const bool fPos = (posNow >= 0) && m_ves->SetPlayPosition(MUSIC, static_cast<unsigned int>(posNow));
            Report(!nPan && !nSpeed && !nPos && fPan && Near(panGot, 0.3, 0.02) && fPan0 && fSpeed && fPos, "music.pan_speed_position_only_from_focus_vessel",
                   Fmt("second vessel (no focus): SetPan=%d SetPlaybackSpeed=%d SetPlayPosition=%d (expected all false); focus vessel: SetPan=%d (GetPan %g) SetPlaybackSpeed=%d SetPlayPosition=%d (expected all true)",
                       nPan, nSpeed, nPos, fPan, panGot, fSpeed, fPos));
            // restore the second vessel's values
            g_proxies[2]->SetDefaultSoundEnabled(MUSICID, true);
            g_proxies[2]->SetVolume(MUSIC, 1.0f);
            float d = -1;
            double dd = -1;
            g_proxies[2]->GetDefaultVolume(VT::Music, d);
            g_proxies[2]->SetVolume(VT::Music, d);
            g_proxies[2]->GetDefaultVolume(VT::Main, d);
            g_proxies[2]->SetVolume(VT::Main, d);
            g_proxies[2]->GetDefaultSetting(SS::MusicPlayInternal, dd);
            g_proxies[2]->SetSetting(SS::MusicPlayInternal, dd);
            g_proxies[2]->GetDefaultSetting(SS::MusicPlayExternal, dd);
            g_proxies[2]->SetSetting(SS::MusicPlayExternal, dd);
        }
        m_musicState = 2; m_tMusic = 0;
        return;
    case 2:     // the FOCUS vessel switches its own music off: the shared track stops
        m_ves->SetDefaultSoundEnabled(MUSICID, false);
        m_tMusic = now;
        m_musicState = 3;
        return;
    case 3:
        if (!playing)
        {
            Report(true, "music.focus_off_stops_music", Fmt("music stopped %llu ms after the focus vessel's SetDefaultSoundEnabled(MusicFolder,false)", static_cast<unsigned long long>(now - m_tMusic)));
            // with no music playing, pan, speed, and position are refused even from the focus vessel, and nothing is remembered for the next track (checked in state 4)
            const bool sPan = m_ves->SetPan(MUSIC, 0.5f), sSpeed = m_ves->SetPlaybackSpeed(MUSIC, 1.5f), sPos = m_ves->SetPlayPosition(MUSIC, 5000);
            m_musicStoppedSets = Fmt("SetPan=%d SetPlaybackSpeed=%d SetPlayPosition=%d", sPan, sSpeed, sPos);
            m_musicStoppedSetsRefused = !sPan && !sSpeed && !sPos;
            m_ves->SetDefaultSoundEnabled(MUSICID, true);
            m_tMusic = now;
            m_musicState = 4;
        }
        else if (now - m_tMusic > 4000)
        {
            Report(false, "music.focus_off_stops_music", "still playing 4 s after the focus vessel's SetDefaultSoundEnabled(MusicFolder,false)");
            m_ves->SetDefaultSoundEnabled(MUSICID, true);
            Skip("music.focus_on_resumes_music", "the music never stopped");
            Skip("music.pan_speed_position_refused_while_stopped", "the music never stopped");
            StartMusicLevelStage(now);
        }
        return;
    case 4:
        if (playing)
        {
            Report(true, "music.focus_on_resumes_music", Fmt("music playing again %llu ms after re-enabling", static_cast<unsigned long long>(now - m_tMusic)));
            const float pan = m_ves->GetPan(MUSIC), speed = m_ves->GetPlaybackSpeed(MUSIC);
            const int pos = m_ves->GetPlayPosition(MUSIC);
            Report(m_musicStoppedSetsRefused && Near(pan, 0, 0.02) && Near(speed, 1, 0.02) && (pos >= 0) && (pos < 4000), "music.pan_speed_position_refused_while_stopped",
                   Fmt("while stopped: %s (expected all false); the next track: pan %g, speed %g, position %d (expected 0, 1, and near the start: nothing remembered)", m_musicStoppedSets.c_str(), pan, speed, pos));
            StartMusicLevelStage(now);
        }
        else if (now - m_tMusic > 12000)
        {
            Report(false, "music.focus_on_resumes_music", "music not playing 12 s after re-enabling the focus vessel's music");
            Skip("music.levels_follow_focus", "the music did not play again");
            FinishMusicStage();
        }
        return;
    case 5:     // focus moved to the second vessel: wait until it plays the music at its own levels
        CheckMusicLevelsAfterFocusChange(now, m_vessel2, 0.7 * 0.5 * 0.4, "music.levels_follow_focus_to_second_vessel", 6);
        return;
    case 6:     // focus moved back to the first vessel
        CheckMusicLevelsAfterFocusChange(now, m_vessel, 0.8 * 0.6 * 0.9, "music.levels_follow_focus_back_to_first_vessel", 7);
        return;
    case 7:
        FinishMusicStage();
        return;
    }
}

// Starts the music level checks (state 5), which need the second vessel
void TestModule::StartMusicLevelStage(const ULONGLONG now)
{
    if (!g_proxies[2])
    {
        Skip("music.levels_follow_focus", "no second XRSound vessel in the scenario");
        FinishMusicStage();
        return;
    }
    StartMusicLevelChecks();
    m_musicState = 5;
    m_tMusic = now;
}

// Puts the music levels back and goes on to the steps after the music stage
void TestModule::FinishMusicStage()
{
    RestoreDefaultLevels();
    m_ves->SetVolume(XRSound::MusicFolder, 1.0f);
    if (g_proxies[2]) g_proxies[2]->SetVolume(XRSound::MusicFolder, 1.0f);
    SetSteps(StepsAfterMusic());
    m_stage = ST_STEPS_AFTER_MUSIC;
}

// The music's volume is the focus vessel's music item level x its Music level x its Main level x the music's own requested volume.
// Measures that requested volume with the first vessel's levels at 1.0, sets different levels on the two vessels, and then moves focus
// to the second vessel (CheckMusicLevelsAfterFocusChange then expects the music to follow the levels of the vessel with focus).
void TestModule::StartMusicLevelChecks()
{
    const int MUSIC = XRSound::MusicFolder;
    const char *ves1Name = m_vessel->GetName();
    m_ves->SetSetting(SS::EnableVerboseLogging, 1);
    g_proxies[2]->SetSetting(SS::EnableVerboseLogging, 1);
    m_log.SkipToEnd();
    m_ves->SetVolume(VT::Music, 0.5f);      // so that setting it to 1.0 below always changes the volume, and logs it
    m_ves->SetVolume(MUSIC, 1.0f);
    m_ves->SetVolume(VT::Music, 1.0f);
    m_ves->SetVolume(VT::Main, 1.0f);
    m_log.Read();
    m_musicRequested = m_log.LastVolume(ves1Name, MUSIC);
    Log(Fmt("XRSOUNDTEST INFO music volume with every level at 1.0 (its requested volume): %s", VolStr(m_musicRequested).c_str()));
    // different levels on each vessel; only the focus vessel's levels may change the music
    m_log.SkipToEnd();
    m_ves->SetVolume(MUSIC, 0.8f);
    m_ves->SetVolume(VT::Music, 0.6f);
    m_ves->SetVolume(VT::Main, 0.9f);
    g_proxies[2]->SetVolume(MUSIC, 0.7f);
    g_proxies[2]->SetVolume(VT::Music, 0.5f);
    g_proxies[2]->SetVolume(VT::Main, 0.4f);
    m_log.Read();
    const double v1 = m_log.LastVolume(ves1Name, MUSIC);
    const double v2 = m_log.LastVolume(m_vessel2->GetName(), MUSIC);
    const bool requestedOk = !std::isnan(m_musicRequested) && (m_musicRequested > 0.05);
    Report(requestedOk && LoggedNear(v1, m_musicRequested * 0.8 * 0.6 * 0.9) && std::isnan(v2), "music.levels_are_focus_vessels",
           Fmt("requested %s; logged %s (expected requested x item 0.8 x Music 0.6 x Main 0.9); second vessel logged %s (expected nothing: it does not have focus)",
               VolStr(m_musicRequested).c_str(), VolStr(v1).c_str(), VolStr(v2).c_str()));
    m_log.SkipToEnd();
    oapiSetFocusObject(m_vessel2->GetHandle());
}

// Waits up to 5 s for the vessel with focus to log the music volume, expects requested x levelProduct, then moves focus to the other vessel
// and goes to nextState.
void TestModule::CheckMusicLevelsAfterFocusChange(const ULONGLONG now, VESSEL *pFocus, const double levelProduct, const char *checkName, const int nextState)
{
    m_log.Read();
    const double v = m_log.LastVolume(pFocus->GetName(), XRSound::MusicFolder);
    const double expected = m_musicRequested * levelProduct;
    if (!LoggedNear(v, expected) && (now - m_tMusic < 5000))
        return;     // not applied yet
    Report(LoggedNear(v, expected), checkName, Fmt("'%s' has focus: logged %s, expected requested %.4f x %.3f = %.4f, %llu ms after the focus change",
                                                   pFocus->GetName(), VolStr(v).c_str(), m_musicRequested, levelProduct, expected, static_cast<unsigned long long>(now - m_tMusic)));
    m_log.SkipToEnd();
    if (nextState == 6) oapiSetFocusObject(m_vessel->GetHandle());
    m_musicState = nextState;
    m_tMusic = now;
}
