// ==============================================================
// BuiltInSounds.cpp : XRSoundApiTest checks of the default sounds: their volume types, cabin ambience and ATC, the update interval, time acceleration, ATC conditions, and the gear-up warning
// ==============================================================

#include <algorithm>
#include <cstring>
#include "XRSoundApiTest.h"

// =====================================================================================================
// Each kind of default sound is controlled by its own volume type, by no other type, and by its item level.  An add-on cannot play
// a default sound itself (PlayWav refuses default sound IDs), so this makes XRSound play them the way a pilot would: a short main
// engine burn, an RCS pulse, an RCS mode switch (a callout), an autopilot (a sound effect), the landing gear (a door sound), and the
// cockpit view (air conditioning).  Each sound is set to play as a global sound first, so that the view and the distance to the camera
// do not change its volume.  The same events check these settings: CustomEnginesThrusterIndexes (the main engine is made a custom
// engine too, with this test's tone as the custom engine sound), MinThrusterLevelForRCSSoundEffects, LogVesselAnimations, and LogThrusterData.
// =====================================================================================================
void TestModule::StartRoutingStage()
{
    PrepareLevels();
    m_stepLog.SkipToEnd();
    // the main engine's thruster index, as a custom engine
    const THRUSTER_HANDLE mainThruster = m_vessel->GetGroupThruster(THGROUP_MAIN, 0);
    int mainIndex = -1;
    for (DWORD i = 0; mainThruster && (i < m_vessel->GetThrusterCount()); ++i)
        if (m_vessel->GetThrusterHandleByIndex(i) == mainThruster) mainIndex = static_cast<int>(i);
    const bool customLoaded = m_ves->LoadWav(XRSound::CustomEngines, FILE_WAV, PT::Global);
    const bool customSet = m_ves->SetSetting(SS::CustomEnginesThrusterIndexes, Fmt("%d", mainIndex).c_str());
    Log(Fmt("XRSOUNDTEST INFO custom engine sound loaded=%d, CustomEnginesThrusterIndexes set to the main thruster's index %d: %d", customLoaded, mainIndex, customSet));
    // RCS threshold: the RCS sound must not play at full RCS thrust while the threshold is 1.0 (the total thrust that it compares is at most 1.0)
    m_ves->SetSetting(SS::MinThrusterLevelForRCSSoundEffects, 1.0);
    m_rcsGateOpen = false;
    m_rcsPlayedBelowThreshold = false;
    m_ves->SetSetting(SS::LogVesselAnimations, 1);
    m_ves->SetSetting(SS::LogThrusterData, 1);
    m_origAttitudeMode = m_vessel->GetAttitudeMode();
    const int newMode = (m_origAttitudeMode == RCS_LIN) ? RCS_ROT : RCS_LIN;
    const bool toLinear = (newMode == RCS_LIN);
    // the last value is the playback type that XRSound loads the sound with, which is restored when XRSound has not loaded it yet
    m_routing = {
        { XRSound::MainEngines, "MainEngines", VT::Engines, PT::BothViewFar },
        { XRSound::RCSSustain, "RCSSustain", VT::RCS, PT::BothViewMedium },
        { XRSound::AutopilotOn, "AutopilotOn", VT::Effects, PT::InternalOnly },     // 0.7 s long (the RCS mode switch click is too short to check reliably)
        { toLinear ? XRSound::Translation : XRSound::Rotation, toLinear ? "Translation" : "Rotation", VT::Callouts, PT::Radio },
        { XRSound::AirConditioning, "AirConditioning", VT::CabinAmbience, PT::InternalOnly },
        { XRSound::CustomEngines, "CustomEngines", VT::Engines, PT::Global },
    };
    // the Delta-glider's landing gear sound that plays while the gear moves (XRSound-DeltaGlider.cfg, [animation_7] MovingSoundID)
    if (!strcmp(m_vessel->GetClassName(), "DeltaGlider"))
        m_routing.push_back({ DG_GEAR_MOVING_SOUND_ID, "GearMoving", VT::Effects, PT::BothViewClose });
    else
        Skip("routing.GearMoving_is_Effects", "the focus vessel is not a Delta-glider, so its gear sound ID is not known");
    for (RoutingTarget &r : m_routing)
    {
        m_ves->GetPlaybackType(r.id, r.origType);     // leaves origType at the load type if XRSound has not loaded the sound yet
        m_ves->SetPlaybackType(r.id, PT::Global);
    }
    m_vessel->SetAttitudeMode(newMode);
    m_vessel->SetThrusterGroupLevel(THGROUP_MAIN, 0.3);
    m_vessel->SetThrusterGroupLevel(THGROUP_ATT_PITCHUP, 1.0);
    m_vessel->ActivateNavmode(NAVMODE_PROGRADE);    // any autopilot except kill-rotation plays the autopilot-on sound
    m_vessel->SendBufferedKey(OAPI_KEY_G);          // the Delta-glider's landing gear key
    oapiCameraAttach(m_vessel->GetHandle(), 0);     // cockpit view: XRSound plays the air conditioning there
}

// Called every frame: checks each sound in the frame that XRSound is playing it.  Returns true when the stage is over.
bool TestModule::RoutingTick(const ULONGLONG now)
{
    const ULONGLONG elapsed = now - m_tStep;
    // for the first second the RCS threshold is 1.0, and the RCS sound must stay silent; then the threshold goes back to its configured value
    if (!m_rcsGateOpen)
    {
        if (m_ves->IsWavPlaying(XRSound::RCSSustain))
            m_rcsPlayedBelowThreshold = true;
        if (elapsed >= 1000)
        {
            Report(!m_rcsPlayedBelowThreshold, "setting.effect.MinThrusterLevelForRCSSoundEffects",
                   Fmt("with the threshold at 1.0 and full RCS thrust, the RCS sound %s during the first second", m_rcsPlayedBelowThreshold ? "PLAYED" : "stayed silent"));
            RestoreSetting(m_ves, SS::MinThrusterLevelForRCSSoundEffects);
            m_rcsGateOpen = true;
        }
    }
    bool allDone = true;
    for (RoutingTarget &r : m_routing)
    {
        if (r.done) continue;
        if (((r.id == XRSound::RCSSustain) && !m_rcsGateOpen) || !m_ves->IsWavPlaying(r.id)) { allDone = false; continue; }
        CheckRouting(r);
        r.done = true;
        if (r.id == XRSound::RCSSustain) m_vessel->SetThrusterGroupLevel(THGROUP_ATT_PITCHUP, 0);
    }
    if (RoutingTargetDone(XRSound::MainEngines) && RoutingTargetDone(XRSound::CustomEngines))
        m_vessel->SetThrusterGroupLevel(THGROUP_MAIN, 0);
    // the stage lasts at least 2 seconds, so that the gear has moved (for the LogVesselAnimations check) and LogThrusterData has logged
    if ((!allDone || (elapsed < 2000)) && (elapsed < 6000))
        return false;

    for (const RoutingTarget &r : m_routing)
        if (!r.done)
            Report(false, Fmt("routing.%s_is_%s", r.name, VolName(r.type)), "XRSound did not play the sound within 6 s of the event that should start it");
    m_stepLog.Read();
    const std::string vesselTag = Fmt("[%s][", m_vessel->GetName());
    double gearAnim = -1;
    m_ves->GetSetting(SS::LandingGearAnimationID, gearAnim);
    const std::string animationTag = Fmt("LogVesselAnimations: [%s][animation ID = %d,", m_vessel->GetClassName(), static_cast<int>(gearAnim));
    Report(m_stepLog.AnyLineContains(vesselTag, animationTag), "setting.effect.LogVesselAnimations", Fmt("XRSound.log has a line with '%s' after the landing gear moved", animationTag.c_str()));
    Report(m_stepLog.AnyLineContains(vesselTag, "LogThrusterData: [thruster index"), "setting.effect.LogThrusterData", "XRSound.log has the thruster data lines");
    const std::string customFile = m_stepLog.LastStartedFile(m_vessel->GetName(), XRSound::CustomEngines);
    Report(customFile == FILE_WAV, "setting.effect.CustomEnginesThrusterIndexes.plays_custom_sound",
           Fmt("with the main thruster as a custom engine, the custom engine sound played '%s' (expected '%s', the test tone loaded for it)", customFile.c_str(), FILE_WAV));
    m_vessel->SetThrusterGroupLevel(THGROUP_MAIN, 0);
    m_vessel->SetThrusterGroupLevel(THGROUP_ATT_PITCHUP, 0);
    m_vessel->SetAttitudeMode(m_origAttitudeMode);
    m_vessel->DeactivateNavmode(NAVMODE_PROGRADE);
    m_vessel->ActivateNavmode(NAVMODE_KILLROT);     // stop the rotation from the RCS pulse and the autopilot
    m_vessel->SendBufferedKey(OAPI_KEY_G);          // gear back up
    // Note: restoring the original playback type leaves an override equal to it, which plays the same as no override.
    for (const RoutingTarget &r : m_routing)
        m_ves->SetPlaybackType(r.id, r.origType);
    RestoreSetting(m_ves, SS::MinThrusterLevelForRCSSoundEffects);
    RestoreCustomEngines();
    StartLoggingOffChecks();
    RestoreDefaultLevels();
    return true;    // the camera stays in the cockpit for the ambient step
}

// Sets CustomEnginesThrusterIndexes back to its configured value
void TestModule::RestoreCustomEngines()
{
    std::vector<char> customDefault(512, '\0');
    if (m_ves->GetDefaultSetting(SS::CustomEnginesThrusterIndexes, customDefault.data(), static_cast<int>(customDefault.size())))
        m_ves->SetSetting(SS::CustomEnginesThrusterIndexes, customDefault.data());
}

// LogVesselAnimations and LogThrusterData off: while the gear goes back up (from the end of the routing step) and the thrusters fire (the
// kill-rotation autopilot, and the main engine in the update interval step), XRSound logs neither.  FinishLoggingOffChecks checks the log.
void TestModule::StartLoggingOffChecks()
{
    m_ves->SetSetting(SS::LogVesselAnimations, 0);
    m_ves->SetSetting(SS::LogThrusterData, 0);
    m_offLog.SkipToEnd();
    m_gearAnimStart = GearAnimationState(m_vessel, m_ves);
}

void TestModule::FinishLoggingOffChecks(const bool mainEngineFired)
{
    m_offLog.Read();
    const std::string vesselTag = Fmt("[%s][", m_vessel->GetName());
    const double gearNow = GearAnimationState(m_vessel, m_ves);
    const bool gearMoved = (gearNow >= 0) && (gearNow != m_gearAnimStart);
    const bool animationLines = m_offLog.AnyLineContains(vesselTag, "LogVesselAnimations: [");
    const bool thrusterLines = m_offLog.AnyLineContains(vesselTag, "LogThrusterData: [");
    Report(gearMoved && !animationLines, "setting.effect.LogVesselAnimations.off",
           Fmt("with it off, the landing gear moved from %g to %g, and XRSound.log %s", m_gearAnimStart, gearNow, animationLines ? "HAS animation lines" : "has no animation lines"));
    Report(mainEngineFired && !thrusterLines, "setting.effect.LogThrusterData.off",
           Fmt("with it off, the main engine fired=%d, and XRSound.log %s", mainEngineFired, thrusterLines ? "HAS thruster data lines" : "has no thruster data lines"));
    RestoreSetting(m_ves, SS::LogVesselAnimations);
    RestoreSetting(m_ves, SS::LogThrusterData);
}

// The state (0 to 1) of the vessel's landing gear animation (its LandingGearAnimationID), or -1 if it has none
double TestModule::GearAnimationState(VESSEL *pVessel, XRSound *p)
{
    double gearAnim = -1;
    p->GetSetting(SS::LandingGearAnimationID, gearAnim);
    return (gearAnim >= 0) ? pVessel->GetAnimation(static_cast<UINT>(gearAnim)) : -1;
}

bool TestModule::RoutingTargetDone(const int soundID) const
{
    return std::any_of(m_routing.begin(), m_routing.end(), [soundID](const RoutingTarget &r) { return (r.id == soundID) && r.done; });
}

// Sets each volume type to 0.5 in turn (and back to 1.0), then the sound's item level: only its own type and its item level may halve its volume
void TestModule::CheckRouting(const RoutingTarget &r)
{
    const char *ves1Name = m_vessel->GetName();
    auto loggedAfter = [&](const auto &change)
    {
        m_log.SkipToEnd();
        change();
        m_log.Read();
        return m_log.LastVolume(ves1Name, r.id);
    };
    const double half = loggedAfter([&] { m_ves->SetVolume(r.type, 0.5f); });
    const double full = loggedAfter([&] { m_ves->SetVolume(r.type, 1.0f); });
    bool ok = (full > 0.001) && LoggedNear(half, full * 0.5);
    std::string detail = Fmt("%s 0.5 -> %s, 1.0 -> %s;", VolName(r.type), VolStr(half).c_str(), VolStr(full).c_str());
    for (const VT t : ALL_VOLUME_TYPES)
    {
        if ((t == r.type) || (t == VT::Main) || (t == VT::Music)) continue;     // Main applies to every sound, and Music only to the music
        const double v = loggedAfter([&] { m_ves->SetVolume(t, 0.5f); });
        m_ves->SetVolume(t, 1.0f);
        if (!std::isnan(v)) { ok = false; detail += Fmt(" WRONG: %s 0.5 -> %s;", VolName(t), VolStr(v).c_str()); }
    }
    const double item = loggedAfter([&] { m_ves->SetVolume(r.id, 0.5f); });
    m_ves->SetVolume(r.id, 1.0f);
    if (!LoggedNear(item, full * 0.5)) ok = false;
    detail += Fmt(" item level 0.5 -> %s", VolStr(item).c_str());
    Report(ok, Fmt("routing.%s_is_%s", r.name, VolName(r.type)), detail);
}

// =====================================================================================================
// Ambient step (in the cockpit view): the cabin ambience and ATC groups play from a folder set with SetDefaultSoundGroupFolder, after
// their configured delays (CabinAmbienceMin/Max and ATCMinDelay/MaxDelay), at their own volume types (CabinAmbience and ATC); music
// plays in the cockpit only with MusicPlayInternal on; and SetDefaultSoundEnabled(false) stops a playing default sound.
// =====================================================================================================
void TestModule::StartAmbientStep()
{
    PrepareLevels();
    m_stepLog.SkipToEnd();
    const char *cabin = m_ves->GetDefaultSoundGroupFolder(XRSound::CabinAmbienceGroup);
    const char *atc = m_ves->GetDefaultSoundGroupFolder(XRSound::RadioATCGroup);
    m_origCabinFolder = cabin ? cabin : "";
    m_origATCFolder = atc ? atc : "";
    const bool f1 = m_ves->SetDefaultSoundGroupFolder(XRSound::CabinAmbienceGroup, TEST_FOLDER);
    const bool f2 = m_ves->SetDefaultSoundGroupFolder(XRSound::RadioATCGroup, TEST_FOLDER);
    // each delay setting reschedules the group's next clip from now
    m_ves->SetSetting(SS::ATCDelayPlanetMultiplier, 1.0);
    m_ves->SetSetting(SS::ATCMaxDelay, 10);
    m_ves->SetSetting(SS::ATCMinDelay, 10);
    m_ves->SetSetting(SS::CabinAmbienceMax, 7);
    m_ves->SetSetting(SS::CabinAmbienceMin, 7);
    Log(Fmt("XRSOUNDTEST INFO ambient step: group folders set=%d,%d; next cabin ambience clip in 7 s, next ATC clip in 10 s", f1, f2));
    // SetDefaultSoundEnabled(false) stops a default sound that is playing (the air conditioning plays in the cockpit)
    const bool wasPlaying = m_ves->IsWavPlaying(XRSound::AirConditioning);
    m_ves->SetDefaultSoundEnabled(XRSound::AirConditioning, false);
    const bool stillPlaying = m_ves->IsWavPlaying(XRSound::AirConditioning);
    m_ves->SetDefaultSoundEnabled(XRSound::AirConditioning, true);
    Report(wasPlaying && !stillPlaying, "defaultsound.disable_stops_playing_sound", Fmt("air conditioning playing before=%d, right after SetDefaultSoundEnabled(false)=%d", wasPlaying, stillPlaying));
    // MusicPlayInternal is off in the configuration, so the cockpit view paused the music; turning it on must play it here
    double internal = -1;
    m_ves->GetSetting(SS::MusicPlayInternal, internal);
    const bool musicNow = MusicAudible();
    Report(!((internal == static_cast<double>(XRSound::MusicPlay::Off)) && musicNow), "setting.effect.MusicPlayInternal.off",
           Fmt("MusicPlayInternal=%g; music playing in the cockpit view=%d", internal, musicNow));
    m_ves->SetSetting(SS::MusicPlayInternal, static_cast<int>(XRSound::MusicPlay::On));
    m_ambientCabinDone = m_ambientATCDone = m_ambientMusicDone = m_ambientAirDone = false;
}

bool TestModule::AmbientTick(const ULONGLONG now)
{
    const ULONGLONG elapsed = now - m_tStep;
    const char *ves1Name = m_vessel->GetName();
    struct Group { bool &done; int id; const char *name; VT type; double minMs; double maxMs; const char *delaySetting; };
    // the upper limits leave a few seconds for a slow frame: XRSound checks the delays only at the vessel's updates
    Group groups[] = { { m_ambientCabinDone, XRSound::CabinAmbienceGroup, "CabinAmbienceGroup", VT::CabinAmbience, 6000, 10500, "CabinAmbienceMin/Max" },
                       { m_ambientATCDone, XRSound::RadioATCGroup, "RadioATCGroup", VT::ATC, 9000, 13500, "ATCMinDelay/MaxDelay" } };
    for (Group &g : groups)
    {
        if (g.done || !m_ves->IsWavPlaying(g.id)) continue;
        g.done = true;
        m_stepLog.Read();
        const std::string file = m_stepLog.LastStartedFile(ves1Name, g.id);
        Report(file.find(Fmt("%s\\", TEST_FOLDER)) == 0, Fmt("group.%s.plays_from_new_folder", g.name), Fmt("played '%s'", file.c_str()));
        Report((elapsed >= g.minMs) && (elapsed <= g.maxMs), Fmt("setting.effect.%s", g.delaySetting), Fmt("started %llu ms after the delays were set (expected %g to %g ms)", static_cast<unsigned long long>(elapsed), g.minMs, g.maxMs));
        CheckRouting({ g.id, g.name, g.type, PT::Global });
    }
    if (!m_ambientMusicDone && MusicAudible())
    {
        m_ambientMusicDone = true;
        Report(true, "setting.effect.MusicPlayInternal.on", Fmt("music started in the cockpit view %llu ms after MusicPlayInternal was turned on", static_cast<unsigned long long>(elapsed)));
    }
    if (!m_ambientAirDone && m_ves->IsWavPlaying(XRSound::AirConditioning))
    {
        m_ambientAirDone = true;
        Report(true, "defaultsound.enable_starts_sound_again", Fmt("air conditioning playing again %llu ms after SetDefaultSoundEnabled(true)", static_cast<unsigned long long>(elapsed)));
    }
    const bool allDone = m_ambientCabinDone && m_ambientATCDone && m_ambientMusicDone && m_ambientAirDone;
    if (!allDone && (elapsed < 15000))
        return false;

    if (!m_ambientCabinDone) Report(false, "group.CabinAmbienceGroup.plays_from_new_folder", "no cabin ambience clip within 15 s");
    if (!m_ambientATCDone) Report(false, "group.RadioATCGroup.plays_from_new_folder", "no ATC clip within 15 s");
    if (!m_ambientMusicDone) Report(false, "setting.effect.MusicPlayInternal.on", "no music in the cockpit view within 15 s of MusicPlayInternal on");
    if (!m_ambientAirDone) Report(false, "defaultsound.enable_starts_sound_again", "no air conditioning within 15 s of SetDefaultSoundEnabled(true)");
    RestoreGroupFolder(m_ves, XRSound::CabinAmbienceGroup, m_origCabinFolder);
    RestoreGroupFolder(m_ves, XRSound::RadioATCGroup, m_origATCFolder);
    for (const SS s : { SS::ATCDelayPlanetMultiplier, SS::ATCMinDelay, SS::ATCMaxDelay, SS::CabinAmbienceMin, SS::CabinAmbienceMax, SS::MusicPlayInternal })
        RestoreSetting(m_ves, s);
    oapiCameraAttach(m_vessel->GetHandle(), 1);     // back to the external view (the fade and music checks need it)
    RestoreDefaultLevels();
    return true;
}

// =====================================================================================================
// UpdateInterval: XRSound updates each vessel's sounds once per that vessel's UpdateInterval.  With it at 1.0 s, the engine sound starts
// and stops about 1 s after each thrust change that follows an update; at the configured value (normally 0.05 s), within a few frames.
// Meanwhile CustomEnginesThrusterIndexes is empty, so the main engine is not a custom engine, and the custom engine sound must not play.
// =====================================================================================================
void TestModule::StartUpdateIntervalStep()
{
    m_ves->SetSetting(SS::UpdateInterval, 1.0);
    m_ves->SetSetting(SS::CustomEnginesThrusterIndexes, "");
    m_customPlayed = false;
    m_vessel->SetThrusterGroupLevel(THGROUP_MAIN, 0.3);
    m_updatePhase = 0;
    m_updateTimes.clear();
}

bool TestModule::UpdateIntervalTick(const ULONGLONG now)
{
    m_customPlayed |= m_ves->IsWavPlaying(XRSound::CustomEngines);
    // phases 0-2 at 1.0 s: wait for the sound to start, then stop, then start; phases 3-4 at the configured interval: stop, then start
    const bool wantPlaying = ((m_updatePhase % 2) == 0);
    if ((m_updatePhase < 5) && (m_ves->IsWavPlaying(XRSound::MainEngines) == wantPlaying))
    {
        m_updateTimes.push_back(now);
        ++m_updatePhase;
        if (m_updatePhase == 3)
            RestoreSetting(m_ves, SS::UpdateInterval);
        m_vessel->SetThrusterGroupLevel(THGROUP_MAIN, ((m_updatePhase % 2) == 0) ? 0.3 : 0.0);
    }
    if ((m_updatePhase < 5) && (now - m_tStep < 10000))
        return false;

    m_vessel->SetThrusterGroupLevel(THGROUP_MAIN, 0);
    RestoreSetting(m_ves, SS::UpdateInterval);
    const bool mainEngineFired = (m_updatePhase >= 1);      // the main engine sound played, so the engine fired
    Report(mainEngineFired && !m_customPlayed, "setting.effect.CustomEnginesThrusterIndexes.empty",
           Fmt("with it empty, the main engine fired=%d, and the custom engine sound %s", mainEngineFired, m_customPlayed ? "PLAYED" : "stayed silent"));
    RestoreCustomEngines();
    FinishLoggingOffChecks(mainEngineFired);
    double configured = 0;
    m_ves->GetDefaultSetting(SS::UpdateInterval, configured);
    if (m_updatePhase < 5)
    {
        Report(false, "setting.effect.UpdateInterval", Fmt("the engine sound did not follow the thrust changes within 10 s (phase %d)", m_updatePhase));
        return true;
    }
    // m_updateTimes: [0] first start (any delay), [1] stop and [2] start at 1.0 s, [3] stop (at the update already scheduled 1.0 s ahead),
    // and [4] start at the configured interval
    const long long slow1 = static_cast<long long>(m_updateTimes[1] - m_updateTimes[0]), slow2 = static_cast<long long>(m_updateTimes[2] - m_updateTimes[1]);
    const long long fast = static_cast<long long>(m_updateTimes[4] - m_updateTimes[3]);
    const long long fastLimit = static_cast<long long>(configured * 1000) + 250;
    Report((slow1 >= 700) && (slow1 <= 1400) && (slow2 >= 700) && (slow2 <= 1400) && (fast <= fastLimit), "setting.effect.UpdateInterval",
           Fmt("at 1.0 s the sound followed the thrust after %lld and %lld ms (expected about 1000); at the configured %.3f s, after %lld ms (expected at most %lld)", slow1, slow2, configured, fast, fastLimit));
    return true;
}

// =====================================================================================================
// DisableAutopilotsTimeAccThreshold: above this time acceleration, XRSound turns off the vessel's autopilots
// =====================================================================================================
void TestModule::StartTimeAccStep()
{
    m_ves->SetSetting(SS::DisableAutopilotsTimeAccThreshold, 10);
    m_vessel->ActivateNavmode(NAVMODE_HLEVEL);
    m_timeAccRaised = false;
}

bool TestModule::TimeAccTick(const ULONGLONG now)
{
    const ULONGLONG elapsed = now - m_tStep;
    const bool engaged = m_vessel->GetNavmodeState(NAVMODE_HLEVEL);
    if (!m_timeAccRaised)
    {
        if (!engaged)
        {
            Report(false, "setting.effect.DisableAutopilotsTimeAccThreshold", "the autopilot turned off at 1x time acceleration, below the threshold of 10x");
            return FinishTimeAccStep();
        }
        if (elapsed < 500) return false;
        oapiSetTimeAcceleration(100);
        m_timeAccRaised = true;
        m_tTimeAcc = now;
        return false;
    }
    if (engaged && (now - m_tTimeAcc < 3000))
        return false;
    Report(!engaged, "setting.effect.DisableAutopilotsTimeAccThreshold",
           Fmt("the autopilot stayed on for 0.5 s at 1x, and %s at 100x (threshold 10x)", engaged ? "was STILL ON 3 s later" : Fmt("turned off %llu ms later", static_cast<unsigned long long>(now - m_tTimeAcc)).c_str()));
    return FinishTimeAccStep();
}

bool TestModule::FinishTimeAccStep()
{
    oapiSetTimeAcceleration(1);
    RestoreSetting(m_ves, SS::DisableAutopilotsTimeAccThreshold);
    m_vessel->DeactivateNavmode(NAVMODE_HLEVEL);
    return true;
}

// =====================================================================================================
// ATC conditions, with the third vessel: XRSound plays ATC only when ATCAllowWhileLanded, ATCAllowInAtmosphere, or ATCAllowDuringReentry
// allows it for the vessel's situation, and multiplies the ATC delays by ATCDelayPlanetMultiplier when the vessel is at least
// ATCDelayPlanetDistance km above the planet.  The test moves the third vessel into each situation (holding it there every frame),
// gives it focus, and times its next ATC clip (ATCMinDelay = ATCMaxDelay = 10 s).
// =====================================================================================================
// Each "not allowed" case waits 13 s and expects no clip; each allowed case expects the clip after the (multiplied) delay.  The upper limits
// leave a few seconds for a slow frame: XRSound checks the delay only at the vessel's updates.
const std::vector<TestModule::ATCCase> &TestModule::ATCCases()
{
    static const std::vector<ATCCase> cases = {
        { "ATCAllowWhileLanded.off", Situation::Landed, SS::ATCAllowWhileLanded, 0, 1e6, 1, 0, 13000, false },
        { "ATCAllowWhileLanded.on", Situation::Landed, SS::ATCAllowWhileLanded, 1, 1e6, 1, 9000, 13500, true },
        { "ATCAllowInAtmosphere.off", Situation::Atmosphere, SS::ATCAllowInAtmosphere, 0, 1e6, 1, 0, 13000, false },
        { "ATCAllowInAtmosphere.on", Situation::Atmosphere, SS::ATCAllowInAtmosphere, 1, 1e6, 1, 9000, 13500, true },
        { "ATCAllowDuringReentry.off", Situation::Reentry, SS::ATCAllowDuringReentry, 0, 1e6, 1, 0, 13000, false },
        { "ATCAllowDuringReentry.on", Situation::Reentry, SS::ATCAllowDuringReentry, 1, 1e6, 1, 9000, 13500, true },
        // 5 km up, at least 0 km: the delay is doubled
        { "ATCDelayPlanetDistanceAndMultiplier", Situation::Atmosphere, SS::ATCAllowInAtmosphere, 1, 0, 2, 19000, 24000, true },
    };
    return cases;
}

void TestModule::StartATCConditionsStep()
{
    m_atcCase = 0;
    m_atcPhase = 0;
    if (!m_ves3)
    {
        Skip("setting.effect.ATCAllow", "no third XRSound vessel in the scenario");
        return;
    }
    m_ves3->SetSetting(SS::EnableVerboseLogging, 1);
    const char *atc = m_ves3->GetDefaultSoundGroupFolder(XRSound::RadioATCGroup);
    m_origATCFolder3 = atc ? atc : "";
    m_ves3->SetDefaultSoundGroupFolder(XRSound::RadioATCGroup, TEST_FOLDER);
    PlaceThirdVessel(Situation::Landed, true);
    oapiSetFocusObject(m_vessel3->GetHandle());
}

bool TestModule::ATCConditionsTick(const ULONGLONG now)
{
    if (!m_ves3) return true;
    const std::vector<ATCCase> &cases = ATCCases();
    if (m_atcPhase == 0)
    {
        // the landed reference position: wait a moment for the vessel to settle on the ground, then record it
        if (now - m_tStep < 1500) return false;
        const OBJHANDLE hEarth = oapiGetGbodyByName(const_cast<char *>("Earth"));
        m_vessel3->GetRelativePos(hEarth, m_siteR);
        m_vessel3->GetRelativeVel(hEarth, m_siteV);
        m_atcPhase = 1;
    }
    if (m_atcPhase == 1)
    {
        // set up the next case: stop any clip, put the vessel in the situation, set the settings (the delay settings reschedule the next clip from now)
        if (m_atcCase >= cases.size())
            return FinishATCConditionsStep();
        const ATCCase &c = cases[m_atcCase];
        m_ves3->SetDefaultSoundEnabled(XRSound::RadioATCGroup, false);
        PlaceThirdVessel(c.situation, false);
        m_ves3->SetSetting(c.allowSetting, c.allow);
        m_ves3->SetSetting(SS::ATCDelayPlanetMultiplier, c.multiplier);
        m_ves3->SetSetting(SS::ATCDelayPlanetDistance, c.planetDistance);
        m_ves3->SetDefaultSoundEnabled(XRSound::RadioATCGroup, true);
        m_ves3->SetSetting(SS::ATCMaxDelay, 10);
        m_ves3->SetSetting(SS::ATCMinDelay, 10);
        m_tATCCase = now;
        m_atcPhase = 2;
        return false;
    }
    // phase 2: wait for the clip, or for the end of the window
    const ATCCase &c = cases[m_atcCase];
    const ULONGLONG elapsed = now - m_tATCCase;
    const bool playing = m_ves3->IsWavPlaying(XRSound::RadioATCGroup);
    if (!playing && (elapsed < c.maxMs))
        return false;
    const std::string state = Fmt("ground contact=%d, groundspeed %.1f m/s, static pressure %.0f Pa, dynamic pressure %.0f Pa, altitude %.0f m",
                                  m_vessel3->GroundContact(), m_vessel3->GetGroundspeed(), m_vessel3->GetAtmPressure(), m_vessel3->GetDynPressure(), m_vessel3->GetAltitude());
    if (c.expectPlay)
        Report(playing && (elapsed >= c.minMs), Fmt("setting.effect.%s", c.name), Fmt("ATC clip %s after %llu ms (expected %g to %g ms); %s", playing ? "started" : "did NOT start", static_cast<unsigned long long>(elapsed), c.minMs, c.maxMs, state.c_str()));
    else
        Report(!playing, Fmt("setting.effect.%s", c.name), Fmt("%s within %llu ms (expected none); %s", playing ? "an ATC clip STARTED" : "no ATC clip", static_cast<unsigned long long>(elapsed), state.c_str()));
    ++m_atcCase;
    m_atcPhase = 1;
    return false;
}

bool TestModule::FinishATCConditionsStep()
{
    m_ves3->SetDefaultSoundEnabled(XRSound::RadioATCGroup, false);
    m_ves3->SetDefaultSoundEnabled(XRSound::RadioATCGroup, true);
    for (const SS s : { SS::ATCAllowWhileLanded, SS::ATCAllowInAtmosphere, SS::ATCAllowDuringReentry, SS::ATCDelayPlanetMultiplier, SS::ATCDelayPlanetDistance, SS::ATCMinDelay, SS::ATCMaxDelay })
        RestoreSetting(m_ves3, s);
    return true;    // the gear-up warning step uses the third vessel next
}

// Puts the third vessel in a situation: landed at its site, or 5 km up at 150 m/s (in the atmosphere), or 20 km up at 2 km/s (dynamic pressure
// well over XRSound's 75 kPa for reentry), relative to the air.  The airborne situations are held every frame (see clbkPreStep).
void TestModule::PlaceThirdVessel(const Situation situation, const bool landedAtSite)
{
    VESSELSTATUS2 vs;
    memset(&vs, 0, sizeof(vs));
    vs.version = 2;
    vs.rbody = oapiGetGbodyByName(const_cast<char *>("Earth"));
    m_holdThird = false;
    if (landedAtSite || (situation == Situation::Landed))
    {
        vs.status = 1;
        vs.surf_lng = THIRD_VESSEL_SITE_LNG * RAD;
        vs.surf_lat = THIRD_VESSEL_SITE_LAT * RAD;
        vs.surf_hdg = 0;
        m_vessel3->DefSetStateEx(&vs);
        return;
    }
    const double altitude = (situation == Situation::Atmosphere) ? 5000 : 20000;
    const double airspeed = (situation == Situation::Atmosphere) ? 150 : 2000;
    m_holdStatus = AirborneStatus(altitude, airspeed, 0);
    m_holdThird = true;
    m_vessel3->DefSetStateEx(&m_holdStatus);
}

// A free-flight state above the landed site: the site's velocity is the planet's rotation there, so it points east and is the air's velocity
VESSELSTATUS2 TestModule::AirborneStatus(const double altitude, const double airspeedEast, const double verticalSpeed) const
{
    VESSELSTATUS2 vs;
    memset(&vs, 0, sizeof(vs));
    vs.version = 2;
    vs.rbody = oapiGetGbodyByName(const_cast<char *>("Earth"));
    vs.status = 0;
    const VECTOR3 up = unit(m_siteR), east = unit(m_siteV);
    vs.rpos = m_siteR + up * altitude;
    vs.rvel = m_siteV + east * airspeedEast + up * verticalSpeed;
    return vs;
}

// =====================================================================================================
// Gear-up warning: descending below WarningGearIsUpAltitude with the gear up plays the warning, but only if LandingGearAnimationID names the
// gear.  The third vessel, with its gear up, is moved down from 600 m at 5 m/s every frame (a set path, so that its attitude and lift cannot
// make it climb), and the settings are changed while it descends.
// =====================================================================================================
void TestModule::StartGearWarningStep()
{
    m_gearPhase = 0;
    if (!m_ves3)
    {
        Skip("setting.effect.WarningGearIsUpAltitude", "no third XRSound vessel in the scenario");
        return;
    }
    m_holdThird = false;
    m_ves3->GetSetting(SS::LandingGearAnimationID, m_gearAnimID);
    m_ves3->SetSetting(SS::WarningGearIsUpAltitude, 0);
    m_gearWarned = false;
}

bool TestModule::GearWarningTick(const ULONGLONG now)
{
    if (!m_ves3) return true;
    const ULONGLONG elapsed = now - m_tStep;
    const VESSELSTATUS2 vs = AirborneStatus(600 - 5 * (elapsed / 1000.0), 120, -5);
    m_vessel3->DefSetStateEx(&vs);
    const bool warning = m_ves3->IsWavPlaying(XRSound::WarningGearIsUp);
    if (m_gearPhase == 0)
    {
        // altitude setting 0: no warning
        m_gearWarned |= warning;
        if (elapsed < 1500) return false;
        Report(!m_gearWarned, "setting.effect.WarningGearIsUpAltitude.below", Fmt("descending at %.0f m with the gear up and WarningGearIsUpAltitude 0: warning %s", m_vessel3->GetAltitude(), m_gearWarned ? "PLAYED" : "silent"));
        m_ves3->SetSetting(SS::WarningGearIsUpAltitude, 1000);
        m_ves3->SetSetting(SS::LandingGearAnimationID, -1);
        m_gearWarned = false;
        m_gearPhase = 1;
        return false;
    }
    if (m_gearPhase == 1)
    {
        // no gear animation: no warning, even below the warning altitude
        m_gearWarned |= warning;
        if (elapsed < 3000) return false;
        Report(!m_gearWarned, "setting.effect.LandingGearAnimationID.none", Fmt("descending at %.0f m with WarningGearIsUpAltitude 1000 and LandingGearAnimationID -1: warning %s", m_vessel3->GetAltitude(), m_gearWarned ? "PLAYED" : "silent"));
        m_ves3->SetSetting(SS::LandingGearAnimationID, m_gearAnimID);
        m_gearPhase = 2;
        return false;
    }
    // the gear animation named again: the warning plays
    if (!warning && (elapsed < 5500)) return false;
    Report(warning, "setting.effect.WarningGearIsUpAltitude.above", Fmt("descending at %.0f m with WarningGearIsUpAltitude 1000 and LandingGearAnimationID %g: warning %s", m_vessel3->GetAltitude(), m_gearAnimID, warning ? "played" : "did NOT play"));
    for (const SS s : { SS::WarningGearIsUpAltitude, SS::LandingGearAnimationID, SS::EnableVerboseLogging })
        RestoreSetting(m_ves3, s);
    RestoreGroupFolder(m_ves3, XRSound::RadioATCGroup, m_origATCFolder3);
    PlaceThirdVessel(Situation::Landed, true);     // park it
    oapiSetFocusObject(m_vessel->GetHandle());
    return true;
}
