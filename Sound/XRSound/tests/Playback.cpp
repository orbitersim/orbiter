// ==============================================================
// Playback.cpp : XRSoundApiTest checks of per-sound volume, pan, speed, and position, durations, playback type fades, the module sound, and simulation pause
// ==============================================================

#include <algorithm>
#include "XRSoundApiTest.h"

// =====================================================================================================
// Per-sound volume, loop position, and playback type
// =====================================================================================================
void TestModule::StartVolumeStage()
{
    // verbose logging makes XRSound.log record the volume each sound plays at (turned off again in FinishVolumeStage)
    m_ves->SetSetting(SS::EnableVerboseLogging, 1);
    m_log.SkipToEnd();
    const bool ld = m_ves->LoadWav(SND_VOL, FILE_WAV, PT::Global);
    Report(ld, "sound.load", Fmt("LoadWav(%d,%s)=%d", SND_VOL, FILE_WAV, ld));
    float v = -1;
    bool g = m_ves->GetVolume(SND_VOL, v);
    Report(g && Near(v, 1.0, 1e-5), "soundvolume.new_sound_is_1", Fmt("get=%d v=%g", g, v));
    const bool s = m_ves->SetVolume(SND_VOL, 0.4f);
    g = m_ves->GetVolume(SND_VOL, v);
    Report(s && g && Near(v, 0.4, 1e-5), "soundvolume.set_stored", Fmt("set=%d get=%d v=%g", s, g, v));
    // invalid values leave it unchanged
    const bool b1 = m_ves->SetVolume(SND_VOL, -0.1f), b2 = m_ves->SetVolume(SND_VOL, 1.1f), b3 = m_ves->SetVolume(SND_VOL, NaNf);
    m_ves->GetVolume(SND_VOL, v);
    Report(!b1 && !b2 && !b3 && Near(v, 0.4, 1e-5), "soundvolume.invalid_rejected", Fmt("-0.1=%d 1.1=%d NaN=%d v=%g", b1, b2, b3, v));
    // PlayWav with no volume plays at the stored volume: the volume given to the audio library is 0.4 * the Custom level * the Main level
    const bool p = m_ves->PlayWav(SND_VOL, true);
    m_ves->GetVolume(SND_VOL, v);
    Report(p && Near(v, 0.4, 1e-5) && m_ves->IsWavPlaying(SND_VOL), "soundvolume.playwav_default_uses_stored", Fmt("play=%d storedAfter=%g playing=%d", p, v, m_ves->IsWavPlaying(SND_VOL)));
    float custom = -1, mainLevel = -1;
    m_ves->GetVolume(VT::Custom, custom);
    m_ves->GetVolume(VT::Main, mainLevel);
    m_log.Read();
    const double logged = m_log.LastVolume(m_vessel->GetName(), SND_VOL);
    Report(LoggedNear(logged, 0.4 * custom * mainLevel), "soundvolume.playwav_default_plays_at_stored",
           Fmt("logged %s, expected 0.4 x Custom %g x Main %g = %.4f", VolStr(logged).c_str(), custom, mainLevel, 0.4 * custom * mainLevel));
    // SetVolume on a playing looping sound must not restart it
    m_pVol1 = m_ves->GetPlayPosition(SND_VOL);
    m_tVol1 = GetTickCount64();
    const bool s2 = m_ves->SetVolume(SND_VOL, 0.2f);
    m_pVol2 = m_ves->GetPlayPosition(SND_VOL);
    m_tVol2 = GetTickCount64();
    m_ves->GetVolume(SND_VOL, v);
    const bool playing = m_ves->IsWavPlaying(SND_VOL);
    Report(s2 && Near(v, 0.2, 1e-5) && playing && (m_pVol2 >= m_pVol1) && (m_pVol2 - m_pVol1 < 100), "soundvolume.setvolume_while_playing_immediate",
           Fmt("set=%d stored=%g playing=%d posBefore=%d posAfter=%d wallMs=%llu", s2, v, playing, m_pVol1, m_pVol2, static_cast<unsigned long long>(m_tVol2 - m_tVol1)));
    // an explicit volume updates the stored value, even while playing
    const bool p2 = m_ves->PlayWav(SND_VOL, true, 0.7f);
    m_ves->GetVolume(SND_VOL, v);
    Report(p2 && Near(v, 0.7, 1e-5), "soundvolume.explicit_updates_stored", Fmt("play=%d stored=%g", p2, v));
    const bool p3 = m_ves->PlayWav(SND_VOL, true);
    m_ves->GetVolume(SND_VOL, v);
    Report(p3 && Near(v, 0.7, 1e-5), "soundvolume.default_keeps_explicit", Fmt("play=%d stored=%g", p3, v));
    // a negative volume other than UseStoredVolume is treated as 0
    const bool p4 = m_ves->PlayWav(SND_VOL, true, -0.5f);
    m_ves->GetVolume(SND_VOL, v);
    Report(p4 && Near(v, 0.0, 1e-5), "soundvolume.negative_treated_as_zero", Fmt("play=%d stored=%g", p4, v));
    m_ves->SetVolume(SND_VOL, 0.5f);

    // ---- playback types
    PT pt = PT::InternalOnly;
    const bool g5 = m_ves->GetPlaybackType(SND_VOL, pt);
    Report(g5 && (pt == PT::Global), "playbacktype.initial_is_load_type", Fmt("get=%d type=%d", g5, static_cast<int>(pt)));
    bool allOk = true;
    std::string detail;
    for (int t = 1; t <= 7; ++t)
    {
        PT got = PT::InternalOnly;
        const bool s5 = m_ves->SetPlaybackType(SND_VOL, static_cast<PT>(t));
        const bool g6 = m_ves->GetPlaybackType(SND_VOL, got);
        if (!(s5 && g6 && (got == static_cast<PT>(t)))) { allOk = false; detail += Fmt("[%d: set=%d get=%d got=%d]", t, s5, g6, static_cast<int>(got)); }
    }
    Report(allOk, "playbacktype.roundtrip_all_7", detail);
    m_ves->SetPlaybackType(SND_VOL, PT::Radio);
    PT keep = PT::Wind;
    const bool i1 = m_ves->SetPlaybackType(SND_VOL, static_cast<PT>(0)), i2 = m_ves->SetPlaybackType(SND_VOL, static_cast<PT>(8)), i3 = m_ves->SetPlaybackType(SND_VOL, static_cast<PT>(-1));
    m_ves->GetPlaybackType(SND_VOL, keep);
    Report(!i1 && !i2 && !i3 && (keep == PT::Radio), "playbacktype.invalid_rejected", Fmt("0=%d 8=%d -1=%d current=%d", i1, i2, i3, static_cast<int>(keep)));
    PT sent = PT::Wind;
    const bool unk = m_ves->SetPlaybackType(7777, PT::Radio) || m_ves->GetPlaybackType(7777, sent);
    Report(!unk && (sent == PT::Wind), "playbacktype.unknown_id_rejected", Fmt("sent=%d", static_cast<int>(sent)));
    // LoadWav of the same file (already loaded) applies the new type and replaces the SetPlaybackType override
    const bool ld2 = m_ves->LoadWav(SND_VOL, FILE_WAV, PT::BothViewClose);
    PT after = PT::Wind;
    m_ves->GetPlaybackType(SND_VOL, after);
    Report(ld2 && (after == PT::BothViewClose), "playbacktype.loadwav_replaces_override", Fmt("load=%d type=%d (expected %d)", ld2, static_cast<int>(after), static_cast<int>(PT::BothViewClose)));
    Report(m_ves->IsWavPlaying(SND_VOL), "playbacktype.loadwav_same_file_keeps_playing");
    // a default sound keeps its override
    PT dorig = PT::InternalOnly;
    const bool dg0 = m_ves->GetPlaybackType(XRSound::SwitchOn, dorig);
    const bool ds = m_ves->SetPlaybackType(XRSound::SwitchOn, PT::Radio);
    PT dgot = PT::Wind;
    const bool dg = m_ves->GetPlaybackType(XRSound::SwitchOn, dgot);
    Report(ds && dg && (dgot == PT::Radio), "playbacktype.default_sound_override", Fmt("initialGet=%d initial=%d set=%d get=%d got=%d", dg0, static_cast<int>(dorig), ds, dg, static_cast<int>(dgot)));
    if (dg0) m_ves->SetPlaybackType(XRSound::SwitchOn, dorig);
    // a module stores the type (module sounds always play as global sounds)
    const bool ml = m_mod->LoadWav(SND_MOD, FILE_WAV, PT::Global);
    const bool ms = m_mod->SetPlaybackType(SND_MOD, PT::Wind);
    PT mg = PT::Global;
    const bool mgg = m_mod->GetPlaybackType(SND_MOD, mg);
    Report(ml && ms && mgg && (mg == PT::Wind), "playbacktype.module_stored", Fmt("load=%d set=%d get=%d got=%d", ml, ms, mgg, static_cast<int>(mg)));
    m_mod->SetPlaybackType(SND_MOD, PT::Global);
}

void TestModule::FinishVolumeStage(const ULONGLONG now)
{
    const int p3 = m_ves->GetPlayPosition(SND_VOL);
    // the position must have kept advancing since the SetVolume call, by about the real time elapsed, and not restarted
    const long long wall = static_cast<long long>(now - m_tVol2);
    Report(m_ves->IsWavPlaying(SND_VOL) && (p3 > m_pVol2 + 300) && (p3 - m_pVol2 >= wall - 250) && (p3 - m_pVol2 <= wall + 400), "soundvolume.setvolume_does_not_restart",
           Fmt("posBefore=%d posAfter(+%lldms wall)=%d delta=%d playing=%d", m_pVol2, wall, p3, p3 - m_pVol2, m_ves->IsWavPlaying(SND_VOL)));
    m_ves->StopWav(SND_VOL);
    Report(!m_ves->IsWavPlaying(SND_VOL), "sound.stop");
    // the stored volume survives StopWav
    float v = -1;
    m_ves->GetVolume(SND_VOL, v);
    Report(Near(v, 0.5, 1e-5), "soundvolume.survives_stop", Fmt("v=%g", v));
    // loading a DIFFERENT file into the ID resets its volume to 1.0
    const bool ld = m_ves->LoadWav(SND_VOL, FILE_OGG, PT::Global);
    m_ves->GetVolume(SND_VOL, v);
    Report(ld && Near(v, 1.0, 1e-5), "soundvolume.different_file_resets_to_1", Fmt("load=%d v=%g", ld, v));
    double verbose = 0;
    m_ves->GetDefaultSetting(SS::EnableVerboseLogging, verbose);
    m_ves->SetSetting(SS::EnableVerboseLogging, verbose);
}

// =====================================================================================================
// Pan, speed, and position set before a sound starts
// =====================================================================================================
void TestModule::RunPreStart()
{
    const bool ld = m_ves->LoadWav(SND_PRE, FILE_WAV, PT::Global);
    Report(ld, "prestart.load");
    // the "not playing" values while stopped, with nothing pending
    Report((m_ves->GetPan(SND_PRE) == -100.0f) && (m_ves->GetPlaybackSpeed(SND_PRE) == 0.0f) && (m_ves->GetPlayPosition(SND_PRE) < 0), "prestart.sentinels_before",
           Fmt("pan=%g speed=%g pos=%d", m_ves->GetPan(SND_PRE), m_ves->GetPlaybackSpeed(SND_PRE), m_ves->GetPlayPosition(SND_PRE)));
    const bool sp = m_ves->SetPan(SND_PRE, -0.5f);
    const bool ss = m_ves->SetPlaybackSpeed(SND_PRE, 1.5f);
    const bool spos = m_ves->SetPlayPosition(SND_PRE, 4000);
    Report(sp && ss && spos, "prestart.setters_return_true_while_stopped", Fmt("pan=%d speed=%d pos=%d", sp, ss, spos));
    const float gp = m_ves->GetPan(SND_PRE), gs = m_ves->GetPlaybackSpeed(SND_PRE);
    const int gpos = m_ves->GetPlayPosition(SND_PRE);
    Report((gp == -100.0f) && (gs == 0.0f) && (gpos < 0), "prestart.getters_keep_sentinels_while_stopped", Fmt("pan=%g speed=%g pos=%d", gp, gs, gpos));
    const bool bp = m_ves->SetPan(SND_PRE, 1.5f), bs = m_ves->SetPlaybackSpeed(SND_PRE, 0.0f), bs2 = m_ves->SetPlaybackSpeed(SND_PRE, -1.0f);
    const bool bunk = m_ves->SetPan(7777, 0.0f) || m_ves->SetPlaybackSpeed(7777, 1.0f) || m_ves->SetPlayPosition(7777, 0);
    Report(!bp && !bs && !bs2 && !bunk, "prestart.invalid_values_rejected", Fmt("pan1.5=%d speed0=%d speed-1=%d unknownId=%d", bp, bs, bs2, bunk));
    const bool pl = m_ves->PlayWav(SND_PRE, true);
    const int pos = m_ves->GetPlayPosition(SND_PRE);
    const float pan = m_ves->GetPan(SND_PRE), spd = m_ves->GetPlaybackSpeed(SND_PRE);
    Report(pl && (pos >= 3900) && (pos <= 5500), "prestart.position_applied_at_play", Fmt("play=%d pos=%d (expected ~4000)", pl, pos));
    Report(Near(pan, -0.5, 0.02), "prestart.pan_applied_at_play", Fmt("pan=%g (expected -0.5)", pan));
    Report(Near(spd, 1.5, 0.02), "prestart.speed_applied_at_play", Fmt("speed=%g (expected 1.5)", spd));
    // the pending values apply to one start only: stop and play again, and it is centered, at speed 1.0, from the start
    m_ves->StopWav(SND_PRE);
    Report((m_ves->GetPan(SND_PRE) == -100.0f) && !m_ves->IsWavPlaying(SND_PRE), "prestart.sentinels_after_stop", Fmt("pan=%g", m_ves->GetPan(SND_PRE)));
    m_ves->PlayWav(SND_PRE, true);
    const int pos2 = m_ves->GetPlayPosition(SND_PRE);
    const float pan2 = m_ves->GetPan(SND_PRE), spd2 = m_ves->GetPlaybackSpeed(SND_PRE);
    Report((pos2 >= 0) && (pos2 < 800) && Near(pan2, 0.0, 0.02) && Near(spd2, 1.0, 0.02), "prestart.pending_values_are_one_shot", Fmt("pos=%d pan=%g speed=%g", pos2, pan2, spd2));
    // the setters still work while playing
    const bool wp = m_ves->SetPan(SND_PRE, 0.5f);
    const bool ws = m_ves->SetPlaybackSpeed(SND_PRE, 0.5f);
    const bool wpos = m_ves->SetPlayPosition(SND_PRE, 8000);
    const int pos3 = m_ves->GetPlayPosition(SND_PRE);
    Report(wp && ws && wpos && Near(m_ves->GetPan(SND_PRE), 0.5, 0.02) && Near(m_ves->GetPlaybackSpeed(SND_PRE), 0.5, 0.02) && (pos3 >= 7900) && (pos3 <= 9000), "prestart.setters_while_playing",
           Fmt("pan=%d speed=%d pos=%d -> pan=%g speed=%g pos=%d", wp, ws, wpos, m_ves->GetPan(SND_PRE), m_ves->GetPlaybackSpeed(SND_PRE), pos3));
    m_ves->StopWav(SND_PRE);
    // a pending position is checked against the duration (the tone is 12 s long)
    unsigned dur = 0;
    const bool gd = m_ves->GetDuration(SND_PRE, dur);
    const bool over = m_ves->SetPlayPosition(SND_PRE, 13000);
    const bool within = m_ves->SetPlayPosition(SND_PRE, 11000);
    m_ves->SetPlayPosition(SND_PRE, 0);
    Report(gd && !over && within, "prestart.position_checked_against_cached_duration", Fmt("duration=%u(%d) beyond=%d within=%d", dur, gd, over, within));
}

// =====================================================================================================
// GetDuration for every supported file type
// =====================================================================================================
void TestModule::RunDuration()
{
    struct F { int id; const char *file; const char *label; double expect; double tol; };
    const int wavHeader = WavHeaderMillis(FILE_WAV);
    // lossy encoders pad the start and end of the tone a little (the MP3 is 12.07 s long by ffprobe), so OGG and MP3 get a wider tolerance
    const F files[] = { { SND_DUR_WAV, FILE_WAV, "wav", (wavHeader > 0) ? wavHeader : 12000.0, 3 }, { SND_DUR_OGG, FILE_OGG, "ogg", 12000, 120 },
                        { SND_DUR_MP3, FILE_MP3, "mp3", 12000, 120 }, { SND_DUR_FLAC, FILE_FLAC, "flac", 12000, 3 } };
    for (const F &f : files)
    {
        const bool ld = m_ves->LoadWav(f.id, f.file, PT::Global);
        unsigned ms = 0xDEADBEEF;
        const ULONGLONG t0 = GetTickCount64();
        const bool g = m_ves->GetDuration(f.id, ms);
        const ULONGLONG t1 = GetTickCount64();
        unsigned ms2 = 0;
        const bool g2 = m_ves->GetDuration(f.id, ms2);
        Report(ld && g && g2 && Near(ms, f.expect, f.tol) && (ms == ms2), Fmt("duration.%s", f.label),
               Fmt("load=%d get=%d ms=%u second=%u expected=%g(+/-%g) wavHeaderMs=%d firstCallWallMs=%llu", ld, g, ms, ms2, f.expect, f.tol, wavHeader, static_cast<unsigned long long>(t1 - t0)));
    }
    unsigned sentinel = 0xDEADBEEF;
    const bool unk = m_ves->GetDuration(7777, sentinel);
    Report(!unk && (sentinel == 0xDEADBEEF), "duration.unknown_id_false", Fmt("get=%d value=0x%X", unk, sentinel));
    // a file that does not exist: LoadWav fails, and so does GetDuration
    const bool ldBad = m_ves->LoadWav(SND_MISSING, "XRSound\\ApiTest\\no_such_file.wav", PT::Global);
    unsigned s2 = 0xDEADBEEF;
    const bool gBad = m_ves->GetDuration(SND_MISSING, s2);
    Report(!ldBad && !gBad && (s2 == 0xDEADBEEF), "duration.missing_file_false", Fmt("load=%d get=%d", ldBad, gBad));
    unsigned dms = 0xDEADBEEF;
    const bool dg = m_ves->GetDuration(XRSound::SwitchOn, dms);
    Log(Fmt("XRSOUNDTEST INFO GetDuration(default SwitchOn)=%d ms=%u", dg, dg ? dms : 0));
    m_mod->LoadWav(SND_MOD, FILE_WAV, PT::Global);
    unsigned mms = 0;
    const bool mg = m_mod->GetDuration(SND_MOD, mms);
    Report(mg && Near(mms, (wavHeader > 0) ? wavHeader : 12000, 3), "duration.module_proxy", Fmt("get=%d ms=%u", mg, mms));
}

// =====================================================================================================
// Playback type fades and SilenceOfSpace, in the external view in space: each playback type is set on a sound that is then started,
// and the first volume that XRSound logs for it must match the fade for that type (see VesselXRSoundEngine::ComputeFinalVolume).
// =====================================================================================================
double TestModule::CameraDistance(VESSEL *pVessel) const
{
    VECTOR3 vesselPos, cameraPos;
    pVessel->Local2Global(_V(0, 0, 0), vesselPos);
    oapiCameraGlobalPos(&cameraPos);
    return length(vesselPos - cameraPos);
}

// The volume that XRSound gives a sound of volume 1.0 of this playback type, for the focus vessel in the external view.  This restates
// VesselXRSoundEngine::ComputeFinalVolume and its fades on purpose, so that a change to them fails here until the test is updated to match.
double TestModule::ExpectedFade(const PT type, const bool silenceOfSpace, const double cameraDistance, const double pressure)
{
    const bool inAtmosphere = (pressure > 0.1);     // VesselXRSoundEngine::InAtmosphere
    const double pressureFade = silenceOfSpace ? std::min(1.0, pressure / (101325.0 / 10)) : 1;     // full volume at a tenth of an atmosphere
    switch (type)
    {
    case PT::InternalOnly: return 0;        // cockpit view only
    case PT::Radio: return 1;               // the vessel has focus
    case PT::Global: return 1;
    case PT::Wind: return pressureFade;
    case PT::BothViewFar: case PT::BothViewMedium: case PT::BothViewClose:
    {
        if (silenceOfSpace && !inAtmosphere) return 0;      // silent in vacuum
        const double loudest = (type == PT::BothViewFar) ? 125 : (type == PT::BothViewMedium) ? 50 : 20;    // the distance at which it is loudest
        const double v = ((cameraDistance > loudest) ? (loudest * loudest) / (cameraDistance * cameraDistance) : 1) * pressureFade;
        return (v < 0.04) ? 0 : v;          // too faint to carry
    }
    }
    return -1;
}

// Starts the sound afresh at volume 1.0 and returns the volume that XRSound logged for it
double TestModule::StartedVolume(XRSound *p, const char *logID, const int soundID)
{
    p->StopWav(soundID);
    m_log.SkipToEnd();
    p->PlayWav(soundID, true, 1.0f);
    m_log.Read();
    return m_log.LastVolume(logID, soundID);
}

// Checks every playback type, with SilenceOfSpace on and then off, on a sound of the focus vessel p (in the external view); situation names the checks
void TestModule::CheckFades(XRSound *p, VESSEL *pVessel, const char *situation)
{
    const char *logID = pVessel->GetName();
    const double d = CameraDistance(pVessel);
    const double pressure = pVessel->GetAtmPressure();
    Log(Fmt("XRSOUNDTEST INFO fades %s: camera %.1f m from '%s', static pressure %g Pa", situation, d, logID, pressure));
    p->LoadWav(SND_FADE, FILE_WAV, PT::Global);
    for (const bool silence : { true, false })
    {
        p->SetSetting(SS::SilenceOfSpace, silence ? 1 : 0);
        bool ok = true;
        std::string detail = Fmt("camera %.1f m, pressure %g Pa: ", d, pressure);
        for (int t = static_cast<int>(PT::InternalOnly); t <= static_cast<int>(PT::Global); ++t)
        {
            const PT type = static_cast<PT>(t);
            p->SetPlaybackType(SND_FADE, type);
            const double v = StartedVolume(p, logID, SND_FADE);
            const double expected = ExpectedFade(type, silence, d, pressure);
            const bool typeOk = LoggedNear(v, expected);
            if (!typeOk) ok = false;
            detail += Fmt("[type %d: %s, expected %.4f%s]", t, VolStr(v).c_str(), expected, typeOk ? "" : " WRONG");
        }
        Report(ok, Fmt("playbacktype.fades.%s.SilenceOfSpace_%s", situation, silence ? "on" : "off"), detail);
    }
    p->StopWav(SND_FADE);
    RestoreSetting(p, SS::SilenceOfSpace);
}

void TestModule::RunFadeChecks()
{
    PrepareLevels();
    const char *ves1Name = m_vessel->GetName();
    CheckFades(m_ves, m_vessel, "in_space");
    // SetPlaybackType on a sound that is playing takes effect at once, without restarting it: Global (1.0) to InternalOnly (0 in the external view) and back
    {
        m_ves->SetPlaybackType(SND_FADE, PT::Global);
        const double started = StartedVolume(m_ves, ves1Name, SND_FADE);
        m_ves->SetPlayPosition(SND_FADE, 3000);     // so that a restart would show as a position back near 0
        const int pos0 = m_ves->GetPlayPosition(SND_FADE);
        m_log.SkipToEnd();
        const bool s1 = m_ves->SetPlaybackType(SND_FADE, PT::InternalOnly);
        m_log.Read();
        const double internalOnly = m_log.LastVolume(ves1Name, SND_FADE);
        m_log.SkipToEnd();
        const bool s2 = m_ves->SetPlaybackType(SND_FADE, PT::Global);
        m_log.Read();
        const double global = m_log.LastVolume(ves1Name, SND_FADE);
        const int pos1 = m_ves->GetPlayPosition(SND_FADE);
        const bool playing = m_ves->IsWavPlaying(SND_FADE);
        Report(s1 && s2 && LoggedNear(started, 1) && LoggedNear(internalOnly, 0) && LoggedNear(global, 1) && playing && (pos0 >= 2900) && (pos1 >= pos0),"playbacktype.change_while_playing_takes_effect_at_once",
               Fmt("started Global at %s; set InternalOnly=%d: logged %s (expected 0); set Global=%d: logged %s (expected 1); still playing=%d, position %d -> %d (expected no restart)",
                   VolStr(started).c_str(), s1, VolStr(internalOnly).c_str(), s2, VolStr(global).c_str(), playing, pos0, pos1));
        m_ves->StopWav(SND_FADE);
    }
    // a radio sound plays only for the vessel that has focus
    if (g_proxies[2])
    {
        const char *ves2Name = m_vessel2->GetName();
        g_proxies[2]->LoadWav(SND_FADE, FILE_WAV, PT::Radio);
        const double radio = StartedVolume(g_proxies[2], ves2Name, SND_FADE);
        g_proxies[2]->SetPlaybackType(SND_FADE, PT::Global);
        const double global = StartedVolume(g_proxies[2], ves2Name, SND_FADE);
        g_proxies[2]->StopWav(SND_FADE);
        Report(LoggedNear(radio, 0) && LoggedNear(global, 1), "playbacktype.radio_needs_focus", Fmt("second vessel (no focus): Radio %s (expected 0), Global %s (expected 1)", VolStr(radio).c_str(), VolStr(global).c_str()));
    }
    RestoreDefaultLevels();
}

// =====================================================================================================
// Playback type fades in the atmosphere, with the third vessel in focus, held 25 km up: the air pressure there (a few kPa) is below the
// tenth of an atmosphere at which XRSound plays at full volume, so the pressure fade is partial.
// =====================================================================================================
void TestModule::StartAtmosphereFadeStep()
{
    if (!m_ves3)
    {
        Skip("playbacktype.fades.in_atmosphere", "no third XRSound vessel in the scenario");
        return;
    }
    m_holdStatus = AirborneStatus(25000, 150, 0);
    m_holdThird = true;
    m_vessel3->DefSetStateEx(&m_holdStatus);
    m_ves3->SetSetting(SS::EnableVerboseLogging, 1);
    for (const VT t : ALL_VOLUME_TYPES)
        m_ves3->SetVolume(t, 1.0f);
}

bool TestModule::AtmosphereFadeTick(const ULONGLONG now)
{
    if (!m_ves3) return true;
    if (now - m_tStep < 1500) return false;     // let the vessel settle into its held state
    const double fraction = m_vessel3->GetAtmPressure() / (101325.0 / 10);
    Report((fraction > 0.05) && (fraction < 0.95), "playbacktype.fades.in_atmosphere.partial_pressure",
           Fmt("static pressure %.0f Pa at %.0f m: %.3f of the pressure for full volume (expected between 0.05 and 0.95)", m_vessel3->GetAtmPressure(), m_vessel3->GetAltitude(), fraction));
    CheckFades(m_ves3, m_vessel3, "in_atmosphere");
    for (const VT t : ALL_VOLUME_TYPES)
    {
        float def = 1.0f;
        if (m_ves3->GetDefaultVolume(t, def))
            m_ves3->SetVolume(t, def);
    }
    RestoreSetting(m_ves3, SS::EnableVerboseLogging);
    PlaceThirdVessel(Situation::Landed, true);      // park it
    oapiSetFocusObject(m_vessel->GetHandle());
    oapiCameraAttach(m_vessel->GetHandle(), 1);
    return true;
}

// =====================================================================================================
// Module sound: it plays, and changing its volume while it plays does not restart it
// =====================================================================================================
void TestModule::StartModuleSound()
{
    float v = -1;
    const bool g = m_mod->GetVolume(SND_MOD, v);
    Report(g && Near(v, 1.0, 1e-5), "module.sound_volume_default_1", Fmt("get=%d v=%g", g, v));
    const bool s = m_mod->SetVolume(SND_MOD, 0.6f);
    const bool p = m_mod->PlayWav(SND_MOD, true);
    m_pMod0 = m_mod->GetPlayPosition(SND_MOD);
    m_mod->GetVolume(SND_MOD, v);
    Report(s && p && Near(v, 0.6, 1e-5) && m_mod->IsWavPlaying(SND_MOD), "module.sound_plays_with_stored_volume", Fmt("set=%d play=%d stored=%g", s, p, v));
    m_mod->SetVolume(VT::Custom, 0.5f);
    m_mod->SetVolume(SND_MOD, 0.3f);
}

void TestModule::FinishModuleSound()
{
    const int p = m_mod->GetPlayPosition(SND_MOD);
    Report(m_mod->IsWavPlaying(SND_MOD) && (p - m_pMod0 >= 300), "module.setvolume_while_playing_does_not_restart", Fmt("pos0=%d pos1=%d", m_pMod0, p));
    float def = 1.0f;
    m_mod->GetDefaultVolume(VT::Custom, def);
    m_mod->SetVolume(VT::Custom, def);
    m_mod->SetVolume(SND_MOD, 1.0f);
    m_mod->StopWav(SND_MOD);
}

// =====================================================================================================
// Simulation pause: sounds hold their position while paused, calls still work, and they continue from there on resume
// =====================================================================================================
void TestModule::StartPauseStage()
{
    const bool ld = m_ves->LoadWav(SND_PAUSE, FILE_WAV, PT::Global);
    m_ves->SetVolume(SND_PAUSE, 1.0f);
    const bool p = m_ves->PlayWav(SND_PAUSE, true);
    Report(ld && p && m_ves->IsWavPlaying(SND_PAUSE), "pause.setup", Fmt("load=%d play=%d", ld, p));
}

void TestModule::RequestPause(const ULONGLONG now)
{
    m_p0 = m_ves->GetPlayPosition(SND_PAUSE);
    m_tPauseReq = now;
    Log(Fmt("XRSOUNDTEST INFO requesting sim pause: sound %d position %d ms, tick %llu", SND_PAUSE, m_p0, static_cast<unsigned long long>(now)));
    oapiSetPause(true);
}

void TestModule::PauseTick(const ULONGLONG now)
{
    if (!m_pauseHookStarted)
    {
        m_pauseHookStarted = true;
        m_tPauseHook = now;
        m_pA = m_ves->GetPlayPosition(SND_PAUSE);
        Log(Fmt("XRSOUNDTEST INFO paused: first keyboard-hook call %llu ms after the pause request; position %d (was %d at request)", static_cast<unsigned long long>(now - m_tPauseReq), m_pA, m_p0));
        return;
    }
    const ULONGLONG elapsed = now - m_tPauseHook;
    if (!m_pauseActionsDone && (elapsed >= 500))
    {
        m_pauseActionsDone = true;
        const bool a1 = m_ves->SetVolume(SND_PAUSE, 0.3f);
        float vv = -1;
        m_ves->GetVolume(SND_PAUSE, vv);
        const bool airConditioning = m_ves->GetDefaultSoundEnabled(XRSound::AirConditioning);
        const bool a2 = m_ves->SetDefaultSoundEnabled(XRSound::AirConditioning, false);
        const bool a3 = m_ves->SetDefaultSoundEnabled(XRSound::AirConditioning, true);
        m_ves->SetDefaultSoundEnabled(XRSound::AirConditioning, airConditioning);
        float custom = 1.0f;
        m_ves->GetVolume(VT::Custom, custom);
        const bool a4 = m_ves->PlayWav(SND_PAUSE, true);                // already playing: only its loop and volume change
        const bool a5 = m_ves->SetVolume(VT::Custom, 0.8f);
        const bool a6 = m_ves->SetPlaybackType(SND_PAUSE, PT::Radio);
        const bool a7 = m_ves->SetPaused(SND_PAUSE, false);             // an explicit un-pause while the sim itself is paused
        m_ves->SetVolume(VT::Custom, custom);
        m_pB = m_ves->GetPlayPosition(SND_PAUSE);
        Report(a1 && a2 && a3 && a4 && a5 && a6 && a7 && Near(vv, 0.3, 1e-5), "pause.calls_succeed_while_paused",
               Fmt("SetVolume=%d(v=%g) SetDefaultSoundEnabled(false)=%d (true)=%d PlayWav=%d SetVolume(Custom)=%d SetPlaybackType=%d SetPaused(false)=%d posAfterCalls=%d", a1, vv, a2, a3, a4, a5, a6, a7, m_pB));
    }
    if (!m_pauseMidDone && (elapsed >= 2500))
    {
        m_pauseMidDone = true;
        m_pMid = m_ves->GetPlayPosition(SND_PAUSE);
    }
    if (elapsed >= 4000)
    {
        m_pC = m_ves->GetPlayPosition(SND_PAUSE);
        const bool playing = m_ves->IsWavPlaying(SND_PAUSE);
        // the sim was paused for 'elapsed' ms of real time; the position must not have advanced by anything like that
        Report(playing && (m_pC - m_pA <= 400) && (m_pC >= m_pA), "pause.position_frozen_during_sim_pause",
               Fmt("wallPausedMs=%llu posAtPause=%d posAfterCalls=%d posMid(2500)=%d posEnd=%d deltaDuringPause=%d playing=%d hookCallsWhilePaused=%d",
                   static_cast<unsigned long long>(elapsed), m_pA, m_pB, m_pMid, m_pC, m_pC - m_pA, playing, m_hookPausedCalls));
        Report((m_pB - m_pA <= 300) && (m_pMid - m_pB <= 300), "pause.position_frozen_after_calls", Fmt("A=%d B=%d mid=%d end=%d", m_pA, m_pB, m_pMid, m_pC));
        m_tResumeReq = now;
        m_stage = ST_RESUME_FIRST;
        oapiSetPause(false);
    }
}
