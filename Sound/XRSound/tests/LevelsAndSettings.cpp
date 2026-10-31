// ==============================================================
// LevelsAndSettings.cpp : XRSoundApiTest checks of the version, volume types, settings, default sound volumes, sound group folders, and the volume mix
// ==============================================================

#include <algorithm>
#include <cstring>
#include "XRSoundApiTest.h"

// Tests one numeric setting on proxy idx.  Every proxy (vessel or module) must accept it.  Also checks that:
//  - GetDefaultSetting equals the initial GetSetting value and never changes,
//  - the other proxies' values are unaffected by every change made here (isolation).
static void TestNumericSetting(const int idx, const NumSetting &ns)
{
    XRSound *p = g_proxies[idx];
    const char *proxyLabel = g_proxyLabels[idx];
    const std::string prefix = Fmt("setting.%s.%s.", proxyLabel, ns.name);
    double orig = -777;
    const bool g0 = p->GetSetting(ns.s, orig);
    double def0 = -778;
    const bool d0 = p->GetDefaultSetting(ns.s, def0);
    Report(g0 && d0 && Near(orig, def0, 1e-9), prefix + "get_initial_equals_default", Fmt("get=%d initial=%g getDefault=%d default=%g", g0, orig, d0, def0));
    if (!g0) return;
    double otherOrig[3] = { 0, 0, 0 };
    bool otherOk[3] = { false, false, false };
    for (int k = 0; k < 3; ++k)
        if ((k != idx) && g_proxies[k]) otherOk[k] = g_proxies[k]->GetSetting(ns.s, otherOrig[k]);
    double last = orig;
    bool allValid = true, defaultsStable = true, isolated = true;
    int differing = 0;
    std::string detail, isolationDetail;
    for (const double v : ns.valid)
    {
        double got = -999;
        const bool ok = p->SetSetting(ns.s, v);
        const bool gg = p->GetSetting(ns.s, got);
        if (!(ok && gg && Near(got, v, 1e-6 * (1 + std::fabs(v))))) { allValid = false; detail += Fmt("[set(%g)=%d get=%d got=%g]", v, ok, gg, got); }
        double dd = -1;
        if (!(p->GetDefaultSetting(ns.s, dd) && Near(dd, def0, 1e-9))) defaultsStable = false;
        for (int k = 0; k < 3; ++k)
        {
            if ((k == idx) || !g_proxies[k] || !otherOk[k]) continue;
            double ov = -1;
            const bool og = g_proxies[k]->GetSetting(ns.s, ov);
            if (!Near(v, otherOrig[k], 1e-9)) ++differing;
            if (!og || !Near(ov, otherOrig[k], 1e-9)) { isolated = false; isolationDetail += Fmt("[after %s.set(%g): %s=%g expected %g]", proxyLabel, v, g_proxyLabels[k], ov, otherOrig[k]); }
        }
        last = v;
    }
    Report(allValid, prefix + "valid_roundtrip", Fmt("values=%zu ", ns.valid.size()) + detail);
    Report(defaultsStable, prefix + "default_unchanged_by_set", Fmt("default=%g", def0));
    Report(isolated, prefix + "isolated_from_other_proxies", Fmt("others compared=%d (value differed from the other's in %d comparisons) ", otherOk[0] + otherOk[1] + otherOk[2], differing) + isolationDetail);
    bool allInvalid = true;
    detail.clear();
    for (const double v : ns.invalid)
    {
        double got = -999;
        const bool ok = p->SetSetting(ns.s, v);
        const bool gg = p->GetSetting(ns.s, got);
        if (ok || !gg || !Near(got, last, 1e-6 * (1 + std::fabs(last)))) { allInvalid = false; detail += Fmt("[set(%g)=%d get=%d got=%g expected-unchanged=%g]", v, ok, gg, got, last); }
    }
    Report(allInvalid, prefix + "invalid_rejected", Fmt("values=%zu ", ns.invalid.size()) + detail);
    // a numeric setting through the string forms must fail (set, get, getDefault)
    char buf[64] = "keep", buf2[64] = "keep2";
    const bool sg = p->GetSetting(ns.s, buf, sizeof(buf));
    const bool sd = p->GetDefaultSetting(ns.s, buf2, sizeof(buf2));
    const bool ss = p->SetSetting(ns.s, "1");
    Report(!sg && !sd && !ss && !strcmp(buf, "keep") && !strcmp(buf2, "keep2"), prefix + "string_forms_refused", Fmt("getStr=%d getDefaultStr=%d setStr=%d", sg, sd, ss));
    // restore, and the default must still be what it was
    const bool r = p->SetSetting(ns.s, orig);
    double back = -888, dd2 = -889;
    p->GetSetting(ns.s, back);
    p->GetDefaultSetting(ns.s, dd2);
    Report(r && Near(back, orig, 1e-6 * (1 + std::fabs(orig))) && Near(dd2, def0, 1e-9), prefix + "restore", Fmt("orig=%g back=%g default=%g", orig, back, dd2));
}

// Returns a string setting of proxy p.  The only string setting is CustomEnginesThrusterIndexes, and every value this test sets is far
// shorter than the buffer.
static std::string GetStringSetting(XRSound *p, const SS s, bool &ok, const bool bDefault = false)
{
    std::vector<char> buf(512, '\0');
    ok = bDefault ? p->GetDefaultSetting(s, buf.data(), static_cast<int>(buf.size())) : p->GetSetting(s, buf.data(), static_cast<int>(buf.size()));
    return buf.data();
}

static void TestStringSetting(const int idx, const SS s, const char *sname, const std::vector<const char *> &valid, const std::vector<const char *> &invalid)
{
    XRSound *p = g_proxies[idx];
    const char *proxyLabel = g_proxyLabels[idx];
    const std::string prefix = Fmt("setting.%s.%s.", proxyLabel, sname);
    bool g0 = false, d0 = false;
    const std::string orig = GetStringSetting(p, s, g0);
    const std::string def0 = GetStringSetting(p, s, d0, true);
    Report(g0 && d0 && (orig == def0), prefix + "get_initial_equals_default", Fmt("get=%d initial='%s' getDefault=%d default='%s'", g0, orig.c_str(), d0, def0.c_str()));
    if (!g0) return;
    std::string otherOrig[3];
    bool otherOk[3] = { false, false, false };
    for (int k = 0; k < 3; ++k)
        if ((k != idx) && g_proxies[k]) otherOrig[k] = GetStringSetting(g_proxies[k], s, otherOk[k]);
    std::string last = orig;
    bool allValid = true, defaultsStable = true, isolated = true;
    std::string detail, isolationDetail;
    for (const char *v : valid)
    {
        const bool ok = p->SetSetting(s, v);
        bool gg = false, dg = false;
        const std::string got = GetStringSetting(p, s, gg);
        if (!(ok && gg && (got == v))) { allValid = false; detail += Fmt("[set('%s')=%d get=%d got='%s']", v, ok, gg, got.c_str()); }
        if (!((GetStringSetting(p, s, dg, true) == def0) && dg)) defaultsStable = false;
        for (int k = 0; k < 3; ++k)
        {
            if ((k == idx) || !g_proxies[k] || !otherOk[k]) continue;
            bool og = false;
            const std::string ov = GetStringSetting(g_proxies[k], s, og);
            if (!og || (otherOrig[k] != ov)) { isolated = false; isolationDetail += Fmt("[after %s.set('%s'): %s='%s' expected '%s']", proxyLabel, v, g_proxyLabels[k], ov.c_str(), otherOrig[k].c_str()); }
        }
        last = v;
    }
    Report(allValid, prefix + "valid_roundtrip", Fmt("values=%zu ", valid.size()) + detail);
    Report(defaultsStable, prefix + "default_unchanged_by_set", Fmt("default='%s'", def0.c_str()));
    Report(isolated, prefix + "isolated_from_other_proxies", isolationDetail);
    bool allInvalid = true;
    detail.clear();
    {
        const bool ok = p->SetSetting(s, static_cast<const char *>(nullptr));
        bool gg = false;
        const std::string got = GetStringSetting(p, s, gg);
        if (ok || !gg || (last != got)) { allInvalid = false; detail += Fmt("[set(nullptr)=%d get=%d got='%s']", ok, gg, got.c_str()); }
    }
    for (const char *v : invalid)
    {
        const bool ok = p->SetSetting(s, v);
        bool gg = false;
        const std::string got = GetStringSetting(p, s, gg);
        if (ok || !gg || (last != got)) { allInvalid = false; detail += Fmt("[set('%s')=%d get=%d got='%s' expected-unchanged='%s']", v, ok, gg, got.c_str(), last.c_str()); }
    }
    Report(allInvalid, prefix + "invalid_rejected", Fmt("values=%zu+nullptr ", invalid.size()) + detail);
    // buffer sizing (GetSetting and GetDefaultSetting): a buffer with no room for the terminating null fails and is left untouched,
    // a buffer with exactly enough room succeeds, and a null buffer fails
    {
        // each too-small buffer is exactly as long as the size passed for it, so a write past that size shows up as a changed byte
        const size_t len = last.size();
        std::vector<char> tiny(len, 'Z');
        const bool tooSmall = p->GetSetting(s, tiny.data(), static_cast<int>(tiny.size()));
        const bool untouched = std::all_of(tiny.begin(), tiny.end(), [](const char c) { return c == 'Z'; });
        std::vector<char> exact(len + 1, 'Q');
        const bool fits = p->GetSetting(s, exact.data(), static_cast<int>(exact.size()));
        const bool fitsOk = fits && (last == exact.data());
        const bool nullBuf = p->GetSetting(s, nullptr, 16);
        std::vector<char> tiny2(def0.size(), 'Y');
        const bool dTooSmall = p->GetDefaultSetting(s, tiny2.data(), static_cast<int>(tiny2.size()));
        const bool dUntouched = std::all_of(tiny2.begin(), tiny2.end(), [](const char c) { return c == 'Y'; });
        const bool dNull = p->GetDefaultSetting(s, nullptr, 16);
        Report(!tooSmall && untouched && fitsOk && !nullBuf && !dTooSmall && dUntouched && !dNull, prefix + "buffer_sizing",
               Fmt("len=%zu tooSmall=%d untouched=%d exactFits=%d exactMatch=%d nullBuf=%d | default: tooSmall=%d untouched=%d nullBuf=%d", len, tooSmall, untouched, fits, fitsOk, nullBuf, dTooSmall, dUntouched, dNull));
    }
    // a string setting through the numeric forms must fail
    {
        double d = -321.5, d2 = -322.5;
        const bool gg = p->GetSetting(s, d);
        const bool gd = p->GetDefaultSetting(s, d2);
        const bool st = p->SetSetting(s, 1.0);
        Report(!gg && !gd && !st && (d == -321.5) && (d2 == -322.5), prefix + "numeric_forms_refused", Fmt("getNum=%d getDefaultNum=%d setNum=%d", gg, gd, st));
    }
    // restore (an empty original is a valid value)
    const bool r = p->SetSetting(s, orig.c_str());
    bool gb = false, gd = false;
    const std::string back = GetStringSetting(p, s, gb);
    const std::string def1 = GetStringSetting(p, s, gd, true);
    Report(r && (back == orig) && (def1 == def0), prefix + "restore", Fmt("orig='%s' back='%s' default='%s'", orig.c_str(), back.c_str(), def1.c_str()));
}

// =====================================================================================================
// Full mode, synchronous checks: version, volume types, settings, default sound volumes, sound group folders
// =====================================================================================================
void TestModule::RunSyncChecks()
{
    const float v = m_ves->GetVersion();
    Report(v > 4.1 - 0.0011, "version.at_least_4_1", Fmt("vessel GetVersion=%.4f module GetVersion=%.4f", v, m_mod->GetVersion()));

    TestVolumeTypes();
    TestSettings();
    TestSoundVolumeSync();
    TestGroupFolders();
    TestIntOverload();
}

void TestModule::TestVolumeTypes()
{
    for (int idx = 0; idx < 3; ++idx)
    {
        XRSound *p = g_proxies[idx];
        if (!p) continue;
        const char *lab = g_proxyLabels[idx];
        for (const VT t : ALL_VOLUME_TYPES)
        {
            const std::string prefix = Fmt("volume.%s.%s.", lab, VolName(t));
            // Main, ATC, and Music start at their XRSound.cfg levels; every other type starts at 1.0
            const bool expectOne = !((t == VT::Main) || (t == VT::ATC) || (t == VT::Music));
            float init = -7.0f, def = -8.0f;
            const bool g0 = p->GetVolume(t, init);
            const bool d0 = p->GetDefaultVolume(t, def);
            Report(g0 && d0 && Near(init, def, 1e-6) && (!expectOne || Near(init, 1.0, 1e-6)), prefix + "initial_equals_default",
                   Fmt("get=%d initial=%g getDefault=%d default=%g (expected 1.0 for this type: %d)", g0, init, d0, def, expectOne));
            float otherOrig[3] = { 0, 0, 0 };
            bool otherOk[3] = { false, false, false };
            for (int k = 0; k < 3; ++k)
                if ((k != idx) && g_proxies[k]) otherOk[k] = g_proxies[k]->GetVolume(t, otherOrig[k]);
            bool okAll = true, defStable = true, isolated = true;
            int differing = 0;
            std::string detail, isolationDetail;
            for (const float lvl : { 0.0f, 0.25f, 0.5f, 1.0f, 0.75f })
            {
                float got = -7.0f, dd = -9.0f;
                const bool s = p->SetVolume(t, lvl);
                const bool g = p->GetVolume(t, got);
                if (!(s && g && Near(got, lvl, 1e-5))) { okAll = false; detail += Fmt("[set(%g)=%d get=%d got=%g]", lvl, s, g, got); }
                if (!(p->GetDefaultVolume(t, dd) && Near(dd, def, 1e-6))) defStable = false;
                for (int k = 0; k < 3; ++k)
                {
                    if ((k == idx) || !g_proxies[k] || !otherOk[k]) continue;
                    float ov = -1;
                    const bool og = g_proxies[k]->GetVolume(t, ov);
                    if (!Near(lvl, otherOrig[k], 1e-6)) ++differing;
                    if (!og || !Near(ov, otherOrig[k], 1e-6)) { isolated = false; isolationDetail += Fmt("[after %s.set(%g): %s=%g expected %g]", lab, lvl, g_proxyLabels[k], ov, otherOrig[k]); }
                }
            }
            Report(okAll, prefix + "roundtrip", detail);
            Report(defStable, prefix + "default_unchanged_by_set", Fmt("default=%g", def));
            Report(isolated, prefix + "isolated_from_other_proxies", Fmt("(value differed from the others' in %d comparisons) ", differing) + isolationDetail);
            p->SetVolume(t, 0.5f);
            okAll = true;
            detail.clear();
            for (const float lvl : { -0.1f, 1.1f, NaNf, std::numeric_limits<float>::infinity() })
            {
                float got = -7.0f;
                const bool s = p->SetVolume(t, lvl);
                const bool g = p->GetVolume(t, got);
                if (s || !g || !Near(got, 0.5, 1e-5)) { okAll = false; detail += Fmt("[set(%g)=%d get=%d got=%g]", lvl, s, g, got); }
            }
            Report(okAll, prefix + "invalid_rejected", detail);
            p->SetVolume(t, def);     // back to the configured level
            float back = -1;
            p->GetVolume(t, back);
            Report(Near(back, def, 1e-6), prefix + "reset_to_default", Fmt("back=%g default=%g", back, def));
        }
        // invalid enum values (0 was the 4.0 Vessel type, which no longer exists; 10 is one past Custom)
        float sentinel = -7.0f;
        const bool bad = p->SetVolume(static_cast<VT>(0), 0.5f) || p->SetVolume(static_cast<VT>(10), 0.5f) || p->SetVolume(static_cast<VT>(99), 0.5f)
                      || p->GetVolume(static_cast<VT>(0), sentinel) || p->GetVolume(static_cast<VT>(10), sentinel) || p->GetVolume(static_cast<VT>(-1), sentinel)
                      || p->GetDefaultVolume(static_cast<VT>(0), sentinel) || p->GetDefaultVolume(static_cast<VT>(10), sentinel) || p->GetDefaultVolume(static_cast<VT>(-1), sentinel);
        Report(!bad && (sentinel == -7.0f), Fmt("volume.%s.invalid_type_rejected", lab), Fmt("sentinel=%g", sentinel));
    }
    // the default Main, ATC, and Music levels are XRSound.cfg's MasterVolume, [ATC] Volume, and MusicVolume, for the module and for the vessel
    // (the Delta-glider's override file, XRSound-DeltaGlider.cfg, does not change them)
    const double cfgMain = ReadXRSoundCfgLevel("SYSTEM", "MasterVolume", 1.0), cfgATC = ReadXRSoundCfgLevel("ATC", "Volume", 1.0), cfgMusic = ReadXRSoundCfgLevel("SYSTEM", "MusicVolume", 1.0);
    for (XRSound *p : { m_mod, m_ves })
    {
        float mainDef = -1, atcDef = -1, musicDef = -1;
        p->GetDefaultVolume(VT::Main, mainDef);
        p->GetDefaultVolume(VT::ATC, atcDef);
        p->GetDefaultVolume(VT::Music, musicDef);
        Report(Near(mainDef, cfgMain, 1e-5) && Near(atcDef, cfgATC, 1e-5) && Near(musicDef, cfgMusic, 1e-5), Fmt("volume.%s.config_defaults_match_xrsound_cfg", (p == m_mod) ? "mod" : "ves"),
               Fmt("Main=%g (MasterVolume %g in %s) ATC=%g ([ATC] Volume %g) Music=%g (MusicVolume %g)", mainDef, cfgMain, XRSOUND_CFG, atcDef, cfgATC, musicDef, cfgMusic));
    }
}

void TestModule::TestSettings()
{
    const auto table = MakeNumSettings();
    for (int idx = 0; idx < 3; ++idx)
    {
        if (!g_proxies[idx]) continue;
        for (const NumSetting &ns : table)
            TestNumericSetting(idx, ns);
        TestStringSetting(idx, SS::CustomEnginesThrusterIndexes, "CustomEnginesThrusterIndexes", { "0 1 4", "3", "", "0" }, { "-1", "a", "1.5", "0 b" });
    }
    // invalid enum values
    double d = -55.5, d2 = -56.5;
    char buf[16] = "keep";
    const bool bad = m_ves->SetSetting(static_cast<SS>(0), 1.0) || m_ves->SetSetting(static_cast<SS>(999), 1.0) || m_ves->SetSetting(static_cast<SS>(-3), "x")
                  || m_ves->GetSetting(static_cast<SS>(0), d) || m_ves->GetSetting(static_cast<SS>(999), d) || m_ves->GetSetting(static_cast<SS>(0), buf, sizeof(buf))
                  || m_ves->GetDefaultSetting(static_cast<SS>(0), d2) || m_ves->GetDefaultSetting(static_cast<SS>(999), d2) || m_ves->GetDefaultSetting(static_cast<SS>(0), buf, sizeof(buf));
    Report(!bad && (d == -55.5) && (d2 == -56.5) && !strcmp(buf, "keep"), "setting.invalid_enum_rejected", Fmt("d=%g d2=%g buf='%s'", d, d2, buf));
}

// SetSetting(s, 0), SetSetting(s, true), and SetSetting(s, 5) must compile (the int overload) and work
void TestModule::TestIntOverload()
{
    double orig = 0, got = -1;
    m_ves->GetSetting(SS::SilenceOfSpace, orig);
    const bool a = m_ves->SetSetting(SS::SilenceOfSpace, 0);
    const bool ga = m_ves->GetSetting(SS::SilenceOfSpace, got);
    const bool resA = a && ga && (got == 0.0);
    const bool b = m_ves->SetSetting(SS::SilenceOfSpace, true);
    const bool gb = m_ves->GetSetting(SS::SilenceOfSpace, got);
    const bool resB = b && gb && (got == 1.0);
    double origMin = 0;
    m_ves->GetSetting(SS::ATCMinDelay, origMin);
    const bool c = m_ves->SetSetting(SS::ATCMinDelay, 5);       // out of range (minimum 10), so rejected
    const bool c2 = m_ves->SetSetting(SS::ATCMinDelay, 60);
    const bool gc = m_ves->GetSetting(SS::ATCMinDelay, got);
    const bool resC = !c && c2 && gc && (got == 60.0);
    m_ves->SetSetting(SS::ATCMinDelay, origMin);
    m_ves->SetSetting(SS::SilenceOfSpace, orig);
    Report(resA && resB && resC, "setting.int_overload", Fmt("set(,0)=%d/%d set(,true)=%d/%d set(,5)rejected=%d set(,60)=%d got=%g", a, resA, b, resB, !c, c2, got));
}

// the item level of default sounds, and unknown sound IDs
void TestModule::TestSoundVolumeSync()
{
    float f = -3;
    const bool unkSet = m_ves->SetVolume(7777, 0.5f);
    const bool unkGet = m_ves->GetVolume(7777, f);
    Report(!unkSet && !unkGet && (f == -3), "soundvolume.unknown_id_rejected", Fmt("set=%d get=%d f=%g", unkSet, unkGet, f));
    struct D { int id; const char *name; };
    const D ids[] = { { XRSound::MainEngines, "MainEngines" }, { XRSound::SwitchOn, "SwitchOn" }, { XRSound::RCSAttackPlusX, "RCSAttackPlusX" }, { XRSound::AudioGreeting, "AudioGreeting" },
                      { XRSound::RadioATCGroup, "RadioATCGroup" }, { XRSound::CabinAmbienceGroup, "CabinAmbienceGroup" }, { XRSound::MusicFolder, "MusicFolder" } };
    for (const D &d : ids)
    {
        float orig = -1, got = -1;
        const bool g0 = m_ves->GetVolume(d.id, orig);
        float v2Orig = -1;
        const bool v2ok = g_proxies[2] && g_proxies[2]->GetVolume(d.id, v2Orig);
        const bool s = m_ves->SetVolume(d.id, 0.5f);
        const bool g = m_ves->GetVolume(d.id, got);
        const bool badHi = m_ves->SetVolume(d.id, 1.1f);
        const bool badNaN = m_ves->SetVolume(d.id, NaNf);
        float after = -1;
        m_ves->GetVolume(d.id, after);
        float v2After = -1;
        const bool v2iso = !v2ok || (g_proxies[2]->GetVolume(d.id, v2After) && Near(v2After, v2Orig, 1e-6));
        m_ves->SetVolume(d.id, g0 ? orig : 1.0f);
        const std::string name = Fmt("soundvolume.default.%s", d.name);
        Report(g0 && Near(orig, 1.0, 1e-5) && s && g && Near(got, 0.5, 1e-5) && !badHi && !badNaN && Near(after, 0.5, 1e-5), name,
               Fmt("initial=%g(%d) set=%d got=%g invalid1.1=%d invalidNaN=%d after=%g", orig, g0, s, got, badHi, badNaN, after));
        if (v2ok)
            Report(v2iso, name + ".isolated_from_second_vessel", Fmt("second vessel item level before=%g after the first vessel's SetVolume(0.5)=%g", v2Orig, v2After));
    }
}

void TestModule::TestGroupFolders()
{
    struct G { XRSound::DefaultSoundID id; const char *name; };
    const G groups[] = { { XRSound::CabinAmbienceGroup, "CabinAmbienceGroup" }, { XRSound::RadioATCGroup, "RadioATCGroup" },
                         { XRSound::MachCalloutsGroup, "MachCalloutsGroup" }, { XRSound::AltitudeCalloutsGroup, "AltitudeCalloutsGroup" },
                         { XRSound::DockingDistanceCalloutsGroup, "DockingDistanceCalloutsGroup" }, { XRSound::MusicFolder, "MusicFolder" } };
    int passed = 0;
    for (const G &g : groups)
    {
        const std::string name = Fmt("group.%s", g.name);
        const char *origP = m_ves->GetDefaultSoundGroupFolder(g.id);
        if (!origP)
        {
            Skip(name, "GetDefaultSoundGroupFolder returned nullptr before any set: group has no default sounds loaded for this vessel");
            continue;
        }
        const std::string orig = origP;
        const char *o2p = g_proxies[2] ? g_proxies[2]->GetDefaultSoundGroupFolder(g.id) : nullptr;
        const std::string o2 = o2p ? o2p : "";
        const bool s = m_ves->SetDefaultSoundGroupFolder(g.id, TEST_FOLDER);
        if (o2p)
        {
            const char *a2 = g_proxies[2]->GetDefaultSoundGroupFolder(g.id);
            Report(a2 && (o2 == a2), name + ".isolated_from_second_vessel", Fmt("second vessel folder '%s' before, '%s' after the first vessel set '%s'", o2.c_str(), a2 ? a2 : "(null)", TEST_FOLDER));
            // and the other direction: the second vessel sets its own folder, and the first vessel keeps the test folder
            const bool s2 = g_proxies[2]->SetDefaultSoundGroupFolder(g.id, "XRSound\\Default");
            const char *b1 = m_ves->GetDefaultSoundGroupFolder(g.id);
            Report(s2 && b1 && !strcmp(b1, TEST_FOLDER), name + ".second_vessel_set_isolated_from_first", Fmt("second vessel set=%d; first vessel folder '%s'", s2, b1 ? b1 : "(null)"));
            g_proxies[2]->SetDefaultSoundGroupFolder(g.id, o2.c_str());
        }
        const char *gp = m_ves->GetDefaultSoundGroupFolder(g.id);
        const bool eq = gp && !strcmp(gp, TEST_FOLDER);
        const std::string afterSet = gp ? gp : "(null)";
        const bool bad1 = m_ves->SetDefaultSoundGroupFolder(g.id, "XRSound\\NoSuchFolder_XRSoundApiTest");
        const char *gp2 = m_ves->GetDefaultSoundGroupFolder(g.id);
        const bool keep = gp2 && !strcmp(gp2, TEST_FOLDER);
        const bool bad2 = m_ves->SetDefaultSoundGroupFolder(g.id, nullptr);
        const bool bad3 = m_ves->SetDefaultSoundGroupFolder(g.id, "");
        const bool restore = m_ves->SetDefaultSoundGroupFolder(g.id, orig.c_str());
        const char *gp3 = m_ves->GetDefaultSoundGroupFolder(g.id);
        const bool back = gp3 && (orig == gp3);
        const bool ok = s && eq && !bad1 && keep && !bad2 && !bad3 && restore && back;
        if (ok) ++passed;
        Report(ok, name, Fmt("orig='%s' set=%d get='%s' badFolderRejected=%d previousKept=%d nullptrRejected=%d emptyRejected=%d restore=%d back='%s'",
                             orig.c_str(), s, afterSet.c_str(), !bad1, keep, !bad2, !bad3, restore, gp3 ? gp3 : "(null)"));
    }
    // invalid IDs
    const bool inv = m_ves->SetDefaultSoundGroupFolder(XRSound::MainEngines, TEST_FOLDER) || (m_ves->GetDefaultSoundGroupFolder(XRSound::MainEngines) != nullptr);
    Report(!inv, "group.non_group_id_rejected");
    Report(passed >= 1, "group.at_least_one_group_succeeded", Fmt("groups succeeded=%d", passed));
    // modules have no default sounds
    const bool modSet = m_mod->SetDefaultSoundGroupFolder(XRSound::CabinAmbienceGroup, TEST_FOLDER);
    Report(!modSet, "group.module_proxy_refused", Fmt("set=%d", modSet));
    // SetDefaultSoundEnabled returns true on success
    const bool en0 = m_ves->GetDefaultSoundEnabled(XRSound::AirConditioning);
    const bool r1 = m_ves->SetDefaultSoundEnabled(XRSound::AirConditioning, false);
    const bool g1 = m_ves->GetDefaultSoundEnabled(XRSound::AirConditioning);
    const bool r2 = m_ves->SetDefaultSoundEnabled(XRSound::AirConditioning, true);
    const bool g2 = m_ves->GetDefaultSoundEnabled(XRSound::AirConditioning);
    m_ves->SetDefaultSoundEnabled(XRSound::AirConditioning, en0);
    Report(r1 && !g1 && r2 && g2, "defaultsound.enabled_return_value", Fmt("initial=%d set(false)=%d get=%d set(true)=%d get=%d", en0, r1, g1, r2, g2));
    const bool rBad = m_ves->SetDefaultSoundEnabled(static_cast<XRSound::DefaultSoundID>(SND_VOL), false);
    Report(!rBad, "defaultsound.non_default_id_rejected");
}

// =====================================================================================================
// The volume given to the audio library, read from XRSound.log.  For a sound that the add-on loaded, that is its stored volume (from
// SetVolume(soundID) or PlayWav) x its type's level x the Main level; for a default sound, the volume XRSound plays it at x its item
// level (SetVolume(soundID)) x its type's level x the Main level.  These catch volume bugs that are hard to hear: a sound counted under
// the wrong volume type, a level change that never reaches a playing sound, and one vessel's levels leaking into another vessel's
// sounds.  All synchronous: each API call below applies the volume (and XRSound logs it) before it returns.
// =====================================================================================================
void TestModule::RunMixChecks()
{
    const char *ves1Name = m_vessel->GetName();
    const char *ves2Name = g_proxies[2] ? m_vessel2->GetName() : nullptr;
    PrepareLevels();    // RestoreDefaultLevels puts the configured levels back afterward
    m_log.SkipToEnd();

    // ---- two vessels play the same sound ID at different levels: each vessel's sound uses only its own levels
    m_ves->LoadWav(SND_MIX, FILE_WAV, PT::Global);
    m_ves->SetVolume(VT::Main, 0.8f);
    m_ves->SetVolume(VT::Custom, 0.5f);
    const bool p1 = m_ves->PlayWav(SND_MIX, true, 0.6f);
    bool p2 = false;
    if (ves2Name)
    {
        g_proxies[2]->LoadWav(SND_MIX, FILE_WAV, PT::Global);
        g_proxies[2]->SetVolume(VT::Main, 0.5f);
        g_proxies[2]->SetVolume(VT::Custom, 0.4f);
        p2 = g_proxies[2]->PlayWav(SND_MIX, true, 1.0f);
    }
    m_log.Read();
    double v1 = m_log.LastVolume(ves1Name, SND_MIX);
    Report(p1 && LoggedNear(v1, 0.24), "mix.volume_is_product_of_levels", Fmt("play=%d logged %s, expected volume 0.6 x Custom 0.5 x Main 0.8 = 0.24", p1, VolStr(v1).c_str()));
    if (ves2Name)
    {
        const double v2 = m_log.LastVolume(ves2Name, SND_MIX);
        Report(p2 && LoggedNear(v2, 0.2), "mix.second_vessel_uses_own_levels", Fmt("play=%d logged %s, expected volume 1.0 x Custom 0.4 x Main 0.5 = 0.2", p2, VolStr(v2).c_str()));
    }
    else Skip("mix.second_vessel_uses_own_levels", "no second XRSound vessel in the scenario");

    // ---- level changes reach the playing sound at once, and never the other vessel's sound
    struct Change { const char *name; VT type; int soundID; float level; double expected; };
    const Change changes[] = {
        { "mix.type_level_change_reaches_playing_sound", VT::Custom, 0, 0.25f, 0.12 },          // 0.6 x 0.25 x 0.8
        { "mix.main_level_change_reaches_playing_sound", VT::Main, 0, 1.0f, 0.15 },             // 0.6 x 0.25 x 1.0
        { "mix.sound_volume_change_reaches_playing_sound", VT::Custom, SND_MIX, 0.2f, 0.05 }    // 0.2 x 0.25 x 1.0
    };
    for (const Change &c : changes)
    {
        m_log.SkipToEnd();
        const bool s = c.soundID ? m_ves->SetVolume(c.soundID, c.level) : m_ves->SetVolume(c.type, c.level);
        m_log.Read();
        v1 = m_log.LastVolume(ves1Name, SND_MIX);
        Report(s && LoggedNear(v1, c.expected), c.name, Fmt("set=%d logged %s, expected %.4f", s, VolStr(v1).c_str(), c.expected));
        if (ves2Name)
        {
            const double v2 = m_log.LastVolume(ves2Name, SND_MIX);
            Report(std::isnan(v2), std::string(c.name) + ".not_on_second_vessel", Fmt("second vessel's sound logged %s (expected no change)", VolStr(v2).c_str()));
        }
    }

    // ---- EnableVerboseLogging: with it off, the same kind of change logs nothing; with it on again, it logs the volume
    {
        m_ves->SetSetting(SS::EnableVerboseLogging, 0);
        m_log.SkipToEnd();
        const bool sOff = m_ves->SetVolume(SND_MIX, 0.4f);
        m_log.Read();
        const double vOff = m_log.LastVolume(ves1Name, SND_MIX);
        m_ves->SetSetting(SS::EnableVerboseLogging, 1);
        m_log.SkipToEnd();
        m_ves->SetVolume(SND_MIX, 0.2f);
        m_log.Read();
        const double vOn = m_log.LastVolume(ves1Name, SND_MIX);
        Report(sOff && std::isnan(vOff) && LoggedNear(vOn, 0.05), "setting.effect.EnableVerboseLogging",
               Fmt("volume change with it off: set=%d, logged %s (expected nothing); with it on again: logged %s (expected 0.2 x 0.25 x 1.0 = 0.05)", sOff, VolStr(vOff).c_str(), VolStr(vOn).c_str()));
    }
    if (ves2Name)
    {
        m_log.SkipToEnd();
        g_proxies[2]->SetVolume(VT::Main, 1.0f);
        m_log.Read();
        const double v2 = m_log.LastVolume(ves2Name, SND_MIX);
        v1 = m_log.LastVolume(ves1Name, SND_MIX);
        Report(LoggedNear(v2, 0.4) && std::isnan(v1), "mix.second_vessel_change_not_on_first_vessel",
               Fmt("second vessel's sound logged %s (expected 1.0 x 1.0 x 0.4 x 1.0 = 0.4); first vessel's sound logged %s (expected no change)", VolStr(v2).c_str(), VolStr(v1).c_str()));
        g_proxies[2]->StopWav(SND_MIX);
    }
    m_ves->StopWav(SND_MIX);
    m_ves->SetVolume(SND_MIX, 1.0f);

    // ---- the module's sound uses the module's own levels
    m_log.SkipToEnd();
    m_mod->LoadWav(SND_MOD_MIX, FILE_WAV, PT::Global);
    m_mod->SetVolume(VT::Main, 0.5f);
    m_mod->SetVolume(VT::Custom, 0.6f);
    const bool pm = m_mod->PlayWav(SND_MOD_MIX, true, 0.5f);
    m_log.Read();
    const double vm = m_log.LastVolume(MODULE_NAME, SND_MOD_MIX);
    Report(pm && LoggedNear(vm, 0.15), "mix.module_volume_is_product_of_levels", Fmt("play=%d logged %s, expected volume 0.5 x Custom 0.6 x Main 0.5 = 0.15", pm, VolStr(vm).c_str()));
    m_mod->StopWav(SND_MOD_MIX);

    RestoreDefaultLevels();
}
