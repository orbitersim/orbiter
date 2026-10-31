// ==============================================================
// TestSupport.cpp : XRSoundApiTest helpers: check reporting, the XRSound.log and XRSound.cfg readers, and the numeric settings table
// ==============================================================

#include <algorithm>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include "XRSoundApiTest.h"

int g_pass = 0, g_fail = 0, g_skip = 0;

XRSound *g_proxies[3] = { nullptr, nullptr, nullptr };
const char *g_proxyLabels[3] = { "ves", "mod", "ves2" };

// Returns the printf-formatted string, at whatever length it needs
std::string Fmt(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    va_list apCopy;
    va_copy(apCopy, ap);
    const int length = vsnprintf(nullptr, 0, format, apCopy);
    va_end(apCopy);
    std::string result;
    if (length > 0)
    {
        result.resize(static_cast<size_t>(length) + 1);
        vsnprintf(&result[0], result.size(), format, ap);
        result.resize(static_cast<size_t>(length));
    }
    va_end(ap);
    return result;
}

// oapiWriteLogV("%s", ...): Orbiter 2024's oapiWriteLog treats its text as a printf format, and the details may contain a '%'
void Log(const std::string &line)
{
    oapiWriteLogV("%s", line.c_str());
}

void Report(const bool ok, const std::string &name, const std::string &details)
{
    if (ok) ++g_pass; else ++g_fail;
    Log(Fmt("XRSOUNDTEST %s %s %s", ok ? "PASS" : "FAIL", name.c_str(), details.c_str()));
}

void Skip(const std::string &name, const char *reason)
{
    ++g_skip;
    Log(Fmt("XRSOUNDTEST SKIP %s %s", name.c_str(), reason));
}

bool Near(const double a, const double b, const double tol) { return std::fabs(a - b) <= tol; }

const char *VolName(const VT t)
{
    switch (t)
    {
    case VT::Main: return "Main";
    case VT::Engines: return "Engines";
    case VT::RCS: return "RCS";
    case VT::Effects: return "Effects";
    case VT::Callouts: return "Callouts";
    case VT::CabinAmbience: return "CabinAmbience";
    case VT::ATC: return "ATC";
    case VT::Music: return "Music";
    case VT::Custom: return "Custom";
    }
    return "?";
}

// Length in ms of a canonical RIFF/WAVE file, from its header; -1 on error
int WavHeaderMillis(const char *path)
{
    FILE *f = fopen(path, "rb");
    if (!f) return -1;
    char hdr[12];
    if ((fread(hdr, 1, 12, f) != 12) || memcmp(hdr, "RIFF", 4) || memcmp(hdr + 8, "WAVE", 4)) { fclose(f); return -1; }
    unsigned byteRate = 0, dataSize = 0;
    for (;;)
    {
        char id[4];
        unsigned size;
        if ((fread(id, 1, 4, f) != 4) || (fread(&size, 4, 1, f) != 1)) break;
        if (!memcmp(id, "fmt ", 4))
        {
            unsigned char fmt[16];
            if ((size < 16) || (fread(fmt, 1, 16, f) != 16)) break;
            memcpy(&byteRate, fmt + 8, sizeof(byteRate));
            if (size > 16) fseek(f, size - 16, SEEK_CUR);
        }
        else if (!memcmp(id, "data", 4)) { dataSize = size; break; }
        else fseek(f, size + (size & 1), SEEK_CUR);
    }
    fclose(f);
    if (!byteRate || !dataSize) return -1;
    return static_cast<int>(static_cast<double>(dataSize) * 1000.0 / byteRate + 0.5);
}

// Returns a level (0 to 1) from "name = value" in the [section] of XRSound.cfg, read the way XRSound reads it: the names are not case-sensitive,
// the last occurrence wins, an out-of-range value means the default, and so does a missing one.  (MasterVolume, MusicVolume, and the ATC Volume
// all default to 1.0.)
double ReadXRSoundCfgLevel(const char *section, const char *name, const double defaultLevel)
{
    std::ifstream in(XRSOUND_CFG);
    std::string line, currentSection;
    double value = defaultLevel;
    while (std::getline(in, line))
    {
        const size_t start = line.find_first_not_of(" \t");
        if ((start == std::string::npos) || (line[start] == '#')) continue;
        if (line[start] == '[')
        {
            const size_t end = line.find(']', start);
            currentSection = line.substr(start + 1, (end == std::string::npos) ? std::string::npos : end - start - 1);
            continue;
        }
        const size_t equals = line.find('=', start);
        if (equals == std::string::npos) continue;
        std::string key = line.substr(start, equals - start);
        key.erase(key.find_last_not_of(" \t") + 1);
        if ((_stricmp(currentSection.c_str(), section) == 0) && (_stricmp(key.c_str(), name) == 0))
        {
            const double v = atof(line.c_str() + equals + 1);
            value = ((v >= 0) && (v <= 1)) ? v : defaultLevel;
        }
    }
    return value;
}

// ---------------------------------------------------------------------------------------------------------
// XRSound.log reader for the volume checks
// ---------------------------------------------------------------------------------------------------------
// Skips everything logged so far: Read() then returns only lines logged after this call
void XRSoundLogReader::SkipToEnd()
{
    std::ifstream in(XRSOUND_LOG, std::ios::binary | std::ios::ate);
    m_offset = in ? static_cast<std::streamoff>(in.tellg()) : 0;
    m_lines.clear();
}

// Reads the lines logged since the last Read or SkipToEnd (XRSound flushes each line as it writes it)
void XRSoundLogReader::Read()
{
    std::ifstream in(XRSOUND_LOG, std::ios::binary);
    if (!in) return;
    in.seekg(m_offset);
    std::string line;
    while (std::getline(in, line))
    {
        if (!line.empty() && (line.back() == '\r')) line.pop_back();
        m_lines.push_back(line);
    }
    in.clear();
    in.seekg(0, std::ios::end);
    m_offset = static_cast<std::streamoff>(in.tellg());
}

// Returns the last volume logged for soundID by the engine with the supplied log ID (vessel name or module name) among the lines read
// since SkipToEnd, or NaN if none.  Logged lines look like this:
//   ... - [GL-01][12.345] XRSoundEngine::ApplyFinalVolume: volume = 0.2160 for [soundID = 300, playbackType = Global (7), filename = '...']
double XRSoundLogReader::LastVolume(const char *logID, const int soundID) const
{
    const std::string idTag = Fmt("[%s][", logID);
    const std::string volumeTag = "ApplyFinalVolume: volume = ";
    const std::string soundTag = Fmt("[soundID = %d,", soundID);
    for (auto it = m_lines.rbegin(); it != m_lines.rend(); ++it)
    {
        const size_t v = it->find(volumeTag);
        if ((v != std::string::npos) && (it->find(idTag) != std::string::npos) && (it->find(soundTag, v) != std::string::npos))
            return atof(it->c_str() + v + volumeTag.size());
    }
    return NaNd;
}

// Returns the file named in the last volume line logged for soundID by the engine with the supplied log ID among the lines read since SkipToEnd,
// or an empty string if none.  XRSound logs a sound's first volume when the sound starts, so this is the file of the sound that started last.
std::string XRSoundLogReader::LastStartedFile(const char *logID, const int soundID) const
{
    const std::string idTag = Fmt("[%s][", logID);
    const std::string volumeTag = "ApplyFinalVolume: volume = ";
    const std::string soundTag = Fmt("[soundID = %d,", soundID);
    for (auto it = m_lines.rbegin(); it != m_lines.rend(); ++it)
    {
        if ((it->find(volumeTag) == std::string::npos) || (it->find(idTag) == std::string::npos) || (it->find(soundTag) == std::string::npos))
            continue;
        const size_t f = it->find("filename = '");
        if (f == std::string::npos) continue;
        const size_t start = f + strlen("filename = '");
        const size_t end = it->find('\'', start);
        return it->substr(start, (end == std::string::npos) ? std::string::npos : end - start);
    }
    return std::string();
}

// Returns true if any line read since SkipToEnd contains both strings
bool XRSoundLogReader::AnyLineContains(const std::string &a, const std::string &b) const
{
    return std::any_of(m_lines.begin(), m_lines.end(), [&](const std::string &line) { return (line.find(a) != std::string::npos) && (line.find(b) != std::string::npos); });
}

std::string VolStr(const double v) { return std::isnan(v) ? std::string("(none logged)") : Fmt("%.4f", v); }

// a logged volume is rounded to 4 decimal places
bool LoggedNear(const double logged, const double expected) { return !std::isnan(logged) && Near(logged, expected, 0.00015); }

// ---------------------------------------------------------------------------------------------------------
// Settings tables
// ---------------------------------------------------------------------------------------------------------
std::vector<NumSetting> MakeNumSettings()
{
    return {
        { SS::SilenceOfSpace, "SilenceOfSpace", { 0, 1 }, { 2, -1, 0.5, NaNd } },
        { SS::MusicOrder, "MusicOrder", { 0, 1 }, { 2, -1, 0.5, NaNd } },
        { SS::UpdateInterval, "UpdateInterval", { 0.02, 1.0, 0.25 }, { 0.01, 1.01, -1, NaNd } },
        { SS::EnableVerboseLogging, "EnableVerboseLogging", { 0, 1 }, { 2, -1, 0.5, NaNd } },
        { SS::LogVesselAnimations, "LogVesselAnimations", { 0, 1 }, { 2, -1, 0.5, NaNd } },
        { SS::LogThrusterData, "LogThrusterData", { 0, 1 }, { 2, -1, 0.5, NaNd } },
        { SS::DisableAutopilotsTimeAccThreshold, "DisableAutopilotsTimeAccThreshold", { 0, 10000, 55.5 }, { -1, 10001, NaNd } },
        { SS::MinThrusterLevelForRCSSoundEffects, "MinThrusterLevelForRCSSoundEffects", { 0, 1.0, 0.3 }, { -0.1, 1.1, NaNd } },
        { SS::ATCMinDelay, "ATCMinDelay", { 10, 900, 123 }, { 9, 901, 12.5, NaNd } },
        { SS::ATCMaxDelay, "ATCMaxDelay", { 10, 900, 321 }, { 9, 901, 12.5, NaNd } },
        { SS::ATCAllowWhileLanded, "ATCAllowWhileLanded", { 0, 1 }, { 2, -1, 0.5, NaNd } },
        { SS::ATCAllowInAtmosphere, "ATCAllowInAtmosphere", { 0, 1 }, { 2, -1, 0.5, NaNd } },
        { SS::ATCAllowDuringReentry, "ATCAllowDuringReentry", { 0, 1 }, { 2, -1, 0.5, NaNd } },
        { SS::ATCDelayPlanetDistance, "ATCDelayPlanetDistance", { 0, 1e6, 500.5 }, { -1, 1e6 + 1, NaNd } },
        { SS::ATCDelayPlanetMultiplier, "ATCDelayPlanetMultiplier", { 1, 100, 7.5 }, { 0.99, 100.1, NaNd } },
        { SS::CabinAmbienceMin, "CabinAmbienceMin", { 7, 300, 50 }, { 6, 301, 7.5, NaNd } },
        { SS::CabinAmbienceMax, "CabinAmbienceMax", { 7, 300, 60 }, { 6, 301, 7.5, NaNd } },
        { SS::MusicPlayInternal, "MusicPlayInternal", { 0, 1, 2 }, { 3, -1, 1.5, NaNd } },
        { SS::MusicPlayExternal, "MusicPlayExternal", { 0, 1, 2 }, { 3, -1, 1.5, NaNd } },
        { SS::WarningGearIsUpAltitude, "WarningGearIsUpAltitude", { 0, 1000, 250.5 }, { -1, 1001, NaNd } },
        { SS::LandingGearAnimationID, "LandingGearAnimationID", { -1, 0, 5 }, { -2, 1.5, NaNd } },
    };
}
