// ==============================================================
// XRSound engine implementation: runtime settings (XRSound::SetSetting / GetSetting).
//
// Copyright (c) 2026 Douglas Beachy and contributors
// Licensed under the MIT License
// ==============================================================

#include <cmath>
#include <climits>
#include <cstring>
#include <sstream>
#include <string>

#include "XRSoundEngine.h"
#include "XRSoundConfigFileParser.h"

// Functionality added in XRSound version 4.1: the engine side of XRSound::SetSetting and XRSound::GetSetting.
//
// Every setting is per instance: the four methods below change and read only THIS engine's active values (XRSoundEngine::ActiveValues), which were initialized from
// this engine's configuration when the engine was created.  They work identically from a vessel proxy and from a module proxy; a module simply stores the vessel-related
// settings and nothing reads them there.  Side effects that only make sense for a vessel (rescheduling a random sound group) are applied
// by VesselXRSoundEngine::OnSettingChanged.
//
// Rules (see the XRSound.h comments for the public contract):
//   * A setter that is given an out-of-range or wrong-type value returns false and leaves the current value unchanged.
//   * The valid ranges are the named constants in XRSoundConfigFileParser, which are also what ParseLine uses for the configuration file.
//   * Nothing here ever assigns to a configuration object or a configuration file: these only change this engine's active values, which die with the engine.

namespace
{
    // Whether a setting is numeric (the double forms of SetSetting / GetSetting) or a string.
    enum class SettingKind { Invalid, Numeric, String };

    SettingKind KindOf(const XRSound::Setting setting)
    {
        switch (setting)
        {
        case XRSound::Setting::SilenceOfSpace:
        case XRSound::Setting::MusicOrder:
        case XRSound::Setting::UpdateInterval:
        case XRSound::Setting::EnableVerboseLogging:
        case XRSound::Setting::LogVesselAnimations:
        case XRSound::Setting::LogThrusterData:
        case XRSound::Setting::DisableAutopilotsTimeAccThreshold:
        case XRSound::Setting::MinThrusterLevelForRCSSoundEffects:
        case XRSound::Setting::ATCMinDelay:
        case XRSound::Setting::ATCMaxDelay:
        case XRSound::Setting::ATCAllowWhileLanded:
        case XRSound::Setting::ATCAllowInAtmosphere:
        case XRSound::Setting::ATCAllowDuringReentry:
        case XRSound::Setting::ATCDelayPlanetDistance:
        case XRSound::Setting::ATCDelayPlanetMultiplier:
        case XRSound::Setting::CabinAmbienceMin:
        case XRSound::Setting::CabinAmbienceMax:
        case XRSound::Setting::MusicPlayInternal:
        case XRSound::Setting::MusicPlayExternal:
        case XRSound::Setting::WarningGearIsUpAltitude:
        case XRSound::Setting::LandingGearAnimationID:
            return SettingKind::Numeric;

        case XRSound::Setting::CustomEnginesThrusterIndexes:
            return SettingKind::String;

        default:
            return SettingKind::Invalid;
        }
    }

    // value must be exactly 0 or 1 (a NaN fails both comparisons)
    bool ToBool(const double value, bool &result)
    {
        if (value == 0.0)
            result = false;
        else if (value == 1.0)
            result = true;
        else
            return false;
        return true;
    }

    // value must be a whole number that fits in an int (a NaN or infinity fails)
    bool ToInt(const double value, int &result)
    {
        if (!std::isfinite(value) || (value != std::floor(value)) || (value < static_cast<double>(INT_MIN)) || (value > static_cast<double>(INT_MAX)))
            return false;
        result = static_cast<int>(value);
        return true;
    }

    // value must be a whole number in min..max inclusive
    bool ToIntInRange(const double value, const int min, const int max, int &result)
    {
        int i;
        if (!ToInt(value, i) || (i < min) || (i > max))
            return false;
        result = i;
        return true;
    }

    // value must be in min..max inclusive (a NaN fails both comparisons)
    bool ToDoubleInRange(const double value, const double min, const double max, double &result)
    {
        if (!((value >= min) && (value <= max)))
            return false;
        result = value;
        return true;
    }

    // Applies a numeric setting to the supplied active values after validating it.
    // Returns true if the value was valid and applied, false if it was invalid (nothing changes then).
    bool ApplyNumericSetting(XRSoundEngine::ActiveValues &active, const XRSound::Setting setting, const double value)
    {
        bool b;
        int i;
        double d;

        switch (setting)
        {
        case XRSound::Setting::SilenceOfSpace:
            if (!ToBool(value, b)) return false;
            active.SilenceOfSpace = b;
            return true;

        case XRSound::Setting::MusicOrder:
        {
            XRSoundConfigFileParser::SeqRandom order;       // used only to validate the value against the public enum
            if (!ToInt(value, i) || !XRSoundConfigFileParser::MusicOrderToInternal(i, order)) return false;     // the value is the public XRSound::MusicOrder enum's integer value
            active.MusicOrder = static_cast<XRSound::MusicOrder>(i);
            return true;
        }

        case XRSound::Setting::UpdateInterval:
            if (!ToDoubleInRange(value, XRSoundConfigFileParser::UPDATE_INTERVAL_MIN, XRSoundConfigFileParser::UPDATE_INTERVAL_MAX, d)) return false;
            active.UpdateInterval = d;
            return true;

        case XRSound::Setting::EnableVerboseLogging:
            // Note: XRSound.cfg's EnableVerboseLogging is ignored by _DEBUG builds (they default to true), but an explicit runtime call is honored in all builds.
            if (!ToBool(value, b)) return false;
            active.EnableVerboseLogging = b;
            return true;

        case XRSound::Setting::LogVesselAnimations:
            if (!ToBool(value, b)) return false;
            active.LogVesselAnimations = b;
            return true;

        case XRSound::Setting::LogThrusterData:
            if (!ToBool(value, b)) return false;
            active.LogThrusterData = b;
            return true;

        case XRSound::Setting::DisableAutopilotsTimeAccThreshold:
            if (!ToDoubleInRange(value, XRSoundConfigFileParser::DISABLE_AUTOPILOTS_TIME_ACC_THRESHOLD_MIN, XRSoundConfigFileParser::DISABLE_AUTOPILOTS_TIME_ACC_THRESHOLD_MAX, d)) return false;
            active.DisableAutopilotsTimeAccThreshold = d;
            return true;

        case XRSound::Setting::MinThrusterLevelForRCSSoundEffects:
            if (!ToDoubleInRange(value, XRSoundConfigFileParser::MIN_THRUSTER_LEVEL_FOR_RCS_SOUND_EFFECTS_MIN, XRSoundConfigFileParser::MIN_THRUSTER_LEVEL_FOR_RCS_SOUND_EFFECTS_MAX, d)) return false;
            active.MinThrusterLevelForRCSSoundEffects = d;
            return true;

        case XRSound::Setting::ATCMinDelay:
            if (!ToIntInRange(value, XRSoundConfigFileParser::ATC_DELAY_MIN, XRSoundConfigFileParser::ATC_DELAY_MAX, i)) return false;
            active.ATCMinDelay = i;
            return true;

        case XRSound::Setting::ATCMaxDelay:
            if (!ToIntInRange(value, XRSoundConfigFileParser::ATC_DELAY_MIN, XRSoundConfigFileParser::ATC_DELAY_MAX, i)) return false;
            active.ATCMaxDelay = i;
            return true;

        case XRSound::Setting::ATCAllowWhileLanded:
            if (!ToBool(value, b)) return false;
            active.ATCAllowWhileLanded = b;
            return true;

        case XRSound::Setting::ATCAllowInAtmosphere:
            if (!ToBool(value, b)) return false;
            active.ATCAllowInAtmosphere = b;
            return true;

        case XRSound::Setting::ATCAllowDuringReentry:
            if (!ToBool(value, b)) return false;
            active.ATCAllowDuringReentry = b;
            return true;

        case XRSound::Setting::ATCDelayPlanetDistance:
            if (!ToDoubleInRange(value, XRSoundConfigFileParser::ATC_DELAY_PLANET_DISTANCE_MIN, XRSoundConfigFileParser::ATC_DELAY_PLANET_DISTANCE_MAX, d)) return false;
            active.ATCDelayPlanetDistance = d;
            return true;

        case XRSound::Setting::ATCDelayPlanetMultiplier:
            if (!ToDoubleInRange(value, XRSoundConfigFileParser::ATC_DELAY_PLANET_MULTIPLIER_MIN, XRSoundConfigFileParser::ATC_DELAY_PLANET_MULTIPLIER_MAX, d)) return false;
            active.ATCDelayPlanetMultiplier = d;
            return true;

        case XRSound::Setting::CabinAmbienceMin:
            if (!ToIntInRange(value, XRSoundConfigFileParser::CABIN_AMBIENCE_DELAY_MIN, XRSoundConfigFileParser::CABIN_AMBIENCE_DELAY_MAX, i)) return false;
            active.CabinAmbienceMin = i;
            return true;

        case XRSound::Setting::CabinAmbienceMax:
            if (!ToIntInRange(value, XRSoundConfigFileParser::CABIN_AMBIENCE_DELAY_MIN, XRSoundConfigFileParser::CABIN_AMBIENCE_DELAY_MAX, i)) return false;
            active.CabinAmbienceMax = i;
            return true;

        case XRSound::Setting::MusicPlayInternal:
        case XRSound::Setting::MusicPlayExternal:
        {
            XRSoundConfigFileParser::MusicPlay play;       // used only to validate the value against the public enum
            if (!ToInt(value, i) || !XRSoundConfigFileParser::MusicPlayToInternal(i, play)) return false;      // the value is the public XRSound::MusicPlay enum's integer value
            if (setting == XRSound::Setting::MusicPlayInternal)
                active.MusicPlayInternal = static_cast<XRSound::MusicPlay>(i);
            else
                active.MusicPlayExternal = static_cast<XRSound::MusicPlay>(i);
            return true;
        }

        case XRSound::Setting::WarningGearIsUpAltitude:
            if (!ToDoubleInRange(value, XRSoundConfigFileParser::WARNING_GEAR_IS_UP_ALTITUDE_MIN, XRSoundConfigFileParser::WARNING_GEAR_IS_UP_ALTITUDE_MAX, d)) return false;
            active.WarningGearIsUpAltitude = d;
            return true;

        case XRSound::Setting::LandingGearAnimationID:
            // -1 means "no landing gear animation"; otherwise it is an animation ID, which is >= 0
            if (!ToIntInRange(value, -1, INT_MAX, i)) return false;
            active.LandingGearAnimationID = i;
            return true;

        default:
            return false;   // not a numeric setting
        }
    }

    // Reads a numeric setting from the supplied active values.
    bool ReadNumericSetting(const XRSoundEngine::ActiveValues &active, const XRSound::Setting setting, double &value)
    {
        switch (setting)
        {
        case XRSound::Setting::SilenceOfSpace:                      value = active.SilenceOfSpace ? 1.0 : 0.0; return true;
        case XRSound::Setting::MusicOrder:                          value = static_cast<double>(static_cast<int>(active.MusicOrder)); return true;
        case XRSound::Setting::UpdateInterval:                      value = active.UpdateInterval; return true;
        case XRSound::Setting::EnableVerboseLogging:                value = active.EnableVerboseLogging ? 1.0 : 0.0; return true;
        case XRSound::Setting::LogVesselAnimations:                 value = active.LogVesselAnimations ? 1.0 : 0.0; return true;
        case XRSound::Setting::LogThrusterData:                     value = active.LogThrusterData ? 1.0 : 0.0; return true;
        case XRSound::Setting::DisableAutopilotsTimeAccThreshold:   value = active.DisableAutopilotsTimeAccThreshold; return true;
        case XRSound::Setting::MinThrusterLevelForRCSSoundEffects:  value = active.MinThrusterLevelForRCSSoundEffects; return true;

        case XRSound::Setting::ATCMinDelay:                         value = active.ATCMinDelay; return true;
        case XRSound::Setting::ATCMaxDelay:                         value = active.ATCMaxDelay; return true;
        case XRSound::Setting::ATCAllowWhileLanded:                 value = active.ATCAllowWhileLanded ? 1.0 : 0.0; return true;
        case XRSound::Setting::ATCAllowInAtmosphere:                value = active.ATCAllowInAtmosphere ? 1.0 : 0.0; return true;
        case XRSound::Setting::ATCAllowDuringReentry:               value = active.ATCAllowDuringReentry ? 1.0 : 0.0; return true;
        case XRSound::Setting::ATCDelayPlanetDistance:              value = active.ATCDelayPlanetDistance; return true;
        case XRSound::Setting::ATCDelayPlanetMultiplier:            value = active.ATCDelayPlanetMultiplier; return true;
        case XRSound::Setting::CabinAmbienceMin:                    value = active.CabinAmbienceMin; return true;
        case XRSound::Setting::CabinAmbienceMax:                    value = active.CabinAmbienceMax; return true;
        case XRSound::Setting::MusicPlayInternal:                   value = static_cast<double>(static_cast<int>(active.MusicPlayInternal)); return true;
        case XRSound::Setting::MusicPlayExternal:                   value = static_cast<double>(static_cast<int>(active.MusicPlayExternal)); return true;
        case XRSound::Setting::WarningGearIsUpAltitude:             value = active.WarningGearIsUpAltitude; return true;
        case XRSound::Setting::LandingGearAnimationID:              value = active.LandingGearAnimationID; return true;

        default:
            return false;   // not a numeric setting
        }
    }

    // Validates a space-separated list of thruster indexes (non-negative integers; e.g., "0 1 4"): every token must be all digits.  An empty (or all-whitespace) list is valid:
    // it means "no custom-engine thrusters" (the configured value for a vessel with no custom engines is empty), so a caller can always put the configured value back.
    // On success, 'normalized' receives the tokens separated by single spaces (empty for an empty list).
    bool NormalizeThrusterIndexes(const char *pValue, std::string &normalized)
    {
        std::istringstream iss(pValue);
        std::string token;
        std::string result;
        while (iss >> token)
        {
            if (token.length() > 9)     // keeps the value within an int
                return false;
            for (char c : token)
            {
                if ((c < '0') || (c > '9'))
                    return false;
            }

            if (!result.empty())
                result += " ";
            result += token;
        }

        if (result.length() >= static_cast<size_t>(MAX_VALUE_LENGTH))
            return false;

        normalized = result;
        return true;
    }

    // Copies a string into the caller's buffer, including the terminating null.  Returns false (copying nothing) if the buffer is missing or too small.
    bool CopyToBuffer(const std::string &value, char *pBuffer, const int bufferLength)
    {
        if (!pBuffer || (bufferLength <= 0) || (value.length() >= static_cast<size_t>(bufferLength)))
            return false;

        memcpy(pBuffer, value.c_str(), value.length() + 1);
        return true;
    }
}

//===========================================================================================
// XRSoundEngine settings (used by both vessel and module engines)
//===========================================================================================

// Implements XRSound::SetSetting(const Setting, const double): changes this engine's active value only.
bool XRSoundEngine::SetSettingNumber(const XRSound::Setting setting, const double value)
{
    if (!IsSoundEngineInitialized())
        return false;

    if (KindOf(setting) != SettingKind::Numeric)
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetSettingNumber WARNING: setting %d is not a numeric setting; ignored (value = %.6g)", static_cast<int>(setting), value);
        return false;
    }

    double oldValue = 0;
    ReadNumericSetting(m_active, setting, oldValue);
    if (!ApplyNumericSetting(m_active, setting, value))
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetSettingNumber WARNING: invalid value %.6g for setting %d; ignored (current value = %.6g)", value, static_cast<int>(setting), oldValue);
        return false;
    }

    VERBOSE_LOG(this, "XRSoundEngine::SetSettingNumber: setting %d changed from %.6g to %.6g", static_cast<int>(setting), oldValue, value);
    OnSettingChanged(setting);      // note: if this just turned verbose logging off, the line above was the last one logged
    return true;
}

// Implements XRSound::GetSetting(const Setting, double &): reads this engine's active value.
bool XRSoundEngine::GetSettingNumber(const XRSound::Setting setting, double &value)
{
    if (!IsSoundEngineInitialized() || (KindOf(setting) != SettingKind::Numeric))
        return false;

    return ReadNumericSetting(m_active, setting, value);
}

// Implements XRSound::SetSetting(const Setting, const char *): CustomEnginesThrusterIndexes; changes this engine's active value only.
bool XRSoundEngine::SetSettingString(const XRSound::Setting setting, const char *pValue)
{
    if (!IsSoundEngineInitialized())
        return false;

    if (KindOf(setting) != SettingKind::String)
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetSettingString WARNING: setting %d is not a string setting; ignored", static_cast<int>(setting));
        return false;
    }

    std::string normalized;
    if (!pValue || !NormalizeThrusterIndexes(pValue, normalized))
    {
        VERBOSE_LOG(this, "XRSoundEngine::SetSettingString WARNING: invalid CustomEnginesThrusterIndexes value '%s' (it must be a space-separated list of non-negative integers, or empty for none); ignored", pValue ? pValue : "<null>");
        return false;
    }
    const std::string oldValue = m_active.CustomEnginesThrusterIndexes;
    m_active.CustomEnginesThrusterIndexes = normalized;
    VERBOSE_LOG(this, "XRSoundEngine::SetSettingString: setting %d changed from [%s] to [%s]", static_cast<int>(setting), oldValue.c_str(), normalized.c_str());
    OnSettingChanged(setting);
    return true;
}

// Implements XRSound::GetSetting(const Setting, char *, const int): reads this engine's active value.
bool XRSoundEngine::GetSettingString(const XRSound::Setting setting, char *pBuffer, const int bufferLength)
{
    if (!IsSoundEngineInitialized() || (KindOf(setting) != SettingKind::String))
        return false;

    const std::string &value = m_active.CustomEnginesThrusterIndexes;
    if (!CopyToBuffer(value, pBuffer, bufferLength))
    {
        VERBOSE_LOG(this, "XRSoundEngine::GetSettingString WARNING: buffer is missing or too small (bufferLength = %d, needed = %d) for setting %d",
            bufferLength, static_cast<int>(value.length() + 1), static_cast<int>(setting));
        return false;
    }
    return true;
}

//===========================================================================================
// Configured defaults (XRSound::GetDefaultVolume / GetDefaultSetting)
//
// These return the starting values for THIS vessel or module, read from this engine's own parsed configuration (m_pConfig: XRSound.cfg plus, for a vessel, its class override
// file).  Nothing writes the configuration after the parse, so the result never changes during the session and is never affected by SetVolume / SetSetting.
// The values come from FillConfiguredValues, the same function InitializeActiveValues uses, so a default is always exactly the value the active value started with.
// The validation and failure rules are the same as GetVolumeByType / GetSettingNumber / GetSettingString.
//===========================================================================================

// Implements XRSound::GetDefaultVolume(const VolumeType, float &)
bool XRSoundEngine::GetDefaultVolumeByType(const XRSound::VolumeType type, float &level)
{
    if (!IsSoundEngineInitialized() || !m_pConfig || !IsValidVolumeType(type))
        return false;

    ActiveValues defaults;
    FillConfiguredValues(*m_pConfig, defaults);
    level = defaults.volumeLevels[static_cast<int>(type)];
    return true;
}

// Implements XRSound::GetDefaultSetting(const Setting, double &)
bool XRSoundEngine::GetDefaultSettingNumber(const XRSound::Setting setting, double &value)
{
    if (!IsSoundEngineInitialized() || !m_pConfig || (KindOf(setting) != SettingKind::Numeric))
        return false;

    ActiveValues defaults;
    FillConfiguredValues(*m_pConfig, defaults);
    return ReadNumericSetting(defaults, setting, value);
}

// Implements XRSound::GetDefaultSetting(const Setting, char *, const int)
bool XRSoundEngine::GetDefaultSettingString(const XRSound::Setting setting, char *pBuffer, const int bufferLength)
{
    if (!IsSoundEngineInitialized() || !m_pConfig || (KindOf(setting) != SettingKind::String))
        return false;

    ActiveValues defaults;
    FillConfiguredValues(*m_pConfig, defaults);
    const std::string &value = defaults.CustomEnginesThrusterIndexes;
    if (!CopyToBuffer(value, pBuffer, bufferLength))
    {
        VERBOSE_LOG(this, "XRSoundEngine::GetDefaultSettingString WARNING: buffer is missing or too small (bufferLength = %d, needed = %d) for setting %d",
            bufferLength, static_cast<int>(value.length() + 1), static_cast<int>(setting));
        return false;
    }
    return true;
}
