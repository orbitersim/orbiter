// ==============================================================
// XRSoundEngine41.h : Defines the XRSoundEngine version 4.1 interface.
//
// Copyright (c) 2026 Douglas Beachy and contributors
// Licensed under the MIT License
// ==============================================================

#pragma once

#include "XRSoundEngine30.h"

// NOTE: do not change the order of these methods!  We need to maintain compatibility with
// previous versions of the static client library (XRSound.lib) that uses this engine object.
// Each method here has a unique name (no overloads) on purpose: MSVC groups the overloads of one name together
// in the vtable, which would break the "append only" rule above.
// The names here intentionally differ from the matching public XRSound method names (e.g., SetVolumeByType
// implements XRSound::SetVolume(VolumeType, level)).

class XRSoundEngine41 : public XRSoundEngine30
{
public:
    // XRSound::SetVolume/GetVolume(const VolumeType, ...)
    virtual bool SetVolumeByType(const XRSound::VolumeType type, const float level) = 0;
    virtual bool GetVolumeByType(const XRSound::VolumeType type, float &level) = 0;   // can't be const

    // XRSound::SetVolume/GetVolume(const int soundID, ...)
    virtual bool SetSoundVolume(const int soundID, const float volume) = 0;
    virtual bool GetSoundVolume(const int soundID, float &volume) = 0;   // can't be const

    // XRSound::SetPlaybackType/GetPlaybackType
    virtual bool SetSoundPlaybackType(const int soundID, const XRSound::PlaybackType playbackType) = 0;
    virtual bool GetSoundPlaybackType(const int soundID, XRSound::PlaybackType &playbackType) = 0;   // can't be const

    // XRSound::GetDuration
    virtual bool GetSoundDuration(const int soundID, unsigned int &durationMillis) = 0;

    // XRSound::SetSetting/GetSetting (numeric and string forms)
    virtual bool SetSettingNumber(const XRSound::Setting setting, const double value) = 0;
    virtual bool GetSettingNumber(const XRSound::Setting setting, double &value) = 0;   // can't be const
    virtual bool SetSettingString(const XRSound::Setting setting, const char *pValue) = 0;
    virtual bool GetSettingString(const XRSound::Setting setting, char *pBuffer, const int bufferLength) = 0;   // can't be const

    // XRSound::GetDefaultVolume / GetDefaultSetting (numeric and string forms): the configured starting values for this vessel or module, read from the engine's own parsed
    // configuration, never from its active values.  Appended after the existing slots (4.1 is unreleased, so this is safe).
    virtual bool GetDefaultVolumeByType(const XRSound::VolumeType type, float &level) = 0;   // can't be const
    virtual bool GetDefaultSettingNumber(const XRSound::Setting setting, double &value) = 0;   // can't be const
    virtual bool GetDefaultSettingString(const XRSound::Setting setting, char *pBuffer, const int bufferLength) = 0;   // can't be const
};
