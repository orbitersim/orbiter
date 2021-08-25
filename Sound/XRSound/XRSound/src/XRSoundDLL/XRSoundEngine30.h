// ==============================================================
// XRSoundEngine30.h : Defines the XRSoundEngine version 2.x interface.
//
// Copyright (c) 2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include "XRSoundEngine20.h"

// NOTE: do not change the order of these methods!  We need to maintain compatibility with 
// previous versions of the static client library (XRSound.lib) that uses this engine object.

class XRSoundEngine30 : public XRSoundEngine20
{
public:
    virtual bool  SetPan(const int soundID, const float pan) = 0;
    virtual float GetPan(const int soundID) = 0;
    
    virtual bool  SetPlaybackSpeed(const int soundID, const float speed = 1.0) = 0;
    virtual float GetPlaybackSpeed(const int soundID) = 0;
    
    virtual bool  SetPlayPosition(const int soundID, const unsigned int positionMillis) = 0;
    virtual int   GetPlayPosition(const int soundID) = 0;
};
