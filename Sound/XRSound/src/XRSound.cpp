// ==============================================================
// XRSound source file with static methods.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#include "XRSoundImpl.h"

// static factory method for vessels to use
XRSound *XRSound::CreateInstance(VESSEL *pVessel)
{
    XRSoundImpl *pInst = new XRSoundImpl();
    pInst->Initialize(pVessel);   // TODO: (optional): write warning to XRSound.log here if XRSound.dll not found (i.e., if this call returns false)
    return pInst;
}

// static factory method for modules to use
XRSound *XRSound::CreateInstance(const char *pUniqueModuleName)
{
    XRSoundImpl *pInst = new XRSoundImpl();
    pInst->Initialize(pUniqueModuleName);   // TODO: (optional): write warning to XRSound.log here if XRSound.dll not found (i.e., if this call returns false)
    return pInst;
}