// ==============================================================
// XRSoundEngine class that is bound to an Orbiter module (i.e., to a unique ID); this file is not distributed with XRSound.
// The code for this exists in XRSound.dll.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include "XRSoundEngine.h"

class ModuleXRSoundEngine : public XRSoundEngine
{
public:
    static ModuleXRSoundEngine *CreateInstance(const char *pUniqueModuleName);

    const std::string &GetModuleName() const { return m_csModuleName; }
    virtual EngineType GetEngineType() override { return EngineType::Module; }
    virtual const char *GetLogID() override { return GetModuleName().c_str(); }

    virtual bool SetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID, const bool bEnabled) override;
    virtual bool GetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID) override;
    virtual bool SetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID, const char *pSubfolderPath) override;
    virtual const char *GetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID) const override;

protected:
    std::string m_csModuleName;   // unique module ID

    virtual void FreeResources() override;
    virtual void UpdateSoundState(WavContext &context) override;

private:
    // these are private to prevent incorrect creation or deletion outside of our static CreateInstance and DestroyInstance methods
    ModuleXRSoundEngine(const char *pUniqueModuleName);
    virtual ~ModuleXRSoundEngine() override;
};
