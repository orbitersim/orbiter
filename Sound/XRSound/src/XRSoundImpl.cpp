// ==============================================================
// XRSound static library main source file.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
//
// This software is FREEWARE and may not be sold!
// ==============================================================

#include "XRSoundImpl.h"
#include <Strsafe.h> 

// NOTE: In order to maximize compatibility with users using versions of Visual Studio other than VS 2019, do not call any MSVCRT methods in this code.
// More information is at https://connect.microsoft.com/VisualStudio/feedback/details/1144980/error-lnk2001-unresolved-external-symbol-imp-iob-func.

// Constructor for impls tied to a vessel
XRSoundImpl::XRSoundImpl() : XRSound(),
    m_hDLL(nullptr), m_pEngine(nullptr)
{
}

XRSoundImpl::~XRSoundImpl()
{
    // Note: do not free m_pEngine here: it is a BORROWED reference, and is managed by the DLL side, both for modules and for vessels (and it lives in the XRSound.dll's heap!).
    // Also, if this is called from a *module*, it is quite possible that Orbiter already freed our XRSoundDLL module (since order is
    // indeterminate), and so any calls we would try to make to the singleton XRSoundDLL object in XRSoundDLL now would crash 
    // because the object would have already been freed.
}

// Instantiates a new XRSoundImpl instance tied to the supplied vessel
// Returns true on success, false if XRSound.dll not present.
bool XRSoundImpl::Initialize(VESSEL *pVessel)
{
    if (!pVessel)
        return false;

    bool retVal = false;
    // Note: GetModuleHandle is faster than LoadLibrary (and no need to call FreeLibrary on it), but it's not 
    // thread-safe.  However, GetModuleHandle is fine for our purposes since Orbiter is not multi-threaded anyway.
    m_hDLL = GetModuleHandle("XRSound.dll");  
    if (m_hDLL)
    {
        // Note: the m_pEngine acquired by this call is a *borrowed reference*: do not attempt to delete it
        VesselXRSoundEngineInstanceFuncPtr pFunc = reinterpret_cast<VesselXRSoundEngineInstanceFuncPtr>(GetProcAddress(m_hDLL, "GetXRSoundEngineInstance"));
        if (pFunc)
            m_pEngine = (pFunc)(pVessel->GetHandle());   // returns nullptr if sound initialization fails

        if (m_pEngine)
        {
            // Log both this vessel's XRSound.lib version and the XRSound.dll version
            // Note: in order to maximize cross-compiler version linking compatibility, we don't want to use any msvcrt functions in this library, so we can't use sprintf here.
            // Also, Orbiter's oapiWriteLog takes a char * instead of const char *, which I presume is just a bug, so we can safely (?) assume that is just a typo in the method signature.
            const float dllVersion = GetVersion();
            _ASSERTE(dllVersion > 0);
            char messageBuf[512];
            if (dllVersion >= 2.0)
            {
                const char *pVesselOrModuleName = GetLogID();
                const XRSoundEngine::EngineType engineType = GetEngineType();
                const char *pEngineType = XRSoundImpl::EngineTypeToStr(engineType);  // "Vessel", "Module", etc.

                StringCchPrintf(messageBuf, sizeof(messageBuf), "[XRSound INFO] %s '%s' built with XRSound API version %.2f", 
                    pEngineType, pVesselOrModuleName, XRSOUND_ENGINE_VERSION);
                oapiWriteLog(messageBuf);
            }

            if (dllVersion < XRSOUND_ENGINE_VERSION)
            {
                // user is running with an older XRSound.dll version than this vessel was linked with
                StringCchPrintf(messageBuf, sizeof(messageBuf), "[XRSOUND WARNING] XRSound.dll version %0.2f is installed, but an active Orbiter vessel or module was built with XRSound version %.2f.  Please install the latest XRSound version from https://www.alteaaerospace.com.",
                    dllVersion, XRSOUND_ENGINE_VERSION);
                oapiWriteLog(messageBuf);
            }
        }
    }
    
    return IsPresent();   // true if m_pEngine is not nullptr
}

// Static method that returns a string for the supplied engineType
const char *XRSoundImpl::EngineTypeToStr(const XRSoundEngine::EngineType engineType)
{
    const char *pStr;    // always initialized below
    switch (engineType)
    {
    case XRSoundEngine::EngineType::Module:
        pStr = "Module";
        break;

    case XRSoundEngine::EngineType::Vessel:
        pStr = "Vessel";
        break;

    default:
        // should never happen
        pStr = "(Warning: unknown object type)";
        break;
    }
    return pStr;
}

// Instantiates a new XRSoundImpl instance tied to the supplied module name.
//   pUniqueModuleName: may not be nullptr or empty
// Returns true on success, false if XRSound.dll not present.
bool XRSoundImpl::Initialize(const char *pUniqueModuleName)
{
    if (!pUniqueModuleName || !*pUniqueModuleName)
        return false;

    bool retVal = false;
    m_hDLL = GetModuleHandle("XRSound.dll");
    if (m_hDLL)
    {
        // Note: the m_pEngine acquired by this call is a *borrowed reference*: do not attempt to delete it
        ModuleXRSoundEngineInstanceFuncPtr pFunc = reinterpret_cast<ModuleXRSoundEngineInstanceFuncPtr>(GetProcAddress(m_hDLL, "GetModuleXRSoundEngineInstance"));
        if (pFunc)
            m_pEngine = (pFunc)(pUniqueModuleName);   // returns nullptr if sound initialization fails or if another module has previously registered using pUniqueModuleName
    }

    GetVersion();  // log a warning to Orbiter.log if the .lib and .dll versions don't match
    return IsPresent();   // true if m_pEngine is not nullptr
}

// Returns the version of XRSound.dll, or 0 if DLL not present.
float XRSoundImpl::GetVersion() const
{
    float version = 0;
    if (IsPresent())
        version = m_pEngine->GetVersion();

    return version;
}

// Loads the specified wav file and assigns the supplied sound ID to it.
// If you load a different wav file with the same ID as the previous wav file, the previous wav file is unloaded.
//   soundID: sound ID to be assigned to this wav file
//   pSoundFilename: path under $ORBITER_ROOT\XRSound of wav file to load; may not be nullptr
//   playbackType: denotes how sound will be faded
//
// Returns true on success, false if file not found or XRSound.dll not present.
bool XRSoundImpl::LoadWav(const int soundID, const char *pSoundFilename, const PlaybackType playbackType)
{
    if (!IsPresent())
        return false;

    // client code is not allowed to load a new sound into *DefaultGroup* sound slot (which would make no sense)
    if (IsDefaultSoundGroup(soundID))
        return false;

    _ASSERTE(pSoundFilename);
    _ASSERTE(*pSoundFilename);
    if (!pSoundFilename || !*pSoundFilename)
        return false;

    return m_pEngine->LoadWav(soundID, pSoundFilename, playbackType);
}

// Play the wav file with the specified ID.
//   soundID: unique sound ID originally passed  to LoadWav
//   bLoop: true to loop sound continuously until StopWav called, false to play only once
//   volume: 0 (mute) to 1.0 (loudest)
//
// Returns true on success, false if invalid sound ID or if XRSound.dll not present.
bool XRSoundImpl::PlayWav(const int soundID, const bool bLoop, const float volume)
{
    if (!IsPresent())
        return false;

    if (IsDefaultSoundOrGroup(soundID))
        return false;    // cannot manually affect default sounds or group sounds from client code side

    return m_pEngine->PlayWav(soundID, bLoop, volume);
}

// Stop playing the wav file with the specified ID.
//   soundID: unique sound ID originally passed to LoadWav
//
// Returns true on success, false if invalid sound ID or if XRSound.dll not present.
bool XRSoundImpl::StopWav(const int soundID)
{
    if (!IsPresent())
        return false;

    if (IsDefaultSoundOrGroup(soundID))
        return false;    // cannot manually affect default sounds or group sounds from client code side

    return m_pEngine->StopWav(soundID);
}

// Returns false if the specified sound is not playing or XRSound.dll is not present.
bool XRSoundImpl::IsWavPlaying(const int soundID) const
{
    // Note: OK to query if a *default* sound or group sound is playing, so we don't check for that here

    if (IsPresent())
        return m_pEngine->IsWavPlaying(soundID);

    return false;
}

// Pause or unpause a sound.
//   soundID: unique sound ID originally passed to LoadWav.
//   bPause: true to pause sound, false to unpause it
// Returns true on success, false if invalid sound ID or XRSound.dll not present.
bool XRSoundImpl::SetPaused(const int soundID, const bool bPause)
{
    if (!IsPresent())
        return false;

    if (IsDefaultSoundOrGroup(soundID))
        return false;    // cannot manually affect default sounds or group sounds from client code side

    return m_pEngine->SetPaused(soundID, bPause);
}

// Detect if a sound is paused.
//   soundID: unique sound ID originally passed to LoadWav.
// Returns true if sound is paused, or false if not paused, invalid sound ID, or XRSound.dll not present.
bool XRSoundImpl::IsPaused(const int soundID) const
{
    // Note: OK to query if a *default* sound or group sound is paused, so we don't check for that here

    if (IsPresent())
        return m_pEngine->IsPaused(soundID);

    return false;
}

// Enable or disable a default sound.  This can also be done globally via the configuration file; however, this option 
// is useful if you want to disable some sound effects at runtime.
//   soundID: which default sound to enable or disable
//   bEnabled: true to enable default sound, false to disable it
//
// Returns true on success, false if XRSound.dll not present or this default sound is disabled via config file (i.e., this default sound is not loaded).
bool XRSoundImpl::SetDefaultSoundEnabled(const DefaultSoundID soundID, const bool bEnabled)
{
    if (IsPresent())
        return m_pEngine->SetDefaultSoundEnabled(soundID, bEnabled);

    return false;
}

// Returns true if the specified default sound is enabled, false if sound is disabled or XRSound.dll not present.
//   option: which default sound ID to check
bool XRSoundImpl::GetDefaultSoundEnabled(const DefaultSoundID soundID) const
{
    if (IsPresent())
        return m_pEngine->GetDefaultSoundEnabled(soundID);

    return false;
}

// Set the default subfolder path for a default sound group, relative to $ORBITER_ROOT.
//   group: which XRSound group to update
//   pSubfolderPath: subfolder path relative to $ORBITER_ROOT; may not be nullptr or empty
//
// Returns true on success, false if XRSound.dll not present.
bool XRSoundImpl::SetDefaultSoundGroupFolder(const DefaultSoundID defaultSoundID, const char *pSubfolderPath)
{
    _ASSERTE(pSubfolderPath);
    _ASSERTE(*pSubfolderPath);

    // sanity-check the path
    if (!pSubfolderPath || !*pSubfolderPath)
        return false;       // path can't be nullptr or empty

    if (IsPresent())
        return m_pEngine->SetDefaultSoundGroupFolder(defaultSoundID, pSubfolderPath);

    return false;
}

// Returns the default subfolder path for a default sound group, relative to $ORBITER_ROOT, 
// or nullptr if XRSoundDLL not present.
//   group: which default XRSound group to check
const char *XRSoundImpl::GetDefaultSoundGroupFolder(const DefaultSoundID defaultSoundID) const
{
    if (IsPresent())
        return m_pEngine->GetDefaultSoundGroupFolder(defaultSoundID);
    
    return nullptr;
}

//===========================================================================================
// Methods added in XRSound API version 2.0
//===========================================================================================

// Calls XRSound.dll and returns the engine type
XRSoundEngine::EngineType XRSoundImpl::GetEngineType() const
{
    if (!HaveMinDLLVersion(2.0))
        return XRSoundEngine::EngineType::Unknown;

    return m_pEngine->GetEngineType();
}

// Calls XRSound.dll and returns the log ID (e.g., vessel name or module name), or "(name unknown)" if installed XRSound.dll version is 1.0
const char *XRSoundImpl::GetLogID() const
{
    if (!HaveMinDLLVersion(2.0))
        return "(name unknown)";

    return m_pEngine->GetLogID();
}

//===========================================================================================
// Methods added in XRSound API version 3.0
//===========================================================================================

bool XRSoundImpl::SetPan(const int soundID, const float pan)
{
    if (!HaveMinDLLVersion(3.0))
        return false;

    return m_pEngine->SetPan(soundID, pan);
}

float XRSoundImpl::GetPan(const int soundID)
{
    if (!HaveMinDLLVersion(3.0))
        return 0;

    return m_pEngine->GetPan(soundID);
}

bool XRSoundImpl::SetPlaybackSpeed(const int soundID, const float speed)
{
    if (!HaveMinDLLVersion(3.0))
        return false;

    return m_pEngine->SetPlaybackSpeed(soundID, speed);
}

float XRSoundImpl::GetPlaybackSpeed(const int soundID)
{
    if (!HaveMinDLLVersion(3.0))
        return 0;

    return m_pEngine->GetPlaybackSpeed(soundID);
}

bool XRSoundImpl::SetPlayPosition(const int soundID, const unsigned int positionMillis)
{
    if (!HaveMinDLLVersion(3.0))
        return false;

    return m_pEngine->SetPlayPosition(soundID, positionMillis);
}

int XRSoundImpl::GetPlayPosition(const int soundID)
{
    if (!HaveMinDLLVersion(3.0))
        return 0;

    return m_pEngine->GetPlayPosition(soundID);
}

//===========================================================================================
// NOTE: methods below here are only called internally; they are not part of the public API.
//===========================================================================================

// Returns true if the supplied sound ID is a default sound ID that does NOT denote a group, false if not
bool XRSoundImpl::IsDefaultSound(const int soundID) const
{
    if (IsPresent())
        return m_pEngine->IsDefaultSound(static_cast<XRSound::DefaultSoundID>(soundID));
    
    return false;
}

// Returns true if the supplied sound ID is a default sound ID that denotes a group (i.e., ends in "Group"), false if not
bool XRSoundImpl::IsDefaultSoundGroup(const int soundID) const
{
    if (IsPresent())
        return m_pEngine->IsDefaultSoundGroup(static_cast<XRSound::DefaultSoundID>(soundID));

    return false;
}

// Returns true if the supplied sound ID is a default sound *or* a default group ID
bool XRSoundImpl::IsDefaultSoundOrGroup(const int soundID) const
{
    if (IsPresent())
        return m_pEngine->IsDefaultSoundOrGroup(static_cast<XRSound::DefaultSoundID>(soundID));

    return false;
}

// Added in XRSound 2.0
// Returns true of XRSound.dll is present *and* is >= minimumVersionRequired
bool XRSoundImpl::HaveMinDLLVersion(const float minimumVersionRequired) const
{
    return (GetVersion() >= minimumVersionRequired);  // also checks IsPresent()
}
