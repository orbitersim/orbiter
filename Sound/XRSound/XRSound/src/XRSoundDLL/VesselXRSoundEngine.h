// ==============================================================
// XRSoundEngine class that is bound to an Orbiter vessel; this file is not distributed with XRSound.
// The code for this exists in XRSound.dll.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include "XRSoundEngine.h"
#include "AnimationState.h"

class DefaultSoundPreStep;
class DefaultSoundGroupPreStep;
class SoundPreStep;


// WARNING: if using static linking, do not allocate or free any memory in any inline methods here: XRSound.dll uses its own heap, so any
// code that allocates or frees memory must reside in XRSound.dll, *not* in the calling DLL via non-virtual inline code
// in this header file.

// NOTE: these methods must all be *virtual* so that they can be located via the object pointer from inside 
// the caller's DLL: by design, there is no import library for XRSound.dll.
// Do NOT use any inline code for any methods invoked by XRSoundImpl, or it will reside in the caller's DLL instead of XRSound.dll.
//
// NOTE: this class is not thread-safe!  However, Orbiter is not multi-threaded, so that should not be an issue.
class VesselXRSoundEngine : public XRSoundEngine
{
public:
    // Note: constructor and destructor are private by design; callers should always use CreateInstance and DestroyInstance
    // to create and destroy instances of XRSoundEngine.
    static VesselXRSoundEngine *CreateInstance(OBJHANDLE hVessel);

    virtual EngineType GetEngineType() override { return EngineType::Vessel; }
    virtual const char *GetLogID() override { return GetVesselName(); }

    // Methods only invoked by XRSoundDLL (our Orbiter module class).  Because these are non-virtual, it is impossible for XRSoundLib to invoke them
    // because it cannot link with them when the Orbiter vessel using XRSoundLib tries to link.
    bool HasFocus() const { return (oapiGetFocusObject() == m_hVessel); }
    bool InCockpitView() const { return (HasFocus() && oapiCameraInternal()); }
    bool InAtmosphere();
    bool IsLanded();    // can't be const; we get the vessel * from Orbiter
    bool InReentry();   // ditto
    double GetGearAdjustedAltitude();
    double GetLandingGearAnimationState();
    bool HasLandingGear();
    bool IsDocked();
    double GetDockingDistance();
    double GetPlasmaLevel();
    double GetExternalTemperature();
    bool IsOATValid();

    virtual bool SetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID, const bool bEnabled) override;
    virtual bool GetDefaultSoundEnabled(const XRSound::DefaultSoundID soundID) override;

    virtual bool SetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID, const char *pSubfolderPath) override;
    virtual const char *GetDefaultSoundGroupFolder(const XRSound::DefaultSoundID groupSoundID) const override;

    void LoadGlobalSounds();
    virtual void LoadDefaultSounds();  // subclasses, if there ever are any, would probably want to override this method
    virtual void FreeDefaultSounds();

    void AddSoundPreStep(SoundPreStep *pPreStep);
    bool AddDefaultSound(DefaultSoundPreStep *pPreStep, const int soundID, const char *pSoundFileOrFolderName, const XRSound::PlaybackType playbackType);
    DefaultSoundGroupPreStep *FindDefaultSoundGroupPreStep(const int soundID) const;  // NOTE: can't use this for DefaultSound objects in general b/c many PreSteps have more than one sound slot (but groups only have one slot, so that is fine).
    
    // These are only here for debugging multi-vessel scenarios.
    // TODO: #ifdef out these methods once we no longer need them for debugging.
    bool IsXR1() { return (GetVesselName() == "XR1-01"); }
    bool IsXR2() { return (GetVesselName() == "XR2-01"); }
    bool IsDG() { return (GetVesselClassName().Left(11) == "DeltaGlider"); }
    bool IsISS() { return (GetVesselName() == "ISS"); }

    // Returns a vessel's animation ("door") state for the supplied animation ID; 
    // used by DefaultSoundPreStep callback methods to determine which sound to play.
    //
    // Returns nullptr if this vessel does not have an animation with the supplied ID.
    //
    // WARNING: do not maintain a reference to the returned pointer across frames: the returned
    // pointer is a borrowed reference and is only valid during the current frame.
    const AnimationState *GetAnimationState(const int animID) const
    {
        auto it = m_animIDStateMap.find(animID);
        return ((it != m_animIDStateMap.end()) ? &(it->second) : nullptr);
    }

    VESSEL *GetVessel()
    {
        VESSEL *pRetVal = nullptr;
        if (oapiIsVessel(m_hVessel))
            pRetVal = oapiGetVesselInterface(m_hVessel);
        return pRetVal;
    }

    OBJHANDLE GetVesselHandle() const { return m_hVessel; }

    const CString &GetVesselName()
    {
        VESSEL *pVessel = GetVessel();
        if (pVessel)
            m_csCachedVesselName = pVessel->GetName();
        else
        {
            if (m_csCachedVesselName.IsEmpty())
                m_csCachedVesselName = "<deleted>";   // could only happen if vessel is created and deleted within a few frames
        }
        return m_csCachedVesselName;
    }

    const CString &GetVesselClassName()
    {
        VESSEL *pVessel = GetVessel();
        if (pVessel)
            m_csCachedVesselClass = pVessel->GetClassName();
        else
        {
            if (m_csCachedVesselClass.IsEmpty())
                m_csCachedVesselClass = "<deleted>";   // could only happen if vessel is created and deleted within a few frames
        }
        return m_csCachedVesselClass;
    }

    double GetCameraDistance();
    // invoked ~20 times per second to update this sound's position relative to Orbiter's camera as well as update sounds via irrKlang engine
    void clbkPreStep(const double simt, const double simdt, const double mjd);

protected:
    virtual void FreeResources() override;
    virtual void UpdateSoundState(WavContext &context) override;

    void UpdateAllSoundStates();   // invoked every frame from clbkPreStep
    void PollAllAnimationStates();
    void FadeVolumeForDistanceAndPressure(float &volume, const double maxLoudnessDistance);
    void FadeVolumeForPressure(float &volume);

    vector<SoundPreStep *> m_allSoundPreSteps;
    OBJHANDLE m_hVessel;       // handle of Orbiter vessel associated with this engine; NOTE: may have been destroyed; always check first!

    // This should be called shortly after this engine is initialized; it caches data used for logging
    // purposes so that we can still log those values (for a short time) even after the vessel is 
    // deleted by Orbiter.
    void InitializeCachedData()
    {
        GetVessel();
        GetVesselName();
    }

    // defines map of all of this vessel's animation ("door") states for the current frame
    typedef unordered_map<int, AnimationState> HASHMAP_ANIMID_ANIMATIONSTATE;
    typedef pair<int, AnimationState> ANIMID_ANIMATIONSTATE_Pair;

    // map of all animation states read for this current frame
    HASHMAP_ANIMID_ANIMATIONSTATE m_animIDStateMap;

private:
    // these are private to prevent incorrect creation or deletion outside of our static CreateInstance and DestroyInstance methods
    VesselXRSoundEngine(const OBJHANDLE hVessel);
    virtual ~VesselXRSoundEngine() override;

    CString m_csCachedVesselName;  // used by GetVesselName
    CString m_csCachedVesselClass;
};