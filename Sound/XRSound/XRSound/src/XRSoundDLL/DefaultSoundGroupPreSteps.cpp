// ==============================================================
// Implements default group sound handlers for XRSound.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#include "XRSoundDLL.h"
#include "DefaultSoundGroupPreSteps.h"

// -----------------------------------------------------------------------------------
// This abstract subclass handles playing back a default sound from a group of sounds
// -----------------------------------------------------------------------------------

// Constructor
DefaultSoundGroupPreStep::DefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine) :
    DefaultSoundPreStep(pEngine),
    m_pSoundFilesList(nullptr), m_currentSoundFileIndex(0)
{
}

// Destructor
DefaultSoundGroupPreStep::~DefaultSoundGroupPreStep()
{
    delete m_pSoundFilesList;
}

// Returns true if initialization successful, false if sound is disabled or folder parse failed (the sound will not play)
bool DefaultSoundGroupPreStep::Initialize(const int soundID, const char *pFolderSubpath, const XRSound::PlaybackType playbackType)
{
    m_soundID = soundID;
    m_playbackType = playbackType;

    return SetFolder(pFolderSubpath);
}

// Set the active folder and recursively (re)scan its contents.
//
// Returns true on success, or false if folder parse failed (i.e., folder does not exist or is empty)
bool DefaultSoundGroupPreStep::SetFolder(const char *pFolderSubpath)
{
    // Note: we never reset m_bWavPresent = false here: if this call fails, the *previous* folder remains set

    _ASSERTE(pFolderSubpath);
    if (!*pFolderSubpath)
        return false;       // empty

    delete m_pSoundFilesList;       // in case we are calling this multiple times

    // do not recurse subfolders
    m_pSoundFilesList = new FileList(pFolderSubpath, false, m_pEngine->GetValidSoundFileExtensions());
    const bool bSuccess = m_pSoundFilesList->Scan();
    CString msg;
    if (!bSuccess)
    {
        msg.Format("DefaultSoundGroupPreStep::SetFolder ERROR: folder path '%s' set for soundID %d does not exist.",
            pFolderSubpath, m_soundID);
        WriteLog(msg);
        return false;
    }

    if (m_pSoundFilesList->IsEmpty())
    {
        msg.Format("DefaultSoundGroupPreStep::SetFolder ERROR: folder path '%s' set for soundID %d exists, but no supported sound file types found.",
            pFolderSubpath, m_soundID);
        WriteLog(msg);
        return false;
    }

    // if we reach here, folder is valid and contains at least one valid sound file
    VERBOSE_LOG(m_pEngine, "DefaultSoundGroupPreStep::SetFolder: successfully scanned folder path '%s' for soundID %d: %d sound file(s) found with extensions [%s].",
        pFolderSubpath, m_soundID, m_pSoundFilesList->GetScannedFileCount(), static_cast<const char *>(GetConfig().SupportedSoundFileTypes));
    m_bWavPresent = true;
    m_csFolderSubpath = pFolderSubpath;  // success; e.g., "Default\Cabin Ambience"

    return true;
}

// Returns a random sound file path, different from the previous call's value
CString DefaultSoundGroupPreStep::GetRandomSoundFile()
{
    _ASSERTE(m_pSoundFilesList);
    CString file = m_pSoundFilesList->GetRandomFile();
    if (file.IsEmpty())
    {
        CString msg;
        msg.Format("DefaultSoundGroupPreStep::GetRandomSoundFile WARNING: sound files list is empty for soundID %d", m_soundID);
        WriteLog(msg);
    }
    return file;
}

// Returns the next sound file in the list
CString DefaultSoundGroupPreStep::GetNextSoundFile()
{
    _ASSERTE(m_pSoundFilesList);
    CString file = m_pSoundFilesList->GetFile(m_currentSoundFileIndex);
    m_currentSoundFileIndex++;
    if (m_currentSoundFileIndex >= m_pSoundFilesList->GetScannedFileCount())
        m_currentSoundFileIndex = 0;  // wrapped around

    if (file.IsEmpty())
    {
        CString msg;
        msg.Format("DefaultSoundGroupPreStep::GetNextSoundFile WARNING: sound files list is empty for soundID %d", m_soundID);
        WriteLog(msg);
    }
    return file;
}

// Locate the first file in our file list with the specified basename, load it, and play it.
//   pBasename: e.g., "1000" to locate "1000.flac", "1000.mp3" etc.  May not be nullptr or empty.
bool DefaultSoundGroupPreStep::LoadAndPlayWavWithBasename(const char *pBasename, const bool bLoop, const float volume)
{
    _ASSERTE(pBasename);
    _ASSERTE(*pBasename);
    _ASSERTE(m_pSoundFilesList);

    if (!pBasename || !*pBasename || !m_pSoundFilesList)
        return false;

    bool bSuccess = false;

    if (LoadWavWithBasename(pBasename))
        bSuccess = PlayWav(bLoop, volume);

    return bSuccess;
}

// Locate the first file in our file list with the specified basename.
bool DefaultSoundGroupPreStep::LoadWavWithBasename(const char *pBasename)
{
    _ASSERTE(pBasename);
    _ASSERTE(*pBasename);
    _ASSERTE(m_pSoundFilesList);

    if (!pBasename || !*pBasename || !m_pSoundFilesList)
        return false;

    bool bSuccess = false;

    const CString *pcsFilespec = m_pSoundFilesList->FindFileWithBasename(pBasename);
    if (pcsFilespec)
        bSuccess = LoadWav(*pcsFilespec);

    return bSuccess;
}

// ------------------------------------------------------------------------

// Class that plays a random sound at random intervals between minDelay and maxDelay (in seconds)
// Constructor
RandomDefaultSoundGroupPreStep::RandomDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine) :
    DefaultSoundGroupPreStep(pEngine), m_nextPlayTime(-1)
{
    // Note: we can't call ResetTimer() here yet because the subclasse is not constructed yet
}

// ------------------------------------------------------------------------

// set m_nextPlayTime.  NOTE: this is *realtime*, not simt!
void RandomDefaultSoundGroupPreStep::ResetTimer()
{
    MinMaxDelay minMax = GetMinMaxDelay();

    const double minDelay = minMax.Min;
    double maxDelay = minMax.Max;

    // sanity-check the max time
    if (maxDelay < minDelay)
        maxDelay = minDelay;

    const double range = maxDelay - minDelay;
    const double delay = ((oapiRand() * range)) + minDelay;

    _ASSERTE(delay >= minDelay);
    _ASSERTE(delay <= maxDelay);
    m_nextPlayTime = XRSoundDLL::GetSystemUptime() + delay;  // this is *realtime*, not simt (we don't time ACC to affect this).
}

void RandomDefaultSoundGroupPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    double m_systemUptime = XRSoundDLL::GetSystemUptime();

    if (m_nextPlayTime < 0)
        ResetTimer();       // it's the first time through here, so initialize the timer

    // if it's time to play a random sound, confirm with the subclass that it's OK to play it now
    if (m_systemUptime >= m_nextPlayTime)
    {
        ResetTimer();
        //_ASSERTE(m_systemUptime > simt); // simulator time can be bigger than system time using a very fast time acceleration

        if (ShouldPlayNow(simt, simdt, mjd))
            PlayRandom();
    }
}

// Loads and plays a random ambient sound via our soundID
void RandomDefaultSoundGroupPreStep::PlayRandom()
{
    if (!IsWavPlaying())    // wait until next step if the previous (long) wav is still playing
    {
        const CString csSoundFile = GetRandomSoundFile();  // never repeats from the previous value (unless only one file exists in the list).
        if (!csSoundFile.IsEmpty())       // sanity check; should never be empty here
        {
            LoadWav(csSoundFile);
            PlayWav(false);
        }
    }
}

// ------------------------------------------------------------------------

// Plays an ambient sound in cockpit mode at configured random intervals.
AmbientDefaultSoundGroupPreStep::AmbientDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine) :
    RandomDefaultSoundGroupPreStep(pEngine)
{
}

bool AmbientDefaultSoundGroupPreStep::ShouldPlayNow(const double simt, const double simdt, const double mjd)
{
    // since this is an internal sound, only bother to play it if we are in internal view [implies focus] (slight optimization)
    return InCockpitView();
}

RandomDefaultSoundGroupPreStep::MinMaxDelay AmbientDefaultSoundGroupPreStep::GetMinMaxDelay()
{
    return RandomDefaultSoundGroupPreStep::MinMaxDelay(GetConfig().CabinAmbienceMin, GetConfig().CabinAmbienceMax);
}

// ------------------------------------------------------------------------

// Plays an ATC sound at configured random intervals.
ATCDefaultSoundGroupPreStep::ATCDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine) :
    RandomDefaultSoundGroupPreStep(pEngine)
{
}

bool ATCDefaultSoundGroupPreStep::ShouldPlayNow(const double simt, const double simdt, const double mjd)
{
    bool bShouldPlay = false;

    // since this is a radio sound, only bother to play it if we have focus (slight optimization)
    if (HasFocus())
    {
        if (m_pEngine->IsLanded())
            bShouldPlay = GetConfig().ATCAllowWhileLanded;
        else if (m_pEngine->InReentry())
            bShouldPlay = GetConfig().ATCAllowDuringReentry;
        else if (m_pEngine->InAtmosphere())
            bShouldPlay = GetConfig().ATCAllowInAtmosphere;
        else
            bShouldPlay = true;   // normal flight
    }

    return bShouldPlay;
}

// returns next minimum and maximum delay times in seconds
RandomDefaultSoundGroupPreStep::MinMaxDelay ATCDefaultSoundGroupPreStep::GetMinMaxDelay()
{
    double min = GetConfig().ATCMinDelay;
    double max = GetConfig().ATCMaxDelay;
    
    // see if the user disabled these sounds by setting min or max to zero
    if ((min <= 0) || (max <= 0))
        return RandomDefaultSoundGroupPreStep::MinMaxDelay(MAXINT, MAXINT);  // sounds will never play

    const VESSEL *pVessel = GetVessel();
    if (pVessel)  // sanity check in case Orbiter deleted it out from under us as the other checks missed it
    {
        const double distInKm = GetVessel()->GetAltitude() / 1000.0;
        if (distInKm >= GetConfig().ATCDelayPlanetDistance)
        {
            // delay ATC per config
            min *= GetConfig().ATCDelayPlanetMultiplier;  // Note: ATCDelayPlanetMultiplier was validated to minimum of 1.0
            max *= GetConfig().ATCDelayPlanetMultiplier;
        }
    }
    // DEV DEBUGGING ONLY: if (IsDG()) sprintf(oapiDebugString(), "Next ATC min=%.1lf, max=%.1lf", min, max); 

    return RandomDefaultSoundGroupPreStep::MinMaxDelay(min, max);
}

// hook the base class method so we can override the volume
bool ATCDefaultSoundGroupPreStep::PlayWav(const bool bLoop, float volume)
{
    return DefaultSoundPreStep::PlayWav(bLoop, GetConfig().ATCVolume);
}

// ------------------------------------------------------------------------

// Constructor
AltitudeCalloutsDefaultSoundGroupPreStep::AltitudeCalloutsDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine) :
    DefaultSoundGroupPreStep(pEngine),
    m_previousFrameAltitude(-1), m_nextMinimumCalloutTime(-1)
{
}

// invoked at every timestep.  This code was (mostly) cloned from AltitudeCalloutsPreStep::clbkPrePostStep
void AltitudeCalloutsDefaultSoundGroupPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    const double altitude = m_pEngine->GetGearAdjustedAltitude();
    if (m_previousFrameAltitude >= 0)    // do we have valid data yet?
    {
        // get our vertical speed in meters per second
        VECTOR3 v;
        pVessel->GetAirspeedVector(FRAME_HORIZON, v);
        const double currentDescentRate = (pVessel->GroundContact() ? 0 : v.y);  // in m/s

        // If gear status is avilable AND if descending at > 0.25 m/s/s below 275 meters, warn pilot if gear is fully up; do NOT warn pilot if gear is in motion OR if the ship 
        // is below standard "wheels-down" altitude.
        // Also, we do not check or reset the m_nextMinimumCalloutTime here because this should play independently of the altitude callouts.
        if (m_pEngine->HasLandingGear())
        {
            if ((altitude < m_previousFrameAltitude) && (altitude < GetConfig().WarningGearIsUpAltitude) && (m_pEngine->GetLandingGearAnimationState() != 1.0) &&  // gear not fully deployed?
                (currentDescentRate <= -0.25) && (altitude > 0))
            {
                // NOTE: we do not just use LoadAndPlayWav here because we need to load this into the correct slot in case the user disabled it via code!
                LoadAndPlayWavUsingID(XRSound::WarningGearIsUp, GetConfig().WarningGearIsUp, false, XRSound::PlaybackType::Radio);
                goto exit;
            }
        }

        // do not play callouts until minimum time has elapsed, in case pilot is hovering at the same altitude
        // also, do not play on the FIRST frame of the simulation
        if ((simt >= m_nextMinimumCalloutTime) && (m_previousFrameAltitude >= 0))
        {
            // check special case for landing clearance
            const double landingClearanceAlt = 1500;  // this is via a setting in the XR code, but we just use the default value here
            if ((m_previousFrameAltitude > landingClearanceAlt) && (altitude <= landingClearanceAlt))   // descent
            {
                // do not play the callout if vertical speed is too high; i.e., if we are going to crash!
                if (currentDescentRate > -150)   // vertical speed is in NEGATIVE m/s
                {
                    // NOTE: we do not just use LoadAndPlayWav here because we need to load this into the correct slot in case the user disabled it via code!
                    if (LoadAndPlayWavUsingID(XRSound::YouAreClearedToLand, GetConfig().YouAreClearedToLand, false, XRSound::PlaybackType::Radio))
                        SetNextMinimumCalloutTime(simt);
                }
                    
            }
            else  // normal altitude checks
            {
                static const double altitudeCallouts[] =
                {
                    5000, 4000, 3000, 2000, 1000, 900, 800, 700, 600,
                    500, 400, 300, 200, 100, 75, 50, 40, 30, 20, 15, 10,
                    9, 8, 7, 6, 5, 4, 3, 2, 1
                };

                // optimization: skip loop if distance > max callout distance
                if (altitude <= altitudeCallouts[0])
                {
                    for (int i = 0; i < (sizeof(altitudeCallouts) / sizeof(double)); i++)
                    {
                        const double a = altitudeCallouts[i];

                        // play on descent only
                        if ((m_previousFrameAltitude > a) && (altitude <= a))   // descent
                        {
                            CString csBasename;
                            csBasename.Format("%d", static_cast<int>(a));
                            if (LoadAndPlayWavWithBasename(csBasename))
                                SetNextMinimumCalloutTime(simt);
                            break;
                        }
                    }
                }
            }
        }
    }  // if (m_previousFrameAltitude >= 0)

exit:
    // save for next loop
    m_previousFrameAltitude = altitude;
}

// ------------------------------------------------------------------------

// Constructor
DockingCalloutsDefaultSoundGroupPreStep::DockingCalloutsDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine) :
    DefaultSoundGroupPreStep(pEngine),
    m_previousFrameDistance(-1), m_nextMinimumCalloutTime(-1), m_bJustDocked(false), m_previousWasDocked(false),
    m_previousSimt(-1), m_undockingMsgTime(-1), m_intervalStartTime(-1), m_intervalStartDistance(-1)
{
}

// This is based on the XR vessels' DockingCalloutsPreStep method
void DockingCalloutsDefaultSoundGroupPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    // check if docked 
    if (m_pEngine->IsDocked())
    {
        // check whether we just docked
        if (m_previousFrameDistance >= 0)
        {
            m_bJustDocked = true;       // so we play our follow-up callout shortly
            // NOTE: we do not just use LoadAndPlayWav here because we need to load this into the correct slot in case the user disabled it via code!
            LoadAndPlayWavUsingID(XRSound::DockingCallout, GetConfig().DockingCallout, false, XRSound::PlaybackType::Radio);     // e.g., "Contact!" voice callout
            LoadAndPlayWavUsingID(XRSound::Docking, GetConfig().Docking, false, XRSound::PlaybackType::InternalOnly);

            m_previousFrameDistance = -1;    // reset
        }

        m_previousWasDocked = true;   // remember this
        m_previousSimt = simt;
        return;     // nothing more to do when docked
    }
    else  // not docked
    {
        // check whether we just undocked IF we have had time to set m_previousWasDocked before
        if ((m_previousSimt >= 0) && m_previousWasDocked)
        {
            // NOTE: we do not just use LoadAndPlayWav here because we need to load this into the correct slot in case the user disabled it via code!
            LoadAndPlayWavUsingID(XRSound::Undocking, GetConfig().Undocking, false, XRSound::PlaybackType::InternalOnly);
            m_undockingMsgTime = simt + 0.667;    // wait 2/3-second before playing confirmation
        }
        else if ((m_undockingMsgTime > 0) && (simt >= m_undockingMsgTime))
        {
            // NOTE: we do not just use LoadAndPlayWav here because we need to load this into the correct slot in case the user disabled it via code!
            LoadAndPlayWavUsingID(XRSound::UndockingCallout, GetConfig().UndockingCallout, false, XRSound::PlaybackType::Radio);   // e.g., "Undocking confirmed" voice callout
            m_undockingMsgTime = -1;    // reset
        }

        m_previousWasDocked = false;   // remember this
    }

    // returns -1 if no docking target set
    const double distance = m_pEngine->GetDockingDistance();
    if (distance < 0)
    {
        // no docking port in range, so reset intervals
        m_intervalStartTime = -1;
        m_intervalStartDistance = -1;
    }
    else   // docking port is in range, so check whether we need to reinitialize m_intervalStartTime
    {
        if (m_intervalStartTime < 0)
        {
            // docking port just came into range
            m_intervalStartDistance = distance;
            m_intervalStartTime = simt;
        }
    }

    if ((distance >= 0) && (m_previousFrameDistance >= 0))  // no callouts if not in range OR if we just entered range but haven't updated previous distance yet.
    {
        _ASSERTE(m_intervalStartTime >= 0);
        _ASSERTE(m_intervalStartDistance >= 0);

        // Note: in order to support UCD (Universal Cargo Deck), only play callouts for the other vessel's docking port if the ship has closed at least 0.1 meter over the last second (0.1 m/s)
        // Vessel distance "jitters" even when a vessel is attached to UCD which is attached in the XR payload bay.
        const double timeSinceIntervalStart = simt - m_intervalStartTime;
        double closingRate = 0;
        if (timeSinceIntervalStart >= 1.0)  // time to take another interval measurement?
        {
            // see if we are closing at >= 0.1 meter-per-second (postive == approaching the docking port)
            closingRate = -((distance - m_intervalStartDistance) / timeSinceIntervalStart);

            // DEV DEBUGGING ONLY: sprintf(oapiDebugString(), "closingRate=%lf, m_intervalStartDistance=%lf, m_intervalStartTime=%lf, simt=%lf", closingRate, m_intervalStartDistance, m_intervalStartTime, simt);

            // reset for next interval measurement
            m_intervalStartDistance = distance;
            m_intervalStartTime = simt;
        }

        // do not play callouts until minimum time has elapsed, in case pilot is hovering at the same distance
        // also, do not play on the FIRST frame of the simulation or if there is no active docking target
        if ((simt >= m_nextMinimumCalloutTime) && (m_previousFrameDistance >= 0))
        {
            static const double distanceCallouts[] =
            {
                5000, 4000, 3000, 2000, 1000, 900, 800, 700, 600,
                500, 400, 300, 200, 100, 75, 50, 40, 30, 20, 15, 10,
                9, 8, 7, 6, 5, 4, 3, 2, 1
            };

            // optimization: skip loop if distance > max callout distance
            if (distance <= distanceCallouts[0])
            {
                for (int i = 0; i < (sizeof(distanceCallouts) / sizeof(double)); i++)
                {
                    const double dist = distanceCallouts[i];

                    // play on approach only
                    if ((m_previousFrameDistance > dist) && (distance <= dist))   // closing
                    {
                        CString csBasename;
                        csBasename.Format("%d", static_cast<int>(dist));
                        if (LoadAndPlayWavWithBasename(csBasename))
                            SetNextMinimumCalloutTime(simt);
                        break;
                    }
                }
            }
        }
    }

    // save for next loop
    m_previousSimt = simt;
    m_previousFrameDistance = distance;
}

// ------------------------------------------------------------------------

// Constructor
MachCalloutsDefaultSoundGroupPreStep::MachCalloutsDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine) :
    DefaultSoundGroupPreStep(pEngine), 
    m_previousMach(-1), m_nextMinimumCalloutTime(-1)
{
    // load our two custom sounds handled by this prestep
    m_pEngine->LoadWav(XRSound::SonicBoom, GetConfig().SonicBoom, XRSound::PlaybackType::BothViewFar);
    m_pEngine->LoadWav(XRSound::SubsonicCallout, GetConfig().SubsonicCallout, XRSound::PlaybackType::Radio);
}

// This is based on the XR vessels' MachCalloutsPreStep method
void MachCalloutsDefaultSoundGroupPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    VESSEL *pVessel = GetVessel();
    if (!pVessel)
        return;     // sanity check

    const double mach = pVessel->GetMachNumber();
    const bool groundContact = pVessel->GroundContact();

    if (!groundContact && (mach <= 0))  // prevent resets when on ground
    {
        m_previousMach = MAXLONG;   // out of the atmosphere
        return;     // nothing more to do
    }

    // if no atmosphere, reset callout data; this is necessary in case the ship is instantly transported via editing the config file
    //
    // CORE BUG WORKAROUND: on IO, GetAtmPressure() == 0 but GetMachNumber() > 1!  Therefore, we must check current mach number instead of atmPressure.
    // In addition, disable mach callouts if OAT temperature is not valid (i.e., static pressure too low)
    if ((m_previousMach <= 0) || (mach <= 0) || !m_pEngine->IsOATValid())
    {
        m_previousMach = 0;
        goto exit;  // no reason to perform additional checks
    }

    // do not play callouts until minimum time has elapsed, in case pilot is hovering at the same mach
    // also, do not play on the FIRST frame of the simulation
    if ((simt >= m_nextMinimumCalloutTime) && (m_previousMach >= 0))
    {
        // check for special mach callouts
        if ((m_previousMach >= 1.0) && (mach < 1.0))    // decelerating below mach 1
        {
            m_pEngine->StopWav(XRSound::SonicBoom);  // in case it's still playing from before
            m_pEngine->PlayWav(XRSound::SonicBoom);
            LoadAndPlayWavWithBasename("Subsonic");
        }
        else if ((m_previousMach < 1.0) && (mach >= 1.0))  // accelerating past mach 1
        {
            m_pEngine->StopWav(XRSound::SonicBoom);  // in case it's still playing from before
            m_pEngine->PlayWav(XRSound::SonicBoom);
            LoadAndPlayWavWithBasename("Mach 1");
        }
        else if ((m_previousMach < 27.0) && (mach >= 27.0))  // do not play "mach 27+" on deceleration
        {
            LoadAndPlayWavWithBasename("Mach 27 Plus");
        }
        else  // perform standard mach callouts
        {
            for (double m = 2; m < 27; m++)
            {
                if (((m_previousMach < m) && (mach >= m)) ||  // acceleration
                    ((m_previousMach > m) && (mach <= m)))    // deceleration
                {
                    CString csBasename;
                    csBasename.Format("Mach %d", static_cast<int>(m));
                    LoadAndPlayWavWithBasename(csBasename);
                    break;
                }
            }
        }
    }

exit:
    // save for next loop
    m_previousMach = mach;
}

// ------------------------------------------------------------------------

// Constructor
MusicDefaultSoundGroupPreStep::MusicDefaultSoundGroupPreStep(VesselXRSoundEngine *pEngine) :
    DefaultSoundGroupPreStep(pEngine),
    m_startNextSongRealtime(0)  // 0 == play song immediately
{
}

// NOTE: this prestep operates on our global, vessel-independent MusicFolder sound (has a static WavContext; see FindWavContext).
void MusicDefaultSoundGroupPreStep::clbkPreStep(const double simt, const double simdt, const double mjd)
{
    // if this vessel does not have focus, it should not have any effect on the global MusicFolder sound
    if (!HasFocus())
        return;

    // To prevent intermittent music stuttering on Orbiter startup when Orbiter hangs for a full second, which starves the irrKLang engine, 
    // don't play any music until two seconds after simulation start.
    if (simt < 2.0)
    {
        if (IsWavPlaying())
            SetPaused(true);
        return;
    }

    // figure out if we should play music or not based on settings
    const bool bInternalView = m_pEngine->InCockpitView();
    bool bPlayMusic = false;
    if (m_pEngine->InAtmosphere()) 
    {
        if (bInternalView)
            bPlayMusic = (GetConfig().MusicPlayInternal == XRSoundConfigFileParser::MusicPlay::On);
        else
            bPlayMusic = (GetConfig().MusicPlayExternal == XRSoundConfigFileParser::MusicPlay::On);
    }
    else   // we're in space
    {
        if (bInternalView)
            bPlayMusic = (GetConfig().MusicPlayInternal == XRSoundConfigFileParser::MusicPlay::Space) || (GetConfig().MusicPlayInternal == XRSoundConfigFileParser::MusicPlay::On);
        else
            bPlayMusic = (GetConfig().MusicPlayExternal == XRSoundConfigFileParser::MusicPlay::Space) || (GetConfig().MusicPlayExternal == XRSoundConfigFileParser::MusicPlay::On);
    }

    // We don't want to constantly keep replaying the song from the beginning each time the song shouldn't play, so instead we pause / unpause the song
    // if it's already playing.
    if (!bPlayMusic)
    {
        if (IsWavPlaying())
            SetPaused(true);    // so we can resume it later
        // else it's already stopped playing, so nothing more to do here
    }
    else  // we should play music
    {
        if (IsPaused())
        {
            // resume current song where we paused it before
            SetPaused(false);
        }
        else if (!IsWavPlaying())
        {
            // song finished; start the next one after waiting two seconds of realtime (not simtime)
            if (m_startNextSongRealtime < 0)
            {
                m_startNextSongRealtime = XRSoundDLL::GetSystemUptime() + 2.0;
            }
            else if (XRSoundDLL::GetSystemUptime() >= m_startNextSongRealtime)
            {
                // time to start the next song
                m_startNextSongRealtime = -1;        // reset
                CString csMusicFile = ((GetConfig().MusicOrder == XRSoundConfigFileParser::SeqRandom::Sequential) ? GetNextSoundFile() : GetRandomSoundFile());

                if (!csMusicFile.IsEmpty())
                    LoadAndPlayWav(csMusicFile, GetConfig().MusicVolume);
                // else list is empty or file could not be played, so we will restart the timer to try the next song, if any, in the next frame since the sound is not playing
            }
        }
        // else song is already playing, so nothing more to do
    }
}

