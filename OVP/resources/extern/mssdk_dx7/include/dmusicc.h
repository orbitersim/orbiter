/************************************************************************
*                                                                       *
*   dmusicc.h -- This module defines the DirectMusic core API's         *
*                                                                       *
*   Copyright (c) 1998, Microsoft Corp. All rights reserved.            *
*                                                                       *
************************************************************************/

#ifndef _DMUSICC_
#define _DMUSICC_

#include <windows.h>

#define COM_NO_WINDOWS_H
#include <objbase.h>

#include <mmsystem.h>

#include "dls1.h"
#include "dmerror.h"
#include "dmdls.h"
#include "dsound.h"
#include "dmusbuff.h"

#include <pshpack8.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DMUS_MAX_DESCRIPTION 128
#define DMUS_MAX_DRIVER 128

typedef struct _DMUS_BUFFERDESC *LPDMUS_BUFFERDESC;
typedef struct _DMUS_BUFFERDESC{
    DWORD dwSize;
    DWORD dwFlags;
    GUID guidBufferFormat;
    DWORD cbBuffer;
} DMUS_BUFFERDESC;

/* DMUS_EFFECT_ flags are used in the dwEffectFlags fields of both DMUS_PORTCAPS 
 * and DMUS_PORTPARAMS.
 */
#define DMUS_EFFECT_NONE             0x00000000
#define DMUS_EFFECT_REVERB           0x00000001
#define DMUS_EFFECT_CHORUS           0x00000002

/* For DMUS_PORTCAPS dwClass
 */ 
#define DMUS_PC_INPUTCLASS       (0)
#define DMUS_PC_OUTPUTCLASS      (1)

/* For DMUS_PORTCAPS dwFlags
 */
#define DMUS_PC_DLS              (0x00000001)
#define DMUS_PC_EXTERNAL         (0x00000002)
#define DMUS_PC_SOFTWARESYNTH    (0x00000004)
#define DMUS_PC_MEMORYSIZEFIXED  (0x00000008)
#define DMUS_PC_GMINHARDWARE     (0x00000010)
#define DMUS_PC_GSINHARDWARE     (0x00000020)
#define DMUS_PC_XGINHARDWARE     (0x00000040)
#define DMUS_PC_DIRECTSOUND      (0x00000080)
#define DMUS_PC_SHAREABLE        (0x00000100)
#define DMUS_PC_DLS2             (0x00000200)
#define DMUS_PC_SYSTEMMEMORY     (0x7FFFFFFF)


typedef struct _DMUS_PORTCAPS *LPDMUS_PORTCAPS;
typedef struct _DMUS_PORTCAPS
{
    DWORD   dwSize;
    DWORD   dwFlags;
    GUID    guidPort;
    DWORD   dwClass;
    DWORD   dwType;
    DWORD   dwMemorySize;
    DWORD   dwMaxChannelGroups;
    DWORD   dwMaxVoices;    
    DWORD   dwMaxAudioChannels;
    DWORD   dwEffectFlags;
    WCHAR   wszDescription[DMUS_MAX_DESCRIPTION];
} DMUS_PORTCAPS;

/* Values for DMUS_PORTCAPS dwType. This field indicates the underlying 
 * driver type of the port.
 */
#define DMUS_PORT_WINMM_DRIVER      (0)
#define DMUS_PORT_USER_MODE_SYNTH   (1)
#define DMUS_PORT_KERNEL_MODE       (2)

/* These flags (set in dwValidParams) indicate which other members of the */
/* DMUS_PORTPARAMS are valid. */
/* */
#define DMUS_PORTPARAMS_VOICES           0x00000001
#define DMUS_PORTPARAMS_CHANNELGROUPS    0x00000002
#define DMUS_PORTPARAMS_AUDIOCHANNELS    0x00000004
#define DMUS_PORTPARAMS_SAMPLERATE       0x00000008
#define DMUS_PORTPARAMS_EFFECTS          0x00000020
#define DMUS_PORTPARAMS_SHARE            0x00000040

typedef struct _DMUS_PORTPARAMS *LPDMUS_PORTPARAMS;
typedef struct _DMUS_PORTPARAMS
{
    DWORD   dwSize;
    DWORD   dwValidParams;
    DWORD   dwVoices;
    DWORD   dwChannelGroups;
    DWORD   dwAudioChannels;
    DWORD   dwSampleRate;
    DWORD   dwEffectFlags;
    BOOL    fShare;
} DMUS_PORTPARAMS;

typedef struct _DMUS_SYNTHSTATS *LPDMUS_SYNTHSTATS;
typedef struct _DMUS_SYNTHSTATS
{
    DWORD   dwSize;             /* Size in bytes of the structure */
    DWORD   dwValidStats;       /* Flags indicating which fields below are valid. */
    DWORD   dwVoices;           /* Average number of voices playing. */
    DWORD   dwTotalCPU;         /* Total CPU usage as percent * 100. */
    DWORD   dwCPUPerVoice;      /* CPU per voice as percent * 100. */
    DWORD   dwLostNotes;        /* Number of notes lost in 1 second. */
    DWORD   dwFreeMemory;       /* Free memory in bytes */
    long    lPeakVolume;        /* Decibel level * 100. */
} DMUS_SYNTHSTATS;

#define DMUS_SYNTHSTATS_VOICES          (1 << 0)
#define DMUS_SYNTHSTATS_TOTAL_CPU       (1 << 1)
#define DMUS_SYNTHSTATS_CPU_PER_VOICE   (1 << 2)
#define DMUS_SYNTHSTATS_LOST_NOTES      (1 << 3)
#define DMUS_SYNTHSTATS_PEAK_VOLUME     (1 << 4)
#define DMUS_SYNTHSTATS_FREE_MEMORY     (1 << 5)

#define DMUS_SYNTHSTATS_SYSTEMMEMORY    DMUS_PC_SYSTEMMEMORY

typedef struct _DMUS_WAVES_REVERB_PARAMS
{
    float   fInGain;        /* Input gain in dB (to avoid output overflows) */
    float   fReverbMix;     /* Reverb mix in dB. 0dB means 100% wet reverb (no direct signal)
                            Negative values gives less wet signal.
                            The coeficients are calculated so that the overall output level stays 
                            (approximately) constant regardless of the ammount of reverb mix. */
    float   fReverbTime;    /* The reverb decay time, in milliseconds. */
    float   fHighFreqRTRatio; /* The ratio of the high frequencies to the global reverb time. 
                            Unless very 'splashy-bright' reverbs are wanted, this should be set to 
                            a value < 1.0.
                            For example if dRevTime==1000ms and dHighFreqRTRatio=0.1 than the 
                            decay time for high frequencies will be 100ms.*/

} DMUS_WAVES_REVERB_PARAMS;

/*  Note: Default values for Reverb are:
    fInGain             = 0.0dB   (no change in level)
    fReverbMix          = -10.0dB   (a reasonable reverb mix)
    fReverbTime         = 1000.0ms (one second global reverb time)
    fHighFreqRTRatio    = 0.001    (the ratio of the high frequencies to the global reverb time) 
*/

typedef enum
{
    DMUS_CLOCK_SYSTEM = 0,
    DMUS_CLOCK_WAVE = 1
} DMUS_CLOCKTYPE;

typedef struct _DMUS_CLOCKINFO *LPDMUS_CLOCKINFO;
typedef struct _DMUS_CLOCKINFO
{
    DWORD           dwSize;
    DMUS_CLOCKTYPE  ctType;
    GUID            guidClock;          /* Identifies this time source */
    WCHAR           wszDescription[DMUS_MAX_DESCRIPTION];
} DMUS_CLOCKINFO;

interface IDirectMusic;
interface IDirectMusicBuffer;
interface IDirectMusicPort;
interface IDirectMusicThru;
interface IReferenceClock;

#ifndef __cplusplus 
typedef interface IDirectMusic IDirectMusic;
typedef interface IDirectMusicBuffer IDirectMusicBuffer;
typedef interface IDirectMusicPort IDirectMusicPort;
typedef interface IDirectMusicThru IDirectMusicThru;
typedef interface IReferenceClock IReferenceClock;
#endif

typedef IDirectMusic *LPDIRECTMUSIC;
typedef IDirectMusicBuffer *LPDIRECTMUSICBUFFER;
typedef IDirectMusicPort *LPDIRECTMUSICPORT;

#undef  INTERFACE
#define INTERFACE  IDirectMusic
DECLARE_INTERFACE_(IDirectMusic, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusic */
    STDMETHOD(EnumPort)             (THIS_ DWORD dwIndex, 
                                           LPDMUS_PORTCAPS pPortCaps) PURE;
    STDMETHOD(CreateMusicBuffer)    (THIS_ LPDMUS_BUFFERDESC pBufferDesc, 
                                           LPDIRECTMUSICBUFFER *ppBuffer, 
                                           LPUNKNOWN pUnkOuter) PURE;
    STDMETHOD(CreatePort)           (THIS_ REFCLSID rclsidPort, 
                                           LPDMUS_PORTPARAMS pPortParams, 
                                           LPDIRECTMUSICPORT *ppPort, 
                                           LPUNKNOWN pUnkOuter) PURE;
    STDMETHOD(EnumMasterClock)      (THIS_ DWORD dwIndex, 
                                           LPDMUS_CLOCKINFO lpClockInfo) PURE;
    STDMETHOD(GetMasterClock)       (THIS_ LPGUID pguidClock, 
                                           IReferenceClock **ppReferenceClock) PURE;
    STDMETHOD(SetMasterClock)       (THIS_ REFGUID rguidClock) PURE;
    STDMETHOD(Activate)             (THIS_ BOOL fEnable) PURE;
    STDMETHOD(GetDefaultPort)       (THIS_ LPGUID pguidPort) PURE;
    STDMETHOD(SetDirectSound)       (THIS_ LPDIRECTSOUND pDirectSound,
                                           HWND hWnd) PURE;
    
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicBuffer
DECLARE_INTERFACE_(IDirectMusicBuffer, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusicBuffer */
    STDMETHOD(Flush)                (THIS) PURE;
    STDMETHOD(TotalTime)            (THIS_ LPREFERENCE_TIME prtTime) PURE;
    
    STDMETHOD(PackStructured)       (THIS_ REFERENCE_TIME rt,
                                           DWORD dwChannelGroup,
                                           DWORD dwChannelMessage) PURE;
    
    STDMETHOD(PackUnstructured)     (THIS_ REFERENCE_TIME rt,
                                           DWORD dwChannelGroup,
                                           DWORD cb,
                                           LPBYTE lpb) PURE;
    
    STDMETHOD(ResetReadPtr)         (THIS) PURE;
    STDMETHOD(GetNextEvent)         (THIS_ LPREFERENCE_TIME prt,
                                           LPDWORD pdwChannelGroup,
                                           LPDWORD pdwLength,
                                           LPBYTE *ppData) PURE;

    STDMETHOD(GetRawBufferPtr)      (THIS_ LPBYTE *ppData) PURE;
    STDMETHOD(GetStartTime)         (THIS_ LPREFERENCE_TIME prt) PURE;
    STDMETHOD(GetUsedBytes)         (THIS_ LPDWORD pcb) PURE;
    STDMETHOD(GetMaxBytes)          (THIS_ LPDWORD pcb) PURE;
    STDMETHOD(GetBufferFormat)      (THIS_ LPGUID pGuidFormat) PURE;

    STDMETHOD(SetStartTime)         (THIS_ REFERENCE_TIME rt) PURE;
    STDMETHOD(SetUsedBytes)         (THIS_ DWORD cb) PURE;
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicInstrument
DECLARE_INTERFACE_(IDirectMusicInstrument, IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)           (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)            (THIS) PURE;
    STDMETHOD_(ULONG,Release)           (THIS) PURE;

    /* IDirectMusicInstrument */
    STDMETHOD(GetPatch)                 (THIS_ DWORD* pdwPatch) PURE;
    STDMETHOD(SetPatch)                 (THIS_ DWORD dwPatch) PURE;
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicDownloadedInstrument
DECLARE_INTERFACE_(IDirectMusicDownloadedInstrument, IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)           (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)            (THIS) PURE;
    STDMETHOD_(ULONG,Release)           (THIS) PURE;

    /* IDirectMusicDownloadedInstrument */
    /* None at this time */
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicCollection
DECLARE_INTERFACE_(IDirectMusicCollection, IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)           (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)            (THIS) PURE;
    STDMETHOD_(ULONG,Release)           (THIS) PURE;

    /* IDirectMusicCollection */
    STDMETHOD(GetInstrument)            (THIS_ DWORD dwPatch, 
                                               IDirectMusicInstrument** ppInstrument) PURE;
    STDMETHOD(EnumInstrument)           (THIS_ DWORD dwIndex, 
                                               DWORD* pdwPatch, 
                                               LPWSTR pwszName, 
                                               DWORD dwNameLen) PURE;
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicDownload 
DECLARE_INTERFACE_(IDirectMusicDownload , IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /* IDirectMusicDownload */
    STDMETHOD(GetBuffer)            (THIS_ void** ppvBuffer, 
                                           DWORD* pdwSize) PURE;
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicPortDownload
DECLARE_INTERFACE_(IDirectMusicPortDownload, IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /* IDirectMusicPortDownload */
    STDMETHOD(GetBuffer)            (THIS_ DWORD dwDLId, 
                                           IDirectMusicDownload** ppIDMDownload) PURE;
    STDMETHOD(AllocateBuffer)       (THIS_ DWORD dwSize, 
                                           IDirectMusicDownload** ppIDMDownload) PURE;
    STDMETHOD(GetDLId)              (THIS_ DWORD* pdwStartDLId, 
                                           DWORD dwCount) PURE;
    STDMETHOD(GetAppend)            (THIS_ DWORD* pdwAppend) PURE;
    STDMETHOD(Download)             (THIS_ IDirectMusicDownload* pIDMDownload) PURE;
    STDMETHOD(Unload)               (THIS_ IDirectMusicDownload* pIDMDownload) PURE;
};

/* Standard values for voice priorities. Numerically higher priorities are higher in priority.
 * These priorities are used to set the voice priority for all voices on a channel. They are
 * used in the dwPriority parameter of IDirectMusicPort::GetPriority and returned in the
 * lpwPriority parameter of pdwPriority.
 *
 * These priorities are shared with DirectSound.
 */

#ifndef _DIRECTAUDIO_PRIORITIES_DEFINED_
#define _DIRECTAUDIO_PRIORITIES_DEFINED_

#define DAUD_CRITICAL_VOICE_PRIORITY    (0xF0000000)
#define DAUD_HIGH_VOICE_PRIORITY        (0xC0000000)
#define DAUD_STANDARD_VOICE_PRIORITY    (0x80000000)
#define DAUD_LOW_VOICE_PRIORITY         (0x40000000)
#define DAUD_PERSIST_VOICE_PRIORITY     (0x10000000) 

/* These are the default priorities assigned if not overridden. By default priorities are
 * equal across channel groups (e.g. channel 5 on channel group 1 has the same priority as
 * channel 5 on channel group 2).
 *
 * In accordance with DLS level 1, channel 10 has the highest priority, followed by 1 through 16
 * except for 10.
 */
#define DAUD_CHAN1_VOICE_PRIORITY_OFFSET    (0x0000000E)
#define DAUD_CHAN2_VOICE_PRIORITY_OFFSET    (0x0000000D)
#define DAUD_CHAN3_VOICE_PRIORITY_OFFSET    (0x0000000C)
#define DAUD_CHAN4_VOICE_PRIORITY_OFFSET    (0x0000000B)
#define DAUD_CHAN5_VOICE_PRIORITY_OFFSET    (0x0000000A)
#define DAUD_CHAN6_VOICE_PRIORITY_OFFSET    (0x00000009)
#define DAUD_CHAN7_VOICE_PRIORITY_OFFSET    (0x00000008)
#define DAUD_CHAN8_VOICE_PRIORITY_OFFSET    (0x00000007)
#define DAUD_CHAN9_VOICE_PRIORITY_OFFSET    (0x00000006)
#define DAUD_CHAN10_VOICE_PRIORITY_OFFSET   (0x0000000F)
#define DAUD_CHAN11_VOICE_PRIORITY_OFFSET   (0x00000005)
#define DAUD_CHAN12_VOICE_PRIORITY_OFFSET   (0x00000004)
#define DAUD_CHAN13_VOICE_PRIORITY_OFFSET   (0x00000003)
#define DAUD_CHAN14_VOICE_PRIORITY_OFFSET   (0x00000002)
#define DAUD_CHAN15_VOICE_PRIORITY_OFFSET   (0x00000001)
#define DAUD_CHAN16_VOICE_PRIORITY_OFFSET   (0x00000000)
 
 
#define DAUD_CHAN1_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN1_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN2_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN2_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN3_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN3_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN4_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN4_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN5_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN5_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN6_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN6_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN7_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN7_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN8_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN8_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN9_DEF_VOICE_PRIORITY   (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN9_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN10_DEF_VOICE_PRIORITY  (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN10_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN11_DEF_VOICE_PRIORITY  (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN11_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN12_DEF_VOICE_PRIORITY  (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN12_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN13_DEF_VOICE_PRIORITY  (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN13_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN14_DEF_VOICE_PRIORITY  (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN14_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN15_DEF_VOICE_PRIORITY  (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN15_VOICE_PRIORITY_OFFSET)
#define DAUD_CHAN16_DEF_VOICE_PRIORITY  (DAUD_STANDARD_VOICE_PRIORITY | DAUD_CHAN16_VOICE_PRIORITY_OFFSET)

#endif  /* _DIRECTAUDIO_PRIORITIES_DEFINED_ */


#undef  INTERFACE
#define INTERFACE  IDirectMusicPort
DECLARE_INTERFACE_(IDirectMusicPort, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusicPort */
    /*  */
    STDMETHOD(PlayBuffer)           (THIS_ LPDIRECTMUSICBUFFER pBuffer) PURE;
    STDMETHOD(SetReadNotificationHandle) (THIS_ HANDLE hEvent) PURE;
    STDMETHOD(Read)                 (THIS_ LPDIRECTMUSICBUFFER pBuffer) PURE;
    STDMETHOD(DownloadInstrument)   (THIS_ IDirectMusicInstrument *pInstrument, 
                                     IDirectMusicDownloadedInstrument **ppDownloadedInstrument,
                                     DMUS_NOTERANGE *pNoteRanges,
                                     DWORD dwNumNoteRanges) PURE;
    STDMETHOD(UnloadInstrument)     (THIS_ IDirectMusicDownloadedInstrument *pDownloadedInstrument) PURE;
    STDMETHOD(GetLatencyClock)      (THIS_ IReferenceClock **ppClock) PURE;
    STDMETHOD(GetRunningStats)      (THIS_ LPDMUS_SYNTHSTATS pStats) PURE;
    STDMETHOD(Compact)              (THIS) PURE;
    STDMETHOD(GetCaps)              (THIS_ LPDMUS_PORTCAPS pPortCaps) PURE;
    STDMETHOD(DeviceIoControl)      (THIS_ DWORD dwIoControlCode, 
                                           LPVOID lpInBuffer, 
                                           DWORD nInBufferSize, 
                                           LPVOID lpOutBuffer, 
                                           DWORD nOutBufferSize, 
                                           LPDWORD lpBytesReturned, 
                                           LPOVERLAPPED lpOverlapped) PURE;
    STDMETHOD(SetNumChannelGroups)  (THIS_ DWORD dwChannelGroups) PURE;
    STDMETHOD(GetNumChannelGroups)  (THIS_ LPDWORD pdwChannelGroups) PURE;
    STDMETHOD(Activate)             (THIS_ BOOL fActive) PURE;
    STDMETHOD(SetChannelPriority)   (THIS_ DWORD dwChannelGroup, DWORD dwChannel, DWORD dwPriority) PURE;
    STDMETHOD(GetChannelPriority)   (THIS_ DWORD dwChannelGroup, DWORD dwChannel, LPDWORD pdwPriority) PURE;
    STDMETHOD(SetDirectSound)       (THIS_ LPDIRECTSOUND pDirectSound, LPDIRECTSOUNDBUFFER pDirectSoundBuffer) PURE;
    STDMETHOD(GetFormat)            (THIS_ LPWAVEFORMATEX pWaveFormatEx, LPDWORD pdwWaveFormatExSize, LPDWORD pdwBufferSize) PURE;
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicThru
DECLARE_INTERFACE_(IDirectMusicThru, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;
    
    /* IDirectMusicThru 
     */
    STDMETHOD(ThruChannel)          (THIS_ DWORD dwSourceChannelGroup, 
                                           DWORD dwSourceChannel, 
                                           DWORD dwDestinationChannelGroup,
                                           DWORD dwDestinationChannel,
                                           LPDIRECTMUSICPORT pDestinationPort) PURE;
};

#ifndef __IReferenceClock_INTERFACE_DEFINED__
#define __IReferenceClock_INTERFACE_DEFINED__

DEFINE_GUID(IID_IReferenceClock,0x56a86897,0x0ad4,0x11ce,0xb0,0x3a,0x00,0x20,0xaf,0x0b,0xa7,0x70);

#undef  INTERFACE
#define INTERFACE  IReferenceClock
DECLARE_INTERFACE_(IReferenceClock, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)           (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)            (THIS) PURE;
    STDMETHOD_(ULONG,Release)           (THIS) PURE;

    /*  IReferenceClock */
    /*  */
    
    /*  get the time now */
    STDMETHOD(GetTime)                  (THIS_ REFERENCE_TIME *pTime) PURE;

    /*  ask for an async notification that a time has elapsed */
    STDMETHOD(AdviseTime)               (THIS_ REFERENCE_TIME baseTime,         /*  base time */
                                               REFERENCE_TIME streamTime,       /*  stream offset time */
                                               HANDLE hEvent,                   /*  advise via this event */
                                               DWORD * pdwAdviseCookie) PURE;   /*  where your cookie goes */

    /*  ask for an async periodic notification that a time has elapsed */
    STDMETHOD(AdvisePeriodic)           (THIS_ REFERENCE_TIME startTime,        /*  starting at this time */
                                               REFERENCE_TIME periodTime,       /*  time between notifications */
                                               HANDLE hSemaphore,               /*  advise via a semaphore */
                                               DWORD * pdwAdviseCookie) PURE;   /*  where your cookie goes */

    /*  cancel a request for notification */
    STDMETHOD(Unadvise)                 (THIS_ DWORD dwAdviseCookie) PURE;
};

#endif /* __IReferenceClock_INTERFACE_DEFINED__ */

DEFINE_GUID(CLSID_DirectMusic,0x636b9f10,0x0c7d,0x11d1,0x95,0xb2,0x00,0x20,0xaf,0xdc,0x74,0x21);
DEFINE_GUID(CLSID_DirectMusicCollection,0x480ff4b0, 0x28b2, 0x11d1, 0xbe, 0xf7, 0x0, 0xc0, 0x4f, 0xbf, 0x8f, 0xef);
DEFINE_GUID(CLSID_DirectMusicSynth,0x58C2B4D0,0x46E7,0x11D1,0x89,0xAC,0x00,0xA0,0xC9,0x05,0x41,0x29);

DEFINE_GUID(IID_IDirectMusic,0x6536115a,0x7b2d,0x11d2,0xba,0x18,0x00,0x00,0xf8,0x75,0xac,0x12);
DEFINE_GUID(IID_IDirectMusicBuffer,0xd2ac2878, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicPort, 0x08f2d8c9,0x37c2,0x11d2,0xb9,0xf9,0x00,0x00,0xf8,0x75,0xac,0x12);
DEFINE_GUID(IID_IDirectMusicThru, 0xced153e7, 0x3606, 0x11d2, 0xb9, 0xf9, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);
DEFINE_GUID(IID_IDirectMusicPortDownload,0xd2ac287a, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicDownload,0xd2ac287b, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicCollection,0xd2ac287c, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicInstrument,0xd2ac287d, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicDownloadedInstrument,0xd2ac287e, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Alternate interface ID for IID_IDirectMusic, available in DX7 release and after. */
DEFINE_GUID(IID_IDirectMusic2,0x6fc2cae1, 0xbc78, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);


/* Property Query GUID_DMUS_PROP_GM_Hardware - Local GM set, no need to download
 * Property Query GUID_DMUS_PROP_GS_Hardware - Local GS set, no need to download
 * Property Query GUID_DMUS_PROP_XG_Hardware - Local XG set, no need to download
 * Property Query GUID_DMUS_PROP_DLS1        - Support DLS level 1
 * Property Query GUID_DMUS_PROP_INSTRUMENT2 - Support new INSTRUMENT2 download format
 * Property Query GUID_DMUS_PROP_XG_Capable  - Support minimum requirements of XG
 * Property Query GUID_DMUS_PROP_GS_Capable  - Support minimum requirements of GS
 * Property Query GUID_DMUS_PROP_SynthSink_DSOUND - Synthsink talks to DSound
 * Property Query GUID_DMUS_PROP_SynthSink_WAVE - Synthsink talks to Wave device
 *
 * Item 0: Supported
 * Returns a DWORD which is non-zero if the feature is supported
 */
DEFINE_GUID(GUID_DMUS_PROP_GM_Hardware, 0x178f2f24, 0xc364, 0x11d1, 0xa7, 0x60, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);
DEFINE_GUID(GUID_DMUS_PROP_GS_Hardware, 0x178f2f25, 0xc364, 0x11d1, 0xa7, 0x60, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);
DEFINE_GUID(GUID_DMUS_PROP_XG_Hardware, 0x178f2f26, 0xc364, 0x11d1, 0xa7, 0x60, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);
DEFINE_GUID(GUID_DMUS_PROP_XG_Capable,  0x6496aba1, 0x61b0, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);
DEFINE_GUID(GUID_DMUS_PROP_GS_Capable,  0x6496aba2, 0x61b0, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);
DEFINE_GUID(GUID_DMUS_PROP_DLS1,        0x178f2f27, 0xc364, 0x11d1, 0xa7, 0x60, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);
DEFINE_GUID(GUID_DMUS_PROP_DLS2,        0xf14599e5, 0x4689, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);
DEFINE_GUID(GUID_DMUS_PROP_INSTRUMENT2, 0x865fd372, 0x9f67, 0x11d2, 0x87, 0x2a, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_DMUS_PROP_SynthSink_DSOUND,0xaa97844, 0xc877, 0x11d1, 0x87, 0xc, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_DMUS_PROP_SynthSink_WAVE,0xaa97845, 0xc877, 0x11d1, 0x87, 0xc, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_DMUS_PROP_SampleMemorySize, 0x178f2f28, 0xc364, 0x11d1, 0xa7, 0x60, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);
DEFINE_GUID(GUID_DMUS_PROP_SamplePlaybackRate, 0x2a91f713, 0xa4bf, 0x11d2, 0xbb, 0xdf, 0x0, 0x60, 0x8, 0x33, 0xdb, 0xd8);

/* Property Get/Set GUID_DMUS_PROP_WriteLatency
 *
 * Item 0: Synth buffer write latency, in milliseconds
 * Get/Set SynthSink latency, the average time after the play head that the next buffer gets written.
 */
DEFINE_GUID(GUID_DMUS_PROP_WriteLatency,0x268a0fa0, 0x60f2, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);

/* Property Get/Set GUID_DMUS_PROP_WritePeriod
 *
 * Item 0: Synth buffer write period, in milliseconds
 * Get/Set SynthSink buffer write period, time span between successive writes.
 */
DEFINE_GUID(GUID_DMUS_PROP_WritePeriod,0x268a0fa1, 0x60f2, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);

/* Property Get GUID_DMUS_PROP_MemorySize
 *
 * Item 0: Memory size
 * Returns a DWORD containing the total number of bytes of sample RAM
 */
DEFINE_GUID(GUID_DMUS_PROP_MemorySize,  0x178f2f28, 0xc364, 0x11d1, 0xa7, 0x60, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);

/* Property Set GUID_DMUS_PROP_WavesReverb
 *
 * Item 0: DMUS_WAVES_REVERB structure
 * Sets reverb parameters
 */
DEFINE_GUID(GUID_DMUS_PROP_WavesReverb,0x4cb5622, 0x32e5, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);

/* Property Set GUID_DMUS_PROP_Effects
 *
 * Item 0: DWORD with effects flags. 
 * Get/Set effects bits, same as dwEffectFlags in DMUS_PORTPARAMS and DMUS_PORTCAPS:
 * DMUS_EFFECT_NONE 
 * DMUS_EFFECT_REVERB 
 * DMUS_EFFECT_CHORUS 
 */
DEFINE_GUID(GUID_DMUS_PROP_Effects, 0xcda8d611, 0x684a, 0x11d2, 0x87, 0x1e, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Property Set GUID_DMUS_PROP_LegacyCaps
 *
 * Item 0: The MIDINCAPS or MIDIOUTCAPS which describes the port's underlying WinMM device. This property is only supported
 * by ports which wrap WinMM devices.
 */

DEFINE_GUID(GUID_DMUS_PROP_LegacyCaps,0xcfa7cdc2, 0x00a1, 0x11d2, 0xaa, 0xd5, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);

/* Property Set GUID_DMUS_PROP_Volume
 *
 * Item 0: A long which contains an offset, in 1/100 dB, to be added to the final volume
 *
 */
DEFINE_GUID(GUID_DMUS_PROP_Volume, 0xfedfae25L, 0xe46e, 0x11d1, 0xaa, 0xce, 0x00, 0x00, 0xf8, 0x75, 0xac, 0x12);

/* Min and Max values for setting volume with GUID_DMUS_PROP_Volume */

#define DMUS_VOLUME_MAX     2000        /* +20 dB */
#define DMUS_VOLUME_MIN   -20000        /* -200 dB */

#ifdef __cplusplus
}; /* extern "C" */
#endif

#include <poppack.h>

#endif /* #ifndef _DMUSICC_ */
