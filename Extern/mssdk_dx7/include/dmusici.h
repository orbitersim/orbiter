/************************************************************************
*                                                                       *
*   dmusici.h -- This module contains the API for the                   *
*                DirectMusic performance layer                          *
*                                                                       *
*   Copyright (c) 1998, Microsoft Corp. All rights reserved.            *
*                                                                       *
************************************************************************/

#ifndef _DMUSICI_
#define _DMUSICI_

#include <windows.h>

#define COM_NO_WINDOWS_H
#include <objbase.h>

#include <mmsystem.h>
#include "dmusicc.h"

#include <pshpack8.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef WORD            TRANSITION_TYPE;
typedef __int64         REFERENCE_TIME;
typedef long            MUSIC_TIME;

#define DMUS_PPQ        768     /* parts per quarter note */

interface IDirectMusic;
interface IDirectMusicTrack;
interface IDirectMusicPerformance;
interface IDirectMusicTool;
interface IDirectMusicSegment;
interface IDirectMusicSegmentState;
interface IDirectMusicGraph;
interface IDirectMusicPort;
interface IDirectMusicBuffer;
interface IDirectMusicInstrument;
interface IDirectMusicDownloadedInstrument;
interface IDirectMusicBand;
interface IDirectMusicChordMap;
interface IDirectMusicLoader;
interface IDirectMusicObject;
#ifndef __cplusplus 
typedef interface IDirectMusic IDirectMusic;
typedef interface IDirectMusicTrack IDirectMusicTrack;
typedef interface IDirectMusicPerformance IDirectMusicPerformance;
typedef interface IDirectMusicTool IDirectMusicTool;
typedef interface IDirectMusicSegment IDirectMusicSegment;
typedef interface IDirectMusicSegmentState IDirectMusicSegmentState;
typedef interface IDirectMusicGraph IDirectMusicGraph;
typedef interface IDirectMusicPort IDirectMusicPort;
typedef interface IDirectMusicBuffer IDirectMusicBuffer;
typedef interface IDirectMusicInstrument IDirectMusicInstrument;
typedef interface IDirectMusicDownloadedInstrument IDirectMusicDownloadedInstrument;
typedef interface IDirectMusicBand IDirectMusicBand;
typedef interface IDirectMusicChordMap IDirectMusicChordMap;
typedef interface IDirectMusicObject IDirectMusicObject;
typedef interface IDirectMusicLoader IDirectMusicLoader;
#endif

typedef enum enumDMUS_COMMANDT_TYPES
{
    DMUS_COMMANDT_GROOVE            = 0,
    DMUS_COMMANDT_FILL              = 1,
    DMUS_COMMANDT_INTRO             = 2,
    DMUS_COMMANDT_BREAK             = 3,
    DMUS_COMMANDT_END               = 4,
    DMUS_COMMANDT_ENDANDINTRO       = 5
} DMUS_COMMANDT_TYPES;

typedef enum enumDMUS_SHAPET_TYPES
{
    DMUS_SHAPET_FALLING             = 0,
    DMUS_SHAPET_LEVEL               = 1,
    DMUS_SHAPET_LOOPABLE            = 2,
    DMUS_SHAPET_LOUD                = 3,
    DMUS_SHAPET_QUIET               = 4,
    DMUS_SHAPET_PEAKING             = 5,
    DMUS_SHAPET_RANDOM              = 6,
    DMUS_SHAPET_RISING              = 7,
    DMUS_SHAPET_SONG                = 8
}   DMUS_SHAPET_TYPES;

typedef enum enumDMUS_COMPOSEF_FLAGS
{       
    DMUS_COMPOSEF_NONE              = 0,
    DMUS_COMPOSEF_ALIGN             = 0x1,
    DMUS_COMPOSEF_OVERLAP           = 0x2,
    DMUS_COMPOSEF_IMMEDIATE         = 0x4,
    DMUS_COMPOSEF_GRID              = 0x8,
    DMUS_COMPOSEF_BEAT              = 0x10,
    DMUS_COMPOSEF_MEASURE           = 0x20,
    DMUS_COMPOSEF_AFTERPREPARETIME  = 0x40,
    DMUS_COMPOSEF_MODULATE          = 0x1000,
    DMUS_COMPOSEF_LONG              = 0x2000
}   DMUS_COMPOSEF_FLAGS;

#define DMUS_PMSG_PART                                                                              \
    DWORD               dwSize;                                                                     \
    REFERENCE_TIME      rtTime;             /* real time (in 100 nanosecond increments) */          \
    MUSIC_TIME          mtTime;             /* music time */                                        \
    DWORD               dwFlags;            /* various bits (see DMUS_PMSG_FLAGS enumeration) */    \
    DWORD               dwPChannel;         /* Performance Channel. The Performance can */          \
                                            /* use this to determine the port/channel. */           \
    DWORD               dwVirtualTrackID;   /* virtual track ID */                                  \
    IDirectMusicTool*   pTool;              /* tool interface pointer */                            \
    IDirectMusicGraph*  pGraph;             /* tool graph interface pointer */                      \
    DWORD               dwType;             /* PMSG type (see DMUS_PMSGT_TYPES defines) */              \
    DWORD               dwVoiceID;          /* unique voice id which allows synthesizers to */      \
                                            /* identify a specific event. For DirectX 6.0, */       \
                                            /* this field should always be 0. */                    \
    DWORD               dwGroupID;          /* Track group id */                                 \
    IUnknown*           punkUser;           /* user com pointer, auto released upon PMSG free */

/* every DMUS_PMSG is based off of this structure. The Performance needs 
   to access these members consistently in every PMSG that goes through it. */
typedef struct _DMUS_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

} DMUS_PMSG;

/* DMUS_PMSGF_FLAGS fill the DMUS_PMSG's dwFlags member */
typedef enum enumDMUS_PMSGF_FLAGS
{
    DMUS_PMSGF_REFTIME          = 1,      /* if rtTime is valid */
    DMUS_PMSGF_MUSICTIME        = 2,      /* if mtTime is valid */
    DMUS_PMSGF_TOOL_IMMEDIATE   = 4,      /* if PMSG should be processed immediately */ 
    DMUS_PMSGF_TOOL_QUEUE       = 8,      /* if PMSG should be processed a little early, at Queue time */
    DMUS_PMSGF_TOOL_ATTIME      = 16,     /* if PMSG should be processed at the time stamp */
    DMUS_PMSGF_TOOL_FLUSH       = 32      /* if PMSG is being flushed */
    /* The values of DMUS_TIME_RESOLVE_FLAGS may also be used inside the */
    /* DMUS_PMSG's dwFlags member. */
} DMUS_PMSGF_FLAGS;

/* DMUS_PMSGT_TYPES fill the DMUS_PMSG's dwType member */
typedef enum enumDMUS_PMSGT_TYPES
{
    DMUS_PMSGT_MIDI             = 0,      /* MIDI short message */
    DMUS_PMSGT_NOTE             = 1,      /* Interactive Music Note */
    DMUS_PMSGT_SYSEX            = 2,      /* MIDI long message (system exclusive message) */
    DMUS_PMSGT_NOTIFICATION     = 3,      /* Notification message */
    DMUS_PMSGT_TEMPO            = 4,      /* Tempo message */
    DMUS_PMSGT_CURVE            = 5,      /* Control change / pitch bend, etc. curve */
    DMUS_PMSGT_TIMESIG          = 6,      /* Time signature */
    DMUS_PMSGT_PATCH            = 7,      /* Patch changes */
    DMUS_PMSGT_TRANSPOSE        = 8,      /* Transposition messages */
    DMUS_PMSGT_CHANNEL_PRIORITY = 9,      /* Channel priority */
    DMUS_PMSGT_STOP             = 10,     /* Stop message */
    DMUS_PMSGT_DIRTY            = 11,     /* Tells Tools that cache GetParam() info to refresh */
    DMUS_PMSGT_USER             = 255     /* User message */
} DMUS_PMSGT_TYPES;

/* DMUS_SEGF_FLAGS correspond to IDirectMusicPerformance::PlaySegment, and other API */
typedef enum enumDMUS_SEGF_FLAGS
{
    DMUS_SEGF_REFTIME           = 64,     /* time parameter is in reference time  */
    DMUS_SEGF_SECONDARY         = 128,    /* secondary segment */
    DMUS_SEGF_QUEUE             = 256,    /* queue at the end of the primary segment queue (primary only) */
    DMUS_SEGF_CONTROL           = 512,    /* play as a control track (secondary segments only) */
    DMUS_SEGF_AFTERPREPARETIME  = 1<<10,  /* play after the prepare time (See IDirectMusicPerformance::GetPrepareTime) */
    DMUS_SEGF_GRID              = 1<<11,  /* play on grid boundary */
    DMUS_SEGF_BEAT              = 1<<12,  /* play on beat boundary */
    DMUS_SEGF_MEASURE           = 1<<13,  /* play on measure boundary */
    DMUS_SEGF_DEFAULT           = 1<<14,  /* use segment's default boundary */
    DMUS_SEGF_NOINVALIDATE      = 1<<15   /* play without invalidating the currently playing segment(s) */
} DMUS_SEGF_FLAGS;

/* DMUS_TIME_RESOLVE_FLAGS correspond to IDirectMusicPerformance::GetResolvedTime, and can */
/* also be used interchangeably with the corresponding DMUS_SEGF_FLAGS, since their values */
/* are intentionally the same */
typedef enum enumDMUS_TIME_RESOLVE_FLAGS
{
    DMUS_TIME_RESOLVE_AFTERPREPARETIME  = 1<<10,  /* resolve to a time after the prepare time */
    DMUS_TIME_RESOLVE_GRID              = 1<<11,  /* resolve to a time on a grid boundary */
    DMUS_TIME_RESOLVE_BEAT              = 1<<12,  /* resolve to a time on a beat boundary */
    DMUS_TIME_RESOLVE_MEASURE           = 1<<13   /* resolve to a time on a measure boundary */
} DMUS_TIME_RESOLVE_FLAGS;

/* The following flags are sent in the IDirectMusicTrack::Play() method */
/* inside the dwFlags parameter */
typedef enum enumDMUS_TRACKF_FLAGS
{
    DMUS_TRACKF_SEEK            = 1,      /* set on a seek */
    DMUS_TRACKF_LOOP            = 2,      /* set on a loop (repeat) */
    DMUS_TRACKF_START           = 4,      /* set on first call to Play */
    DMUS_TRACKF_FLUSH           = 8,      /* set when this call is in response to a flush on the perfomance */
    DMUS_TRACKF_DIRTY           = 16,     /* set when the track should consider any cached values from a previous call to GetParam to be invalidated */
} DMUS_TRACKF_FLAGS;

#define DMUS_MAXSUBCHORD 8

typedef struct _DMUS_SUBCHORD
{
    DWORD   dwChordPattern;     /* Notes in the subchord */
    DWORD   dwScalePattern;     /* Notes in the scale */
    DWORD   dwInversionPoints;  /* Where inversions can occur */
    DWORD   dwLevels;           /* Which levels are supported by this subchord */
    BYTE    bChordRoot;         /* Root of the subchord */
    BYTE    bScaleRoot;         /* Root of the scale */
} DMUS_SUBCHORD;

typedef struct _DMUS_CHORD_KEY
{
    WCHAR           wszName[16];        /* Name of the chord */
    WORD            wMeasure;           /* Measure this falls on */
    BYTE            bBeat;              /* Beat this falls on */
    BYTE            bSubChordCount;     /* Number of chords in the list of subchords */
    DMUS_SUBCHORD   SubChordList[DMUS_MAXSUBCHORD]; /* List of sub chords */
    DWORD           dwScale;            /* Scale underlying the entire chord */
    BYTE            bKey;               /* Key underlying the entire chord */
} DMUS_CHORD_KEY;

/* DMUS_NOTE_PMSG */
typedef struct _DMUS_NOTE_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    MUSIC_TIME mtDuration;     /* duration */
    WORD    wMusicValue;       /* Description of note in chord and key. */
    WORD    wMeasure;          /* Measure in which this note occurs */
    short   nOffset;           /* Offset from grid at which this note occurs */
    BYTE    bBeat;             /* Beat (in measure) at which this note occurs */
    BYTE    bGrid;             /* Grid offset from beat at which this note occurs */
    BYTE    bVelocity;         /* Note velocity */
    BYTE    bFlags;            /* see DMUS_NOTE_FLAGS */
    BYTE    bTimeRange;        /* Range to randomize time. */
    BYTE    bDurRange;         /* Range to randomize duration. */
    BYTE    bVelRange;         /* Range to randomize velocity. */
    BYTE    bPlayModeFlags;    /* Play mode */
    BYTE    bSubChordLevel;    /* Which subchord level this note uses.  */
    BYTE    bMidiValue;        /* The MIDI note value, converted from wMusicValue */
    char    cTranspose;        /* Transposition to add to midi note value after converted from wMusicValue. */
} DMUS_NOTE_PMSG;

typedef enum enumDMUS_NOTEF_FLAGS
{
    DMUS_NOTEF_NOTEON = 1,     /* Set if this is a MIDI Note On. Otherwise, it is MIDI Note Off */
} DMUS_NOTEF_FLAGS;

/* The DMUS_PLAYMODE_FLAGS are used to determine how to convert wMusicValue
   into the appropriate bMidiValue.
*/

typedef enum enumDMUS_PLAYMODE_FLAGS
{
    DMUS_PLAYMODE_KEY_ROOT          = 1,  /* Transpose on top of the key root. */
    DMUS_PLAYMODE_CHORD_ROOT        = 2,  /* Transpose on top of the chord root. */
    DMUS_PLAYMODE_SCALE_INTERVALS   = 4,  /* Use scale intervals from scale pattern. */
    DMUS_PLAYMODE_CHORD_INTERVALS   = 8,  /* Use chord intervals from chord pattern. */
    DMUS_PLAYMODE_NONE              = 16, /* No mode. Indicates the parent part's mode should be used. */
} DMUS_PLAYMODE_FLAGS;

/* The following are playback modes that can be created by combining the DMUS_PLAYMODE_FLAGS
   in various ways:
*/

/* Fixed. wMusicValue holds final MIDI note value. This is used for drums, sound effects, and sequenced
   notes that should not be transposed by the chord or scale.
*/
#define DMUS_PLAYMODE_FIXED             0  
/* In fixed to key, the musicvalue is again a fixed MIDI value, but it
   is transposed on top of the key root. 
*/
#define DMUS_PLAYMODE_FIXEDTOKEY        DMUS_PLAYMODE_KEY_ROOT
/* In fixed to chord, the musicvalue is also a fixed MIDI value, but it
   is transposed on top of the chord root. 
*/
#define DMUS_PLAYMODE_FIXEDTOCHORD      DMUS_PLAYMODE_CHORD_ROOT
/* In Pedalpoint, the key root is used and the notes only track the intervals in
   the scale. The chord root and intervals are completely ignored. This is useful
   for melodic lines that play relative to the key root.
*/
#define DMUS_PLAYMODE_PEDALPOINT        (DMUS_PLAYMODE_KEY_ROOT | DMUS_PLAYMODE_SCALE_INTERVALS)
/* In the Melodic mode, the chord root is used but the notes only track the intervals in
   the scale. The key root and chord intervals are completely ignored. This is useful
   for melodic lines that play relative to the chord root. 
*/
#define DMUS_PLAYMODE_MELODIC           (DMUS_PLAYMODE_CHORD_ROOT | DMUS_PLAYMODE_SCALE_INTERVALS)
/* Normal chord mode is the prevalent playback mode. 
   The notes track the intervals in the chord, which is based on the chord root. 
   If there is a scale component to the MusicValue, the additional intervals 
   are pulled from the scale and added.
   If the chord does not have an interval to match the chord component of
   the MusicValue, the note is silent.
*/
#define DMUS_PLAYMODE_NORMALCHORD       (DMUS_PLAYMODE_CHORD_ROOT | DMUS_PLAYMODE_CHORD_INTERVALS)
/* If it is desirable to play a note that is above the top of the chord, the
   always play mode (known as "purpleized" in a former life) finds a position
   for the note by using intervals from the scale. Essentially, this mode is
   a combination of the Normal and Melodic playback modes, where a failure
   in Normal causes a second try in Melodic mode.
*/
#define DMUS_PLAYMODE_ALWAYSPLAY        (DMUS_PLAYMODE_MELODIC | DMUS_PLAYMODE_NORMALCHORD)

/*  Legacy names for modes... */
#define DMUS_PLAYMODE_PURPLEIZED        DMUS_PLAYMODE_ALWAYSPLAY
#define DMUS_PLAYMODE_SCALE_ROOT        DMUS_PLAYMODE_KEY_ROOT
#define DMUS_PLAYMODE_FIXEDTOSCALE      DMUS_PLAYMODE_FIXEDTOKEY


/* DMUS_MIDI_PMSG */
typedef struct _DMUS_MIDI_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    BYTE    bStatus;
    BYTE    bByte1;
    BYTE    bByte2;
    BYTE    bPad[1];
} DMUS_MIDI_PMSG;

/* DMUS_PATCH_PMSG */
typedef struct _DMUS_PATCH_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    BYTE    byInstrument;
    BYTE    byMSB;
    BYTE    byLSB;
    BYTE    byPad[1];
} DMUS_PATCH_PMSG;

/* DMUS_TRANSPOSE_PMSG */
typedef struct _DMUS_TRANSPOSE_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    short   nTranspose;
} DMUS_TRANSPOSE_PMSG;

/* DMUS_CHANNEL_PRIORITY_PMSG */
typedef struct _DMUS_CHANNEL_PRIORITY_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    DWORD   dwChannelPriority;
} DMUS_CHANNEL_PRIORITY_PMSG;

/* DMUS_TEMPO_PMSG */
typedef struct _DMUS_TEMPO_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    double  dblTempo;                       /* the tempo */
} DMUS_TEMPO_PMSG;

#define DMUS_TEMPO_MAX          1000
#define DMUS_TEMPO_MIN          1

#define DMUS_MASTERTEMPO_MAX    100.0f
#define DMUS_MASTERTEMPO_MIN    0.01f

/* DMUS_SYSEX_PMSG */
typedef struct _DMUS_SYSEX_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    DWORD   dwLen;          /* length of the data */
    BYTE    abData[1];      /* array of data, length equal to dwLen */
} DMUS_SYSEX_PMSG;

/* DMUS_CURVE_PMSG */
typedef struct _DMUS_CURVE_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    MUSIC_TIME      mtDuration;      /* how long this curve lasts */
    MUSIC_TIME      mtOriginalStart; /* must be set to either zero when this PMSG is created or to the original mtTime of the curve */
    MUSIC_TIME      mtResetDuration; /* how long after the curve is finished to reset to the
                                        reset value, nResetValue */
    short           nStartValue;     /* curve's start value */
    short           nEndValue;       /* curve's end value */
    short           nResetValue;     /* curve's reset value, sent after mtResetDuration or
                                        upon a flush or invalidation */
    WORD            wMeasure;        /* Measure in which this curve occurs */
    short           nOffset;         /* Offset from grid at which this curve occurs */
    BYTE            bBeat;           /* Beat (in measure) at which this curve occurs */
    BYTE            bGrid;           /* Grid offset from beat at which this curve occurs */
    BYTE            bType;           /* type of curve */
    BYTE            bCurveShape;     /* shape of curve */
    BYTE            bCCData;         /* CC# if this is a control change type */
    BYTE            bFlags;          /* set to 1 if the nResetValue must be sent when the 
                                        time is reached or an invalidate occurs because
                                        of a transition. If 0, the curve stays
                                        permanently stuck at the new value. All bits besides
                                        1 are reserved. */

} DMUS_CURVE_PMSG;

typedef enum enumDMUS_CURVE_FLAGS
{
    DMUS_CURVE_RESET = 1,           /* Set if the curve needs to be reset. */
} DMUS_CURVE_FLAGS;


#define DMUS_CURVE_RESET    1        

/* Curve shapes */
enum
{ 
    DMUS_CURVES_LINEAR  = 0,
    DMUS_CURVES_INSTANT = 1,
    DMUS_CURVES_EXP     = 2,
    DMUS_CURVES_LOG     = 3,
    DMUS_CURVES_SINE    = 4
};
/* curve types */
#define DMUS_CURVET_PBCURVE      0x03
#define DMUS_CURVET_CCCURVE      0x04
#define DMUS_CURVET_MATCURVE     0x05
#define DMUS_CURVET_PATCURVE     0x06

/* DMUS_TIMESIG_PMSG */
typedef struct _DMUS_TIMESIG_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    /* Time signatures define how many beats per measure, which note receives */
    /* the beat, and the grid resolution. */
    BYTE    bBeatsPerMeasure;       /* beats per measure (top of time sig) */
    BYTE    bBeat;                  /* what note receives the beat (bottom of time sig.) */
                                    /* we can assume that 0 means 256th note */
    WORD    wGridsPerBeat;          /* grids per beat */
} DMUS_TIMESIG_PMSG;

/* notification type values */
/* The following correspond to GUID_NOTIFICATION_SEGMENT */
#define DMUS_NOTIFICATION_SEGSTART      0
#define DMUS_NOTIFICATION_SEGEND        1
#define DMUS_NOTIFICATION_SEGALMOSTEND  2
#define DMUS_NOTIFICATION_SEGLOOP       3
#define DMUS_NOTIFICATION_SEGABORT      4
/* The following correspond to GUID_NOTIFICATION_PERFORMANCE */
#define DMUS_NOTIFICATION_MUSICSTARTED  0
#define DMUS_NOTIFICATION_MUSICSTOPPED  1
/* The following corresponds to GUID_NOTIFICATION_MEASUREANDBEAT */
#define DMUS_NOTIFICATION_MEASUREBEAT   0
/* The following corresponds to GUID_NOTIFICATION_CHORD */
#define DMUS_NOTIFICATION_CHORD         0
/* The following correspond to GUID_NOTIFICATION_COMMAND */
#define DMUS_NOTIFICATION_GROOVE        0
#define DMUS_NOTIFICATION_EMBELLISHMENT 1

/* DMUS_NOTIFICATION_PMSG */
typedef struct _DMUS_NOTIFICATION_PMSG
{
    /* begin DMUS_PMSG_PART */
    DMUS_PMSG_PART
    /* end DMUS_PMSG_PART */

    GUID    guidNotificationType;
    DWORD   dwNotificationOption;
    DWORD   dwField1;
    DWORD   dwField2;
} DMUS_NOTIFICATION_PMSG;


#define DMUS_MAX_NAME           64         /* Maximum object name length. */
#define DMUS_MAX_CATEGORY       64         /* Maximum object category name length. */
#define DMUS_MAX_FILENAME       MAX_PATH
    
typedef struct _DMUS_VERSION {
  DWORD    dwVersionMS;
  DWORD    dwVersionLS;
}DMUS_VERSION, FAR *LPDMUS_VERSION;

/* Time Signature structure, used by IDirectMusicStyle */
/* Also used as a parameter for GetParam() and SetParam */
typedef struct _DMUS_TIMESIGNATURE
{
    MUSIC_TIME mtTime;
    BYTE    bBeatsPerMeasure;       /* beats per measure (top of time sig) */
    BYTE    bBeat;                  /* what note receives the beat (bottom of time sig.) */
                                    /* we can assume that 0 means 256th note */
    WORD    wGridsPerBeat;          /* grids per beat */
} DMUS_TIMESIGNATURE;

/*      The DMUSOBJECTDESC structure is used to communicate everything you could */
/*      possibly use to describe a DirectMusic object.  */

typedef struct _DMUS_OBJECTDESC
{
    DWORD          dwSize;                 /* Size of this structure. */
    DWORD          dwValidData;            /* Flags indicating which fields below are valid. */
    GUID           guidObject;             /* Unique ID for this object. */
    GUID           guidClass;              /* GUID for the class of object. */
    FILETIME       ftDate;                 /* Last edited date of object. */
    DMUS_VERSION   vVersion;               /* Version. */
    WCHAR          wszName[DMUS_MAX_NAME]; /* Name of object. */
    WCHAR          wszCategory[DMUS_MAX_CATEGORY]; /* Category for object (optional). */
    WCHAR          wszFileName[DMUS_MAX_FILENAME]; /* File path. */
    LONGLONG       llMemLength;            /* Size of Memory data. */
    LPBYTE         pbMemData;              /* Memory pointer for data. */
} DMUS_OBJECTDESC;

typedef DMUS_OBJECTDESC *LPDMUS_OBJECTDESC;

/*      Flags for dwValidData. When set, a flag indicates that the  */
/*      corresponding field in DMUSOBJECTDESC holds valid data. */

#define DMUS_OBJ_OBJECT         (1 << 0)     /* Object GUID is valid. */
#define DMUS_OBJ_CLASS          (1 << 1)     /* Class GUID is valid. */
#define DMUS_OBJ_NAME           (1 << 2)     /* Name is valid. */
#define DMUS_OBJ_CATEGORY       (1 << 3)     /* Category is valid. */
#define DMUS_OBJ_FILENAME       (1 << 4)     /* File path is valid. */
#define DMUS_OBJ_FULLPATH       (1 << 5)     /* Path is full path. */
#define DMUS_OBJ_URL            (1 << 6)     /* Path is URL. */
#define DMUS_OBJ_VERSION        (1 << 7)     /* Version is valid. */
#define DMUS_OBJ_DATE           (1 << 8)     /* Date is valid. */
#define DMUS_OBJ_LOADED         (1 << 9)     /* Object is currently loaded in memory. */
#define DMUS_OBJ_MEMORY         (1 << 10)    /* Object is pointed to by pbMemData. */

typedef IDirectMusicObject __RPC_FAR *LPDMUS_OBJECT;
typedef IDirectMusicLoader __RPC_FAR *LPDMUS_LOADER;
typedef IDirectMusicBand __RPC_FAR *LPDMUS_BAND;


#define DMUSB_LOADED    (1 << 0)        /* Set when band has been loaded */
#define DMUSB_DEFAULT   (1 << 1)        /* Set when band is default band for a style */

#undef  INTERFACE
#define INTERFACE  IDirectMusicBand
DECLARE_INTERFACE_(IDirectMusicBand, IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /* IDirectMusicBand */
    STDMETHOD(CreateSegment)        (THIS_ IDirectMusicSegment** ppSegment) PURE;
    STDMETHOD(Download)             (THIS_ IDirectMusicPerformance* pPerformance) PURE;     
    STDMETHOD(Unload)               (THIS_ IDirectMusicPerformance* pPerformance) PURE;     
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicObject
DECLARE_INTERFACE_(IDirectMusicObject, IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /* IDirectMusicObject */
    STDMETHOD(GetDescriptor)        (THIS_ LPDMUS_OBJECTDESC pDesc) PURE;
    STDMETHOD(SetDescriptor)        (THIS_ LPDMUS_OBJECTDESC pDesc) PURE;
    STDMETHOD(ParseDescriptor)      (THIS_ LPSTREAM pStream, 
                                           LPDMUS_OBJECTDESC pDesc) PURE;
};

#undef  INTERFACE
#define INTERFACE  IDirectMusicLoader
DECLARE_INTERFACE_(IDirectMusicLoader, IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /* IDirectMusicLoader */
    STDMETHOD(GetObject)            (THIS_ LPDMUS_OBJECTDESC pDesc,
                                           REFIID riid,
                                           LPVOID FAR *ppv) PURE;
    STDMETHOD(SetObject)            (THIS_ LPDMUS_OBJECTDESC pDesc) PURE;
    STDMETHOD(SetSearchDirectory)   (THIS_ REFGUID rguidClass, 
                                           WCHAR *pwzPath, 
                                           BOOL fClear) PURE;
    STDMETHOD(ScanDirectory)        (THIS_ REFGUID rguidClass, 
                                           WCHAR *pwzFileExtension, 
                                           WCHAR *pwzScanFileName) PURE;
    STDMETHOD(CacheObject)          (THIS_ IDirectMusicObject * pObject) PURE;
    STDMETHOD(ReleaseObject)        (THIS_ IDirectMusicObject * pObject) PURE;
    STDMETHOD(ClearCache)           (THIS_ REFGUID rguidClass) PURE;
    STDMETHOD(EnableCache)          (THIS_ REFGUID rguidClass, 
                                           BOOL fEnable) PURE;
    STDMETHOD(EnumObject)           (THIS_ REFGUID rguidClass, 
                                           DWORD dwIndex, 
                                           LPDMUS_OBJECTDESC pDesc) PURE;
};                                  

/*  Stream object supports IDirectMusicGetLoader interface to access loader while file parsing. */

#undef  INTERFACE
#define INTERFACE  IDirectMusicGetLoader
DECLARE_INTERFACE_(IDirectMusicGetLoader, IUnknown)
{
    /* IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /* IDirectMusicGetLoader */
    STDMETHOD(GetLoader)            (THIS_ IDirectMusicLoader ** ppLoader) PURE;
};

/*////////////////////////////////////////////////////////////////////
// IDirectMusicSegment */
#undef  INTERFACE
#define INTERFACE  IDirectMusicSegment
DECLARE_INTERFACE_(IDirectMusicSegment, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusicSegment */
    STDMETHOD(GetLength)                (THIS_ MUSIC_TIME* pmtLength) PURE;
    STDMETHOD(SetLength)                (THIS_ MUSIC_TIME mtLength) PURE;
    STDMETHOD(GetRepeats)               (THIS_ DWORD* pdwRepeats) PURE;
    STDMETHOD(SetRepeats)               (THIS_ DWORD  dwRepeats) PURE;
    STDMETHOD(GetDefaultResolution)     (THIS_ DWORD* pdwResolution) PURE;
    STDMETHOD(SetDefaultResolution)     (THIS_ DWORD  dwResolution) PURE;
    STDMETHOD(GetTrack)                 (THIS_ REFGUID rguidType, 
                                               DWORD dwGroupBits, 
                                               DWORD dwIndex, 
                                               IDirectMusicTrack** ppTrack) PURE;
    STDMETHOD(GetTrackGroup)            (THIS_ IDirectMusicTrack* pTrack, 
                                               DWORD* pdwGroupBits) PURE;
    STDMETHOD(InsertTrack)              (THIS_ IDirectMusicTrack* pTrack, 
                                               DWORD dwGroupBits) PURE;
    STDMETHOD(RemoveTrack)              (THIS_ IDirectMusicTrack* pTrack) PURE;
    STDMETHOD(InitPlay)                 (THIS_ IDirectMusicSegmentState** ppSegState, 
                                               IDirectMusicPerformance* pPerformance,
                                               DWORD dwFlags) PURE;
    STDMETHOD(GetGraph)                 (THIS_ IDirectMusicGraph** ppGraph) PURE;
    STDMETHOD(SetGraph)                 (THIS_ IDirectMusicGraph* pGraph) PURE;
    STDMETHOD(AddNotificationType)      (THIS_ REFGUID rguidNotificationType) PURE;
    STDMETHOD(RemoveNotificationType)   (THIS_ REFGUID rguidNotificationType) PURE;
    STDMETHOD(GetParam)                 (THIS_ REFGUID rguidType, 
                                               DWORD dwGroupBits, 
                                               DWORD dwIndex, 
                                               MUSIC_TIME mtTime, 
                                               MUSIC_TIME* pmtNext, 
                                               void* pParam) PURE; 
    STDMETHOD(SetParam)                 (THIS_ REFGUID rguidType, 
                                               DWORD dwGroupBits, 
                                               DWORD dwIndex, 
                                               MUSIC_TIME mtTime, 
                                               void* pParam) PURE;
    STDMETHOD(Clone)                    (THIS_ MUSIC_TIME mtStart, 
                                               MUSIC_TIME mtEnd, 
                                               IDirectMusicSegment** ppSegment) PURE;
    STDMETHOD(SetStartPoint)            (THIS_ MUSIC_TIME mtStart) PURE;
    STDMETHOD(GetStartPoint)            (THIS_ MUSIC_TIME* pmtStart) PURE;
    STDMETHOD(SetLoopPoints)            (THIS_ MUSIC_TIME mtStart, 
                                               MUSIC_TIME mtEnd) PURE;
    STDMETHOD(GetLoopPoints)            (THIS_ MUSIC_TIME* pmtStart, 
                                               MUSIC_TIME* pmtEnd) PURE;
    STDMETHOD(SetPChannelsUsed)         (THIS_ DWORD dwNumPChannels, 
                                               DWORD* paPChannels) PURE;
};

/*/////////////////////////////////////////////////////////////////////
// IDirectMusicSegmentState */
#undef  INTERFACE
#define INTERFACE  IDirectMusicSegmentState
DECLARE_INTERFACE_(IDirectMusicSegmentState, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusicSegmentState */
    STDMETHOD(GetRepeats)           (THIS_ DWORD* pdwRepeats) PURE;
    STDMETHOD(GetSegment )          (THIS_ IDirectMusicSegment** ppSegment) PURE;
    STDMETHOD(GetStartTime)         (THIS_ MUSIC_TIME* pmtStart) PURE;
    STDMETHOD(GetSeek)              (THIS_ MUSIC_TIME* pmtSeek) PURE;
    STDMETHOD(GetStartPoint)        (THIS_ MUSIC_TIME* pmtStart) PURE;
};

/*////////////////////////////////////////////////////////////////////
// IDirectMusicTrack */
#undef  INTERFACE
#define INTERFACE  IDirectMusicTrack
DECLARE_INTERFACE_(IDirectMusicTrack, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)         (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)          (THIS) PURE;
    STDMETHOD_(ULONG,Release)         (THIS) PURE;

    /*  IDirectMusicTrack */
    STDMETHOD(Init)                   (THIS_ IDirectMusicSegment* pSegment) PURE;
    STDMETHOD(InitPlay)               (THIS_ IDirectMusicSegmentState* pSegmentState, 
                                             IDirectMusicPerformance* pPerformance, 
                                             void** ppStateData, 
                                             DWORD dwVirtualTrackID,
                                             DWORD dwFlags) PURE;
    STDMETHOD(EndPlay)                (THIS_ void* pStateData) PURE;
    STDMETHOD(Play)                   (THIS_ void* pStateData, 
                                             MUSIC_TIME mtStart, 
                                             MUSIC_TIME mtEnd, 
                                             MUSIC_TIME mtOffset, 
                                             DWORD dwFlags, 
                                             IDirectMusicPerformance* pPerf, 
                                             IDirectMusicSegmentState* pSegSt, 
                                             DWORD dwVirtualID) PURE;
    STDMETHOD(GetParam)               (THIS_ REFGUID rguidType, 
                                             MUSIC_TIME mtTime, 
                                             MUSIC_TIME* pmtNext, 
                                             void* pParam) PURE; 
    STDMETHOD(SetParam)               (THIS_ REFGUID rguidType, 
                                             MUSIC_TIME mtTime, 
                                             void* pParam) PURE;
    STDMETHOD(IsParamSupported)       (THIS_ REFGUID rguidType) PURE;
    STDMETHOD(AddNotificationType)    (THIS_ REFGUID rguidNotificationType) PURE;
    STDMETHOD(RemoveNotificationType) (THIS_ REFGUID rguidNotificationType) PURE;
    STDMETHOD(Clone)                  (THIS_ MUSIC_TIME mtStart, 
                                             MUSIC_TIME mtEnd, 
                                             IDirectMusicTrack** ppTrack) PURE;
};

/*////////////////////////////////////////////////////////////////////
// IDirectMusicPerformance */
#undef  INTERFACE
#define INTERFACE  IDirectMusicPerformance
DECLARE_INTERFACE_(IDirectMusicPerformance, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusicPerformance */
    STDMETHOD(Init)                 (THIS_ IDirectMusic** ppDirectMusic,
                                           LPDIRECTSOUND pDirectSound,
                                           HWND hWnd) PURE;
    STDMETHOD(PlaySegment)          (THIS_ IDirectMusicSegment* pSegment, 
                                           DWORD dwFlags, 
                                           __int64 i64StartTime, 
                                           IDirectMusicSegmentState** ppSegmentState) PURE;
    STDMETHOD(Stop)                 (THIS_ IDirectMusicSegment* pSegment, 
                                           IDirectMusicSegmentState* pSegmentState, 
                                           MUSIC_TIME mtTime, 
                                           DWORD dwFlags) PURE;
    STDMETHOD(GetSegmentState)      (THIS_ IDirectMusicSegmentState** ppSegmentState, 
                                           MUSIC_TIME mtTime) PURE;
    STDMETHOD(SetPrepareTime)       (THIS_ DWORD dwMilliSeconds) PURE;
    STDMETHOD(GetPrepareTime)       (THIS_ DWORD* pdwMilliSeconds) PURE;
    STDMETHOD(SetBumperLength)      (THIS_ DWORD dwMilliSeconds) PURE;
    STDMETHOD(GetBumperLength)      (THIS_ DWORD* pdwMilliSeconds) PURE;
    STDMETHOD(SendPMsg)             (THIS_ DMUS_PMSG* pPMSG) PURE;
    STDMETHOD(MusicToReferenceTime) (THIS_ MUSIC_TIME mtTime, 
                                           REFERENCE_TIME* prtTime) PURE;
    STDMETHOD(ReferenceToMusicTime) (THIS_ REFERENCE_TIME rtTime, 
                                           MUSIC_TIME* pmtTime) PURE;
    STDMETHOD(IsPlaying)            (THIS_ IDirectMusicSegment* pSegment, 
                                           IDirectMusicSegmentState* pSegState) PURE;
    STDMETHOD(GetTime)              (THIS_ REFERENCE_TIME* prtNow, 
                                           MUSIC_TIME* pmtNow) PURE;
    STDMETHOD(AllocPMsg)            (THIS_ ULONG cb, 
                                           DMUS_PMSG** ppPMSG) PURE;
    STDMETHOD(FreePMsg)             (THIS_ DMUS_PMSG* pPMSG) PURE;
    STDMETHOD(GetGraph)             (THIS_ IDirectMusicGraph** ppGraph) PURE;
    STDMETHOD(SetGraph)             (THIS_ IDirectMusicGraph* pGraph) PURE;
    STDMETHOD(SetNotificationHandle)(THIS_ HANDLE hNotification, 
                                           REFERENCE_TIME rtMinimum) PURE;
    STDMETHOD(GetNotificationPMsg)  (THIS_ DMUS_NOTIFICATION_PMSG** ppNotificationPMsg) PURE;
    STDMETHOD(AddNotificationType)  (THIS_ REFGUID rguidNotificationType) PURE;
    STDMETHOD(RemoveNotificationType)(THIS_ REFGUID rguidNotificationType) PURE;
    STDMETHOD(AddPort)              (THIS_ IDirectMusicPort* pPort) PURE;
    STDMETHOD(RemovePort)           (THIS_ IDirectMusicPort* pPort ) PURE;
    STDMETHOD(AssignPChannelBlock)  (THIS_ DWORD dwBlockNum, 
                                           IDirectMusicPort* pPort, 
                                           DWORD dwGroup ) PURE;
    STDMETHOD(AssignPChannel)       (THIS_ DWORD dwPChannel, 
                                           IDirectMusicPort* pPort, 
                                           DWORD dwGroup, 
                                           DWORD dwMChannel ) PURE;
    STDMETHOD(PChannelInfo)         (THIS_ DWORD dwPChannel, 
                                           IDirectMusicPort** ppPort, 
                                           DWORD* pdwGroup, 
                                           DWORD* pdwMChannel ) PURE;
    STDMETHOD(DownloadInstrument)   (THIS_ IDirectMusicInstrument* pInst, 
                                           DWORD dwPChannel, 
                                           IDirectMusicDownloadedInstrument** ppDownInst, 
                                           DMUS_NOTERANGE* pNoteRanges, 
                                           DWORD dwNumNoteRanges, 
                                           IDirectMusicPort** ppPort, 
                                           DWORD* pdwGroup, 
                                           DWORD* pdwMChannel ) PURE;
    STDMETHOD(Invalidate)           (THIS_ MUSIC_TIME mtTime, 
                                           DWORD dwFlags) PURE;
    STDMETHOD(GetParam)             (THIS_ REFGUID rguidType, 
                                           DWORD dwGroupBits, 
                                           DWORD dwIndex, 
                                           MUSIC_TIME mtTime, 
                                           MUSIC_TIME* pmtNext, 
                                           void* pParam) PURE; 
    STDMETHOD(SetParam)             (THIS_ REFGUID rguidType, 
                                           DWORD dwGroupBits, 
                                           DWORD dwIndex, 
                                           MUSIC_TIME mtTime, 
                                           void* pParam) PURE;
    STDMETHOD(GetGlobalParam)       (THIS_ REFGUID rguidType, 
                                           void* pParam, 
                                           DWORD dwSize) PURE;
    STDMETHOD(SetGlobalParam)       (THIS_ REFGUID rguidType, 
                                           void* pParam, 
                                           DWORD dwSize) PURE;
    STDMETHOD(GetLatencyTime)       (THIS_ REFERENCE_TIME* prtTime) PURE;
    STDMETHOD(GetQueueTime)         (THIS_ REFERENCE_TIME* prtTime) PURE;
    STDMETHOD(AdjustTime)           (THIS_ REFERENCE_TIME rtAmount) PURE;
    STDMETHOD(CloseDown)            (THIS) PURE;
    STDMETHOD(GetResolvedTime)      (THIS_ REFERENCE_TIME rtTime,
                                           REFERENCE_TIME* prtResolved,
                                           DWORD dwTimeResolveFlags) PURE;
    STDMETHOD(MIDIToMusic)          (THIS_ BYTE bMIDIValue,
                                           DMUS_CHORD_KEY* pChord,
                                           BYTE bPlayMode,
                                           BYTE bChordLevel,
                                           WORD *pwMusicValue) PURE;
    STDMETHOD(MusicToMIDI)          (THIS_ WORD wMusicValue,
                                           DMUS_CHORD_KEY* pChord,
                                           BYTE bPlayMode,
                                           BYTE bChordLevel,
                                           BYTE *pbMIDIValue) PURE;
    STDMETHOD(TimeToRhythm)         (THIS_ MUSIC_TIME mtTime,
                                           DMUS_TIMESIGNATURE *pTimeSig,
                                           WORD *pwMeasure,
                                           BYTE *pbBeat,
                                           BYTE *pbGrid,
                                           short *pnOffset) PURE;
    STDMETHOD(RhythmToTime)         (THIS_ WORD wMeasure,
                                           BYTE bBeat,
                                           BYTE bGrid,
                                           short nOffset,
                                           DMUS_TIMESIGNATURE *pTimeSig,
                                           MUSIC_TIME *pmtTime) PURE;                                        
};

/*////////////////////////////////////////////////////////////////////
// IDirectMusicTool */
#undef  INTERFACE
#define INTERFACE  IDirectMusicTool
DECLARE_INTERFACE_(IDirectMusicTool, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusicTool */
    STDMETHOD(Init)                 (THIS_ IDirectMusicGraph* pGraph) PURE;
    STDMETHOD(GetMsgDeliveryType)   (THIS_ DWORD* pdwDeliveryType ) PURE;
    STDMETHOD(GetMediaTypeArraySize)(THIS_ DWORD* pdwNumElements ) PURE;
    STDMETHOD(GetMediaTypes)        (THIS_ DWORD** padwMediaTypes, 
                                           DWORD dwNumElements) PURE;
    STDMETHOD(ProcessPMsg)          (THIS_ IDirectMusicPerformance* pPerf, 
                                           DMUS_PMSG* pPMSG) PURE;
    STDMETHOD(Flush)                (THIS_ IDirectMusicPerformance* pPerf, 
                                           DMUS_PMSG* pPMSG, 
                                           REFERENCE_TIME rtTime) PURE;
};

/*////////////////////////////////////////////////////////////////////
// IDirectMusicGraph */
#undef  INTERFACE
#define INTERFACE  IDirectMusicGraph
DECLARE_INTERFACE_(IDirectMusicGraph, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusicGraph */
    STDMETHOD(StampPMsg)            (THIS_ DMUS_PMSG* pPMSG) PURE;
    STDMETHOD(InsertTool)           (THIS_ IDirectMusicTool* pTool, 
                                           DWORD* pdwPChannels, 
                                           DWORD cPChannels, 
                                           LONG lIndex) PURE;
    STDMETHOD(GetTool)              (THIS_ DWORD dwIndex, 
                                           IDirectMusicTool** ppTool) PURE;
    STDMETHOD(RemoveTool)           (THIS_ IDirectMusicTool* pTool) PURE;
};

/*/////////////////////////////////////////////////////////////////////
// IDirectMusicStyle */
#undef  INTERFACE
#define INTERFACE  IDirectMusicStyle
DECLARE_INTERFACE_(IDirectMusicStyle, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)         (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)          (THIS) PURE;
    STDMETHOD_(ULONG,Release)         (THIS) PURE;

    /*  IDirectMusicStyle */
    STDMETHOD(GetBand)                (THIS_ WCHAR* pwszName, 
                                             IDirectMusicBand** ppBand) PURE;
    STDMETHOD(EnumBand)               (THIS_ DWORD dwIndex, 
                                             WCHAR *pwszName) PURE;
    STDMETHOD(GetDefaultBand)         (THIS_ IDirectMusicBand** ppBand) PURE;
    STDMETHOD(EnumMotif)              (THIS_ DWORD dwIndex, 
                                             WCHAR* pwszName) PURE;
    STDMETHOD(GetMotif)               (THIS_ WCHAR* pwszName, 
                                             IDirectMusicSegment** ppSegment) PURE;
    STDMETHOD(GetDefaultChordMap)     (THIS_ IDirectMusicChordMap** ppChordMap) PURE;
    STDMETHOD(EnumChordMap)           (THIS_ DWORD dwIndex, 
                                             WCHAR *pwszName) PURE;
    STDMETHOD(GetChordMap)            (THIS_ WCHAR* pwszName, 
                                             IDirectMusicChordMap** ppChordMap) PURE;
    STDMETHOD(GetTimeSignature)       (THIS_ DMUS_TIMESIGNATURE* pTimeSig) PURE;
    STDMETHOD(GetEmbellishmentLength) (THIS_ DWORD dwType, 
                                             DWORD dwLevel, 
                                             DWORD* pdwMin, 
                                             DWORD* pdwMax) PURE;
    STDMETHOD(GetTempo)               (THIS_ double* pTempo) PURE;
};

/*/////////////////////////////////////////////////////////////////////
// IDirectMusicChordMap */
#undef  INTERFACE
#define INTERFACE  IDirectMusicChordMap
DECLARE_INTERFACE_(IDirectMusicChordMap, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)        (THIS) PURE;
    STDMETHOD_(ULONG,Release)       (THIS) PURE;

    /*  IDirectMusicChordMap */
    STDMETHOD(GetScale)             (THIS_ DWORD* pdwScale) PURE;
};

/*/////////////////////////////////////////////////////////////////////
// IDirectMusicComposer */
#undef  INTERFACE
#define INTERFACE  IDirectMusicComposer
DECLARE_INTERFACE_(IDirectMusicComposer, IUnknown)
{
    /*  IUnknown */
    STDMETHOD(QueryInterface)               (THIS_ REFIID, LPVOID FAR *) PURE;
    STDMETHOD_(ULONG,AddRef)                (THIS) PURE;
    STDMETHOD_(ULONG,Release)               (THIS) PURE;

    /*  IDirectMusicComposer */
    STDMETHOD(ComposeSegmentFromTemplate)   (THIS_ IDirectMusicStyle* pStyle, 
                                                   IDirectMusicSegment* pTemplate, 
                                                   WORD wActivity, 
                                                   IDirectMusicChordMap* pChordMap, 
                                                   IDirectMusicSegment** ppSegment) PURE;
    STDMETHOD(ComposeSegmentFromShape)      (THIS_ IDirectMusicStyle* pStyle, 
                                                   WORD wNumMeasures, 
                                                   WORD wShape, 
                                                   WORD wActivity, 
                                                   BOOL fIntro, 
                                                   BOOL fEnd, 
                                                   IDirectMusicChordMap* pChordMap, 
                                                   IDirectMusicSegment** ppSegment ) PURE;
    STDMETHOD(ComposeTransition)            (THIS_ IDirectMusicSegment* pFromSeg, 
                                                   IDirectMusicSegment* pToSeg, 
                                                   MUSIC_TIME mtTime, 
                                                   WORD wCommand, 
                                                   DWORD dwFlags, 
                                                   IDirectMusicChordMap* pChordMap, 
                                                   IDirectMusicSegment** ppTransSeg) PURE;
    STDMETHOD(AutoTransition)               (THIS_ IDirectMusicPerformance* pPerformance, 
                                                   IDirectMusicSegment* pToSeg, 
                                                   WORD wCommand, 
                                                   DWORD dwFlags, 
                                                   IDirectMusicChordMap* pChordMap, 
                                                   IDirectMusicSegment** ppTransSeg, 
                                                   IDirectMusicSegmentState** ppToSegState, 
                                                   IDirectMusicSegmentState** ppTransSegState) PURE;
    STDMETHOD(ComposeTemplateFromShape)     (THIS_ WORD wNumMeasures, 
                                                   WORD wShape, 
                                                   BOOL fIntro, 
                                                   BOOL fEnd, 
                                                   WORD wEndLength, 
                                                   IDirectMusicSegment** ppTemplate) PURE;
    STDMETHOD(ChangeChordMap)            (THIS_ IDirectMusicSegment* pSegment, 
                                                   BOOL fTrackScale, 
                                                   IDirectMusicChordMap* pChordMap) PURE;
};

/* CLSID's */
DEFINE_GUID(CLSID_DirectMusicPerformance,0xd2ac2881, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicSegment,0xd2ac2882, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicSegmentState,0xd2ac2883, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicGraph,0xd2ac2884, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicTempoTrack,0xd2ac2885, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicSeqTrack,0xd2ac2886, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicSysExTrack,0xd2ac2887, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicTimeSigTrack,0xd2ac2888, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicStyle,0xd2ac288a, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicChordTrack,0xd2ac288b, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicCommandTrack,0xd2ac288c, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicStyleTrack,0xd2ac288d, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicMotifTrack,0xd2ac288e, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicChordMap,0xd2ac288f, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicComposer,0xd2ac2890, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicSignPostTrack,0xf17e8672, 0xc3b4, 0x11d1, 0x87, 0xb, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicLoader,0xd2ac2892, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicBandTrack,0xd2ac2894, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicBand,0x79ba9e00, 0xb6ee, 0x11d1, 0x86, 0xbe, 0x0, 0xc0, 0x4f, 0xbf, 0x8f, 0xef);
DEFINE_GUID(CLSID_DirectMusicChordMapTrack,0xd2ac2896, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(CLSID_DirectMusicMuteTrack,0xd2ac2898, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Special GUID for all object types. This is used by the loader. */
DEFINE_GUID(GUID_DirectMusicAllTypes,0xd2ac2893, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Notification guids */
DEFINE_GUID(GUID_NOTIFICATION_SEGMENT,0xd2ac2899, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_NOTIFICATION_PERFORMANCE,0x81f75bc5, 0x4e5d, 0x11d2, 0xbc, 0xc7, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xeb);
DEFINE_GUID(GUID_NOTIFICATION_MEASUREANDBEAT,0xd2ac289a, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_NOTIFICATION_CHORD,0xd2ac289b, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_NOTIFICATION_COMMAND,0xd2ac289c, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Track param type guids */
/* Use to get/set a DMUS_COMMAND_PARAM param in the Command track */
DEFINE_GUID(GUID_CommandParam,0xd2ac289d, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Use to get a DMUS_COMMAND_PARAM_2 param in the Command track */
DEFINE_GUID(GUID_CommandParam2, 0x28f97ef7, 0x9538, 0x11d2, 0x97, 0xa9, 0x0, 0xc0, 0x4f, 0xa3, 0x6e, 0x58);

/* Use to get/set a DMUS_CHORD_PARAM param in the Chord track */
DEFINE_GUID(GUID_ChordParam,0xd2ac289e, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Use to get a DMUS_RHYTHM_PARAM param in the Chord track */
DEFINE_GUID(GUID_RhythmParam,0xd2ac289f, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Use to get/set an IDirectMusicStyle param in the Style track */
DEFINE_GUID(GUID_IDirectMusicStyle,0xd2ac28a1, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Use to get a DMUS_TIMESIGNATURE param in the Style and TimeSig tracks */
DEFINE_GUID(GUID_TimeSignature,0xd2ac28a4, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Use to get/set a DMUS_TEMPO_PARAM param in the Tempo track */
DEFINE_GUID(GUID_TempoParam,0xd2ac28a5, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Use to set an IDirectMusicBand param in the Band track */
DEFINE_GUID(GUID_IDirectMusicBand,0xd2ac28ac, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Use to get/set an IDirectMusicChordMap param in the ChordMap track */
DEFINE_GUID(GUID_IDirectMusicChordMap,0xd2ac28ad, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Use to get/set a DMUS_MUTE_PARAM param in the Mute track */
DEFINE_GUID(GUID_MuteParam,0xd2ac28af, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* These guids are used in IDirectMusicSegment::SetParam to tell the band track to perform various actions.
 */
/* Download bands for the IDirectMusicSegment */
DEFINE_GUID(GUID_Download,0xd2ac28a7, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Unload bands for the IDirectMusicSegment */
DEFINE_GUID(GUID_Unload,0xd2ac28a8, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Connect segment's bands to an IDirectMusicCollection */
DEFINE_GUID(GUID_ConnectToDLSCollection, 0x1db1ae6b, 0xe92e, 0x11d1, 0xa8, 0xc5, 0x0, 0xc0, 0x4f, 0xa3, 0x72, 0x6e);

/* Enable/disable autodownloading of bands */
DEFINE_GUID(GUID_Enable_Auto_Download,0xd2ac28a9, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_Disable_Auto_Download,0xd2ac28aa, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Clear all bands */
DEFINE_GUID(GUID_Clear_All_Bands,0xd2ac28ab, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Set segment to manage all program changes, bank selects, etc. for simple playback of a standard MIDI file */
DEFINE_GUID(GUID_StandardMIDIFile, 0x6621075, 0xe92e, 0x11d1, 0xa8, 0xc5, 0x0, 0xc0, 0x4f, 0xa3, 0x72, 0x6e);
/* For compatibility with beta releases... */
#define GUID_IgnoreBankSelectForGM 	GUID_StandardMIDIFile

/* Disable/enable param guids. Use these in SetParam calls to disable or enable sending
 * specific PMsg types.
 */
DEFINE_GUID(GUID_DisableTimeSig, 0x45fc707b, 0x1db4, 0x11d2, 0xbc, 0xac, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xeb);
DEFINE_GUID(GUID_EnableTimeSig, 0x45fc707c, 0x1db4, 0x11d2, 0xbc, 0xac, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xeb);
DEFINE_GUID(GUID_DisableTempo, 0x45fc707d, 0x1db4, 0x11d2, 0xbc, 0xac, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xeb);
DEFINE_GUID(GUID_EnableTempo, 0x45fc707e, 0x1db4, 0x11d2, 0xbc, 0xac, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xeb);

/* Used in SetParam calls for pattern-based tracks.  A nonzero value seeds the random number 
generator for variation selection; a value of zero reverts to the default behavior of 
getting the seed from the system clock.
*/
DEFINE_GUID(GUID_SeedVariations, 0x65b76fa5, 0xff37, 0x11d2, 0x81, 0x4e, 0x0, 0xc0, 0x4f, 0xa3, 0x6e, 0x58);

/* Global data guids */
DEFINE_GUID(GUID_PerfMasterTempo,0xd2ac28b0, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_PerfMasterVolume,0xd2ac28b1, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_PerfMasterGrooveLevel,0xd2ac28b2, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(GUID_PerfAutoDownload, 0xfb09565b, 0x3631, 0x11d2, 0xbc, 0xb8, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xeb);

/* GUID for default GM/GS dls collection. */
DEFINE_GUID(GUID_DefaultGMCollection, 0xf17e8673, 0xc3b4, 0x11d1, 0x87, 0xb, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* IID's */
DEFINE_GUID(IID_IDirectMusicLoader, 0x2ffaaca2, 0x5dca, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);
DEFINE_GUID(IID_IDirectMusicGetLoader,0x68a04844, 0xd13d, 0x11d1, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);
DEFINE_GUID(IID_IDirectMusicObject,0xd2ac28b5, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicSegment, 0xf96029a2, 0x4282, 0x11d2, 0x87, 0x17, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicSegmentState, 0xa3afdcc7, 0xd3ee, 0x11d1, 0xbc, 0x8d, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xeb);
DEFINE_GUID(IID_IDirectMusicTrack, 0xf96029a1, 0x4282, 0x11d2, 0x87, 0x17, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicPerformance,0x7d43d03, 0x6523, 0x11d2, 0x87, 0x1d, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicTool,0xd2ac28ba, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicGraph,0x2befc277, 0x5497, 0x11d2, 0xbc, 0xcb, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xeb);
DEFINE_GUID(IID_IDirectMusicStyle,0xd2ac28bd, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicChordMap,0xd2ac28be, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicComposer,0xd2ac28bf, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);
DEFINE_GUID(IID_IDirectMusicBand,0xd2ac28c0, 0xb39b, 0x11d1, 0x87, 0x4, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

/* Alternate interface IDs, available in DX7 release and after. */
DEFINE_GUID(IID_IDirectMusicPerformance2,0x6fc2cae0, 0xbc78, 0x11d2, 0xaf, 0xa6, 0x0, 0xaa, 0x0, 0x24, 0xd8, 0xb6);
DEFINE_GUID(IID_IDirectMusicSegment2, 0xd38894d1, 0xc052, 0x11d2, 0x87, 0x2f, 0x0, 0x60, 0x8, 0x93, 0xb1, 0xbd);

#ifdef __cplusplus
}; /* extern "C" */
#endif

#include <poppack.h>

#endif /* #ifndef _DMUSICI_ */
