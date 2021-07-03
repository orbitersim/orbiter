/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File:       dsutil.cpp
 *  Content:    Routines for dealing with sounds from resources
 *
 *
 ***************************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

///////////////////////////////////////////////////////////////////////////////
//
// DSLoadSoundBuffer    Loads an IDirectSoundBuffer from a Win32 resource in
//                      the current application.
//
// Params:
//  pDS         -- Pointer to an IDirectSound that will be used to create
//                 the buffer.
//
//  lpName      -- Name of WAV resource to load the data from.  Can be a
//                 resource id specified using the MAKEINTRESOURCE macro.
//
// Returns an IDirectSoundBuffer containing the wave data or NULL on error.
//
// example:
//  in the application's resource script (.RC file)
//      Turtle WAV turtle.wav
//
//  some code in the application:
//      IDirectSoundBuffer *pDSB = DSLoadSoundBuffer(pDS, "Turtle");
//
//      if (pDSB)
//      {
//          IDirectSoundBuffer_Play(pDSB, 0, 0, DSBPLAY_TOEND);
//          /* ... */
//
///////////////////////////////////////////////////////////////////////////////
IDirectSoundBuffer *DSLoadSoundBuffer(IDirectSound *pDS, LPCTSTR lpName);

///////////////////////////////////////////////////////////////////////////////
//
// DSReloadSoundBuffer  Reloads an IDirectSoundBuffer from a Win32 resource in
//                      the current application. normally used to handle
//                      a DSERR_BUFFERLOST error.
// Params:
//  pDSB        -- Pointer to an IDirectSoundBuffer to be reloaded.
//
//  lpName      -- Name of WAV resource to load the data from.  Can be a
//                 resource id specified using the MAKEINTRESOURCE macro.
//
// Returns a BOOL indicating whether the buffer was successfully reloaded.
//
// example:
//  in the application's resource script (.RC file)
//      Turtle WAV turtle.wav
//
//  some code in the application:
//  TryAgain:
//      HRESULT hres = IDirectSoundBuffer_Play(pDSB, 0, 0, DSBPLAY_TOEND);
//
//      if (FAILED(hres))
//      {
//          if ((hres == DSERR_BUFFERLOST) &&
//              DSReloadSoundBuffer(pDSB, "Turtle"))
//          {
//              goto TryAgain;
//          }
//          /* deal with other errors... */
//      }
//
///////////////////////////////////////////////////////////////////////////////
BOOL DSReloadSoundBuffer(IDirectSoundBuffer *pDSB, LPCTSTR lpName);

///////////////////////////////////////////////////////////////////////////////
//
// DSGetWaveResource    Finds a WAV resource in a Win32 module.
//
// Params:
//  hModule     -- Win32 module handle of module containing WAV resource.
//                 Pass NULL to indicate current application.
//
//  lpName      -- Name of WAV resource to load the data from.  Can be a
//                 resource id specified using the MAKEINTRESOURCE macro.
//
//  ppWaveHeader-- Optional pointer to WAVEFORMATEX * to receive a pointer to
//                 the WAVEFORMATEX header in the specified WAV resource.
//                 Pass NULL if not required.
//
//  ppbWaveData -- Optional pointer to BYTE * to receive a pointer to the
//                 waveform data in the specified WAV resource.  Pass NULL if
//                 not required.
//
//  pdwWaveSize -- Optional pointer to DWORD to receive the size of the
//                 waveform data in the specified WAV resource.  Pass NULL if
//                 not required.
//
// Returns a BOOL indicating whether a valid WAV resource was found.
//
///////////////////////////////////////////////////////////////////////////////
BOOL DSGetWaveResource(HMODULE hModule, LPCTSTR lpName,
    WAVEFORMATEX **ppWaveHeader, BYTE **ppbWaveData, DWORD *pdwWaveSize);

///////////////////////////////////////////////////////////////////////////////
//
// HSNDOBJ             Handle to a SNDOBJ object.
//
//  SNDOBJs are implemented in dsutil as an example layer built on top
//      of DirectSound.
//
//      A SNDOBJ is generally used to manage individual
//      sounds which need to be played multiple times concurrently.  A
//      SNDOBJ represents a queue of IDirectSoundBuffer objects which
//      all refer to the same buffer memory.
//
//      A SNDOBJ also automatically reloads the sound resource when
//      DirectSound returns a DSERR_BUFFERLOST
//
///////////////////////////////////////////////////////////////////////////////
#ifndef _HSNDOBJ_DEFINED
DECLARE_HANDLE32(HSNDOBJ);
#endif

///////////////////////////////////////////////////////////////////////////////
//
// SndObjCreate     Loads a SNDOBJ from a Win32 resource in
//		    the current application.
//
// Params:
//  pDS         -- Pointer to an IDirectSound that will be used to create
//                 the SNDOBJ.
//
//  lpName      -- Name of WAV resource to load the data from.  Can be a
//                 resource id specified using the MAKEINTRESOURCE macro.
//
//  iConcurrent -- Integer representing the number of concurrent playbacks of
//                 to plan for.  Attempts to play more than this number will
//                 succeed but will restart the least recently played buffer
//                 even if it is not finished playing yet.
//
// Returns an HSNDOBJ or NULL on error.
//
// NOTES:
//      SNDOBJs automatically restore and reload themselves as required.
//
///////////////////////////////////////////////////////////////////////////////
HSNDOBJ SndObjCreate(IDirectSound *pDS, LPCTSTR lpName, int iConcurrent);

///////////////////////////////////////////////////////////////////////////////
//
// SndObjDestroy  Frees a SNDOBJ and releases all of its buffers.
//
// Params:
//  hSO         -- Handle to a SNDOBJ to free.
//
///////////////////////////////////////////////////////////////////////////////
void SndObjDestroy(HSNDOBJ hSO);

///////////////////////////////////////////////////////////////////////////////
//
// SndObjPlay	Plays a buffer in a SNDOBJ.
//
// Params:
//  hSO         -- Handle to a SNDOBJ to play a buffer from.
//
//  dwPlayFlags -- Flags to pass to IDirectSoundBuffer::Play.  It is not
//                 legal to play an SndObj which has more than one buffer
//                 with the DSBPLAY_LOOPING flag.  Pass 0 to stop playback.
//
///////////////////////////////////////////////////////////////////////////////
BOOL SndObjPlay(HSNDOBJ hSO, DWORD dwPlayFlags);

///////////////////////////////////////////////////////////////////////////////
//
// SndObjStop	Stops one or more buffers in a SNDOBJ.
//
// Params:
//  hSO         -- Handle to a SNDOBJ to play a buffer from.
//
///////////////////////////////////////////////////////////////////////////////
BOOL SndObjStop(HSNDOBJ hSO);

///////////////////////////////////////////////////////////////////////////////
//
// SndObjGetFreeBuffer	    returns one of the cloned buffers that is
//			    not currently playing
//
// Params:
//  hSO 	-- Handle to a SNDOBJ
//
// NOTES:
//  This function is provided so that callers can set things like pan etc
//  before playing the buffer.
//
// EXAMPLE:
//  ...
//
///////////////////////////////////////////////////////////////////////////////
IDirectSoundBuffer *SndObjGetFreeBuffer(HSNDOBJ hSO);

///////////////////////////////////////////////////////////////////////////////
//
// helper routines
//
///////////////////////////////////////////////////////////////////////////////

BOOL DSFillSoundBuffer(IDirectSoundBuffer *pDSB, BYTE *pbWaveData, DWORD dwWaveSize);
BOOL DSParseWaveResource(void *pvRes, WAVEFORMATEX **ppWaveHeader, BYTE **ppbWaveData, DWORD *pdwWaveSize);

#ifdef __cplusplus
}
#endif
