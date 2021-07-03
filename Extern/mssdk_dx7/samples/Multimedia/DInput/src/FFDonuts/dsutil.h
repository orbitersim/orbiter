//-----------------------------------------------------------------------------
// File: dsutil.cpp
//
// Desc: Routines for dealing with sounds from resources
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#ifndef DSUTIL_H
#define DSUTIL_H




//-----------------------------------------------------------------------------
// Helper routines
//-----------------------------------------------------------------------------
HRESULT DSUtil_FillSoundBuffer( LPDIRECTSOUNDBUFFER pDSB, BYTE* pbWaveData,
					            DWORD dwWaveSize );
HRESULT DSUtil_ParseWaveResource( VOID* pvRes, WAVEFORMATEX** ppWaveHeader,
						          BYTE** ppbWaveData, DWORD* pdwWaveSize );




//-----------------------------------------------------------------------------
// Name: DSUtil_LoadSoundBuffer()
// Desc: Loads an IDirectSoundBuffer from a Win32 resource in the current
//       application.
//-----------------------------------------------------------------------------
LPDIRECTSOUNDBUFFER DSUtil_LoadSoundBuffer( LPDIRECTSOUND* pDS,
										    LPCTSTR strName );




//-----------------------------------------------------------------------------
// Name: DSUtil_ReloadSoundBuffer()
// Desc: Reloads an IDirectSoundBuffer from a Win32 resource in the current
//       application. normally used to handle a DSERR_BUFFERLOST error.
//-----------------------------------------------------------------------------
HRESULT DSUtil_ReloadSoundBuffer( LPDIRECTSOUNDBUFFER pDSB, LPCTSTR strName );




//-----------------------------------------------------------------------------
// Name: DSUtil_GetWaveResource()
// Desc: Finds a WAV resource in a Win32 module.
//-----------------------------------------------------------------------------
HRESULT DSUtil_GetWaveResource( HMODULE hModule, LPCTSTR strName,
                                WAVEFORMATEX** ppWaveHeader, BYTE** ppbWaveData,
				     		    DWORD* pdwWaveSize );




//-----------------------------------------------------------------------------
// Name: struct SoundObject
// Desc: Used to manage individual sounds which need to be played multiple
//       times concurrently.  A SoundObject represents a queue of
//       IDirectSoundBuffer objects which all refer to the same buffer memory.
//-----------------------------------------------------------------------------
struct SoundObject
{
    BYTE* pbWaveData;                 // Ptr into wave resource (for restore)
    DWORD cbWaveSize;                 // Size of wave data (for restore)
    DWORD dwNumBuffers;               // Number of sound buffers.
    DWORD dwCurrent;                  // Current sound buffer
    LPDIRECTSOUNDBUFFER* pdsbBuffers; // List of sound buffers
};




//-----------------------------------------------------------------------------
// Name: DSUtil_CreateSound()
// Desc: Loads a SoundObject from a Win32 resource in the current application.
//-----------------------------------------------------------------------------
SoundObject* DSUtil_CreateSound( LPDIRECTSOUND pDS, LPCTSTR strName,
								 DWORD dwNumConcurrentBuffers );




//-----------------------------------------------------------------------------
// Name: DSUtil_DestroySound()
// Desc: Frees a SoundObject and releases all of its buffers.
//-----------------------------------------------------------------------------
VOID DSUtil_DestroySound( SoundObject* pSound );




//-----------------------------------------------------------------------------
// Name: DSUtil_PlayPannedSound()
// Desc: Play a sound, but first set the panning according to where the
//       object is on the screen. fScreenXPos is between -1.0f (left) and
//       1.0f (right).
//-----------------------------------------------------------------------------
VOID DSUtil_PlayPannedSound( SoundObject* pSound, FLOAT fScreenXPos );




//-----------------------------------------------------------------------------
// Name: DSUtil_PlaySound()
// Desc: Plays a buffer in a SoundObject.
//-----------------------------------------------------------------------------
HRESULT DSUtil_PlaySound( SoundObject* pSound, DWORD dwPlayFlags );




//-----------------------------------------------------------------------------
// Name: DSUtil_StopSound()
// Desc: Stops one or more buffers in a SoundObject.
//-----------------------------------------------------------------------------
HRESULT DSUtil_StopSound( SoundObject* pSound );




//-----------------------------------------------------------------------------
// Name: DSUtil_GetFreeSoundBuffer()
// Desc: Returns one of the cloned buffers that is not currently playing
//-----------------------------------------------------------------------------
LPDIRECTSOUNDBUFFER DSUtil_GetFreeSoundBuffer( SoundObject* pSound );




#endif // DSUTIL_H
