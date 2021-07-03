//-----------------------------------------------------------------------------
// File: Helper.h
//
// Desc: DirectMusic Helper Functions
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef DMUTIL_H
#define DMUTIL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <windowsx.h>

interface IDirectMusic;
interface IDirectMusicPort;
interface IDirectMusicPerformance;
interface IDirectMusicLoader;
interface IDirectMusicSegment;

IDirectSoundBuffer* DMLoad3DSoundBuffer();

HRESULT InitDirectMusic(HWND hWnd);
HRESULT FreeDirectMusic();

IDirectMusicPort* CreateSWSynthPort(void);
IDirectMusicPerformance* CreatePerformance(void);
IDirectMusicLoader* CreateLoader(void);
IDirectMusicSegment* LoadSegment(LPCTSTR lpName);


#ifdef __cplusplus
}
#endif


#endif // DMUTIL_H



