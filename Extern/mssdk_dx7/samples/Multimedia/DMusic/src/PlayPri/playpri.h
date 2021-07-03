//-----------------------------------------------------------------------------
// File: PlayPri.h
//
// Desc: Header file for for DirectMusic sample
//
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef PLAY_PRI_H
#define PLAY_PRI_H

#define MULTI_TO_WIDE( x,y )	MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, y, -1, x, _MAX_PATH );
static char szDirectMusicMedia[] = "\\DMusic\\Media";

//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectMusic( LPSTR lpCmdLine );
HRESULT InitializeSynth();
HRESULT LoadSegment( BOOL fUseCWD );
extern HRESULT FreeDirectMusic();
BOOL GetSearchPath(WCHAR wszPath[MAX_PATH]);

#endif // !defined(PLAY_PRI_H)
