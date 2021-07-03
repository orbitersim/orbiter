//-----------------------------------------------------------------------------
// File: Helper.h
//
// Desc: Header file for DirectMusic sample
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef HELPER_H
#define HELPER_H

#include "EchoTool.h"

#define MULTI_TO_WIDE( x,y )	MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, y, -1, x, _MAX_PATH );
static char szDirectMusicMedia[] = "\\DMusic\\Media";

//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectMusic( LPSTR lpCmdLine );
HRESULT AddTool();
HRESULT InitializeSynth();
HRESULT LoadSegment( BOOL fUseCWD );
extern HRESULT FreeDirectMusic();
BOOL GetSearchPath(WCHAR wszPath[MAX_PATH]);

//-----------------------------------------------------------------------------
// External global vars
//-----------------------------------------------------------------------------
extern CEchoTool* g_pEchoTool;

#endif // !defined(HELPER_H)

