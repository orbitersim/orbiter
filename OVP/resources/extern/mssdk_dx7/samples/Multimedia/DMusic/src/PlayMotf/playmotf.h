//-----------------------------------------------------------------------------
// File: PlayMotf.h
//
// Desc: Plays DMusic motifs
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef PLAY_MOTIF_H
#define PLAY_MOTIF_H


static CHAR g_strDirectXRegKey[] = "Software\\Microsoft\\DirectX";
static CHAR g_strSamplesPath[]   = "DXSDK Samples Path";
static CHAR g_strMediaPath[]     = "\\DMusic\\Media";


//-----------------------------------------------------------------------------
// External function-prototypes
//-----------------------------------------------------------------------------
extern HRESULT InitDirectMusic( LPSTR strCmdLine );
extern HRESULT FreeDirectMusic();

HRESULT PlayMotif( WCHAR* pwstrMotifName );
HRESULT InitializeSynth();
HRESULT LoadSegment( BOOL bUseCWD );
BOOL    GetSearchPath( CHAR strPath[MAX_PATH] );


#endif


