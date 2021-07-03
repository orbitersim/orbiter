//-----------------------------------------------------------------------------
// File: gfx.h
//
// Desc: Graphics routines
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include "ddraw.h"

HRESULT InitGraphics();
VOID    CleanupGraphics();
HRESULT BltSplashScreen( RECT* prc );
HRESULT BltNumber( CHAR* strScore, int x, int y );
HRESULT BltObject( int x, int y, LPDIRECTDRAWSURFACE pdds, RECT* src,
                   DWORD dwFlags );
VOID    EraseScreen();
VOID    FlipScreen();
HRESULT RestoreSurfaces();
VOID    SetGamePalette();



