//-----------------------------------------------------------------------------
// File: DDutil.h
//
// Desc: Routines for loading bitmap and palettes from resources
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <tchar.h>

LPDIRECTDRAWPALETTE DDUtil_LoadPalette( LPDIRECTDRAW pDD, TCHAR* strBitmap );
HRESULT DDUtil_CopyBitmap( LPDIRECTDRAWSURFACE pdds, HBITMAP hbm, 
                           int x, int y, int dx, int dy );
DWORD   DDUtil_ColorMatch( LPDIRECTDRAWSURFACE pdds, COLORREF rgb );
HRESULT DDUtil_SetColorKey( LPDIRECTDRAWSURFACE pdds, COLORREF rgb );




