//-----------------------------------------------------------------------------
// File: ddutil.cpp
//
// Desc: Routines for loading bitmap and palettes from resources
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#ifndef DDUTIL_H
#define DDUTIL_H




LPDIRECTDRAWPALETTE  DDUtil_LoadPalette( LPDIRECTDRAW4 pDD, LPCSTR strBitmap );
LPDIRECTDRAWSURFACE4 DDUtil_LoadBitmap( LPDIRECTDRAW4 pDD, LPCSTR strBitmap,
										int dx, int dy );
HRESULT DDUtil_ReLoadBitmap( LPDIRECTDRAWSURFACE4 pdds, LPCSTR strBitmap );
HRESULT DDUtil_CopyBitmap( LPDIRECTDRAWSURFACE4 pdds, HBITMAP hbm, int x, int y,
					       int dx, int dy );
DWORD   DDUtil_ColorMatch( LPDIRECTDRAWSURFACE4 pdds, COLORREF rgb );
HRESULT DDUtil_SetColorKey( LPDIRECTDRAWSURFACE4 pdds, COLORREF rgb );




#endif // DDUTIL_H

