//-----------------------------------------------------------------------------
// File: ddutil.cpp
//
// Desc: Routines for loading bitmap and palettes from resources
//
//
// Copyright (c) 1995-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#include <windows.h>
#include <windowsx.h>
#include <ddraw.h>
#include "ddutil.h"




//-----------------------------------------------------------------------------
// Name: DDUtil_LoadBitmap()
// Desc: Create a DirectDrawSurface from a bitmap resource.
//-----------------------------------------------------------------------------
LPDIRECTDRAWSURFACE4 DDUtil_LoadBitmap( LPDIRECTDRAW4 pDD, LPCSTR strBitmap,
									    int dx, int dy )
{
    HBITMAP              hbm;
    BITMAP               bm;
    DDSURFACEDESC2       ddsd;
    LPDIRECTDRAWSURFACE4 pdds;

    // Try to load the bitmap as a resource, if that fails, try it as a file
    hbm = (HBITMAP)LoadImage( GetModuleHandle(NULL), strBitmap, IMAGE_BITMAP,
		                      dx, dy, LR_CREATEDIBSECTION );
    if( NULL == hbm )
        hbm = (HBITMAP)LoadImage( NULL, strBitmap, IMAGE_BITMAP, dx, dy,
                                  LR_LOADFROMFILE | LR_CREATEDIBSECTION );
    if( NULL == hbm )
        return NULL;

    // Get size of the bitmap
    GetObject( hbm, sizeof(bm), &bm );

    // Create a DirectDrawSurface for this bitmap
    ZeroMemory( &ddsd, sizeof(ddsd) );
    ddsd.dwSize         = sizeof(ddsd);
    ddsd.dwFlags        = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
    ddsd.dwWidth        = bm.bmWidth;
    ddsd.dwHeight       = bm.bmHeight;
    
	if( FAILED( pDD->CreateSurface( &ddsd, &pdds, NULL ) ) )
        return NULL;

    DDUtil_CopyBitmap( pdds, hbm, 0, 0, 0, 0 );
    DeleteObject( hbm );
    return pdds;
}




//-----------------------------------------------------------------------------
// Name: DDUtil_ReLoadBitmap()
// Desc: Load a bitmap from a file or resource into a directdraw surface.
//       normaly used to re-load a surface after a restore.
//-----------------------------------------------------------------------------
HRESULT DDUtil_ReLoadBitmap( LPDIRECTDRAWSURFACE4 pdds, LPCSTR strBitmap )
{
    HBITMAP hbm;
    HRESULT hr;

    // Try to load the bitmap as a resource, if that fails, try it as a file
    hbm = (HBITMAP)LoadImage( GetModuleHandle(NULL), strBitmap, IMAGE_BITMAP,
		                      0, 0, LR_CREATEDIBSECTION );
    if( NULL == hbm )
        hbm = (HBITMAP)LoadImage( NULL, strBitmap, IMAGE_BITMAP, 0, 0,
                                  LR_LOADFROMFILE | LR_CREATEDIBSECTION );
    if( NULL == hbm )
    {
        OutputDebugString( "DDUtil_ReLoadBitmap: handle is null\n" );
        return E_FAIL;
    }

    hr = DDUtil_CopyBitmap( pdds, hbm, 0, 0, 0, 0 );
    if( FAILED( hr ) )
        OutputDebugString( "DDUtil_ReLoadBitmap: copy bitmap failed\n" );

    DeleteObject( hbm );
    return hr;
}




//-----------------------------------------------------------------------------
// Name: DDUtil_CopyBitmap()
// Desc: Draw a bitmap into a DirectDrawSurface
//-----------------------------------------------------------------------------
HRESULT DDUtil_CopyBitmap( LPDIRECTDRAWSURFACE4 pdds, HBITMAP hbm,
						   int x, int y, int dx, int dy )
{
    HDC            hdcImage;
    HDC            hdc;
    BITMAP         bm;
    DDSURFACEDESC2 ddsd;
    HRESULT        hr;

    if( hbm == NULL || pdds == NULL )
        return E_FAIL;

    // Make sure this surface is restored.
    pdds->Restore();

    // Select bitmap into a memoryDC so we can use it.
    hdcImage = CreateCompatibleDC( NULL );
    if( !hdcImage )
        OutputDebugString("createcompatible dc failed\n");
    SelectObject( hdcImage, hbm );

    // Get size of the bitmap
    GetObject( hbm, sizeof(bm), &bm );
    dx = ( dx == 0 ? bm.bmWidth  : dx );  // Use the passed size, unless zero
    dy = ( dy == 0 ? bm.bmHeight : dy );

    // Get size of surface.
    ddsd.dwSize  = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    pdds->GetSurfaceDesc( &ddsd );

    if( SUCCEEDED( hr = pdds->GetDC( &hdc ) ) )
    {
        StretchBlt( hdc, 0, 0, ddsd.dwWidth, ddsd.dwHeight, hdcImage,
			        x, y, dx, dy, SRCCOPY );
        pdds->ReleaseDC( hdc );
    }

    DeleteDC( hdcImage );
    
	return hr;
}




//-----------------------------------------------------------------------------
// Name: DDUtil_LoadPalette()
// Desc: Create a DirectDraw palette object from a bitmap resource
//       if the resource does not exist or NULL is passed create a
//       default 332 palette.
//-----------------------------------------------------------------------------
LPDIRECTDRAWPALETTE DDUtil_LoadPalette( LPDIRECTDRAW4 pDD, LPCSTR strBitmap )
{
    LPDIRECTDRAWPALETTE pPalette;
    int                 i;
    int                 n;
    int                 file;
    HRSRC               hRes;
    BITMAPINFOHEADER*   pbi;
    PALETTEENTRY        pe[256];
    RGBQUAD*            prgb;

    // Build a 332 palette as the default.
    for( i = 0; i < 256; i++ )
    {
        pe[i].peRed   = (BYTE)( ( (i>>5) & 0x07 ) * 255 / 7 );
        pe[i].peGreen = (BYTE)( ( (i>>2) & 0x07 ) * 255 / 7 );
        pe[i].peBlue  = (BYTE)( ( (i>>0) & 0x03 ) * 255 / 3 );
        pe[i].peFlags = (BYTE)0;
    }

	if( strBitmap )
	{
	    // Get a pointer to the bitmap resource.
		if( hRes = FindResource( NULL, strBitmap, RT_BITMAP ) )
		{
			pbi = (BITMAPINFOHEADER*)LockResource( LoadResource( NULL, hRes ) );
			if( NULL == pbi )
				OutputDebugString("lock resource failed\n");
			prgb = (RGBQUAD*)( (BYTE*)pbi + pbi->biSize );
			if( NULL == pbi || pbi->biSize < sizeof(BITMAPINFOHEADER) )
				n = 0;
			else if( pbi->biBitCount > 8 )
				n = 0;
			else if( pbi->biClrUsed == 0 )
				n = 1 << pbi->biBitCount;
			else
				n = pbi->biClrUsed;

			// A DIB color table has its colors stored BGR not RGB
			// so flip them around.
			for( i = 0; i < n; i++ )
			{
				pe[i].peRed   = prgb[i].rgbRed;
				pe[i].peGreen = prgb[i].rgbGreen;
				pe[i].peBlue  = prgb[i].rgbBlue;
				pe[i].peFlags = 0;
			}
		}
	    else if( ( file = _lopen( strBitmap, OF_READ ) ) != -1 )
		{
			BITMAPFILEHEADER bf;
			BITMAPINFOHEADER bi;

			_lread( file, &bf, sizeof(bf) );
			_lread( file, &bi, sizeof(bi) );
			_lread( file, pe, sizeof(pe) );
			_lclose( file );
        
			if( bi.biSize != sizeof(BITMAPINFOHEADER) )
				n = 0;
			else if( bi.biBitCount > 8 )
				n = 0;
			else if( bi.biClrUsed == 0 )
				n = 1 << bi.biBitCount;
			else
				n = bi.biClrUsed;

			//  A DIB color table has its colors stored BGR not RGB
			//  so flip them around.
			for (i = 0; i < n; i++)
			{
				BYTE r = pe[i].peRed;

				pe[i].peRed  = pe[i].peBlue;
				pe[i].peBlue = r;
			}
		}
    }

	// Return the newly created palette
    if( FAILED( pDD->CreatePalette( DDPCAPS_8BIT, pe, &pPalette, NULL ) ) )
		return NULL;
	return pPalette;
}




//-----------------------------------------------------------------------------
// Name: DDUtil_ColorMatch()
// Desc: Convert a RGB color to a pysical color.
//       We do this by leting GDI SetPixel() do the color matching
//       then we lock the memory and see what it got mapped to.
//-----------------------------------------------------------------------------
DWORD DDUtil_ColorMatch( LPDIRECTDRAWSURFACE4 pdds, COLORREF rgb )
{
    COLORREF       rgbT;
    HDC            hdc;
    DWORD          dw = CLR_INVALID;
    DDSURFACEDESC2 ddsd;
    HRESULT        hr;

    //  Use GDI SetPixel to color match for us
    if( rgb != CLR_INVALID && SUCCEEDED( pdds->GetDC( &hdc ) ) )
    {
        rgbT = GetPixel( hdc, 0, 0 );     // Save current pixel value
        SetPixel( hdc, 0, 0, rgb );       // Set our value
        pdds->ReleaseDC( hdc );
    }

    // Now lock the surface so we can read back the converted color
    ddsd.dwSize = sizeof(ddsd);
    while( ( hr = pdds->Lock( NULL, &ddsd, 0, NULL ) ) == DDERR_WASSTILLDRAWING )
	{
		// Wait for surface to be free
	}
    if( SUCCEEDED( hr ) )
    {
        dw = *(DWORD *)ddsd.lpSurface;                 // Get DWORD
        if( ddsd.ddpfPixelFormat.dwRGBBitCount < 32 )
            dw &= (1 << ddsd.ddpfPixelFormat.dwRGBBitCount) - 1;  // Mask it to bpp
        pdds->Unlock( NULL );
    }

    // Now put the color that was there back.
    if( rgb != CLR_INVALID && SUCCEEDED( pdds->GetDC(&hdc) ) )
    {
        SetPixel( hdc, 0, 0, rgbT );
        pdds->ReleaseDC( hdc );
    }

    return dw;
}




//-----------------------------------------------------------------------------
// Name: DDUtil_SetColorKey()
// Desc: Set a color key for a surface, given a RGB.
//       If you pass CLR_INVALID as the color key, the pixel
//       in the upper-left corner will be used.
//-----------------------------------------------------------------------------
HRESULT DDUtil_SetColorKey( LPDIRECTDRAWSURFACE4 pdds, COLORREF rgb )
{
    DDCOLORKEY ddck;
    ddck.dwColorSpaceLowValue  = DDUtil_ColorMatch( pdds, rgb );
    ddck.dwColorSpaceHighValue = ddck.dwColorSpaceLowValue;
    
	return pdds->SetColorKey( DDCKEY_SRCBLT, &ddck );
}


