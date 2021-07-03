//-----------------------------------------------------------------------------
// File: DDutil.cpp
//
// Desc: Routines for loading bitmap and palettes from resources
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#undef  WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#include <ddraw.h>
#include "ddutil.h"




//-----------------------------------------------------------------------------
// Name: DDUtil_CopyBitmap()
// Desc: Draw a bitmap into a DirectDrawSurface
//-----------------------------------------------------------------------------
HRESULT DDUtil_CopyBitmap( LPDIRECTDRAWSURFACE pdds, HBITMAP hbm, int x, int y,
                           int dx, int dy )
{
    HDC           hdcImage;
    HDC           hdc;
    BITMAP        bm;
    DDSURFACEDESC ddsd;
    HRESULT       hr;

    if( hbm == NULL || pdds == NULL )
        return E_FAIL;

    // Make sure this surface is restored.
    pdds->Restore();

    // Select bitmap into a memoryDC so we can use it.
    hdcImage = CreateCompatibleDC(NULL);
    if( !hdcImage )
    {
        OutputDebugString( TEXT("CreateCompatibleDC() failed\n") );
        return E_FAIL;
    }
    SelectObject( hdcImage, hbm );

    // Get size of the bitmap
    GetObject( hbm, sizeof(bm), &bm );    // get size of bitmap
    dx = dx == 0 ? bm.bmWidth  : dx;    // use the passed size, unless zero
    dy = dy == 0 ? bm.bmHeight : dy;

    // Gt size of surface.
    ddsd.dwSize  = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    pdds->GetSurfaceDesc(&ddsd);

    if( SUCCEEDED( hr = pdds->GetDC(&hdc) ) )
    {
        StretchBlt( hdc, 0, 0, ddsd.dwWidth, ddsd.dwHeight, hdcImage,
                    x, y, dx, dy, SRCCOPY);
        pdds->ReleaseDC( hdc );
    }

    DeleteDC( hdcImage );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DDUtil_LoadPalette()
// Desc: Create a DirectDraw palette object from a bitmap resoure
//       If the resource does not exist or NULL is passed create a
//       default 332 palette.
//-----------------------------------------------------------------------------
LPDIRECTDRAWPALETTE DDUtil_LoadPalette( LPDIRECTDRAW pDD, TCHAR* strBitmap )
{
    LPDIRECTDRAWPALETTE pddPalette;
    int                 i;
    int                 n;
    HANDLE              fh;
    HRSRC               h;
    LPBITMAPINFOHEADER  lpbi;
    PALETTEENTRY        ape[256];
    RGBQUAD*            prgb;
    DWORD               dwRead;

    // Build a 332 palette as the default.
    for( i=0; i<256; i++ )
    {
        ape[i].peRed   = (BYTE)(((i >> 5) & 0x07) * 255 / 7);
        ape[i].peGreen = (BYTE)(((i >> 2) & 0x07) * 255 / 7);
        ape[i].peBlue  = (BYTE)(((i >> 0) & 0x03) * 255 / 3);
        ape[i].peFlags = (BYTE)0;
    }

    // Gt a pointer to the bitmap resource.
    if( strBitmap && ( h = FindResource( NULL, strBitmap, RT_BITMAP ) ) )
    {
        lpbi = (LPBITMAPINFOHEADER)LockResource( LoadResource( NULL, h ) );
        if( NULL == lpbi )
            OutputDebugString(TEXT("lock resource failed\n"));
        prgb = (RGBQUAD*)((BYTE*)lpbi + lpbi->biSize);

        if (lpbi == NULL || lpbi->biSize < sizeof(BITMAPINFOHEADER))
            n = 0;
        else if (lpbi->biBitCount > 8)
            n = 0;
        else if (lpbi->biClrUsed == 0)
            n = 1 << lpbi->biBitCount;
        else
            n = lpbi->biClrUsed;

        // A DIB color table has its colors stored BGR not RGB, so flip them
        for(i=0; i<n; i++ )
        {
            ape[i].peRed   = prgb[i].rgbRed;
            ape[i].peGreen = prgb[i].rgbGreen;
            ape[i].peBlue  = prgb[i].rgbBlue;
            ape[i].peFlags = 0;
        }
    }
    else if( strBitmap && ( fh = CreateFile( strBitmap, GENERIC_READ,
                    FILE_SHARE_READ, NULL, OPEN_EXISTING,
                    FILE_ATTRIBUTE_NORMAL, NULL ) ) != INVALID_HANDLE_VALUE )
    {
        BITMAPFILEHEADER bf;
        BITMAPINFOHEADER bi;

        ReadFile( fh, &bf, sizeof(bf), &dwRead, NULL );
        ReadFile( fh, &bi, sizeof(bi), &dwRead, NULL );
        ReadFile( fh, ape, sizeof(ape), &dwRead, NULL );
        CloseHandle( fh );

        if( bi.biSize != sizeof(BITMAPINFOHEADER) )
            n = 0;
        else if( bi.biBitCount > 8 )
            n = 0;
        else if( bi.biClrUsed == 0 )
            n = 1 << bi.biBitCount;
        else
            n = bi.biClrUsed;

        // A DIB color table has its colors stored BGR not RGB so flip them
        for(i=0; i<n; i++ )
        {
            BYTE r = ape[i].peRed;
            ape[i].peRed  = ape[i].peBlue;
            ape[i].peBlue = r;
        }
    }

    pDD->CreatePalette( DDPCAPS_8BIT, ape, &pddPalette, NULL );

    return pddPalette;
}




//-----------------------------------------------------------------------------
// Name: DDUtil_ColorMatch()
// Desc: Convert a RGB color to a pysical color. We do this by leting GDI
//       SetPixel() do the color matching.
//-----------------------------------------------------------------------------
DWORD DDUtil_ColorMatch( LPDIRECTDRAWSURFACE pdds, COLORREF rgb )
{
    DDSURFACEDESC ddsd;
    COLORREF rgbT;
    HDC      hdc;
    DWORD    dw = CLR_INVALID;
    HRESULT  hr;

    // Wse GDI SetPixel to color match for us
    if( rgb != CLR_INVALID && SUCCEEDED( pdds->GetDC(&hdc) ) )
    {
        rgbT = GetPixel( hdc, 0, 0 );   // Save current pixel value
        SetPixel( hdc, 0, 0, rgb );     // Set our value
        pdds->ReleaseDC( hdc );
    }

    // Now lock the surface so we can read back the converted color
    ddsd.dwSize = sizeof(ddsd);
    while( ( hr = pdds->Lock( NULL, &ddsd, 0, NULL ) ) == DDERR_WASSTILLDRAWING )
    {}

    if( SUCCEEDED(hr) )
    {
        dw  = *(DWORD *)ddsd.lpSurface;                     // get DWORD
        dw &= (1 << ddsd.ddpfPixelFormat.dwRGBBitCount)-1;  // mask it to bpp
        pdds->Unlock(NULL);
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
// Desc: Set a color key for a surface, given a RGB. If you pass CLR_INVALID as
//       the color key, the pixel in the upper-left corner will be used.
//-----------------------------------------------------------------------------
HRESULT DDUtil_SetColorKey( LPDIRECTDRAWSURFACE pdds, COLORREF rgb )
{
    DDCOLORKEY ddck;
    ddck.dwColorSpaceLowValue  = DDUtil_ColorMatch( pdds, rgb );
    ddck.dwColorSpaceHighValue = ddck.dwColorSpaceLowValue;

    return pdds->SetColorKey( DDCKEY_SRCBLT, &ddck );
}



