//
// MLBmpSrf.h
//
// Encapsulates a bitmap bound to a DirectDraw surface
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#ifndef _MLBmpSrf_
#define _MLBmpSrf_

class OptimalPalette;
class BitmapSurface
{
public:
    BitmapSurface(LPDIRECTDRAW pDDraw, LPCSTR szResourceOrFileName, int cx, int cy);
    ~BitmapSurface();

    // AddToOptimalPalette.
    //
    // This can be done before restore. Normally an optimal palette would be built when the 
    // game art was compiled. However, future versions of MusicLines call for user-loadable
    // art; therefore, we do this at load time. The performance impact is negligible.
    //
    // If there was enough art for this to really slow down startup time, the palette could
    // be recached somewhere every time a new tile set was specified.
    //
    HRESULT AddToOptimalPalette(OptimalPalette *pPalette);

    // Restore serves multiple purposes. The first call to it actually attempts to create the surface.
    // Future calls just do the work to restore the bitmap if the surface has been lost,
    // including the work to restore to the current palette.
    //
    HRESULT Restore();

    // Chance the owning DDraw interface.
    //
    HRESULT SetDDraw(LPDIRECTDRAW pDDraw);

    // Build the DirectDraw palette entries for this bitmap. Does not actually create the palette as
    // these entries might be combined with others to make a master palette.
    //
    // XXX If a meta-palette is constructed, then there needs to be a way to indicate to this object
    // that the bits have to be translated into that palette before surface construction. 
    //
    HRESULT GetPalette(PALETTEENTRY pal[256]);

    // GetColorMatch returns the pixel value in the current mode for the given color reference.
    //
    HRESULT GetColorMatch(COLORREF cr, DWORD *pdwColor);

    // This class is really just a wrapper for the surface pointer which handles restore nicely.
    // Most of the time the caller really wants this.
    //
    inline LPDIRECTDRAWSURFACE GetSurface() { return m_pSurface; }
        
private:
    LPDIRECTDRAW                m_pDDraw;               // -> DDraw interface
    LPSTR                       m_pstrName;             // Resource or file name
    int                         m_cx, m_cy;             // Desired size
    LPDIRECTDRAWSURFACE         m_pSurface;             // The DirectDraw surface

private:
    HBITMAP LoadImage();
};

#endif // _MLBmpSrf_    


