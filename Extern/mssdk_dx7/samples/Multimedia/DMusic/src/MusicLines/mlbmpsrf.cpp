//
// MLBmpSrf.cpp
//
// Encapsulates a bitmap bound to a DirectDraw surface
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <ddraw.h>
#include "Debug.h"
#include "MLOptPal.h"
#include "MLBmpSrf.h"

#define BITMAP_TYPE     ((WORD)0x4D42)      // BM

// BitmapSurface constructor
//
// Doesn't actually do much except save the initial values. Surface will be created on the
// first call to Restore if it doesn't exist.
//
BitmapSurface::BitmapSurface(
    LPDIRECTDRAW pDDraw,
    LPCSTR szResourceOrFileName,
    int cx,
    int cy)
{
    if (HIWORD(szResourceOrFileName))
    {
        // Really a string, not a MAKEINTRESOURCE
        //
        m_pstrName = new char[strlen(szResourceOrFileName) + 1];
        strcpy(m_pstrName, szResourceOrFileName);
    }
    else
    {   
        m_pstrName = (LPSTR)szResourceOrFileName;
    }

    m_pDDraw = pDDraw;
    m_pDDraw->AddRef();

    m_cx = cx;
    m_cy = cy;

    m_pSurface = NULL;
}

// BitmapSurface::~BitmapSurface
//
BitmapSurface::~BitmapSurface()
{
    if (m_pSurface)
    {
        m_pSurface->Release();
    }

    if (HIWORD(m_pstrName))
    {
        delete[] m_pstrName;
    }

    if (m_pDDraw)
    {
        m_pDDraw->Release();
    }
}

// BitmapSurface::AddToOptimalPalette
//
// Add this bitmap to an optimal palette object. Traverse the bits
// and add each pixel's color entry once. 
//
HRESULT BitmapSurface::AddToOptimalPalette(
    OptimalPalette *pPalette)
{
    HRESULT hr;
    PALETTEENTRY pal[MAX_PALETTE_ENTRIES];
    HBITMAP hbm;

    // Get palette and pointer to bitmap bits
    //
    hr = GetPalette(pal);
    if (FAILED(hr))
    {
        Trace(0, "Could not get palette");
        return hr;
    }            

    if ((hbm = LoadImage()) == NULL)
    {
        return E_FAIL;
    }

    BITMAP bm;
    GetObject(hbm, sizeof(bm), &bm);

    LPBYTE pBits = (LPBYTE)bm.bmBits;

    // Walk the bitmap, adding one instance of each color per pixel that uses
    // it.
    //    
    LONG idxX;
    LONG idxY;

    for (idxY = 0; idxY < bm.bmHeight; idxY++)
    {
        for (idxX = 0; idxX < bm.bmWidth; idxX++)
        {
            pPalette->AddPixel(pal[*pBits++]);
        }

        pBits += (bm.bmWidthBytes - bm.bmWidth);
    }

    DeleteObject(hbm);

    return S_OK;
}
    

// BitmapSurface::Restore
//
// Build or restore the bitmap into a surface. Rerun through GDI to build the art into the current
// palette and pixel format.
//
HRESULT BitmapSurface::Restore()
{
    HRESULT hr;
    HBITMAP hbm;
    DDSURFACEDESC ddsd;

    if ((hbm = LoadImage()) == NULL)
    {
        return E_FAIL;
    }

    BITMAP bmInfo;
    if (GetObject(hbm, sizeof(bmInfo), &bmInfo) == 0)
    {
        TraceMsg("GetObject failed", GetLastError());
        DeleteObject(hbm);
        return E_FAIL;
    }

    // If there isn't a surface yet, create one. Otherwise, try to restore.
    //
    if (!m_pSurface)
    {
        ZeroMemory(&ddsd, sizeof(ddsd));
        ddsd.dwSize         = sizeof(ddsd);
        ddsd.dwFlags        = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH;
        ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
        ddsd.dwWidth        = bmInfo.bmWidth;
        ddsd.dwHeight       = bmInfo.bmHeight;

        hr = m_pDDraw->CreateSurface(&ddsd, &m_pSurface, NULL);
        if (FAILED(hr))
        {
            TraceMsg("CreateSurface(OFFSCREEN_PLAIN)", hr);
            DeleteObject(hbm);
            return hr;
        }

        if (HIWORD(m_pstrName))
        {
            Trace(1, "Created surface %p for bitmap %s", m_pSurface, m_pstrName);
        }
        else
        {
            Trace(1, "Created surface %p for bitmap #%d", m_pSurface, m_pstrName);
        }
    }

    hr = m_pSurface->Restore();
    if (FAILED(hr))
    {
        TraceMsg("Restore", hr);
        DeleteObject(hbm);
        return hr;
    }
    
    // In order for palette to be right, we need to get GDI to do color
    // conversion for us. Select the bitmap into a memory DC and blt the bits
    // into the surface via GDI to make this happen.
    // 
    HDC hdcMemory = CreateCompatibleDC(NULL);
    if (hdcMemory == NULL)
    {
        TraceMsg("CreateCompatibleDC", GetLastError());
        DeleteObject(hbm);
        return E_FAIL;
    }

    SelectObject(hdcMemory, hbm);

    int cx = (m_cx ? m_cx : bmInfo.bmWidth);
    int cy = (m_cy ? m_cy : bmInfo.bmHeight);

    // Blt from bitmap size into surface size (maybe stretching)
    //
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;
    m_pSurface->GetSurfaceDesc(&ddsd);

    HDC hdcSurface;
    hr = m_pSurface->GetDC(&hdcSurface);
    if (SUCCEEDED(hr))
    {
        StretchBlt(hdcSurface,                              // Dest DC
                   0, 0, ddsd.dwWidth, ddsd.dwHeight,       // Dest rect
                   hdcMemory,                               // Source DC
                   0, 0, cx, cy,                            // Source rect
                   SRCCOPY);
    }
    else
    {
        TraceMsg("pSurface->GetDC()", hr);
    }

    m_pSurface->ReleaseDC(hdcSurface);
    DeleteDC(hdcMemory);
    DeleteObject(hbm);

    return hr;
}                

// BitmapSurface::SetDDraw
//
// Force everything to be recreated when the owning ddraw has been deleted and 
// recreated.
//
HRESULT BitmapSurface::SetDDraw(
    LPDIRECTDRAW pDDraw)
{
    if (m_pSurface)
    {
        m_pSurface->Release();
        m_pSurface = NULL;
    }

    if (m_pDDraw)
    {
        m_pDDraw->Release();
    }
    m_pDDraw = pDDraw;
    if (m_pDDraw)
    {
        m_pDDraw->AddRef();
    }

    return S_OK;
}

// BitmapSurface::GetPalette
// 
// Retrieve the palette of the given bitmap in a format palletable (ha ha) to
// DirectDraw.
//
HRESULT BitmapSurface::GetPalette(PALETTEENTRY pal[MAX_PALETTE_ENTRIES])
{
    BOOL                fPalAllocated = FALSE;
    RGBQUAD             *pRGB;
    UINT                nRGB;
    LPBITMAPINFOHEADER  pBitmapInfo;

    // For this we actually need to open the file or resource manually.
    //
    HRSRC hRsrc = FindResource(NULL, m_pstrName, RT_BITMAP);
    if (hRsrc)
    {
        HGLOBAL hGlobal = LoadResource(NULL, hRsrc);
        if (hGlobal == NULL)
        {
            TraceMsg("GetPalette: LoadResource", GetLastError());
            return E_FAIL;
        }
        
        LPVOID lpv = LockResource(hGlobal);
        if (lpv == NULL)
        {
            TraceMsg("GetPalette: LockResource", GetLastError());
            return E_FAIL;
        }
      
        pBitmapInfo = (LPBITMAPINFOHEADER)lpv;
        if (pBitmapInfo->biPlanes != 1 ||
            pBitmapInfo->biBitCount != 8 ||
            pBitmapInfo->biCompression != BI_RGB)
        {
            Trace(0, "GetPalette: Bitmap is invalid format");
            return E_INVALIDARG;
        }
        
        nRGB = pBitmapInfo->biClrUsed;
        if (!nRGB)
        {
            nRGB = MAX_PALETTE_ENTRIES;
        }
        
        pRGB = (RGBQUAD*)(((LPBYTE)lpv) + pBitmapInfo->biSize);        
    }
    else
    {
        HANDLE hFile;

        hFile = CreateFile(m_pstrName,
                           GENERIC_READ,
                           FILE_SHARE_READ,
                           NULL,                    // Security attributes
                           OPEN_EXISTING,
                           FILE_ATTRIBUTE_NORMAL,
                           NULL);                   // hTemplateFile
        if (hFile == NULL)
        {
            Trace(0, "SetPalette: Could not open \"%s\" as file or resource", m_pstrName);
            return E_FAIL;
        }

        BITMAPFILEHEADER bf;        
        BITMAPINFOHEADER bi;
        DWORD cb;

        if ((!ReadFile(hFile, (LPVOID)&bf, sizeof(bf), &cb, NULL) || cb != sizeof(bf)) ||
            (!ReadFile(hFile, (LPVOID)&bi, sizeof(bi), &cb, NULL) || cb != sizeof(bi)))
        {
            Trace(0, "SetPalette: Bitmap file is truncated");
            CloseHandle(hFile);
            return E_FAIL;
        }
        
        if (bf.bfType != BITMAP_TYPE)
        {
            Trace(0, "SetPalette: Bitmap is not a bitmap.");
            CloseHandle(hFile);
            return E_INVALIDARG;
        }

        if (bi.biPlanes != 1 ||
            bi.biBitCount != 8 ||
            bi.biCompression != BI_RGB)
        {
            Trace(0, "SetPalette: Bitmap file is invalid format.");
            return E_INVALIDARG;
        }
        
        nRGB = bi.biClrUsed;
        if (!nRGB)
        {
            nRGB = MAX_PALETTE_ENTRIES;
        }
        
        if (bi.biSize > sizeof(bi))
        {
            SetFilePointer(hFile, bi.biSize - sizeof(bi), NULL, FILE_CURRENT);
        }

        fPalAllocated = TRUE;
        pRGB = new RGBQUAD[nRGB];

        DWORD cbPal = sizeof(RGBQUAD) * nRGB;
        if (!ReadFile(hFile, (LPVOID)pRGB, cbPal, &cb, NULL) || cb != cbPal)
        {
            Trace(0, "SetPalette: Palette in bitmap file is truncated");
            delete[] pRGB;
            CloseHandle(hFile);
            return E_FAIL;
        }
        CloseHandle(hFile);
    }

    for (UINT idxPal = 0; idxPal < nRGB; idxPal++)
    {
        pal[idxPal].peRed   = pRGB[idxPal].rgbRed;
        pal[idxPal].peBlue  = pRGB[idxPal].rgbBlue;
        pal[idxPal].peGreen = pRGB[idxPal].rgbGreen;
        pal[idxPal].peFlags = 0;
    }

    ZeroMemory(&pal[idxPal], (MAX_PALETTE_ENTRIES - idxPal) * sizeof(RGBQUAD));
    
    if (fPalAllocated)
    {
        delete[] pRGB;
    }

    return S_OK;
}

// BitmapSurface::GetColorMatch
//
// Given a color reference, use GDI to find the nearest pixel value which 
// matches it. 
//
HRESULT BitmapSurface::GetColorMatch(
    COLORREF rgb,
    DWORD *pdwColor)
{
    COLORREF rgbT;
    HDC hdc;
    DWORD dw;
    HRESULT hr;

    //
    //  use GDI SetPixel to color match for us
    //
    hr = m_pSurface->GetDC(&hdc);
    if (FAILED(hr))
    {
        TraceMsg("Surface GetDC", hr);
        return hr;
    }

    if (SUCCEEDED(hr))
    {
        rgbT = GetPixel(hdc, 0, 0);             // save current pixel value
        SetPixel(hdc, 0, 0, rgb);               // set our value
        m_pSurface->ReleaseDC(hdc);
    }

    //
    // now lock the surface so we can read back the converted color
    //
    DDSURFACEDESC ddsd;
    ddsd.dwSize = sizeof(ddsd);
    while ((hr = m_pSurface->Lock(NULL, &ddsd, 0, NULL)) == DDERR_WASSTILLDRAWING)
        ;

    if (FAILED(hr))
    {
        TraceMsg("Lock surface", hr);
        return hr;
    }

    dw  = *(DWORD *)ddsd.lpSurface;                     
    if (ddsd.ddpfPixelFormat.dwRGBBitCount < 32)
       dw &= (1 << ddsd.ddpfPixelFormat.dwRGBBitCount)-1;  // mask it to bpp

    m_pSurface->Unlock(NULL);

    //
    //  now put the color that was there back.
    //
    hr = m_pSurface->GetDC(&hdc);
    if (FAILED(hr))
    {
        TraceMsg("Surface GetDC", hr);
        return hr;
    }

    SetPixel(hdc, 0, 0, rgbT);
    m_pSurface->ReleaseDC(hdc);

    *pdwColor = dw;
    return S_OK;
}

// BitmapSurface::LoadImage
//
// Perform a GDI LoadImage call on the file name or resource name and return
// the corresponding HBITMAP.
//
HBITMAP BitmapSurface::LoadImage()
{
    HBITMAP hbm;

    // Convert bitmap into a DIB section with the Win32 LoadImage API. First
    // try resource then file.
    //
    if ((hbm = (HBITMAP)::LoadImage(GetModuleHandle(NULL),
                                    m_pstrName,
                                    IMAGE_BITMAP,
                                    m_cx, m_cy,
                                    LR_CREATEDIBSECTION)) != NULL)
    {
        return hbm;
    }

    if ((hbm = (HBITMAP)::LoadImage(NULL,
                                    m_pstrName,
                                    IMAGE_BITMAP,
                                    m_cx, m_cy,
                                    LR_CREATEDIBSECTION | LR_LOADFROMFILE)) != NULL)
    {
        return hbm;
    }

    Trace(0, "Could not find \"%s\" as either bitmap or resource.", m_pstrName);
    return NULL;
}
