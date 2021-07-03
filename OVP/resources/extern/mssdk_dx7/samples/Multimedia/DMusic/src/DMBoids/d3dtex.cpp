/*
**-----------------------------------------------------------------------------
**  File:       d3dtex.h
**  Purpose:    
**  Notes:
**
**  Copyright (c) 1995-1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/

/**************************************************************************
 D3DTexture class

 **************************************************************************/
#define D3D_OVERLOADS

#include <windows.h>
#include <windowsx.h>
#include <ddraw.h>
#include <d3d.h>

#include "d3dtex.h"

/**************************************************************************
 DEBUG junk
 **************************************************************************/
#if defined(DEBUG) || defined(_DEBUG)
static void DPF(char *fmt, ...)
{
    char ach[128];
    va_list va;
    va_start( va, fmt );
    wvsprintf( ach, fmt, va );
    va_end( va );
    OutputDebugString("TEXTURE: ");
    OutputDebugString(ach);
    OutputDebugString("\r\n");
}
#else
#define DPF if (0) 
#endif

/**************************************************************************
 ChooseTextureFormat
 **************************************************************************/

typedef struct {
    DWORD           bpp;        // we want a texture format of this bpp
    DDPIXELFORMAT   ddpf;       // place the format here
}   FindTextureData;

HRESULT CALLBACK FindTextureCallback(DDSURFACEDESC *DeviceFmt, LPVOID lParam)
{
    FindTextureData * FindData = (FindTextureData *)lParam;
    DDPIXELFORMAT ddpf = DeviceFmt->ddpfPixelFormat;

    DPF("FindTexture: %d %s%s%s %08X %08X %08X %08X", 
	ddpf.dwRGBBitCount, 
        (ddpf.dwFlags & (DDPF_ALPHA|DDPF_ALPHAPIXELS)) ? "ALPHA " : "", 
        (ddpf.dwFlags &	(DDPF_RGB)) ? "RGB " : "", 
        (ddpf.dwFlags &	(DDPF_PALETTEINDEXED8 | DDPF_PALETTEINDEXED4)) ? "PAL " : "", 
	ddpf.dwRBitMask,
	ddpf.dwGBitMask,
	ddpf.dwBBitMask,
	ddpf.dwRGBAlphaBitMask);

    //
    // we use GetDC/BitBlt to init textures so we only
    // want to use formats that GetDC will support.
    //
    if (ddpf.dwFlags & (DDPF_ALPHA|DDPF_ALPHAPIXELS))
        return DDENUMRET_OK;

    if (ddpf.dwRGBBitCount <= 8 &&
        !(ddpf.dwFlags & (DDPF_PALETTEINDEXED8 | DDPF_PALETTEINDEXED4)))
        return DDENUMRET_OK;

    if (ddpf.dwRGBBitCount > 8 && !(ddpf.dwFlags & DDPF_RGB))
        return DDENUMRET_OK;

    //
    // GetDC does not work for 1 or 4 bpp
    //
    if (ddpf.dwRGBBitCount < 8)
        return DDENUMRET_OK;

    //
    // keep the texture format that is nearest to the bitmap we have
    //
    if (FindData->ddpf.dwRGBBitCount == 0 ||
       (ddpf.dwRGBBitCount >= FindData->bpp &&
       (UINT)(ddpf.dwRGBBitCount - FindData->bpp) < (UINT)(FindData->ddpf.dwRGBBitCount - FindData->bpp)))
    {
        FindData->ddpf = ddpf;
    }

    return DDENUMRET_OK;
}

void ChooseTextureFormat(IDirect3DDevice2 *Device, DWORD bpp, DDPIXELFORMAT *pddpf)
{
    FindTextureData FindData;
    ZeroMemory(&FindData, sizeof(FindData));
    FindData.bpp = bpp;
    Device->EnumTextureFormats(FindTextureCallback, (LPVOID)&FindData);
    *pddpf = FindData.ddpf;

    DPF("ChooseTexture: %d %s%s%s %08X %08X %08X %08X", 
	pddpf->dwRGBBitCount, 
        (pddpf->dwFlags & (DDPF_ALPHA|DDPF_ALPHAPIXELS)) ? "ALPHA " : "", 
        (pddpf->dwFlags &	(DDPF_RGB)) ? "RGB " : "", 
        (pddpf->dwFlags &	(DDPF_PALETTEINDEXED8 | DDPF_PALETTEINDEXED4)) ? "PAL " : "", 
	pddpf->dwRBitMask,
	pddpf->dwGBitMask,
	pddpf->dwBBitMask,
	pddpf->dwRGBAlphaBitMask);
}

/**************************************************************************
 PaletteFromBitmap
 **************************************************************************/

static IDirectDrawPalette * PaletteFromBitmap(IDirectDraw *DirectDraw, HBITMAP Bitmap)
{
    IDirectDrawPalette *    Palette = NULL;
    HDC                     BitmapDC;
    DWORD                   adw[256];
    int                     colors, i;

    //
    // get the color table from the DIBSection
    //
    BitmapDC = CreateCompatibleDC(NULL);
    SelectObject(BitmapDC, Bitmap);
    colors = GetDIBColorTable(BitmapDC, 0, 256, (RGBQUAD *)adw);
    DeleteDC(BitmapDC);

    if (colors != 0)
    {
        //
        // convert BGR to RGB
        //
        for (i=0; i<colors; i++)
            adw[i] = RGB(GetBValue(adw[i]),GetGValue(adw[i]),GetRValue(adw[i]));

        //
        // create a DirectDraw palette for the texture.
        //
        DirectDraw->CreatePalette(colors <= 16 ? DDPCAPS_4BIT : DDPCAPS_8BIT,
            (PALETTEENTRY *)adw, &Palette, NULL);
    }

    return Palette;
}

/**************************************************************************
 GetDD

 get the IDirectDraw from a IDirect3DDevice, we need the DD
 to create surfaces.

 this function does not do a AddRef on the DirectDraw object
 (ie you dont need to Release)

 NOTE if your app has this around as a global you can use
 the global instead of this code.

 **************************************************************************/
static IDirectDraw * GetDD(IDirect3DDevice2 *Device)
{
    IDirectDraw *       DirectDraw;
    IDirectDrawSurface *Target;
    IDirectDrawSurface2*Target2;

    //
    // get the render target (we need it to get the IDirectDraw)
    //
    if (Device==NULL || Device->GetRenderTarget(&Target) != DD_OK)
        return NULL;

    //
    // get the DirectDraw object, but first we need a IDirectDrawSurface2
    //
    if (Target->QueryInterface(IID_IDirectDrawSurface2, (void**)&Target2) != DD_OK)
        return NULL;
    Target->Release();
    Target2->GetDDInterface((void**)&DirectDraw);
    Target2->Release();
    DirectDraw->Release();  // dont up ref count

    return DirectDraw;
}

/**************************************************************************
 D3DTexture::Load

 load a bitmap from the resource, or bmp file and create a
 D3D texture map

 **************************************************************************/

BOOL D3DTexture::Load(IDirect3DDevice2 *Device, char *BitmapName, int ColorKeyIndex)
{
    BITMAP              bm;
    DDSURFACEDESC       ddsd;
    HBITMAP             Bitmap;
    IDirectDraw *       DirectDraw;
	DDCOLORKEY			ColorKey;

    DPF("D3DTexture::Load(%s)", BitmapName);

    //
    // we need a IDirectDraw so we can create a surface.
    //
    if ((DirectDraw = GetDD(Device)) == NULL)
        return FALSE;

    //
    // load the bitmap from a resource or file.
    //
    Bitmap = (HBITMAP)LoadImage(GetModuleHandle(NULL), BitmapName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);

    if (Bitmap == NULL)
        Bitmap = (HBITMAP)LoadImage(NULL, BitmapName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE|LR_CREATEDIBSECTION);

    if (Bitmap == NULL)
        return FALSE;

    // free any existing texture.
    Release();

    GetObject(Bitmap, sizeof(bm), &bm);      // get size of bitmap

    //
    // find the best texture format to use.
    //
    ZeroMemory(&ddsd, sizeof(ddsd));
    ddsd.dwSize = sizeof(ddsd);
    ChooseTextureFormat(Device, bm.bmBitsPixel, &ddsd.ddpfPixelFormat);
    ddsd.dwFlags |= DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH | DDSD_PIXELFORMAT;
    ddsd.dwWidth = bm.bmWidth;
    ddsd.dwHeight = bm.bmHeight;

    //
    // create a video memory texture
    //
    // if we are dealing with a HAL create in video memory, else
    // create in system memory.
    //
    D3DDEVICEDESC hal, hel;
    ZeroMemory(&hal, sizeof(hal));
    hal.dwSize = sizeof(hal);
    ZeroMemory(&hel, sizeof(hel));
    hel.dwSize = sizeof(hel);
    Device->GetCaps(&hal, &hel);

    // BUGBUG should we check for texture caps?
    if (hal.dcmColorModel)
        ddsd.ddsCaps.dwCaps = DDSCAPS_VIDEOMEMORY | DDSCAPS_TEXTURE | DDSCAPS_ALLOCONLOAD;
    else
        ddsd.ddsCaps.dwCaps = DDSCAPS_SYSTEMMEMORY| DDSCAPS_TEXTURE;

    if (DirectDraw->CreateSurface(&ddsd, &DeviceSurface, NULL) != DD_OK)
        goto error;

    //
    // create a system memory surface for the texture.
    //
    // we use this system memory surface for the ::Load call
    // and this surface does not get lost.
    //
    if (hal.dcmColorModel)
    {
        ddsd.ddsCaps.dwCaps = DDSCAPS_SYSTEMMEMORY | DDSCAPS_TEXTURE;

        if (DirectDraw->CreateSurface(&ddsd, &MemorySurface, NULL) != DD_OK)
            goto error;
    }
    else
    {
        //
        // when dealing with a SW rasterizer we dont need to make two
        // surfaces.
        //
        MemorySurface = DeviceSurface;
        DeviceSurface->AddRef();
    }

	//
	// if the user specified a colorkey, set it
	//
	if (ColorKeyIndex != -1) {
		ColorKey.dwColorSpaceLowValue = ColorKeyIndex;
		ColorKey.dwColorSpaceHighValue = ColorKeyIndex;

		DeviceSurface->SetColorKey(DDCKEY_SRCBLT, &ColorKey);
	}

    //
    // create a palette for the texture
    //
    if (ddsd.ddpfPixelFormat.dwRGBBitCount <= 8)
    {
        Palette = PaletteFromBitmap(DirectDraw, Bitmap);

        //
        // now we have a palette, attach the palette to the Surface
        // and the texture
        //
        MemorySurface->SetPalette(Palette);
        DeviceSurface->SetPalette(Palette);
    }

    //
    // copy the bitmap to our surface
    //
    if (!Copy(Bitmap))
        goto error;

    //
    // get the texture handle
    //
    IDirect3DTexture2 *	Texture;
    DeviceSurface->QueryInterface(IID_IDirect3DTexture2, (void**)&Texture);
    Texture->GetHandle(Device, &Handle);
    Texture->Release();

    //
    // we are done, delete the bitmap (we made a copy) and return.
    //
    DeleteObject(Bitmap);
    return TRUE;

error:
    if (Bitmap)
        DeleteObject(Bitmap);
    Release();
    return FALSE;
}

/**************************************************************************
 D3DTexture::Copy

 init the system memory surface from a GDI Bitmap

 **************************************************************************/

BOOL D3DTexture::Copy(HBITMAP Bitmap)
{
    BITMAP  bm;
    HDC     BitmapDC;
    HDC     SurfaceDC;
    BOOL    result = FALSE;

    DPF("D3DTexture::Copy");

    GetObject(Bitmap, sizeof(bm), &bm);    // get size of bitmap

    BitmapDC = CreateCompatibleDC(NULL);
    SelectObject(BitmapDC, Bitmap);

    if (MemorySurface->GetDC(&SurfaceDC) == DD_OK)
    {
        BitBlt(SurfaceDC, 0, 0, bm.bmWidth, bm.bmHeight, BitmapDC, 0, 0, SRCCOPY);
        MemorySurface->ReleaseDC(SurfaceDC);
        result = Restore();
    }
    DeleteDC(BitmapDC);
    return result;
}


/**************************************************************************
 D3DTexture::Restore

 restore the texture image

 **************************************************************************/

BOOL D3DTexture::Restore()
{
    HRESULT             err;
    IDirect3DTexture2  *MemoryTexture;
    IDirect3DTexture2  *DeviceTexture;

    DPF("D3DTexture::Restore");

    if (DeviceSurface == NULL || MemorySurface == NULL)
        return FALSE;

    //
    // we dont need to do this step for system memory surfaces.
    //
    if (DeviceSurface == MemorySurface)
        return TRUE;

    //
    // restore the video memory texture.
    //
    if (DeviceSurface->Restore() != DD_OK)
        return FALSE;

    //
    // call IDirect3DTexture::Load() to copy the texture to the device.
    //
    DeviceSurface->QueryInterface(IID_IDirect3DTexture2, (void**)&DeviceTexture);
    MemorySurface->QueryInterface(IID_IDirect3DTexture2, (void**)&MemoryTexture);

    err = DeviceTexture->Load(MemoryTexture);

    DeviceTexture->Release();
    MemoryTexture->Release();

    return err == DD_OK;
}

/**************************************************************************
 D3DTexture::Release

 free the texture, free all objects associated with this texture

 NOTE we cant do this in the destructor because DirectDraw
 will clean up all the surfaces when the main direct draw object
 is destroyed, before a static destructor will be called.

 **************************************************************************/

void D3DTexture::Release()
{
    DPF("D3DTexture::Release");

    if (MemorySurface)
       MemorySurface->Release();
    MemorySurface = 0;

    if (DeviceSurface)
       DeviceSurface->Release();
    DeviceSurface = 0;

    if (Palette)
       Palette->Release();
    Palette = 0;

    Handle = 0;
}
