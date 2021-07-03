//-----------------------------------------------------------------------------
// File: Texture.cpp
//
// Desc: Code to show how to create textures. Note that the texture code here
//       is very simplified, in that it only loads 16-bit textures and does not
//       do a great job of managing the textures.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved
//-----------------------------------------------------------------------------

#define STRICT
#include <windows.h>
#include <d3d.h>




//-----------------------------------------------------------------------------
// Name: TextureSearchCallback()
// Desc: Enumeration callback routine to find a 16-bit texture format. This
//       function is invoked by the ID3DDevice::EnumTextureFormats() function
//       to sort through all the available texture formats for a given device.
//       The pixel format of each enumerated texture format is passed into the
//       "pddpf" parameter. The 2nd parameter is to be used however the app
//       sees fit. In this case, we are using it as an output parameter to 
//       return a normal 16-bit texture format.
//-----------------------------------------------------------------------------
static HRESULT CALLBACK TextureSearchCallback( DDPIXELFORMAT* pddpf,
                                               VOID* param )
{
    // Note: Return with DDENUMRET_OK to continue enumerating more formats.

    // Skip any funky modes
    if( pddpf->dwFlags & (DDPF_LUMINANCE|DDPF_BUMPLUMINANCE|DDPF_BUMPDUDV) )
        return DDENUMRET_OK;
    
    // Skip any FourCC formats
    if( pddpf->dwFourCC != 0 )
        return DDENUMRET_OK;

    // Skip alpha modes
    if( pddpf->dwFlags&DDPF_ALPHAPIXELS )
        return DDENUMRET_OK;

    // We only want 16-bit formats, so skip all others
    if( pddpf->dwRGBBitCount != 16 )
        return DDENUMRET_OK;

    // We found a good match. Copy the current pixel format to our output
    // parameter
    memcpy( (DDPIXELFORMAT*)param, pddpf, sizeof(DDPIXELFORMAT) );

    // Return with DDENUMRET_CANCEL to end enumeration.
    return DDENUMRET_CANCEL;
}




//-----------------------------------------------------------------------------
// Name: CreateTextureFromBitmap()
// Desc: Use a bitmap to create a texture for the specified device. This code
//       gets the attributes of the texture from the bitmap, creates the
//       texture, and then copies the bitmap into the texture.
//-----------------------------------------------------------------------------
static LPDIRECTDRAWSURFACE7 CreateTextureFromBitmap( LPDIRECT3DDEVICE7 pd3dDevice,
                                                     HBITMAP hbm )
{
    LPDIRECTDRAWSURFACE7 pddsTexture;
    HRESULT hr;

    // Get the device caps so we can check if the device has any constraints
    // when using textures
    D3DDEVICEDESC7 ddDesc;
    if( FAILED( pd3dDevice->GetCaps( &ddDesc ) ) )
        return NULL;

    // Get the bitmap structure (to extract width, height, and bpp)
    BITMAP bm;
    GetObject( hbm, sizeof(BITMAP), &bm );
    DWORD dwWidth  = (DWORD)bm.bmWidth;
    DWORD dwHeight = (DWORD)bm.bmHeight;

    // Setup the new surface desc for the texture. Note how we are using the
    // texture manage attribute, so Direct3D does alot of dirty work for us
    DDSURFACEDESC2 ddsd;
    ZeroMemory( &ddsd, sizeof(DDSURFACEDESC2) );
    ddsd.dwSize          = sizeof(DDSURFACEDESC2);
    ddsd.dwFlags         = DDSD_CAPS|DDSD_HEIGHT|DDSD_WIDTH|
                           DDSD_PIXELFORMAT|DDSD_TEXTURESTAGE;
    ddsd.ddsCaps.dwCaps  = DDSCAPS_TEXTURE;
    ddsd.dwWidth         = dwWidth;
    ddsd.dwHeight        = dwHeight;

    // Turn on texture management for hardware devices
    if( ddDesc.deviceGUID == IID_IDirect3DHALDevice )
        ddsd.ddsCaps.dwCaps2 = DDSCAPS2_TEXTUREMANAGE;
    else if( ddDesc.deviceGUID == IID_IDirect3DTnLHalDevice )
        ddsd.ddsCaps.dwCaps2 = DDSCAPS2_TEXTUREMANAGE;
    else
        ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;

    // Adjust width and height, if the driver requires it
    if( ddDesc.dpcTriCaps.dwTextureCaps & D3DPTEXTURECAPS_POW2 )
    {
        for( ddsd.dwWidth=1;  dwWidth>ddsd.dwWidth;   ddsd.dwWidth<<=1 );
        for( ddsd.dwHeight=1; dwHeight>ddsd.dwHeight; ddsd.dwHeight<<=1 );
    }
    if( ddDesc.dpcTriCaps.dwTextureCaps & D3DPTEXTURECAPS_SQUAREONLY )
    {
        if( ddsd.dwWidth > ddsd.dwHeight ) ddsd.dwHeight = ddsd.dwWidth;
        else                               ddsd.dwWidth  = ddsd.dwHeight;
    }

    // Enumerate the texture formats, and find the closest device-supported
    // texture pixel format. The TextureSearchCallback function for this
    // tutorial is simply looking for a 16-bit texture. Real apps may be
    // interested in other formats, for alpha textures, bumpmaps, etc..
    pd3dDevice->EnumTextureFormats( TextureSearchCallback, &ddsd.ddpfPixelFormat );
    if( 0L == ddsd.ddpfPixelFormat.dwRGBBitCount )
        return NULL;

    // Get the device's render target, so we can then use the render target to
    // get a ptr to a DDraw object. We need the DirectDraw interface for
    // creating surfaces.
    LPDIRECTDRAWSURFACE7 pddsRender;
    LPDIRECTDRAW7        pDD;
    pd3dDevice->GetRenderTarget( &pddsRender );
    pddsRender->GetDDInterface( (VOID**)&pDD );
    pddsRender->Release();

    // Create a new surface for the texture
    if( FAILED( hr = pDD->CreateSurface( &ddsd, &pddsTexture, NULL ) ) )
    {
        pDD->Release();
        return NULL;
    }

    // Done with DDraw
    pDD->Release();

    // Now, copy the bitmap to the texture surface. To do this, we are creating
    // a DC for the bitmap and a DC for the surface, so we can use the BitBlt()
    // call to copy the actual bits.

    // Get a DC for the bitmap
    HDC hdcBitmap = CreateCompatibleDC( NULL );
    if( NULL == hdcBitmap )
    {
        pddsTexture->Release();
        return NULL;
    }
    SelectObject( hdcBitmap, hbm );

    // Get a DC for the surface
    HDC hdcTexture;
    if( SUCCEEDED( pddsTexture->GetDC( &hdcTexture ) ) )
    {
        // Copy the bitmap image to the surface.
        BitBlt( hdcTexture, 0, 0, bm.bmWidth, bm.bmHeight, hdcBitmap,
                0, 0, SRCCOPY );
        pddsTexture->ReleaseDC( hdcTexture );
    }
    DeleteDC( hdcBitmap );

    // Return the newly created texture
    return pddsTexture;
}




//-----------------------------------------------------------------------------
// Name: CreateTexture()
// Desc: Is passed a filename and creates a local Bitmap from that file. Some
//       logic and file parsing code could go here to support other image
//       file formats.
//-----------------------------------------------------------------------------
LPDIRECTDRAWSURFACE7 CreateTexture( LPDIRECT3DDEVICE7 pd3dDevice, 
                                    CHAR* strName )
{
    // Create a bitmap and load the texture file into it. Check the 
    // executable's resource first.
    HBITMAP hbm = (HBITMAP)LoadImage( GetModuleHandle(NULL), strName, 
                              IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
    if( NULL == hbm )
    {
        // If not in the resource, try to load the bitmap as a file. Real code
        // would try to find the bitmap among many file paths.
        hbm = (HBITMAP)LoadImage( NULL, strName, IMAGE_BITMAP, 0, 0, 
                                  LR_LOADFROMFILE|LR_CREATEDIBSECTION );
        if( NULL == hbm )
            return NULL;
    }

    // The actual work of creating the texture is done in this next function.
    return CreateTextureFromBitmap( pd3dDevice, hbm );
}




