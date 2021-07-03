//-----------------------------------------------------------------------------
// File: Mipmap.cpp
//
// Desc: Example code showing how to do mipmap textures.
//
//       Note: This code uses the D3D Framework helper library, but it does NOT
//       use functionality from the D3DTextr.cpp file. Rather, all texture
//       functions are performed locally to illustrate the complete
//       implementation of mipmaps.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <stdio.h>
#include <math.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
// Structure to hold information for a mipmap texture
class CD3DMipMap
{
    HBITMAP              m_hbmBitmap[10];     // Bitmaps containing images
    DWORD                m_dwMipMapCount;     // Levels of mipmap
    LPDIRECTDRAWSURFACE7 m_pddsSurface;       // Surface of the mipmap

public:
    CD3DMipMap();

    HRESULT Create( CHAR** strMipmapNames );
    VOID    Delete();
    HRESULT Restore( LPDIRECT3DDEVICE7 pd3dDevice );
    VOID    Invalidate();

    LPDIRECTDRAWSURFACE7 GetSurface() { return m_pddsSurface; };
};





//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    D3DVERTEX   m_Mesh[4];     // Rendering vertices
    CD3DMipMap* m_pMipMap;     // The main mipmap

protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );
    HRESULT FinalCleanup();

    static HRESULT ConfirmDevice( DDCAPS*, D3DDEVICEDESC7* );
public:
    CMyD3DApplication();
};




//-----------------------------------------------------------------------------
// Name: WinMain()
// Desc: Entry point to the program. Initializes everything, and goes into a
//       message-processing loop. Idle time is used to render the scene.
//-----------------------------------------------------------------------------
INT WINAPI WinMain( HINSTANCE hInst, HINSTANCE, LPSTR strCmdLine, INT )
{
    CMyD3DApplication d3dApp;

    if( FAILED( d3dApp.Create( hInst, strCmdLine ) ) )
        return 0;

    return d3dApp.Run();
}




//-----------------------------------------------------------------------------
// Name: CMyD3DApplication()
// Desc: Application constructor. Sets attributes for the app.
//-----------------------------------------------------------------------------
CMyD3DApplication::CMyD3DApplication()
                  :CD3DApplication()
{
    m_strWindowTitle  = TEXT("Mipmap: Direct3D Mipmapping Sample");
    m_bAppUseZBuffer  = FALSE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    m_pMipMap         = new CD3DMipMap();
}





//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    CHAR* strMipmapNames[] = { "brick0", "brick1", "brick2", "brick3",
                               "brick4", NULL };

    // Create the mipmaps (loads bitmaps from "brickN" files)
    if( FAILED( m_pMipMap->Create( strMipmapNames ) ) )
    {
        MessageBox( NULL, TEXT( "Could not load bitmap images" ),
                    TEXT( "Mipmap" ), MB_ICONWARNING | MB_OK );
        DEBUG_MSG( "Could not load bitmap images" );
        return E_FAIL;
    }

    // Initialize mesh used to render the mipmaps
    m_Mesh[0] = D3DVERTEX( D3DVECTOR(-1,-1, 0), D3DVECTOR(0,0,1), 0.0f, 1.0f );
    m_Mesh[1] = D3DVERTEX( D3DVECTOR(-1, 1, 0), D3DVECTOR(0,0,1), 0.0f, 0.0f );
    m_Mesh[2] = D3DVERTEX( D3DVECTOR( 1,-1, 0), D3DVECTOR(0,0,1), 1.0f, 1.0f );
    m_Mesh[3] = D3DVERTEX( D3DVECTOR( 1, 1, 0), D3DVECTOR(0,0,1), 1.0f, 0.0f );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    D3DMATRIX matView;
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f, (FLOAT)(13*sin(fTimeKey)-15) );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: Render()
// Desc: Called once per frame, the call is the entry point for 3d
//       rendering. This function sets up render states, clears the
//       viewport, and renders the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::Render()
{
    // Clear the viewport
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET, 0x00004455, 0, 0 );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        D3DMATRIX matWorld;

        // Draw the left quad. Set renderstates for bilinear filtering
        D3DUtil_SetTranslateMatrix( matWorld, -1.1f, 0.0f, 0.0f );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MIPFILTER, D3DTFP_NONE );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_Mesh, 4, 0 );
    
        // Draw the right quad. Set renderstates for mipmapping
        D3DUtil_SetTranslateMatrix( matWorld, +1.1f, 0.0f, 0.0f );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MIPFILTER, D3DTFP_LINEAR );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_Mesh, 4, 0 );

        // End the scene.
        m_pd3dDevice->EndScene();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    // Build the mipmap device surfaces and textures.
    if( FAILED( m_pMipMap->Restore( m_pd3dDevice ) ) )
        return E_FAIL;

    m_pd3dDevice->SetTexture( 0, m_pMipMap->GetSurface() );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );

    // Set the transform matrices.
    D3DMATRIX matProj;
    D3DUtil_SetProjectionMatrix( matProj, 1.57f, 1.0f, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // This simple sample uses only ambient light
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0xffffffff );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    SAFE_DELETE( m_pMipMap );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Called when the app is exitting, or the device is being changed,
//       this function deletes any device dependant objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DeleteDeviceObjects()
{
    m_pMipMap->Invalidate();
    
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: ConfirmDevice()
// Desc: Called during device intialization, this code checks the device
//       for some minimum set of capabilities
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::ConfirmDevice( DDCAPS* pddDriverCaps,
                                          D3DDEVICEDESC7* pd3dDeviceDesc )
{
    // Get Triangle Caps
    LPD3DPRIMCAPS pdpcTriCaps = &pd3dDeviceDesc->dpcTriCaps;

    // Check Texture modes for MIPLINEAR MipMapping
    if( !( pdpcTriCaps->dwTextureFilterCaps & D3DPTFILTERCAPS_LINEARMIPLINEAR ) )
        return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CD3DMipMap()
// Desc: Constructs a mipmap object
//-----------------------------------------------------------------------------
CD3DMipMap::CD3DMipMap()
{
    m_pddsSurface = NULL;
}




//-----------------------------------------------------------------------------
// Name: MipMap_Create()
// Desc: Creates the mipmap structure and loads image data from bitmaps.
//-----------------------------------------------------------------------------
HRESULT CD3DMipMap::Create( CHAR** strMipMapNames )
{
    for( WORD wNum=0; wNum<10; wNum++ )
    {
        HBITMAP hbm = (HBITMAP)LoadImage( GetModuleHandle(NULL),
                                          strMipMapNames[wNum], IMAGE_BITMAP,
                                          0, 0, LR_CREATEDIBSECTION );
        if( NULL == hbm ) 
        {
            if( 0 == wNum )
                return E_FAIL;

            m_dwMipMapCount = wNum;
            return S_OK;
        }

        m_hbmBitmap[wNum] = hbm;
    }

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: TextureSearchCallback()
// Desc: Callback function used to enumerate texture formats.
//-----------------------------------------------------------------------------
HRESULT CALLBACK TextureSearchCallback( DDPIXELFORMAT* pddpf, VOID* param )
{
    DDSURFACEDESC2* pddsd = (DDSURFACEDESC2*)param;

    // Skip unwanted formats
    if( pddpf->dwRGBBitCount != pddsd->dwFlags )
        return DDENUMRET_OK;
    if( pddpf->dwFlags & (DDPF_LUMINANCE|DDPF_ALPHAPIXELS) )
        return DDENUMRET_OK;
    if( pddpf->dwFlags & (DDPF_BUMPLUMINANCE|DDPF_BUMPDUDV) )
        return DDENUMRET_OK;
    if( 0 != pddpf->dwFourCC )
        return DDENUMRET_OK;

    memcpy( &pddsd->ddpfPixelFormat, pddpf, sizeof(DDPIXELFORMAT) );
    return DDENUMRET_CANCEL;
}




//-----------------------------------------------------------------------------
// Name: MipMap_Restore()
// Desc: Creates the device-dependant surface and texture for the mipmap
//-----------------------------------------------------------------------------
HRESULT CD3DMipMap::Restore( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Release any previously created objects
    Invalidate();

    // Get size info for top level bitmap
    BITMAP bm; 
    GetObject( m_hbmBitmap[0], sizeof(BITMAP), &bm ); 

    // Set up and create the mipmap surface
    DDSURFACEDESC2 ddsd;
    ZeroMemory( &ddsd, sizeof(DDSURFACEDESC2) );
    ddsd.dwSize          = sizeof(DDSURFACEDESC2);
    ddsd.dwFlags         = DDSD_CAPS|DDSD_MIPMAPCOUNT|DDSD_WIDTH|DDSD_HEIGHT|
                           DDSD_PIXELFORMAT;
    ddsd.ddsCaps.dwCaps  = DDSCAPS_TEXTURE|DDSCAPS_MIPMAP|DDSCAPS_COMPLEX;
    ddsd.dwMipMapCount   = m_dwMipMapCount;
    ddsd.dwWidth         = bm.bmWidth;
    ddsd.dwHeight        = bm.bmHeight;

    // Get the device caps
    D3DDEVICEDESC7 ddDesc;
    DWORD         dwDeviceCaps;
    if( FAILED( pd3dDevice->GetCaps( &ddDesc ) ) )
        return E_FAIL;
    dwDeviceCaps = ddDesc.dpcTriCaps.dwTextureCaps;

    // Turn on texture management for hardware devices
    if( IsEqualIID(ddDesc.deviceGUID, IID_IDirect3DHALDevice) || IsEqualIID(ddDesc.deviceGUID, IID_IDirect3DTnLHalDevice) )
        ddsd.ddsCaps.dwCaps2 = DDSCAPS2_TEXTUREMANAGE;
    else
        ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;

    // Enumerate a good texture format. Search for a 16-bit format first
    DDSURFACEDESC2 ddsdSearch;
    ddsdSearch.dwFlags = 16;
    pd3dDevice->EnumTextureFormats( TextureSearchCallback, &ddsdSearch );
    
    // If that wasn't found, check for a 32-bit format
    if( 16 != ddsdSearch.ddpfPixelFormat.dwRGBBitCount )
    {
        ddsdSearch.dwFlags = 32;
        pd3dDevice->EnumTextureFormats( TextureSearchCallback, &ddsdSearch );
        if( 32 != ddsdSearch.ddpfPixelFormat.dwRGBBitCount )
            return E_FAIL;
    }

    // If we got a good texture format, use it to create the surface
    memcpy( &ddsd.ddpfPixelFormat, &ddsdSearch.ddpfPixelFormat,
            sizeof(DDPIXELFORMAT) );

    // Get a DDraw ptr (from the device's render target) for creating surfaces.
    // Note: the Release() calls just serve to decrement the ref count, but the
    // ptrs are still valid.
    LPDIRECTDRAWSURFACE7 pddsRender;
    LPDIRECTDRAW7        pDD  = NULL;
    pd3dDevice->GetRenderTarget( &pddsRender );
    pddsRender->GetDDInterface( (VOID**)&pDD );
    pddsRender->Release();

    // Create the mipmap surface
    if( FAILED( pDD->CreateSurface( &ddsd, &m_pddsSurface, NULL ) ) )
    {
        pDD->Release();
        return E_FAIL;
    }

    // Done with DDraw
    pDD->Release();

    // Loop through each surface in the mipmap, copying the bitmap to the temp
    // surface, and then blitting the temp surface to the real one.
    LPDIRECTDRAWSURFACE7 pddsDest = m_pddsSurface;

    for( WORD wNum=0; wNum < m_dwMipMapCount; wNum++ )
    {
        // Copy the bitmap image to the surface
        BITMAP bm; 
        GetObject( m_hbmBitmap[wNum], sizeof(BITMAP), &bm ); 

        // Create a DC and setup the bitmap
        HDC hdcBitmap = CreateCompatibleDC( NULL );
        if( NULL == hdcBitmap )
            return E_FAIL;

        SelectObject( hdcBitmap, m_hbmBitmap[wNum] );

        HDC hdcSurface;
        if( SUCCEEDED( pddsDest->GetDC( &hdcSurface ) ) )
        {
            BitBlt( hdcSurface, 0, 0, bm.bmWidth, bm.bmHeight, hdcBitmap,
                    0, 0, SRCCOPY );
            pddsDest->ReleaseDC( hdcSurface );
        }

        DeleteDC( hdcBitmap );

        // Get the next surface in the chain. Do a Release() call, though, to
        // avoid increasing the ref counts on the surfaces.
        DDSCAPS2 ddsCaps;
        ddsCaps.dwCaps  = DDSCAPS_TEXTURE | DDSCAPS_MIPMAP;
        ddsCaps.dwCaps2 = 0;
        ddsCaps.dwCaps3 = 0;
        ddsCaps.dwCaps4 = 0;
        if( SUCCEEDED( pddsDest->GetAttachedSurface( &ddsCaps, &pddsDest ) ) )
            pddsDest->Release();
    }

    // For palettized textures, use the bitmap and GetDIBColorTable() to build
    // and attach a palette to the surface here.

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: MipMap_Invalidate()
// Desc: Frees device dependant objects for the mipmap
//-----------------------------------------------------------------------------
VOID CD3DMipMap::Invalidate()
{
    SAFE_RELEASE( m_pddsSurface );
}




//-----------------------------------------------------------------------------
// Name: Mipmap_Delete()
// Desc: Frees device dependant objects for the mipmap
//-----------------------------------------------------------------------------
VOID CD3DMipMap::Delete()
{
    for( WORD wNum = 0; wNum < m_dwMipMapCount; wNum++ )
        DeleteObject( m_hbmBitmap[wNum] );
}





