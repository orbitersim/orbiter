//-----------------------------------------------------------------------------
// File: BumpEarth.cpp
//
// Desc: Direct3D environment mapping / bump mapping sample. The technique
//       used perturbs the environment map to simulate bump mapping.
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define D3D_OVERLOADS
#define STRICT
#include <math.h>
#include <stdio.h>
#include "D3DApp.h"
#include "D3DUtil.h"
#include "D3DTextr.h"
#include "D3DMath.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
enum BUMPMAPFORMAT { BUMPMAP_U5V5L6, BUMPMAP_U8V8L8, BUMPMAP_U8V8 };

// Vertex with 2nd set of tex coords (for environment map)
struct BUMPVERTEX
{
    D3DVERTEX v;
    FLOAT     tu2, tv2;
};

// Converts a FLOAT to a DWORD for use in SetRenderState() calls
inline DWORD F2DW( FLOAT f ) { return *((DWORD*)&f); }




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    LPDIRECTDRAWSURFACE7 m_pddsBumpMap;

    BUMPMAPFORMAT m_BumpMapFormat;
    DWORD         m_dwNumSphereVertices;
    BUMPVERTEX*   m_pSphereVertices;
    BOOL          m_bHighTesselation;
    BOOL          m_bTextureOn;
    BOOL          m_bBumpMapOn;
    BOOL          m_bEnvMapOn;

    VOID    SetMenuStates();
    VOID    InitSphereVertices();
    VOID    ApplyEnvironmentMap( BUMPVERTEX* pv, DWORD dwNumVertices );
    HRESULT InitBumpMap( BUMPMAPFORMAT format,
                         LPDIRECTDRAWSURFACE7 pddsBumpSrc );
    static HRESULT ConfirmDevice( DDCAPS* pddDriverCaps,
                                  D3DDEVICEDESC7* pd3dDeviceDesc );
protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );
    HRESULT FinalCleanup();

public:
    CMyD3DApplication();

    LRESULT MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
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
    m_strWindowTitle   = TEXT("BumpEarth: Direct3D BumpMapping Demo");
    m_bAppUseZBuffer   = FALSE;
    m_bShowStats       = TRUE;
    m_fnConfirmDevice  = ConfirmDevice;

    m_pddsBumpMap      = NULL;
    m_pSphereVertices  = NULL;
    m_bTextureOn       = TRUE;
    m_bBumpMapOn       = TRUE;
    m_bEnvMapOn        = TRUE;
    m_bHighTesselation = TRUE;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Initialize geometry
    InitSphereVertices();

    // Load texture maps
    D3DTextr_CreateTextureFromFile( "block.bmp" );
    D3DTextr_CreateTextureFromFile( "earth.bmp" );
    D3DTextr_CreateTextureFromFile( "earthbump.bmp", 0, D3DTEXTR_32BITSPERPIXEL );
    D3DTextr_CreateTextureFromFile( "EarthEnvMap.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: ApplyEnvironmentMap()
// Desc: Performs a calculation on each of the vertices' normals to determine
//       what the texture coordinates should be for the environment map.
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::ApplyEnvironmentMap( BUMPVERTEX* pv,
                                             DWORD dwNumVertices )
{
    // Get the World-View(WV) and Projection(P) matrices
    D3DMATRIX W, V, WV;
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_VIEW,      &V );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD,     &W );
    m_pd3dDevice->MultiplyTransform( D3DTRANSFORMSTATE_VIEW, &W );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_VIEW,      &WV );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,      &V );

    // Loop through the vertices, transforming each one and calculating
    // the correct texture coordinates.
    for( WORD i = 0; i < dwNumVertices; i++ )
    {
        FLOAT nx = pv[i].v.nx;
        FLOAT ny = pv[i].v.ny;
        FLOAT nz = pv[i].v.nz;
    
        FLOAT nxv = nx*WV._11 + ny*WV._21 + nz*WV._31 + WV._41;
        FLOAT nyv = nx*WV._12 + ny*WV._22 + nz*WV._32 + WV._42;
        FLOAT nzv = nx*WV._13 + ny*WV._23 + nz*WV._33 + WV._43;
        FLOAT nwv = nx*WV._14 + ny*WV._24 + nz*WV._34 + WV._44;

        FLOAT nlen = (FLOAT)sqrt( nxv*nxv + nyv*nyv + nzv*nzv );

        pv[i].v.tu = 0.5f + 1.37f*nxv/nlen;
        pv[i].v.tv = 0.5f - 1.37f*nyv/nlen;
    }
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Animates the scene
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Rotate and scale planet
    D3DMATRIX matWorld, matRotate, matScale;
    D3DUtil_SetRotateYMatrix( matRotate, -fTimeKey/3.0f );
    D3DUtil_SetScaleMatrix( matScale, 0.2857f, 0.2857f, 0.2857f );
    D3DMath_MatrixMultiply( matWorld, matScale, matRotate );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
    
    // Apply the environment map    
    ApplyEnvironmentMap( m_pSphereVertices, m_dwNumSphereVertices );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: Render()
// Desc: Renders the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::Render()
{
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET, 0x00000000, 1.0f, 0L );

    if( FAILED( m_pd3dDevice->BeginScene() ) )
        return S_OK; // Don't return a "fatal" error

    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_WRAP0, D3DWRAP_U | D3DWRAP_V );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP, D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAOP, D3DTOP_SELECTARG1 );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP, D3DTOP_DISABLE );

    if( m_bTextureOn )
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface( "earth.bmp" ) );
    else
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface( "block.bmp" ) );
    
    m_pd3dDevice->SetTextureStageState(0, D3DTSS_TEXCOORDINDEX, 1 );
    m_pd3dDevice->SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_SELECTARG1);
    m_pd3dDevice->SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );

    if( m_bBumpMapOn )
    {
        m_pd3dDevice->SetTexture( 1, m_pddsBumpMap );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_TEXCOORDINDEX, 1 );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP, D3DTOP_BUMPENVMAPLUMINANCE );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG1, D3DTA_TEXTURE );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG2, D3DTA_CURRENT );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_BUMPENVMAT00, F2DW(0.5f) );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_BUMPENVMAT01, F2DW(0.0f) );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_BUMPENVMAT10, F2DW(0.0f) );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_BUMPENVMAT11, F2DW(0.5f) );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_BUMPENVLSCALE, F2DW(1.0f) );
        m_pd3dDevice->SetTextureStageState( 1, D3DTSS_BUMPENVLOFFSET, F2DW(0.0f) );

        if( m_bEnvMapOn )
        {
            m_pd3dDevice->SetTexture( 2, D3DTextr_GetSurface( "EarthEnvMap.bmp" ) );
            m_pd3dDevice->SetTextureStageState( 2, D3DTSS_TEXCOORDINDEX, 0 );
            m_pd3dDevice->SetTextureStageState( 2, D3DTSS_COLOROP, D3DTOP_ADD );
            m_pd3dDevice->SetTextureStageState( 2, D3DTSS_COLORARG1, D3DTA_TEXTURE );
            m_pd3dDevice->SetTextureStageState( 2, D3DTSS_COLORARG2, D3DTA_CURRENT );
        }
        else
            m_pd3dDevice->SetTextureStageState( 2, D3DTSS_COLOROP, D3DTOP_DISABLE );
    }
    else
    {
        if( m_bEnvMapOn )
        {
            m_pd3dDevice->SetTexture( 1, D3DTextr_GetSurface( "EarthEnvMap.bmp" ) );
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_TEXCOORDINDEX, 0 );
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP, D3DTOP_ADD );
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG1, D3DTA_TEXTURE );
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG2, D3DTA_CURRENT );
        }
        else
            m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP, D3DTOP_DISABLE );
        
        m_pd3dDevice->SetTextureStageState( 2, D3DTSS_COLOROP, D3DTOP_DISABLE );
    }

    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP,
                                 D3DFVF_XYZ|D3DFVF_NORMAL|D3DFVF_TEX2,
                                 m_pSphereVertices, m_dwNumSphereVertices, 0x0 );

    m_pd3dDevice->EndScene();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitBumpMap()
// Desc: Copies raw bits from _lpBumpSrc into the type of bump map requested
//       as 'format' into pBumpMap.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitBumpMap( BUMPMAPFORMAT format,
                                        LPDIRECTDRAWSURFACE7 pddsBumpSrc )
{
    // Clean up old bumpmap
    SAFE_RELEASE( m_pddsBumpMap );

    // Check parameters
    if( NULL == pddsBumpSrc )
        return E_FAIL;

    // Create the bump map surface. Surface desc depends on bump format
    DDSURFACEDESC2 ddsd; 
    ddsd.dwSize = sizeof(ddsd);
    pddsBumpSrc->GetSurfaceDesc( &ddsd );
    ddsd.dwFlags                 = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH |
                                   DDSD_PIXELFORMAT | DDSD_TEXTURESTAGE;
    ddsd.ddsCaps.dwCaps          = DDSCAPS_TEXTURE;
    ddsd.ddsCaps.dwCaps2         = 0L;
    ddsd.ddsCaps.dwCaps3         = 0L;
    ddsd.ddsCaps.dwCaps4         = 0L;
    ddsd.dwTextureStage          = 0L;
    ddsd.ddpfPixelFormat.dwSize  = sizeof(DDPIXELFORMAT);
    ddsd.ddpfPixelFormat.dwFlags = DDPF_BUMPDUDV;

    // Turn on texture management for HW devices
    if( m_pDeviceInfo->bHardware )
        ddsd.ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;
    else
        ddsd.ddsCaps.dwCaps  |= DDSCAPS_SYSTEMMEMORY;

    switch( format )
    {
        case BUMPMAP_U8V8:
            ddsd.ddpfPixelFormat.dwBumpBitCount         = 16;
            ddsd.ddpfPixelFormat.dwBumpDuBitMask        = 0x000000ff;
            ddsd.ddpfPixelFormat.dwBumpDvBitMask        = 0x0000ff00;
            ddsd.ddpfPixelFormat.dwBumpLuminanceBitMask = 0x00000000;
            break;

        case BUMPMAP_U5V5L6:
            ddsd.ddpfPixelFormat.dwFlags |= DDPF_BUMPLUMINANCE;
            ddsd.ddpfPixelFormat.dwBumpBitCount         = 16;
            ddsd.ddpfPixelFormat.dwBumpDuBitMask        = 0x0000001f;
            ddsd.ddpfPixelFormat.dwBumpDvBitMask        = 0x000003e0;
            ddsd.ddpfPixelFormat.dwBumpLuminanceBitMask = 0x0000fc00;
            break;

        case BUMPMAP_U8V8L8:
            ddsd.ddpfPixelFormat.dwFlags |= DDPF_BUMPLUMINANCE;
            ddsd.ddpfPixelFormat.dwBumpBitCount         = 24;
            ddsd.ddpfPixelFormat.dwBumpDuBitMask        = 0x000000ff;
            ddsd.ddpfPixelFormat.dwBumpDvBitMask        = 0x0000ff00;
            ddsd.ddpfPixelFormat.dwBumpLuminanceBitMask = 0x00ff0000;
            break;
    }

    // Create the bumpmap's surface and texture objects
    if( FAILED( m_pDD->CreateSurface( &ddsd, &m_pddsBumpMap, NULL ) ) )
        return E_FAIL;

    // Fill the bits of the new texture surface with bits from
    // a private format.
    while( pddsBumpSrc->Lock( NULL, &ddsd, 0, 0 ) == DDERR_WASSTILLDRAWING );
    DWORD lSrcPitch = ddsd.lPitch;
    BYTE* pSrc      = (BYTE*)ddsd.lpSurface;

    while( m_pddsBumpMap->Lock( NULL, &ddsd, 0, 0 ) == DDERR_WASSTILLDRAWING );
    DWORD lDstPitch = ddsd.lPitch;
    BYTE* pDst      = (BYTE*)ddsd.lpSurface;

    for( DWORD y=0; y<ddsd.dwHeight; y++ )
    {
        BYTE* pDstT = pDst;
        BYTE* pSrcB0 = (BYTE*)pSrc;
        BYTE* pSrcB1 = ( pSrcB0 + lSrcPitch );
        BYTE* pSrcB2 = ( pSrcB0 - lSrcPitch );


        if( y == ddsd.dwHeight-1 )   // Don't go past the last line
            pSrcB1 = pSrcB0;
        if( y == 0 )                 // Don't go before first line
            pSrcB2 = pSrcB0;

        for( DWORD x=0; x<ddsd.dwWidth; x++ )
        {
            LONG v00 = *(pSrcB0+0); // Get the current pixel
            LONG v01 = *(pSrcB0+4); // and the pixel to the right
            LONG vM1 = *(pSrcB0-4); // and the pixel to the left
            LONG v10 = *(pSrcB1+0); // and the pixel one line below.
            LONG v1M = *(pSrcB2+0); // and the pixel one line above.

            LONG iDu = (vM1-v01); // The delta-u bump value
            LONG iDv = (v1M-v10); // The delta-v bump value

            if ( (v00 < vM1) && (v00 < v01) )  // If we are at valley
            {
                iDu = vM1-v00;                 // Choose greater of 1st order diffs
                if ( iDu < v00-v01 ) iDu = v00-v01;
            }

            WORD uL  = 255;  // The luminance bump value
            if( v00 > 120 )  // If above sea-level (specific to the earth bmp),
                uL = 63;     // make the land masses less shiny.

            switch( format )
            {
                case BUMPMAP_U8V8:
                    *pDstT++ = (BYTE)iDu;
                    *pDstT++ = (BYTE)iDv;
                    break;

                case BUMPMAP_U5V5L6:
                    *(WORD*)pDstT  =  (iDu >> 3) & 0x1f;
                    *(WORD*)pDstT |= ((iDv >> 3) & 0x1f)<<5;
                    *(WORD*)pDstT |= ( (uL >> 2) & 0x3f)<<10;
                    pDstT += 2;
                    break;

                case BUMPMAP_U8V8L8:
                    *pDstT++ = (BYTE)iDu;
                    *pDstT++ = (BYTE)iDv;
                    *pDstT++ = (BYTE)uL;
                    break;
            }

            pSrcB0+=4; // Move one pixel to the left (src is 32-bpp)
            pSrcB1+=4;
            pSrcB2+=4;
        }
        pSrc += lSrcPitch; // Move to the next line
        pDst += lDstPitch;
    }

    m_pddsBumpMap->Unlock(0);
    pddsBumpSrc->Unlock(0);

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: TextureSearchCallback()
// Desc: Callback function used to enumerate texture formats.
//-----------------------------------------------------------------------------
HRESULT CALLBACK TextureSearchCallback( DDPIXELFORMAT* pddpf, VOID* param )
{
    // Skip unwanted formats
    if( (pddpf->dwFlags & DDPF_BUMPDUDV) == 0 ) 
        return DDENUMRET_OK;

    HMENU hMenu = (HMENU)param;    

    // Look for IDM_U8V8
    if( pddpf->dwBumpBitCount         == 16         &&
        pddpf->dwBumpDuBitMask        == 0x000000ff &&
        pddpf->dwBumpDvBitMask        == 0x0000ff00 &&
        pddpf->dwBumpLuminanceBitMask == 0x00000000 )
    {
        EnableMenuItem( hMenu, IDM_U8V8, MF_ENABLED );
        return DDENUMRET_OK;
    }

    // Look for BUMPMAP_U5V5L6
    if( pddpf->dwBumpBitCount         == 16         &&
        pddpf->dwBumpDuBitMask        == 0x0000001f &&
        pddpf->dwBumpDvBitMask        == 0x000003e0 &&
        pddpf->dwBumpLuminanceBitMask == 0x0000fc00 )
    {
        EnableMenuItem( hMenu, IDM_U5V5L6, MF_ENABLED );
        return DDENUMRET_OK;
    }

        
    // Look for BUMPMAP_U8V8L8
    if( pddpf->dwBumpBitCount         == 24         &&
        pddpf->dwBumpDuBitMask        == 0x000000ff &&
        pddpf->dwBumpDvBitMask        == 0x0000ff00 &&
        pddpf->dwBumpLuminanceBitMask == 0x00ff0000 )
    {
        EnableMenuItem( hMenu, IDM_U8V8L8, MF_ENABLED );
        return DDENUMRET_OK;
    }

    // Unknown bump map texture format found
    return DDENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    D3DTextr_RestoreAllTextures( m_pd3dDevice );

    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ADDRESS, D3DTADDRESS_WRAP );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_ADDRESS, D3DTADDRESS_WRAP );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 2, D3DTSS_ADDRESS, D3DTADDRESS_WRAP );
    m_pd3dDevice->SetTextureStageState( 2, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 2, D3DTSS_MINFILTER, D3DTFN_LINEAR );

    // Get the aspect ratio
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT fAspect = ((FLOAT)vp.dwHeight) / vp.dwWidth;

    // Set projection matrix
    D3DMATRIX matProj;
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, fAspect, 0.1f, 22.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set view matrix
    D3DMATRIX matView;
    D3DVECTOR vEyePt( 0.0f, 0.0f, 1.0f );
    D3DVECTOR vLookatPt( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vUpVec( 0.0f, 1.0f, 0.0f );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );
    
    // Find out which bump map texture are supported by this device
    HMENU hMenu = GetMenu( m_hWnd );    
    EnableMenuItem( hMenu, IDM_U8V8L8, MF_GRAYED );
    EnableMenuItem( hMenu, IDM_U5V5L6, MF_GRAYED );
    EnableMenuItem( hMenu, IDM_U8V8, MF_GRAYED );
    m_pd3dDevice->EnumTextureFormats( TextureSearchCallback, hMenu );

    // Choose the default bump map format from what is supported
    if( GetMenuState( hMenu, IDM_U8V8, 0 )        == MF_ENABLED )
        m_BumpMapFormat = BUMPMAP_U8V8;
    else if( GetMenuState( hMenu, IDM_U5V5L6, 0 ) == MF_ENABLED )
        m_BumpMapFormat = BUMPMAP_U5V5L6;
    else if( GetMenuState( hMenu, IDM_U8V8L8, 0 ) == MF_ENABLED )
        m_BumpMapFormat = BUMPMAP_U8V8L8;
    else
        return E_FAIL; // Unknown bump map texture format supported

    if( FAILED( InitBumpMap( m_BumpMapFormat, 
                             D3DTextr_GetSurface( "earthbump.bmp" ) ) ) )
        return E_FAIL;

    // Set menu states
    SetMenuStates();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    SAFE_DELETE( m_pSphereVertices );
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DeleteDeviceObjects()
// Desc: Called when the app is exitting, or the device is being changed,
//       this function deletes any device dependant objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DeleteDeviceObjects()
{
    D3DTextr_InvalidateAllTextures();

    SAFE_RELEASE( m_pddsBumpMap );

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
    if( pd3dDeviceDesc->dwTextureOpCaps & D3DTEXOPCAPS_BUMPENVMAP )
        return S_OK;
    if( pd3dDeviceDesc->dwTextureOpCaps & D3DTEXOPCAPS_BUMPENVMAPLUMINANCE )
        return S_OK;
    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: InitSphereVertices()
// Desc: Sets up the vertices for a bump-mapped sphere.
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::InitSphereVertices()
{
    SAFE_DELETE( m_pSphereVertices );

    // Choose a tesselation level   
    DWORD dwNumSphereRings    = 5;
    DWORD dwNumSphereSegments = 10;

    if( m_bHighTesselation )
    {
        dwNumSphereRings    = 15;
        dwNumSphereSegments = 15;
    }
    
    // Allocate space for the sphere
    m_dwNumSphereVertices = 2 * dwNumSphereRings * (dwNumSphereSegments+1);
    m_pSphereVertices     = new BUMPVERTEX[m_dwNumSphereVertices];
    BUMPVERTEX* vtx = m_pSphereVertices;

    // Establish constants used in sphere generation
    FLOAT fDeltaRingAngle = ( g_PI / dwNumSphereRings );
    FLOAT fDeltaSegAngle  = ( 2.0f * g_PI / dwNumSphereSegments );

    // Generate the group of rings for the sphere
    for( DWORD ring = 0; ring < dwNumSphereRings; ring++ )
    {    
        FLOAT r0 = sinf( (ring+0) * fDeltaRingAngle );
        FLOAT r1 = sinf( (ring+1) * fDeltaRingAngle );
        FLOAT y0 = cosf( (ring+0) * fDeltaRingAngle );
        FLOAT y1 = cosf( (ring+1) * fDeltaRingAngle );

        // Generate the group of segments for the current ring
        for( DWORD seg = 0; seg < (dwNumSphereSegments+1); seg++ )
        {
            FLOAT x0 =  r0 * sinf( seg * fDeltaSegAngle );
            FLOAT z0 =  r0 * cosf( seg * fDeltaSegAngle );
            FLOAT x1 =  r1 * sinf( seg * fDeltaSegAngle );
            FLOAT z1 =  r1 * cosf( seg * fDeltaSegAngle );

            // Add two vertices to the strip which makes up the sphere
            (*vtx++).v = D3DVERTEX( D3DVECTOR(x0,y0,z0), D3DVECTOR(x0,y0,z0),
                                  -((FLOAT)seg)/dwNumSphereSegments,
                                  (ring+0)/(FLOAT)dwNumSphereRings );

            (*vtx++).v = D3DVERTEX( D3DVECTOR(x1,y1,z1), D3DVECTOR(x1,y1,z1),
                                  -((FLOAT)seg)/dwNumSphereSegments,
                                  (ring+1)/(FLOAT)dwNumSphereRings );
        }
    }

    // Initialize the 2nd set of texture coords
    for( DWORD i=0; i<m_dwNumSphereVertices; i++ )
    {
        m_pSphereVertices[i].tu2 = m_pSphereVertices[i].v.tu;
        m_pSphereVertices[i].tv2 = m_pSphereVertices[i].v.tv;
    }
}




//-----------------------------------------------------------------------------
// Name: SetMenuStates()
// Desc: 
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::SetMenuStates()
{
    HMENU hMenu = GetMenu( m_hWnd );

    CheckMenuItem( hMenu, IDM_TEXTURETOGGLE,
                    m_bTextureOn ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_BUMPMAPTOGGLE,
                    m_bBumpMapOn ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_ENVMAPTOGGLE,
                    m_bEnvMapOn ? MF_CHECKED : MF_UNCHECKED );
    
    CheckMenuItem( hMenu, IDM_U8V8L8,
                    m_BumpMapFormat==BUMPMAP_U8V8L8 ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_U5V5L6,
                    m_BumpMapFormat==BUMPMAP_U5V5L6 ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_U8V8,
                    m_BumpMapFormat==BUMPMAP_U8V8 ? MF_CHECKED : MF_UNCHECKED );

    CheckMenuItem( hMenu, IDM_LOW_TESSELATION,
                    (!m_bHighTesselation) ? MF_CHECKED : MF_UNCHECKED );
    CheckMenuItem( hMenu, IDM_HIGH_TESSELATION,
                    m_bHighTesselation ? MF_CHECKED : MF_UNCHECKED );
}




//-----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: Message proc function to handle key and menu input
//-----------------------------------------------------------------------------
LRESULT CMyD3DApplication::MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                    LPARAM lParam )
{
    if( WM_COMMAND == uMsg )
    {
        switch( LOWORD(wParam) )
        {
            case IDM_TEXTURETOGGLE:
            case IDM_BUMPMAPTOGGLE:
            case IDM_ENVMAPTOGGLE:
                if( LOWORD(wParam) == IDM_TEXTURETOGGLE )
                    m_bTextureOn = !m_bTextureOn;
                if( LOWORD(wParam) == IDM_BUMPMAPTOGGLE )
                    m_bBumpMapOn = !m_bBumpMapOn;
                if( LOWORD(wParam) == IDM_ENVMAPTOGGLE )
                    m_bEnvMapOn  = !m_bEnvMapOn;

                SetMenuStates();
                break;
        
            case IDM_U8V8L8:
            case IDM_U5V5L6:
            case IDM_U8V8:
                if( LOWORD(wParam) == IDM_U8V8L8 )
                    m_BumpMapFormat = BUMPMAP_U8V8L8;
                if( LOWORD(wParam) == IDM_U5V5L6 )
                    m_BumpMapFormat = BUMPMAP_U5V5L6;
                if( LOWORD(wParam) == IDM_U8V8 )
                    m_BumpMapFormat = BUMPMAP_U8V8;

                InitBumpMap( m_BumpMapFormat, 
                             D3DTextr_GetSurface( "earthbump.bmp" ) );

                SetMenuStates();
                break;

            case IDM_LOW_TESSELATION:
            case IDM_HIGH_TESSELATION:
                if( LOWORD(wParam) == IDM_HIGH_TESSELATION )
                    m_bHighTesselation = TRUE;
                else
                    m_bHighTesselation = FALSE;

                InitSphereVertices();

                SetMenuStates();
                break;
        }
    }

    return CD3DApplication::MsgProc( hWnd, uMsg, wParam, lParam );
}



