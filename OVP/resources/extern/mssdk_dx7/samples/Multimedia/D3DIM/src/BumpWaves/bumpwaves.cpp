//-----------------------------------------------------------------------------
// File: Waves.cpp
//
// Desc: Code to simulate reflections off waves using bumpmapping.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define D3D_OVERLOADS
#define STRICT
#include <time.h>
#include <math.h>
#include <stdio.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Function prototypes and global (or static) variables
//-----------------------------------------------------------------------------
#define BACKGROUNDIMAGE TEXT("lake2w2.bmp")

inline FLOAT rnd() { return (((FLOAT)rand()-(FLOAT)rand())/(2L*RAND_MAX)); }
inline FLOAT RND() { return (((FLOAT)rand())/RAND_MAX); }
inline DWORD F2DW( FLOAT f ) { return *((DWORD*)&f); }

struct BUMPVERTEX
{
    D3DVERTEX v;
    FLOAT     tu2, tv2;
};




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    BUMPVERTEX  m_WaterVertices[4];
    D3DVERTEX   m_BackgroundVertices[4];           // Vertices used to render the backdrop

    LPDIRECTDRAWSURFACE7 m_pddsBumpMap;
    LPDIRECTDRAWSURFACE7 m_pddsBackground;

    LPDIRECTDRAWSURFACE7 CreateBumpMap( DWORD dwWidth, DWORD dwHeight );
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
    m_strWindowTitle  = TEXT( "Waves: Bump-mapping In Action" );
    m_bAppUseZBuffer  = FALSE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    m_pddsBumpMap = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Initializes vertices used to render the particles
    D3DVECTOR vNorm = D3DVECTOR( 0.0f, 1.0f, 0.0f );

    m_WaterVertices[0].v = D3DVERTEX( D3DVECTOR(-1000.0f, 0.0f, -1250.0f ), vNorm,  0.0f, 0.7f );
    m_WaterVertices[1].v = D3DVERTEX( D3DVECTOR(-1000.0f, 0.0f,     0.0f ), vNorm,  0.0f,-0.3f );
    m_WaterVertices[2].v = D3DVERTEX( D3DVECTOR( 1000.0f, 0.0f, -1250.0f ), vNorm,  1.0f, 0.7f );
    m_WaterVertices[3].v = D3DVERTEX( D3DVECTOR( 1000.0f, 0.0f,     0.0f ), vNorm,  1.0f,-0.3f );
    m_WaterVertices[0].tu2 =  0.000f; m_WaterVertices[0].tv2 = 1.0f;
    m_WaterVertices[1].tu2 =  0.000f; m_WaterVertices[1].tv2 = 147/256.0f;
    m_WaterVertices[2].tu2 =  1.000f; m_WaterVertices[2].tv2 = 1.0f;
    m_WaterVertices[3].tu2 =  1.000f; m_WaterVertices[3].tv2 = 147/256.0f;

    // Initializes vertices used to render the background
    vNorm = D3DVECTOR( 0.0f, 0.0f, -1.0f );
    m_BackgroundVertices[0] = D3DVERTEX( D3DVECTOR(-1000.0f,    0.0f, 0.0f ), vNorm, 0.0f, 147/256.0f );
    m_BackgroundVertices[1] = D3DVERTEX( D3DVECTOR(-1000.0f, 1000.0f, 0.0f ), vNorm, 0.0f, 0.0f );
    m_BackgroundVertices[2] = D3DVERTEX( D3DVECTOR( 1000.0f,    0.0f, 0.0f ), vNorm, 1.0f, 147/256.0f );
    m_BackgroundVertices[3] = D3DVERTEX( D3DVECTOR( 1000.0f, 1000.0f, 0.0f ), vNorm, 1.0f, 0.0f );

    // Load the texture for the background image
    D3DTextr_CreateTextureFromFile( BACKGROUNDIMAGE );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Nin r is 0.04 if amplitude is 32 to miss temporal aliasing
    // Max r is 0.16 for amplitude is 8 to miss spatial aliasing
    FLOAT r = 0.04f;    
    FLOAT s = r * sinf( fTimeKey * 9.0f );
    FLOAT c = r * cosf( fTimeKey * 9.0f );

    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_BUMPENVMAT00, F2DW( c ) );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_BUMPENVMAT01, F2DW(-s ) );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_BUMPENVMAT10, F2DW( s ) );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_BUMPENVMAT11, F2DW( c ) );

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
    // Begin the scene
    if( FAILED( m_pd3dDevice->BeginScene() ) )
        return S_OK;

    // Draw the background
    m_pd3dDevice->SetTexture( 0, m_pddsBackground );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 0 );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_SELECTARG1 );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP,   D3DTOP_DISABLE );

    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                 m_BackgroundVertices, 4, 0 );

    // Render the waves
    m_pd3dDevice->SetTexture( 0, m_pddsBumpMap );
    m_pd3dDevice->SetTexture( 1, m_pddsBackground );

    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ADDRESS, D3DTADDRESS_MIRROR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 0 );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_BUMPENVLSCALE,  F2DW(0.90f) );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_BUMPENVLOFFSET, F2DW(0.00f) );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_BUMPENVMAPLUMINANCE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_CURRENT );


    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_ADDRESS, D3DTADDRESS_MIRROR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_TEXCOORDINDEX, 1 );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP,   D3DTOP_SELECTARG1 );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG2, D3DTA_CURRENT );

    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, 
                                 D3DFVF_XYZ|D3DFVF_NORMAL|D3DFVF_TEX2,
                                 m_WaterVertices, 4, NULL );

    // End the scene.
    m_pd3dDevice->EndScene();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateBumpMap()
// Desc: Creates a bumpmap
//-----------------------------------------------------------------------------
LPDIRECTDRAWSURFACE7 CMyD3DApplication::CreateBumpMap( DWORD dwWidth,
                                                       DWORD dwHeight )
{
    LPDIRECTDRAWSURFACE7 pddsBumpMap;
    DDSURFACEDESC2       ddsd; 

    // Create the bump map surface. Surface desc depends on bump format
    ZeroMemory( &ddsd, sizeof(ddsd) );
    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags         = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH |
                           DDSD_PIXELFORMAT | DDSD_TEXTURESTAGE;
    ddsd.ddsCaps.dwCaps  = DDSCAPS_TEXTURE;
    ddsd.ddsCaps.dwCaps2 = 0L;
    ddsd.ddsCaps.dwCaps3 = 0L;
    ddsd.ddsCaps.dwCaps4 = 0L;
    ddsd.dwWidth         = dwWidth;
    ddsd.dwHeight        = dwHeight;
    ddsd.dwTextureStage  = 0L;

    // Turn on texture management for HW devices
    if( m_pDeviceInfo->bHardware )
        ddsd.ddsCaps.dwCaps2 |= DDSCAPS2_TEXTUREMANAGE;
    else
        ddsd.ddsCaps.dwCaps |= DDSCAPS_SYSTEMMEMORY;

    // Pixel format for U8V8 bumpmap format
    ddsd.ddpfPixelFormat.dwSize                 = sizeof(DDPIXELFORMAT);
    ddsd.ddpfPixelFormat.dwFlags                = DDPF_BUMPDUDV;
    ddsd.ddpfPixelFormat.dwBumpBitCount         = 16;
    ddsd.ddpfPixelFormat.dwBumpDuBitMask        = 0x000000ff;
    ddsd.ddpfPixelFormat.dwBumpDvBitMask        = 0x0000ff00;
    ddsd.ddpfPixelFormat.dwBumpLuminanceBitMask = 0x00000000;

    // Create the bumpmap's surface and texture objects
    if( FAILED( m_pDD->CreateSurface( &ddsd, &pddsBumpMap, NULL ) ) )
        return NULL;

    // Lock the surface and write in some bumps for the waves
    ddsd.dwSize = sizeof(DDSURFACEDESC2);
    while( pddsBumpMap->Lock( NULL, &ddsd, 0, 0 ) == DDERR_WASSTILLDRAWING );
    CHAR* pDst = (char*)ddsd.lpSurface;
    CHAR  iDu, iDv;

    for( DWORD y=0; y<ddsd.dwHeight; y++ )
    {
        for( DWORD x=0; x<ddsd.dwWidth; x++ )
        {
            FLOAT fx = x/(FLOAT)ddsd.dwWidth - 0.5f;
            FLOAT fy = y/(FLOAT)ddsd.dwHeight - 0.5f;

            FLOAT r = sqrtf( fx*fx + fy*fy );
            iDu = (CHAR)( ( 32 * cosf( 300.0f * r ) ) * expf( -r * 5.0f ) );
            iDv = (CHAR)( ( 32 * sinf( 300.0f * r ) ) * expf( -r * 5.0f ) );

            iDu += (CHAR)( 16 * cosf( 300.0f * ( fx + fy ) * 0.5f ) );
            iDv += (CHAR)( 16 * sinf( 300.0f * ( fx + fy ) * 0.5f ) );
      
            iDu += (CHAR)( 7 * cosf( 280.0f * ( fx * 0.85f - fy ) * 0.5f ) );
            iDv += (CHAR)( 7 * sinf( 280.0f * ( fx * 0.85f - fy ) * 0.5f ) );

            pDst[2*x+0] = iDu;
            pDst[2*x+1] = iDv;
        }
        pDst += ddsd.lPitch;
    }
    pddsBumpMap->Unlock(0);

    return pddsBumpMap;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    // Restore textures and setup the bitmap
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pddsBackground = D3DTextr_GetSurface( BACKGROUNDIMAGE );
    m_pddsBumpMap = CreateBumpMap( 256, 256 );

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 400.0f, -1650.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f,   0.0f,     0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f,   1.0f,     0.0f );
    D3DMATRIX matWorld, matView, matProj;

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, 1.00f, 1.0f, 1.0f, 3000.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set any appropiate state
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,        0xffffffff );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 1, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
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

    return E_FAIL;
}



