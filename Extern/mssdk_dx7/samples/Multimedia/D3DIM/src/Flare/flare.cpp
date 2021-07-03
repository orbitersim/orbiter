//-----------------------------------------------------------------------------
// File: Flare.cpp
//
// Desc: Example code showing how to simulate lens flares using the
//       blending features of the 3D rasterizer. The lens flare images
//       are loaded from .BMP files into D3D textures.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1996-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define D3D_OVERLOADS
#define STRICT
#include <math.h>
#include <stdio.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"




//-----------------------------------------------------------------------------
// Name: Flare
// Desc: Data structure for the lens flares
//-----------------------------------------------------------------------------
struct Flare 
{
    INT                wType;       // 0..5, matches flare material indices
    FLOAT              fLoc;        // postion on axis
    FLOAT              fScale;      // relative size
    FLOAT              r,g,b;       // color
    D3DVECTOR          vPositionPt; // 3D position for rendering
    FLOAT              fRenderSize; // size for rendering the flare
};

#define     NUM_FLARES 12        // Number of lens flares




//-----------------------------------------------------------------------------
// Name: SetFlare()
// Desc: Helper function to initialize a flare
//-----------------------------------------------------------------------------
Flare SetFlare( INT wType, FLOAT fLocation, FLOAT fScale, FLOAT fRed,
                FLOAT fGreen, FLOAT fBlue )
{
    Flare ret;
    ret.wType  = wType;
    ret.fLoc   = fLocation;
    ret.fScale = fScale;
    ret.r      = fRed;
    ret.g      = fGreen;
    ret.b      = fBlue;
    return ret;
}




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    LPDIRECTDRAWSURFACE7 m_pddsShineTextures[10];
    LPDIRECTDRAWSURFACE7 m_pddsFlareTextures[5];

    Flare       m_Flare[NUM_FLARES]; // The actual flares
    D3DVERTEX   m_Mesh[4];           // Vertices used to render flares
    D3DTLVERTEX m_Background[4];     // Vertices used to render the backdrop

    HRESULT RenderFlares();
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
    m_strWindowTitle  = TEXT( "Flare: Lens Flare Demo" );
    m_bAppUseZBuffer  = FALSE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    for( DWORD s=0; s<10; s++ )
        m_pddsShineTextures[s] = NULL;
    for( DWORD f=0; f<5; f++ )
        m_pddsFlareTextures[f] = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Initialize the array of lens flares
    //                   type   loc   scale  red  green  blue
    m_Flare[0]  = SetFlare( -1,  1.00f, 0.45f, 0.0f, 0.0f, 1.0f );
    m_Flare[1]  = SetFlare( -1,  1.00f, 0.30f, 0.0f, 1.0f, 0.0f );
    m_Flare[2]  = SetFlare( -1,  1.00f, 0.47f, 1.0f, 0.0f, 0.0f );
    m_Flare[3]  = SetFlare(  2,  1.30f, 0.06f, 0.6f, 0.0f, 0.0f );
    m_Flare[4]  = SetFlare(  3,  1.00f, 0.15f, 0.4f, 0.0f, 0.0f );
    m_Flare[5]  = SetFlare(  1,  0.50f, 0.30f, 0.3f, 0.0f, 0.0f );
    m_Flare[6]  = SetFlare(  3,  0.20f, 0.07f, 0.3f, 0.0f, 0.0f );
    m_Flare[7]  = SetFlare(  0,  0.00f, 0.06f, 0.3f, 0.0f, 0.0f );
    m_Flare[8]  = SetFlare(  4, -0.25f, 0.11f, 0.5f, 0.0f, 0.0f );
    m_Flare[9]  = SetFlare(  4, -0.40f, 0.03f, 0.6f, 0.0f, 0.0f );
    m_Flare[10] = SetFlare(  4, -0.60f, 0.06f, 0.4f, 0.0f, 0.0f );
    m_Flare[11] = SetFlare(  4, -1.00f, 0.04f, 0.2f, 0.0f, 0.0f );

    // Initializes vertices used to render the flares
    D3DVECTOR vNorm = D3DVECTOR( 0.0f, 0.0f, 1.0f );
    m_Mesh[0] = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 0.0f ), vNorm, 0.0f, 1.0f );
    m_Mesh[1] = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 0.0f ), vNorm, 0.0f, 0.0f );
    m_Mesh[2] = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 0.0f ), vNorm, 1.0f, 1.0f );
    m_Mesh[3] = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 0.0f ), vNorm, 1.0f, 0.0f );

    // Initializes vertices used to render the background
	D3DVECTOR vFar = D3DVECTOR( 0.0f, 0.0f, 0.5f );
    m_Background[0] = D3DTLVERTEX( vFar, 0.5f, 0xffffffff, 0, 0.0f, 1.0f );
    m_Background[1] = D3DTLVERTEX( vFar, 0.5f, 0xffffffff, 0, 0.0f, 0.0f );
    m_Background[2] = D3DTLVERTEX( vFar, 0.5f, 0xffffffff, 0, 1.0f, 1.0f );
    m_Background[3] = D3DTLVERTEX( vFar, 0.5f, 0xffffffff, 0, 1.0f, 0.0f );

    // Load in lens flares' images as textures
    D3DTextr_CreateTextureFromFile( "dx5_logo.bmp" );
    D3DTextr_CreateTextureFromFile( "shine0.bmp" );
    D3DTextr_CreateTextureFromFile( "shine1.bmp" );
    D3DTextr_CreateTextureFromFile( "shine2.bmp" );
    D3DTextr_CreateTextureFromFile( "shine3.bmp" );
    D3DTextr_CreateTextureFromFile( "shine4.bmp" );
    D3DTextr_CreateTextureFromFile( "shine5.bmp" );
    D3DTextr_CreateTextureFromFile( "shine6.bmp" );
    D3DTextr_CreateTextureFromFile( "shine7.bmp" );
    D3DTextr_CreateTextureFromFile( "shine8.bmp" );
    D3DTextr_CreateTextureFromFile( "shine9.bmp" );
    D3DTextr_CreateTextureFromFile( "flare0.bmp" );
    D3DTextr_CreateTextureFromFile( "flare1.bmp" );
    D3DTextr_CreateTextureFromFile( "flare2.bmp" );
    D3DTextr_CreateTextureFromFile( "flare3.bmp" );
    D3DTextr_CreateTextureFromFile( "flare4.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Mess with the light position
    D3DVECTOR vLightPt;
    vLightPt.x = (FLOAT)( sin(fTimeKey*0.73)*12 );
    vLightPt.y = (FLOAT)( 5.0f+10.0f*sin(fTimeKey*0.678) );
    vLightPt.z = (FLOAT)( sin(fTimeKey*0.895)*12 );

    // Compute the vectors between the from, lookat, and light positions.   
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f,  0.0f );
    D3DVECTOR vFromPt   = D3DVECTOR( 0.0f, 0.0f,-20.0f );
    D3DVECTOR vViewVec  = Normalize( vLookatPt - vFromPt );
    D3DVECTOR vLightVec = Normalize( vLightPt - vFromPt );

    // Compute the vector and center point for the lens flare axis
    FLOAT     fDotProduct = DotProduct( vLightVec, vViewVec );
    D3DVECTOR vNewLightPt = vFromPt + 1.0f/fDotProduct * vLightVec * 1.01f;
    D3DVECTOR vCenterPt   = vFromPt + vViewVec*1.01f;
    D3DVECTOR vAxisVec    = vNewLightPt - vCenterPt;

    // Compute the offset of the lens flare along the flare axis
    D3DVECTOR dx         = Normalize( vAxisVec );
    FLOAT     fDeltaX    = ( dx.x - dx.y );
    FLOAT     fDeltaY    = ( dx.y + dx.x );
    FLOAT     fViewScale = (FLOAT)sqrt( fDeltaX*fDeltaX + fDeltaY*fDeltaY );

    // Store the lens flares positions for each flare
    for( DWORD i=0; i<NUM_FLARES; i++ )
    {
        // Store the position of the flare along the axis
        m_Flare[i].vPositionPt = vCenterPt + vAxisVec * m_Flare[i].fLoc;

        // Store the render size of the flare. This is the lens flare size
        // corrected for the orientation of the flaring axis.
        m_Flare[i].fRenderSize = fViewScale * m_Flare[i].fScale;
    }
    
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RenderFlares()
// Desc: Draws the set of flares
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RenderFlares()
{
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl );

    // Turn on alpha blending for the flares
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );

    // Do the flares
    for( DWORD i=0; i<NUM_FLARES; i++ )
    {
        // Set up the emissive color of the flare
        mtrl.emissive.r = m_Flare[i].r;
        mtrl.emissive.g = m_Flare[i].g;
        mtrl.emissive.b = m_Flare[i].b;
        m_pd3dDevice->SetMaterial( &mtrl );

        if( m_Flare[i].wType < 0 )
        {
            static dwShineTic = 0;
            if( ++dwShineTic > 9 )
                dwShineTic = 0;

            m_pd3dDevice->SetTexture( 0, m_pddsShineTextures[dwShineTic] );
        } 
        else 
        {
            m_pd3dDevice->SetTexture( 0, m_pddsFlareTextures[m_Flare[i].wType] );
        }

        // Translate the world matrix to the flare position
        D3DMATRIX matWorld;
        D3DUtil_SetTranslateMatrix( matWorld, m_Flare[i].vPositionPt );

        // Scale the world matrix to the flare size.
        matWorld._11 = m_Flare[i].fRenderSize;
        matWorld._22 = m_Flare[i].fRenderSize;
        matWorld._33 = 1.0f;
        
        // Set the new world transform and render the flare
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_Mesh, 4, 0 );
    }

    // Restore the material and states
    mtrl.emissive.r = mtrl.emissive.g = mtrl.emissive.b = 1.0f;
    m_pd3dDevice->SetMaterial( &mtrl );

    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );

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
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        // Draw the background
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("dx5_logo.bmp") );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX,
                                     m_Background, 4, 0 );

        // Render the flares
        RenderFlares();

        // End the scene.
        m_pd3dDevice->EndScene();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    D3DTextr_RestoreAllTextures( m_pd3dDevice );

    // Set up the dimensions for the background image
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    m_Background[0].sy = (FLOAT)vp.dwHeight;
    m_Background[2].sy = (FLOAT)vp.dwHeight;
    m_Background[2].sx = (FLOAT)vp.dwWidth;
    m_Background[3].sx = (FLOAT)vp.dwWidth;
    
    //Create the render material
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl );
    m_pd3dDevice->SetMaterial( &mtrl );

    CHAR strTexName[40];

    for( WORD i=0; i<10; i++ )
    {
        sprintf( strTexName, "shine%d.bmp", i );
        m_pddsShineTextures[i] = D3DTextr_GetSurface( strTexName );
    }
    for( i=0; i<5; i++ )
    {
        sprintf( strTexName, "flare%d.bmp", i );
        m_pddsFlareTextures[i] = D3DTextr_GetSurface( strTexName );
    }
        
    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f, -20.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f,   0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 0.0f,   1.0f );
    D3DMATRIX matView, matProj;

    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, 1.57f, 1.0f, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set any appropiate state
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,    0xffffffff );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,   D3DBLEND_ONE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND,  D3DBLEND_ONE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

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
    // Get triangle caps (Hardware or software) and check for alpha blending
    LPD3DPRIMCAPS pdpc = &pd3dDeviceDesc->dpcTriCaps;

    if( 0 == ( pdpc->dwSrcBlendCaps & pdpc->dwDestBlendCaps & D3DBLEND_ONE ) )
        return E_FAIL;

    return S_OK;
}





