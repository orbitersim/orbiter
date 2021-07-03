//-----------------------------------------------------------------------------
// File: Fireworks.cpp
//
// Desc: Example code showing how to use particles to simulate a fireworks
//       explosion.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define D3D_OVERLOADS
#define STRICT
#include <math.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"




//-----------------------------------------------------------------------------
// Function prototypes and global (or static) variables
//-----------------------------------------------------------------------------
#define NUM_PARTICLES   100
#define RANDOM_NUM      (((FLOAT)rand()-(FLOAT)rand())/RAND_MAX)
#define RANDOM_VECTOR   D3DVECTOR(RANDOM_NUM,RANDOM_NUM,RANDOM_NUM)




//-----------------------------------------------------------------------------
// Name: Particle
// Desc: Data structure for each particle
//-----------------------------------------------------------------------------
struct Particle
{
    D3DVECTOR vPosition;
    D3DVECTOR vLaunchVelocity;
    D3DVECTOR vInitialPosition;
    D3DVECTOR vInitialVelocity;
    FLOAT     r, g, b;
    FLOAT     fLifetime;
    FLOAT     fMaturity;
    WORD      wType;
    FLOAT     fSize;
};




//-----------------------------------------------------------------------------
// Name: SetParticle()
// Desc: Helper function to initialize a particle
//-----------------------------------------------------------------------------
Particle SetParticle( WORD wType, FLOAT fLifeTime, D3DVECTOR vBasePosition,
                      D3DVECTOR vBaseVelocity )
{
    Particle ret;
    ret.vPosition         = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    ret.vLaunchVelocity   = vBaseVelocity;
    ret.vInitialPosition  = vBasePosition;
    ret.vInitialVelocity  = vBaseVelocity + 15.0f * Normalize(RANDOM_VECTOR);
    ret.r = ret.g = ret.b = 0.1f;
    ret.fLifetime         = fLifeTime * ( 1.0f + RANDOM_NUM/4.0f );
    ret.fMaturity         = 0.0f;
    ret.fSize             = 0.1f;
    ret.wType             = wType;
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
    Particle    m_Particle[NUM_PARTICLES]; // Array of particles
    D3DVERTEX   m_Mesh[4];                 // Vertices used to render particles
    D3DTLVERTEX m_Background[4];           // Vertices used to render the backdrop
    FLOAT       m_fStartTimeKey;           // Time reference for calculations

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
    m_strWindowTitle  = TEXT( "Fireworks: Using Particles" );
    m_bAppUseZBuffer  = FALSE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    m_fStartTimeKey   = 0.0f;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Initialize the array of particles
    for( WORD i=0; i<NUM_PARTICLES; i++ )
        m_Particle[i] = SetParticle( (i%3), 4.0f, D3DVECTOR(0.0f,0.0f,0.0f),
                                     D3DVECTOR(0.0f,30.0f,0.0f) );

    // Initializes vertices used to render the particles
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

    // Load in textures
    D3DTextr_CreateTextureFromFile( "lake.bmp" );
    D3DTextr_CreateTextureFromFile( "firework.bmp" );

    return S_OK;
}





//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    D3DVECTOR a0 = D3DVECTOR( 0.0f, -9.8f, 0.0f );
    FLOAT     t  = fTimeKey - m_fStartTimeKey - 1.0f;
    FLOAT     k  = 1.8f;

    DWORD dwNumOldParticles = 0L;

    // Store the particles positions
    for( DWORD i=0; i<NUM_PARTICLES; i++ )
    {
        if( t < 0.0f ) // Particle is in "launch" mode
        {
            D3DVECTOR v0 = m_Particle[i].vLaunchVelocity;
            D3DVECTOR r0 = m_Particle[i].vInitialPosition;
            m_Particle[i].vPosition = r0 + v0*(t-(1.0f+RANDOM_NUM)/20)/1.5f;
        } 
        else // Particle is in "explode" mode
        {
            D3DVECTOR v0 = m_Particle[i].vInitialVelocity;
            D3DVECTOR r0 = m_Particle[i].vInitialPosition;
            
            FLOAT fDamping = (1.0f-expf(-k*t))/(k*k);
            m_Particle[i].vPosition = r0 + a0*t/k + (k*v0+a0)*fDamping;
            m_Particle[i].fMaturity = t / m_Particle[i].fLifetime;

            FLOAT st = m_Particle[i].fMaturity+0.5f;
            m_Particle[i].r     = expf(-0.5f*st*st);
            m_Particle[i].g     = expf(-1.8f*st*st);
            m_Particle[i].b     = expf(-2.0f*st*st);
            m_Particle[i].fSize = expf(-1.0f*st*st);

			// Retire old particles
            if( m_Particle[i].fMaturity >= 1.0f )
                dwNumOldParticles++;
        }
    }

    // When all paritcles are expired, put the particle system in launch mode
    if( NUM_PARTICLES == dwNumOldParticles )
    {
        m_fStartTimeKey = fTimeKey;

        D3DVECTOR vLaunchVelocity = D3DVECTOR( 20.0f*RANDOM_NUM, 30.0f, 0.0f );

        for( WORD i=0; i<NUM_PARTICLES; i++ )
            m_Particle[i] = SetParticle( (i%3), 4.0f, D3DVECTOR(0.0f,0.0f,0.0f),
                                         vLaunchVelocity );
    }

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
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        // Draw the background
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("lake.bmp") );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX,
                                     m_Background, 4, 0 );

        // Turn on alpha blending for the particles
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("firework.bmp") );

        // Render the particles
        for( DWORD i=0; i<NUM_PARTICLES; i++ )
        {
            if( m_Particle[i].fMaturity >= 1.0f )
                continue;

            // Set up the emissive color of the particle
            mtrl.emissive.r = m_Particle[i].r;
            mtrl.emissive.g = m_Particle[i].g;
            mtrl.emissive.b = m_Particle[i].b;
            m_pd3dDevice->SetMaterial( &mtrl );

            // Translate and scale the world matrix for each particle
            D3DMATRIX matWorld;
            D3DUtil_SetTranslateMatrix( matWorld, m_Particle[i].vPosition );
            matWorld._11 = m_Particle[i].fSize;
            matWorld._22 = m_Particle[i].fSize;
        
            // Set the new world transform and render the particle
            m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );

            m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                         m_Mesh, 4, 0 );
        }

        // Finished with alpha blending
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );

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

    // Set the view matrices (left and right are for stereoscopic viewing)
    D3DVECTOR vEyePt      = D3DVECTOR( 0.0f, 0.0f, -20.0f );
    D3DVECTOR vLeftEyePt  = D3DVECTOR( 0.3f, 0.0f, -20.0f );
    D3DVECTOR vRightEyePt = D3DVECTOR(-0.3f, 0.0f, -20.0f );
    D3DVECTOR vLookatPt   = D3DVECTOR( 0.0f, 0.0f,   0.0f );
    D3DVECTOR vUpVec      = D3DVECTOR( 0.0f, 1.0f,   0.0f );
    D3DUtil_SetViewMatrix( m_matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetViewMatrix( m_matLeftView,  vLeftEyePt,  vLookatPt, vUpVec );
    D3DUtil_SetViewMatrix( m_matRightView, vRightEyePt, vLookatPt, vUpVec );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &m_matView );

    // Set the projection transform
    D3DMATRIX matProj;
    D3DUtil_SetProjectionMatrix( matProj, 1.57f, 1.0f, 1.0f, 100.0f );
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
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ADDRESS,   D3DTADDRESS_CLAMP );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    D3DTextr_DestroyTexture( "lake.bmp" );
    D3DTextr_DestroyTexture( "firework.bmp" );
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




