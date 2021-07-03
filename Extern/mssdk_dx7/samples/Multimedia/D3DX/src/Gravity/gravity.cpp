//-----------------------------------------------------------------------------
// File: gravity.cpp
//
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#include "d3dx.h"
#include "resource.h"
#include "stdio.h"

#define NAME_OF_THE_APP "D3DX - Gravity"

#define RELEASENULL(pObject) if (pObject) {pObject->Release(); pObject = NULL;}
    
#define NUM_PLANETS         300
#define NUM_PARTICLES       1500
#define NUM_MATERIALS       25
#define NUM_SPHERES         25  //reused by planets
#define FULLSCREEN_WIDTH    640
#define FULLSCREEN_HEIGHT   480

#define Y_OFFSET(distance)       distance*(Random(2.0f)-1.0f)

typedef struct _SUN
{
    D3DXVECTOR4 pos;
    float distance;
    ID3DXSimpleShape* pSphere;
    DWORD dwMaterial;
} SUN;

typedef struct _PLANET
{
    D3DXVECTOR4 pos;
    float distance;
    DWORD dwSphere;
    DWORD dwMaterial;
    BOOL bAttract;
} PLANET;

typedef struct _PARTICLE
{
    D3DXVECTOR4 pos;
    float distance;
    DWORD dwMaterial;
    BOOL bAttract;
} PARTICLE;

class CGravity
{
public:
    CGravity();
    ~CGravity();
    void                    PauseDrawing();
    void                    RestartDrawing();
    void                    UpdateTime();
    void                    UnInit();
    HRESULT                 InitD3DX();
    HRESULT                 InitRenderer();
    HRESULT                 HandleModeChanges();
    void                    DestroySpheres();
    HRESULT                 GenerateSpheres();
    HRESULT                 Draw();
    void                    ApplyGravity(   float* pfDistance, 
                                            D3DXVECTOR4* pPos, 
                                            BOOL* pbAttract);

    BOOL                    m_bD3DXReady;
    BOOL                    m_bActive;
    BOOL                    m_bIsFullscreen;

    HWND                    m_hwndMain;
    RECT                    m_rWindowedRect;

    LPDIRECT3DDEVICE7       m_pD3DDev;
    LPDIRECT3D7             m_pD3D;
    LPDIRECTDRAW7           m_pDD;

    float                   m_fViewRot[3];
    float                   m_fSunRot[3];

    ID3DXContext*           m_pD3DX;

    SUN                     m_Sun;
    PLANET                  m_Planets[NUM_PLANETS];
    PARTICLE                m_Particles[NUM_PARTICLES];

    ID3DXMatrixStack*       m_pWorldStack;
    ID3DXMatrixStack*       m_pViewStack;

    D3DLIGHT7               m_LightOnSun;
    D3DLIGHT7               m_LightFromSun;
    D3DMATERIAL7            m_SunMaterial;
    D3DMATERIAL7            m_PlanetMaterials[NUM_MATERIALS];
    ID3DXSimpleShape*       m_Spheres[NUM_SPHERES];

    double                  m_dAbsoluteTime;
    double                  m_dElapsedTime;
    double                  m_dPeriod;
    LARGE_INTEGER           m_liLastTime;
};

CGravity* g_pGravity;

CGravity::CGravity()
{  
    m_bD3DXReady        = FALSE;
    m_bIsFullscreen     = FALSE;
    m_pD3DDev           = NULL;
    m_pD3D              = NULL;
    m_pDD               = NULL;
    m_pD3DX             = NULL;
    m_fViewRot[0]       =  1.0f;
    m_fViewRot[1]       = -1.0f;
    m_fViewRot[2]       =  0.0f;
    m_fSunRot[0]        =  0.0f;
    m_fSunRot[1]        =  0.0f;
    m_fSunRot[2]        =  0.0f;
    m_pWorldStack       = NULL;
    m_pViewStack        = NULL;
    m_bActive           = !m_bIsFullscreen;
    m_liLastTime.QuadPart   = 0;
    m_dAbsoluteTime     = 0;
    m_dElapsedTime      = 0;
    m_Sun.pSphere       = NULL;

    for( int i = 0; i < NUM_SPHERES; i++ )
    {
        m_Spheres[i] = NULL;
    }

    LARGE_INTEGER liFrequency;
    if(!QueryPerformanceFrequency(&liFrequency))
        liFrequency.QuadPart = 1;

    m_dPeriod = (double)1/liFrequency.QuadPart;
    if(!QueryPerformanceCounter(&m_liLastTime))
        m_liLastTime.QuadPart = 0;
}

CGravity::~CGravity()
{
    g_pGravity->UnInit();
}

void InterpretError(HRESULT hr)
{
    char errStr[100];
    D3DXGetErrorString(hr, 100, errStr );
    MessageBox(NULL,errStr,"D3DX Error",MB_OK);
}

float Random(float fMax)
{
    return fMax*rand()/RAND_MAX;
}

void CGravity::UpdateTime()
{ 
    LARGE_INTEGER liCurrTime;
    if(!QueryPerformanceCounter(&liCurrTime))
        liCurrTime.QuadPart = m_liLastTime.QuadPart + 1;
    
    m_dElapsedTime = (double)(liCurrTime.QuadPart - m_liLastTime.QuadPart)*
                                m_dPeriod;

    m_dAbsoluteTime += m_dElapsedTime;
    m_liLastTime = liCurrTime;
}


void CGravity::PauseDrawing()
{
    g_pGravity->m_bActive = FALSE;
    if( m_bIsFullscreen )
        ShowCursor(TRUE);
}

void CGravity::RestartDrawing()
{
    g_pGravity->m_bActive = TRUE;
    if( m_bIsFullscreen )
        ShowCursor(FALSE);
}

//*****************************************************************************
// Renderer Initialization Code
//*****************************************************************************

HRESULT CGravity::InitD3DX()
{
    HRESULT hr;
    DWORD i;
    char buff[1024];

    if( FAILED( hr = D3DXInitialize() ) )
        return hr;



    // D3DX Initialization
    hr = D3DXCreateContextEx(   D3DX_DEFAULT,           // D3DX handle
                                m_bIsFullscreen ? D3DX_CONTEXT_FULLSCREEN:0, // flags
                                m_hwndMain,
                                NULL,                   // focusWnd
                                D3DX_DEFAULT,           // colorbits
                                D3DX_DEFAULT,           // alphabits
                                D3DX_DEFAULT,           // numdepthbits
                                0,                      // numstencilbits
                                D3DX_DEFAULT,           // numbackbuffers
                                m_bIsFullscreen? FULLSCREEN_WIDTH:D3DX_DEFAULT,  // width
                                m_bIsFullscreen? FULLSCREEN_HEIGHT:D3DX_DEFAULT, // height
                                D3DX_DEFAULT,           // refresh rate
                                &m_pD3DX                // returned D3DX interface
                       );
    if( FAILED(hr) )
        return hr;

    m_pD3DDev = m_pD3DX->GetD3DDevice();
    if( m_pD3DDev == NULL )
        return E_FAIL;

    m_pD3D = m_pD3DX->GetD3D();
    if( m_pD3D == NULL )
        return E_FAIL;

    m_pDD = m_pD3DX->GetDD();
    if( m_pDD == NULL )
        return E_FAIL;

    m_bD3DXReady = TRUE;
    return InitRenderer();
}

// ***************************************************************************
// Renderer Initialization Code
// ***************************************************************************

HRESULT CGravity::InitRenderer()
{
    HRESULT hr;
    int i;

    if( !m_bD3DXReady )
        return E_FAIL;

    hr = m_pD3DX->SetClearColor(D3DRGBA(0,0,0,0));
    if( FAILED(hr) )
        return hr;

    srand(4);
    hr = m_pD3DDev->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    if ( FAILED(hr) )
        return hr;

    hr = m_pD3DDev->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, TRUE );
    if ( FAILED(hr) )
        return hr;

    hr = m_pD3DX->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
    if ( FAILED(hr) )
        return hr;

    D3DVALUE dvR,dvG,dvB;
    dvR = 1.0f;
    dvG = 0.9f;
    dvB = 0.6f;
    memset(&m_LightFromSun,0,sizeof(D3DLIGHT7));

    // Light which illuminates the "planets"
    m_LightFromSun.dltType = D3DLIGHT_POINT;
    m_LightFromSun.dvAttenuation0 = 0.5f;
    m_LightFromSun.dvAttenuation1 = 0.008f;
    m_LightFromSun.dvAttenuation2 = 0.0f;
    m_LightFromSun.dcvDiffuse.dvR = dvR;
    m_LightFromSun.dcvDiffuse.dvG = dvG;
    m_LightFromSun.dcvDiffuse.dvB = dvB;
    m_LightFromSun.dcvSpecular.dvR = dvR;
    m_LightFromSun.dcvSpecular.dvG = dvG;
    m_LightFromSun.dcvSpecular.dvB = dvB;
    m_LightFromSun.dvRange = 5000.0f;

    // Light which illuminates the "sun"
    memcpy(&m_LightOnSun, &m_LightFromSun, sizeof(D3DLIGHT7) );
    m_LightOnSun.dvAttenuation0 = 0.0f;
    m_LightOnSun.dvAttenuation1 = 0.003f;
    m_LightOnSun.dvAttenuation2 = 0.0f;
    m_LightOnSun.dcvDiffuse.dvR = dvR;
    m_LightOnSun.dcvDiffuse.dvG = dvG;
    m_LightOnSun.dcvDiffuse.dvB = dvB;
    m_LightOnSun.dcvSpecular.dvR = 1.0f;
    m_LightOnSun.dcvSpecular.dvG = 1.0f;
    m_LightOnSun.dcvSpecular.dvB = 1.0f;

    hr = m_pD3DDev->LightEnable( 0, TRUE );
    if ( FAILED(hr) )
        return hr;

    memset(&m_SunMaterial,0,sizeof(D3DMATERIAL7));
    m_SunMaterial.diffuse.r = dvR;
    m_SunMaterial.diffuse.g = dvG;
    m_SunMaterial.diffuse.b = dvB;
    m_SunMaterial.specular.r = 1.0f;
    m_SunMaterial.specular.g = 1.0f;
    m_SunMaterial.specular.b = 1.0f;
    m_SunMaterial.power = 3.0f;

    for( i = 0; i < NUM_MATERIALS; i++ ) 
    {
        memcpy(&m_PlanetMaterials[i],&m_SunMaterial,sizeof(D3DMATERIAL7));        
        m_PlanetMaterials[i].diffuse.r = Random(1.0f);
        m_PlanetMaterials[i].diffuse.g = Random(1.0f);
        m_PlanetMaterials[i].diffuse.b = Random(1.0f);
        m_PlanetMaterials[i].power = 1.0f;
    }

    if( FAILED( hr = GenerateSpheres() ) )
        return hr;
    
    float fPlanetRad;
    for( i = 0; i < NUM_PARTICLES; i++ )
    {
        // Make the planets.
        fPlanetRad = (float)Random(3.2f) + 0.7f;

        D3DXMATRIX initRot;
        D3DXMatrixRotationAxis(&initRot, &D3DXVECTOR3( 0.0f, 1.0f, 0.0f ), Random(2*D3DX_PI) + 1 );
        m_Particles[i].distance = Random(400.0f) + 50.0f;
        D3DXVECTOR4 vUnit( 1.0f, Y_OFFSET(m_Particles[i].distance/5000.0f), 0.0f, 1.0f );
        D3DXVec4Normalize(&vUnit,&vUnit);
        D3DXVec4Transform( &vUnit, &vUnit, &initRot );
        m_Particles[i].pos = vUnit;

        m_Particles[i].dwMaterial = (DWORD)Random((FLOAT) NUM_MATERIALS);
        m_Particles[i].bAttract = TRUE;
    }

    // Create Matrix Stack
    D3DXCreateMatrixStack( 0, &m_pWorldStack );

    // Create Matrix Stack
    D3DXCreateMatrixStack( 0, &m_pViewStack );

    return S_OK;
}

// ***************************************************************************
// GenerateSpheres
// ***************************************************************************

HRESULT CGravity::GenerateSpheres()
{
    HRESULT hr;
    
    float fPlanetRad;
    for( int i = 0; i < NUM_SPHERES; i++ ) 
    {
        fPlanetRad = (float)Random(3.2f) + 0.7f;
        hr = D3DXCreateSphere(  m_pD3DDev,
                                fPlanetRad,
                                (int)max(4,fPlanetRad*2),
                                (int)max(3,fPlanetRad*3),
                                1,
                                &m_Spheres[i] );
        if( FAILED(hr) )
            return hr;
    }

    // Make the sun.
    hr = D3DXCreateSphere(m_pD3DDev,20.0f,50,25,1,&m_Sun.pSphere);
    if( FAILED(hr) )
        return hr;

    for( i = 0; i < NUM_PLANETS; i++ )
    {
        // Make the planets.
        fPlanetRad = (float)Random(3.2f) + 0.7f;

        D3DXMATRIX initRot;
        D3DXMatrixRotationAxis(&initRot, &D3DXVECTOR3( 0.0f, 1.0f, 0.0f ), Random(2*D3DX_PI) + 1 );
        m_Planets[i].distance = Random(400.0f) + 50.0f;
        D3DXVECTOR4 vUnit( 1.0f, Y_OFFSET((float) m_Planets[i].distance/5000.0f), 0.0f, 0.0f );
        D3DXVec4Normalize(&vUnit,&vUnit);
        D3DXVec4Transform( &vUnit, &vUnit, &initRot );
        m_Planets[i].pos = vUnit;


        m_Planets[i].dwSphere = (DWORD)Random((FLOAT) NUM_SPHERES);

        m_Planets[i].dwMaterial = (DWORD)Random((FLOAT) NUM_MATERIALS);
        m_Planets[i].bAttract = TRUE;
    }

    return S_OK;
}

void CGravity::DestroySpheres()
{
    for( int i = 0; i < NUM_SPHERES; i++ )
    {
        RELEASENULL(m_Spheres[i]);
    }
}

void CGravity::UnInit()
{
    DestroySpheres();
    RELEASENULL(m_Sun.pSphere);
    RELEASENULL(m_pD3DDev);
    RELEASENULL(m_pD3D);
    RELEASENULL(m_pDD);
    RELEASENULL(m_pWorldStack);
    RELEASENULL(m_pViewStack);
    RELEASENULL(m_pD3DX);
    m_bD3DXReady = FALSE;
    D3DXUninitialize();
}

void CGravity::ApplyGravity(float* pfDistance, D3DXVECTOR4* pPos, BOOL* pbAttract)
{
    BOOL bFlippedState = FALSE;
    if( *pbAttract == TRUE )
    {
        // Do some fake gravity stuff... (a little too much gravity... :) )
        if( *pfDistance > 1 )
        {
            // Get sucked towards the sun:
            *pfDistance *= (float)pow(0.999,m_dElapsedTime*10);
            
            if( *pfDistance < 50 )
            {
                *pfDistance -= (float)(m_dElapsedTime*15);
            }
            else if( *pfDistance < 100 )
            {
                *pfDistance -= (float)(m_dElapsedTime*10);
            }
            else if( *pfDistance < 150 )
            {
                *pfDistance -= (float)(m_dElapsedTime*5);
            }
            *pfDistance = max(0.1f,*pfDistance);
            
            D3DXMATRIX rot;
            D3DXMatrixRotationAxis(&rot, &D3DXVECTOR3( 0.0f, 1.0f, 0.0f ), 
                                    (float)(80.0f*pow(*pfDistance,-1.1f)*m_dElapsedTime));
            D3DXVec4Transform( pPos, pPos, &rot );
            (*pPos).y *= (float)pow(0.999,m_dElapsedTime*30);
        }
        if( *pfDistance <= 1 )
        {
            // Teleport out of the sun.
            *pbAttract = FALSE;
            bFlippedState = TRUE;
        }
    }
    if( *pbAttract == FALSE )
    {
        if( *pfDistance > 500 )
        {
            // move to a new far away location, and
            // start getting sucked in again
            *pfDistance = Random(100.0f) + 500.0f;
            (*pPos).y = Y_OFFSET(*pfDistance/10000.0f);
            D3DXVec4Normalize(pPos,pPos);
            *pbAttract = TRUE;
            return;
        }
        if( bFlippedState )
        {
            (*pPos).y = 20.0f;
            D3DXVec4Normalize(pPos,pPos);
            if( Random(1.0f) > 0.5f )
            {
                (*pPos).y = -(*pPos).y;
            }
            *pfDistance = 10.0f;
        }
        *pfDistance *= (float) pow(2.0, m_dElapsedTime);
    }
}

// *********************************************************************************
// Rendering Code
// *********************************************************************************

HRESULT CGravity::Draw()
{
    HRESULT hr;
    int i;

    if( !m_bD3DXReady )
    {
        return E_FAIL;
    }
    if( !m_bActive )
    {
        return S_OK;
    }

    hr = m_pD3DDev->BeginScene();
    if ( SUCCEEDED(hr) )
    {
        hr = m_pD3DX->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
        if ( FAILED(hr) )
            return hr;
        
        UpdateTime();
        
        float fViewDist = 400+300*(float)sin(m_dAbsoluteTime*0.2);
        
        m_LightOnSun.dvPosition.dvX = 0.0f;
        m_LightOnSun.dvPosition.dvY = 0.0f;
        m_LightOnSun.dvPosition.dvZ = -100.f + fViewDist;
        m_pD3DDev->SetLight( 0, &m_LightOnSun );
        
        // Set up state for drawing the sun.
        
        m_fSunRot[0] = (float)(m_dAbsoluteTime/2);
        m_fSunRot[1] = (float)(m_dAbsoluteTime/2);
        m_fSunRot[2] = (float)(m_dAbsoluteTime);
        
        m_pD3DDev->SetMaterial(&m_SunMaterial);

        D3DXMATRIX matSunWorld, matTemp;
        D3DXMatrixRotationAxis(&matSunWorld, &D3DXVECTOR3( 1.0f, 0.0f, 0.0f ),
                               m_fSunRot[0] );
        D3DXMatrixRotationAxis(&matTemp, &D3DXVECTOR3( 0.0f, 1.0f, 0.0f ), 
                               m_fSunRot[1] );
        D3DXMatrixMultiply(&matSunWorld,&matSunWorld,&matTemp);
        D3DXMatrixRotationAxis(&matTemp, &D3DXVECTOR3( 0.0f, 0.0f, 1.0f ), 
                               m_fSunRot[2] );
        D3DXMatrixMultiply(&matSunWorld,&matSunWorld,&matTemp);
        D3DXMatrixTranslation(&matTemp,0.0f,0.0f,fViewDist);
        D3DXMatrixMultiply(&matSunWorld,&matSunWorld,&matTemp);

        m_pD3DDev->SetTransform( D3DTRANSFORMSTATE_WORLD, 
                                 (D3DMATRIX *)matSunWorld );

        D3DXMATRIX matSunView;
        D3DXMatrixIdentity(&matSunView);
        m_pD3DDev->SetTransform( D3DTRANSFORMSTATE_VIEW, 
                                 (D3DMATRIX *)matSunView );
        
        m_Sun.pSphere->Draw();

        m_pViewStack->LoadIdentity();
        m_fViewRot[0]-=(float)(0.075*m_dElapsedTime);
        m_fViewRot[1]+=(float)(0.01*m_dElapsedTime);
        m_fViewRot[2]+=(float)(0.015*m_dElapsedTime);
        
        m_pViewStack->RotateAxis( &D3DXVECTOR3( 1.0f, 0.0f, 0.0f ), 
                                  m_fViewRot[0] );
        m_pViewStack->RotateAxis( &D3DXVECTOR3( 0.0f, 1.0f, 0.0f ), 
                                  m_fViewRot[1] );
        m_pViewStack->RotateAxis( &D3DXVECTOR3( 0.0f, 0.0f, 1.0f ), 
                                  m_fViewRot[2] );
        m_pViewStack->Translate(0.0f,0.0f,fViewDist);
        m_pD3DDev->SetTransform( D3DTRANSFORMSTATE_VIEW, 
                                 (D3DMATRIX*)m_pViewStack->GetTop() );
        
    
        // Set up state for drawing the planets
        
        m_pD3DDev->SetLight( 0, &m_LightFromSun );

        // Draw the planets
        for( i = 0; i < NUM_PLANETS; i++ )
        {
            m_pD3DDev->SetMaterial(&m_PlanetMaterials[m_Planets[i].dwMaterial]);
            m_pWorldStack->LoadIdentity();
        
            // Do some fake gravity stuff... (a little too much gravity... :) )
            ApplyGravity(&m_Planets[i].distance,&m_Planets[i].pos, &m_Planets[i].bAttract);
            m_pWorldStack->Translate( m_Planets[i].pos.x * m_Planets[i].distance, 
                                      m_Planets[i].pos.y * m_Planets[i].distance,
                                      m_Planets[i].pos.z * m_Planets[i].distance );

            m_pD3DDev->SetTransform( D3DTRANSFORMSTATE_WORLD, 
                                     (D3DMATRIX *)m_pWorldStack->GetTop() );
            
            m_Spheres[m_Planets[i].dwSphere]->Draw();
        }

        m_pWorldStack->LoadIdentity();
        m_pD3DDev->SetTransform( D3DTRANSFORMSTATE_WORLD, 
                                 (D3DMATRIX *)m_pWorldStack->GetTop() );

        // Draw the particles
        D3DVERTEX vParticle;
        for( i = 0; i < NUM_PARTICLES; i++ )
        {
            m_pD3DDev->SetMaterial(&m_PlanetMaterials[m_Particles[i].dwMaterial]);

            // Do some fake gravity stuff... (a little too much gravity... :) )
            ApplyGravity(&m_Particles[i].distance,&m_Particles[i].pos, 
                         &m_Particles[i].bAttract);
    
            vParticle.dvX = m_Particles[i].pos.x* m_Particles[i].distance;
            vParticle.dvY = m_Particles[i].pos.y* m_Particles[i].distance;
            vParticle.dvZ = m_Particles[i].pos.z* m_Particles[i].distance;
            vParticle.dvNX = -m_Particles[i].pos.x;
            vParticle.dvNY = -m_Particles[i].pos.y;
            vParticle.dvNZ = -m_Particles[i].pos.z;
            m_pD3DDev->DrawPrimitive(D3DPT_POINTLIST,
                                     D3DFVF_VERTEX,
                                     &vParticle,
                                     1,
                                     D3DDP_WAIT );
        }

     
    
        m_pD3DDev->EndScene();
    }
    
    hr = m_pD3DX->UpdateFrame( 0 );
    if ( hr == DDERR_SURFACELOST || hr == DDERR_SURFACEBUSY )
        hr = HandleModeChanges();

    return hr;
}

HRESULT CGravity::HandleModeChanges()
{
    HRESULT hr;
    hr = m_pDD->TestCooperativeLevel();

    if( SUCCEEDED( hr ) || hr == DDERR_WRONGMODE )
    {
        UnInit();

        if(FAILED(hr = InitD3DX()))
            return hr;
    }
    else if( hr != DDERR_EXCLUSIVEMODEALREADYSET &&
             hr != DDERR_NOEXCLUSIVEMODE )
    {
        // Busted!!
        return hr;
    }
    return S_OK;
}

// ****************************************************************************
// Windowing Code...
// ****************************************************************************

LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch(uMsg)
    {
    case WM_ACTIVATEAPP:
        {
            if( !g_pGravity )
                break;

            if( g_pGravity->m_bIsFullscreen )
            {
                if( (BOOL)wParam )
                    g_pGravity->RestartDrawing();
                else
                    g_pGravity->PauseDrawing();
            }
        }
        break;
    case WM_CREATE:
        break;
    case WM_CLOSE:
        PostQuitMessage(0);
        break;
    case WM_SIZE:
        if( g_pGravity
            && g_pGravity->m_bD3DXReady 
            && !g_pGravity->m_bIsFullscreen
            )
        {
            HRESULT hr;
            
            if( wParam == SIZE_MINIMIZED )
            {
                g_pGravity->m_bActive = FALSE;
                break;
            }
            else if( LOWORD(lParam)>0 && HIWORD(lParam)>0 )
            {
                if( FAILED(hr = g_pGravity->m_pD3DX->Resize(LOWORD(lParam),HIWORD(lParam))))
                {
                    InterpretError(hr);
                    g_pGravity->m_bD3DXReady = FALSE;
                    PostQuitMessage(0);
                }
            }
            g_pGravity->m_bActive = TRUE;
            
        }
        break;
    case WM_KEYDOWN:
        switch( wParam )
        {
        case VK_ESCAPE:
        {    
            PostQuitMessage(0);
            break;
        }
        }
        break;
   case WM_COMMAND:
        if ( 1 == HIWORD(wParam) )
        {
            switch ( LOWORD(wParam) )
            {
            case IDM_FULLSCREEN:
                if( g_pGravity && g_pGravity->m_bD3DXReady )
                {
                    HRESULT hr;
                    g_pGravity->m_bIsFullscreen = ! g_pGravity->m_bIsFullscreen;
                    g_pGravity->m_bD3DXReady = FALSE;

                    if ( g_pGravity->m_bIsFullscreen )
                    {
                        // going to fullscreen
                        GetWindowRect( hwnd, &g_pGravity->m_rWindowedRect );
                    }
                    ShowCursor(!(g_pGravity->m_bIsFullscreen));
                    hr = g_pGravity->m_pD3DX->Clear(D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER);
                    if ( FAILED(hr) )
                    {
                        InterpretError(hr);
                        g_pGravity->PauseDrawing();
                        PostQuitMessage(-1);
                        break;
                    }
                    g_pGravity->UnInit();

                    if ( !g_pGravity->m_bIsFullscreen )
                    {
                        RECT& r = g_pGravity->m_rWindowedRect;
                        SetWindowPos(hwnd, HWND_NOTOPMOST, 
                                    r.left, 
                                    r.top, 
                                    r.right-r.left, 
                                    r.bottom-r.top, 
                                    SWP_NOACTIVATE );
                    }

                    hr = g_pGravity->InitD3DX();
                    if ( FAILED(hr) )
                    {
                        InterpretError(hr);
                        g_pGravity->PauseDrawing();
                        PostQuitMessage(-1);
                        break;
                    }
                }
                break;
            }
        }
        break;
    default:
        break;
    }

    return DefWindowProc(hwnd,uMsg,wParam,lParam);

}

int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, 
    LPSTR lpszCmdLine, int nCmdShow) 
{
    HRESULT     hr;    
    MSG         msg; 
    WNDCLASS    wc; 
    HACCEL      hAccelApp;
    HCURSOR     hcur = NULL;
    int         ret = 0;

    g_pGravity = new CGravity; // set up our data AFTER starting up d3dx!
    if( !g_pGravity )
    {
        ret = -1;
        goto Exit;
    }
    
    // Register the window class for the main window. 
 
    if (!hPrevInstance) 
    { 
        hcur = CopyCursor(LoadCursor(NULL, IDC_ARROW));

        wc.style = 0; 
        wc.lpfnWndProc = (WNDPROC) WndProc; 
        wc.cbClsExtra = 0; 
        wc.cbWndExtra = 0; 
        wc.hInstance = hInstance; 
        wc.hIcon = LoadIcon(hInstance, MAKEINTRESOURCE(IDI_APP_ICON)); 
        wc.hCursor = hcur; 
        wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH); 
        wc.lpszMenuName =  NULL; 
        wc.lpszClassName = NAME_OF_THE_APP; 
 
        if (!RegisterClass(&wc)) 
        {
            ret = -1;
            goto Exit;
        }
    } 
  
    // Create the window

    g_pGravity->m_hwndMain = CreateWindow(  NAME_OF_THE_APP, 
                                            NAME_OF_THE_APP, 
                                            WS_OVERLAPPEDWINDOW, 
                                            CW_USEDEFAULT, 
                                            CW_USEDEFAULT, 
                                            400, 
                                            400, 
                                            (HWND) NULL, 
                                            (HMENU) NULL, 
                                            hInstance, 
                                            (LPVOID) NULL); 
 
    if (!g_pGravity->m_hwndMain) 
    {
        ret = -1;
        goto Exit;
    }
 

    // Hide the cursor if necessary
    if( g_pGravity->m_bIsFullscreen )
    {
        ShowCursor(FALSE);
    }

    // Show the window
    ShowWindow(g_pGravity->m_hwndMain, nCmdShow); 
    UpdateWindow(g_pGravity->m_hwndMain); 

    hAccelApp = LoadAccelerators(hInstance, MAKEINTRESOURCE(IDR_APP_ACCELERATOR));
    if ( !hAccelApp )
    {
        ret = -1;
        goto Exit;
    }

    // Initialize D3DX
    hr = g_pGravity->InitD3DX();
    if ( FAILED(hr) )
    {
        InterpretError(hr);
        ret = -1;
        goto Exit;
    }

    BOOL bGotMsg;
    PeekMessage( &msg, NULL, 0U, 0U, PM_NOREMOVE );
    while( WM_QUIT != msg.message  )
    {
        bGotMsg = PeekMessage( &msg, NULL, 0U, 0U, PM_REMOVE );
 
        if( bGotMsg )
        {
            if ( !TranslateAccelerator( g_pGravity->m_hwndMain, hAccelApp, &msg ) )
            {
                TranslateMessage( &msg );
                DispatchMessage( &msg );
            }
         }
        else
        {
            if( g_pGravity && g_pGravity->m_bActive )
            {
                hr = g_pGravity->Draw();
                if( FAILED(hr) )
                {
                    InterpretError(hr);
                    g_pGravity->m_bD3DXReady = FALSE;
                    PostQuitMessage(-1);
                }
            }
            else
            {
                WaitMessage();
            }
        }
    }
    delete g_pGravity; // clean up our data BEFORE shutting down d3dx!

Exit:
    if(hcur)
        DestroyCursor(hcur);
    
    return ret;
}
