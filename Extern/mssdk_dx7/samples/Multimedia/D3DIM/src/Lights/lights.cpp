//-----------------------------------------------------------------------------
// File: Lights.cpp
//
// Desc: Example code showing how to do lights in D3D.
//
//       Note: This code uses the D3D Framework helper library.
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include <time.h>
#include <stdio.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define WALL_MESH_SIZE      12
#define NUM_WALL_VERTICES   (WALL_MESH_SIZE*WALL_MESH_SIZE)
#define NUM_WALL_INDICES    ((WALL_MESH_SIZE-1)*(WALL_MESH_SIZE-1)*6)

#define SPHERE_MESH_SIZE    4
#define NUM_SPHERE_VERTICES (2+SPHERE_MESH_SIZE*SPHERE_MESH_SIZE*2)
#define NUM_SPHERE_INDICES  ( (SPHERE_MESH_SIZE*4 + SPHERE_MESH_SIZE*4* \
                              (SPHERE_MESH_SIZE-1) ) * 3 )

#define rnd() ( ( ((FLOAT)rand())-((FLOAT)rand()) ) / RAND_MAX )




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    D3DLIGHTTYPE m_dltType;
    
    D3DVERTEX    m_WallVertices[NUM_WALL_VERTICES];
    WORD         m_WallIndices[NUM_WALL_INDICES];
    D3DVERTEX    m_SphereVertices[NUM_SPHERE_VERTICES];
    WORD         m_SphereIndices[NUM_SPHERE_INDICES];

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
    m_strWindowTitle  = TEXT( "Lights: D3D Lighting Sample" );
    m_bAppUseZBuffer  = FALSE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    m_dltType = D3DLIGHT_POINT;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    WORD i, j, ind, v;

    // Seed the random number generator
    srand( time(0) );

    // Generate a square mesh in XZ plane from 0,0 to 1,1 for the walls
    for( i=0; i<WALL_MESH_SIZE; i++ )
    {
        for( j=0; j<WALL_MESH_SIZE; j++ )
        {
            FLOAT      x = i / (FLOAT)(WALL_MESH_SIZE-1);
            FLOAT      z = j / (FLOAT)(WALL_MESH_SIZE-1);
            D3DVERTEX* v = &m_WallVertices[i*WALL_MESH_SIZE+j];
            (*v) = D3DVERTEX( 10.0f*D3DVECTOR(x,0.0f,z),
                              D3DVECTOR(0.0f,1.0f,0.0f), x, z );
        }
    }

    // Generate the wall indices
    for( i=ind=0; i<WALL_MESH_SIZE-1; i++ )
    {
        for( j=0; j<WALL_MESH_SIZE-1; j++ )
        {
            m_WallIndices[ind++] = (i+0)*WALL_MESH_SIZE + (j+0);
            m_WallIndices[ind++] = (i+0)*WALL_MESH_SIZE + (j+1);
            m_WallIndices[ind++] = (i+1)*WALL_MESH_SIZE + (j+0);
            m_WallIndices[ind++] = (i+1)*WALL_MESH_SIZE + (j+0);
            m_WallIndices[ind++] = (i+0)*WALL_MESH_SIZE + (j+1);
            m_WallIndices[ind++] = (i+1)*WALL_MESH_SIZE + (j+1);
        }
    }

    // Generate the sphere data
    FLOAT dj = g_PI/(SPHERE_MESH_SIZE+1.f);
    FLOAT di = g_PI/SPHERE_MESH_SIZE;

    // Vertices 0 and 1 are the north and south poles
    m_SphereVertices[0] = D3DVERTEX( D3DVECTOR(0.0f, 1.0f, 0.0f), 
                           D3DVECTOR(0.0f,-1.0f, 0.0f), rnd(), rnd() );
    m_SphereVertices[1] = D3DVERTEX( D3DVECTOR(0.0f, -1.0f, 0.0f), 
                           D3DVECTOR(0.0f, 1.0f, 0.0f), rnd(), rnd() );

    for( j=0; j<SPHERE_MESH_SIZE; j++ )
    {
        for( i=0; i<SPHERE_MESH_SIZE*2; i++ ) 
        {
            D3DVECTOR   p;

            p.y = (FLOAT)( cos((j+1) * dj) );
            p.x = (FLOAT)( sin(i * di) * sin((j+1) * dj) );
            p.z = (FLOAT)( cos(i * di) * sin((j+1) * dj) );
            m_SphereVertices[2+i+j*SPHERE_MESH_SIZE*2] = D3DVERTEX(p, -p, rnd(), rnd());
        }
    }

    // Now generate the traingle indices. Strip around north pole first
    for( i=0; i<SPHERE_MESH_SIZE*2; i++ )
    {
        m_SphereIndices[3*i+0] = 0;
        m_SphereIndices[3*i+1] = i+2;
        m_SphereIndices[3*i+2] = i+3;
        if( i==SPHERE_MESH_SIZE*2-1 )
            m_SphereIndices[3*i+2] = 2;
    }

    // Now all the middle strips
    for( j=0; j<SPHERE_MESH_SIZE-1; j++ )
    {
        v = 2+j*SPHERE_MESH_SIZE*2;
        ind = 3*SPHERE_MESH_SIZE*2 + j*6*SPHERE_MESH_SIZE*2;
        for( i=0; i<SPHERE_MESH_SIZE*2; i++ )
        {
            m_SphereIndices[6*i+0+ind] = v+i;
            m_SphereIndices[6*i+2+ind] = v+i+1;
            m_SphereIndices[6*i+1+ind] = v+i+SPHERE_MESH_SIZE*2;

            m_SphereIndices[6*i+0+ind+3] = v+i+SPHERE_MESH_SIZE*2;
            m_SphereIndices[6*i+2+ind+3] = v+i+1;
            m_SphereIndices[6*i+1+ind+3] = v+i+SPHERE_MESH_SIZE*2+1;
            if( i==SPHERE_MESH_SIZE*2-1 )
            {
                m_SphereIndices[6*i+2+ind+0] = v+i+1-2*SPHERE_MESH_SIZE;
                m_SphereIndices[6*i+2+ind+3] = v+i+1-2*SPHERE_MESH_SIZE;
                m_SphereIndices[6*i+1+ind+3] = v+i+SPHERE_MESH_SIZE*2+1-2*SPHERE_MESH_SIZE;
            }
        }
    }

    // Finally strip around south pole
    v   = NUM_SPHERE_VERTICES-SPHERE_MESH_SIZE*2;
    ind = NUM_SPHERE_INDICES-3*SPHERE_MESH_SIZE*2;
    for( i=0; i<SPHERE_MESH_SIZE*2; i++ )
    {
        m_SphereIndices[3*i+0+ind] = 1;
        m_SphereIndices[3*i+1+ind] = v+i+1;
        m_SphereIndices[3*i+2+ind] = v+i;
        if( i==SPHERE_MESH_SIZE*2-1 )
            m_SphereIndices[3*i+1+ind] = v;
    }

    // Create some textures
    D3DTextr_CreateTextureFromFile( "Banana.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Rotate through the various lights types
    m_dltType = (D3DLIGHTTYPE)(1+(((DWORD)fTimeKey)/5)%3);

    // Make sure light is supported by the device
    DWORD dwCaps = m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps;

    if( 0 == ( dwCaps & D3DVTXPCAPS_POSITIONALLIGHTS ) )
    {
        if( m_dltType == D3DLIGHT_POINT || m_dltType == D3DLIGHT_SPOT )
            m_dltType = D3DLIGHT_DIRECTIONAL;
    }

    // Values for the light position, direction, and color
    FLOAT x = sinf( fTimeKey*2.000f );
    FLOAT y = sinf( fTimeKey*2.246f );
    FLOAT z = sinf( fTimeKey*2.640f );

    // Set up the light structure
    D3DLIGHT7 light;
    ZeroMemory( &light, sizeof(light) );
    light.dltType       = m_dltType;
    light.dcvDiffuse.r  = 0.5f + 0.5f * x;
    light.dcvDiffuse.g  = 0.5f + 0.5f * y;
    light.dcvDiffuse.b  = 0.5f + 0.5f * z;
    light.dvRange       = D3DLIGHT_RANGE_MAX;
    
    switch( m_dltType )
    {
        case D3DLIGHT_POINT:
            light.dvPosition     = 4.5f * D3DVECTOR( x, y, z );
            light.dvAttenuation1 = 0.4f;
            break;
        case D3DLIGHT_DIRECTIONAL:
            light.dvDirection    = D3DVECTOR( x, y, z );
            break;
        case D3DLIGHT_SPOT:
            light.dvDirection    = D3DVECTOR( x, y, z );
            light.dvFalloff      = 100.0f;
            light.dvTheta        =   0.8f;
            light.dvPhi          =   1.0f;
            light.dvAttenuation0 =   1.0f;
    }

    // Set the light
    m_pd3dDevice->SetLight( 0, &light );

    // Move the camera position around
    FLOAT     toc = 0.3f*x - g_PI/4;
    D3DVECTOR vFrom( sinf(toc)*4.0f, 3.0f, -cosf(toc)*4.0f );
    D3DVECTOR vAt( 0.0f, 0.0f, 0.0f );
    D3DVECTOR vUp( 0.0f, 1.0f, 0.0f );
    SetViewParams( &vFrom, &vAt, &vUp, 0.1f );

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
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET, 0x00000000, 1.0f, 0L );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        D3DMATRIX matWorld, matTrans, matRotate;

         // Draw the bottom wall
        D3DUtil_SetTranslateMatrix( matTrans,-5.0f, -5.0f, -5.0f );
        D3DUtil_SetRotateZMatrix( matRotate, 0.0f );
        D3DMath_MatrixMultiply( matWorld, matRotate, matTrans );
        m_pd3dDevice->SetTransform(D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                            m_WallVertices, NUM_WALL_VERTICES,
                                            m_WallIndices, NUM_WALL_INDICES, 0 );

        // Draw the back wall
        D3DUtil_SetTranslateMatrix( matTrans, 5.0f,-5.0f, -5.0f );
        D3DUtil_SetRotateZMatrix( matRotate, g_PI/2 );
        D3DMath_MatrixMultiply( matWorld, matRotate, matTrans );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                            m_WallVertices, NUM_WALL_VERTICES,
                                            m_WallIndices, NUM_WALL_INDICES, 0 );

        // Draw the side wall
        D3DUtil_SetTranslateMatrix( matTrans, -5.0f, -5.0f, 5.0f );
        D3DUtil_SetRotateXMatrix( matRotate,  -g_PI/2 );
        D3DMath_MatrixMultiply( matWorld, matRotate, matTrans );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                            m_WallVertices, NUM_WALL_VERTICES,
                                            m_WallIndices, NUM_WALL_INDICES, 0 );

        // Draw sphere at light's position
        D3DLIGHT7 light;
        m_pd3dDevice->GetLight( 0, &light );
        D3DUtil_SetTranslateMatrix( matWorld, light.dvPosition.x,
                                    light.dvPosition.y, light.dvPosition.z );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
        m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX, 
                                            m_SphereVertices, NUM_SPHERE_VERTICES,
                                            m_SphereIndices, NUM_SPHERE_INDICES, 0 );

        // Output the name of the light type
        switch( m_dltType )
        {
            case D3DLIGHT_POINT:
                OutputText(  0, 20, TEXT("Point Light") );
                break;
            case D3DLIGHT_SPOT:
                OutputText( 0, 20, TEXT("Spot Light") );
                break;
            case D3DLIGHT_DIRECTIONAL:
                OutputText( 0, 20, TEXT("Directional Light") );
                break;
        }
    }

    // End the scene.
    m_pd3dDevice->EndScene();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    // Create and set up the object material
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    // Set up a texture
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("Banana.bmp") );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );

    // Set the transform matrices
    D3DMATRIX matWorld, matProj;
    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetProjectionMatrix( matProj, 1.57f, 1.0f, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Turn on lighting. Light will be set during FrameMove() call
    m_pd3dDevice->LightEnable( 0, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,  0x20202020 );

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




