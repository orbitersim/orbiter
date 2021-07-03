//-----------------------------------------------------------------------------
// File: SphereMap.cpp
//
// Desc: Example code showing how to use sphere-mapping in D3DIM. The main part
//       of the code is how the sample must do it's own world-view
//       transformation of the object's normals. Based of the value of the
//       transformed normals, the corresponding vertex's texture coords are set
//       to the appopriate spot in the spheremap.
//
//       Note: This code uses the D3D Framework helper library.
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include <stdio.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "D3DFile.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    CD3DFile*  m_pTeapotFile;
    D3DVERTEX* m_pTeapotVertices;
    DWORD      m_dwTeapotNumVertices;
    WORD*      m_pTeapotIndices;
    DWORD      m_dwTeapotNumIndices;

    HRESULT ApplySphereMapToObject( D3DVERTEX* pvVertices,
                                    DWORD dwNumVertices );
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
    m_strWindowTitle  = TEXT( "SphereMap: Using Spheremaps in Direct3D" );
    m_bAppUseZBuffer  = TRUE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    m_pTeapotFile         = NULL;
    m_pTeapotVertices     = NULL;
    m_dwTeapotNumVertices = 0L;
    m_pTeapotIndices      = NULL;
    m_dwTeapotNumIndices  = 0L;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    m_pTeapotFile = new CD3DFile();

    HRESULT hr1 = m_pTeapotFile->Load( "teapot.x" );
    HRESULT hr2 = m_pTeapotFile->GetMeshVertices( "", &m_pTeapotVertices,
                                                  &m_dwTeapotNumVertices );
    HRESULT hr3 = m_pTeapotFile->GetMeshIndices( "", &m_pTeapotIndices,
                                                 &m_dwTeapotNumIndices );

    if( FAILED(hr1|hr2|hr3) )
    {
        MessageBox( NULL, TEXT("Could not load TEAPOT.X file."),
                    TEXT("SphereMap Sample"), MB_OK|MB_ICONERROR );
        return E_FAIL;
    }

    // Load the spheremap texture
    if( FAILED( D3DTextr_CreateTextureFromFile( TEXT("SphereMap.bmp") ) ) )
    {
        MessageBox( NULL, TEXT("Could not load SPHEREMAP.BMP texture."),
                    TEXT("SphereMap Sample"), MB_OK|MB_ICONERROR );
        return E_FAIL;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: ApplySphereMapToObject()
// Desc: Uses the current orientation of the vertices to calculate the object's
//       spheremapped texture coords.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::ApplySphereMapToObject( D3DVERTEX* pvVertices,
                                                   DWORD dwNumVertices )
{
    // Get the current world-view matrix
    D3DMATRIX matWorld, matView, matWV;
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_VIEW,  &matView );
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
    D3DMath_MatrixMultiply( matWV, matWorld, matView );

    // Extract world-view matrix elements for speed
    FLOAT m11 = matWV._11,   m21 = matWV._21,   m31 = matWV._31;
    FLOAT m12 = matWV._12,   m22 = matWV._22,   m32 = matWV._32;
    FLOAT m13 = matWV._13,   m23 = matWV._23,   m33 = matWV._33;

    // Loop through the vertices, transforming each one and calculating
    // the correct texture coordinates.
    for( WORD i = 0; i < dwNumVertices; i++ )
    {
        FLOAT nx = pvVertices[i].nx;
        FLOAT ny = pvVertices[i].ny;
        FLOAT nz = pvVertices[i].nz;

        // Check the z-component, to skip any vertices that face backwards
        if( nx*m13 + ny*m23 + nz*m33 > 0.0f )
            continue;

        // Assign the spheremap's texture coordinates
        pvVertices[i].tu = 0.5f * ( 1.0f + ( nx*m11 + ny*m21 + nz*m31 ) );
        pvVertices[i].tv = 0.5f * ( 1.0f - ( nx*m12 + ny*m22 + nz*m32 ) );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Setup the world spin matrix
    D3DMATRIX matRotate;
    D3DUtil_SetRotationMatrix( matRotate, D3DVECTOR(1.0f,1.0f,0.0f), 
                               fTimeKey/2  );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matRotate );

    ApplySphereMapToObject( m_pTeapotVertices, m_dwTeapotNumVertices );

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
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
                         0x000000ff, 1.0f, 0L );

    // Begin the scene 
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                              m_pTeapotVertices, m_dwTeapotNumVertices, 
                              m_pTeapotIndices, m_dwTeapotNumIndices, NULL );

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
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("SphereMap.bmp") );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,        0xffffffff );

    // Get the aspect ratio
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT fAspect = ((FLOAT)vp.dwHeight) / vp.dwWidth;

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f, -4.5f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f,  0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f,  0.0f );
    D3DMATRIX matWorld, matProj;

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, fAspect, 1.0f, 20.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );
    SetViewParams( &vEyePt, &vLookatPt, &vUpVec, 0.1f);

    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    SAFE_DELETE( m_pTeapotFile );

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




