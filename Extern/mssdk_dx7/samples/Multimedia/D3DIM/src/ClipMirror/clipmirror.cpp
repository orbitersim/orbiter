//-----------------------------------------------------------------------------
// File: ClipMirror.cpp
//
// Desc: This sample shows how to use clip planes to implement a planar mirror.
//       The scene is reflected in a mirror and rendered in a 2nd pass. The 
//       corners of the mirrors, together with the camera eye point, are used
//       to define a custom set of clip planes so that the reflected geometry
//       appears only within the mirror's boundaries.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "D3DFile.h"




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    CD3DFile* m_pTeapotFile; // The teapot object
    D3DMATRIX m_matTeapot;   // Teapot's local matrix

    D3DVERTEX m_vMirror[4];  // Vertices of the mirror

    D3DVECTOR m_vEyePt;      // Vectors defining the camera
    D3DVECTOR m_vLookatPt;
    D3DVECTOR m_vUpVec;

    HRESULT RenderMirror();
    HRESULT RenderScene();

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
// Name: CreatePlane()
// Desc: Helper function to get a plane equation from 3 points in space.
//-----------------------------------------------------------------------------
FLOAT* CreatePlane( FLOAT* plane, D3DVECTOR& a, D3DVECTOR& b, D3DVECTOR& c )
{
    D3DVECTOR n = Normalize( CrossProduct( c-b, c-a ) );
    plane[0] = n.x; plane[1] = n.y; plane[2] = n.z; 
    plane[3] = -( n.x*c.x + n.y*c.y + n.z*c.z );

    return plane;
}




//-----------------------------------------------------------------------------
// Name: CMyD3DApplication()
// Desc: Application constructor. Sets attributes for the app.
//-----------------------------------------------------------------------------
CMyD3DApplication::CMyD3DApplication()
                  :CD3DApplication()
{
    m_strWindowTitle  = TEXT( "ClipMirror: Using D3D Clip Planes" );
    m_bAppUseZBuffer  = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    m_pTeapotFile     = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Create and load the teapot object
    m_pTeapotFile = new CD3DFile();

    if( FAILED( m_pTeapotFile->Load( "teapot.x" ) ) )
    {
        MessageBox( NULL, TEXT("Could not load TEAPOT.X file."),
                    TEXT("ClipMirror Sample"), MB_OK|MB_ICONERROR );
        return E_FAIL;
    }

    // Initialize the mirror's vertices
    D3DVECTOR vNorm( 0.0f, 0.0f, 1.0f );
    m_vMirror[0] = D3DVERTEX( D3DVECTOR(-1.5f, 1.5f,-3.0f), vNorm, 0.0f, 0.0f );
    m_vMirror[1] = D3DVERTEX( D3DVECTOR(-1.5f,-1.5f,-3.0f), vNorm, 0.0f, 0.0f );
    m_vMirror[2] = D3DVERTEX( D3DVECTOR( 1.5f, 1.5f,-3.0f), vNorm, 0.0f, 0.0f );
    m_vMirror[3] = D3DVERTEX( D3DVECTOR( 1.5f,-1.5f,-3.0f), vNorm, 0.0f, 0.0f );

    // Initialize the camera's orientation
    m_vEyePt    = D3DVECTOR( 0.0f, 2.0f, 6.5f );
    m_vLookatPt = D3DVECTOR( 0.0f, 0.0f, 0.0f );
    m_vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Set the teapot's local moatrix (rotating about the y-axis)
    D3DUtil_SetRotateYMatrix( m_matTeapot, fTimeKey );

    // Move the camera about the z-axis
    m_vEyePt.x = 0.0f + 4.0f * sinf( fTimeKey/2.9f );
    m_vEyePt.y = 3.0f + 2.0f * cosf( fTimeKey/3.7f );
    m_vEyePt.z = sqrtf( 50.0f - m_vEyePt.x*m_vEyePt.x - m_vEyePt.y*m_vEyePt.y );

    D3DMATRIX matView;
    D3DUtil_SetViewMatrix( matView, m_vEyePt, m_vLookatPt, m_vUpVec );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RenderScene()
// Desc: Renders all objects in the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RenderScene()
{
    D3DMATRIX matLocal, matWorldSaved;
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSaved );
    
    // Build the local matrix
    D3DMath_MatrixMultiply( matLocal, m_matTeapot, matWorldSaved );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matLocal );
    
    // Render the object
    m_pTeapotFile->Render( m_pd3dDevice );

    // Restore the modified render states
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSaved );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RenderMirror()
// Desc: Renders the scene as reflected in a mirror. The corners of the mirror
//       define a plane, which is used to build the reflection matrix. The
//       scene is rendered with the cull-mode reversed, since all normals in
//       the scene are likewise reflected.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RenderMirror()
{
    // Save the world matrix so it can be restored
    D3DMATRIX matWorldSaved;
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSaved );

    // Get the four corners of the mirror
    D3DVECTOR a, b, c, d;
    a.x = m_vMirror[0].x; a.y = m_vMirror[0].y; a.z = m_vMirror[0].z;
    b.x = m_vMirror[1].x; b.y = m_vMirror[1].y; b.z = m_vMirror[1].z;
    c.x = m_vMirror[2].x; c.y = m_vMirror[2].y; c.z = m_vMirror[2].z;
    d.x = m_vMirror[3].x; d.y = m_vMirror[3].y; d.z = m_vMirror[3].z;

    // Compute the plane's mirror
    D3DVECTOR n = Normalize( CrossProduct( a-b, b-c ) );
    FLOAT D     = sqrtf( a.x*a.x + a.y*a.y + a.z*a.z );
    
    // Construct the reflection matrix
    D3DMATRIX matMirror;
    D3DUtil_SetIdentityMatrix( matMirror );
    matMirror._11 -= 2*n.x*n.x; matMirror._12 -= 2*n.x*n.y; matMirror._13 -= 2*n.x*n.z;
    matMirror._21 -= 2*n.y*n.x; matMirror._22 -= 2*n.y*n.y; matMirror._23 -= 2*n.y*n.z;
    matMirror._31 -= 2*n.z*n.x; matMirror._32 -= 2*n.z*n.y; matMirror._33 -= 2*n.z*n.z;
    matMirror._41 -= 2*D*n.x;   matMirror._42 -= 2*D*n.y;   matMirror._43 -= 2*D*n.z;
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,  &matMirror );

    // Reverse the cull mode (since normals will be reflected)
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE, D3DCULL_CW );
    
    // Set the custom clip planes (so geometry is clipped by mirror edges).
    // This is the heart of this sample. The mirror has 4 edges, so there are
    // 4 clip planes, each defined by two mirror vertices and the eye point.
    FLOAT plane[4];
    m_pd3dDevice->SetClipPlane( 0, CreatePlane( plane, a, b, m_vEyePt ) );
    m_pd3dDevice->SetClipPlane( 1, CreatePlane( plane, d, c, m_vEyePt ) );
    m_pd3dDevice->SetClipPlane( 2, CreatePlane( plane, c, a, m_vEyePt ) );
    m_pd3dDevice->SetClipPlane( 3, CreatePlane( plane, b, d, m_vEyePt ) );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CLIPPLANEENABLE, 0x0f );

    // Render the scene
    RenderScene();
        
    // Restore the modified render states
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSaved );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CLIPPLANEENABLE, 0x00 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE, D3DCULL_CCW );

    // Finally, render the mirror itself (as an alpha-blended quad)
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                 m_vMirror, 4, NULL );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE,   FALSE );
                
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
        // Render the scene
        RenderScene();

        // Render the scene in the mirror
        RenderMirror();

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
    // Set up the textures
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    
    // Set miscellaneous render states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );

    // Set up the matrices
    D3DMATRIX matWorld, matView, matProj;
    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, m_vEyePt, m_vLookatPt, m_vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, 1.0f, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Setup a material (used by the mirror)
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 0.0f, 0.0f, 0.0f, 0.5f );
    m_pd3dDevice->SetMaterial( &mtrl );

    // Set up a light
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 1.0f, -1.0f, -1.0f );
        m_pd3dDevice->SetLight( 0, &light );
        m_pd3dDevice->LightEnable( 0, TRUE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0x00000000 );
    }
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0x55555555 );

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
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    return S_OK;
}




