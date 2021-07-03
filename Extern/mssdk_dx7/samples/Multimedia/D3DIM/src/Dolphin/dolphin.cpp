//-----------------------------------------------------------------------------
// File: Dolphin.cpp
//
// Desc: Sample of swimming dolphin
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <stdio.h>
#include "D3DApp.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "D3DTextr.h"
#include "D3DFile.h"



#define WATER_COLOR          0x00006688




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Main class to run this application. Most functionality is inherited
//       from the CD3DApplication base class.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    // The DirectX file objects
    CD3DFile* m_pDolphinGroupObject;
    CD3DFile* m_pDolphinObject;
    CD3DFile* m_pFloorObject;

    // Vertex data from the file objects
    D3DVERTEX* m_pDolphinVertices;
    D3DVERTEX* m_pDolphin1Vertices;
    D3DVERTEX* m_pDolphin2Vertices;
    D3DVERTEX* m_pDolphin3Vertices;
    DWORD      m_dwNumDolphinVertices;
    
    D3DVERTEX* m_pFloorVertices;
    DWORD      m_dwNumFloorVertices;

public:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT );
    HRESULT FinalCleanup();

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
// Desc: Constructor
//-----------------------------------------------------------------------------
CMyD3DApplication::CMyD3DApplication()
                  :CD3DApplication()
{
    // Override base class members
    m_strWindowTitle  = TEXT("Dolphin: Blending Meshes in Real Time");
    m_bAppUseZBuffer  = TRUE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    // Initialize member variables
    m_pDolphinGroupObject = NULL;
    m_pDolphinObject      = NULL;
    m_pFloorObject        = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    HRESULT hr;

    m_pDolphinGroupObject = new CD3DFile();
    m_pDolphinObject      = new CD3DFile();
    m_pFloorObject        = new CD3DFile();

    hr  = m_pDolphinGroupObject->Load( "dolphin_group.x" );
    hr |= m_pDolphinGroupObject->GetMeshVertices( "Dolph01",
                              &m_pDolphin1Vertices, &m_dwNumDolphinVertices );
    hr |= m_pDolphinGroupObject->GetMeshVertices( "Dolph02",
                              &m_pDolphin2Vertices, &m_dwNumDolphinVertices );
    hr |= m_pDolphinGroupObject->GetMeshVertices( "Dolph03",
                              &m_pDolphin3Vertices, &m_dwNumDolphinVertices );

    if( FAILED(hr) )
    {
        MessageBox( NULL, TEXT("Error loading DOLPHIN_GROUP.X file"),
                    TEXT("Dolphins"), MB_OK|MB_ICONERROR );
        return E_FAIL;
    }

    hr  = m_pDolphinObject->Load( "dolphin.x" );
    hr |= m_pDolphinObject->GetMeshVertices( "Dolph02", &m_pDolphinVertices,
                                             &m_dwNumDolphinVertices );
    if( FAILED(hr) )
    {
        MessageBox( NULL, TEXT("Error loading DOLPHIN.X file"),
                    TEXT("Dolphins"), MB_OK|MB_ICONERROR );
        return E_FAIL;
    }

    hr  = m_pFloorObject->Load( "seafloor.x" );
    hr |= m_pFloorObject->GetMeshVertices( "SeaFloor", &m_pFloorVertices,
                                           &m_dwNumFloorVertices );
    if( FAILED(hr) )
    {
        MessageBox( NULL, TEXT("Error loading SEAFLOOR.X file"),
                    TEXT("Dolphins"), MB_OK|MB_ICONERROR );
        return E_FAIL;
    }

    D3DTextr_CreateTextureFromFile( "seafloor.bmp" );
    D3DTextr_CreateTextureFromFile( "dolphin.bmp" );
    
    srand(5);

    // Scale the sea floor vertices, and add some bumpiness
    for( DWORD i=0; i<m_dwNumFloorVertices; i++ )
    {
        m_pFloorVertices[i].y  += (rand()/(FLOAT)RAND_MAX);
        m_pFloorVertices[i].y  += (rand()/(FLOAT)RAND_MAX);
        m_pFloorVertices[i].y  += (rand()/(FLOAT)RAND_MAX);
        m_pFloorVertices[i].tu *= 10;
        m_pFloorVertices[i].tv *= 10;
    }

    // Scale the dolphin vertices (the model file is too big)
    for( i=0; i<m_dwNumDolphinVertices; i++ )
    {
        D3DVECTOR vScale( 0.01f, 0.01f, 0.01f );
        *((D3DVECTOR*)(&m_pDolphin1Vertices[i])) *= vScale;
        *((D3DVECTOR*)(&m_pDolphin2Vertices[i])) *= vScale;
        *((D3DVECTOR*)(&m_pDolphin3Vertices[i])) *= vScale;
    }
    
    return S_OK;
}


//-----------------------------------------------------------------------------
// Name: BlendMeshes()
// Desc: Does a linear interpolation between all vertex positions and normals
//       in two source meshes and outputs the result to the destination mesh.
//       This function assumes that all strided vertices have the same stride,
//       and that each mesh contains the same number of vertices
//-----------------------------------------------------------------------------
VOID BlendMeshes( D3DVERTEX* pDstMesh, D3DVERTEX* pSrcMesh1,
                  D3DVERTEX* pSrcMesh2, DWORD dwNumVertices, FLOAT fWeight )
{
    FLOAT fInvWeight = 1.0f - fWeight;

    // LERP positions and normals
    for( DWORD i=0; i<dwNumVertices; i++ )
    {
        pDstMesh->x  = fWeight*pSrcMesh1->x  + fInvWeight*pSrcMesh2->x;
        pDstMesh->y  = fWeight*pSrcMesh1->y  + fInvWeight*pSrcMesh2->y;
        pDstMesh->z  = fWeight*pSrcMesh1->z  + fInvWeight*pSrcMesh2->z;
        pDstMesh->nx = fWeight*pSrcMesh1->nx + fInvWeight*pSrcMesh2->nx;
        pDstMesh->ny = fWeight*pSrcMesh1->ny + fInvWeight*pSrcMesh2->ny;
        pDstMesh->nz = fWeight*pSrcMesh1->nz + fInvWeight*pSrcMesh2->nz;

        pDstMesh++;
        pSrcMesh1++;
        pSrcMesh2++;
    }
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    FLOAT fKickFreq = 2*fTimeKey;

    // Animate the dolphin mesh
    FLOAT fWeight = (FLOAT)sin(fKickFreq);

    if( fWeight < 0.0f )
    {
        BlendMeshes( m_pDolphinVertices, m_pDolphin3Vertices, 
                     m_pDolphin2Vertices, m_dwNumDolphinVertices, -fWeight );
    }
    else
    {
        BlendMeshes( m_pDolphinVertices, m_pDolphin1Vertices, 
                     m_pDolphin2Vertices, m_dwNumDolphinVertices, fWeight );
    }

    // Move the dolphin in a circle
    CD3DFileObject* pObject = m_pDolphinObject->FindObject( "x3ds_Dolph02" );
    if( pObject )
    {
        D3DMATRIX* pmatDolphin = pObject->GetMatrix();
        FLOAT      fPhase = fTimeKey/3;
        
        D3DMATRIX matTrans1, matRotate1, matRotate2;
        D3DUtil_SetRotateZMatrix( matRotate1, -(FLOAT)cos(fKickFreq)/6 );
        D3DUtil_SetRotateYMatrix( matRotate2, fPhase );
        D3DUtil_SetTranslateMatrix( matTrans1, -5*(FLOAT)sin(fPhase), 
                                    (FLOAT)sin(fKickFreq)/2, 10-10*(FLOAT)cos(fPhase) );

        D3DUtil_SetIdentityMatrix( *pmatDolphin );
        D3DMath_MatrixMultiply( *pmatDolphin, matTrans1,  *pmatDolphin );
        D3DMath_MatrixMultiply( *pmatDolphin, matRotate2, *pmatDolphin );
        D3DMath_MatrixMultiply( *pmatDolphin, matRotate1, *pmatDolphin );
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
    // Clear the viewport
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
                         WATER_COLOR, 1.0f, 0L );

    // Begin the scene 
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        m_pFloorObject->Render( m_pd3dDevice );

        m_pDolphinObject->Render( m_pd3dDevice );

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
    // Set up the lighting states
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.0f, -1.0f, 0.0f );
        m_pd3dDevice->SetLight( 0, &light );
        m_pd3dDevice->LightEnable( 0, TRUE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
    }
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0x33333333 );

    // Set the transform matrices
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT fAspect = ((FLOAT)vp.dwHeight) / vp.dwWidth;

    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f, -10.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f,   0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f,   0.0f );
    D3DMATRIX matWorld, matProj;

    D3DUtil_SetIdentityMatrix( matWorld );
    SetViewParams( &vEyePt, &vLookatPt, &vUpVec , 0.1f);
    D3DUtil_SetProjectionMatrix( matProj, g_PI/3, fAspect, 1.0f, 1000.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set up textures
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    // Set default render states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );

    // Turn on fog
    FLOAT fFogStart =  1.0f;
    FLOAT fFogEnd   = 50.0f;
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGENABLE,    TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGCOLOR,     WATER_COLOR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGTABLEMODE, D3DFOG_NONE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGVERTEXMODE,  D3DFOG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGSTART, *((DWORD *)(&fFogStart)) );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGEND,   *((DWORD *)(&fFogEnd)) );

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
    SAFE_DELETE( m_pDolphinGroupObject );
    SAFE_DELETE( m_pDolphinObject );
    SAFE_DELETE( m_pFloorObject );

    return S_OK;
}




