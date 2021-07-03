//-----------------------------------------------------------------------------
// File: VBuffer.cpp
//
// Desc: Example code showing how to use Direct3D vertex buffers.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"


//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define FLAG_SIZE         10
#define NUM_FLAG_VERTICES ((FLAG_SIZE+1)*(FLAG_SIZE+1))
#define NUM_FLAG_INDICES  (FLAG_SIZE*FLAG_SIZE*6)
#define NUM_POLE_VERTICES 8*2





//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    LPDIRECT3DVERTEXBUFFER7 m_pVertexBuffer;

    WORD        m_pFlagIndices[NUM_FLAG_INDICES];
    D3DVERTEX   m_pPoleVertices[NUM_POLE_VERTICES];
    D3DTLVERTEX m_pBackground[4];

    HRESULT CreateFlagVertexBuffer();

protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT RestoreSurfaces();
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
    m_strWindowTitle  = TEXT( "VBuffer: Direct3D Vertex Buffer Demo" );
    m_bAppUseZBuffer  = FALSE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;

    m_pVertexBuffer   = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    for( WORD i=0, ix=0; ix<FLAG_SIZE; ix++ )
    {
        for( WORD iy=0; iy<FLAG_SIZE; iy++ )
        {
            m_pFlagIndices[i++] = (ix+0) + (iy+1)*(FLAG_SIZE+1);
            m_pFlagIndices[i++] = (ix+1) + (iy+0)*(FLAG_SIZE+1);
            m_pFlagIndices[i++] = (ix+0) + (iy+0)*(FLAG_SIZE+1);
            m_pFlagIndices[i++] = (ix+0) + (iy+1)*(FLAG_SIZE+1);
            m_pFlagIndices[i++] = (ix+1) + (iy+1)*(FLAG_SIZE+1);
            m_pFlagIndices[i++] = (ix+1) + (iy+0)*(FLAG_SIZE+1);
        }
    }

    for( int r=0; r<8; r++ )
    {
        FLOAT theta = (r/8.0f)*2*3.1415926283f;
        FLOAT x     = (FLOAT)cos(theta)*0.1f;
        FLOAT z     = -(FLOAT)sin(theta)*0.1f;

        D3DVECTOR vNorm = Normalize( D3DVECTOR( x, 0.0f, z ) );

        m_pPoleVertices[2*r+0] = D3DVERTEX( D3DVECTOR( x, 10.0f, z ),
                                            vNorm, r/8.0f, 0.0f );
        m_pPoleVertices[2*r+1] = D3DVERTEX( D3DVECTOR( x, 0.0f, z ),
                                            vNorm, r/8.0f, 1.0f );
    }

    m_pBackground[0] = D3DTLVERTEX( D3DVECTOR( 0, 0, 0.99f ), 0.5, -1, 0, 0.0f, 0.6f );
    m_pBackground[1] = D3DTLVERTEX( D3DVECTOR( 0, 0, 0.99f ), 0.5, -1, 0, 0.0f, 0.0f );
    m_pBackground[2] = D3DTLVERTEX( D3DVECTOR( 0, 0, 0.99f ), 0.5, -1, 0, 1.0f, 0.6f );
    m_pBackground[3] = D3DTLVERTEX( D3DVECTOR( 0, 0, 0.99f ), 0.5, -1, 0, 1.0f, 0.0f );

    // Create some textures
    D3DTextr_CreateTextureFromFile( "dx5_logo.bmp" );
    D3DTextr_CreateTextureFromFile( "Cloud3.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    D3DVERTEX* pFlagVertices;
    if( SUCCEEDED( m_pVertexBuffer->Lock( DDLOCK_WAIT, (VOID**)&pFlagVertices,
                                          NULL ) ) )
    {
        for( int ix=0; ix<FLAG_SIZE+1; ix++ )
        {
            for( int iy=0; iy<FLAG_SIZE+1; iy++ )
            {
                FLOAT z = ix*0.2f*(FLOAT)sin(ix-fTimeKey*6 )/(FLAG_SIZE+1);
                pFlagVertices[ix+iy*(FLAG_SIZE+1)].z = z;
            }
        }

        m_pVertexBuffer->Unlock();
    }

    // Move the clouds
    FLOAT t = fTimeKey/40.0f;
    FLOAT u = (((DWORD)(t*10000))%10000)/10000.0f;
    m_pBackground[0].tu = 0 - u;
    m_pBackground[1].tu = 0 - u;
    m_pBackground[2].tu = 1 - u;
    m_pBackground[3].tu = 1 - u;

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
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("Cloud3.bmp") );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX,
                                     m_pBackground, 4, 0 );

        // Draw the pole
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_pPoleVertices, 16, 0 );

        //Display the object
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("dx5_logo.bmp") );
        m_pd3dDevice->DrawIndexedPrimitiveVB( D3DPT_TRIANGLELIST, 
                                              m_pVertexBuffer, 0,
                                              NUM_FLAG_VERTICES, m_pFlagIndices,
                                              NUM_FLAG_INDICES, D3DDP_WAIT );

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
    // Create and fill a vertex buffer with flag geometry
    if( FAILED( CreateFlagVertexBuffer() ) )
        return E_FAIL;
    
    // Size the background
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT fAspect = ((FLOAT)vp.dwHeight) / vp.dwWidth;
    m_pBackground[0].sy = (FLOAT)vp.dwHeight;
    m_pBackground[2].sy = (FLOAT)vp.dwHeight;
    m_pBackground[2].sx = (FLOAT)vp.dwWidth;
    m_pBackground[3].sx = (FLOAT)vp.dwWidth;

    // Create and set up the object material
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR(-1.0f, 7.5f,-3.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 2.0f, 7.5f, 0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
    D3DMATRIX matWorld, matView, matProj;

    D3DUtil_SetTranslateMatrix( matWorld, 0.0f, 0.0f, 0.0f );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, 1.57f, fAspect, 1.0f, 100.0f );
    
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set texture and miscellaneous states
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0x88888888 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, FALSE );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateFlagVertexBuffer()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::CreateFlagVertexBuffer()
{
    if( NULL == m_pVertexBuffer )
    {
        // Create the vertex buffer.
        D3DVERTEXBUFFERDESC vbdesc;
        ZeroMemory( &vbdesc, sizeof(D3DVERTEXBUFFERDESC) );
        vbdesc.dwSize        = sizeof(D3DVERTEXBUFFERDESC);
        vbdesc.dwCaps        = 0L;
        vbdesc.dwFVF         = D3DFVF_VERTEX;
        vbdesc.dwNumVertices = NUM_FLAG_VERTICES;

        // If the device does not support transform and lighting in hardware,
        // make sure the vertex buffers end up in system memory.
        if( IID_IDirect3DTnLHalDevice != (*m_pDeviceInfo->pDeviceGUID) )
            vbdesc.dwCaps |= D3DVBCAPS_SYSTEMMEMORY;

        if( FAILED( m_pD3D->CreateVertexBuffer( &vbdesc, &m_pVertexBuffer, 0L ) ) )
            return E_FAIL;
    }

    // Lock and fill the vertex buffer with Flag data
    D3DVERTEX* pFlagVertices;
    if( SUCCEEDED( m_pVertexBuffer->Lock( DDLOCK_WAIT, (VOID**)&pFlagVertices,
                                          NULL ) ) )
    {
        for( WORD ix=0; ix<FLAG_SIZE+1; ix++ )
        {
            for( WORD iy=0; iy<FLAG_SIZE+1; iy++ )
            {
                FLOAT tu = ix/(FLOAT)FLAG_SIZE;
                FLOAT tv = iy/(FLOAT)FLAG_SIZE;
                FLOAT x  = 0.2f + tu * 3.31f;
                FLOAT y  = 8.0f + tv * 1.82f;

                pFlagVertices[ix+iy*(FLAG_SIZE+1)] = D3DVERTEX( D3DVECTOR(x,y,0),
                                                                D3DVECTOR(0,0,-1),
                                                                tu, 1-tv );
            }
        }

        m_pVertexBuffer->Unlock();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RestoreSurfaces()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RestoreSurfaces()
{
    // We need to restore the contents of our vertex buffer, which may have
    // been stored in video memory. 
    CreateFlagVertexBuffer();

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

    SAFE_RELEASE( m_pVertexBuffer );

    return S_OK;
}




