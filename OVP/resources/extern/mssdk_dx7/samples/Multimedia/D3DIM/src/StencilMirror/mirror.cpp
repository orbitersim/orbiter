//-----------------------------------------------------------------------------
// File: Mirror.cpp
//
// Desc: Example code showing how to use stencil buffers to implement planar
//       mirrors.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
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
    LPDIRECTDRAWSURFACE7 m_pddsDepthBuffer; // Depth/stencil buffer

    D3DVERTEX    m_pMirrorVertices[4];      // Vertices of mirror
    D3DMATERIAL7 m_mtrlMirrorMaterial;      // Material of the mirror
    D3DMATRIX    m_matMirrorMatrix;         // Matrix to position mirror

    CD3DFile*    m_pTerrainObject;          // X file of terrain
    D3DMATRIX    m_matTerrainMatrix;        // Matrix to position terrain

    CD3DFile*    m_pFileObject;             // X file object to render
    D3DMATRIX    m_matFileObjectMatrix;     // Matrix to animate X file object

    HRESULT RenderScene();
    HRESULT RenderMirror();
    HRESULT CreateStencilBuffer();
    static HRESULT WINAPI EnumZBufferFormatsCallback( DDPIXELFORMAT* pddpf,
                                                      VOID* pddpfDesired );
protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );
    HRESULT FinalCleanup();

public:
    CMyD3DApplication();

    static HRESULT ConfirmDevice( DDCAPS* pddDriverCaps,
                                  D3DDEVICEDESC7* pd3dDeviceDesc );
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
    m_strWindowTitle  = TEXT( "StencilMirror: Stencil Mirror Sample" );
    m_bAppUseZBuffer  = FALSE; // We will create our own zbuffer w/stencil
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    m_pddsDepthBuffer = NULL;
    m_pFileObject     = NULL;
    m_pTerrainObject  = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Create a background texture
    D3DTextr_CreateTextureFromFile("seafloor.bmp");

    m_pMirrorVertices[0] = D3DVERTEX( D3DVECTOR( -20.5f, 0.0f,-20.5f ),
                                      D3DVECTOR(0,1,0), 0.0f, 1.0f );
    m_pMirrorVertices[1] = D3DVERTEX( D3DVECTOR( -20.5f, 0.0f, 20.5f ),
                                      D3DVECTOR(0,1,0), 0.0f, 0.0f );
    m_pMirrorVertices[2] = D3DVERTEX( D3DVECTOR(  20.5f, 0.0f,-20.5f ),
                                      D3DVECTOR(0,1,0), 1.0f, 1.0f );
    m_pMirrorVertices[3] = D3DVERTEX( D3DVECTOR(  20.5f, 0.0f, 20.5f ),
                                      D3DVECTOR(0,1,0), 1.0f, 0.0f );

    // Load the main file object
    m_pFileObject = new CD3DFile();
    if( FAILED( m_pFileObject->Load( "heli.x" ) ) )
        return E_FAIL;

    // Load the terrain
    m_pTerrainObject = new CD3DFile();
    if( FAILED( m_pTerrainObject->Load( "seafloor.x" ) ) )
        return E_FAIL;

    // Get the terrain vertices, so we can tweak 'em
    D3DVERTEX* pVertices;
    DWORD      dwNumVertices;
    if( FAILED( m_pTerrainObject->GetMeshVertices( "Seafloor", &pVertices, &dwNumVertices ) ) )
        return E_FAIL;

    // Add more bumpiness to the terrain object
    for( DWORD i=0; i<dwNumVertices; i++ )
    {
        pVertices[i].y  += (rand()/(FLOAT)RAND_MAX);
        pVertices[i].y  += (rand()/(FLOAT)RAND_MAX);
        pVertices[i].y  += (rand()/(FLOAT)RAND_MAX);
        pVertices[i].tu *= 10;
        pVertices[i].tv *= 10;
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
    // Position the terrain
    D3DUtil_SetTranslateMatrix( m_matTerrainMatrix, 0.0f, 8.0f, 0.0f );

    // Position the mirror (water polygon intersecting with terrain)
    D3DUtil_SetTranslateMatrix( m_matMirrorMatrix, 0.0f, 0.0f, 0.0f );

    // Position and animate the main object
    FLOAT fObjectPosX = 10.0f*sinf(fTimeKey/2);
    FLOAT fObjectPosY =  3.0f;
    FLOAT fObjectPosZ = 10.0f*cosf(fTimeKey/2);
    D3DMATRIX matRotate, matScale, matTranslate;
    D3DUtil_SetRotateYMatrix( matRotate, fTimeKey/2-g_PI/2  );
    D3DUtil_SetScaleMatrix( matScale, 0.5f, 0.5f, 0.5f );
    D3DUtil_SetTranslateMatrix( matTranslate, fObjectPosX, fObjectPosY, fObjectPosZ );
    D3DMath_MatrixMultiply( m_matFileObjectMatrix, matRotate, matTranslate );
    D3DMath_MatrixMultiply( m_matFileObjectMatrix, matScale, m_matFileObjectMatrix );

    // Move the camera around
    FLOAT fEyeX = 15.0f * sinf( fTimeKey/9.0f );
    FLOAT fEyeY =  3.0f * sinf( fTimeKey/25.0f ) + 5.5f;
    FLOAT fEyeZ = 15.0f * cosf( fTimeKey/9.0f );
    D3DMATRIX matView;
    D3DVECTOR vEyePt    = D3DVECTOR( fEyeX, fEyeY, fEyeZ );
    D3DVECTOR vLookatPt = D3DVECTOR( fObjectPosX, fObjectPosY, fObjectPosZ );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RenderScene()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RenderScene()
{
    // Render terrain
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &m_matTerrainMatrix );
    m_pTerrainObject->Render( m_pd3dDevice );

    // Draw the mirror
    m_pd3dDevice->SetTexture( 0, NULL );
    m_pd3dDevice->SetMaterial( &m_mtrlMirrorMaterial );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &m_matMirrorMatrix );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,     D3DBLEND_DESTCOLOR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND,    D3DBLEND_ZERO );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                 m_pMirrorVertices, 4, NULL );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
    
    // Draw the object. Note: do this last, in case the object has alpha
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &m_matFileObjectMatrix );
    m_pFileObject->Render( m_pd3dDevice );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RenderMirror()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RenderMirror()
{
    // Turn depth buffer off, and stencil buffer on
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFUNC,     D3DCMP_ALWAYS );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF,      0x1 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILMASK,     0xffffffff );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILWRITEMASK,0xffffffff );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILZFAIL, D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFAIL,  D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS,  D3DSTENCILOP_REPLACE );

    // Make sure no pixels are written to the z-buffer or frame buffer
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZWRITEENABLE,  FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE );

    // Draw the reflecting surface into the stencil buffer
    m_pd3dDevice->SetTexture( 0, NULL);
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &m_matMirrorMatrix );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                 m_pMirrorVertices, 4, NULL );

    // Reflect camera in X-Z plane mirror
    D3DMATRIX matView, matViewSaved, matScale;
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_VIEW, &matViewSaved );
    D3DUtil_SetScaleMatrix( matScale, 1, -1, 1 );
    D3DMath_MatrixMultiply( matView, matScale, matViewSaved );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    // Setup render states to a blended render scene against mask in stencil
    // buffer. An important step here is to reverse the cull-order of the
    // polygons, since the view matrix is being relected.
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZWRITEENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFUNC,  D3DCMP_EQUAL );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS,  D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,     D3DBLEND_DESTCOLOR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND,    D3DBLEND_ZERO );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE,     D3DCULL_CW );

    // Clear the zbuffer (leave frame- and stencil-buffer intact, obviously)
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_ZBUFFER, 0L, 1.0f, 0L );

    // Render the scene
    RenderScene();

    // Restore render states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE,         D3DCULL_CCW );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE,    FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
    m_pd3dDevice->SetTransform(   D3DTRANSFORMSTATE_VIEW,          &matViewSaved );

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

    // Clear the viewport, zbuffer, and stencil buffer
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL,
                         0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        // Render the scene
        RenderScene();

        // Render the reflection in the mirror
        RenderMirror();

        // End the scene.
        m_pd3dDevice->EndScene();
     }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: EnumZBufferFormatsCallback()
// Desc: Enumeration function to report valid pixel formats for z-buffers.
//-----------------------------------------------------------------------------
HRESULT WINAPI CMyD3DApplication::EnumZBufferFormatsCallback( DDPIXELFORMAT* pddpf,
                                                              VOID* pContext )
{
    DDSURFACEDESC2* pddsdOut = (DDSURFACEDESC2*)pContext;

    // Looking for a zbuffer with 1 or more stencil bits
    if( pddpf->dwStencilBitDepth >= 1 )
    {
        pddsdOut->ddpfPixelFormat = (*pddpf);
        return D3DENUMRET_CANCEL;
    }

    return D3DENUMRET_OK;
}




//-----------------------------------------------------------------------------
// Name: CreateStencilBuffer()
// Desc: Creates a depth buffer capable of z-buffering and stencil-buffering.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::CreateStencilBuffer()
{
    DDSURFACEDESC2 ddsd;
    ddsd.dwSize = sizeof(ddsd);

    // Get depth-buffer dimensions from the render target
    m_pddsRenderTarget->GetSurfaceDesc( &ddsd );
    DWORD dwFrameBufferDepth = ddsd.ddpfPixelFormat.dwRGBBitCount;

    // Setup the depth-buffer surface description
    ddsd.dwFlags = DDSD_WIDTH|DDSD_HEIGHT|DDSD_CAPS|DDSD_PIXELFORMAT;

    if( m_pDeviceInfo->bHardware )
        ddsd.ddsCaps.dwCaps = DDSCAPS_ZBUFFER|DDSCAPS_VIDEOMEMORY;
    else
        ddsd.ddsCaps.dwCaps = DDSCAPS_ZBUFFER|DDSCAPS_SYSTEMMEMORY;

    // Get an appropiate pixel format from enumeration of the formats.
    ddsd.ddpfPixelFormat.dwFlags = DDPF_ZBUFFER|DDPF_STENCILBUFFER;
    m_pD3D->EnumZBufferFormats( (*m_pDeviceInfo->pDeviceGUID),
                                EnumZBufferFormatsCallback, (VOID*)&ddsd );

    // Release the old depth-buffer, in case there was one
    m_pddsRenderTarget->DeleteAttachedSurface( 0, NULL );

    // Create and attach a depth-buffer. The SetRenderTarget() call is needed
    // to rebuild internal structures for the newly attached depth-buffer.
    if( FAILED( m_pDD->CreateSurface( &ddsd, &m_pddsDepthBuffer, NULL ) ) )
        return E_FAIL;
    if( FAILED( m_pddsRenderTarget->AddAttachedSurface( m_pddsDepthBuffer ) ) )
        return E_FAIL;
    if( FAILED( m_pd3dDevice->SetRenderTarget( m_pddsRenderTarget, 0L ) ) )
    {
        // See if call failed due to invalid zbuffer depth. (Some cards require
        // that zbuffer depth == frame buffer depth).
        if( dwFrameBufferDepth != ddsd.ddpfPixelFormat.dwRGBBitCount )
            return D3DFWERR_INVALIDZBUFFERDEPTH;
        else
            return E_FAIL;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    HRESULT hr;

    // Create the stencil buffer, and reset the viewport which gets trashed
    // in the process.
    if( FAILED( hr = CreateStencilBuffer() ) )
        return hr;

    // Set up a material
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 0.5f, 0.8f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    D3DUtil_InitMaterial( m_mtrlMirrorMaterial, 0.5f, 0.8f, 1.0f );

    // Set up misc render states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,        0x80808080 );

    // Set up textures
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_SELECTARG1 );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 5.5f, -15.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 1.5f,   0.0f  );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f,   0.0f  );
    D3DMATRIX matWorld, matView, matProj;

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, 1.0f, 1.0f, 50.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set up the light
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.0f, -1.0f, 1.0f );
        m_pd3dDevice->SetLight( 0, &light );
        m_pd3dDevice->LightEnable( 0, TRUE );
    }

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

    SAFE_RELEASE( m_pddsDepthBuffer );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FinalCleanup()
{
    SAFE_DELETE( m_pFileObject );
    SAFE_DELETE( m_pTerrainObject );

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
    // Get device's stencil caps
    DWORD dwStencilCaps = pd3dDeviceDesc->dwStencilCaps;

    if( 0 == dwStencilCaps )
        return E_FAIL;

    return S_OK;
}




