//-----------------------------------------------------------------------------
// File: ShadowVol.cpp
//
// Desc: Example code showing how to use stencil buffers to implement shadow
//       volumes.
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
    LPDIRECTDRAWSURFACE7 m_pddsDepthBuffer;

    CD3DFile* m_pFileObject;
    CD3DFile* m_pTerrainObject;

    D3DMATRIX m_matObjectMatrix;
    D3DMATRIX m_matTerrainMatrix;

    D3DVERTEX m_pLightVertices[4];
    D3DVERTEX m_pShadowCasterVertices[4];
    D3DVERTEX m_pShadowVolumeVertices[8];
    WORD      m_pShadowVolumeIndices[24];

    HRESULT CreateStencilBuffer();
    HRESULT DrawShadow();
    HRESULT RenderShadow();
    
    static HRESULT ConfirmDevice( DDCAPS* pddDriverCaps,
                                  D3DDEVICEDESC7* pd3dDeviceDesc );
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
    m_strWindowTitle  = TEXT( "ShadowVol: Stencil Shadow Volume Sample" );
    m_bAppUseZBuffer  = FALSE; // Note: this sample will create it's own
                               // depth buffer for stenciling
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
    // Setup vertices for the shadow caster. Simply a square for this sample
    m_pShadowCasterVertices[0] = D3DVERTEX( D3DVECTOR( -1, 2, -1 ),
                                            D3DVECTOR(0,1,0), 0, 1 );
    m_pShadowCasterVertices[1] = D3DVERTEX( D3DVECTOR( -1, 2,  1 ),
                                            D3DVECTOR(0,1,0), 0, 0 );
    m_pShadowCasterVertices[2] = D3DVERTEX( D3DVECTOR(  1, 2, -1 ),
                                            D3DVECTOR(0,1,0), 1, 1 );
    m_pShadowCasterVertices[3] = D3DVERTEX( D3DVECTOR(  1, 2,  1 ),
                                            D3DVECTOR(0,1,0), 1, 0 );

    // Setup vertices/indices for the shadow volume. Since our caster is simply
    // a square, the volume will be a rectangular prism with the caster as the
    // top cap. Note: the base of the shadow volume will be computed on a per-
    // frame basis, using the light position relative to the caster.
    WORD wIndices[] = { 1,5,3,3,5,7, 0,4,1,1,4,5, 0,2,4,4,2,6, 2,3,6,6,3,7 };
    ZeroMemory( m_pShadowVolumeVertices, 8*sizeof(D3DVERTEX) );
    memcpy( m_pShadowVolumeVertices, m_pShadowCasterVertices, 4*sizeof(D3DVERTEX) );
    memcpy( m_pShadowVolumeIndices, wIndices, sizeof(wIndices) );

    // Load an object to have in the shadow
    m_pFileObject = new CD3DFile();
    if( FAILED( m_pFileObject->Load( "Teapot.x" ) ) )
    {
        MessageBox( m_hWnd, "Couldn't load object file", m_strWindowTitle, MB_ICONERROR|MB_OK );
        return E_FAIL;
    }

    // Load some terrain
    m_pTerrainObject = new CD3DFile();
    if( FAILED( m_pTerrainObject->Load( "SeaFloor.x" ) ) )
    {
        MessageBox( m_hWnd, "Couldn't load object file", m_strWindowTitle, MB_ICONERROR|MB_OK );
        return E_FAIL;
    }

    // Get the terrain vertices, so we can tweak 'em
    D3DVERTEX* pVertices;
    DWORD      dwNumVertices;
    if( FAILED( m_pTerrainObject->GetMeshVertices( "Seafloor", &pVertices, &dwNumVertices ) ) )
    {
        MessageBox( m_hWnd, "Bad object file", m_strWindowTitle, MB_ICONERROR|MB_OK );
        return E_FAIL;
    }

    // Add more bumpiness to the terrain object
    for( DWORD i=0; i<dwNumVertices; i++ )
    {
        pVertices[i].x  *= 0.6f;
        pVertices[i].z  *= 0.6f;
        pVertices[i].y  += 1*(rand()/(FLOAT)RAND_MAX);
        pVertices[i].y  += 2*(rand()/(FLOAT)RAND_MAX);
        pVertices[i].y  += 1*(rand()/(FLOAT)RAND_MAX);
        pVertices[i].tu *= 3;
        pVertices[i].tv *= 3;
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
    // Compute the Shadow and rotate angles for this frame
    static FLOAT fTime = 0.0f;
    FLOAT fRotateAngle = (FLOAT)( fTime / 7 );
    FLOAT fShadowAngle   = (FLOAT)( (sin(fTime)+1.0f)*0.6f );
    fTime += .50f;

    // Setup the world spin matrix
    D3DMATRIX matScale, matRotate;
    D3DUtil_SetScaleMatrix( matScale, 2.0f, 2.0f, 2.0f );
    D3DUtil_SetRotateYMatrix( matRotate, -fRotateAngle );
    D3DMath_MatrixMultiply( m_matObjectMatrix, matRotate, matScale );
    
    D3DUtil_SetScaleMatrix( m_matTerrainMatrix, 1.0f, 1.0f, 1.0f );

    // Move the light
    FLOAT Lx =  2 * sinf(fTimeKey/1);
    FLOAT Ly =  1 * sinf(fTimeKey/19) + 5;
    FLOAT Lz =  2 * cosf(fTimeKey/1);
    D3DLIGHT7 light;
    D3DUtil_InitLight( light, D3DLIGHT_POINT, Lx, Ly, Lz );
    light.dvAttenuation0 = 0.9f;
    light.dvAttenuation1 = 0.0f;
    m_pd3dDevice->SetLight( 0, &light );

    m_pLightVertices[0] = D3DVERTEX( D3DVECTOR(-0.3f+Lx, Ly,-0.3f+Lz ), D3DVECTOR(0,1,0), 0, 1 );
    m_pLightVertices[1] = D3DVERTEX( D3DVECTOR(-0.3f+Lx, Ly, 0.3f+Lz ), D3DVECTOR(0,1,0), 0, 0 );
    m_pLightVertices[2] = D3DVERTEX( D3DVECTOR( 0.3f+Lx, Ly,-0.3f+Lz ), D3DVECTOR(0,1,0), 1, 1 );
    m_pLightVertices[3] = D3DVERTEX( D3DVECTOR( 0.3f+Lx, Ly, 0.3f+Lz ), D3DVECTOR(0,1,0), 1, 0 );

    // Build the shadow volume. The upper half (1st 4 vertices) remains fixed,
    // but the lower half (2nd 4 vertices) is extruded from the point of the
    // light source. The scale of the extrusion (using 25 for this sample) is
    // some arbitrary value. large enough to go below the scene's terrain.
    for( DWORD i=0; i<4; i++ )
    {
        FLOAT dx = Lx - m_pShadowVolumeVertices[i].x;
        FLOAT dy = Ly - m_pShadowVolumeVertices[i].y;
        FLOAT dz = Lz - m_pShadowVolumeVertices[i].z;
        m_pShadowVolumeVertices[i+4].x = m_pShadowVolumeVertices[i].x - (25*dx/dy);
        m_pShadowVolumeVertices[i+4].y = m_pShadowVolumeVertices[i].y - 25;
        m_pShadowVolumeVertices[i+4].z = m_pShadowVolumeVertices[i].z - (25*dz/dy);
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: RenderShadow()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::RenderShadow()
{
    // Turn depth buffer off, and stencil buffer on
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZWRITEENABLE,  FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE, TRUE );

    // Set up stencil compare fuction, reference value, and masks
    // Stencil test passes if ((ref & mask) cmpfn (stencil & mask)) is true
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFUNC,     D3DCMP_ALWAYS );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF,      0x1 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILMASK,     0xffffffff );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILWRITEMASK,0xffffffff );

    // If ztest passes, write 1 into stencil buffer
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILZFAIL, D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFAIL,  D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS,  D3DSTENCILOP_REPLACE );

    // Make sure that no pixels get drawn to the frame buffer
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE );

    // Draw front-side of shadow volume in stencil/z only
    m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                        m_pShadowVolumeVertices, 8,
                                        m_pShadowVolumeIndices, 24, NULL );

    // Now reverse cull order so back sides of shadow volume are written,
    // writing 0's into stencil. Result will be any pixel which still has a bit
    // set in the stencil buffer, is inside the shadow.
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF, 0x0 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE,   D3DCULL_CW );

    // Draw back-side of shadow volume in stencil/z only
    m_pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                        m_pShadowVolumeVertices, 8,
                                        m_pShadowVolumeIndices, 24, NULL );

    // Restore render states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_CULLMODE, D3DCULL_CCW );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZWRITEENABLE,     TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE,    FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DrawShadow()
// Desc: Draws a big gray polygon over scene according to the mask in the
//       stencil buffer. (Any pixel with stencil==1 is in the shadow.)
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DrawShadow()
{
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,       FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE, TRUE );

    // Turn on alphablending
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND, D3DBLEND_SRCALPHA );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA );

    // Only write where the stencil value == 1
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF,  0x1 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFUNC, D3DCMP_EQUAL );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS, D3DSTENCILOP_KEEP );

    // Get viewport dimensions for big, gray square
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport(&vp);
    FLOAT sx = (FLOAT)vp.dwWidth;
    FLOAT sy = (FLOAT)vp.dwHeight;

    // Draw a big, gray square
    D3DTLVERTEX vBigGraySquare[4];
    vBigGraySquare[0] = D3DTLVERTEX( D3DVECTOR( 0,sy,0.0f),1.0f,0x7f000000,0,0,0 );
    vBigGraySquare[1] = D3DTLVERTEX( D3DVECTOR( 0, 0,0.0f),1.0f,0x7f000000,0,0,0 );
    vBigGraySquare[2] = D3DTLVERTEX( D3DVECTOR(sx,sy,0.0f),1.0f,0x7f000000,0,0,0 );
    vBigGraySquare[3] = D3DTLVERTEX( D3DVECTOR(sx, 0,0.0f),1.0f,0x7f000000,0,0,0 );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX,
                                 vBigGraySquare, 4, NULL );

    // Restore render states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,          TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE,    FALSE );
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
    // Clear the viewport, zbuffer, and stencil buffer
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER|D3DCLEAR_STENCIL,
                         0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &m_matTerrainMatrix );
        m_pTerrainObject->Render( m_pd3dDevice );
    
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &m_matObjectMatrix );
        m_pFileObject->Render( m_pd3dDevice );
        
        // Render light
        D3DMATRIX matIdentity;
        D3DUtil_SetIdentityMatrix( matIdentity );
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matIdentity );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_pLightVertices, 4, NULL );

        // Draw shadowcaster
        m_pd3dDevice->SetTexture( 0, NULL);
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_pShadowCasterVertices, 4, NULL );

        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                     m_pShadowVolumeVertices+4, 4, NULL );

       

        // Render the shadow volume into the stenicl buffer, then add it into
        // the scene
        RenderShadow();
        DrawShadow();

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

    // Create and set up the shine materials w/ textures
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0, 10, -16.5 );
    D3DVECTOR vLookatPt = D3DVECTOR( 0, 0,   0  );
    D3DVECTOR vUpVec    = D3DVECTOR( 0, 1,   0  );
    D3DMATRIX matWorld, matView, matProj;

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, 1.0f, 1.0f, 100.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    m_pd3dDevice->LightEnable( 0, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,  0x30303030 );

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

    // Make sure device supports point lights
    if( 0 == ( pd3dDeviceDesc->dwVertexProcessingCaps &
                                            D3DVTXPCAPS_POSITIONALLIGHTS ) )
        return E_FAIL;

    return S_OK;
}




