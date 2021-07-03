//-----------------------------------------------------------------------------
// File: StencilDepth.cpp
//
// Desc: Example code showing how to use stencil buffers to show the depth
//       complexity of a scene.
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
#include "resource.h"




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    LPDIRECTDRAWSURFACE7 m_pddsDepthBuffer; // Depth/stencil buffer
    BOOL      m_bShowDepthComplexity;
    CD3DFile* m_pFileObject;

    HRESULT CreateStencilBuffer();
    VOID    SetStatesForRecordingDepthComplexity();
    VOID    ShowDepthComplexity();
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
    static HRESULT ConfirmDevice( DDCAPS* pddDriverCaps,
                           D3DDEVICEDESC7* pd3dDeviceDesc );
    LRESULT MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam );

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
    m_strWindowTitle  = TEXT( "StencilDepth: Shows the Scene Depth Complexity" );
    m_bAppUseZBuffer  = FALSE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;

    m_bShowDepthComplexity = TRUE;
    m_pFileObject          = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Load a .X file
    m_pFileObject = new CD3DFile();
    if( FAILED( m_pFileObject->Load( "heli.x" ) ) )
        return E_FAIL;

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

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetStatesForRecordingDepthComplexity()
// Desc: Turns on stencil and other states for recording the depth complexity
//       during the rendering of a scene.
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::SetStatesForRecordingDepthComplexity()
{
    // Clear the stencil buffer
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_STENCIL, 0x0, 0.0f, 0L );

    // Turn stenciling
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFUNC,     D3DCMP_ALWAYS );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF,      0 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILMASK,     0x00000000 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILWRITEMASK,0xffffffff );
    
    // Increment the stencil buffer for each pixel drawn
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILZFAIL, D3DSTENCILOP_INCRSAT );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFAIL,  D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS,  D3DSTENCILOP_INCRSAT );
}




//-----------------------------------------------------------------------------
// Name: ShowDepthComplexity()
// Desc: Draws the contents of the stencil buffer in false color. Use alpha
//       blending of one red, one green, and one blue rectangle to do false
//       coloring of bits 1, 2, and 4 in the stencil buffer.
//-----------------------------------------------------------------------------
VOID CMyD3DApplication::ShowDepthComplexity()
{
    // Get viewport dimensions for the rectangle
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport( &vp );
    FLOAT sx = (FLOAT)vp.dwWidth;
    FLOAT sy = (FLOAT)vp.dwHeight;

    // Build the viewport-covering recangle
    D3DTLVERTEX v[4];
    v[0] = D3DTLVERTEX( D3DVECTOR(  0, sy, 0.5f ), 0.5f, 0, 0, 0, 0 );
    v[1] = D3DTLVERTEX( D3DVECTOR(  0,  0, 0.5f ), 0.5f, 0, 0, 0, 0 );
    v[2] = D3DTLVERTEX( D3DVECTOR( sx, sy, 0.5f ), 0.5f, 0, 0, 0, 0 );
    v[3] = D3DTLVERTEX( D3DVECTOR( sx,  0, 0.5f ), 0.5f, 0, 0, 0, 0 );

    // Turn off the buffer, and enable alpha blending
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,          FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCCOLOR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCCOLOR );

    // Set up the stencil states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILZFAIL, D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFAIL,  D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILPASS,  D3DSTENCILOP_KEEP );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILFUNC,  D3DCMP_NOTEQUAL );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILREF,   0 );

    // Set the background to black
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET, 0x00000000, 1.0f, 0L );

    // Draw a red rectangle wherever the 1st stencil bit is set
    v[0].color = v[1].color = v[2].color = v[3].color = 0xffff0000;
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILMASK, 0x01 );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX, v, 4, 0 );

    // Draw a green rectangle wherever the 2nd stencil bit is set
    v[0].color = v[1].color = v[2].color = v[3].color = 0xff00ff00;
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILMASK, 0x02 );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX, v, 4, 0 );

    // Draw a blue rectangle wherever the 3rd stencil bit is set
    v[0].color = v[1].color = v[2].color = v[3].color = 0xff0000ff;
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILMASK, 0x04 );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX, v, 4, 0 );

    // Restore states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,          TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_STENCILENABLE,    FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );
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
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER, 0x000000ff,
                         1.0f, 0L );

    // Begin the scene 
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        if( m_bShowDepthComplexity )
            SetStatesForRecordingDepthComplexity();

        // Render the scene
        m_pFileObject->Render( m_pd3dDevice );

        // Show the depth complexity of the scene
        if( m_bShowDepthComplexity )
            ShowDepthComplexity();

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

    // Looking for a zbuffer with 4 or more stencil bits
    if( pddpf->dwStencilBitDepth >= 4 )
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

    // Create the stencil buffer
    if( FAILED( hr = CreateStencilBuffer() ) )
        return hr;

    // Setup textures (the .X file may have textures)
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_COLORVERTEX,    TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,        0x40404040 );

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0, 0, -15.0f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0, 0,   0  );
    D3DVECTOR vUpVec    = D3DVECTOR( 0, 1,   0  );
    D3DMATRIX matWorld, matView, matProj;

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/4, 1.0f, 1.0f, 100.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Setup a material
    D3DMATERIAL7      mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    m_pd3dDevice->SetMaterial( &mtrl );

    // Set up the light
    if( m_pDeviceInfo->ddDeviceDesc.dwVertexProcessingCaps &
                                                D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.0f, -1.0f, 0.0f );
        m_pd3dDevice->SetLight( 0, &light );
        m_pd3dDevice->LightEnable( 0, TRUE );
        m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
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

    if( dwStencilCaps & (D3DSTENCILCAPS_KEEP|D3DSTENCILCAPS_INCRSAT) )
        return S_OK;

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: Overrrides the main WndProc, so the sample can do custom message 
//       handling (e.g. processing mouse, keyboard, or menu commands).
//-----------------------------------------------------------------------------
LRESULT CMyD3DApplication::MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                                    LPARAM lParam )
{
    if( WM_COMMAND == uMsg )
    {
        switch( LOWORD(wParam) )
        {
            case IDM_SHOWDEPTHCOMPLEXITY:
                m_bShowDepthComplexity = !m_bShowDepthComplexity;
                CheckMenuItem( GetMenu(hWnd), IDM_SHOWDEPTHCOMPLEXITY,
                          m_bShowDepthComplexity ? MF_CHECKED : MF_UNCHECKED );
                break;
        }
    }

    return CD3DApplication::MsgProc( hWnd, uMsg, wParam, lParam );
}



