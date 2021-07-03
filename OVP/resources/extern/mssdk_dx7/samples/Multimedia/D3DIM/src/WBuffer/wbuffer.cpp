//-----------------------------------------------------------------------------
// File: WBuffer.cpp
//
// Desc: Example code showing how to use a w-buffer in Direct3D. This sample
//       draws a reen sort-of-waffly object obscuring a blue plane. The two
//       objects are close in Z, so Z-buffering artifacts may occur. Switching
//       to w-buffering shows that many of these artifacts go away, since
//       w-buffering is generally a superior form of depth buffering.
//
//       Note: This code uses the D3D Framework helper library.
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <stdio.h>
#include <math.h>
#include <time.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"
#include "resource.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define NUM_WAFFLE_SLICES 20




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    HMENU          m_hMenu;

    D3DLVERTEX     m_pvBluePlane[4];
    D3DLVERTEX     m_pvGreenWaffle[NUM_WAFFLE_SLICES*2];
    D3DZBUFFERTYPE m_dwDepthBufferType;

protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );
    HRESULT FinalCleanup();

public:
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
    m_strWindowTitle  = TEXT( "WBuffer: Direct3D W-buffering Demo" );
    m_bAppUseZBuffer  = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = NULL;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Set up the vertices for the blue plane
    D3DVECTOR v1 = D3DVECTOR(-0.8f,-0.8f, 1.0f );
    D3DVECTOR v2 = D3DVECTOR(-0.8f, 0.8f, 1.0f );
    D3DVECTOR v3 = D3DVECTOR( 0.8f, 0.8f, 1.0f );
    D3DVECTOR v4 = D3DVECTOR( 0.8f,-0.8f, 1.0f );
    m_pvBluePlane[0] = D3DLVERTEX( v1, 0xff0000ff, 0x0, 0.0f, 1.0f );
    m_pvBluePlane[1] = D3DLVERTEX( v2, 0xff0000ff, 0x0, 0.0f, 0.0f );
    m_pvBluePlane[2] = D3DLVERTEX( v3, 0xff0000ff, 0x0, 1.0f, 1.0f );
    m_pvBluePlane[3] = D3DLVERTEX( v4, 0xff0000ff, 0x0, 1.0f, 0.0f );

    // Set up the vertices for the waffle object
    for( DWORD i=0; i<NUM_WAFFLE_SLICES; i++ )
    {
        FLOAT u = ((FLOAT)(i))/(NUM_WAFFLE_SLICES-1);
        FLOAT x = 2*u - 1.0f;
        FLOAT z = 0.0f;
        if( i%4 == 0 ) z = -0.5f;
        if( i%4 == 2 ) z = +0.5f;

        D3DVECTOR v1 = D3DVECTOR( x, -1.0f, z );
        D3DVECTOR v2 = D3DVECTOR( x, +1.0f, z );

        m_pvGreenWaffle[2*i+0] = D3DLVERTEX( v1, 0xff00ff00, 0x0, u, 1.0f );
        m_pvGreenWaffle[2*i+1] = D3DLVERTEX( v2, 0xff00ff00, 0x0, u, 0.0f );
    }

    // Create a texture
    D3DTextr_CreateTextureFromFile( "wbuffer.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Rotate the object about the z-axis
    D3DMATRIX matWorld;
    D3DUtil_SetRotateZMatrix( matWorld, (g_PI/2) * ( 1.0f + sinf(fTimeKey) ) );

    // Move the object back in forth along the z-axis
    matWorld._43 = 50.0f + 40.0f * sinf(fTimeKey);

    // Put the new world matrix into effect
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );

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
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER, 0L, 1.0f, 0L );

    // Set the depth-buffering states
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZWRITEENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZFUNC,   D3DCMP_LESSEQUAL );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, m_dwDepthBufferType );

    // Begin the scene 
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface( "wbuffer.bmp" ) );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_LVERTEX,
                                     m_pvGreenWaffle, NUM_WAFFLE_SLICES*2, NULL );

        m_pd3dDevice->SetTexture( 0, NULL );
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLEFAN, D3DFVF_LVERTEX,
                                     m_pvBluePlane, 4, NULL );
        
        // End the scene.
        m_pd3dDevice->EndScene();
    }

    // Output text to the backbuffer
    if( D3DZB_FALSE == m_dwDepthBufferType )
        OutputText( 5, 15, "Not using depth-buffer" );
    if( D3DZB_TRUE == m_dwDepthBufferType )
        OutputText( 5, 15, "Using Z-buffer" );
    if( D3DZB_USEW == m_dwDepthBufferType )
        OutputText( 5, 15, "Using W-buffer" );

    CHAR strBuffer[80];
    D3DMATRIX matWorld;
    m_pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );
    sprintf( strBuffer, "Z-coordinate = %.2f", matWorld._43 );
    OutputText( 5, 30, strBuffer );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    // Set menu items
    m_hMenu = GetMenu( m_hWnd );
    EnableMenuItem( m_hMenu, IDM_USEWBUFFER,    MF_ENABLED );
    EnableMenuItem( m_hMenu, IDM_USEZBUFFER,    MF_ENABLED );
    EnableMenuItem( m_hMenu, IDM_NODEPTHBUFFER, MF_ENABLED );
    CheckMenuItem(  m_hMenu, IDM_USEWBUFFER,    MF_UNCHECKED );
    CheckMenuItem(  m_hMenu, IDM_USEZBUFFER,    MF_CHECKED );
    CheckMenuItem(  m_hMenu, IDM_NODEPTHBUFFER, MF_UNCHECKED );

    m_dwDepthBufferType = D3DZB_TRUE;

    // Get triangle caps and check for w-buffering
    D3DPRIMCAPS* pdpc = &m_pDeviceInfo->ddDeviceDesc.dpcTriCaps;
    if( 0L == ( pdpc->dwRasterCaps & D3DPRASTERCAPS_WBUFFER ) )
        EnableMenuItem( m_hMenu, IDM_USEWBUFFER, MF_GRAYED );
    
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE );

    // Set the transform matrices
    D3DMATRIX matWorld, matView, matProj;
    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetIdentityMatrix( matView );
    D3DUtil_SetProjectionMatrix( matProj, 10.0f*g_PI/180.0f, 1.0f, 0.01f, 100.0f );

    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Note: in DX7, setting D3DRENDERSTATE_LIGHTING to FALSE is needed to 
    // turn off vertex lighting (and use the color in the D3DLVERTEX instead.)
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, FALSE );

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




//-----------------------------------------------------------------------------
// Name: MsgProc()
// Desc: Local WndProc() function to handle menu operations.
//-----------------------------------------------------------------------------
LRESULT CMyD3DApplication::MsgProc( HWND hWnd, UINT msg, WPARAM wParam,
                                    LPARAM lParam )
{
    if( WM_COMMAND == msg )
    {
        switch( LOWORD(wParam) )
        {
            case IDM_USEWBUFFER:
                m_dwDepthBufferType = D3DZB_USEW;
                CheckMenuItem( m_hMenu, IDM_USEWBUFFER,    MF_CHECKED   );
                CheckMenuItem( m_hMenu, IDM_USEZBUFFER,    MF_UNCHECKED );
                CheckMenuItem( m_hMenu, IDM_NODEPTHBUFFER, MF_UNCHECKED );
                break;

            case IDM_USEZBUFFER:
                m_dwDepthBufferType = D3DZB_TRUE;
                CheckMenuItem( m_hMenu, IDM_USEWBUFFER,    MF_UNCHECKED );
                CheckMenuItem( m_hMenu, IDM_USEZBUFFER,    MF_CHECKED   );
                CheckMenuItem( m_hMenu, IDM_NODEPTHBUFFER, MF_UNCHECKED );
                break;

            case IDM_NODEPTHBUFFER:
                m_dwDepthBufferType = D3DZB_FALSE;
                CheckMenuItem( m_hMenu, IDM_USEWBUFFER,    MF_UNCHECKED );
                CheckMenuItem( m_hMenu, IDM_USEZBUFFER,    MF_UNCHECKED );
                CheckMenuItem( m_hMenu, IDM_NODEPTHBUFFER, MF_CHECKED   );
                break;
        }
    }

    return CD3DApplication::MsgProc( hWnd, msg, wParam, lParam );
}




