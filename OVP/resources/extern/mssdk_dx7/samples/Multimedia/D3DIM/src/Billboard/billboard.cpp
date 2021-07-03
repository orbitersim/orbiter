//-----------------------------------------------------------------------------
// File: Billboard.cpp
//
// Desc: Example code showing how to do billboarding. The sample uses
//       billboarding to draw some trees.
//       
//       Note: this implementation is for billboards that are fixed to rotate
//       about the Y-axis, which is good for things like trees. For
//       unconstrained billboards, like explosions in a flight sim, the
//       technique is the same, but the the billboards are positioned slightly
//       different. Try using the inverse of the view matrix, TL-vertices, or
//       some other technique.
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1995-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <stdio.h>
#include <math.h>
#include "D3DApp.h"
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define NUM_DEBRIS  512     // Tree, shadow, and point data
#define NUM_TREES   128
#define MAX_WIDTH    50.0f

inline FLOAT RandomPos()   { return (MAX_WIDTH*(FLOAT)(rand()-rand()))/RAND_MAX; }
inline FLOAT RandomColor() { return 0.1f+0.5f*rand()/RAND_MAX; }




//-----------------------------------------------------------------------------
// Name: class CMyD3DApplication
// Desc: Application class. The base class provides just about all the
//       functionality we want, so we're just supplying stubs to interface with
//       the non-C++ functions of the app.
//-----------------------------------------------------------------------------
class CMyD3DApplication : public CD3DApplication
{
    BOOL        m_bUseAlphaTest;
    D3DTLVERTEX m_BackgroundMesh[4];
    D3DLVERTEX  m_Debris[NUM_DEBRIS];
    D3DLVERTEX  m_TreeMesh[4];
    D3DVECTOR   m_TreePositions[NUM_TREES];
    FLOAT       m_fViewAngle; // Angle off Y-axis, used to rotate billboards

    static HRESULT ConfirmDevice( DDCAPS*, D3DDEVICEDESC7* );
    HRESULT DrawBackground();
    HRESULT DrawDebris();
    HRESULT DrawTreeShadows();
    HRESULT DrawTrees();

protected:
    HRESULT OneTimeSceneInit();
    HRESULT InitDeviceObjects();
    HRESULT DeleteDeviceObjects();
    HRESULT Render();
    HRESULT FrameMove( FLOAT fTimeKey );

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
    m_strWindowTitle  = TEXT( "Billboard: D3D Billboarding Example" );
    m_bAppUseZBuffer  = TRUE;
    m_bAppUseStereo   = TRUE;
    m_bShowStats      = TRUE;
    m_fnConfirmDevice = ConfirmDevice;
}




//-----------------------------------------------------------------------------
// Name: OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::OneTimeSceneInit()
{
    // Initialize the tree and debris data
    for( WORD i=0; i<NUM_TREES; i++ )
        m_TreePositions[i] = D3DVECTOR( RandomPos(), 0.0f, RandomPos() );

    for( i=0; i<NUM_DEBRIS; i++ )
        m_Debris[i] = D3DLVERTEX( D3DVECTOR( RandomPos(), 0.0f, RandomPos() ),
                          D3DRGBA( RandomColor(), RandomColor(), 0.0f, 1.0f ),
                          0, 0.0f, 0.0f );

    // Initialize the tree and background meshes
    m_TreeMesh[0] = D3DLVERTEX( D3DVECTOR(-1, 0, 0), 0xffffffff, 0, 0.0f, 1.0f );
    m_TreeMesh[1] = D3DLVERTEX( D3DVECTOR(-1, 2, 0), 0xffffffff, 0, 0.0f, 0.0f );
    m_TreeMesh[2] = D3DLVERTEX( D3DVECTOR( 1, 0, 0), 0xffffffff, 0, 1.0f, 1.0f );
    m_TreeMesh[3] = D3DLVERTEX( D3DVECTOR( 1, 2, 0), 0xffffffff, 0, 1.0f, 0.0f );

    m_BackgroundMesh[0] = D3DTLVERTEX( D3DVECTOR(0,0,0.99f), 0.5f, -1, 0, 0, 1 );
    m_BackgroundMesh[1] = D3DTLVERTEX( D3DVECTOR(0,0,0.99f), 0.5f, -1, 0, 0, 0 );
    m_BackgroundMesh[2] = D3DTLVERTEX( D3DVECTOR(0,0,0.99f), 0.5f, -1, 0, 1, 1 );
    m_BackgroundMesh[3] = D3DTLVERTEX( D3DVECTOR(0,0,0.99f), 0.5f, -1, 0, 1, 0 );

    // Create some textures
    D3DTextr_CreateTextureFromFile( "Cloud3.bmp" );
    D3DTextr_CreateTextureFromFile( "Shadow1.bmp", 0, D3DTEXTR_TRANSPARENTWHITE );
    D3DTextr_CreateTextureFromFile( "Tree0.bmp",   0, D3DTEXTR_TRANSPARENTBLACK );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::InitDeviceObjects()
{
    // Check the alpha test caps of the device. (The alpha test offers a 
    // performance boost to not render pixels less than some alpha threshold.)
    DWORD dwAlphaCaps = m_pDeviceInfo->ddDeviceDesc.dpcTriCaps.dwAlphaCmpCaps;
    m_bUseAlphaTest = ( dwAlphaCaps & D3DPCMPCAPS_GREATEREQUAL );
        
    // Set up the dimensions for the background image
    D3DVIEWPORT7 vp;
    m_pd3dDevice->GetViewport( &vp );
    m_BackgroundMesh[0].sy = (FLOAT)vp.dwHeight;
    m_BackgroundMesh[2].sy = (FLOAT)vp.dwHeight;
    m_BackgroundMesh[2].sx = (FLOAT)vp.dwWidth;
    m_BackgroundMesh[3].sx = (FLOAT)vp.dwWidth;

    // Set the transform matrices (view and world are updated per frame)
    D3DMATRIX matProj;
    D3DUtil_SetProjectionMatrix( matProj, g_PI/2, 1.0f, 1.0f, 100.0f );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set up the default texture states
    D3DTextr_RestoreAllTextures( m_pd3dDevice );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAOP,   D3DTOP_SELECTARG1 );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    m_pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE, TRUE );

    // Note: in DX7, setting D3DRENDERSTATE_LIGHTING to FALSE is needed to 
    // turn off vertex lighting (and use the color in the D3DLVERTEX instead.)
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, FALSE );
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
// Name: FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::FrameMove( FLOAT fTimeKey )
{
    // Move the camera about a large circle through the trees
    m_fViewAngle = ( (fTimeKey/12)-(FLOAT)floor(fTimeKey/12) ) * 2 * g_PI;

    FLOAT x1 = 20.0f*(FLOAT)cos(m_fViewAngle);
    FLOAT y1 =  3.0f;
    FLOAT z1 = 20.0f*(FLOAT)sin(m_fViewAngle);

    FLOAT x2 = 20.0f*(FLOAT)cos(m_fViewAngle+0.1f);
    FLOAT y2 =  3.0f;
    FLOAT z2 = 20.0f*(FLOAT)sin(m_fViewAngle+0.1f);

    D3DVECTOR up(0.0f, 1.0f, 0.0f);
    D3DVECTOR from( x1, y1, z1 );
    D3DVECTOR to( x2, y2, z2 );
    D3DVECTOR at = from + 10*Normalize(to-from);

    SetViewParams( &from, &at, &up, 0.1f);

    // Scroll the background texture
    FLOAT tu = (fTimeKey/9)-(FLOAT)floor(fTimeKey/9);
    m_BackgroundMesh[0].tu = m_BackgroundMesh[1].tu = tu;
    m_BackgroundMesh[2].tu = m_BackgroundMesh[3].tu = tu - 1.0f;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DrawBackground()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DrawBackground()
{
    // Draw the background
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("Cloud3.bmp") );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,            FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, FALSE );
    m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_TLVERTEX,
                                 m_BackgroundMesh, 4, 0 );
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DrawDebris()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DrawDebris()
{
    D3DMATRIX matIdentity;
    D3DUtil_SetIdentityMatrix( matIdentity );

    // Render the debris
    m_pd3dDevice->SetTexture( 0, NULL );
    m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matIdentity );
    m_pd3dDevice->DrawPrimitive( D3DPT_POINTLIST, D3DFVF_LVERTEX,
                                 m_Debris, NUM_DEBRIS, 0 );
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DrawTreeShadows()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DrawTreeShadows()
{
    // Set state for rendering shadows
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("Shadow1.bmp") );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_SRCCOLOR );

    // Enable alpha testing (avoids drawing pixels with less than a certain
    // alpha.)
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHATESTENABLE, m_bUseAlphaTest );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHAREF,        0x08 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHAFUNC, D3DCMP_GREATEREQUAL );

    // Rotate the world matrix, to lay the shadow on the ground
    D3DMATRIX matWorld;
    D3DUtil_SetRotateXMatrix( matWorld, g_PI/2.0f);

    // Loop through the trees rendering the shadows
    for( WORD i=0; i<NUM_TREES; i++ )
    {
        // Tranlate the world matrix to move the shadow into place
        matWorld._41 = m_TreePositions[i].x;
        matWorld._42 = m_TreePositions[i].y;
        matWorld._43 = m_TreePositions[i].z;
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );

        // Render the shadow's polygons
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_LVERTEX,
                                     m_TreeMesh, 4, 0 );
    }

    // Restore state
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHATESTENABLE,  FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DrawTrees()
// Desc:
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::DrawTrees()
{
    // Set state for drawing trees
    m_pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("Tree0.bmp") );

    // Billboards are in XY plane, so turn off texture perpective correction
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, FALSE );

    // Trees should be using the zbuffer
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, TRUE );
    
    // Set diffuse blending for alpha set in vertices. 
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE,   TRUE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA );

    // Enable alpha testing (avoids drawing pixels with less than a certain
    // alpha.)
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHATESTENABLE, m_bUseAlphaTest );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHAREF,        0x08 );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHAFUNC, D3DCMP_GREATEREQUAL );

    // Set up a rotation matrix to orient the billboard towards the camera.
    // This is for billboards that are fixed about the Y-axis. See note at top
    // of file for more info.
    D3DMATRIX matWorld;
    D3DUtil_SetRotateYMatrix( matWorld, -m_fViewAngle );

    // Loop through and render all trees
    for( DWORD i=0; i<NUM_TREES; i++ )
    {
        // Translate the billboard into place
        matWorld._41 = m_TreePositions[i].x;
        matWorld._42 = m_TreePositions[i].y;
        matWorld._43 = m_TreePositions[i].z;
        m_pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );

        // Render the billboards polygons
        m_pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_LVERTEX,
                                     m_TreeMesh, 4, 0 );
    }

    // Restore state
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHATESTENABLE,  FALSE );
    m_pd3dDevice->SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, FALSE );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_Render()
// Desc: Called once per frame, the call is the entry point for 3d
//       rendering. This function sets up render states, clears the
//       viewport, and renders the scene.
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::Render()
{
    // Clear the viewport
    m_pd3dDevice->Clear( 0, NULL, D3DCLEAR_ZBUFFER, 0L, 1.0f, 0L );

    // Begin the scene
    if( SUCCEEDED( m_pd3dDevice->BeginScene() ) )
    {
        // Draw all items in the scene
        DrawBackground();
        DrawDebris();
        DrawTreeShadows();
        DrawTrees();

        // End the scene.
        m_pd3dDevice->EndScene();
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_ConfirmDevice()
// Desc: Called during device intialization, this code checks the device
//       for some minimum set of capabilities
//-----------------------------------------------------------------------------
HRESULT CMyD3DApplication::ConfirmDevice( DDCAPS* pddDriverCaps,
                           D3DDEVICEDESC7* pd3dDeviceDesc )
{
    // This sample uses alpha textures and/or straight alpha. Make sure the 
    // device supports them
    DWORD dwDeviceCaps = pd3dDeviceDesc->dpcTriCaps.dwTextureCaps;
    if( dwDeviceCaps & D3DPTEXTURECAPS_ALPHAPALETTE )
        return S_OK;
    if( dwDeviceCaps & D3DPTEXTURECAPS_ALPHA )
        return S_OK;

    return E_FAIL;
}




