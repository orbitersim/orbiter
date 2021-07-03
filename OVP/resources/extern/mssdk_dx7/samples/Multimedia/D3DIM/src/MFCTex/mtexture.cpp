//-----------------------------------------------------------------------------
// File: mtexture.cpp
//
// Desc: Example code showing how to do multitexturing in D3D
//
//       Note: This code uses the D3D Framework helper library.
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------
#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include <time.h>
#include <stdio.h>
#include "D3DTextr.h"
#include "D3DUtil.h"
#include "D3DMath.h"




//-----------------------------------------------------------------------------
// Declare the application globals for use in main MFC file
//-----------------------------------------------------------------------------
TCHAR* g_strAppTitle       = TEXT( "MFCTex: MFC Multitexture Sample" );
BOOL   g_bAppUseZBuffer    = FALSE;




//-----------------------------------------------------------------------------
// Define a custom vertex that uses XYZ, a normal, and two sets of tex coords
//-----------------------------------------------------------------------------
struct MyVertex
{
    FLOAT x, y, z;
    DWORD dwColor;
    FLOAT tu, tv;
    FLOAT tu2, tv2;
};

#define MYVERTEX (D3DFVF_XYZ | D3DFVF_DIFFUSE | D3DFVF_TEX2 )

#define FILL_MYVERTEX( v, ax, ay, az, acolor, atu, atv, atu2, atv2 )  \
{   v.x = ax; v.y = ay; v.z = az; v.dwColor  = acolor;\
    v.tu = atu; v.tv = atv; v.tu2 = atu2; v.tv2 = atv2;\
}




//-----------------------------------------------------------------------------
// Storage of the texture stage states
//-----------------------------------------------------------------------------
WORD  g_wT0COp,   g_wT1COp,   g_wT2COp;
WORD  g_wT0CArg1, g_wT1CArg1, g_wT2CArg1;
WORD  g_wT0CArg2, g_wT1CArg2, g_wT2CArg2;
WORD  g_wT0AOp,   g_wT1AOp,   g_wT2AOp;
WORD  g_wT0AArg1, g_wT1AArg1, g_wT2AArg1;
WORD  g_wT0AArg2, g_wT1AArg2, g_wT2AArg2;
DWORD g_dwTextureFactor         = 0xffffffff;
DWORD g_dwDiffuseColor          = 0xffffffff;
DWORD g_dwMaxTextureBlendStages = 1;





//-----------------------------------------------------------------------------
// Function prototypes and global (or static) variables
//-----------------------------------------------------------------------------
// Vertices and indices for drawing a room. (Note the use of MyVertex)
#define WALL_VERT_NUM 10
MyVertex            g_avWallVertices[WALL_VERT_NUM];
const DWORD         g_dwNumWallVertices = 10;
D3DVERTEX           g_avFloorVertices[8];
WORD                g_awFloorIndices[12];
const DWORD         g_dwNumFloorVertices = 8;
const DWORD         g_dwNumFloorIndices  = 12;

// Filenames for the textures (one for each texture stage)
BOOL g_bTexturesChanged = FALSE;
CHAR g_strTexture0[256];
CHAR g_strTexture1[256];
CHAR g_strTexture2[256];




//-----------------------------------------------------------------------------
// Name: App_OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT App_OneTimeSceneInit()
{
    // Declare the 8 vertices which define the room
    D3DVECTOR vLTF(-5, 5, 5), vRTF( 5, 5, 5), vLBF(-5,-5, 5), vRBF( 5,-5, 5);
    D3DVECTOR vLTB(-5, 5,-5), vRTB( 5, 5,-5), vLBB(-5,-5,-5), vRBB( 5,-5,-5);

    FILL_MYVERTEX( g_avWallVertices[ 0], -5.0f,-5.0f, 5.0f, 0xff4040ff, 0.00f, 1.0f, 1.00, 1.0f );
    FILL_MYVERTEX( g_avWallVertices[ 1], -5.0f, 5.0f, 5.0f, 0xff40ff40, 0.00f, 0.0f, 1.00, 0.0f );
    FILL_MYVERTEX( g_avWallVertices[ 2],  5.0f,-5.0f, 5.0f, 0xffff4040, 1.00f, 1.0f, 0.00, 1.0f );
    FILL_MYVERTEX( g_avWallVertices[ 3],  5.0f, 5.0f, 5.0f, 0xffffff40, 1.00f, 0.0f, 0.00, 0.0f );
    FILL_MYVERTEX( g_avWallVertices[ 4],  5.0f,-5.0f,-5.0f, 0xffff40ff, 2.00f, 1.0f, 1.00, 1.0f );
    FILL_MYVERTEX( g_avWallVertices[ 5],  5.0f, 5.0f,-5.0f, 0xff40ffff, 2.00f, 0.0f, 1.00, 0.0f );
    FILL_MYVERTEX( g_avWallVertices[ 6], -5.0f,-5.0f,-5.0f, 0xffffffff, 3.00f, 1.0f, 1.00, 1.0f );
    FILL_MYVERTEX( g_avWallVertices[ 7], -5.0f, 5.0f,-5.0f, 0xff404040, 3.00f, 0.0f, 1.00, 0.0f );
    FILL_MYVERTEX( g_avWallVertices[ 8], -5.0f,-5.0f, 5.0f, 0xff4040ff, 4.00f, 1.0f, 0.00, 1.0f );
    FILL_MYVERTEX( g_avWallVertices[ 9], -5.0f, 5.0f, 5.0f, 0xff40ff40, 4.00f, 0.0f, 0.00, 0.0f );

    // Floor & ceiling
    g_avFloorVertices[0] = D3DVERTEX( vLBF, D3DVECTOR( 0, 1, 0 ), 0.0f, 0.0f );
    g_avFloorVertices[1] = D3DVERTEX( vRBF, D3DVECTOR( 0, 1, 0 ), 0.0f, 1.0f );
    g_avFloorVertices[2] = D3DVERTEX( vRBB, D3DVECTOR( 0, 1, 0 ), 1.0f, 1.0f );
    g_avFloorVertices[3] = D3DVERTEX( vLBB, D3DVECTOR( 0, 1, 0 ), 1.0f, 0.0f );
    g_avFloorVertices[4] = D3DVERTEX( vLTF, D3DVECTOR( 0,-1, 0 ), 0.0f, 0.0f );
    g_avFloorVertices[5] = D3DVERTEX( vRTF, D3DVECTOR( 0,-1, 0 ), 0.0f, 1.0f );
    g_avFloorVertices[6] = D3DVERTEX( vRTB, D3DVECTOR( 0,-1, 0 ), 1.0f, 1.0f );
    g_avFloorVertices[7] = D3DVERTEX( vLTB, D3DVECTOR( 0,-1, 0 ), 1.0f, 0.0f );

    // Indices for the floor and cieling
    g_awFloorIndices[ 0] = 0; g_awFloorIndices[ 1] = 1; g_awFloorIndices[ 2] = 2;
    g_awFloorIndices[ 3] = 2; g_awFloorIndices[ 4] = 3; g_awFloorIndices[ 5] = 0;
    g_awFloorIndices[ 6] = 6; g_awFloorIndices[ 7] = 5; g_awFloorIndices[ 8] = 4;
    g_awFloorIndices[ 9] = 4; g_awFloorIndices[10] = 7; g_awFloorIndices[11] = 6;

    // Create some textures
    strcpy( g_strTexture0, "env2.bmp" );
    strcpy( g_strTexture1, "spotlite.bmp" );
    strcpy( g_strTexture2, "env3.bmp" );
    D3DTextr_CreateTextureFromFile( "floor.bmp", 0 );
    D3DTextr_CreateTextureFromFile( g_strTexture0, 0 );
    D3DTextr_CreateTextureFromFile( g_strTexture1, 1 );
    D3DTextr_CreateTextureFromFile( g_strTexture2, 2 );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT App_FrameMove( LPDIRECT3DDEVICE7 pd3dDevice, FLOAT fTimeKey )
{
    // Setup the world spin matrix
    D3DMATRIX matWorldSpin;
    D3DUtil_SetRotateYMatrix( matWorldSpin, -fTimeKey/9 );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSpin );

    // hack to keep texture coordinates in a better range
    static FLOAT fTimeKeySub = 0.0f;
    FLOAT fTexTimeKey = fTimeKey + fTimeKeySub;
    if( fTexTimeKey > 160.0f )
    {
        fTimeKeySub -= 160.0f;
    }

    // Rotate the light map around the walls each frame
    for( int i=0; i<(WALL_VERT_NUM/2); i++ )
    {
        g_avWallVertices[2*i+0].tu2 = fTexTimeKey/(WALL_VERT_NUM/2)+i;
        g_avWallVertices[2*i+1].tu2 = fTexTimeKey/(WALL_VERT_NUM/2)+i;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetTextureStageStatesForRendering()
// Desc: Sets up the texture stage, as per the global variables defining each
//       stage.
//-----------------------------------------------------------------------------
HRESULT SetTextureStageStatesForRendering( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // If new textures were selected, restore them now
    if( g_bTexturesChanged )
        D3DTextr_RestoreAllTextures( pd3dDevice );
    g_bTexturesChanged = FALSE;

    pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREFACTOR, g_dwTextureFactor );

    pd3dDevice->SetTexture( 0, D3DTextr_GetSurface( g_strTexture0 ) );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_TEXCOORDINDEX, 0 );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, g_wT0CArg1 );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   g_wT0COp   );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, g_wT0CArg2 );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAARG1, g_wT0AArg1 );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAOP,   g_wT0AOp   );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAARG2, g_wT0AArg2 );

    if( g_dwMaxTextureBlendStages > 1 )
    {
        pd3dDevice->SetTexture( 1, D3DTextr_GetSurface( g_strTexture1 ) );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_TEXCOORDINDEX, 1 );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG1, g_wT1CArg1 );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP,   g_wT1COp   );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_COLORARG2, g_wT1CArg2 );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_ALPHAARG1, g_wT1AArg1 );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_ALPHAOP,   g_wT1AOp   );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_ALPHAARG2, g_wT1AArg2 );
    }

    if( g_dwMaxTextureBlendStages > 2)
    {
        pd3dDevice->SetTexture( 2, D3DTextr_GetSurface( g_strTexture2 ) );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_TEXCOORDINDEX, 0 );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_MAGFILTER, D3DTFG_LINEAR );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_COLORARG1, g_wT2CArg1 );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_COLOROP,   g_wT2COp   );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_COLORARG2, g_wT2CArg2 );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_ALPHAARG1, g_wT2AArg1 );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_ALPHAOP,   g_wT2AOp   );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_ALPHAARG2, g_wT2AArg2 );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_Render()
// Desc: Called once per frame, the call is the entry point for 3d
//       rendering. This function sets up render states, clears the
//       viewport, and renders the scene.
//-----------------------------------------------------------------------------
HRESULT App_Render( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Begin the scene
    if( FAILED( pd3dDevice->BeginScene() ) )
        return E_FAIL;

    // Set the diffuse color of the walls
    for( DWORD i=0; i<WALL_VERT_NUM; i++ )
        g_avWallVertices[i].dwColor = g_dwDiffuseColor;

    // Turn off multi-texturing by disabling higher states
    if( g_dwMaxTextureBlendStages > 2 )
    {
        pd3dDevice->SetTextureStageState( 2, D3DTSS_COLOROP, D3DTOP_DISABLE );
        pd3dDevice->SetTextureStageState( 2, D3DTSS_ALPHAOP, D3DTOP_DISABLE );
    }
    if( g_dwMaxTextureBlendStages > 1 )
    {
        pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP, D3DTOP_DISABLE );
        pd3dDevice->SetTextureStageState( 1, D3DTSS_ALPHAOP, D3DTOP_DISABLE );
    }

    // Render the floor and ceiling (using single-textured vertices)
    pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("floor.bmp") );
    pd3dDevice->SetTexture( 1, NULL );
    pd3dDevice->SetTexture( 2, NULL );

    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_SELECTARG1 );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_ALPHAOP,   D3DTOP_DISABLE );
    pd3dDevice->SetTextureStageState( 1, D3DTSS_COLOROP,   D3DTOP_DISABLE );
    pd3dDevice->SetTextureStageState( 1, D3DTSS_ALPHAOP,   D3DTOP_DISABLE );
    pd3dDevice->SetTextureStageState( 2, D3DTSS_COLOROP,   D3DTOP_DISABLE );
    pd3dDevice->SetTextureStageState( 2, D3DTSS_ALPHAOP,   D3DTOP_DISABLE );

    pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX, 
                                      g_avFloorVertices, g_dwNumFloorVertices,
                                      g_awFloorIndices, g_dwNumFloorIndices, NULL );

    // Setup the texture stages' state
    SetTextureStageStatesForRendering( pd3dDevice );

    // Render the walls of the room (using multi-texture vertices)
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, MYVERTEX,
                               g_avWallVertices, g_dwNumWallVertices, NULL );

    // End the scene.
    pd3dDevice->EndScene();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT App_InitDeviceObjects( HWND hWnd, LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Create and set up the shine materials w/ textures
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    pd3dDevice->SetMaterial( &mtrl );

    // Set the transform matrices
    D3DMATRIX matWorld, matView, matProj;
    D3DVECTOR vEyePt    = D3DVECTOR( 0, 0, -2.5 );
    D3DVECTOR vLookatPt = D3DVECTOR( 0, 0,   0  );
    D3DVECTOR vUpVec    = D3DVECTOR( 0, 1,   0  );

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/2, 1.0f, 1.0f, 100.0f );

    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set any appropiate state
    pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,  FALSE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,  0xffffffff );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, FALSE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE );

    D3DTextr_RestoreAllTextures( pd3dDevice );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_DeleteDeviceObjects()
// Desc: Called when the app is exitting, or the device is being changed,
//       this function deletes any device dependant objects.
//-----------------------------------------------------------------------------
VOID App_DeleteDeviceObjects( HWND hWnd, LPDIRECT3DDEVICE7 pd3dDevice )
{
    D3DTextr_InvalidateAllTextures();
}




//-----------------------------------------------------------------------------
// Name: App_FinalCleanup()
// Desc: Called before the app exits, this function gives the app the chance
//       to cleanup after itself.
//-----------------------------------------------------------------------------
HRESULT App_FinalCleanup()
{
    return S_OK;
}




//----------------------------------------------------------------------------
// Name: App_RestoreSurfaces
// Desc: Restores any previously lost surfaces. Must do this for all surfaces
//       (including textures) that the app created.
//----------------------------------------------------------------------------
HRESULT App_RestoreSurfaces()
{
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_ConfirmDevice()
// Desc: Called during device intialization, this code checks the device
//       for some minimum set of capabilities
//-----------------------------------------------------------------------------
HRESULT App_ConfirmDevice( DDCAPS* pddDriverCaps,
                           D3DDEVICEDESC7* pd3dDeviceDesc )
{
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: TextureInUse()
// Desc: Determines whether a given texture is currently being used
//-----------------------------------------------------------------------------
BOOL TextureInUse( CHAR* strTexture )
{
    if( stricmp( strTexture, g_strTexture0 ) )
        if( stricmp( strTexture, g_strTexture1 ) )
            if( stricmp( strTexture, g_strTexture2 ) )
                if( stricmp( strTexture, "floor.bmp" ) )
                    return FALSE;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: App_SetTextureMaps()
// Desc: Changes the texture maps used during rendering of the room.
//-----------------------------------------------------------------------------
VOID App_SetTextureMaps( const CHAR* strTexture0, const CHAR* strTexture1,
                         const CHAR* strTexture2 )
{
    if( FALSE == TextureInUse( g_strTexture0 ) )
        D3DTextr_DestroyTexture( g_strTexture0 );
    if( FALSE == TextureInUse( g_strTexture1 ) )
        D3DTextr_DestroyTexture( g_strTexture1 );
    if( FALSE == TextureInUse( g_strTexture2 ) )
        D3DTextr_DestroyTexture( g_strTexture2 );

    if( stricmp( strTexture0, g_strTexture0 ) )
    {
        strcpy( g_strTexture0, strTexture0 );
        D3DTextr_CreateTextureFromFile( g_strTexture0, 0 );
    }

    if( stricmp( strTexture1, g_strTexture1 ) )
    {
        strcpy( g_strTexture1, strTexture1 );
        D3DTextr_CreateTextureFromFile( g_strTexture1, 1 );
    }

    if( stricmp( strTexture2, g_strTexture2 ) )
    {
        strcpy( g_strTexture2, strTexture2 );
        D3DTextr_CreateTextureFromFile( g_strTexture2, 2 );
    }

    g_bTexturesChanged = TRUE;
}




