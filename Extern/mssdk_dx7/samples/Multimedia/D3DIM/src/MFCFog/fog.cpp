//-----------------------------------------------------------------------------
// File: Fog.cpp
//
// Desc: Example code showing how to do fog in D3D
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
// Declare the application globals for use in WinMain.cpp
//-----------------------------------------------------------------------------
TCHAR* g_strAppTitle       = TEXT( "MFCFog: D3D Fog Sample Using MFC" );
BOOL   g_bAppUseZBuffer    = TRUE;
BOOL   g_bAppUseBackBuffer = TRUE;




//-----------------------------------------------------------------------------
// Function prototypes and global (or static) variables
//-----------------------------------------------------------------------------
inline DWORD FtoDW( FLOAT f ) { return *((DWORD*)&f); }

D3DVERTEX* g_pDiskVertices        = NULL;
DWORD      g_dwNumDiskVertices    = 0L;
D3DVERTEX* g_pColumnVertices      = NULL;
DWORD      g_dwNumColumnVertices  = 0L;
D3DVERTEX* g_pTerrainVertices     = NULL;
DWORD      g_dwNumTerrainVertices = 0L;
WORD*      g_pTerrainIndices      = NULL;
DWORD      g_dwNumTerrainIndices  = 0L;

// View frustum parameters
FLOAT g_fNearPlane      =   1.0f;
FLOAT g_fFarPlane       = 150.0f;

// Parameters for rendering the scene
BOOL  g_bHiResTerrain;
FLOAT g_fFogStart;
FLOAT g_fFogEnd;
FLOAT g_fFogDensity;
DWORD g_dwFogColor;
DWORD g_dwFogMode;
BOOL  g_bRangeBasedFog;
BOOL  g_bUsingTableFog;
BOOL  g_bDeviceUsesWFog;




//----------------------------------------------------------------------------
// Name: GenerateTerrainGrid()
// Desc: Generates vertices for the terrain
//----------------------------------------------------------------------------
VOID GenerateTerrainGrid( DWORD dwGridSize, FLOAT fScale )
{
    DWORD i, j, ind;

    g_dwNumTerrainVertices = dwGridSize * dwGridSize;
    g_dwNumTerrainIndices  = (dwGridSize-1) * (dwGridSize-1) * 6;
    g_pTerrainVertices     = new D3DVERTEX[ g_dwNumTerrainVertices ];
    g_pTerrainIndices      = new WORD[ g_dwNumTerrainIndices ];

    for( i=0; i<dwGridSize; i++ )
    {
        for( j=0; j<dwGridSize; j++ )
        {
            FLOAT u  = i / (FLOAT)(dwGridSize-1);
            FLOAT v  = j / (FLOAT)(dwGridSize-1);
            FLOAT x  = fScale * ( 2*u - 1 );
            FLOAT z  = fScale * ( 2*v - 1 );

            g_pTerrainVertices[i*dwGridSize+j] =
                         D3DVERTEX( D3DVECTOR(x,0,z), D3DVECTOR(0,1,0), u, v );
        }
    }

    for( i=ind=0; i<dwGridSize-1; i++ )
    {
        for( j=0; j<dwGridSize-1; j++ )
        {
            g_pTerrainIndices[ind++] = (WORD)( (i+0)*dwGridSize + (j+0) );
            g_pTerrainIndices[ind++] = (WORD)( (i+0)*dwGridSize + (j+1) );
            g_pTerrainIndices[ind++] = (WORD)( (i+1)*dwGridSize + (j+0) );
            g_pTerrainIndices[ind++] = (WORD)( (i+1)*dwGridSize + (j+0) );
            g_pTerrainIndices[ind++] = (WORD)( (i+0)*dwGridSize + (j+1) );
            g_pTerrainIndices[ind++] = (WORD)( (i+1)*dwGridSize + (j+1) );
        }
    }
}




//----------------------------------------------------------------------------
// Name: GenerateTerrainDisk()
// Desc: Generates a trianglestrip for a disk
//----------------------------------------------------------------------------
VOID GenerateTerrainDisk( DWORD dwNumSegments, FLOAT fScale,
                          D3DVERTEX** ppVertices, DWORD* pdwNumVertices )
{
    // Allocate space for the sphere
    D3DVERTEX* v    = new D3DVERTEX[2 * dwNumSegments * (dwNumSegments)];
    *pdwNumVertices = 2 * dwNumSegments * (dwNumSegments);
    *ppVertices     = v;

    // Generate a spiralized trianglestrip
    for( DWORD ring = 0; ring < dwNumSegments; ring++ )
    {
        for( DWORD seg=0; seg < dwNumSegments; seg++ )
                {
                        FLOAT fTheta = (seg*2*g_PI) / dwNumSegments;
            FLOAT r0     = (ring + fTheta/(2*g_PI))*fScale/dwNumSegments;
            FLOAT r1     = r0 + fScale/dwNumSegments;

            FLOAT x   = (FLOAT)sin( fTheta );
            FLOAT z   = (FLOAT)cos( fTheta );

            FLOAT y0  =  (FLOAT)sin(r0*z*z+r0*x*x);
            FLOAT nx0 = -(FLOAT)cos(r0*z*z+r0*x*x)*r0*2*x;
            FLOAT ny0 = 1.0f;
            FLOAT nz0 = -(FLOAT)cos(r0*z*z+r0*x*x)*r0*2*z;

            FLOAT y1  =  (FLOAT)sin(r1*z*z+r1*x*x);
            FLOAT nx1 = -(FLOAT)cos(r1*z*z+r1*x*x)*r1*2*x;
            FLOAT ny1 = 1.0f;
            FLOAT nz1 = -(FLOAT)cos(r1*z*z+r1*x*x)*r1*2*z;

            // Add two vertices to the strip at each step
            *v++ = D3DVERTEX( D3DVECTOR(r0*x,y0,r0*z),
                              Normalize(D3DVECTOR(nx0,ny0,nz0)),
                              (r0*x)/fScale,(r0*z)/fScale );

            *v++ = D3DVERTEX( D3DVECTOR(r1*x,y1,r1*z),
                              Normalize(D3DVECTOR(nx1,ny1,nz1)),
                              (r1*x)/fScale,(r1*z)/fScale );
        }
    }
}




//----------------------------------------------------------------------------
// Name: GenerateColumn()
// Desc: Generates a trianglestrip for a column
//----------------------------------------------------------------------------
VOID GenerateColumn( DWORD dwNumSegments, FLOAT fRadius, FLOAT fHeight,
                     D3DVERTEX** ppVertices, DWORD* pdwNumVertices )
{
    // Allocate space for the sphere
    D3DVERTEX* v    = new D3DVERTEX[2*(dwNumSegments+1)];
    *pdwNumVertices = 2 * (dwNumSegments+1);
    *ppVertices     = v;

    // Generate a trianglestrip
    for( DWORD seg=0; seg<=dwNumSegments; seg++ )
    {
        FLOAT fTheta = (2*g_PI*seg)/dwNumSegments;
        FLOAT nx     = (FLOAT)sin(fTheta);
        FLOAT nz     = (FLOAT)cos(fTheta);
        FLOAT r      = fRadius;
        FLOAT u      = (1.0f*seg)/dwNumSegments;

        // Add two vertices to the strip at each step
        *v++ = D3DVERTEX( D3DVECTOR( r*nx, fHeight, r*nz ),
                          D3DVECTOR( nx, 0.0f, nz ), u, 1.0f );
        *v++ = D3DVERTEX( D3DVECTOR( r*nx, -1.0f, r*nz ),
                          D3DVECTOR( nx, 0.0f, nz ), u, 0.0f );
    }
}




//-----------------------------------------------------------------------------
// Name: App_OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT App_OneTimeSceneInit()
{
    // Generate some geometry for the app
    GenerateTerrainGrid( 4, 50.0f );
    GenerateTerrainDisk( 30, 50.0f, &g_pDiskVertices, &g_dwNumDiskVertices );
    GenerateColumn( 30, 1.0f, 10.0f, &g_pColumnVertices, &g_dwNumColumnVertices );

    // Create some textures
    D3DTextr_CreateTextureFromFile( "SeaFloor.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT App_FrameMove( LPDIRECT3DDEVICE7 pd3dDevice, FLOAT fTimeKey )
{
    // Move the camera along an ellipse
    D3DVECTOR from = D3DVECTOR( 20*(FLOAT)sin(fTimeKey/2), 5.0f,
                                40*(FLOAT)cos(fTimeKey/2) );
    D3DVECTOR at   = D3DVECTOR( 20*(FLOAT)sin(fTimeKey/2+1.57), 4.0f,
                                40*(FLOAT)cos(fTimeKey/2+1.57) );
    D3DVECTOR up   = D3DVECTOR( 0.0f, 1.0f, 0.0f );

    D3DMATRIX matView;
    D3DUtil_SetViewMatrix( matView, from, at, up );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );
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
    // Clear the viewport
    pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
                       g_dwFogColor, 1.0f, 0L );

    pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGENABLE, TRUE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGCOLOR,  g_dwFogColor );

    pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGTABLESTART,   FtoDW(g_fFogStart) );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGTABLEEND,     FtoDW(g_fFogEnd) );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGTABLEDENSITY, FtoDW(g_fFogDensity) );

    if( g_bUsingTableFog )
    {
            pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGVERTEXMODE,  D3DFOG_NONE );
            pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGTABLEMODE,   g_dwFogMode );
    }
    else
    {
            pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGTABLEMODE,   D3DFOG_NONE );
            pd3dDevice->SetRenderState( D3DRENDERSTATE_FOGVERTEXMODE,  g_dwFogMode );
        pd3dDevice->SetRenderState( D3DRENDERSTATE_RANGEFOGENABLE, g_bRangeBasedFog );
    }

    // Begin the scene
    if( SUCCEEDED( pd3dDevice->BeginScene() ) )
    {
        // Reset the world matrix
        D3DMATRIX matWorld;
        D3DUtil_SetIdentityMatrix( matWorld );
        pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );

        // Draw the terrain
        if( g_bHiResTerrain )
            pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                       g_pDiskVertices, g_dwNumDiskVertices, 0 );
        else
            pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                                              g_pTerrainVertices, g_dwNumTerrainVertices,
                                              g_pTerrainIndices, g_dwNumTerrainIndices, 0 );

        // Draw the columns
        for( DWORD i=0; i<20; i++ )
        {
            FLOAT tx = (i%10)*10.0f - 50.0f;
            FLOAT ty =  0.0f;
            FLOAT tz = (i<=10) ? 20.0f : -20.0f;

            D3DUtil_SetTranslateMatrix( matWorld, tx, ty, tz );
            pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );

            pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                                       g_pColumnVertices, g_dwNumColumnVertices, 0 );
        }

        // End the scene.
        pd3dDevice->EndScene();
    }

    return S_OK;
}



//-----------------------------------------------------------------------------
// Name: App_InitDeviceObjects()
// Desc: Initialize scene objects.
//-----------------------------------------------------------------------------
HRESULT App_InitDeviceObjects( HWND hWnd, LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Check parameters
    if( NULL==pd3dDevice )
        return E_INVALIDARG;

    // Get the device caps
    D3DDEVICEDESC7 ddDesc;
    DWORD         dwRasterCaps;
    if( FAILED( pd3dDevice->GetCaps( &ddDesc ) ) )
        return E_FAIL;
    dwRasterCaps = ddDesc.dpcTriCaps.dwRasterCaps;

    if( dwRasterCaps & D3DPRASTERCAPS_WFOG )
        g_bDeviceUsesWFog = TRUE;
    else
        g_bDeviceUsesWFog = FALSE;

    // Set up the object material
    D3DMATERIAL7 mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    pd3dDevice->SetMaterial( &mtrl );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,  0x00000000 );

    // Set up a texture
    D3DTextr_RestoreAllTextures( pd3dDevice );
    pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("SeaFloor.bmp") );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    // Set the transform matrices
    D3DMATRIX matWorld, matProj;
    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetProjectionMatrix( matProj, g_PI/2.0f, 1.0f, g_fNearPlane, g_fFarPlane );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set up the light
    if( ddDesc.dwVertexProcessingCaps & D3DVTXPCAPS_POSITIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_POINT, 0.0f, 50.0f, 0.0f );
        light.dvAttenuation0 =  0.1f;
        light.dvRange        = 200.0f;
        pd3dDevice->SetLight( 0, &light );
        pd3dDevice->LightEnable( 0, TRUE );
        pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING,    TRUE );
    }

    pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,            TRUE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,       TRUE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE,     FALSE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_TEXTUREPERSPECTIVE, TRUE );

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
    SAFE_DELETE( g_pDiskVertices );
    SAFE_DELETE( g_pColumnVertices );
    SAFE_DELETE( g_pTerrainVertices );
    SAFE_DELETE( g_pTerrainIndices );

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
    if( pd3dDeviceDesc->dpcTriCaps.dwRasterCaps & D3DPRASTERCAPS_FOGVERTEX )
        return S_OK;

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: App_SetFogParameters()
// Desc: Sets the apps parameters for rendering the scene
//-----------------------------------------------------------------------------
VOID App_SetFogParameters( BOOL bHiResTerrain, DWORD dwMode, DWORD dwColor,
                           BOOL bRangeBased, BOOL bTableFog,
                           FLOAT fStart, FLOAT fEnd, FLOAT fDensity )
{
    g_bHiResTerrain  = bHiResTerrain;
    g_dwFogMode      = dwMode;
    g_bRangeBasedFog = bRangeBased;
    g_bUsingTableFog = bTableFog;
    g_dwFogColor     = dwColor;
    g_fFogStart      = ( fStart*(g_fFarPlane-g_fNearPlane) ) + g_fNearPlane;
    g_fFogEnd        = ( fEnd*(g_fFarPlane-g_fNearPlane) ) + g_fNearPlane;
    g_fFogDensity    = fDensity;

    // Set fog start and end values for table (pixel) fog mode on devices that
    // do not use WFOG. These devices expect fog between 0.0 and 1.0.
    if( (FALSE==g_bDeviceUsesWFog) && (TRUE==g_bUsingTableFog) )
    {
        g_fFogStart = fStart;
        g_fFogEnd   = fEnd;
    }
}



