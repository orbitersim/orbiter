//-----------------------------------------------------------------------------
// File: Bend.cpp
//
// Desc: Example code showing how to do a skinning effect. The code
//       does a linear interplation on two meshes of an ellipsoid,
//       resulting in a visual "bending" effect, with smooth skin.
//
//       Note: This code uses the D3D Framework helper library.
//
//
// Copyright (c) 1997-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include "D3DTextr.h"
#include "D3DUtil.h"


//-----------------------------------------------------------------------------
// Declare the application globals for use in WinMain.cpp
//-----------------------------------------------------------------------------
TCHAR* g_strAppTitle       = TEXT( "Bend: Surface Skinning Example" );
BOOL   g_bAppUseZBuffer    = TRUE;    // Create/use a z-buffering




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define LERP(m,x0,x1)       ((x0) + (m)*((x1)-(x0)))
#define PI                  3.14159265358979323846f
#define ELLIPSE_RADIUS      1.5f
#define ELLIPSE_NUMRINGS    20
#define ELLIPSE_NUMSECTIONS 20
#define ELLIPSE_X_LENGTH    1.0f
#define ELLIPSE_Y_LENGTH    3.0f
#define ELLIPSE_Z_LENGTH    1.0f

D3DVERTEX* g_pvModelVertices1 = NULL;            //object's vertices
D3DVERTEX* g_pvModelVertices2 = NULL;            //object's vertices
D3DVERTEX* g_pvRenderVertices = NULL;            //object's vertices
WORD*      g_pwRenderIndices  = NULL;            //object's indices
DWORD      g_dwNumVertices;
DWORD      g_dwNumIndices;




//-----------------------------------------------------------------------------
// Function prototypes and global (or static) variables
//-----------------------------------------------------------------------------
VOID    AppPause( BOOL );
VOID    RotateVertexInX( FLOAT, DWORD, D3DVERTEX*, D3DVERTEX* );
BOOL    GenerateSphere( FLOAT, DWORD, DWORD, FLOAT, FLOAT, FLOAT, D3DVERTEX**,
                        DWORD*, WORD**, DWORD* );
VOID    BlendObjects( DWORD, D3DVERTEX*, D3DVERTEX*, D3DVERTEX* );




//-----------------------------------------------------------------------------
// Name: App_OneTimeSceneInit()
// Desc: Called during initial app startup, this function performs all the
//       permanent initialization.
//-----------------------------------------------------------------------------
HRESULT App_OneTimeSceneInit()
{
    // Generate the object data
    GenerateSphere( ELLIPSE_RADIUS, ELLIPSE_NUMRINGS, ELLIPSE_NUMSECTIONS, 
                    ELLIPSE_X_LENGTH, ELLIPSE_Y_LENGTH, ELLIPSE_Z_LENGTH, 
                    &g_pvRenderVertices, &g_dwNumVertices, 
                    &g_pwRenderIndices, &g_dwNumIndices );
    RotateVertexInX( (FLOAT)(PI/2), g_dwNumVertices, g_pvRenderVertices,
                     g_pvRenderVertices );

    // Make two copies of the object (for modification of the vertices)
    g_pvModelVertices1 = new D3DVERTEX[g_dwNumVertices];
    g_pvModelVertices2 = new D3DVERTEX[g_dwNumVertices];

    for( DWORD i=0; i<g_dwNumVertices; i++ )
    {
        g_pvModelVertices1[i] = g_pvRenderVertices[i];
        g_pvModelVertices2[i] = g_pvRenderVertices[i];
    }


    // Create textures
    D3DTextr_CreateTextureFromFile( "Banana.bmp" );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_FrameMove()
// Desc: Called once per frame, the call is the entry point for animating
//       the scene.
//-----------------------------------------------------------------------------
HRESULT App_FrameMove( LPDIRECT3DDEVICE7 pd3dDevice, FLOAT fTimeKey )
{
    // Compute the bend and rotate angles for this frame
    FLOAT fRotateAngle = (FLOAT)( fTimeKey / 3 );
    FLOAT fBendAngle   = (FLOAT)( (sin(fTimeKey)+1.0f)*0.6f );

    // Setup the world spin matrix
    D3DMATRIX matWorldSpin;
    D3DUtil_SetRotateYMatrix( matWorldSpin, fRotateAngle );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorldSpin );

    // Bend two copies of the object in different directions and 
    // merge (blend) them into one set of vertex data.
    RotateVertexInX( fBendAngle, g_dwNumVertices, g_pvModelVertices2,
                     g_pvModelVertices1 );
    BlendObjects( g_dwNumVertices, g_pvModelVertices1, g_pvModelVertices2,
                  g_pvRenderVertices );

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
                       0x000000ff, 1.0f, 0L );

    // Begin the scene 
    if( SUCCEEDED( pd3dDevice->BeginScene() ) )
    {
        //Display the object
        pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                          g_pvRenderVertices, g_dwNumVertices, 
                          g_pwRenderIndices, g_dwNumIndices, NULL );

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
    if( FAILED( pd3dDevice->GetCaps( &ddDesc ) ) )
        return E_FAIL;

    // Setup the material
    D3DMATERIAL7      mtrl;
    D3DUtil_InitMaterial( mtrl, 1.0f, 1.0f, 1.0f );
    mtrl.power = 40.0f;
    pd3dDevice->SetMaterial( &mtrl );

    // Set up the textures
    D3DTextr_RestoreAllTextures( pd3dDevice );
    pd3dDevice->SetTexture( 0, D3DTextr_GetSurface("Banana.bmp") );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG2, D3DTA_DIFFUSE );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_MODULATE );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_MINFILTER, D3DTFN_LINEAR );
    pd3dDevice->SetTextureStageState( 0, D3DTSS_MAGFILTER, D3DTFG_LINEAR );

    // Miscellaneous render states
    pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT,        0x40404040 );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_DITHERENABLE,   TRUE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_SPECULARENABLE, TRUE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE,        TRUE );

    // Set the transform matrices
    D3DVECTOR vEyePt    = D3DVECTOR( 0.0f, 0.0f, -6.5f );
    D3DVECTOR vLookatPt = D3DVECTOR( 0.0f, 0.0f,  0.0f );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f,  0.0f );
    D3DMATRIX matWorld, matView, matProj;

    D3DUtil_SetIdentityMatrix( matWorld );
    D3DUtil_SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    D3DUtil_SetProjectionMatrix( matProj, 1.57f, 1.0f, 1.0f, 100.0f );

    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    // Set up lighting states
    if( ddDesc.dwVertexProcessingCaps & D3DVTXPCAPS_DIRECTIONALLIGHTS )
    {
        D3DLIGHT7 light;
        D3DUtil_InitLight( light, D3DLIGHT_DIRECTIONAL, 0.0f, -1.0f, 0.0f );
        pd3dDevice->SetLight( 0, &light );
        pd3dDevice->LightEnable( 0, TRUE );
        pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );
    }

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
    SAFE_DELETE( g_pvModelVertices1 );
    SAFE_DELETE( g_pvModelVertices2 );
    SAFE_DELETE( g_pvRenderVertices );
    SAFE_DELETE( g_pwRenderIndices );

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
// Name: RotateVertexInX()
// Desc: Rotates an array of vertices by an amount theta about the x-axis.
//-----------------------------------------------------------------------------
VOID RotateVertexInX( FLOAT fTheta, DWORD dwCount,
                      D3DVERTEX* pvInVertices, D3DVERTEX* pvOutVertices )
{
    FLOAT fSin = (FLOAT)sin(fTheta); 
    FLOAT fCos = (FLOAT)cos(fTheta);
    
    for( DWORD i=0; i<dwCount; i++ )
    {
        FLOAT y = pvInVertices[i].y;
        FLOAT z = pvInVertices[i].z;
        pvOutVertices[i].y = fCos*y + fSin*z;
        pvOutVertices[i].z = -fSin*y + fCos*z;

        FLOAT ny = pvInVertices[i].ny;
        FLOAT nz = pvInVertices[i].nz;
        pvOutVertices[i].ny = fCos*ny + fSin*nz;
        pvOutVertices[i].nz = -fSin*ny + fCos*nz;
    }
}



//-----------------------------------------------------------------------------
// Name: GenerateSphere()
// Desc: Makes vertex and index data for a sphere.
//-----------------------------------------------------------------------------
BOOL GenerateSphere( FLOAT fRadius, DWORD dwNumRings, DWORD dwNumSections, 
                     FLOAT sx, FLOAT sy, FLOAT sz,
                     D3DVERTEX** ppvVertices, DWORD* pdwNumVertices,
                     WORD** ppwIndices, DWORD* pdwNumIndices )
{
    FLOAT x, y, z, v, rsintheta; // Temporary variables
    DWORD i, j;            // counters
    DWORD n, m;            // counters

    //Generate space for the required triangles and vertices.
    DWORD      dwNumTriangles = (dwNumRings+1) * dwNumSections * 2;
    DWORD      dwNumVertices  = (dwNumRings+1) * dwNumSections + 2;
    D3DVERTEX* pvVertices     = new D3DVERTEX[dwNumVertices];
    DWORD      dwNumIndices   = dwNumTriangles*3;
    WORD*      pwIndices      = new WORD[dwNumIndices];

    // Generate vertices at the top and bottom points.
    D3DVECTOR vPoint  = D3DVECTOR( 0.0f, sy*fRadius, 0.0f );
    D3DVECTOR vNormal = D3DVECTOR( 0.0f, 0.0f, 1.0f );
    pvVertices[0]               = D3DVERTEX(  vPoint,  vNormal, 0.0f, 0.0f );
    pvVertices[dwNumVertices-1] = D3DVERTEX( -vPoint, -vNormal, 0.0f, 0.0f );

    // Generate vertex points for rings
    FLOAT dtheta = (FLOAT)(PI / (dwNumRings + 2));     //Angle between each ring
    FLOAT dphi   = (FLOAT)(2*PI / dwNumSections); //Angle between each section
    FLOAT theta  = dtheta;
    n = 1; //vertex being generated, begins at 1 to skip top point

    dwNumRings += 1;
    dwNumRings -= 1;

    for( i = 0; i < (dwNumRings+1); i++ )
    {
        y = fRadius * (FLOAT)cos(theta); // y is the same for each ring
        v = theta / PI;     // v is the same for each ring
        rsintheta = fRadius * (FLOAT)sin(theta);
        FLOAT phi = 0.0f;

        for( j = 0; j < dwNumSections; j++ )
        {
            x = rsintheta * (FLOAT)sin(phi);
            z = rsintheta * (FLOAT)cos(phi);
        
            FLOAT u = (FLOAT)(1.0 - phi / (2*PI) );
            
            vPoint        = D3DVECTOR( sx*x, sy*y, sz*z );
            vNormal       = D3DVECTOR( x/fRadius, y/fRadius, z/fRadius );
            pvVertices[n] = D3DVERTEX( vPoint, vNormal, u, v );

            phi += dphi;
            ++n;
        }
        theta += dtheta;
    }

    // Generate triangles for top and bottom caps.
    for( i = 0; i < dwNumSections; i++ )
    {
        DWORD t1 = 3*i;
        DWORD t2 = 3*(dwNumTriangles - dwNumSections + i);

        pwIndices[t1+0] = (WORD)(0);
        pwIndices[t1+1] = (WORD)(i + 1);
        pwIndices[t1+2] = (WORD)(1 + ((i + 1) % dwNumSections));

        pwIndices[t2+0] = (WORD)( dwNumVertices - 1 );
        pwIndices[t2+1] = (WORD)( dwNumVertices - 2 - i );
        pwIndices[t2+2] = (WORD)( dwNumVertices - 2 - ((1 + i) % dwNumSections) );
    }

    // Generate triangles for the rings
    m = 1;            // 1st vertex begins at 1 to skip top point
    n = dwNumSections; // triangle being generated, skip the top cap 
    
    for( i = 0; i < dwNumRings; i++ )
    {
        for( j = 0; j < dwNumSections; j++ )
        {
            pwIndices[3*n+0] = (WORD)(m + j);
            pwIndices[3*n+1] = (WORD)(m + dwNumSections + j);
            pwIndices[3*n+2] = (WORD)(m + dwNumSections + ((j + 1) % dwNumSections));
            n++;
            
            pwIndices[3*n+0] = (WORD)(m + j);
            pwIndices[3*n+1] = (WORD)(m + dwNumSections + ((j + 1) % dwNumSections));
            pwIndices[3*n+2] = (WORD)(m + ((j + 1) % dwNumSections));
            n++;
        }
        m += dwNumSections;
    }

    (*pdwNumIndices)  = dwNumIndices;
    (*ppwIndices)     = pwIndices;
    (*pdwNumVertices) = dwNumVertices;
    (*ppvVertices)    = pvVertices;

    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: BlendObjects()
// Desc: Merges two sets of vertices together
//-----------------------------------------------------------------------------
VOID BlendObjects( DWORD dwCount, D3DVERTEX* pvInputVertices1, 
                   D3DVERTEX* pvInputVertices2,
                   D3DVERTEX* pvOutputVertices )
{
    D3DVERTEX* p1 = pvInputVertices1;
    D3DVERTEX* p2 = pvInputVertices2;
    D3DVERTEX* p3 = pvOutputVertices;

    FLOAT fMinZ = -ELLIPSE_Y_LENGTH * ELLIPSE_RADIUS;
    FLOAT fMaxZ = +ELLIPSE_Y_LENGTH * ELLIPSE_RADIUS;

    for( DWORD i=0; i<dwCount; i++ )
    {
        FLOAT m;
        FLOAT a = ( p2->z - fMinZ ) / ( fMaxZ - fMinZ );

        if( a >= 0.75f )
            m = 0.0f;
        else if( a >= 0.5f )
        {
            FLOAT x = 4*(0.75f-a);
            m = (x*x)*0.5f;
        }
        else if( a >= 0.25f )
        {
            FLOAT x = 4*(a-0.25f);
            m = 1.0f-(x*x)*0.5f;
        }
        else
            m = 1.0f;

        p3->x = LERP( m, p1->x, p2->x );
        p3->y = LERP( m, p1->y, p2->y );
        p3->z = LERP( m, p1->z, p2->z );

        p1++; p2++; p3++;
    }
}

