//-----------------------------------------------------------------------------
// File: Render.cpp
//
// Desc: Simple tutorial code to draw a 3D scene using textures.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------

#define STRICT
#define D3D_OVERLOADS
#include <math.h>
#include <d3d.h>




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
LPDIRECTDRAWSURFACE7 CreateTexture( LPDIRECT3DDEVICE7 pd3dDevice, CHAR* strName );

LPDIRECTDRAWSURFACE7 g_pddsTexture1 = NULL;     // Some textures for the cube
LPDIRECTDRAWSURFACE7 g_pddsTexture2 = NULL;
LPDIRECTDRAWSURFACE7 g_pddsTexture3 = NULL;

#define NUM_CUBE_VERTICES (4*6)
D3DVERTEX g_pCubeVertices[NUM_CUBE_VERTICES]; // Vertices for the cube




//-----------------------------------------------------------------------------
// Name: CreateCube()
// Desc: Sets up the vertices for a cube. Don't forget to set the texture
//       coordinates for each vertex.
//-----------------------------------------------------------------------------
VOID CreateCube( D3DVERTEX* pVertices )
{
    // Define the normals for the cube
    D3DVECTOR n0( 0.0f, 0.0f,-1.0f ); // Front face
    D3DVECTOR n1( 0.0f, 0.0f, 1.0f ); // Back face
    D3DVECTOR n2( 0.0f, 1.0f, 0.0f ); // Top face
    D3DVECTOR n3( 0.0f,-1.0f, 0.0f ); // Bottom face
    D3DVECTOR n4( 1.0f, 0.0f, 0.0f ); // Right face
    D3DVECTOR n5(-1.0f, 0.0f, 0.0f ); // Left face

    // Front face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n0, 0.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n0, 1.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n0, 0.0f, 1.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n0, 1.0f, 1.0f );

    // Back face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n1, 1.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n1, 1.0f, 1.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n1, 0.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n1, 0.0f, 1.0f );

    // Top face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n2, 0.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n2, 1.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n2, 0.0f, 1.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n2, 1.0f, 1.0f );

    // Bottom face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n3, 0.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n3, 0.0f, 1.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n3, 1.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n3, 1.0f, 1.0f );

    // Right face
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f,-1.0f), n4, 0.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f, 1.0f, 1.0f), n4, 1.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f,-1.0f), n4, 0.0f, 1.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR( 1.0f,-1.0f, 1.0f), n4, 1.0f, 1.0f );

    // Left face
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f,-1.0f), n5, 1.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f,-1.0f), n5, 1.0f, 1.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f, 1.0f, 1.0f), n5, 0.0f, 0.0f );
    *pVertices++ = D3DVERTEX( D3DVECTOR(-1.0f,-1.0f, 1.0f), n5, 0.0f, 1.0f );
}




//-----------------------------------------------------------------------------
// Name: App_InitDeviceObjects()
// Desc: Initialize scene objects. This function sets up our scene, which is 
//       simply a rotating, texture-mapped cube.
//-----------------------------------------------------------------------------
HRESULT App_InitDeviceObjects( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Generate the vertices for the cube
    CreateCube( g_pCubeVertices );

    // Create some textures from file-based bitmaps
    g_pddsTexture1 = CreateTexture( pd3dDevice, "tree1.bmp" );
    g_pddsTexture2 = CreateTexture( pd3dDevice, "tex1.bmp" );
    g_pddsTexture3 = CreateTexture( pd3dDevice, "earth.bmp" );

    // For simplicity, just use ambient lighting and a white material
    D3DMATERIAL7 mtrl;
	ZeroMemory( &mtrl, sizeof(mtrl) );
	mtrl.diffuse.r = mtrl.diffuse.g = mtrl.diffuse.b = 1.0f;
	mtrl.ambient.r = mtrl.ambient.g = mtrl.ambient.b = 1.0f;
    pd3dDevice->SetMaterial( &mtrl );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0xffffffff );

    // Set the projection matrix. Note that the view and world matrices are
    // set in the App_FrameMove() function, so they can be animated each
    // frame.
    D3DMATRIX matProj;
    ZeroMemory( &matProj, sizeof(D3DMATRIX) );
    matProj._11 =  2.0f;
    matProj._22 =  2.0f;
    matProj._33 =  1.0f;
    matProj._34 =  1.0f;
    matProj._43 = -1.0f;
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_FrameMove()
// Desc: Called once per frame, the call is used for animating the scene. The
//       device is used for changing various render states, and the timekey is
//       used for timing of the dynamics of the scene.
//-----------------------------------------------------------------------------
HRESULT App_FrameMove( LPDIRECT3DDEVICE7 pd3dDevice, FLOAT fTimeKey )
{
    // Set the view matrix so that the camera is backed out along the z-axis,
    // and looks down on the cube (rotating along the x-axis by -0.5 radians).
    D3DMATRIX matView;
    ZeroMemory( &matView, sizeof(D3DMATRIX) );
    matView._11 = 1.0f;
    matView._22 =  (FLOAT)cos(-0.5f);
    matView._23 =  (FLOAT)sin(-0.5f);
    matView._32 = -(FLOAT)sin(-0.5f);
    matView._33 =  (FLOAT)cos(-0.5f);
    matView._43 = 5.0f;
    matView._44 = 1.0f;
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    // Set the world matrix to rotate along the y-axis, in sync with the 
    // timekey
    D3DMATRIX matRotate;
    ZeroMemory( &matRotate, sizeof(D3DMATRIX) );
    matRotate._11 =  (FLOAT)cos(fTimeKey);
    matRotate._13 =  (FLOAT)sin(fTimeKey);
    matRotate._22 =  1.0f;
    matRotate._31 = -(FLOAT)sin(fTimeKey);
    matRotate._33 =  (FLOAT)cos(fTimeKey);
    matRotate._44 =  1.0f;
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matRotate );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_Render()
// Desc: Renders the scene. This tutorial draws textured-mapped, rotating cube.
//-----------------------------------------------------------------------------
HRESULT App_Render( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Clear the viewport to a deep blue color
    pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET, 0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( FAILED( pd3dDevice->BeginScene() ) )
        return E_FAIL;

    // Draw the front and back faces of the cube using texture 1
    pd3dDevice->SetTexture( 0, g_pddsTexture1 );
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                               g_pCubeVertices+0, 4, NULL );
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                               g_pCubeVertices+4, 4, NULL );

    // Draw the top and bottom faces of the cube using texture 2
    pd3dDevice->SetTexture( 0, g_pddsTexture2 );
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                               g_pCubeVertices+8, 4, NULL );
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                               g_pCubeVertices+12, 4, NULL );

    // Draw the left and right faces of the cube using texture 3
    pd3dDevice->SetTexture( 0, g_pddsTexture3 );
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                               g_pCubeVertices+16, 4, NULL );
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX, 
                               g_pCubeVertices+20, 4, NULL );
    
    // End the scene.
    pd3dDevice->EndScene();

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_DeleteDeviceObjects()
// Desc: Called when the device is being deleted, this function deletes any
//       device dependant objects.
//-----------------------------------------------------------------------------
VOID App_DeleteDeviceObjects( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Release the texture objects that were created earlier
    if( g_pddsTexture1 ) g_pddsTexture1->Release();
    if( g_pddsTexture2 ) g_pddsTexture2->Release();
    if( g_pddsTexture3 ) g_pddsTexture3->Release();

    g_pddsTexture1 = NULL;
    g_pddsTexture2 = NULL;
    g_pddsTexture3 = NULL;
}




