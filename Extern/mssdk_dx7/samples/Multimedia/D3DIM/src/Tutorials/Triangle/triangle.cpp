//-----------------------------------------------------------------------------
// File: Triangle.cpp
//
// Desc: Simple tutorial code to show the rendering of one triangle.
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
LPDIRECT3DMATERIAL3 g_pmtrlObjectMtrl = NULL;
D3DVERTEX           g_pvTriangleVertices[6];




//-----------------------------------------------------------------------------
// Name: App_InitDeviceObjects()
// Desc: Initialize scene objects. This function is called after all the
//       DirectDraw and Direct3D objects have been initialized. It makes sense
//       to structure code this way, separating the DDraw/D3D initialization
//       code from the app-specific intialization code.
//-----------------------------------------------------------------------------
HRESULT App_InitDeviceObjects( LPDIRECT3DDEVICE7 pd3dDevice )
{
	// Data for the geometry of the triangle. Note that this tutorial only
	// uses ambient lighting, so the vertices' normals are not actually used.
	D3DVECTOR p1( 0.0f, 3.0f, 0.0f );
	D3DVECTOR p2( 3.0f,-3.0f, 0.0f );
	D3DVECTOR p3(-3.0f,-3.0f, 0.0f );
	D3DVECTOR vNormal( 0.0f, 0.0f, 1.0f );
	
	// Initialize the 3 vertices for the front of the triangle
	g_pvTriangleVertices[0] = D3DVERTEX( p1, vNormal, 0, 0 );
	g_pvTriangleVertices[1] = D3DVERTEX( p2, vNormal, 0, 0 );
	g_pvTriangleVertices[2] = D3DVERTEX( p3, vNormal, 0, 0 );
    
	// Initialize the 3 vertices for the back of the triangle
	g_pvTriangleVertices[3] = D3DVERTEX( p1, -vNormal, 0, 0 );
	g_pvTriangleVertices[4] = D3DVERTEX( p3, -vNormal, 0, 0 );
	g_pvTriangleVertices[5] = D3DVERTEX( p2, -vNormal, 0, 0 );
    
	// Set the material as yellow. We're setting the ambient color here
	// since this tutorial only uses ambient lighting. For apps that use real
	// lights, the diffuse and specular values should be set. (In addition, the
	// polygons' vertices need normals for true lighting.)
    D3DMATERIAL7 mtrl;
    ZeroMemory( &mtrl, sizeof(mtrl) );
    mtrl.ambient.r = 1.0f;
    mtrl.ambient.g = 1.0f;
    mtrl.ambient.b = 0.0f;
    pd3dDevice->SetMaterial( &mtrl );

	// The ambient lighting value is another state to set. Here, we are turning
	// ambient lighting on to full white.
    pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, 0xffffffff );

    // Set the transform matrices. Direct3D uses three independant matrices:
	// the world matrix, the view matrix, and the projection matrix. For
	// convienence, we are first setting up an identity matrix.
    D3DMATRIX mat;
	mat._11 = mat._22 = mat._33 = mat._44 = 1.0f;
	mat._12 = mat._13 = mat._14 = mat._41 = 0.0f;
	mat._21 = mat._23 = mat._24 = mat._42 = 0.0f;
	mat._31 = mat._32 = mat._34 = mat._43 = 0.0f;
	
	// The world matrix controls the position and orientation of the polygons
	// in world space. We'll use it later to spin the triangle.
	D3DMATRIX matWorld = mat;
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matWorld );

	// The view matrix defines the position and orientation of the camera.
	// Here, we are just moving it back along the z-axis by 10 units.
	D3DMATRIX matView = mat;
	matView._43 = 10.0f;
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

	// The projection matrix defines how the 3D scene is "projected" onto the
	// 2D render target (the backbuffer surface). Refer to the docs for more
	// info about projection matrices.
	D3DMATRIX matProj = mat;
	matProj._11 =  2.0f;
	matProj._22 =  2.0f;
	matProj._34 =  1.0f;
	matProj._43 = -1.0f;
	matProj._44 =  0.0f;
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
	// For this simple tutorial, we are rotating the triangle about the y-axis.
	// To do this, just set up a 4x4 matrix defining the rotation, and set it
	// as the new world transform.
	FLOAT fCos = (FLOAT)cos( fTimeKey );
	FLOAT fSin = (FLOAT)sin( fTimeKey );
	D3DMATRIX matSpin;
	
	matSpin._11 = fCos;  matSpin._12 = 0.0f;  matSpin._13 =-fSin;  matSpin._14 = 0.0f;
	matSpin._21 = 0.0f;  matSpin._22 = 1.0f;  matSpin._23 = 0.0f;  matSpin._24 = 0.0f;
	matSpin._31 = fSin;  matSpin._32 = 0.0f;  matSpin._33 = fCos;  matSpin._34 = 0.0f;
	matSpin._41 = 0.0f;  matSpin._42 = 0.0f;  matSpin._43 = 0.0f;  matSpin._44 = 1.0f;

	pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &matSpin );

	return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_Render()
// Desc: Called once per frame, the function is called to render the scene.
//       Rendering consists of the following:
//       (1) Clear the viewport
//       (2) Call BeginScene()
//       (3) Draw the polygons (primitives) and change the state as needed
//       (4) Call EndScene()
//
//       After this call completes, the device's render target (the backbuffer
//       surface) will contain the rendered image of the scene. All that is
//       left is to copy the contents of the backbuffer to the primary.
//-----------------------------------------------------------------------------
HRESULT App_Render( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Clear the viewport to a blue color (dwColor = 0x000000ff). For apps with
	// z-buffers, this call can also clear the z-buffer (if present) as well.
    pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET, 0x000000ff, 0L, 0L );

    // Begin the scene
    if( FAILED( pd3dDevice->BeginScene() ) )
		return E_FAIL;

	// Draw the triangle using a DrawPrimitive() call. Subsequent
	// tutorials will go into more detail on the various calls for
	// drawing polygons.
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
                               g_pvTriangleVertices, 6, NULL );

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
	// Release the material that was created earlier.
    if( g_pmtrlObjectMtrl )
		g_pmtrlObjectMtrl->Release();
	g_pmtrlObjectMtrl = NULL;
}




