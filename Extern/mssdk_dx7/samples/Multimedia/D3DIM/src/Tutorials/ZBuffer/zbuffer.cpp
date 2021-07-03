//-----------------------------------------------------------------------------
// File: Z-buffer.cpp
//
// Desc: Simple tutorial code to show how to enable z-buffering. The z-buffer
//       itself is created in the winmain.cpp file. This file controls
//       rendering and the setting of renderstates (some of which affect
//       z-buffering).
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
#define NUM_OBJECTS 4
D3DMATRIX           g_matLocal[NUM_OBJECTS];
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
	g_pvTriangleVertices[0] = D3DVERTEX( p1, vNormal, 0.0f, 0.0f );
	g_pvTriangleVertices[1] = D3DVERTEX( p2, vNormal, 0.0f, 0.0f );
	g_pvTriangleVertices[2] = D3DVERTEX( p3, vNormal, 0.0f, 0.0f );
    
	// Initialize the 3 vertices for the back of the triangle
	g_pvTriangleVertices[3] = D3DVERTEX( p1, -vNormal, 0.0f, 0.0f );
	g_pvTriangleVertices[4] = D3DVERTEX( p3, -vNormal, 0.0f, 0.0f );
	g_pvTriangleVertices[5] = D3DVERTEX( p2, -vNormal, 0.0f, 0.0f );
    
	// Set the material as yellow. We're setting the ambient color here
	// since this tutorial only uses ambient lighting. For apps that use real
	// lights, the diffuse and specular values should be set. (In addition, the
	// polygons' vertices need normals for true lighting.)
    D3DMATERIAL7 mtrl;
    ZeroMemory( &mtrl, sizeof(mtrl) );
    mtrl.ambient.r = 1.0f;
    mtrl.ambient.g = 1.0f;
    mtrl.ambient.b = 1.0f;
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
    // For this tutorial, we are rotating several triangles about the y-axis.
	// (Note: the triangles are meant to intersect, to show how z-buffering
	// handles hidden-surface removal.)

	// For each object, set up a local rotation matrix to be applied right
	// before rendering the object's polygons.
	for( int i=0; i<NUM_OBJECTS; i++ )
	{
		ZeroMemory( &g_matLocal[i], sizeof(D3DMATRIX) );
		g_matLocal[i]._11 = (FLOAT)cos( fTimeKey + (3.14159*i)/NUM_OBJECTS );
		g_matLocal[i]._33 = (FLOAT)cos( fTimeKey + (3.14159*i)/NUM_OBJECTS );
		g_matLocal[i]._13 = (FLOAT)sin( fTimeKey + (3.14159*i)/NUM_OBJECTS );
		g_matLocal[i]._31 = (FLOAT)sin( fTimeKey + (3.14159*i)/NUM_OBJECTS );
		g_matLocal[i]._22 = g_matLocal[i]._44 = 1.0f;
	}

	return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_Render()
// Desc: Renders the scene. This tutorial draws a bunch of intersecting
//       triangles that are rotating about the y-axis. Without z-buffering,
//       the polygons could not be drawn correctly (unless the app performed
//       complex polygon-division routines and sorted the polygons in back-to-
//       front order.)
//-----------------------------------------------------------------------------
HRESULT App_Render( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Clear the viewport to a blue color. Also "clear" the z-buffer to the
	// value 1.0 (which represents the far clipping plane).
    pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
		               0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( FAILED( pd3dDevice->BeginScene() ) )
		return E_FAIL;

	// Enable z-buffering. (Note: we don't really need to do this every frame.)
    pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, TRUE );

	// Draw all the objects. Note: you can tweak the above statement to disable
	// the z-buffer, and compare the difference in output. With z-buffering,
	// the inter-penetrating triangles are drawn correctly.
	for( int i=0; i<NUM_OBJECTS; i++ )
	{
		// Alternate the color of every other object
		DWORD dwColor = ( i%2 ) ? 0x0000ff00 : 0x00ffff00;
	    pd3dDevice->SetRenderState( D3DRENDERSTATE_AMBIENT, dwColor );

		// Set the local matrix for the object
		pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &g_matLocal[i] );

		// Draw the object. (Note: Subsequent tutorials will go into more
		// detail on the various calls for drawing polygons.)
		pd3dDevice->DrawPrimitive( D3DPT_TRIANGLELIST, D3DFVF_VERTEX,
			                       g_pvTriangleVertices, 6, NULL );
	}

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
}




