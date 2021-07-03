//-----------------------------------------------------------------------------
// File: DrawPrims.cpp
//
// Desc: Simple tutorial code to show how to use the various draw primitives
//       calls for rendering polygons in Direct3D.
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
D3DMATRIX g_matLocal[NUM_OBJECTS];


struct MyFlexibleVertex
{
    D3DVECTOR vPosition;
    D3DVECTOR vNormal;
};

#define CONE_HEIGHT        3.0f
#define CONE_RADIUS        1.0f
#define NUM_CONE_SIDES     20
#define NUM_CONE_VERTICES (NUM_CONE_SIDES+1)

MyFlexibleVertex g_pvCone[NUM_CONE_VERTICES]; // untransformed tri fan with flexible verts


#define NUM_WALL_SIDES     6
#define NUM_WALL_VERTICES (2*NUM_WALL_SIDES)
D3DVERTEX g_pvWall[NUM_WALL_VERTICES+4]; // untransformed, tri strip

#define NUM_CUBE_VERTICES (8)
#define NUM_CUBE_INDICES  (6*3*2)
D3DTLVERTEX g_pvCube[NUM_CUBE_VERTICES];
WORD        g_pwCubeIndices[NUM_CUBE_INDICES];

#define g_PI 3.1415926283f




//-----------------------------------------------------------------------------
// Name: App_InitDeviceObjects()
// Desc: Initialize scene objects. This function is called after all the
//       DirectDraw and Direct3D objects have been initialized. It makes sense
//       to structure code this way, separating the DDraw/D3D initialization
//       code from the app-specific intialization code.
//-----------------------------------------------------------------------------
HRESULT App_InitDeviceObjects( LPDIRECT3DDEVICE7 pd3dDevice )
{
    DWORD i;

    for( i=0; i<NUM_WALL_SIDES; i++ )
    {
        FLOAT x = (FLOAT)sin( 2*g_PI*i/(NUM_WALL_SIDES-1) );
        FLOAT z = (FLOAT)cos( 2*g_PI*i/(NUM_WALL_SIDES-1) );

        g_pvWall[2*i+0] = D3DVERTEX( 10.0f * D3DVECTOR( x, -0.1f, z ),
                                     D3DVECTOR( -x, 0, -z ), 0, 0 );
        g_pvWall[2*i+1] = D3DVERTEX( 10.0f * D3DVECTOR( x, 0.1f, z ),
                                     D3DVECTOR( -x, 0, -z ), 0, 0 );
    }
    
    g_pvCone[0].vPosition = D3DVECTOR( 0, CONE_HEIGHT/2, 0 );
    g_pvCone[0].vNormal   = Normalize( D3DVECTOR( 0, 1, 0 ) );

    for( i=0; i<NUM_CONE_SIDES; i++ )
    {
        FLOAT x = (FLOAT)sin( 2*g_PI*i/(NUM_CONE_SIDES-1) );
        FLOAT y = -CONE_HEIGHT/2;
        FLOAT z = (FLOAT)cos( 2*g_PI*i/(NUM_CONE_SIDES-1) );
        g_pvCone[i+1].vPosition = CONE_RADIUS * D3DVECTOR( x, y, z );
        g_pvCone[i+1].vNormal   = Normalize( D3DVECTOR( x, 0.5f, z ) );
    }
    
    WORD* pwIndex = g_pwCubeIndices;
    *pwIndex++ = 1;  *pwIndex++ = 2;  *pwIndex++ = 3;
    *pwIndex++ = 2;  *pwIndex++ = 1;  *pwIndex++ = 0;
    *pwIndex++ = 4;  *pwIndex++ = 5;  *pwIndex++ = 6;
    *pwIndex++ = 6;  *pwIndex++ = 5;  *pwIndex++ = 7;
    *pwIndex++ = 3;  *pwIndex++ = 2;  *pwIndex++ = 6;
    *pwIndex++ = 3;  *pwIndex++ = 6;  *pwIndex++ = 7;
    *pwIndex++ = 0;  *pwIndex++ = 1;  *pwIndex++ = 4;
    *pwIndex++ = 4;  *pwIndex++ = 1;  *pwIndex++ = 5;
    *pwIndex++ = 2;  *pwIndex++ = 0;  *pwIndex++ = 4;
    *pwIndex++ = 2;  *pwIndex++ = 4;  *pwIndex++ = 6;
    *pwIndex++ = 1;  *pwIndex++ = 3;  *pwIndex++ = 5;
    *pwIndex++ = 5;  *pwIndex++ = 3;  *pwIndex++ = 7;

    // Set the material as yellow. We're setting the ambient color here
    // since this tutorial only uses ambient lighting. For apps that use real
    // lights, the diffuse and specular values should be set. (In addition, the
    // polygons' vertices need normals for true lighting.)
    D3DMATERIAL7 mtrl;
    ZeroMemory( &mtrl, sizeof(mtrl) );
    mtrl.diffuse.r = mtrl.diffuse.g = mtrl.diffuse.b = 1.0f;
    mtrl.ambient.r = mtrl.ambient.g = mtrl.ambient.b = 1.0f;
    pd3dDevice->SetMaterial( &mtrl );

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

    // Enable z-buffering.
    pd3dDevice->SetRenderState( D3DRENDERSTATE_ZENABLE, TRUE );

    // Set up the light. Note: to be friendly for cards that can do hardware
    // transform-and-lighting (TnL), we should (but we don't) check the caps
    // to see if the device can support directional lights. 
    D3DLIGHT7 light;
    ZeroMemory( &light, sizeof(D3DLIGHT7) );
    light.dltType       =  D3DLIGHT_DIRECTIONAL;
    light.dcvDiffuse.r  =  1.0f;
    light.dcvDiffuse.g  =  1.0f;
    light.dcvDiffuse.b  =  1.0f;
    light.dvDirection.x =  1.0f;
    light.dvDirection.y = -1.0f;
    light.dvDirection.z =  1.0f;
    pd3dDevice->SetLight( 0, &light );
    pd3dDevice->LightEnable( 0, TRUE );
    pd3dDevice->SetRenderState( D3DRENDERSTATE_LIGHTING, TRUE );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: SetViewMatrix()
// Desc: Given an eye point, a lookat point, and an up vector, this
//       function builds a 4x4 view matrix.
//-----------------------------------------------------------------------------
HRESULT SetViewMatrix( D3DMATRIX& mat, D3DVECTOR& vFrom, D3DVECTOR& vAt,
                       D3DVECTOR& vWorldUp )
{
    // Get the z basis vector, which points straight ahead. This is the
    // difference from the eyepoint to the lookat point.
    D3DVECTOR vView = vAt - vFrom;

    FLOAT fLength = Magnitude( vView );
    if( fLength < 1e-6f )
        return E_INVALIDARG;

    // Normalize the z basis vector
    vView /= fLength;

    // Get the dot product, and calculate the projection of the z basis
    // vector onto the up vector. The projection is the y basis vector.
    FLOAT fDotProduct = DotProduct( vWorldUp, vView );

    D3DVECTOR vUp = vWorldUp - fDotProduct * vView;

    // If this vector has near-zero length because the input specified a
    // bogus up vector, let's try a default up vector
    if( 1e-6f > ( fLength = Magnitude( vUp ) ) )
    {
        vUp = D3DVECTOR( 0.0f, 1.0f, 0.0f ) - vView.y * vView;

        // If we still have near-zero length, resort to a different axis.
        if( 1e-6f > ( fLength = Magnitude( vUp ) ) )
        {
            vUp = D3DVECTOR( 0.0f, 0.0f, 1.0f ) - vView.z * vView;

            if( 1e-6f > ( fLength = Magnitude( vUp ) ) )
                return E_INVALIDARG;
        }
    }

    // Normalize the y basis vector
    vUp /= fLength;

    // The x basis vector is found simply with the cross product of the y
    // and z basis vectors
    D3DVECTOR vRight = CrossProduct( vUp, vView );
    
    // Start building the matrix. The first three rows contains the basis
    // vectors used to rotate the view to point at the lookat point
    mat._11 = vRight.x;  mat._12 = vUp.x;  mat._13 = vView.x;  mat._14 = 0.0f;
    mat._21 = vRight.y;  mat._22 = vUp.y;  mat._23 = vView.y;  mat._24 = 0.0f;
    mat._31 = vRight.z;  mat._32 = vUp.z;  mat._33 = vView.z;  mat._34 = 0.0f;

    // Do the translation values (rotations are still about the eyepoint)
    mat._41 = - DotProduct( vFrom, vRight );
    mat._42 = - DotProduct( vFrom, vUp );
    mat._43 = - DotProduct( vFrom, vView );
    mat._44 = 1.0f;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: TransformVertices()
// Desc: Manually transform vertices from local 3D space to 2D screen
//       coordinates.
//-----------------------------------------------------------------------------
HRESULT TransformVertices( LPDIRECT3DDEVICE7 pd3dDevice,
                           D3DTLVERTEX* pvVertices, DWORD dwNumVertices )
{
    // Get the width and height of the viewport. This is needed to scale the
    // transformed vertices to fit the render window.
    D3DVIEWPORT7 vp;
    pd3dDevice->GetViewport( &vp );
    DWORD dwClipWidth  = vp.dwWidth/2;
    DWORD dwClipHeight = vp.dwHeight/2;

    // Get the current matrix set. This is needed for the transformation.
    D3DMATRIX matWorld, matView, matProj;
    pd3dDevice->GetTransform( D3DTRANSFORMSTATE_WORLD,      &matWorld );
    pd3dDevice->GetTransform( D3DTRANSFORMSTATE_VIEW,       &matView );
    pd3dDevice->GetTransform( D3DTRANSFORMSTATE_PROJECTION, &matProj );
    D3DMATRIX matSet = matWorld * matView * matProj;

    // Transform each vertex through the current matrix set
    for( DWORD i=0; i<dwNumVertices; i++ )
    {
        // Get the untransformed vertex position
        FLOAT x = pvVertices[i].sx;
        FLOAT y = pvVertices[i].sy;
        FLOAT z = pvVertices[i].sz;

        // Transform it through the current matrix set
        FLOAT xp = matSet._11*x + matSet._21*y + matSet._31*z + matSet._41;
        FLOAT yp = matSet._12*x + matSet._22*y + matSet._32*z + matSet._42;
        FLOAT zp = matSet._13*x + matSet._23*y + matSet._33*z + matSet._43;
        FLOAT wp = matSet._14*x + matSet._24*y + matSet._34*z + matSet._44;

        // Finally, scale the vertices to screen coords. This step first 
        // "flattens" the coordinates from 3D space to 2D device coordinates,
        // by dividing each coordinate by the wp value. Then, the x- and
        // y-components are transformed from device coords to screen coords.
        // Note 1: device coords range from -1 to +1 in the viewport.
        // Note 2: the sz-coordinate will be used in the z-buffer.
        pvVertices[i].sx  = ( 1.0f + (xp/wp) ) * dwClipWidth;
        pvVertices[i].sy  = ( 1.0f - (yp/wp) ) * dwClipHeight;
        pvVertices[i].sz  = zp / wp;
        pvVertices[i].rhw = wp;
    }

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
    // Move the view point around in a circle. The view is most conveniently
    // defined by an eye point, a lookat point, and a vector defining the up
    // direction.
    D3DVECTOR vEyePt    = D3DVECTOR( 5*sinf( fTimeKey), 3, 5*cosf( fTimeKey ) );
    D3DVECTOR vLookatPt = D3DVECTOR( 4*sinf( fTimeKey+0.1f), 2.5, 4*cosf( fTimeKey+0.1f ) );
    D3DVECTOR vUpVec    = D3DVECTOR( 0, 1, 0 );

    // Use the above parameters to build a new view matrix and put it into
    // effect.
    D3DMATRIX matView;
    SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    // For TLVERTEX-type vertices, the app needs to do it's own transform and
    // lighting. That will be done here, after the matrices are set up. This
    // tutorial uses TLVERTEX's to draw a cube. We start with untransformed
    // vertices for the cube, manually transform them, and then render them
    // as normal during the render loop.
    
    // Fill the array of TL vertices with untransformed data for the cube
    g_pvCube[0] = D3DTLVERTEX( D3DVECTOR(-1,-1,-1 ), 0, 0xffffffff, 0, 0, 0 );
    g_pvCube[1] = D3DTLVERTEX( D3DVECTOR( 1,-1,-1 ), 0, 0xff00ffff, 0, 0, 0 );
    g_pvCube[2] = D3DTLVERTEX( D3DVECTOR(-1, 1,-1 ), 0, 0xffff00ff, 0, 0, 0 );
    g_pvCube[3] = D3DTLVERTEX( D3DVECTOR( 1, 1,-1 ), 0, 0xff0000ff, 0, 0, 0 );
    g_pvCube[4] = D3DTLVERTEX( D3DVECTOR(-1,-1, 1 ), 0, 0xffffff00, 0, 0, 0 );
    g_pvCube[5] = D3DTLVERTEX( D3DVECTOR( 1,-1, 1 ), 0, 0xff00ff00, 0, 0, 0 );
    g_pvCube[6] = D3DTLVERTEX( D3DVECTOR(-1, 1, 1 ), 0, 0xffff0000, 0, 0, 0 );
    g_pvCube[7] = D3DTLVERTEX( D3DVECTOR( 1, 1, 1 ), 0, 0xff000000, 0, 0, 0 );

    // Let the app transform the cube's 8 vertices
    TransformVertices( pd3dDevice, g_pvCube, 8 );

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
    // Clear the viewport and z-buffer
    pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
                       0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( FAILED( pd3dDevice->BeginScene() ) )
        return E_FAIL;

    // Draw the wall, composed of a D3DVERTEX-type triangle strip
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLESTRIP, D3DFVF_VERTEX,
                               g_pvWall, NUM_WALL_VERTICES, NULL );

    // Draw the cone, which is a triangle fan of custom, flexible vertices
    pd3dDevice->DrawPrimitive( D3DPT_TRIANGLEFAN, D3DFVF_XYZ|D3DFVF_NORMAL,
                               g_pvCone, NUM_CONE_VERTICES, NULL );

    // Draw the cube, which is app-transformed (and lit), indexed vertices
    pd3dDevice->DrawIndexedPrimitive( D3DPT_TRIANGLELIST, D3DFVF_TLVERTEX,
                                      g_pvCube, NUM_CUBE_VERTICES,
                                      g_pwCubeIndices, NUM_CUBE_INDICES, NULL );

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




