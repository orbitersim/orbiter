//-----------------------------------------------------------------------------
// File: Render.cpp
//
// Desc: Simple tutorial code to draw a 3D scene.
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
#define CONE_HEIGHT        3.0f
#define CONE_RADIUS        1.0f
#define NUM_CONE_SIDES     20
#define NUM_CONE_VERTICES (NUM_CONE_SIDES+1)

D3DVERTEX g_pvCone[NUM_CONE_VERTICES]; // untransformed tri fan with flexible verts


#define g_PI 3.1415926283


#define NUM_CONES 10
D3DMATRIX g_matConePos[NUM_CONES];
FLOAT     g_afConeColor[NUM_CONES][3];

inline FLOAT RandomVal()   { return ((FLOAT)(rand()-rand()))/RAND_MAX; }




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

    // Set the cone's vertices. The cone is a triangle fan, so the zeroth
    // vertex is the tip of the cone (center of the fan) and the rest of the
    // vertices are on the base on the cone.
    g_pvCone[0] = D3DVERTEX( D3DVECTOR( 0.0f, CONE_HEIGHT/2, 0.0f ), 
                             Normalize( D3DVECTOR( 0.0f, 1.0f, 0.0f ) ),
                             0.0f, 0.0f );

    for( i=0; i<NUM_CONE_SIDES; i++ )
    {
        FLOAT x = (FLOAT)sin( 2*g_PI*i/(NUM_CONE_SIDES-1) );
        FLOAT y = -CONE_HEIGHT/2;
        FLOAT z = (FLOAT)cos( 2*g_PI*i/(NUM_CONE_SIDES-1) );
        g_pvCone[i+1] = D3DVERTEX( CONE_RADIUS * D3DVECTOR( x, y, z ), 
                                   Normalize( D3DVECTOR( x, 0.5f, z ) ),
                                   0.0f, 0.0f );
    }

    // Set position and color attributes for each cone
    for( i=0; i<NUM_CONES; i++ )
    {
        ZeroMemory( &g_matConePos[i], sizeof(D3DMATRIX) );
        g_matConePos[i]._11 = 1.0f;               // Set the cone's scale...
        g_matConePos[i]._22 = 1.0f;
        g_matConePos[i]._33 = 1.0f;
        g_matConePos[i]._44 = 1.0f;

        g_matConePos[i]._41 = 10*RandomVal();     // ...the cone's position
        g_matConePos[i]._42 = 0.0f;
        g_matConePos[i]._43 = 10*RandomVal();

        g_afConeColor[i][0] = (1+RandomVal())/2;; // ...and it's color.
        g_afConeColor[i][1] = (1+RandomVal())/2;;
        g_afConeColor[i][2] = (1+RandomVal())/2;;
    }

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
    ZeroMemory( &light, sizeof(light) );
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
    D3DVECTOR vEyePt    = D3DVECTOR( 5*sinf(fTimeKey), 3.0f, 
                                     5*cosf(fTimeKey) );
    D3DVECTOR vLookatPt = D3DVECTOR( 4*sinf(fTimeKey+0.1f), 2.5f, 
                                     4*cosf( fTimeKey+0.1f) );
    D3DVECTOR vUpVec    = D3DVECTOR( 0.0f, 1.0f, 0.0f );

    // Use the above parameters to build a new view matrix and put it into
    // effect.
    D3DMATRIX matView;
    SetViewMatrix( matView, vEyePt, vLookatPt, vUpVec );
    pd3dDevice->SetTransform( D3DTRANSFORMSTATE_VIEW, &matView );

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: App_Render()
// Desc: Renders the scene. This tutorial draws a bunch of random-colored
//       cones.
//-----------------------------------------------------------------------------
HRESULT App_Render( LPDIRECT3DDEVICE7 pd3dDevice )
{
    // Clear the viewport and z-buffer
    pd3dDevice->Clear( 0, NULL, D3DCLEAR_TARGET|D3DCLEAR_ZBUFFER,
                       0x000000ff, 1.0f, 0L );

    // Begin the scene
    if( FAILED( pd3dDevice->BeginScene() ) )
        return E_FAIL;

    // Draw the set of cones
    for( DWORD i=0; i<NUM_CONES; i++ )
    {
        // Set the base material color for the cone
        D3DMATERIAL7 mtrl;
        ZeroMemory( &mtrl, sizeof(D3DMATERIAL7) );
        mtrl.diffuse.r = g_afConeColor[i][0];
        mtrl.diffuse.g = g_afConeColor[i][1];
        mtrl.diffuse.b = g_afConeColor[i][2];
        pd3dDevice->SetMaterial( &mtrl );

        // Position the cone with the world matrix
        pd3dDevice->SetTransform( D3DTRANSFORMSTATE_WORLD, &g_matConePos[i] );

        // Draw the cone
        pd3dDevice->DrawPrimitive( D3DPT_TRIANGLEFAN, D3DFVF_VERTEX, 
                                   g_pvCone, NUM_CONE_VERTICES, NULL );
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




