//-----------------------------------------------------------------------------
// File: faces.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )


D3DVECTOR avCubeVertices[] = { -0.5f, -0.5f, -0.5f,  -0.5f, -0.5f,  0.5f,
                               -0.5f,  0.5f, -0.5f,  -0.5f,  0.5f,  0.5f,
                                0.5f, -0.5f, -0.5f,   0.5f, -0.5f,  0.5f,
                                0.5f,  0.5f, -0.5f,   0.5f,  0.5f,  0.5f };

// face_data is a vertex count followed by the vertex reference in the
// vertices list. If normal_count > 0, then pairs of vectors are
// referenced: first the vertex and secondly the normal reference. the
// list is terminated by a 0 (vertex count to a face)


//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL build_face1( LPDIRECT3DRMMESHBUILDER3 pMeshBuilder )
{
    // Add a single face with all normals pointing in the same direction.
    D3DVECTOR avVertices[4] = { 0.0f, 0.0f, 0.0f,   0.0f, 0.0f, 1.0f,
		                        1.0f, 0.0f, 1.0f,   1.0f, 0.0f, 0.0f };
    D3DVECTOR avNormals[4] =  { 0.0f, 1.0f, 0.0f,   0.0f, 1.0f, 0.0f,
		                        0.0f, 1.0f, 0.0f,   0.0f, 1.0f, 0.0f };
    DWORD adwFaceData[] = { 4, 0, 0, 1, 1, 2, 2, 3, 3, 0 };

    if( FAILED( pMeshBuilder->AddFaces( 4, avVertices, 4, avNormals,
										adwFaceData, NULL ) ) )
		return FALSE;
   	return TRUE;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL build_face2( LPDIRECT3DRMMESHBUILDER3 pMeshBuilder )
{
    // Add a single face with normals pointing in different directions to
    // approximate a curved surface.
    D3DVECTOR avVertices[4] = { 0.0f, 0.0f, 0.0f,   0.0f, 0.0f, 1.0f, 
		                        1.0f, 0.0f, 1.0f,   1.0f, 0.0f, 0.0f };
    D3DVECTOR avNormals[4]  = { 0.5f, 0.7f, 0.5f,   0.5f, 0.7f,-0.5f, 
		                       -0.5f, 0.7f,-0.5f,  -0.5f, 0.7f, 0.5f };
    DWORD adwFaceData[] = {4, 0, 0, 1, 1, 2, 2, 3, 3, 0};

    if( FAILED( pMeshBuilder->AddFaces( 4, avVertices, 4, avNormals,
		                                adwFaceData, NULL ) ) )
		return FALSE;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL build_cube1( LPDIRECT3DRMMESHBUILDER3 pMeshBuilder )
{
    // cube 1 has planar faces, one normal is shared by all vertices of a
    // face
    DWORD adwFace1Data[] = {  4,  1,  0,  5,  1,  7,  2,  3,  3,
	                          4,  2,  4,  3,  5,  7,  6,  6,  7,
	                          4,  5,  8,  4,  9,  6, 10,  7, 11,
	                          4,  0, 12,  2, 13,  6, 14,  4, 15,
	                          4,  0, 16,  4, 17,  5, 18,  1, 19,
	                          4,  0, 20,  1, 21,  3, 22,  2, 23, 0 };

    D3DVECTOR avCube1Normals[] = { 0.0f, 0.0f, 1.0f,   0.0f, 0.0f, 1.0f, 
		                           0.0f, 0.0f, 1.0f,   0.0f, 0.0f, 1.0f, 
		                           0.0f, 1.0f, 0.0f,   0.0f, 1.0f, 0.0f, 
		                           0.0f, 1.0f, 0.0f,   0.0f, 1.0f, 0.0f, 
		                           1.0f, 0.0f, 0.0f,   1.0f, 0.0f, 0.0f, 
		                           1.0f, 0.0f, 0.0f,   1.0f, 0.0f, 0.0f, 
		                           0.0f, 0.0f,-1.0f,   0.0f, 0.0f,-1.0f, 
		                           0.0f, 0.0f,-1.0f,   0.0f, 0.0f,-1.0f, 
		                           0.0f,-1.0f, 0.0f,   0.0f,-1.0f, 0.0f, 
		                           0.0f,-1.0f, 0.0f,   0.0f,-1.0f, 0.0f, 
		                          -1.0f, 0.0f, 0.0f,  -1.0f, 0.0f, 0.0f, 
								  -1.0f, 0.0f, 0.0f,  -1.0f, 0.0f, 0.0f };

    if( FAILED( pMeshBuilder->AddFaces( 8, avCubeVertices, 6*4, avCube1Normals,
			                            adwFace1Data, NULL ) ) )
		return FALSE;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL build_cube2( LPDIRECT3DRMMESHBUILDER3 pMeshBuilder )
{
    // cube 2 has rounded faces, the normals radiate out at each vertex,
    // one normal to each vertex

    DWORD adwFace2Data[] = { 4, 1, 1, 5, 5, 7, 7, 3, 3,
	                         4, 2, 2, 3, 3, 7, 7, 6, 6,
							 4, 5, 5, 4, 4, 6, 6, 7, 7,
							 4, 0, 0, 2, 2, 6, 6, 4, 4,
							 4, 0, 0, 4, 4, 5, 5, 1, 1,
							 4, 0, 0, 1, 1, 3, 3, 2, 2, 0 };

    D3DVECTOR avCube2Normals[] = { -1.0f, -1.0f, -1.0f,   -1.0f, -1.0f,  1.0f,
	                               -1.0f,  1.0f, -1.0f,   -1.0f,  1.0f,  1.0f,
                                    1.0f, -1.0f, -1.0f,    1.0f, -1.0f,  1.0f,
									1.0f,  1.0f, -1.0f,    1.0f,  1.0f,  1.0f };

    if( FAILED( pMeshBuilder->AddFaces( 8, avCubeVertices, 8, avCube2Normals,
			                            adwFace2Data, NULL ) ) )
		return FALSE;
    return TRUE;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM,
				 LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2,
	             LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera )
{
    LPDIRECT3DRMMESHBUILDER3 light_builder, face1_builder, face2_builder;
    LPDIRECT3DRMMESHBUILDER3 cube1_builder, cube2_builder;
    LPDIRECT3DRMMESH         light_mesh, face1_mesh, face2_mesh;
    LPDIRECT3DRMMESH         cube1_mesh, cube2_mesh;
    LPDIRECT3DRMFRAME3       frame1, frame2, face_frame1, face_frame2;
    LPDIRECT3DRMFRAME3       light_frame, lights;
    LPDIRECT3DRMLIGHT        light1, light2;

    light_builder = face1_builder = face2_builder = NULL;
    light_mesh    = face1_mesh    = face2_mesh    = NULL;
	frame1        = frame2        = face_frame1   = face_frame2 = NULL;
	cube1_builder = cube2_builder = NULL;
    cube1_mesh    = cube2_mesh    = NULL;
    light_frame   = lights        = NULL;
	light1        = light2        = NULL;

    // This Demo shows the construction of planar faces with vertex
    // normals for realistic modelling of flat and curved surfaces by
    // polygonal approximation.

    dev->SetQuality( D3DRMRENDER_GOURAUD );
	
    if( FAILED( pD3DRM->CreateFrame( scene, &face_frame1 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( scene, &face_frame2 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( scene, &frame1 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( scene, &frame2 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( scene, &light_frame ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( light_frame, &lights ) ) )
		goto generic_error;

    lights->SetPosition( light_frame, 0.0f, 0.0f, 2.2f );
    light_frame->SetPosition( scene, 0.0f, 0.4f, 0.0f );

    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_POINT,
		                                 0.9f, 0.8f, 0.7f, &light1 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT, 
		                                0.02f, 0.01f, 0.01f, &light2 ) ) )
		goto generic_error;
    if( FAILED( lights->AddLight( light1 ) ) )
		goto generic_error;
    if( FAILED( scene->AddLight( light2 ) ) )
		goto generic_error;
    SAFE_RELEASE(light1);
    SAFE_RELEASE(light2);

    if( FAILED( pD3DRM->CreateMeshBuilder( &face1_builder ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &face2_builder ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &cube1_builder ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &cube2_builder ) ) )
		goto generic_error;

    if( !build_face1(face1_builder ) )
		goto generic_error;
    if( !build_face2(face2_builder ) )
		goto generic_error;
    if( !build_cube1(cube1_builder ) )
		goto generic_error;
    if( !build_cube2(cube2_builder ) )
		goto generic_error;

    if( FAILED( face1_builder->CreateMesh( &face1_mesh ) ) )
		goto generic_error;
    if( FAILED( face2_builder->CreateMesh( &face2_mesh ) ) )
		goto generic_error;
    if( FAILED( cube1_builder->CreateMesh( &cube1_mesh ) ) )
		goto generic_error;
    if( FAILED( cube2_builder->CreateMesh( &cube2_mesh ) ) )
		goto generic_error;

    SAFE_RELEASE( face1_builder );
    SAFE_RELEASE( face2_builder );
    SAFE_RELEASE( cube1_builder );
    SAFE_RELEASE( cube2_builder );

    if( FAILED( face_frame1->AddVisual( (LPDIRECT3DRMVISUAL)face1_mesh ) ) )
		goto generic_error;
    if( FAILED( face_frame2->AddVisual( (LPDIRECT3DRMVISUAL)face2_mesh ) ) )
		goto generic_error;
    if( FAILED( frame1->AddVisual( (LPDIRECT3DRMVISUAL)cube1_mesh ) ) )
		goto generic_error;
    if( FAILED( frame2->AddVisual( (LPDIRECT3DRMVISUAL)cube2_mesh ) ) )
		goto generic_error;

    if( FAILED( pD3DRM->CreateMeshBuilder( &light_builder ) ) )
		goto generic_error;
    if( FAILED( light_builder->Load( "sphere0.x", NULL, D3DRMLOAD_FROMFILE,
		                             NULL, NULL ) ) )
    {
		MSG("Failed to load sphere0.x\n");
		goto ret_with_error;
    }
    light_builder->SetColorRGB( 1.0f, 1.0f, 1.0f );
    light_builder->Scale( 0.1f, 0.1f, 0.1f );
    light_builder->SetQuality( D3DRMRENDER_UNLITFLAT );

    if( FAILED( light_builder->CreateMesh( &light_mesh ) ) )
		goto generic_error;
    SAFE_RELEASE(light_builder);

    if( FAILED( lights->AddVisual( (LPDIRECT3DRMVISUAL) light_mesh ) ) )
		goto generic_error;

    camera->SetPosition( scene, 0.0f, 2.0f, -5.0f );
    face_frame1->SetPosition( scene, -0.5f, 0.5f, 2.0f );
    face_frame2->SetPosition( scene, -0.5f, -1.5f, 2.0f );
    frame1->SetPosition( scene, -2.0f, 0.0f, 2.0f );
    frame2->SetPosition( scene, 1.5f, 0.0f, 2.0f );

    face_frame1->SetOrientation( scene, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, -1.0f );
    face_frame2->SetOrientation( scene, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, -1.0f );
    camera->SetOrientation( scene, 0.0f, -0.2f, 1.0f, 0.0f, 1.0f, 0.0f );
    frame1->SetRotation( scene, 1.0f, 0.0f, 1.0f, -0.005f );
    frame2->SetRotation( scene, 0.0f, 1.0f, 1.0f, -0.005f );
    light_frame->SetRotation( scene, 0.0f, 1.0f, 0.0f, -0.02f );

    SAFE_RELEASE(light_mesh);
    SAFE_RELEASE(face1_mesh);
    SAFE_RELEASE(face2_mesh);
    SAFE_RELEASE(cube1_mesh);
    SAFE_RELEASE(cube2_mesh);

    SAFE_RELEASE(frame1);
    SAFE_RELEASE(frame2);
    SAFE_RELEASE(face_frame1);
    SAFE_RELEASE(face_frame2);
    SAFE_RELEASE(light_frame);
    SAFE_RELEASE(lights);
    return TRUE;
generic_error:
    MSG("An error has occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE(light_builder);
    SAFE_RELEASE(face1_builder);
    SAFE_RELEASE(face2_builder);
    SAFE_RELEASE(cube1_builder);
    SAFE_RELEASE(cube2_builder);
    SAFE_RELEASE(light_mesh);
    SAFE_RELEASE(face1_mesh);
    SAFE_RELEASE(face2_mesh);
    SAFE_RELEASE(cube1_mesh);
    SAFE_RELEASE(cube2_mesh);
    SAFE_RELEASE(frame1);
    SAFE_RELEASE(frame2);
    SAFE_RELEASE(face_frame1);
    SAFE_RELEASE(face_frame2);
    SAFE_RELEASE(light_frame);
    SAFE_RELEASE(lights);
    SAFE_RELEASE(light1);
    SAFE_RELEASE(light2);
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
VOID OverrideDefaults( BOOL* pbNoTextures, BOOL* pbResizingDisabled, 
					   BOOL* pbConstRenderQuality, CHAR** pstrName )
{
    (*pbNoTextures) = TRUE;
    (*pbConstRenderQuality) = TRUE;
    (*pstrName) = "Faces Direct3DRM Example";

}
