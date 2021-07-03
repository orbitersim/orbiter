//-----------------------------------------------------------------------------
// File: hier2.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM,
				 LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2,
	             LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera )
{
    D3DRMRENDERQUALITY quality = D3DRMRENDER_FLAT;
    LPDIRECT3DRMLIGHT l1 = NULL;
    LPDIRECT3DRMLIGHT l2 = NULL;
    LPDIRECT3DRMFRAME3 lights = NULL;
    LPDIRECT3DRMMESHBUILDER3 torus_builder = NULL;
    LPDIRECT3DRMMESHBUILDER3 sphere_builder = NULL;
    LPDIRECT3DRMMESHBUILDER3 cube1_builder = NULL;
    LPDIRECT3DRMMESHBUILDER3 cube2_builder = NULL;
    LPDIRECT3DRMMESH cube1_mesh = NULL;
    LPDIRECT3DRMMESH cube2_mesh = NULL;
    LPDIRECT3DRMMESH torus_mesh = NULL;
    LPDIRECT3DRMMESH sphere_mesh = NULL;
    LPDIRECT3DRMFRAME3 torus = NULL;
    LPDIRECT3DRMFRAME3 sphere = NULL;
    LPDIRECT3DRMFRAME3 cube1 = NULL;
    LPDIRECT3DRMFRAME3 cube2 = NULL;
 
    // This Demo shows a more complex hierarchy of frames

    dev->SetQuality( quality );
    dev->SetShades( 16 );

    // initialize the lights in the scene
    if( FAILED( pD3DRM->CreateFrame( scene, &lights ) ) )
		goto generic_error;
    if( FAILED( lights->SetPosition( scene, D3DVAL(5), D3DVAL(5),
                                -D3DVAL(5) ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_POINT, D3DVAL(0.9),
                                  D3DVAL(0.8), D3DVAL(0.7), &l1 ) ) )
		goto generic_error;
    if( FAILED( lights->AddLight( l1 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT, D3DVAL(0.1),
                                  D3DVAL(0.1), D3DVAL(0.1), &l2 ) ) )
		goto generic_error;
    if( FAILED( scene->AddLight( l2 ) ) )
		goto generic_error;

	// load mesh files
    if( FAILED( pD3DRM->CreateMeshBuilder( &torus_builder ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &sphere_builder ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &cube1_builder ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &cube2_builder ) ) )
		goto generic_error;
    if( FAILED( torus_builder->Load( "torus.x", NULL, D3DRMLOAD_FROMFILE,
		                             NULL, NULL ) ) )
    {
		MSG("Failed to load torus.x.\n");
		goto ret_with_error;
    }
    if( FAILED( sphere_builder->Load( "sphere4.x", NULL, D3DRMLOAD_FROMFILE,
		                              NULL, NULL ) ) )
	{
        MSG("Failed to load sphere4.x.\n");
		goto ret_with_error;
    }
    if( FAILED( cube1_builder->Load( "cube.x", NULL, D3DRMLOAD_FROMFILE,
		                             NULL, NULL ) ) )
	{
        MSG("Failed to load cube.x.\n");
		goto ret_with_error;
    }
    if( FAILED( cube2_builder->Load( "cube.x", NULL, D3DRMLOAD_FROMFILE,
		                             NULL, NULL ) ) )
	{
        MSG("Failed to load cube.x.\n");
		goto ret_with_error;
    }

    if (FAILED(cube1_builder->Scale( D3DVAL(0.25), D3DVAL(0.5),
                                 D3DVAL(1.0) ) ) )
		goto generic_error;
    if (FAILED(cube2_builder->Scale( D3DVAL(0.5), D3DVAL(1.5),
                                 D3DVAL(1.0) ) ) )
		goto generic_error;

    if (FAILED(cube1_builder->SetColorRGB( D3DVAL(0.7), 
                                        D3DVAL(0.0), D3DVAL(0.8) ) ) )
		goto generic_error;
    if (FAILED(cube2_builder->SetColorRGB( D3DVAL(0.0),
                                        D3DVAL(1.0), D3DVAL(0.5) ) ) )
		goto generic_error;
    if (FAILED(torus_builder->SetColorRGB( D3DVAL(0.2),
                                        D3DVAL(1.0), D3DVAL(0.8) ) ) )
		goto generic_error;

    if (FAILED( torus_builder->CreateMesh( &torus_mesh ) ) )
		goto generic_error;
    if( FAILED( sphere_builder->CreateMesh( &sphere_mesh ) ) )
	goto generic_error;
    if( FAILED( cube1_builder->CreateMesh( &cube1_mesh ) ) )
	goto generic_error;
    if( FAILED( cube2_builder->CreateMesh( &cube2_mesh ) ) )
	goto generic_error;
    SAFE_RELEASE(torus_builder);
    SAFE_RELEASE(sphere_builder);
    SAFE_RELEASE(cube1_builder);
    SAFE_RELEASE(cube2_builder);

    // create a torus frame within the scene create torus frame within
    // frame of sphere cube frame within frame of torus

    if( FAILED( pD3DRM->CreateFrame( scene, &sphere ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( sphere, &torus ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( torus, &cube1 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( torus, &cube2 ) ) )
		goto generic_error;

    // add the loaded mesh into the frame
    if( FAILED( torus->AddVisual( (LPDIRECT3DRMVISUAL) torus_mesh ) ) )
		goto generic_error;
    if( FAILED( sphere->AddVisual( (LPDIRECT3DRMVISUAL) sphere_mesh ) ) )
		goto generic_error;
    if( FAILED( cube1->AddVisual( (LPDIRECT3DRMVISUAL) cube1_mesh ) ) )
		goto generic_error;
    if( FAILED( cube2->AddVisual( (LPDIRECT3DRMVISUAL) cube2_mesh ) ) )
		goto generic_error;

    // set up the frames position, orientation and rotations

    camera->SetPosition( scene, D3DVAL(0), D3DVAL(0), -D3DVAL(25) );
	sphere->SetPosition( scene, D3DVAL(0), D3DVAL(0), D3DVAL(0) );
	torus->SetPosition( sphere, D3DVAL(6), D3DVAL(0), D3DVAL(0) );
	cube1->SetPosition( torus, D3DVAL(0), D3DVAL(4), D3DVAL(0) );
	cube2->SetPosition( torus, D3DVAL(0), -D3DVAL(4), D3DVAL(0) );
	
    sphere->SetRotation( scene, D3DVAL(0), D3DVAL(0), D3DVAL(1),D3DVAL(0.01) );
	torus->SetRotation( sphere, D3DVAL(1), D3DVAL(0), D3DVAL(0), D3DVAL(0.02) );
	cube1->SetRotation( torus, D3DVAL(0.1), D3DVAL(0.2),
                               D3DVAL(0.7), D3DVAL(0.03) );
	cube2->SetRotation( torus, D3DVAL(0.7), D3DVAL(0.1),
                               D3DVAL(0.2), D3DVAL(0.03) );
	
    SAFE_RELEASE(lights);
    SAFE_RELEASE(torus_mesh);
    SAFE_RELEASE(sphere_mesh);
    SAFE_RELEASE(cube1_mesh);
    SAFE_RELEASE(cube2_mesh);
    SAFE_RELEASE(torus);
    SAFE_RELEASE(sphere);
    SAFE_RELEASE(cube1);
    SAFE_RELEASE(cube2);
    SAFE_RELEASE(l1);
    SAFE_RELEASE(l2);
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE(l1);
    SAFE_RELEASE(l2);
    SAFE_RELEASE(lights);
    SAFE_RELEASE(torus_builder);
    SAFE_RELEASE(sphere_builder);
    SAFE_RELEASE(cube1_builder);
    SAFE_RELEASE(cube2_builder);
    SAFE_RELEASE(cube1_mesh);
    SAFE_RELEASE(cube2_mesh);
    SAFE_RELEASE(torus_mesh);
    SAFE_RELEASE(sphere_mesh);
    SAFE_RELEASE(torus);
    SAFE_RELEASE(sphere);
    SAFE_RELEASE(cube1);
    SAFE_RELEASE(cube2);
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
    (*pstrName) = "Frame Hierarchy II D3D RM Example";
}
