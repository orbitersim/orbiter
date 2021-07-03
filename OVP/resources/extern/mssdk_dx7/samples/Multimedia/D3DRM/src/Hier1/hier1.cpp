//-----------------------------------------------------------------------------
// File: hier1.cpp
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
	             LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera)
{
    D3DRMRENDERQUALITY quality = D3DRMRENDER_FLAT;
    LPDIRECT3DRMFRAME3 lights = NULL;
    LPDIRECT3DRMFRAME3 axis = NULL;
    LPDIRECT3DRMMESH torus_mesh = NULL;
    LPDIRECT3DRMMESH sphere_mesh = NULL;
    LPDIRECT3DRMFRAME3 torus = NULL;
    LPDIRECT3DRMFRAME3 sphere = NULL;
    LPDIRECT3DRMLIGHT lp = NULL;
    LPDIRECT3DRMLIGHT la = NULL;
    LPDIRECT3DRMMESHBUILDER3 builder = NULL;

    // This Demo shows a simple hierarchy of frames

    if( FAILED( dev->SetQuality( quality)))
		goto generic_error;

    // initialize the lights in the scene
    if( FAILED( pD3DRM->CreateFrame( scene, &lights ) ) )
		goto generic_error;
    if( FAILED(lights->SetPosition( scene, D3DVAL(5), D3DVAL(5), 
                                -D3DVAL(9) ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_POINT, D3DVAL(0.9), 
                                  D3DVAL(0.8), D3DVAL(0.7), &lp ) ) )
		goto generic_error;
    if( FAILED( lights->AddLight( lp)))
		goto generic_error;

    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT, D3DVAL(0.1), 
                                  D3DVAL(0.1), D3DVAL(0.1), &la ) ) )
		goto generic_error;
    if( FAILED( scene->AddLight( la ) ) )
		goto generic_error;

    // load mesh files
    if( FAILED( pD3DRM->CreateMeshBuilder( &builder ) ) )
		goto generic_error;
    if( FAILED( builder->Load( "torus.x", NULL,
    			D3DRMLOAD_FROMFILE, NULL, NULL) ) )
    {
		MSG("Failed to load torus.x\n" );
		goto ret_with_error;
    }
    if( FAILED(builder->CreateMesh( &torus_mesh)))
		goto generic_error;
    SAFE_RELEASE(builder);

    if( FAILED( pD3DRM->CreateMeshBuilder( &builder ) ) )
		goto generic_error;
    if( FAILED( builder->Load( "sphere2.x", NULL,
    				D3DRMLOAD_FROMFILE, NULL, NULL) ) )
    {
		MSG("Failed to load sphere2.x\n" );
		goto ret_with_error;
    }
    if( FAILED( builder->CreateMesh( &sphere_mesh ) ) )
		goto generic_error;
    SAFE_RELEASE(builder);

    if( FAILED( torus_mesh->SetGroupColorRGB( -1, D3DVAL(0.0), 
                                            D3DVAL(0.0), D3DVAL(1.0))))
		goto generic_error;

    // create a torus frame within the scene create axis frame within
    // frame of sphere create torus frame within frame of axis
    if( FAILED( pD3DRM->CreateFrame( scene, &sphere ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( sphere, &axis ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( axis, &torus ) ) )
		goto generic_error;

	// load a texture and wrap it onto the sphere
	LPDIRECT3DRMTEXTURE3 tex;
	LPDIRECT3DRMWRAP wrap;
	D3DVALUE height;

	D3DRMBOX box;
	D3DVALUE miny, maxy;
	if( FAILED( sphere_mesh->GetBox( &box ) ) )
	    goto generic_error;
	maxy = box.max.y;
	miny = box.min.y;
	height = maxy - miny;

	if( FAILED( pD3DRM->CreateWrap( D3DRMWRAP_CYLINDER, NULL, D3DVAL(0.0),
	                          D3DVAL(0.0), D3DVAL(0.0), D3DVAL(0.0),
	                          D3DVAL(0.0), D3DVAL(1.0), D3DVAL(0.0),
	                          D3DVAL(1.0), D3DVAL(0.0), D3DVAL(0.0),
	                          D3DDivide(maxy, height), D3DVAL(1.0),
	                          D3DDivide(D3DVAL(1), height), &wrap)))
		goto generic_error;
	if( FAILED( wrap->Apply( (LPDIRECT3DRMOBJECT)sphere_mesh)))
	    goto generic_error;
	SAFE_RELEASE(wrap);

	if( FAILED( pD3DRM->LoadTexture( "tex2.ppm", &tex ) ) )
	{
		MSG("Failed to load tex2.ppm\n" );
	    goto ret_with_error;
	}
	if( FAILED( tex->SetColors( 16 ) ) )
	    goto generic_error;
	if( FAILED( tex->SetShades( 8 ) ) )
	    goto generic_error;
	LPDIRECT3DRMTEXTURE tex_1;
    if ( FAILED( tex->QueryInterface( IID_IDirect3DRMTexture, (void **)&tex_1)))
        goto generic_error;
	if( FAILED( sphere_mesh->SetGroupTexture( -1, tex_1 ) ) )
	    goto generic_error;
	SAFE_RELEASE(tex_1);
    SAFE_RELEASE(tex);

    // add the loaded mesh into the frame
    if( FAILED(torus->AddVisual( (LPDIRECT3DRMVISUAL) torus_mesh ) ) )
		goto generic_error;
    if( FAILED(sphere->AddVisual( (LPDIRECT3DRMVISUAL) sphere_mesh ) ) )
		goto generic_error;

    // set up the frames position, orientation and rotation
    if( FAILED(camera->SetPosition( scene, D3DVAL(0), D3DVAL(0), -D3DVAL(10) ) ) )
		goto generic_error;
    if( FAILED(sphere->SetPosition( scene, D3DVAL(0), D3DVAL(0), D3DVAL(0) ) ) )
		goto generic_error;
    if( FAILED(axis->SetPosition( sphere, D3DVAL(2), D3DVAL(0), D3DVAL(0) ) ) )
		goto generic_error;
    if( FAILED(torus->SetPosition( axis, D3DVAL(1.0), D3DVAL(0), D3DVAL(0) ) ) )
		goto generic_error;

    if( FAILED(sphere->SetRotation( scene, D3DVAL(0), D3DVAL(1.0), D3DVAL(0.5),D3DVAL(0.05) ) ) )
		goto generic_error;
    if( FAILED(axis->SetRotation( sphere, D3DVAL(1), D3DVAL(0), D3DVAL(0), D3DVAL(0.01) ) ) )
		goto generic_error;
    if( FAILED(torus->SetRotation( axis, D3DVAL(0), D3DVAL(0), D3DVAL(1), -D3DVAL(0.05) ) ) )
		goto generic_error;

    SAFE_RELEASE(lights);
    SAFE_RELEASE(axis);
    SAFE_RELEASE(torus_mesh);
    SAFE_RELEASE(sphere_mesh);
    SAFE_RELEASE(torus);
    SAFE_RELEASE(sphere);
    SAFE_RELEASE(lp);
    SAFE_RELEASE(la);
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE(lights);
    SAFE_RELEASE(axis);
    SAFE_RELEASE(torus_mesh);
    SAFE_RELEASE(sphere_mesh);
    SAFE_RELEASE(torus);
    SAFE_RELEASE(sphere);
    SAFE_RELEASE(lp);
    SAFE_RELEASE(la);
    SAFE_RELEASE(builder);
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
VOID OverrideDefaults( BOOL* pbNoTextures, BOOL* pbResizingDisabled, 
					   BOOL* pbConstRenderQuality, CHAR** pstrName )
{
	(*pbConstRenderQuality) = TRUE;
    (*pstrName) = "Frame Hierarchy D3D RM Example";
}
