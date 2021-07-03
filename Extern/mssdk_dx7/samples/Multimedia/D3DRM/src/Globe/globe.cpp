//-----------------------------------------------------------------------------
// File: globe.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )


D3DRMRENDERQUALITY quality = D3DRMRENDER_GOURAUD;

LPDIRECT3DRMANIMATION2 anim;
int motion_points = 17;

D3DVECTOR motion[17] =
{	{-14.0f,  4.0f, 45.0f }, {  9.0f,-3.0f, 36.0f }, {  5.0f, 0.0f,  5.0f },
	{ -5.0f,  8.0f, 25.0f }, {  0.0f, 3.0f, 20.0f }, { -3.0f,-4.0f, 10.0f },
	{  8.0f, 10.0f, 15.0f }, { 16.0f, 0.0f, 30.0f }, { 10.0f,-4.0f, 42.0f }, 
	{-15.0f,  0.0f, 37.0f }, { -5.0f,-7.0f, 15.0f }, {  5.0f, 5.0f, 20.0f },
	{  5.0f, 10.0f, 30.0f }, { 13.0f, 8.0f, 50.0f }, {  0.0f, 8.0f, 25.0f },
	{  0.0f,  0.0f, 20.0f }, {-14.0f, 4.0f, 45.0f }
};





//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
void __cdecl
CleanupPathCallback( LPDIRECT3DRMOBJECT, VOID* )
{
    anim->Release();
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
void __cdecl
MoveCameraCallback(LPDIRECT3DRMFRAME3 frame, void *arg, D3DVALUE delta)
{
    static D3DVALUE t = D3DVAL(0.0);
    LPDIRECT3DRMFRAME3 world_frame = (LPDIRECT3DRMFRAME3) arg;
    LPDIRECT3DRMFRAME3 scene;

    frame->GetScene( &scene);

    t += D3DVAL(0.08);
    anim->SetFrame( frame);
    anim->SetTime( t);

    frame->LookAt( world_frame, scene, D3DRMCONSTRAIN_Z);

    scene->Release();
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
BOOL run_world_scene( LPDIRECT3DRM3 pD3DRM, LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera)
{
    LPDIRECT3DRMFRAME3 light1 = NULL;
    LPDIRECT3DRMFRAME3 world_frame = NULL;
    LPDIRECT3DRMLIGHT l1 = NULL;
    LPDIRECT3DRMLIGHT l2 = NULL;
    LPDIRECT3DRMTEXTURE3 tex = NULL;
    LPDIRECT3DRMWRAP wrap = NULL;
    LPDIRECT3DRMMESHBUILDER3 sphere3_builder = NULL;
    LPDIRECT3DRMMATERIAL2 mat = NULL;
    D3DRMBOX box;
    D3DVALUE miny, maxy, height;

    if( FAILED( pD3DRM->CreateFrame( scene, &light1)))
		goto generic_error;
    if( FAILED( light1->SetPosition( scene, D3DVAL(2), D3DVAL(0.0), D3DVAL(22))))
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_DIRECTIONAL, D3DVAL(0.9),
                                  D3DVAL(0.9), D3DVAL(0.9), &l1)))
		goto generic_error;
    if( FAILED( light1->AddLight( l1)))
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT, D3DVAL(0.1),
                                  D3DVAL(0.1), D3DVAL(0.1), &l2)))
		goto generic_error;
    if( FAILED( scene->AddLight( l2)))
		goto generic_error;
    if( FAILED( camera->SetPosition( scene, D3DVAL(0.0), D3DVAL(0.0), D3DVAL(0.0))))
		goto generic_error;
    if( FAILED( camera->SetOrientation( scene, D3DVAL(0.0), D3DVAL(0.0), D3DVAL(1), D3DVAL(0.0),
						D3DVAL(1), D3DVAL(0.0))))
		goto generic_error;

    if( FAILED( pD3DRM->CreateFrame( scene, &world_frame)))
		goto generic_error;
    if( FAILED( world_frame->SetPosition( scene, D3DVAL(0.0), D3DVAL(0.0), D3DVAL(30))))
		goto generic_error;
    if( FAILED( world_frame->SetOrientation( scene, D3DVAL(0.0), D3DVAL(0.0), D3DVAL(1),
                                        D3DVAL(0.0), D3DVAL(1), D3DVAL(0.0))))
		goto generic_error;
    if( FAILED( world_frame->SetRotation( scene, D3DVAL(0.03), 
                                     D3DVAL(0.1), D3DVAL(0.0), D3DVAL(0.1))))
		goto generic_error;

    if( FAILED( pD3DRM->CreateMeshBuilder( &sphere3_builder)))
		goto generic_error;
    if( FAILED( sphere3_builder->Load( "sphere3.x",
    					NULL, D3DRMLOAD_FROMFILE, NULL, NULL) ) )
	{
		MSG("Failed to load sphere3.x.\n" );
		goto ret_with_error;
    }
    if( FAILED( pD3DRM->CreateMaterial( D3DVAL(20.0), &mat)))
		goto generic_error;
    if( FAILED( sphere3_builder->SetMaterial( mat)))
		goto generic_error;
    mat->Release(); mat = NULL;
    if( FAILED( sphere3_builder->Scale(
    				   D3DVAL(2), D3DVAL(2), D3DVAL(2))))
		goto generic_error;

    if( FAILED( sphere3_builder->SetColorRGB( D3DVAL(1), D3DVAL(1), D3DVAL(1))))
		goto generic_error;
    if( FAILED( sphere3_builder->GetBox( &box)))
		goto generic_error;
    maxy = box.max.y;
    miny = box.min.y;
    height = maxy - miny;

    if( FAILED( pD3DRM->CreateWrap( D3DRMWRAP_CYLINDER, NULL,
									D3DVAL(0.0), D3DVAL(0.0), D3DVAL(0.0),
									D3DVAL(0.0), D3DVAL(1.0), D3DVAL(0.0),
									D3DVAL(0.0), D3DVAL(0.0), D3DVAL(1.0),
									D3DVAL(0.0), D3DDivide(miny, height),
									D3DVAL(1.0), D3DDivide(-D3DVAL(1.0), height),
									&wrap ) ) )
		goto generic_error;
    if( FAILED( wrap->Apply( (LPDIRECT3DRMOBJECT) sphere3_builder)))
		goto generic_error;

    if( FAILED( pD3DRM->LoadTexture( "tex1.ppm", &tex) ) )
	{
		MSG("Failed to load tex1.ppm\n" );
		goto ret_with_error;
    }
    if( FAILED( tex->SetShades( 32)))
		goto generic_error;
    if( FAILED( sphere3_builder->SetTexture( tex)))
		goto generic_error;

    if( FAILED( world_frame->AddVisual( (LPDIRECT3DRMVISUAL) sphere3_builder ) ) )
		goto generic_error;

    if( FAILED( camera->AddMoveCallback( MoveCameraCallback, 
				    (void *) world_frame, D3DRMCALLBACK_PREORDER ) ) )
		goto generic_error;
    if( FAILED( camera->AddDestroyCallback( CleanupPathCallback, NULL ) ) )
		goto generic_error;

    SAFE_RELEASE(light1);
    SAFE_RELEASE(world_frame);
    SAFE_RELEASE(sphere3_builder);
    SAFE_RELEASE(l1);
    SAFE_RELEASE(l2);
    SAFE_RELEASE(tex);
    SAFE_RELEASE(wrap);
    return TRUE;
generic_error:
    MSG("An error occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE(light1);
    SAFE_RELEASE(world_frame);
    SAFE_RELEASE(l1);
    SAFE_RELEASE(l2);
    SAFE_RELEASE(tex);
    SAFE_RELEASE(wrap);
    SAFE_RELEASE(sphere3_builder);
    SAFE_RELEASE(mat);
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM,
				 LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2,
	             LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera)
{
    int i;

    // This example flys a camera around a globe. The path taken is
    // defined by a spline curve.

    if (FAILED(dev->SetQuality( quality)))
	goto generic_error;

    if (FAILED(pD3DRM->CreateAnimation( &anim)))
	goto generic_error;
    if (FAILED(anim->SetOptions( D3DRMANIMATION_CLOSED | D3DRMANIMATION_LINEARPOSITION | D3DRMANIMATION_POSITION)))
	goto generic_error;
    for (i = 0; i < motion_points; i++) {
        if (FAILED(anim->AddPositionKey( D3DVAL(i), motion[i].x, motion[i].y,
	                         motion[i].z/2)))
				 goto generic_error;
    }
    if (!run_world_scene( pD3DRM, scene, camera))
	goto ret_with_error;
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
ret_with_error:
    return FALSE;
}





//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
VOID OverrideDefaults( BOOL* pbNoTextures, BOOL* pbResizingDisabled, 
					   BOOL* pbConstRenderQuality, CHAR** pstrName )
{
    (*pstrName)     = "Globe Direct3DRM Example";
}
