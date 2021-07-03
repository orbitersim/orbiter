//-----------------------------------------------------------------------------
// File: trans.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )


unsigned char check[] = 
{
    1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2,
    1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0, 0, 1,
    2, 3, 3, 2, 1, 0, 0, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
    2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1,
};

D3DRMPALETTEENTRY checkPal[] =
{
    {0, 0, 0, D3DRMPALETTE_READONLY},
    {127, 255, 127, D3DRMPALETTE_READONLY},
    {0, 0, 255, D3DRMPALETTE_READONLY},
    {255, 255, 127, D3DRMPALETTE_READONLY},
    {255, 255, 255, D3DRMPALETTE_READONLY},
};

D3DRMIMAGE checkImage =
{
    32, 32,
    1, 1,
    8, FALSE,
    32,
    check, NULL,
    0xff, 0xff, 0xff, 0xff,
    5, checkPal,
};




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
void __cdecl
MutateTextureCallback( LPDIRECT3DRMFRAME3 frame, void* arg, D3DVALUE delta)
{
    LPDIRECT3DRMTEXTURE3 tex = (LPDIRECT3DRMTEXTURE3) arg;
    static int col = -1;
    static int delay = 10;
    static int count = 0;
    int i;

    if (--delay)
	return;
    delay = 10;

    if (col >= 0)
	for (i = 0; i < sizeof(check); i++)
	    if (check[i] == 4)
		check[i] = col;

    count++;
    col = count & 3;

    for (i = 0; i < sizeof(check); i++)
	if (check[i] == col)
	    check[i] = 4;

    tex->Changed( D3DRMTEXTURE_CHANGEDPIXELS, 0, NULL);
}


//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM,
				 LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2 view,
	             LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera )
{
    LPDIRECT3DRMFRAME3 box1 = NULL;
    LPDIRECT3DRMFRAME3 box2 = NULL;
    LPDIRECT3DRMFRAME3 sphere = NULL;
    LPDIRECT3DRMFRAME3 decal = NULL;
    LPDIRECT3DRMFRAME3 light = NULL;
    LPDIRECT3DRMMESH obj = NULL;
    LPDIRECT3DRMLIGHT dlight = NULL;
    LPDIRECT3DRMLIGHT alight = NULL;
    LPDIRECT3DRMTEXTURE3 tex = NULL;
    LPDIRECT3DRMTEXTURE tex_1 = NULL;
    LPDIRECT3DRMMESHBUILDER3 builder = NULL;
    LPDIRECT3DRMWRAP wrap = NULL;
    LPDIRECT3DRMTEXTURE3 sphereTex = NULL;
    LPDIRECT3DRMTEXTURE sphereTex_1 = NULL;
    LPDIRECT3DRMTEXTURE3 decalTex = NULL;
    LPDIRECT3DRMMATERIAL2 mat = NULL;
    LPDIRECT3DRMMATERIAL mat_1 = NULL;
    D3DRMBOX box;
    D3DVALUE miny, maxy;
    D3DVALUE height;

    view = view;
    dev = dev;
    camera = camera;		/* unused */

    if (FAILED(scene->SetSceneBackground( D3DRGB(D3DVAL(0.2), D3DVAL(0.2),
                                      D3DVAL(0.2)))))
		goto generic_error;

    if (FAILED(pD3DRM->CreateFrame( scene, &light)))
	    goto generic_error;
    if (FAILED(light->SetPosition( scene, D3DVAL(2.0), D3DVAL(3.0),
                               D3DVAL(8.0))))
		goto generic_error;
    if (FAILED(light->SetOrientation( scene, D3DVAL(-1.0), D3DVAL(-1.0),
                                  D3DVAL(1.0), D3DVAL(0.0), D3DVAL(1.0), D3DVAL(0.0))))
		goto generic_error;
    if (FAILED(pD3DRM->CreateLightRGB( D3DRMLIGHT_DIRECTIONAL, D3DVAL(1),
                                  D3DVAL(1), D3DVAL(1), &dlight)))
		goto generic_error;
    if (FAILED(light->AddLight( dlight)))
	    goto generic_error;
    SAFE_RELEASE(dlight);
    SAFE_RELEASE(light);

    if (FAILED(pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT, D3DVAL(0.1),
                                  D3DVAL(0.1), D3DVAL(0.1), &alight)))
		goto generic_error;
    if (FAILED(scene->AddLight( alight)))
	    goto generic_error;
    SAFE_RELEASE(alight);

    if (FAILED(pD3DRM->CreateFrame( scene, &box1)))
	    goto generic_error;
    if (FAILED(box1->SetRotation( scene, D3DVAL(0.0), D3DVAL(1.0), D3DVAL(0.0),
			      D3DVAL(-0.02))))
		goto generic_error;
    if (FAILED(box1->SetPosition( scene, D3DVAL(2.0), D3DVAL(0.0), D3DVAL(7.0))))
	    goto generic_error;
    if (FAILED(pD3DRM->CreateMeshBuilder( &builder)))
	    goto generic_error;
    if( FAILED( builder->Load( "ncube.x", NULL,
    			D3DRMLOAD_FROMFILE, NULL, NULL)))
	{
        MSG("Failed to load ncube.x.\n");
		goto ret_with_error;
    }
    if (FAILED(builder->SetPerspective( TRUE)))
	    goto generic_error;
    if (FAILED(builder->CreateMesh( &obj)))
	    goto generic_error;
    SAFE_RELEASE(builder);
    if (FAILED(pD3DRM->CreateTexture(&checkImage, &tex)))
	    goto generic_error;
    if (FAILED(tex->QueryInterface( IID_IDirect3DRMTexture, (void **)&tex_1)))
	    goto generic_error;
    if (FAILED(obj->SetGroupTexture( D3DRMGROUP_ALLGROUPS, tex_1)))
	    goto generic_error;
    if (FAILED(obj->SetGroupMapping( D3DRMGROUP_ALLGROUPS, D3DRMMAP_PERSPCORRECT)))
	    goto generic_error;
    if (FAILED(box1->AddVisual( (LPDIRECT3DRMVISUAL) obj)))
	    goto generic_error;
    if (FAILED(box1->AddMoveCallback( MutateTextureCallback, tex,
                                      D3DRMCALLBACK_PREORDER)))
	    goto generic_error;
    SAFE_RELEASE(obj);
    SAFE_RELEASE(tex_1);
    SAFE_RELEASE(tex);

    if (FAILED(pD3DRM->CreateFrame( scene, &box2)))
	    goto generic_error;
    if (FAILED(box2->SetRotation( scene, D3DVAL(0.1), D3DVAL(1.0), D3DVAL(0.0),
                              D3DVAL(0.1))))
		goto generic_error;
    if (FAILED(box2->SetPosition( box1, D3DVAL(-4.0), D3DVAL(0.0), D3DVAL(0.0))))
	    goto generic_error;
    if (FAILED( pD3DRM->CreateMeshBuilder( &builder)))
	    goto generic_error;
    if( FAILED( builder->Load( "ncube.x", NULL,
    			D3DRMLOAD_FROMFILE, NULL, NULL)))
    {
		MSG("Failed to load ncube.x.\n");
		goto ret_with_error;
    }
    if (FAILED(builder->SetPerspective( TRUE)))
	    goto generic_error;
    if (FAILED(builder->CreateMesh( &obj)))
	    goto generic_error;
    SAFE_RELEASE(builder);
    if( FAILED( pD3DRM->LoadTexture( "checker.ppm", &tex)))
	{
		MSG("Failed to load checker.ppm.\n");
		goto ret_with_error;
    }
    if (FAILED(tex->QueryInterface( IID_IDirect3DRMTexture, (void **)&tex_1)))
	    goto generic_error;
    if (FAILED(tex->SetDecalTransparency( TRUE)))
	    goto generic_error;
    if (FAILED(obj->SetGroupTexture( D3DRMGROUP_ALLGROUPS, tex_1)))
	    goto generic_error;
    if (FAILED(obj->SetGroupMapping( D3DRMGROUP_ALLGROUPS, D3DRMMAP_PERSPCORRECT)))
	    goto generic_error;
    if (FAILED(box2->AddVisual( (LPDIRECT3DRMVISUAL) obj)))
	    goto generic_error;
    SAFE_RELEASE(obj);
    SAFE_RELEASE(tex_1);
    SAFE_RELEASE(tex);
    SAFE_RELEASE(box2);

    if (FAILED(pD3DRM->CreateMeshBuilder( &builder)))
	    goto generic_error;
    if( FAILED( builder->Load( "sphere3.x", NULL,
    			D3DRMLOAD_FROMFILE, NULL, NULL)))
	{
        MSG("Failed to load sphere3.x.\n");
		goto ret_with_error;
    }
    if (FAILED(builder->CreateMesh( &obj)))
	    goto generic_error;
    SAFE_RELEASE(builder);
    if (FAILED(obj->GetBox( &box)))
	    goto generic_error;
    maxy = box.max.y;
    miny = box.min.y;
    height = maxy - miny;

    if (FAILED(pD3DRM->CreateWrap( D3DRMWRAP_CYLINDER, NULL,
			      D3DVAL(0.0), D3DVAL(0.0), D3DVAL(0.0),
			      D3DVAL(0.0), D3DVAL(1.0), D3DVAL(0.0),
			      D3DVAL(0.0), D3DVAL(0.0), D3DVAL(1.0),
			      D3DVAL(0.0), D3DDivide(miny, height),
		              D3DVAL(1.0), D3DDivide(D3DVAL(-1.0),height),
		              &wrap)))
		goto generic_error;
    if (FAILED(wrap->Apply( (LPDIRECT3DRMOBJECT) obj)))
	    goto generic_error;
    SAFE_RELEASE(wrap);

    if( FAILED( pD3DRM->LoadTexture( "tex3.ppm", &sphereTex) ) )
	{
		MSG("Failed to load tex3.ppm.\n");
		goto ret_with_error;
    }
    if (FAILED(sphereTex->QueryInterface( IID_IDirect3DRMTexture, (void **)&sphereTex_1)))
	    goto generic_error;
    if (FAILED(sphereTex->SetColors( 16)))
	    goto generic_error;
    if (FAILED(obj->SetGroupTexture( D3DRMGROUP_ALLGROUPS, sphereTex_1)))
	    goto generic_error;
    if (FAILED(pD3DRM->CreateMaterial(D3DVAL(16.0), &mat)))
	    goto generic_error;
    if (FAILED(mat->QueryInterface( IID_IDirect3DRMMaterial, (void **)&mat_1)))
	    goto generic_error;
    if (FAILED(obj->SetGroupMaterial( D3DRMGROUP_ALLGROUPS, mat_1)))
	    goto generic_error;
    SAFE_RELEASE(sphereTex_1);
    SAFE_RELEASE(sphereTex);
    SAFE_RELEASE(mat_1);
    SAFE_RELEASE(mat);

    if (FAILED(pD3DRM->CreateFrame( scene, &sphere)))
	    goto generic_error;
    if (FAILED(sphere->AddVisual( (LPDIRECT3DRMVISUAL) obj)))
	    goto generic_error;
    SAFE_RELEASE(obj);
    if (FAILED(sphere->SetPosition( scene, D3DVAL(0.0), D3DVAL(0.0), D3DVAL(8.0))))
	    goto generic_error;
    if (FAILED(sphere->SetRotation( scene, D3DVAL(-0.1), D3DVAL(1.0),
	                        D3DVAL(0.0), D3DVAL(0.02))))
	    goto generic_error;
    if (FAILED(pD3DRM->CreateFrame( sphere, &decal)))
	    goto generic_error;

    if( FAILED( pD3DRM->LoadTexture( "checker.ppm", &decalTex) ))
	{
		MSG("Failed to load checker.ppm.\n");
		goto ret_with_error;
    }
    if (FAILED(decalTex->SetDecalScale( TRUE)))
	    goto generic_error;
    if (FAILED(decalTex->SetDecalSize( D3DVAL(2.0), D3DVAL(2.0))))
	    goto generic_error;
    if (FAILED(decalTex->SetDecalOrigin( 4, 4)))
	    goto generic_error;
    if (FAILED(decalTex->SetDecalTransparency( TRUE)))
	    goto generic_error;
    if (FAILED(decal->SetPosition( sphere, D3DVAL(0.0), D3DVAL(0.0), D3DVAL(2.0))))
	    goto generic_error;
    if (FAILED(decal->AddVisual( (LPDIRECT3DRMVISUAL) decalTex)))
	    goto generic_error;
    SAFE_RELEASE(decalTex);

    SAFE_RELEASE(box1);
    SAFE_RELEASE(decal);
    SAFE_RELEASE(sphere);
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE(box1);
    SAFE_RELEASE(box2);
    SAFE_RELEASE(sphere);
    SAFE_RELEASE(decal);
    SAFE_RELEASE(light);
    SAFE_RELEASE(obj);
    SAFE_RELEASE(dlight);
    SAFE_RELEASE(alight);
    SAFE_RELEASE(tex);
    SAFE_RELEASE(builder);
    SAFE_RELEASE(wrap);
    SAFE_RELEASE(sphereTex);
    SAFE_RELEASE(decalTex);
    SAFE_RELEASE(mat);
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
    (*pstrName) = "Transparency Direct3DRM Example";
}
