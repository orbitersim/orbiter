//-----------------------------------------------------------------------------
// File: tex1.cpp
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
				 LPDIRECT3DRMDEVICE3, LPDIRECT3DRMVIEWPORT2,
	             LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 )
{
    LPDIRECT3DRMMESH mesh = NULL;
    LPDIRECT3DRMMESHBUILDER3 builder = NULL;
    LPDIRECT3DRMFRAME3 frame = NULL;
    LPDIRECT3DRMTEXTURE3 tex = NULL;
    LPDIRECT3DRMMATERIAL2 mat = NULL;
    LPDIRECT3DRMWRAP wrap = NULL;
    LPDIRECT3DRMLIGHT l1 = NULL;
    LPDIRECT3DRMLIGHT l2 = NULL;
    D3DRMBOX box;
    D3DVALUE miny, maxy;
    D3DVALUE height;

    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT, D3DVAL(0.2),
                                  D3DVAL(0.2), D3DVAL(0.2), &l1)))
		goto generic_error;
    if( FAILED( scene->AddLight( l1)))
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( scene, &frame)))
		goto generic_error;
    if( FAILED( frame->SetOrientation( scene, -D3DVAL(1), -D3DVAL(1),
                                  D3DVAL(1), D3DVAL(0), D3DVAL(1), D3DVAL(0))))
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_DIRECTIONAL, D3DVAL(1),
                                  D3DVAL(1), D3DVAL(1), &l2)))
		goto generic_error;
    if( FAILED( frame->AddLight( l2)))
		goto generic_error;
    SAFE_RELEASE(frame);

    if( FAILED( pD3DRM->CreateFrame( scene, &frame)))
		goto generic_error;
    if( FAILED( frame->SetPosition( scene, D3DVAL(0), D3DVAL(0), D3DVAL(15))))
		goto generic_error;
    if( FAILED( frame->SetRotation( scene, D3DVAL(1.1), D3DVAL(0.3), 
                               D3DVAL(0.0), D3DVAL(0.04))))
		goto generic_error;

    if( FAILED( pD3DRM->CreateMeshBuilder( &builder)))
		goto generic_error;
    if( FAILED( builder->Load( "sphere4.x", NULL, D3DRMLOAD_FROMFILE,
		                       NULL, NULL ) ) )
	{
        MSG("Failed to load sphere4.x.\n" );
		goto ret_with_error;
    }

    if( FAILED( pD3DRM->LoadTexture( "tex7.ppm", &tex) ) )
	{
        MSG("Failed to load tex7.ppm.\n" );
		goto ret_with_error;
    }
    if( FAILED( pD3DRM->CreateMaterial( D3DVAL(10.0), &mat)))
		goto generic_error;

    if( FAILED( builder->SetTexture( tex)))
		goto generic_error;
    if( FAILED( builder->SetMaterial( mat)))
		goto generic_error;
    if( FAILED( builder->SetColorRGB( D3DVAL(1.0), D3DVAL(1.0), D3DVAL(1.0))))
		goto generic_error;

    if( FAILED( builder->CreateMesh( &mesh)))
		goto generic_error;
    SAFE_RELEASE(builder);

    if( FAILED( mesh->GetBox( &box)))
		goto generic_error;
    maxy = box.max.y;
    miny = box.min.y;
    height = maxy - miny;

    if( FAILED( pD3DRM->CreateWrap( D3DRMWRAP_CYLINDER, NULL, 0.0f,
                              0.0f, 0.0f, 0.0f,
                              1.0f, 0.0f, 0.0f,
                              0.0f, 1.0f, 0.0f,
                              D3DDivide(miny, height), 1.0f,
                              D3DDivide(-1.0, height), &wrap)))
		goto generic_error;

    if( FAILED(wrap->Apply( (LPDIRECT3DRMOBJECT)mesh)))
		goto generic_error;
    if( FAILED(frame->AddVisual( (LPDIRECT3DRMVISUAL) mesh)))
		goto generic_error;

    SAFE_RELEASE(frame);
    SAFE_RELEASE(wrap);
    SAFE_RELEASE(mesh);
    SAFE_RELEASE(tex);
    SAFE_RELEASE(mat);
    SAFE_RELEASE(l1);
    SAFE_RELEASE(l2);
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE(mesh);
    SAFE_RELEASE(builder);
    SAFE_RELEASE(frame);
    SAFE_RELEASE(tex);
    SAFE_RELEASE(mat);
    SAFE_RELEASE(wrap);
    SAFE_RELEASE(l1);
    SAFE_RELEASE(l2);
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
    (*pstrName) = "Texture Mapping Direct3DRM Example";
}
