//-----------------------------------------------------------------------------
// File: quat.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )
#define PI2 6.28318f
#define XPOS 2.0f
#define INTERPOLATE_STEP 0.05f

static D3DRMQUATERNION q;

static int view_width, view_height;

VOID ReadMouse( WORD* pButtons, WORD* pX, WORD* pY );




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
void __cdecl
UserControl(LPDIRECT3DRMFRAME3 frame, void* arg, D3DVALUE delta)
{
    WORD mx, my, mb;
    LPDIRECT3DRMFRAME3 scene;
    D3DVALUE angle;
    D3DVECTOR vx = { 1.0f, 0.0f, 0.0f };
    D3DVECTOR vy = { 0.0f, 1.0f, 0.0f };
    D3DRMQUATERNION qx, qy;
    D3DRMMATRIX4D mat;

    arg = arg;

    ReadMouse(&mb, &mx, &my);
    angle = D3DMultiply(
                 ( -0.5f + D3DVAL(D3DDivide(D3DVAL(my), D3DVAL(view_height)))), 
                 PI2 );
    D3DRMQuaternionFromRotation(&qx, &vx, angle);
    angle = D3DMultiply(
                 ( -0.5f + D3DVAL(D3DDivide(D3DVAL(mx), D3DVAL(view_width)))),
                 PI2 );
    D3DRMQuaternionFromRotation(&qy, &vy, angle);
    D3DRMQuaternionMultiply(&q, &qx, &qy);
    D3DRMMatrixFromQuaternion(mat, &q);
    frame->AddTransform( D3DRMCOMBINE_REPLACE, mat);
    frame->GetScene( &scene);
    frame->SetPosition( scene, D3DVAL(-XPOS), D3DVAL(0),
                               D3DVAL(0));

    SAFE_RELEASE(scene);
}



//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
void __cdecl
Follow( LPDIRECT3DRMFRAME3 frame, VOID*, D3DVALUE delta )
{
    LPDIRECT3DRMFRAME3 scene;
    WORD mx, my, mb;
    static D3DRMQUATERNION qstart = { 1.0f, { 0.0f, 0.0f, 0.0f } };
    static D3DRMQUATERNION qend;
    static D3DVALUE alpha = 0.0f;

    ReadMouse(&mb, &mx, &my);
    if (mb && alpha == 0.0f )
	{
		qend = q;
		alpha = D3DVAL(INTERPOLATE_STEP);
    }
    if (alpha > 0.0f ) 
	{
		D3DRMQUATERNION interp;
		D3DRMMATRIX4D mat;

		D3DRMQuaternionSlerp(&interp, &qstart, &qend, alpha);
		D3DRMMatrixFromQuaternion(mat, &interp);

		if (alpha >= 1.0f )
		{
			D3DRMMatrixFromQuaternion(mat, &qend);
			alpha  = 0.0f;
			qstart = qend;
		} 
		else
			alpha += D3DVAL(INTERPOLATE_STEP);

		frame->AddTransform( D3DRMCOMBINE_REPLACE, mat);
		frame->GetScene( &scene);
		frame->SetPosition( scene, D3DVAL(XPOS), 0.0f, 0.0f );
		SAFE_RELEASE(scene);
    }
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM, 
				 LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2 view,
	             LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera )
{
    D3DRMRENDERQUALITY quality = D3DRMRENDER_FLAT;
    LPDIRECT3DRMFRAME3 lights = NULL;
    LPDIRECT3DRMFRAME3 user = NULL;
    LPDIRECT3DRMFRAME3 follower = NULL;
    LPDIRECT3DRMMESHBUILDER3 builder = NULL;
    LPDIRECT3DRMMESH mesh = NULL;
    LPDIRECT3DRMLIGHT l1 = NULL;
    LPDIRECT3DRMLIGHT l2 = NULL;


    if( FAILED( dev->SetQuality( quality ) ) )
		goto generic_error;

    view_width = view->GetWidth();
    view_height = view->GetHeight();

    // initialize the lights in the scene
    if( FAILED( pD3DRM->CreateFrame( scene, &lights ) ) )
		goto generic_error;
    if( FAILED( lights->SetOrientation( scene, -1.0f, -1.0f, 0.0f, 0.0f, 1.0f, 0.0f ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_DIRECTIONAL, 
										0.8f, 0.6f, 0.7f, &l1 ) ) )
		goto generic_error;
    if( FAILED( lights->AddLight( l1 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT,
										0.1f, 0.1f, 0.1f, &l2 ) ) )
		goto generic_error;
    if( FAILED( scene->AddLight( l2 ) ) )
		goto generic_error;

    if( FAILED( pD3DRM->CreateMeshBuilder( &builder ) ) )
		goto generic_error;
    if( FAILED( builder->Load( "dropship.x", NULL, D3DRMLOAD_FROMFILE,
		                       NULL, NULL) ) )
	{
        MSG("Failed to load dropship.x.\n" );
		goto ret_with_error;
    }
    if( FAILED( builder->Scale( 0.1f, 0.08f, 0.1f ) ) )
		goto generic_error;
    if( FAILED( builder->CreateMesh( &mesh ) ) )
		goto generic_error;
    SAFE_RELEASE(builder);

    if( FAILED( camera->SetPosition( scene, 0.0f, 0.0f, -12.0f ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( scene, &user ) ) )
		goto generic_error;
    if( FAILED( user->AddVisual( (LPDIRECT3DRMVISUAL) mesh ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( scene, &follower ) ) )
		goto generic_error;
    if( FAILED( follower->AddVisual( (LPDIRECT3DRMVISUAL) mesh ) ) )
		goto generic_error;

    if( FAILED( follower->SetPosition( scene, D3DVAL(XPOS), D3DVAL(0),
                                  D3DVAL(0))))
		goto generic_error;

    if( FAILED( user->AddMoveCallback( UserControl, NULL,
                                       D3DRMCALLBACK_PREORDER)))
		goto generic_error;
    if( FAILED( follower->AddMoveCallback( Follow, NULL,
                                           D3DRMCALLBACK_PREORDER)))
		goto generic_error;

    SAFE_RELEASE(lights);
    SAFE_RELEASE(user);
    SAFE_RELEASE(follower);
    SAFE_RELEASE(mesh);
    SAFE_RELEASE(l1);
    SAFE_RELEASE(l2);
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE(lights);
    SAFE_RELEASE(user);
    SAFE_RELEASE(follower);
    SAFE_RELEASE(builder);
    SAFE_RELEASE(mesh);
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
    (*pbNoTextures) = TRUE;
    (*pbConstRenderQuality) = TRUE;
    (*pstrName) = "Quaternion Direct3DRM Example";
}
