//-----------------------------------------------------------------------------
// File: tex4.cpp
//
// Desc: A texture mapping example using a spherical mapping relative to the
//       camera. The texture is also used as a background for the scene.
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )




//-----------------------------------------------------------------------------
// Name: ApplyWrapCallback()
// Desc: 
//-----------------------------------------------------------------------------
VOID __cdecl ApplyWrapCallback( LPDIRECT3DRMFRAME3 pFrame, VOID* pArg, 
							    D3DVALUE delta )
{
    LPDIRECT3DRMWRAP  pWrap = (LPDIRECT3DRMWRAP)pArg;
    LPDIRECT3DRMFRAME pFrame_1;
    DWORD             dwCount;
    LPDIRECT3DRMMESH  pMesh;

    // QI for the LPDIRECT3DRMFRAME interface
    if( FAILED( pFrame->QueryInterface( IID_IDirect3DRMFrame,
                                        (VOID**)&pFrame_1) ) )
	{
        MSG("Failed to QI for D3DFrame in Callback.\n" );
        return;
    }

    // Get the number of Visuals
    pFrame->GetVisuals( &dwCount, NULL );

    // Allocate space for the Visuals
    LPUNKNOWN *ppVisuals = new LPUNKNOWN[dwCount];
    pFrame->GetVisuals( &dwCount, ppVisuals);

    for( DWORD i = 0; i < dwCount; i++ )
    {
        if( SUCCEEDED( ( ppVisuals[i])->QueryInterface( IID_IDirect3DRMMesh, 
			                                            (VOID**)&pMesh ) ) )
        {
            pWrap->ApplyRelative( pFrame_1, pMesh );
            pMesh->Release();
        }

		// Release the visual after we're done with it
		ppVisuals[i]->Release();
    }

    delete [] ppVisuals;
}




//-----------------------------------------------------------------------------
// Name: CleanupWrapCallback()
// Desc: 
//-----------------------------------------------------------------------------
VOID __cdecl CleanupWrapCallback( LPDIRECT3DRMOBJECT, VOID* pArg )
{
    LPDIRECT3DRMWRAP pWrap = (LPDIRECT3DRMWRAP)pArg;
    pWrap->Release();
}




//-----------------------------------------------------------------------------
// Name: BuildScene()
// Desc: 
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM, LPDIRECT3DRMDEVICE3,
				 LPDIRECT3DRMVIEWPORT2, LPDIRECT3DRMFRAME3 pScene, 
				 LPDIRECT3DRMFRAME3 pCamera )
{
    LPDIRECT3DRMMESH         pMesh    = NULL;
    LPDIRECT3DRMFRAME3       pFrame   = NULL;
    LPDIRECT3DRMFRAME3       pAxis    = NULL;
    LPDIRECT3DRMFRAME3       pChild   = NULL;
    LPDIRECT3DRMTEXTURE3     pTex     = NULL;
    LPDIRECT3DRMWRAP         pWrap    = NULL;
    LPDIRECT3DRMLIGHT        pLight1  = NULL;
    LPDIRECT3DRMLIGHT        pLight2  = NULL;
    LPDIRECT3DRMMESHBUILDER3 pBuilder = NULL;

    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT, D3DVAL(0.2),
                                        D3DVAL(0.2), D3DVAL(0.2), &pLight1 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_DIRECTIONAL, D3DVAL(1),
                                        D3DVAL(1), D3DVAL(1), &pLight2 ) ) )
		goto generic_error;
    if( FAILED( pScene->AddLight( pLight1 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( pScene, &pFrame ) ) )
		goto generic_error;
    if( FAILED( pFrame->SetOrientation( pScene, -D3DVAL(1), -D3DVAL(1),
                                        D3DVAL(1), D3DVAL(0), D3DVAL(1),
										D3DVAL(0) ) ) )
		goto generic_error;
    if( FAILED( pFrame->AddLight( pLight2 ) ) )
		goto generic_error;
    SAFE_RELEASE( pFrame );

    if( FAILED( pD3DRM->CreateFrame( pScene, &pFrame ) ) )
		goto generic_error;
    if( FAILED( pFrame->SetPosition( pScene, D3DVAL(0), D3DVAL(0), D3DVAL(15) ) ) )
		goto generic_error;
    if( FAILED( pFrame->SetRotation( pScene, D3DVAL(1.1), D3DVAL(0.3),
                                     D3DVAL(0.0), D3DVAL(0.04) ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &pBuilder ) ) )
		goto generic_error;
    if( FAILED( pBuilder->Load( "sphere3.x", NULL, D3DRMLOAD_FROMFILE, NULL,
		                        NULL ) ) )
    {
        MSG("Failed to load sphere3.x.\n");
		goto ret_with_error;
    }
    if( FAILED( pBuilder->Scale( D3DVAL(0.2), D3DVAL(0.2), D3DVAL(0.2) ) ) )
		goto generic_error;
    if( FAILED( pBuilder->CreateMesh( &pMesh ) ) )
		goto generic_error;
    SAFE_RELEASE( pBuilder );
    if( FAILED( pFrame->AddVisual( (LPDIRECT3DRMVISUAL)pMesh ) ) )
		goto generic_error;
    SAFE_RELEASE( pMesh );

    if( FAILED( pD3DRM->CreateWrap( D3DRMWRAP_SPHERE, pCamera,
			                        D3DVAL(0.0), D3DVAL(0.0),  D3DVAL(0.0),
			                        D3DVAL(0.0), D3DVAL(1.0),  D3DVAL(0.0),
									D3DVAL(0.0), D3DVAL(0.0), -D3DVAL(1.0),
									D3DVAL(0.0), D3DVAL(0.0),
									D3DVAL(5.0), D3DVAL(5.0), &pWrap ) ) )
		goto generic_error;

    if( FAILED( pD3DRM->CreateFrame( pFrame, &pAxis ) ) )
		goto generic_error;
    if( FAILED( pAxis->SetRotation( pFrame, D3DVAL(0), D3DVAL(1), D3DVAL(0),
		                            D3DVAL(0.04) ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( pAxis, &pChild ) ) )
		goto generic_error;
    if( FAILED( pChild->SetPosition( pAxis, D3DVAL(3), D3DVAL(0), D3DVAL(0) ) ) )
		goto generic_error;
    if( FAILED( pChild->SetRotation( pAxis, D3DVAL(1), D3DVAL(1), D3DVAL(0),
                                     D3DVAL(0.04) ) ) )
		goto generic_error;

    if( FAILED( pD3DRM->LoadTexture( "lake.ppm", &pTex ) ) )
	{
		MSG("Failed to load lake.ppm.\n");
		goto ret_with_error;
    }
    if( FAILED( pTex->SetColors( 32 ) ) )
		goto generic_error;
    if( FAILED( pTex->SetDecalScale( TRUE ) ) )
		goto generic_error;
    if( FAILED( pTex->SetDecalOrigin( 128, 128 ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &pBuilder ) ) )
		goto generic_error;
    if( FAILED( pBuilder->Load( "tpot2.x", NULL, D3DRMLOAD_FROMFILE, NULL,
		                        NULL ) ) )
	{
        MSG("Failed to load tpot2.x.\n");
		goto ret_with_error;
    }

    if( FAILED( pBuilder->Scale( D3DVAL(2), D3DVAL(2), D3DVAL(2) ) ) )
	    goto generic_error;
    if( FAILED( pBuilder->SetTexture( pTex ) ) )
	    goto generic_error;
    if( FAILED( pBuilder->SetQuality( D3DRMRENDER_GOURAUD ) ) )
	    goto generic_error;
    if( FAILED( pBuilder->CreateMesh( &pMesh ) ) )
	    goto generic_error;
    SAFE_RELEASE( pBuilder );

    if( FAILED( pChild->AddVisual( pMesh ) ) )
	    goto generic_error;
    if( FAILED( pChild->AddMoveCallback( ApplyWrapCallback, (VOID*)pWrap,
                                         D3DRMCALLBACK_PREORDER ) ) )
	    goto generic_error;
    if( FAILED( pChild->AddDestroyCallback( CleanupWrapCallback, pWrap ) ) )
	    goto generic_error;
    if( FAILED( pScene->SetSceneBackgroundImage( pTex ) ) )
	    goto generic_error;

    SAFE_RELEASE( pMesh );
    SAFE_RELEASE( pFrame );
    SAFE_RELEASE( pAxis );
    SAFE_RELEASE( pChild );
    SAFE_RELEASE( pTex );
    SAFE_RELEASE( pLight1 );
    SAFE_RELEASE( pLight2 );

    // Note: don't release the wrap
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE( pMesh );
    SAFE_RELEASE( pFrame );
    SAFE_RELEASE( pAxis );
    SAFE_RELEASE( pChild );
    SAFE_RELEASE( pTex );
    SAFE_RELEASE( pWrap );
    SAFE_RELEASE( pLight1 );
    SAFE_RELEASE( pLight2 );
    SAFE_RELEASE( pBuilder );
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: OverrideDefaults()
// Desc: 
//-----------------------------------------------------------------------------
VOID OverrideDefaults( BOOL* pbNoTextures, BOOL* pbResizingDisabled, 
					   BOOL* pbConstRenderQuality, CHAR** pstrName )
{
    (*pbConstRenderQuality) = TRUE;
    (*pstrName) = "Texture Mapping IV Direct3DRM Example";
}
