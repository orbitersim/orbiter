//-----------------------------------------------------------------------------
// File: loadfile.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )



//-----------------------------------------------------------------------------
// Name: BuildScene()
// Desc: Initializes the scene.
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM,
				 LPDIRECT3DRMDEVICE3 pDevice, LPDIRECT3DRMVIEWPORT2,
				 LPDIRECT3DRMFRAME3 pScene, LPDIRECT3DRMFRAME3 pCamera )
{
    LPDIRECT3DRMFRAME3       pLightsFrame = NULL;
    LPDIRECT3DRMMESHBUILDER3 pMeshBuilder = NULL;
    LPDIRECT3DRMFRAME3       pMeshFrame   = NULL;
    LPDIRECT3DRMLIGHT       pLight1      = NULL;
    LPDIRECT3DRMLIGHT       pLight2      = NULL;

    // Add some lights to the scene
    if( FAILED( pD3DRM->CreateFrame( pScene, &pLightsFrame ) ) )
		goto generic_error;
    if( FAILED( pLightsFrame->SetPosition( pScene, 5.0f, 5.0f, -1.0f ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_DIRECTIONAL, 0.9f,
                                  0.8f, 0.7f, &pLight1 ) ) )
		goto generic_error;
    if( FAILED( pLightsFrame->AddLight( pLight1 ) ) )
	    goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT, 0.1f,
                                  0.1f, 0.1f, &pLight2 ) ) )
		goto generic_error;
    if( FAILED( pScene->AddLight( pLight2 ) ) )
		goto generic_error;

    // Load mesh file
    if( FAILED( pD3DRM->CreateMeshBuilder( &pMeshBuilder ) ) )
		goto generic_error;
    if( FAILED( pMeshBuilder->Load( "tiger.x", NULL, D3DRMLOAD_FROMFILE,
		                            NULL, NULL ) ) )
	{
        MSG("Failed to load tiger.x\n" );
		goto ret_with_error;
    }

    // Create a frame for the mesh within the scene
    if( FAILED( pD3DRM->CreateFrame( pScene, &pMeshFrame ) ) )
		goto generic_error;

    // Add the loaded mesh into the frame
    if( FAILED( pMeshFrame->AddVisual( (LPDIRECT3DRMVISUAL)pMeshBuilder ) ) )
		goto generic_error;

    // Set up the frames position, orientation and rotation
    pCamera->SetPosition( pScene, 0.0f, 1.0f, -5.0f );
    pCamera->SetOrientation( pScene, 0.0f, -0.2f, 1.0f, 0.0f, 1.0f, 0.0f );
    pMeshFrame->SetRotation( pScene, 0.0f, 1.0f, 0.0f, 0.02f );

    SAFE_RELEASE( pMeshFrame );
    SAFE_RELEASE( pLightsFrame );
    SAFE_RELEASE( pMeshBuilder );
    SAFE_RELEASE( pLight1 );
    SAFE_RELEASE( pLight2 );
    return TRUE;

generic_error:
    MSG("A failure occured while building the scene.\n");
ret_with_error:
    SAFE_RELEASE( pMeshFrame );
    SAFE_RELEASE( pLightsFrame );
    SAFE_RELEASE( pMeshBuilder );
    SAFE_RELEASE( pLight1 );
    SAFE_RELEASE( pLight2 );
    return FALSE;
}




//-----------------------------------------------------------------------------
// Name: OverrideDefualts()
// Desc: Let's the sample customize default behavior.
//-----------------------------------------------------------------------------
VOID OverrideDefaults( BOOL* pbNoTextures, BOOL* pbResizingDisabled, 
					   BOOL* pbConstRenderQuality, CHAR** pstrName )
{
	(*pbNoTextures) = TRUE;
    (*pstrName)     = "FileLoad: D3DRM File Loading Sample";
}
