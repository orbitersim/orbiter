//-----------------------------------------------------------------------------
// File: fly.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>
#include <math.h>
#include <stdlib.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )



struct PathInfo
{
    FLOAT                  fTime;
    LPDIRECT3DRMFRAME3     pChaseFrame;
    LPDIRECT3DRMFRAME3     pPlaneFrame;
    LPDIRECT3DRMANIMATION2 pFlightPath;
};

#define NUM_SMOKE_TRAILS 7
DWORD dwNumSmokeTrails = 0;
DWORD dwNumTrailsDone = 0;

LPDIRECT3DRMFRAME3 apSmokeFrame[NUM_SMOKE_TRAILS];




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
void __cdecl
CleanupObjectsCallback( LPDIRECT3DRMOBJECT pObj, VOID* pArg )
{
    PathInfo* pInfo = (PathInfo*)pArg;

    for( int i=0; i<NUM_SMOKE_TRAILS; i++ )
		apSmokeFrame[i]->Release();

    pInfo->pChaseFrame->Release();
    pInfo->pPlaneFrame->Release();
    pInfo->pFlightPath->Release();
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
void __cdecl
MoveCameraCallback( LPDIRECT3DRMFRAME3 pCamera, VOID* pArg, D3DVALUE delta )
{
    PathInfo* pInfo = (PathInfo*)pArg;

    D3DVECTOR dir, up;
    D3DVECTOR dirCam, upCam;
    LPDIRECT3DRMFRAME3 pScene;
    D3DVALUE a_bit;

    pCamera->GetScene( &pScene );
    pInfo->fTime += 0.04f;
    pInfo->pFlightPath->SetFrame( pCamera );
    pInfo->pFlightPath->SetTime( pInfo->fTime );

    pInfo->pFlightPath->SetFrame( pInfo->pPlaneFrame );
    pInfo->pFlightPath->SetTime( pInfo->fTime + 0.5f );

    pInfo->pFlightPath->SetFrame( pInfo->pChaseFrame );
    pInfo->pFlightPath->SetTime( pInfo->fTime + 1.0f );

    pCamera->LookAt( pInfo->pPlaneFrame, pScene, D3DRMCONSTRAIN_Z);
    pInfo->pPlaneFrame->LookAt( pInfo->pChaseFrame, pScene,
                                      D3DRMCONSTRAIN_Y);
    pCamera->GetOrientation( pScene, &dirCam, &upCam);
    pInfo->pPlaneFrame->GetOrientation( pScene, &dir, &up);
    up.x = dir.x - dirCam.x;
    up.y = dir.y - dirCam.y + 1.0f;
    up.z = dir.z - dirCam.z;
                
    pInfo->pPlaneFrame->SetOrientation( pScene, dir.x, dir.y, dir.z,
					                    up.x, up.y, up.z );

    if( dwNumTrailsDone < NUM_SMOKE_TRAILS )
	{
		pScene->AddVisual( (LPDIRECT3DRMVISUAL)apSmokeFrame[dwNumSmokeTrails]);
		dwNumTrailsDone++;
    } 
	else 
	{
		if( dwNumSmokeTrails == NUM_SMOKE_TRAILS )
			dwNumSmokeTrails = 0;
    }
    a_bit = D3DDivide(D3DDivide(D3DVAL(dwNumSmokeTrails), D3DVAL(NUM_SMOKE_TRAILS)), 10.0f);
    pInfo->pFlightPath->SetFrame( apSmokeFrame[dwNumSmokeTrails]);
    pInfo->pFlightPath->SetTime( pInfo->fTime + 0.4f - a_bit );
    apSmokeFrame[dwNumSmokeTrails]->SetOrientation( pScene, dir.x, dir.y, dir.z,
						              up.x, up.y, up.z);
    dwNumSmokeTrails++;
    pScene->Release();
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM,
				 LPDIRECT3DRMDEVICE3 pDevice, LPDIRECT3DRMVIEWPORT2 pViewport,
	             LPDIRECT3DRMFRAME3 pScene, LPDIRECT3DRMFRAME3 pCamera )
{
    LPDIRECT3DRMFRAME3 lights = NULL;
    D3DRMBOX box;
    LPDIRECT3DRMMESHBUILDER3 plane_builder = NULL;
    LPDIRECT3DRMMESHBUILDER3 mesh_builder = NULL;
    LPDIRECT3DRMMESHBUILDER3 smoke_builder = NULL;
    LPDIRECT3DRMMESH plane = NULL;
    LPDIRECT3DRMMESH mesh = NULL;
    LPDIRECT3DRMMESH smokemesh = NULL;
    LPDIRECT3DRMLIGHT ambient = NULL;
    LPDIRECT3DRMLIGHT parallel = NULL;
    D3DCOLOR smokec;
    LPDIRECT3DRMFRAME3 frame = NULL;
    LPDIRECT3DRMFRAME3 sl = NULL;
    LPDIRECT3DRMFRAME3 sr = NULL;
    int i;
    int numPts = 11;
    D3DVECTOR path[] = { -8.0f,  3.0f, -12.0f,   -4.0f,  2.0f,  -8.0f,
	                     -2.0f,  0.0f,  -4.0f,    9.0f, -1.0f,   7.0f,
	                      4.0f,  6.0f,  10.0f,   -4.0f,  5.0f,   9.0f,
	                      5.5f,  3.5f,  -6.5f,    2.0f,  5.0f, -10.0f,
	                      0.0f,  4.0f, -15.0f,   -5.0f,  4.0f, -15.0f,
	                     -8.0f,  3.0f, -12.0f };
    
	D3DVALUE path_t[] = { 0.0f, 1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f,
		                  8.0f, 9.0f, 10.0f };

    static PathInfo info;

    if( FAILED( pViewport->SetField( 0.8f ) ) )
		goto generic_error;
    if( FAILED( pDevice->SetQuality( D3DRMRENDER_GOURAUD ) ) )
		goto generic_error;
#ifdef FOG
    if( FAILED( pDevice->SetDither( TRUE ) ) )
		goto generic_error;
    if( FAILED( pScene->SetFogEnable( TRUE ) ) )
		goto generic_error;
    if( FAILED( pScene->SetFogParams( 1, 30, 1 ) ) )
		goto generic_error;
#endif

    // This Demo flies a plane through a small landscape, followed by a
    // camera. The paths are spline curves.

    // Initialize smoke trail
    smokec = D3DRMCreateColorRGBA(0.6f, 0.6f, 0.6f, 0.5f );
    if( FAILED( pD3DRM->CreateMeshBuilder( &smoke_builder ) ) )
		goto generic_error;
    if( FAILED( smoke_builder->Load( "sphere0.x", NULL, D3DRMLOAD_FROMFILE,
		                             NULL, NULL ) ) )
	{
		MSG("Failed to load sphere0.x.\n" );
		goto ret_with_error;
    }
    
	if( FAILED( smoke_builder->Scale( 0.015f, 0.015f, 0.015f ) ) )
		goto generic_error;

    if( FAILED( smoke_builder->CreateMesh( &smokemesh ) ) )
		goto generic_error;
    for( i=0; i<NUM_SMOKE_TRAILS; i++ )
	{
		if( FAILED( pD3DRM->CreateFrame( pScene, &apSmokeFrame[i] ) ) )
			goto generic_error;
		if( FAILED( pD3DRM->CreateFrame( apSmokeFrame[i], &sl ) ) )
			goto generic_error;
		if( FAILED( pD3DRM->CreateFrame( apSmokeFrame[i], &sr ) ) )
			goto generic_error;

		if( FAILED( sl->AddVisual( (LPDIRECT3DRMVISUAL)smokemesh ) ) )
			goto generic_error;
		if( FAILED( sr->AddVisual( (LPDIRECT3DRMVISUAL)smokemesh ) ) )
			goto generic_error;
		if( FAILED( sr->SetPosition( apSmokeFrame[i], -0.1f, 0.0f, 0.0f ) ) )
			goto generic_error;
		if (FAILED( apSmokeFrame[i]->SetMaterialMode( D3DRMMATERIAL_FROMFRAME ) ) )
			goto generic_error;
		if (FAILED( apSmokeFrame[i]->SetColor( smokec ) ) )
			goto generic_error;
		if (FAILED( sl->SetMaterialMode( D3DRMMATERIAL_FROMPARENT ) ) )
			goto generic_error;
		if (FAILED( sr->SetMaterialMode( D3DRMMATERIAL_FROMPARENT ) ) )
			goto generic_error;
		SAFE_RELEASE(sl);
		SAFE_RELEASE(sr);
    }

	// initialize the lights in the scene
    if( FAILED( pD3DRM->CreateFrame( pScene, &lights ) ) )
		goto generic_error;
    if( FAILED( lights->SetPosition( pScene, 5.0f, 5.0f, -5.0f ) ) )
		goto generic_error;
                                
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_PARALLELPOINT,
		                                  0.8f, 0.6f, 0.7f, &parallel ) ) )
		goto generic_error;
                                  
    if( FAILED( lights->AddLight( parallel ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateLightRGB( D3DRMLIGHT_AMBIENT,
		                                  0.1f, 0.1f, 0.1f, &ambient ) ) )
		goto generic_error;
    if( FAILED( pScene->AddLight( ambient ) ) )
		goto generic_error;

    // load mesh file
    if( FAILED( pD3DRM->CreateFrame( pScene, &frame ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateMeshBuilder( &mesh_builder ) ) )
		goto generic_error;
    if( FAILED( mesh_builder->Load( "land4.x", NULL, D3DRMLOAD_FROMFILE,
		                            NULL, NULL ) ) )
		goto generic_error;
    if( FAILED( mesh_builder->Scale( 10.0f, 8.0f, 10.0f ) ) )
		goto generic_error;
    if( FAILED( mesh_builder->GetBox( &box ) ) )
		goto generic_error;

    // Color the landscape's faces.
    if( mesh_builder )
	{
		LPDIRECT3DRMFACEARRAY faces;
		LPDIRECT3DRMFACE this_face;
		int face_count, vertex_count;
		D3DVALUE range, height;
		D3DVECTOR *coords;

		if( FAILED(mesh_builder->GetFaces( &faces ) ) )
			goto generic_error;
		face_count = faces->GetSize();
			
		range = box.max.y - box.min.y;

		// color the faces according to the height
		for( int i=0; i<face_count; i++ )
		{
			DWORD dwVertexCount;

			faces->GetElement( i, &this_face);
			dwVertexCount = this_face->GetVertexCount();
			coords = (LPD3DVECTOR) malloc( dwVertexCount * sizeof(D3DVECTOR));

			this_face->GetVertices( &dwVertexCount, coords, NULL);
			vertex_count=dwVertexCount;
			
			if( dwVertexCount )
			{
				// find maximum height of the face
				height = coords[0].y;
				for( DWORD j=1; j<dwVertexCount; j++ )
				{
					if( coords[j].y > height )
						height = coords[j].y;
				}
				height = D3DDivide((height - box.min.y), range);

				if( height < 0.03f )     // water
					this_face->SetColorRGB( 0.20f, 0.20f, 0.50f );
				else if( height < 0.3f ) // greenery
					this_face->SetColorRGB( 0.10f, 0.80f, 0.10f );
				else if( height < 0.5f ) // rocks
					this_face->SetColorRGB( 0.60f, 0.30f, 0.30f );
				else if( height < 0.7f ) // dirty snow
					this_face->SetColorRGB( 0.80f, 0.65f, 0.65f );
				else                     // snow 
					this_face->SetColorRGB( 1.00f, 1.00f, 1.00f );
			}
			free(coords);
			SAFE_RELEASE(this_face);
		}
		SAFE_RELEASE(faces);
    }
    
	if( FAILED( mesh_builder->CreateMesh( &mesh ) ) )
		goto generic_error;
    if( FAILED( frame->AddVisual( (LPDIRECT3DRMVISUAL)mesh ) ) )
		goto generic_error;

    if( FAILED( pD3DRM->CreateMeshBuilder( &plane_builder ) ) )
		goto generic_error;
    
	if( FAILED( plane_builder->Load( "dropship.x", NULL, D3DRMLOAD_FROMFILE,
		                             NULL, NULL ) ) )
	{
        MSG("Failed to load dropship.x.\n" );
		goto ret_with_error;
    }
    if( FAILED( plane_builder->Scale( 0.015f, 0.008f, 0.015f ) ) )
		goto generic_error;
    if( FAILED( plane_builder->CreateMesh( &plane ) ) )
		goto generic_error;

    if( FAILED( pD3DRM->CreateAnimation( &info.pFlightPath ) ) )
		goto generic_error;
    info.pFlightPath->SetOptions( D3DRMANIMATION_CLOSED
    					 | D3DRMANIMATION_SPLINEPOSITION
					 | D3DRMANIMATION_POSITION);
    for( i=0; i<numPts; i++ )
		info.pFlightPath->AddPositionKey( path_t[i], path[i].x, path[i].y, path[i].z);

    info.fTime = 0.0f;
    if( FAILED( pD3DRM->CreateFrame( pScene, &info.pChaseFrame ) ) )
		goto generic_error;
    if( FAILED( pD3DRM->CreateFrame( pScene, &info.pPlaneFrame ) ) )
		goto generic_error;
    if( FAILED( info.pPlaneFrame->AddVisual( (LPDIRECT3DRMVISUAL)plane ) ) )
		goto generic_error;

    if( FAILED( pCamera->AddMoveCallback( MoveCameraCallback, (VOID*)&info,
                                          D3DRMCALLBACK_PREORDER ) ) )
		goto generic_error;
    if( FAILED( pCamera->AddDestroyCallback( CleanupObjectsCallback, &info ) ) )
		goto generic_error;

    SAFE_RELEASE( lights );
    SAFE_RELEASE( plane_builder );
    SAFE_RELEASE( mesh_builder );
    SAFE_RELEASE( smoke_builder );
    SAFE_RELEASE( plane );
    SAFE_RELEASE( mesh );
    SAFE_RELEASE( smokemesh );
    SAFE_RELEASE( ambient );
    SAFE_RELEASE( parallel );
    SAFE_RELEASE( frame );
    return TRUE;

generic_error:
    MSG("A failure has occurred while building the scene.\n");
ret_with_error:
    SAFE_RELEASE( lights );
    SAFE_RELEASE( plane_builder );
    SAFE_RELEASE( mesh_builder );
    SAFE_RELEASE( smoke_builder );
    SAFE_RELEASE( plane );
    SAFE_RELEASE( mesh );
    SAFE_RELEASE( smokemesh );
    SAFE_RELEASE( ambient );
    SAFE_RELEASE( parallel );
    SAFE_RELEASE( frame );
    SAFE_RELEASE( sl );
    SAFE_RELEASE( sr );
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
    (*pstrName)     = "Fly Full-Screen Direct3DRM Example";
}
