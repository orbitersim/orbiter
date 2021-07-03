//-----------------------------------------------------------------------------
// File: morph.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <d3drmwin.h>

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )



#define NO_NORMAL { 0.0f, 0.0f, 0.0f }, 0.0f, 0.0f, 0

// cube corners
#define CFTL(v)	{ {-v,  v, -v }, NO_NORMAL }
#define CFTR(v)	{ { v,  v, -v }, NO_NORMAL }
#define CFBR(v)	{ { v, -v, -v }, NO_NORMAL }
#define CFBL(v)	{ {-v, -v, -v }, NO_NORMAL }
#define CBTL(v)	{ {-v,  v,  v }, NO_NORMAL }
#define CBTR(v)	{ { v,  v,  v }, NO_NORMAL }
#define CBBR(v)	{ { v, -v,  v }, NO_NORMAL }
#define CBBL(v)	{ {-v, -v,  v }, NO_NORMAL }

// face centers
#define FRT(v)	{ { 0.0f, 0.0f,  -v  }, NO_NORMAL }
#define BCK(v)	{ { 0.0f, 0.0f,   v  }, NO_NORMAL }
#define BTM(v)	{ { 0.0f,  -v,  0.0f }, NO_NORMAL }
#define TOP(v)	{ { 0.0f,   v,  0.0f }, NO_NORMAL }
#define LFT(v)	{ {  -v,  0.0f, 0.0f }, NO_NORMAL }
#define RGT(v)	{ {   v,  0.0f, 0.0f }, NO_NORMAL }


D3DRMVERTEX verts0[] =
{   CFTL(1.0f), CFTR(1.0f), CFBR(1.0f), CFBL(1.0f),
    CBBR(1.0f), CBTR(1.0f), CBTL(1.0f), CBBL(1.0f),
    CFBR(1.0f), CBBR(1.0f), CBBL(1.0f), CFBL(1.0f),
    CBTL(1.0f), CBTR(1.0f), CFTR(1.0f), CFTL(1.0f),
    CBTL(1.0f), CFTL(1.0f), CFBL(1.0f), CBBL(1.0f),
    CFBR(1.0f), CFTR(1.0f), CBTR(1.0f), CBBR(1.0f),
    FRT(5.0f), BCK(5.0f), BTM(5.0f), TOP(5.0f), LFT(5.0f), RGT(5.0f)
};

D3DRMVERTEX verts1[] =
{   CFTL(2), CFTR(2), CFBR(2), CFBL(2),
    CBBR(2), CBTR(2), CBTL(2), CBBL(2),
    CFBR(2), CBBR(2), CBBL(2), CFBL(2),
    CBTL(2), CBTR(2), CFTR(2), CFTL(2),
    CBTL(2), CFTL(2), CFBL(2), CBBL(2),
    CFBR(2), CFTR(2), CBTR(2), CBBR(2),
    FRT(1), BCK(1), BTM(1), TOP(1), LFT(1), RGT(1)
};

unsigned faces[] =
{    0,  1, 24,  1,  2, 24,  2,  3, 24,  3,  0, 24,
     4,  5, 25,  5,  6, 25,  6,  7, 25,  7,  4, 25,
     8,  9, 26,  9, 10, 26, 10, 11, 26, 11,  8, 26,
    12, 13, 27, 13, 14, 27, 14, 15, 27, 15, 12, 27,
    16, 17, 28, 17, 18, 28, 18, 19, 28, 19, 16, 28,
    20, 21, 29, 21, 22, 29, 22, 23, 29, 23, 20, 29
};




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
void __cdecl
DestroyInterpCallback( LPDIRECT3DRMOBJECT obj, VOID* arg )
{
    LPDIRECT3DRMINTERPOLATOR meshInterp = (LPDIRECT3DRMINTERPOLATOR)arg;

    if (meshInterp) meshInterp->Release();
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
void __cdecl
InterpolateCallback( LPDIRECT3DRMFRAME3 frame, void *arg, D3DVALUE delta )
{
    static D3DVALUE val = 0.0f;
    LPDIRECT3DRMINTERPOLATOR meshInterp = (LPDIRECT3DRMINTERPOLATOR) arg;

    meshInterp->Interpolate(val += delta, NULL, D3DRMINTERPOLATION_CLOSED | D3DRMINTERPOLATION_LINEAR);
}




//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
VOID generate_normals(D3DRMVERTEX verts[])
{
    unsigned i;
    D3DVECTOR a, b;

    for (i = 0; i < 24; i++)
    {
		D3DRMVectorSubtract(&a, &verts[faces[i * 3 + 1]].position, &verts[i].position);
		D3DRMVectorSubtract(&b, &verts[faces[i * 3 + 2]].position, &verts[i].position);
		D3DRMVectorCrossProduct(&verts[i].normal, &a, &b);
		D3DRMVectorNormalize(&verts[i].normal);
    }
}


#define D3DTRY(x) if (FAILED(x)) goto generic_error
#define CREATE(type, obj) CreateObject(CLSID_C##type, NULL, IID_I##type, (void **) obj)
#define CREATE3(type, obj) CreateObject(CLSID_C##type, NULL, IID_I##type##3, (void **) obj)



//-----------------------------------------------------------------------------
// Name:
// Desc:
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM, 
				 LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2 view,
	             LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera)
{
    LPDIRECT3DRMMESH mesh = NULL, meshProxy = NULL;
    LPDIRECT3DRMFRAME3 frame = NULL, proxyFrame = NULL;
    LPDIRECT3DRMFRAME3 axis = NULL;
    LPDIRECT3DRMFRAME3 orbit = NULL;
    LPDIRECT3DRMLIGHT light1 = NULL;
    LPDIRECT3DRMLIGHT light2 = NULL;
    LPDIRECT3DRMINTERPOLATOR meshInterp = NULL;

    dev, view, camera;

    // generate flat-shading normals for faces
    generate_normals(verts0);
    generate_normals(verts1);

    D3DTRY( pD3DRM->CreateObject( CLSID_CDirect3DRMMeshInterpolator, NULL, 
				                  IID_IDirect3DRMInterpolator,
								  (VOID**)&meshInterp));
    D3DTRY( meshInterp->QueryInterface(IID_IDirect3DRMMesh, (void **) &meshProxy));

    D3DTRY( meshInterp->SetIndex(D3DVAL(0)));
    D3DTRY( meshProxy->SetVertices(0, 0, 30, verts1));
    D3DTRY( meshProxy->SetGroupColorRGB(0, D3DVAL(0.5), D3DVAL(0.4), D3DVAL(0.7)));

    D3DTRY( meshInterp->SetIndex(D3DVAL(25)));
    D3DTRY( meshProxy->SetVertices(0, 0, 30, verts0));
    D3DTRY( meshProxy->SetGroupColorRGB(0, D3DVAL(1), D3DVAL(0.0), D3DVAL(0.3)));

    D3DTRY( meshInterp->SetIndex(D3DVAL(40)));
    D3DTRY( meshProxy->SetGroupColorRGB(0, D3DVAL(0.6), D3DVAL(0.6), D3DVAL(0.6)));

    D3DTRY( meshInterp->SetIndex(D3DVAL(75)));
    meshProxy->Release();

    D3DTRY( pD3DRM->CREATE(Direct3DRMLight, &light1));
    D3DTRY( light1->SetType(D3DRMLIGHT_AMBIENT));
    D3DTRY( light1->SetColorRGB(D3DVAL(0.2), D3DVAL(0.2), D3DVAL(0.2)));

    D3DTRY( pD3DRM->CREATE(Direct3DRMLight, &light2));
    D3DTRY( light2->SetType(D3DRMLIGHT_DIRECTIONAL));
    D3DTRY( light2->SetColorRGB(D3DVAL(1), D3DVAL(1), D3DVAL(1)));

    D3DTRY( scene->AddLight(light1));
    D3DTRY( scene->SetSceneBackgroundRGB(D3DVAL(0.3), D3DVAL(0.6), D3DVAL(0.3)));

    D3DTRY( pD3DRM->CREATE3(Direct3DRMFrame, &frame));
    D3DTRY( scene->AddChild(frame));
    D3DTRY( frame->SetOrientation(scene, D3DVAL(0.5), -D3DVAL(0.5), D3DVAL(1),
				 D3DVAL(0), D3DVAL(1), D3DVAL(0)));

    D3DTRY( frame->AddLight(light2));
    frame->Release();

    D3DTRY( pD3DRM->CREATE3(Direct3DRMFrame, &frame));
    D3DTRY( scene->AddChild(frame));
    D3DTRY( frame->SetPosition(scene, D3DVAL(0), D3DVAL(0), D3DVAL(15)));
    D3DTRY( frame->SetOrientation(scene, D3DVAL(0), D3DVAL(1.0), D3DVAL(0),
    					D3DVAL(0), D3DVAL(0), D3DVAL(1)));
    D3DTRY( frame->SetRotation(scene, D3DVAL(0), D3DVAL(0.9), D3DVAL(1.0), D3DVAL(0.04)));

    D3DTRY( pD3DRM->CREATE(Direct3DRMMesh, &mesh));
    D3DTRY( mesh->AddGroup(30, 24, 3, faces, NULL));
    D3DTRY( mesh->SetGroupQuality(0, D3DRMRENDER_FLAT));

    D3DTRY( meshInterp->AttachObject(mesh));
    D3DTRY( frame->AddVisual(mesh));

    D3DTRY( frame->AddMoveCallback( InterpolateCallback, meshInterp,
                                    D3DRMCALLBACK_PREORDER ));
    D3DTRY( frame->AddDestroyCallback( DestroyInterpCallback, meshInterp));

    D3DTRY( pD3DRM->CREATE3(Direct3DRMFrame, &axis));
    D3DTRY( frame->AddChild(axis));
    D3DTRY( axis->SetRotation(frame, 0.0f, 1.0f, 0.0f, 0.04f ) );

    D3DTRY( pD3DRM->CREATE3(Direct3DRMFrame, &orbit));
    D3DTRY(axis->AddChild(orbit));
    D3DTRY(orbit->SetPosition(axis, 2.6f, 0.0f, 0.0f ));

    SAFE_RELEASE(mesh);
    SAFE_RELEASE(frame);
    SAFE_RELEASE(axis);
    SAFE_RELEASE(orbit);
    SAFE_RELEASE(light1);
    SAFE_RELEASE(light2);
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
    SAFE_RELEASE(mesh);
    SAFE_RELEASE(frame);
    SAFE_RELEASE(axis);
    SAFE_RELEASE(orbit);
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
    (*pbConstRenderQuality) = TRUE;
    (*pstrName) = "Interpolator Direct3DRM Example";

}
