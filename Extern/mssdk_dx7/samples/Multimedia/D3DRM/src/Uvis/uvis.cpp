//-----------------------------------------------------------------------------
// File: uvis.cpp
//
// Desc:
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <math.h>
#include <stdlib.h>
#include <d3d.h>
#include <d3drmwin.h>
#include "d3dmacs.h"

#define SAFE_RELEASE(x) if (x != NULL) {x->Release(); x = NULL;}
#define MSG(str) MessageBox( NULL, str, "Application Message", MB_OK )

typedef struct _Flame {
    int		valid;
    D3DVECTOR	velocity;	/* direction the flame is going */
    D3DVECTOR	orientation;	/* random vector normal to velocity */
    D3DVECTOR	position;	/* current position */
    int		time;		/* current time */
    int		lifetime;	/* time to die */
} Flame;

/*
 * A flame is a diamond shape oriented along its direction vector.
 * This updates the four vertices of the flame, given its current time.
 */
void UpdateFlame(Flame* flame, D3DVERTEX v[])
{
    D3DVECTOR dir = flame->velocity;
    D3DVECTOR off = flame->orientation;
    D3DVECTOR norm;
    D3DVECTOR start, end, left, right;
    double size;
    int i;

    D3DRMVectorNormalise(&dir);

    /*
     * Calculate a normal vector to the flame
     */
    D3DRMVectorCrossProduct(&norm, &dir, &off);
    D3DRMVectorNormalise(&norm);

    /*
     * The size starts off small, gets bigger towards the middle
     * and smaller towards the end.
     */
    if (flame->time < flame->lifetime / 2)
	size = (double) flame->time / (double)(flame->lifetime / 2);
    else
	size = ((double) (flame->lifetime - flame->time)
		/ (double)(flame->lifetime / 2));

    /*
     * Calculate the four corners of the diamond.
     */
    D3DRMVectorScale(&dir, &dir, D3DVAL(size));
    D3DRMVectorScale(&off, &off, D3DVAL(size / 4));
    start = flame->position;
    D3DRMVectorAdd(&end, &start, &dir);
    D3DRMVectorScale(&dir, &dir, D3DVAL(0.5));
    D3DRMVectorAdd(&left, &start, &dir);
    right = left;
    D3DRMVectorAdd(&left, &left, &off);
    D3DRMVectorSubtract(&right, &right, &off);


    /*
     * Update the flame's position.
     */
    D3DRMVectorAdd(&flame->position, &flame->position, &flame->velocity);
    flame->time++;
    if (flame->time == flame->lifetime)
	flame->valid = 0;

    /*
     * And fill in the vertices.  There are eight, four for each side of
     * the flame.
     */
    i = 0;
    v[i].x = start.x; v[i].y = start.y; v[i].z = start.z;
    v[i].nx = -norm.x; v[i].ny = -norm.y; v[i].nz = -norm.z;
    v[i].tu = D3DVAL(0); v[i].tv = D3DVAL(0);
    i++;

    v[i].x = left.x; v[i].y = left.y; v[i].z = left.z;
    v[i].nx = -norm.x; v[i].ny = -norm.y; v[i].nz = -norm.z;
    v[i].tu = D3DVAL(1); v[i].tv = D3DVAL(0);
    i++;

    v[i].x = end.x; v[i].y = end.y; v[i].z = end.z;
    v[i].nx = -norm.x; v[i].ny = -norm.y; v[i].nz = -norm.z;
    v[i].tu = D3DVAL(1); v[i].tv = D3DVAL(1);
    i++;

    v[i].x = right.x; v[i].y = right.y; v[i].z = right.z;
    v[i].nx = -norm.x; v[i].ny = -norm.y; v[i].nz = -norm.z;
    v[i].tu = D3DVAL(0); v[i].tv = D3DVAL(1);
    i++;

    v[i].x = start.x; v[i].y = start.y; v[i].z = start.z;
    v[i].nx = norm.x; v[i].ny = norm.y; v[i].nz = norm.z;
    v[i].tu = D3DVAL(0); v[i].tv = D3DVAL(0);
    i++;

    v[i].x = right.x; v[i].y = right.y; v[i].z = right.z;
    v[i].nx = norm.x; v[i].ny = norm.y; v[i].nz = norm.z;
    v[i].tu = D3DVAL(0); v[i].tv = D3DVAL(1);
    i++;

    v[i].x = end.x; v[i].y = end.y; v[i].z = end.z;
    v[i].nx = norm.x; v[i].ny = norm.y; v[i].nz = norm.z;
    v[i].tu = D3DVAL(1); v[i].tv = D3DVAL(1);
    i++;

    v[i].x = left.x; v[i].y = left.y; v[i].z = left.z;
    v[i].nx = norm.x; v[i].ny = norm.y; v[i].nz = norm.z;
    v[i].tu = D3DVAL(1); v[i].tv = D3DVAL(0);
    i++;

}

void InitFlame(Flame* flame)
{
    D3DVECTOR d, u;

    flame->valid = TRUE;

    do {
      D3DRMVectorRandom(&d);
      d.y = d.y * d.y;
      d.y = d.y * d.y;
    } while (d.y < D3DVAL(0.3));

    /*
     * Pick a vector normal to d
     */
    if (d.y != D3DVAL(0.0) || d.z != D3DVAL(0.0))
    {
	u.x = D3DVAL(0.0);
	if (d.y == D3DVAL(0.0))
	{
	    u.y = D3DVAL(1.0);
	    u.z = D3DVAL(0.0);
	} else {
	    D3DVALUE n_fix =
		D3DVAL(1.0)
		+ D3DDivide(D3DMultiply(d.z, d.z), D3DMultiply(d.y, d.y));
	    u.z = D3DVAL(sqrt(D3DDivide(D3DVAL(1.0), D3DVAL(n_fix))));
	    u.y = -D3DMultiply(u.z, D3DDivide(d.z, d.y));
	}
    } else {
	u.x = D3DVAL(0.0);
	u.y = D3DVAL(0.0);
	u.z = D3DVAL(1.0);
    }

    /*
     * Randomize it.
     */
    D3DRMVectorRotate(&u, &u, &d, D3DDivide(D3DVAL(6.28 * (rand() % 100)),
    					    D3DVAL(100.0)));

    D3DRMVectorScale(&d, &d, D3DVAL(0.1));
    flame->velocity = d;
    flame->orientation = u;
    flame->position.x = D3DVAL(0);
    flame->position.y = D3DVAL(0);
    flame->position.z = D3DVAL(0);
    flame->time = 0;
    do {
	flame->lifetime = rand() % 30;
    } while (flame->lifetime < 5);
}

#define MAX_FLAMES	100

typedef struct _Fire {
    Flame flames[MAX_FLAMES];
    LPDIRECT3DRMDEVICE dev;
    LPDIRECT3DEXECUTEBUFFER eb;
    LPDIRECT3DMATERIAL mat;
} Fire;

void __cdecl
CleanupFireObjects(LPDIRECT3DRMOBJECT dev, void* arg)
{
    Fire* fire = (Fire*) arg;

    if (fire->eb) {
	fire->eb->Release();
	fire->mat->Release();
	fire->eb = NULL;
	fire->dev = NULL;
    }
}

typedef struct _FireExecuteBuffer {
    D3DVERTEX		v[8 * MAX_FLAMES];
    D3DINSTRUCTION	op_state_light1;
    D3DSTATE		state1;
    D3DINSTRUCTION	op_set_status;
    D3DSTATUS		setstatus1;
    D3DINSTRUCTION	op_process_vertices1;
    D3DPROCESSVERTICES	processvertices1;
    D3DINSTRUCTION	op_state_render;
    D3DSTATE		state2;
    D3DSTATE		state3;
    D3DSTATE		state4;
    D3DINSTRUCTION	op_triangle_list;
    D3DTRIANGLE		tri[4 * MAX_FLAMES];
    D3DINSTRUCTION	exit1;
} FireExecuteBuffer;

BOOL CreateFireObjects(Fire* fire, LPDIRECT3DRMDEVICE dev)
{
    D3DEXECUTEBUFFERDESC desc;
    D3DEXECUTEDATA data;
    LPDIRECT3D lpD3D = NULL;
    LPDIRECT3DDEVICE lpD3DDev = NULL;
    LPDIRECT3DMATERIAL mat = NULL;
    LPDIRECT3DEXECUTEBUFFER eb = NULL;
    D3DMATERIALHANDLE hMat;
    D3DMATERIAL orange;
    void* p;
    int i;

    SAFE_RELEASE(fire->eb);

    dev->GetDirect3DDevice(&lpD3DDev);
    if (!lpD3DDev)
	goto generic_error;
    if (FAILED(lpD3DDev->GetDirect3D(&lpD3D)))
	goto generic_error;

    desc.dwSize = sizeof(desc);
    desc.dwFlags = D3DDEB_BUFSIZE;
    desc.dwBufferSize = sizeof(FireExecuteBuffer);
    
    if (FAILED(lpD3DDev->CreateExecuteBuffer(&desc, &eb, NULL)))
	goto generic_error;

    if (FAILED(lpD3D->CreateMaterial(&mat, NULL)))
	goto generic_error;
    if (FAILED(mat->GetHandle(lpD3DDev, &hMat)))
	goto generic_error;

    memset(&orange, 0, sizeof(orange));
    orange.dwSize = sizeof(orange);
    orange.diffuse.r = D3DVAL(1.0);
    orange.diffuse.g = D3DVAL(0.5);
    orange.diffuse.b = D3DVAL(0.0);
    orange.diffuse.a = D3DVAL(1.0);
    orange.ambient.r = D3DVAL(1.0);
    orange.ambient.g = D3DVAL(0.5);
    orange.ambient.b = D3DVAL(0.0);
    orange.ambient.a = D3DVAL(1.0);
    orange.dwRampSize = 32;
    if (FAILED(mat->SetMaterial(&orange)))
	goto generic_error;

    if (FAILED(eb->Lock(&desc)))
	goto generic_error;
    p = (void*) ((char*) desc.lpData + 8 * MAX_FLAMES * sizeof(D3DVERTEX));

    OP_STATE_LIGHT(1, p);
	STATE_DATA(D3DLIGHTSTATE_MATERIAL, hMat, p);
	
    OP_SET_STATUS(D3DSETSTATUS_ALL, D3DSTATUS_DEFAULT, 2048, 2048, 0, 0, p);
    
    OP_PROCESS_VERTICES(1, p);
	PROCESSVERTICES_DATA(D3DPROCESSVERTICES_TRANSFORMLIGHT,
			 0, 8 * MAX_FLAMES, p);
    OP_STATE_RENDER(3, p);
	STATE_DATA(D3DRENDERSTATE_SHADEMODE, D3DSHADE_FLAT, p);
	STATE_DATA(D3DRENDERSTATE_TEXTUREHANDLE, 0, p);
	STATE_DATA(D3DRENDERSTATE_BLENDENABLE, FALSE, p);

    OP_TRIANGLE_LIST(4 * MAX_FLAMES, p);
    for (i = 0; i < MAX_FLAMES; i++) {
	D3DTRIANGLE* t;
	int base;

	t = (D3DTRIANGLE*) p;
	base = 4 * i;

	t->v1 = base + 0;
	t->v2 = base + 1;
	t->v3 = base + 3;
	t->wFlags = D3DTRIFLAG_EDGEENABLETRIANGLE;
	t++;

	t->v1 = base + 1;
	t->v2 = base + 2;
	t->v3 = base + 3;
	t->wFlags = D3DTRIFLAG_EDGEENABLETRIANGLE;
	t++;
	
	t->v1 = base + 0;
	t->v2 = base + 1;
	t->v3 = base + 3;
	t->wFlags = D3DTRIFLAG_EDGEENABLETRIANGLE;
	t++;

	t->v1 = base + 1;
	t->v2 = base + 2;
	t->v3 = base + 3;
	t->wFlags = D3DTRIFLAG_EDGEENABLETRIANGLE;
	t++;
	
	p = (char*) t;
    }
    OP_EXIT(p);

    if (FAILED(eb->Unlock()))
	goto generic_error;

    data.dwSize = sizeof(data);
    data.dwVertexOffset = 0;
    data.dwVertexCount = 8 * MAX_FLAMES;
    data.dwInstructionOffset = 8 * MAX_FLAMES * sizeof(D3DVERTEX);
    data.dwInstructionLength = sizeof(FireExecuteBuffer) - data.dwInstructionOffset;
    data.dwHVertexOffset = 0;
    if (FAILED(eb->SetExecuteData(&data)))
	goto generic_error;

    fire->eb = eb;
    fire->mat = mat;
    fire->dev = dev;
    if (FAILED(dev->AddDestroyCallback(CleanupFireObjects, fire)))
	goto generic_error;

    SAFE_RELEASE(lpD3DDev);
    SAFE_RELEASE(lpD3D);
    return TRUE;
generic_error:
    SAFE_RELEASE(lpD3D);
    SAFE_RELEASE(lpD3DDev);
    SAFE_RELEASE(mat);
    SAFE_RELEASE(eb);
    return FALSE;
}

BOOL RenderFire(Fire* fire, LPDIRECT3DRMDEVICE dev, LPDIRECT3DRMVIEWPORT view)
{
    D3DVERTEX* v;
    D3DEXECUTEBUFFERDESC desc;
    D3DEXECUTEDATA data;
    LPDIRECT3DDEVICE lpD3DDev = NULL;
    LPDIRECT3DVIEWPORT lpD3DView = NULL;
    int i;

    if (fire->dev != dev) {
	if (!CreateFireObjects(fire, dev))
	    return FALSE;
    }

    dev->GetDirect3DDevice(&lpD3DDev);
    view->GetDirect3DViewport(&lpD3DView);
    if (!lpD3DDev || !lpD3DView)
	goto ret_with_error;

    for (i = 0; i < MAX_FLAMES; i++)
	if (!fire->flames[i].valid)
	    InitFlame(&fire->flames[i]);

    desc.dwSize = sizeof(desc);
    desc.dwFlags = 0;
    
    if (FAILED(fire->eb->Lock(&desc)))
	goto ret_with_error;
    v = (D3DVERTEX*) desc.lpData;

    for (i = 0; i < MAX_FLAMES; i++)
	UpdateFlame(&fire->flames[i], &v[8 * i]);

    if (FAILED(fire->eb->Unlock()))
	goto ret_with_error;

    if (FAILED(lpD3DDev->Execute(fire->eb, lpD3DView, D3DEXECUTE_CLIPPED)))
	goto ret_with_error;

    data.dwSize = sizeof data;
    if (FAILED(fire->eb->GetExecuteData(&data)))
	goto ret_with_error;
    if (FAILED(view->ForceUpdate(data.dsStatus.drExtent.x1,
		      data.dsStatus.drExtent.y1,
		      data.dsStatus.drExtent.x2,
		      data.dsStatus.drExtent.y2)))
		      goto ret_with_error;

    SAFE_RELEASE(lpD3DDev);
    SAFE_RELEASE(lpD3DView);
    return TRUE;
ret_with_error:
    SAFE_RELEASE(lpD3DDev);
    SAFE_RELEASE(lpD3DView);
    return FALSE;
}

int __cdecl FireCallback(LPDIRECT3DRMUSERVISUAL uvis,
		 void* arg,
		 D3DRMUSERVISUALREASON reason,
		 LPDIRECT3DRMDEVICE dev,
		 LPDIRECT3DRMVIEWPORT view)
{
    Fire* fire = (Fire*) arg;

    if (reason == D3DRMUSERVISUAL_CANSEE)
	return TRUE;

    if (reason == D3DRMUSERVISUAL_RENDER) {
	if (!RenderFire(fire, dev, view))
	    return DDERR_GENERIC;
	else
	    return D3D_OK;
    }

    return 0;
}

void __cdecl DestroyFire(LPDIRECT3DRMOBJECT obj, void* arg)
{
    Fire* fire = (Fire*) arg;

    if (fire->dev)
	fire->dev->DeleteDestroyCallback(CleanupFireObjects, arg);
    CleanupFireObjects(fire->dev, (void*) fire);
    free(fire);
}

LPDIRECT3DRMUSERVISUAL CreateFire(LPDIRECT3DRM3 pD3DRM)
{
    Fire* fire;
    LPDIRECT3DRMUSERVISUAL uvis = NULL;

    fire = (Fire*)malloc(sizeof(Fire));
    if (!fire)
	goto ret_with_error;
    memset(fire, 0, sizeof(Fire));

    if (FAILED(pD3DRM->CreateUserVisual(FireCallback, (void*) fire, &uvis)))
	goto ret_with_error;
    if (FAILED(uvis->AddDestroyCallback(DestroyFire, (void*) fire)))
	goto ret_with_error;   
    return uvis;
ret_with_error:
    if (fire)
	free(fire);
    SAFE_RELEASE(uvis);
    return NULL;
}

//-----------------------------------------------------------------------------
// Name: BuildScene()
// Desc: 
//-----------------------------------------------------------------------------
BOOL BuildScene( LPDIRECT3DRM3 pD3DRM,
				 LPDIRECT3DRMDEVICE3 dev, LPDIRECT3DRMVIEWPORT2 view,
				 LPDIRECT3DRMFRAME3 scene, LPDIRECT3DRMFRAME3 camera )
{
    D3DRMRENDERQUALITY quality = D3DRMRENDER_GOURAUD;
    LPDIRECT3DRMFRAME3 lights = NULL;
    LPDIRECT3DRMFRAME3 frame = NULL;
    LPDIRECT3DRMLIGHT light1 = NULL;
    LPDIRECT3DRMLIGHT light2 = NULL;
    LPDIRECT3DRMUSERVISUAL uvis = NULL;

    view = view;		/* not used */

    if (FAILED(dev->SetQuality(quality)))
	goto generic_error;

    /*
     * initialize the lights in the scene
     */
    if (FAILED(pD3DRM->CreateFrame(scene, &lights)))
	goto generic_error;
    if (FAILED(lights->SetPosition(scene, D3DVAL(5), D3DVAL(5), -D3DVAL(1))))
	goto generic_error;
    if(FAILED(lights->SetOrientation(scene, D3DVAL(0), D3DVAL(0), D3DVAL(1),
			   D3DVAL(0), D3DVAL(1), D3DVAL(1))))
			   goto generic_error;
    if(FAILED(pD3DRM->CreateLightRGB(D3DRMLIGHT_DIRECTIONAL, D3DVAL(0.9),
			  D3DVAL(0.8), D3DVAL(0.7), &light1)))
			  goto generic_error;
    if (FAILED(lights->AddLight(light1)))
	goto generic_error;
    if(FAILED(pD3DRM->CreateLightRGB(D3DRMLIGHT_AMBIENT, D3DVAL(0.1),
			  D3DVAL(0.1), D3DVAL(0.1), &light2)))
			  goto generic_error;
    if (FAILED(scene->AddLight(light2)))
	goto generic_error;

    /*
     * create a frame within the scene
     */
    if (FAILED(pD3DRM->CreateFrame(scene, &frame)))
	goto generic_error;

    /*
     * add the fire into the frame
     */
    uvis = CreateFire(pD3DRM);
    if (!uvis)
	goto generic_error;
    if (FAILED(frame->AddVisual(uvis)))
	goto generic_error;

    /*
     * set up the frames position, orientation and rotation
     */
    if (FAILED(camera->SetPosition(scene, D3DVAL(0), D3DVAL(0.5), -D3DVAL(10))))
	goto generic_error;
    if(FAILED(camera->SetOrientation(scene, D3DVAL(0), D3DVAL(0), D3DVAL(1), D3DVAL(0),
			   D3DVAL(1), D3DVAL(0))))
			   goto generic_error;
    if(FAILED(frame->SetRotation(scene, D3DVAL(0), D3DVAL(1), D3DVAL(0),
		       D3DVAL(0.02))))
		       goto generic_error;

    SAFE_RELEASE(uvis);
    SAFE_RELEASE(frame);
    SAFE_RELEASE(lights);
    SAFE_RELEASE(light1);
    SAFE_RELEASE(light2);
    return TRUE;
generic_error:
    MSG("A failure occurred while building the scene.\n");
    SAFE_RELEASE(lights);
    SAFE_RELEASE(frame);
    SAFE_RELEASE(light1);
    SAFE_RELEASE(light2);
    SAFE_RELEASE(uvis);
    return FALSE;
}

//-----------------------------------------------------------------------------
// Name: OverrideDefaults
// Desc: 
//-----------------------------------------------------------------------------
VOID OverrideDefaults( BOOL* pbNoTextures, BOOL* pbResizingDisabled, 
					   BOOL* pbConstRenderQuality, CHAR** pstrName )
{
	(*pbNoTextures) = TRUE;
	(*pbConstRenderQuality) = TRUE;
    (*pstrName)     = "User Visual Direct3DRM Example";
}
