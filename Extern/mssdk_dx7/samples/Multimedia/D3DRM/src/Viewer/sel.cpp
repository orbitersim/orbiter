/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: sel.cpp
 *
 ***************************************************************************/

#include <math.h>
#include <d3drmwin.h>
#include "sel.h"
#include "rodcone.h"
#include "viewer.h"

static int showBoxes = FALSE;
static LPDIRECT3DRMFRAME3 sFrame = NULL;
static LPDIRECT3DRMVISUAL sVisual = NULL;
static LPDIRECT3DRMLIGHT sLight = NULL;
static LPDIRECT3DRMMESH selectionBox = NULL;
static LPDIRECT3DRMMESHBUILDER3 makeBox(D3DRMBOX*);

LPDIRECT3DRMFRAME3 clipboardFrame = NULL;
LPDIRECT3DRMVISUAL clipboardVisual = NULL;

void ShowBoxes(int show)
{
    showBoxes = show;
    SelectVisual(sVisual, sFrame);
}

int ToggleBoxes()
{
    ShowBoxes(!showBoxes);
    return showBoxes;
}

LPDIRECT3DRMFRAME3 SelectedFrame()
{
    return sFrame;
}

LPDIRECT3DRMVISUAL SelectedVisual()
{
    return sVisual;
}

LPDIRECT3DRMLIGHT SelectedLight()
{
    return sLight;
}

void DeselectVisual()
{
    if (sFrame && selectionBox)
	sFrame->DeleteVisual(selectionBox);
    sFrame = NULL;
    sVisual = NULL;
    selectionBox = NULL;
}

void SelectVisual(LPDIRECT3DRMVISUAL visual, LPDIRECT3DRMFRAME3 frame)
{
    DeselectVisual();
    sVisual = visual;
    sFrame = frame;

    if (sVisual)
    {	LPDIRECT3DRMLIGHTARRAY lights;

	sLight = 0;
	sFrame->GetLights(&lights);
	if (lights)
	{   if (lights->GetSize())
	    {	lights->GetElement(0, &sLight);
		sLight->Release(); /* reinstate reference count */
	    }
	    lights->Release();
	}

	if (showBoxes && visual)
	{   D3DRMBOX box;
	    LPDIRECT3DRMMESHBUILDER3 lpMeshVis;
	    LPDIRECT3DRMPROGRESSIVEMESH lpProgMeshVis;
	    LPDIRECT3DRMMESHBUILDER3 builder;

	    if (SUCCEEDED(visual->QueryInterface(IID_IDirect3DRMProgressiveMesh,
						 (LPVOID*) &lpProgMeshVis)))
	    {
		lpProgMeshVis->GetBox(&box);
		lpProgMeshVis->Release();
	    } else if 
		(SUCCEEDED(visual->QueryInterface(IID_IDirect3DRMMeshBuilder3,
						  (LPVOID*) &lpMeshVis)))
	    {   
		lpMeshVis->GetBox(&box);
		lpMeshVis->Release();
	    } else {
		/* Some other type of visual for which we don't support
		   showing the bounding box at the moment - just return */
		selectionBox = NULL;
		return;
	    }

	    builder = makeBox(&box);
	    builder->CreateMesh(&selectionBox);
	    sFrame->AddVisual(selectionBox);
	    selectionBox->Release();
	}
    }
}

void SelectPM(LPDIRECT3DRMPROGRESSIVEMESH lpPM)
{
    float fVal;
    HRESULT hres;
    HWND win = active_window->win;

    if (!lpPM)
    {
	if (active_window->lpPM)
	{
	    active_window->lpPM->Release();
	    active_window->lpPM = NULL;
	    EnableScrollBar(win, SB_VERT, ESB_DISABLE_BOTH);
	}
	return;
    }
    
    lpPM->AddRef();
    
    if (active_window->lpPM)
    {
	active_window->lpPM->Release();
    }

    active_window->lpPM = lpPM;

    hres = lpPM->GetDetail(&fVal);
    if (SUCCEEDED(hres))
    {
	SetScrollPos(win, SB_VERT, (DWORD)(100.0 - (DWORD)(fVal * 100.0)), TRUE);
	gfVal = fVal;
    }
    
    EnableScrollBar(win, SB_VERT, ESB_ENABLE_BOTH);
}

#define noUSE_RAYPICK
void FindAndSelectVisual(LPDIRECT3DRMVIEWPORT2 view, int x, int y)
{
    LPDIRECT3DRMVISUAL visual;
    LPDIRECT3DRMFRAME frame;
    LPDIRECT3DRMFRAMEARRAY frames;
    LPDIRECT3DRMMESHBUILDER3 mesh;

    /*
     * Make sure we don't try to select the selection box of the current
     * selection.
     */
    DeselectVisual();

#ifdef USE_RAYPICK
    LPDIRECT3DRMFRAME3 pCamera, pScene;
    LPDIRECT3DRMPICKED2ARRAY picked;
    D3DRMRAY rmRay;
    D3DRMVECTOR4D v4Src;

    view->GetCamera(&pCamera);
    pCamera->GetPosition(NULL, &rmRay.dvPos);
    pCamera->GetScene(&pScene);
    pCamera->Release();

    v4Src.x = (float)x;
    v4Src.y = (float)y;
    v4Src.z = 0.0f;
    v4Src.w = 1.0f;

    view->InverseTransform(&rmRay.dvDir, &v4Src);
    rmRay.dvDir.x -= rmRay.dvPos.x;
    rmRay.dvDir.y -= rmRay.dvPos.y;
    rmRay.dvDir.z -= rmRay.dvPos.z;

    pScene->RayPick(NULL, &rmRay, 0, &picked);
    pScene->Release();
#else
    LPDIRECT3DRMPICKEDARRAY picked;

    view->Pick(x, y, &picked);
#endif

    if (picked)
    {	if (picked->GetSize())
	{
	    LPDIRECT3DRMPROGRESSIVEMESH pm;
	    LPDIRECT3DRMFRAME3 pFrame3;

	    picked->GetPick(0, &visual, &frames, 0);
	    frames->GetElement(frames->GetSize() - 1, &frame);
	    frame->QueryInterface(IID_IDirect3DRMFrame3, (LPVOID*)&pFrame3);
		
	    if (SUCCEEDED(visual->QueryInterface(IID_IDirect3DRMProgressiveMesh,
						 (LPVOID*) &pm))) {
		SelectPM(pm);
	        SelectVisual(pm, pFrame3);
	        pm->Release();
	    } else if 
		(SUCCEEDED(visual->QueryInterface(IID_IDirect3DRMMeshBuilder3, 
						  (void **) &mesh)))
	    {   
		SelectPM(NULL);
		SelectVisual(mesh, pFrame3);
		mesh->Release();
	    } else {
		/* Some other sort of visual. Lets select it anyway */
		SelectPM(NULL);
		SelectVisual(visual, pFrame3);
	    }
	    
	    pFrame3->Release();
	    frame->Release();
	    frames->Release();
	    visual->Release();
	}
	picked->Release();
    }
}

void CutVisual()
{
    LPDIRECT3DRMFRAME3 frame;

    if (clipboardFrame)
	clipboardFrame->Release();

    if (sFrame) {
	clipboardFrame = sFrame;
	clipboardVisual = sVisual;

	DeselectVisual();

	clipboardFrame->AddRef();
	clipboardFrame->GetParent(&frame);
	if (frame) {
	    frame->DeleteChild(clipboardFrame);
	    frame->Release();
	}
    }
}

void CopyVisual()
{
    LPDIRECT3DRMFRAME3 frame;

    if (clipboardFrame)
	clipboardFrame->Release();

    if (sFrame) {
	sFrame->Clone(0, IID_IDirect3DRMFrame, (void **) &clipboardFrame);
	sVisual->Clone(0, IID_IDirect3DRMVisual, (void **) &clipboardVisual);

	clipboardFrame->AddVisual(clipboardVisual);
	clipboardVisual->Release();

	clipboardFrame->GetParent(&frame);
	if (frame) {
	    frame->DeleteChild(clipboardFrame);
	    frame->Release();
	}
    }
}

void PasteVisual(LPDIRECT3DRMFRAME3 scene)
{
    if (clipboardFrame)
    {
	LPDIRECT3DRMFRAME3 frame;
	LPDIRECT3DRMVISUAL visual;

	clipboardFrame->Clone(0, IID_IDirect3DRMFrame3, (void **) &frame);
	clipboardVisual->Clone(0, IID_IDirect3DRMVisual, (void **) &visual);

	frame->AddVisual(visual);
	scene->AddChild(frame);
	visual->Release();
	frame->Release();
    }
}

void DeleteVisual()
{
    if (sFrame)
    {
	LPDIRECT3DRMFRAME3 parent, frame = sFrame;

	DeselectVisual();
	SelectPM(NULL);
	frame->GetParent(&parent);
	parent->DeleteChild(frame);
	parent->Release();
    }
}

void ClearClipboard()
{
    if (clipboardFrame)
	clipboardFrame->Release();
}

static LPDIRECT3DRMMESHBUILDER3 makeBox(D3DRMBOX* box)
{
    LPDIRECT3DRMMESHBUILDER3 mesh;
    static D3DVECTOR zero = {D3DVAL(0.0), D3DVAL(0.0), D3DVAL(0.0)};
    static D3DVECTOR dir = {D3DVAL(0.0), D3DVAL(0.0), D3DVAL(0.0)};
    D3DVECTOR a, b;

    lpD3DRM->CreateMeshBuilder(&mesh);

    dir.z = box->max.z + D3DVAL(1.0);
    AddRod(mesh, D3DVAL(0.05), zero, dir);
    a = dir;
    a.z += D3DVAL(0.6);
    AddCone(mesh, D3DVAL(0.2), dir, a);
    a = box->min;
    b = a;
    b.y = box->max.y;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b; b.x = box->max.x;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b; b.y = box->min.y;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b; b.x = box->min.x;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b; b.z = box->max.z;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b; b.x = box->max.x;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b; b.y = box->max.y;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b; b.x = box->min.x;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b; b.y = box->min.y;
    AddRod(mesh, D3DVAL(0.05), a, b);
    b.y = box->max.y; a = b; b.z = box->min.z;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a = b = box->max; b.z = box->min.z;
    AddRod(mesh, D3DVAL(0.05), a, b);
    a.y = box->min.y; b = a; b.z = box->min.z;
    AddRod(mesh, D3DVAL(0.05), a, b);

    mesh->SetColor(D3DRMCreateColorRGB(D3DVAL(1.0), D3DVAL(1.0), D3DVAL(1.0)));
    return mesh;
}
