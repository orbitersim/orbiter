// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// hudbutton.cpp
// User interface for HUD button controls
// ==============================================================

#include "ShuttleA.h"
#include "hudbutton.h"

#define STRICT 1

// panel coordinates
static const float bb_x0 = (float)(HUDBTN_X+44);
static const float bb_y0 =  16.5f;
static const float bb_dx =  44.0f;
// texture coordinates
static const float texw = (float)PANEL2D_TEXW;
static const float texh = (float)PANEL2D_TEXH;
static const float tx_x0 = texw-297.0f;
static const float tx_y0 = texh- 64.5f;
static const float tx_dx =  42.0f;
static const float tx_dy =  32.0f;

HUDButton::HUDButton (VESSEL3 *v): PanelElement (v)
{
}

// ==============================================================

void HUDButton::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	const DWORD nvtx = 4*3;
	const DWORD nidx = 6*3;
	static NTVERTEX vtx[nvtx] = {
		// "Orbit" button
		{bb_x0,                 bb_y0,      0,  0,0,0,  tx_x0/texw,             tx_y0/texh},
		{bb_x0+tx_dx,           bb_y0,      0,  0,0,0,  (tx_x0+tx_dx)/texw,     tx_y0/texh},
		{bb_x0,                 bb_y0+tx_dy,0,  0,0,0,  tx_x0/texw,             tx_y0/texh},
		{bb_x0+tx_dx,           bb_y0+tx_dy,0,  0,0,0,  (tx_x0+tx_dx)/texw,     tx_y0/texh},
		// "Surface" button
		{bb_x0+bb_dx,           bb_y0,      0,  0,0,0,  (tx_x0+1.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+bb_dx+tx_dx,     bb_y0,      0,  0,0,0,  (tx_x0+2.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+bb_dx,           bb_y0+tx_dy,0,  0,0,0,  (tx_x0+1.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+bb_dx+tx_dx,     bb_y0+tx_dy,0,  0,0,0,  (tx_x0+2.0f*tx_dx)/texw,tx_y0/texh},
		// "Dock" button
		{bb_x0+2.0f*bb_dx,      bb_y0,      0,  0,0,0,  (tx_x0+2.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+2.0f*bb_dx+tx_dx,bb_y0,      0,  0,0,0,  (tx_x0+3.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+2.0f*bb_dx,      bb_y0+tx_dy,0,  0,0,0,  (tx_x0+2.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+2.0f*bb_dx+tx_dx,bb_y0+tx_dy,0,  0,0,0,  (tx_x0+3.0f*tx_dx)/texw,tx_y0/texh}
	};
	static WORD idx[nidx] = {
		0,1,2, 3,2,1,
		4,5,6, 7,6,5,
		8,9,10, 11,10,9
	};

	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
}

// ==============================================================

bool HUDButton::Redraw2D (SURFHANDLE surf)
{
	static const float tv_idle = tx_y0/texh;
	static const float tv_active = (tx_y0+tx_dy)/texh;
	int i, vofs, mode = oapiGetHUDMode();
	float tv;
	for (i = 0; i < 3; i++) {
		vofs = vtxofs + i*4;
		tv = (i+1 == mode ? tv_active : tv_idle);
		grp->Vtx[vofs+2].tv = grp->Vtx[vofs+3].tv = tv;
	}
	return false;
}

// ==============================================================

bool HUDButton::ProcessMouse2D (int event, int mx, int my)
{
	if (mx%44 < 42) oapiSetHUDMode (HUD_NONE+(mx/44));
	return false;
}