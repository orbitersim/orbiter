// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// navbutton.cpp
// User interface for navigation buttons
// ==============================================================

#define STRICT 1
#include "navbutton.h"
#include "ShuttleA.h"

// panel coordinates
static const float bb_x0 = (float)NAVBTN_X;
static const float bb_y0 =  16.5f;
static const float bb_dx =  44.0f;
// texture coordinates
static const float texw = (float)PANEL2D_TEXW; // texture width
static const float texh = (float)PANEL2D_TEXH; // texture height
static const float tx_x0 = texw-297.0f;
static const float tx_y0 = texh- 32.5f;
static const float tx_dx =  42.0f;
static const float tx_dy =  32.0f;

// ==============================================================

NavButton::NavButton (VESSEL3 *v): PanelElement (v)
{
}

// ==============================================================

void NavButton::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	const DWORD nvtx = 4*6;
	const DWORD nidx = 6*6;
	static NTVERTEX vtx[nvtx] = {
		// "Kill rot" button
		{bb_x0+5.0f*bb_dx,      bb_y0,      0,  0,0,0,  (tx_x0+5.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+5.0f*bb_dx+tx_dx,bb_y0,      0,  0,0,0,  (tx_x0+6.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+5.0f*bb_dx,      bb_y0+tx_dy,0,  0,0,0,  (tx_x0+5.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+5.0f*bb_dx+tx_dx,bb_y0+tx_dy,0,  0,0,0,  (tx_x0+6.0f*tx_dx)/texw,tx_y0/texh},
		// "Hlevel" button
		{bb_x0+4.0f*bb_dx,      bb_y0,      0,  0,0,0,  (tx_x0+4.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+4.0f*bb_dx+tx_dx,bb_y0,      0,  0,0,0,  (tx_x0+5.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+4.0f*bb_dx,      bb_y0+tx_dy,0,  0,0,0,  (tx_x0+4.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+4.0f*bb_dx+tx_dx,bb_y0+tx_dy,0,  0,0,0,  (tx_x0+5.0f*tx_dx)/texw,tx_y0/texh},
		// "Prograde" button
		{bb_x0+3.0f*bb_dx,      bb_y0,      0,  0,0,0,  (tx_x0+3.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+3.0f*bb_dx+tx_dx,bb_y0,      0,  0,0,0,  (tx_x0+4.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+3.0f*bb_dx,      bb_y0+tx_dy,0,  0,0,0,  (tx_x0+3.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+3.0f*bb_dx+tx_dx,bb_y0+tx_dy,0,  0,0,0,  (tx_x0+4.0f*tx_dx)/texw,tx_y0/texh},
		// "Retrograde" button
		{bb_x0+2.0f*bb_dx,      bb_y0,      0,  0,0,0,  (tx_x0+2.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+2.0f*bb_dx+tx_dx,bb_y0,      0,  0,0,0,  (tx_x0+3.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+2.0f*bb_dx,      bb_y0+tx_dy,0,  0,0,0,  (tx_x0+2.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+2.0f*bb_dx+tx_dx,bb_y0+tx_dy,0,  0,0,0,  (tx_x0+3.0f*tx_dx)/texw,tx_y0/texh},
		// "Orbit normal" button
		{bb_x0+bb_dx,           bb_y0,      0,  0,0,0,  (tx_x0+1.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+bb_dx+tx_dx,     bb_y0,      0,  0,0,0,  (tx_x0+2.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+bb_dx,           bb_y0+tx_dy,0,  0,0,0,  (tx_x0+1.0f*tx_dx)/texw,tx_y0/texh},
		{bb_x0+bb_dx+tx_dx,     bb_y0+tx_dy,0,  0,0,0,  (tx_x0+2.0f*tx_dx)/texw,tx_y0/texh},
		// "Orbit antinormal" button
		{bb_x0,                 bb_y0,      0,  0,0,0,  tx_x0/texw,             tx_y0/texh},
		{bb_x0+tx_dx,           bb_y0,      0,  0,0,0,  (tx_x0+tx_dx)/texw,     tx_y0/texh},
		{bb_x0,                 bb_y0+tx_dy,0,  0,0,0,  tx_x0/texw,             tx_y0/texh},
		{bb_x0+tx_dx,           bb_y0+tx_dy,0,  0,0,0,  (tx_x0+tx_dx)/texw,     tx_y0/texh}
	};
	static WORD idx[nidx] = {
		0,1,2, 3,2,1,
		4,5,6, 7,6,5,
		8,9,10, 11,10,9,
		12,13,14, 15,14,13,
		16,17,18, 19,18,17,
		20,21,22, 23,22,21
	};

	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
}

// ==============================================================

bool NavButton::Redraw2D (SURFHANDLE surf)
{
	static const float tv_idle = tx_y0/texh;
	static const float tv_active = (tx_y0+tx_dy)/texh;
	int vofs;

	for (DWORD i = NAVMODE_KILLROT; i <= NAVMODE_ANTINORMAL; i++) {
		vofs = vtxofs+(i-NAVMODE_KILLROT)*4;
		grp->Vtx[vofs+2].tv = grp->Vtx[vofs+3].tv =
			(vessel->GetNavmodeState(i) ? tv_active : tv_idle);
	}
		
	return false;
}

// ==============================================================

bool NavButton::ProcessMouse2D (int event, int mx, int my)
{
	static int navmode[6] = {
		NAVMODE_ANTINORMAL, NAVMODE_NORMAL,
		NAVMODE_RETROGRADE, NAVMODE_PROGRADE,
		NAVMODE_HLEVEL, NAVMODE_KILLROT
	};
	if (mx%44 < 42) {
		int bt = mx/44;
		if (bt >= 0 && bt < 6) {
			vessel->ToggleNavmode (navmode[bt]);
			return true;
		}
	}
	return false;
}