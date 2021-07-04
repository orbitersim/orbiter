// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2009 Martin Schweiger
//                   All rights reserved
//
// AirlockSwitch.cpp
// User interface for row of switches on overhead panel
// ==============================================================

#define STRICT 1
#include "AirlockSwitch.h"
#include "DeltaGlider.h"

// constants for texture coordinates
static const float texw = (float)PANEL2D_TEXW; // texture width
static const float texh = (float)PANEL2D_TEXH; // texture height
static const float tx_dx =  25.0f;
static const float tx_dy =  38.0f;
static const float tx_x0 = 989.0f;
static const float tx_y0 = texh-611.0f;
static const float bb_x0 = 241.0f;      // left edge of left-most button
static const float bb_y0 =  26.0f;      // top edge of button row
static const float bb_dx =  61.0f;       // button spacing

const DWORD nbutton = 3;

// ==============================================================

AirlockSwitch::AirlockSwitch (VESSEL3 *v): PanelElement (v)
{
}

// ==============================================================

void AirlockSwitch::Reset2D ()
{
	DWORD i;
	for (i = 0; i < nbutton; i++) btnstate[i] = 0;
}

// ==============================================================

void AirlockSwitch::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	const DWORD NVTX = 4*nbutton;
	const DWORD NIDX = 6*nbutton;
	const WORD IDX_TPL[6] = {0,1,2,3,2,1};
	NTVERTEX VTX[NVTX];
	WORD IDX[NIDX];

	DWORD i, j;
	memset (VTX, 0, NVTX*sizeof(NTVERTEX));
	for (i = 0; i < nbutton; i++) {
		for (j = 0; j < 4; j++) {
			VTX[i*4+j].x = bb_x0 + i*bb_dx + (j%2)*tx_dx;
			VTX[i*4+j].y = bb_y0 + (j/2)*tx_dy;
			VTX[i*4+j].tu = (tx_x0 + ((j+1)%2)*tx_dx)/texw;
			VTX[i*4+j].tv = (tx_y0 + (j/2)*tx_dy)/texh;
		}
		for (j = 0; j < 6; j++)
			IDX[i*6+j] = (WORD)(i*4)+IDX_TPL[j];
	}
	AddGeometry(hMesh, grpidx, VTX, NVTX, IDX, NIDX);
}

// ==============================================================

bool AirlockSwitch::Redraw2D (SURFHANDLE surf)
{
	DeltaGlider *dg = (DeltaGlider*)vessel;

	int i, j, state, vofs;
	for (i = 0; i < nbutton; i++) {
		switch (i) {
			case 0: state = (dg->olock_status == DeltaGlider::DOOR_OPEN || dg->olock_status == DeltaGlider::DOOR_OPENING ? 1:0); break;
			case 1: state = (dg->ilock_status == DeltaGlider::DOOR_OPEN || dg->ilock_status == DeltaGlider::DOOR_OPENING ? 1:0); break;
			case 2: state = 0; break;
		}
		if (state != btnstate[i]) {
			btnstate[i] = state;
			vofs = vtxofs + i*4;
			for (j = 0; j < 4; j++)
				grp->Vtx[vofs+j].tu = (tx_x0+((j+1)%2-state)*tx_dx)/texw;
		}
	}
	return false;
}

// ==============================================================

bool AirlockSwitch::ProcessMouse2D (int event, int mx, int my)
{
	DeltaGlider *dg = (DeltaGlider*)vessel;

	int state, btn = mx / 61;
	if (mx - btn*61 >= 26) return false;
	state = (my < 19 ? 0:1);
	if (state != btnstate[btn]) {
		switch (btn) {
			case 0: dg->ActivateOuterAirlock (state == 0 ? DeltaGlider::DOOR_CLOSING : DeltaGlider::DOOR_OPENING); return true;
			case 1: dg->ActivateInnerAirlock (state == 0 ? DeltaGlider::DOOR_CLOSING : DeltaGlider::DOOR_OPENING); return true;
			case 2: return false;
		}
	}
	return false;
}
