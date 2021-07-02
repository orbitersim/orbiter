// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// switches.cpp
// Panel switch templates
// ==============================================================

#define STRICT 1
#include "switches.h"

static const float texw = (float)PANELEL_TEXW;
static const float texh = (float)PANELEL_TEXH;

// ==============================================================

const float PanelSwitch1::tx_x0 =   0.5f;
const float PanelSwitch1::tx_y0 = texh-43.5f;
const float PanelSwitch1::tx_dx =  19.0f;
const float PanelSwitch1::tx_dy =  43.0f;

// --------------------------------------------------------------

PanelSwitch1::PanelSwitch1 (ShuttleA *v, bool threestate,
	MESHHANDLE hMesh, int meshgrp, int vofs)
	: PanelElement(v), sh(v), enablecenter(threestate)
{
	vstate = -1;
	SelectGeometry (hMesh, meshgrp, vofs);
}

// --------------------------------------------------------------

PanelSwitch1::PanelSwitch1 (ShuttleA *v, float tgt_x0, float tgt_y0, bool threestate)
: PanelElement(v), sh(v), x0(tgt_x0), y0(tgt_y0), enablecenter(threestate)
{
	vstate = 0;
}

// --------------------------------------------------------------

void PanelSwitch1::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	static const DWORD nvtx = 4;
	static const DWORD nidx = 6;
	const NTVERTEX vtx[nvtx] = {
		{x0,      y0,      0,  0,0,0,  tx_x0/texw,        tx_y0/texh        },
		{x0+tx_dx,y0,      0,  0,0,0,  (tx_x0+tx_dx)/texw,tx_y0/texh        },
		{x0,      y0+tx_dy,0,  0,0,0,  tx_x0/texw,        (tx_y0+tx_dy)/texh},
		{x0+tx_dx,y0+tx_dy,0,  0,0,0,  (tx_x0+tx_dx)/texw,(tx_y0+tx_dy)/texh}
	};
	static const WORD idx[nidx] = {
		0,1,2,  3,2,1
	};
	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
}

// --------------------------------------------------------------

void PanelSwitch1::Reset2D (int panelid)
{
	vstate = -1;
}

// --------------------------------------------------------------

bool PanelSwitch1::Redraw2D (SURFHANDLE surf)
{
	int state = GetTargetState();
	if (state != vstate) {
		static float tu[4] = {tx_x0/texw,(tx_x0+tx_dx)/texw,tx_x0/texw,(tx_x0+tx_dx)/texw};
		float dtu = (float)(state*20.0)/texw;
		for (int i = 0; i < 4; i++)
			grp->Vtx[vtxofs+i].tu = tu[i]+dtu;
		vstate = state;
	}
	return false;
}

// --------------------------------------------------------------

bool PanelSwitch1::ProcessMouse2D (int event, int mx, int my)
{
	int tgtstate = vstate;
	if (my < 21) {
		if (vstate < 2) tgtstate = (enablecenter ? vstate+1 : 2);
	} else {
		if (vstate > 0) tgtstate = (enablecenter ? vstate-1 : 0);
	}
	if (tgtstate != vstate) {
		SetTargetState(tgtstate);
		return true;
	} else
		return false;
}


// ==============================================================

const float PanelIndicator1::tx_x0 = 122.5f;
const float PanelIndicator1::tx_y0 = texh-43.5f; // top edge of bottom label
const float PanelIndicator1::tx_dx =  40.0f;
const float PanelIndicator1::tx_dy =  11.0f;
const float PanelIndicator1::ystep =  10.0f;

// --------------------------------------------------------------

PanelIndicator1::PanelIndicator1 (ShuttleA *v, MESHHANDLE hMesh, int meshgrp, int vofs)
: PanelElement(v), sh(v)
{
	vstate = -1;
	SelectGeometry (hMesh, meshgrp, vofs);
}

// --------------------------------------------------------------

PanelIndicator1::PanelIndicator1 (ShuttleA *v, float tgt_x0, float tgt_y0)
: PanelElement(v), sh(v), x0(tgt_x0), y0(tgt_y0)
{
	vstate = -1;
}

// --------------------------------------------------------------

void PanelIndicator1::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	static const DWORD nvtx = 4;
	static const DWORD nidx = 6;
	const NTVERTEX vtx[nvtx] = {
		{x0,      y0,      0,  0,0,0,  tx_x0/texw,        tx_y0/texh        },
		{x0+tx_dx,y0,      0,  0,0,0,  (tx_x0+tx_dx)/texw,tx_y0/texh        },
		{x0,      y0+tx_dy,0,  0,0,0,  tx_x0/texw,        (tx_y0+tx_dy)/texh},
		{x0+tx_dx,y0+tx_dy,0,  0,0,0,  (tx_x0+tx_dx)/texw,(tx_y0+tx_dy)/texh}
	};
	static const WORD idx[nidx] = {
		0,1,2,  3,2,1
	};
	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
}

// --------------------------------------------------------------

void PanelIndicator1::Reset2D (int panelid)
{
	vstate = 0;
}

// --------------------------------------------------------------

bool PanelIndicator1::Redraw2D (SURFHANDLE surf)
{
	int state = GetTargetState();
	if (state != vstate) {
		static float tv[4] = {tx_y0/texh,tx_y0/texh,(tx_y0+tx_dy)/texh,(tx_y0+tx_dy)/texh};
		float dtv = (float)(state*ystep)/texh;
		for (int i = 0; i < 4; i++)
			grp->Vtx[vtxofs+i].tv = tv[i]-dtv;
		vstate = state;
	}
	return false;
}

