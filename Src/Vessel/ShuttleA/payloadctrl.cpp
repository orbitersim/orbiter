// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// payloadctrl.cpp
// User interface payload controls
// ==============================================================

#define STRICT 1
#include "payloadctrl.h"

static const float texw = (float)PANELEL_TEXW;
static const float texh = (float)PANELEL_TEXH;
static const float tx_x0 =  80.0f;
static const float tx_y0 = texh-31.0f;
static const float tx_dx =  42.0f;
static const float tx_dy =  31.0f;
static const float bb_x0 = 723.0f;
static const float bb_y0 =  50.0f;

// ==============================================================

PayloadRelease::PayloadRelease (ShuttleA *v, MESHHANDLE hMesh)
: PanelElement (v)
{
	for (int i = 0; i < 6; i++)
		vstate[i] = 0;
	SelectGeometry (hMesh, 2, 16);
}

// --------------------------------------------------------------

void PayloadRelease::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	static const DWORD nvtx = 4*6;
	static const DWORD nidx = 6*6;
	static NTVERTEX vtx[nvtx];
	int i, j, ofs;
	float x0, y0;
	memset (vtx, 0, nvtx*sizeof(NTVERTEX));
	for (i = ofs = 0; i < 2; i++) {
		x0 = (1-i)*53.0f + bb_x0;
		for (j = 0; j < 3; j++, ofs += 4) {
			y0 = j*46.0f + bb_y0;
			vtx[ofs+1].x = vtx[ofs+3].x = tx_dx + (vtx[ofs].x = vtx[ofs+2].x = x0);
			vtx[ofs+2].y = vtx[ofs+3].y = tx_dy + (vtx[ofs].y = vtx[ofs+1].y = y0);
			vtx[ofs+1].tu = vtx[ofs+3].tu = tx_dx/texw + (vtx[ofs].tu = vtx[ofs+2].tu = tx_x0/texw);
			vtx[ofs+2].tv = vtx[ofs+3].tv = tx_dy/texh + (vtx[ofs].tv = vtx[ofs+1].tv = tx_y0/texh);
		}
	}
	static WORD idx[nidx];
	for (i = j = 0; i < 6; i++) {
		ofs = i*4;
		idx[j++] = ofs;
		idx[j++] = ofs+1;
		idx[j++] = ofs+2;
		idx[j++] = ofs+3;
		idx[j++] = ofs+2;
		idx[j++] = ofs+1;
	}
		
	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
}

// --------------------------------------------------------------

bool PayloadRelease::Redraw2D (SURFHANDLE surf)
{
	bool modified = false;
	ShuttleA *sh = (ShuttleA*)vessel;
	for (int i = 0; i < 6; i++) {
		int state = (sh->GetAttachmentStatus(sh->payload_attachment[i]) ? sh->cargo_arm_status ? 1 : 0 : 2);
		if (state != vstate[i]) {
			static float tu[4] = {tx_x0/texw, (tx_x0+tx_dx)/texw, tx_x0/texw, (tx_x0+tx_dx)/texw};
			float dtu = (float)(state*tx_dx)/texw;
			for (int j = 0; j < 4; j++)
				grp->Vtx[vtxofs+i*4+j].tu = tu[j]+dtu;
			vstate[i] = state;
			modified = true;
		}
	}
	return modified;
}

// --------------------------------------------------------------

bool PayloadRelease::ProcessMouse2D (int event, int mx, int my)
{
	ShuttleA *sh = (ShuttleA*)vessel;
	if (!sh->cargo_arm_status) return false;

	int col = mx/53;
	int row = my/46;
	if (mx - col*53 > 42 || my - row*46 > 31) return false;
	int i = row + (1-col)*3;
	if (sh->GetAttachmentStatus(sh->payload_attachment[i])) {
		sh->ToggleGrapple(i);
		return true;
	}
	return false;
}



// ==============================================================

PayloadArmSwitch::PayloadArmSwitch (ShuttleA *v, MESHHANDLE hMesh)
: PanelSwitch1 (v, false, hMesh, 2, 40)
{
}

// --------------------------------------------------------------

int PayloadArmSwitch::GetTargetState ()
{
	return (sh->cargo_arm_status ? 0 : 2);
}

// --------------------------------------------------------------

void PayloadArmSwitch::SetTargetState (int state)
{
	sh->ActivateCargo (state == 2 ? 0 : 1);
}


// ==============================================================

PayloadArmIndicator::PayloadArmIndicator (ShuttleA *v, MESHHANDLE hMesh)
: PanelIndicator1 (v, hMesh, 2, 60)
{
}

// --------------------------------------------------------------

int PayloadArmIndicator::GetTargetState ()
{
	return (sh->cargo_arm_status ? 1 : 2);
}