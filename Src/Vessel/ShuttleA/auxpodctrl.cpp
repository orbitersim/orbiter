// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// auxpodctrl.h
// User interface for auxiliary thruster pod control
// ==============================================================

#include "ShuttleA.h"
#include "auxpodctrl.h"

#define STRICT 1

static const float texw = (float)PANEL2D_TEXW;
static const float texh = (float)PANEL2D_TEXH;
static const float tx_x0 = texw-12.5f;
static const float tx_y0 = texh-83.5f;
static const float tx_dx =  11.0f;
static const float tx_dy =  33.0f;
static const float tx_x0_bt = texw-80.0f;
static const float tx_y0_bt = texh-83.0f;
static const float tx_dx_bt =  21.0f;
static const float tx_dy_bt =  32.0f;

static const float bb_dl_x0 = (float)PODCTRL_X;
static const float bb_dl_y0 = (float)PODCTRL_Y;
static const float bb_dl_dx2 = tx_dx*0.5f;
static const float bb_dl_dy2 = tx_dy*0.5f;
static const float bb_dl_dx  = 82.0f;

static const double bb_ln_xc = bb_dl_x0+ 38.0; // left needle centre x
static const double bb_rn_xc = bb_dl_x0+102.0; // right needle centre x
static const double bb_n_yc  = bb_dl_y0+ 27.0; // needle centre y

static double preset_angle[3] = {0, PI05, PI};

// ==============================================================
// returns the needle vertices for a given pod angle

void set_needle_vtx (UINT which, double angle, NTVERTEX *vtx)
{
	const double r1      = 28.0;
	const double r2      =  4.0;
	const double w2      =  1.7;
	const double xofs[2] = {bb_ln_xc,bb_rn_xc};
	const double yofs    = bb_n_yc;

	const double x0[4]   = {r2,r1,r2,r1};
	const double y0[4]   = {-w2,-w2,w2,w2};

	angle = PI-angle;
	double cosa = cos(angle), sina = sin(angle);

	for (int i = 0; i < 4; i++) {
		vtx[i].x = (float)(x0[i]*cosa - y0[i]*sina + xofs[which]);
		vtx[i].y = (float)(x0[i]*sina + y0[i]*cosa + yofs);
	}
}

// ==============================================================
// returns the preset indicator vertices for a given command angle

void set_preset_vtx (UINT which, double angle, NTVERTEX *vtx)
{
	const double r1      = 32.0;
	const double r2      = 26.0;
	const double w2      =  5.0;
	const double xofs[2] = {bb_ln_xc,bb_rn_xc};
	const double yofs    = bb_n_yc;

	const double x0[4]   = {r2,r1,r2,r1};
	const double y0[4]   = {-w2,-w2,w2,w2};

	angle = PI-angle;
	double cosa = cos(angle), sina = sin(angle);

	for (int i = 0; i < 4; i++) {
		vtx[i].x = (float)(x0[i]*cosa - y0[i]*sina + xofs[which]);
		vtx[i].y = (float)(x0[i]*sina + y0[i]*cosa + yofs);
	}
}

// ==============================================================

AuxPodCtrl::AuxPodCtrl (ShuttleA *shuttlea): PanelElement (shuttlea)
{
	int i;

	sh = shuttlea;
	mode = ctrl = preset = 0;
	redraw_buttons = false;
	for (i = 0; i < 2; i++)
		pod_angle_cmd[i] = pod_angle_ind[i] = pod_preset_ind[i] = sh->GetPodAngle(i);
	for (i = 0; i < 3; i++)
		preset_active[i] = false;
}

// ==============================================================

void AuxPodCtrl::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx, DWORD grpidx_disp)
{
	// Controls
	const float bb_x0 = PODCTRL_X+87.0f;
	const float bb_dx =  20.0f;
	const float bb_y0 = PODCTRL_Y+86.0f;
	const DWORD nvtx = 4*7;
	const DWORD nidx = 6*7;
	const NTVERTEX vtx[nvtx] = {
		// left pod tilt toggle
		{bb_x0,         bb_y0,         0,  0,0,0,  tx_x0_bt/texw,           tx_y0_bt/texh           },
		{bb_x0+tx_dx_bt,bb_y0,         0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,tx_y0_bt/texh           },
		{bb_x0,         bb_y0+tx_dy_bt,0,  0,0,0,  tx_x0_bt/texw,           (tx_y0_bt+tx_dy_bt)/texh},
		{bb_x0+tx_dx_bt,bb_y0+tx_dy_bt,0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,(tx_y0_bt+tx_dy_bt)/texh},
		// right pod tilt toggle
		{bb_x0+bb_dx,         bb_y0,         0,  0,0,0,  tx_x0_bt/texw,           tx_y0_bt/texh           },
		{bb_x0+bb_dx+tx_dx_bt,bb_y0,         0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,tx_y0_bt/texh           },
		{bb_x0+bb_dx,         bb_y0+tx_dy_bt,0,  0,0,0,  tx_x0_bt/texw,           (tx_y0_bt+tx_dy_bt)/texh},
		{bb_x0+bb_dx+tx_dx_bt,bb_y0+tx_dy_bt,0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,(tx_y0_bt+tx_dy_bt)/texh},
		// indicator light
		{0,0,0,  0,0,0,   (texw-46)/texw,(texh-48)/texh},
		{0,0,0,  0,0,0,   (texw-46)/texw,(texh-48)/texh},
		{0,0,0,  0,0,0,   (texw-46)/texw,(texh-48)/texh},
		{0,0,0,  0,0,0,   (texw-46)/texw,(texh-48)/texh},
		// left angle needle
		{0,0,0,  0,0,0,   (texw-40.5f)/texw,(texh- 84.5f)/texh},
		{0,0,0,  0,0,0,   (texw-40.5f)/texw,(texh-114.5f)/texh},
		{0,0,0,  0,0,0,   (texw-35.5f)/texw,(texh- 84.5f)/texh},
		{0,0,0,  0,0,0,   (texw-35.5f)/texw,(texh-114.5f)/texh},
		// right angle needle
		{0,0,0,  0,0,0,   (texw-40.5f)/texw,(texh- 84.5f)/texh},
		{0,0,0,  0,0,0,   (texw-40.5f)/texw,(texh-114.5f)/texh},
		{0,0,0,  0,0,0,   (texw-35.5f)/texw,(texh- 84.5f)/texh},
		{0,0,0,  0,0,0,   (texw-35.5f)/texw,(texh-114.5f)/texh},
		// left angle preset indicator
		{0,0,0,  0,0,0,   (texw-41.5f)/texw,(texh- 91.5f)/texh},
		{0,0,0,  0,0,0,   (texw-41.5f)/texw,(texh- 84.5f)/texh},
		{0,0,0,  0,0,0,   (texw-55.5f)/texw,(texh- 91.5f)/texh},
		{0,0,0,  0,0,0,   (texw-55.5f)/texw,(texh- 84.5f)/texh},
		// right angle preset indicator
		{0,0,0,  0,0,0,   (texw-41.5f)/texw,(texh- 91.5f)/texh},
		{0,0,0,  0,0,0,   (texw-41.5f)/texw,(texh- 84.5f)/texh},
		{0,0,0,  0,0,0,   (texw-55.5f)/texw,(texh- 91.5f)/texh},
		{0,0,0,  0,0,0,   (texw-55.5f)/texw,(texh- 84.5f)/texh}
	};
	const WORD idx[nidx] = {
		0,1,2, 3,2,1,
		4,5,6, 7,6,5,
		8,9,10, 11,10,9,
		12,13,14, 15,14,13,
		16,17,18, 19,18,17,
		20,21,22, 23,22,21,
		24,25,26, 27,26,25
	};
	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
	ctrlgrp = grp;
	ctrlofs = vtxofs;
	needleofs = ctrlofs+3*4;

	for (UINT i = 0; i < 2; i++) {
		set_needle_vtx (i, pod_angle_ind[i], grp->Vtx+(needleofs+i*4));
		set_preset_vtx (i, pod_angle_cmd[i], grp->Vtx+(needleofs+8+i*4));
	}
}

// ==============================================================

void AuxPodCtrl::Reset2D (int panelid)
{
	int i;

	for (i = 0; i < 2; i++) {
		pod_angle_cmd[i] = sh->GetPodAngle (i);
	}
}

// ==============================================================

bool AuxPodCtrl::Redraw2D (SURFHANDLE surf)
{
	static double EPS = 1e-6;

	static const float tu[4] = {tx_x0_bt/texw, (tx_x0_bt+tx_dx_bt)/texw, tx_x0_bt/texw, (tx_x0_bt+tx_dx_bt)/texw};
	static const float dtu[3] = {0, tx_dx_bt/texw, 2.0f*tx_dx_bt/texw};

	int i, j, state;

	if (redraw_buttons) {
		// pod tilt toggles
		float bt_dtu;
		for (i = 0; i < 2; i++) {
			state = ((ctrl >> i) & 1 ? mode : 0);
			bt_dtu = dtu[state];
			for	(j = 0; j < 4; j++)
				ctrlgrp->Vtx[ctrlofs+i*4+j].tu = tu[j] + bt_dtu;
		}
		redraw_buttons = false;
	}

	double pod_angle[2];
	for (i = 0; i < 2; i++) pod_angle[i] = sh->GetPodAngle(i);

	if (preset) {
		for (i = 0; i < 2; i++)
			if (fabs(pod_angle[i]-pod_angle_cmd[i]) > EPS) break;
		if (i == 2) preset = 0; // done
	}
	if (preset && !preset_active[preset-1]) {
		// activate indicator light
		static const float x[4] = {bb_dl_x0+15.0f, bb_dl_x0+24.0f, bb_dl_x0+15.0f, bb_dl_x0+24.0f};
		static const float y[4] = {bb_dl_y0+114.0f, bb_dl_y0+114.0f, bb_dl_y0+116.0f, bb_dl_y0+116.0f};
		for (i = 0; i < 4; i++) {
			ctrlgrp->Vtx[ctrlofs+8+i].x = x[i] + (preset-1)*23.0f;
			ctrlgrp->Vtx[ctrlofs+8+i].y = y[i];
		}
		for (i = 0; i < 3; i++) preset_active[i] = false;
		preset_active[preset-1] = true;
	} else if (!preset && (preset_active[0] || preset_active[1] || preset_active[2])) {
		// deactivate indicator light
		for (i = 0; i < 4; i++)
			ctrlgrp->Vtx[ctrlofs+8+i].x = ctrlgrp->Vtx[ctrlofs+8+i].y = 0;
		for (i = 0; i < 3; i++) preset_active[i] = false;
	}

	for (i = 0; i < 2; i++) {
		if (pod_angle[i] != pod_angle_ind[i]) {
			pod_angle_ind[i] = pod_angle[i];
			set_needle_vtx (i, pod_angle[i], grp->Vtx + (needleofs+i*4));
		}
		if (pod_angle_cmd[i] != pod_preset_ind[i]) {
			pod_preset_ind[i] = pod_angle_cmd[i];
			set_preset_vtx (i, pod_angle_cmd[i], grp->Vtx + (needleofs+8+i*4));
		}
	}
	return false;
}

// ==============================================================

bool AuxPodCtrl::ProcessMouse2D (int event, int mx, int my)
{
	// toggle switches
	if (mx >= 88 && mx < 127) {
		if (event & PANEL_MOUSE_LBDOWN) {
			if      (mx <   97) ctrl = 1; // left pod
			else if (mx >= 118) ctrl = 2; // right pod
			else                ctrl = 3; // both
			mode = (my < 14 ? 1:2); // forward/back
			toggle_t = oapiGetSimTime();
			preset = 0;
			redraw_buttons = true;
		}
	// preset buttons
	} else if (mx >= 11 && mx < 74 && my >= 6 && my < 22) {
		int bt = (mx-11)/23;
		if ((mx-11)%23 < 17) {
			pod_angle_cmd[0] = pod_angle_cmd[1] = preset_angle[bt];
			preset = bt+1;
			sh->CommandPodAngle (3, preset_angle[bt]);
		}
	}
	if (event & PANEL_MOUSE_LBUP) {
		mode = 0;   // stop
		redraw_buttons = true;
	}
	if (mode) {
		UINT i;
		double t = oapiGetSimTime();
		double dt = t-toggle_t;
		toggle_t = t;
		double dangle = dt*POD_ROTREQUEST_SPEED;
		double pod_angle_ref = (ctrl & 1 ? pod_angle_cmd[0] : pod_angle_cmd[1]);
		if (mode == 1) pod_angle_ref = min (pod_angle_ref+dangle, PI);
		else           pod_angle_ref = max (pod_angle_ref-dangle, 0);
		for (i = 0; i < 2; i++)
			if ((ctrl >> i) & 1)
				pod_angle_cmd[i] = pod_angle_ref;
		sh->CommandPodAngle (ctrl, pod_angle_ref);
	}

	return false;
}
