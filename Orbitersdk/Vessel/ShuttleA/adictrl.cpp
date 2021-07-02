// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// adictrl.h
// User interface for attitude reference selector
// ==============================================================

#include "ShuttleA.h"
#include "adictrl.h"
#include "attref.h"
#include "paneltext.h"

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

static const float bb_dl_x0 = (float)ADICTRL_X+ 43.0f;
static const float bb_dl_y0 = (float)ADICTRL_Y+101.0f;
static const float bb_dl_dx2 = tx_dx*0.5f;
static const float bb_dl_dy2 = tx_dy*0.5f;
static const float bb_dl_dx  = 82.0f;

// ==============================================================

ADICtrl::ADICtrl (ShuttleA *shuttlea): PanelElement (shuttlea)
{
	sh = shuttlea;
	refmode = -1;
	tgtmode = -1;
	for (int i = 0; i < 3; i++) btstate[i] = 0;
	btactive = -1;
	settgt = false;
	errmode_is_local = true;

	memset (&dispprm, 0, sizeof(dispprm));
	dispprm.frmmode = dispprm.tgtmode = -1;
	dispprm.navref = (NAVHANDLE)-1;
}

// ==============================================================

void ADICtrl::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx, DWORD grpidx_disp)
{
	// Controls
	const float bb_x0 = bb_dl_x0+10.0f;
	const float bb_dx =  20.0f;
	const float bb_y0 = bb_dl_y0+39.0f;
	const float bb_tgt_x0 = bb_x0-46.0f;
	const float bb_mde_x0 = bb_x0+80.0f;
	const DWORD nvtx = 4*7;
	const DWORD nidx = 6*7;
	const NTVERTEX vtx[nvtx] = {
		// frame dial
		{bb_dl_x0-bb_dl_dx2,bb_dl_y0-bb_dl_dy2,0,  0,0,0,  tx_x0/texw,        tx_y0/texh        },
		{bb_dl_x0+bb_dl_dx2,bb_dl_y0-bb_dl_dy2,0,  0,0,0,  (tx_x0+tx_dx)/texw,tx_y0/texh        },
		{bb_dl_x0-bb_dl_dx2,bb_dl_y0+bb_dl_dy2,0,  0,0,0,  tx_x0/texw,        (tx_y0+tx_dy)/texh},
		{bb_dl_x0+bb_dl_dx2,bb_dl_y0+bb_dl_dy2,0,  0,0,0,  (tx_x0+tx_dx)/texw,(tx_y0+tx_dy)/texh},
		// target dial
		{bb_dl_x0+bb_dl_dx-bb_dl_dx2,bb_dl_y0-bb_dl_dy2,0,  0,0,0,  tx_x0/texw,        tx_y0/texh        },
		{bb_dl_x0+bb_dl_dx+bb_dl_dx2,bb_dl_y0-bb_dl_dy2,0,  0,0,0,  (tx_x0+tx_dx)/texw,tx_y0/texh        },
		{bb_dl_x0+bb_dl_dx-bb_dl_dx2,bb_dl_y0+bb_dl_dy2,0,  0,0,0,  tx_x0/texw,        (tx_y0+tx_dy)/texh},
		{bb_dl_x0+bb_dl_dx+bb_dl_dx2,bb_dl_y0+bb_dl_dy2,0,  0,0,0,  (tx_x0+tx_dx)/texw,(tx_y0+tx_dy)/texh},
		// roll switch
		{bb_x0,         bb_y0,         0,  0,0,0,  tx_x0_bt/texw,           tx_y0_bt/texh           },
		{bb_x0+tx_dx_bt,bb_y0,         0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,tx_y0_bt/texh           },
		{bb_x0,         bb_y0+tx_dy_bt,0,  0,0,0,  tx_x0_bt/texw,           (tx_y0_bt+tx_dy_bt)/texh},
		{bb_x0+tx_dx_bt,bb_y0+tx_dy_bt,0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,(tx_y0_bt+tx_dy_bt)/texh},
		// pitch switch
		{bb_x0+bb_dx,         bb_y0,         0,  0,0,0,  tx_x0_bt/texw,           tx_y0_bt/texh           },
		{bb_x0+bb_dx+tx_dx_bt,bb_y0,         0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,tx_y0_bt/texh           },
		{bb_x0+bb_dx,         bb_y0+tx_dy_bt,0,  0,0,0,  tx_x0_bt/texw,           (tx_y0_bt+tx_dy_bt)/texh},
		{bb_x0+bb_dx+tx_dx_bt,bb_y0+tx_dy_bt,0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,(tx_y0_bt+tx_dy_bt)/texh},
		// yaw switch
		{bb_x0+2.0f*bb_dx,         bb_y0,         0,  0,0,0,  tx_x0_bt/texw,           tx_y0_bt/texh           },
		{bb_x0+2.0f*bb_dx+tx_dx_bt,bb_y0,         0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,tx_y0_bt/texh           },
		{bb_x0+2.0f*bb_dx,         bb_y0+tx_dy_bt,0,  0,0,0,  tx_x0_bt/texw,           (tx_y0_bt+tx_dy_bt)/texh},
		{bb_x0+2.0f*bb_dx+tx_dx_bt,bb_y0+tx_dy_bt,0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,(tx_y0_bt+tx_dy_bt)/texh},
		// ref/tgt selector
		{bb_tgt_x0,         bb_y0,         0,  0,0,0,  tx_x0_bt/texw,           tx_y0_bt/texh           },
		{bb_tgt_x0+tx_dx_bt,bb_y0,         0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,tx_y0_bt/texh           },
		{bb_tgt_x0,         bb_y0+tx_dy_bt,0,  0,0,0,  tx_x0_bt/texw,           (tx_y0_bt+tx_dy_bt)/texh},
		{bb_tgt_x0+tx_dx_bt,bb_y0+tx_dy_bt,0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,(tx_y0_bt+tx_dy_bt)/texh},
		// needle/rate reference mode
		{bb_mde_x0,         bb_y0,         0,  0,0,0,  tx_x0_bt/texw,           tx_y0_bt/texh           },
		{bb_mde_x0+tx_dx_bt,bb_y0,         0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,tx_y0_bt/texh           },
		{bb_mde_x0,         bb_y0+tx_dy_bt,0,  0,0,0,  tx_x0_bt/texw,           (tx_y0_bt+tx_dy_bt)/texh},
		{bb_mde_x0+tx_dx_bt,bb_y0+tx_dy_bt,0,  0,0,0,  (tx_x0_bt+tx_dx_bt)/texw,(tx_y0_bt+tx_dy_bt)/texh}
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

	// Display
	const float dsp_texw = 512.0f;
	const float dsp_texh = 512.0f;
	const float tx_dsp_x0 = 330.5f;
	const float tx_dsp_y0 = dsp_texh-46.5f;
	const float tx_dsp_dx = 147.0f;
	const float tx_dsp_dy =  47.0f;
	const float bb_dsp_x0 = bb_dl_x0-32.5f;
	const float bb_dsp_y0 = bb_dl_y0-86.5f;
	const NTVERTEX dvtx[4] = {
		{bb_dsp_x0,          bb_dsp_y0,          0,  0,0,0,  tx_dsp_x0/dsp_texw,            tx_dsp_y0/dsp_texh            },
		{bb_dsp_x0+tx_dsp_dx,bb_dsp_y0,          0,  0,0,0,  (tx_dsp_x0+tx_dsp_dx)/dsp_texw,tx_dsp_y0/dsp_texh            },
		{bb_dsp_x0,          bb_dsp_y0+tx_dsp_dy,0,  0,0,0,  tx_dsp_x0/dsp_texw,            (tx_dsp_y0+tx_dsp_dy)/dsp_texh},
		{bb_dsp_x0+tx_dsp_dx,bb_dsp_y0+tx_dsp_dy,0,  0,0,0,  (tx_dsp_x0+tx_dsp_dx)/dsp_texw,(tx_dsp_y0+tx_dsp_dy)/dsp_texh}
	};
	const WORD didx[6] = {
		0,1,2, 3,2,1
	};
	AddGeometry (hMesh, grpidx_disp, dvtx, 4, didx, 6);
	dispgrp = grp;
	dispofs = vtxofs;
}

// ==============================================================

bool ADICtrl::Redraw2D (SURFHANDLE surf)
{
	static double x[4] = {-bb_dl_dx2,bb_dl_dx2,-bb_dl_dx2,bb_dl_dx2};
	static double y[4] = {-bb_dl_dy2,-bb_dl_dy2,bb_dl_dy2,bb_dl_dy2};
	static const float tu[4] = {tx_x0_bt/texw, (tx_x0_bt+tx_dx_bt)/texw, tx_x0_bt/texw, (tx_x0_bt+tx_dx_bt)/texw};
	static const float dtu[3] = {0, tx_dx_bt/texw, 2.0f*tx_dx_bt/texw};

	int i, j;

	// frame dial
	int attref_mode = sh->GetAttref()->GetMode();
	if (attref_mode == 4) attref_mode += sh->GetAttref()->GetNavid();
	if (refmode != attref_mode) {
		refmode = attref_mode;
		double phi = (refmode-2)*(45*RAD);
		double sinp = sin(phi), cosp = cos(phi);
		for (int i = 0; i < 4; i++) {
			ctrlgrp->Vtx[ctrlofs+i].x = (float)(x[i]*cosp - y[i]*sinp + bb_dl_x0);
			ctrlgrp->Vtx[ctrlofs+i].y = (float)(x[i]*sinp + y[i]*cosp + bb_dl_y0);
		}
	}

	// target dial
	int atttgt_mode = sh->GetAttref()->GetTgtmode();
	if (tgtmode != atttgt_mode) {
		tgtmode = atttgt_mode;
		double phi = (tgtmode-1)*(45*RAD);
		double sinp = sin(phi), cosp = cos(phi);
		for (int i = 0; i < 4; i++) {
			ctrlgrp->Vtx[ctrlofs+4+i].x = (float)(x[i]*cosp - y[i]*sinp + bb_dl_x0+bb_dl_dx);
			ctrlgrp->Vtx[ctrlofs+4+i].y = (float)(x[i]*sinp + y[i]*cosp + bb_dl_y0);
		}
	}

	// offset toggles
	float bt_dtu;
	for (i = 0; i < 3; i++) {
		bt_dtu = dtu[btstate[i]];
		for (j = 0; j < 4; j++)
			ctrlgrp->Vtx[ctrlofs+8+i*4+j].tu = tu[j] + bt_dtu;
	}

	// offset selector switch
	bt_dtu = dtu[(settgt ? 2:1)];
	for (j = 0; j < 4; j++)
		ctrlgrp->Vtx[ctrlofs+20+j].tu = tu[j] + bt_dtu;

	// err/rate selector switch
	bt_dtu = dtu[(errmode_is_local ? 2:1)];
	for (j = 0; j < 4; j++)
		ctrlgrp->Vtx[ctrlofs+24+j].tu = tu[j] + bt_dtu;

	UpdateDisplay (surf, true);

	return false;
}

// ==============================================================

const int dsp_x0 = 330;
const int dsp_y0 = 512-47;

void ADICtrl::UpdateDisplay (SURFHANDLE surf, bool force)
{
	static const int frm_tx_y0    = 53;
	static const int frm_tx_dy    =  7;
	static const int frm_tx_x0[6] = {0, 404, 44, 17, 75, 98};
	static const int frm_tx_dx[6] = {16, 16, 30, 26, 22, 23};
	static const int nav_tx_x0[6] = {381, 122, 335, 231, 163, 141};
	static const int nav_tx_dx[6] = {22, 17, 21, 36, 36, 20};
	static const int tgt_tx_y0    = 46;
	static const int tgt_tx_x0[4] = {293, 315, 338, 371};
	static const int tgt_tx_dx[4] = {22, 22, 33, 31};

	static const int frm_bb_x0 = dsp_x0+22;
	static const int frm_bb_y0 = dsp_y0+ 1;
	static const int tgt_bb_x0 = dsp_x0+22;
	static const int tgt_bb_y0 = dsp_y0+25;

	VECTOR3 ofs;
	OBJHANDLE hRef = 0;
	NAVHANDLE hNav = 0;
	int i, len, c, mode, tgtmode, x0, dx;
	
	mode = sh->GetAttref()->GetMode();
	tgtmode = sh->GetAttref()->GetTgtmode();

	if (mode == 4) {
		DWORD navid = sh->GetAttref()->GetNavid();
		hNav = sh->GetNavSource (navid);
		if (navid) mode = 5;
	} else if (mode >= 1 && mode <= 3) {
		if (mode == 3) hRef = sh->GetSurfaceRef();
		else           hRef = sh->GetGravityRef();
	}

	if (mode != dispprm.frmmode || hRef != dispprm.frmref || (mode >= 4 && hNav != dispprm.navref)) {
		dispprm.frmmode = mode;
		dispprm.frmref = hRef;
		dispprm.navref = hNav;
		x0 = frm_bb_x0;
		dx = frm_tx_dx[mode];
		oapiColourFill (surf, 0, x0, frm_bb_y0, dsp_x0+148-x0, frm_tx_dy);
		oapiBlt (surf, surf, x0, frm_bb_y0, frm_tx_x0[mode], frm_tx_y0, dx, frm_tx_dy);
		x0 += dx;
		if ((mode >= 1 && mode <= 3) && hRef) {
			char cbuf[256];
			oapiGetObjectName (hRef, cbuf, 256);
			len = strlen(cbuf);
			x0 += 4;
			for (i = 0; i < len; i++) {
				c = toupper (cbuf[i]);
				dx = small_font_width[c];
				oapiBlt (surf, surf, x0, frm_bb_y0, small_font_xpos[c], small_font_ypos[0], dx, frm_tx_dy);
				x0 += dx;
			}
		}
		if (mode >= 4) {
			x0 += 4;
			if ((i = (hNav ? oapiGetNavType (hNav) : 0)) < 6) {
				dx = nav_tx_dx[i];
				oapiBlt (surf, surf, x0, frm_bb_y0, nav_tx_x0[i], frm_tx_y0, dx, frm_tx_dy);
				x0 += dx;
			}
		}
	}

	ofs = sh->GetAttref()->GetEulerOffset();
	DispAngle (surf, ofs.x, 30, 8, dispprm.frmofs[0]);
	DispAngle (surf, ofs.y, 58, 8, dispprm.frmofs[1]);
	DispAngle (surf, ofs.z, 86, 8, dispprm.frmofs[2]);

	ofs = sh->GetAttref()->GetTgtOffset();
	DispAngle (surf, ofs.y, 58, 32, dispprm.tgtofs[0]);
	DispAngle (surf, ofs.z, 86, 32, dispprm.tgtofs[1]);

	ofs = sh->GetAttref()->GetEulerAngles ();
	DispAngle (surf, ofs.x, 30, 15, dispprm.frmdev[0]);
	DispAngle (surf, ofs.y, 58, 15, dispprm.frmdev[1]);
	DispAngle (surf, ofs.z, 86, 15, dispprm.frmdev[2]);

	if (!sh->GetAttref()->GetTgtEulerAngles (ofs)) {
		tgtmode = 0;
		ofs = _V(0,0,0);
	}
	DispAngle (surf, ofs.y, 58, 39, dispprm.tgtdev[0]);
	DispAngle (surf, ofs.z, 86, 39, dispprm.tgtdev[1]);

	if (tgtmode != dispprm.tgtmode) {
		dispprm.tgtmode = tgtmode;
		x0 = frm_bb_x0;
		oapiColourFill (surf, 0, x0, tgt_bb_y0, dsp_x0+148-x0, frm_tx_dy);
		if (tgtmode >= 0 && tgtmode < 4) {
			dx = tgt_tx_dx[tgtmode];
			oapiBlt (surf, surf, x0, tgt_bb_y0, tgt_tx_x0[tgtmode], tgt_tx_y0, dx, frm_tx_dy);
			x0 += dx;
		}
	}
}

void ADICtrl::DispAngle (SURFHANDLE surf, double angle, int x, int y, char curstr[3])
{
	char cbuf[64];
	int i, w, iangle = (int)(angle * DEG + 0.5);
	sprintf (cbuf, "%03d", iangle);
	x += dsp_x0;
	y += dsp_y0;
	for (i = 0; i < 3; i++) {
		w = small_font_width[cbuf[i]];
		if (!curstr || cbuf[i] != curstr[i]) {
			oapiBlt (surf, surf, x, y, small_font_xpos[cbuf[i]], small_font_ypos[0], w, small_font_height);
			curstr[i] = cbuf[i];
		}
		x += w;
	}
}

// ==============================================================

bool ADICtrl::ProcessMouse2D (int event, int mx, int my)
{
	if (my < 36) {
		return ProcessDials (event, mx, my);
	} else if (my >= 57 && my < 89) {
		return ProcessSwitches (event, mx, my-57);
	}
	return false;
}

// ==============================================================

bool ADICtrl::ProcessDials (int event, int mx, int my)
{
	if (mx >= 16 && mx < 52) {  // frame dial
		int attref_mode = sh->GetAttref()->GetMode();
		int attref_navid = sh->GetAttref()->GetNavid();

		if (event & PANEL_MOUSE_LBDOWN) {
			switch (attref_mode) {
			case 1:
			case 2:
			case 3: sh->GetAttref()->SetMode (attref_mode-1); return true;
			case 4: switch (attref_navid) {
						case 0: sh->GetAttref()->SetMode (3); return true;
						case 1: sh->GetAttref()->SetNavid (0); return true;
					}
					break;
			}
		} else if (event & PANEL_MOUSE_RBDOWN) {
			switch (attref_mode) {
			case 0:
			case 1:
			case 2: sh->GetAttref()->SetMode (attref_mode+1); return true;
			case 3: sh->GetAttref()->SetMode (4); sh->GetAttref()->SetNavid(0); return true;
			case 4: if (!sh->GetAttref()->GetNavid()) {
						sh->GetAttref()->SetNavid(1);
						return true;
					}
					break;
			}
		}
	} else if (mx >= 98 && mx < 134) { // target dial
		int atttgt_mode = sh->GetAttref()->GetTgtmode();
		if (event & PANEL_MOUSE_LBDOWN) {
			if (atttgt_mode > 0) {
				sh->GetAttref()->SetTgtmode (atttgt_mode-1);
				return true;
			}
		} else if (event & PANEL_MOUSE_RBDOWN) {
			if (atttgt_mode < 3) {
				sh->GetAttref()->SetTgtmode (atttgt_mode+1);
				return true;
			}
		}
	}
	return false;
}

// ==============================================================

bool ADICtrl::ProcessSwitches (int event, int mx, int my)
{
	static double taction = 0;
	static double dtstep = 1;
	static int count = 0;

	if (mx < 23) {
		if (mx < 17 && (event & PANEL_MOUSE_LBDOWN)) {
			settgt = (my >= 16);
			return true;
		}
		return false;
	} else mx -= 23;

	if (mx < 23) {
		if (mx < 17 && (event & PANEL_MOUSE_LBDOWN)) {
			if (settgt) sh->GetAttref()->SetTgtOffset (_V(0,0,0));
			else        sh->GetAttref()->SetEulerOffset (_V(0,0,0));
			return true;
		}
		return false;
	} else mx -= 23;

	if (mx > 80) {
		if (mx < 97 && (event & PANEL_MOUSE_LBDOWN)) {
			sh->SetAtttgtFrameMode (my > 16 ? 1:0);
			return true;
		}
		return false;
	}

	if (event & PANEL_MOUSE_LBDOWN) {
		if (mx < 57 && mx%20 < 17) {
			btactive = mx/20;
			btstate[btactive] = (my < 16 ? 1 : 2);
			dtstep = 0.3;
			count = 0;
			taction = oapiGetSysTime()-dtstep;
			return true;
		}
	} else if (event & PANEL_MOUSE_LBUP) {
		if (btactive >= 0) {
			btstate[btactive] = 0;
			btactive = -1;
			return true;
		}
	} else if ((event & PANEL_MOUSE_LBPRESSED) && btactive >= 0 && btstate[btactive]) {
		double t = oapiGetSysTime();
		if (t-taction >= dtstep) {
			taction = t;
			if (count < 31) {
				dtstep = dtstep *0.9;
				count++;
			}
			double step = (btstate[btactive] == 1 ? 1.0*RAD : -1.0*RAD);
			VECTOR3 ofs = (settgt ? sh->GetAttref()->GetTgtOffset() : sh->GetAttref()->GetEulerOffset());
			ofs.data[btactive] += step;
			if (settgt) sh->GetAttref()->SetTgtOffset (ofs);
			else        sh->GetAttref()->SetEulerOffset (ofs);
			return true;
		}
	}
	return false;
}