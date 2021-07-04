// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// DGSwitches.cpp
// Prototypes for DG-specific cockpit switches and dials
// ==============================================================

#include "DeltaGlider.h"
#include "DGSwitches.h"

// ==============================================================

double DGSwitch1::travel = 28.0*RAD;
const int DGSwitch1::nvtx = 33;
const float DGSwitch1::tu0[3] = {53.0f/2048.0f, 105.0f/2048.0f, 79.0f/2048.0f};
const float DGSwitch1::tv0 = 337.0f/1024.0f;
const float DGSwitch1::tw  =  26.0f/2048.0f;
const float DGSwitch1::th  =  52.0f/1024.0f;

// --------------------------------------------------------------

DGSwitch1::DGSwitch1 (VESSEL3 *v, Mode m)
: PanelElement(v), mode(m)
{
	state = vstate = vstate2 = CENTER; // we always initiate as centered, even for 2state switches
}

// --------------------------------------------------------------

void DGSwitch1::DefineAnimationVC (const VECTOR3 &ref, const VECTOR3 &axis,
	DWORD meshgrp, int vtxofs)
{
	rf = ref;
	ax = axis;
	mgrp = meshgrp;
	vofs = vtxofs;
}

// --------------------------------------------------------------

void DGSwitch1::DefineAnimation2D (MESHHANDLE hMesh, DWORD meshgrp, int vtxofs)
{
	grp = oapiMeshGroup (hMesh, meshgrp);
	mgrp = meshgrp;
	vofs = vtxofs;
}

// --------------------------------------------------------------

void DGSwitch1::ResetVC (DEVMESHHANDLE hMesh)
{
	PanelElement::ResetVC (hMesh);
	vstate = CENTER;
}

// --------------------------------------------------------------

bool DGSwitch1::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		if (p.y < 0.5) Down();
		else           Up();
	} else if (event & PANEL_MOUSE_LBUP) {
		if (mode == SPRING)
			SetState (CENTER);
	}
	return (state != vstate);
}

// --------------------------------------------------------------

bool DGSwitch1::ProcessMouse2D (int event, int mx, int my)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		if (my < 26) Up();
		else         Down();
	} else if (event & PANEL_MOUSE_LBUP) {
		if (mode == SPRING)
			SetState (CENTER);
	}
	return (state != vstate2);
}

// --------------------------------------------------------------

bool DGSwitch1::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	static double phi[3] = {0.0, travel, -travel};
	if (state != vstate) {
		int i;
		double phi0 = phi[vstate];
		double phi1 = phi[state];
		double dphi = phi1-phi0;
		VECTOR3 p, pt;
		MATRIX3 R = rotm(ax,dphi); // rotation matrix from current to new state

		NTVERTEX vtx[nvtx];
		WORD vperm[nvtx];
		for (i = 0; i < nvtx; i++) vperm[i] = vofs + i;
		GROUPREQUESTSPEC grs = {vtx, nvtx, vperm, 0, 0, 0, 0, 0};
		oapiGetMeshGroup (hMesh, mgrp, &grs);
		for (i = 0; i < nvtx; i++) {
			p.x = vtx[i].x - rf.x;
			p.y = vtx[i].y - rf.y;
			p.z = vtx[i].z - rf.z;
			pt = mul(R,p);
			vtx[i].x = (float)(pt.x + rf.x);
			vtx[i].y = (float)(pt.y + rf.y);
			vtx[i].z = (float)(pt.z + rf.z);
			p.x = vtx[i].nx;
			p.y = vtx[i].ny;
			p.z = vtx[i].nz;
			pt = mul(R,p);
			vtx[i].nx = (float)pt.x;
			vtx[i].ny = (float)pt.y;
			vtx[i].nz = (float)pt.z;
		}
		GROUPEDITSPEC ges = {GRPEDIT_VTXCRD|GRPEDIT_VTXNML, 0, vtx, nvtx, vperm};
		oapiEditMeshGroup (hMesh, mgrp, &ges);
		vstate = state;
	}
	return false;
}

// --------------------------------------------------------------

bool DGSwitch1::Redraw2D (SURFHANDLE surf)
{
	if (state != vstate2) {
		const int nvtx = 4;
		for (int i = 0; i < nvtx; i++) {
			grp->Vtx[vofs+i].tu = tu0[state] + (i&1 ? tw:0);
		}
		vstate2 = state;
	}
	return false;
}

// --------------------------------------------------------------

bool DGSwitch1::SetState (State s)
{
	// note: it is admissable to force a 2-state switch to center position
	if (state != s) {
		state = s;
		return true;
	}
	return false;
}

// --------------------------------------------------------------

DGSwitch1::State DGSwitch1::Up ()
{
	if (state != UP)
		SetState (state == DOWN && mode != TWOSTATE ? CENTER : UP);
	return state;
}

// --------------------------------------------------------------

DGSwitch1::State DGSwitch1::Down ()
{
	if (state != DOWN)
		SetState (state == UP && mode != TWOSTATE ? CENTER : DOWN);
	return state;
}

// ==============================================================

const int DGSwitch2::nvtx = 28;
double DGSwitch2::travel = 15.0*RAD;

// --------------------------------------------------------------

DGSwitch2::DGSwitch2 (VESSEL3 *v)
: PanelElement(v)
{
	orient = VERT;
	state = vstate = CENTER;
}

// --------------------------------------------------------------

void DGSwitch2::DefineAnimation2D (Orientation o, DWORD meshgrp, DWORD vofs)
{
	orient = o;
	mgrp = meshgrp;
	vtxofs = vofs;
}

// --------------------------------------------------------------

void DGSwitch2::DefineAnimationVC (const VECTOR3 &ref, const VECTOR3 &axis,
	DWORD meshgrp, DWORD vofs)
{
	rf = ref;
	ax = axis;
	mgrp = meshgrp;
	vtxofs = vofs;
}

// --------------------------------------------------------------

void DGSwitch2::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, mgrp);
}

// --------------------------------------------------------------

bool DGSwitch2::ProcessMouse2D (int event, int mx, int my)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		if (orient == VERT) SetState (my <  22 ? UP : DOWN);
		else                SetState (mx >= 22 ? UP : DOWN);
	} else if (event & PANEL_MOUSE_LBUP) {
		SetState (CENTER);
	}
	return (state != vstate);
}

// --------------------------------------------------------------

bool DGSwitch2::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		SetState (p.y < 0.5 ? DOWN : UP);
	} else if (event & PANEL_MOUSE_LBUP) {
		SetState (CENTER);
	}
	return (state != vstate);
}

// --------------------------------------------------------------

bool DGSwitch2::Redraw2D (SURFHANDLE surf)
{
	static const float texw = (float)PANEL2D_TEXW; // texture width
	if (state != vstate) {
		int ofs = state*16;
		if (orient == HORZ_RL && state) ofs = 48-ofs;
		for (int i = 0; i < 4; i++)
			grp->Vtx[vtxofs+i].tu = (1053.5f+ofs+(i%2)*15)/texw;
		vstate = state;
	}
	return false;
}

// --------------------------------------------------------------

bool DGSwitch2::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	static double phi[3] = {0.0, travel, -travel};
	if (state != vstate) {
		int i;
		double phi0 = phi[vstate];
		double phi1 = phi[state];
		double dphi = phi1-phi0;
		VECTOR3 p, pt;
		MATRIX3 R = rotm(ax,dphi); // rotation matrix from current to new state

		NTVERTEX vtx[nvtx];
		WORD vperm[nvtx];
		for (i = 0; i < nvtx; i++) vperm[i] = (WORD)(vtxofs + i);
		GROUPREQUESTSPEC grs = {vtx, nvtx, vperm, 0, 0, 0, 0, 0};
		oapiGetMeshGroup (hMesh, mgrp, &grs);
		for (i = 0; i < nvtx; i++) {
			p.x = vtx[i].x - rf.x;
			p.y = vtx[i].y - rf.y;
			p.z = vtx[i].z - rf.z;
			pt = mul(R,p);
			vtx[i].x = (float)(pt.x + rf.x);
			vtx[i].y = (float)(pt.y + rf.y);
			vtx[i].z = (float)(pt.z + rf.z);
			p.x = vtx[i].nx;
			p.y = vtx[i].ny;
			p.z = vtx[i].nz;
			pt = mul(R,p);
			vtx[i].nx = (float)pt.x;
			vtx[i].ny = (float)pt.y;
			vtx[i].nz = (float)pt.z;
		}
		GROUPEDITSPEC ges = {GRPEDIT_VTXCRD|GRPEDIT_VTXNML, 0, vtx, nvtx, vperm};
		oapiEditMeshGroup (hMesh, mgrp, &ges);
		vstate = state;
	}
	return false;
}

// --------------------------------------------------------------

bool DGSwitch2::SetState (State s)
{
	if (state != s) {
		state = s;
		return true;
	}
	return false;
}

// ==============================================================

const int DGDial1::nvtx = 76;

// --------------------------------------------------------------

DGDial1::DGDial1 (VESSEL3 *v, int np, double pos0, double delta)
: PanelElement(v), npos(np), p0(pos0), dp(delta)
{
	pos = 0;
	vpos = -1; // undefined
}

// --------------------------------------------------------------

void DGDial1::DefineAnimationVC (const VECTOR3 &ref, const VECTOR3 &axis, DWORD meshgrp, int vtxofs)
{
	rf = ref;
	ax = axis;
	mgrp = meshgrp;
	vofs = vtxofs;
}

// --------------------------------------------------------------

void DGDial1::ResetVC (DEVMESHHANDLE hMesh)
{
	vpos = -1;
}

// --------------------------------------------------------------

bool DGDial1::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		if (p.x < 0.5) Left();
		else           Right();
	}
	return (pos != vpos);
}

// --------------------------------------------------------------

bool DGDial1::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (pos != vpos) {
		int i;
		double phi0 = (vpos >= 0 ? p0 + vpos*dp : 0.0);
		double phi1 = p0 + pos*dp;
		double dphi = phi0-phi1;
		VECTOR3 p, pt;
		MATRIX3 R = rotm(ax,dphi);

		NTVERTEX vtx[nvtx];
		WORD vperm[nvtx];
		for (i = 0; i < nvtx; i++) vperm[i] = vofs + i;
		GROUPREQUESTSPEC grs = {vtx, nvtx, vperm, 0, 0, 0, 0, 0};
		oapiGetMeshGroup (hMesh, mgrp, &grs);
		for (i = 0; i < nvtx; i++) {
			p.x = vtx[i].x - rf.x;
			p.y = vtx[i].y - rf.y;
			p.z = vtx[i].z - rf.z;
			pt = mul(R,p);
			vtx[i].x = (float)(pt.x + rf.x);
			vtx[i].y = (float)(pt.y + rf.y);
			vtx[i].z = (float)(pt.z + rf.z);
			p.x = vtx[i].nx;
			p.y = vtx[i].ny;
			p.z = vtx[i].nz;
			pt = mul(R,p);
			vtx[i].nx = (float)pt.x;
			vtx[i].ny = (float)pt.y;
			vtx[i].nz = (float)pt.z;
		}
		GROUPEDITSPEC ges = {GRPEDIT_VTXCRD|GRPEDIT_VTXNML, 0, vtx, nvtx, vperm};
		oapiEditMeshGroup (hMesh, mgrp, &ges);

		vpos = pos;
	}
	return false;
}

// --------------------------------------------------------------

bool DGDial1::SetPosition (int newpos)
{
	if (newpos != pos) {
		pos = newpos;
		return true;
	}
	return false;
}

// --------------------------------------------------------------

int DGDial1::Left ()
{
	if (pos > 0) pos--;
	return pos;
}

// --------------------------------------------------------------

int DGDial1::Right ()
{
	if (pos < npos-1) pos++;
	return pos;
}

// ==============================================================

const int DGButton2::nvtx = 20;

// --------------------------------------------------------------

DGButton2::DGButton2 (VESSEL3 *v)
: PanelElement(v)
{
	state = vstate = OFF;
}

// --------------------------------------------------------------

void DGButton2::DefineAnimationVC (const VECTOR3 &axis, DWORD meshgrp, DWORD vofs)
{
	mgrp = meshgrp;
	vtxofs = vofs;
	ax = axis;
}

// --------------------------------------------------------------

bool DGButton2::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		state = ON;
	} else if (event & PANEL_MOUSE_LBUP) {
		state = OFF;
	}
	return (state != vstate);
}

// --------------------------------------------------------------

bool DGButton2::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	static const double zpos[2] = {0, 0.004};
	if (state != vstate) {
		int i;
		double dz = zpos[state]-zpos[vstate];
		VECTOR3 shift = ax*dz;
		float dsx = (float)shift.x, dsy = (float)shift.y, dsz = (float)shift.z;

		// animate button
		NTVERTEX vtx[nvtx];
		WORD vperm[nvtx];
		for (i = 0; i < nvtx; i++) vperm[i] = (WORD)(vtxofs+i);
		GROUPREQUESTSPEC grs = {vtx, nvtx, vperm, 0, 0, 0, 0, 0};
		oapiGetMeshGroup (hMesh, mgrp, &grs);
		for (i = 0; i < nvtx; i++) {
			vtx[i].x += dsx;
			vtx[i].y += dsy;
			vtx[i].z += dsz;
		}
		GROUPEDITSPEC ges = {GRPEDIT_VTXCRD, 0, vtx, nvtx, vperm};
		oapiEditMeshGroup (hMesh, mgrp, &ges);

		vstate = state;
	}
	return false;
}

// --------------------------------------------------------------

void DGButton2::SetState (State newstate)
{
	if (newstate != state)
		state = newstate;
}

// ==============================================================

const int DGButton3::nvtx = 20;
const int DGButton3::nvtx_lbl = 8;

// --------------------------------------------------------------

DGButton3::DGButton3 (VESSEL3 *v)
: PanelElement(v)
{
	state = vstate = OFF;
}

// --------------------------------------------------------------

void DGButton3::DefineAnimation2D (DWORD meshgrp, DWORD vofs)
{
	mgrp = meshgrp;
	vtxofs = vofs;
}

// --------------------------------------------------------------

void DGButton3::DefineAnimationVC (const VECTOR3 &axis, DWORD meshgrp, DWORD meshgrp_label,
	DWORD vofs, DWORD vofs_label)
{
	mgrp = meshgrp;
	mgrp_lbl = meshgrp_label;
	vtxofs = vofs;
	vtxofs_lbl = vofs_label;
	ax = axis;
}

// --------------------------------------------------------------

void DGButton3::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, mgrp);
}

// --------------------------------------------------------------

void DGButton3::ResetVC (DEVMESHHANDLE hMesh)
{
	vstate = OFF;
}

// --------------------------------------------------------------

bool DGButton3::ProcessMouse2D (int event, int mx, int my)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		state = (state == OFF ? PRESSED_FROM_OFF : PRESSED_FROM_ON);
	} else if (event & PANEL_MOUSE_LBUP) {
		state = (state == PRESSED_FROM_OFF ? ON : OFF);
	}
	return (state != vstate);
}

// --------------------------------------------------------------

bool DGButton3::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		state = (state == OFF ? PRESSED_FROM_OFF : PRESSED_FROM_ON);
	} else if (event & PANEL_MOUSE_LBUP) {
		state = (state == PRESSED_FROM_OFF ? ON : OFF);
	}
	return (state != vstate);
}

// --------------------------------------------------------------

bool DGButton3::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	static const double zpos[4] = {0, 0.005, 0.0065, 0.0065};
	if (state != vstate) {
		int i;
		double dz = zpos[state]-zpos[vstate];
		VECTOR3 shift = ax*dz;
		float dsx = (float)shift.x, dsy = (float)shift.y, dsz = (float)shift.z;

		// animate button
		NTVERTEX vtx[nvtx];
		WORD vperm[nvtx];
		for (i = 0; i < nvtx; i++) vperm[i] = (WORD)(vtxofs+i);
		GROUPREQUESTSPEC grs = {vtx, nvtx, vperm, 0, 0, 0, 0, 0};
		oapiGetMeshGroup (hMesh, mgrp, &grs);
		for (i = 0; i < nvtx; i++) {
			vtx[i].x += dsx;
			vtx[i].y += dsy;
			vtx[i].z += dsz;
		}
		GROUPEDITSPEC ges = {GRPEDIT_VTXCRD, 0, vtx, nvtx, vperm};
		oapiEditMeshGroup (hMesh, mgrp, &ges);

		// animate label
		NTVERTEX vtx_lbl[nvtx_lbl];
		WORD vperm_lbl[nvtx_lbl];
		for (i = 0; i < nvtx_lbl; i++) vperm_lbl[i] = (WORD)(vtxofs_lbl+i);
		GROUPREQUESTSPEC grs_lbl = {vtx_lbl, nvtx_lbl, vperm_lbl, 0, 0, 0, 0, 0};
		oapiGetMeshGroup (hMesh, mgrp_lbl, &grs_lbl);
		for (i = 0; i < nvtx_lbl; i++) {
			vtx_lbl[i].x += dsx;
			vtx_lbl[i].y += dsy;
			vtx_lbl[i].z += dsz;
		}

		// show/hide indicator
		DWORD ges_flag = GRPEDIT_VTXCRD;
		bool have_ind = (vstate != OFF);
		bool need_ind = (state != OFF);
		if (have_ind != need_ind) {
			vtx_lbl[6].tv = vtx_lbl[7].tv = (float)((need_ind ? 1.5:10.5)/1024.0);
			ges_flag |= GRPEDIT_VTXTEXV;
		}
		GROUPEDITSPEC ges_lbl = {ges_flag, 0, vtx_lbl, nvtx_lbl, vperm_lbl};
		oapiEditMeshGroup (hMesh, mgrp_lbl, &ges_lbl);

		vstate = state;
	}
	return false;
}

// --------------------------------------------------------------

void DGButton3::SetState (State newstate)
{
	if (newstate != state)
		state = newstate;
}