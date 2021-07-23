// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// RcsSubsys.cpp
// Reaction control subsystem: lin/rot selection, attitude programs
// ==============================================================

#define STRICT 1
#include "RcsSubsys.h"
#include "meshres_p0.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

// ==============================================================
// Reaction control subsystem
// ==============================================================

RcsSubsystem::RcsSubsystem (DeltaGlider *dg)
: DGSubsystem (dg)
{
	// create component instances
	AddSubsystem (modeselector = new RcsModeSelector (this));

	ELID_PROGBUTTONS = AddElement (progbuttons = new RcsProgButtons(this));
}

// --------------------------------------------------------------

RcsSubsystem::~RcsSubsystem ()
{
}

// --------------------------------------------------------------

void RcsSubsystem::SetMode (int mode)
{
	modeselector->SetMode (mode);
}

// --------------------------------------------------------------

void RcsSubsystem::SetProg (int prog, bool active)
{
	progbuttons->SetMode (prog, active);
	DG()->TriggerRedrawArea (0, 0, ELID_PROGBUTTONS);
}

// --------------------------------------------------------------

bool RcsSubsystem::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	// RCS program buttons
	DG()->RegisterPanelArea (hPanel, ELID_PROGBUTTONS, _R(1124,62,1272,110), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN, 0, progbuttons);

	return DGSubsystem::clbkLoadPanel2D (panelid, hPanel, viewW, viewH);
}

// --------------------------------------------------------------

bool RcsSubsystem::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Navmode indicator/selector on the top right of the front panel
	oapiVCRegisterArea (ELID_PROGBUTTONS, PANEL_REDRAW_USER | PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_PROGBUTTONS, VC_NAV_BUTTONS_mousearea[0], VC_NAV_BUTTONS_mousearea[1], VC_NAV_BUTTONS_mousearea[2], VC_NAV_BUTTONS_mousearea[3]);
	{
		static DWORD navbtn_vofs[6] = {VC_BTN_NAVMODE_1_vofs, VC_BTN_NAVMODE_2_vofs, VC_BTN_NAVMODE_3_vofs,
			                           VC_BTN_NAVMODE_4_vofs, VC_BTN_NAVMODE_5_vofs, VC_BTN_NAVMODE_6_vofs}; 
		static DWORD navlbl_vofs[6] = {VC_BTN_NAVMODE_1_LABEL_vofs, VC_BTN_NAVMODE_2_LABEL_vofs, VC_BTN_NAVMODE_3_LABEL_vofs,
			                           VC_BTN_NAVMODE_4_LABEL_vofs, VC_BTN_NAVMODE_5_LABEL_vofs, VC_BTN_NAVMODE_6_LABEL_vofs};
		progbuttons->DefineAnimationsVC (VC_BTN_NAVMODE_1_axis, GRP_BUTTON3_VC, GRP_LIT_SURF_VC, navbtn_vofs, navlbl_vofs);
	}

	return DGSubsystem::clbkLoadVC (vcid);
}


// ==============================================================
// Control selector dial
// ==============================================================

RcsModeSelector::RcsModeSelector (RcsSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ELID_DIAL = AddElement (dial = new RcsModeDial (this));
}

// --------------------------------------------------------------

int RcsModeSelector::GetMode () const
{
	return DG()->GetAttitudeMode();
}

// --------------------------------------------------------------

void RcsModeSelector::SetMode (int mode)
{
	int curmode = GetMode();
	if (curmode != mode) DG()->SetAttitudeMode (mode);
	DG()->TriggerRedrawArea (0, 0, ELID_DIAL);
}

// --------------------------------------------------------------

bool RcsModeSelector::IncMode ()
{
	int mode = GetMode();
	if (mode < 2) {
		SetMode (mode+1);
		return true;
	} else return false;
}

// --------------------------------------------------------------

bool RcsModeSelector::DecMode ()
{
	int mode = GetMode();
	if (mode) {
		SetMode (mode-1);
		return true;
	} else return false;
}

// --------------------------------------------------------------

bool RcsModeSelector::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	// RCS mode dial
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_DIAL, _R(100, 69,140,113), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN, 0, dial);

	return true;
}

// --------------------------------------------------------------

bool RcsModeSelector::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// RCS mode dial
	oapiVCRegisterArea (ELID_DIAL, PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_DIAL, VC_RCS_DIAL_mousearea[0], VC_RCS_DIAL_mousearea[1], VC_RCS_DIAL_mousearea[2], VC_RCS_DIAL_mousearea[3]);
	dial->DefineAnimationVC (VC_RCS_DIAL_ref, VC_RCS_DIAL_axis, GRP_DIAL1_VC, VC_RCS_DIAL_vofs);

	return true;
}


// ==============================================================
// Mode dial
// ==============================================================

RcsModeDial::RcsModeDial (RcsModeSelector *comp)
: DGDial1(comp->DG(), 3, -50*RAD, 50*RAD), component(comp)
{
}

// --------------------------------------------------------------

void RcsModeDial::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 0;
}

// --------------------------------------------------------------

void RcsModeDial::ResetVC (DEVMESHHANDLE hMesh)
{
	DGDial1::ResetVC (hMesh);
	DWORD mode = component->DG()->GetAttitudeMode();
	SetPosition (mode);
}

// --------------------------------------------------------------

bool RcsModeDial::Redraw2D (SURFHANDLE surf)
{
	// constants for texture coordinates
	static const float texw = (float)PANEL2D_TEXW; // texture width
	static const float texh = (float)PANEL2D_TEXH; // texture height
	static const float tx_x0 = 1160.5f;            // left edge of texture block
	static const float tx_y0 = texh-615.5f;        // top edge of texture block
	static const float tx_dx = 39.0f;              // texture block width
	static const float tx_dy = 43.0f;              // texture block height
	static float tu[4] = {tx_x0/texw,(tx_x0+tx_dx)/texw,tx_x0/texw,(tx_x0+tx_dx)/texw};

	float dtu = (float)(component->DG()->GetAttitudeMode()*40.0)/texw;
	for (int i = 0; i < 4; i++)
		grp->Vtx[vtxofs+i].tu = tu[i]+dtu;
	return false;
}

// --------------------------------------------------------------

bool RcsModeDial::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	int pos = component->GetMode();
	SetPosition(pos);
	return DGDial1::RedrawVC (hMesh, surf);
}

// --------------------------------------------------------------

bool RcsModeDial::ProcessMouse2D (int event, int mx, int my)
{
	return (mx < 20 ? component->DecMode() : component->IncMode());
}

// --------------------------------------------------------------

bool RcsModeDial::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGDial1::ProcessMouseVC (event, p)) {
		int pos = GetPosition();
		component->SetMode (pos);
		return true;
	}
	return false;
}


// ==============================================================
// RCS program buttons
// ==============================================================

RcsProgButtons::RcsProgButtons (RcsSubsystem *_subsys)
: PanelElement (_subsys->DG()), subsys(_subsys)
{
	for (int i = 0; i < 6; i++)
		btn[i] = new DGButton3 (subsys->DG());
}

// --------------------------------------------------------------

RcsProgButtons::~RcsProgButtons ()
{
	for (int i = 0; i < 6; i++)
		delete btn[i];
}

// --------------------------------------------------------------

void RcsProgButtons::SetMode (int mode, bool active)
{
	static int modebtn[6] = {0,5,2,1,4,3};
	int b = modebtn[mode-1];
	if (active) {
		if (btn[b]->GetState () == DGButton3::OFF)
			btn[b]->SetState (DGButton3::ON);
	} else {
		btn[b]->SetState (DGButton3::OFF);
	}
}
// --------------------------------------------------------------

void RcsProgButtons::DefineAnimationsVC (const VECTOR3 &axis, DWORD meshgrp, DWORD meshgrp_label,
	DWORD vofs[6], DWORD vofs_label[6])
{
	for (int i = 0; i < 6; i++) 
		btn[i]->DefineAnimationVC (axis, meshgrp, meshgrp_label, vofs[i], vofs_label[i]);
}

// --------------------------------------------------------------

void RcsProgButtons::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 20;
}

// --------------------------------------------------------------

void RcsProgButtons::ResetVC (DEVMESHHANDLE hMesh)
{
	for (int i = 0; i < 6; i++)
		btn[i]->ResetVC (hMesh);
}

// --------------------------------------------------------------

bool RcsProgButtons::Redraw2D (SURFHANDLE)
{
	// constants for texture coordinates
	static const float texh = (float)PANEL2D_TEXH; // texture height
	static const float tx_y0 = texh-597.0f;        // top edge of texture block
	static const float tx_dy = 24.0f;              // texture block height
	static const float tv0_active = (tx_y0)/texh, tv1_active = (tx_y0+tx_dy)/texh;
	static const float tv0_idle = (tx_y0+tx_dy+0.5f)/texh, tv1_idle = (tx_y0+tx_dy+0.5f)/texh;
	float tv0, tv1;
	int vofs;

	for (DWORD i = NAVMODE_KILLROT; i <= NAVMODE_ANTINORMAL; i++) {
		if (subsys->DG()->GetNavmodeState (i)) tv0 = tv0_active, tv1 = tv1_active;
		else                             tv0 = tv0_idle,   tv1 = tv1_idle;
		vofs = vtxofs+(i-NAVMODE_KILLROT)*4;
		grp->Vtx[vofs+0].tv = grp->Vtx[vofs+1].tv = tv0;
		grp->Vtx[vofs+2].tv = grp->Vtx[vofs+3].tv = tv1;
	}
		
	return false;
}

// --------------------------------------------------------------

bool RcsProgButtons::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	for (int i = 0; i < 6; i++)
		btn[i]->RedrawVC (hMesh, surf);
	return false;
}

// --------------------------------------------------------------

bool RcsProgButtons::ProcessMouse2D (int event, int mx, int my)
{
	static const int navmode[2][4] = {
		{0,NAVMODE_PROGRADE,NAVMODE_NORMAL,0},
		{NAVMODE_KILLROT,NAVMODE_RETROGRADE,NAVMODE_ANTINORMAL,NAVMODE_HLEVEL}
	};
	int mode = navmode[my/24][mx/37];
	if (mode) subsys->DG()->ToggleNavmode (mode);
	return (mode != 0);
}

// --------------------------------------------------------------

bool RcsProgButtons::ProcessMouseVC (int event, VECTOR3 &p)
{
	static int modemap[2][4] = {{1,4,6,2},{0,3,5,0}};
	static int btnmode[6] = {1,4,3,6,5,2};
	static int modebtn[6] = {0,5,2,1,4,3};
	int ix = (int)(p.x*169.0);
	int iy = (int)(p.y*63);
	int br = ix/43;
	int bc = iy/33;
	if (ix-br*43 >= 40) return false;
	if (iy-bc*33 >= 30) return false;
	int mode = modemap[bc][br];
	if (!mode) return false;
	int b = modebtn[mode-1];
	int i;

	if (event & PANEL_MOUSE_LBDOWN) {
		for (i = 0; i < 6; i++) {
			if (i==b) {
				btn[i]->SetState (btn[i]->GetState() == DGButton3::OFF ? DGButton3::PRESSED_FROM_OFF : DGButton3::PRESSED_FROM_ON);
			} else {
				bool ison = subsys->DG()->GetNavmodeState (btnmode[i]);
				btn[i]->SetState (ison ? DGButton3::ON : DGButton3::OFF);
			}
		}
		subsys->DG()->ToggleNavmode (mode);
	} else if (event & PANEL_MOUSE_LBUP) {
		btn[b]->SetState (btn[b]->GetState() == DGButton3::PRESSED_FROM_OFF ? DGButton3::ON : DGButton3::OFF);
	}
	return true;
}

