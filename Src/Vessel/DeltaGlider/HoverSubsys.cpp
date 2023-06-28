// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// HoverSubsys.cpp
// Hover engine control subsystem: manual input and automatic modes
// ==============================================================

#define STRICT 1
#include "HoverSubsys.h"
#include "meshres_p0.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

using std::min;
using std::max;

// constants for texture coordinates
static const float texw = (float)PANEL2D_TEXW; // texture width

// ==============================================================
// Hover control subsystem
// ==============================================================

HoverSubsystem::HoverSubsystem (DeltaGlider *dg)
: DGSubsystem (dg)
{
	// create the subsystem components
	AddSubsystem (attctrl = new HoverAttitudeComponent (this));
	AddSubsystem (holdctrl = new HoverHoldComponent (this));
	AddSubsystem (manctrl = new HoverManualComponent (this));

	for (int i = 0; i < 3; i++)
		hoverlevel[i] = 0.0;
}

// --------------------------------------------------------------

void HoverSubsystem::IncGroupLevel (double dlvl)
{
	for (int i = 0; i < 3; i++)
		hoverlevel[i] = max (0.0, min (1.0, hoverlevel[i]+dlvl));
}

// --------------------------------------------------------------

void HoverSubsystem::ActivateHold (bool active)
{
	holdctrl->Activate (active);
}

// --------------------------------------------------------------

void HoverSubsystem::clbkPostStep (double simt, double simdt, double mjd)
{
	int i;
	double hoverlevel_cur[3];

	for (i = 0; i < 3; i++)
		hoverlevel[i] = hoverlevel_cur[i] = DG()->GetHoverThrusterLevel (i);

	DGSubsystem::clbkPostStep (simt, simdt, mjd);

	for (i = 0; i < 3; i++)
		if (hoverlevel[i] != hoverlevel_cur[i])
			DG()->SetHoverThrusterLevel (i, hoverlevel[i]);
}

// --------------------------------------------------------------

void HoverSubsystem::clbkReset2D (int panelid, MESHHANDLE hMesh)
{
	if (panelid != 0) return;
	DGSubsystem::clbkReset2D (panelid, hMesh);
}

// --------------------------------------------------------------

void HoverSubsystem::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	if (vcid != 0) return;
	DGSubsystem::clbkResetVC (vcid, hMesh);
}


// ==============================================================
// Base class for hover subsystem components
// ==============================================================

HoverSubsystemComponent::HoverSubsystemComponent (HoverSubsystem *_subsys)
: DGSubsystem(_subsys)
{
}


// ==============================================================
// Automatic hover attitude balance submode
// ==============================================================

HoverAttitudeComponent::HoverAttitudeComponent (HoverSubsystem *_subsys)
: HoverSubsystemComponent(_subsys)
{
	mode = 0;

	phover = phover_cmd = 0.0;
	rhover = rhover_cmd = 0.0;

	ELID_MODEDIAL     = AddElement (modedial     = new HoverCtrlDial (this));
	ELID_PHOVERSWITCH = AddElement (phoverswitch = new PHoverCtrl (this));
	ELID_RHOVERSWITCH = AddElement (rhoverswitch = new RHoverCtrl (this));
	ELID_DISPLAY      = AddElement (hoverdisp    = new HoverDisp (this));
}

// --------------------------------------------------------------

bool HoverAttitudeComponent::IncPHover (int dir)
{
	if (dir && mode == 2) {
		const double cmd_speed = 0.5;
		double dcmd = oapiGetSimStep() * cmd_speed * PHOVER_RANGE * (dir == 1 ? -1.0:1.0);
		phover_cmd = min (PHOVER_RANGE, max (-PHOVER_RANGE, phover_cmd+dcmd));
	}
	return true;
}

// --------------------------------------------------------------

bool HoverAttitudeComponent::IncRHover (int dir)
{
	if (dir && mode == 2) {
		const double cmd_speed = 0.5;
		double dcmd = oapiGetSimStep() * cmd_speed * RHOVER_RANGE * (dir == 1 ? -1.0:1.0);
		rhover_cmd = min (RHOVER_RANGE, max (-RHOVER_RANGE, rhover_cmd+dcmd));
	}
	return true;
}

// --------------------------------------------------------------

void HoverAttitudeComponent::AutoHoverAtt ()
{
	double lvl;

	// Pitch command
	lvl = -DG()->GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ROTMODE);
	if (!lvl) lvl = DG()->GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ROTMODE);
	phover_cmd = lvl*PHOVER_RANGE;

	// Roll command
	lvl = -DG()->GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ROTMODE);
	if (!lvl) lvl = DG()->GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ROTMODE);
	rhover_cmd = lvl*RHOVER_RANGE;
}

// --------------------------------------------------------------

void HoverAttitudeComponent::TrackHoverAtt ()
{
	if (mode) {
		phover = DG()->GetPitch();
		rhover = DG()->GetBank();
		const double fb_scale = 3.0/4.55; // scaling between front and back hovers (distance from CG)
		double Lf = HoverSubsys()->GetThrusterLevel(0);
		double Ll = HoverSubsys()->GetThrusterLevel(1);
		double Lr = HoverSubsys()->GetThrusterLevel(2);
		double Lb = (Ll+Lr)*0.5;
		double Lm = (Lf+Lb)*0.5;
		double Tf = Lf;
		double Tb = Lb*fb_scale;
		double Tm = (Tf+Tb)*0.5;
		if (fabs(phover) <= MAX_AUTO_HOVER_ATT && fabs(rhover) <= MAX_AUTO_HOVER_ATT) { // within control range
			const double p_alpha = 0.2;
			const double p_beta = 0.6;
			const double r_alpha = 0.2;
			const double r_beta = 0.6;
			double dp = phover - phover_cmd;
			double dr = rhover - rhover_cmd;
			VECTOR3 avel;
			DG()->GetAngularVel(avel);
			double dpv =  avel.x;
			double drv = -avel.z;
			double balance_fb = -p_alpha*dp - p_beta*dpv;
			double balance_lr = -r_alpha*dr - r_beta*drv;
			if (Lf || Lb) {
				// front/back balance
				double Lf_cmd = Lm+balance_fb;
				double Lb_cmd = Lm-balance_fb;
				double D = (Lf_cmd-Lf + (Lb_cmd-Lb)*fb_scale)/(1.0+fb_scale);
				Lf_cmd -= D;
				Lb_cmd -= D;

				double Lmin = min (Lf_cmd, Lb_cmd);
				double Lmax = max (Lf_cmd, Lb_cmd);
				if (Lmin < 0.0) {
					if (Lf_cmd < 0.0) Lf_cmd = 0.0, Lb_cmd += Lmin/fb_scale;
					else              Lb_cmd = 0.0, Lf_cmd += Lmin*fb_scale;
				}
				if (Lmax > 1.0) {
					if (Lf_cmd > 1.0) Lf_cmd = 1.0, Lb_cmd += (Lmax-1.0)/fb_scale;
					else              Lb_cmd = 1.0, Lf_cmd += (Lmax-1.0)*fb_scale;
				}
				// left/right balance
				double Ll_cmd = Lb_cmd-balance_lr;
				double Lr_cmd = Lb_cmd+balance_lr;
				Lmin = min (Ll_cmd, Lr_cmd);
				Lmax = max (Ll_cmd, Lr_cmd);
				if (Lmin < 0.0) {
					if (Ll_cmd < 0.0) Ll_cmd = 0.0, Lr_cmd += Lmin;
					else              Lr_cmd = 0.0, Ll_cmd += Lmin;
				}
				if (Lmax > 1.0) {
					if (Ll_cmd > 1.0) Ll_cmd = 1.0, Lr_cmd += Lmax-1.0;
					else              Lr_cmd = 1.0, Ll_cmd += Lmax-1.0;
				}
				HoverSubsys()->SetThrusterLevel (0, Lf_cmd);
				HoverSubsys()->SetThrusterLevel (1, Ll_cmd);
				HoverSubsys()->SetThrusterLevel (2, Lr_cmd);
			}
		} else {
			double L_cmd = 2.0*Tm / (1.0 + fb_scale);
			for (int i = 0; i < 3; i++)
				HoverSubsys()->SetThrusterLevel (i, L_cmd);
		}
	} else {
		phover = rhover = phover_cmd = rhover_cmd = 0.0;
	}
	DG()->TriggerRedrawArea (0, 0, ELID_DISPLAY);
}

// --------------------------------------------------------------

void HoverAttitudeComponent::clbkSaveState (FILEHANDLE scn)
{
	if (mode) {
		if (mode == 1) { // auto
			oapiWriteScenario_int (scn, (char*)"HOVERMODE", mode);
		} else { // manual
			char cbuf[256];
			sprintf (cbuf, "%d %0.3lf %0.3lf", mode, phover_cmd, rhover_cmd);
			oapiWriteScenario_string (scn, (char*)"HOVERMODE", cbuf);
		}
	}
}

// --------------------------------------------------------------

bool HoverAttitudeComponent::clbkParseScenarioLine (const char *line)
{
	if (!_strnicmp (line, "HOVERMODE", 9)) {
		double ph, rh;
		int n = sscanf (line+9, "%d%lf%lf", &mode, &ph, &rh);
		if (mode == 2 && n == 3) // copy manual settings
			phover_cmd = ph, rhover_cmd = rh;
		return true;
	}
	return false;
}

// --------------------------------------------------------------

void HoverAttitudeComponent::clbkPostStep (double simt, double simdt, double mjd)
{
	if (mode == 1) AutoHoverAtt();
	TrackHoverAtt();
}

// --------------------------------------------------------------

bool HoverAttitudeComponent::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);

	// Hover control dial
	DG()->RegisterPanelArea (hPanel, ELID_MODEDIAL,     _R(356,426,396,470), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN, panel2dtex, modedial);

	// Hover manual switches
	DG()->RegisterPanelArea (hPanel, ELID_PHOVERSWITCH, _R(436,434,452,478), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP, panel2dtex, phoverswitch);
	phoverswitch->DefineAnimation2D (DGSwitch2::VERT, GRP_INSTRUMENTS_ABOVE_P0, 160);
	DG()->RegisterPanelArea (hPanel, ELID_RHOVERSWITCH, _R(423,513,467,529), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP, panel2dtex, rhoverswitch);
	rhoverswitch->DefineAnimation2D (DGSwitch2::HORZ, GRP_INSTRUMENTS_ABOVE_P0, 164);

	// Hover balance display
	DG()->RegisterPanelArea (hPanel, ELID_DISPLAY,      _R( 0,  0, 0,  0), PANEL_REDRAW_USER,   PANEL_MOUSE_IGNORE, panel2dtex, hoverdisp);

	return true;
}

// --------------------------------------------------------------

bool HoverAttitudeComponent::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Hover control dial
	oapiVCRegisterArea (ELID_MODEDIAL, PANEL_REDRAW_USER | PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_MODEDIAL, VC_HOVER_DIAL_mousearea[0], VC_HOVER_DIAL_mousearea[1], VC_HOVER_DIAL_mousearea[2], VC_HOVER_DIAL_mousearea[3]);
	modedial->DefineAnimationVC (VC_HOVER_DIAL_ref, VC_HOVER_DIAL_axis, GRP_DIAL1_VC, VC_HOVER_DIAL_vofs);

	// Hover manual switches
	oapiVCRegisterArea (ELID_PHOVERSWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_PHOVERSWITCH, VC_HOVER_PSWITCH_mousearea[0], VC_HOVER_PSWITCH_mousearea[1], VC_HOVER_PSWITCH_mousearea[2], VC_HOVER_PSWITCH_mousearea[3]);
	phoverswitch->DefineAnimationVC (VC_HOVER_PSWITCH_ref, VC_HOVER_PSWITCH_axis, GRP_SWITCH2_VC, VC_HOVER_PSWITCH_vofs);
	oapiVCRegisterArea (ELID_RHOVERSWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_RHOVERSWITCH, VC_HOVER_RSWITCH_mousearea[0], VC_HOVER_RSWITCH_mousearea[1], VC_HOVER_RSWITCH_mousearea[2], VC_HOVER_RSWITCH_mousearea[3]);
	rhoverswitch->DefineAnimationVC (VC_HOVER_RSWITCH_ref, VC_HOVER_RSWITCH_axis, GRP_SWITCH2_VC, VC_HOVER_RSWITCH_vofs);

	// Hover balance display
	oapiVCRegisterArea (ELID_DISPLAY, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);

	return true;
}


// ==============================================================
// Automatic hover hold altitude/vspeed submode
// ==============================================================

HoverHoldComponent::HoverHoldComponent (HoverSubsystem *_subsys)
: HoverSubsystemComponent(_subsys)
{
	extern GDIParams g_Param;

	holdalt   = 0.0;
	holdvspd  = 0.0;
	active = false;
	altmode = ALTMODE_GROUND;
	hovermode = HOLD_ALT;

	ELID_DISPLAY     = AddElement (holddisp = new HoverHoldAltIndicator(this, g_Param.surf));
	ELID_HOLDBTN     = AddElement (holdbtn  = new HoverAltBtn (this));
	ELID_ALTSET      = AddElement (altset   = new HoverAltSwitch (this));
	ELID_ALTRESET    = AddElement (altreset = new HoverAltResetBtn (this));
	ELID_MODEBUTTONS = AddElement (modebuttons = new HoverAltModeButtons (this));
}

// --------------------------------------------------------------

void HoverHoldComponent::Activate (bool ison)
{
	if (ison != active) {
		active = ison;

		holdbtn->SetState(active ? DGButton3::ON : DGButton3::OFF);
		DG()->TriggerRedrawArea (0, 0, ELID_HOLDBTN);

		if (hovermode == HOLD_ALT) {  // use default VESSEL method for altitude hold
			if (active) DG()->SetHoverHoldAltitude (holdalt, altmode==ALTMODE_GROUND);
			else        DG()->DeactivateNavmode (NAVMODE_HOLDALT);
		} else {
			// vspd hold is implemented below
			if (active) { // initial conditions
				holdT = oapiGetSimTime();
				VECTOR3 v;
				DG()->GetHorizonAirspeedVector(v);
				pvh = v.y;
			}
		}
	}
}

// --------------------------------------------------------------

void HoverHoldComponent::SetTargetAlt (double alt)
{
	holdalt = alt;
	if (active) DG()->SetHoverHoldAltitude (holdalt, altmode==ALTMODE_GROUND);
}

// --------------------------------------------------------------

void HoverHoldComponent::SetTargetVspd (double vspd)
{
	holdvspd = vspd;
}

// --------------------------------------------------------------

void HoverHoldComponent::SetTargetPrm (double prm)
{
	if (hovermode == HOLD_ALT)
		SetTargetAlt (prm);
	else
		SetTargetVspd (prm);
}

// --------------------------------------------------------------

void HoverHoldComponent::SetTargetAltCurrent ()
{
	SetTargetAlt (DG()->GetAltitude (altmode));
}

// --------------------------------------------------------------

void HoverHoldComponent::SetHoverMode (HoverMode mode)
{
	if (mode != hovermode) {
		if (active) {
			Activate (false);
			if (mode == HOLD_ALT) {
				// if switching directly from active VSPD to active ALT mode,
				// set current altitude as target
				SetTargetAltCurrent();
			} else if (mode == HOLD_VSPD) {
				// if switching directly from active ALT to active VSPD mode,
				// set target vspd to current vspd
				VECTOR3 v;
				DG()->GetHorizonAirspeedVector(v);
				SetTargetVspd (v.y);
			}
			hovermode = mode;
			Activate (true);
		} else
			hovermode = mode;
	}
}

// --------------------------------------------------------------

void HoverHoldComponent::HoverHoldVspd (double vh_tgt)
{
	double t = oapiGetSimTime();
	double dt = t - holdT;
	if (!dt) return;
	holdT = t;

	VECTOR3 v;
	DG()->GetHorizonAirspeedVector(v);
	double vh = v.y;
	double ah = (vh-pvh)/dt;
	pvh = vh;

	double dvh = vh_tgt-vh;
	double a_tgt = dvh;
	double da = a_tgt - ah;
	double a_max = DG()->GetMaxHoverThrust()/DG()->GetMass();
	double dlvl = da/a_max;
	double dlvl_max = dt;
	if (fabs(dlvl) > dlvl_max)
		dlvl = (dlvl > 0.0 ? dlvl_max : -dlvl_max);

	HoverSubsys()->IncGroupLevel (dlvl);
}

// --------------------------------------------------------------

void HoverHoldComponent::clbkSaveState (FILEHANDLE scn)
{
	char cbuf[256];
	sprintf (cbuf, "%d %d %0.4le %0.4le", (int)active, (int)hovermode, holdalt, holdvspd);
	oapiWriteScenario_string (scn, (char*)"HOVERHOLD", cbuf);
}

// --------------------------------------------------------------

bool HoverHoldComponent::clbkParseScenarioLine (const char *line)
{
	if (!_strnicmp (line, "HOVERHOLD", 9)) {
		int iact, imode;
		double alt, vspd;
		int n = sscanf (line+9, "%d%d%lf%lf", &iact, &imode, &alt, &vspd);
		if (n == 4) {
			hovermode = (HoverMode)imode;
			holdalt = alt;
			holdvspd = vspd;
			if (iact) Activate (true);
		}
		return true;
	}
	return false;
}

// --------------------------------------------------------------

void HoverHoldComponent::clbkPostStep (double simt, double simdt, double mjd)
{
	// vertical speed hover autopilot
	if (active && hovermode == HOLD_VSPD)
		HoverHoldVspd (holdvspd);
}

// --------------------------------------------------------------

bool HoverHoldComponent::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);

	// readouts
	DG()->RegisterPanelArea (hPanel, ELID_DISPLAY, _R(486,442,543,466), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, panel2dtex, holddisp);

	// Hover hold activate button
	DG()->RegisterPanelArea (hPanel, ELID_HOLDBTN, _R(491,415,533,445), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP, panel2dtex, holdbtn);
	holdbtn->DefineAnimation2D (GRP_INSTRUMENTS_ABOVE_P0, 172);

	// Hover hold select switch
	DG()->RegisterPanelArea (hPanel, ELID_ALTSET, _R(484,491,528,507), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP, panel2dtex, altset);
	altset->DefineAnimation2D (DGSwitch2::HORZ, GRP_INSTRUMENTS_ABOVE_P0, 168);

	// Hover hold reset button
	DG()->RegisterPanelArea (hPanel, ELID_ALTRESET, _R(532,492,546,506), PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP, panel2dtex, altreset);

	// Hover hold mode selector buttons
	DG()->RegisterPanelArea (hPanel, ELID_MODEBUTTONS, _R(487,520,541,544), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN, panel2dtex, modebuttons);
	modebuttons->DefineAnimation2D (GRP_INSTRUMENTS_ABOVE_P0, 176);

	return true;
}

// --------------------------------------------------------------

bool HoverHoldComponent::clbkLoadVC (int vcid)
{
	switch (vcid) {
	case 0: // VC pilot position
		// readouts
		oapiVCRegisterArea (ELID_DISPLAY, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);

		// Hover hold activate button
		oapiVCRegisterArea (ELID_HOLDBTN, PANEL_REDRAW_MOUSE | PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP);
		oapiVCSetAreaClickmode_Spherical (ELID_HOLDBTN, VC_BTN_HOVER_HOLDALT_ref, VC_BTN_HOVER_HOLDALT_mouserad);
		holdbtn->DefineAnimationVC (VC_BTN_HOVER_HOLDALT_axis, GRP_BUTTON3_VC, GRP_LIT_SURF_VC, VC_BTN_HOVER_HOLDALT_vofs, VC_BTN_HOVER_HOLDALT_LABEL_vofs);

		// Hover hold select switch
		oapiVCRegisterArea (ELID_ALTSET, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
		oapiVCSetAreaClickmode_Quadrilateral (ELID_ALTSET, VC_HOVER_HOLDALT_SWITCH_mousearea[0], VC_HOVER_HOLDALT_SWITCH_mousearea[1], VC_HOVER_HOLDALT_SWITCH_mousearea[2], VC_HOVER_HOLDALT_SWITCH_mousearea[3]);
		altset->DefineAnimationVC (VC_HOVER_HOLDALT_SWITCH_ref, VC_HOVER_HOLDALT_SWITCH_axis, GRP_SWITCH2_VC, VC_HOVER_HOLDALT_SWITCH_vofs);

		// Hover hold reset button
		oapiVCRegisterArea (ELID_ALTRESET, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP);
		oapiVCSetAreaClickmode_Spherical (ELID_ALTRESET, VC_BTN_HOVER_HOLDALT_CUR_ref, VC_BTN_HOVER_HOLDALT_CUR_mouserad);
		altreset->DefineAnimationVC (VC_BTN_HOVER_HOLDALT_CUR_axis, GRP_BUTTON2_VC, VC_BTN_HOVER_HOLDALT_CUR_vofs);

		// Hover mode selector buttons
		oapiVCRegisterArea (ELID_MODEBUTTONS, PANEL_REDRAW_USER | PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP);
		oapiVCSetAreaClickmode_Quadrilateral (ELID_MODEBUTTONS, VC_HOVERMODE_BUTTONS_mousearea[0], VC_HOVERMODE_BUTTONS_mousearea[1], VC_HOVERMODE_BUTTONS_mousearea[2], VC_HOVERMODE_BUTTONS_mousearea[3]);
		{
			static DWORD hoverbtn_vofs[2] = {VC_BTN_HOVERMODE_1_vofs,VC_BTN_HOVERMODE_2_vofs};
			static DWORD hoverbtn_label_vofs[2] = {VC_BTN_HOVERMODE_1_LABEL_vofs, VC_BTN_HOVERMODE_2_LABEL_vofs};
			modebuttons->DefineAnimationsVC (VC_BTN_HOVERMODE_1_axis, GRP_BUTTON3_VC, GRP_LIT_SURF_VC, hoverbtn_vofs, hoverbtn_label_vofs);
		}

		break;
	}
	return true;
}


// ==============================================================
// Manual hover control submode
// ==============================================================

HoverManualComponent::HoverManualComponent (HoverSubsystem *_subsys)
: HoverSubsystemComponent(_subsys)
{
	ELID_THROTTLE = AddElement (throttle = new HoverThrottle (this));

	// Hover throttle VC animation
	static UINT HoverThrottleGrp[2] = {GRP_THROTTLE_HOVER_1_VC,GRP_THROTTLE_HOVER_2_VC};
	static MGROUP_ROTATE HoverThrottle (1, HoverThrottleGrp, 2,
		_V(-0.41,0.85,6.9226), _V(1,0,0), (float)(50*RAD));
	anim_hoverthrottle = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_hoverthrottle, 0, 1, &HoverThrottle);

}

// --------------------------------------------------------------

bool HoverManualComponent::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);

	DG()->RegisterPanelArea (hPanel, ELID_THROTTLE,  _R( 4,304, 57,444), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBPRESSED, panel2dtex, throttle);

	return true;
}

// --------------------------------------------------------------

bool HoverManualComponent::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Hover throttle
	oapiVCRegisterArea (ELID_THROTTLE, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_THROTTLE, _V(-0.44,0.87,6.81), _V(-0.35,0.87,6.81), _V(-0.44,0.95,6.91), _V(-0.35,0.95,6.91));

	return true;
}


// ==============================================================

HoverCtrlDial::HoverCtrlDial (HoverAttitudeComponent *_ctrl)
: DGDial1 (_ctrl->DG(), 3, -50*RAD, 50*RAD), ctrl(_ctrl)
{
}

// --------------------------------------------------------------

void HoverCtrlDial::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 148;
}

// --------------------------------------------------------------

void HoverCtrlDial::ResetVC (DEVMESHHANDLE hMesh)
{
	DGDial1::ResetVC (hMesh);
	int mode = ctrl->Mode();
	SetPosition (mode);
}

// --------------------------------------------------------------

bool HoverCtrlDial::Redraw2D (SURFHANDLE surf)
{
	// constants for texture coordinates
	static const float texw = (float)PANEL2D_TEXW; // texture width
	static const float texh = (float)PANEL2D_TEXH; // texture height
	static const float tx_x0 = 1160.5f;            // left edge of texture block
	static const float tx_y0 = texh-615.5f;        // top edge of texture block
	static const float tx_dx = 39.0f;              // texture block width
	static const float tx_dy = 43.0f;              // texture block height
	static float tu[4] = {tx_x0/texw,(tx_x0+tx_dx)/texw,tx_x0/texw,(tx_x0+tx_dx)/texw};

	float dtu = (float)(ctrl->Mode()*40.0)/texw;
	for (int i = 0; i < 4; i++)
		grp->Vtx[vtxofs+i].tu = tu[i]+dtu;
	return false;
}

// --------------------------------------------------------------

bool HoverCtrlDial::ProcessMouse2D (int event, int mx, int my)
{
	int mode = ctrl->Mode();

	if (mx < 20) { // dial turn left
		if (mode > 0) {
			ctrl->SetMode (mode-1);
			return true;
		}
	} else { // dial turn right
		if (mode < 2) {
			ctrl->SetMode (mode+1);
			return true;
		}
	}
	return false;
}

// --------------------------------------------------------------

bool HoverCtrlDial::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGDial1::ProcessMouseVC (event, p)) {
		int pos = GetPosition();
		ctrl->SetMode (pos);
		return true;
	}
	return false;
}


// ==============================================================

HoverDisp::HoverDisp (HoverAttitudeComponent *_ctrl)
: PanelElement(_ctrl->DG()), ctrl(_ctrl)
{
	pofs_cur = rofs_cur = 0;
	pofs_cmd = rofs_cmd = 0;
	memset (&vc_grp, 0, sizeof(GROUPREQUESTSPEC));
	for (int i = 0; i < 8; i++)
		vperm[i] = (WORD)(i+VC_HOVER_INDICATOR_vofs);
}

// --------------------------------------------------------------

HoverDisp::~HoverDisp ()
{
	if (vc_grp.Vtx) delete []vc_grp.Vtx;
}

// --------------------------------------------------------------

void HoverDisp::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 152;
}

// --------------------------------------------------------------

void HoverDisp::ResetVC (DEVMESHHANDLE hMesh)
{
	vc_grp.nVtx = 8;
	if (!vc_grp.Vtx) vc_grp.Vtx = new NTVERTEX[vc_grp.nVtx];
	if (oapiGetMeshGroup (hMesh, GRP_VC_INSTR_VC, &vc_grp) != 0) { // problems
		delete []vc_grp.Vtx;
		vc_grp.Vtx = 0;
	}
}

// --------------------------------------------------------------

bool HoverDisp::Redraw2D (SURFHANDLE surf)
{
	DeltaGlider *dg = (DeltaGlider*)vessel;
	int j, ofs;
	double g;
	const float x0 = 371.5f;
	const float y0 = 515.5f;
	const float dx =  10.0f;
	const float dy =  10.0f;

	g = max (-PHOVER_RANGE, min(PHOVER_RANGE, ctrl->PHover()));
	ofs = (int)floor((g/PHOVER_RANGE)*18+0.5);
	if (ofs != pofs_cur) {
		for (j = 0; j < 4; j++)
			grp->Vtx[vtxofs+j].y = y0 + dy*(j/2) + ofs;
		pofs_cur = ofs;
	}
	g = max (-RHOVER_RANGE, min(RHOVER_RANGE, ctrl->RHover()));
	ofs = (int)floor((g/RHOVER_RANGE)*18+0.5);
	if (ofs != rofs_cur) {
		for (j = 0; j < 4; j++)
			grp->Vtx[vtxofs+j].x = x0 + dx*(j%2) - ofs;
		rofs_cur = ofs;
	}

	g = max (-PHOVER_RANGE, min(PHOVER_RANGE, ctrl->PHover(false)));
	ofs = (int)floor((g/PHOVER_RANGE)*18+0.5);
	if (ofs != pofs_cmd) {
		for (j = 0; j < 4; j++)
			grp->Vtx[vtxofs+4+j].y = y0 + dy*(j/2) + ofs;
		pofs_cmd = ofs;
	}
	g = max (-RHOVER_RANGE, min(RHOVER_RANGE, ctrl->RHover(false)));
	ofs = (int)floor((g/RHOVER_RANGE)*18+0.5);
	if (ofs != rofs_cmd) {
		for (j = 0; j < 4; j++)
			grp->Vtx[vtxofs+4+j].x = x0 + dx*(j%2) - ofs;
		rofs_cmd = ofs;
	}

	return false;
}

// --------------------------------------------------------------

bool HoverDisp::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	const double &slope = vc_lpanel_tilt;
	const VECTOR3 &cnt = VC_HOVER_INDICATOR_ref;
	static const double cosa = cos(slope), sina = sin(slope);
	static const double indsize = 0.002586;
	static const double xrange = 0.0103/RHOVER_RANGE;
	static const double yrange = 0.0103/PHOVER_RANGE;

	DeltaGlider *dg = (DeltaGlider*)vessel;
	NTVERTEX *Vtx = vc_grp.Vtx;
	if (hMesh && Vtx) {
		int i, j;
		double dx, dy;
		float y, z;
		dx = -max (-RHOVER_RANGE, min(RHOVER_RANGE, ctrl->RHover()))*xrange;
		dy = -max (-PHOVER_RANGE, min(PHOVER_RANGE, ctrl->PHover()))*yrange;
		for (j = 0; j < 4; j++) {
			Vtx[4+j].x = cnt.x + dx + indsize*(j%2 ? 1:-1);
			Vtx[4+j].y = dy + indsize*(j/2 ? 1:-1);
		}
		dx = -ctrl->RHover(false)*xrange;
		dy = -ctrl->PHover(false)*yrange;
		for (j = 0; j < 4; j++) {
			Vtx[j].x = cnt.x + dx + indsize*(j%2 ? 1:-1);
			Vtx[j].y = dy + indsize*(j/2 ? 1:-1);
		}
		for (i = 0; i < 8; i++) {
			y = Vtx[i].y;
			z = i < 4 ? -0.0002f : -0.0004f;
			Vtx[i].y = (float)(cnt.y + y*cosa - z*sina);
			Vtx[i].z = (float)(cnt.z + y*sina + z*cosa);
		}
		GROUPEDITSPEC ges = {GRPEDIT_VTXCRD,0,vc_grp.Vtx,vc_grp.nVtx,vperm};
		oapiEditMeshGroup (hMesh, GRP_VC_INSTR_VC, &ges);

	}
	return false;
}


// ==============================================================

PHoverCtrl::PHoverCtrl (HoverAttitudeComponent *_ctrl)
: DGSwitch2(_ctrl->DG()), ctrl(_ctrl)
{
}

// --------------------------------------------------------------

bool PHoverCtrl::ProcessMouse2D (int event, int mx, int my)
{
	static int state = 0;
	if (DGSwitch2::ProcessMouse2D (event, mx, my))
		state = (int)GetState();
	ctrl->IncPHover (state);
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}

// --------------------------------------------------------------

bool PHoverCtrl::ProcessMouseVC (int event, VECTOR3 &p)
{
	static int state = 0;
	if (DGSwitch2::ProcessMouseVC (event, p))
		state = (int)GetState();
	ctrl->IncPHover (state);
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}


// ==============================================================

RHoverCtrl::RHoverCtrl (HoverAttitudeComponent *_ctrl)
: DGSwitch2(_ctrl->DG()), ctrl(_ctrl)
{
}

// --------------------------------------------------------------

bool RHoverCtrl::ProcessMouse2D (int event, int mx, int my)
{
	static int state = 0;
	if (DGSwitch2::ProcessMouse2D (event, mx, my))
		state = (int)GetState();
	ctrl->IncRHover (state);
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}

// --------------------------------------------------------------

bool RHoverCtrl::ProcessMouseVC (int event, VECTOR3 &p)
{
	static int state = 0;
	if (DGSwitch2::ProcessMouseVC (event, p))
		state = (int)GetState();
	ctrl->IncRHover (state);
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}


// ==============================================================

HoverAltBtn::HoverAltBtn (HoverHoldComponent *hhac)
: DGButton3 (hhac->DG()), ctrl(hhac)
{
}

// --------------------------------------------------------------

bool HoverAltBtn::ProcessMouse2D (int event, int mx, int my)
{
	if (DGButton3::ProcessMouse2D (event, mx, my)) {
		DGButton3::State state = GetState();
		if      (state == DGButton3::OFF) ctrl->Activate (false);
		else if (state == DGButton3::ON)  ctrl->Activate (true);
	}
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}

// --------------------------------------------------------------

bool HoverAltBtn::Redraw2D (SURFHANDLE hSurf)
{
	static const float texh = (float)PANEL2D_TEXH;
	if (state != vstate) {
		int ofs = (state == DGButton3::OFF ? 0 : 30);
		for (int i = 0; i < 4; i++)
			grp->Vtx[vtxofs+i].tv = (391+ofs+(i/2)*30)/texh;
		vstate = state;
	}
	return false;
}

// --------------------------------------------------------------

bool HoverAltBtn::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGButton3::ProcessMouseVC (event, p)) {
		DGButton3::State state = GetState();
		if      (state == DGButton3::OFF) ctrl->Activate (false);
		else if (state == DGButton3::ON)  ctrl->Activate (true);
	}
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}

// ==============================================================

HoverAltModeButtons::HoverAltModeButtons (HoverHoldComponent *hhac)
: PanelElement (hhac->DG()), ctrl(hhac)
{
	vmode = HoverHoldComponent::HOLD_NONE;
	for (int i = 0; i < 2; i++)
		btn[i] = new DGButton3 (vessel);
}

// --------------------------------------------------------------

HoverAltModeButtons::~HoverAltModeButtons ()
{
	for (int i = 0; i < 2; i++)
		delete btn[i];
}

// --------------------------------------------------------------

void HoverAltModeButtons::DefineAnimation2D (int meshgrp, int vofs)
{
	gidx = meshgrp;
	vtxofs = vofs;
}

// --------------------------------------------------------------

void HoverAltModeButtons::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, gidx);
}

// --------------------------------------------------------------

void HoverAltModeButtons::DefineAnimationsVC (const VECTOR3 &axis, DWORD meshgrp, DWORD meshgrp_label,
	DWORD vofs[2], DWORD vofs_label[2])
{
	for (int i = 0; i < 2; i++) 
		btn[i]->DefineAnimationVC (axis, meshgrp, meshgrp_label, vofs[i], vofs_label[i]);
}

// --------------------------------------------------------------

bool HoverAltModeButtons::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	for (int i = 0; i < 2; i++)
		btn[i]->RedrawVC (hMesh, surf);
	return false;
}

// --------------------------------------------------------------

bool HoverAltModeButtons::ProcessMouse2D (int event, int mx, int my)
{
	if (event & PANEL_MOUSE_LBDOWN) {
		ctrl->SetHoverMode (mx < 27 ? HoverHoldComponent::HOLD_ALT :
									  HoverHoldComponent::HOLD_VSPD);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool HoverAltModeButtons::Redraw2D (SURFHANDLE hSurf)
{
	static const float texh = (float)PANEL2D_TEXH;
	int ofs = (ctrl->GetHoverMode() == HoverHoldComponent::HOLD_ALT ? 0 : 30);
	for (int i = 0; i < 4; i++)
		grp->Vtx[vtxofs+i].tv = (391+ofs+(i/2)*30)/texh;
	return false;
}

// --------------------------------------------------------------

bool HoverAltModeButtons::ProcessMouseVC (int event, VECTOR3 &p)
{
	int i;
	int ix = (int)(p.x*64.0);
	int b = ix/33;
	if (ix-b*33 >= 30) return false;

	if (event & PANEL_MOUSE_LBDOWN) {
		for (i = 0; i < 2; i++)
			btn[i]->SetState (i==b ? DGButton3::PRESSED_FROM_OFF : DGButton3::OFF);
		DeltaGlider *dg = (DeltaGlider*)vessel;
		vmode = (HoverHoldComponent::HoverMode)(b+1);
		ctrl->SetHoverMode (vmode);
	} else if (event & PANEL_MOUSE_LBUP) {
		btn[b]->SetState (DGButton3::ON);
	}
	return true;
}

// --------------------------------------------------------------

void HoverAltModeButtons::ResetVC (DEVMESHHANDLE hMesh)
{
	for (int i = 0; i < 2; i++)
		btn[i]->ResetVC (hMesh);
	if (vmode != ctrl->GetHoverMode()) {
		vmode = ctrl->GetHoverMode();
		for (int i = 0; i < 2; i++)
			btn[i]->SetState (i+1 == (int)vmode ? DGButton3::ON : DGButton3::OFF);
	}
}

// ==============================================================

HoverAltSwitch::HoverAltSwitch (HoverHoldComponent *hhac)
: DGSwitch2(hhac->DG()), ctrl(hhac)
{
}

// --------------------------------------------------------------

void HoverAltSwitch::Set (int state, double refT)
{
	if (state) {
		double prm = ctrl->TargetPrm();
		double t = oapiGetSysTime();
		double dt = oapiGetSysStep();
		double downt = t-refT;
		double dprm = dt * max(fabs(prm),1.0);
		if (downt < 10.0) dprm *= 1e-6 + downt*(1.0-1e-6)/10.0;
		if (state == 1) dprm = -dprm;
		ctrl->SetTargetPrm (prm + dprm);
	}
}

// --------------------------------------------------------------

bool HoverAltSwitch::ProcessMouse2D (int event, int mx, int my)
{
	static int state = 0;
	static double refT = 0.0;
	if (event & PANEL_MOUSE_LBDOWN)
		refT = oapiGetSysTime();

	if (DGSwitch2::ProcessMouse2D (event, mx, my))
		state = (int)GetState();
	
	Set (state, refT);

	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}

// --------------------------------------------------------------

bool HoverAltSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	static int state = 0;
	static double refT = 0.0;
	if (event & PANEL_MOUSE_LBDOWN)
		refT = oapiGetSysTime();

	if (DGSwitch2::ProcessMouseVC (event, p))
		state = (int)GetState();

	Set (state, refT);

	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}

// ==============================================================

HoverAltResetBtn::HoverAltResetBtn (HoverHoldComponent *hhac)
: DGButton2(hhac->DG()), ctrl(hhac)
{
}

// --------------------------------------------------------------

bool HoverAltResetBtn::ProcessMouse2D (int event, int mx, int my)
{
	DGButton2::ProcessMouse2D (event, mx, my);
	if (event & PANEL_MOUSE_LBDOWN) {
		if (ctrl->GetHoverMode() == HoverHoldComponent::HOLD_ALT)
			ctrl->SetTargetAltCurrent ();
		else
			ctrl->SetTargetVspd (0.0);
	}
	return false; // no animation
}

// --------------------------------------------------------------

bool HoverAltResetBtn::ProcessMouseVC (int event, VECTOR3 &p)
{
	DGButton2::ProcessMouseVC (event, p);
	if (event & PANEL_MOUSE_LBDOWN) {
		if (ctrl->GetHoverMode() == HoverHoldComponent::HOLD_ALT)
			ctrl->SetTargetAltCurrent ();
		else
			ctrl->SetTargetVspd (0.0);
	}
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP)) != 0;
}

// ==============================================================

HoverHoldAltIndicator::HoverHoldAltIndicator (HoverHoldComponent *hhac, SURFHANDLE blitsrc)
: PanelElement (hhac->DG()), ctrl(hhac), bsrc(blitsrc)
{
	btgt = 0;
}

// --------------------------------------------------------------

void HoverHoldAltIndicator::Reset2D (int panelid, MESHHANDLE hMesh)
{
	btgt = oapiGetTextureHandle (hMesh, 3);
}

// --------------------------------------------------------------

void HoverHoldAltIndicator::ResetVC (DEVMESHHANDLE hMesh)
{
	btgt = oapiGetTextureHandle (ctrl->DG()->vcmesh_tpl, 14);
	strcpy (holdstr, "         ");
	holdmode_disp = HoverHoldComponent::HOLD_NONE;
	hold_disp = false;
}

// --------------------------------------------------------------

bool HoverHoldAltIndicator::Redraw2D (SURFHANDLE surf)
{
	return Redraw();
}

// --------------------------------------------------------------

bool HoverHoldAltIndicator::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE hSurf)
{
	return Redraw();
}

// --------------------------------------------------------------

bool HoverHoldAltIndicator::Redraw ()
{
	bool refresh = false;
	if (!btgt) return refresh;

	if (holdmode_disp != ctrl->hovermode) {
		oapiBlt (btgt, bsrc, 314, 2, (ctrl->hovermode-1)*25, 14, 25, 8);
		holdmode_disp = ctrl->hovermode;
		refresh = true;
	}

	if (hold_disp != ctrl->active) {
		oapiBlt (btgt, bsrc, 348, 2, ctrl->active ? 51:78, 14, 27, 8);
		hold_disp = ctrl->active;
		refresh = true;
	}

	char cbuf[10];
	FormatValue (cbuf, 10, holdmode_disp == HoverHoldComponent::HOLD_ALT ? ctrl->holdalt : ctrl->holdvspd, 4);
	if (strncmp (cbuf, holdstr, 10)) {
		UpdateReadout (cbuf, holdstr);
		refresh = true;
	}
	return refresh;
}

// --------------------------------------------------------------

void HoverHoldAltIndicator::UpdateReadout (const char *tgtstr, char *curstr)
{
	int tgtx = 314;
	int tgty = 12;
	const int srcy = 0;
	int srcx;
	const int w = 8;
	const int h = 11;
	bool have_tgt = true, have_cur = true;
	char c;
	int i;
	for (i = 0; i < 10 && (have_tgt || have_cur); i++) {
		if (!tgtstr[i]) have_tgt = false;
		if (!curstr[i]) have_cur = false;
		c = (have_tgt ? tgtstr[i] : ' ');
		if (c != curstr[i]) {
			if (c >= '0' && c <= '9') srcx = (c-'0')*8;
			else switch(c) {
				case '.': srcx = 10*8; break;
				case '+': srcx = 11*8; break;
				case '-': srcx = 12*8; break;
				case 'k': srcx = 13*8; break;
				case 'M': srcx = 14*8; break;
				case 'G': srcx = 15*8; break;
				default:  srcx = 16*8; break;
			}
			oapiBlt (btgt, bsrc, tgtx, tgty, srcx, srcy, w, h);
			curstr[i] = c;
		}
		tgtx += w;
	}
	if (i < 10) curstr[i] = '\0';
}

// ==============================================================
// Panel elements for manual control submode
// ==============================================================

HoverThrottle::HoverThrottle (HoverManualComponent *_ctrl)
: PanelElement(_ctrl->DG()), ctrl(_ctrl)
{
	ppos = 0.0f;
}

// --------------------------------------------------------------

void HoverThrottle::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 56;
}

// --------------------------------------------------------------

void HoverThrottle::ResetVC (DEVMESHHANDLE hMesh)
{
	sliderpos = (UINT)-1;
}

// --------------------------------------------------------------

bool HoverThrottle::Redraw2D (SURFHANDLE surf)
{
	// constants for texture coordinates
	static const float tx_dy = 18.0f;
	static const float bb_y0 = 428.5f;

	int j;
	float pos;
	static const float sy[4] = {bb_y0,bb_y0,bb_y0+tx_dy,bb_y0+tx_dy};

	DeltaGlider *dg = ctrl->DG();
	double level = dg->GetHoverThrusterLevel (0);
	pos = (float)(-level*116.0);
	if (pos != ppos) {
		for (j = 0; j < 4; j++) grp->Vtx[vtxofs+j].y = sy[j]+pos;
		ppos = pos;
	}
	return false;
}

// --------------------------------------------------------------

bool HoverThrottle::ProcessMouse2D (int event, int mx, int my)
{
	DeltaGlider *dg = ctrl->DG();
	my = max (0, min (116, my-9));
	dg->SetThrusterGroupLevel (dg->thg_hover, 1.0-my/116.0);
	return true;
}

// --------------------------------------------------------------

bool HoverThrottle::ProcessMouseVC (int event, VECTOR3 &p)
{
	static double py = 0.0;

	if (event & PANEL_MOUSE_LBDOWN) { // record which slider to operate
		py = p.y;
	} else {
		DeltaGlider *dg = ctrl->DG();
		double lvl = max (0.0, min (1.0, dg->GetHoverThrusterLevel (0) + (p.y-py)));
		if (lvl < 0.01) lvl = 0.0;
		for (int i = 0; i < 3; i++) dg->SetHoverThrusterLevel (i, lvl);
		py = p.y;
	}
	return true;
}

// --------------------------------------------------------------

bool HoverThrottle::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	DeltaGlider *dg = ctrl->DG();
	double level = dg->GetHoverThrusterLevel (0);
	UINT pos = (UINT)(level*500.0);
	if (pos != sliderpos) {
		dg->SetAnimation (ctrl->anim_hoverthrottle, level);
		sliderpos = pos;
	}
	return false;
}
