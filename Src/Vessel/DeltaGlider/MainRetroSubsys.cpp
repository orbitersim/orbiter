// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// MainRetroSubsys.cpp
// Subsystem for main and retro engine control
// ==============================================================

#define STRICT 1
#include "MainRetroSubsys.h"
#include "meshres.h"
#include "meshres_p0.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

using std::min;
using std::max;

// constants for texture coordinates
static const float texw = (float)PANEL2D_TEXW; // texture width
static const float texh = (float)PANEL2D_TEXH; // texture height
static const float tx_x0 = 1147.5f;            // left edge of texture block
static const float tx_y0 = texh-614.5f;        // top edge of texture block
static const float tx_dx = 7.0f;               // texture block width
static const float tx_dy = 6.0f;               // texture block height
// constants for panel coordinates
static const float bb_dx =  7.0f;
static const float bb_dy =  3.0f;
static const float pm_x0 = 27.0f;       // left edge of button block
static const float pm_y0 = 103.5f;      // top edge of button block
static const float sc_y0 = 431.5f;


// ==============================================================
// Main and retro engine control subsystem
// ==============================================================

MainRetroSubsystem::MainRetroSubsystem (DeltaGlider *v)
: DGSubsystem (v)
{
	// create component instances
	AddSubsystem (throttle = new MainRetroThrottle (this));
	AddSubsystem (gimbalctrl = new GimbalControl (this));
	AddSubsystem (retrocover = new RetroCoverControl (this));
}

// --------------------------------------------------------------

void MainRetroSubsystem::OpenRetroCover ()
{
	retrocover->OpenRetroCover();
}

// --------------------------------------------------------------

void MainRetroSubsystem::CloseRetroCover ()
{
	retrocover->CloseRetroCover();
}

// --------------------------------------------------------------

const AnimState2 &MainRetroSubsystem::RetroCoverState() const
{
	return retrocover->State();
}

// --------------------------------------------------------------

void MainRetroSubsystem::clbkReset2D (int panelid, MESHHANDLE hMesh)
{
	if (panelid != 0) return;
	DGSubsystem::clbkReset2D (panelid, hMesh);
}

// --------------------------------------------------------------

void MainRetroSubsystem::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	if (vcid != 0) return;
	DGSubsystem::clbkResetVC (vcid, hMesh);
}


// ==============================================================
// Main/retro engine throttle
// ==============================================================

MainRetroThrottle::MainRetroThrottle (MainRetroSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ELID_LEVERS = AddElement (levers = new MainRetroThrottleLevers (this));

	// VC animation: Left main engine throttle
	static UINT MainThrottleLGrp[2] = {GRP_THROTTLE_MAIN_L1_VC,GRP_THROTTLE_MAIN_L2_VC};
	static MGROUP_ROTATE MainThrottleL (1, MainThrottleLGrp, 2,
		_V(0,0.72,6.9856), _V(1,0,0), (float)(50*RAD));
	anim_lever[0] = DG()->CreateAnimation (0.4);
	DG()->AddAnimationComponent (anim_lever[0], 0, 1, &MainThrottleL);

	// VC animation: Right main engine throttle
	static UINT MainThrottleRGrp[2] = {GRP_THROTTLE_MAIN_R1_VC,GRP_THROTTLE_MAIN_R2_VC};
	static MGROUP_ROTATE MainThrottleR (1, MainThrottleRGrp, 2,
		_V(0,0.72,6.9856), _V(1,0,0), (float)(50*RAD));
	anim_lever[1] = DG()->CreateAnimation (0.4);
	DG()->AddAnimationComponent (anim_lever[1], 0, 1, &MainThrottleR);
}

// --------------------------------------------------------------

bool MainRetroThrottle::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_LEVERS, _R(4,122,57,297), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED, panel2dtex, levers);

	return true;
}

// --------------------------------------------------------------

bool MainRetroThrottle::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Throttle lever animations
	oapiVCRegisterArea (ELID_LEVERS, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_LEVERS, _V(-0.372,0.918,6.905), _V(-0.279,0.918,6.905), _V(-0.372,0.885,7.11), _V(-0.279,0.885,7.11));

	return true;
}


// ==============================================================

MainRetroThrottleLevers::MainRetroThrottleLevers (MainRetroThrottle *comp)
: PanelElement(comp->DG()), component(comp)
{
	for (int i = 0; i < 2; i++)
		ppos[i] = 0.0f;
}

// --------------------------------------------------------------

void MainRetroThrottleLevers::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 48;
}

// --------------------------------------------------------------

void MainRetroThrottleLevers::ResetVC (DEVMESHHANDLE hMesh)
{
	for (int i = 0; i < 2; i++)
		sliderpos[i] = (UINT)-1;
}

// --------------------------------------------------------------

bool MainRetroThrottleLevers::Redraw2D (SURFHANDLE surf)
{
	// constants for texture coordinates
	static const float tx_dy = 18.0f;
	static const float bb_y0 = 241.5f;

	int i, j, vofs;
	float pos;
	static const float sy[4] = {bb_y0,bb_y0,bb_y0+tx_dy,bb_y0+tx_dy};

	DeltaGlider *dg = component->DG();
	for (i = 0; i < 2; i++) {
		double level = dg->GetMainThrusterLevel (i);
		if (level > 0) pos = (float)(-8.0-level*108.0);
		else {
			level = dg->GetRetroThrusterLevel (i);
			if (level > 0) pos = (float)(8.0+level*30.0);
			else           pos = 0.0f;
		}
		if (pos != ppos[i]) {
			vofs = vtxofs+i*4;
			for (j = 0; j < 4; j++) grp->Vtx[vofs+j].y = sy[j]+pos;
			ppos[i] = pos;
		}
	}
	return false;
}

// --------------------------------------------------------------

bool MainRetroThrottleLevers::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE hSurf)
{
	UINT i, pos;

	DeltaGlider *dg = component->DG();
	for (i = 0; i < 2; i++) {
		double level = dg->GetMainThrusterLevel(i);
		if (level > 0) pos = 150 + (UINT)(level*300.0);
		else {
			level = dg->GetRetroThrusterLevel(i);
			pos = 150 - (UINT)(level*150.0);
		}
		if (pos != sliderpos[i]) {
			dg->SetAnimation (component->anim_lever[i], (sliderpos[i] = pos)/450.0);
		}
	}
	return true;
}

// --------------------------------------------------------------

bool MainRetroThrottleLevers::ProcessMouse2D (int event, int mx, int my)
{
	DeltaGlider *dg = component->DG();
	static int ctrl = 0;
	if (event & PANEL_MOUSE_LBDOWN) { // record which slider to operate
		if      (mx <  12) ctrl = 0; // left engine
		else if (mx >= 37) ctrl = 1; // right engine
		else               ctrl = 2; // both
	}
	if ((my -= 9) < 0) my = 0;
	else if (my > 157) my = 157;
	dg->SetMainRetroLevel (ctrl, my <= 108 ? 1.0-my/108.0  : 0.0,   // main thruster level
			                     my >= 125 ? (my-125)/32.0 : 0.0);  // retro thruster level
	return true;
}

// --------------------------------------------------------------

bool MainRetroThrottleLevers::ProcessMouseVC (int event, VECTOR3 &p)
{
	static int ctrl = 0, mode = 0;
	static double py = 0.0;

	if (event & PANEL_MOUSE_LBDOWN) { // record which slider to operate
		if      (p.x < 0.3) ctrl = 0; // left engine
		else if (p.x > 0.7) ctrl = 1; // right engine
		else                ctrl = 2; // both
		mode = 2;
		py = p.y;
	} else {
		for (int i = 0; i < 2; i++) {
			if (ctrl == i || ctrl == 2) {
				DeltaGlider *dg = component->DG();
				double lvl = dg->GetMainThrusterLevel(i) - dg->GetRetroThrusterLevel(i);
				if      (lvl > 0.0) mode = 0;
				else if (lvl < 0.0) mode = 1;
				double lmin = (mode == 0 ? 0.0 : -1.0); // prevent direct crossover from main to retro
				double lmax = (mode == 1 ? 0.0 :  1.0); // prevent direct crossover from retro to main
				lvl = max (lmin, min (lmax, lvl + 2.0*(p.y-py)));
				if (fabs (lvl) < 0.01) lvl = 0.0;
				if (lvl >= 0.0) dg->SetMainRetroLevel (i, lvl, 0.0);
				else            dg->SetMainRetroLevel (i, 0.0, -lvl);
			}
		}
		py = p.y;
	}
	return true;
}


// ==============================================================
// Main engine gimbal control
// ==============================================================

GimbalControl::GimbalControl (MainRetroSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	mode = 0;
	mpmode = mymode = 0;
	for (int i = 0; i < 2; i++) {
		mpgimbal[i] = mpgimbal_cmd[i] = 0.0;
		mygimbal[i] = mygimbal_cmd[i] = 0.0;
		mpswitch[i] = myswitch[i] = 0;
	}
	ELID_MODEDIAL      = AddElement (modedial      = new MainGimbalDial (this));
	ELID_PGIMBALSWITCH = AddElement (pgimbalswitch = new PMainGimbalCtrl (this));
	ELID_YGIMBALSWITCH = AddElement (ygimbalswitch = new YMainGimbalCtrl (this));
	ELID_DISPLAY       = AddElement (gimbaldisp    = new MainGimbalDisp (this));
}

// --------------------------------------------------------------

void GimbalControl::SetMainPGimbal (int which, double lvl)
{
	VECTOR3 dir;
	DG()->GetMainThrusterDir (which, dir);
	dir /= dir.z;
	mpgimbal[which] = dir.y = MAIN_PGIMBAL_RANGE*lvl;
	DG()->SetMainThrusterDir (which, unit(dir));
}

// --------------------------------------------------------------

void GimbalControl::SetMainYGimbal (int which, double lvl)
{
	VECTOR3 dir;
	DG()->GetMainThrusterDir (which, dir);
	dir /= dir.z;
	mygimbal[which] = dir.x = MAIN_YGIMBAL_RANGE*lvl;
	DG()->SetMainThrusterDir (which, unit(dir));
}

// --------------------------------------------------------------

bool GimbalControl::IncMainPGimbal (int which, int dir)
{
	const double cmd_speed = 0.5;
	double dcmd = oapiGetSimStep() * cmd_speed * MAIN_PGIMBAL_RANGE * (dir == 1 ? -1.0:1.0);
	for (int i = 0; i < 2; i++) {
		if (dir && which & (1 << i)) {
			if (mode == 2)
				mpgimbal_cmd[i] = min (MAIN_PGIMBAL_RANGE, max (-MAIN_PGIMBAL_RANGE, mpgimbal_cmd[i]+dcmd));
			mpswitch[i] = 3-dir;
		} else
			mpswitch[i] = 0;
	}
	return true;
}

// --------------------------------------------------------------

bool GimbalControl::IncMainYGimbal (int which, int dir)
{
	const double cmd_speed = 0.5;
	double dcmd = oapiGetSimStep() * cmd_speed * MAIN_YGIMBAL_RANGE * (dir == 1 ? 1.0:-1.0);
	for (int i = 0; i < 2; i++) {
		if (dir && which & (1 << i)) {
			if (mode == 2)
				mygimbal_cmd[i] = min (MAIN_YGIMBAL_RANGE, max (-MAIN_YGIMBAL_RANGE, mygimbal_cmd[i]+dcmd));
			myswitch[i] = 3-dir;
		} else
			myswitch[i] = 0;
	}
	return true;
}

// --------------------------------------------------------------

void GimbalControl::AutoMainGimbal ()
{
	int i;
	double lvl, mlvl, plvl[2];

	// Pitch gimbal
	// a) pitch command
	lvl = DG()->GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ROTMODE);
	if (!lvl) lvl = -DG()->GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ROTMODE);
	plvl[0] = plvl[1] = lvl;

	// b) roll command
	lvl = DG()->GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ROTMODE);
	if (!lvl) lvl = -DG()->GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ROTMODE);
	plvl[0] += lvl;
	plvl[1] -= lvl;

	// scale to range and apply
	mlvl = max(fabs(plvl[0]), fabs(plvl[1]));
	if (mlvl > 1.0) 
		for (i = 0; i < 2; i++) plvl[i] /= mlvl;
	for (i = 0; i < 2; i++)
		mpgimbal_cmd[i] = plvl[i]*MAIN_PGIMBAL_RANGE;

	// Yaw gimbal
	// a) compensate for main thrust differences
	double t0 = DG()->GetMainThrusterLevel (0);
	double t1 = DG()->GetMainThrusterLevel (1);
	double tt = t0+t1;
	mlvl = (tt ? (t0-t1)/tt : 0.0);
	
	// b) yaw command
	lvl = DG()->GetManualControlLevel(THGROUP_ATT_YAWLEFT, MANCTRL_ROTMODE);
	if (!lvl) lvl = -DG()->GetManualControlLevel(THGROUP_ATT_YAWRIGHT, MANCTRL_ROTMODE);
	mlvl += lvl;

	// scale to range and apply
	mlvl = min (1.0, max (-1.0, mlvl));
	for (i = 0; i < 2; i++)
		mygimbal_cmd[i] = mlvl*MAIN_YGIMBAL_RANGE;
}

// --------------------------------------------------------------

void GimbalControl::TrackMainGimbal ()
{
	VECTOR3 dir;
	bool update = false;
	int i;
	double dphi = oapiGetSimStep()*MAIN_GIMBAL_SPEED;
	for (i = 0; i < 2; i++) {
		if (mpgimbal[i] != mpgimbal_cmd[i]) {
			update = true;
			if (mpgimbal[i] < mpgimbal_cmd[i])
				mpgimbal[i] = min (mpgimbal[i]+dphi, mpgimbal_cmd[i]);
			else
				mpgimbal[i] = max (mpgimbal[i]-dphi, mpgimbal_cmd[i]);
		}
		if (mygimbal[i] != mygimbal_cmd[i]) {
			update = true;
			if (mygimbal[i] < mygimbal_cmd[i])
				mygimbal[i] = min (mygimbal[i]+dphi, mygimbal_cmd[i]);
			else
				mygimbal[i] = max (mygimbal[i]-dphi, mygimbal_cmd[i]);
		}
	}
	if (update) {
		for (i = 0; i < 2; i++) {
			DG()->GetMainThrusterDir (i, dir);
			dir /= dir.z;
			dir.y = mpgimbal[i];
			dir.x = mygimbal[i];
			DG()->SetMainThrusterDir (i, unit(dir));
		}
		DG()->TriggerRedrawArea (0, 0, ELID_DISPLAY);
	}
}

// --------------------------------------------------------------

void GimbalControl::clbkSaveState (FILEHANDLE scn)
{
	if (mode) {
		if (mode == 1) { // auto
			oapiWriteScenario_int (scn, (char*)"MGIMBALMODE", mode);
		} else { // manual
			char cbuf[256];
			sprintf (cbuf, "%d %0.3lf %0.3lf %0.3lf %0.3lf",
				mode, mpgimbal_cmd[0], mpgimbal_cmd[1], mygimbal_cmd[0], mygimbal_cmd[1]);
			oapiWriteScenario_string (scn, (char*)"MGIMBALMODE", cbuf);
		}
	}
}

// --------------------------------------------------------------

bool GimbalControl::clbkParseScenarioLine (const char *line)
{
	if (!_strnicmp (line, "MGIMBALMODE", 11)) {
		double pg[2], yg[2];
		int n = sscanf (line+11, "%d%lf%lf%lf%lf", &mode, pg+0, pg+1, yg+0, yg+1);
		if (mode ==2 && n == 5) // copy manual settings
			for (int i = 0; i < 2; i++)
				mpgimbal_cmd[i] = pg[i], mygimbal_cmd[i] = yg[i];
		return true;
	}
	return false;
}

// --------------------------------------------------------------

void GimbalControl::clbkPostStep (double simt, double simdt, double mjd)
{
	if (mode == 1) AutoMainGimbal();
	TrackMainGimbal();
}

// --------------------------------------------------------------

bool GimbalControl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);

	// Gimbal control dial
	DG()->RegisterPanelArea (hPanel, ELID_MODEDIAL,      _R(203,426,243,470), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN, panel2dtex, modedial);

	// Gimbal manual switches
	DG()->RegisterPanelArea (hPanel, ELID_PGIMBALSWITCH, _R(285,433,320,477), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP, panel2dtex, pgimbalswitch);
	DG()->RegisterPanelArea (hPanel, ELID_YGIMBALSWITCH, _R(280,502,324,537), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP, panel2dtex, ygimbalswitch);

	// Gimbal display
	DG()->RegisterPanelArea (hPanel, ELID_DISPLAY,       _R(  0,  0,  0,  0), PANEL_REDRAW_USER,   PANEL_MOUSE_IGNORE, panel2dtex, gimbaldisp);

	return true;
}

// --------------------------------------------------------------

bool GimbalControl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Gimbal control dial
	oapiVCRegisterArea (ELID_MODEDIAL, PANEL_REDRAW_USER | PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_MODEDIAL, VC_GIMBAL_DIAL_mousearea[0], VC_GIMBAL_DIAL_mousearea[1], VC_GIMBAL_DIAL_mousearea[2], VC_GIMBAL_DIAL_mousearea[3]);
	modedial->DefineAnimationVC (VC_GIMBAL_DIAL_ref, VC_GIMBAL_DIAL_axis, GRP_DIAL1_VC, VC_GIMBAL_DIAL_vofs);

	// Gimbal manual switches
	oapiVCRegisterArea (ELID_PGIMBALSWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_PGIMBALSWITCH, VC_GIMBAL_PSWITCH_mousearea[0], VC_GIMBAL_PSWITCH_mousearea[1], VC_GIMBAL_PSWITCH_mousearea[2], VC_GIMBAL_PSWITCH_mousearea[3]);
	oapiVCRegisterArea (ELID_YGIMBALSWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_YGIMBALSWITCH, VC_GIMBAL_YSWITCH_mousearea[0], VC_GIMBAL_YSWITCH_mousearea[1], VC_GIMBAL_YSWITCH_mousearea[2], VC_GIMBAL_YSWITCH_mousearea[3]);

	// Gimbal status display
	oapiVCRegisterArea (ELID_DISPLAY, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);

	return true;
}

// ==============================================================

MainGimbalDial::MainGimbalDial (GimbalControl *gc)
: DGDial1 (gc->DG(), 3, -50*RAD, 50*RAD), ctrl(gc)
{
}

// --------------------------------------------------------------

void MainGimbalDial::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 112;
}

// --------------------------------------------------------------

void MainGimbalDial::ResetVC (DEVMESHHANDLE hMesh)
{
	DGDial1::ResetVC (hMesh);
	int mode = ctrl->Mode();
	SetPosition (mode);
}

// --------------------------------------------------------------

bool MainGimbalDial::Redraw2D (SURFHANDLE surf)
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

bool MainGimbalDial::ProcessMouse2D (int event, int mx, int my)
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

bool MainGimbalDial::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGDial1::ProcessMouseVC (event, p)) {
		int pos = GetPosition();
		ctrl->SetMode (pos);
		return true;
	}
	return false;
}

// ==============================================================

MainGimbalDisp::MainGimbalDisp (GimbalControl *gc)
: PanelElement (gc->DG()), ctrl(gc)
{
	for (int i = 0; i < 2; i++) {
		pofs_cur[i] = yofs_cur[i] = 0;
		pofs_cmd[i] = yofs_cmd[i] = 0;
	}
	memset (&vc_grp, 0, sizeof(GROUPREQUESTSPEC));
	for (int i = 0; i < 16; i++)
		vperm[i] = (WORD)(i+VC_GIMBAL_INDICATOR_LEFT_vofs);
}

// --------------------------------------------------------------

MainGimbalDisp::~MainGimbalDisp ()
{
	if (vc_grp.Vtx) delete []vc_grp.Vtx;
}

// --------------------------------------------------------------

void MainGimbalDisp::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 116;
}

// --------------------------------------------------------------

void MainGimbalDisp::ResetVC (DEVMESHHANDLE hMesh)
{
	vc_grp.nVtx = 16;
	if (!vc_grp.Vtx) vc_grp.Vtx = new NTVERTEX[vc_grp.nVtx];
	if (oapiGetMeshGroup (hMesh, GRP_VC_INSTR_VC, &vc_grp) != 0) { // problems
		delete []vc_grp.Vtx;
		vc_grp.Vtx = 0;
	}
}

// --------------------------------------------------------------

bool MainGimbalDisp::Redraw2D (SURFHANDLE surf)
{
	DeltaGlider *dg = (DeltaGlider*)vessel;
	int i, j, ofs;
	double g;
	const float x0 = 197.5f;
	const float xx =  42.0f;
	const float y0 = 515.5f;
	const float dx =  10.0f;
	const float dy =  10.0f;

	for (i = 0; i < 2; i++) {
		g = ctrl->MainPGimbal(i);
		ofs = (int)floor((g/MAIN_PGIMBAL_RANGE)*18+0.5);
		if (ofs != pofs_cur[i]) {
			for (j = 0; j < 4; j++)
				grp->Vtx[vtxofs+4*i+j].y = y0 + dy*(j/2) + ofs;
			pofs_cur[i] = ofs;
		}
		g = ctrl->MainYGimbal(i);
		ofs = (int)floor((g/MAIN_YGIMBAL_RANGE)*18+0.5);
		if (ofs != yofs_cur[i]) {
			for (j = 0; j < 4; j++)
				grp->Vtx[vtxofs+4*i+j].x = x0 + i*xx + dx*(j%2) - ofs;
			yofs_cur[i] = ofs;
		}
	}

	for (i = 0; i < 2; i++) {
		g = ctrl->MainPGimbal(i, false);
		ofs = (int)floor((g/MAIN_PGIMBAL_RANGE)*18+0.5);
		if (ofs != pofs_cmd[i]) {
			for (j = 0; j < 4; j++)
				grp->Vtx[vtxofs+8+4*i+j].y = y0 + dy*(j/2) + ofs;
			pofs_cmd[i] = ofs;
		}
		g = ctrl->MainYGimbal(i, false);
		ofs = (int)floor((g/MAIN_YGIMBAL_RANGE)*18+0.5);
		if (ofs != yofs_cmd[i]) {
			for (j = 0; j < 4; j++)
				grp->Vtx[vtxofs+8+4*i+j].x = x0 + i*xx + dx*(j%2) - ofs;
			yofs_cmd[i] = ofs;
		}
	}

	return false;
}

// --------------------------------------------------------------

bool MainGimbalDisp::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	const double &slope = vc_lpanel_tilt;
	const VECTOR3 (&cnt)[2] = VC_GIMBAL_INDICATOR_ref;
	static const double cosa = cos(slope), sina = sin(slope);
	static const double indsize = 0.002586;
	static const double xrange = 0.0103/MAIN_YGIMBAL_RANGE;
	static const double yrange = 0.0103/MAIN_PGIMBAL_RANGE;

	DeltaGlider *dg = (DeltaGlider*)vessel;
	NTVERTEX *Vtx = vc_grp.Vtx;
	if (hMesh && Vtx) {
		int i, j;
		double dx, dy;
		float y, z;
		for (i = 0; i < 2; i++) {
			dx = -ctrl->MainYGimbal(i)*xrange;
			dy = -ctrl->MainPGimbal(i)*yrange;
			for (j = 0; j < 4; j++) {
				Vtx[4+i*8+j].x = cnt[i].x + dx + indsize*(j%2 ? 1:-1);
				Vtx[4+i*8+j].y = dy + indsize*(j/2 ? 1:-1);
			}
			dx = -ctrl->MainYGimbal(i,false)*xrange;
			dy = -ctrl->MainPGimbal(i,false)*yrange;
			for (j = 0; j < 4; j++) {
				Vtx[i*8+j].x = cnt[i].x + dx + indsize*(j%2 ? 1:-1);
				Vtx[i*8+j].y = dy + indsize*(j/2 ? 1:-1);
			}
		}
		for (i = 0; i < 16; i++) {
			y = Vtx[i].y;
			z = (i%8) < 4 ? -0.0002f : -0.0004f;
			Vtx[i].y = (float)(cnt[0].y + y*cosa - z*sina);
			Vtx[i].z = (float)(cnt[0].z + y*sina + z*cosa);
		}
		GROUPEDITSPEC ges = {GRPEDIT_VTXCRD, 0, vc_grp.Vtx, vc_grp.nVtx, vperm};
		oapiEditMeshGroup (hMesh, GRP_VC_INSTR_VC, &ges);

	}
	return false;
}

// ==============================================================

PMainGimbalCtrl::PMainGimbalCtrl (GimbalControl *gc)
: PanelElement (gc->DG()), ctrl(gc)
{
	for (int i = 0; i < 2; i++)
		vc_state[i] = 0;
}

// --------------------------------------------------------------

void PMainGimbalCtrl::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 132;
}

// --------------------------------------------------------------

void PMainGimbalCtrl::ResetVC (DEVMESHHANDLE hMesh)
{
	GROUPREQUESTSPEC grs;
	memset (&grs, 0, sizeof(GROUPREQUESTSPEC));
	grs.Vtx = vtx0;
	grs.nVtx = nvtx_per_switch*2;
	oapiGetMeshGroup (hMesh, GRP_SWITCH2_VC, &grs);
}

// --------------------------------------------------------------

bool PMainGimbalCtrl::Redraw2D (SURFHANDLE surf)
{
	int i, j, state;
	for (i = 0; i < 2; i++) {
		state = ctrl->mpswitch[i];
		for (j = 0; j < 4; j++)
			grp->Vtx[vtxofs+i*4+j].tu = (1053.5f+state*16+(j%2)*15)/texw;
	}
	return false;
}

// --------------------------------------------------------------

bool PMainGimbalCtrl::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	const VECTOR3 &ref = VC_GIMBAL_PSWITCH_ref;
	static const double tilt[3] = {0,15*RAD,-15*RAD};

	int i, j, ofs, state;
	bool redraw = false;
	for (i = 0; i < 2; i++) {
		state = ctrl->mpswitch[i];
		if (state != vc_state[i]) {
			vc_state[i] = state;
			redraw = true;
		}
	}
	if (!redraw) return false;

	NTVERTEX vtx[nvtx_per_switch*2];
	memcpy (vtx, vtx0, nvtx_per_switch*2*sizeof(NTVERTEX));

	for (i = 0; i < 2; i++) {
		ofs = i*nvtx_per_switch;
		state = vc_state[i];
		if (!state) continue;
		MATRIX3 R = rotm(VC_GIMBAL_PSWITCH_axis,tilt[state]);
		for (j = 0; j < nvtx_per_switch; j++) {
			VECTOR3 v = {vtx[ofs+j].x-ref.x, vtx[ofs+j].y-ref.y, vtx[ofs+j].z-ref.z};
			VECTOR3 vr = mul(R,v);
			vtx[ofs+j].x = (float)(vr.x + ref.x);
			vtx[ofs+j].y = (float)(vr.y + ref.y);
			vtx[ofs+j].z = (float)(vr.z + ref.z);
			VECTOR3 n = {vtx[ofs+j].nx, vtx[ofs+j].ny, vtx[ofs+j].nz};
			VECTOR3 nr = mul(R,n);
			vtx[ofs+j].nx = (float)nr.x;
			vtx[ofs+j].ny = (float)nr.y;
			vtx[ofs+j].nz = (float)nr.z;
		}
	}

	static const int grpid = GRP_SWITCH2_VC;
	GROUPEDITSPEC ges = {GRPEDIT_VTXCRD|GRPEDIT_VTXNML,0,vtx,nvtx_per_switch*2,0};
	oapiEditMeshGroup (hMesh, grpid, &ges);
	return false;
}

// --------------------------------------------------------------

bool PMainGimbalCtrl::ProcessMouse2D (int event, int mx, int my)
{
	static int state = 0, mode = 0;
	if (event & PANEL_MOUSE_LBDOWN) {
		if      (mx <  10) state = 1;
		else if (mx >= 25) state = 2;
		else               state = 3;
		if      (my <  22) mode = 2;
		else               mode = 1;
	} else if (event & PANEL_MOUSE_LBUP) {
		state = 0;
	}
	return ctrl->IncMainPGimbal (state, mode);
}

// --------------------------------------------------------------

bool PMainGimbalCtrl::ProcessMouseVC (int event, VECTOR3 &p)
{
	static int state = 0, mode = 0;
	if (event & PANEL_MOUSE_LBDOWN) {
		if      (p.x < 0.25) state = 1;
		else if (p.x > 0.75) state = 2;
		else                 state = 3;
		if      (p.y < 0.5 ) mode = 1;
		else                 mode = 2;
	} else if (event & PANEL_MOUSE_LBUP) {
		state = 0;
	}
	ctrl->IncMainPGimbal (state, mode);
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP));
}

// ==============================================================

YMainGimbalCtrl::YMainGimbalCtrl (GimbalControl *gc)
: PanelElement (gc->DG()), ctrl(gc)
{
	int i;
	for (i = 0; i < 2; i++)
		vc_state[i] = 0;
	for (i = 0; i < nvtx_per_switch*2; i++)
		vperm[i] = (WORD)(i+VC_GIMBAL_YSWITCH_vofs);
}

// --------------------------------------------------------------

void YMainGimbalCtrl::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 140;
}

// --------------------------------------------------------------

void YMainGimbalCtrl::ResetVC (DEVMESHHANDLE hMesh)
{
	GROUPREQUESTSPEC grs;
	memset (&grs, 0, sizeof(GROUPREQUESTSPEC));
	grs.Vtx = vtx0;
	grs.nVtx = nvtx_per_switch*2;
	grs.VtxPerm = vperm;
	oapiGetMeshGroup (hMesh, GRP_SWITCH2_VC, &grs);
}

// --------------------------------------------------------------

bool YMainGimbalCtrl::Redraw2D (SURFHANDLE surf)
{
	int i, j, state;
	for (i = 0; i < 2; i++) {
		state = ctrl->myswitch[i];
		for (j = 0; j < 4; j++)
			grp->Vtx[vtxofs+i*4+j].tu = (1053.5+state*16+(j%2)*15)/texw;
	}
	return false;
}

// --------------------------------------------------------------

bool YMainGimbalCtrl::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	const VECTOR3 &ref = VC_GIMBAL_YSWITCH_ref;
	static const double tilt[3] = {0,15*RAD,-15*RAD};

	int i, j, ofs, state;
	bool redraw = false;
	for (i = 0; i < 2; i++) {
		state = ctrl->myswitch[i];
		if (state != vc_state[i]) {
			vc_state[i] = state;
			redraw = true;
		}
	}
	if (!redraw) return false;

	NTVERTEX vtx[nvtx_per_switch*2];
	memcpy (vtx, vtx0, nvtx_per_switch*2*sizeof(NTVERTEX));

	for (i = 0; i < 2; i++) {
		ofs = i*nvtx_per_switch;
		state = vc_state[i];
		if (!state) continue;
		MATRIX3 R = rotm(VC_GIMBAL_YSWITCH_axis,tilt[state]);
		for (j = 0; j < nvtx_per_switch; j++) {
			VECTOR3 v = {vtx[ofs+j].x-ref.x, vtx[ofs+j].y-ref.y, vtx[ofs+j].z-ref.z};
			VECTOR3 vr = mul(R,v);
			vtx[ofs+j].x = (float)(vr.x + ref.x);
			vtx[ofs+j].y = (float)(vr.y + ref.y);
			vtx[ofs+j].z = (float)(vr.z + ref.z);
			VECTOR3 n = {vtx[ofs+j].nx, vtx[ofs+j].ny, vtx[ofs+j].nz};
			VECTOR3 nr = mul(R,n);
			vtx[ofs+j].nx = (float)nr.x;
			vtx[ofs+j].ny = (float)nr.y;
			vtx[ofs+j].nz = (float)nr.z;
		}
	}

	static const int grpid = GRP_SWITCH2_VC;
	GROUPEDITSPEC ges = {GRPEDIT_VTXCRD|GRPEDIT_VTXNML,0,vtx,nvtx_per_switch*2,vperm};
	oapiEditMeshGroup (hMesh, grpid, &ges);
	return false;
}

// --------------------------------------------------------------

bool YMainGimbalCtrl::ProcessMouse2D (int event, int mx, int my)
{
	static int state = 0, mode = 0;
	if (event & PANEL_MOUSE_LBDOWN) {
		if      (my <  10) state = 1;
		else if (my >= 25) state = 2;
		else               state = 3;
		if      (mx <  22) mode = 1;
		else               mode = 2;
	} else if (event & PANEL_MOUSE_LBUP) {
		state = 0;
	}
	return ctrl->IncMainYGimbal (state, mode);
}

// --------------------------------------------------------------

bool YMainGimbalCtrl::ProcessMouseVC (int event, VECTOR3 &p)
{
	static int state = 0, mode = 0;
	if (event & PANEL_MOUSE_LBDOWN) {
		if      (p.x < 0.25) state = 1;
		else if (p.x > 0.75) state = 2;
		else                 state = 3;
		if      (p.y < 0.5 ) mode = 1;
		else                 mode = 2;
	} else if (event & PANEL_MOUSE_LBUP) {
		state = 0;
	}
	ctrl->IncMainYGimbal (state, mode);
	return (event & (PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP));
}


// ==============================================================
// Retro cover control
// ==============================================================

RetroCoverControl::RetroCoverControl (MainRetroSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	rcover_state.SetOperatingSpeed(RCOVER_OPERATING_SPEED);
	ELID_SWITCH = AddElement (sw = new RetroCoverSwitch (this));
	ELID_INDICATOR = AddElement (indicator = new RetroCoverIndicator(this));

	// Retro cover animation
	static UINT RCoverTLGrp[2] = {GRP_RCoverTL1,GRP_RCoverTL2};
	static MGROUP_ROTATE RCoverTL (0, RCoverTLGrp, 2,
		_V(-2.156,-0.49,6.886), _V(-0.423,0.23,-0.877), (float)( 70*RAD));
	static UINT RCoverBLGrp[2] = {GRP_RCoverBL1,GRP_RCoverBL2};
	static MGROUP_ROTATE RCoverBL (0, RCoverBLGrp, 2,
		_V(-2.156,-0.49,6.886), _V(-0.434,-0.037,-0.9), (float)(-70*RAD));
	static UINT RCoverTRGrp[2] = {GRP_RCoverTR1,GRP_RCoverTR2};
	static MGROUP_ROTATE RCoverTR (0, RCoverTRGrp, 2,
		_V( 2.156,-0.49,6.886), _V( 0.423,0.23,-0.877), (float)(-70*RAD));
	static UINT RCoverBRGrp[2] = {GRP_RCoverBR1,GRP_RCoverBR2};
	static MGROUP_ROTATE RCoverBR (0, RCoverBRGrp, 2,
		_V( 2.156,-0.49,6.886), _V( 0.434,-0.037,-0.9), (float)( 70*RAD));
	anim_rcover = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_rcover, 0, 1, &RCoverTL);
	DG()->AddAnimationComponent (anim_rcover, 0, 1, &RCoverBL);
	DG()->AddAnimationComponent (anim_rcover, 0, 1, &RCoverTR);
	DG()->AddAnimationComponent (anim_rcover, 0, 1, &RCoverBR);

}

// --------------------------------------------------------------

void RetroCoverControl::OpenRetroCover ()
{
	rcover_state.Open();
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("RCOVER", "OPEN");
}

// --------------------------------------------------------------

void RetroCoverControl::CloseRetroCover ()
{
	rcover_state.Close();
	DG()->UpdateStatusIndicators();
	DG()->EnableRetroThrusters (false);
	DG()->RecordEvent ("RCOVER", "Close");
}

// --------------------------------------------------------------

void RetroCoverControl::clbkPostCreation ()
{
	DG()->EnableRetroThrusters (rcover_state.IsOpen());
	DG()->SetAnimation (anim_rcover, rcover_state.State());
}

// --------------------------------------------------------------

void RetroCoverControl::clbkSaveState (FILEHANDLE scn)
{
	rcover_state.SaveState (scn, "RCOVER");
}

// --------------------------------------------------------------

bool RetroCoverControl::clbkParseScenarioLine (const char *line)
{
	return rcover_state.ParseScenarioLine (line, "RCOVER");
}

// --------------------------------------------------------------

void RetroCoverControl::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate retro covers
	if (rcover_state.Process (simdt)) {
		DG()->SetAnimation (anim_rcover, rcover_state.State());
		DG()->UpdateStatusIndicators();
		if (rcover_state.IsOpen())
			DG()->EnableRetroThrusters(true);
		DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);
	}

}

// --------------------------------------------------------------

bool RetroCoverControl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);

	// Retro engine cover switch
	DG()->RegisterPanelArea (hPanel, ELID_SWITCH, _R(1129,496,1155,548), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, sw);
	sw->DefineAnimation2D (DG()->panelmesh0, GRP_INSTRUMENTS_ABOVE_P0, 180);

	// Retro engine cover indicator
	DG()->RegisterPanelArea (hPanel, ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, panel2dtex, indicator);

	return true;
}

// --------------------------------------------------------------

bool RetroCoverControl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Retro engine cover switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_RCOVER_SWITCH_mousearea[0], VC_RCOVER_SWITCH_mousearea[1], VC_RCOVER_SWITCH_mousearea[2], VC_RCOVER_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_RCOVER_SWITCH_ref, VC_RCOVER_SWITCH_axis, GRP_SWITCH1_VC, VC_RCOVER_SWITCH_vofs);

	// Retro engine cover indicator
	oapiVCRegisterArea (ELID_INDICATOR, PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE);

	return true;
}

// --------------------------------------------------------------

bool RetroCoverControl::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "RCOVER")) {
		if (!_stricmp (event, "CLOSE")) CloseRetroCover();
		else                            OpenRetroCover();
		return true;
	}
	return false;
}

// ==============================================================

RetroCoverSwitch::RetroCoverSwitch (RetroCoverControl *comp)
: DGSwitch1(comp->DG(), DGSwitch1::SPRING), component(comp)
{
}

// --------------------------------------------------------------

bool RetroCoverSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		DGSwitch1::State state = GetState();
		switch (state) {
			case DGSwitch1::UP:   component->CloseRetroCover(); break;
			case DGSwitch1::DOWN: component->OpenRetroCover(); break;
		}
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool RetroCoverSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		DGSwitch1::State state = GetState();
		switch (state) {
			case DGSwitch1::UP:   component->CloseRetroCover(); break;
			case DGSwitch1::DOWN: component->OpenRetroCover(); break;
		}
		return true;
	}
	return false;
}

// ==============================================================

RetroCoverIndicator::RetroCoverIndicator (RetroCoverControl *comp)
: PanelElement(comp->DG()), component(comp)
{
	vlight_2D = vlight_VC = false;
}

// --------------------------------------------------------------

void RetroCoverIndicator::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 188;
	vlight_2D = false;
}

// --------------------------------------------------------------

void RetroCoverIndicator::ResetVC (DEVMESHHANDLE hMesh)
{
	vlight_VC = false;
}

// --------------------------------------------------------------

bool RetroCoverIndicator::Redraw2D (SURFHANDLE surf)
{
	const AnimState2 &state = component->State();
	double d;
	bool showlights = (state.State() == 1.0 || (state.Speed() && modf(oapiGetSimTime()*1.7, &d) < 0.5));
	if (showlights != vlight_2D) {
		float v = (showlights ? 420.0f : 412.0f)/1024.0f;
		for (int i = 0; i < 4; i++)
			grp->Vtx[vtxofs+i].tv = v;
		vlight_2D = showlights;
	}
	return false;
}

// --------------------------------------------------------------

bool RetroCoverIndicator::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (!hMesh) return false;

	const AnimState2 &state = component->State();
	double d;
	bool showlights = (state.State() == 1.0 || (state.Speed() && modf(oapiGetSimTime()*1.7, &d) < 0.5));
	if (showlights != vlight_VC) {
		GROUPEDITSPEC ges;
		static const WORD vtxofs = VC_RCOVER_INDICATOR_vofs;
		static const DWORD nvtx = 4;
		static WORD vidx[nvtx] = {vtxofs,vtxofs+1,vtxofs+2,vtxofs+3};
		static const float u[2] = {0.0586f,0.0713f};
		NTVERTEX vtx[nvtx];
		for (DWORD i = 0; i < nvtx; i++)
			vtx[i].tu = u[showlights ? 1:0];
		ges.flags = GRPEDIT_VTXTEXU;
		ges.Vtx = vtx;
		ges.vIdx = vidx;
		ges.nVtx = nvtx;
		oapiEditMeshGroup (hMesh, GRP_VC4_LIT_VC, &ges);
		vlight_VC = showlights;
	}
	return false;
}
