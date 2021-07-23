// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// MfdSubsys.cpp
// Subsystem for MFD instruments
// ==============================================================

#define STRICT 1

#include "MfdSubsys.h"
#include "meshres_vc.h"

// ==============================================================
// Constants for button labels
// ==============================================================

// constants for texture coordinates
static const int texw = PANEL2D_TEXW; // texture width
static const int texh = PANEL2D_TEXH; // texture height
static const int tx_x = 996;
static const int tx_y = texh-624;
//static const int tx_w = 28;
static const int tx_h = 226;
// constants for panel coordinates
static const int btn_y0 = texh-473;
static const int btn_dy = 41;
static const int btn_x0[2][2] = {{172,519},{735,1082}};

extern GDIParams g_Param;

const int CHX[256] = { // MFD label font: character x-offsets
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,642/*+*/,0,661/*-*/,0,596/* / */,
	492/*0*/,501/*1*/,510/*2*/,520/*3*/,529/*4*/,538/*5*/,547/*6*/,556/*7*/,565/*8*/,575/*9*/,627/*:*/,621/*;*/,602/*<*/,652/*=*/,612/*>*/,0,
	0,1/*A*/,11/*B*/,21/*C*/,32/*D*/,43/*E*/,54/*F*/,63/*G*/,75/*H*/,86/*I*/,92/*J*/,101/*K*/,111/*L*/,120/*M*/,132/*N*/,143/*O*/,
	155/*P*/,165/*Q*/,176/*R*/,187/*S*/,198/*T*/,207/*U*/,218/*V*/,229/*W*/,242/*X*/,253/*Y*/,263/*Z*/,0,0,0,0,0,
	0,273/*a*/,282/*b*/,291/*c*/,299/*d*/,309/*e*/,318/*f*/,324/*g*/,333/*h*/,342/*i*/,347/*j*/,353/*k*/,362/*l*/,367/*m*/,380/*n*/,389/*o*/,
	398/*p*/,407/*q*/,416/*r*/,423/*s*/,431/*t*/,438/*u*/,447/*v*/,456/*w*/,466/*x*/,475/*y*/,483/*z*/,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

const int CHW[256] = { // MFD label font: character widths
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,6/*+*/,0,4/*-*/,0,4/* / */,
	6/*0*/,6/*1*/,6/*2*/,6/*3*/,6/*4*/,6/*5*/,6/*6*/,6/*7*/,6/*8*/,6/*9*/,2/*:*/,2/*;*/,6/*<*/,6/*=*/,6/*>*/,0,
	0,8/*A*/,7/*B*/,7/*C*/,7/*D*/,6/*E*/,6/*F*/,8/*G*/,7/*H*/,2/*I*/,5/*J*/,7/*K*/,6/*L*/,8/*M*/,7/*N*/,8/*O*/,
	6/*P*/,8/*Q*/,7/*R*/,7/*S*/,6/*T*/,7/*U*/,8/*V*/,11/*W*/,7/*X*/,8/*Y*/,7/*Z*/,0,0,0,0,0,
	0,6/*a*/,6/*b*/,6/*c*/,6/*d*/,6/*e*/,4/*f*/,6/*g*/,6/*h*/,2/*i*/,3/*j*/,5/*k*/,2/*l*/,8/*m*/,6/*n*/,6/*o*/,
	6/*p*/,6/*q*/,4/*r*/,6/*s*/,4/*t*/,6/*u*/,6/*v*/,9/*w*/,6/*x*/,6/*y*/,6/*z*/,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};

const int CHY = 22;
const int CHH = 12;

const int lblx[2][2] = {{185,533},{748,1095}};
const int lbly[6] = {texh-467,texh-426,texh-385,texh-344,texh-303,texh-262};

// ==============================================================
// MFD subsystem
// ==============================================================

MfdSubsystem::MfdSubsystem (DeltaGlider *v, int mfdident)
: DGSubsystem(v), mfdid(mfdident)
{
	ELID_BTNROW = AddElement (btnrow = new MfdButtonRow (this));
	for (int i = 0; i < 2; i++)
		ELID_BTNCOL[i] = AddElement (btncol[i] = new MfdButtonCol (this, i));
}

// --------------------------------------------------------------

void MfdSubsystem::ModeChanged ()
{
	for (int i = 0; i < 2; i++)
		DG()->TriggerRedrawArea (0, 0, ELID_BTNCOL[i]);
}

// --------------------------------------------------------------

bool MfdSubsystem::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	const int xofs = (mfdid == MFD_LEFT ? 173:736);
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_BTNROW, _R( 51+xofs,359,321+xofs,377), PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_ONREPLAY, panel2dtex, btnrow); // bottom button row
	DG()->RegisterPanelArea (hPanel, ELID_BTNCOL[0], _R(    xofs,100, 25+xofs,323), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, panel2dtex, btncol[0]); // left button column
	DG()->RegisterPanelArea (hPanel, ELID_BTNCOL[1], _R(348+xofs,100,373+xofs,323), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY, panel2dtex, btncol[1]); // right button column

	return true;
}

// --------------------------------------------------------------

bool MfdSubsystem::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	vctex = oapiGetTextureHandle (DG()->vcmesh_tpl, 20);
	const double xofs = (mfdid == MFD_LEFT ? -0.2684 : 0.0616);

	oapiVCRegisterArea (ELID_BTNROW, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_BTNROW, _V(0.0840+xofs, 1.0745, 7.2238), _V(0.1228+xofs, 1.0745, 7.2238), _V(0.0840+xofs, 1.0587, 7.2180), _V(0.1228+xofs, 1.0587, 7.2180));

	oapiVCRegisterArea (ELID_BTNCOL[0], PANEL_REDRAW_MOUSE|PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_BTNCOL[0], _V(0+xofs, 1.2155, 7.2751), _V(0.0168+xofs, 1.2155, 7.2751), _V(0+xofs, 1.0963, 7.2317), _V(0.0168+xofs, 1.0963, 7.2317));

	oapiVCRegisterArea (ELID_BTNCOL[1], PANEL_REDRAW_MOUSE|PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP|PANEL_MOUSE_LBPRESSED|PANEL_MOUSE_ONREPLAY);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_BTNCOL[1], _V(0.1900+xofs, 1.2155, 7.2751), _V(0.2068+xofs, 1.2155, 7.2751), _V(0.1900+xofs, 1.0963, 7.2317), _V(0.2068+xofs, 1.0963, 7.2317));

	return true;
}


// ==============================================================
// MfdButtonGrp: row/column group of MFD buttons
// ==============================================================

MfdButtonGrp::MfdButtonGrp (MfdSubsystem *_subsys, DWORD _nbtn)
: PanelElement(_subsys->DG()), subsys(_subsys), nbtn(_nbtn)
{
	ispushed = new bool[nbtn];
	for (DWORD i = 0; i < nbtn; i++)
		ispushed[i] = false;
	pending_action = 0;
}

// --------------------------------------------------------------

MfdButtonGrp::~MfdButtonGrp ()
{
	delete []ispushed;
}

// --------------------------------------------------------------

void MfdButtonGrp::PushButtonVC (DEVMESHHANDLE hMesh, int meshgrp, int btn, bool down)
{
	if (down == ispushed[btn]) return; // nothing to do
	ispushed[btn] = down;

	static const int nvtx_per_button = 12;
	static const double depth = 0.004;   // button travel
	static const double tilt = 20.0*RAD; // inclination of MFD panel
	static const float dz_down = (float)( depth*cos(tilt));
	static const float dy_down = (float)(-depth*sin(tilt));
	NTVERTEX dvtx[nvtx_per_button];
	WORD vofs[nvtx_per_button];
	float dz = (down ? dz_down : -dz_down);
	float dy = (down ? dy_down : -dy_down);
	for (int i = 0; i < nvtx_per_button; i++) {
		dvtx[i].y = dy;
		dvtx[i].z = dz;
		vofs[i] = btn*nvtx_per_button + i;
	}
	GROUPEDITSPEC ges = {GRPEDIT_VTXCRDADDY|GRPEDIT_VTXCRDADDZ, 0, dvtx, nvtx_per_button, vofs};
	oapiEditMeshGroup (hMesh, meshgrp, &ges);
}

// ==============================================================
// MfdButtonRow: POW/SEL/MNU buttons
// ==============================================================

MfdButtonRow::MfdButtonRow (MfdSubsystem *_subsys)
: MfdButtonGrp(_subsys, 2)
{
}

// --------------------------------------------------------------

bool MfdButtonRow::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	static const int grpid[2] = {GRP_LMFD_BBUTTONS_VC, GRP_RMFD_BBUTTONS_VC};
	if (pending_action) {
		PushButtonVC (hMesh, grpid[subsys->MfdId()], pending_btn, pending_action==1);
		pending_action = 0;
	}
	return false;
}

// --------------------------------------------------------------

bool MfdButtonRow::ProcessMouse2D (int event, int mx, int my)
{
	bool proc = false;
	if (mx < 26)                    oapiToggleMFD_on (subsys->MfdId()), proc = true;
	else if (mx >= 214 && mx < 240) oapiSendMFDKey (subsys->MfdId(), OAPI_KEY_F1), proc = true;
	else if (mx > 244)              oapiSendMFDKey (subsys->MfdId(), OAPI_KEY_GRAVE), proc = true;
	return proc;
}

// --------------------------------------------------------------

bool MfdButtonRow::ProcessMouseVC (int event, VECTOR3 &p)
{
	bool proc = false;
	double dp;
	bool pushed;
	int anim_btn = -1;

	if (event & PANEL_MOUSE_LBDOWN) {
		if (modf (p.x*7.0/4.0, &dp) < 0.75) {
			curbtn = anim_btn = (int)dp;
			pushed = true;
			switch (curbtn) {
			case 0:
				oapiSendMFDKey (subsys->MfdId(), OAPI_KEY_F1);
				proc = true;
				break;
			case 1:
				oapiSendMFDKey (subsys->MfdId(), OAPI_KEY_GRAVE);
				proc = true;
				break;
			}
		}
	} else if (curbtn >= 0) {
		if (event & PANEL_MOUSE_LBUP) {
			anim_btn = curbtn;
			pushed = false;
			curbtn = -1;
		}
	}
	// animate button pushes
	if (anim_btn >= 0) {
		pending_btn = anim_btn;
		pending_action = (pushed ? 1:2);
		return true;
	}

	return false;
}

// ==============================================================
// MfdButtonCol: Left/right MFD button column
// ==============================================================

MfdButtonCol::MfdButtonCol (MfdSubsystem *_subsys, int side)
: MfdButtonGrp(_subsys, 6), sd(side)
{
	xcnt = lblx[subsys->MfdId()][sd];
	curbtn = -1;
}

// --------------------------------------------------------------

bool MfdButtonCol::Redraw2D (SURFHANDLE surf)
{
	int btn, x, /*y,*/ len, i, w;
	const char *label;

	for (btn = 0; btn < nbtn; btn++)
		oapiBlt (surf, surf, xcnt-14, lbly[btn], 773, 22, 28, CHH); // blank label

	for (btn = 0; btn < 6; btn++) {
		if (label = oapiMFDButtonLabel (subsys->MfdId(), btn+sd*6)) {
			len = strlen(label);
			for (w = i = 0; i < len; i++) w += CHW[label[i]];
			for (i = 0, x = xcnt-w/2; i < len; i++) {
				w = CHW[label[i]];
				if (w) {
					oapiBlt (surf, surf, x, lbly[btn], CHX[label[i]], CHY, w, CHH);
					x += w;
				}
			}
		} else break;
	}
    return false;
}

// --------------------------------------------------------------

bool MfdButtonCol::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (pending_action) { // process button push

		static const int grpid[2][2] = {{GRP_LMFD_LBUTTONS_VC,GRP_LMFD_RBUTTONS_VC},
		                                {GRP_RMFD_LBUTTONS_VC,GRP_RMFD_RBUTTONS_VC}};
		PushButtonVC (hMesh, grpid[subsys->MfdId()][sd], pending_btn, pending_action==1);
		pending_action = 0;
		return false;

	} else { // process label change

		static const int CHX[256] = { // MFD label font: character x-offsets
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,333/*+*/,0,342/*-*/,0,359/* / */,
			492/*0*/,501/*1*/,510/*2*/,520/*3*/,529/*4*/,538/*5*/,547/*6*/,556/*7*/,565/*8*/,575/*9*/,627/*:*/,621/*;*/,373/*<*/,652/*=*/,381/*>*/,0,
			0,81/*A*/,90/*B*/,100/*C*/,110/*D*/,120/*E*/,129/*F*/,138/*G*/,150/*H*/,159/*I*/,164/*J*/,173/*K*/,183/*L*/,192/*M*/,203/*N*/,213/*O*/,
			224/*P*/,233/*Q*/,243/*R*/,253/*S*/,263/*T*/,271/*U*/,281/*V*/,291/*W*/,305/*X*/,314/*Y*/,324/*Z*/,0,0,0,0,0,
			0,273/*a*/,282/*b*/,291/*c*/,299/*d*/,309/*e*/,318/*f*/,324/*g*/,333/*h*/,342/*i*/,347/*j*/,353/*k*/,362/*l*/,367/*m*/,380/*n*/,389/*o*/,
			398/*p*/,407/*q*/,416/*r*/,423/*s*/,431/*t*/,438/*u*/,447/*v*/,456/*w*/,466/*x*/,475/*y*/,483/*z*/,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		};

		const int CHW[256] = { // MFD label font: character widths
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,6/*+*/,0,6/*-*/,0,5/* / */,
			6/*0*/,6/*1*/,6/*2*/,6/*3*/,6/*4*/,6/*5*/,6/*6*/,6/*7*/,6/*8*/,6/*9*/,2/*:*/,2/*;*/,8/*<*/,6/*=*/,8/*>*/,0,
			0,9/*A*/,9/*B*/,9/*C*/,9/*D*/,8/*E*/,8/*F*/,10/*G*/,8/*H*/,4/*I*/,7/*J*/,8/*K*/,7/*L*/,10/*M*/,9/*N*/,10/*O*/,
			8/*P*/,9/*Q*/,9/*R*/,9/*S*/,8/*T*/,9/*U*/,8/*V*/,12/*W*/,8/*X*/,9/*Y*/,8/*Z*/,0,0,0,0,0,
			0,6/*a*/,6/*b*/,6/*c*/,6/*d*/,6/*e*/,4/*f*/,6/*g*/,6/*h*/,2/*i*/,3/*j*/,5/*k*/,2/*l*/,8/*m*/,6/*n*/,6/*o*/,
			6/*p*/,6/*q*/,4/*r*/,6/*s*/,4/*t*/,6/*u*/,6/*v*/,9/*w*/,6/*x*/,6/*y*/,6/*z*/,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
		};
		const int CHY = 1012;

		surf = subsys->VcTex();

		const int xcnt0 = 148, dx = 40, wlbl = 32, hlbl = 12;
		const char *label;
		int btn, xcnt, x, y = 14+sd*41+subsys->MfdId()*82, w, len, i;

		for (btn = 0; btn < 6; btn++)
			oapiBlt (surf, surf, xcnt0-wlbl/2+btn*dx, y, 0, 128, wlbl, hlbl); // blank label

		for (btn = 0; btn < 6; btn++) {
			if (label = oapiMFDButtonLabel (subsys->MfdId(), btn+sd*6)) {
				len = strlen(label);
				for (w = i = 0; i < len; i++) w += CHW[label[i]];
				xcnt = xcnt0 + btn*dx;
				for (i = 0, x = xcnt-w/2; i < len; i++) {
					w = CHW[label[i]];
					if (w) {
						oapiBlt (surf, surf, x, y, CHX[label[i]], CHY, w, hlbl);
						x += w;
					}
				}
			} else break;
		}
		return true;
	}
}

// --------------------------------------------------------------

bool MfdButtonCol::ProcessMouse2D (int event, int mx, int my)
{
	int process_btn = -1;

	if (event & PANEL_MOUSE_LBDOWN) {
		if (my%41 < 18)
			curbtn = process_btn = my/41 + sd*6;
	} else if (curbtn >= 0) {
		process_btn = curbtn;
		if (event & PANEL_MOUSE_LBUP)
			curbtn = -1;
	}
	if (process_btn >= 0) {
		oapiProcessMFDButton (subsys->MfdId(), process_btn, event);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool MfdButtonCol::ProcessMouseVC (int event, VECTOR3 &p)
{
	double dp;
	bool pushed;
	int anim_btn = -1, process_btn = -1;

	if (event & PANEL_MOUSE_LBDOWN) {
		if (modf (p.y*23.0/4.0, &dp) < 0.75) {
			curbtn = anim_btn = process_btn = (int)dp;
			pushed = true;
		}
	} else if (curbtn >= 0) {
		process_btn = curbtn;
		if (event & PANEL_MOUSE_LBUP) {
			anim_btn = curbtn;
			curbtn = -1;
			pushed = false;
		}
	}
	if (process_btn >= 0) {
		oapiProcessMFDButton (subsys->MfdId(), process_btn + sd*6, event);
	}
	// animate button pushes
	if (anim_btn >= 0) {
		pending_btn = anim_btn;
		pending_action = (pushed ? 1:2);
		return true;
	}
	return false;
}

