// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// GearSubsys.cpp
// Subsystem for landing gear control
// ==============================================================

#define STRICT 1

#include "GearSubsys.h"
#include "meshres.h"
#include "meshres_vc.h"
#include "meshres_p0.h"
#include "dg_vc_anim.h"

// ==============================================================
// Landing gear subsystem
// ==============================================================

GearSubsystem::GearSubsystem (DeltaGlider *v)
: DGSubsystem (v)
{
	// create component instances
	AddSubsystem (gearctrl = new GearControl (this));
	AddSubsystem (wheelbrake = new Wheelbrake (this));
}

// --------------------------------------------------------------

void GearSubsystem::LowerGear ()
{
	gearctrl->LowerGear ();
}

// --------------------------------------------------------------

void GearSubsystem::RaiseGear ()
{
	gearctrl->RaiseGear ();
}

// --------------------------------------------------------------

const AnimState2 &GearSubsystem::GearState() const
{
	return gearctrl->GearState();
}

// ==============================================================
// Gear control: lever+indicator
// ==============================================================

GearControl::GearControl (GearSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	gear_state.SetOperatingSpeed (GEAR_OPERATING_SPEED);
	glever_state.SetOperatingSpeed (4.0);

	ELID_LEVER = AddElement (lever = new GearLever (this));
	ELID_INDICATOR = AddElement (indicator = new GearIndicator (this));

	// Landing gear animation
	static UINT NWheelStrutGrp[2] = {GRP_NWheelStrut1,GRP_NWheelStrut2};
	static MGROUP_ROTATE NWheelStrut (0, NWheelStrutGrp, 2,
		_V(0,-1.048,8.561), _V(1,0,0), (float)(-95*RAD));
	static UINT NWheelFCoverGrp[2] = {GRP_NWheelFCover1,GRP_NWheelFCover2};
	static MGROUP_ROTATE NWheelFCover (0, NWheelFCoverGrp, 2,
		_V(0,-1.145,8.65), _V(1,0,0), (float)(-90*RAD));
	static UINT NWheelLCoverGrp[2] = {GRP_NWheelLCover1,GRP_NWheelLCover2};
	static MGROUP_ROTATE NWheelLCover1 (0, NWheelLCoverGrp, 2,
		_V(-0.3,-1.222,7.029), _V(0,0.052,0.999), (float)(-90*RAD));
	static MGROUP_ROTATE NWheelLCover2 (0, NWheelLCoverGrp, 2,
		_V(-0.3,-1.222,7.029), _V(0,0.052,0.999), (float)( 90*RAD));
	static UINT NWheelRCoverGrp[2] = {GRP_NWheelRCover1,GRP_NWheelRCover2};
	static MGROUP_ROTATE NWheelRCover1 (0, NWheelRCoverGrp, 2,
		_V( 0.3,-1.222,7.029), _V(0,0.052,0.999), (float)( 90*RAD));
	static MGROUP_ROTATE NWheelRCover2 (0, NWheelRCoverGrp, 2,
		_V( 0.3,-1.222,7.029), _V(0,0.052,0.999), (float)(-90*RAD));
	static UINT LWheelStrutGrp[2] = {GRP_LWheelStrut1,GRP_LWheelStrut2};
	static MGROUP_ROTATE LWheelStrut (0, LWheelStrutGrp, 2,
		_V(-3.607,-1.137,-3.08), _V(0,0,1), (float)(-90*RAD));
	static UINT RWheelStrutGrp[2] = {GRP_RWheelStrut1,GRP_RWheelStrut2};
	static MGROUP_ROTATE RWheelStrut (0, RWheelStrutGrp, 2,
		_V( 3.607,-1.137,-3.08), _V(0,0,1), (float)(90*RAD));
	static UINT LWheelOCoverGrp[4] = {GRP_LWheelOCover1,GRP_LWheelOCover2,GRP_LWheelOCover3,GRP_LWheelOCover4};
	static MGROUP_ROTATE LWheelOCover (0, LWheelOCoverGrp, 4,
		_V(-3.658,-1.239,-3.038), _V(0,0,1), (float)(-110*RAD));
	static UINT LWheelICoverGrp[2] = {GRP_LWheelICover1,GRP_LWheelICover2};
	static MGROUP_ROTATE LWheelICover1 (0, LWheelICoverGrp, 2,
		_V(-2.175,-1.178,-3.438), _V(0,0,1), (float)(90*RAD));
	static MGROUP_ROTATE LWheelICover2 (0, LWheelICoverGrp, 2,
		_V(-2.175,-1.178,-3.438), _V(0,0,1), (float)(-90*RAD));
	static UINT RWheelOCoverGrp[4] = {GRP_RWheelOCover1,GRP_RWheelOCover2,GRP_RWheelOCover3,GRP_RWheelOCover4};
	static MGROUP_ROTATE RWheelOCover (0, RWheelOCoverGrp, 4,
		_V( 3.658,-1.239,-3.038), _V(0,0,1), (float)( 110*RAD));
	static UINT RWheelICoverGrp[2] = {GRP_RWheelICover1,GRP_RWheelICover2};
	static MGROUP_ROTATE RWheelICover1 (0, RWheelICoverGrp, 2,
		_V( 2.175,-1.178,-3.438), _V(0,0,1), (float)(-90*RAD));
	static MGROUP_ROTATE RWheelICover2 (0, RWheelICoverGrp, 2,
		_V( 2.175,-1.178,-3.438), _V(0,0,1), (float)( 90*RAD));
	anim_gear = DG()->CreateAnimation (1);
	DG()->AddAnimationComponent (anim_gear, 0.3, 1, &NWheelStrut);
	DG()->AddAnimationComponent (anim_gear, 0.3, 0.9, &NWheelFCover);
	DG()->AddAnimationComponent (anim_gear, 0, 0.3, &NWheelLCover1);
	DG()->AddAnimationComponent (anim_gear, 0.7, 1.0, &NWheelLCover2);
	DG()->AddAnimationComponent (anim_gear, 0, 0.3, &NWheelRCover1);
	DG()->AddAnimationComponent (anim_gear, 0.7, 1.0, &NWheelRCover2);
	DG()->AddAnimationComponent (anim_gear, 0, 1, &LWheelStrut);
	DG()->AddAnimationComponent (anim_gear, 0, 1, &RWheelStrut);
	DG()->AddAnimationComponent (anim_gear, 0, 1, &LWheelOCover);
	DG()->AddAnimationComponent (anim_gear, 0, 0.3, &LWheelICover1);
	DG()->AddAnimationComponent (anim_gear, 0.7, 1, &LWheelICover2);
	DG()->AddAnimationComponent (anim_gear, 0, 1, &RWheelOCover);
	DG()->AddAnimationComponent (anim_gear, 0, 0.3, &RWheelICover1);
	DG()->AddAnimationComponent (anim_gear, 0.7, 1, &RWheelICover2);

	// VC gear lever animation
	static UINT GearLeverGrp = GRP_GEAR_LEVER_VC;
	static MGROUP_ROTATE GearLeverTransform (1, &GearLeverGrp, 1,
		VC_GEARLEVER_ref, VC_GEARLEVER_axis, (float)(-70*RAD));
	anim_gearlever = DG()->CreateAnimation (0.5);
	DG()->AddAnimationComponent (anim_gearlever, 0, 1, &GearLeverTransform);
}

// --------------------------------------------------------------

void GearControl::LowerGear ()
{
	if (DG()->GroundContact()) {
		VECTOR3 nml = {0,1,0}, vnml;
		DG()->HorizonInvRot(nml, vnml);
		if (vnml.y > 0.0) return;
	}
	// we cannot deploy the landing gear if we are already sitting on the ground

	gear_state.Open();
	glever_state.Open();
	DG()->UpdateStatusIndicators();
	DG()->TriggerPanelRedrawArea (0, ELID_LEVER);
	DG()->TriggerRedrawArea (2, 0, ELID_INDICATOR);
	DG()->RecordEvent ("GEAR", "DOWN");
}

// --------------------------------------------------------------

void GearControl::RaiseGear ()
{
	gear_state.Close();
	glever_state.Close();
	DG()->UpdateStatusIndicators();
	DG()->TriggerPanelRedrawArea (0, ELID_LEVER);
	DG()->TriggerRedrawArea (2, 0, ELID_INDICATOR);
	DG()->RecordEvent ("GEAR", "UP");
}

// --------------------------------------------------------------

void GearControl::RevertGear ()
{
	if (gear_state.IsOpen() || gear_state.IsOpening())
		RaiseGear();
	else
		LowerGear();
}

// --------------------------------------------------------------

void GearControl::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate landing gear
	if (gear_state.Process (simdt)) {
		DG()->SetAnimation (anim_gear, gear_state.State());
		DG()->SetGearParameters (gear_state.State());
		DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);
		DG()->UpdateStatusIndicators();
	}

	// animate gear lever
	if (glever_state.Process (simdt)) {
		DG()->SetAnimation (anim_gearlever, glever_state.State());
	}
}

// --------------------------------------------------------------

bool GearControl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_LEVER, _R(93,373,125,598), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN, panel2dtex, lever);
	DG()->RegisterPanelArea (hPanel, ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, panel2dtex, indicator);

	return true;
}

// --------------------------------------------------------------

bool GearControl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	SURFHANDLE tex1 = oapiGetTextureHandle (DG()->vcmesh_tpl, 16);

	// Gear lever
	oapiVCRegisterArea (ELID_LEVER, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_LEVER, VC_GEARLEVER_mousearea[0], VC_GEARLEVER_mousearea[1], VC_GEARLEVER_mousearea[2], VC_GEARLEVER_mousearea[3]);

	// Gear indicator
	oapiVCRegisterArea (ELID_INDICATOR, PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE);
	oapiVCRegisterArea (ELID_INDICATOR, _R(32,127,61,158), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_BACKGROUND, tex1);

	return true;
}

// --------------------------------------------------------------

void GearControl::clbkSaveState (FILEHANDLE scn)
{
	gear_state.SaveState (scn, "GEAR");
}

// --------------------------------------------------------------

bool GearControl::clbkParseScenarioLine (const char *line)
{
	if (gear_state.ParseScenarioLine (line, "GEAR")) {
		if (gear_state.IsOpen() || gear_state.IsOpening()) {
			glever_state.SetOpened();
		} else {
			glever_state.SetClosed();
		}
		return true;
	}
	return false;
}

// --------------------------------------------------------------

void GearControl::clbkPostCreation ()
{
	DG()->SetAnimation (anim_gear, gear_state.State());
	DG()->SetAnimation (anim_gearlever, glever_state.State());
	DG()->SetGearParameters (gear_state.State());
}

// --------------------------------------------------------------

bool GearControl::clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp)
{
	// show gear deployment status
	int cx = hps->CX, cy = hps->CY;
	if (gear_state.IsOpen() || (!gear_state.IsClosed() && fmod (oapiGetSimTime(), 1.0) < 0.5)) {
		int d = hps->Markersize/2;
		if (cx >= -d*3 && cx < hps->W+d*3 && cy >= d && cy < hps->H+d*5) {
			skp->Rectangle (cx-d/2, cy-d*5, cx+d/2, cy-d*4);
			skp->Rectangle (cx-d*3, cy-d*2, cx-d*2, cy-d);
			skp->Rectangle (cx+d*2, cy-d*2, cx+d*3, cy-d);
		}
	}
	return true;
}

// --------------------------------------------------------------

bool GearControl::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "GEAR")) {
		if (!_stricmp (event, "UP")) RaiseGear();
		else                         LowerGear();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

int GearControl::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	if (KEYMOD_ALT(kstate) || KEYMOD_CONTROL(kstate) || KEYMOD_SHIFT(kstate))
		return 0;

	if (key == OAPI_KEY_G) {
		RevertGear();
		return 1;
	}
	return 0;
}

// ==============================================================

GearLever::GearLever (GearControl *comp)
: PanelElement(comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

void GearLever::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 68;
}

// --------------------------------------------------------------

bool GearLever::Redraw2D (SURFHANDLE surf)
{
	static const float tx_dx =  176.0f;
	static const float bb_y0 =  413.0f;
	bool leverdown = (component->GearState().Speed() ? component->GearState().IsOpening() : component->GearState().IsOpen());
	float y = (leverdown ? bb_y0+tx_dx : bb_y0);
	grp->Vtx[vtxofs+2].y = grp->Vtx[vtxofs+3].y = y;

	return false;
}

// --------------------------------------------------------------

bool GearLever::ProcessMouse2D (int event, int mx, int my)
{
	if (component->GearState().IsClosed() || component->GearState().IsClosing()) {
		if (my < 151) component->LowerGear();
	} else {
		if (my >  46) component->RaiseGear();
	}
	return false;
}

// --------------------------------------------------------------

bool GearLever::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (p.y > 0.5) component->RaiseGear();
	else           component->LowerGear();
	return false;
}

// ==============================================================

GearIndicator::GearIndicator (GearControl *comp)
: PanelElement(comp->DG()), component(comp)
{
	tofs = (double)rand()/(double)RAND_MAX;
	light = true;
}

// --------------------------------------------------------------

void GearIndicator::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 72;
}

// --------------------------------------------------------------

void GearIndicator::ResetVC (DEVMESHHANDLE hMesh)
{
	light = true;
}

// --------------------------------------------------------------

bool GearIndicator::Redraw2D (SURFHANDLE surf)
{
	static const float texw = (float)PANEL2D_TEXW; // texture width
	int i, j;
	double d;
	int xofs = (component->GearState().IsClosed() ? 1018 :
		        component->GearState().IsOpen() ? 1030 :
			    (modf (oapiGetSimTime()+tofs, &d) < 0.5 ? 1042 : 1020));
	for (i = 0; i < 3; i++) {
		for (j = 0; j < 4; j++)
			grp->Vtx[vtxofs+i*4+j].tu = (xofs + (j%2)*10)/texw;
	}
	return false;
}

// --------------------------------------------------------------

bool GearIndicator::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (!hMesh) return false;

	double d;
	bool showlights = (component->GearState().IsClosed() ? false :
		               component->GearState().IsOpen() ? true :
					   (modf (oapiGetSimTime()+tofs, &d) < 0.5));
	if (showlights != light) {
		GROUPEDITSPEC ges;
		static WORD vtxofs = VC_GEAR_INDICATOR_vofs;
		static const DWORD nvtx = 2;
		static WORD vidx[nvtx] = {vtxofs,(WORD)(vtxofs+1)};
		static float v[2] = {0.2427f,0.3042f};
		NTVERTEX vtx[nvtx];
		for (DWORD i = 0; i < nvtx; i++)
			vtx[i].tv = v[(showlights ? 1:0)];
		ges.flags = GRPEDIT_VTXTEXV;
		ges.Vtx = vtx;
		ges.vIdx = vidx;
		ges.nVtx = nvtx;
		oapiEditMeshGroup (hMesh, GRP_VC4_LIT_VC, &ges);
		light = showlights;
	}
	return false;
}


// ==============================================================
// Wheelbrake
// ==============================================================

Wheelbrake::Wheelbrake (GearSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ELID_LEVER = AddElement (lever = new WheelbrakeLever (this));
}

// --------------------------------------------------------------

bool Wheelbrake::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_LEVER, _R(1221,494,1273,557), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP, panel2dtex, lever);

	return true;
}

// ==============================================================

WheelbrakeLever::WheelbrakeLever (Wheelbrake *comp)
: PanelElement(comp->DG()), component(comp)
{
	for (int i = 0; i < 2; i++)
		isdown[i] = false;
}

// --------------------------------------------------------------

void WheelbrakeLever::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 104;
}

// --------------------------------------------------------------

bool WheelbrakeLever::Redraw2D (SURFHANDLE surf)
{
	static const float texh = (float)PANEL2D_TEXH; // texture height
	static const float tx_y0 = texh-650.0f;
	static const float tx_dy = 77.0f;
	int i, j;
	for (i = 0; i < 2; i++) {
		double lvl = vessel->GetWheelbrakeLevel (i+1);
		bool down = (lvl > 0.5);
		if (down != isdown[i]) {
			float tv = (down ? tx_y0+tx_dy : tx_y0)/texh;
			for (j = 2; j < 4; j++)
				grp->Vtx[vtxofs+i*4+j].tv = tv;
			isdown[i] = down;
		}
	}
	return false;
}

// --------------------------------------------------------------

bool WheelbrakeLever::ProcessMouse2D (int event, int mx, int my)
{
	int which = (mx < 15 ? 1 : mx > 37 ? 2 : 0);
	bool press = (event == PANEL_MOUSE_LBDOWN);
	vessel->SetWheelbrakeLevel (press ? 1.0:0.0, which);
	return false;
}
