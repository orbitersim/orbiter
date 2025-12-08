// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// DockingSubsys.cpp
// Nosecone and undock controls
// ==============================================================

#include "DockingSubsys.h"
#include "meshres.h"
#include "meshres_p0.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

// ==============================================================
// Docking control subsystem
// ==============================================================

DockingCtrlSubsystem::DockingCtrlSubsystem (DeltaGlider *v)
: DGSubsystem (v)
{
	// create component instances
	AddSubsystem (noseconectrl = new NoseconeCtrl (this));
	AddSubsystem (undockctrl = new UndockCtrl (this));
	AddSubsystem (eladderctrl = new EscapeLadderCtrl (this));
	AddSubsystem (dsealctrl = new DocksealCtrl (this));
}

// --------------------------------------------------------------

const AnimState2 &DockingCtrlSubsystem::NconeState() const
{
	return noseconectrl->NconeState();
}

// --------------------------------------------------------------

void DockingCtrlSubsystem::OpenNcone ()
{
	noseconectrl->OpenNcone();
}

// --------------------------------------------------------------

void DockingCtrlSubsystem::CloseNcone ()
{
	noseconectrl->CloseNcone();
}

// --------------------------------------------------------------

void DockingCtrlSubsystem::ExtendLadder ()
{
	eladderctrl->ExtendLadder();
}

// --------------------------------------------------------------

void DockingCtrlSubsystem::RetractLadder ()
{
	eladderctrl->RetractLadder();
}

// --------------------------------------------------------------

const AnimState2 &DockingCtrlSubsystem::LadderState () const
{
	return eladderctrl->State();
}

// --------------------------------------------------------------

void DockingCtrlSubsystem::clbkDockEvent (int dock, OBJHANDLE mate)
{
	dsealctrl->SetDockStatus (mate != 0);
}

// ==============================================================
// Nosecone control
// ==============================================================

NoseconeCtrl::NoseconeCtrl (DockingCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ncone_state.SetOperatingSpeed (NOSE_OPERATING_SPEED);
	nlever_state.SetOperatingSpeed (4.0);

	ELID_LEVER = AddElement (lever = new NoseconeLever (this));
	ELID_INDICATOR = AddElement (indicator = new NoseconeIndicator (this));

	// Nosecone animation
	static UINT NConeTLGrp[2] = {GRP_NConeTL1,GRP_NConeTL2};
	static MGROUP_ROTATE NConeTL (0, NConeTLGrp, 2,
		_V(-0.424,-0.066,9.838), _V(-0.707,-0.707,0), (float)(150*RAD));
	static UINT NConeTRGrp[2] = {GRP_NConeTR1,GRP_NConeTR2};
	static MGROUP_ROTATE NConeTR (0, NConeTRGrp, 2,
		_V( 0.424,-0.066,9.838), _V(-0.707, 0.707,0), (float)(150*RAD));
	static UINT NConeBLGrp[2] = {GRP_NConeBL1,GRP_NConeBL2};
	static MGROUP_ROTATE NConeBL (0, NConeBLGrp, 2,
		_V(-0.424,-0.914,9.838), _V( 0.707,-0.707,0), (float)(150*RAD));
	static UINT NConeBRGrp[2] = {GRP_NConeBR1,GRP_NConeBR2};
	static MGROUP_ROTATE NConeBR (0, NConeBRGrp, 2,
		_V( 0.424,-0.914,9.838), _V( 0.707, 0.707,0), (float)(150*RAD));
	static UINT NConeDockGrp[1] = {GRP_NConeDock};
	static MGROUP_TRANSLATE NConeDock (0, NConeDockGrp, 1, _V(0,0,0.06));
	// virtual cockpit mesh animation (nose cone visible from cockpit)
	static UINT VCNConeTLGrp[1] = {GRP_NOSECONE_L_VC};
	static MGROUP_ROTATE VCNConeTL (1, VCNConeTLGrp, 1,
		_V(-0.424,-0.066,9.838), _V(-0.707,-0.707,0), (float)(150*RAD));
	static UINT VCNConeTRGrp[1] = {GRP_NOSECONE_R_VC};
	static MGROUP_ROTATE VCNConeTR (1, VCNConeTRGrp, 1,
		_V( 0.424,-0.066,9.838), _V(-0.707, 0.707,0), (float)(150*RAD));
	anim_nose = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_nose, 0.01, 0.92, &NConeTL);
	DG()->AddAnimationComponent (anim_nose, 0.01, 0.92, &VCNConeTL);
	DG()->AddAnimationComponent (anim_nose, 0.02, 0.925, &NConeTR);
	DG()->AddAnimationComponent (anim_nose, 0.02, 0.925, &VCNConeTR);
	DG()->AddAnimationComponent (anim_nose, 0, 0.91, &NConeBL);
	DG()->AddAnimationComponent (anim_nose, 0.015, 0.915, &NConeBR);
	DG()->AddAnimationComponent (anim_nose, 0.8, 1, &NConeDock);

	// Nosecone lever VC animatuion
	static UINT NoseconeLeverGrp = GRP_NOSECONE_LEVER_VC;
	static MGROUP_ROTATE NoseconeLeverTransform (1, &NoseconeLeverGrp, 1,
		VC_NCONELEVER_ref, VC_NCONELEVER_axis, (float)(-70*RAD));
	anim_noselever = DG()->CreateAnimation (0.5);
	DG()->AddAnimationComponent (anim_noselever, 0, 1, &NoseconeLeverTransform);

}

// --------------------------------------------------------------

void NoseconeCtrl::OpenNcone ()
{
	ncone_state.Open();
	nlever_state.Open();
	DG()->UpdateStatusIndicators();
	DG()->TriggerPanelRedrawArea (0, ELID_LEVER);
	DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);
	DG()->RecordEvent ("NOSECONE", "OPEN");
}

// --------------------------------------------------------------

void NoseconeCtrl::CloseNcone ()
{
	ncone_state.Close();
	nlever_state.Close();
	DG()->UpdateStatusIndicators();
	DG()->TriggerPanelRedrawArea (0, ELID_LEVER);
	DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);

	if (!((DockingCtrlSubsystem*)Parent())->LadderState().IsClosed())
		((DockingCtrlSubsystem*)Parent())->RetractLadder(); // retract ladder before closing the nose cone

	DG()->RecordEvent ("NOSECONE", "CLOSE");
}

// --------------------------------------------------------------

void NoseconeCtrl::RevertNcone ()
{
	if (ncone_state.IsOpen() || ncone_state.IsOpening())
		CloseNcone();
	else
		OpenNcone();
}

// --------------------------------------------------------------

void NoseconeCtrl::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate nose cone
	if (ncone_state.Process (simdt)) {
		DG()->SetAnimation (anim_nose, ncone_state.State());
		DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);
		DG()->UpdateStatusIndicators();

		// don't allow closing the nosecone while docked
		if (DG()->GetDockStatus(DG()->GetDockHandle(0))) {
			if (ncone_state.IsClosing() && ncone_state.State() < 0.98)
				OpenNcone();
		}
	}

	// animate VC nosecone lever
	if (nlever_state.Process (simdt)) {
		DG()->SetAnimation (anim_noselever, nlever_state.State());
	}
}

// --------------------------------------------------------------

bool NoseconeCtrl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_LEVER, _R(1221,359,1260,473), PANEL_REDRAW_USER, PANEL_MOUSE_LBDOWN, panel2dtex, lever);
	DG()->RegisterPanelArea (hPanel, ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, panel2dtex, indicator);

	return true;
}

// --------------------------------------------------------------

bool NoseconeCtrl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Nosecone lever
	oapiVCRegisterArea (ELID_LEVER, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_LEVER, VC_NCONELEVER_mousearea[0], VC_NCONELEVER_mousearea[1], VC_NCONELEVER_mousearea[2], VC_NCONELEVER_mousearea[3]);

	// Nosecone indicator
	oapiVCRegisterArea (ELID_INDICATOR, PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE);

	return true;
}

// --------------------------------------------------------------

void NoseconeCtrl::clbkSaveState (FILEHANDLE scn)
{
	ncone_state.SaveState (scn, "NOSECONE");
}

// --------------------------------------------------------------

bool NoseconeCtrl::clbkParseScenarioLine (const char *line)
{
	if (ncone_state.ParseScenarioLine (line, "NOSECONE")) {
		if (ncone_state.IsOpen() || ncone_state.IsOpening()) {
			nlever_state.SetOpened();
		} else {
			nlever_state.SetClosed();
		}
		return true;
	}
	return false;
}

// --------------------------------------------------------------

void NoseconeCtrl::clbkPostCreation ()
{
	DG()->SetAnimation (anim_nose, ncone_state.State());
	DG()->SetAnimation (anim_noselever, nlever_state.State());
}

// --------------------------------------------------------------

bool NoseconeCtrl::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "NOSECONE")) {
		if (!_stricmp (event, "CLOSE")) CloseNcone();
		else                            OpenNcone();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

int NoseconeCtrl::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	if (KEYMOD_ALT(kstate) || KEYMOD_CONTROL(kstate) || KEYMOD_SHIFT(kstate))
		return 0;

	if (key == OAPI_KEY_K) {
		RevertNcone();
		return 1;
	}
	return 0;
}

// ==============================================================

NoseconeLever::NoseconeLever (NoseconeCtrl *comp)
: PanelElement(comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

void NoseconeLever::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 84;
}

// --------------------------------------------------------------

bool NoseconeLever::Redraw2D (SURFHANDLE surf)
{
	static const float texh = (float)PANEL2D_TEXH; // texture height
	bool leverdown = (component->NconeState().Speed() ? component->NconeState().IsOpening() : component->NconeState().IsOpen());
	float y0, dy, tv0;
	if (leverdown) y0 = 432.5f, dy = 21.0f, tv0 = texh-677.5f;
	else           y0 = 358.5f, dy = 19.0f, tv0 = texh-696.5f;
	int j;
	for (j = 0; j < 4; j++) {
		grp->Vtx[vtxofs+j].y = y0 + (j/2)*dy;
		grp->Vtx[vtxofs+j].tv = (tv0 + (j/2)*dy)/texh;
	}
	return false;
}

// --------------------------------------------------------------

bool NoseconeLever::ProcessMouse2D (int event, int mx, int my)
{
	if (component->NconeState().IsClosed() || component->NconeState().IsClosing()) {
		if (my < 58) component->OpenNcone();
	} else {
		if (my > 36) component->CloseNcone();
	}
	return false;
}

// --------------------------------------------------------------

bool NoseconeLever::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (p.y > 0.5) component->CloseNcone();
	else           component->OpenNcone();
	return false;
}


// ==============================================================

NoseconeIndicator::NoseconeIndicator (NoseconeCtrl *comp)
: PanelElement(comp->DG()), component(comp)
{
	tofs = (double)rand()/(double)RAND_MAX;
	light = true;
}

// --------------------------------------------------------------

void NoseconeIndicator::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 88;
}

// --------------------------------------------------------------

void NoseconeIndicator::ResetVC (DEVMESHHANDLE hMesh)
{
	light = true;
}

// --------------------------------------------------------------

bool NoseconeIndicator::Redraw2D (SURFHANDLE surf)
{
	static const float texw = (float)PANEL2D_TEXW; // texture width

	int i, j;
	double d;
	int xofs = (component->NconeState().IsClosed() ? 1014 :
		        component->NconeState().IsOpen()   ? 1027 :
                (modf (oapiGetSimTime()+tofs, &d) < 0.5 ? 1040 : 1014));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < 3; j++)
			grp->Vtx[vtxofs+i*3+j].tu = (xofs + (j%2)*12)/texw;
	}
	return false;
}

// --------------------------------------------------------------

bool NoseconeIndicator::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (!hMesh) return false;

	double d;
	bool showlights = (component->NconeState().IsClosed() ? false :
		               component->NconeState().IsOpen() ? true :
                       (modf (oapiGetSimTime()+tofs, &d) < 0.5));
	if (showlights != light) {
		GROUPEDITSPEC ges;
		static WORD vtxofs = VC_NCONE_INDICATOR_vofs;
		static const DWORD nvtx = 2;
		static WORD vidx[nvtx] = {vtxofs,WORD(vtxofs+1)};
		static float v[2] = {0.2427f,0.3003f};
		NTVERTEX vtx[nvtx];
		for (DWORD i = 0; i < nvtx; i++)
			vtx[i].tv = v[showlights ? 1:0];
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
// Undock control
// ==============================================================

UndockCtrl::UndockCtrl (DockingCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	undock_state.SetOperatingSpeed (10.0);
	ELID_LEVER = AddElement (lever = new UndockLever (this));

	// Undock lever animation
	static UINT UndockLeverGrp = GRP_UNDOCK_LEVER_VC;
	static MGROUP_ROTATE UndockLeverTransform (1, &UndockLeverGrp, 1,
		VC_UNDOCKLEVER_ref, VC_UNDOCKLEVER_axis, (float)(-90*RAD));
	anim_undocklever = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_undocklever, 0, 1, &UndockLeverTransform);
}

// --------------------------------------------------------------

void UndockCtrl::PullLever ()
{
	undock_state.Open();
	DG()->Undock(0);
}

// --------------------------------------------------------------

void UndockCtrl::ReleaseLever ()
{
	undock_state.Close();
}

// --------------------------------------------------------------

void UndockCtrl::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate undock lever
	if (undock_state.Process (simdt))
		DG()->SetAnimation (anim_undocklever, undock_state.State());
}

// --------------------------------------------------------------

bool UndockCtrl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_LEVER, _R(1151,369,1193,450), PANEL_REDRAW_MOUSE,  PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, lever);

	return true;
}

// --------------------------------------------------------------

bool UndockCtrl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Undock lever
	oapiVCRegisterArea (ELID_LEVER, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_LEVER, VC_UNDOCKLEVER_mousearea[0], VC_UNDOCKLEVER_mousearea[1], VC_UNDOCKLEVER_mousearea[2], VC_UNDOCKLEVER_mousearea[3]);

	return true;
}

// ==============================================================

UndockLever::UndockLever (UndockCtrl *comp)
: PanelElement(comp->DG()), component(comp)
{
	btndown = false;
}

// --------------------------------------------------------------

void UndockLever::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 100;
	btndown = false;
}

// --------------------------------------------------------------

bool UndockLever::Redraw2D (SURFHANDLE surf)
{
	static const float texh = (float)PANEL2D_TEXH; // texture height
	static const float bb_y0 =  368.0f;     // top edge of button block
	static const float tx_dy = 103.0f;       // texture block height
	static const float tx_y0 = texh-356.0f; // top edge of texture block

	float y = (btndown ? bb_y0+tx_dy : bb_y0);
	float tv = (btndown ? tx_y0+tx_dy : tx_y0)/texh;
	grp->Vtx[vtxofs+2].y = grp->Vtx[vtxofs+3].y = y;
	grp->Vtx[vtxofs+2].tv = grp->Vtx[vtxofs+3].tv = tv;
	return false;
}

// --------------------------------------------------------------

bool UndockLever::ProcessMouse2D (int event, int mx, int my)
{
	if (event & PANEL_MOUSE_LBDOWN) vessel->Undock (0);
	btndown = (event == PANEL_MOUSE_LBDOWN);
	return true;
}

// --------------------------------------------------------------

bool UndockLever::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (event & PANEL_MOUSE_LBDOWN) component->PullLever();
	else                            component->ReleaseLever();
	return false;
}


// ==============================================================
// Escape ladder control
// ==============================================================

EscapeLadderCtrl::EscapeLadderCtrl (DockingCtrlSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ladder_state.SetOperatingSpeed (LADDER_OPERATING_SPEED);
	ELID_SWITCH = AddElement (sw = new LadderSwitch (this));
	ELID_INDICATOR = AddElement (indicator = new LadderIndicator (this));

	// Escape ladder animation
	static UINT LadderGrp[2] = {GRP_Ladder1,GRP_Ladder2};
	static MGROUP_TRANSLATE Ladder1 (0, LadderGrp, 2, _V(0,0,1.1));
	static MGROUP_ROTATE Ladder2 (0, LadderGrp, 2,
		_V(0,-1.05,9.85), _V(1,0,0), (float)(80*RAD));
	anim_ladder = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_ladder, 0, 0.5, &Ladder1);
	DG()->AddAnimationComponent (anim_ladder, 0.5, 1, &Ladder2);
}

// --------------------------------------------------------------

void EscapeLadderCtrl::ExtendLadder ()
{
	if (!((DockingCtrlSubsystem*)Parent())->NconeState().IsOpen()) return;
	// don't extend ladder if nosecone is closed

	if (DG()->GetDockStatus(DG()->GetDockHandle(0))) return;
	// don't extend ladder if dock is engaged

	ladder_state.Open();
	DG()->RecordEvent ("LADDER", "OPEN");
}

// --------------------------------------------------------------

void EscapeLadderCtrl::RetractLadder ()
{
	ladder_state.Close();
	DG()->RecordEvent ("LADDER", "CLOSE");
}

// --------------------------------------------------------------

void EscapeLadderCtrl::clbkPostCreation ()
{
	DG()->SetAnimation (anim_ladder, ladder_state.State());
}

// --------------------------------------------------------------

void EscapeLadderCtrl::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate escape ladder
	if (ladder_state.Process (simdt)) {
		DG()->SetAnimation (anim_ladder, ladder_state.State());
		DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);
	}
}

// --------------------------------------------------------------

void EscapeLadderCtrl::clbkSaveState (FILEHANDLE scn)
{
	ladder_state.SaveState (scn, "LADDER");
}

// --------------------------------------------------------------

bool EscapeLadderCtrl::clbkParseScenarioLine (const char *line)
{
	return ladder_state.ParseScenarioLine (line, "LADDER");
}

// --------------------------------------------------------------

bool EscapeLadderCtrl::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "LADDER")) {
		if (!_stricmp (event, "CLOSE")) RetractLadder();
		else                            ExtendLadder();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool EscapeLadderCtrl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	// Ladder extend/retract switch

	DG()->RegisterPanelArea (hPanel, ELID_SWITCH, _R(1171,496,1197,548), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, sw);
	sw->DefineAnimation2D (DG()->panelmesh0, GRP_INSTRUMENTS_ABOVE_P0, 44);

	// Ladder indicator
	DG()->RegisterPanelArea (hPanel, ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, panel2dtex, indicator);

	return true;
}

// --------------------------------------------------------------

bool EscapeLadderCtrl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Ladder extend/retract switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_ELADDER_SWITCH_mousearea[0], VC_ELADDER_SWITCH_mousearea[1], VC_ELADDER_SWITCH_mousearea[2], VC_ELADDER_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_ELADDER_SWITCH_ref, VC_ELADDER_SWITCH_axis, GRP_SWITCH1_VC, VC_ELADDER_SWITCH_vofs);

	// Ladder indicator
	oapiVCRegisterArea (ELID_INDICATOR, PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE);

	return true;
}

// ==============================================================

LadderSwitch::LadderSwitch (EscapeLadderCtrl *comp)
: DGSwitch1(comp->DG(), DGSwitch1::SPRING), component(comp)
{
}

// --------------------------------------------------------------

bool LadderSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		if (GetState() == UP)        component->RetractLadder();
		else if (GetState() == DOWN) component->ExtendLadder();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool LadderSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		if (GetState() == UP)        component->RetractLadder();
		else if (GetState() == DOWN) component->ExtendLadder();
		return true;
	}
	return false;
}

// ==============================================================

LadderIndicator::LadderIndicator (EscapeLadderCtrl *comp)
: PanelElement(comp->DG()), component(comp)
{
	vlight_2D = vlight_VC = false;
}

// --------------------------------------------------------------

void LadderIndicator::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 192;
	vlight_2D = false;
}

// --------------------------------------------------------------

void LadderIndicator::ResetVC (DEVMESHHANDLE hMesh)
{
	vlight_VC = false;
}

// --------------------------------------------------------------

bool LadderIndicator::Redraw2D (SURFHANDLE surf)
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

bool LadderIndicator::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (!hMesh) return false;

	const AnimState2 &state = component->State();
	double d;
	bool showlights = (state.State() == 1.0 || (state.Speed() && modf(oapiGetSimTime()*1.7, &d) < 0.5));
	if (showlights != vlight_VC) {
		GROUPEDITSPEC ges;
		static const WORD vtxofs = VC_ELADDER_INDICATOR_vofs;
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

// ==============================================================
// Dock seal control
// ==============================================================

DocksealCtrl::DocksealCtrl (DockingCtrlSubsystem *_subsys)
: DGSubsystem (_subsys)
{
	isDocked = false;
	dockTime = -1e10;

	ELID_INDICATOR = AddElement (indicator = new DocksealIndicator (this));
}

// --------------------------------------------------------------

void DocksealCtrl::SetDockStatus (bool docked)
{
	isDocked = docked;
	dockTime = oapiGetSimTime();
	if (!docked || dockTime < 1.0) {
		dockTime -= 1e10;
		isSealing = false;
	} else isSealing = true;
	DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);
}

// --------------------------------------------------------------

void DocksealCtrl::clbkPostStep (double simt, double simdt, double mjd)
{
	if (isSealing) {
		DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);
		isSealing = (simt-simdt-dockTime <= 10.0);
	}
}

// --------------------------------------------------------------

void DocksealCtrl::clbkPostCreation ()
{
	DOCKHANDLE hDock = DG()->GetDockHandle (0);
	OBJHANDLE mate = DG()->GetDockStatus(hDock);
	if (mate) {
		isDocked = true;
		DG()->TriggerRedrawArea (0, 0, ELID_INDICATOR);
	}
}

// --------------------------------------------------------------

bool DocksealCtrl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_INDICATOR, _R(0,0,0,0), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, panel2dtex, indicator);

	return true;
}

// --------------------------------------------------------------

bool DocksealCtrl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// dock seal indicator
	oapiVCRegisterArea (ELID_INDICATOR, PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE);

	return false;
}

// ==============================================================

DocksealIndicator::DocksealIndicator (DocksealCtrl *comp)
: PanelElement(comp->DG()), component(comp)
{
	vlight_2D = vlight_VC = false;
}

// --------------------------------------------------------------

void DocksealIndicator::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 196;
}

// --------------------------------------------------------------

void DocksealIndicator::ResetVC (DEVMESHHANDLE hMesh)
{
	vlight_VC = false;
}

// --------------------------------------------------------------

bool DocksealIndicator::Redraw2D (SURFHANDLE surf)
{
	bool showlights = false;
	double d, dt;
	if (component->isDocked) {
		if ((dt = oapiGetSimTime()-component->dockTime) > 10.0)
			showlights = true;
		else
			showlights = (modf (dt, &d) < 0.5);
	}
	if (showlights != vlight_2D) {
		float v = (showlights ? 420.0f : 412.0f)/1024.0f;
		for (int i = 0; i < 4; i++)
			grp->Vtx[vtxofs+i].tv = v;
		vlight_2D = showlights;
	}
	return false;
}

// --------------------------------------------------------------

bool DocksealIndicator::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	if (!hMesh) return false;

	bool showlights = false;
	double d, dt;
	if (component->isDocked) {
		if ((dt = oapiGetSimTime()-component->dockTime) > 10.0)
			showlights = true;
		else
			showlights = (modf (dt, &d) < 0.5);
	}
	if (showlights != vlight_VC) {
		GROUPEDITSPEC ges;
		static const WORD vtxofs = VC_SEAL_INDICATOR_vofs;
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