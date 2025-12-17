// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "PlBayOp.h"
#include "meshres_vc.h"
#include <stdio.h>

using std::min;
using std::max;

extern GDIParams g_Param;
extern HELPCONTEXT g_hc;
extern const char *ActionString[5];

// ==============================================================

PayloadBayOp::PayloadBayOp (Atlantis *_sts):ImGuiDialog("Payload Operations"), sts(_sts)
{
	m_panel = oapiLoadTexture("Atlantis/r13l.dds");
	int w,h;
	oapiGetSurfaceSize(m_panel, &w, &h);
	m_panelSize = ImVec2(w, h);
	m_scale = 0.5f;

	m_sTex.m_switchup = oapiLoadTexture("Atlantis/switchup.png");
	m_sTex.m_switchdown = oapiLoadTexture("Atlantis/switchdown.png");
	m_sTex.m_switchmid = oapiLoadTexture("Atlantis/switchmid.png");

	SetupSwitches();
	SetupTalkbacks();

	int i;

	// Cargo bay doors
	for (i = 0; i < 2; i++) BayDoor[i] = BD_DISABLE;
	BayDoorOp = BDO_STOP;
	BayDoorStatus.Set (AnimState::CLOSED, 0);

	// Radiators
	for (i = 0; i < 2; i++) MechPwr[i] = MP_OFF;
	for (i = 0; i < 2; i++) RadiatorCtrl[i] = RC_OFF;
	for (i = 0; i < 2; i++) RadLatchCtrl[i] = LC_OFF;
	RadiatorStatus.Set (AnimState::CLOSED, 0);
	RadLatchStatus.Set (AnimState::CLOSED, 0);

	// Ku-band antenna
	KuCtrl = KU_GND;
	KuDirectCtrl = KU_DIRECT_OFF;
	KuAntennaStatus.Set (AnimState::CLOSED, 0);
}

PayloadBayOp::~PayloadBayOp() {
//	oapiReleaseTexture(m_tkbk_label);
	oapiReleaseTexture(m_panel);
	oapiReleaseTexture(m_sTex.m_switchup);
	oapiReleaseTexture(m_sTex.m_switchdown);
	oapiReleaseTexture(m_sTex.m_switchmid);
}

void PayloadBayOp::SetupSwitches() {
	auto nop = [](){};
	m_switches.reserve(11);

	// Bay Door 1
	m_switches.emplace_back(BayDoor[0], ImVec2{204.0f, 180.0f},
		nop,
		[this](){SetDoorAction (AnimState::STOPPED);});

	// Bay Door 2
	m_switches.emplace_back(BayDoor[1], ImVec2{362.0f, 180.0f},
		nop,
		[this](){SetDoorAction (AnimState::STOPPED);});

	// Mech Pwr 1
	m_switches.emplace_back(MechPwr[0], ImVec2{514.0f, 180.0f},
		nop,
		[this](){
			SetRadiatorAction (AnimState::STOPPED);
			SetRadLatchAction (AnimState::STOPPED);
		});

	// Mech Pwr 2
	m_switches.emplace_back(MechPwr[1], ImVec2{674.0f, 180.0f},
		nop,
		[this](){
			SetRadiatorAction (AnimState::STOPPED);
			SetRadLatchAction (AnimState::STOPPED);
		});

	// Bay Door open/close/stop
	m_switches.emplace_back(BayDoorOp, ImVec2{194.0f, 540.0f},
		[this](){SetDoorAction (AnimState::OPENING);},
		[this](){SetDoorAction (AnimState::CLOSING);},
		[this](){SetDoorAction (AnimState::STOPPED);});

	// Radiator A Control
	m_switches.emplace_back(RadiatorCtrl[0], ImVec2{678.0f, 540.0f},
		[this](){SetRadiatorAction (AnimState::OPENING);},
		[this](){SetRadiatorAction (AnimState::CLOSING);},
		[this](){SetRadiatorAction (AnimState::STOPPED);});

	// Radiator B Control
	m_switches.emplace_back(RadiatorCtrl[1], ImVec2{840.0f, 540.0f},
		[this](){SetRadiatorAction (AnimState::OPENING);},
		[this](){SetRadiatorAction (AnimState::CLOSING);},
		[this](){SetRadiatorAction (AnimState::STOPPED);});

	// Radiator A Latch
	m_switches.emplace_back(RadLatchCtrl[0], ImVec2{354.0f, 540.0f},
		[this](){SetRadLatchAction (AnimState::OPENING);},
		[this](){SetRadLatchAction (AnimState::CLOSING);},
		[this](){SetRadLatchAction (AnimState::STOPPED);});

	// Radiator B Latch
	m_switches.emplace_back(RadLatchCtrl[1], ImVec2{512.0f, 540.0f},
		[this](){SetRadLatchAction (AnimState::OPENING);},
		[this](){SetRadLatchAction (AnimState::CLOSING);},
		[this](){SetRadLatchAction (AnimState::STOPPED);});

	// Ku Antenna Deploy
	m_switches.emplace_back(KuCtrl, ImVec2{354.0f, 880.0f},
		[this](){SetKuAntennaAction (AnimState::OPENING);},
		[this](){SetKuAntennaAction (AnimState::CLOSING);},
		[this](){SetKuAntennaAction (AnimState::STOPPED);});


	// Ku Antenna Direct Control
	m_switches.emplace_back(KuDirectCtrl, ImVec2{200.0f, 880.0f},
		[this](){SetKuAntennaAction (AnimState::CLOSING);},
		nop);

}

void PayloadBayOp::SetupTalkbacks() {
	m_talkbacks.reserve(6);
	m_talkbacks.emplace_back(ImVec2(199.0f, 378.0f), BayDoorStatus.action,   std::array<int, 5>{0, 3, 4, 0, 0});
	m_talkbacks.emplace_back(ImVec2(358.0f, 378.0f), RadLatchStatus.action,  std::array<int, 5>{0, 2, 4, 0, 0});
	m_talkbacks.emplace_back(ImVec2(519.0f, 378.0f), RadLatchStatus.action,  std::array<int, 5>{0, 2, 4, 0, 0});
	m_talkbacks.emplace_back(ImVec2(680.0f, 378.0f), RadiatorStatus.action,  std::array<int, 5>{0, 1, 4, 0, 0});
	m_talkbacks.emplace_back(ImVec2(839.0f, 378.0f), RadiatorStatus.action,  std::array<int, 5>{0, 1, 4, 0, 0});
	m_talkbacks.emplace_back(ImVec2(358.0f, 715.0f), KuAntennaStatus.action, std::array<int, 5>{0, 1, 4, 0, 0});
}

void PayloadBayOp::Display() {
	ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0,0));
	ImGui::SetNextWindowSize(m_panelSize * m_scale);
	if(ImGui::Begin(name.c_str(), &active, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoBackground)) {
		// Draw background image
		ImVec2 size = ImGui::GetContentRegionAvail();
		ImGui::Image(m_panel, size);

		// Draw switches
		bool changed = false;
		for(auto &sw: m_switches) {
			changed |= sw.Draw(&m_sTex, ImVec2(24, 64), m_scale);
		}
		if(changed)
			UpdateVC();

		// Draw talkbacks
		for(auto &tb: m_talkbacks) {
			tb.Draw(g_Param.tkbk_label, ImVec2(48, 30), m_scale);
		}

		// Context menu
		ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(6,6));
		if(ImGui::BeginPopupContextWindow("##PlBayOpMenu")) {
			ImGui::Text("Scale");
			if(ImGui::RadioButton("x0.5", m_scale == 0.5f)) {
				m_scale = 0.5f;
				ImGui::CloseCurrentPopup();
			}
			ImGui::SameLine();
			if(ImGui::RadioButton("x1", m_scale == 1.0f)) {
				m_scale = 1.0f;
				ImGui::CloseCurrentPopup();
			}

			ImGui::Separator();
			if (ImGui::Selectable("Help")) {
				g_hc.topic = (char*)"/BayOp.htm";
				oapiOpenHelp (&g_hc);
			}
			if (ImGui::Selectable("Close")) { oapiCloseDialog(this); }
			ImGui::EndPopup();
		}
		ImGui::PopStyleVar();
	}
	ImGui::End();
	ImGui::PopStyleVar();
}

// ==============================================================

void PayloadBayOp::Step (double t, double dt)
{
	// Operate cargo doors
	if (BayDoorStatus.Moving()) {
		double da = dt * DOOR_OPERATING_SPEED;
		if (BayDoorStatus.Closing()) {
			if (BayDoorStatus.pos > 0.0)
				BayDoorStatus.pos = max (0.0, BayDoorStatus.pos-da);
			else
				SetDoorAction (AnimState::CLOSED);
		} else { // door opening
			if (BayDoorStatus.pos < 1.0)
				BayDoorStatus.pos = min (1.0, BayDoorStatus.pos+da);
			else
				SetDoorAction (AnimState::OPEN);
		}
		sts->SetBayDoorPosition (BayDoorStatus.pos);
	}

	// Operate radiators
	if (RadiatorStatus.Moving()) {
		double da = dt * RAD_OPERATING_SPEED;
		if (RadiatorStatus.Closing()) {
			if (RadiatorStatus.pos > 0.0)
				RadiatorStatus.pos = max (0.0, RadiatorStatus.pos-da);
			else
				SetRadiatorAction (AnimState::CLOSED);
		} else { // radiator deploying
			if (RadiatorStatus.pos < 1.0)
				RadiatorStatus.pos = min (1.0, RadiatorStatus.pos+da);
			else
				SetRadiatorAction (AnimState::OPEN);
		}
		sts->SetRadiatorPosition (RadiatorStatus.pos);
	}

	// Operate radiator latches
	if (RadLatchStatus.Moving()) {
		double da = dt * RADLATCH_OPERATING_SPEED;
		if (RadLatchStatus.Closing()) {
			if (RadLatchStatus.pos > 0.0)
				RadLatchStatus.pos = max (0.0, RadLatchStatus.pos-da);
			else
				SetRadLatchAction (AnimState::CLOSED);
		} else { // radiator latches releasing
			if (RadLatchStatus.pos < 1.0)
				RadLatchStatus.pos = min (1.0, RadLatchStatus.pos+da);
			else
				SetRadLatchAction (AnimState::OPEN);
		}
		sts->SetRadLatchPosition (RadLatchStatus.pos);
	}

	// Operate Ku-band antenna
	if (KuAntennaStatus.Moving()) {
		double da = dt * KU_OPERATING_SPEED;
		if (KuAntennaStatus.Closing()) {
			if (KuAntennaStatus.pos > 0.0)
				KuAntennaStatus.pos = max (0.0, KuAntennaStatus.pos-da);
			else
				SetKuAntennaAction (AnimState::CLOSED);
		} else { // antenna deploying
			if (KuAntennaStatus.pos < 1.0)
				KuAntennaStatus.pos = min (1.0, KuAntennaStatus.pos+da);
			else
				SetKuAntennaAction (AnimState::OPEN);
		}
		sts->SetKuAntennaPosition (KuAntennaStatus.pos);
	}
}

// ==============================================================

void PayloadBayOp::SetDoorAction (AnimState::Action action, bool simple)
{
	int i;

	if (KuAntennaStatus.action != AnimState::CLOSED) return;
	// operate payload bay doors only if Ku-band antenna is stowed

	if (RadiatorStatus.action != AnimState::CLOSED) return;
	// operate payload bay doors only if radiators are stowed

	if (simple)
		for (i = 0; i < 2; i++) BayDoor[i] = BD_ENABLE;
	// Make sure both systems are online

	for (i = 0; i < 2; i++)
		if ((action != AnimState::STOPPED) && (BayDoor[i] != BD_ENABLE)) return;
	// operate doors only if both systems are enabled

	if (action == AnimState::STOPPED && BayDoorStatus.Static()) return;
	// stopping doesn't make sense if the doors are already fully open or closed

	BayDoorStatus.action = action;
	if (action == AnimState::CLOSED)    sts->SetBayDoorPosition (BayDoorStatus.pos = 0.0);
	else if (action == AnimState::OPEN) sts->SetBayDoorPosition (BayDoorStatus.pos = 1.0);
	sts->RecordEvent ("CARGODOOR", ActionString[action]);

	UpdateVC();
}

// ==============================================================

void PayloadBayOp::SetRadiatorAction (AnimState::Action action)
{
	int i;

	if (BayDoorStatus.action != AnimState::OPEN) return;
	// allow radiator operation only once the bay doors are fully open

	for (i = 0; i < 2; i++)
		if ((action != AnimState::STOPPED) && (MechPwr[i] != MP_ON)) return;
	// operate radiators only if power is online

	for (i = 0; i < 2; i++) { // check both systems are set correctly
		if (action == AnimState::OPENING && RadiatorCtrl[i] != RC_DEPLOY) return;
		if (action == AnimState::CLOSING && RadiatorCtrl[i] != RC_STOW) return;
	}

	if (action == AnimState::STOPPED && RadiatorStatus.Static()) return;
	// stopping doesn't make sense if the radiators are already fully deployed or stowed

	if (action == AnimState::OPENING && RadiatorStatus.Closed() && !RadLatchStatus.Open()) return;
	// don't deploy radiators if the latches are not fully released

	RadiatorStatus.action = action;
	sts->RecordEvent ("RADIATOR", ActionString[action]);

	UpdateVC();
}

// ==============================================================

void PayloadBayOp::RevertDoorAction ()
{
	SetDoorAction (BayDoorStatus.action == AnimState::CLOSED || BayDoorStatus.action == AnimState::CLOSING ?
		AnimState::OPENING : AnimState::CLOSING, true);
}

// ==============================================================

void PayloadBayOp::SetRadLatchAction (AnimState::Action action)
{
	int i;

	for (i = 0; i < 2; i++)
		if ((action != AnimState::STOPPED) && (MechPwr[i] != MP_ON)) return;
	// operate radiator latches only if power is online

	for (i = 0; i < 2; i++) { // check both systems are set correctly
		if (action == AnimState::OPENING && RadLatchCtrl[i] != LC_RELEASE) return;
		if (action == AnimState::CLOSING && RadLatchCtrl[i] != LC_LATCH) return;
	}

	if (action == AnimState::STOPPED && RadLatchStatus.Static()) return;
	// stopping doesn't make sense if the radiators are already fully deployed or stowed

	RadLatchStatus.action = action;
	sts->RecordEvent ("RADLATCH", ActionString[action]);

	UpdateVC();
}

// ==============================================================

void PayloadBayOp::SetKuAntennaAction (AnimState::Action action)
{
	if (BayDoorStatus.action != AnimState::OPEN) return;
	// allow radiator operation only once the bay doors are fully open

	if (action == AnimState::STOPPED && KuAntennaStatus.Static()) return;
	// stopping doesn't make sense if the doors are already fully open or closed

	KuAntennaStatus.action = action;
	sts->RecordEvent ("KUBAND", ActionString[action]);

	UpdateVC();
}

// ==============================================================

void PayloadBayOp::RevertKuAntennaAction ()
{
	SetKuAntennaAction (KuAntennaStatus.action == AnimState::CLOSED || KuAntennaStatus.action == AnimState::CLOSING ?
		AnimState::OPENING : AnimState::CLOSING);
}

// ==============================================================

bool PayloadBayOp::ParseScenarioLine (char *line)
{
	if (!_strnicmp (line, "CARGODOOR", 9)) {
		sscan_state (line+9, BayDoorStatus);
		return true;
	} else if (!_strnicmp (line, "RADIATOR", 8)) {
		sscan_state (line+8, RadiatorStatus);
		return true;
	} else if (!_strnicmp (line, "RADLATCH", 8)) {
		sscan_state (line+8, RadLatchStatus);
		return true;
	} else if (!_strnicmp (line, "KUBAND", 6)) {
		sscan_state (line+6, KuAntennaStatus);
		return true;
	}
	return false;
}

// ==============================================================

void PayloadBayOp::SaveState (FILEHANDLE scn)
{
	if (!BayDoorStatus.Closed())
		WriteScenario_state (scn, (char*)"CARGODOOR", BayDoorStatus);
	if (!RadiatorStatus.Closed())
		WriteScenario_state (scn, (char*)"RADIATOR", RadiatorStatus);
	if (!RadLatchStatus.Closed())
		WriteScenario_state (scn, (char*)"RADLATCH", RadLatchStatus);
	if (!KuAntennaStatus.Closed())
		WriteScenario_state (scn, (char*)"KUBAND", KuAntennaStatus);
}

// ==============================================================

void PayloadBayOp::DefineAnimations (UINT vcidx)
{
	static VECTOR3 switch_rot = {0,0,1};
	static VECTOR3 switch_row1 = {1.3068,2.1991,12.7983};
	static VECTOR3 switch_row2 = {1.2132,2.1377,12.7983};
	static VECTOR3 switch_row3 = {1.1244,2.0794,12.7983};

	// Animations for switches on panel R13L in the VC
	static UINT VC_R13L_S1_Grp = GRP_SwitchR13L_1_VC;
	static MGROUP_ROTATE VC_R13L_S1 (vcidx, &VC_R13L_S1_Grp, 1,
		switch_row1, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[0] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[0], 0, 1, &VC_R13L_S1);

	static UINT VC_R13L_S2_Grp = GRP_SwitchR13L_2_VC;
	static MGROUP_ROTATE VC_R13L_S2 (vcidx, &VC_R13L_S2_Grp, 1,
		switch_row1, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[1] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[1], 0, 1, &VC_R13L_S2);

	static UINT VC_R13L_S3_Grp = GRP_SwitchR13L_3_VC;
	static MGROUP_ROTATE VC_R13L_S3 (vcidx, &VC_R13L_S3_Grp, 1,
		switch_row1, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[2] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[2], 0, 1, &VC_R13L_S3);

	static UINT VC_R13L_S4_Grp = GRP_SwitchR13L_4_VC;
	static MGROUP_ROTATE VC_R13L_S4 (vcidx, &VC_R13L_S4_Grp, 1,
		switch_row1, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[3] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[3], 0, 1, &VC_R13L_S4);

	static UINT VC_R13L_S5_Grp = GRP_SwitchR13L_5_VC;
	static MGROUP_ROTATE VC_R13L_S5 (vcidx, &VC_R13L_S5_Grp, 1,
		switch_row2, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[4] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[4], 0, 1, &VC_R13L_S5);

	static UINT VC_R13L_S6_Grp = GRP_SwitchR13L_6_VC;
	static MGROUP_ROTATE VC_R13L_S6 (vcidx, &VC_R13L_S6_Grp, 1,
		switch_row2, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[5] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[5], 0, 1, &VC_R13L_S6);

	static UINT VC_R13L_S7_Grp = GRP_SwitchR13L_7_VC;
	static MGROUP_ROTATE VC_R13L_S7 (vcidx, &VC_R13L_S7_Grp, 1,
		switch_row2, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[6] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[6], 0, 1, &VC_R13L_S7);

	static UINT VC_R13L_S8_Grp = GRP_SwitchR13L_8_VC;
	static MGROUP_ROTATE VC_R13L_S8 (vcidx, &VC_R13L_S8_Grp, 1,
		switch_row2, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[7] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[7], 0, 1, &VC_R13L_S8);

	static UINT VC_R13L_S9_Grp = GRP_SwitchR13L_9_VC;
	static MGROUP_ROTATE VC_R13L_S9 (vcidx, &VC_R13L_S9_Grp, 1,
		switch_row2, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[8] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[8], 0, 1, &VC_R13L_S9);

	static UINT VC_R13L_S10_Grp = GRP_SwitchR13L_10_VC;
	static MGROUP_ROTATE VC_R13L_S10 (vcidx, &VC_R13L_S10_Grp, 1,
		switch_row3, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[9] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[9], 0, 1, &VC_R13L_S10);

	static UINT VC_R13L_S11_Grp = GRP_SwitchR13L_11_VC;
	static MGROUP_ROTATE VC_R13L_S11 (vcidx, &VC_R13L_S11_Grp, 1,
		switch_row3, switch_rot, (float)(90.0*RAD));
	anim_VC_R13L[10] = sts->CreateAnimation (0.5);
	sts->AddAnimationComponent (anim_VC_R13L[10], 0, 1, &VC_R13L_S11);
}

// ==============================================================

void PayloadBayOp::RegisterVC ()
{
	SURFHANDLE tkbk_tex = oapiGetTextureHandle (sts->hOrbiterVCMesh, 5);

	// register the complete panel for mouse events
	oapiVCRegisterArea (AID_R13L, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (AID_R13L, _V(1.3543,2.23023,12.8581), _V(1.3543,2.23023,12.5486), _V(1.0868,2.0547,12.8581), _V(1.0868,2.0547,12.5486));

	// register the talkbacks
	oapiVCRegisterArea (AID_R13L_TKBK1, _R(  0,0, 32,18), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_NONE, tkbk_tex);
	oapiVCRegisterArea (AID_R13L_TKBK2, _R( 32,0, 64,18), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_NONE, tkbk_tex);
	oapiVCRegisterArea (AID_R13L_TKBK3, _R( 64,0, 96,18), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_NONE, tkbk_tex);
	oapiVCRegisterArea (AID_R13L_TKBK4, _R( 96,0,128,18), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_NONE, tkbk_tex);
	oapiVCRegisterArea (AID_R13L_TKBK5, _R(128,0,160,18), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_NONE, tkbk_tex);
	oapiVCRegisterArea (AID_R13L_TKBK6, _R(160,0,192,18), PANEL_REDRAW_USER, PANEL_MOUSE_IGNORE, PANEL_MAP_NONE, tkbk_tex);

	for (int i = 0; i < 6; i++)
		tkbk_state[i] = -1;
}

// ==============================================================

void PayloadBayOp::UpdateVC ()
{
	int i;
	for (i = 0; i < 2; i++) {
		bool enable = (BayDoor[i] == BD_ENABLE);
		sts->SetAnimation (anim_VC_R13L[i], enable ? 0:1);
	}
	for (i = 0; i < 2; i++) {
		bool enable = (MechPwr[i] == MP_ON);
		sts->SetAnimation (anim_VC_R13L[i+2], enable ? 0:1);
	}
	sts->SetAnimation(anim_VC_R13L[4], BayDoorOp == BDO_OPEN ? 0 : BayDoorOp == BDO_CLOSE ? 1 : 0.5);
	for (i = 0; i < 2; i++) {
		sts->SetAnimation (anim_VC_R13L[5+i], RadLatchCtrl[i] == LC_RELEASE ? 0 : RadLatchCtrl[i] == LC_LATCH ? 1 : 0.5);
	}
	for (i = 0; i < 2; i++) {
		sts->SetAnimation (anim_VC_R13L[7+i], RadiatorCtrl[i] == RC_DEPLOY ? 0 : RadiatorCtrl[i] == RC_STOW ? 1 : 0.5);
	}
	sts->SetAnimation (anim_VC_R13L[9], KuDirectCtrl == KU_DIRECT_ON ? 0 : 1);
	sts->SetAnimation (anim_VC_R13L[10], KuCtrl == KU_DEPLOY ? 0 : KuCtrl == KU_STOW ? 1 : 0.5);

	for (i = 0; i < 6; i++)
		oapiVCTriggerRedrawArea (-1, AID_R13L_TKBK1+i);
}

// ==============================================================

bool PayloadBayOp::VCDrawTalkback (SURFHANDLE surf, int idx, int label)
{
	if (tkbk_state[idx] == label) return false; // nothing to do
	tkbk_state[idx] = label;
	oapiBlt (surf, g_Param.tkbk_label, 0, 0, label*32, 0, 32, 18);
	return true;
}

// ==============================================================

bool PayloadBayOp::VCMouseEvent (int id, int event, VECTOR3 &p)
{
	if (id != AID_R13L) return false;
	bool action = false;

	if (p.y >= 0.1113 && p.y <= 0.2461) {
		if (p.x >= 0.1387 && p.x <= 0.2617) {
			BayDoor[0] = (p.y < 0.1787 ? BD_ENABLE:BD_DISABLE);
			if (BayDoor[0] == BD_DISABLE) SetDoorAction (AnimState::STOPPED);
			action = true;
		} else if (p.x >= 0.2910 && p.x <= 0.4180) {
			BayDoor[1] = (p.y < 0.1787 ? BD_ENABLE:BD_DISABLE);
			if (BayDoor[1] == BD_DISABLE) SetDoorAction (AnimState::STOPPED);
			action = true;
		} else if (p.x >= 0.4395 && p.x <= 0.5625) {
			MechPwr[0] = (p.y < 0.1787 ? MP_ON:MP_OFF);
			if (MechPwr[0] == MP_OFF) {
				SetRadiatorAction (AnimState::STOPPED);
				SetRadLatchAction (AnimState::STOPPED);
			}
			action = true;
		} else if (p.x >= 0.5996 && p.x < 0.7188) {
			MechPwr[1] = (p.y < 0.1787 ? MP_ON:MP_OFF);
			if (MechPwr[1] == MP_OFF) {
				SetRadiatorAction (AnimState::STOPPED);
				SetRadLatchAction (AnimState::STOPPED);
			}
			action = true;
		}
	} else if (p.y >= 0.4590 && p.y <= 0.6016) {
		bool up = (p.y < 0.5303);
		if (p.x >= 0.125 && p.x <= 0.2539) {
			BayDoorOp = (up ? (BayDoorOp == BDO_STOP ? BDO_OPEN : BDO_STOP) : BayDoorOp == BDO_STOP ? BDO_CLOSE : BDO_STOP);
			SetDoorAction (BayDoorOp == BDO_OPEN ? AnimState::OPENING : BayDoorOp == BDO_CLOSE ? AnimState::CLOSING : AnimState::STOPPED);
			action = true;
		} else if (p.x >= 0.2832 && p.x <= 0.4082) {
			RadLatchCtrl[0] = (up ? (RadLatchCtrl[0] == LC_OFF ? LC_RELEASE : LC_OFF) : RadLatchCtrl[0] == LC_OFF ? LC_LATCH : LC_OFF);
			SetRadLatchAction (RadLatchCtrl[0] == LC_RELEASE ? AnimState::OPENING : RadLatchCtrl[0] == LC_LATCH ? AnimState::CLOSING : AnimState::STOPPED);
			action = true;
		} else if (p.x >= 0.4414 && p.x <= 0.5645) {
			RadLatchCtrl[1] = (up ? (RadLatchCtrl[1] == LC_OFF ? LC_RELEASE : LC_OFF) : RadLatchCtrl[1] == LC_OFF ? LC_LATCH : LC_OFF);
			SetRadLatchAction (RadLatchCtrl[1] == LC_RELEASE ? AnimState::OPENING : RadLatchCtrl[1] == LC_LATCH ? AnimState::CLOSING : AnimState::STOPPED);
			action = true;
		} else if (p.x >= 0.5996 && p.x <= 0.7227) {
			RadiatorCtrl[0] = (up ? (RadiatorCtrl[0] == RC_OFF ? RC_DEPLOY : RC_OFF) : RadiatorCtrl[0] == RC_OFF ? RC_STOW : RC_OFF);
			SetRadiatorAction (RadiatorCtrl[0] == RC_DEPLOY ? AnimState::OPENING : RadiatorCtrl[0] == RC_STOW ? AnimState::CLOSING : AnimState::STOPPED);
			action = true;
		} else if (p.x >= 0.7559 && p.x <= 0.8789) {
			RadiatorCtrl[1] = (up ? (RadiatorCtrl[1] == RC_OFF ? RC_DEPLOY : RC_OFF) : RadiatorCtrl[1] == RC_OFF ? RC_STOW : RC_OFF);
			SetRadiatorAction (RadiatorCtrl[1] == RC_DEPLOY ? AnimState::OPENING : RadiatorCtrl[1] == RC_STOW ? AnimState::CLOSING : AnimState::STOPPED);
			action = true;
		}
	} else if (p.y >= 0.7891 && p.y <= 0.9219) {
		bool up = (p.y < 0.8555);
		if (p.x >= 0.1328 && p.x <= 0.2559) {
			KuDirectCtrl = (up ? KU_DIRECT_ON : KU_DIRECT_OFF);
			if (KuDirectCtrl == KU_DIRECT_ON) SetKuAntennaAction (AnimState::CLOSING);
			action = true;
		} else if (p.x >= 0.2871 && p.x <= 0.4082) {
			KuCtrl = (up ? (KuCtrl == KU_GND ? KU_DEPLOY : KU_GND) : KuCtrl == KU_GND ? KU_STOW : KU_GND);
			SetKuAntennaAction (KuCtrl == KU_DEPLOY ? AnimState::OPENING : KuCtrl == KU_STOW ? AnimState::CLOSING : AnimState::STOPPED);
			action = true;
		}
	}

	if (action) {
		UpdateVC ();
	}
	return false;
}

// ==============================================================

bool PayloadBayOp::VCRedrawEvent (int id, int event, SURFHANDLE surf)
{
	switch (id) {
	case AID_R13L_TKBK1: {
		static int label[5] = {0,3,4,0,0};
		return VCDrawTalkback (surf, 0, label[BayDoorStatus.action]);
		}
	case AID_R13L_TKBK2:
	case AID_R13L_TKBK3: {
		static int label[5] = {0,2,4,0,0};
		return VCDrawTalkback (surf, id-AID_R13L_TKBK1, label[RadLatchStatus.action]);
		}
	case AID_R13L_TKBK4:
	case AID_R13L_TKBK5: {
		static int label[5] = {0,1,4,0,0};
		return VCDrawTalkback (surf, id-AID_R13L_TKBK1, label[RadiatorStatus.action]);
		}
	case AID_R13L_TKBK6: {
		static int label[5] = {0,1,4,0,0};
		return VCDrawTalkback (surf, id-AID_R13L_TKBK1, label[KuAntennaStatus.action]);
		}
	}
	return false;
}
