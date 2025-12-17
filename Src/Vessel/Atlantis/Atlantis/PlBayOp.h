// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __PLBAYOP_H
#define __PLBAYOP_H

#include "Atlantis.h"
#define IMGUI_DEFINE_MATH_OPERATORS
#include "imgui.h"
#include "imgui_extras.h"
#include <functional>
#include <vector>
#include <array>

// ==============================================================
// class PayloadBayOp
// Manages payload bay operations (bay doors and radiators),
// both the physical state of the systems, and the state of
// the user interface (panel switches, etc.)
// ==============================================================


struct SwitchTex {
	SURFHANDLE m_switchup;
	SURFHANDLE m_switchdown;
	SURFHANDLE m_switchmid;
};

// States: 0:up, 1:mid, 2:down
static bool Switch(int *state, ImVec2 centerPos, ImVec2 size, SwitchTex *tex, bool twostate = false) {
	bool changed = false;
	ImGui::PushID(state);
	SURFHANDLE s = tex->m_switchup;
	switch(*state) {
		case 0: s = tex->m_switchup; break;
		case 1: if(!twostate) {s = tex->m_switchmid; break;}
		case 2: s = tex->m_switchdown; break;
	}

	ImVec2 cornerPos = centerPos - size / 2.0f;

	// Draw the switch
	ImGui::SetCursorPos(cornerPos);
	ImGui::Image(s, size);

	// Use 2 invisible buttons to detect clicks on the upper/lower portion
	ImVec2 btnSize = ImVec2(size.x, size.y / 2.0f);
	ImGui::SetCursorPos(cornerPos);
	if(ImGui::InvisibleButton("##top", btnSize)) {
		if(*state != 0) {
			(*state)--;
			changed = true;
		}
	}

	ImGui::SetCursorPos(cornerPos + ImVec2(0, size.y/2.0f));
	if(ImGui::InvisibleButton("##bottom", btnSize)) {
		if(*state != 2) {
			(*state)++;
			if(twostate && *state == 2) *state = 1;
			changed = true;
		}
	}

	ImGui::PopID();
	return changed;
}

class Talkback {
public:
	ImVec2 m_centerPos;
	const AnimState::Action &m_idx;
	float m_idx2offset[5];
	Talkback(ImVec2 centerPos, const AnimState::Action &idx, const std::array<int, 5> &labels):m_idx(idx), m_centerPos(centerPos) {
		for(int i = 0; i< 5; i++) {
			m_idx2offset[i] = (float)labels[i] / 8.0f; 
		}
	}

	void Draw(SURFHANDLE surf, ImVec2 size, float scale) {
		ImVec2 cornerPos = m_centerPos - size / 2.0f;
		ImVec2 uv0 = {0.0f, 0.0f};
		ImVec2 uv1 = {1.0f, 0.5f};
		float xoffset = m_idx2offset[m_idx];
		uv0.x = xoffset;
		uv1.x = xoffset + 1.0/8.0;

		ImGui::SetCursorPos(cornerPos * scale);
		ImGui::Image(surf, size * scale, uv0, uv1);
	}
};

class ToggleSwitch {
public:
	std::function<void (void)> m_onUp;
	std::function<void (void)> m_onMid;
	std::function<void (void)> m_onDown;
	bool m_twostate;
	int &m_state;
	ImVec2 m_centerPos;
	
	ToggleSwitch(int &state, ImVec2 centerPos, std::function<void (void)> up, std::function<void (void)> down):ToggleSwitch(state, centerPos, up, down, [](){}) {
		m_twostate = true;
	}
	ToggleSwitch(int &state, ImVec2 centerPos, std::function<void (void)> up, std::function<void (void)> down, std::function<void (void)> mid):m_state(state) {
		m_onUp = up;
		m_onMid = mid;
		m_onDown = down;
		m_twostate = false;
		m_centerPos = centerPos;
	}

	bool Draw(SwitchTex *tex, ImVec2 size, float scale = 1.0f) {
		if(Switch(&m_state, m_centerPos * scale, size * scale, tex, m_twostate)) {
			switch(m_state) {
				case 0: m_onUp(); break;
				case 1: m_onMid(); break;
				case 2: m_onDown(); break;
			}
			return true;
		}
		return false;
	}
};

class PayloadBayOp: public ImGuiDialog {
	friend class Atlantis;
public:
	PayloadBayOp (Atlantis *_sts);
	~PayloadBayOp();
	void DefineAnimations (UINT vcidx);
	void RegisterVC ();
	void UpdateVC ();
	bool VCMouseEvent (int id, int event, VECTOR3 &p);
	bool VCRedrawEvent (int id, int event, SURFHANDLE surf);

	void Step (double t, double dt);
	inline AnimState::Action GetDoorAction () const { return BayDoorStatus.action; }
	void SetDoorAction (AnimState::Action action, bool simple = false);
	void RevertDoorAction (); // simplified operation

	inline AnimState::Action GetRadiatorAction () const { return RadiatorStatus.action; }
	void SetRadiatorAction (AnimState::Action action);

	void SetRadLatchAction (AnimState::Action action);

	void SetKuAntennaAction (AnimState::Action action);
	void RevertKuAntennaAction (); // simplified operation

	bool ParseScenarioLine (char *line);
	void SaveState (FILEHANDLE scn);

	void SetupSwitches();
	void SetupTalkbacks();
	void Display() override;
	void OnDraw() override {}
private:
	SURFHANDLE m_panel;
	SwitchTex m_sTex;
	ImVec2 m_panelSize;
	float m_scale;
	std::vector<ToggleSwitch> m_switches;
	std::vector<Talkback> m_talkbacks;

	Atlantis *sts; // vessel instance pointer
	bool VCDrawTalkback (SURFHANDLE surf, int idx, int label);

	// status of control switches
	enum :int {BD_ENABLE,BD_DISABLE};
	enum :int {BDO_OPEN,BDO_STOP,BDO_CLOSE};
	enum :int {MP_ON,MP_OFF};
	enum :int {LC_RELEASE,LC_OFF,LC_LATCH};
	enum :int {RC_DEPLOY,RC_OFF,RC_STOW};
	enum :int {KU_DEPLOY,KU_GND,KU_STOW};
	enum :int {KU_DIRECT_ON, KU_DIRECT_OFF};

	int BayDoor[2];
	int BayDoorOp;
	int MechPwr[2];
	int RadLatchCtrl[2];
	int RadiatorCtrl[2];
	int KuCtrl;
	int KuDirectCtrl;

	// VC switch animations
	UINT anim_VC_R13L[11];
	int tkbk_state[6];

	// physical status
	AnimState BayDoorStatus;
	AnimState RadiatorStatus;
	AnimState RadLatchStatus;
	AnimState KuAntennaStatus;
};

#endif // !__PLBAYOP_H