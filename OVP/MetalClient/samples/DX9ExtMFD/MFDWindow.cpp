// ==============================================================
// Copyright (C) 2006-2026 Martin Schweiger
// Licensed under the MIT License
// ==============================================================

#define IMGUI_DEFINE_MATH_OPERATORS
#include "MFDWindow.h"
#include "imgui.h"
#include "imgui_extras.h"
#include "IconsFontAwesome6.h"
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <vector>

static std::vector<bool> g_usedWindowIds;

static int GetFreeWindowId ()
{
	for (size_t i = 0; i < g_usedWindowIds.size(); ++i) {
		if (!g_usedWindowIds[i]) {
			g_usedWindowIds[i] = true;
			return (int)i + 1;
		}
	}
	g_usedWindowIds.push_back(true);
	return (int)g_usedWindowIds.size();
}

static void FreeWindowId (int id)
{
	int index = id - 1;
	if (index >= 0 && index < (int)g_usedWindowIds.size())
		g_usedWindowIds[index] = false;
}

static DWORD MapImGuiToOAPIKey (ImGuiKey key)
{
	switch (key) {
	case ImGuiKey_Escape: return OAPI_KEY_ESCAPE;
	case ImGuiKey_1: return OAPI_KEY_1;
	case ImGuiKey_2: return OAPI_KEY_2;
	case ImGuiKey_3: return OAPI_KEY_3;
	case ImGuiKey_4: return OAPI_KEY_4;
	case ImGuiKey_5: return OAPI_KEY_5;
	case ImGuiKey_6: return OAPI_KEY_6;
	case ImGuiKey_7: return OAPI_KEY_7;
	case ImGuiKey_8: return OAPI_KEY_8;
	case ImGuiKey_9: return OAPI_KEY_9;
	case ImGuiKey_0: return OAPI_KEY_0;
	case ImGuiKey_Minus: return OAPI_KEY_MINUS;
	case ImGuiKey_Equal: return OAPI_KEY_EQUALS;
	case ImGuiKey_Backspace: return OAPI_KEY_BACK;
	case ImGuiKey_Tab: return OAPI_KEY_TAB;
	case ImGuiKey_Q: return OAPI_KEY_Q;
	case ImGuiKey_W: return OAPI_KEY_W;
	case ImGuiKey_E: return OAPI_KEY_E;
	case ImGuiKey_R: return OAPI_KEY_R;
	case ImGuiKey_T: return OAPI_KEY_T;
	case ImGuiKey_Y: return OAPI_KEY_Y;
	case ImGuiKey_U: return OAPI_KEY_U;
	case ImGuiKey_I: return OAPI_KEY_I;
	case ImGuiKey_O: return OAPI_KEY_O;
	case ImGuiKey_P: return OAPI_KEY_P;
	case ImGuiKey_LeftBracket: return OAPI_KEY_LBRACKET;
	case ImGuiKey_RightBracket: return OAPI_KEY_RBRACKET;
	case ImGuiKey_Enter: return OAPI_KEY_RETURN;
	case ImGuiKey_LeftCtrl: return OAPI_KEY_LCONTROL;
	case ImGuiKey_RightCtrl: return OAPI_KEY_RCONTROL;
	case ImGuiKey_A: return OAPI_KEY_A;
	case ImGuiKey_S: return OAPI_KEY_S;
	case ImGuiKey_D: return OAPI_KEY_D;
	case ImGuiKey_F: return OAPI_KEY_F;
	case ImGuiKey_G: return OAPI_KEY_G;
	case ImGuiKey_H: return OAPI_KEY_H;
	case ImGuiKey_J: return OAPI_KEY_J;
	case ImGuiKey_K: return OAPI_KEY_K;
	case ImGuiKey_L: return OAPI_KEY_L;
	case ImGuiKey_Semicolon: return OAPI_KEY_SEMICOLON;
	case ImGuiKey_Apostrophe: return OAPI_KEY_APOSTROPHE;
	case ImGuiKey_GraveAccent: return OAPI_KEY_GRAVE;
	case ImGuiKey_LeftShift: return OAPI_KEY_LSHIFT;
	case ImGuiKey_Backslash: return OAPI_KEY_BACKSLASH;
	case ImGuiKey_Z: return OAPI_KEY_Z;
	case ImGuiKey_X: return OAPI_KEY_X;
	case ImGuiKey_C: return OAPI_KEY_C;
	case ImGuiKey_V: return OAPI_KEY_V;
	case ImGuiKey_B: return OAPI_KEY_B;
	case ImGuiKey_N: return OAPI_KEY_N;
	case ImGuiKey_M: return OAPI_KEY_M;
	case ImGuiKey_Comma: return OAPI_KEY_COMMA;
	case ImGuiKey_Period: return OAPI_KEY_PERIOD;
	case ImGuiKey_Slash: return OAPI_KEY_SLASH;
	case ImGuiKey_RightShift: return OAPI_KEY_RSHIFT;
	case ImGuiKey_KeypadMultiply: return OAPI_KEY_MULTIPLY;
	case ImGuiKey_LeftAlt: return OAPI_KEY_LALT;
	case ImGuiKey_RightAlt: return OAPI_KEY_RALT;
	case ImGuiKey_Space: return OAPI_KEY_SPACE;
	case ImGuiKey_CapsLock: return OAPI_KEY_CAPITAL;
	case ImGuiKey_F1: return OAPI_KEY_F1;
	case ImGuiKey_F2: return OAPI_KEY_F2;
	case ImGuiKey_F3: return OAPI_KEY_F3;
	case ImGuiKey_F4: return OAPI_KEY_F4;
	case ImGuiKey_F5: return OAPI_KEY_F5;
	case ImGuiKey_F6: return OAPI_KEY_F6;
	case ImGuiKey_F7: return OAPI_KEY_F7;
	case ImGuiKey_F8: return OAPI_KEY_F8;
	case ImGuiKey_F9: return OAPI_KEY_F9;
	case ImGuiKey_F10: return OAPI_KEY_F10;
	case ImGuiKey_NumLock: return OAPI_KEY_NUMLOCK;
	case ImGuiKey_ScrollLock: return OAPI_KEY_SCROLL;
	case ImGuiKey_Keypad7: return OAPI_KEY_NUMPAD7;
	case ImGuiKey_Keypad8: return OAPI_KEY_NUMPAD8;
	case ImGuiKey_Keypad9: return OAPI_KEY_NUMPAD9;
	case ImGuiKey_KeypadSubtract: return OAPI_KEY_SUBTRACT;
	case ImGuiKey_Keypad4: return OAPI_KEY_NUMPAD4;
	case ImGuiKey_Keypad5: return OAPI_KEY_NUMPAD5;
	case ImGuiKey_Keypad6: return OAPI_KEY_NUMPAD6;
	case ImGuiKey_KeypadAdd: return OAPI_KEY_ADD;
	case ImGuiKey_Keypad1: return OAPI_KEY_NUMPAD1;
	case ImGuiKey_Keypad2: return OAPI_KEY_NUMPAD2;
	case ImGuiKey_Keypad3: return OAPI_KEY_NUMPAD3;
	case ImGuiKey_Keypad0: return OAPI_KEY_NUMPAD0;
	case ImGuiKey_KeypadDecimal: return OAPI_KEY_DECIMAL;
	case ImGuiKey_F11: return OAPI_KEY_F11;
	case ImGuiKey_F12: return OAPI_KEY_F12;
	default: return 0;
	}
}

struct Layout {
	float gap;
	float BW, BH;
	float ds;
};

static Layout ComputeLayout (float cw)
{
	float fs = ImGui::GetFontSize();
	Layout l;
	l.gap = std::max(3.0f, std::round(fs * 0.25f));
	float bw = (cw * 35.0f) / 300.0f;
	l.BW = std::round(std::clamp(bw, 2.5f * fs, 5.0f * fs));
	l.BH = std::round(std::clamp((bw * 2.0f) / 3.0f, ImGui::GetFrameHeight(), 3.0f * fs));
	l.ds = std::floor(cw - 2.0f * (l.BW + l.gap) - 2.0f * l.gap);
	return l;
}

static float ContentHeight (float cw)
{
	Layout l = ComputeLayout(cw);
	return l.ds + 3.0f * l.gap + l.BH;
}

static float MinContentWidth ()
{
	float fs = ImGui::GetFontSize();
	float cw = 8.0f * fs;
	while (ComputeLayout(cw).ds < 8.0f * fs) cw += 1.0f;
	return cw;
}

static ImVec2 Decoration ()
{
	const ImGuiStyle &style = ImGui::GetStyle();
	return ImVec2(2.0f * style.WindowPadding.x, ImGui::GetFrameHeight() + 2.0f * style.WindowPadding.y);
}

static void KeepAspect (ImGuiSizeCallbackData *data)
{
	ImVec2 dec = Decoration();
	float minCw = MinContentWidth();
	float cw = data->DesiredSize.x - dec.x;
	if (ImGui::GetMouseCursor() == ImGuiMouseCursor_ResizeNS) {
		float ch = data->DesiredSize.y - dec.y;
		float lo = minCw, hi = std::max(minCw, 2.0f * ch + 1000.0f);
		for (int i = 0; i < 40; i++) {
			float mid = 0.5f * (lo + hi);
			if (ContentHeight(mid) < ch) lo = mid; else hi = mid;
		}
		cw = std::round(hi);
	}
	cw = std::max(cw, minCw);
	data->DesiredSize.x = cw + dec.x;
	data->DesiredSize.y = ContentHeight(cw) + dec.y;
}

class DlgExtMFD : public ImGuiDialog {
public:
	DlgExtMFD (const std::string &name, MFDWindow *mfd) : ImGuiDialog(name.c_str()), mfd(mfd) {}
	void Display () override;
	void OnDraw () override;
	void OnClose () override { oapiUnregisterExternMFD(mfd); }

private:
	void MFDButton (int id, const char *label, const ImVec2 &pos, const ImVec2 &size);
	MFDWindow *mfd;
};

void DlgExtMFD::Display ()
{
	ImVec2 dec = Decoration();
	float cw = 360.0f;
	ImGui::SetNextWindowPos(ImGui::GetMousePos() + ImVec2(-10.0f, 10.0f), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSize(ImVec2(cw + dec.x, ContentHeight(cw) + dec.y), ImGuiCond_FirstUseEver);
	float minCw = MinContentWidth();
	ImGui::SetNextWindowSizeConstraints(ImVec2(minCw + dec.x, ContentHeight(minCw) + dec.y), ImVec2(FLT_MAX, FLT_MAX), KeepAspect);

	char title[256] = "DX9 MFD [";
	size_t n = strlen(title);
	oapiGetObjectName(mfd->GetVessel(), title + n, (int)(sizeof(title) - n - 64));
	snprintf(title + strlen(title), sizeof(title) - strlen(title), "]###%s", name.c_str());

	bool visible = ImGui::Begin(title, &active, ImGuiWindowFlags_NoScrollbar | ImGuiWindowFlags_NoScrollWithMouse);

	if (ImGui::MenuButton(ICON_FA_CIRCLE_QUESTION, "Help"))
		mfd->OpenModeHelp();

	bool stick = mfd->GetStickToVessel();
	if (ImGui::MenuButton(stick ? ICON_FA_THUMBTACK : ICON_FA_THUMBTACK_SLASH,
		stick ? "Unpin this MFD from the vessel" : "Pin this MFD to the current vessel", ImGui::GetFontSize() * 1.7f))
		mfd->StickToVessel(!stick);

	if (visible) OnDraw();
	ImGui::End();
	if (!active) OnClose();
}

void DlgExtMFD::MFDButton (int id, const char *label, const ImVec2 &pos, const ImVec2 &size)
{
	ImGui::PushID(id);
	ImGui::SetCursorPos(pos);
	ImGui::Button(label ? label : "", size);
	if (ImGui::IsItemActivated()) mfd->ProcessButton(id, PANEL_MOUSE_LBDOWN);
	if (ImGui::IsItemDeactivated()) mfd->ProcessButton(id, PANEL_MOUSE_LBUP);
	ImGui::PopID();
}

void DlgExtMFD::OnDraw ()
{
	if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows)) {
		for (int k = ImGuiKey_NamedKey_BEGIN; k < ImGuiKey_NamedKey_END; k++) {
			if (ImGui::IsKeyPressed((ImGuiKey)k, false)) {
				DWORD key = MapImGuiToOAPIKey((ImGuiKey)k);
				if (key) mfd->SendKey(key);
			}
		}
	}

	ImVec2 origin = ImGui::GetCursorStartPos();
	float cw = ImGui::GetWindowWidth() - Decoration().x;
	Layout l = ComputeLayout(cw);
	int ds = std::max(1, (int)l.ds);
	if (ds != mfd->GetDisplaySize()) mfd->SetDisplaySize(ds);

	ImVec2 dsp = origin + ImVec2(l.BW + 2.0f * l.gap, l.gap);
	ImVec2 scr = ImGui::GetWindowPos() + dsp;
	ImGui::GetWindowDrawList()->AddRectFilled(scr, scr + ImVec2((float)ds, (float)ds), IM_COL32_BLACK);
	SURFHANDLE surf = mfd->GetDisplaySurface();
	if (surf) {
		ImGui::SetCursorPos(dsp);
		ImGui::Image(surf, ImVec2((float)ds, (float)ds));
	}

	ImVec2 bsz(l.BW, l.BH);
	float x1 = origin.x + l.gap;
	float x2 = origin.x + cw - l.gap - l.BW;
	float y0 = (float)(ds / 7);
	float dy = (float)((ds * 2) / 13);
	float y1 = origin.y + y0 - l.BH / 2.0f;
	for (int i = 0; i < 6; i++) {
		MFDButton(i, mfd->GetButtonLabel(i), ImVec2(x1, y1 + i * dy), bsz);
		MFDButton(i + 6, mfd->GetButtonLabel(i + 6), ImVec2(x2, y1 + i * dy), bsz);
	}

	float cx = dsp.x + ds / 2.0f;
	float yb = origin.y + ds + 2.0f * l.gap;
	MFDButton(12, "PWR", ImVec2(cx - (l.BW * 7.0f) / 4.0f, yb), bsz);
	MFDButton(13, "SEL", ImVec2(cx - l.BW / 2.0f, yb), bsz);
	MFDButton(14, "MNU", ImVec2(cx + (l.BW * 3.0f) / 4.0f, yb), bsz);
	ImGui::BeginDisabled();
	MFDButton(15, "N/A", ImVec2(cx + l.BW * 2.0f, yb), bsz);
	ImGui::EndDisabled();
}

// ==============================================================
// class MFDWindow

MFDWindow::MFDWindow (const MFDSPEC &spec): ExternMFD (spec)
{
	vstick = false;
	windowId = GetFreeWindowId();
	char cbuf[64];
	snprintf(cbuf, sizeof(cbuf), "DX9ExtMFD_%d", windowId);
	window = std::make_unique<DlgExtMFD>(cbuf, this);
	oapiOpenDialog(window.get());
}

MFDWindow::~MFDWindow ()
{
	FreeWindowId(windowId);
}

void MFDWindow::SetVessel (OBJHANDLE hV)
{
	ExternMFD::SetVessel (hV);
}

void MFDWindow::SetDisplaySize (int size)
{
	MFDSPEC spec = {{0, 0, size, size}, 6, 6, size / 7, (size * 2) / 13};
	ExternMFD::Resize (spec);
}

void MFDWindow::ProcessButton (int bt, int event)
{
	switch (bt) {
	case 12:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_ESCAPE);
		break;
	case 13:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_F1);
		break;
	case 14:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey (OAPI_KEY_GRAVE);
		break;
	case 15:
		break;
	default:
		ExternMFD::ProcessButton (bt, event);
		break;
	}
}

void MFDWindow::clbkFocusChanged (OBJHANDLE hFocus)
{
	if (!vstick)
		ExternMFD::clbkFocusChanged (hFocus);
}

void MFDWindow::StickToVessel (bool stick)
{
	vstick = stick;
	if (!vstick)
		SetVessel (oapiGetFocusObject());
}
