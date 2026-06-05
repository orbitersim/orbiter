// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                  ORBITER MODULE: ExtMFD
//                  Part of the ORBITER SDK
//            Copyright (C) 2006-2026 Martin Schweiger
//                   All rights reserved
//
// MFDWindow.cpp
//
// Class implementation for MFDWindow. Defines the properties and
// state of an MFD display in a dialog box
// ==============================================================
#define IMGUI_DEFINE_MATH_OPERATORS
#include "MFDWindow.h"
#include <stdio.h>
#include <cstring>
#include "imgui.h"
#include "imgui_extras.h"
#include "IconsFontAwesome6.h"

// ==============================================================
// class MFDWindow

DWORD MapImGuiToOAPIKey(ImGuiKey key) {
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

const int button_yoffset = 30;

class DlgExtMFD : public ImGuiDialog {
public:
	DlgExtMFD(const std::string& name, MFDWindow* mfd);
	void OnDraw() override;
	void Display() override;
	void OnClose() override	{
		oapiUnregisterExternMFD(m_mfd);
	}
	MFDWindow* m_mfd;
	ImVec2 m_oldSize;
};

DlgExtMFD::DlgExtMFD(const std::string& name, MFDWindow* mfd) : ImGuiDialog(name.c_str(), {382,366}) {
	m_mfd = mfd;
	m_oldSize = ImVec2(0, 0);
}

static void AspectRatio(ImGuiSizeCallbackData* data)
{
	MFDWindow *mfd = (MFDWindow *)data->UserData;
	if(mfd->GetAspectRatioState()) {
		//cf https://github.com/ocornut/imgui/pull/8028
		int current_cursor = ImGui::GetMouseCursor();
		if(current_cursor == ImGuiMouseCursor_ResizeNWSE || current_cursor == ImGuiMouseCursor_ResizeNESW)
		{
			if(mfd->aspect_ratio > data->DesiredSize.x / data->DesiredSize.y)
				data->DesiredSize.x = mfd->aspect_ratio * data->DesiredSize.y;
			else
				data->DesiredSize.y = data->DesiredSize.x / mfd->aspect_ratio;
		}
		else if(current_cursor == ImGuiMouseCursor_ResizeNS)
			data->DesiredSize.x = mfd->aspect_ratio * data->DesiredSize.y;
		else if(current_cursor == ImGuiMouseCursor_ResizeEW)
			data->DesiredSize.y = data->DesiredSize.x / mfd->aspect_ratio;
	} else {
		if(data->DesiredSize.y)
			mfd->aspect_ratio = data->DesiredSize.x / data->DesiredSize.y;
	}
}

void DlgExtMFD::Display() {
	// Position the window just below the cursor
	ImVec2 mouse = ImGui::GetMousePos();
	ImVec2 offset(-10.0f, 10.0f);
	ImVec2 pos = mouse + offset;
	ImGui::SetNextWindowPos(pos, ImGuiCond_FirstUseEver);

	ImGui::SetNextWindowSize(ImVec2(defaultSize.width, defaultSize.height), ImGuiCond_Once);
	ImGui::SetNextWindowSizeConstraints(ImVec2(382,366), ImVec2(FLT_MAX, FLT_MAX), AspectRatio, m_mfd);
	
	char cbuf[256] = ICON_FA_TABLET_SCREEN_BUTTON " MFD [";
	oapiGetObjectName(m_mfd->GetVessel(), cbuf + 9, 246);
	strcat(cbuf, "]###");
	strcat(cbuf, name.c_str());

	bool visible = ImGui::Begin(cbuf, &active);
	bool stick = m_mfd->GetStickToVessel();

	if(ImGui::MenuButton(ICON_FA_CIRCLE_QUESTION, "Help")) {
		m_mfd->OpenModeHelp();
	}

	if(ImGui::MenuButton(stick ? ICON_FA_THUMBTACK : ICON_FA_THUMBTACK_SLASH, stick ? "Unpin this MFD from the vessel":"Pin this MFD to the current vessel", ImGui::GetFontSize()*1.7f))
	{
		m_mfd->ToggleStickToVessel();
	}
	
	bool locked = m_mfd->GetAspectRatioState();
	if(ImGui::MenuButton(locked ? ICON_FA_LOCK : ICON_FA_LOCK_OPEN, locked ? "Unlock aspect ratio" : "Lock aspect ratio", ImGui::GetFontSize()*3.4f)) {
		m_mfd->ToggleLockAspectRatio();
	}

	if(visible) {
		OnDraw();
	}
	ImGui::End();
	if (!active) OnClose();
}



void DlgExtMFD::OnDraw() {

	if (ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows)) {
		for (int k = ImGuiKey_NamedKey_BEGIN; k < ImGuiKey_NamedKey_END; k++) {
			ImGuiKey key = (ImGuiKey)k;
			if (ImGui::IsKeyPressed(key, false)) { // false for no repeat
				DWORD oapi_key = MapImGuiToOAPIKey(key);
				if (oapi_key) m_mfd->SendKey(oapi_key);
			}
		}
	}

	const int button_row_width = 50;
	const int button_bottom_height = 50;
	const ImVec2 button_sz = ImVec2(40, 20);


	ImGuiWindowFlags window_flags = ImGuiWindowFlags_HorizontalScrollbar;
	ImGui::BeginChild("ChildL", ImVec2(button_row_width, ImGui::GetContentRegionAvail().y - button_bottom_height), false, window_flags);

	ImVec2 sz = ImGui::GetContentRegionAvail();

	for (int i = 0; i < 6; i++) {
		const char* label = m_mfd->GetButtonLabel(i);
		if (label) {
			ImGui::PushID(i);
			ImGui::SetCursorPosY((i * sz.y - button_yoffset) / 6 + button_yoffset);
			ImGui::Button(label, button_sz);
			if (ImGui::IsItemHovered()) {
				if (ImGui::IsMouseClicked(ImGuiMouseButton_Left)) {
					m_mfd->ProcessButton(i, PANEL_MOUSE_LBDOWN);
				}
				else if (ImGui::IsMouseReleased(ImGuiMouseButton_Left)) {
					m_mfd->ProcessButton(i, PANEL_MOUSE_LBUP);
				}
			}
			ImGui::PopID();
		}
	}

	ImGui::EndChild();
	ImGui::SameLine();
	ImGui::BeginChild("ChildM", ImVec2(ImGui::GetContentRegionAvail().x - button_row_width, ImGui::GetContentRegionAvail().y - button_bottom_height), false, window_flags);
	sz = ImGui::GetContentRegionAvail();

	if (sz.x != m_oldSize.x || sz.y != m_oldSize.y) {
		if (sz.x > 80 && sz.y > 80) {
			MFDSPEC spec = { {0,0,(int)sz.x,(int)sz.y},6,6,button_yoffset,(int)(sz.y / 6.0) };
			m_mfd->Resize(spec);
			m_oldSize = sz;
		}
	}
	SURFHANDLE surf = m_mfd->GetDisplaySurface();
	if (surf) {
		ImVec2 uv_min = ImVec2(0.0f, 0.0f);                 // Top-left
		ImVec2 uv_max = ImVec2(1.0f, 1.0f);                 // Lower-right
		ImVec4 tint_col = ImVec4(1.0f, 1.0f, 1.0f, 1.0f);   // No tint
		ImVec4 border_col = ImVec4(1.0f, 1.0f, 1.0f, 0.0f);
		ImGui::Image(surf, ImVec2(sz.x, sz.y), uv_min, uv_max, tint_col, border_col);
	}
	ImGui::EndChild();
	ImGui::SameLine();

	ImGui::BeginChild("ChildR", ImVec2(ImGui::GetContentRegionAvail().x, ImGui::GetContentRegionAvail().y - button_bottom_height), false, window_flags);

	for (int i = 6; i < 12; i++) {
		const char* label = m_mfd->GetButtonLabel(i);
		if (label) {
			ImGui::PushID(i);
			ImGui::SetCursorPosY(((i - 6) * sz.y - button_yoffset) / 6 + button_yoffset);
			ImGui::Button(label, button_sz);
			if (ImGui::IsItemHovered()) {
				if (ImGui::IsMouseClicked(ImGuiMouseButton_Left)) {
					m_mfd->ProcessButton(i, PANEL_MOUSE_LBDOWN);
				}
				else if (ImGui::IsMouseReleased(ImGuiMouseButton_Left)) {
					m_mfd->ProcessButton(i, PANEL_MOUSE_LBUP);
				}
			}
			ImGui::PopID();
		}
	}

	ImGui::EndChild();
	ImGui::BeginChild("ChildB", ImVec2(ImGui::GetContentRegionAvail().x, ImGui::GetContentRegionAvail().y), false, window_flags);

	//sz = ImGui::GetContentRegionAvail();

	//sz.x - button_sz.x * 4
	ImGui::SetCursorPosX(60);


	if (ImGui::Button("SEL", button_sz)) {
		m_mfd->ProcessButton(13, PANEL_MOUSE_LBDOWN);
	}
	ImGui::SameLine();
	if (ImGui::Button("MNU", button_sz)) {
		m_mfd->ProcessButton(14, PANEL_MOUSE_LBDOWN);
	}
	ImGui::EndChild();
}


MFDWindow::MFDWindow(const MFDSPEC& spec) : ExternMFD(spec)
{
	fnth = 0;
	vstick = false;
	char cbuf[128];
	sprintf(cbuf, "ExtMFD%lld", (uint64_t)Id());
	m_window = std::make_unique<DlgExtMFD>(cbuf, this);
	oapiOpenDialog(m_window.get());
}

MFDWindow::~MFDWindow()
{
}

void MFDWindow::SetVessel(OBJHANDLE hV)
{
	ExternMFD::SetVessel(hV);
}

void MFDWindow::ProcessButton(int bt, int event)
{
	switch (bt) {
	case 12:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey(OAPI_KEY_ESCAPE);
		break;
	case 13:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey(OAPI_KEY_F1);
		break;
	case 14:
		if (event == PANEL_MOUSE_LBDOWN)
			SendKey(OAPI_KEY_GRAVE);
		break;
	default:
		ExternMFD::ProcessButton(bt, event);
		break;
	}
}

void MFDWindow::clbkFocusChanged(OBJHANDLE hFocus)
{
	if (!vstick) {
		ExternMFD::clbkFocusChanged(hFocus);
	}
}

void MFDWindow::ToggleStickToVessel()
{
	vstick = !vstick;
	if (!vstick) {
		SetVessel(oapiGetFocusObject());
	}
}

