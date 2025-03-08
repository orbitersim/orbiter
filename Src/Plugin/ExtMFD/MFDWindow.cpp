// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                  ORBITER MODULE: ExtMFD
//                  Part of the ORBITER SDK
//            Copyright (C) 2006 Martin Schweiger
//                   All rights reserved
//
// MFDWindow.cpp
//
// Class implementation for MFDWindow. Defines the properties and
// state of an MFD display in a dialog box
// ==============================================================
#include "MFDWindow.h"
#include <stdio.h>
#include <cstring>
#include "imgui.h"
#include "imgui_extras.h"
#include "IconsFontAwesome6.h"

// ==============================================================
// class MFDWindow

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

void DlgExtMFD::Display() {
    ImGui::SetNextWindowSize(ImVec2(defaultSize.width, defaultSize.height), ImGuiCond_FirstUseEver);
	ImGui::SetNextWindowSizeConstraints(ImVec2(382,366), ImVec2(FLT_MAX, FLT_MAX));
	
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
	
	if(visible) {
		OnDraw();
	}
	ImGui::End();
	if (!active) OnClose();
}



void DlgExtMFD::OnDraw() {

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

	ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.9, 0, 0, 1));
	if (ImGui::Button("PWR", button_sz)) {
		m_mfd->ProcessButton(12, PANEL_MOUSE_LBDOWN);

	}
	ImGui::PopStyleColor();
	ImGui::SameLine();
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
	oapiCloseDialog(m_window.get());
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
