// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Custom function selection dialog
// ======================================================================

#include "DlgFunction.h"
#include "Orbiter.h"
#include "imgui.h"
#include "IconsFontAwesome6.h"

extern Orbiter *g_pOrbiter;

DlgFunction::DlgFunction() : ImGuiDialog(ICON_FA_PUZZLE_PIECE " Orbiter: Custom functions", {340, 290}) {
	SetHelp("html/orbiter.chm", "/customcmd.htm");
}

void DlgFunction::OnDraw() {
	ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
	for (auto it = g_pOrbiter->m_vCustomCmd.begin(); it != g_pOrbiter->m_vCustomCmd.end(); it++) {
		if (ImGui::Button(it->label.c_str(), button_sz)) {
			it->func(it->context);
		}
        if (ImGui::IsItemHovered()) {
            ImGui::BeginTooltip();
            ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
            ImGui::TextUnformatted(it->desc.c_str());
            ImGui::PopTextWrapPos();
            ImGui::EndTooltip();
        }
    }
}
