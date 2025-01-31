// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Custom function selection dialog
// ======================================================================

#include "DlgFunction.h"
#include "Orbiter.h"
#include "imgui.h"

extern Orbiter *g_pOrbiter;

DlgFunction::DlgFunction() : ImGuiDialog("Orbiter: Custom functions", {340, 290}) {
}

void DlgFunction::OnDraw() {
	ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
    for (int i = 0; i < g_pOrbiter->ncustomcmd; i++) {
	    if(ImGui::Button(g_pOrbiter->customcmd[i].label, button_sz)) {
            g_pOrbiter->customcmd[i].func (g_pOrbiter->customcmd[i].context);
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::BeginTooltip();
            ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
            ImGui::TextUnformatted(g_pOrbiter->customcmd[i].desc);
            ImGui::PopTextWrapPos();
            ImGui::EndTooltip();
        }
    }
}
