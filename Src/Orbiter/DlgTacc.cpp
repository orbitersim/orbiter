// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Time acceleration dialog
// ======================================================================

#include "DlgTacc.h"
#include "Orbiter.h"
#include "imgui.h"
#include "IconsFontAwesome6.h"

extern TimeData td;
extern Orbiter *g_pOrbiter;

DlgTacc::DlgTacc() : ImGuiDialog(ICON_FA_CLOCK " Orbiter: Time acceleration",{357,135}) {
	SetHelp("html/orbiter.chm", "/timeacc.htm");
}

void DlgTacc::OnDraw() {
    const ImVec2 button_sz(ImVec2(50, 20));

    if(ImGui::Button("0.1x", button_sz)) {
        g_pOrbiter->SetWarpFactor (0.1);
    }
    ImGui::SameLine();
    if(ImGui::Button("1x", button_sz)) {
        g_pOrbiter->SetWarpFactor (1.0);
    }
    ImGui::SameLine();
    if(ImGui::Button("10x", button_sz)) {
        g_pOrbiter->SetWarpFactor (10.0);
    }
    ImGui::SameLine();
    if(ImGui::Button("100x", button_sz)) {
        g_pOrbiter->SetWarpFactor (100.0);
    }
    ImGui::SameLine();
    if(ImGui::Button("1000x", button_sz)) {
        g_pOrbiter->SetWarpFactor (1000.0);
    }
    ImGui::SameLine();
    if(ImGui::Button("10000x", button_sz)) {
        g_pOrbiter->SetWarpFactor (10000.0);
    }

    float warp = td.Warp();
    ImGui::SetNextItemWidth(-FLT_MIN);
    if(ImGui::SliderFloat("##slider warp", &warp, 0.1f, 10000.0f, "%.1f", ImGuiSliderFlags_Logarithmic)) {
        g_pOrbiter->SetWarpFactor (warp);
    }

    ImGui::NewLine(); 

    ImGui::SetCursorPosX((ImGui::GetWindowSize().x - button_sz.x) * 0.5f);
    if(ImGui::Button(g_pOrbiter->IsRunning()?"Pause":"Resume", button_sz))
        g_pOrbiter->TogglePause();

}
