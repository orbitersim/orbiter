// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Time acceleration dialog
// ======================================================================
#define IMGUI_DEFINE_MATH_OPERATORS

#include "DlgTacc.h"
#include "Orbiter.h"
#include "imgui.h"
#include "imgui_internal.h"
#include "IconsFontAwesome6.h"
#include <algorithm>

extern TimeData td;
extern Orbiter *g_pOrbiter;

// Snap to 1..9,10 per decade (1,2,3,...,9,10,20,30,...)
static float SnapToDecimal(float v, float min_v, float max_v)
{
    v = std::clamp(v, min_v, max_v);

    float decade = floorf(log10f(v));
    float base   = powf(10.0f, decade);

    float norm = v / base;
    float snapped = roundf(norm);

    // Handle rounding up to next decade cleanly
    if (snapped >= 10.0f)
    {
        snapped = 10.0f;
    }

    float result = snapped * base;

    return std::clamp(result, min_v, max_v);
}

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
    ImGuiIO& io = ImGui::GetIO();

    // The time acceleration slider has a snapping behavior (disabled if SHIFT is pressed)
    float warp_min = 0.1f;
    float warp_max = 10000.0f;

    // If snapping, draw the grabber as a "ghost" (inactive color)
    // We'll draw the snapped version later with the active color
    if(!io.KeyShift)
        ImGui::PushStyleColor(ImGuiCol_SliderGrabActive, ImGui::GetColorU32(ImGuiCol_SliderGrab));

    // Use a fake format so that the SliderFloat will show the snapped value
    char format[64];
    snprintf(format, 64, "%.1f", warp);
    format[63] = '\0';
    ImGui::SliderFloat("##slider warp", &warp,
	                        warp_min, warp_max,
	                        format,
	                        ImGuiSliderFlags_Logarithmic);

    if(!io.KeyShift)
        ImGui::PopStyleColor();

    bool active = ImGui::IsItemActive();
    bool dragging = active && ImGui::IsMouseDragging(ImGuiMouseButton_Left);

    if(active && !io.KeyShift)
        warp = SnapToDecimal(warp, warp_min, warp_max);

    g_pOrbiter->SetWarpFactor(warp);

    // Draw the snapped grabber while dragging
    if (dragging && !io.KeyShift)
    {
        ImGuiStyle& style = ImGui::GetStyle();
        ImGuiWindow* window = ImGui::GetCurrentWindow();
        ImRect frame_bb(ImGui::GetItemRectMin(),
                        ImGui::GetItemRectMax());

        // Convert snapped value to parametric t (log scale)
        float t = (log10f(warp) - log10f(warp_min)) /
                  (log10f(warp_max) - log10f(warp_min));
        t = ImClamp(t, 0.0f, 1.0f);

        float grab_w = style.GrabMinSize;
        float x = ImLerp(frame_bb.Min.x + grab_w * 0.5f,
                          frame_bb.Max.x - grab_w * 0.5f,
                          t);

        ImRect snapped_grab(
            ImVec2(x - grab_w * 0.5f, frame_bb.Min.y + 2),
            ImVec2(x + grab_w * 0.5f, frame_bb.Max.y - 2));


        window->DrawList->AddRectFilled(
            snapped_grab.Min,
            snapped_grab.Max,
            ImGui::GetColorU32(ImGuiCol_SliderGrabActive),
            style.GrabRounding);

    }

    ImGui::NewLine(); 

    ImGui::SetCursorPosX((ImGui::GetWindowSize().x - button_sz.x) * 0.5f);
    if(ImGui::Button(g_pOrbiter->IsRunning()?"Pause":"Resume", button_sz))
        g_pOrbiter->TogglePause();

}
