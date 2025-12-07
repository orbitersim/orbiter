// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Menu bar configuration dialog
// ======================================================================

#include "DlgMenuCfg.h"
#include "MenuInfoBar.h"
#include "Pane.h"
#include "imgui.h"
#include "imgui_extras.h"
#include "Orbiter.h"
#include "IconsFontAwesome6.h"

extern Orbiter *g_pOrbiter;
extern Pane *g_pane;

DlgMenuCfg::DlgMenuCfg(): ImGuiDialog(ICON_FA_SLIDERS " Orbiter: Configure menu bars")
{
	SetHelp("html/orbiter.chm", "/menucfg.htm");
}

void DlgMenuCfg::OnDraw()
{
	const char *modes[] = {"Show", "Hide", "Auto-hide"};

	CFG_UIPRM &prm = g_pOrbiter->Cfg()->CfgUIPrm;
	MenuInfoBar *mib = g_pane->MIBar();

	if(ImGui::BeginTabBar("##MenuCfgTab")) {
		if(ImGui::BeginTabItem("Appearance")) {
			ImGui::SeparatorText("Menu bar");
			ImGui::PushID(1);
			ImGui::RadioButton("Show", &prm.MenuMode, 0);
			ImGui::SameLine();
			ImGui::RadioButton("Hide", &prm.MenuMode, 1);
			ImGui::SameLine();
			ImGui::RadioButton("Auto-hide", &prm.MenuMode, 2);

			ImGui::Checkbox("Always show labels", &prm.bMenuLabelAlways);
			ImGui::SliderInt("Opacity", &prm.MenuOpacity, 0, 10);
			ImGui::SliderInt("Animation speed", &prm.MenuScrollspeed, 1, 20);
			ImGui::SliderInt("Button size", &prm.MenuButtonSize, 10, 64);
			ImGui::SliderInt("Hovered size", &prm.MenuButtonHoverSize, 10, 64);
			ImGui::SliderInt("Spacing", &prm.MenuButtonSpacing, 1, 32);
			ImGui::PopID();

			ImGui::SeparatorText("Info bars");
			ImGui::PushID(2);
			ImGui::RadioButton("Show", &prm.InfoMode, 0);
			ImGui::SameLine();
			ImGui::RadioButton("Hide", &prm.InfoMode, 1);
			ImGui::SameLine();
			ImGui::RadioButton("Auto-hide", &prm.InfoMode, 2);
			ImGui::Checkbox("Always show warp factor", &prm.bWarpAlways);
			ImGui::SliderInt("Opacity", &prm.InfoOpacity, 0, 10);
			ImGui::PopID();

			ImGui::SeparatorText("FPS info bar");
			const char *fpsmode[]={"None", "On the left", "On the right"};
			ImGui::Combo("##FPSMode", &prm.FPS, fpsmode, IM_ARRAYSIZE(fpsmode));

			ImGui::EndTabItem();
		}
		if(ImGui::BeginTabItem("Menu items")) {
			ImGui::BeginChild("##Child");
			bool changed = false;
			for(auto &pref: g_pane->MIBar()->GetPreferences()) {
				changed |= ImGui::Checkbox(pref.label.c_str(), &pref.enabled);
			}
			if(changed)
				g_pane->MIBar()->SyncPreferences();
			ImGui::EndChild();
			ImGui::EndTabItem();  
		}
		ImGui::EndTabBar();
	}
}
