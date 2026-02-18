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

#define TRANSLATION_CONTEXT "Dialog MenuCfg"
#include "I18NAPI.h"

extern Orbiter *g_pOrbiter;
extern Pane *g_pane;

DlgMenuCfg::DlgMenuCfg(): ImGuiDialog(ICON_FA_SLIDERS, _("Orbiter: Configure menu bars"))
{
	SetHelp("html/orbiter.chm", "/menucfg.htm");
}

void DlgMenuCfg::OnDraw()
{
	const char *modes[] = {_("Show"), _("Hide"), _("Auto-hide")};

	CFG_UIPRM &prm = g_pOrbiter->Cfg()->CfgUIPrm;
	MenuInfoBar *mib = g_pane->MIBar();

	if(ImGui::BeginTabBar("##MenuCfgTab")) {
		if(ImGui::BeginTabItem(_("Appearance"))) {
			ImGui::BeginChild("##Child");
			ImGui::SeparatorText(_("Menu bar"));
			ImGui::PushID(1);
			ImGui::RadioButton(_("Show"), &prm.MenuMode, 0);
			ImGui::SameLine();
			ImGui::RadioButton(_("Hide"), &prm.MenuMode, 1);
			ImGui::SameLine();
			ImGui::RadioButton(_("Auto-hide"), &prm.MenuMode, 2);

			ImGui::Checkbox(_("Always show labels"), &prm.bMenuLabelAlways);
			ImGui::SliderInt(_("Opacity"), &prm.MenuOpacity, 0, 10);
			ImGui::SliderInt(_("Animation speed"), &prm.MenuScrollspeed, 1, 20);
			ImGui::SliderInt(_("Button size"), &prm.MenuButtonSize, 10, 64);
			ImGui::SliderInt(_("Hovered size"), &prm.MenuButtonHoverSize, 10, 64);
			ImGui::SliderInt(_("Spacing"), &prm.MenuButtonSpacing, 1, 32);
			ImGui::PopID();

			ImGui::SeparatorText(_("Info bars"));
			ImGui::PushID(2);
			ImGui::RadioButton(_("Show"), &prm.InfoMode, 0);
			ImGui::SameLine();
			ImGui::RadioButton(_("Hide"), &prm.InfoMode, 1);
			ImGui::SameLine();
			ImGui::RadioButton(_("Auto-hide"), &prm.InfoMode, 2);
			ImGui::Checkbox(_("Always show warp factor"), &prm.bWarpAlways);
			ImGui::SliderInt(_("Opacity"), &prm.InfoOpacity, 0, 10);
			ImGui::PopID();

			ImGui::SeparatorText(_("FPS info bar"));
			const char *fpsmode[]={_("None"), _("On the left"), _("On the right")};
			ImGui::Combo("##FPSMode", &prm.FPS, fpsmode, IM_ARRAYSIZE(fpsmode));

			ImGui::EndChild();
			ImGui::EndTabItem();
		}
		if(ImGui::BeginTabItem(_("Menu items"))) {
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
