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

	ImGui::BeginGroupPanel("Menu bar");
		ImGui::PushID(1);
		ImGui::RadioButton("Show", &prm.MenuMode, 0);
		ImGui::SameLine();
		ImGui::RadioButton("Hide", &prm.MenuMode, 1);
		ImGui::SameLine();
		ImGui::RadioButton("Auto-hide", &prm.MenuMode, 2);
		mib->SetMenuMode (prm.MenuMode);

		if(ImGui::Checkbox("Labels only", &prm.bMenuLabelOnly))
			mib->SetLabelOnly (prm.bMenuLabelOnly);

		if(ImGui::SliderInt("Opacity", &prm.MenuOpacity, 0, 10))
			mib->SetOpacity (prm.MenuOpacity);

		if(ImGui::SliderInt("Scroll speed", &prm.MenuScrollspeed, 1, 20))
			mib->SetScrollspeed (prm.MenuScrollspeed);
		ImGui::PopID();		
	ImGui::EndGroupPanel();


	ImGui::BeginGroupPanel("Info bars");
		ImGui::PushID(2);
		ImGui::RadioButton("Show", &prm.InfoMode, 0);
		ImGui::SameLine();
		ImGui::RadioButton("Hide", &prm.InfoMode, 1);
		ImGui::SameLine();
		ImGui::RadioButton("Auto-hide", &prm.InfoMode, 2);
		mib->SetInfoMode (prm.InfoMode);

		if(ImGui::Checkbox("Always show warp factor", &prm.bWarpAlways))
			mib->SetWarpAlways (prm.bWarpAlways);

		if(ImGui::Checkbox("Use scientific notation for warp", &prm.bWarpScientific))
			mib->SetWarpScientific (prm.bWarpScientific);

		if(ImGui::SliderInt("Opacity", &prm.InfoOpacity, 0, 10))
			mib->SetOpacityInfo (prm.InfoOpacity);
		ImGui::PopID();
	ImGui::EndGroupPanel();

	ImGui::BeginGroupPanel("Auxiliary info bars");
		const char *auxmode[]={"None", "Frame rate", "Render statistics", "Viewport info"};
		ImGui::Combo("Left", &prm.InfoAuxIdx[0], auxmode, IM_ARRAYSIZE(auxmode));
		mib->SetAuxInfobar (0, prm.InfoAuxIdx[0]);
		ImGui::Combo("Right", &prm.InfoAuxIdx[1], auxmode, IM_ARRAYSIZE(auxmode));
		mib->SetAuxInfobar (1, prm.InfoAuxIdx[1]);

	ImGui::EndGroupPanel();

	ImGui::BeginGroupPanel("Action indicator");
		const char *actionmode[]={"Flash on action", "Show on pause/record/playback", "Don't show"};
		ImGui::Combo("Mode", &prm.PauseIndMode, actionmode, IM_ARRAYSIZE(actionmode));
		mib->SetPauseIndicatorMode (prm.PauseIndMode);
	ImGui::EndGroupPanel();
}
