// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE
#include "orbitersdk.h"
#include "imgui.h"
#include <stdio.h>
#include <unistd.h>

static const char *hires_enabled = "Textures2/DG";
static const char *hires_disabled = "Textures2/~DG";

class DGConfigDialog: public ImGuiDialog {
public:
	DGConfigDialog (): ImGuiDialog ("DG configuration") {}
	void Open ();
	void Display () override;

protected:
	void OnDraw () override {}

private:
	int hires = 1;
};

class DGConfig: public LaunchpadItem {
public:
	DGConfig(): LaunchpadItem() {}
	char *Name() { return (char*)"DG Configuration"; }
	char *Description();
	bool clbkOpen ();
	static bool HiresEnabled();
	static void EnableHires (bool enable);

private:
	DGConfigDialog dlg;
};

static DGConfig *g_item = 0;

char *DGConfig::Description()
{
	return (char*)"Global configuration for the default Delta-glider.";
}

bool DGConfig::clbkOpen ()
{
	dlg.Open ();
	return true;
}

bool DGConfig::HiresEnabled ()
{
	// check if the DG highres texture directory is present
	return access (hires_enabled, F_OK) == 0;
}

void DGConfig::EnableHires (bool enable)
{
	if (HiresEnabled() == enable) return; // nothing to do

	if (enable) {
		rename (hires_disabled, hires_enabled);
	} else {
		// to disable the highres textures, we simply rename the directory
		// so that orbiter's texture manager can't find it
		rename (hires_enabled, hires_disabled);
	}
}

void DGConfigDialog::Open ()
{
	if (!IsActive())
		hires = DGConfig::HiresEnabled() ? 1 : 0;
	oapiOpenDialog (this);
}

void DGConfigDialog::Display ()
{
	const float em = ImGui::GetFontSize();
	ImGui::SetNextWindowPos (ImGui::GetMainViewport()->GetCenter(), ImGuiCond_Appearing, ImVec2 (0.5f, 0.5f));
	ImGui::SetNextWindowSizeConstraints (ImVec2 (18.0f*em, 0.0f), ImVec2 (18.0f*em, FLT_MAX));
	if (ImGui::Begin (name.c_str(), &active, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoSavedSettings)) {
		ImGui::SeparatorText ("Texture resolution");
		ImGui::RadioButton ("High (1024x1024)", &hires, 1);
		ImGui::RadioButton ("Low (512x512)", &hires, 0);
		ImGui::Spacing ();
		ImGui::Separator ();
		ImGui::Spacing ();
		if (ImGui::Button ("OK", ImVec2 (5.0f*em, 0))) {
			DGConfig::EnableHires (hires != 0);
			active = false;
		}
		ImGui::SameLine ();
		if (ImGui::Button ("Cancel", ImVec2 (5.0f*em, 0)))
			active = false;
	}
	ImGui::End ();
}

// ==============================================================
// The DLL entry point
// ==============================================================

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	g_item = new DGConfig;
	// create the new config item
	LAUNCHPADITEM_HANDLE root = oapiFindLaunchpadItem ("Vessel configuration");
	// find the config root entry provided by orbiter
	oapiRegisterLaunchpadItem (g_item, root);
	// register the DG config entry
}

// ==============================================================
// The DLL exit point
// ==============================================================

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister the launchpad items
	oapiUnregisterLaunchpadItem (g_item);
	delete g_item;
}
