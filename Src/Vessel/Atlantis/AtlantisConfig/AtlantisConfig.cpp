// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE
#include "orbitersdk.h"
#include "imgui.h"
#include <stdio.h>
#include <unistd.h>

static const char *tex_hires_enabled = "Textures2/Atlantis";
static const char *tex_hires_disabled = "Textures2/~Atlantis";

static const char *vcmsh_fname = "Meshes/Atlantis/AtlantisVC.msh";
static const char *msh_hires_bkup = "Meshes/Atlantis/~AtlantisVC_hi.msh";
static const char *msh_lores_bkup = "Meshes/Atlantis/~AtlantisVC_lo.msh";

static bool Exists (const char *path)
{
	return access (path, F_OK) == 0;
}

class AtlantisConfigDialog: public ImGuiDialog {
public:
	AtlantisConfigDialog (): ImGuiDialog ("Atlantis configuration") {}
	void Open ();
	void Display () override;

protected:
	void OnDraw () override {}

private:
	int texhires = 1;
	int mshhires = 1;
};

class AtlantisConfig: public LaunchpadItem {
public:
	AtlantisConfig(): LaunchpadItem() {}
	char *Name() { return (char*)"Atlantis Configuration"; }
	char *Description();
	bool clbkOpen ();
	static bool TexHiresEnabled();
	static void TexEnableHires (bool enable);
	static bool MshHiresEnabled();
	static bool MshEnableHires (bool enable);

private:
	AtlantisConfigDialog dlg;
};

static AtlantisConfig *g_item = 0;

char *AtlantisConfig::Description()
{
	return (char*)"Global configuration for the default Space Shuttle Atlantis.";
}

bool AtlantisConfig::clbkOpen ()
{
	dlg.Open ();
	return true;
}

bool AtlantisConfig::TexHiresEnabled ()
{
	// check if the Atlantis highres texture directory is present
	return Exists (tex_hires_enabled);
}

void AtlantisConfig::TexEnableHires (bool enable)
{
	if (TexHiresEnabled() == enable) return; // nothing to do

	if (enable) {
		rename (tex_hires_disabled, tex_hires_enabled);
	} else {
		// to disable the highres textures, we simply rename the directory
		// so that orbiter's texture manager can't find it
		rename (tex_hires_enabled, tex_hires_disabled);
	}
}

bool AtlantisConfig::MshHiresEnabled ()
{
	// check if backup of low-res mesh is present
	return Exists (msh_lores_bkup);
}

bool AtlantisConfig::MshEnableHires (bool enable)
{
	if (MshHiresEnabled() == enable) return false; // nothing to do

	if (enable) {
		if (!Exists (msh_hires_bkup)) return false; // high-res backup not found
		if (Exists (vcmsh_fname) && !Exists (msh_lores_bkup))
			rename (vcmsh_fname, msh_lores_bkup); // back up low-res mesh
		rename (msh_hires_bkup, vcmsh_fname); // activate high-res mesh
	} else {
		if (!Exists (msh_lores_bkup)) return false; // low-res backup not found
		if (Exists (vcmsh_fname) && !Exists (msh_hires_bkup))
			rename (vcmsh_fname, msh_hires_bkup); // back up high-res mesh
		rename (msh_lores_bkup, vcmsh_fname); // activate low-res mesh
	}
	return true;
}

void AtlantisConfigDialog::Open ()
{
	if (!IsActive()) {
		texhires = AtlantisConfig::TexHiresEnabled() ? 1 : 0;
		mshhires = AtlantisConfig::MshHiresEnabled() ? 1 : 0;
	}
	oapiOpenDialog (this);
}

void AtlantisConfigDialog::Display ()
{
	const float em = ImGui::GetFontSize();
	ImGui::SetNextWindowPos (ImGui::GetMainViewport()->GetCenter(), ImGuiCond_Appearing, ImVec2 (0.5f, 0.5f));
	ImGui::SetNextWindowSizeConstraints (ImVec2 (22.0f*em, 0.0f), ImVec2 (22.0f*em, FLT_MAX));
	if (ImGui::Begin (name.c_str(), &active, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoSavedSettings)) {
		ImGui::SeparatorText ("Atlantis orbiter: texture resolution");
		ImGui::RadioButton ("High##tex", &texhires, 1);
		ImGui::SameLine ();
		ImGui::RadioButton ("Low##tex", &texhires, 0);
		ImGui::SeparatorText ("Atlantis orbiter: mesh resolution");
		ImGui::RadioButton ("High##msh", &mshhires, 1);
		ImGui::SameLine ();
		ImGui::RadioButton ("Low##msh", &mshhires, 0);
		ImGui::Spacing ();
		ImGui::TextWrapped ("High resolution textures and meshes provide a better visual appearance, but can reduce the performance of the simulator, in particular on less powerful computers.");
		ImGui::TextWrapped ("If you experience problems, try switching to the low resolution options.");
		ImGui::Spacing ();
		ImGui::Separator ();
		ImGui::Spacing ();
		if (ImGui::Button ("OK", ImVec2 (5.0f*em, 0))) {
			AtlantisConfig::TexEnableHires (texhires != 0);
			AtlantisConfig::MshEnableHires (mshhires != 0);
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
	g_item = new AtlantisConfig;
	// create the new config item
	LAUNCHPADITEM_HANDLE root = oapiFindLaunchpadItem ("Vessel configuration");
	// find the config root entry provided by orbiter
	oapiRegisterLaunchpadItem (g_item, root);
	// register the Atlantis config entry
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
