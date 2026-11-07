// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// AboutTab class
//=============================================================================

#include <windows.h>
#include "Orbiter.h"
#include "Launchpad.h"
#include "TabAbout.h"
#include "Help.h"
#include "OrbiterPlatform.h"
#include "about.hpp"
#include "imgui.h"

static const char *LicenseText =
	"Orbiter Space Flight Simulator\n\n"
	"MIT License\n\n"
	"Copyright 2000-2026 Martin Schweiger\n\n"
	"Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated "
	"documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation "
	"the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to "
	"permit persons to whom the Software is furnished to do so, subject to the following conditions:\n\n"
	"The above copyright notice and this permission notice shall be included in all copies or substantial portions of "
	"the Software.\n\n"
	"THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO "
	"THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE "
	"AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, "
	"TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE "
	"SOFTWARE.";

//-----------------------------------------------------------------------------
// AboutTab class

orbiter::AboutTab::AboutTab (LaunchpadDialog *lp): LaunchpadTab (lp, "About", "tab_about")
{
	m_openLicense = false;
}

//-----------------------------------------------------------------------------

void orbiter::AboutTab::Draw ()
{
	const float em = ImGui::GetFontSize ();

	ImGui::PushFont (pLp->HeadingFont (), pLp->HeadingFont () ? pLp->HeadingFont ()->LegacySize : 0.0f);
	ImGui::TextUnformatted (NAME1);
	ImGui::PopFont ();
	ImGui::TextUnformatted (SIG4);
	ImGui::TextUnformatted (SIG1B);

	ImGui::SeparatorText ("Installed components");
	ImGui::BulletText ("D3D9Client module by Jarmo Nikkanen and Peter Schneider");
	ImGui::BulletText ("XRSound module Copyright (c) Doug Beachy");

	ImGui::SeparatorText ("On the web");
	struct { const char *label, *addr; } web[] = {
		{ "Orbiter home", SIG2 },
		{ "Orbiter web forum", SIG5 },
		{ "Youtube channel", SIG6 },
	};
	if (ImGui::BeginTable ("##web", 2, ImGuiTableFlags_SizingFixedFit)) {
		for (auto &w : web) {
			ImGui::TableNextRow ();
			ImGui::TableNextColumn ();
			ImGui::TextUnformatted (w.label);
			ImGui::TableNextColumn ();
			ImGui::PushID (w.addr);
			if (ImGui::TextLink (w.addr))
				oplat::OpenURL (std::string ("https://") + w.addr);
			ImGui::PopID ();
		}
		ImGui::EndTable ();
	}

	ImGui::Spacing ();
	ImGui::TextWrapped ("Orbiter is published under the MIT License.");
	ImGui::Spacing ();

	if (ImGui::Button ("Orbiter homepage"))
		oplat::OpenURL (std::string ("https://") + SIG2);
	ImGui::SameLine ();
	if (ImGui::Button ("License"))
		m_openLicense = true;
	ImGui::SameLine ();
	if (ImGui::Button ("Credits and contributions"))
		::OpenHelp ("Html/Credit.chm", "Credit");

	if (m_openLicense) {
		m_openLicense = false;
		ImGui::OpenPopup ("Orbiter license");
	}
	ImGuiViewport *vp = ImGui::GetMainViewport ();
	ImGui::SetNextWindowPos (vp->GetCenter (), ImGuiCond_Appearing, ImVec2 (0.5f, 0.5f));
	ImGui::SetNextWindowSize (ImVec2 (em * 32.0f, em * 26.0f), ImGuiCond_Appearing);
	if (ImGui::BeginPopupModal ("Orbiter license", nullptr, ImGuiWindowFlags_NoSavedSettings)) {
		float bh = ImGui::GetFrameHeightWithSpacing ();
		ImGui::BeginChild ("##lic", ImVec2 (0, -bh), ImGuiChildFlags_Borders);
		ImGui::TextWrapped ("%s", LicenseText);
		ImGui::EndChild ();
		if (ImGui::Button ("OK", ImVec2 (em * 5.0f, 0)) || ImGui::IsKeyPressed (ImGuiKey_Escape))
			ImGui::CloseCurrentPopup ();
		ImGui::EndPopup ();
	}
}
