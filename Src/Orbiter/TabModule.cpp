// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ModuleTab class
//=============================================================================

#include <windows.h>
#include "Orbiter.h"
#include "Launchpad.h"
#include "TabModule.h"
#include "OrbiterPlatform.h"
#include "imgui.h"
#include <algorithm>
#include <filesystem>
namespace fs = std::filesystem;

static const char *PluginDir = "Modules/Plugin";

//-----------------------------------------------------------------------------

orbiter::ModuleTab::ModuleTab (LaunchpadDialog *lp): LaunchpadTab (lp, "Modules", "tab_modules")
{
	m_sel = -1;
	m_expand = 1;
	m_listWidth = 300.0f;
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::GetConfig (const Config *cfg)
{
	RefreshLists ();
	int listw = cfg->CfgWindowPos.LaunchpadModListWidth;
	m_listWidth = listw > 0 ? (float)listw : 300.0f;
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::SetConfig (Config *cfg)
{
	cfg->CfgWindowPos.LaunchpadModListWidth = (int)m_listWidth;
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::RefreshLists ()
{
	m_modules.clear ();
	m_categories.clear ();
	m_sel = -1;

	const std::string suffix = OCompatModuleSuffix ();
	std::error_code ec;
	for (const auto &entry : fs::directory_iterator (PluginDir, ec)) {
		fs::path modpath;
		std::string name;
		if (entry.is_directory (ec)) {
			name = entry.path ().filename ().string ();
			modpath = entry.path () / (name + suffix);
			if (!fs::exists (modpath, ec)) continue;
		}
		else if (entry.path ().extension ().string () == suffix) {
			modpath = entry.path ();
			name = modpath.stem ().string ();
		}
		else continue;

		ModuleRec rec;
		rec.name = name;
		rec.active = pCfg->IsActiveModule (name);
		auto &lp = pCfg->CfgCmdlinePrm.LoadPlugins;
		rec.locked = std::find (lp.begin (), lp.end (), name) != lp.end ();
		if (rec.locked) rec.active = true;

		std::string cat, desc;
		if (!oplat::ReadModuleInfo (modpath.string (), cat, desc) || cat.empty ())
			cat = "Miscellaneous";
		if (cat == "Graphics engines")
			continue;
		desc.erase (std::remove (desc.begin (), desc.end (), '\r'), desc.end ());
		rec.info = desc;
		m_modules.push_back (rec);

		auto it = std::find_if (m_categories.begin (), m_categories.end (), [&](const Category &c) { return c.name == cat; });
		if (it == m_categories.end ()) {
			m_categories.push_back ({ cat, {} });
			it = m_categories.end () - 1;
		}
		it->modules.push_back (m_modules.size () - 1);
	}

	auto ciless = [](const std::string &a, const std::string &b) { return strcasecmp (a.c_str (), b.c_str ()) < 0; };
	std::sort (m_categories.begin (), m_categories.end (), [&](const Category &a, const Category &b) { return ciless (a.name, b.name); });
	for (auto &c : m_categories)
		std::sort (c.modules.begin (), c.modules.end (), [&](size_t a, size_t b) { return ciless (m_modules[a].name, m_modules[b].name); });
	m_expand = 1;
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::SetActive (size_t idx, bool active)
{
	ModuleRec &rec = m_modules[idx];
	if (rec.locked || rec.active == active) return;
	rec.active = active;
	if (active) {
		pCfg->AddActiveModule (rec.name);
		pLp->App ()->LoadModule (PluginDir, rec.name.c_str ());
	}
	else {
		pCfg->DelActiveModule (rec.name);
		pLp->App ()->UnloadModule (rec.name);
	}
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::DeactivateAll ()
{
	for (size_t i = 0; i < m_modules.size (); i++)
		SetActive (i, false);
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::Draw ()
{
	const float sc = pLp->UIScale ();
	const ImGuiStyle &style = ImGui::GetStyle ();
	float footer = ImGui::GetFrameHeight () + style.ItemSpacing.y;
	float h = ImGui::GetContentRegionAvail ().y - footer;

	ImGui::BeginChild ("##modlist", ImVec2 (m_listWidth * sc, h), ImGuiChildFlags_Borders | ImGuiChildFlags_ResizeX);
	for (auto &cat : m_categories) {
		if (m_expand) ImGui::SetNextItemOpen (m_expand > 0);
		if (ImGui::TreeNodeEx (cat.name.c_str (), ImGuiTreeNodeFlags_SpanAvailWidth)) {
			for (size_t idx : cat.modules) {
				ModuleRec &rec = m_modules[idx];
				ImGui::PushID ((int)idx);
				bool active = rec.active;
				ImGui::BeginDisabled (rec.locked);
				if (ImGui::Checkbox ("##act", &active))
					SetActive (idx, active);
				ImGui::EndDisabled ();
				if (rec.locked && ImGui::IsItemHovered (ImGuiHoveredFlags_AllowWhenDisabled))
					ImGui::SetTooltip ("This module has been requested on the command line and cannot be deactivated interactively.");
				ImGui::SameLine ();
				if (ImGui::Selectable (rec.name.c_str (), m_sel == (int)idx))
					m_sel = (int)idx;
				if (ImGui::IsItemHovered () && ImGui::IsMouseDoubleClicked (ImGuiMouseButton_Left) && !rec.locked)
					SetActive (idx, !rec.active);
				ImGui::PopID ();
			}
			ImGui::TreePop ();
		}
	}
	m_expand = 0;
	m_listWidth = ImGui::GetWindowWidth () / sc;
	ImGui::EndChild ();

	ImGui::SameLine ();
	ImGui::BeginChild ("##modinfo", ImVec2 (0, h), ImGuiChildFlags_Borders);
	if (m_sel >= 0 && m_sel < (int)m_modules.size ()) {
		const ModuleRec &rec = m_modules[m_sel];
		ImGui::PushFont (pLp->HeadingFont (), pLp->HeadingFont () ? pLp->HeadingFont ()->LegacySize : 0.0f);
		ImGui::TextUnformatted (rec.name.c_str ());
		ImGui::PopFont ();
		if (!rec.info.empty ())
			ImGui::TextWrapped ("%s", rec.info.c_str ());
	}
	ImGui::EndChild ();

	if (ImGui::Button ("Expand all")) m_expand = 1;
	ImGui::SameLine ();
	if (ImGui::Button ("Collapse all")) m_expand = -1;
	ImGui::SameLine ();
	if (ImGui::Button ("Deactivate all")) DeactivateAll ();
}
