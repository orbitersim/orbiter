// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// OptionsTab class
//=============================================================================

#include "TabOptions.h"
#include "DlgOptions.h"
#include "Help.h"
#include "imgui.h"

//=============================================================================

orbiter::OptionsTab::OptionsTab (LaunchpadDialog* lp)
	: LaunchpadTab (lp, "Options", "tab_param")
	, m_page (0)
{
	m_opt = std::make_unique<DlgOptions> ();
	m_pages.push_back ({ "Visual settings", "/tab_visual.htm", &DlgOptions::DrawVisual, false });
	m_pages.push_back ({ "Physics settings", "/tab_param.htm", &DlgOptions::DrawPhysics, false });
	for (auto &t : DlgOptions::tabs) {
		std::string name = t.name;
		size_t n = name.find_first_not_of (' ');
		bool child = n > 0 && n != std::string::npos;
		m_pages.push_back ({ child ? name.substr (n) : name, t.helptopic, t.func, child });
	}
}

//-----------------------------------------------------------------------------

orbiter::OptionsTab::~OptionsTab ()
{
}

//-----------------------------------------------------------------------------

bool orbiter::OptionsTab::OpenHelp ()
{
	if (m_page < m_pages.size ())
		OpenDefaultHelp (m_pages[m_page].helptopic);
	return true;
}

//-----------------------------------------------------------------------------

void orbiter::OptionsTab::LaunchpadShowing (bool show)
{
	if (show) {
		m_opt = std::make_unique<DlgOptions> ();
		m_page = 0;
	}
}

//-----------------------------------------------------------------------------

void orbiter::OptionsTab::Draw ()
{
	const float sc = pLp->UIScale ();
	ImGui::BeginChild ("##optlist", ImVec2 (190.0f * sc, 0), ImGuiChildFlags_Borders | ImGuiChildFlags_ResizeX);
	for (size_t i = 0; i < m_pages.size (); i++) {
		if (m_pages[i].child) ImGui::Indent ();
		if (ImGui::Selectable (m_pages[i].name.c_str (), m_page == i))
			m_page = i;
		if (m_pages[i].child) ImGui::Unindent ();
	}
	ImGui::EndChild ();

	ImGui::SameLine ();
	ImGui::BeginChild ("##optpage", ImVec2 (0, 0), ImGuiChildFlags_None);
	if (m_page < m_pages.size ()) {
		m_opt->currentPage.clear ();
		(m_opt.get ()->*m_pages[m_page].func) ();
		if (!m_opt->currentPage.empty ()) {
			std::string req = m_opt->currentPage;
			size_t n = req.find_first_not_of (' ');
			if (n != std::string::npos) req = req.substr (n);
			for (size_t i = 0; i < m_pages.size (); i++)
				if (m_pages[i].name == req) m_page = i;
		}
	}
	ImGui::EndChild ();
}
