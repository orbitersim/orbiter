// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// VideoTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#include <windows.h>
#include "Orbiter.h"
#include "Launchpad.h"
#include "TabVideo.h"
#include "GraphicsAPI.h"
#include "OrbiterPlatform.h"
#include "imgui.h"
#include <algorithm>

static const char *strInfo_Default = "No graphics engine has been selected. Orbiter will run in console mode.";
static const char *PluginDir = "Modules/Plugin";

//-----------------------------------------------------------------------------
// DefVideoTab class

orbiter::DefVideoTab::DefVideoTab (LaunchpadDialog *lp): LaunchpadTab (lp, "Video", "tab_video")
{
	idxClient = 0;
	strInfo = strInfo_Default;
	EnumerateClients ();
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::EnumerateClients ()
{
	m_clients.clear ();
	m_clients.push_back ({ "Console mode (no engine loaded)", "", strInfo_Default });
	ScanDir (PluginDir);
	std::sort (m_clients.begin () + 1, m_clients.end (), [](const Client &a, const Client &b) {
		return strcasecmp (a.name.c_str (), b.name.c_str ()) < 0;
	});
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::ScanDir (const fs::path &dir)
{
	const std::string suffix = OCompatModuleSuffix ();
	std::error_code ec;
	for (auto &entry : fs::directory_iterator (dir, ec)) {
		fs::path modulepath;
		std::string clientname;
		if (entry.is_directory (ec)) {
			clientname = entry.path ().filename ().string ();
			modulepath = dir / clientname / (clientname + suffix);
			if (!fs::exists (modulepath, ec))
				continue;
		}
		else if (entry.path ().extension ().string () == suffix) {
			modulepath = entry.path ();
			clientname = modulepath.stem ().string ();
		}
		else
			continue;

		std::string cat, desc;
		if (oplat::ReadModuleInfo (modulepath.string (), cat, desc) && cat == "Graphics engines") {
			desc.erase (std::remove (desc.begin (), desc.end (), '\r'), desc.end ());
			m_clients.push_back ({ clientname, modulepath.string (), desc });
		}
	}
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::Activated ()
{
	if (!idxClient && pLp->App ()->GetGraphicsClient ()) {
		for (size_t i = 1; i < m_clients.size (); i++)
			if (pCfg->IsActiveModule (m_clients[i].name)) {
				idxClient = i;
				strInfo = m_clients[i].info;
				break;
			}
	}
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::OnGraphicsClientLoaded (oapi::GraphicsClient *gc, const char *moduleName)
{
	std::string fname = fs::path (moduleName ? moduleName : "").stem ().string ();
	size_t newIdx = 0;
	for (size_t i = 1; i < m_clients.size (); i++)
		if (!strcasecmp (m_clients[i].name.c_str (), fname.c_str ())) { newIdx = i; break; }
	if (newIdx != idxClient) {
		pCfg->AddActiveModule (fname);
		idxClient = newIdx;
	}

	std::string cat, desc;
	if (moduleName && oplat::ReadModuleInfo (moduleName, cat, desc) && !desc.empty ()) {
		desc.erase (std::remove (desc.begin (), desc.end (), '\r'), desc.end ());
		strInfo = desc;
	}
	else if (newIdx)
		strInfo = m_clients[newIdx].info;
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SetConfig (Config *cfg)
{
	oapi::GraphicsClient *gc = pLp->App ()->GetGraphicsClient ();
	if (gc) {
		gc->clbkRefreshVideoData ();
		oapi::GraphicsClient::VIDEODATA *data = gc->GetVideoData ();
		cfg->CfgDevPrm.bFullscreen = data->fullscreen;
		cfg->CfgDevPrm.bNoVsync    = data->novsync;
		cfg->CfgDevPrm.bPageflip   = data->pageflip;
		cfg->CfgDevPrm.bTryStencil = data->trystencil;
		cfg->CfgDevPrm.bForceEnum  = data->forceenum;
		cfg->CfgDevPrm.WinW        = data->winw;
		cfg->CfgDevPrm.WinH        = data->winh;
		cfg->CfgDevPrm.Device_idx  = data->deviceidx;
		cfg->CfgDevPrm.Device_mode = data->modeidx;
		cfg->CfgDevPrm.Device_out  = data->outputidx;
		cfg->CfgDevPrm.Device_style= data->style;
	} else {
		cfg->CfgDevPrm.bFullscreen = false;
		cfg->CfgDevPrm.bNoVsync    = true;
		cfg->CfgDevPrm.bPageflip   = true;
		cfg->CfgDevPrm.bTryStencil = false;
		cfg->CfgDevPrm.bForceEnum  = true;
		cfg->CfgDevPrm.WinW        = 400;
		cfg->CfgDevPrm.WinH        = 300;
		cfg->CfgDevPrm.Device_idx  = 0;
		cfg->CfgDevPrm.Device_mode = 0;
		cfg->CfgDevPrm.Device_out  = 0;
		cfg->CfgDevPrm.Device_style= 1;
	}
	cfg->CfgDevPrm.bStereo = false;
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SelectClientIndex (size_t idx)
{
	if (idx == idxClient || idx >= m_clients.size ()) return;
	if (idxClient) {
		const std::string &name = m_clients[idxClient].name;
		pCfg->DelActiveModule (name);
		pLp->App ()->UnloadModule (name);
		pCfg->CfgDevPrm.Device_idx = -1;
		idxClient = 0;
	}
	strInfo = strInfo_Default;
	if (idx) {
		pLp->App ()->LoadModule (PluginDir, m_clients[idx].name.c_str ());
		if (pLp->App ()->GetGraphicsClient () && !idxClient) {
			idxClient = idx;
			pCfg->AddActiveModule (m_clients[idx].name);
			strInfo = m_clients[idx].info;
		}
	}
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::Draw ()
{
	ImGui::TextUnformatted ("Graphics engine");
	ImGui::SetNextItemWidth (-FLT_MIN);
	if (idxClient >= m_clients.size ()) idxClient = 0;
	if (ImGui::BeginCombo ("##engine", m_clients[idxClient].name.c_str ())) {
		for (size_t i = 0; i < m_clients.size (); i++) {
			if (ImGui::Selectable (m_clients[i].name.c_str (), i == idxClient))
				SelectClientIndex (i);
			if (i == idxClient) ImGui::SetItemDefaultFocus ();
		}
		ImGui::EndCombo ();
	}
	if (!strInfo.empty ()) {
		ImGui::PushStyleColor (ImGuiCol_Text, ImGui::GetStyleColorVec4 (ImGuiCol_TextDisabled));
		ImGui::TextWrapped ("%s", strInfo.c_str ());
		ImGui::PopStyleColor ();
	}

	oapi::GraphicsClient *gc = pLp->App ()->GetGraphicsClient ();
	if (gc && idxClient) {
		ImGui::Spacing ();
		ImGui::Separator ();
		ImGui::Spacing ();
		ImGui::BeginChild ("##clientprm", ImVec2 (0, 0), ImGuiChildFlags_None);
		gc->clbkLaunchpadVideoTab ();
		ImGui::EndChild ();
	}
}
