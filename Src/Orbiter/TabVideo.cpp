// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// VideoTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#include "TabVideo.h"
#include "Orbiter.h"
#include "resource.h"
#include <windows.h>

using namespace std;

static PCSTR strInfo_Default = "";

//-----------------------------------------------------------------------------
// DefVideoTab class

orbiter::DefVideoTab::DefVideoTab(LaunchpadDialog2 *lp)
    : LaunchpadTab2(lp, "Video"), m_selgc(0) {
    EnumerateClients();
}

//-----------------------------------------------------------------------------

orbiter::DefVideoTab::~DefVideoTab() {}

// //-----------------------------------------------------------------------------
//
// void orbiter::DefVideoTab::ShowInterface(HWND hTab, bool show)
// {
// 	static int item[] = {
// 		IDC_VID_STATIC1, IDC_VID_STATIC2, IDC_VID_STATIC3,
// IDC_VID_STATIC5, 		IDC_VID_STATIC6, IDC_VID_STATIC7,
// IDC_VID_STATIC8, IDC_VID_STATIC9, 		IDC_VID_DEVICE, IDC_VID_ENUM,
// IDC_VID_STENCIL, 		IDC_VID_FULL, IDC_VID_WINDOW, IDC_VID_MODE,
// IDC_VID_BPP, IDC_VID_VSYNC, 		IDC_VID_PAGEFLIP, IDC_VID_WIDTH,
// IDC_VID_HEIGHT, IDC_VID_ASPECT, 		IDC_VID_4X3, IDC_VID_16X10,
// IDC_VID_16X9, IDC_VID_INFO
// 	};
// 	for (int i = 0; i < ARRAYSIZE(item); i++) {
// 		ShowWindow(GetDlgItem(hTab, item[i]), show ? SW_SHOW : SW_HIDE);
// 	}
// }

//-----------------------------------------------------------------------------

orbiter::DefVideoTab::GcEntry
orbiter::DefVideoTab::LoadInfoFile(const fs::path &path) {
    auto name = path.stem().u8string();
    // add module record
    GcEntry info = {};
    info.name = name;
    info.mdinfo = std::string();
    info.path = path;
    info.category = "Miscellaneous";

    if (std::ifstream infoFile(fs::path(path).replace_extension(".info"));
        infoFile.is_open()) {
        std::string line;
        bool inDesc = false;
        while (std::getline(infoFile, line)) {
            if (line.find("BEGIN_DESC") != std::string::npos) {
                inDesc = true;
            } else if (line.find("END_DESC") != std::string::npos) {
                inDesc = false;
            } else if (inDesc) {
                info.mdinfo.append(line).append({'\n'});
            } else if (line.find("CATEGORY") != std::string::npos) {
                constexpr auto off = "CATEGORY "sv.length();
                info.category = line.substr(off);
            }
        }
    }
    return info;
}

void orbiter::DefVideoTab::OnGraphicsClientLoaded(
    oapi::GraphicsClient *gc, const std::string_view moduleName) {
    auto fname = fs::path(moduleName).stem().u8string();

    auto newIdx = std::distance(
        m_gcs.cbegin(),
        std::find_if(m_gcs.cbegin(), m_gcs.cend(),
                     [&fname](auto &p) { return p.name == fname; }));

    if (newIdx != m_selgc) {
        m_cfg->AddActiveModule(fname);
        m_selgc = newIdx;
    }
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SetConfig(Config *cfg) {
    // retrieve standard parameters from client, if available
    if (oapi::GraphicsClient *gc = m_lp->App()->GetGraphicsClient()) {
        // gc->clbkRefreshVideoData();
        // oapi::GraphicsClient::VIDEODATA *data = gc->GetVideoData();
        // cfg->CfgDevPrm.bFullscreen = data->fullscreen;
        // cfg->CfgDevPrm.bNoVsync = data->novsync;
        // cfg->CfgDevPrm.bPageflip = data->pageflip;
        // cfg->CfgDevPrm.bTryStencil = data->trystencil;
        // cfg->CfgDevPrm.bForceEnum = data->forceenum;
        // cfg->CfgDevPrm.WinW = data->winw;
        // cfg->CfgDevPrm.WinH = data->winh;
        // cfg->CfgDevPrm.Device_idx = data->deviceidx;
        // cfg->CfgDevPrm.Device_mode = data->modeidx;
        // cfg->CfgDevPrm.Device_out = data->outputidx;
        // cfg->CfgDevPrm.Device_style = data->style;
    } else {
        // should not be required
        cfg->CfgDevPrm.bFullscreen = false;
        cfg->CfgDevPrm.bNoVsync = true;
        cfg->CfgDevPrm.bPageflip = true;
        cfg->CfgDevPrm.bTryStencil = false;
        cfg->CfgDevPrm.bForceEnum = true;
        cfg->CfgDevPrm.WinW = 400;
        cfg->CfgDevPrm.WinH = 300;
        cfg->CfgDevPrm.Device_idx = 0;
        cfg->CfgDevPrm.Device_mode = 0;
        cfg->CfgDevPrm.Device_out = 0;
        cfg->CfgDevPrm.Device_style = 1;
    }
    cfg->CfgDevPrm.bStereo = false; // not currently set
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::EnumerateClients() {
    m_gcs.clear();
    m_gcs.push_back({"", "Console mode (no engine loaded)",
                     "No graphics engine has been selected. Orbiter will run "
                     "in console mode."});
    ScanDir("Modules/Plugin");
}

//-----------------------------------------------------------------------------
//! Find Graphics engine DLLs in dir
void orbiter::DefVideoTab::ScanDir(const fs::path &dir) {
    for (auto &entry : fs::directory_iterator(dir)) {
        fs::path modulepath;
        auto clientname = entry.path().stem().string();
        if (entry.is_directory()) {
            modulepath =
                dir / clientname / (clientname + ORBITER_SOFILE_EXTENSION);
            if (!fs::exists(modulepath))
                continue;
        } else if (entry.path().extension().string() ==
                   ORBITER_SOFILE_EXTENSION)
            modulepath = entry.path();
        else
            continue;

        auto gcent = LoadInfoFile(modulepath);
        if (gcent.category != "Graphics engines")
            continue;

        m_gcs.push_back(gcent);
    }
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SelectClientIndex(const size_t idx) {
    if (m_selgc > 0) {
        // unload the current client
        const auto &gcent = m_gcs.at(m_selgc);
        m_cfg->DelActiveModule(gcent.name);
        m_lp->App()->UnloadModule(gcent.name);
        m_cfg->CfgDevPrm.Device_idx = -1;
        m_loadedImages.clear();
    }
    if (idx > 0 && idx < m_gcs.size()) {
        // load the new client
        const auto &gcent = m_gcs.at(idx);
        m_lp->App()->LoadModule("Modules/Plugin", gcent.name.c_str());
    } else {
        m_selgc = 0;
    }
}

void orbiter::DefVideoTab::OnDraw(WithLpImCtx &ctx) {
    const auto &gcent = m_gcs.at(m_selgc);
    ImGui::SetNextItemWidth(300.0f);
    if (ImGui::BeginCombo("Graphics engine", gcent.name.c_str())) {
        for (auto it = m_gcs.cbegin(); it != m_gcs.cend(); ++it) {
            if (ImGui::Selectable(it->name.c_str(),
                                  std::distance(m_gcs.cbegin(), it) ==
                                      m_selgc)) {
                SelectClientIndex(std::distance(m_gcs.cbegin(), it));
            }
        }
        ImGui::EndCombo();
    }
    if (oapi::GraphicsClient *gc = m_lp->App()->GetGraphicsClient()) {
        ImGui::SeparatorText("Engine settings");
        gc->DrawLaunchpadVideoTab(ctx);
    }
    // TODO: collapsing header? maybe? idk it looked weird when I did it
    ImGui::SeparatorText("Engine info");
    ImGui::Markdown(ctx, gcent.mdinfo, m_loadedImages);
}
