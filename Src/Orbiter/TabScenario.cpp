// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ScenarioTab class
//=============================================================================

#include "TabScenario.h"
#include "Launchpad.h"
#include "Orbiter.h"
#include "UIUtil.h"
#include <imgui_internal.h>
#include <sstream>
#include <string>

using namespace std;

//-----------------------------------------------------------------------------

orbiter::ScenarioTab::ScenarioTab(const LaunchpadDialog2 *lp)
    : LaunchpadTab2(lp, "Scenarios"), img_folder1(nullptr),
      img_folder2(nullptr), img_scn1(nullptr), tree(ScenarioTreeItem()),
      startPaused(false) {
    saveScnDesc.reserve(64);
    saveScnDesc.reserve(256);

    img_folder1 = std::make_shared<LpImage>(m_lp->Win(),
                                            "Textures/OrbiterCore/Folder1.png");
    img_folder2 = std::make_shared<LpImage>(m_lp->Win(),
                                            "Textures/OrbiterCore/Folder2.png");
    img_scn1 =
        std::make_shared<LpImage>(m_lp->Win(), "Textures/OrbiterCore/Scn1.png");

    RefreshList(false);
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::GetConfig(const Config *cfg) {
    startPaused = cfg->CfgLogicPrm.bStartPaused;
    scnListW = cfg->CfgWindowPos.LaunchpadScnListWidth;
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::SetConfig(Config *cfg) {
    cfg->CfgLogicPrm.bStartPaused = startPaused;
    cfg->CfgWindowPos.LaunchpadScnListWidth = scnListW;
}

void orbiter::ScenarioTab::RenderTree(
    const SimpleTree<ScenarioTreeItem> &tree) {
    bool selected = tree.item.path == selection;
    const bool prev_selected = selected;
    if (tree.children.empty()) {
        ImGui::SetNextItemAllowOverlap();

        ImGui::PushID(tree.item.path.u8string().c_str());
        ImGui::Selectable("##Select", &selected);
        ImGui::PopID();
        ImGui::SameLine(0.0, 0.0);
        ImGui::Bullet();

        if (selected && !prev_selected) {
            selection = tree.item.path;
            ScenarioChanged();
        }
        ImGui::SameLine();
        ImGui::Image(
            selected
                ? tree.item.selIcon->TexID()
                : tree.item.icon->TexID(),
            ImVec2(16, 16));
        ImGui::SameLine();
        ImGui::Text(tree.item.name.c_str());
        return;
    }

    ImGui::PushID(tree.item.path.u8string().c_str());
    const bool wasOpen = ImGui::TreeNodeGetOpen(ImGui::GetID("##TreeNode"));
    const bool treeNode =
        ImGui::TreeNodeEx("##TreeNode", ImGuiTreeNodeFlags_SpanAvailWidth);
    if (treeNode != wasOpen) {
        selection = tree.item.path;
        ScenarioChanged();
    }
    ImGui::SameLine(0, 0);
    ImGui::Image(
        treeNode ? tree.item.selIcon->TexID()
                 : tree.item.icon->TexID(),
        ImVec2(16, 16));
    ImGui::SameLine();
    ImGui::Text(tree.item.name.c_str());
    if (treeNode) {
        for (const auto &subtree : tree.children) {
            RenderTree(subtree);
        }
        ImGui::TreePop();
    }
    ImGui::PopID();
}

void orbiter::ScenarioTab::OnDraw(oapi::WithImCtx<LpImCtx> &ctx) {
    const auto height =
        ImGui::GetContentRegionAvail().y - ImGui::GetFrameHeight();
    ImGui::BeginChild("ScnList", ImVec2(static_cast<float>(scnListW), height),
                      ImGuiChildFlags_Border | ImGuiChildFlags_ResizeX,
                      ImGuiWindowFlags_HorizontalScrollbar);
    scnListW = static_cast<int>(ImGui::GetWindowWidth());
    for (const auto &subtree : tree.children) {
        RenderTree(subtree);
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ScnDesc", ImVec2(0, height), ImGuiChildFlags_None,
                      ImGuiWindowFlags_HorizontalScrollbar);
    ImGui::Markdown(ctx, desc, loadedImages);
    ImGui::EndChild();
    if (ImGui::Button("Save current...")) {
        if (fs::exists(m_lp->App()->ScnPath("(Current state)"))) {
            ImGui::OpenPopup("Save Scenario##SaveScn");
        } else {
            // TODO: notification
            SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Save Error",
                                     "No current simulation state available.",
                                     ctx->Win()->Inner());
        }
    }
    ImGui::SameLine();
    if (ImGui::Button("Clear quicksaves")) {
        ClearQSFolder();
    }
    ImGui::SameLine();
    ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 1.0f);
    ImGui::Checkbox("Start paused", &startPaused);
    ImGui::PopStyleVar();

    const ImVec2 center = ImGui::GetMainViewport()->GetCenter();
    ImGui::SetNextWindowPos(center, ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));
    if (ImGui::BeginPopupModal("Save Scenario##SaveScn", nullptr,
                               ImGuiWindowFlags_AlwaysAutoResize)) {
        ImGui::PushFont(ctx->BoldFont());
        ImGui::Text("Name");
        ImGui::PopFont();
        ImGui::InputText("##SaveScnName", saveScnName);

        ImGui::PushFont(ctx->BoldFont());
        ImGui::Text("Description");
        ImGui::PopFont();
        ImGui::InputTextMultiline("##SaveScnDesc", saveScnDesc,
                                  ImVec2(0.0f, 0.0f));

        if (ImGui::Button("Save")) {
            if (saveScnName.length() > 63) {
                // TODO: notification
                SDL_ShowSimpleMessageBox(
                    SDL_MESSAGEBOX_ERROR, "Save Error",
                    "Scenario name too long (max 63 characters).",
                    ctx->Win()->Inner());
            } else {
                const int res =
                    SaveCurScenarioAs(saveScnName.c_str(), saveScnDesc.c_str());
                if (res == 2) {
                    ImGui::OpenPopup("Warning##SaveScnOverwrite");
                } else if (res == 1) {
                    // TODO: notification
                    SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Save Error",
                                             "Error writing scenario file.",
                                             ctx->Win()->Inner());
                } else {
                    RefreshList(true);
                    ImGui::CloseCurrentPopup();
                    saveScnName.clear();
                    saveScnDesc.clear();
                }
            }
        }
        ImGui::SameLine();
        if (ImGui::Button("Cancel")) {
            ImGui::CloseCurrentPopup();
            saveScnName.clear();
            saveScnDesc.clear();
        }
        ImGui::SetNextWindowPos(center, ImGuiCond_Appearing,
                                ImVec2(0.5f, 0.5f));
        if (ImGui::BeginPopupModal("Warning##SaveScnOverwrite", nullptr,
                                   ImGuiWindowFlags_AlwaysAutoResize)) {
            ImGui::Text("File exists. Overwrite?");
            if (ImGui::Button("Yes")) {
                if (SaveCurScenarioAs(saveScnName.c_str(), saveScnDesc.c_str(),
                                      true)) {
                    // TODO: notification
                    SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, "Save Error",
                                             "Error writing scenario file.",
                                             ctx->Win()->Inner());
                    ImGui::CloseCurrentPopup();
                } else {
                    RefreshList(true);
                    ImGui::ClosePopupToLevel(0, true);
                    saveScnName.clear();
                    saveScnDesc.clear();
                }
            }
            ImGui::SameLine();
            if (ImGui::Button("No")) {
                ImGui::CloseCurrentPopup();
            }
            ImGui::EndPopup();
        }

        ImGui::EndPopup();
    }
}

// TODO: Recursion limit?
SimpleTree<orbiter::ScenarioTreeItem>
orbiter::ScenarioTab::BuildScnTree(const ScenarioTreeItem &root) {
    auto tree = SimpleTree(root);
    for (const auto &elem :
         fs::directory_iterator(std::filesystem::path(root.path))) {
        if (elem.is_directory()) {
            auto item = ScenarioTreeItem{};
            item.icon = img_folder1;
            item.selIcon = img_folder2;
            item.path = elem.path();
            item.name = elem.path().filename().u8string();
            tree.children.push_back(BuildScnTree(item));
        } else {
            if (elem.path().filename().u8string() == "Description.txt") {
                continue;
            }
            auto item = ScenarioTreeItem{};
            item.icon = img_scn1;
            item.selIcon = img_scn1;
            item.path = elem.path();
            item.name = elem.path().stem().u8string();
            auto subtree = SimpleTree(item);
            tree.children.push_back(subtree);
        }
    }
    return tree;
}

void orbiter::ScenarioTab::RefreshList(bool preserveSelection) {
    ScenarioTreeItem root = {};
    root.icon = img_folder1;
    root.selIcon = img_folder2;
    root.path = fs::path(m_cfg->CfgDirPrm.ScnDir);
    root.name = "Scenarios";
    tree = BuildScnTree(root);
    if (!tree.children.empty() && !preserveSelection) {
        selection = tree.children.front().item.path;
        ScenarioChanged();
    }
}

void orbiter::ScenarioTab::ScenarioChanged() {
    loadedImages.clear();
    std::ifstream file;
    if (is_directory(selection)) {
        file.open(fs::path(selection).append("Description.txt").c_str());
    } else {
        file.open(selection);
    }
    std::string line;
    if (!file.is_open()) {
        // TODO: log error?
        return;
    }
    bool in_desc = false;
    std::stringstream desc;
    while (std::getline(file, line)) {
        if (line.find("BEGIN_DESC") != std::string::npos) {
            in_desc = true;
        } else if (line.find("END_DESC") != std::string::npos) {
            break;
        } else if (in_desc) {
            desc << line << "\n";
        }
    }
    this->desc = desc.str();
}

//-----------------------------------------------------------------------------
// Name: SaveCurScenarioAs()
// Desc: copy current scenario file into 'name', replacing description with
// 'desc'.
//		 return value: 0=ok, 1=failed, 2=file exists (only checked if
// replace=false)
//-----------------------------------------------------------------------------
int orbiter::ScenarioTab::SaveCurScenarioAs(const char *name, const char *desc,
                                            bool replace) {
    string cbuf;
    bool skip = false;
    fs::path path = m_lp->App()->ScnPath(name);
    if (!replace) {
        // check if exists
        if (fs::exists(path))
            return 2;
    }
    ofstream ofs(path);
    if (!ofs)
        return 1;
    ifstream ifs(m_lp->App()->ScnPath("(Current state)"));
    if (!ifs)
        return 1;
    ofs << "BEGIN_DESC" << endl;
    ofs << desc << endl;
    ofs << "END_DESC" << endl;
    while (std::getline(ifs, cbuf)) {
        if (cbuf == "BEGIN_DESC")
            skip = true;
        else if (cbuf == "END_DESC")
            skip = false;
        else if (!skip)
            ofs << cbuf << endl;
    }
    if (fs::absolute(selection) == fs::absolute(path)) {
        ScenarioChanged();
    }
    return 0;
}

//-----------------------------------------------------------------------------
// Name: ClearQSFolder()
// Desc: Delete all scenarios in the Quicksave folder
//-----------------------------------------------------------------------------
void orbiter::ScenarioTab::ClearQSFolder() {
    fs::path scnpath{m_lp->App()->ScnPath("Quicksave")};
    scnpath.replace_extension(); // remove ".scn"

    std::error_code ec;
    fs::remove_all(scnpath, ec);
    if (!ec) {
        fs::create_directory(scnpath);
    }
    if (selection.parent_path() == scnpath) {
        RefreshList(false);
    } else {
        RefreshList(true);
    }
}
