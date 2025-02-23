// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ModuleTab class
//=============================================================================

#include "TabModule.h"
#include "Launchpad.h"
#include "Orbiter.h"
#include "resource.h"
#include <filesystem>
#include <imgui_internal.h>
#include <windows.h>
namespace fs = std::filesystem;

using std::max;

extern char DBG_MSG[256];

//-----------------------------------------------------------------------------

orbiter::ModuleTab::ModuleTab(LaunchpadDialog2 *lp)
    : LaunchpadTab2(lp, "Modules") {}

//-----------------------------------------------------------------------------

orbiter::ModuleTab::~ModuleTab() {}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::GetConfig(const Config *cfg) {
    RefreshLists();

    splitWidth = cfg->CfgWindowPos.LaunchpadModListWidth;
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::SetConfig(Config *cfg) {
    cfg->CfgWindowPos.LaunchpadModListWidth = splitWidth;
}

void orbiter::ModuleTab::OnDraw(WithLpImCtx &ctx) {
    static const std::string defaultDesc = u8R"(
# Optional Orbiter plugin modules.

Click on a category to show or hide its entries.

Check or uncheck items to activate the corresponding modules.

Select an item to see a description of the module function.
)";
    if (splitWidth == 0) {
        splitWidth = static_cast<int>(0.33f * ImGui::GetContentRegionAvail().x);
    }
    const auto height =
        ImGui::GetContentRegionAvail().y - ImGui::GetFrameHeight();
    ImGui::BeginChild("ModList", ImVec2(static_cast<float>(splitWidth), height),
                      ImGuiChildFlags_Border | ImGuiChildFlags_ResizeX,
                      ImGuiWindowFlags_None);
    splitWidth = static_cast<int>(ImGui::GetWindowWidth());
    for (auto &category : categories) {
        bool selected = category.name == selection && selectionIsCat;

        ImGui::PushID(category.name.c_str());
        ImGui::PushID("Category");
        if (doExpandCollapse) {
            ImGui::SetNextItemOpen(collapseExpand);
        }
        const bool wasOpen = ImGui::TreeNodeGetOpen(ImGui::GetID("##TreeNode"));
        const bool treeNode =
            ImGui::TreeNodeEx("##TreeNode", ImGuiTreeNodeFlags_SpanAvailWidth);
        if (treeNode != wasOpen && !doExpandCollapse) {
            selection = category.name;
            selectionIsCat = true;
            desc.clear();
            loadedImages.clear();
        }
        ImGui::SameLine();
        ImGui::Text(category.name.c_str());
        if (treeNode) {
            for (auto &item : category.items) {
                selected = item.name == selection && !selectionIsCat;
                const bool prev_selected = selected;
                bool enabled = item.active;
                const bool prev_enabled = enabled;
                ImGui::PushID(item.name.c_str());
                ImGui::PushID("Item");

                if (item.locked) {
                    ImGui::BeginDisabled();
                    ImGui::BeginGroup();
                }
                ImGui::Checkbox("##Checkbox", &enabled);
                ImGui::SameLine();
                ImGui::Selectable(item.name.c_str(), &selected);
                if (item.locked) {
                    ImGui::EndDisabled();
                    ImGui::SameLine();
                    ImGui::Text(u8"\uf059");
                    ImGui::EndGroup();
                    ImGui::SetItemTooltip(
                        "This module has been requested on the command line "
                        "and cannot be deactivated interactively.");
                }

                if (selected && !prev_selected) {
                    selection = item.name;
                    selectionIsCat = false;
                    desc = item.info;
                    if (desc.empty()) {
                        desc = "This module does not provide any info.";
                    }
                    loadedImages.clear();
                }
                if (enabled && !prev_enabled) {
                    item.active = true;
                    m_cfg->AddActiveModule(item.name);
                    m_lp->App()->LoadModule("Modules/Plugin",
                                            item.name.c_str());
                } else if (!enabled && prev_enabled) {
                    item.active = false;
                    m_cfg->DelActiveModule(item.name);
                    m_lp->App()->UnloadModule(item.name);
                }

                ImGui::PopID();
                ImGui::PopID();
            }
            ImGui::TreePop();
        }
        ImGui::PopID();
        ImGui::PopID();
    }
    if (desc.empty()) {
        desc = defaultDesc;
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ModDesc", ImVec2(0.0, height), ImGuiChildFlags_None,
                      ImGuiWindowFlags_HorizontalScrollbar);
    ImGui::Markdown(ctx, desc, loadedImages);
    ImGui::EndChild();
    doExpandCollapse = false;
    if (ImGui::Button("Expand all")) {
        collapseExpand = true;
        doExpandCollapse = true;
    }
    ImGui::SameLine();
    if (ImGui::Button("Collapse All")) {
        collapseExpand = false;
        doExpandCollapse = true;
    }
    ImGui::SameLine();
    if (ImGui::Button("Deactivate all")) {
        DeactivateAll();
    }
}

//-----------------------------------------------------------------------------
void orbiter::ModuleTab::RefreshLists() {
    categories.clear();

    const fs::path moddir{"Modules/Plugin"};

    for (const auto &file : fs::directory_iterator(moddir)) {
        if (file.path().extension().u8string() == ORBITER_SOFILE_EXTENSION) {
            auto name = file.path().stem().u8string();
            // add module record
            ModuleInfo info = {};
            info.name = name;
            info.info = std::string();
            info.active = false;
            info.locked = false;

            // check if module is set active in config
            if (m_cfg->IsActiveModule(info.name))
                info.active = true;

            // check if module is set active in command line
            if (std::find(m_cfg->CfgCmdlinePrm.LoadPlugins.begin(),
                          m_cfg->CfgCmdlinePrm.LoadPlugins.end(), info.name) !=
                m_cfg->CfgCmdlinePrm.LoadPlugins.end()) {
                info.active = true;
                info.locked = true; // modules activated from the command line
                                    // are not to be unloaded
            }

            std::string category = "Miscellaneous";
            std::ifstream infoFile(
                fs::path(file.path()).replace_extension(".info"));
            if (infoFile.is_open()) {
                std::string line;
                bool inDesc = false;
                while (std::getline(infoFile, line)) {
                    if (line.find("BEGIN_DESC") != std::string::npos) {
                        inDesc = true;
                    } else if (line.find("END_DESC") != std::string::npos) {
                        inDesc = false;
                    } else if (inDesc) {
                        info.info.append(line).append("\n");
                    } else if (line.find("CATEGORY") != std::string::npos) {
                        constexpr auto off = "CATEGORY "sv.length();
                        category = line.substr(off);
                    }
                }
            }

            if (category == "Graphics engines")
                continue; // graphics client modules are loaded via the Video
                          // tab

            // find the category entry
            ModuleCategory &catItem = GetCategoryItem(category);

            catItem.items.push_back(info);
        }
    }
    std::sort(categories.begin(), categories.end(),
              [](const auto &a, const auto &b) {
                  return std::lexicographical_compare(
                      a.name.begin(), a.name.end(), b.name.begin(),
                      b.name.end(), [](char c1, char c2) {
                          return std::tolower(c1) < std::tolower(c2);
                      });
              });
    for (auto &category : categories) {
        std::sort(category.items.begin(), category.items.end(),
                  [](const auto &a, const auto &b) {
                      return std::lexicographical_compare(
                          a.name.begin(), a.name.end(), b.name.begin(),
                          b.name.end(), [](char c1, char c2) {
                              return std::tolower(c1) < std::tolower(c2);
                          });
                  });
    }
}

orbiter::ModuleTab::ModuleCategory &
orbiter::ModuleTab::GetCategoryItem(std::string_view catName) {
    for (auto &cat : categories) {
        if (cat.name == catName) {
            return cat;
        }
    }
    categories.push_back(
        ModuleCategory{std::string(catName), std::vector<ModuleInfo>()});
    return categories.back();
}

void orbiter::ModuleTab::DeactivateAll() {
    for (auto &cat : categories) {
        for (auto &info : cat.items) {
            info.active = false;
            m_cfg->DelActiveModule(info.name);
            m_lp->App()->UnloadModule(info.name);
        }
    }
}
