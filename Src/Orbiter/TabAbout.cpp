// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// AboutTab class
//=============================================================================

#include "TabAbout.h"
#include "Orbiter.h"
#include "about.hpp"

//-----------------------------------------------------------------------------
// AboutTab class

orbiter::AboutTab::AboutTab(LaunchpadDialog2 *lp)
    : LaunchpadTab2(lp, "About"), savedWidth(0) {
    icon =
        std::make_shared<LpImage>(lp->Win(), "Textures/OrbiterCore/Icon64.png");
}

//-----------------------------------------------------------------------------

void orbiter::AboutTab::RenderCentered(WithLpImCtx &ctx) {
    ImGui::BeginGroup();

    ImGui::Image(icon->TexID(), ImVec2(64, 64));
    ImGui::SameLine();
    ImGui::SetCursorPosY(ImGui::GetFrameHeight() / 2);
    ImGui::BeginGroup();
    ImGui::Text(NAME1);
    ImGui::Text(SIG4);
    ImGui::Text(SIG1B);
    ImGui::EndGroup();

    ImGui::EndGroup();
}

void orbiter::AboutTab::OnDraw(WithLpImCtx &ctx) {
    auto max = ImGui::GetContentRegionAvail();
    auto origin = ImGui::GetCursorScreenPos();
    auto center = ImVec2(max.x / 2 + origin.x, max.y / 2 + origin.y);
    ImGui::SetNextWindowPos(center, ImGuiCond_Always, ImVec2(0.5f, 0.5f));
    if (ImGui::BeginChild("##About", ImVec2(512, 512), ImGuiChildFlags_Border,
                          ImGuiWindowFlags_None)) {
        // HACK: center this window by rendering first with 0 alpha, then saving
        // the width. Content is not dynamic width so it won't matter.
        if (savedWidth == 0) {
            ImGui::PushStyleVar(ImGuiStyleVar_Alpha, 0.0f);
            RenderCentered(ctx);
            ImGui::PopStyleVar();
            savedWidth = ImGui::GetItemRectSize().x;
        }
        ImGui::SetCursorPosX(ImGui::GetContentRegionAvail().x / 2 -
                             savedWidth / 2);
        RenderCentered(ctx);

        ImGui::SetNextItemOpen(true, ImGuiCond_Appearing);
        if (ImGui::CollapsingHeader("Installed components")) {
            for (const auto &module : m_lp->App()->LoadedModules()) {
                if (module.pModule) {
                    // TODO: cache this? (or use the .info file?)
                    if (const auto copy =
                            module.pModule->clbkGetModuleCopyright()) {
                        ImGui::BulletText(copy);
                    }
                }
            }
        }

        ImGui::SeparatorText("On the web");
        if (ImGui::BeginTable("##OnTheWeb", 2,
                              ImGuiTableFlags_SizingStretchProp)) {
            ImGui::TableSetupColumn("##Title",
                                    ImGuiTableColumnFlags_WidthFixed);
            // N.B. no images will be loaded => no allocation done.
            // TODO: add a "MarkdownNoImages" or just simply "TextLink"
            // function?
            auto loadedImages = std::vector<std::shared_ptr<LpImage>>();
            ImGui::TableNextColumn();
            ImGui::TextUnformatted("Orbiter home:");
            ImGui::TableNextColumn();
            ImGui::Markdown(ctx, "<http://orbit.medphys.ucl.ac.uk>");
            ImGui::TableNextColumn();
            ImGui::TextUnformatted("Orbiter web forum:");
            ImGui::TableNextColumn();
            ImGui::Markdown(ctx, "<https://orbiter-forum.com>");
            ImGui::TableNextColumn();
            ImGui::TextUnformatted("YouTube channel:");
            ImGui::TableNextColumn();
            ImGui::Markdown(ctx, "<https://www.youtube.com/user/orbitersim>");
            ImGui::EndTable();
        }

        ImGui::TextWrapped("Orbiter is published under the MIT License. "
                           "Details can be found under \"License\" below.");

        if (ImGui::CollapsingHeader("License")) {
            ImGui::TextWrapped(
                "Orbiter Space Flight Simulator"
                "\n\nMIT License"
                "\n\nCopyright 2000-2021 Martin Schweiger"
                "\n\nPermission is hereby granted, free of charge, to any "
                "person obtaining a copy of this software and associated "
                "documentation files (the \"Software\"), to deal in the "
                "Software without restriction, including without limitation "
                "the rights to use, copy, modify, merge, publish, distribute, "
                "sublicense, and/or sell copies of the Software, and to permit "
                "persons to whom the Software is furnished to do so, subject "
                "to the following conditions:"
                "\n\nThe above copyright notice and this permission notice "
                "shall be included in all copies or substantial portions of "
                "the Software."
                "\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF "
                "ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO "
                "THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR "
                "PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR "
                "COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER "
                "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR "
                "OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE "
                "SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.");
        }

        const auto avail = ImGui::GetContentRegionAvail();
        ImGui::SetCursorPosX(avail.x / 6.0f);
        if (ImGui::Button("Orbiter Homepage", ImVec2(avail.x / 3.0f, 0.0))) {
            SDL_OpenURL("http://orbit.medphys.ucl.ac.uk");
        }
        ImGui::SameLine();
        if (ImGui::Button("Credits and Contributors",
                          ImVec2(avail.x / 3.0f, 0.0))) {
            // TODO: help page
        }
    }
    ImGui::EndChild();
}
