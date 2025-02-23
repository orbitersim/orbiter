// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Template for simulation options pages
// ======================================================================

#include "OptionsPages.h"
#include "Camera.h"
#include "DlgCtrl.h"
#include "Orbiter.h"
#include "Psys.h"
#include "Uxtheme.h"
#include "resource.h"
#include <SDL3/SDL.h>
#include <array>
#include <cstdint>
#include <imgui_internal.h>
#include <utility>
#include <windows.h>

using std::max;
using std::min;

extern Orbiter *g_pOrbiter;
extern PlanetarySystem *g_psys;
extern Camera *g_camera;

// ======================================================================

OptionsPageContainer::OptionsPageContainer(Originator orig, Config *cfg)
    : m_orig(orig), m_cfg(cfg), m_pages(), m_currentPage(std::nullopt),
      splitWidth(0) {}

// ----------------------------------------------------------------------

OptionsPageContainer::~OptionsPageContainer() { Clear(); }

// ----------------------------------------------------------------------

std::optional<std::shared_ptr<OptionsPage>>
OptionsPageContainer::CurrentPage() {
    if (!m_currentPage.has_value())
        return std::nullopt;
    return m_currentPage.value()->page;
}

// ----------------------------------------------------------------------

void OptionsPageContainer::CreatePages() {
    if (m_orig == LAUNCHPAD) {
        AddPage(std::move(std::make_shared<OptionsPage_Visual>(this)));
        AddPage(std::move(std::make_shared<OptionsPage_Physics>(this)));
    }
    AddPage(std::move(std::make_shared<OptionsPage_Instrument>(this)));
    AddPage(std::move(std::make_shared<OptionsPage_Vessel>(this)));
    size_t parent = AddPage(std::move(std::make_shared<OptionsPage_UI>(this)));
    AddPage(std::move(std::make_shared<OptionsPage_Joystick>(this)), parent);
    AddPage(std::move(std::make_shared<OptionsPage_CelSphere>(this)));
    parent = AddPage(std::move(std::make_shared<OptionsPage_VisHelper>(this)));
    AddPage(std::move(std::make_shared<OptionsPage_Planetarium>(this)), parent);
    AddPage(std::move(std::make_shared<OptionsPage_Labels>(this)), parent);
    AddPage(std::move(std::make_shared<OptionsPage_Forces>(this)), parent);
    AddPage(std::move(std::make_shared<OptionsPage_Axes>(this)), parent);
}

// ----------------------------------------------------------------------

size_t OptionsPageContainer::AddPage(std::shared_ptr<OptionsPage> pPage,
                                     std::optional<size_t> parent) {

    std::optional<std::shared_ptr<OptionsPage>> parentEnt = std::nullopt;
    if (parent.has_value() && parent.value() < m_pages.size()) {
        const auto parentPos =
            m_pages.cbegin() + static_cast<ptrdiff_t>(parent.value());
        parentEnt = parentPos->page;
    }

    m_pages.push_back({std::move(pPage), std::move(parentEnt)});
    return m_pages.size() - 1;
}

// ----------------------------------------------------------------------

std::optional<std::shared_ptr<OptionsPage>>
OptionsPageContainer::FindPage(std::string_view name) const {
    for (const auto &entry : m_pages) {
        if (entry.page->Name() == name) {
            return entry.page;
        }
    }
    return std::nullopt;
}

// ----------------------------------------------------------------------

void OptionsPageContainer::SwitchPage(const std::string_view name) {
    for (auto &entry : m_pages) {
        if (entry.page->Name() == name) {
            m_currentPage = &entry;
            return;
        }
    }
    m_currentPage = std::nullopt;
}

void OptionsPageContainer::SwitchPage(const size_t page) {
    m_currentPage = &m_pages.at(page);
}

// ----------------------------------------------------------------------

void OptionsPageContainer::SwitchPage(
    const std::shared_ptr<OptionsPage> &page) {
    for (auto &entry : m_pages) {
        if (entry.page == page) {
            m_currentPage = &entry;
            return;
        }
    }
    m_currentPage = std::nullopt;
}

// ----------------------------------------------------------------------

void OptionsPageContainer::Clear() { m_pages.clear(); }

// ----------------------------------------------------------------------

void OptionsPageContainer::UpdatePages(bool resetView) {
    splitWidth = m_cfg->CfgWindowPos.LaunchpadOptListWidth;
    for (const auto &entry : m_pages) {
        entry.page->UpdateControls();
    }
    if (resetView) {
        m_currentPage = &m_pages.front();
    }
}

// ----------------------------------------------------------------------

void OptionsPageContainer::UpdateConfig() const {
    m_cfg->CfgWindowPos.LaunchpadOptListWidth = splitWidth;
    for (const auto &entry : m_pages) {
        entry.page->UpdateConfig();
    }
}

void OptionsPageContainer::RenderTree(oapi::WithImCtx &ctx,
                                      OptionTreeEntry *parent) {
    ImGui::SetNextItemOpen(true, ImGuiCond_Appearing);
    auto hasChildren =
        std::any_of(m_pages.begin(), m_pages.end(), [parent](const auto &page) {
            return page.parent == parent->page;
        });

    bool selected =
        m_currentPage.has_value() && m_currentPage.value() == parent;
    const bool prev_selected = selected;

    if (hasChildren) {
        ImGui::PushID(parent->page->Name().c_str());
        ImGui::SetNextItemOpen(true, ImGuiCond_Appearing);
        const bool treeNode = ImGui::TreeNodeEx(
            "##TreeNode", ImGuiTreeNodeFlags_SpanAvailWidth |
                              ImGuiTreeNodeFlags_OpenOnDoubleClick |
                              (selected ? ImGuiTreeNodeFlags_Selected
                                        : ImGuiTreeNodeFlags_None));
        if (ImGui::IsItemClicked(ImGuiMouseButton_Left)) {
            m_currentPage = parent;
        }
        ImGui::SameLine(0, 0);
        ImGui::Text(parent->page->Name().c_str());
        if (treeNode) {
            bool foundStart = false;
            for (auto entry = m_pages.begin(); entry != m_pages.end();
                 ++entry) {
                if (entry->parent == parent->page) {
                    foundStart = true;
                    RenderTree(ctx, &*entry);
                } else if (entry->parent != parent->page && foundStart) {
                    break;
                }
            }
            ImGui::TreePop();
        }
        ImGui::PopID();
    } else {
        ImGui::SetNextItemAllowOverlap();
        ImGui::PushID(parent->page->Name().c_str());
        ImGui::Selectable("##Select", &selected);
        ImGui::PopID();
        ImGui::SameLine(0.0, 0.0);
        ImGui::Bullet();

        if (selected && !prev_selected) {
            m_currentPage = parent;
        }
        ImGui::SameLine();
        ImGui::Text(parent->page->Name().c_str());
    }
}

void OptionsPageContainer::OnDraw(oapi::WithImCtx &ctx) {
    if (splitWidth == 0) {
        splitWidth = static_cast<int>(0.33f * ImGui::GetContentRegionAvail().x);
    }
    ImGui::BeginChild("PageList", ImVec2(static_cast<float>(splitWidth), 0.0),
                      ImGuiChildFlags_Border | ImGuiChildFlags_ResizeX,
                      ImGuiWindowFlags_None);
    splitWidth = static_cast<int>(ImGui::GetWindowWidth());
    for (auto entry = m_pages.begin(); entry != m_pages.end(); ++entry) {
        if (!entry->parent.has_value())
            RenderTree(ctx, &*entry);
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("PageOpts", ImVec2(0.0, 0.0), ImGuiChildFlags_None,
                      ImGuiWindowFlags_HorizontalScrollbar);
    if (m_currentPage.has_value()) {
        m_currentPage.value()->page->OnDraw(ctx);
    }
    ImGui::EndChild();
}

// ======================================================================

OptionsPage::OptionsPage(OptionsPageContainer *container, std::string name)
    : m_container(container), m_name(std::move(name)) {}

// ======================================================================

OptionsPage_Visual::OptionsPage_Visual(OptionsPageContainer *container)
    : OptionsPage(container, "Visual settings") {}

// ----------------------------------------------------------------------

void OptionsPage_Visual::OnDraw(oapi::WithImCtx &ctx) {
    ImGui::SeparatorText("Planetary effects");
    if (ImGui::BeginTable("##Checkboxes", 2,
                          ImGuiTableFlags_SizingStretchProp)) {
        ImGui::TableSetupColumn("##Col1", ImGuiTableColumnFlags_WidthFixed);

        ImGui::TableNextColumn();
        ImGui::Checkbox("Cloud layers", &Cfg()->CfgVisualPrm.bClouds);
        ImGui::TableNextColumn();
        ImGui::BeginDisabled(!Cfg()->CfgVisualPrm.bClouds);
        ImGui::Checkbox("Cloud shadows", &Cfg()->CfgVisualPrm.bCloudShadows);
        ImGui::EndDisabled();

        ImGui::TableNextColumn();
        ImGui::Checkbox("Horizon haze", &Cfg()->CfgVisualPrm.bHaze);
        ImGui::TableNextColumn();
        ImGui::Checkbox("Distance fog", &Cfg()->CfgVisualPrm.bFog);

        ImGui::TableNextColumn();
        ImGui::Checkbox("Specular water reflections",
                        &Cfg()->CfgVisualPrm.bWaterreflect);
        ImGui::TableNextColumn();
        ImGui::BeginDisabled(!Cfg()->CfgVisualPrm.bWaterreflect);
        ImGui::Checkbox("Specular ripples",
                        &Cfg()->CfgVisualPrm.bSpecularRipple);
        ImGui::EndDisabled();

        ImGui::TableNextColumn();
        ImGui::Checkbox("Planet night lights, at level (0-1):",
                        &Cfg()->CfgVisualPrm.bNightlights);
        ImGui::TableNextColumn();
        ImGui::SetNextItemWidth(64.0f);
        ImGui::BeginDisabled(!Cfg()->CfgVisualPrm.bNightlights);
        ImGui::InputDouble("##NightLights",
                           &Cfg()->CfgVisualPrm.LightBrightness, 0, 0, "%.2f");
        Cfg()->CfgVisualPrm.LightBrightness =
            std::clamp(Cfg()->CfgVisualPrm.LightBrightness, 0.0, 1.0);
        ImGui::EndDisabled();

        ImGui::TableNextColumn();
        ImGui::AlignTextToFramePadding();
        ImGui::TextUnformatted("Surface elevation mode");
        auto elevMode = "disabled";
        if (Cfg()->CfgVisualPrm.ElevMode == 1) {
            elevMode = "linear interpolation";
        } else if (Cfg()->CfgVisualPrm.ElevMode == 2) {
            elevMode = "cubic interpolation";
        }
        ImGui::TableNextColumn();
        ImGui::SetNextItemWidth(150.0f);
        if (ImGui::BeginCombo("##SurfElev", elevMode)) {
            if (ImGui::Selectable("disabled",
                                  Cfg()->CfgVisualPrm.ElevMode == 0)) {
                Cfg()->CfgVisualPrm.ElevMode = 0;
            }
            if (ImGui::Selectable("linear interpolation",
                                  Cfg()->CfgVisualPrm.ElevMode == 1)) {
                Cfg()->CfgVisualPrm.ElevMode = 1;
            }
            if (ImGui::Selectable("cubic interpolation",
                                  Cfg()->CfgVisualPrm.ElevMode == 2)) {
                Cfg()->CfgVisualPrm.ElevMode = 2;
            }
            ImGui::EndCombo();
        }

        ImGui::TableNextColumn();
        ImGui::AlignTextToFramePadding();
        ImGui::TextUnformatted("Max resolution level (1-21)");
        ImGui::TableNextColumn();
        ImGui::SetNextItemWidth(64.0f);
        ImGui::InputInt("##ResLvl", &Cfg()->CfgVisualPrm.PlanetMaxLevel, 0, 0);
        Cfg()->CfgVisualPrm.PlanetMaxLevel =
            std::clamp(Cfg()->CfgVisualPrm.PlanetMaxLevel, 1, 21);

        ImGui::EndTable();
    }

    ImGui::SeparatorText("General effects");
    if (ImGui::BeginTable("##GeneralEffects", 2,
                          ImGuiTableFlags_SizingStretchProp)) {
        ImGui::TableSetupColumn("##Col1", ImGuiTableColumnFlags_WidthFixed);

        ImGui::TableNextColumn();
        ImGui::Checkbox("Vessel shadows", &Cfg()->CfgVisualPrm.bVesselShadows);
        ImGui::TableNextColumn();
        ImGui::Checkbox("Reentry flames", &Cfg()->CfgVisualPrm.bReentryFlames);

        ImGui::TableNextColumn();
        ImGui::Checkbox("Object shadows", &Cfg()->CfgVisualPrm.bShadows);
        ImGui::TableNextColumn();
        ImGui::Checkbox("Particle streams",
                        &Cfg()->CfgVisualPrm.bParticleStreams);

        ImGui::TableNextColumn();
        ImGui::Checkbox("Specular object reflections",
                        &Cfg()->CfgVisualPrm.bSpecular);
        ImGui::TableNextColumn();
        ImGui::Checkbox("Local light sources",
                        &Cfg()->CfgVisualPrm.bLocalLight);

        ImGui::TableNextColumn();
        ImGui::AlignTextToFramePadding();
        ImGui::TextUnformatted("Ambient light level (0-255)");
        ImGui::TableNextColumn();
        ImGui::SetNextItemWidth(64.0f);
        int val = Cfg()->CfgVisualPrm.AmbientLevel;
        ImGui::InputInt("##AmbLtLvl", &val, 0, 0);
        Cfg()->CfgVisualPrm.AmbientLevel =
            static_cast<uint8_t>(std::clamp(val, 0, 255));

        ImGui::EndTable();
    }

    ImGui::SeparatorText("");
    ImGui::TextWrapped("Some graphics clients may ignore, override or extend "
                       "some of these settings.");
}

OptionsPage_Physics::OptionsPage_Physics(OptionsPageContainer *container)
    : OptionsPage(container, "Physics settings") {}

void OptionsPage_Physics::OnDraw(oapi::WithImCtx &ctx) {
    ImGui::SeparatorText("Perturbations");

    ImGui::Checkbox("Nonspherical gravity sources",
                    &Cfg()->CfgPhysicsPrm.bNonsphericalGrav);
    ImGui::Checkbox("Radiation pressure",
                    &Cfg()->CfgPhysicsPrm.bRadiationPressure);
    ImGui::Checkbox("Gravity-gradient torque",
                    &Cfg()->CfgPhysicsPrm.bDistributedMass);
    ImGui::Checkbox("Atmospheric wind effects", &Cfg()->CfgPhysicsPrm.bAtmWind);
}

// ======================================================================

OptionsPage_Instrument::OptionsPage_Instrument(OptionsPageContainer *container)
    : OptionsPage(container, "Instruments & panels") {}

void OptionsPage_Instrument::OnDraw(oapi::WithImCtx &ctx) {
    ImGui::SeparatorText("Multi-functional displays");

    if (ImGui::BeginTable("##MFD", 2, ImGuiTableFlags_SizingStretchProp)) {
        ImGui::TableSetupColumn("##Col1", ImGuiTableColumnFlags_WidthFixed);

        ImGui::TableNextColumn();
        ImGui::AlignTextToFramePadding();
        ImGui::TextUnformatted("MFD refresh interval [s]:");
        ImGui::TableNextColumn();
        ImGui::SetNextItemWidth(64.0f);
        ImGui::InputDouble("##MFDRefreshInt", &Cfg()->CfgLogicPrm.InstrUpdDT, 0,
                           0, "%.2f");
        Cfg()->CfgLogicPrm.InstrUpdDT =
            std::max(0.01, Cfg()->CfgLogicPrm.InstrUpdDT);

        ImGui::TableNextColumn();
        ImGui::AlignTextToFramePadding();
        ImGui::TextUnformatted("Glass cockpit MFD size:");
        ImGui::TableNextColumn();
        ImGui::SetNextItemWidth(64.0f);
        ImGui::InputInt("##MFDGCSize", &Cfg()->CfgLogicPrm.MFDSize, 0, 0);
        Cfg()->CfgLogicPrm.MFDSize =
            std::clamp(Cfg()->CfgLogicPrm.MFDSize, 1, 10);

        ImGui::TableNextColumn();
        ImGui::Checkbox("Transparent glass cockpit MFD",
                        &Cfg()->CfgLogicPrm.bMfdTransparent);
        ImGui::TableNextColumn();

        ImGui::TableNextColumn();
        ImGui::AlignTextToFramePadding();
        ImGui::TextUnformatted("Virtual cockpit MFD texture size:");
        ImGui::TableNextColumn();
        const char *size = "";
        switch (Cfg()->CfgInstrumentPrm.VCMFDSize) {
        case 256:
            size = "256x256";
            break;
        case 512:
            size = "512x512";
            break;
        case 1024:
            size = "1024x1024";
            break;
        default:
            break;
        }
        ImGui::SetNextItemWidth(128.0f);
        if (ImGui::BeginCombo("##VCMFDSize", size)) {
            if (ImGui::Selectable("256x256",
                                  Cfg()->CfgInstrumentPrm.VCMFDSize == 256)) {
                Cfg()->CfgInstrumentPrm.VCMFDSize = 256;
            }
            if (ImGui::Selectable("512x512",
                                  Cfg()->CfgInstrumentPrm.VCMFDSize == 512)) {
                Cfg()->CfgInstrumentPrm.VCMFDSize = 512;
            }
            if (ImGui::Selectable("1024x1024",
                                  Cfg()->CfgInstrumentPrm.VCMFDSize == 1024)) {
                Cfg()->CfgInstrumentPrm.VCMFDSize = 1024;
            }
            ImGui::EndCombo();
        }

        ImGui::EndTable();
    }

    ImGui::SeparatorText("Instrument panels");
    if (ImGui::BeginTable("##InstPanels", 2,
                          ImGuiTableFlags_SizingStretchProp)) {
        ImGui::TableSetupColumn("##Col1", ImGuiTableColumnFlags_WidthFixed);

        ImGui::TableNextColumn();
        ImGui::AlignTextToFramePadding();
        ImGui::TextUnformatted("Panel scroll speed:");
        ImGui::TableNextColumn();
        ImGui::SetNextItemWidth(64.0f);
        auto val = Cfg()->CfgLogicPrm.PanelScrollSpeed * 0.1;
        ImGui::InputDouble("##ScrollSpeed", &val, 0, 0, "%.2f");
        Cfg()->CfgLogicPrm.PanelScrollSpeed =
            std::clamp(val, -100.0, 100.0) * 10.0;

        ImGui::TableNextColumn();
        ImGui::AlignTextToFramePadding();
        ImGui::TextUnformatted("2D panel scale:");
        ImGui::TableNextColumn();
        ImGui::SetNextItemWidth(64.0f);
        ImGui::InputDouble("##PanelScale", &Cfg()->CfgLogicPrm.PanelScale, 0, 0,
                           "%.2f");
        Cfg()->CfgLogicPrm.PanelScale =
            std::clamp(Cfg()->CfgLogicPrm.PanelScale, 0.25, 4.0);

        ImGui::EndTable();
    }
}

OptionsPage_Vessel::OptionsPage_Vessel(OptionsPageContainer *container)
    : OptionsPage(container, "Vessel settings") {}

void OptionsPage_Vessel::OnDraw(oapi::WithImCtx &ctx) {
    ImGui::Checkbox("Limited fuel", &Cfg()->CfgLogicPrm.bLimitedFuel);
    ImGui::Checkbox("Auto-refuel on pad", &Cfg()->CfgLogicPrm.bPadRefuel);
    auto flightModel = Cfg()->CfgLogicPrm.FlightModelLevel != 0;
    ImGui::Checkbox("Complex flight model", &flightModel);
    Cfg()->CfgLogicPrm.FlightModelLevel = flightModel ? 1 : 0;
    auto damageSetting = Cfg()->CfgLogicPrm.DamageSetting != 0;
    ImGui::Checkbox("Damage and failure simulation", &damageSetting);
    Cfg()->CfgLogicPrm.DamageSetting = damageSetting ? 1 : 0;
}

// ======================================================================

OptionsPage_UI::OptionsPage_UI(OptionsPageContainer *container)
    : OptionsPage(container, "User interface & input") {}

void OptionsPage_UI::OnDraw(oapi::WithImCtx &ctx) {
    const char *strMouseMode[3] = {
        "Focus requires click", "Hybrid: Click required only for child windows",
        "Focus follows mouse"};
    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted("Mouse focus behaviour");
    ImGui::PushItemWidth(350.0f);
    if (ImGui::BeginCombo("##FocusBhv",
                          strMouseMode[Cfg()->CfgUIPrm.MouseFocusMode])) {
        if (ImGui::Selectable(strMouseMode[0],
                              Cfg()->CfgUIPrm.MouseFocusMode == 0)) {
            Cfg()->CfgUIPrm.MouseFocusMode = 0;
        }
        if (ImGui::Selectable(strMouseMode[1],
                              Cfg()->CfgUIPrm.MouseFocusMode == 1)) {
            Cfg()->CfgUIPrm.MouseFocusMode = 1;
        }
        if (ImGui::Selectable(strMouseMode[2],
                              Cfg()->CfgUIPrm.MouseFocusMode == 2)) {
            Cfg()->CfgUIPrm.MouseFocusMode = 2;
        }
        ImGui::EndCombo();
    }
    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted("UI font size:");
    ImGui::SameLine();
    ImGui::PushItemWidth(128.0f);
    ImGui::InputFloat("##UIFontSize", &Cfg()->CfgFontPrm.ImGui_FontSize, 0.5f, 1.0f, "%.1f");
    // These seem like reasonable limits. If anyone complains, we can change 'em I guess.
    Cfg()->CfgFontPrm.ImGui_FontSize = std::clamp(Cfg()->CfgFontPrm.ImGui_FontSize, 1.0f, 200.0f);
    // I tried to get it to work without a restart... but it crashes for mysterious reasons. Feel free to fix, anyone!
    ImGui::SameLine();
    ImGui::TextUnformatted("(This option requires a restart of the Launchpad to take effect)");
}

OptionsPage_Joystick::OptionsPage_Joystick(OptionsPageContainer *container)
    : OptionsPage(container, "Joystick"), m_njoy(0) {
    m_joylist = SDL_GetJoysticks(&m_njoy);
}

OptionsPage_Joystick::~OptionsPage_Joystick() {
    if (m_joylist) {
        SDL_free(m_joylist);
    }
}

void OptionsPage_Joystick::OnDraw(oapi::WithImCtx &ctx) {
    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted("Joystick device");

    ImGui::SameLine();
    // TODO: profile this and see if realtime enumeration actually hurts
    if (ImGui::Button("Refresh joystick list")) {
        if (m_joylist)
            SDL_free(m_joylist);
        m_njoy = 0;
        m_joylist = SDL_GetJoysticks(&m_njoy);
    }

    auto selJoyName = SDL_GetJoystickNameForID(Cfg()->CfgJoystickPrm.Joy_idx);
    if (Cfg()->CfgJoystickPrm.Joy_idx == 0) {
        selJoyName = "<Disabled>";
    } else if (selJoyName == nullptr) {
        selJoyName = "<Could not find selected joystick>";
    }
    if (ImGui::BeginCombo("##Joylist", selJoyName)) {
        if (ImGui::Selectable("<Disabled>",
                              Cfg()->CfgJoystickPrm.Joy_idx == 0)) {
            Cfg()->CfgJoystickPrm.Joy_idx = 0;
        }
        for (int i = 0; i < m_njoy; i++) {
            auto joyName = SDL_GetJoystickNameForID(m_joylist[i]);
            if (joyName == nullptr) {
                joyName = "<Could not get joystick name>";
            }
            if (ImGui::Selectable(joyName, Cfg()->CfgJoystickPrm.Joy_idx ==
                                               m_joylist[i])) {
                Cfg()->CfgJoystickPrm.Joy_idx = m_joylist[i];
            }
        }
        ImGui::EndCombo();
    }

    ImGui::TextUnformatted("TODO - new input system WIP");
}

OptionsPage_CelSphere::OptionsPage_CelSphere(OptionsPageContainer *container)
    : OptionsPage(container, "Celestial sphere") {
    PopulateStarmapList();
    PopulateBgImageList();
}

static bool operator!=(const StarRenderPrm &lhs, const StarRenderPrm &rhs) {
    return lhs.brt_min != rhs.brt_min || lhs.mag_hi != rhs.mag_hi ||
           lhs.mag_lo != rhs.mag_lo || lhs.map_log != rhs.map_log;
}

void OptionsPage_CelSphere::OnDraw(oapi::WithImCtx &ctx) {
    ImGui::Separator();
    bool changed = ImGui::Checkbox("Background stars: show as pixels",
                                   &Cfg()->CfgVisualPrm.bUseStarDots);
    if (changed) {
        g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE,
                                    OPTITEM_CELSPHERE_ACTIVATESTARDOTS);
    }

    auto &starPrm = Cfg()->CfgVisualPrm.StarPrm;
    const auto oldPrm = starPrm;
    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted("Map app. magnitude");
    ImGui::SameLine();
    ImGui::SetNextItemWidth(48.0f);
    ImGui::InputDouble("##AppMagMax", &starPrm.mag_hi, 0, 0, "%.1f");
    starPrm.mag_hi = std::clamp(starPrm.mag_hi, -2.0, starPrm.mag_lo);
    ImGui::SameLine();
    ImGui::TextUnformatted("to max. brightness 1");

    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted("Map app. magnitude");
    ImGui::SameLine();
    ImGui::SetNextItemWidth(48.0);
    ImGui::InputDouble("##AppMagMin", &starPrm.mag_lo, 0, 0, "%.1f");
    starPrm.mag_lo = std::clamp(starPrm.mag_lo, starPrm.mag_hi, 15.0);
    ImGui::SameLine();
    ImGui::TextUnformatted("to min. brightness");
    ImGui::SameLine();
    ImGui::SetNextItemWidth(48.0);
    ImGui::InputDouble("##BrtMin", &starPrm.brt_min, 0, 0, "%.2f");
    starPrm.brt_min = std::clamp(starPrm.brt_min, 0.0, 1.0);

    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted("Magnitude-brightness mapping:");
    ImGui::SameLine();
    auto mapping = starPrm.map_log ? 1 : 0;
    ImGui::RadioButton("linear", &mapping, 0);
    ImGui::SameLine();
    ImGui::RadioButton("exponential", &mapping, 1);
    starPrm.map_log = mapping == 1;

    if (starPrm != oldPrm) {
        g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE,
                                    OPTITEM_CELSPHERE_STARDISPLAYPARAM);
    }

    ImGui::Separator();
    changed = ImGui::Checkbox("Background stars: show as image",
                              &Cfg()->CfgVisualPrm.bUseStarImage);
    if (changed) {
        g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE,
                                    OPTITEM_CELSPHERE_ACTIVATESTARIMAGE);
    }
    auto starpath = Cfg()->CfgVisualPrm.StarImagePath.u8string();
    const auto starpathNameIt =
        std::find_if(m_pathStarmap.cbegin(), m_pathStarmap.cend(),
                     [starpath](auto &p) { return p.second == starpath; });
    auto starpathName = std::string("<No starmap images installed>");
    if (starpathNameIt != m_pathStarmap.cend()) {
        starpathName = starpathNameIt->first;
    }
    ImGui::SetNextItemWidth(300.0f);
    if (ImGui::BeginCombo("##Starmap", starpathName.c_str())) {
        for (const auto &[name, path] : m_pathStarmap) {
            if (ImGui::Selectable(name.c_str(), starpath == path)) {
                Cfg()->CfgVisualPrm.StarImagePath = path;
                g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE,
                                            OPTITEM_CELSPHERE_STARIMAGECHANGED);
            }
        }
        ImGui::EndCombo();
    }
    ImGui::Separator();
    changed =
        ImGui::Checkbox("Background map", &Cfg()->CfgVisualPrm.bUseBgImage);
    if (changed) {
        g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE,
                                    OPTITEM_CELSPHERE_ACTIVATEBGIMAGE);
    }
    auto bgpath = Cfg()->CfgVisualPrm.CSphereBgPath.u8string();
    const auto bgpathNameIt =
        std::find_if(m_pathBgImage.cbegin(), m_pathBgImage.cend(),
                     [bgpath](auto &p) { return p.second == bgpath; });
    auto bgpathName = std::string("<No background images installed>");
    if (bgpathNameIt != m_pathBgImage.cend()) {
        bgpathName = bgpathNameIt->first;
    }
    ImGui::SetNextItemWidth(300.0f);
    if (ImGui::BeginCombo("##BgMap", bgpathName.c_str())) {
        for (const auto &[name, path] : m_pathBgImage) {
            if (ImGui::Selectable(name.c_str(), bgpathName == path)) {
                Cfg()->CfgVisualPrm.CSphereBgPath = path;
                g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE,
                                            OPTITEM_CELSPHERE_BGIMAGECHANGED);
            }
        }
        ImGui::EndCombo();
    }
    constexpr auto brtMin = 0.0;
    constexpr auto brtMax = 100.0;
    auto brt = Cfg()->CfgVisualPrm.CSphereBgIntens;
    ImGui::SetNextItemWidth(300.0f);
    changed = ImGui::SliderScalar("Brightness", ImGuiDataType_Double, &brt,
                                  &brtMin, &brtMax, "%.1f");
    Cfg()->CfgVisualPrm.CSphereBgIntens = std::clamp(brt, 0.0, 100.0);
    if (changed) {
        g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE,
                                    OPTITEM_CELSPHERE_BGIMAGEBRIGHTNESS);
    }
}

void OptionsPage_CelSphere::PopulateStarmapList() {
    m_pathStarmap.clear();

    if (std::ifstream ifs(Cfg()->ConfigPath("CSphere/bkgimage")); ifs) {
        std::string line;
        bool found = false;
        while (std::getline(ifs, line)) {
            if (!found) {
                if (line.find("BEGIN_STARMAPS") != std::string::npos) {
                    found = true;
                }
                continue;
            }
            if (line.find("END_STARMAPS") != std::string::npos) {
                break;
            }
            if (const auto ix = line.find_first_of('|');
                ix != std::string::npos) {
                std::string name = line.substr(0, ix);
                std::string path = line.substr(ix + 1);
                m_pathStarmap.push_back(std::move(
                    std::make_pair(std::move(name), std::move(path))));
            }
        }
    }
}

void OptionsPage_CelSphere::PopulateBgImageList() {
    m_pathBgImage.clear();

    if (std::ifstream ifs(Cfg()->ConfigPath("CSphere/bkgimage")); ifs) {
        std::string line;
        bool found = false;
        while (std::getline(ifs, line)) {
            if (!found) {
                if (line.find("BEGIN_BACKGROUNDS") != std::string::npos) {
                    found = true;
                }
                continue;
            }
            if (line.find("BEGIN_BACKGROUNDS") != std::string::npos) {
                break;
            }
            if (const auto ix = line.find_first_of('|');
                ix != std::string::npos) {
                std::string name = line.substr(0, ix);
                std::string path = line.substr(ix + 1);
                m_pathBgImage.push_back(std::move(
                    std::make_pair(std::move(name), std::move(path))));
            }
        }
    }
}

OptionsPage_VisHelper::OptionsPage_VisHelper(OptionsPageContainer *container)
    : OptionsPage(container, "Visual helpers") {}

void OptionsPage_VisHelper::OnDraw(oapi::WithImCtx &ctx) {
    ImGui::TextWrapped(
        "Options for displaying markers and gridlines on the celestial sphere, "
        "labels for planetary surface features, as well as coordinate axes and "
        "force vectors.");
    ImGui::Separator();
    ImGui::NewLine();

    if (ImGui::BeginTable("##Table", 2, ImGuiTableFlags_SizingStretchProp)) {
        ImGui::TableSetupColumn("##Col1", ImGuiTableColumnFlags_WidthFixed);

        ImGui::TableNextColumn();
        auto plnEnable =
            (Cfg()->CfgVisHelpPrm.flagPlanetarium & PLN_ENABLE) != 0;
        ImGui::Checkbox("Planetarium mode (F9)", &plnEnable);
        if (plnEnable)
            Cfg()->CfgVisHelpPrm.flagPlanetarium |= PLN_ENABLE;
        else
            Cfg()->CfgVisHelpPrm.flagPlanetarium &= ~PLN_ENABLE;
        ImGui::TableNextColumn();
        if (ImGui::Button("Planetarium options", ImVec2(150.0f, 0.0f))) {
            Container()->SwitchPage("Planetarium");
        }

        ImGui::TableNextColumn();
        auto mkrEnable = (Cfg()->CfgVisHelpPrm.flagMarkers & MKR_ENABLE) != 0;
        ImGui::Checkbox("Surface/object labels (Alt-F9)", &mkrEnable);
        if (mkrEnable)
            Cfg()->CfgVisHelpPrm.flagMarkers |= MKR_ENABLE;
        else
            Cfg()->CfgVisHelpPrm.flagMarkers &= ~MKR_ENABLE;
        ImGui::TableNextColumn();
        if (ImGui::Button("Label options", ImVec2(150.0f, 0.0f))) {
            Container()->SwitchPage("Labels");
        }

        ImGui::TableNextColumn();
        auto bodyVecEnable =
            (Cfg()->CfgVisHelpPrm.flagBodyForce & BFV_ENABLE) != 0;
        ImGui::Checkbox("Body force vectors", &bodyVecEnable);
        if (bodyVecEnable)
            Cfg()->CfgVisHelpPrm.flagBodyForce |= BFV_ENABLE;
        else
            Cfg()->CfgVisHelpPrm.flagBodyForce &= ~BFV_ENABLE;
        ImGui::TableNextColumn();
        if (ImGui::Button("Body force options", ImVec2(150.0f, 0.0f))) {
            Container()->SwitchPage("Body forces");
        }

        ImGui::TableNextColumn();
        auto frameAxesEnable =
            (Cfg()->CfgVisHelpPrm.flagFrameAxes & FAV_ENABLE) != 0;
        ImGui::Checkbox("Object frame axes", &frameAxesEnable);
        if (frameAxesEnable)
            Cfg()->CfgVisHelpPrm.flagFrameAxes |= FAV_ENABLE;
        else
            Cfg()->CfgVisHelpPrm.flagFrameAxes &= ~FAV_ENABLE;
        ImGui::TableNextColumn();
        if (ImGui::Button("Frame axis options", ImVec2(150.0f, 0.0f))) {
            Container()->SwitchPage("Object axes");
        }

        ImGui::EndTable();
    }
}

static void SetOrClearFlag(bool enabled, DWORD &val, const DWORD flag) {
    if (enabled)
        val |= flag;
    else
        val &= ~flag;
}

OptionsPage_Planetarium::OptionsPage_Planetarium(
    OptionsPageContainer *container)
    : OptionsPage(container, "Planetarium") {}

void OptionsPage_Planetarium::OnDraw(oapi::WithImCtx &ctx) {
    auto &flag = Cfg()->CfgVisHelpPrm.flagPlanetarium;
    const auto oldFlag = flag;
    auto plnEnable = (flag & PLN_ENABLE) != 0;
    ImGui::Checkbox("Planetarium mode (F9)", &plnEnable);
    SetOrClearFlag(plnEnable, flag, PLN_ENABLE);
    if (!plnEnable) {
        ImGui::BeginDisabled();
    }

    ImGui::SeparatorText("Grids and circles");
    auto horizGrid = (flag & PLN_HGRID) != 0;
    auto celGrid = (flag & PLN_CGRID) != 0;
    auto eclGrid = (flag & PLN_EGRID) != 0;
    auto galGrid = (flag & PLN_GGRID) != 0;
    auto tgtEquator = (flag & PLN_EQU) != 0;
    ImGui::Checkbox("Local horizon grid", &horizGrid);
    ImGui::Checkbox("Celestial grid", &celGrid);
    ImGui::Checkbox("Ecliptic grid", &eclGrid);
    ImGui::Checkbox("Galactic grid", &galGrid);
    ImGui::Checkbox("Target equator", &tgtEquator);
    SetOrClearFlag(horizGrid, flag, PLN_HGRID);
    SetOrClearFlag(celGrid, flag, PLN_CGRID);
    SetOrClearFlag(eclGrid, flag, PLN_EGRID);
    SetOrClearFlag(galGrid, flag, PLN_GGRID);
    SetOrClearFlag(tgtEquator, flag, PLN_EQU);

    ImGui::SeparatorText("Constellations");
    auto labels = (flag & PLN_CNSTLABEL) != 0;
    ImGui::Checkbox("Labels", &labels);
    SetOrClearFlag(labels, flag, PLN_CNSTLABEL);
    if (!labels)
        ImGui::BeginDisabled();
    ImGui::SameLine();
    if (ImGui::RadioButton("Full", (flag & PLN_CNSTLONG) != 0)) {
        flag |= PLN_CNSTLONG;
    }
    ImGui::SameLine();
    if (ImGui::RadioButton("3-letter", (flag & PLN_CNSTLONG) == 0)) {
        flag &= ~PLN_CNSTLONG;
    }
    if (!labels)
        ImGui::EndDisabled();

    auto boundaries = (flag & PLN_CNSTBND) != 0;
    ImGui::Checkbox("Boundaries", &boundaries);
    SetOrClearFlag(boundaries, flag, PLN_CNSTBND);
    auto patterns = (flag & PLN_CONST) != 0;
    ImGui::Checkbox("Patterns", &patterns);
    SetOrClearFlag(patterns, flag, PLN_CONST);

    ImGui::SeparatorText("Celestial markers");
    auto markers = (flag & PLN_CCMARK) != 0;
    ImGui::Checkbox("Show markers", &markers);
    SetOrClearFlag(markers, flag, PLN_CCMARK);
    if (!markers)
        ImGui::BeginDisabled();
    ImGui::BeginChild("##MarkerList", ImVec2(0, 0), ImGuiChildFlags_Borders);
    if (g_psys) {
        for (auto &entry : g_psys->LabelList()) {
            if (ImGui::Selectable(entry.name.c_str(), entry.active)) {
                entry.active = true;
                std::ifstream fcfg(Cfg()->ConfigPath(g_psys->Name()));
                g_psys->ScanLabelLists(fcfg);
            }
        }
    }

    ImGui::EndChild();
    if (!markers)
        ImGui::EndDisabled();

    if (!plnEnable)
        ImGui::EndDisabled();

    if (oldFlag != flag) {
        g_pOrbiter->OnOptionChanged(OPTCAT_PLANETARIUM,
                                    OPTITEM_PLANETARIUM_DISPFLAG);
    }
}

OptionsPage_Labels::OptionsPage_Labels(OptionsPageContainer *container)
    : OptionsPage(container, "Labels") {}

void OptionsPage_Labels::OnDraw(oapi::WithImCtx &ctx) {
    auto &flag = Cfg()->CfgVisHelpPrm.flagMarkers;
    const auto oldFlag = flag;
    auto mkrEnable = (flag & MKR_ENABLE) != 0;
    ImGui::Checkbox("Surface and object labels (Alt-F9)", &mkrEnable);
    SetOrClearFlag(mkrEnable, flag, MKR_ENABLE);
    if (!mkrEnable)
        ImGui::BeginDisabled();

    ImGui::SeparatorText("Object markers");
    auto vessels = (flag & MKR_VMARK) != 0;
    auto celbody = (flag & MKR_CMARK) != 0;
    auto surfbase = (flag & MKR_BMARK) != 0;
    auto vortx = (flag & MKR_RMARK) != 0;
    ImGui::Checkbox("Vessels", &vessels);
    ImGui::Checkbox("Celestial bodies", &celbody);
    ImGui::Checkbox("Surface bases", &surfbase);
    ImGui::Checkbox("VOR transmitters", &vortx);
    SetOrClearFlag(vessels, flag, MKR_VMARK);
    SetOrClearFlag(celbody, flag, MKR_CMARK);
    SetOrClearFlag(surfbase, flag, MKR_BMARK);
    SetOrClearFlag(vortx, flag, MKR_RMARK);

    ImGui::SeparatorText("Surface features");
    // FIXME FIXME FIXME: wait till I'm done with getting all the render window
    // and GC shit done so I can actually implement this

    if (!mkrEnable)
        ImGui::EndDisabled();
}

OptionsPage_Forces::OptionsPage_Forces(OptionsPageContainer *container)
    : OptionsPage(container, "Body forces") {}

void OptionsPage_Forces::OnDraw(oapi::WithImCtx &ctx) {
    auto &flag = Cfg()->CfgVisHelpPrm.flagBodyForce;
    const auto oldFlag = flag;
    auto bfvEnable = (flag & BFV_ENABLE) != 0;
    ImGui::Checkbox("Body force vectors", &bfvEnable);
    SetOrClearFlag(bfvEnable, flag, BFV_ENABLE);
    if (!bfvEnable)
        ImGui::BeginDisabled();

    ImGui::SeparatorText("Linear forces");
    auto weight = (flag & BFV_WEIGHT) != 0;
    auto lift = (flag & BFV_LIFT) != 0;
    auto thrust = (flag & BFV_THRUST) != 0;
    auto drag = (flag & BFV_DRAG) != 0;
    auto side = (flag & BFV_SIDEFORCE) != 0;
    auto total = (flag & BFV_TOTAL) != 0;
    ImGui::Checkbox("Weight", &weight);
    ImGui::Checkbox("Lift", &lift);
    ImGui::Checkbox("Thrust", &thrust);
    ImGui::Checkbox("Drag", &drag);
    ImGui::Checkbox("Side force", &side);
    ImGui::Checkbox("Total", &total);
    SetOrClearFlag(weight, flag, BFV_WEIGHT);
    SetOrClearFlag(lift, flag, BFV_LIFT);
    SetOrClearFlag(thrust, flag, BFV_THRUST);
    SetOrClearFlag(drag, flag, BFV_DRAG);
    SetOrClearFlag(side, flag, BFV_SIDEFORCE);
    SetOrClearFlag(total, flag, BFV_TOTAL);

    ImGui::SeparatorText("Angular moments");
    auto torque = (flag & BFV_TORQUE) != 0;
    ImGui::Checkbox("Torque", &torque);
    SetOrClearFlag(torque, flag, BFV_TORQUE);

    ImGui::SeparatorText("Display");

    ImGui::AlignTextToFramePadding();
    ImGui::TextUnformatted("Scale:");
    ImGui::SameLine();
    if (ImGui::RadioButton("Linear", (flag & BFV_LOGSCALE) == 0)) {
        flag &= ~BFV_LOGSCALE;
    }
    ImGui::SameLine();
    if (ImGui::RadioButton("Logarithmic", (flag & BFV_LOGSCALE) != 0)) {
        flag |= BFV_LOGSCALE;
    }

    auto scalePos =
        25.0f * (1.0f + 0.5f * std::log2(Cfg()->CfgVisHelpPrm.scaleBodyForce));
    ImGui::SetNextItemWidth(300.0f);
    ImGui::SliderFloat("##Scale", &scalePos, 0.0f, 50.0f, "");
    Cfg()->CfgVisHelpPrm.scaleBodyForce =
        std::pow(2.0f, 0.08f * (scalePos - 25.0f));

    ImGui::TextUnformatted("Opacity:");
    auto opacPos = Cfg()->CfgVisHelpPrm.opacBodyForce * 50.0f;
    ImGui::SetNextItemWidth(300.0f);
    ImGui::SliderFloat("##OpacPos", &opacPos, 0.0f, 50.0f, "");
    Cfg()->CfgVisHelpPrm.opacBodyForce = opacPos * 0.02f;

    if (!bfvEnable)
        ImGui::EndDisabled();
}

OptionsPage_Axes::OptionsPage_Axes(OptionsPageContainer *container)
    : OptionsPage(container, "Object axes") {}

void OptionsPage_Axes::OnDraw(oapi::WithImCtx &ctx) {
    auto &flag = Cfg()->CfgVisHelpPrm.flagFrameAxes;
    const auto oldFlag = flag;
    auto favEnable = (flag & FAV_ENABLE) != 0;
    ImGui::Checkbox("Object frame axes", &favEnable);
    SetOrClearFlag(favEnable, flag, FAV_ENABLE);
    if (!favEnable)
        ImGui::BeginDisabled();

    ImGui::SeparatorText("Objects");
    auto vessels = (flag & FAV_VESSEL) != 0;
    auto celbody = (flag & FAV_CELBODY) != 0;
    auto surfbase = (flag & FAV_BASE) != 0;
    ImGui::Checkbox("Vessels", &vessels);
    ImGui::Checkbox("Celestial bodies", &celbody);
    ImGui::Checkbox("Surface bases", &surfbase);
    SetOrClearFlag(vessels, flag, FAV_VESSEL);
    SetOrClearFlag(celbody, flag, FAV_CELBODY);
    SetOrClearFlag(surfbase, flag, FAV_BASE);

    ImGui::SeparatorText("Display");
    auto negAxes = (flag & FAV_NEGATIVE) != 0;
    ImGui::Checkbox("Show negative axes", &negAxes);
    SetOrClearFlag(negAxes, flag, FAV_NEGATIVE);

    ImGui::TextUnformatted("Scale:");
    auto scalePos =
        25.0f * (1.0f + 0.5f * std::log2(Cfg()->CfgVisHelpPrm.scaleFrameAxes));
    ImGui::SetNextItemWidth(300.0f);
    ImGui::SliderFloat("##Scale", &scalePos, 0.0f, 50.0f, "");
    Cfg()->CfgVisHelpPrm.scaleFrameAxes =
        std::pow(2.0f, 0.08f * (scalePos - 25.0f));

    ImGui::TextUnformatted("Opacity:");
    auto opacPos = Cfg()->CfgVisHelpPrm.opacFrameAxes * 50.0f;
    ImGui::SetNextItemWidth(300.0f);
    ImGui::SliderFloat("##OpacPos", &opacPos, 0.0f, 50.0f, "");
    Cfg()->CfgVisHelpPrm.opacFrameAxes = opacPos * 0.02f;

    if (!favEnable)
        ImGui::EndDisabled();
}
