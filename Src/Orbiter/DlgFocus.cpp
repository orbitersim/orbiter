// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Focus vessel selection dialog
// ======================================================================

#include "DlgFocus.h"
#include "Psys.h"
#include "OrbiterAPI.h"
#include "Orbiter.h"
#include "Camera.h"
#include "imgui.h"
#include <map>
#include "IconsFontAwesome6.h"

extern Camera *g_camera;
extern PlanetarySystem *g_psys;
extern Orbiter *g_pOrbiter;
extern Vessel *g_focusobj, *g_pfocusobj;

DlgFocus::DlgFocus() : ImGuiDialog(ICON_FA_ROCKET " Orbiter: Select spacecraft", {300, 320}) {
	SetHelp("html/orbiter.chm", "/selvessel.htm");
}

void DlgFocus::OnDraw() {
    const char *tabs[] = {
        "All", "Nearby", "Location", "Class"
    };

    void (DlgFocus::* func[])() = {
        &DlgFocus::DrawAll, &DlgFocus::DrawNearby, &DlgFocus::DrawLocation, &DlgFocus::DrawClass
    };

    ImGuiTabBarFlags tab_bar_flags = ImGuiTabBarFlags_None;
    if (ImGui::BeginTabBar("FocusTabBar", tab_bar_flags))
    {
        for(size_t i = 0; i<sizeof(tabs)/sizeof(tabs[0]);i++) {
            if(ImGui::BeginTabItem(tabs[i])) {
                (this->*func[i])();
                ImGui::EndTabItem();
            }
        }
        ImGui::EndTabBar();
    }
}

void DlgFocus::DrawAll() {
    ImGuiWindowFlags window_flags =  ImGuiChildFlags_ResizeX;
    ImGui::Text("Spacecraft : %s", m_SelectedShip.c_str());
    ImGui::BeginChild("ChildL", ImVec2(150, 0), true, window_flags);

    for (int i = 0; i < g_psys->nVessel(); i++) {
        Vessel *vessel = g_psys->GetVessel(i);
        if (vessel->GetEnableFocus()) {
            std::string name = vessel->Name();
            const bool is_selected = m_SelectedShip == name;
            ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
            if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
            ImGui::TreeNodeEx(vessel->Name(), node_flags);
            if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen()) {
                m_SelectedShip = vessel->Name();
            }
        }
	}

    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
    ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
    if(ImGui::Button("Select", button_sz)) {
        Vessel *vessel = g_psys->GetVessel (m_SelectedShip.c_str(), true);
        if (vessel) {
            g_pOrbiter->SetFocusObject (vessel);
        }
    }
    if(ImGui::Button("Previous", button_sz)) {
        if (g_pfocusobj) {
            g_pOrbiter->SetFocusObject (g_pfocusobj);
			m_SelectedShip = g_pfocusobj->Name();
        }
    }
    ImGui::EndChild();
}
void DlgFocus::DrawNearby() {
    ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;
    ImGui::Text("Spacecraft : %s", m_SelectedShip.c_str());
    ImGui::BeginChild("ChildL", ImVec2(150, 0), true, window_flags);

    const Vector &campos = g_camera->GPos();
    for (int i = 0; i < g_psys->nVessel(); i++) {
        Vessel *vessel = g_psys->GetVessel(i);
        if (vessel->GetEnableFocus()) {
            double dst = campos.dist (vessel->GPos());
			if (dst <= m_Range * 1000.0f) {
                std::string name = vessel->Name();
                const bool is_selected = m_SelectedShip == name;
                ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
                if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
                char cbuf[128];
                sprintf(cbuf,"%s (%.1fkm)", vessel->Name(), dst/1000.0f);
                ImGui::TreeNodeEx(cbuf, node_flags);
                if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen()) {
                    m_SelectedShip = vessel->Name();
                }
            }
        }
	}

    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
    ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
    if(ImGui::Button("Select", button_sz)) {
        Vessel *vessel = g_psys->GetVessel (m_SelectedShip.c_str(), true);
        if (vessel) {
            g_pOrbiter->SetFocusObject (vessel);
        }
    }
    if(ImGui::Button("Previous", button_sz)) {
        if (g_pfocusobj) {
            g_pOrbiter->SetFocusObject (g_pfocusobj);
			m_SelectedShip = g_pfocusobj->Name();
        }
    }

    ImGui::SetNextItemWidth(-FLT_MIN);
    if(ImGui::SliderFloat("##slider warp", &m_Range, 1.0f, 1000000.0f, "%.1fkm", ImGuiSliderFlags_Logarithmic)) {
    }

    ImGui::EndChild();
}
void DlgFocus::DrawLocation() {
    std::map<const char *, std::vector<Vessel *>> vesselMap; 
	for (int i = 0; i < g_psys->nVessel(); i++) {
		Vessel *vessel = g_psys->GetVessel(i);
		if (vessel->GetEnableFocus()) {
			const CelestialBody *ref = vessel->ElRef();
            vesselMap[ref->Name()].push_back(vessel);
        }
    }

    ImGuiWindowFlags window_flags =  ImGuiChildFlags_ResizeX;
    ImGui::Text("Spacecraft : %s", m_SelectedShip.c_str());
    ImGui::BeginChild("ChildL", ImVec2(150,0), true, window_flags);

    for(auto &kw : vesselMap) {
        if(ImGui::TreeNodeEx(kw.first)) {
            for(auto &vessel: kw.second) {
                const bool is_selected = m_SelectedShip == vessel->Name();
                ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
                if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
                ImGui::TreeNodeEx(vessel->Name(), node_flags);
                if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen()) {
                    m_SelectedShip = vessel->Name();
                }
            }

            ImGui::TreePop();
        }
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
    ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
    if(ImGui::Button("Select", button_sz)) {
        Vessel *vessel = g_psys->GetVessel (m_SelectedShip.c_str(), true);
        if (vessel) {
            g_pOrbiter->SetFocusObject (vessel);
        }
    }
    if(ImGui::Button("Previous", button_sz)) {
        if (g_pfocusobj) {
            g_pOrbiter->SetFocusObject (g_pfocusobj);
			m_SelectedShip = g_pfocusobj->Name();
        }
    }

    ImGui::EndChild();
}
void DlgFocus::DrawClass() {
    std::map<std::string, std::vector<Vessel *>> vesselMap; 
	for (int i = 0; i < g_psys->nVessel(); i++) {
		Vessel *vessel = g_psys->GetVessel(i);
		if (vessel->GetEnableFocus()) {
            vesselMap[vessel->ClassName()].push_back(vessel);
        }
    }

    ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;
    ImGui::Text("Spacecraft : %s", m_SelectedShip.c_str());
    ImGui::BeginChild("ChildL", ImVec2(150,0), true, window_flags);

    for(auto &kw : vesselMap) {
        if(ImGui::TreeNodeEx(kw.first.c_str())) {
            for(auto &vessel: kw.second) {
                const bool is_selected = m_SelectedShip == vessel->Name();
                ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
                if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
                ImGui::TreeNodeEx(vessel->Name(), node_flags);
                if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen()) {
                    m_SelectedShip = vessel->Name();
                }
            }

            ImGui::TreePop();
        }
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
    ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
    if(ImGui::Button("Select", button_sz)) {
        Vessel *vessel = g_psys->GetVessel (m_SelectedShip.c_str(), true);
        if (vessel) {
            g_pOrbiter->SetFocusObject (vessel);
        }
    }
    if(ImGui::Button("Previous", button_sz)) {
        if (g_pfocusobj) {
            g_pOrbiter->SetFocusObject (g_pfocusobj);
			m_SelectedShip = g_pfocusobj->Name();
        }
    }

    ImGui::EndChild();
}
