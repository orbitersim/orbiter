// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// "Map" dialog
// ======================================================================
#include "DlgMap.h"
#include "Orbiter.h"
#include "Psys.h"
#include "Celbody.h"
#include "Psys.h"
#include "imgui.h"
#include "IconsFontAwesome6.h"
#include "DlgInfo.h"

extern TimeData td;

extern PlanetarySystem *g_psys;

DlgMap::DlgMap() : ImGuiDialog(ICON_FA_GLOBE " Orbiter: Map", {640, 400}) {
    planet = "Select...";
	enableInfo = false;
	searchbuf[0] = '\0';
	selectionfilter = DISP_MOON | DISP_VESSEL | DISP_BASE;

	prm = &g_pOrbiter->Cfg()->CfgMapPrm;
}
void DlgMap::Display() {
    ImGui::SetNextWindowSize(ImVec2(defaultSize.width, defaultSize.height), ImGuiCond_FirstUseEver);
	
	bool visible = ImGui::Begin(name.c_str(), &active);
	if(ImGui::MenuButton(ICON_FA_ARROW_ROTATE_LEFT, "Reset map")) {
		Reset();
	}
	
	if(visible) {
		OnDraw();
	}
	ImGui::End();
	if (!active) OnClose();
}

void DlgMap::Reset()
{
    zoom=1.0;
    vectormap->SetZoom(zoom);
    vectormap->SetCenter(0.0,0.0);
	vectormap->UnsetSelection();
    vectormap->Update();
    vectormap->DrawMap ();
	searchbuf[0] = '\0';
	enableInfo = false;
	selectionfilter = DISP_MOON | DISP_VESSEL | DISP_BASE;
}

void DlgMap::AddCbodyNode(const CelestialBody *cbody) {
    ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth;
    const bool is_selected = planet == cbody->Name();
    if (is_selected)
        node_flags |= ImGuiTreeNodeFlags_Selected;

    if(cbody->nSecondary()) {
        if(!strcmp(cbody->Name(), "Sun"))
            node_flags|=ImGuiTreeNodeFlags_DefaultOpen;

        bool node_open = ImGui::TreeNodeEx(cbody->Name(), node_flags);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen()) {
            planet = cbody->Name();
            SetBody(cbody->Name());
            ImGui::CloseCurrentPopup();
        }
        if(node_open) {
            for (int i = 0; i < cbody->nSecondary(); i++) {
                AddCbodyNode (cbody->Secondary(i));
            }
            ImGui::TreePop();
        }
    } else {
        ImGui::TreeNodeEx(cbody->Name(), node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen()) {
            planet = cbody->Name();
            SetBody(cbody->Name());
            ImGui::CloseCurrentPopup();
        }
    }
}

void DlgMap::DrawTree() {
    for (int i = 0; i < g_psys->nStar(); i++)
        AddCbodyNode (g_psys->GetStar(i));
}


void DlgMap::SetBody(const char *body) {
    planet = body;
    if(vectormap) {
        Planet *b = g_psys->GetPlanet (body);
        if(b) {
            vectormap->SetCBody(b);
            vectormap->Update();
            vectormap->DrawMap ();

       		for (int i = 0; i < vectormap->GetCustomMarkerSet().nset; i++)
			    vectormap->GetCustomMarkerSet().set[i].active = true;

        }
    }
}

bool DlgMap::SetSelection(const char *name)
{
	if (!name[0]) return false; // sanity check

	const int maxhit = 15;
	const char *hitstr[maxhit];
	int nhit = 0, len = strlen(name);
	DWORD i;
	VectorMap::OBJTYPE sel;
	bool found_exact = false;

	if ((selectionfilter & DISP_VESSEL) && (vectormap->GetDisplayFlags() & DISP_VESSEL)) { // search for vessel
		for (i = 0; i < g_psys->nVessel(); i++) {
			Vessel *v = g_psys->GetVessel(i);
			if (!_strnicmp (v->Name(), name, len)) {
				if (nhit < maxhit) hitstr[nhit] = v->Name();
				nhit++;
				if (!found_exact && !_stricmp (v->Name(), name)) {
					sel.type = DISP_VESSEL;
					sel.obj = v;
					found_exact = true;
				}
			}
		}
	}
	if ((selectionfilter & DISP_BASE) && (vectormap->GetDisplayFlags() & DISP_BASE)) { // search for bases
		const Planet *planet = vectormap->GetPlanet();
		if (planet) {
			for (i = 0; i < planet->nBase(); i++) {
				const Base *base = planet->GetBase(i);
				if (!_strnicmp (base->Name(), name, len)) {
					if (nhit < maxhit) hitstr[nhit] = base->Name();
					nhit++;
					if (!found_exact && !_stricmp (base->Name(), name)) {
						sel.type = DISP_BASE;
						sel.obj = base;
						found_exact = true;
					}
				}
			}
		}
	}
	if ((selectionfilter & DISP_NAVAID) && vectormap->GetDisplayFlags() & DISP_NAVAID) { // search for VOR transmitters
		const Planet *planet = vectormap->GetPlanet();
		if (planet) {
			for (i = 0; i < planet->nNav(); i++) {
				const Nav *nav = planet->NavMgr().GetNav(i);
				if (nav->Type() == TRANSMITTER_VOR) {
					const Nav_VOR *vor = (const Nav_VOR*)nav;
					if (!_strnicmp (vor->GetId(), name, len)) {
						if (nhit < maxhit) hitstr[nhit] = vor->GetId();
						nhit++;
						if (!found_exact && !_stricmp (vor->GetId(), name)) {
							sel.type = DISP_NAVAID;
							sel.obj = vor;
							found_exact = true;
						}
					}
				}
			}
		}
	}
	if ((selectionfilter & DISP_MOON) && vectormap->GetDisplayFlags() & DISP_MOON) { // search for moons
		for (i = 0; i < vectormap->GetCBody()->nSecondary(); i++) {
			const CelestialBody *moon = vectormap->GetCBody()->Secondary (i);
			if (!_strnicmp (moon->Name(), name, len)) {
				if (nhit < maxhit) hitstr[nhit] = moon->Name();
				nhit++;
				if (!found_exact && !_stricmp (moon->Name(), name)) {
					sel.type = DISP_MOON;
					sel.obj = moon;
					found_exact = true;
				}
			}
		}
	}

	if (found_exact) {
		vectormap->SetSelection (sel);
		enableInfo = (sel.type == DISP_BASE || sel.type == DISP_VESSEL || sel.type == DISP_MOON);
	}

	return found_exact;
}

bool DlgMap::FindTarget(int mx, int my)
{
	const VectorMap::OBJTYPE obj = vectormap->FindObject(mx, my);
	if(obj.type) {
		vectormap->SetSelection (obj);
		enableInfo = (obj.type == DISP_BASE || obj.type == DISP_VESSEL || obj.type == DISP_MOON);

		switch (obj.type) {
		case DISP_VESSEL:
		case DISP_BASE:
		case DISP_MOON:
			strcpy(searchbuf, ((Body*)obj.obj)->Name());
			break;
		case DISP_NAVAID:
			strcpy(searchbuf, ((Nav*)obj.obj)->GetId());
			break;
		default:
			strcpy (searchbuf, "");
			break;
		}
	}
	return (obj.type != 0);
}

void DlgMap::DrawMap() {
    ImVec2 sz = ImGui::GetContentRegionAvail();
    if(sz.x>=1.0f && sz.y >= 1.0f) {
        static ImVec2 oldsz = sz;//ImVec2(512,256);

        if(!vectormap) {
            Planet *earth = g_psys->GetPlanet ("Earth");
            if(earth) {
                planet = "Earth";
                vectormap=std::make_unique<VectorMap>(earth);
                vectormap->SetCBody(earth);
                vectormap->SetCanvas(nullptr, sz.x, sz.y);
				vectormap->SetDisplayFlags(prm->DispFlag);

                SetBody("Earth");
            }
        }

        if(sz.x != oldsz.x || sz.y != oldsz.y) {
            vectormap->SetCanvas(nullptr, sz.x, sz.y);

            vectormap->Update();
            vectormap->DrawMap ();

            oldsz = sz;
        }

        const double updDT = 1.0;
        if (td.SysT1 > updTmax || td.SysT1 < updTmin) {
            updTmax = td.SysT1 + updDT;
            updTmin = td.SysT1 - updDT;

            vectormap->Update();
            vectormap->DrawMap ();
        }

        ImVec2 uv_min = ImVec2(0.0f, 0.0f);                 // Top-left
        ImVec2 uv_max = ImVec2(1.0f, 1.0f);                 // Lower-right
        ImVec4 tint_col = ImVec4(1.0f, 1.0f, 1.0f, 1.0f);   // No tint
        ImVec4 border_col = ImVec4(0.0f, 0.0f, 0.0f, 0.0f); // 50% opaque white

		ImTextureID map = oapiGetImTextureID(vectormap->GetMap());
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0,0));
        ImGui::ImageButton("##VectorMap", map, ImVec2(sz.x, sz.y), uv_min, uv_max, border_col, tint_col);
		ImGui::PopStyleVar();

        ImGuiIO& io = ImGui::GetIO();
        bool updateMap = false;
        if (ImGui::IsItemHovered() && io.MouseWheel != 0.0) {
            zoom+=io.MouseWheel;
            if(zoom<1.0) zoom=1.0;

            vectormap->SetZoom(zoom);
            updateMap = true;
        }
        if (ImGui::IsItemActive()) {
			if(ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left)) {
				ImVec2 mousepos = ImGui::GetMousePos();
				ImVec2 imagepos = ImGui::GetItemRectMin();
				if(FindTarget(mousepos.x-imagepos.x, mousepos.y-imagepos.y))
					updateMap = true;
			} else {
				double lngc = vectormap->CntLng();
				double latc = vectormap->CntLat();

				double scale = std::min (sz.x, 2.0f*sz.y);
				double scalefac = vectormap->ZoomFac()*scale/Pi2;

				double lng = lngc - io.MouseDelta.x/scalefac;
				double lat = std::max (-Pi05, std::min (Pi05, latc + io.MouseDelta.y/scalefac));
				if (lng != lngc || lat != latc) {
					vectormap->SetCenter (lng, lat);
					updateMap = true;
				}
			}
        }

        if(updateMap) {
            vectormap->Update();
            vectormap->DrawMap ();
        }
    }
}

void DlgMap::DrawMenu() {
	ImGui::SetNextItemWidth(160.0f);
	if(ImGui::BeginCombo("##dlgmap_planet", planet.c_str(), ImGuiComboFlags_HeightLargest)) {
        DrawTree();
		ImGui::EndCombo();
	}
	
    ImGui::SameLine();

	ImGui::SetNextItemWidth(160.0f);
	if(ImGui::BeginCombo("##dlgmap_options", "Options", ImGuiComboFlags_HeightLargest)) {
        int df = vectormap->GetDisplayFlags();
		int olddf = df;

        ImGui::Text("Vessels");
        int vesselmode = (df & DISP_VESSEL ? df & DISP_FOCUSONLY ? 1:0:2);
        ImGui::RadioButton("All", &vesselmode, 0); ImGui::SameLine();
        ImGui::RadioButton("Focus Only", &vesselmode, 1); ImGui::SameLine();
        ImGui::RadioButton("None", &vesselmode, 2);
        ImGui::Separator();
        int vmode = (vesselmode==1 ? DISP_VESSEL | DISP_FOCUSONLY : vesselmode==2 ? 0 : DISP_VESSEL);
        df &= ~(DISP_VESSEL | DISP_FOCUSONLY);
        df |= vmode;

        ImGui::Text("Orbit Display");
        if (ImGui::BeginTable("table orbit display", 2, ImGuiTableFlags_SizingStretchSame)) {
            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::CheckboxFlags("Focus Vessel", &df, DISP_ORBITFOCUS);
            ImGui::TableSetColumnIndex(1);
            ImGui::CheckboxFlags("Target", &df, DISP_ORBITSEL);
            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            int orbitmode=(df & DISP_ORBITPLANE) ? 0 : 1;
            ImGui::RadioButton("Orbit Plane", &orbitmode, 0);
            ImGui::TableSetColumnIndex(1);
            ImGui::RadioButton("Ground Track", &orbitmode, 1);
            df &= ~(DISP_GROUNDTRACK | DISP_ORBITPLANE);
            df |= (orbitmode == 0)? DISP_ORBITPLANE:DISP_GROUNDTRACK;

            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::CheckboxFlags("Horizon Line", &df, DISP_HORIZONLINE); 
            ImGui::EndTable();
        }
        ImGui::Separator();

        ImGui::Text("Terminator");
        if (ImGui::BeginTable("table1", 2, ImGuiTableFlags_SizingStretchSame)) {
            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::CheckboxFlags("Line", &df, DISP_TERMINATOR_LINE);
            ImGui::TableSetColumnIndex(1);
            ImGui::CheckboxFlags("Shaded", &df, DISP_TERMINATOR_SHADE);
            ImGui::EndTable();
        }
        ImGui::Separator();

        ImGui::Text("Surface Markers");
        if (ImGui::BeginTable("table2", 2, ImGuiTableFlags_SizingStretchSame)) {

            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::CheckboxFlags("Grid Line", &df, DISP_GRIDLINE);
            ImGui::TableSetColumnIndex(1);
            ImGui::CheckboxFlags("Surface Bases", &df, DISP_BASE);

            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::CheckboxFlags("Coastlines", &df, DISP_COASTLINE);
            ImGui::TableSetColumnIndex(1);
            ImGui::CheckboxFlags("VOR Transmitters", &df, DISP_NAVAID);

            ImGui::TableNextRow();
            ImGui::TableSetColumnIndex(0);
            ImGui::CheckboxFlags("Contour Lines", &df,DISP_CONTOURS); 
            ImGui::TableSetColumnIndex(1);
            ImGui::CheckboxFlags("Landmarks", &df, DISP_CUSTOMMARKER);

            ImGui::EndTable();
        }
        ImGui::Separator();

        ImGui::Text("Other");
        ImGui::CheckboxFlags("Natural Satellites", &df, DISP_MOON);

        ImGui::EndCombo();

        vectormap->SetDisplayFlags(df);

		if(df!=olddf) {
            vectormap->Update();
            vectormap->DrawMap ();
			prm->DispFlag = df;
		}
    }

	ImGui::SameLine();

	if(ImGui::Button(ICON_FA_FILTER)) {
        ImGui::OpenPopup("dlgmap_search_filter");
	}
    ImVec2 btn_pos = ImGui::GetItemRectMin();
	btn_pos.x-=4;
	btn_pos.y-=5;
    ImGui::SetNextWindowPos(btn_pos);

    if (ImGui::BeginPopup("dlgmap_search_filter"))
    {
        ImGui::Text(ICON_FA_FILTER " Search filter");
        ImGui::Separator();
		
		ImGui::CheckboxFlags("Moons", &selectionfilter, DISP_MOON);
		ImGui::CheckboxFlags("Vessels", &selectionfilter, DISP_VESSEL);
		ImGui::CheckboxFlags("VOR", &selectionfilter, DISP_NAVAID);
		ImGui::CheckboxFlags("Landmarks", &selectionfilter, DISP_CUSTOMMARKER);
		ImGui::CheckboxFlags("Bases", &selectionfilter, DISP_BASE);

	    ImGui::EndPopup();
	}

	ImGui::SameLine();
	ImGui::SetNextItemWidth(160);
	if(ImGui::InputText("##search", searchbuf, sizeof(searchbuf), ImGuiInputTextFlags_EnterReturnsTrue)) {
		SetSelection(searchbuf);
	}
	ImGui::SameLine();
	if(ImGui::Button(ICON_FA_MAGNIFYING_GLASS)) {
		SetSelection(searchbuf);
	}
	ImGui::SetItemTooltip("Search object");

	ImGui::BeginDisabled(!enableInfo);
	ImGui::SameLine();
	if(ImGui::Button(ICON_FA_CIRCLE_INFO)) {
		VectorMap::OBJTYPE obj = vectormap->GetSelection ();
		if (obj.type == DISP_BASE || obj.type == DISP_VESSEL || obj.type == DISP_MOON) {
			DlgInfo *pInfo = g_pOrbiter->DlgMgr()->EnsureEntry<DlgInfo> ();
			pInfo->SetBody ((Body*)obj.obj);
		}
	}
	if(enableInfo)
		ImGui::SetItemTooltip("Open info dialog for object");
	ImGui::EndDisabled();	
}

void DlgMap::OnDraw() {
    DrawMenu();
    DrawMap();
}
