// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Object info window
// ======================================================================
#include "DlgInfo.h"
#include "Celbody.h"
#include "Psys.h"
#include "Astro.h"
#include "Element.h"
#include "Camera.h"
#include "imgui.h"
#include "imgui_extras.h"
#include "IconsFontAwesome6.h"

#define TRANSLATION_CONTEXT "Dialog Info"
#include "i18n.h"

extern PlanetarySystem *g_psys;
extern Vessel *g_focusobj;
extern Camera *g_camera;

DlgInfo::DlgInfo() : ImGuiDialog(ICON_FA_CIRCLE_INFO, _("Orbiter: Object info"), {753,423}) {
	SetHelp("html/orbiter.chm", "/objinfo.htm");
	m_SelectedTarget = g_psys->GetStar(0)->Name();
}

void DlgInfo::AddCbodyNode(const CelestialBody *cbody) {
    ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth;
    const bool is_selected = m_SelectedTarget == cbody->Name();
    if (is_selected)
        node_flags |= ImGuiTreeNodeFlags_Selected;
    if(cbody->nSecondary()) {
        bool node_open = ImGui::TreeNodeEx(_name(cbody->Name()), node_flags);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
            m_SelectedTarget = cbody->Name();
        if(node_open) {
            for (int i = 0; i < cbody->nSecondary(); i++) {
                AddCbodyNode (cbody->Secondary(i));
            }
            ImGui::TreePop();
        }
    } else {
        ImGui::TreeNodeEx(_name(cbody->Name()), node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
            m_SelectedTarget = cbody->Name();
    }
}

void DlgInfo::DrawTree() {
    for (int i = 0; i < g_psys->nStar(); i++)
        AddCbodyNode (g_psys->GetStar(i));

    if (ImGui::TreeNode(_("Spaceports"))) {
        for (int i = 0; i < g_psys->nGrav(); i++) {
            Body *obj = g_psys->GetGravObj (i);
            if (obj->Type() != OBJTP_PLANET) continue;
            Planet *planet = (Planet*)obj;
            if (g_psys->nBase(planet) > 0) {
				if(ImGui::TreeNode(_name(planet->Name()))) {
					for (int j = 0; j < g_psys->nBase(planet); j++) {
						const char *name = g_psys->GetBase (planet,j)->Name();
						const bool is_selected = m_SelectedTarget == name;
						ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
						if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
						ImGui::TreeNodeEx(_name(name), node_flags);
						if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
							m_SelectedTarget = name;
					}
					ImGui::TreePop();
				}
            }
        }
        ImGui::TreePop();
    }
    if (ImGui::TreeNode(_("Vessels"))) {
        for (int i = 0; i < g_psys->nVessel(); i++) {
            const char *name = g_psys->GetVessel(i)->Name();
            const bool is_selected = m_SelectedTarget == name;
            ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
            if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
            ImGui::TreeNodeEx(_name(name), node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
            if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
                m_SelectedTarget = name;
        }
        ImGui::TreePop();
    }
}

void DlgInfo::DrawInfo() {
    Vessel *vessel = g_psys->GetVessel(m_SelectedTarget.c_str(), true);
    if(vessel) {
        DrawInfoVessel(vessel);
        return;
    }
    CelestialBody *cb = g_psys->GetGravObj(m_SelectedTarget.c_str(), true);
    if(cb) {
        DrawInfoCelestialBody(cb);
        return;
    }
    Base *base = g_psys->GetBase(m_SelectedTarget.c_str(), true);
    if(base) {
        DrawInfoBase(base);
        return;
    }
    ImGui::Text(_("Select an object on the left panel"));
}

void DlgInfo::DrawInfoVessel(Vessel *vessel) {
    ImGuiTableFlags flags = ImGuiTableFlags_SizingStretchSame | ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg;
    if(ImGui::BeginAnimatedCollapsingHeader(_("Designation"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table Designation", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Name"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(_name(vessel->Name()));
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Class"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(vessel->ClassName());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Transponder Frequency"));
                ImGui::TableSetColumnIndex(1);
                float f;
                if (vessel->GetXpdrFreq (f))
                    ImGui::Text("%0.2fMHz", f);
                else
                    ImGui::TextUnformatted(_("N/A"));
            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }
    if(ImGui::BeginAnimatedCollapsingHeader(_("Physical Parameters"), ImGuiTreeNodeFlags_DefaultOpen)) {
        // Total mass, dry mass, propellant mass, mean radius, P. moment of inertias
        if (ImGui::BeginTable("table Physical Parameters", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Total mass"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%g kg", vessel->Mass());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Dry mass"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%g kg", vessel->EmptyMass());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Propellant mass"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%g kg", vessel->FuelMass());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Mean radius"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", DistStr (vessel->Size())+1);
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("P. moments of inertia"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("(%4g, %4g, %4g) kg.m²", vessel->PMI().x, vessel->PMI().y, vessel->PMI().z);

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }
    if(ImGui::BeginAnimatedCollapsingHeader(_("Thrusters Group Ratings (vacuum)"), ImGuiTreeNodeFlags_DefaultOpen)) {
        //Main, retro, hover   
        const THGROUP_TYPE thgrp[3] = {THGROUP_MAIN, THGROUP_RETRO, THGROUP_HOVER};
        const char *thrtype[] = {_("Main"), _("Retro"), _("Hover")};
        if (ImGui::BeginTable("table Thrusters Group Ratings", 2, flags))
        {
            for (int i = 0; i < 3; i++) {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(thrtype[i]);
                    ImGui::TableSetColumnIndex(1);
                    double th = vessel->GetThrusterGroupMaxth (thgrp[i]);
                    if (th > 0.0) {
                        ImGui::Text("%sN", FloatStr (th)+1);
                    } else {
                        ImGui::Text(_("N/A"));
                    }
            }
            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }

    const Body *ref = vessel->ElRef();
    const Elements *el = vessel->Els();
    if(el && ref && ImGui::BeginAnimatedCollapsingHeader(_("Osculating Elements (Ecliptic Frame)"), ImGuiTreeNodeFlags_DefaultOpen)) {
        //Reference, semi major axis, excentricity, inclination, longitude of AN, longitude of periapsis, mean longitude
        if (ImGui::BeginTable("table Osculating Elements", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Reference"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(_name(ref->Name()));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Semi-major Axis (a)"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", SciStr (el->a, 5));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Eccentricity (e)"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%g", el->e);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Inclination (i)"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f°", el->i*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Longitude of Ascending Node"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f°", el->theta*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Longitude of Periapsis"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f°", el->omegab*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Mean longitude"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f°", el->MeanLng()*DEG);
            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }

    const SurfParam *sp = vessel->GetSurfParam();
    if(sp && ImGui::BeginAnimatedCollapsingHeader(_("Surface-relative Parameters"), ImGuiTreeNodeFlags_DefaultOpen)) {
        //reference, position, altitude, ground speed, vertical speed, heading, pitch, bank
        if (ImGui::BeginTable("table Surface-relative Parameters", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Reference"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(_name(sp->ref->Name()));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Position"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%07.3f°%s %06.3f°%s", fabs(sp->lng)*DEG, sp->lng >= 0.0 ? _card("E"):_card("W"), fabs(sp->lat)*DEG, sp->lat >= 0.0 ? _card("N"):_card("S"));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Altitude"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%sm", DistStr (sp->alt)+1);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Ground Speed"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%sm/s", FloatStr (sp->groundspd)+1);

            Vector V (mul (sp->L2H, tmul (vessel->ProxyBody()->GRot(), sp->groundvel_glob)));
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Vertical Speed"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%sm/s", FloatStr (V.y)+1);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Heading"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.0f°", sp->dir*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Pitch"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.0f°", sp->pitch*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Bank"));
                ImGui::TableSetColumnIndex(1);
				// TRANSLATORS: Bank xxx° right/left
                ImGui::Text("%0.0f° %s", fabs(sp->bank)*DEG, sp->bank >= 0.0 ? _("left"):_("right"));

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }

   	if (vessel->isInAtmosphere()) {
        if(ImGui::BeginAnimatedCollapsingHeader(_("Atmospheric Parameters"), ImGuiTreeNodeFlags_DefaultOpen)) {
            // Temperature, density, pressure
            if (ImGui::BeginTable("table Atmospheric Parameters", 2, flags))
            {
                double T, rho, p;
                vessel->AtmTemperature (T);
                vessel->AtmPressureAndDensity (p, rho);
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Temperature"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%0.2f K", T);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Density"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%0.4g kg/m^3", rho);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Pressure"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sPa", FloatStr(p)+1);

                ImGui::EndTable();
            }
            ImGui::EndAnimatedCollapsingHeader();
        }
        if(ImGui::BeginAnimatedCollapsingHeader(_("Aerodynamic Parameters"), ImGuiTreeNodeFlags_DefaultOpen)) {
            // Dynamic pressure, true airspeed, mach number, lift, drag, weight, lift/drag ratio, angle of attack
            if (ImGui::BeginTable("table Aerodynamic Parameters", 2, flags))
            {
                double dynp, M, L, D;
                vessel->DynPressure (dynp);
                L = vessel->GetLift();
                D = vessel->GetDrag();
                vessel->MachNumber (M);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Dynamic Pressure"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sPa", FloatStr(dynp)+1);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("True Airspeed"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text( "%sm/s", FloatStr(sp->airspd)+1);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Mach Number"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%g", M);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Lift"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sN", FloatStr(L)+(L >= 0 ? 1:0));

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Drag"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sN", FloatStr(D)+1);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Weight"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sN", FloatStr(vessel->GetWeight())+1);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Lift/Drag Ratio"));
                    ImGui::TableSetColumnIndex(1);
                    if (D)
                        ImGui::Text("%g", L/D);
                    else
                        ImGui::TextUnformatted(_("N/A"));

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Angle of Attack"));
                    ImGui::TableSetColumnIndex(1);
                    if(sp)
                        ImGui::Text("%+0.1f°", -atan2 (sp->groundvel_ship.y, sp->groundvel_ship.z)*DEG);
                    else
                        ImGui::TextUnformatted(_("N/A"));

                ImGui::EndTable();
            }
            ImGui::EndAnimatedCollapsingHeader();
        }
    }

    if (vessel->nDock()) {
        if(ImGui::BeginAnimatedCollapsingHeader(_("Docking Ports"), ImGuiTreeNodeFlags_DefaultOpen)) {
            // port 1,2,3...
            if (ImGui::BeginTable("table Docking Ports", 2, flags))
            {
                for(int i = 0;i<vessel->nDock();i++) {
                    char buf[128];
                    if (vessel->GetDockParams(i)->ids)
    					sprintf (buf, "IDS %06.2f ", vessel->GetDockParams(i)->ids->GetFreq());
                    else
                        buf[0]='\0';
                    Vessel *mate = vessel->DockMate (i);
                    if (mate) sprintf (buf+strlen(buf), _("[Docked to %s]"), _name(mate->Name()));
                    else strcat (buf, _("[free]"));

                    ImGui::TableNextRow();
                        ImGui::TableSetColumnIndex(0);
                        ImGui::Text(_("Port %d"), i+1);
                        ImGui::TableSetColumnIndex(1);
                        ImGui::TextUnformatted(buf);
                }

                ImGui::EndTable();
            }
            ImGui::EndAnimatedCollapsingHeader();
        }
    }

    if(ImGui::BeginAnimatedCollapsingHeader(_("State Propagation"), ImGuiTreeNodeFlags_DefaultOpen)) {
        // update mode, state propagator, subsamples, gravity sources
        if (ImGui::BeginTable("table State Propagation", 2, flags))
        {
            if (vessel->GetStatus() == FLIGHTSTATUS_LANDED) {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Update mode"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(_("IDLE (landed)"));
            } else if (vessel->isAttached()) {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Update mode"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(_("PASSIVE (attached)"));
            } else {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Update mode"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(_("ACTIVE (dynamic)"));

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("State Propagator"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(vessel->isOrbitStabilised() ? _("stabilised") : vessel->CurPropagatorStr());

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Subsamples"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%d", vessel->CurPropagatorSubsamples());

                char cbuf[256];
                cbuf[0] = '\0';
                const GFieldData &gfd = vessel->GetGFieldData();
                for (int i = 0; i < gfd.ngrav; i++) {
                    if (i) strcat (cbuf, ", ");
                    strcat (cbuf, _name(g_psys->GetGravObj(gfd.gravidx[i])->Name()));
                }
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Gravity Sources"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(cbuf);

            }
            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }
}


void DlgInfo::DrawInfoCelestialBody(CelestialBody *cbody) {
    const Elements *el = nullptr;
    const Planet *planet = nullptr;
    const CELBODY *cb = cbody->GetModuleInterface();

    switch (cbody->Type()) {
    case OBJTP_PLANET:
        planet = (Planet*)cbody;
        el = planet->Els();
        break;
    }

    ImGuiTableFlags flags = ImGuiTableFlags_SizingStretchSame | ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg;
    if(ImGui::BeginAnimatedCollapsingHeader(_("Designation"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody designation", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Name"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(_name(cbody->Name()));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Primary"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbody->ElRef() ? _name(cbody->ElRef()->Name()) : _("N/A"));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Solar System"));
                ImGui::TableSetColumnIndex(1);
                const char *psys_name = g_psys->Name().c_str();
                ImGui::TextUnformatted(psys_name ? _name(psys_name) : _("N/A"));

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }
    if(ImGui::BeginAnimatedCollapsingHeader(_("Physical Parameters"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody designation", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Mass"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s kg", SciStr (cbody->Mass(), 4));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Mean Radius"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", SciStr (cbody->Size(), 4));

            int nj;
            char cbuf[128];
            if ((nj = cbody->nJcoeff())) {
                cbuf[0] = '\0';
                for (int i = 0; i < nj; i++) {
                    sprintf (cbuf + strlen(cbuf), "J%d=%s", i+2, SciStr (cbody->Jcoeff (i),3));
                    if (i < nj-1) strcat (cbuf, ", ");
                }
            } else strcpy (cbuf, _("N/A"));
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Gravitational Moments"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbuf);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Siderial day"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s s", SciStr (cbody->RotT(), 4));

            if (el) sprintf (cbuf, "%s s", SciStr (el->OrbitT(), 4));
            else    strcpy (cbuf, _("N/A"));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Orbit Period"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbuf);

            if (planet) sprintf (cbuf, "%0.2f°", planet->Obliquity()*DEG);
            else strcpy (cbuf, _("N/A"));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Obliquity of Ecliptic"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbuf);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Atmosphere"));
                ImGui::TableSetColumnIndex(1);
                if(planet && planet->AtmParams()) {
                    ImGui::TextUnformatted(_("Yes"));
                } else {
                    ImGui::TextUnformatted(_("No"));
                }


            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }
    if(planet && planet->AtmParams() && ImGui::BeginAnimatedCollapsingHeader(_("Atmosphere"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody Atmosphere", 2, flags))
        {
            char cbuf[128];
            const ATMCONST *ap = planet->AtmParams();
            strcpy (cbuf, _("Generic"));
            if (cb && cb->Version() >= 2) {
                CELBODY2 *cb2 = (CELBODY2*)cb;
                ATMOSPHERE *atm = cb2->GetAtmosphere();
                if (atm) strcpy (cbuf, atm->clbkName());
            }

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Atmosphere Model"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbuf);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Surface Pressure"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%sPa", FloatStr (ap->p0)+1);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Surface density"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%skg/m^3", SciStr (ap->rho0));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Specific Gas Constant"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2fJ/(K kg)", ap->R);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Specific Heat Ratio"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f", ap->gamma);

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }

    if(el && ImGui::BeginAnimatedCollapsingHeader(_("Osculating Elements (Ecliptic Frame)"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody Osculating elements (ecliptic frame)", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Semi-major Axis (a)"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", SciStr (el->a, 5));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Eccentricity (e)"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.5g", el->e);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Inclination (i)"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f°", el->i*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Longitude of Ascending Node"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f°", el->theta*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Longitude of Periapsis"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f°", el->omegab*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Mean Longitude"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f°", el->MeanLng()*DEG);

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }
    if(strcmp(cbody->Name(), "Earth") && ImGui::BeginAnimatedCollapsingHeader(_("Geocentric Celestial Position"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody Geocentric Celestial Position", 2, flags))
        {
            char cbuf[128];
            Planet *earth = g_psys->GetPlanet ("Earth");
			Vector p (cbody->GPos() - earth->GPos());
			double r   = p.length();
			double lng = atan2 (p.z, p.x);
			double lat = p.y/r;
			double ra, dc, rah, ram, ras, dcd, dcm, dcs;
			static double ob = earth->Obliquity();
			static double cosob = cos(ob), sinob = sin(ob);
			Ecl2Equ (cosob, sinob, lng, lat, ra, dc);
			ram = modf (posangle(ra) * 24.0/Pi2, &rah) * 60.0;
			ras = modf (ram, &ram) * 60.0;
			dcm = fabs (modf (dc*DEG, &dcd)) * 60.0;
			dcs = modf (dcm, &dcm) * 60.0;

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Right Ascension (RA)"));
                ImGui::TableSetColumnIndex(1);
    			sprintf (cbuf, "%02.0fh %02.0fm %02.2fs", rah, ram, ras);
                ImGui::TextUnformatted(cbuf);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Declination (Dec)"));
                ImGui::TableSetColumnIndex(1);
    			sprintf (cbuf, "%+02.0f° %02.0f' %02.2f''", dcd, dcm, dcs);
                ImGui::TextUnformatted(cbuf);

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }

    if(el && ImGui::BeginAnimatedCollapsingHeader(_("Ecliptic position from primary"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody Ecliptic position from primary", 2, flags))
        {
			Vector p (cbody->GPos() - cbody->ElRef()->GPos());
			double r   = p.length();
			double lng = atan2 (p.z, p.x);
			double lat = p.y/r;

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Longitude"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.3f°", DEG*posangle(lng));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Latitude"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.3f°", DEG*lat);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Radial Distance"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", SciStr (r));

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }
    if(ImGui::BeginAnimatedCollapsingHeader(_("State Propagation"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody State propagation", 2, flags))
        {
            if (cbody->canDynamicPosVel()) {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Mode"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(_("Numerical"));

                const GFieldData &gfd = cbody->GetGFieldData();
                if (gfd.ngrav) {
                    char cbuf[128];
                    cbuf[0] = '\0';
                    for (int i = 0; i < gfd.ngrav; i++) {
                        strcat (cbuf, _name(g_psys->GetGravObj(gfd.gravidx[i])->Name()));
                        if (i < gfd.ngrav-1) strcat (cbuf, ", ");
                    }
                    ImGui::TableNextRow();
                        ImGui::TableSetColumnIndex(0);
                        ImGui::TextUnformatted(_("Gravity Sources"));
                        ImGui::TableSetColumnIndex(1);
                        ImGui::TextUnformatted(cbuf);

                }
            } else {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted(_("Mode"));
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text(_("Analytic (%s)"), cb ? _("from module") : _("2-body"));
            }

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }
}
void DlgInfo::DrawInfoBase(Base *base) {
    ImGuiTableFlags flags = ImGuiTableFlags_SizingStretchSame | ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg;
    if(ImGui::BeginAnimatedCollapsingHeader(_("Designation"), ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table base designation", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Name"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(_name(base->Name()));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Located on"));
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(_name(base->RefPlanet()->Name()));

           	double lng, lat;
        	base->EquPos (lng, lat);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(_("Position"));
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%07.3f°%s  %06.3f°%s",fabs(lng)*DEG, lng >= 0.0 ? _card("E"):_card("W"),fabs(lat)*DEG, lat >= 0.0 ? _card("N"):_card("S"));

            ImGui::EndTable();
        }
        ImGui::EndAnimatedCollapsingHeader();
    }

    // Landing pads
    //     pad 1
    //     pad xxx
    if(base->nPad()) {
        if(ImGui::BeginAnimatedCollapsingHeader(_("Landing Pads"), ImGuiTreeNodeFlags_DefaultOpen)) {
            if (ImGui::BeginTable("table Landing Pads", 2, flags))
            {
                const char *c, *statusstr[3] = {_("free"), "", _("reserved")};
                for(int i=0;i<base->nPad();i++) {
                    char cbuf[256];
                    cbuf[0] = '\0';
                    int status = base->PadStatus(i)->status;
                    if (status == 1) c = base->PadStatus(i)->vessel->Name();
                    else c = statusstr[status];
                    if (base->PadStatus(i)->nav)
                        sprintf (cbuf, "ILS %06.2f ", base->PadStatus(i)->nav->GetFreq());
    				sprintf (cbuf+strlen (cbuf), "[%s]", c);

                    ImGui::TableNextRow();
                        ImGui::TableSetColumnIndex(0);
                        ImGui::Text(_("Pad %d"), i+1);
                        ImGui::TableSetColumnIndex(1);
                        ImGui::TextUnformatted(cbuf);
                }
                ImGui::EndTable();
            }
            ImGui::EndAnimatedCollapsingHeader();
        }
    }

    // Runways
    if(base->nRwy()) {
        if(ImGui::BeginAnimatedCollapsingHeader(_("Runways"), ImGuiTreeNodeFlags_DefaultOpen)) {
            if (ImGui::BeginTable("table Runways", 2, flags))
            {
                for(int i=0;i<base->nRwy();i++) {
                    char cbuf[256];
                    const RwySpec *rwy = base->RwyStatus (i);
                    int dir = (int)(posangle(rwy->appr1)*DEG*0.1+0.5);
                    char ils1[20], ils2[20];
                    if (rwy->ils1) sprintf (ils1, "%06.2f", rwy->ils1->GetFreq());
                    else strcpy (ils1, "--");
                    if (rwy->ils2) sprintf (ils2, "%06.2f", rwy->ils2->GetFreq());
                    else strcpy (ils2, "--");
                    sprintf (cbuf, _("ILS %s/%s, length %0.0fm"), ils1, ils2, rwy->length);

                    ImGui::TableNextRow();
                        ImGui::TableSetColumnIndex(0);
                        ImGui::Text(_("Runway %02d/%02d"), dir, (dir+18)%36);
                        ImGui::TableSetColumnIndex(1);
                        ImGui::TextUnformatted(cbuf);
                }
                ImGui::EndTable();
            }
            ImGui::EndAnimatedCollapsingHeader();
        }
    }

    // VOR
    if(base->nVOR()) {
        if(ImGui::BeginAnimatedCollapsingHeader(_("VOR Transmitters"), ImGuiTreeNodeFlags_DefaultOpen)) {
            if (ImGui::BeginTable("table VOR", 2, flags))
            {
                for(int i=0;i<base->nVOR();i++) {
                    const Nav *nav = base->VOR(i);
                    if(nav->Type() == TRANSMITTER_VOR) {
                        ImGui::TableNextRow();
                            ImGui::TableSetColumnIndex(0);
                            ImGui::TextUnformatted(nav->GetId());
                            ImGui::TableSetColumnIndex(1);
                            ImGui::Text(_("%06.2f, range %sm"), nav->GetFreq(), DistStr (nav->GetRange()));
                    }
                }
                ImGui::EndTable();
            }
            ImGui::EndAnimatedCollapsingHeader();
        }
    }
}

void DlgInfo::OnDraw() {
        ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;
        ImGui::BeginChild("ChildL", ImVec2(250, 0), true, window_flags);
        {
            ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x/2, 20));

            if(ImGui::Button(_("Focus Vessel")))
				m_SelectedTarget = g_focusobj->Name();
            ImGui::SameLine();
            if(ImGui::Button(_("Camera Target")))
				m_SelectedTarget = g_camera->Target()->Name();
            DrawTree();
        }
        ImGui::EndChild();
        ImGui::SameLine();
        ImGui::BeginChild("ChildR", ImVec2(0, 0), 0);
            ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
            ImGui::Text(_("Object: %s"), _name(m_SelectedTarget.c_str()));

            DrawInfo();

        ImGui::EndChild();
}

void DlgInfo::SetBody(Body *body) {
	m_SelectedTarget = body->Name();
}
