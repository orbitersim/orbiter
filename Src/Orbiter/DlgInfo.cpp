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
#include "imgui.h"

extern PlanetarySystem *g_psys;

DlgInfo::DlgInfo() : ImGuiDialog("Orbiter: Object info", {753,423}) {
}

void DlgInfo::AddCbodyNode(const CelestialBody *cbody) {
    ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth;
    const bool is_selected = m_SelectedTarget == cbody->Name();
    if (is_selected)
        node_flags |= ImGuiTreeNodeFlags_Selected;
    if(cbody->nSecondary()) {
        bool node_open = ImGui::TreeNodeEx(cbody->Name(), node_flags);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
            m_SelectedTarget = cbody->Name();
        if(node_open) {
            for (int i = 0; i < cbody->nSecondary(); i++) {
                AddCbodyNode (cbody->Secondary(i));
            }
            ImGui::TreePop();
        }
    } else {
        ImGui::TreeNodeEx(cbody->Name(), node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
            m_SelectedTarget = cbody->Name();
    }
}

void DlgInfo::DrawTree() {
    for (int i = 0; i < g_psys->nStar(); i++)
        AddCbodyNode (g_psys->GetStar(i));

    if (ImGui::TreeNode("Spaceports")) {
        for (int i = 0; i < g_psys->nGrav(); i++) {
            Body *obj = g_psys->GetGravObj (i);
            if (obj->Type() != OBJTP_PLANET) continue;
            Planet *planet = (Planet*)obj;
            if (g_psys->nBase(planet) > 0) {
				if(ImGui::TreeNode(planet->Name())) {
					for (int j = 0; j < g_psys->nBase(planet); j++) {
						const char *name = g_psys->GetBase (planet,j)->Name();
						const bool is_selected = m_SelectedTarget == name;
						ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
						if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
						ImGui::TreeNodeEx(name, node_flags);
						if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
							m_SelectedTarget = name;
					}
					ImGui::TreePop();
				}
            }
        }
        ImGui::TreePop();
    }
    if (ImGui::TreeNode("Vessels")) {
        for (int i = 0; i < g_psys->nVessel(); i++) {
            const char *name = g_psys->GetVessel(i)->Name();
            const bool is_selected = m_SelectedTarget == name;
            ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
            if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
            ImGui::TreeNodeEx(name, node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
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
    ImGui::Text("Select an object on the left panel");
}

void DlgInfo::DrawInfoVessel(Vessel *vessel) {
    ImGuiTableFlags flags = ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg;
    if(ImGui::CollapsingHeader("Designation", ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table Designation", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Name");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(vessel->Name());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Class");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(vessel->ClassName());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Transponder Frequency");
                ImGui::TableSetColumnIndex(1);
                float f;
                if (vessel->GetXpdrFreq (f))
                    ImGui::Text("%0.2fMHz", f);
                else
                    ImGui::TextUnformatted("N/A");
            ImGui::EndTable();
        }
    }
    if(ImGui::CollapsingHeader("Physical Parameters", ImGuiTreeNodeFlags_DefaultOpen)) {
        // Total mass, dry mass, propellant mass, mean radius, P. moment of inertias
        if (ImGui::BeginTable("table Physical Parameters", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Total mass");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%g kg", vessel->Mass());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Dry mass");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%g kg", vessel->EmptyMass());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Propellant mass");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%g kg", vessel->FuelMass());
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Mean radius");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", DistStr (vessel->Size())+1);
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("P. moments of inertia");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"(%4g, %4g, %4g) kg.m²", vessel->PMI().x, vessel->PMI().y, vessel->PMI().z);

            ImGui::EndTable();
        }
    }
    if(ImGui::CollapsingHeader("Thrusters Group Ratings (vacuum)", ImGuiTreeNodeFlags_DefaultOpen)) {
        //Main, retro, hover   
        const THGROUP_TYPE thgrp[3] = {THGROUP_MAIN, THGROUP_RETRO, THGROUP_HOVER};
        const char *thrtype[] = {"Main", "Retro", "Hover"};
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
                        ImGui::Text("N/A");
                    }
            }
            ImGui::EndTable();
        }

    }

    const Body *ref = vessel->ElRef();
    const Elements *el = vessel->Els();
    if(el && ref && ImGui::CollapsingHeader("Osculating Elements (Ecliptic Frame)", ImGuiTreeNodeFlags_DefaultOpen)) {
        //Reference, semi major axis, excentricity, inclination, longitude of AN, longitude of periapsis, mean longitude
        if (ImGui::BeginTable("table Osculating Elements", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Reference");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(ref->Name());

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Semi-major Axis (a)");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", SciStr (el->a, 5));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Excentricity (e)");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%g", el->e);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted(u8"Inclination (igg)");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.2f°", el->i*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Longitude of Ascending Node");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.2f°", el->theta*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Longitude of Periapsis");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.2f°", el->omegab*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Mean longitude");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.2f°", el->MeanLng()*DEG);
            ImGui::EndTable();
        }
    }

    const SurfParam *sp = vessel->GetSurfParam();
    if(sp && ImGui::CollapsingHeader("Surface-relative Parameters", ImGuiTreeNodeFlags_DefaultOpen)) {
        //reference, position, altitude, ground speed, vertical speed, heading, pitch, bank
        if (ImGui::BeginTable("table Surface-relative Parameters", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Reference");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(sp->ref->Name());

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Position");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%07.3f°%c %06.3f°%c", fabs(sp->lng)*DEG, sp->lng >= 0.0 ? 'E':'W', fabs(sp->lat)*DEG, sp->lat >= 0.0 ? 'N':'S');

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Altitude");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%sm", DistStr (sp->alt)+1);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Ground Speed");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%sm/s", FloatStr (sp->groundspd)+1);

            Vector V (mul (sp->L2H, tmul (vessel->ProxyBody()->GRot(), sp->groundvel_glob)));
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Vertical Speed");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%sm/s", FloatStr (V.y)+1);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Heading");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.0f°", sp->dir*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Pitch");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.0f°", sp->pitch*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Bank");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.0f° %s", fabs(sp->bank)*DEG, sp->bank >= 0.0 ? "left":"right");

            ImGui::EndTable();
        }
    }

   	if (vessel->isInAtmosphere()) {
        if(ImGui::CollapsingHeader("Atmospheric Parameters", ImGuiTreeNodeFlags_DefaultOpen)) {
            // Temperature, density, pressure
            if (ImGui::BeginTable("table Atmospheric Parameters", 2, flags))
            {
                double T, rho, p;
                vessel->AtmTemperature (T);
                vessel->AtmPressureAndDensity (p, rho);
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Temperature");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%0.2f K", T);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Density");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%0.4g kg/m^3", rho);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Pressure");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sPa", FloatStr(p)+1);

                ImGui::EndTable();
            }
        }
        if(ImGui::CollapsingHeader("Aerodynamic Parameters", ImGuiTreeNodeFlags_DefaultOpen)) {
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
                    ImGui::TextUnformatted("Dynamic Pressure");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sPa", FloatStr(dynp)+1);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("True Airspeed");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text( "%sm/s", FloatStr(sp->airspd)+1);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Mach Number");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%g", M);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Lift");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sN", FloatStr(L)+(L >= 0 ? 1:0));

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Drag");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sN", FloatStr(D)+1);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Weight");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%sN", FloatStr(vessel->GetWeight())+1);

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Lift/Drag Ratio");
                    ImGui::TableSetColumnIndex(1);
                    if (D)
                        ImGui::Text("%g", L/D);
                    else
                        ImGui::TextUnformatted("N/A");

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Angle of Attack");
                    ImGui::TableSetColumnIndex(1);
                    if(sp)
                        ImGui::Text(u8"%+0.1f°", -atan2 (sp->groundvel_ship.y, sp->groundvel_ship.z)*DEG);
                    else
                        ImGui::TextUnformatted("N/A");

                ImGui::EndTable();
            }
        }
    }

    if (vessel->nDock()) {
        if(ImGui::CollapsingHeader("Docking Ports", ImGuiTreeNodeFlags_DefaultOpen)) {
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
                    if (mate) sprintf (buf+strlen(buf), "[Docked to %s]", mate->Name());
                    else strcat (buf, "[free]");

                    ImGui::TableNextRow();
                        ImGui::TableSetColumnIndex(0);
                        ImGui::Text("Port %d", i+1);
                        ImGui::TableSetColumnIndex(1);
                        ImGui::TextUnformatted(buf);
                }

                ImGui::EndTable();
            }
        }
    }

    if(ImGui::CollapsingHeader("State Propagation", ImGuiTreeNodeFlags_DefaultOpen)) {
        // update mode, state propagator, subsamples, gravity sources
        if (ImGui::BeginTable("table State Propagation", 2, flags))
        {
            if (vessel->GetStatus() == FLIGHTSTATUS_LANDED) {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Update mode");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted("IDLE (landed)");
            } else if (vessel->isAttached()) {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Update mode");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted("PASSIVE (attached)");
            } else {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Update mode");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted("ACTIVE (dynamic)");

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("State Propagator");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(vessel->isOrbitStabilised() ? "stabilised" : vessel->CurPropagatorStr());

                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Subsamples");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("%d", vessel->CurPropagatorSubsamples());

                char cbuf[256];
                cbuf[0] = '\0';
                const GFieldData &gfd = vessel->GetGFieldData();
                for (int i = 0; i < gfd.ngrav; i++) {
                    if (i) strcat (cbuf, ", ");
                    strcat (cbuf, g_psys->GetGravObj(gfd.gravidx[i])->Name());
                }
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Gravity Sources");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted(cbuf);

            }
            ImGui::EndTable();
        }
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

    ImGuiTableFlags flags = ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg;
    if(ImGui::CollapsingHeader("Designation", ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody designation", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Name");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbody->Name());

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Primary");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbody->ElRef() ? cbody->ElRef()->Name() : "N/A");

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Solar System");
                ImGui::TableSetColumnIndex(1);
                const char *psys_name = g_psys->Name().c_str();
                ImGui::TextUnformatted(psys_name ? psys_name : "N/A");

            ImGui::EndTable();
        }
    }
    if(ImGui::CollapsingHeader("Physical Parameters", ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody Physical parameters", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Mass");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s kg", SciStr (cbody->Mass(), 4));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Mean Radius");
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
            } else strcpy (cbuf, "N/A");
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Gravitational Moments");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbuf);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Siderial day");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s s", SciStr (cbody->RotT(), 4));

            if (el) sprintf (cbuf, "%s s", SciStr (el->OrbitT(), 4));
            else    strcpy (cbuf, "N/A");

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Orbit Period");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbuf);

            if (planet) sprintf (cbuf, u8"%0.2f°", planet->Obliquity()*DEG);
            else strcpy (cbuf, "N/A");

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Obliquity of Ecliptic");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbuf);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Atmosphere");
                ImGui::TableSetColumnIndex(1);
                if(planet && planet->AtmParams()) {
                    ImGui::TextUnformatted("Yes");
                } else {
                    ImGui::TextUnformatted("No");
                }


            ImGui::EndTable();
        }
    }
    if(planet && planet->AtmParams() && ImGui::CollapsingHeader("Atmosphere", ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody Atmosphere", 2, flags))
        {
            char cbuf[128];
            const ATMCONST *ap = planet->AtmParams();
            strcpy (cbuf, "Generic");
            if (cb && cb->Version() >= 2) {
                CELBODY2 *cb2 = (CELBODY2*)cb;
                ATMOSPHERE *atm = cb2->GetAtmosphere();
                if (atm) strcpy (cbuf, atm->clbkName());
            }

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Atmosphere Model");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(cbuf);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Surface Pressure");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%sPa", FloatStr (ap->p0)+1);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Surface density");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%skg/m^3", SciStr (ap->rho0));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Specific Gas Constant");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2fJ/(K kg)", ap->R);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Specific Heat Ratio");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.2f", ap->gamma);

            ImGui::EndTable();
        }
    }

    if(el && ImGui::CollapsingHeader("Osculating Elements (Ecliptic Frame)", ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody Osculating elements (ecliptic frame)", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Semi-major Axis (a)");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", SciStr (el->a, 5));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Eccentricity (e)");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%0.5g", el->e);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Inclination (i)");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.2f°", el->i*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Longitude of Ascending Node");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.2f°", el->theta*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Longitude of Periapsis");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.2f°", el->omegab*DEG);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Mean Longitude");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.2f°", el->MeanLng()*DEG);

            ImGui::EndTable();
        }
    }
    if(strcmp(cbody->Name(), "Earth") && ImGui::CollapsingHeader("Geocentric Celestial Position", ImGuiTreeNodeFlags_DefaultOpen)) {
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
                ImGui::TextUnformatted("Right Ascension (RA)");
                ImGui::TableSetColumnIndex(1);
    			sprintf (cbuf, "%02.0fh %02.0fm %02.2fs", rah, ram, ras);
                ImGui::TextUnformatted(cbuf);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Declination (Dec)");
                ImGui::TableSetColumnIndex(1);
    			sprintf (cbuf, u8"%+02.0f° %02.0f' %02.2f''", dcd, dcm, dcs);
                ImGui::TextUnformatted(cbuf);

            ImGui::EndTable();
        }
    }

    if(el && ImGui::CollapsingHeader("Ecliptic position from primary", ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody Ecliptic position from primary", 2, flags))
        {
			Vector p (cbody->GPos() - cbody->ElRef()->GPos());
			double r   = p.length();
			double lng = atan2 (p.z, p.x);
			double lat = p.y/r;

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Longitude");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.3f°", DEG*posangle(lng));

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Latitude");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%0.3f°", DEG*lat);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Radial Distance");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text("%s m", SciStr (r));

            ImGui::EndTable();
        }
    }
    if(ImGui::CollapsingHeader("State Propagation", ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table celbody State propagation", 2, flags))
        {
            if (cbody->canDynamicPosVel()) {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Mode");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::TextUnformatted("Numerical");

                const GFieldData &gfd = cbody->GetGFieldData();
                if (gfd.ngrav) {
                    char cbuf[128];
                    cbuf[0] = '\0';
                    for (int i = 0; i < gfd.ngrav; i++) {
                        strcat (cbuf, g_psys->GetGravObj(gfd.gravidx[i])->Name());
                        if (i < gfd.ngrav-1) strcat (cbuf, ", ");
                    }
                    ImGui::TableNextRow();
                        ImGui::TableSetColumnIndex(0);
                        ImGui::TextUnformatted("Gravity Sources");
                        ImGui::TableSetColumnIndex(1);
                        ImGui::TextUnformatted(cbuf);

                }
            } else {
                ImGui::TableNextRow();
                    ImGui::TableSetColumnIndex(0);
                    ImGui::TextUnformatted("Mode");
                    ImGui::TableSetColumnIndex(1);
                    ImGui::Text("Analytic (%s)", cb ? "from module" : "2-body");
            }

            ImGui::EndTable();
        }
    }

}
void DlgInfo::DrawInfoBase(Base *base) {
    ImGuiTableFlags flags = ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg;
    if(ImGui::CollapsingHeader("Designation", ImGuiTreeNodeFlags_DefaultOpen)) {
        if (ImGui::BeginTable("table base designation", 2, flags))
        {
            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Name");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(base->Name());

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Located on");
                ImGui::TableSetColumnIndex(1);
                ImGui::TextUnformatted(base->RefPlanet()->Name());

           	double lng, lat;
        	base->EquPos (lng, lat);

            ImGui::TableNextRow();
                ImGui::TableSetColumnIndex(0);
                ImGui::TextUnformatted("Position");
                ImGui::TableSetColumnIndex(1);
                ImGui::Text(u8"%07.3f°%c  %06.3f°%c",fabs(lng)*DEG, lng >= 0.0 ? 'E':'W',fabs(lat)*DEG, lat >= 0.0 ? 'N':'S');

            ImGui::EndTable();
        }
    }

    // Landing pads
    //     pad 1
    //     pad xxx
    if(base->nPad()) {
        if(ImGui::CollapsingHeader("Landing Pads", ImGuiTreeNodeFlags_DefaultOpen)) {
            if (ImGui::BeginTable("table Landing Pads", 2, flags))
            {
                const char *c, *statusstr[3] = {"free", "", "reserved"};
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
                        ImGui::Text("Pad %d", i+1);
                        ImGui::TableSetColumnIndex(1);
                        ImGui::TextUnformatted(cbuf);
                }
                ImGui::EndTable();
            }
        }
    }

    // Runways
    if(base->nRwy()) {
        if(ImGui::CollapsingHeader("Runways", ImGuiTreeNodeFlags_DefaultOpen)) {
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
                    sprintf (cbuf, "ILS %s/%s, length %0.0fm", ils1, ils2, rwy->length);

                    ImGui::TableNextRow();
                        ImGui::TableSetColumnIndex(0);
                        ImGui::Text("Runway %02d/%02d", dir, (dir+18)%36);
                        ImGui::TableSetColumnIndex(1);
                        ImGui::TextUnformatted(cbuf);
                }
                ImGui::EndTable();
            }
        }
    }

    // VOR
    if(base->nVOR()) {
        if(ImGui::CollapsingHeader("VOR Transmitters", ImGuiTreeNodeFlags_DefaultOpen)) {
            if (ImGui::BeginTable("table VOR", 2, flags))
            {
                for(int i=0;i<base->nVOR();i++) {
                    const Nav *nav = base->VOR(i);
                    if(nav->Type() == TRANSMITTER_VOR) {
                        ImGui::TableNextRow();
                            ImGui::TableSetColumnIndex(0);
                            ImGui::TextUnformatted(nav->GetId());
                            ImGui::TableSetColumnIndex(1);
                            ImGui::Text("%06.2f, range %sm", nav->GetFreq(), DistStr (nav->GetRange()));
                    }
                }
                ImGui::EndTable();
            }
        }
    }
}

void DlgInfo::OnDraw() {
        ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;
        ImGui::BeginChild("ChildL", ImVec2(250, 0), true, window_flags);
        {
            ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x/2, 20));

            ImGui::Button("Focus Vessel");
            ImGui::SameLine();
            ImGui::Button("Camera Target");
            DrawTree();
        }
        ImGui::EndChild();
        ImGui::SameLine();
        ImGui::BeginChild("ChildR", ImVec2(0, 0), true);
            ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
            ImGui::Text("Object: %s", m_SelectedTarget.c_str());

            DrawInfo();

        ImGui::EndChild();
}

void DlgInfo::SetBody(Body *body) {
	m_SelectedTarget = body->Name();
}
