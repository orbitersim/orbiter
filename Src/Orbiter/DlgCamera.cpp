#include "DlgCamera.h"
#include "OrbiterAPI.h"
#include "Orbiter.h"
#include "Celbody.h"
#include "Psys.h"
#include "Pane.h"
#include "Panel.h"
#include "Panel2D.h"
#include "imgui.h"
#include "imgui_extras.h"
#include "imgui_internal.h"
#include "IconsFontAwesome6.h"

extern PlanetarySystem *g_psys;
extern Camera *g_camera;
extern Pane *g_pane;
extern TimeData td;
extern Orbiter *g_pOrbiter;
extern Vessel *g_focusobj;

DlgCamera::DlgCamera() : ImGuiDialog(ICON_FA_VIDEO " Orbiter: Camera", {512,359}) {
    m_SelectedPreset = -1;
}

void DlgCamera::OnDraw() {
	extmode = g_camera->GetExtMode ();
	intmode = g_camera->GetIntMode ();
	extcam = (g_camera->GetMode () != CAM_COCKPIT);
	ground_lock = g_camera->GroundObserver_TargetLock();
	rot_is_tilt = (!extcam || (extmode == CAMERA_GROUNDOBSERVER && !ground_lock));
	rot_is_pan = (extcam && extmode == CAMERA_GROUNDOBSERVER);
    
    if(followterrain[0]=='\0')
      	sprintf (followterrain, "%0.0lf", g_camera->GroundObserver_TerrainLimit());

	static char* mode_label[] = { "Cockpit", "Track", "Ground" };
	char *curr_mode = (extcam ? extmode == CAMERA_GROUNDOBSERVER ? mode_label[2] : mode_label[1] : mode_label[0]);

	ImGui::PushID("mode");
	ImGui::SeparatorText("Camera mode");
	if (BeginWideCombo("##mode", curr_mode, ImGuiComboFlags_HeightSmall)) {
		bool is_selected = false;
		if (ImGui::Selectable("Cockpit", is_selected))
			g_pOrbiter->SetView(g_focusobj, 0);
		if (ImGui::Selectable("Track", is_selected)) {
			g_pOrbiter->SetView(g_focusobj, 1);
			g_camera->SetTrackMode(CAMERA_TARGETRELATIVE);
		}
		if (ImGui::Selectable("Ground", is_selected))
			SetGroundObserverView();
		ImGui::EndCombo();
	}
	ImGui::PopID();

	int mode = g_camera->GetMode();
	if (mode == CAM_COCKPIT) {
		DrawCockpitMode();
	} else if (mode == CAM_GROUNDOBSERVER) {
		DrawGroundMode();
	} else {
		DrawTrackMode();
	}

#ifdef UNDEF
    ImGuiTabBarFlags tab_bar_flags = ImGuiTabBarFlags_None;
    if (ImGui::BeginTabBar("CameraTabBar", tab_bar_flags))
    {
        for(size_t i = 0; i < sizeof(tabs)/sizeof(tabs[0]); i++) {
            if(ImGui::BeginTabItem(tabs[i].name)) {
				SetHelp("html/orbiter.chm", tabs[i].helptopic);
                (this->*tabs[i].func)();
                ImGui::EndTabItem();
            }
        }
        ImGui::EndTabBar();
    }
#endif
}

void DlgCamera::DrawCockpitMode()
{
	ImGui::Text(g_focusobj->Name());

	ImGui::PushID("mode");
	ImGui::SeparatorText("Cockpit view");
	const char* cockpit_mode_label[] = { "Glass cockpit", "Panel view", "Virtual cockpit" };
	IntCamMode mode = g_camera->GetIntMode();
	if (BeginWideCombo("##cockpit_mode", cockpit_mode_label[mode - CAMERA_GENERICCOCKPIT], ImGuiComboFlags_HeightSmall)) {
		bool is_selected = false;
		for (int i = CAMERA_GENERICCOCKPIT; i <= CAMERA_VIRTUALCOCKPIT; i++) {
			if (ImGui::Selectable(cockpit_mode_label[i - CAMERA_GENERICCOCKPIT], is_selected)) {
				g_pane->SetPanelMode(i - CAMERA_GENERICCOCKPIT + 1);
				mode = (IntCamMode)i;
			}
		}
		ImGui::EndCombo();
	}
	ImGui::PopID();

	if (mode == CAMERA_2DPANELCOCKPIT) {
		ImVec2 area = ImGui::GetContentRegionAvail();
		if (ImGui::BeginChild("ChildL", ImVec2(area.x / 2, 0))) {
			ImGui::PushID("switch_panel");
			ImGui::SeparatorText("Switch panel");
			bool has_neighbour[4] = { false, false, false, false };
			Panel2D* panel = g_pane->GetPanel2D();
			if (panel) {
				for (int i = 0; i < 4; i++)
					has_neighbour[i] = (panel->Connect(i) >= 0);
			} else {
				Panel* panel = g_pane->GetPanel2D_old();
					if (panel) {
						for (int i = 0; i < 4; i++)
							has_neighbour[i] = (panel->Connect(i) >= 0);
					}
			}
			if (ImGui::BeginChild("##switch_panel", ImVec2(0, 50))) {
				ImVec2 area = ImGui::GetContentRegionAvail();
				float xcnt = area.x * 0.5f;
				ImGui::SetCursorPos(ImVec2(xcnt - 12.5f, 0));
				if (!has_neighbour[2])
					ImGui::BeginDisabled();
				if (ImGui::ArrowButton("##up", ImGuiDir_Up))
					g_pane->SwitchPanel(2);
				if (!has_neighbour[2])
					ImGui::EndDisabled();
				ImGui::SetCursorPos(ImVec2(xcnt - 12.5f, 25));
				if (!has_neighbour[3])
					ImGui::BeginDisabled();
				if (ImGui::ArrowButton("##down", ImGuiDir_Down))
					g_pane->SwitchPanel(3);
				if (!has_neighbour[3])
					ImGui::EndDisabled();
				ImGui::SetCursorPos(ImVec2(xcnt - 37.5f, 12));
				if (!has_neighbour[0])
					ImGui::BeginDisabled();
				if (ImGui::ArrowButton("##left", ImGuiDir_Left))
					g_pane->SwitchPanel(0);
				if (!has_neighbour[0])
					ImGui::EndDisabled();
				ImGui::SetCursorPos(ImVec2(xcnt + 12.5f, 12));
				if (!has_neighbour[1])
					ImGui::BeginDisabled();
				if (ImGui::ArrowButton("##right", ImGuiDir_Right))
					g_pane->SwitchPanel(1);
				if (!has_neighbour[1])
					ImGui::EndDisabled();
				ImGui::EndChild();
			}
			ImGui::PopID();
			ImGui::EndChild();
			ImGui::SameLine();
			if (ImGui::BeginChild("ChildR")) {
				ImGui::PushID("scroll_panel");
				ImGui::SeparatorText("Scroll panel");
				if (ImGui::BeginChild("##scroll_panel", ImVec2(0, 50))) {
					ImVec2 area = ImGui::GetContentRegionAvail();
					float xcnt = area.x * 0.5f;
					ImGui::SetCursorPos(ImVec2(xcnt - 12.5f, 0));
					ImGui::ArrowButton("##up", ImGuiDir_Up);
					if (ImGui::IsItemActive())
						g_pane->ShiftPanel(2);
					ImGui::SetCursorPos(ImVec2(xcnt - 12.5f, 25));
					ImGui::ArrowButton("##down", ImGuiDir_Down);
					if (ImGui::IsItemActive())
						g_pane->ShiftPanel(3);
					ImGui::SetCursorPos(ImVec2(xcnt - 37.5f, 12));
					ImGui::ArrowButton("##left", ImGuiDir_Left);
					if (ImGui::IsItemActive())
						g_pane->ShiftPanel(0);
					ImGui::SetCursorPos(ImVec2(xcnt + 12.5f, 12));
					ImGui::ArrowButton("##right", ImGuiDir_Right);
					if (ImGui::IsItemActive())
						g_pane->ShiftPanel(1);
					ImGui::EndChild();
				}
				ImGui::PopID();
				ImGui::EndChild();
			}
		}
	} else if (mode == CAMERA_VIRTUALCOCKPIT) {
		ImGui::PushID("posture");
		ImGui::SeparatorText("Posture");
		if (ImGui::BeginChild("##posture", ImVec2(0, 50))) {
			ImVec2 size(60, 20);
			ImGui::SetCursorPos(ImVec2(65, 0));
			if (ImGui::Button("Forward", size))
				g_focusobj->LeanCamera(1);
			ImGui::SetCursorPos(ImVec2(0, 25));
			if (ImGui::Button("To left", size))
				g_focusobj->LeanCamera(2);
			ImGui::SetCursorPos(ImVec2(65, 25));
			if (ImGui::Button("Default", size))
				g_focusobj->LeanCamera(0);
			ImGui::SetCursorPos(ImVec2(130, 25));
			if (ImGui::Button("To right", size))
				g_focusobj->LeanCamera(3);
			ImGui::EndChild();
		}
		ImGui::PopID();
	}
}

void DlgCamera::DrawTrackMode()
{
	ImGui::PushID("track_target");
	ImGui::SeparatorText("Tracking target");
	const Body* target = g_camera->Target();
	int type = OBJTP_INVALID;
	if (target)
		type = target->Type();
	int category = 0, new_category = 0;
	switch (type) {
	case OBJTP_VESSEL:
		category = 1;
		break;
	case OBJTP_CBODY:
	case OBJTP_STAR:
	case OBJTP_PLANET:
		category = 2;
		break;
	case OBJTP_SURFBASE:
		category = 3;
		break;
	}
	const char* catName[] = { "none", "Vessel", "Celestial body", "Surface base" };
	if (BeginWideCombo("##category", catName[category], ImGuiComboFlags_HeightSmall)) {
		bool is_selected = false;
		for (int i = 1; i < 4; i++)
			if (ImGui::Selectable(catName[i], is_selected))
				new_category = i;
		ImGui::EndCombo();
	}
	if (new_category && new_category != category) {
		Body* body = nullptr;
		switch (new_category) {
		case 1:
			body = g_focusobj; // default to tracking the focus vessel
			break;
		case 2:
			body = (Body*)g_camera->ProxyPlanet(); // default to tracking the closest celestial body
			break;
		case 3:
			body = (Body*)g_camera->ProxyBase(g_camera->ProxyPlanet()); // default to tracking closest base on closest planet
			if (!body)
				body = (Body*)g_camera->ProxyBase(); // failing that, at least find the closest base on any planet
			break;
		}
		if (body)
			g_camera->Attach(body, 1);
		category = new_category;
	}

	switch (category) {
	case 1:
		DrawTrackMode_Vessel();
		break;
	case 2:
		DrawTrackMode_CBody();
		break;
	case 3:
		DrawTrackMode_Base();
		break;
	}
	ImGui::PopID();

	ImGui::PushID("track_frame");
	ImGui::SeparatorText("Camera reference frame");
	DrawTrackMode_Frame();
	ImGui::PopID();
}

void DlgCamera::DrawGroundMode()
{
	const CelestialBody *ref = g_camera->GetGroundRef();
	if (!ref)
		ref = g_camera->ProxyPlanet();
	if (!ref)
		return; // something went wrong
	m_SelectedTarget = ref;

	ImGui::PushID("ground_view");
	ImGui::SeparatorText("Reference body");
	if (BeginWideCombo("##ground_ref", ref->Name(), ImGuiComboFlags_HeightRegular)) {
		for (int i = 0; i < g_psys->nStar(); i++)
			AddCbodyNode(g_psys->GetStar(i));
		if (m_SelectedTarget && m_SelectedTarget != ref) {
			ref = (const CelestialBody*)m_SelectedTarget;
			SetGroundObserverView(ref);
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndCombo();
	}
	ImGui::PopID();
}

void DlgCamera::DrawTrackMode_Vessel()
{
	Body* body = g_camera->Target();
	if (body->Type() != OBJTP_VESSEL)
		return; // something went wrong

	Vessel* vessel = (Vessel*)body;

	if (BeginWideCombo("##vessel", vessel->Name(), ImGuiComboFlags_HeightRegular)) {
		bool is_selected = false;
		for (int i = 0; i < g_psys->nVessel(); i++) {
			Vessel* v = g_psys->GetVessel(i);
			if (ImGui::Selectable(v->Name(), is_selected)) {
				vessel = v;
				g_camera->Attach(v, 1);
			}
		}
		ImGui::EndCombo();
	}
}

void DlgCamera::DrawTrackMode_CBody()
{
	Body* body = g_camera->Target();
	m_SelectedTarget = body;
	CelestialBody * cbody = (CelestialBody*)body;
	if (BeginWideCombo("##cbody", cbody->Name(), ImGuiComboFlags_HeightRegular)) {
		for (int i = 0; i < g_psys->nStar(); i++)
			AddCbodyNode(g_psys->GetStar(i));
		if (m_SelectedTarget && m_SelectedTarget != body) {
			g_camera->Attach((Body*)m_SelectedTarget, 1);
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndCombo();
	}
}

void DlgCamera::DrawTrackMode_Base()
{
	Body* body = g_camera->Target();
	if (body->Type() != OBJTP_SURFBASE)
		return; // something went wrong

	Base* base = (Base*)body;
	m_SelectedTarget = base;

	if (BeginWideCombo("##base", base->Name(), ImGuiComboFlags_HeightRegular)) {
		for (int i = 0; i < g_psys->nStar(); i++)
			AddSurfBaseNode(g_psys->GetStar(i));
		if (m_SelectedTarget && m_SelectedTarget != base) {
			g_camera->Attach((Body*)m_SelectedTarget, 1);
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndCombo();
	}
}

void DlgCamera::DrawTrackMode_Frame()
{
	ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DefaultOpen;
	const char* frame_label[] = { "target-relative", "absolute direction", "global frame",
		"target to ...", "target from ..."};
	int mode = g_camera->GetExtMode();
	int new_mode = mode;
	if (mode < CAMERA_TARGETRELATIVE || mode > CAMERA_TARGETFROMOBJECT)
		return; // something went wrong
	if (BeginWideCombo("##trackframe", frame_label[mode - CAMERA_TARGETRELATIVE], ImGuiComboFlags_HeightRegular)) {
		bool is_selected = false;
		if (ImGui::TreeNodeEx("Movable:", node_flags)) {
			for (int i = CAMERA_TARGETRELATIVE; i <= CAMERA_GLOBALFRAME; i++) {
				ImGui::TreeNodeEx(frame_label[i - CAMERA_TARGETRELATIVE], node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
				if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
					new_mode = i;
			}
			ImGui::TreePop();
		}
		if (ImGui::TreeNodeEx("Fixed:", node_flags)) {
			for (int i = CAMERA_TARGETTOOBJECT; i <= CAMERA_TARGETFROMOBJECT; i++) {
				ImGui::TreeNodeEx(frame_label[i - CAMERA_TARGETRELATIVE], node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
				if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
					new_mode = i;
			}
			ImGui::TreePop();
		}
		if (new_mode != mode) {
			const Body* dirref = g_camera->GetDirRef();
			if (!dirref && new_mode >= CAMERA_TARGETTOOBJECT)
				dirref = g_psys->GetStar(0); // fallback
			g_camera->SetTrackMode((ExtCamMode)new_mode, dirref);
			mode = new_mode;
			ImGui::CloseCurrentPopup();
		}
		ImGui::EndCombo();
	}

	if (mode >= CAMERA_TARGETTOOBJECT) {
		const Body* dirref = g_camera->GetDirRef();
		if (!dirref)
			return; // something went wrong
		m_SelectedTarget = dirref;
		if (BeginWideCombo("##trackref", dirref->Name(), ImGuiComboFlags_HeightRegular)) {
			bool is_selected = false;
			if (ImGui::TreeNodeEx("Celestial bodies", node_flags & !ImGuiTreeNodeFlags_DefaultOpen)) {
				for (int i = 0; i < g_psys->nStar(); i++)
					AddCbodyNode(g_psys->GetStar(i));
				ImGui::TreePop();
			}
			if (ImGui::TreeNodeEx("Vessels", node_flags & !ImGuiTreeNodeFlags_DefaultOpen)) {
				for (int i = 0; i < g_psys->nVessel(); i++) {
					Vessel *vessel = g_psys->GetVessel(i);
					if (vessel != g_camera->Target()) {
						ImGui::TreeNodeEx(vessel->Name(), node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
						if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
							m_SelectedTarget = vessel;
					}
				}
				ImGui::TreePop();
			}
			if (m_SelectedTarget != dirref && m_SelectedTarget != g_camera->Target()) {
				g_camera->SetTrackMode((ExtCamMode)mode, m_SelectedTarget);
				ImGui::CloseCurrentPopup();
			}
			ImGui::EndCombo();
		}
	}
}

bool DlgCamera::SetGroundObserverView(const CelestialBody* ref)
{
	const CelestialBody *proxy = g_camera->ProxyPlanet();
	if (!ref)
		ref = g_camera->GetGroundRef();
	if (!ref)
		ref = proxy;
	if (!ref)
		return false;

	// map current camera position into ground-relative location
	double lng, lat, rad, alt;
	proxy->GlobalToEquatorial(g_camera->GPos(), lng, lat, rad);
	alt = rad - proxy->Size();
	if (alt < 0.0)
		alt = 5.0;

	g_camera->SetGroundMode(CAMERA_GROUNDOBSERVER, ref, lng, lat, alt, nullptr, false);
	g_camera->SetGroundObserver_TargetLock(g_camera->GroundObserver_TargetLock());
	return true;
}

void DlgCamera::DrawControl() {
    ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;
    ImGui::BeginChild("ChildL", ImVec2(250, 0), window_flags);
        ImGui::SeparatorText("Camera mode");

		const char *cameraMode = "Internal";
		if (extcam) {
			switch (extmode) {
			case CAMERA_TARGETRELATIVE:
				cameraMode = "Target Relative"; break;
			case CAMERA_ABSDIRECTION:
				cameraMode = "Absolute Direction"; break;
			case CAMERA_GLOBALFRAME:
				cameraMode = "Global Frame"; break;
			case CAMERA_TARGETTOOBJECT:
				cameraMode = "Target Object"; break;
			case CAMERA_TARGETFROMOBJECT:
				cameraMode = "Target from Object"; break;
			case CAMERA_GROUNDOBSERVER:
				if (ground_lock) {
					cameraMode = "Ground Observer (locked)"; break;
				} else {
					cameraMode = "Ground Observer"; break;
				}
				break;
			}
		}
		ImGui::Text("Camera mode : %s", cameraMode);

		ImVec2 pos;
		double dt = td.SysDT;

		pos.x = 50;
		pos.y = 75;
		ImGui::SetCursorPos(pos); 
		ImGui::ArrowButton("##up", ImGuiDir_Up);
		if(ImGui::IsItemActive()){
			if (rot_is_tilt) g_camera->Rotate   ( 0,  dt);
			else             g_camera->ShiftTheta (-dt);
		}

		pos.x = 50;
		pos.y = 125;
		ImGui::SetCursorPos(pos); 
		ImGui::ArrowButton("##down", ImGuiDir_Down);
		if(ImGui::IsItemActive()){
			if (rot_is_tilt) g_camera->Rotate   ( 0, -dt);
			else             g_camera->ShiftTheta ( dt);
		}

		pos.x = 25;
		pos.y = 100;
		ImGui::SetCursorPos(pos); 
		ImGui::ArrowButton("##left", ImGuiDir_Left);
		if(ImGui::IsItemActive()){
			if (rot_is_tilt) g_camera->Rotate   ( dt,  0);
			else             g_camera->ShiftPhi   (-dt);
		}

		pos.x = 75;
		pos.y = 100;
		ImGui::SetCursorPos(pos); 
		ImGui::ArrowButton("##right", ImGuiDir_Right);
		if(ImGui::IsItemActive()){
			if (rot_is_tilt) g_camera->Rotate   (-dt,  0);
			else             g_camera->ShiftPhi   ( dt);
		}

		pos.x = 35;
		pos.y = 155;
		ImGui::SetCursorPos(pos); 
		if(ImGui::Button("Forward")) {
			g_camera->ResetCockpitDir();
		}
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
        ImGui::SeparatorText("Field of view");
		float fov=oapiCameraAperture()/RAD/0.5;
		if(ImGui::SliderFloat("Field of View", &fov, 10.0, 90.0, "%.1f")) {
			if(fov < 10.0f) fov = 10.0f;
			else if(fov>90.0f) fov = 90.0f;
			oapiCameraSetAperture(fov*RAD*0.5);
    }
    ImGui::EndChild();
}

void DlgCamera::AddCbodyNode(const CelestialBody *cbody) {
    ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DefaultOpen;
    const bool is_selected = m_SelectedTarget == cbody;
    if (is_selected)
        node_flags |= ImGuiTreeNodeFlags_Selected;
    if(cbody->nSecondary()) {
        bool node_open = ImGui::TreeNodeEx(cbody->Name(), node_flags);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
            m_SelectedTarget = cbody;
        if(node_open) {
            for (int i = 0; i < cbody->nSecondary(); i++) {
                AddCbodyNode (cbody->Secondary(i));
            }
            ImGui::TreePop();
        }
    } else {
        ImGui::TreeNodeEx(cbody->Name(), node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
            m_SelectedTarget = cbody;
    }
}

bool DlgCamera::SystemHasBases(const CelestialBody *cbody)
{
	if (cbody->Type() == OBJTP_PLANET) {
		Planet* planet = (Planet*)cbody;
		if (planet->nBase())
			return true;
	}
	for (int i = 0; i < cbody->nSecondary(); i++) {
		if (SystemHasBases(cbody->Secondary(i)))
			return true;
	}
	return false;
}

void DlgCamera::AddSurfBaseNode(const CelestialBody *cbody)
{
	ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DefaultOpen;
	if (SystemHasBases(cbody)) {
		// grey out the cbody headers - bit of a hack
		ImGuiContext &g = *GImGui;
		g.DisabledAlphaBackup = g.Style.Alpha;
		g.Style.Alpha *= g.Style.DisabledAlpha; // PushStyleVar(ImGuiStyleVar_Alpha, g.Style.Alpha * g.Style.DisabledAlpha);
		bool node_open = ImGui::TreeNodeEx(cbody->Name(), node_flags);
		g.Style.Alpha = g.DisabledAlphaBackup; //PopStyleVar();
		if (node_open) {
			if (cbody->Type() == OBJTP_PLANET) {
				Planet* planet = (Planet*)cbody;
				for (int i = 0; i < planet->nBase(); i++) {
					Base* base = planet->GetBase(i);
					ImGui::TreeNodeEx(base->Name(), node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
					if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
						m_SelectedTarget = base;
				}
			}
			for (int i = 0; i < cbody->nSecondary(); i++) {
				AddSurfBaseNode(cbody->Secondary(i));
			}
			ImGui::TreePop();
		}
	}
}

void DlgCamera::DrawTarget() {
    const ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;

	ImGui::BeginChild("ChildL", ImVec2(250, 0), window_flags);
        ImGui::SeparatorText("Available targets");
		ImGui::BeginChild("ChildL_inner");
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
								Base *base = g_psys->GetBase(planet, j);
								const char *name = base->Name();
								const bool is_selected = m_SelectedTarget == base;
								ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
								if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
								ImGui::TreeNodeEx(name, node_flags);
								if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
									m_SelectedTarget = base;
							}
							ImGui::TreePop();
						}
                    }
                }
                ImGui::TreePop();
        }
        if (ImGui::TreeNode("Vessels")) {
            for (int i = 0; i < g_psys->nVessel(); i++) {
				Vessel* vessel = g_psys->GetVessel(i);
                const char *name = vessel->Name();
                const bool is_selected = m_SelectedTarget == vessel;
                ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
                if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
                ImGui::TreeNodeEx(name, node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
                if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
                    m_SelectedTarget = vessel;
            }
            ImGui::TreePop();
        }
	    ImGui::EndChild();
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
        ImGui::SeparatorText("Selected target");
        ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
        ImGui::TextUnformatted(m_SelectedTarget->Name());
        if(ImGui::Button("Apply", button_sz)) {
			Body* obj = (Body*)m_SelectedTarget;
            //Body *obj = g_psys->GetObj (m_SelectedTarget->Name(), true);
            //if (!obj) obj = g_psys->GetBase (m_SelectedTarget->Name(), true);
            if ( obj) g_pOrbiter->SetView (obj, 1);
        }
        if(ImGui::Button("Focus Cockpit", button_sz)) {
            g_pOrbiter->SetView (g_focusobj, 0);
        }
        if(ImGui::Button("Focus Extern", button_sz)) {
            g_pOrbiter->SetView (g_focusobj, 1);
        }
    ImGui::EndChild();
}
void DlgCamera::DrawTrack() {
    ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;
    ImGui::BeginChild("ChildL", ImVec2(250, 0), window_flags);
    {
        ImGui::SeparatorText("Moveable modes");
        ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));

        if(ImGui::Button("Target Relative", button_sz)) {
            g_camera->SetTrackMode (CAMERA_TARGETRELATIVE);
        }
        if(ImGui::Button("Absolute Direction", button_sz)) {
            g_camera->SetTrackMode (CAMERA_ABSDIRECTION);
        }
        if(ImGui::Button("Global Frame", button_sz)) {
            g_camera->SetTrackMode (CAMERA_GLOBALFRAME);
        }
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
        ImGui::SeparatorText("Fixed modes");
        ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));

        if(ImGui::Button("Target From...", button_sz)) {
			Body* obj = (Body*)m_SelectedTarget;
			//Body *obj = g_psys->GetObj (m_SelectedTarget.c_str(), true);
			//if (!obj) obj = g_psys->GetBase (m_SelectedTarget.c_str(), true);
			if (obj && obj != g_camera->Target())
				g_camera->SetTrackMode (CAMERA_TARGETFROMOBJECT, obj);
        }
        if(ImGui::Button("Target To...", button_sz)) {
			Body* obj = (Body*)m_SelectedTarget;
			//Body *obj = g_psys->GetObj (m_SelectedTarget.c_str(), true);
			//if (!obj) obj = g_psys->GetBase (m_SelectedTarget.c_str(), true);
			if (obj && obj != g_camera->Target())
				g_camera->SetTrackMode (CAMERA_TARGETTOOBJECT, obj);
        }
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
							Base* base = g_psys->GetBase(planet, j);
							const char *name = base->Name();
							const bool is_selected = m_SelectedTarget == base;
							ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
							if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
							ImGui::TreeNodeEx(name, node_flags);
							if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
								m_SelectedTarget = base;
						}
			            ImGui::TreePop();
					}
                }
            }
            ImGui::TreePop();
        }
        if (ImGui::TreeNode("Vessels")) {
            for (int i = 0; i < g_psys->nVessel(); i++) {
				Vessel* vessel = g_psys->GetVessel(i);
                const char *name = vessel->Name();
                const bool is_selected = m_SelectedTarget == vessel;
                ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
                if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
                ImGui::TreeNodeEx(name, node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
                if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
                    m_SelectedTarget = vessel;
            }
            ImGui::TreePop();
        }
    ImGui::EndChild();
}
void DlgCamera::DrawGround() {
    const ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;;
    ImGui::BeginChild("ChildL", ImVec2(250, 0), window_flags);
    {
        ImGui::SeparatorText("Ground Location");
        for (int i = 0; i < g_psys->nGrav(); i++) {
            Body *obj = g_psys->GetGravObj (i);
            if (obj->Type() != OBJTP_PLANET) continue;
            Planet *planet = (Planet*)obj;


            if (planet->nGroundObserver() > 0) {
				if(ImGui::TreeNode(planet->Name())) {
					for (int j = 0; j < planet->nGroundObserver(); j++) {
						const GROUNDOBSERVERSPEC *go = planet->GetGroundObserver (j);
						std::string name = std::string(go->site) + "-" + go->addr;
						const bool is_selected = m_SelectedSite == name;
						ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
						if(is_selected) node_flags |= ImGuiTreeNodeFlags_Selected;
						ImGui::TreeNodeEx(name.c_str(), node_flags);
						if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen()) {
							m_SelectedSite = name;
							m_SitePlanet = planet->Name();
							sprintf(longitude, "%+0.6f", go->lng * 180.0/PI);
							sprintf(latitude, "%+0.6f", go->lat * 180.0/PI);
							sprintf(altitude, "%0.4g", go->alt);
						}
					}
					ImGui::TreePop();
				}
            }
        }
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
        ImGui::SeparatorText("Coordinates");
        ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
        ImGui::InputText("Longitude", longitude, 64, ImGuiInputTextFlags_CharsDecimal);
        ImGui::InputText("Latitude",  latitude,  64, ImGuiInputTextFlags_CharsDecimal);
        ImGui::InputText("Altitude",  altitude,  64, ImGuiInputTextFlags_CharsDecimal);

        ImGui::InputText("Follow Terrain",  followterrain,  64, ImGuiInputTextFlags_CharsDecimal);
        
        if(ImGui::Button("Current", button_sz)) {
            const Planet *planet = g_psys->GetPlanet (m_SitePlanet.c_str());
            if(!planet) {
                planet = g_camera->ProxyPlanet();
                m_SitePlanet = planet->Name();
            }
            if (planet) {
                double lng, lat, alt;
                planet->GlobalToEquatorial (*g_camera->GPosPtr(), lng, lat, alt);
                alt -= planet->Size() + planet->Elevation (lng, lat);
                sprintf(longitude, "%+0.6f", lng * 180.0/PI);
                sprintf(latitude, "%+0.6f", lat * 180.0/PI);
                sprintf(altitude, "%0.4g", alt);
            }
            SetCurrentGroundpos();
        }
        if(ImGui::Button("Apply", button_sz)) {
            ApplyObserver();
        }

        if(ImGui::Checkbox("Target lock", &m_TargetLock))
			g_camera->SetGroundObserver_TargetLock (m_TargetLock);

    ImGui::EndChild();
}

void DlgCamera::ApplyObserver() {
	Planet *planet = g_psys->GetPlanet (m_SitePlanet.c_str());
	if (!planet) return;

	double lng, lat, alt = 1.7;
	bool ok;
	ok = (sscanf (longitude, "%lf", &lng) == 1);
	ok = ok && (sscanf (latitude, "%lf", &lat) == 1);
	ok = ok && (sscanf (altitude, "%lf", &alt) == 1);

	double altlimit;
	if (sscanf (followterrain, "%lf", &altlimit))
		g_camera->SetGroundObserver_TerrainLimit (std::max(1.0,altlimit));

	if (ok)
		g_camera->SetGroundMode (CAMERA_GROUNDOBSERVER, planet, lng*RAD, lat*RAD, alt, NULL);

}

void DlgCamera::SetCurrentGroundpos() {
	if (g_camera->GetExtMode() == CAMERA_GROUNDOBSERVER) return; // nothing to do
	ApplyObserver ();

	if (!m_TargetLock) {
		// force lock to target to initialise camera direction
		g_camera->SetGroundObserver_TargetLock (true);
		g_camera->SetGroundObserver_TargetLock (false);
	}
	double alt;
	if (sscanf (followterrain, "%lf", &alt))
		g_camera->SetGroundObserver_TerrainLimit (std::max(1.0,alt));
}

void DlgCamera::DrawPreset() {
    ImGuiWindowFlags window_flags = ImGuiChildFlags_ResizeX;;;
    static float sz1 = 0.0;
    float sz2;
   // ImGui::Splitter(true, 0.5f, 8.0f, &sz1, &sz2, 8, 8, ImGui::GetContentRegionAvail().y);
    ImGui::BeginChild("ChildL", ImVec2(250, 0), window_flags);
    {
        if (ImGui::BeginListBox("##listbox preset", ImVec2(ImGui::GetContentRegionAvail().x*0.99, ImGui::GetContentRegionAvail().y*0.99)))
        {
            for (int i = 0; i < g_camera->nPreset(); i++) {
                char buf[256];
                g_camera->GetPreset(i)->GetDescr (buf, 256);

                const bool is_selected = (m_SelectedPreset == i);
                ImGui::PushID(i);
                if (ImGui::Selectable(buf, is_selected))
                    m_SelectedPreset = i;
                ImGui::PopID();
                // Set the initial focus when opening the combo (scrolling + keyboard navigation focus)
                if (is_selected)
                    ImGui::SetItemDefaultFocus();
            }
            ImGui::EndListBox();
            //ImGui::Text("Selected %d", m_SelectedPreset);
        }
    }
    ImGui::EndChild();
    ImGui::SameLine();
    ImGui::BeginChild("ChildR");
        ImVec2 button_sz(ImVec2(ImGui::GetContentRegionAvail().x, 20));
        if(ImGui::Button("Add", button_sz)) {
            g_camera->AddPreset();
        }
        if(ImGui::Button("Delete", button_sz)) {
            g_camera->DelPreset(m_SelectedPreset);
        }
        if(ImGui::Button("Clear", button_sz)) {
            g_camera->ClearPresets();
        }
        if(ImGui::Button("Recall", button_sz)) {
            g_camera->RecallPreset (m_SelectedPreset);
        }

    ImGui::EndChild();

}

bool DlgCamera::BeginWideCombo(const char* label, const char* preview_value, ImGuiComboFlags flags)
{
	ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
	return ImGui::BeginCombo(label, preview_value, flags);
}
