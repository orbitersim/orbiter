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
}

void DlgCamera::DrawMode()
{
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
				ImGui::BeginDisabled(!has_neighbour[2]);
				if (ImGui::ArrowButton("##up", ImGuiDir_Up))
					g_pane->SwitchPanel(2);
				ImGui::EndDisabled();
				ImGui::SetCursorPos(ImVec2(xcnt - 12.5f, 25));
				ImGui::BeginDisabled(!has_neighbour[3]);
				if (ImGui::ArrowButton("##down", ImGuiDir_Down))
					g_pane->SwitchPanel(3);
				ImGui::EndDisabled();
				ImGui::SetCursorPos(ImVec2(xcnt - 37.5f, 12));
				ImGui::BeginDisabled(!has_neighbour[0]);
				if (ImGui::ArrowButton("##left", ImGuiDir_Left))
					g_pane->SwitchPanel(0);
				ImGui::EndDisabled();
				ImGui::SetCursorPos(ImVec2(xcnt + 12.5f, 12));
				ImGui::BeginDisabled(!has_neighbour[1]);
				if (ImGui::ArrowButton("##right", ImGuiDir_Right))
					g_pane->SwitchPanel(1);
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

	if (ObjectSelector("##tracktgt", target, OBJ_ALL, 0, ImGuiComboFlags_HeightRegular)) {
		if (m_SelectedTarget && m_SelectedTarget != target) {
			g_camera->Attach((Body*)m_SelectedTarget, 1);
		}
	}

	ImGui::PopID();


	ImGui::PushID("track_frame");
	ImGui::SeparatorText("Camera reference frame");

	int mode = g_camera->GetExtMode();
	int new_mode = mode;
	if (mode >= CAMERA_TARGETRELATIVE || mode <= CAMERA_TARGETFROMOBJECT) {
		const char* frame_label[] = { "target-relative", "absolute direction", "global frame",
			"target to ...", "target from ..." };
		if (BeginWideCombo("##trackframe", frame_label[mode - CAMERA_TARGETRELATIVE], ImGuiComboFlags_HeightRegular)) {
			ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_DefaultOpen;
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

			if (ObjectSelector("##trackref", dirref, OBJ_ALL, SEL_EXCLUDE_CAMTGT, ImGuiComboFlags_HeightRegular)) {
				if (m_SelectedTarget != dirref && m_SelectedTarget != g_camera->Target()) {
					g_camera->SetTrackMode((ExtCamMode)mode, m_SelectedTarget);
					ImGui::CloseCurrentPopup();
				}
			}
		}
	}

	ImGui::PopID();
}

void DlgCamera::DrawGroundMode()
{
	const CelestialBody *ref = g_camera->GetGroundRef();
	if (!ref)
		ref = g_camera->ProxyPlanet();
	if (!ref || ref->Type() != OBJTP_PLANET)
		return; // something went wrong

	ImGui::PushID("ground_view");
	ImGui::SeparatorText("Reference body");

	if (ObjectSelector("##ground_ref", ref, OBJ_CELBODY, SEL_NOCAT | SEL_ALLOPEN, ImGuiComboFlags_HeightRegular)) {
		if (m_SelectedTarget && m_SelectedTarget != ref) {
			ref = (const CelestialBody*)m_SelectedTarget;
			SetGroundObserverView(ref);
		}
	}

	ImGui::PopID();


	ImGui::PushID("location");
	ImGui::SeparatorText("Location");

	double lng, lat, alt;
	static double plng = NAN, plat = NAN, palt = NAN;
	static bool modified = false;
	g_camera->GroundObserverLocation(lng, lat, alt);
	if (lng != plng) {
		sprintf(longitude, "%+0.6f", lng * DEG);
		modified = false;
		plng = lng;
	}
	if (lat != plat) {
		sprintf(latitude, "%+0.6f", lat * DEG);
		modified = false;
		plat = lat;
	}
	if (alt != palt) {
		sprintf(altitude, "%0.5g", alt);
		modified = false;
		palt = alt;
	}

	ImGui::PushItemWidth(std::min(std::max(ImGui::GetContentRegionAvail().x - 90.0f, 50.0f), 200.0f));
	if (ImGui::InputText("Longitude [°]", longitude, 64, ImGuiInputTextFlags_CharsDecimal))
		modified = true;
	if (ImGui::InputText("Latitude [°]", latitude, 64, ImGuiInputTextFlags_CharsDecimal))
		modified = true;
	if (ImGui::InputText("Altitude [m]", altitude, 64, ImGuiInputTextFlags_CharsDecimal))
		modified = true;
	ImGui::PopItemWidth();

	float button_w = std::min(100.0f, ImGui::GetContentRegionAvail().x);
	ImVec2 button_sz(ImVec2(button_w, 20));
	ImGui::BeginDisabled(!modified);
	if (ImGui::Button("Apply", button_sz)) {
		double deg_lng, deg_lat, _alt;
		if (sscanf(longitude, "%lf", &deg_lng) == 1 &&
			sscanf(latitude, "%lf", &deg_lat) == 1 &&
			sscanf(altitude, "%lf", &_alt) == 1) {
			g_camera->SetGroundMode(CAMERA_GROUNDOBSERVER, g_camera->GroundObserverReference(), deg_lng * RAD, deg_lat * RAD, _alt, NULL);
		}
	}
	ImGui::EndDisabled();

	ImGui::PopID();


	ImGui::PushID("tgt_lock");
	ImGui::SeparatorText("View target");

	bool tgtlock = g_camera->GroundObserver_TargetLock();
	if (ImGui::Checkbox("Lock view on target", &tgtlock))
		g_camera->SetGroundObserver_TargetLock(tgtlock);


	ImGui::BeginDisabled(!tgtlock);
	const Body *viewtgt = g_camera->GroundObserver_ViewTarget();
	if (ObjectSelector("##view_tgt", viewtgt, OBJ_ALL, 0, ImGuiComboFlags_HeightRegular)) {
		if (m_SelectedTarget && m_SelectedTarget != viewtgt) {
			g_camera->SetGroundObserver_ViewTarget(m_SelectedTarget);
		}
	}
	ImGui::EndDisabled();

	ImGui::PopID();
}

void DlgCamera::DrawControl()
{
	char label[64];
	double dt = td.SysDT;

	bool have_move = (extcam && extmode == CAMERA_GROUNDOBSERVER);
	bool have_dist = (extcam && extmode != CAMERA_GROUNDOBSERVER);
	bool have_pane2 = (have_move || have_dist);
	bool have_fwdbt = !extcam;
	bool disable_rot = ((extcam && (extmode == CAMERA_TARGETTOOBJECT || extmode == CAMERA_TARGETFROMOBJECT)) ||
		                (extcam && extmode == CAMERA_GROUNDOBSERVER && g_camera->GroundObserver_TargetLock()));
	float ctrlw = std::max((have_pane2 ? ImGui::GetContentRegionAvail().x * 0.5f - 5.0f : ImGui::GetContentRegionAvail().x), 75.0f);

	ImGui::PushID("ctrl_rot");
	ImGuiWindowFlags window_flags = 0;  // ImGuiChildFlags_ResizeX;
	ImGui::BeginChild("ChildL", ImVec2(ctrlw, 150), window_flags);
	sprintf(label, "%s view", rot_is_tilt ? "Tilt" : "Rotate");
	ImGui::SeparatorText(label);

	ImGui::BeginDisabled(disable_rot);
	ImVec2 cnt(ImGui::GetContentRegionAvail().x/2 - 10, 75);
	ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y - 25));
	ImGui::ArrowButton("##up", ImGuiDir_Up);
	if (ImGui::IsItemActive()) {
		if (rot_is_tilt) g_camera->Rotate(0, dt);
		else             g_camera->ShiftTheta(-dt);
	}
	ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y + 25));
	ImGui::ArrowButton("##down", ImGuiDir_Down);
	if (ImGui::IsItemActive()) {
		if (rot_is_tilt) g_camera->Rotate(0, -dt);
		else             g_camera->ShiftTheta(dt);
	}
	ImGui::SetCursorPos(ImVec2(cnt.x - 25, cnt.y));
	ImGui::ArrowButton("##left", ImGuiDir_Left);
	if (ImGui::IsItemActive()) {
		if (rot_is_tilt) g_camera->Rotate(dt, 0);
		else             g_camera->ShiftPhi(-dt);
	}
	ImGui::SetCursorPos(ImVec2(cnt.x + 25, cnt.y));
	ImGui::ArrowButton("##right", ImGuiDir_Right);
	if (ImGui::IsItemActive()) {
		if (rot_is_tilt) g_camera->Rotate(-dt, 0);
		else             g_camera->ShiftPhi(dt);
	}
	ImGui::EndDisabled();
	if (have_fwdbt) {
		ImGui::SetCursorPos(ImVec2(cnt.x - 20, cnt.y + 50));
		ImGui::Button("Forward", ImVec2(60, 20));
		if (ImGui::IsItemActive())
			g_camera->ResetCockpitDir();
	}
	ImGui::EndChild();

	if (have_move) {
		ImGui::SameLine();
		ImGui::BeginChild("ChildR", ImVec2(ctrlw, 150), window_flags);
		ImGui::SeparatorText("Move view");
		ImVec2 cnt(ImGui::GetContentRegionAvail().x / 2 - 10, 75);
		ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y - 25));
		ImGui::ArrowButton("##up", ImGuiDir_Up);
		if (ImGui::IsItemActive())
			g_camera->ShiftTheta(-dt);
		ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y + 25));
		ImGui::ArrowButton("##down", ImGuiDir_Down);
		if (ImGui::IsItemActive())
			g_camera->ShiftTheta(dt);
		ImGui::SetCursorPos(ImVec2(cnt.x - 25, cnt.y));
		ImGui::ArrowButton("##left", ImGuiDir_Left);
		if (ImGui::IsItemActive())
			g_camera->AddPhi(-dt);
		ImGui::SetCursorPos(ImVec2(cnt.x + 25, cnt.y));
		ImGui::ArrowButton("##right", ImGuiDir_Right);
		if (ImGui::IsItemActive())
			g_camera->AddPhi(dt);
		ImGui::SetCursorPos(ImVec2(cnt.x - 10, cnt.y - 50));
		ImGui::Button("Up", ImVec2(40, 20));
		if (ImGui::IsItemActive())
			g_camera->ChangeDist(1.0 + dt);
		ImGui::SetCursorPos(ImVec2(cnt.x - 10, cnt.y + 50));
		ImGui::Button("Down", ImVec2(40, 20));
		if (ImGui::IsItemActive())
			g_camera->ChangeDist(1.0 / (1.0 + dt));
		ImGui::EndChild();

	} else if (have_dist) {

		ImGui::SameLine();
		ImGui::BeginChild("ChildR", ImVec2(ctrlw, 150), window_flags);
		ImGui::SeparatorText("Distance");

		strcpy(distance, DistStr(g_camera->Distance()) + 1);
		ImGui::SetNextItemWidth(std::max(ImGui::GetContentRegionAvail().x - 71.0f, 10.0f));
		if (ImGui::InputText("m", distance, 64, ImGuiInputTextFlags_EnterReturnsTrue)) {
			double dst;
			if (sscanf(distance, "%lf", &dst) == 1) {
				// scan for magnitude suffix
				switch (distance[strlen(distance) - 1]) {
				case 'k':
					dst *= 1e3;
					break;
				case 'M':
					dst *= 1e6;
					break;
				case 'G':
					dst *= 1e9;
					break;
				}
				g_camera->ChangeDist(dst / g_camera->Distance());
			}
		}
		ImGui::SameLine();
		ImGui::ArrowButton("##down", ImGuiDir_Down);
		if (ImGui::IsItemActive())
			g_camera->ChangeDist(1.0 / (1.0 + td.SysDT));
		ImGui::SameLine();
		ImGui::ArrowButton("##up", ImGuiDir_Up);
		if (ImGui::IsItemActive())
			g_camera->ChangeDist(1.0 + td.SysDT);

		ImGui::EndChild();
	}

	ImGui::PopID();


	ImGui::PushID("fov");

	bool isCockpit = g_camera->GetMode() == CAM_COCKPIT;
	sprintf(label, "Field of view [%s]", isCockpit ? "Cockpit" : "External");
	ImGui::SeparatorText(label);

	float fov = oapiCameraAperture() * DEG * 2.0;
	ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x);
	if (ImGui::SliderFloat("", &fov, 10.0, 90.0, "%.1f")) {
		if (fov < 10.0f) fov = 10.0f;
		else if (fov > 90.0f) fov = 90.0f;
		oapiCameraSetAperture(fov * RAD * 0.5);
	}

	ImGui::PopID();
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

#ifdef UNDEF
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
#endif

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
					const bool is_selected = m_SelectedTarget == base;
					ImGuiTreeNodeFlags leaf_flags = node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
					if (is_selected)
						leaf_flags |= ImGuiTreeNodeFlags_Selected;
					ImGui::TreeNodeEx(base->Name(), leaf_flags);
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
	ImGuiWindowFlags window_flags = 0; // ImGuiChildFlags_ResizeX;;;
    //static float sz1 = 0.0;
    //float sz2;
   // ImGui::Splitter(true, 0.5f, 8.0f, &sz1, &sz2, 8, 8, ImGui::GetContentRegionAvail().y);
	float listw = std::max(ImGui::GetContentRegionAvail().x - 80.0f, 10.0f);
    ImGui::BeginChild("ChildL", ImVec2(listw, 0), window_flags);
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

bool DlgCamera::ObjectSelector(const char* label, const Body *body, DWORD objtypes, DWORD selector_flags, ImGuiComboFlags flags)
{
	m_SelectedTarget = body;
	bool showcat = !(selector_flags & SEL_NOCAT);

	ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_SpanAvailWidth;
	if (selector_flags & SEL_ALLOPEN)
		node_flags |= ImGuiTreeNodeFlags_DefaultOpen;

	if (BeginWideCombo(label, body ? body->Name() : "", flags)) {

		if (objtypes & OBJ_VESSEL) {
			if (showcat ? ImGui::TreeNodeEx("Vessels", node_flags) : 1) {
				for (int i = 0; i < g_psys->nVessel(); i++) {
					Vessel *vessel = g_psys->GetVessel(i);
					if (vessel != g_camera->Target() || !(selector_flags & SEL_EXCLUDE_CAMTGT)) {
						ImGuiTreeNodeFlags leaf_flags = node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;
						if (vessel == m_SelectedTarget)
							leaf_flags |= ImGuiTreeNodeFlags_Selected;
						ImGui::TreeNodeEx(vessel->Name(), leaf_flags);
						if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
							m_SelectedTarget = vessel;
					}
				}
				if (showcat) ImGui::TreePop();
			}
		}

		if (objtypes & OBJ_CELBODY) {
			if (showcat ? ImGui::TreeNodeEx("Celestial bodies", node_flags) : 1) {
				for (int i = 0; i < g_psys->nStar(); i++)
					AddCbodyNode(g_psys->GetStar(i));
				if (showcat) ImGui::TreePop();
			}
		}

		if (objtypes & OBJ_SURFBASE) {
			if (showcat ? ImGui::TreeNodeEx("Surface bases", node_flags) : 1) {
				for (int i = 0; i < g_psys->nStar(); i++)
					AddSurfBaseNode(g_psys->GetStar(i));
				if (showcat) ImGui::TreePop();
			}
		}

		if (m_SelectedTarget != body)
			ImGui::CloseCurrentPopup();

		ImGui::EndCombo();

		return true;
	}
	return false;
}
