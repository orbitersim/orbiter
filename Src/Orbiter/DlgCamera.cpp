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

DlgCamera::DlgCamera() : ImGuiDialog(ICON_FA_VIDEO " Orbiter: Camera", {280,359}) {
    m_SelectedPreset = -1;
	m_Lng = m_Lat = m_Alt = NAN;
	m_PosEdited = false;
}

void DlgCamera::OnDraw() {
	extmode = g_camera->GetExtMode ();
	intmode = g_camera->GetIntMode ();
	extcam = (g_camera->GetMode () != CAM_COCKPIT);
	ground_lock = g_camera->GroundObserver_TargetLock();
	rot_is_tilt = (!extcam || (extmode == CAMERA_GROUNDOBSERVER && !ground_lock));
	rot_is_pan = (extcam && extmode == CAMERA_GROUNDOBSERVER);
    
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
	const char* mode_label[] = { "Cockpit", "Track", "Ground" };
	const char *curr_mode = (extcam ? extmode == CAMERA_GROUNDOBSERVER ? mode_label[2] : mode_label[1] : mode_label[0]);

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
	Spacer();

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
	Spacer();

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
	Spacer();

	if (mode == CAMERA_2DPANELCOCKPIT) {
		float ctrlw = std::max(ImGui::GetContentRegionAvail().x * 0.5f - 5.0f, 75.0f);
		if (ImGui::BeginChild("ChildL", ImVec2(ctrlw, 0))) {
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
			ImVec2 cnt(ImGui::GetContentRegionAvail().x / 2 - 10, 50);
			ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y - 25));
			ImGui::BeginDisabled(!has_neighbour[2]);
			if (ImGui::ArrowButton("##up", ImGuiDir_Up))
				g_pane->SwitchPanel(2);
			ImGui::EndDisabled();
			ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y + 25));
			ImGui::BeginDisabled(!has_neighbour[3]);
			if (ImGui::ArrowButton("##down", ImGuiDir_Down))
				g_pane->SwitchPanel(3);
			ImGui::EndDisabled();
			ImGui::SetCursorPos(ImVec2(cnt.x - 25, cnt.y));
			ImGui::BeginDisabled(!has_neighbour[0]);
			if (ImGui::ArrowButton("##left", ImGuiDir_Left))
				g_pane->SwitchPanel(0);
			ImGui::EndDisabled();
			ImGui::SetCursorPos(ImVec2(cnt.x + 25, cnt.y));
			ImGui::BeginDisabled(!has_neighbour[1]);
			if (ImGui::ArrowButton("##right", ImGuiDir_Right))
				g_pane->SwitchPanel(1);
			ImGui::EndDisabled();

			ImGui::PopID();
		}
		ImGui::EndChild();
		// Note: According to ImGui documentation, EndChild must be called regardless of BeginChild result,
		// but this behaviour might change in the future - keep track of this!

		ImGui::SameLine();

		if (ImGui::BeginChild("ChildR", ImVec2(ctrlw, 0))) {
			ImGui::PushID("scroll_panel");
			ImGui::SeparatorText("Scroll panel");
			ImVec2 cnt(ImGui::GetContentRegionAvail().x / 2 - 10, 50);
			ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y - 25));
			ImGui::ArrowButton("##up", ImGuiDir_Up);
			if (ImGui::IsItemActive())
				g_pane->ShiftPanel(2);
			ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y + 25));
			ImGui::ArrowButton("##down", ImGuiDir_Down);
			if (ImGui::IsItemActive())
				g_pane->ShiftPanel(3);
			ImGui::SetCursorPos(ImVec2(cnt.x - 25, cnt.y));
			ImGui::ArrowButton("##left", ImGuiDir_Left);
			if (ImGui::IsItemActive())
				g_pane->ShiftPanel(0);
			ImGui::SetCursorPos(ImVec2(cnt.x + 25, cnt.y));
			ImGui::ArrowButton("##right", ImGuiDir_Right);
			if (ImGui::IsItemActive())
				g_pane->ShiftPanel(1);
			ImGui::PopID();
		}
		ImGui::EndChild();
		// Note: According to ImGui documentation, EndChild must be called regardless of BeginChild result,
		// but this behaviour might change in the future - keep track of this!
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
	Spacer();


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
			ImGuiContext &g = *GImGui;
			g.DisabledAlphaBackup = g.Style.Alpha;
			g.Style.Alpha *= g.Style.DisabledAlpha;
			bool node_open = ImGui::TreeNodeEx("Movable:", node_flags);
			g.Style.Alpha = g.DisabledAlphaBackup;
			if (node_open) {
				for (int i = CAMERA_TARGETRELATIVE; i <= CAMERA_GLOBALFRAME; i++) {
					ImGui::TreeNodeEx(frame_label[i - CAMERA_TARGETRELATIVE], node_flags | ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen);
					if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen())
						new_mode = i;
				}
				ImGui::TreePop();
			}
			g.Style.Alpha *= g.Style.DisabledAlpha;
			node_open = ImGui::TreeNodeEx("Fixed:", node_flags);
			g.Style.Alpha = g.DisabledAlphaBackup;
			if (node_open) {
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
	Spacer();

	ImGui::PushID("location");
	ImGui::SeparatorText("Location");

	double lng, lat, alt;
	g_camera->GroundObserverLocation(lng, lat, alt);
	if (lng != m_Lng) {
		sprintf(longitude, "%+0.6f", lng * DEG);
		m_PosEdited = false;
		m_Lng = lng;
	}
	if (lat != m_Lat) {
		sprintf(latitude, "%+0.6f", lat * DEG);
		m_PosEdited = false;
		m_Lat = lat;
	}
	if (alt != m_Alt) {
		sprintf(altitude, "%0.5g", alt);
		m_PosEdited = false;
		m_Alt = alt;
	}

	ImGui::PushItemWidth(std::min(std::max(ImGui::GetContentRegionAvail().x - 90.0f, 50.0f), 200.0f));
	if (ImGui::InputText("Longitude [°]", longitude, 64, ImGuiInputTextFlags_CharsDecimal))
		m_PosEdited = true;
	if (ImGui::InputText("Latitude [°]", latitude, 64, ImGuiInputTextFlags_CharsDecimal))
		m_PosEdited = true;
	if (ImGui::InputText("Altitude [m]", altitude, 64, ImGuiInputTextFlags_CharsDecimal))
		m_PosEdited = true;
	ImGui::PopItemWidth();

	float button_w = std::min(100.0f, ImGui::GetContentRegionAvail().x);
	ImVec2 button_sz(ImVec2(button_w, 20));
	ImGui::BeginDisabled(!m_PosEdited);
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
	Spacer();


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
	bool have_fwdbt = !extcam;
	bool have_posture = (!extcam && g_camera->GetIntMode() == CAMERA_VIRTUALCOCKPIT);
	bool disable_rot = ((extcam && (extmode == CAMERA_TARGETTOOBJECT || extmode == CAMERA_TARGETFROMOBJECT)) ||
		                (extcam && extmode == CAMERA_GROUNDOBSERVER && g_camera->GroundObserver_TargetLock()));

	ImGuiStyle& style = ImGui::GetStyle();

	ImGui::BeginChild("CtrlPanel", ImVec2(ImGui::GetContentRegionAvail().x, 100.0f));
	float ctrlw = std::max(ImGui::GetContentRegionAvail().x * 0.5f - 4.0f, 75.0f);

	ImGui::PushID("ctrl_rot");
	ImGuiWindowFlags window_flags = 0;  // ImGuiChildFlags_ResizeX;
	ImGui::BeginChild("ChildL", ImVec2(ctrlw, 0), window_flags);
	ImGui::SeparatorText(rot_is_tilt ? "Tilt" : "Rotate");

	ImGui::BeginDisabled(disable_rot);
	ImVec2 cnt(ImGui::GetContentRegionAvail().x/2 - 10, 50);
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
		ImGui::BeginDisabled(g_camera->IsCockpitDefaultDir());
		ImGui::SetCursorPos(ImVec2(cnt.x, cnt.y));
		ImGui::Button("X", ImVec2(20, 20));
		if (ImGui::IsItemActive())
			g_camera->ResetCockpitDir();
		ImGui::EndDisabled();
	}
	ImGui::EndChild();

	if (have_move) {
		ImGui::SameLine();
		ImGui::BeginChild("ChildR", ImVec2(ctrlw, 0), window_flags);
		ImGui::SeparatorText("Move");
		ImVec2 cnt(std::max(ImGui::GetContentRegionAvail().x * 0.5f - 35.0f, 25.0f), 50);
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
		ImGui::SetCursorPos(ImVec2(cnt.x + 50, cnt.y - 25));
		ImGui::Button("Up", ImVec2(40, 20));
		if (ImGui::IsItemActive())
			g_camera->ChangeDist(1.0 + dt);
		ImGui::SetCursorPos(ImVec2(cnt.x + 50, cnt.y + 25));
		ImGui::Button("Down", ImVec2(40, 20));
		if (ImGui::IsItemActive())
			g_camera->ChangeDist(1.0 / (1.0 + dt));
		ImGui::EndChild();

	} else if (have_dist) {

		ImGui::SameLine();
		ImGui::BeginChild("ChildR", ImVec2(ctrlw, 0), window_flags);
		ImGui::SeparatorText("Move");
		ImVec2 cnt(ImGui::GetContentRegionAvail().x / 2 - 10, 50);

		strcpy(distance, DistStr(g_camera->Distance()) + 1);
		ImGui::SetCursorPos(ImVec2(0, cnt.y - 20));
		ImGui::Text("Target distance [m]:");
		ImGui::SetCursorPos(ImVec2(0, cnt.y));
		ImGui::SetNextItemWidth(std::max(ImGui::GetContentRegionAvail().x - 56.0f, 10.0f));
		if (ImGui::InputText("##dst", distance, 64, ImGuiInputTextFlags_EnterReturnsTrue)) {
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

	} else if (have_posture) {

		ImGui::SameLine();
		ImGui::BeginChild("ChildR", ImVec2(ctrlw, 0), window_flags);
		ImGui::SeparatorText("Posture");
		ImVec2 cnt(ImGui::GetContentRegionAvail().x / 2, 50);
		ImVec2 size1(60, 20), size2(50, 20);
		ImGui::SetCursorPos(ImVec2(cnt.x - 30, cnt.y - 25));
		if (ImGui::Button("Forward", size1))
			g_focusobj->LeanCamera(1);
		ImGui::SetCursorPos(ImVec2(cnt.x - 52.5, cnt.y));
		if (ImGui::Button("Left", size2))
			g_focusobj->LeanCamera(2);
		ImGui::SetCursorPos(ImVec2(cnt.x + 2.5, cnt.y));
		if (ImGui::Button("Right", size2))
			g_focusobj->LeanCamera(3);
		ImGui::SetCursorPos(ImVec2(cnt.x - 30, cnt.y + 25));
		if (ImGui::Button("Default", size1))
			g_focusobj->LeanCamera(0);

		ImGui::EndChild();

	} else { // dummy

		ImGui::SameLine();
		ImGui::BeginChild("ChildR", ImVec2(ctrlw, 0), window_flags);
		ImGui::SeparatorText("");
		ImGui::EndChild();

	}

	ImGui::PopID();
	ImGui::EndChild();


	// speed slider
	if (have_move) {
		const double min_speed = 1.0, max_speed = 1e10;
		double speed = g_camera->GroundObserver_PanSpeed();
		double pspeed = speed;
		ImGui::PushID("##speed");
		ImGui::SeparatorText("Speed");

		ImGui::PushItemFlag(ImGuiItemFlags_ButtonRepeat, true);
		if (ImGui::ArrowButton("##left", ImGuiDir_Left)) {
			double logspd = log10(speed);
			double step = pow(10, ceil(logspd) - 1);
			speed = std::max(min_speed, speed - step);
			speed = (ceil)(speed / step) * step;  // rounding
		}
		ImGui::SameLine();
		ImVec2 label_sz = ImGui::CalcTextSize("[m/s]");
		ImGui::SetNextItemWidth(ImGui::GetContentRegionAvail().x - label_sz.x - style.FramePadding.x * 4.0f - 20);
		float fspeed = (float)speed;
		// convert this to SliderDouble once ImGui supports logarithmic mode for them
		if (ImGui::SliderFloat("", &fspeed, min_speed, max_speed, "% .3g", ImGuiSliderFlags_Logarithmic | ImGuiSliderFlags_NoRoundToFormat))
			speed = fspeed;
		ImGui::SameLine();

		if (ImGui::ArrowButton("##right", ImGuiDir_Right)) {
			double logspd = log10(speed);
			double step = pow(10, floor(logspd));
			speed = std::min(max_speed, speed + step);
			speed = (floor)(speed / step) * step;  // rounding
		}

		ImGui::SameLine();
		ImGui::Text("[m/s]");

		if (speed != pspeed)
			g_camera->SetGroundObserver_PanSpeed(speed);
		ImGui::PopItemFlag();

		ImGui::PopID();
		Spacer();
	}


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

void DlgCamera::DrawPreset() {
	ImGuiWindowFlags window_flags = 0; // ImGuiChildFlags_ResizeX;;;
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
				if (ImGui::Selectable(buf, is_selected)) {
					m_SelectedPreset = i;
					g_camera->RecallPreset(m_SelectedPreset);
				}
                ImGui::PopID();
                // Set the initial focus when opening the combo (scrolling + keyboard navigation focus)
                if (is_selected)
                    ImGui::SetItemDefaultFocus();
            }
            ImGui::EndListBox();
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
