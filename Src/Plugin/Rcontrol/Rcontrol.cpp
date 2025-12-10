// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                  ORBITER MODULE: Rcontrol
//                  Part of the ORBITER SDK
//
// Rcontrol.cpp
//
// A small plugin which allows to manipulate spacecraft main and
// retro controls via a dialog box.
// ==============================================================

#define ORBITER_MODULE
#include "Orbitersdk.h"
#define IMGUI_DEFINE_MATH_OPERATORS
#include "imgui.h"
#include "imgui_extras.h"
#include "IconsFontAwesome6.h"

// ==============================================================
// The module interface class - singleton implementation

namespace oapi {

	/// \brief Plugin for controlling a spacecrafts's engines via
	///   a dialog box.
	class RControl: public Module, public ImGuiDialog {
	public:
		/// \brief Entry point for open dialog callback
		static void hookOpenDlg(void* context);

		void clbkSimulationStart (RenderMode mode) override;

		void OnDraw();

		/// \param hDLL module instance handle
		RControl(HINSTANCE hDLL);

		/// \brief Protected destructor
		~RControl();

		void SetVessel(VESSEL*);

	private:
		DWORD m_dwCmd;         ///> Handle for plugin entry in custom command list
		int m_dwMenuCmd;

		VESSEL* m_pVessel;     ///> vessel pointer
		std::string m_vesselName;
		float m_RCSlevel;
	};

} // namespace oapi


// ==============================================================
// API interface
// ==============================================================

static oapi::RControl *g_rcontrol;
/// \brief Module entry point 
/// \param hDLL module handle
DLLCLBK void InitModule(HINSTANCE hDLL)
{
	// Create and register the module
	g_rcontrol = new oapi::RControl(hDLL);
	oapiRegisterModule(g_rcontrol);
}

/// \brief Module exit point 
/// \param hDLL module handle
DLLCLBK void ExitModule(HINSTANCE hDLL)
{
	// Delete the module
	delete g_rcontrol;
}

void oapi::RControl::hookOpenDlg(void *ctx) {
	oapi::RControl *self = (oapi::RControl *)ctx;
	oapiOpenDialog(self);
}

// --------------------------------------------------------------

oapi::RControl::RControl(HINSTANCE hDLL)
	: Module(hDLL), ImGuiDialog("Orbiter Remote Vessel Control", {344,328})
{
	// Register the custom command for the plugin
	m_dwCmd = oapiRegisterCustomCmd((char*)"Remote Vessel Control",
		(char*)"Operate the engines of any spacecraft from a dialog box.",
		hookOpenDlg, this);

	m_dwMenuCmd = oapiRegisterCustomMenuCmd ("Rcontrol", "MenuInfoBar/Rcontrol.png", hookOpenDlg, this);

	m_pVessel = nullptr;
	m_RCSlevel = 1.0f;
}

// --------------------------------------------------------------

oapi::RControl::~RControl()
{
	// Unregister the custom command for calling the plugin
	oapiUnregisterCustomCmd(m_dwCmd);
	oapiUnregisterCustomMenuCmd(m_dwMenuCmd);
}

// --------------------------------------------------------------
void oapi::RControl::SetVessel(VESSEL *vessel)
{
	m_pVessel = vessel;
	m_vesselName = vessel->GetName();
}

void oapi::RControl::clbkSimulationStart (RenderMode mode)
{
	SetVessel(oapiGetFocusInterface());
}

bool ButtonPressed(const char *label, const ImVec2 &size)
{
	ImGui::Button(label, size);
	return ImGui::IsItemActive();
}

void oapi::RControl::OnDraw()
{
	char cbuf[128];
	ImGui::SeparatorText("Vessel");
	ImGui::SetNextItemWidth(160.0f);
	if(ImGui::BeginAnimatedCombo("##Vessel", m_vesselName.c_str(), ImGuiComboFlags_HeightLargest)) {
		for (int i = 0; i < oapiGetVesselCount(); i++) {
			OBJHANDLE hVessel = oapiGetVesselByIndex(i);
			VESSEL *vessel = oapiGetVesselInterface(hVessel);
			if (vessel->GetEnableFocus()) {
				const bool is_selected = m_pVessel == vessel;
				if(ImGui::Selectable(vessel->GetName(), is_selected)) {
					if(m_pVessel != vessel) {
						SetVessel(vessel);
					}
				}

			}
		}
		ImGui::EndAnimatedCombo();
	}
	ImGui::SameLine();
	if(ImGui::Button("Set focus")) {
		oapiSetFocusObject(m_pVessel->GetHandle());
	}
	ImGui::SeparatorText("Engines");
	float lvl = -m_pVessel->GetThrusterGroupLevel (THGROUP_RETRO);
	ImGui::SetNextItemWidth(160.0f);
	if(ImGui::SliderFloat("##Retro", &lvl, -1.0, 0.0, ICON_FA_ANGLES_LEFT " Retro", ImGuiSliderFlags_AlwaysClamp)) {
		m_pVessel->SetThrusterGroupLevel (THGROUP_RETRO, -lvl);
	}
	ImGui::SameLine();
	lvl = m_pVessel->GetThrusterGroupLevel (THGROUP_MAIN);
	ImGui::SetNextItemWidth(160.0f);
	if(ImGui::SliderFloat("##Main", &lvl, 0.0, 1.0, "Main " ICON_FA_ANGLES_RIGHT, ImGuiSliderFlags_AlwaysClamp)) {
		m_pVessel->SetThrusterGroupLevel (THGROUP_MAIN, lvl);
	}

	lvl = m_pVessel->GetThrusterGroupLevel (THGROUP_HOVER);
	ImGui::SetNextItemWidth(160.0f);
	if(ImGui::SliderFloat("##Hover", &lvl, 0.0, 1.0, "Hover", ImGuiSliderFlags_AlwaysClamp)) {
		m_pVessel->SetThrusterGroupLevel (THGROUP_HOVER, lvl);
	}
	ImGui::SameLine();
	ImGui::SetNextItemWidth(160.0f);
	ImGui::SliderFloat("##RCS", &m_RCSlevel, 0.0, 1.0, "RCS level", ImGuiSliderFlags_AlwaysClamp);

	float w = ImGui::CalcTextSize(ICON_FA_ARROW_UP).x * 2.0f;
	ImVec2 btnSize = ImVec2(w, w);
	w = ImGui::GetContentRegionAvail().x / 2.0f;
	ImVec2 childSize = ImVec2(w, btnSize.y * 5.0f);
	ImGui::BeginChild("Left", childSize);
	ImGui::SeparatorText("RCS Rotation");

	ImGui::Dummy(btnSize);
	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ARROW_UP, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_PITCHDOWN, m_RCSlevel);

	if(ButtonPressed(ICON_FA_ARROW_ROTATE_LEFT, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BANKLEFT, m_RCSlevel);

	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ARROWS_TO_DOT, btnSize))
		m_pVessel->ToggleNavmode(NAVMODE_KILLROT);

	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ARROW_ROTATE_RIGHT, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BANKRIGHT, m_RCSlevel);

	if(ButtonPressed(ICON_FA_ARROW_LEFT, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_YAWLEFT, m_RCSlevel);
	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ARROW_DOWN, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_PITCHUP, m_RCSlevel);
	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ARROW_RIGHT, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_YAWRIGHT, m_RCSlevel);


	ImGui::EndChild();
	ImGui::SameLine();
	ImGui::BeginChild("Right", childSize);
	ImGui::SeparatorText("RCS Translation");
	ImGui::Dummy(btnSize);
	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ARROW_UP, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_DOWN, m_RCSlevel);
	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ANGLES_UP, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_FORWARD, m_RCSlevel);
	ImGui::Dummy(btnSize);
	ImGui::SameLine();
	ImGui::Dummy(btnSize);
	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ANGLES_DOWN, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_BACK, m_RCSlevel);

	if(ButtonPressed(ICON_FA_ARROW_LEFT, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_LEFT, m_RCSlevel);
	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ARROW_DOWN, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_UP, m_RCSlevel);
	ImGui::SameLine();
	if(ButtonPressed(ICON_FA_ARROW_RIGHT, btnSize))
		m_pVessel->IncThrusterGroupLevel_SingleStep (THGROUP_ATT_RIGHT, m_RCSlevel);
	ImGui::EndChild();

	ImGui::SeparatorText("Nav mode");
	
	btnSize.x = ImGui::GetContentRegionAvail().x / 3.0f - ImGui::GetStyle().FramePadding.x * 4.0f / 3.0f;
	if(ImGui::Button("Killrot", btnSize))
		m_pVessel->ToggleNavmode(NAVMODE_KILLROT);

	ImGui::SameLine();
	if(ImGui::Button("Prograde", btnSize))
		m_pVessel->ToggleNavmode(NAVMODE_PROGRADE);

	ImGui::SameLine();
	if(ImGui::Button("Normal", btnSize))
		m_pVessel->ToggleNavmode(NAVMODE_NORMAL);


	if(ImGui::Button("HLevel", btnSize))
		m_pVessel->ToggleNavmode(NAVMODE_HLEVEL);

	ImGui::SameLine();
	if(ImGui::Button("Retrograde", btnSize))
		m_pVessel->ToggleNavmode(NAVMODE_RETROGRADE);

	ImGui::SameLine();
	if(ImGui::Button("Anti Normal", btnSize))
		m_pVessel->ToggleNavmode(NAVMODE_ANTINORMAL);
}
