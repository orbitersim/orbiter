// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// In-session options dialog
// ======================================================================

#include "DlgOptions.h"
#include "Orbiter.h"
#include "OrbiterAPI.h"
#include "Psys.h"

extern Orbiter* g_pOrbiter;
extern PlanetarySystem* g_psys;

#include "IconsFontAwesome6.h"
#include "imgui.h"
#include "imgui_extras.h"

#define TRANSLATION_CONTEXT "Dialog Options"
#include "I18NAPI.h"

DlgOptions::DlgOptions(): ImGuiDialog(ICON_FA_LIST_CHECK, _("Orbiter: Options"), {692,383})
{
	m_pathStarmap.clear();
	m_pathBgImage.clear();
	featuretarget = _("Select...");

	std::ifstream ifs(g_pOrbiter->Cfg()->ConfigPath("CSphere/bkgimage"));
	if (ifs) {
		char* c;
		char cbuf[256];
		bool found = false;
		while (ifs.getline(cbuf, 256)) {
			if (!found) {
				if (!strcmp(cbuf, "BEGIN_STARMAPS"))
					found = true;
				continue;
			}
			if (!strcmp(cbuf, "END_STARMAPS"))
				break;
			c = strtok(cbuf, "|");
			if (c) {
				std::string label(c);
				c = strtok(NULL, "\n");
				std::string path(c);
				m_pathStarmap.push_back(std::make_pair(label, path));
				if(path == g_pOrbiter->Cfg()->CfgVisualPrm.StarImagePath) {
					currentstarmap = label;
				}
			}
		}

		ifs.clear();
		ifs.seekg(0);

		found = false;
		while (ifs.getline(cbuf, 256)) {
			if (!found) {
				if (!strcmp(cbuf, "BEGIN_BACKGROUNDS"))
					found = true;
				continue;
			}
			if (!strcmp(cbuf, "END_BACKGROUNDS"))
				break;
			c = strtok(cbuf, "|");
			if (c) {
				std::string label(c);
				c = strtok(NULL, "\n");
				std::string path(c);
				m_pathBgImage.push_back(std::make_pair(label, path));
				if(path == g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgPath) {
					currentbgimage = label;
				}
			}
		}
	}
	std::ifstream fcfg(g_pOrbiter->Cfg()->ConfigPath(g_psys->Name().c_str()));
	g_psys->ScanLabelLists(fcfg);
}

void DlgOptions::OnDraw()
{
	const OptionTab tabs[] = {
		{_("Instruments & panels"), "/tab_param.htm",      &DlgOptions::DrawInstrument},
		{_("Vessel settings"),      "/tab_param.htm",      &DlgOptions::DrawVessel},
		{_("User interface"),       "/tab_param.htm",      &DlgOptions::DrawUI},
		{_("      Joystick"),       "/tab_joystick.htm",   &DlgOptions::DrawJoystick},
		{_("Celestial sphere"),     "/opt_celsphere.htm",  &DlgOptions::DrawCelSphere},
		{_("Visual helpers"),       "/vishelper.htm",      &DlgOptions::DrawVisHelper},
		{_("      Planetarium"),    "/vh_planetarium.htm", &DlgOptions::DrawPlanetarium},
		{_("      Labels"),         "/vh_labels.htm",      &DlgOptions::DrawLabels},
		{_("      Body forces"),    "/vh_force.htm",       &DlgOptions::DrawForces},
		{_("      Object axes"),    "/vh_coord.htm",       &DlgOptions::DrawAxes},
	};


    const ImGuiWindowFlags window_flags = ImGuiChildFlags_Borders | ImGuiChildFlags_ResizeX;
	ImGui::BeginChild("OptionSelection", ImVec2(250, 0), window_flags);
	int selected_idx = 0;
	if(currentPage.empty()) {
		currentPage = tabs[0].name;
		SetHelp("html/orbiter.chm", tabs[0].helptopic);
	}
	for(int i = 0; i < sizeof(tabs)/sizeof(tabs[0]); i++) {
			bool selected = currentPage == tabs[i].name;
			if(selected)
				selected_idx = i;
            if(ImGui::Selectable(tabs[i].name, selected)) {
				currentPage = tabs[i].name;
				SetHelp("html/orbiter.chm", tabs[i].helptopic);
            }
			if (selected) {
				ImGui::SetItemDefaultFocus();
			}
        }

	ImGui::EndChild();
	ImGui::SameLine();
	ImVec2 avail = ImGui::GetContentRegionAvail();
	ImGui::BeginChild("OptionContent", avail, ImGuiChildFlags_ResizeX | ImGuiChildFlags_AutoResizeX);
    (this->*tabs[selected_idx].func)();
	ImGui::EndChild();

}

void DlgOptions::DrawInstrument()
{
	CFG_INSTRUMENTPRM &instru = g_pOrbiter->Cfg()->CfgInstrumentPrm;
	CFG_LOGICPRM &logic = g_pOrbiter->Cfg()->CfgLogicPrm;
	
	ImGui::SeparatorText(_("Multi-functional displays"));
	if(ImGui::InputDoubleEx(_("MFD refresh interval [s]"), &logic.InstrUpdDT, 0.01, 2.0, 0.1, 0.5, "%.1f"))
		g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDUPDATEINTERVAL);
	if(ImGui::InputIntEx(_("Glass cockpit MFD size"), &logic.MFDSize, 1, 10))
		g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDGENERICSIZE);
	if(ImGui::Checkbox(_("Transparent glass cockpit MFD"), &logic.bMfdTransparent))
		g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDGENERICTRANSP);

	char preview[64];
	sprintf(preview, "%dx%d", instru.VCMFDSize, instru.VCMFDSize);
	if(ImGui::BeginAnimatedCombo(_("Virtual cockpit MFD texture size"), preview)) {
		bool changed = false;
		changed |= ImGui::RadioButton("256x256", &instru.VCMFDSize, 256);
		changed |= ImGui::RadioButton("512x512", &instru.VCMFDSize, 512);
		changed |= ImGui::RadioButton("1024x1024", &instru.VCMFDSize, 1024);
		if(changed)
			g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDVCSIZE);
		ImGui::EndAnimatedCombo();
	}
	ImGui::SeparatorText(_("Instrument panels"));
	if(ImGui::InputDoubleEx(_("Panel scroll speed"), &logic.PanelScrollSpeed, -100.0, 100.0, 1.0, 15.0, "%.0f"))
		g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_PANELSCROLLSPEED);
	if(ImGui::InputDoubleEx(_("2D panel scale"), &logic.PanelScale, 0.25, 4.0, 0.1, 1.0, "%.2f"))
		g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_PANELSCALE);

}
void DlgOptions::DrawVessel()
{
	CFG_LOGICPRM &logic = g_pOrbiter->Cfg()->CfgLogicPrm;
	ImGui::SeparatorText(_("Vessel settings"));
	if(ImGui::Checkbox(_("Limited fuel"), &logic.bLimitedFuel))
		g_pOrbiter->OnOptionChanged(OPTCAT_VESSEL, OPTITEM_VESSEL_LIMITEDFUEL);
	if(ImGui::Checkbox(_("Auto-refuel on pad"), &logic.bPadRefuel))
		g_pOrbiter->OnOptionChanged(OPTCAT_VESSEL, OPTITEM_VESSEL_PADREFUEL);
	// TRANSLATORS: flight model
	const char *fmodels[] = {	_("Simple"), _("Realistic") };
	ImGui::SliderEnum(_("Flight model"), &logic.FlightModelLevel, fmodels, 2);

	const char *dmodel[] = {_("No damage"), _("Allow damage")};
	ImGui::SliderEnum(_("Damage model"), &logic.DamageSetting, dmodel, 2);
}
void DlgOptions::DrawUI()
{
	ImGui::SeparatorText(_("Mouse focus behavior"));
	ImGui::RadioButton(_("Focus requires click"), &g_pOrbiter->Cfg()->CfgUIPrm.MouseFocusMode, 0);
	ImGui::RadioButton(_("Hybrid: Click required only for child windows"), &g_pOrbiter->Cfg()->CfgUIPrm.MouseFocusMode, 1);
	ImGui::RadioButton(_("Focus follows mouse"), &g_pOrbiter->Cfg()->CfgUIPrm.MouseFocusMode, 2);
}
void DlgOptions::DrawJoystick()
{
	ImGui::SeparatorText(_("Joystick device"));
	DWORD ndev;
	DIDEVICEINSTANCE* joylist;
	g_pOrbiter->GetDInput()->GetJoysticks(&joylist, &ndev);
	DWORD &jidx = g_pOrbiter->Cfg()->CfgJoystickPrm.Joy_idx;

	const char *preview = _("<Disabled>");
	if(jidx > 0 && jidx <= ndev) {
		preview = joylist[jidx - 1].tszProductName;
	}

	if(ImGui::BeginAnimatedCombo("##joydev", preview)) {
		bool selected = jidx == 0;
		if(ImGui::Selectable(_("<Disabled>"), &selected)) {
			jidx = 0;
		}
		if (selected) {
			ImGui::SetItemDefaultFocus();
		}
		for (int i = 0; i < ndev; i++) {
			selected = jidx == (i+1);
			if(ImGui::Selectable(joylist[i].tszProductName, &selected)) {
				jidx = i + 1;
				g_pOrbiter->OnOptionChanged(OPTCAT_JOYSTICK, OPTITEM_JOYSTICK_DEVICE);
			}
			if (selected) {
				ImGui::SetItemDefaultFocus();
			}
		}
		ImGui::EndAnimatedCombo();
	}

	ImGui::BeginDisabled(jidx == 0);
		ImGui::SeparatorText(_("Main engine control"));
		DWORD &thaxis = g_pOrbiter->Cfg()->CfgJoystickPrm.ThrottleAxis;

		const char* axis[] = { _("<Keyboard only>"), _("Z-axis"), _("Slider 0"), _("Slider 1") };
		preview = axis[thaxis];

		if (ImGui::BeginAnimatedCombo("##joythaxis", preview)) {
			for (int n = 0; n < IM_ARRAYSIZE(axis); n++) {
				const bool is_selected = (thaxis == n);
				if (ImGui::Selectable(axis[n], is_selected)) {
					thaxis = n;
					g_pOrbiter->OnOptionChanged(OPTCAT_JOYSTICK, OPTITEM_JOYSTICK_PARAM);
				}

				if (is_selected)
					ImGui::SetItemDefaultFocus();
			}
			ImGui::EndAnimatedCombo();
		}
		ImGui::Checkbox(_("Ignore throttle setting on launch"), &g_pOrbiter->Cfg()->CfgJoystickPrm.bThrottleIgnore);

		ImGui::SeparatorText(_("Calibration"));
		if(ImGui::SliderInt(_("Saturation"), &g_pOrbiter->Cfg()->CfgJoystickPrm.ThrottleSaturation, 0, 10000))
			g_pOrbiter->OnOptionChanged(OPTCAT_JOYSTICK, OPTITEM_JOYSTICK_PARAM);
		if(ImGui::SliderInt(_("Deadzone"), &g_pOrbiter->Cfg()->CfgJoystickPrm.Deadzone, 0, 10000))
			g_pOrbiter->OnOptionChanged(OPTCAT_JOYSTICK, OPTITEM_JOYSTICK_PARAM);

	ImGui::EndDisabled();
}
void DlgOptions::DrawCelSphere()
{
	bool &bUseStarDots = g_pOrbiter->Cfg()->CfgVisualPrm.bUseStarDots;
	ImGui::SeparatorText(_("Celestial sphere"));
	if(ImGui::Checkbox(_("Background stars: show as pixels"), &bUseStarDots))
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATESTARDOTS);

	ImGui::BeginDisabled(!bUseStarDots);
		StarRenderPrm& prm =  g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm;
		bool changed = false;
		if(ImGui::SliderDouble(_("Max magnitude threshold"), &prm.mag_hi, -2.0, 14.99, "%0.1f")) {
			changed = true;
			if(prm.mag_hi >= prm.mag_lo)
				prm.mag_lo = prm.mag_hi+0.01;
		}
		if(ImGui::SliderDouble(_("Min magnitude threshold"), &prm.mag_lo, -1.99, 15.0, "%0.1f")) {
			changed = true;
			if(prm.mag_lo <= prm.mag_hi)
				prm.mag_hi = prm.mag_lo-0.01;
		}
		changed |= ImGui::SliderDouble(_("Min brightness"), &prm.brt_min, 0.01, 1.0, "%0.2f");

		if(changed)
			g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_STARDISPLAYPARAM);

	ImGui::EndDisabled();

	bool &bUseStarImage = g_pOrbiter->Cfg()->CfgVisualPrm.bUseStarImage;
	if(ImGui::Checkbox(_("Background stars: show as image"), &bUseStarImage))
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATESTARIMAGE);

	ImGui::BeginDisabled(!bUseStarImage);
		if(ImGui::BeginAnimatedCombo("##celstarmap", currentstarmap.c_str())) {
			for(const auto &starmap: m_pathStarmap) {
				if(ImGui::Selectable(starmap.first.c_str(), starmap.second == g_pOrbiter->Cfg()->CfgVisualPrm.StarImagePath)) {
					strcpy(g_pOrbiter->Cfg()->CfgVisualPrm.StarImagePath, starmap.second.c_str());
					currentstarmap = starmap.second;
					g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_STARIMAGECHANGED);
				}
			}
			ImGui::EndAnimatedCombo();
		}
	ImGui::EndDisabled();

	bool &bUseBgImage = g_pOrbiter->Cfg()->CfgVisualPrm.bUseBgImage;
	if(ImGui::Checkbox(_("Background map"), &bUseBgImage))
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATEBGIMAGE);

	ImGui::BeginDisabled(!bUseBgImage);
		if(ImGui::BeginAnimatedCombo("##celbgimage", currentbgimage.c_str())) {
			for(const auto &bgimage: m_pathBgImage) {
				if(ImGui::Selectable(bgimage.first.c_str(), bgimage.second == g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgPath)) {
					strcpy(g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgPath, bgimage.second.c_str());
					currentbgimage = bgimage.first;
					g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_BGIMAGECHANGED);
				}
			}
			ImGui::EndAnimatedCombo();
		}
		if(ImGui::SliderDouble(_("Brightness"), &g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgIntens, 0.01, 1.0, "%.2f")) {
			g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_BGIMAGEBRIGHTNESS);
		}

	ImGui::EndDisabled();

}
void DlgOptions::DrawVisHelper()
{
	ImGui::SeparatorText(_("Visual helpers"));
	ImGui::Text(_("Options for displaying markers and gridlines on the celestial"));
	ImGui::Text(_("sphere, labels for planetary surface features, as weel as coordinate"));
	ImGui::Text(_("axis and force vectors"));

	CFG_VISHELPPRM &prm = g_pOrbiter->Cfg()->CfgVisHelpPrm;
	ImGui::CheckboxFlags(_("Planetarium mode (F9)"),          &prm.flagPlanetarium, PLN_ENABLE);
	ImGui::CheckboxFlags(_("Surface/object labels (Alt-F9)"), &prm.flagMarkers,     MKR_ENABLE);
	ImGui::CheckboxFlags(_("Body force vectors"),             &prm.flagBodyForce,   BFV_ENABLE);
	ImGui::CheckboxFlags(_("Object frame axis"),              &prm.flagFrameAxes,   FAV_ENABLE);
}
void DlgOptions::DrawPlanetarium()
{
	bool changed = false;
	CFG_VISHELPPRM &prm = g_pOrbiter->Cfg()->CfgVisHelpPrm;
	changed |= ImGui::CheckboxFlags(_("Planetarium mode (F9)"), &prm.flagPlanetarium, PLN_ENABLE);

	ImGui::BeginDisabled(!(prm.flagPlanetarium & PLN_ENABLE));
		int width = ImGui::GetContentRegionAvail().x;
		ImGui::BeginChild("##left", ImVec2(width/2.0, 0));
			ImGui::SeparatorText(_("Grids and circles"));
			changed |= ImGui::CheckboxFlags(_("Celestial grid"),     &prm.flagPlanetarium, PLN_CGRID);
			changed |= ImGui::CheckboxFlags(_("Ecliptic grid"),      &prm.flagPlanetarium, PLN_EGRID);
			changed |= ImGui::CheckboxFlags(_("Galactic grid"),      &prm.flagPlanetarium, PLN_GGRID);
			changed |= ImGui::CheckboxFlags(_("Local horizon grid"), &prm.flagPlanetarium, PLN_HGRID);
			changed |= ImGui::CheckboxFlags(_("Target Equator"),     &prm.flagPlanetarium, PLN_EQU);

			ImGui::SeparatorText(_("Constellations"));
			changed |= ImGui::CheckboxFlags(_("Labels"),     &prm.flagPlanetarium, PLN_CNSTLABEL);
			ImGui::BeginDisabled(!(prm.flagPlanetarium & PLN_CNSTLABEL));
				changed |= ImGui::CheckboxFlags(_("Long labels"), &prm.flagPlanetarium, PLN_CNSTLONG);
			ImGui::EndDisabled();
			changed |= ImGui::CheckboxFlags(_("Boundaries"), &prm.flagPlanetarium, PLN_CNSTBND);
			changed |= ImGui::CheckboxFlags(_("Patterns"),   &prm.flagPlanetarium, PLN_CONST);
		ImGui::EndChild();
		ImGui::SameLine();
		ImGui::BeginChild("##right");
			ImGui::SeparatorText(_("Celestical markers"));

			changed |= ImGui::CheckboxFlags(_("Show markers"), &prm.flagPlanetarium, PLN_CCMARK);
		
			std::vector<oapi::GraphicsClient::LABELLIST> &list = g_psys->LabelList();
			ImGui::BeginDisabled(!(prm.flagPlanetarium & PLN_CCMARK));
				bool lblchanged = false;
				for(auto &lbl: list) {
					lblchanged |= ImGui::Selectable(lbl.name.c_str(), &lbl.active);
				}

				if(lblchanged) {
					std::ifstream fcfg(g_pOrbiter->Cfg()->ConfigPath(g_psys->Name().c_str()));
					g_psys->ScanLabelLists(fcfg);
				}
			ImGui::EndDisabled();
		ImGui::EndChild();
	ImGui::EndDisabled();
	if(changed)
		g_pOrbiter->OnOptionChanged(OPTCAT_PLANETARIUM, OPTITEM_PLANETARIUM_DISPFLAG);
}


void DlgOptions::AddCbodyNode(const CelestialBody *cbody) {
    ImGuiTreeNodeFlags node_flags = ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_SpanAvailWidth;
    const bool is_selected = featuretarget == cbody->Name();
    if (is_selected)
        node_flags |= ImGuiTreeNodeFlags_Selected;

    if(cbody->nSecondary()) {
        if(!strcmp(cbody->Name(), "Sun"))
            node_flags|=ImGuiTreeNodeFlags_DefaultOpen;

        bool node_open = ImGui::TreeNodeEx(cbody->Name(), node_flags);
        if (ImGui::IsItemClicked() && !ImGui::IsItemToggledOpen()) {
            featuretarget = cbody->Name();
            //SetBody(cbody->Name());
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
            featuretarget = cbody->Name();
            //SetBody(cbody->Name());
            ImGui::CloseCurrentPopup();
        }
    }}

void DlgOptions::DrawLabels()
{
	bool changed = false;
	CFG_VISHELPPRM &prm = g_pOrbiter->Cfg()->CfgVisHelpPrm;
	changed |= ImGui::CheckboxFlags(_("Surface and object labels (Alt-F9)"), &prm.flagMarkers, MKR_ENABLE);

	ImGui::BeginDisabled(!(prm.flagMarkers & MKR_ENABLE));
		int width = ImGui::GetContentRegionAvail().x;
		ImGui::BeginChild("##left", ImVec2(width/2.0, 0));
			ImGui::SeparatorText(_("Object markers"));
			changed |= ImGui::CheckboxFlags(_("Vessels"),          &prm.flagMarkers, MKR_VMARK);
			changed |= ImGui::CheckboxFlags(_("Celestial bodies"), &prm.flagMarkers, MKR_CMARK);
			changed |= ImGui::CheckboxFlags(_("Surface bases"),    &prm.flagMarkers, MKR_BMARK);
			changed |= ImGui::CheckboxFlags(_("VOR transmitters"), &prm.flagMarkers, MKR_RMARK);
		ImGui::EndChild();
		ImGui::SameLine();
		ImGui::BeginChild("##right");
			ImGui::SeparatorText(_("Surface features"));
			changed |= ImGui::CheckboxFlags(_("Features"), &prm.flagMarkers, MKR_LMARK);
			ImGui::BeginDisabled(!(prm.flagMarkers & MKR_LMARK));
				if(ImGui::BeginAnimatedCombo("##featuretarget", featuretarget.c_str(), ImGuiComboFlags_HeightLarge)) {
					for (int i = 0; i < g_psys->nStar(); i++)
						AddCbodyNode (g_psys->GetStar(i));

					ImGui::EndAnimatedCombo();
				}

				Planet* planet = g_psys->GetPlanet(featuretarget.c_str(), true);
				if (planet) {
					ImGui::BeginChild("##labels", ImVec2(0,0), ImGuiChildFlags_Borders);
					if (planet->LabelFormat() < 2) {
						int nlist;
						oapi::GraphicsClient::LABELLIST* list = planet->LabelList(&nlist);
						for(int i = 0; i < nlist; i++) {
							if(ImGui::Selectable(list[i].name.c_str(), &list[i].active)) {
								std::ifstream fcfg(g_pOrbiter->Cfg()->ConfigPath(planet->Name()));
								planet->ScanLabelLists(fcfg);
							}
						}
					} else {
						int nlabel = planet->NumLabelLegend();
						if (nlabel) {
							const oapi::GraphicsClient::LABELTYPE* lspec = planet->LabelLegend();
							for (int i = 0; i < nlabel; i++) {
								if(ImGui::Selectable(lspec[i].name, lspec[i].active)) {
									planet->SetLabelActive(i, !lspec[i].active);
								}
							}
						}
					}
					ImGui::EndChild();
				}
			ImGui::EndDisabled();
		ImGui::EndChild();
	ImGui::EndDisabled();

	if(changed) {
		if (g_psys) {
			g_psys->ActivatePlanetLabels(prm.flagMarkers & MKR_ENABLE && prm.flagMarkers & MKR_LMARK);
		}
	}
}
void DlgOptions::DrawForces()
{
	CFG_VISHELPPRM &prm = g_pOrbiter->Cfg()->CfgVisHelpPrm;
	ImGui::CheckboxFlags(_("Body force vectors"), &prm.flagBodyForce, BFV_ENABLE);

	ImGui::BeginDisabled(!(prm.flagBodyForce & BFV_ENABLE));

		int width = ImGui::GetContentRegionAvail().x;
		ImGui::BeginChild("##left", ImVec2(width/2.0, 0));
			ImGui::SeparatorText(_("Linear forces"));
			ImGui::CheckboxFlags(_("Weight"), &prm.flagBodyForce, BFV_WEIGHT);
			ImGui::CheckboxFlags(_("Thrust"), &prm.flagBodyForce, BFV_THRUST);
			ImGui::CheckboxFlags(_("Lift"),   &prm.flagBodyForce, BFV_LIFT);
			ImGui::CheckboxFlags(_("Drag"),   &prm.flagBodyForce, BFV_DRAG);
			ImGui::CheckboxFlags(_("Side"),   &prm.flagBodyForce, BFV_SIDEFORCE);
			ImGui::Separator();
			ImGui::CheckboxFlags(_("Total"),  &prm.flagBodyForce, BFV_TOTAL);

			ImGui::SeparatorText(_("Angular moments"));
			ImGui::CheckboxFlags(_("Torque"),  &prm.flagBodyForce, BFV_TORQUE);
		ImGui::EndChild();
		ImGui::SameLine();
		ImGui::BeginChild("##right");
			ImGui::SeparatorText(_("Display"));
			// TRANSLATORS: Logarithmic
			if(ImGui::RadioButton(_("Log"), prm.flagBodyForce & BFV_LOGSCALE)) {
				prm.flagBodyForce ^= BFV_LOGSCALE;
			}
			ImGui::SameLine();
			if(ImGui::RadioButton(_("Linear"), !(prm.flagBodyForce & BFV_LOGSCALE))) {
				prm.flagBodyForce ^= BFV_LOGSCALE;
			}
			ImGui::SliderFloat(_("Scale"), &prm.scaleBodyForce, 0.25, 4.0, "%0.2f", ImGuiSliderFlags_AlwaysClamp);
			ImGui::SliderFloat(_("Opacity"), &prm.opacBodyForce, 0.0, 1.0, "%0.2f", ImGuiSliderFlags_AlwaysClamp);
		ImGui::EndChild();

	ImGui::EndDisabled();
}
void DlgOptions::DrawAxes()
{
	CFG_VISHELPPRM &prm = g_pOrbiter->Cfg()->CfgVisHelpPrm;
	ImGui::CheckboxFlags(_("Object frame axis"), &prm.flagFrameAxes, FAV_ENABLE);

	ImGui::BeginDisabled(!(prm.flagFrameAxes & FAV_ENABLE));

		int width = ImGui::GetContentRegionAvail().x;
		ImGui::BeginChild("##left", ImVec2(width/2.0, 0));
			ImGui::SeparatorText(_("Objects"));
			ImGui::CheckboxFlags(_("Vessels"),          &prm.flagFrameAxes, FAV_VESSEL);
			ImGui::CheckboxFlags(_("Celestial bodies"), &prm.flagFrameAxes, FAV_CELBODY);
			ImGui::CheckboxFlags(_("Surface bases"),    &prm.flagFrameAxes, FAV_BASE);
		ImGui::EndChild();
		ImGui::SameLine();
		ImGui::BeginChild("##right");
			ImGui::SeparatorText(_("Display"));
			ImGui::CheckboxFlags(_("Show negative axis"), &prm.flagFrameAxes, FAV_NEGATIVE);
			ImGui::BeginDisabled(!(prm.flagFrameAxes & FAV_NEGATIVE));
				ImGui::SliderFloat(_("Scale"), &prm.scaleFrameAxes, 0.25, 4.0, "%0.2f", ImGuiSliderFlags_AlwaysClamp);
				ImGui::SliderFloat(_("Opacity"), &prm.opacFrameAxes, 0.0, 1.0, "%0.2f", ImGuiSliderFlags_AlwaysClamp);
			ImGui::EndDisabled();
		ImGui::EndChild();

	ImGui::EndDisabled();}
