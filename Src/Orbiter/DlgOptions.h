// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// In-session options dialog
// ======================================================================

/************************************************************************
 * \file DlgOptions.h
 * \brief Implementation of the in-session options dialog.
 */

#ifndef __DLGOPTIONS_H
#define __DLGOPTIONS_H

#include "OrbiterAPI.h"
#include <string>

class CelestialBody;
class DlgOptions: public ImGuiDialog {
public:
	DlgOptions();
	void OnDraw();
	void SwitchPage(const char *page) { currentPage = page; }
	std::string currentPage;
	std::string featuretarget;

	//TODO: add when converting Launchpad
	//void DrawVisual();
	//void DrawPhysics();
	void DrawInstrument();
	void DrawVessel();
	void DrawUI();
	 void DrawJoystick();
	void DrawCelSphere();
	void DrawVisHelper();
	 void DrawPlanetarium();
	 void DrawLabels();
	 void DrawForces();
	 void DrawAxes();

	void AddCbodyNode(const CelestialBody *cbody);

	struct OptionTab {
		const char *name;
		const char *helptopic;
		void (DlgOptions::* func)();
	};

	static inline OptionTab tabs[] = {
		{"Instruments & panels", "/tab_param.htm",      &DlgOptions::DrawInstrument},
		{"Vessel settings",      "/tab_param.htm",      &DlgOptions::DrawVessel},
		{"User interface",       "/tab_param.htm",      &DlgOptions::DrawUI},
		{"      Joystick",       "/tab_joystick.htm",   &DlgOptions::DrawJoystick},
		{"Celestial sphere",     "/opt_celsphere.htm",  &DlgOptions::DrawCelSphere},
		{"Visual helpers",       "/vishelper.htm",      &DlgOptions::DrawVisHelper},
		{"      Planetarium",    "/vh_planetarium.htm", &DlgOptions::DrawPlanetarium},
		{"      Labels",         "/vh_labels.htm",      &DlgOptions::DrawLabels},
		{"      Body forces",    "/vh_force.htm",       &DlgOptions::DrawForces},
		{"      Object axes",    "/vh_coord.htm",       &DlgOptions::DrawAxes},
	};

	std::vector<std::pair<std::string, std::string>> m_pathStarmap;
	std::vector<std::pair<std::string, std::string>> m_pathBgImage;
	std::string currentstarmap;
	std::string currentbgimage;
};

#endif // !__DLGOPTIONS_H