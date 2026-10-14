// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Camera configuration dialog
// ======================================================================

#ifndef __DLGCAMERA_H
#define __DLGCAMERA_H

#include "DlgMgr.h"
#include "Camera.h"

class CelestialBody;
class DlgCamera : public ImGuiDialog {
public:
    DlgCamera();
    void OnDraw() override;

private:
	void DrawMode();
	void DrawCockpitMode();
	void DrawTrackMode();
	void DrawGroundMode();
    void DrawControl();
	void DrawPreset();

	ExtCamMode extmode;
	IntCamMode intmode;
	bool extcam;
	bool ground_lock;
	bool rot_is_tilt, rot_is_pan;
	const Body* m_SelectedTarget;
    std::string m_SelectedSite;
    std::string m_SitePlanet;
    int m_SelectedPreset;

    char longitude[64]="";
    char latitude[64]="";
    char altitude[64]="";
	char distance[64] = "";
//    char followterrain[64]="";
	double m_Lng, m_Lat, m_Alt; // store values to check for change
	bool m_PosEdited;

    void AddCbodyNode(const CelestialBody *body);
	void AddSurfBaseNode(const CelestialBody *cbody);

	bool SystemHasBases(const CelestialBody *cbody);

	// Set camera mode to "Ground observer", tied to "planet".
	// If no planet is specified use the closest planet.
	bool SetGroundObserverView(const CelestialBody* planet = nullptr);


	// Just a wrapper for the ImGui::BeginCombo that avoids the tabbing at the right
	bool BeginWideCombo(const char* label, const char* preview_value, ImGuiComboFlags flags = 0);

	static const DWORD OBJ_VESSEL   = 0x01;
	static const DWORD OBJ_CELBODY  = 0x02;
	static const DWORD OBJ_SURFBASE = 0x04;
	static const DWORD OBJ_ALL = OBJ_VESSEL | OBJ_CELBODY | OBJ_SURFBASE;

	static const DWORD SEL_NOCAT          = 0x01; // don't show type categories
	static const DWORD SEL_ALLOPEN        = 0x02; // show all tree nodes open on start
	static const DWORD SEL_EXCLUDE_CAMTGT = 0x04; // skip the current camera target in the list

	// Convenience function to open a combo box for selecting a simulation object from a list
	// "body" contains a pointer to the selected object on exit. On call, should point to the current object, to initialise the combo preview
	// "objtypes" are bitflags from the OBJ_XXX consts
	// "selector_flags" are bitflags from the SEL_XXX consts
	bool ObjectSelector(const char* label, const Body *body, DWORD objtypes, DWORD selector_flags = 0, ImGuiComboFlags flags = 0);

	void Spacer() { ImGui::Dummy(ImVec2(10, 0)); }

	struct CameraTab {
		const char *name;
		const char *helptopic;
		void (DlgCamera::* func)();
	};

	static inline CameraTab tabs[] = {
		{"Mode", "/cam_mode.htm", &DlgCamera::DrawMode},
		{"Control", "/cam_control.htm", &DlgCamera::DrawControl},
		{"Preset",  "/cam_preset.htm",  &DlgCamera::DrawPreset}
	};

};
#endif // !__DLGCAMERA_H
