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
    void DrawControl();
    void DrawTarget();
    void DrawTrack();
    void DrawGround();
    void DrawFoV();
    void DrawPreset();

	ExtCamMode extmode;
	IntCamMode intmode;
	bool extcam;
	bool ground_lock;
	bool rot_is_tilt, rot_is_pan;
    std::string m_SelectedTarget;
    std::string m_SelectedSite;
    std::string m_SitePlanet;
    int m_SelectedPreset;

    char longitude[64]="";
    char latitude[64]="";
    char altitude[64]="";
    char followterrain[64]="";
    bool m_TargetLock;

    void AddCbodyNode(const CelestialBody *body);
    void ApplyObserver();
    void SetCurrentGroundpos();
	
	struct CameraTab {
		const char *name;
		const char *helptopic;
		void (DlgCamera::* func)();
	};

	static inline CameraTab tabs[] = {
		{"Control", "/cam_control.htm", &DlgCamera::DrawControl},
		{"Target",  "/cam_target.htm",  &DlgCamera::DrawTarget},
		{"Track",   "/cam_track.htm",   &DlgCamera::DrawTrack},
		{"Ground",  "/cam_ground.htm",  &DlgCamera::DrawGround},
		{"FoV",     "/cam_fov.htm",     &DlgCamera::DrawFoV},
		{"Preset",  "/cam_preset.htm",  &DlgCamera::DrawPreset},
	};

};
#endif // !__DLGCAMERA_H