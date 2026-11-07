// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2026 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
// ==============================================================

#ifndef __VIDEOTAB_H
#define __VIDEOTAB_H
#include <vector>
#include <map>
#include <string>

// ==============================================================

class VideoTab {

	struct _AtmoCfg { std::string cfg, file; };

	struct _Mode { std::string label; int w, h; };

	struct _Setup {
		int PlanetPreloadMode, PlanetLoadFrequency, PlanetTileLoadFlags;
		int SketchpadFont, DebugLvl, EnableMeshDbg, DebugBreak;
		int TextureMips, PostProcess, gcGUIMode, EarthCfg;
		int AA, AF;
		float PlanetGlow;
		int LODBias, MeshRes, MaxTiles, TileMipmaps;
		int LightConfig, ShadowMapMode, ShadowFilter, TerrainShadowing;
		int Convergence, Separation;
		int EnvMapMode, CustomCamMode, EnvMapFaces;
		int MicroMode, MicroFilter, BlendMode, MicroBias;
		bool PreLBaseVis, UseNormalMap, EnableGlass, NearClipPlane, CloudMicro, CloudNormals, GDIOverlay, AbsAnims, AtmoQuality;
		bool Irradiance, Glares, Flats, BaseColliders, VesselColliders;
	};

public:
	VideoTab(oapi::D3D9Client *gc);
	~VideoTab();

	void Draw();
	// Draw the Launchpad video tab content with ImGui

	void UpdateConfigData();
	// copy dialog state back to parameter structure

	bool Initialise();
	// Initialise dialog elements

protected:
	void SelectFullscreen(bool);
	void SelectMode(DWORD index);
	bool SelectAdapter(DWORD index);
	// Update dialog after user device selection

	void SelectWidth();
	// Update dialog after window width selection

	void SelectHeight();
	// Update dialog after window height selection

private:
	void DrawSetupDialog();
	void DrawCreditsDialog();
	void InitSetupDialog();
	void SaveSetupState();
	void LoadCredits();
	void ScanAtmoCfgs();
	bool GetConfigName(const char* file, std::string& cfg, std::string& planet);

	oapi::D3D9Client *gclient;
	int aspect_idx;
	bool bAspect;
	DWORD SelectedAdapterIdx;
	std::vector<std::string> Adapters;
	std::vector<_Mode> Modes;
	std::map<std::string, std::vector<_AtmoCfg>> AtmoCfgs;
	std::vector<int> AAValues, AFValues;
	_Setup setup;
	bool bOpenSetup, bOpenCredits;
	bool bMeshDbgInit;
	std::string Credits;
};

#endif // !__VIDEOTAB_H
