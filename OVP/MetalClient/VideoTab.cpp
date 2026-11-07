// ==============================================================
// Class VideoTab (implementation)
// Manages the user selections in the "Video" tab of the Orbiter
// Launchpad dialog.
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2026 Martin Schweiger
//				 2010-2016 Jarmo Nikkanen (D3D9Client implementation)
// ==============================================================

#include "D3D9Client.h"
#include "VideoTab.h"
#include "AABBUtil.h"
#include "D3D9Config.h"
#include "OapiExtension.h"
#include "imgui.h"
#include <vector>
#include <fstream>
#include <sstream>
#include <filesystem>
#include <algorithm>

using namespace oapi;

namespace fs = std::filesystem;

static const char *SETUP_POPUP = "Advanced setup";
static const char *CREDITS_POPUP = "Credits";

static int aspect_wfac[3] = {4,16,16};
static int aspect_hfac[3] = {3,10,9};

// ==============================================================

static bool LCombo(const char *label, float lw, int *idx, const char *const items[], int count)
{
	ImGui::PushID(label);
	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted(label);
	ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(-FLT_MIN);
	if (*idx < 0 || *idx >= count) *idx = 0;
	bool bRet = ImGui::Combo("##c", idx, items, count);
	ImGui::PopID();
	return bRet;
}

static bool LCombo(const char *label, float lw, int *idx, const std::vector<std::string> &items)
{
	std::vector<const char *> p;
	for (auto &s : items) p.push_back(s.c_str());
	if (p.empty()) {
		ImGui::BeginDisabled();
		static const char *none[] = { "" };
		int z = 0;
		bool b = LCombo(label, lw, &z, none, 1);
		ImGui::EndDisabled();
		return b;
	}
	return LCombo(label, lw, idx, p.data(), int(p.size()));
}

static bool Check(const char *label, int *v)
{
	bool b = (*v != 0);
	if (ImGui::Checkbox(label, &b)) {
		*v = b ? 1 : 0;
		return true;
	}
	return false;
}

// ==============================================================

static std::string RtfToText(const std::string &rtf)
{
	std::string out;
	std::vector<bool> skip;
	bool bSkip = false;
	size_t i = 0, n = rtf.size();

	auto put = [&](unsigned c) {
		if (bSkip) return;
		if (c < 0x80) out += char(c);
		else {
			out += char(0xC0 | (c >> 6));
			out += char(0x80 | (c & 0x3F));
		}
	};

	while (i < n) {
		char c = rtf[i];
		if (c == '{') {
			skip.push_back(bSkip);
			i++;
			if (i + 1 < n && rtf[i] == '\\' && rtf[i + 1] == '*') bSkip = true, i += 2;
			continue;
		}
		if (c == '}') {
			if (!skip.empty()) bSkip = skip.back(), skip.pop_back();
			i++;
			continue;
		}
		if (c == '\\') {
			i++;
			if (i >= n) break;
			char d = rtf[i];
			if (d == '\\' || d == '{' || d == '}') { put(unsigned(d)); i++; continue; }
			if (d == '~') { put(' '); i++; continue; }
			if (d == '\'') {
				if (i + 2 < n) put(unsigned(strtoul(rtf.substr(i + 1, 2).c_str(), NULL, 16)));
				i += 3;
				continue;
			}
			if (!isalpha((unsigned char)d)) { i++; continue; }
			size_t s = i;
			while (i < n && isalpha((unsigned char)rtf[i])) i++;
			std::string word = rtf.substr(s, i - s);
			if (i < n && (rtf[i] == '-' || isdigit((unsigned char)rtf[i]))) {
				i++;
				while (i < n && isdigit((unsigned char)rtf[i])) i++;
			}
			if (i < n && rtf[i] == ' ') i++;
			if (word == "par" || word == "line") put('\n');
			else if (word == "tab") put('\t');
			else if (word == "fonttbl" || word == "colortbl" || word == "stylesheet" || word == "info" || word == "fldinst" || word == "pict") bSkip = true;
			continue;
		}
		if (c != '\r' && c != '\n') put((unsigned char)c);
		i++;
	}
	return out;
}


// ==============================================================
// Constructor

VideoTab::VideoTab(D3D9Client *gc)
{
	gclient      = gc;
	aspect_idx	 = 0;
	bAspect		 = false;
	SelectedAdapterIdx = 0;
	bOpenSetup = bOpenCredits = false;
	bMeshDbgInit = Config->EnableMeshDbg != 0;
	memset(&setup, 0, sizeof(setup));
}

VideoTab::~VideoTab()
{

}

// ==============================================================
// Initialise the Launchpad "video" tab

bool VideoTab::Initialise()
{
	D3DDISPLAYMODE curMode;
	D3DADAPTER_IDENTIFIER9 info;

	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	data->forceenum = false;
	data->trystencil = false;

	Adapters.clear();
	Modes.clear();

	ScanAtmoCfgs();

	if (g_pD3DObject == NULL) {
		LogErr("VideoTab::Initialize() Direct3DCreate9 Failed");
		FailedDeviceError();
		return false;
	}

	int nAdapter = g_pD3DObject->GetAdapterCount();

	if (nAdapter == 0) {
		LogErr("VideoTab::Initialize() No DirectX9 Adapters Found");
		FailedDeviceError();
		return false;
	}

	if (data->deviceidx < 0 || (data->deviceidx)>=nAdapter) data->deviceidx = 0;

	for (int i=0;i<nAdapter;i++) {
		HR(g_pD3DObject->GetAdapterIdentifier(i, 0, &info));
		LogAlw("Adapter %d: %s", i, info.Description);
		Adapters.push_back(info.Description);
	}

	HR(g_pD3DObject->GetAdapterDisplayMode(data->deviceidx, &curMode));

	LogAlw("Current Mode W=%u, H=%u", curMode.Width, curMode.Height);

	if (data->style < 0 || data->style > 2) data->style = 1;

	aspect_idx = 0;

	if (data->winw == (4*data->winh)/3 || data->winh == (3*data->winw)/4)	aspect_idx = 1;
	else if (data->winw == (16*data->winh)/10 || data->winh == (10*data->winw)/16) aspect_idx = 2;
	else if (data->winw == (16*data->winh)/9 || data->winh == (9*data->winw)/16) aspect_idx = 3;

	bAspect = aspect_idx != 0;
	if (aspect_idx) aspect_idx--;

	bool bRet = SelectAdapter(data->deviceidx);

	SelectFullscreen(data->fullscreen);

	return bRet;
}


// ==============================================================
//
void VideoTab::SelectMode(DWORD index)
{
	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();
	if (index < Modes.size()) data->modeidx = int(index);
}


// ==============================================================
// Respond to user adapter selection
//
bool VideoTab::SelectAdapter(DWORD index)
{
	SelectedAdapterIdx = index;

	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	if (g_pD3DObject == NULL) {
		LogErr("VideoTab::SelectAdapter(%u) Direct3DCreate9 Failed", index);
		return false;
	}

	char cbuf[64];
	D3DDISPLAYMODE mode;

	if (g_pD3DObject->GetAdapterCount()<=index) {
		LogErr("Adapter Index out of range");
		return false;
	}

	data->deviceidx = int(index);

	Modes.clear();

	DWORD nModes = g_pD3DObject->GetAdapterModeCount(index, D3DFMT_X8R8G8B8);

	if (nModes == 0) {
		LogErr("VideoTab::SelectAdapter() No Display Modes Available");
	}

	for (DWORD k=0;k<nModes;k++) {
		HR(g_pD3DObject->EnumAdapterModes(index, D3DFMT_X8R8G8B8, k, &mode));
		snprintf(cbuf, sizeof(cbuf), "%u x %u  %uHz", mode.Width, mode.Height, mode.RefreshRate);
		LogAlw("Index:%u %u x %u  %uHz (%u)", k, mode.Width, mode.Height, mode.RefreshRate, mode.Format);
		Modes.push_back({ cbuf, int(mode.Width), int(mode.Height) });
	}

	if (data->modeidx < 0 || data->modeidx >= int(Modes.size())) data->modeidx = 0;

	return true;
}


// ==============================================================
//
void VideoTab::SelectFullscreen(bool bFull)
{
	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();
	data->fullscreen = bFull;
}


// ==============================================================
// Respond to user selection of render window width

void VideoTab::SelectWidth ()
{
	if (bAspect) {
		GraphicsClient::VIDEODATA *data = gclient->GetVideoData();
		int wfac = aspect_wfac[aspect_idx], hfac = aspect_hfac[aspect_idx];
		if (data->winw != (wfac*data->winh)/hfac) data->winh = (hfac*data->winw)/wfac;
	}
}

// ==============================================================
// Respond to user selection of render window height

void VideoTab::SelectHeight ()
{
	if (bAspect) {
		GraphicsClient::VIDEODATA *data = gclient->GetVideoData();
		int wfac = aspect_wfac[aspect_idx], hfac = aspect_hfac[aspect_idx];
		if (data->winh != (hfac*data->winw)/wfac) data->winw = (wfac*data->winh)/hfac;
	}
}

// ==============================================================
// copy dialog state back to parameter structure

void VideoTab::UpdateConfigData()
{
	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	if (data->deviceidx < 0 || data->deviceidx >= int(Adapters.size())) data->deviceidx = 0;
	if (data->modeidx < 0 || data->modeidx >= int(Modes.size())) data->modeidx = 0;
	if (data->style < 0 || data->style > 2) data->style = 1;
	data->forceenum = false;
	data->winw = std::max(data->winw, 16);
	data->winh = std::max(data->winh, 16);
}


// ==============================================================
// Draw the Launchpad "video" tab

void VideoTab::Draw()
{
	static const char *styles[] = { "True full screen", "Full screen window", "Window with taskbar" };

	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	float lw = ImGui::GetFontSize() * 9.0f;
	float fw = ImGui::GetFontSize() * 6.0f;

	int dev = data->deviceidx;
	if (LCombo("Device", lw, &dev, Adapters)) SelectAdapter(DWORD(dev));

	ImGui::Spacing();

	if (ImGui::RadioButton("Full screen", data->fullscreen)) SelectFullscreen(true);
	ImGui::SameLine();
	if (ImGui::RadioButton("Window", !data->fullscreen)) SelectFullscreen(false);

	ImGui::Spacing();

	ImGui::BeginDisabled(!data->fullscreen);

	std::vector<std::string> modes;
	for (auto &m : Modes) modes.push_back(m.label);
	int mode = data->modeidx;
	if (LCombo("Resolution", lw, &mode, modes)) SelectMode(DWORD(mode));

	LCombo("Full screen mode", lw, &data->style, styles, 3);

	ImGui::Checkbox("Multiple displays", &data->pageflip);

	ImGui::EndDisabled();

	ImGui::Checkbox("Disable vsync", &data->novsync);

	ImGui::Spacing();

	ImGui::BeginDisabled(data->fullscreen);

	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("Window size");
	ImGui::SameLine(lw);
	ImGui::SetNextItemWidth(fw);
	if (ImGui::InputInt("##width", &data->winw, 0, 0)) SelectWidth();
	ImGui::SameLine();
	ImGui::TextUnformatted("x");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(fw);
	if (ImGui::InputInt("##height", &data->winh, 0, 0)) SelectHeight();

	if (ImGui::Checkbox("Fixed aspect ratio", &bAspect)) SelectWidth();
	ImGui::BeginDisabled(!bAspect);
	ImGui::SameLine();
	if (ImGui::RadioButton("4:3", aspect_idx == 0)) aspect_idx = 0, SelectWidth();
	ImGui::SameLine();
	if (ImGui::RadioButton("16:10", aspect_idx == 1)) aspect_idx = 1, SelectWidth();
	ImGui::SameLine();
	if (ImGui::RadioButton("16:9", aspect_idx == 2)) aspect_idx = 2, SelectWidth();
	ImGui::EndDisabled();

	ImGui::EndDisabled();

	ImGui::Checkbox("Force window size", &data->trystencil);

	ImGui::Spacing();

	if (ImGui::Button("Advanced")) {
		InitSetupDialog();
		ImGui::OpenPopup(SETUP_POPUP);
	}

	DrawSetupDialog();
}




// ***************************************************************************************************
// Advanced setup Dialog
// ***************************************************************************************************

void VideoTab::InitSetupDialog()
{
	DWORD aamax = 0;
	D3DCAPS9 caps;
	memset(&caps, 0, sizeof(caps));

	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	AAValues.clear();
	AFValues.clear();

	if (g_pD3DObject) {
		g_pD3DObject->GetDeviceCaps(SelectedAdapterIdx, D3DDEVTYPE_HAL, &caps);
		if (g_pD3DObject->CheckDeviceMultiSampleType(SelectedAdapterIdx, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_2_SAMPLES, NULL)==S_OK) aamax=2;
		if (g_pD3DObject->CheckDeviceMultiSampleType(SelectedAdapterIdx, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_4_SAMPLES, NULL)==S_OK) aamax=4;
		if (g_pD3DObject->CheckDeviceMultiSampleType(SelectedAdapterIdx, D3DDEVTYPE_HAL, D3DFMT_X8R8G8B8, true, D3DMULTISAMPLE_8_SAMPLES, NULL)==S_OK) aamax=8;
	}
	else LogErr("VideoTab::InitSetupDialog() Direct3DCreate9 Failed");

	LogAlw("InitSetupDialog() Enum Device AA capability = %u",aamax);

	AAValues.push_back(0);
	if (aamax >= 2) AAValues.push_back(2);
	if (aamax >= 4) AAValues.push_back(4);
	if (aamax >= 8) AAValues.push_back(8);

	for (int v : { 2, 4, 8, 12, 16 }) if (caps.MaxAnisotropy >= DWORD(v)) AFValues.push_back(v);

	bool bFS = (data->style == 0 && data->fullscreen);
	bool bGB = data->trystencil;

	if (bFS || bGB) Config->SceneAntialias = 0;

	setup.PlanetPreloadMode = Config->PlanetPreloadMode;
	setup.PlanetLoadFrequency = Config->PlanetLoadFrequency;
	setup.PlanetTileLoadFlags = Config->PlanetTileLoadFlags - 1;
	setup.SketchpadFont = Config->SketchpadFont;
	setup.DebugLvl = Config->DebugLvl;
	setup.EnableMeshDbg = Config->EnableMeshDbg;
	setup.DebugBreak = Config->DebugBreak;
	setup.TextureMips = Config->TextureMips;
	setup.PostProcess = Config->PostProcess;
	setup.gcGUIMode = Config->gcGUIMode;
	setup.PlanetGlow = Config->PlanetGlow;
	setup.LODBias = int(Config->LODBias * 5.0);
	setup.MeshRes = Config->MeshRes;
	setup.MaxTiles = Config->MaxTiles;
	setup.TileMipmaps = Config->TileMipmaps;
	setup.LightConfig = Config->LightConfig;
	setup.ShadowMapMode = Config->ShadowMapMode;
	setup.ShadowFilter = Config->ShadowFilter;
	setup.TerrainShadowing = Config->TerrainShadowing;
	setup.Convergence = std::clamp(int(Config->Convergence * 100.0), 5, 100);
	setup.Separation = std::clamp(int(Config->Separation), 10, 100);
	setup.EnvMapMode = Config->EnvMapMode;
	setup.CustomCamMode = Config->CustomCamMode;
	setup.EnvMapFaces = Config->EnvMapFaces - 1;
	setup.MicroMode = Config->MicroMode;
	setup.MicroFilter = Config->MicroFilter;
	setup.BlendMode = Config->BlendMode;
	setup.MicroBias = Config->MicroBias;
	setup.PreLBaseVis = Config->PreLBaseVis == 1;
	setup.UseNormalMap = Config->UseNormalMap == 1;
	setup.EnableGlass = Config->EnableGlass == 1;
	setup.NearClipPlane = Config->NearClipPlane == 1;
	setup.CloudMicro = Config->CloudMicro == 1;
	setup.CloudNormals = Config->bCloudNormals == 1;
	setup.GDIOverlay = Config->GDIOverlay == 1;
	setup.AbsAnims = Config->bAbsAnims == 1;
	setup.AtmoQuality = Config->bAtmoQuality == 1;
	setup.Irradiance = Config->bIrradiance == 1;
	setup.Glares = Config->bGlares == 1;
	setup.Flats = Config->bFlats == 1;
	setup.BaseColliders = Config->bShowBaseColliders == 1;
	setup.VesselColliders = Config->bShowVesselColliders == 1;

	setup.EarthCfg = 0;
	auto &earth = AtmoCfgs["Earth"];
	for (size_t i = 0; i < earth.size(); i++) {
		if (Config->AtmoCfg["Earth"] == earth[i].file) {
			setup.EarthCfg = int(i);
			break;
		}
	}

	DWORD af = std::min(caps.MaxAnisotropy, DWORD(Config->Anisotrophy));
	auto ita = std::find(AFValues.begin(), AFValues.end(), int(af));
	if (ita == AFValues.end()) ita = std::find(AFValues.begin(), AFValues.end(), 4);
	setup.AF = (ita == AFValues.end()) ? 0 : int(ita - AFValues.begin());

	DWORD aa = std::min(aamax, DWORD(Config->SceneAntialias));
	auto itb = std::find(AAValues.begin(), AAValues.end(), int(aa));
	if (itb == AAValues.end()) itb = std::find(AAValues.begin(), AAValues.end(), 4);
	setup.AA = (itb == AAValues.end()) ? int(AAValues.size()) - 1 : int(itb - AAValues.begin());
}


void VideoTab::SaveSetupState()
{
	// Combo boxes
	Config->SketchpadFont = setup.SketchpadFont;
	Config->EnvMapMode	  = setup.EnvMapMode;
	Config->CustomCamMode = setup.CustomCamMode;
	Config->EnvMapFaces	  = setup.EnvMapFaces + 1;
	Config->TextureMips	  = setup.TextureMips;
	Config->MicroMode	  = setup.MicroMode;
	Config->MicroFilter	  = setup.MicroFilter;
	Config->BlendMode	  = setup.BlendMode;
	Config->TileMipmaps   = setup.TileMipmaps;
	Config->PostProcess   = setup.PostProcess;
	Config->PlanetTileLoadFlags = setup.PlanetTileLoadFlags + 1;
	Config->LightConfig   = setup.LightConfig;
	Config->ShadowMapMode = setup.ShadowMapMode;
	Config->ShadowFilter  = setup.ShadowFilter;
	Config->TerrainShadowing = setup.TerrainShadowing;
	Config->gcGUIMode	  = setup.gcGUIMode;
	Config->MeshRes		  = setup.MeshRes;
	Config->MaxTiles	  = setup.MaxTiles;

	// Check boxes
	Config->UseNormalMap  = setup.UseNormalMap;
	Config->PreLBaseVis   = setup.PreLBaseVis;
	Config->NearClipPlane = setup.NearClipPlane;
	Config->EnableGlass   = setup.EnableGlass;
	Config->EnableMeshDbg = setup.EnableMeshDbg;
	Config->CloudMicro    = setup.CloudMicro;
	Config->GDIOverlay	  = setup.GDIOverlay;
	Config->bAbsAnims	  = setup.AbsAnims;
	Config->bCloudNormals = setup.CloudNormals;
	Config->bFlats		  = setup.Flats;
	Config->DebugBreak	  = setup.DebugBreak;
	Config->bGlares		  = setup.Glares;
	Config->bIrradiance   = setup.Irradiance;
	Config->bAtmoQuality  = setup.AtmoQuality;

	// Sliders
	Config->Convergence   = double(setup.Convergence) * 0.01;
	Config->Separation	  = double(setup.Separation);
	Config->LODBias       = 0.2 * double(setup.LODBias);
	Config->MicroBias     = setup.MicroBias;
	Config->bShowBaseColliders = setup.BaseColliders;
	Config->bShowVesselColliders = setup.VesselColliders;

	// Other things
	Config->PlanetLoadFrequency = setup.PlanetLoadFrequency;
	Config->PlanetPreloadMode = setup.PlanetPreloadMode;
	Config->PlanetGlow = setup.PlanetGlow;
	Config->DebugLvl = setup.DebugLvl;

	if (setup.AF >= 0 && setup.AF < int(AFValues.size())) Config->Anisotrophy = AFValues[setup.AF];
	else Config->Anisotrophy = 2;

	if (setup.AA >= 0 && setup.AA < int(AAValues.size())) Config->SceneAntialias = AAValues[setup.AA];
	else Config->SceneAntialias = 0;

	auto &earth = AtmoCfgs["Earth"];
	if (setup.EarthCfg >= 0 && setup.EarthCfg < int(earth.size()) && !earth[setup.EarthCfg].file.empty()) Config->AtmoCfg["Earth"] = earth[setup.EarthCfg].file;
	else Config->AtmoCfg["Earth"] = "Earth.atm.cfg";
}


void VideoTab::DrawSetupDialog()
{
	static const char *archive[] = { "Cache only", "Archive only", "Cache & archive" };
	static const char *texmips[] = { "Load as defined", "Autogen missing", "Autogen all" };
	static const char *postproc[] = { "None", "Light glow" };
	static const char *guimode[] = { "Disabled", "Side bars", "Floating", "Windows" };
	static const char *fonts[] = { "Crisp", "Antialiased", "Cleartype" };
	static const char *debug[] = { "0", "1", "2", "3", "4" };
	static const char *meshres[] = { "16", "32" };
	static const char *tiles[] = { "600", "1200", "2400" };
	static const char *mipmaps[] = { "Disabled", "Enabled (slow to load)" };
	static const char *lights[] = { "None", "4x partial", "4x full", "8x partial", "8x full", "12x partial", "12x full", "16x partial", "16x full", "20x partial", "20x full" };
	static const char *shadows[] = { "None", "Focus + payload", "Nearby objects", "All visible objects" };
	static const char *shfilter[] = { "9 samples", "27 samples", "27s dither" };
	static const char *terrain[] = { "None", "Stencil", "Projected" };
	static const char *envmode[] = { "Disable (debug)", "Planet only", "Full scene" };
	static const char *cammode[] = { "Disable", "Enabled" };
	static const char *envfaces[] = { "Light", "Medium", "Heavy" };
	static const char *micromode[] = { "Disabled", "Enabled" };
	static const char *microfilter[] = { "Point (fast/good)", "Linear (fast/bad)", "Anisotropic 2x", "Anisotropic 4x (better)", "Anisotropic 8x", "Anisotropic 16x (slow/best)" };
	static const char *blendmode[] = { "Soft light", "Normal light", "Hard light" };

	float fs = ImGui::GetFontSize();
	float lw = fs * 8.5f;
	float cw = fs * 19.0f;

	const ImGuiViewport *vp = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(vp->GetCenter(), ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));

	if (!ImGui::BeginPopupModal(SETUP_POPUP, NULL, ImGuiWindowFlags_AlwaysAutoResize)) return;

	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();
	bool bNoAA = (data->style == 0 && data->fullscreen) || data->trystencil;

	if (ImGui::BeginTable("##setup", 3, ImGuiTableFlags_BordersInnerV)) {

		ImGui::TableSetupColumn("##c0", ImGuiTableColumnFlags_WidthFixed, cw);
		ImGui::TableSetupColumn("##c1", ImGuiTableColumnFlags_WidthFixed, cw);
		ImGui::TableSetupColumn("##c2", ImGuiTableColumnFlags_WidthFixed, cw);

		// Column 1 ---------------------------------------------------------
		ImGui::TableNextColumn();

		ImGui::SeparatorText("Surface texture load options");
		if (ImGui::RadioButton("Load on demand (recommended)", setup.PlanetPreloadMode == 0)) setup.PlanetPreloadMode = 0;
		ImGui::Indent();
		ImGui::AlignTextToFramePadding();
		ImGui::TextUnformatted("Max. load frequency [Hz]");
		ImGui::SameLine();
		ImGui::SetNextItemWidth(-FLT_MIN);
		ImGui::InputInt("##hz", &setup.PlanetLoadFrequency, 0, 0);
		ImGui::Unindent();
		if (ImGui::RadioButton("Pre-load at session start", setup.PlanetPreloadMode == 1)) setup.PlanetPreloadMode = 1;
		LCombo("Tile archive", lw, &setup.PlanetTileLoadFlags, archive, 3);

		ImGui::SeparatorText("Graphics options");
		{
			std::vector<std::string> aa;
			for (int v : AAValues) aa.push_back(v ? std::to_string(v) + "x" : std::string("None"));
			ImGui::BeginDisabled(bNoAA);
			if (bNoAA) setup.AA = 0;
			LCombo("Anti-aliasing", lw, &setup.AA, aa);
			ImGui::EndDisabled();

			std::vector<std::string> af;
			for (int v : AFValues) af.push_back(std::to_string(v) + "x");
			LCombo("Anisotropic filtering", lw, &setup.AF, af);
		}
		ImGui::AlignTextToFramePadding();
		ImGui::TextUnformatted("Planet glow");
		ImGui::SameLine(lw);
		ImGui::SetNextItemWidth(-FLT_MIN);
		ImGui::InputFloat("##glow", &setup.PlanetGlow, 0.0f, 0.0f, "%3.3f");
		LCombo("Texture mipmaps", lw, &setup.TextureMips, texmips, 3);
		LCombo("Post processing", lw, &setup.PostProcess, postproc, 2);
		LCombo("gcGUI mode", lw, &setup.gcGUIMode, guimode, 4);
		{
			std::vector<std::string> earth;
			for (auto &x : AtmoCfgs["Earth"]) earth.push_back(x.cfg);
			LCombo("Earth visual config", lw, &setup.EarthCfg, earth);
		}

		ImGui::Checkbox("Pre-load base visuals at startup", &setup.PreLBaseVis);
		ImGui::Checkbox("Enable advanced texture maps", &setup.UseNormalMap);
		ImGui::Checkbox("Enable improved glass shading", &setup.EnableGlass);
		ImGui::Checkbox("Disable near clip plane compatibility mode", &setup.NearClipPlane);
		ImGui::Checkbox("Enable cloud micro-textures", &setup.CloudMicro);
		ImGui::Checkbox("Enable cloud normal maps", &setup.CloudNormals);
		ImGui::Checkbox("Enable GDI overlay", &setup.GDIOverlay);
		ImGui::Checkbox("Enable absolute animation handling", &setup.AbsAnims);
		ImGui::Checkbox("Enable increased atmosphere quality", &setup.AtmoQuality);

		// Column 2 ---------------------------------------------------------
		ImGui::TableNextColumn();

		ImGui::SeparatorText("Sketchpad");
		LCombo("Font rendering", lw, &setup.SketchpadFont, fonts, 3);

		ImGui::SeparatorText("Generic configuration");
		LCombo("Debug level", lw, &setup.DebugLvl, debug, 5);
		Check("Enable development tools", &setup.EnableMeshDbg);
		if ((setup.EnableMeshDbg != 0) != bMeshDbgInit) ImGui::TextDisabled("Restart Orbiter to apply");
		Check("Enable debug break", &setup.DebugBreak);

		ImGui::SeparatorText("Terrain resolution");
		ImGui::AlignTextToFramePadding();
		ImGui::TextUnformatted("Texture bias");
		ImGui::SameLine(lw);
		ImGui::SetNextItemWidth(-FLT_MIN);
		ImGui::SliderInt("##lodbias", &setup.LODBias, -10, 10, setup.LODBias < 0 ? "Lower %d" : (setup.LODBias > 0 ? "Higher %d" : "%d"));
		LCombo("Mesh resolution", lw, &setup.MeshRes, meshres, 2);
		LCombo("Max tiles allocated", lw, &setup.MaxTiles, tiles, 3);
		LCombo("Surface mipmaps", lw, &setup.TileMipmaps, mipmaps, 2);

		ImGui::SeparatorText("Local lights");
		ImGui::SetNextItemWidth(-FLT_MIN);
		if (setup.LightConfig < 0 || setup.LightConfig > 10) setup.LightConfig = 0;
		ImGui::Combo("##lights", &setup.LightConfig, lights, 11);

		ImGui::SeparatorText("Self shadows");
		LCombo("Vessel mapping", lw, &setup.ShadowMapMode, shadows, 4);
		LCombo("Filter options", lw, &setup.ShadowFilter, shfilter, 3);
		LCombo("Terrain mapping", lw, &setup.TerrainShadowing, terrain, 3);

		// Column 3 ---------------------------------------------------------
		ImGui::TableNextColumn();

		ImGui::SeparatorText("Stereoscopic 3D");
		{
			char lbl[32];
			ImGui::AlignTextToFramePadding();
			ImGui::TextUnformatted("Convergence");
			ImGui::SameLine(lw);
			ImGui::SetNextItemWidth(-FLT_MIN);
			snprintf(lbl, sizeof(lbl), "%1.2fm", float(setup.Convergence) * 0.01f);
			ImGui::SliderInt("##conv", &setup.Convergence, 5, 100, lbl);
			ImGui::AlignTextToFramePadding();
			ImGui::TextUnformatted("Field depth");
			ImGui::SameLine(lw);
			ImGui::SetNextItemWidth(-FLT_MIN);
			snprintf(lbl, sizeof(lbl), "%d%%%%", setup.Separation);
			ImGui::SliderInt("##sepa", &setup.Separation, 10, 100, lbl);
		}

		ImGui::SeparatorText("Reflections and custom cameras");
		LCombo("Reflection mode", lw, &setup.EnvMapMode, envmode, 3);
		LCombo("Custom cameras", lw, &setup.CustomCamMode, cammode, 2);
		LCombo("Update rate", lw, &setup.EnvMapFaces, envfaces, 3);

		ImGui::SeparatorText("Surface micro textures");
		LCombo("Render mode", lw, &setup.MicroMode, micromode, 2);
		LCombo("Texture filter", lw, &setup.MicroFilter, microfilter, 6);
		LCombo("Blend mode", lw, &setup.BlendMode, blendmode, 3);
		ImGui::AlignTextToFramePadding();
		ImGui::TextUnformatted("Mipmap bias");
		ImGui::SameLine(lw);
		ImGui::SetNextItemWidth(-FLT_MIN);
		ImGui::SliderInt("##microbias", &setup.MicroBias, 0, 10, setup.MicroBias == 0 ? "Sharp" : (setup.MicroBias == 10 ? "Smooth" : "%d"));

		ImGui::Spacing();
		ImGui::Checkbox("Enable dynamic ambient light", &setup.Irradiance);
		ImGui::Checkbox("Enable sun glare", &setup.Glares);
		ImGui::Checkbox("Enable terrain flattening", &setup.Flats);

		ImGui::SeparatorText("Collider visualization");
		ImGui::Checkbox("Show base colliders", &setup.BaseColliders);
		ImGui::Checkbox("Show vessel colliders", &setup.VesselColliders);

		ImGui::EndTable();
	}

	ImGui::Separator();

	if (ImGui::Button("Credits")) {
		LoadCredits();
		ImGui::OpenPopup(CREDITS_POPUP);
	}
	DrawCreditsDialog();

	float bw = fs * 6.0f;
	ImGui::SameLine(ImGui::GetContentRegionMax().x - bw);
	if (ImGui::Button("OK", ImVec2(bw, 0.0f))) {
		SaveSetupState();
		ImGui::CloseCurrentPopup();
	}

	ImGui::EndPopup();
}




// ***************************************************************************************************
// Credits Dialog
// ***************************************************************************************************

void VideoTab::LoadCredits()
{
	std::ifstream file("Modules/MetalClient/Credits.rtf", std::ios::binary);
	if (!file) {
		LogErr("Failed to open a file Modules/MetalClient/Credits.rtf");
		Credits.clear();
		return;
	}
	std::stringstream ss;
	ss << file.rdbuf();
	Credits = RtfToText(ss.str());
}

void VideoTab::DrawCreditsDialog()
{
	float fs = ImGui::GetFontSize();
	const ImGuiViewport *vp = ImGui::GetMainViewport();
	ImGui::SetNextWindowPos(vp->GetCenter(), ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));
	ImGui::SetNextWindowSize(ImVec2(fs * 45.0f, fs * 30.0f), ImGuiCond_Appearing);

	if (!ImGui::BeginPopupModal(CREDITS_POPUP)) return;

	ImGui::BeginChild("##credits", ImVec2(0.0f, -ImGui::GetFrameHeightWithSpacing()), ImGuiChildFlags_Borders);
	ImGui::PushTextWrapPos(0.0f);
	ImGui::TextUnformatted(Credits.c_str());
	ImGui::PopTextWrapPos();
	ImGui::EndChild();

	float bw = fs * 6.0f;
	ImGui::SetCursorPosX(ImGui::GetContentRegionMax().x - bw);
	if (ImGui::Button("Close", ImVec2(bw, 0.0f))) ImGui::CloseCurrentPopup();

	ImGui::EndPopup();
}

bool VideoTab::GetConfigName(const char* file, std::string& cfg, std::string& planet)
{
	std::string filename = "GC/" + std::string(file);
	FILEHANDLE hFile = oapiOpenFile(filename.c_str(), FILE_IN_ZEROONFAIL, CONFIG);
	if (hFile) {
		char ConfigName[32] = {}; char PlanetName[32] = {};
		bool bA = oapiReadItem_string(hFile, (char*)"ConfigName", ConfigName);
		bool bB = oapiReadItem_string(hFile, (char*)"Planet", PlanetName);
		oapiCloseFile(hFile, FILE_IN_ZEROONFAIL);
		cfg = std::string(ConfigName);
		planet = std::string(PlanetName);
		return bA && bB;
	}
	return false;
}

void VideoTab::ScanAtmoCfgs()
{
	AtmoCfgs.clear();

	_AtmoCfg cfg = { "Default", "Earth.atm.cfg"};
	AtmoCfgs["Earth"].push_back(cfg);

	std::string dir = OapiExtension::GetConfigDir();
	std::replace(dir.begin(), dir.end(), '\\', '/');

	std::error_code ec;
	for (auto &entry : fs::directory_iterator(fs::path(dir) / "GC", ec)) {
		if (!entry.is_regular_file(ec)) continue;
		std::string name = entry.path().filename().string();
		if (name.empty() || name[0] == '.') continue;
		if (name.size() < 8 || name.compare(name.size() - 8, 8, "_atm.cfg") != 0) continue;
		std::string cfgname, planet;
		if (GetConfigName(name.c_str(), cfgname, planet)) {
			_AtmoCfg c = { cfgname, name };
			AtmoCfgs[planet].push_back(c);
		}
		else oapiWriteLogV("File Not Found [%s]", name.c_str());
	}
}
