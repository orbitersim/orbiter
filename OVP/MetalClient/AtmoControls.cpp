// ==============================================================
// Atmospheric controls implementation
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014-2026 Jarmo Nikkanen
// ==============================================================

#include "D3D9Client.h"
#include "imgui.h"
#include "D3D9Config.h"
#include "AtmoControls.h"
#include "vObject.h"
#include "vPlanet.h"
#include "Mesh.h"
#include "Scene.h"
#include <stdio.h>

using namespace oapi;

extern D3D9Client *g_client;

// ==============================================================

// Defaut c'tor to init members
ScatterParams::ScatterParams() :
	red      ( 0.650 ),  // 0.400 ... 0.700
	green    ( 0.560 ),  // 0.400 ... 0.700
	blue     ( 0.480 ),  // 0.400 ... 0.700
	rpow	 ( 4.0 ),    // -8.0 ... 8.0
	mpow	 ( 1.0 ),    // -2.0 ... 2.0
	rheight  ( 8.0 ),    // 4.0 ... 40.0 [km]
	mheight  ( 1.0 ),    // 0.0 ... 1.5
	// ----------------------------------------
	trb      ( 1.0 ),    // 0.2 ... 3.0
	tr3D	 ( 1.0 ),
	// ----------------------------------------
	rayrat   ( 1.0 ),    // 0.0 ... 3.0
	ray      ( 1.0 ),	 // 0.0 ... 4.0
	tw_bld	 ( 0.0 ),	 // 0.0 ... 3.5
	// ----------------------------------------
	mie      ( 0.0869 ), // 0.0 ... 8.0
	mphase   ( 0.9831 ), // 0.85 ... 0.999
	// ----------------------------------------
	mierat	 ( 1.0 ),    // 0.0 ... 2.0
	aux2	 ( 1.0 ),    // 0.0 ... 2.0
	aux3	 ( 2.0 ),
	tgamma	 ( 1.0 ),
	mphaseb  ( 1.0 ),
	hazei	 ( 1.0 ),
	tw_bri   ( 0.0 ),
	tw_dst	 ( 0.0 ),
	// ----------------------------------------
	orbalt	 ( 250e3 ),
	visalt	 ( 70e3 ),
	wtrans	 ( 0.1 ),
	wspec	 ( 0.8 ),
	wnrml	 ( 1.0 ),
	wboost	 ( 0.0 ),
	zcolor	 (1.0f, 1.0f, 0.9f),
	hcolor	 (1.0f, 0.7f, 0.0f),
	acolor	 (0.9f, 0.9f, 1.0f),
	suni	 (1.0)
{
}

// ==============================================================

namespace AtmoControls {

class AtmoDialog : public ImGuiDialog
{
public:
	AtmoDialog() : ImGuiDialog("Atmospheric controls###D3D9AtmoControls", { 900.0f, 420.0f }) {}
	void Display() override;
	void OnDraw() override;
	void OnClose() override;
	void Hide()
	{
		active = false;
		oapiCloseDialog(this);
	}
};

ScatterParams defs;
ScatterParams *param = NULL;

std::vector<sSlider> Slider;
std::vector<sValue> Values;

DWORD atmpage = 0;
DWORD atmmode = 0;
DWORD dwCmd = 0;
AtmoDialog *dlg = NULL;
vPlanet *vObj = NULL;
bool bVisualize = false;

struct sGroup {
	const char *title[2];
	int first, last;
};

static const sGroup Groups[] = {
	{ { "Twilight and ambient settings", "" }, 0, 2 },
	{ { "Wavelength and scale height settings", "" }, 3, 7 },
	{ { "Terrain", "" }, 8, 10 },
	{ { "Rayleigh settings", "" }, 11, 12 },
	{ { "Mie settings", "Water rendering" }, 13, 16 },
	{ { "Custom settings", "" }, 17, 19 }
};


// ==============================================================

void SetToolTip(int vid, const char *pszText)
{
	if (!pszText) return;
	for (auto& x : Values) if (x.sprm == vid) {
		x.tooltip = string(pszText);
		break;
	}
}

// ==============================================================

void Create()
{
	vObj = NULL;
	bVisualize = false;

	dwCmd = oapiRegisterCustomCmd((char*)"D3D9 Atmospheric Controls", (char*)"This dialog allows to control various atmospheric parameters and effects", OpenDlgClbk, NULL);

	dlg = new AtmoDialog();

	Slider.clear();
	Values.clear();
	Slider.resize(ATM_SLIDER_COUNT);

	for (auto &s : Slider) {
		s.pos = 0.0f;
		s.val = nullptr;
	}

	// Style flags
	// 0x1 = unit in [km]
	// 0x2 = same for orbital and surface setup
	// 0x8 = x^2 "linearization"
	// 0x10 = x^0.5 "linearization"
	// 0x20 = x^4 "linearization"
	// 0x40 = no 'lerp', special handling

	// Slider ID, Value ID, Page ID, Label, min, max, flags

	// PAGE 0 ------------------------------------------------
	ConfigValue(1,  0,  0, "Distance", 0.0, 0.2);
	ConfigValue(2,  2,  0, "Terrain", 0.01, 3.0, 0x8);
	ConfigValue(3,  6,  0, "Building", 0.01, 1.0, 0x8);
	// -------------------------------------------------------
	ConfigValue(4,  1,  0, "Green", 0.46, 0.65);
	ConfigValue(5,  3,  0, "R-Pow", -8.0, 12.0);
	ConfigValue(6,  12, 0, "M-Pow", -8.0, 12.0);
	ConfigValue(7,  9,  0, "R-Height", 6.0, 600.0, 0x1 | 0x8);
	ConfigValue(8,  11, 0, "M-Height", 0.5, 10.0, 0x1 | 0x8);
	// -------------------------------------------------------
	ConfigValue(9,  13, 0, "Brightness", 0.1, 8.0, 0x8);
	ConfigValue(10, 16, 0, "Gamma", 0.2, 1.5);
	ConfigValue(11, 19, 0, "Light/Shad", 0.0, 5.0, 32);
	// -------------------------------------------------------
	ConfigValue(12, 5,  0, "Density", 0.0, 10.0, 8);
	ConfigValue(13, 4,  0, "Ratio", 0.2, 5.0, 0x20);
	// -------------------------------------------------------
	ConfigValue(14, 7,  0, "Density", 0.01, 10.0, 0x8);
	ConfigValue(15, 8,  0, "Phase-A", 0.02, 0.999, 0x8);
	ConfigValue(16, 17, 0, "Phase-B", 0.0, 8.0);
	ConfigValue(17, 14, 0, "Ratio", 0.2, 5.0, 0x20);
	// -------------------------------------------------------
	ConfigValue(18, 10, 0, "CloudAlt", 0.2, 10.0, 0x1 | 0x40);	// Clouds altitude (km)
	ConfigValue(19, 15, 0, "HDR", 0.1, 4.0, 0x8);	// HDR
	ConfigValue(20, 18, 0, "Intensity", 0.0, 5.0, 0x8 | 0x40);	// Clouds intensity

	// PAGE 1 ------------------------------------------------
	ConfigValue(14,  20, 1, "Normals", 0.2, 2.0, 0x8);
	ConfigValue(15,  21, 1, "Spec",  0.5, 1.5);
	ConfigValue(16,  22, 1, "Color", 0.01, 0.4);
	ConfigValue(17,  23, 1, " ", 0.01, 1.0);


	SetToolTip(0, "Light travel distance behind terminator");
	SetToolTip(1, "Green wave lenght. (Green balance)");
	SetToolTip(2, "Terrain brightness during twilight");
	SetToolTip(3, "Main control for atmospheric rayleigh color composition (4.0 for the Earth)");
	SetToolTip(12, "Main control for atmospheric mie color composition");
	SetToolTip(9, "Atmosphere Ray scale height (7km - 9km for the Earth)");
	SetToolTip(11, "Atmosphere Mie scale height (0.6km - 2km for the Earth)");
	// -------------------------------------------------------
	SetToolTip(13, "Terrain/Ocean brightness control (default 1.0)");
	SetToolTip(16, "Terrain/Ocean gamma control value (default 1.0)");
	SetToolTip(19, "Terrain light and shadow boost");
	// -------------------------------------------------------
	SetToolTip(5, "Overall control for rayleigh scattering (i.e. Haze stickness, atmosphere transparency, optical depth");
	SetToolTip(4, "Rayleigh in-scatter out-scatter ratio (1.0 nominal)");
	SetToolTip(6, "Building lighting level");
	// -------------------------------------------------------
	SetToolTip(7, "Overall scale factor for mie scattering. (Mie-particle density)");
	SetToolTip(8, "Directional strength of Henyey-Greenstein phase function");
	SetToolTip(14, "Mie in-scatter out-scatter ratio (1.0 nominal)");
	// -------------------------------------------------------
	SetToolTip(10, "Altitude for cloud lighting calculations");
	SetToolTip(15, "'HDR' Exposure factor");
	SetToolTip(17, "Omnidirectional mie scattering scale factor");
	SetToolTip(18, "[Dual purpose] Clouds intensity [on surface]. Multiscatter light level [on orbit]");
	// -------------------------------------------------------
	SetToolTip(20, "Water normal map strength");
	SetToolTip(21, "Water reflectivity");
	SetToolTip(22, "Water color / transparency");
	SetToolTip(23, "Unused");
}

// ==============================================================

bool IsActive()
{
	return dlg && dlg->IsActive();
}

// ==============================================================

bool Visualize()
{
	if (!IsActive()) return false;
	return bVisualize;
}

// ==============================================================

void Release()
{
	if (dwCmd) oapiUnregisterCustomCmd(dwCmd);
	dwCmd = 0;
	if (dlg) {
		oapiCloseDialog(dlg);
		delete dlg;
		dlg = NULL;
	}
	vObj = NULL;
	param = NULL;
}

// ==============================================================

void OpenDlgClbk(void *context)
{
	if (IsActive()) {
		oapiOpenDialog(dlg);
		return;
	}

	oapiOpenDialog(dlg);

	Scene *scene = g_client->GetScene();

	vObj = NULL;
	if (scene) vObj = scene->GetCameraNearVisual();

	if (vObj) param = vObj->GetAtmoParams(atmmode);
	else      param = &defs;

	if (scene) SetVisual(scene->GetCameraNearVisual());

	atmpage = 0;
	InitPage(atmpage);
}

// ==============================================================

void InitPage(int p)
{
	for (auto& s : Slider) s.val = nullptr;

	for (auto& v : Values) if (v.page == p) {
		assert(v.slider >= 0 && v.slider < ATM_SLIDER_COUNT);
		Slider[v.slider].val = &v;
	}

	UpdateSliders();
}

// ==============================================================

double GetValue(int id)
{
	for (auto& s : Values) if (s.id == id) {
		assert(s.sprm >= 0 && s.sprm < ATM_DATA_COUNT);
		return param->data[s.sprm];
	}
	LogErr("Invalid Slider ID in AtmoControls");
	return 0.0;
}

// ==============================================================

void ConfigValue(int sid, int vid, int pid, string lbl, double min, double max, int style)
{
	// Slider ID, Value ID, Page ID, Label, min, max, flags
	sValue v;
	v.max = max;
	v.min = min;
	v.style = style;
	v.page = pid;
	v.sprm = vid;
	v.id = vid;
	v.slider = sid - 1;
	v.lbl = lbl;
	Values.push_back(v);
}

// ==============================================================

void SetSlider(sSlider& s)
{
	if (!vObj) return;
	if (!s.val) return;

	auto v = s.val;
	double x = double(s.pos);

	assert(v->sprm >= 0 && v->sprm < ATM_DATA_COUNT);

	if (v->style & 8) x = x * x;
	if (v->style & 16) x = sqrt(x);
	if (v->style & 32) x = pow(x, 4.0);

	double val = v->min * (1.0 - x) + v->max * x;

	if (v->style&2) {
		vObj->GetAtmoParams(1)->data[v->sprm] = val;
		vObj->GetAtmoParams(2)->data[v->sprm] = val;
		vObj->GetAtmoParams(3)->data[v->sprm] = val;
	}
	else {
		param->data[v->sprm] = val;
	}

	UpdateSlider(s, false); // Update value display
}

// ==============================================================

void UpdateSliders()
{
	for (auto& s : Slider) UpdateSlider(s, true);
}

// ==============================================================

void UpdateSlider(sSlider& q, bool bSetPos)
{
	char buf[128];
	if (!param) return;

	if (q.val)
	{
		auto v = q.val;
		assert(v->sprm >= 0 && v->sprm < ATM_DATA_COUNT);
		double val = param->data[v->sprm];

		if (bSetPos) {
			double x = (val - v->min) / (v->max - v->min);
			if (v->style & 8) x = sqrt(x);
			if (v->style & 16) x = x * x;
			if (v->style & 32) x = pow(x, 0.25);
			q.pos = float(std::clamp(x, 0.0, 1.0));
		}

		if (v->style & 1) snprintf(buf, 128, "%.1lf k", val);
		else snprintf(buf, 128, "%.3lf", val);
	}
	else {
		buf[0] = 0;
		q.pos = 0.0f;
	}

	q.dsp = buf;
}

// ==============================================================

vPlanet * GetVisual()
{
	return vObj;
}

// ==============================================================

void SetVisual(vObject *vo)
{
	if (!vo) {
		vObj = NULL;
		param = &defs;
		return;
	}

	if (!IsActive() || !dwCmd) return;

	OBJHANDLE hObj = vo->GetObject();

	if (oapiGetObjectType(hObj)!=OBJTP_PLANET) {
		LogErr("Invalid Object Type in AtmoControls");
		vObj = NULL;
		param = &defs;
		return;
	}

	vObj = static_cast<vPlanet *>(vo);

	if (vObj) param = vObj->GetAtmoParams(0);
	else	  param = &defs;

	UpdateSliders();
}

// ==============================================================

void AtmoDialog::Display()
{
	char title[256];
	snprintf(title, sizeof(title), "Atmospheric controls");

	if (vObj) {
		string file = "??";
		auto cfg = Config->AtmoCfg.find(vObj->GetName());
		if (cfg != Config->AtmoCfg.end()) file = cfg->second;

		param = vObj->GetAtmoParams(atmmode);

		double vd = 0.5f;
		if (atmmode == 0) vd = param->cfg_alt < 0.9999f ? param->cfg_alt : 1.0 - param->cfg_halt;

		snprintf(title, sizeof(title), "Atmospheric controls [%s]", vObj->GetName());
		if (vObj->GetAtmoMode() == 1) snprintf(title, sizeof(title), "Atmospheric controls [%s][%s] [Surface] (%3.1f%%)", file.c_str(), vObj->GetName(), (1.0 - vd) * 100.0);
		if (vObj->GetAtmoMode() == 2) snprintf(title, sizeof(title), "Atmospheric controls [%s][%s] [LowOrbit] (%3.1f%%)", file.c_str(), vObj->GetName(), vd * 100.0);
		if (vObj->GetAtmoMode() == 3) snprintf(title, sizeof(title), "Atmospheric controls [%s][%s] [HighOrbit] (%3.1f%%)", file.c_str(), vObj->GetName(), (1.0 - vd) * 100.0);
	}

	string wnd = string(title) + "###D3D9AtmoControls";

	ImGui::SetNextWindowSize(ImVec2(defaultSize.width, defaultSize.height), ImGuiCond_FirstUseEver);
	if (ImGui::Begin(wnd.c_str(), &active, ImGuiWindowFlags_AlwaysAutoResize)) OnDraw();
	ImGui::End();
	if (!active) OnClose();
}

// ==============================================================

void AtmoDialog::OnClose()
{
	oapiCloseDialog(this);
}

// ==============================================================

void AtmoDialog::OnDraw()
{
	static const char *pages[] = { "Atmospheric controls", "Water and sun-glare" };
	static const char *modes[] = { "Auto", "Surface", "Low orbit", "High orbit" };

	UpdateSliders();

	float fs = ImGui::GetFontSize();
	float sw = fs * 3.2f;
	float sh = fs * 14.0f;

	int page = int(atmpage);
	ImGui::SetNextItemWidth(fs * 14.0f);
	if (ImGui::Combo("##page", &page, pages, 2)) {
		atmpage = DWORD(page);
		InitPage(atmpage);
	}

	bool bFirstGroup = true;

	for (const sGroup &g : Groups) {

		bool bAny = false;
		for (int i = g.first; i <= g.last; i++) if (Slider[i].val) bAny = true;
		if (!bAny) continue;

		if (!bFirstGroup) ImGui::SameLine(0.0f, fs * 1.5f);
		bFirstGroup = false;

		ImGui::BeginGroup();
		const char *gt = g.title[atmpage ? 1 : 0];
		ImGui::TextUnformatted(gt[0] ? gt : " ");
		ImGui::Separator();

		for (int i = g.first; i <= g.last; i++) {

			sSlider &s = Slider[i];
			if (i != g.first) ImGui::SameLine();

			ImGui::BeginGroup();
			ImGui::PushID(i);

			const char *lbl = s.val ? s.val->lbl.c_str() : "";
			float tw = ImGui::CalcTextSize(lbl).x;
			float x0 = ImGui::GetCursorPosX();
			ImGui::SetCursorPosX(x0 + std::max(0.0f, (sw - tw) * 0.5f));
			ImGui::TextUnformatted(lbl);
			ImGui::SetCursorPosX(x0);

			ImGui::BeginDisabled(s.val == nullptr);
			ImGui::SetCursorPosX(x0 + (sw - ImGui::GetFrameHeight()) * 0.5f);
			if (ImGui::VSliderFloat("##s", ImVec2(ImGui::GetFrameHeight(), sh), &s.pos, 0.0f, 1.0f, "")) SetSlider(s);
			if (s.val && s.val->tooltip.size() > 2) ImGui::SetItemTooltip("%s", s.val->tooltip.c_str());
			ImGui::EndDisabled();

			tw = ImGui::CalcTextSize(s.dsp.c_str()).x;
			ImGui::SetCursorPosX(x0 + std::max(0.0f, (sw - tw) * 0.5f));
			ImGui::TextUnformatted(s.dsp.c_str());
			ImGui::SetCursorPosX(x0);
			ImGui::Dummy(ImVec2(sw, 0.0f));

			ImGui::PopID();
			ImGui::EndGroup();
		}

		ImGui::EndGroup();
	}

	ImGui::Separator();

	int mode = int(atmmode);
	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("Configuration");
	ImGui::SameLine();
	ImGui::SetNextItemWidth(fs * 7.0f);
	if (ImGui::Combo("##mode", &mode, modes, 4)) atmmode = DWORD(mode);

	ImGui::SameLine();
	if (ImGui::Button("Copy to surface")) {
		if (vObj && vObj->GetAtmoParams(1) != param) memcpy(vObj->GetAtmoParams(1), param, sizeof(ScatterParams));
	}
	ImGui::SameLine();
	if (ImGui::Button("Copy to low orbit")) {
		if (vObj && vObj->GetAtmoParams(2) != param) memcpy(vObj->GetAtmoParams(2), param, sizeof(ScatterParams));
	}
	ImGui::SameLine();
	if (ImGui::Button("Copy to high orbit")) {
		if (vObj && vObj->GetAtmoParams(3) != param) memcpy(vObj->GetAtmoParams(3), param, sizeof(ScatterParams));
	}
	ImGui::SameLine();
	ImGui::Checkbox("Visualize scatter tables", &bVisualize);

	ImGui::SameLine();
	if (ImGui::Button("Load settings")) {
		if (vObj) {
			vObj->LoadAtmoConfig();
			UpdateSliders();
		}
	}
	ImGui::SameLine();
	if (ImGui::Button("Save settings")) {
		if (vObj) vObj->SaveAtmoConfig();
	}
	ImGui::SameLine();
	if (ImGui::Button("Close")) Hide();
}

} //namespace
