// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define ORBITER_MODULE

#include "orbitersdk.h"
#include "imgui.h"
#include <algorithm>
#include <filesystem>
#include <string>
#include <vector>
namespace fs = std::filesystem;

class AtmConfig;

const fs::path CelbodyDir = fs::path("Modules") / "Celbody";
const char *ModuleItem = "MODULE_ATM";

struct {
	HINSTANCE hInst;
	AtmConfig *item;
} gParams;

class AtmConfigDialog;

class AtmConfig: public LaunchpadItem {
	friend class AtmConfigDialog;
public:
	AtmConfig();
	~AtmConfig();
	char *Name() { return (char*)"Atmosphere Configuration"; }
	char *Description();
	bool clbkOpen();

protected:
	struct MODULESPEC {
		std::string module_name;
		std::string model_name;
		std::string model_desc;
	};

	// scan the 'Modules/Celbody' folder for directories containing
	// an 'Atmosphere' directory
	void ScanCelbodies();

	// scan the 'Modules/Celbody/<Name>/Atmosphere' folder for
	// atmosphere plugin modules
	void ScanModules(const std::string &celbody);

	void Read(const std::string &celbody);
	void Write(const std::string &celbody);
	void CelbodyChanged(int idx);
	void OpenHelp();

	std::vector<std::string> celbodies;
	std::vector<MODULESPEC> modules;
	int celbody;
	int model; // 0 = none, otherwise index into modules + 1
	AtmConfigDialog *dlg;
};

class AtmConfigDialog: public ImGuiDialog {
public:
	AtmConfigDialog(AtmConfig *cfg): ImGuiDialog("Atmospheric parameter configuration"), item(cfg) {}

	void Display() override
	{
		const float em = ImGui::GetFontSize();
		ImGuiViewport *vp = ImGui::GetMainViewport();
		ImGui::SetNextWindowPos(vp->GetCenter(), ImGuiCond_Appearing, ImVec2(0.5f, 0.5f));
		ImGui::SetNextWindowSizeConstraints(ImVec2(em * 26.0f, 0.0f), ImVec2(em * 26.0f, FLT_MAX));
		if (ImGui::Begin(name.c_str(), &active, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoSavedSettings))
			OnDraw();
		ImGui::End();
	}

	void OnDraw() override
	{
		const float em = ImGui::GetFontSize();
		ImGui::SeparatorText("Celestial body");
		ImGui::SetNextItemWidth(-FLT_MIN);
		const char *cb = item->celbody >= 0 && item->celbody < (int)item->celbodies.size() ? item->celbodies[item->celbody].c_str() : "";
		if (ImGui::BeginCombo("##celbody", cb)) {
			for (int i = 0; i < (int)item->celbodies.size(); i++)
				if (ImGui::Selectable(item->celbodies[i].c_str(), i == item->celbody))
					item->CelbodyChanged(i);
			ImGui::EndCombo();
		}

		ImGui::SeparatorText("Atmosphere model");
		ImGui::SetNextItemWidth(-FLT_MIN);
		const char *mn = item->model > 0 && item->model <= (int)item->modules.size() ? item->modules[item->model - 1].model_name.c_str() : "[None]";
		if (ImGui::BeginCombo("##model", mn)) {
			if (ImGui::Selectable("[None]", item->model == 0))
				item->model = 0;
			for (int i = 0; i < (int)item->modules.size(); i++) {
				ImGui::PushID(i);
				if (ImGui::Selectable(item->modules[i].model_name.c_str(), item->model == i + 1))
					item->model = i + 1;
				ImGui::PopID();
			}
			ImGui::EndCombo();
		}
		const char *desc = item->model > 0 && item->model <= (int)item->modules.size()
			? item->modules[item->model - 1].model_desc.c_str() : "Atmosphere effects disabled.";
		ImGui::TextWrapped("%s", desc);

		ImGui::Spacing();
		ImGui::Separator();
		ImGui::Spacing();
		float bw = em * 5.0f;
		if (ImGui::Button("Help", ImVec2(bw, 0)))
			item->OpenHelp();
		ImGui::SameLine(ImGui::GetContentRegionMax().x - 2.0f * bw - ImGui::GetStyle().ItemSpacing.x);
		ImGui::BeginDisabled(item->celbodies.empty());
		if (ImGui::Button("Apply", ImVec2(bw, 0)))
			item->Write(item->celbodies[item->celbody]);
		ImGui::EndDisabled();
		ImGui::SameLine();
		if (ImGui::Button("Close", ImVec2(bw, 0)))
			active = false;
	}

private:
	AtmConfig *item;
};

AtmConfig::AtmConfig(): LaunchpadItem()
{
	celbody = -1;
	model = 0;
	dlg = nullptr;
}

AtmConfig::~AtmConfig ()
{
	if (dlg) {
		oapiCloseDialog(dlg);
		delete dlg;
	}
}

char *AtmConfig::Description()
{
	return (char*)"Configure atmospheric parameters for celestial bodies.";
}

void AtmConfig::Read (const std::string &celbody)
{
	model = 0;
	std::string cfgname = celbody + "/Atmosphere.cfg";
	FILEHANDLE hFile = oapiOpenFile (cfgname.c_str(), FILE_IN, CONFIG);
	if (hFile) {
		char name[256] = "";
		if (oapiReadItem_string (hFile, (char*)ModuleItem, name)) {
			for (size_t i = 0; i < modules.size(); i++)
				if (!strcasecmp (modules[i].module_name.c_str(), name)) { model = (int)i + 1; break; }
		}
		oapiCloseFile (hFile, FILE_IN);
	}
}

void AtmConfig::Write (const std::string &celbody)
{
	std::string cfgname = celbody + "/Atmosphere.cfg";
	FILEHANDLE hFile = oapiOpenFile (cfgname.c_str(), FILE_OUT, CONFIG);
	if (hFile) {
		if (model > 0 && model <= (int)modules.size())
			oapiWriteItem_string (hFile, (char*)ModuleItem, (char*)modules[model - 1].module_name.c_str());
		else
			oapiWriteItem_string (hFile, (char*)ModuleItem, (char*)"[None]");
		oapiCloseFile (hFile, FILE_OUT);
	}
}

bool AtmConfig::clbkOpen ()
{
	if (!dlg) dlg = new AtmConfigDialog(this);
	if (!dlg->IsActive()) {
		ScanCelbodies();
		int idx = 0;
		for (size_t i = 0; i < celbodies.size(); i++)
			if (celbodies[i] == "Earth") { idx = (int)i; break; }
		CelbodyChanged(celbodies.empty() ? -1 : idx);
	}
	oapiOpenDialog(dlg);
	return true;
}

void AtmConfig::CelbodyChanged (int idx)
{
	celbody = idx;
	modules.clear();
	model = 0;
	if (idx < 0 || idx >= (int)celbodies.size()) return;
	ScanModules(celbodies[idx]);
	Read(celbodies[idx]);
}

void AtmConfig::OpenHelp ()
{
	HELPCONTEXT hc = {
		(char*)"html/Orbiter.chm",
		(char*)"extra_atmconfig",
		0, 0
	};
	oapiOpenLaunchpadHelp (&hc);
}

void AtmConfig::ScanCelbodies ()
{
	celbodies.clear();
	std::error_code ec;
	for (auto& dir : fs::directory_iterator(CelbodyDir, ec)) {
		if (dir.is_directory(ec) && fs::is_directory(dir.path() / "Atmosphere", ec))
			celbodies.push_back(dir.path().filename().string());
	}
	std::sort(celbodies.begin(), celbodies.end(), [](const std::string &a, const std::string &b) {
		return strcasecmp(a.c_str(), b.c_str()) < 0;
	});
}

void AtmConfig::ScanModules (const std::string &celbody)
{
	modules.clear();
	const std::string suffix = OCompatModuleSuffix();
	std::error_code ec;
	for (auto& entry : fs::directory_iterator(CelbodyDir / celbody / "Atmosphere", ec)) {
		auto module = entry.path();
		if (module.extension().string() != suffix) continue;
		MODULESPEC ms;
		ms.module_name = ms.model_name = module.stem().string();
		HINSTANCE hModule = LoadLibrary(module.string().c_str());
		if (hModule) {
			char* (*name_func)() = (char* (*)())GetProcAddress(hModule, "ModelName");
			if (name_func) ms.model_name = name_func();
			char* (*desc_func)() = (char* (*)())GetProcAddress(hModule, "ModelDesc");
			if (desc_func) ms.model_desc = desc_func();
			FreeLibrary(hModule);
		}
		modules.push_back(ms);
	}
}

// ==============================================================
// The DLL entry point
// ==============================================================

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	gParams.hInst = hDLL;
	gParams.item = new AtmConfig;
	LAUNCHPADITEM_HANDLE root = oapiFindLaunchpadItem ("Celestial body configuration");
	oapiRegisterLaunchpadItem (gParams.item, root);
}

// ==============================================================
// The DLL exit point
// ==============================================================

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	oapiUnregisterLaunchpadItem (gParams.item);
	delete gParams.item;
}
