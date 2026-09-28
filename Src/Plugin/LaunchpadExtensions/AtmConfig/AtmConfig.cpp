// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __linux__
#define STRICT 1
#else // __linux__
// STRICT left out: windows.h handle type-checking switch
#endif // __linux__
#define ORBITER_MODULE

#ifndef __linux__
#include "orbitersdk.h"
#else // __linux__
#include "Orbitersdk.h"
#include "OrbiterResource.h"
#endif // __linux__
#include "resource.h"
#ifdef __linux__
#include <QComboBox>
#include <QDialog>
#include <cstring>
#include <strings.h>
#include <dlfcn.h>
#endif // __linux__
#include <filesystem>
namespace fs = std::filesystem;

using namespace std;

class AtmConfig;

const fs::path CelbodyDir = fs::path("Modules") / "Celbody";
const char *ModuleItem = "MODULE_ATM";

struct {
#ifndef __linux__
	HINSTANCE hInst;
#else // __linux__
	void *hInst;
#endif // __linux__
	AtmConfig *item;
} gParams;

class AtmConfig: public LaunchpadItem {
public:
	AtmConfig();
	~AtmConfig();
	char *Name() { return (char*)"Atmosphere Configuration"; }
	char *Description();
	void Read (const char *celbody);
	void Write(const char *celbody);
#ifndef __linux__
	bool clbkOpen (HWND hLaunchpad);
	void InitDialog (HWND hWnd);
	void UpdateData (HWND hWnd);
	void Apply (HWND hWnd);
	void OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	bool clbkOpen (QWidget *hLaunchpad);
	void InitDialog (QWidget *hWnd);
	void UpdateData (QWidget *hWnd);
	void Apply (QWidget *hWnd);
	void OpenHelp (QWidget *hWnd);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

protected:
	// scan the 'Modules\Celbody' folder for directories, and
	// 'atmosphere' directories in these.
#ifndef __linux__
	void ScanCelbodies (HWND hWnd);
#else // __linux__
	void ScanCelbodies (QWidget *hWnd);
#endif // __linux__

	// scan the 'Modules\Celbody\<Name>\Atmosphere' folder for
	// atmosphere plugin modules
	void ScanModules (const char *celbody);

	void ClearModules ();

	// Populate celbody list and atmosphere model list
#ifndef __linux__
	void ListCelbodies (HWND hWnd);
	void ListModules (HWND hWnd);
#else // __linux__
	void ListCelbodies (QWidget *hWnd);
	void ListModules (QWidget *hWnd);
#endif // __linux__

#ifndef __linux__
	void CelbodyChanged (HWND hWnd);
	void ModelChanged (HWND hWnd);
#else // __linux__
	void CelbodyChanged (QWidget *hWnd);
	void ModelChanged (QWidget *hWnd);
#endif // __linux__

	char celbody[256];

	struct MODULESPEC {
		char module_name[256];
		char model_name[256];
		char model_desc[512];
		MODULESPEC *next;
	} *module_first, *module_curr;
	
};

AtmConfig::AtmConfig(): LaunchpadItem()
{
	module_first = module_curr = 0;
	celbody[0] = '\0';
}

AtmConfig::~AtmConfig ()
{
	ClearModules ();
}

void AtmConfig::ClearModules ()
{
	while (module_first) {
		MODULESPEC *ms = module_first;
		module_first = module_first->next;
		delete ms;
	}
	module_curr = 0;
}

char *AtmConfig::Description()
{
	return (char*)"Configure atmospheric parameters for celestial bodies.";
}

void AtmConfig::Read (const char *celbody)
{
	char cfgname[256];
#ifndef __linux__
	strcpy (cfgname, celbody); strcat (cfgname, "\\Atmosphere.cfg");
#else // __linux__
	strcpy (cfgname, celbody); strcat (cfgname, "/Atmosphere.cfg");
#endif // __linux__
	FILEHANDLE hFile = oapiOpenFile (cfgname, FILE_IN, CONFIG);
	if (hFile) {
		char name[256];
		oapiReadItem_string (hFile, (char*)ModuleItem, name);
		for (module_curr = module_first; module_curr; module_curr = module_curr->next)
#ifndef __linux__
			if (!_stricmp (module_curr->module_name, name)) break;
#else // __linux__
			if (!strcasecmp (module_curr->module_name, name)) break;
#endif // __linux__
		oapiCloseFile (hFile, FILE_IN);
	}
}

void AtmConfig::Write (const char *celbody)
{
	char cfgname[256];
#ifndef __linux__
	strcpy (cfgname, celbody); strcat (cfgname, "\\Atmosphere.cfg");
#else // __linux__
	strcpy (cfgname, celbody); strcat (cfgname, "/Atmosphere.cfg");
#endif // __linux__
	FILEHANDLE hFile = oapiOpenFile (cfgname, FILE_OUT, CONFIG);
	if (hFile) {
		if (module_curr && module_curr->module_name[0])
			oapiWriteItem_string (hFile, (char*)ModuleItem, module_curr->module_name);
		else
			oapiWriteItem_string (hFile, (char*)ModuleItem, (char*)"[None]");
		oapiCloseFile (hFile, FILE_OUT);
	}
}

#ifndef __linux__
bool AtmConfig::clbkOpen (HWND hLaunchpad)
#else // __linux__
bool AtmConfig::clbkOpen (QWidget *hLaunchpad)
#endif // __linux__
{
	// respond to user double-clicking the item in the list
	return OpenDialog (gParams.hInst, hLaunchpad, IDD_CONFIG, DlgProc);
}

#ifndef __linux__
void AtmConfig::InitDialog (HWND hWnd)
#else // __linux__
void AtmConfig::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	ListCelbodies (hWnd);
}

#ifndef __linux__
void AtmConfig::ListCelbodies (HWND hWnd)
#else // __linux__
void AtmConfig::ListCelbodies (QWidget *hWnd)
#endif // __linux__
{
	ScanCelbodies (hWnd);
#ifndef __linux__
	if (!SendDlgItemMessage (hWnd, IDC_COMBO2, CB_GETCOUNT, 0, 0)) return;
	int idx = SendDlgItemMessage (hWnd, IDC_COMBO2, CB_FINDSTRINGEXACT, -1, (LPARAM)"Earth");
	if (idx == CB_ERR) idx = 0;
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_SETCURSEL, idx, 0);
#else // __linux__
	if (!DlgItem<QComboBox> (hWnd, IDC_COMBO2)->count()) return;
	int idx = DlgItem<QComboBox> (hWnd, IDC_COMBO2)->findText ("Earth", Qt::MatchFixedString); // CB_FINDSTRINGEXACT ignores case
	if (idx < 0) idx = 0;
	DlgItem<QComboBox> (hWnd, IDC_COMBO2)->setCurrentIndex (idx);
#endif // __linux__
	CelbodyChanged (hWnd);
}

#ifndef __linux__
void AtmConfig::ListModules (HWND hWnd)
#else // __linux__
void AtmConfig::ListModules (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"[None]");
#else // __linux__
	DlgItem<QComboBox> (hWnd, IDC_COMBO1)->clear();
	oapiComboAddString (DlgItem<QComboBox> (hWnd, IDC_COMBO1), "[None]");
#endif // __linux__

	if (!celbody[0]) return; // nothing to do

	ScanModules (celbody);
	Read (celbody);

	MODULESPEC *ms = module_first;
	while (ms) {
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)ms->model_name);
#else // __linux__
		oapiComboAddString (DlgItem<QComboBox> (hWnd, IDC_COMBO1), ms->model_name);
#endif // __linux__
		ms = ms->next;
	}
	int idx = 0;
	if (module_curr) {
		MODULESPEC *ms = module_first;
		for (idx = 0; ms && ms != module_curr; ms = ms->next, idx++);
		idx++;
	}
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_SETCURSEL, idx, 0);
#else // __linux__
	DlgItem<QComboBox> (hWnd, IDC_COMBO1)->setCurrentIndex (idx);
#endif // __linux__
	ModelChanged (hWnd);
}

#ifndef __linux__
void AtmConfig::UpdateData (HWND hWnd)
#else // __linux__
void AtmConfig::UpdateData (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	int i, model = (int)SendDlgItemMessage (hWnd, IDC_COMBO1, CB_GETCURSEL, 0, 0);
#else // __linux__
	int i, model = DlgItem<QComboBox> (hWnd, IDC_COMBO1)->currentIndex();
#endif // __linux__
	if (!model) {
		module_curr = 0;
	} else {
		for (module_curr = module_first, i = 1; module_curr && i < model; module_curr = module_curr->next, i++);
	}
}

#ifndef __linux__
void AtmConfig::CelbodyChanged (HWND hWnd)
#else // __linux__
void AtmConfig::CelbodyChanged (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	int idx = SendDlgItemMessage (hWnd, IDC_COMBO2, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_GETLBTEXT, idx, (LPARAM)celbody);
#else // __linux__
	int idx = DlgItem<QComboBox> (hWnd, IDC_COMBO2)->currentIndex();
	snprintf (celbody, 256, "%s", DlgItem<QComboBox> (hWnd, IDC_COMBO2)->itemText (idx).toUtf8().constData());
#endif // __linux__
	ListModules (hWnd);
}

#ifndef __linux__
void AtmConfig::ModelChanged (HWND hWnd)
#else // __linux__
void AtmConfig::ModelChanged (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	int i, model = (int)SendDlgItemMessage (hWnd, IDC_COMBO1, CB_GETCURSEL, 0, 0);
#else // __linux__
	int i, model = DlgItem<QComboBox> (hWnd, IDC_COMBO1)->currentIndex();
#endif // __linux__
	if (!model) {
#ifndef __linux__
		SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), "Atmosphere effects disabled.");
#else // __linux__
		oapiSetDlgItemText (hWnd, IDC_EDIT1, "Atmosphere effects disabled.");
#endif // __linux__
	} else {
		MODULESPEC *ms = module_first;
		for (i = 1; i < model && ms; i++)
			ms = ms->next;
#ifndef __linux__
		if (ms) SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), ms->model_desc);
#else // __linux__
		if (ms) oapiSetDlgItemText (hWnd, IDC_EDIT1, ms->model_desc);
#endif // __linux__
	}
}

#ifndef __linux__
void AtmConfig::Apply (HWND hWnd)
#else // __linux__
void AtmConfig::Apply (QWidget *hWnd)
#endif // __linux__
{
	UpdateData (hWnd);
	Write (celbody);
}

#ifndef __linux__
void AtmConfig::OpenHelp (HWND hWnd)
#else // __linux__
void AtmConfig::OpenHelp (QWidget *hWnd)
#endif // __linux__
{
	HELPCONTEXT hc = {
		(char*)"html/Orbiter.chm",
		(char*)"extra_atmconfig",
		0, 0
	};
	oapiOpenLaunchpadHelp (&hc);
}

#ifndef __linux__
void AtmConfig::ScanCelbodies (HWND hWnd)
#else // __linux__
void AtmConfig::ScanCelbodies (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_RESETCONTENT, 0, 0);
#else // __linux__
	DlgItem<QComboBox> (hWnd, IDC_COMBO2)->clear();
#endif // __linux__

	for (auto& dir : fs::directory_iterator(CelbodyDir)) {
		auto path = dir.path();
		if (dir.is_directory()) {
			std::error_code ec;
			auto atmdir = fs::directory_entry(path / "Atmosphere", ec);
			if(!ec && atmdir.is_directory()) {
#ifndef __linux__
				SendDlgItemMessage(hWnd, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)path.filename().string().c_str());
#else // __linux__
				oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_COMBO2), path.filename().string().c_str());
#endif // __linux__
			}
		}
	}
}

void AtmConfig::ScanModules (const char *celbody)
{
	ClearModules ();

	auto path = CelbodyDir / celbody / "Atmosphere";
	MODULESPEC* module_last = 0;
	for (auto& entry : fs::directory_iterator(path)) {
		auto module = entry.path();
#ifndef __linux__
		if (module.extension().string() == ".dll") {
#else // __linux__
		if (module.extension().string() == ".so") {
#endif // __linux__
			const auto name = module.stem().string();

			MODULESPEC* ms = new MODULESPEC;
			if (module_last) module_last->next = ms;
			else             module_first = ms;
			module_last = ms;
			strncpy(ms->module_name, name.c_str(), 255);
			strncpy(ms->model_name, name.c_str(), 255);
			ms->model_desc[0] = '\0';
			ms->next = 0;

			// get info from the module
#ifndef __linux__
			HINSTANCE hModule = LoadLibrary(module.string().c_str());
#else // __linux__
			void *hModule = dlopen(module.string().c_str(), RTLD_NOW);
#endif // __linux__
			if (hModule) {
#ifndef __linux__
				char* (*name_func)() = (char* (*)())GetProcAddress(hModule, "ModelName");
#else // __linux__
				char* (*name_func)() = (char* (*)())dlsym(hModule, "ModelName");
#endif // __linux__
				if (name_func) strncpy(ms->model_name, name_func(), 255);
#ifndef __linux__
				char* (*desc_func)() = (char* (*)())GetProcAddress(hModule, "ModelDesc");
#else // __linux__
				char* (*desc_func)() = (char* (*)())dlsym(hModule, "ModelDesc");
#endif // __linux__
				if (desc_func) strncpy(ms->model_desc, desc_func(), 511);
#ifndef __linux__
				FreeLibrary(hModule);
#else // __linux__
				dlclose(hModule);
#endif // __linux__
			}
		}
	}
}

#ifndef __linux__
INT_PTR CALLBACK AtmConfig::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void AtmConfig::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG:
		SetWindowLongPtr (hWnd, DWLP_USER, (LONG_PTR)lParam); // store class instance for later reference
		((AtmConfig*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDOK:
			((AtmConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->Apply (hWnd);
			//EndDialog (hWnd, 0);
			return 0;
		case IDCANCEL:
			EndDialog (hWnd, 0);
			return 0;
		case IDC_BUTTON1:
			((AtmConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDC_COMBO1:
			if (HIWORD (wParam) == CBN_SELCHANGE)
				((AtmConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->ModelChanged (hWnd);
			return 0;
		case IDC_COMBO2:
			if (HIWORD (wParam) == CBN_SELCHANGE)
				((AtmConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->CelbodyChanged (hWnd);
			return 0;
		}
		break;
	}
	return 0;
#else // __linux__
	// WM_INITDIALOG: the class instance is the context, kept by the handler below (DWLP_USER)
		((AtmConfig*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDOK:
			((AtmConfig*)context)->Apply (hWnd);
			//EndDialog (hWnd, 0);
			return;
		case IDCANCEL:
			qobject_cast<QDialog*> (hWnd)->done (0);
			return;
		case IDC_BUTTON1:
			((AtmConfig*)context)->OpenHelp (hWnd);
			return;
		case IDC_COMBO1:
			if (code == RESN_SELCHANGE)
				((AtmConfig*)context)->ModelChanged (hWnd);
			return;
		case IDC_COMBO2:
			if (code == RESN_SELCHANGE)
				((AtmConfig*)context)->CelbodyChanged (hWnd);
			return;
		}
	});
#endif // __linux__
}

// ==============================================================
// The DLL entry point
// ==============================================================

#ifndef __linux__
DLLCLBK void InitModule (HINSTANCE hDLL)
#else // __linux__
DLLCLBK void InitModule (void *hDLL)
#endif // __linux__
{
	gParams.hInst = hDLL;
	gParams.item = new AtmConfig;
	// create the new config item
	LAUNCHPADITEM_HANDLE root = oapiFindLaunchpadItem ("Celestial body configuration");
	// find the config root entry provided by orbiter
	oapiRegisterLaunchpadItem (gParams.item, root);
	// register the DG config entry
}

// ==============================================================
// The DLL exit point
// ==============================================================

#ifndef __linux__
DLLCLBK void ExitModule (HINSTANCE hDLL)
#else // __linux__
DLLCLBK void ExitModule (void *hDLL)
#endif // __linux__
{
	// Unregister the launchpad items
	oapiUnregisterLaunchpadItem (gParams.item);
	delete gParams.item;
}
