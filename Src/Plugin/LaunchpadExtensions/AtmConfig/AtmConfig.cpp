// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define ORBITER_MODULE

#include "orbitersdk.h"
#include "resource.h"
#include <io.h>

using namespace std;

class AtmConfig;

const char *CelbodyDir = "Modules\\Celbody";
char *ModuleItem = "MODULE_ATM";

struct {
	HINSTANCE hInst;
	AtmConfig *item;
} gParams;

class AtmConfig: public LaunchpadItem {
public:
	AtmConfig();
	~AtmConfig();
	char *Name() { return "Atmosphere Configuration"; }
	char *Description();
	void Read (const char *celbody);
	void Write(const char *celbody);
	bool clbkOpen (HWND hLaunchpad);
	void InitDialog (HWND hWnd);
	void UpdateData (HWND hWnd);
	void Apply (HWND hWnd);
	void OpenHelp (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	// scan the 'Modules\Celbody' folder for directories, and
	// 'atmosphere' directories in these.
	void ScanCelbodies (HWND hWnd);

	// scan the 'Modules\Celbody\<Name>\Atmosphere' folder for
	// atmosphere plugin modules
	void ScanModules (const char *celbody);

	void ClearModules ();

	// Populate celbody list and atmosphere model list
	void ListCelbodies (HWND hWnd);
	void ListModules (HWND hWnd);

	void CelbodyChanged (HWND hWnd);
	void ModelChanged (HWND hWnd);

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
	static char *desc = "Configure atmospheric parameters for celestial bodies.";
	return desc;
}

void AtmConfig::Read (const char *celbody)
{
	char cfgname[256];
	strcpy (cfgname, celbody); strcat (cfgname, "\\Atmosphere.cfg");
	FILEHANDLE hFile = oapiOpenFile (cfgname, FILE_IN, CONFIG);
	if (hFile) {
		char name[256];
		oapiReadItem_string (hFile, ModuleItem, name);
		for (module_curr = module_first; module_curr; module_curr = module_curr->next)
			if (!_stricmp (module_curr->module_name, name)) break;
		oapiCloseFile (hFile, FILE_IN);
	}
}

void AtmConfig::Write (const char *celbody)
{
	char cfgname[256];
	strcpy (cfgname, celbody); strcat (cfgname, "\\Atmosphere.cfg");
	FILEHANDLE hFile = oapiOpenFile (cfgname, FILE_OUT, CONFIG);
	if (hFile) {
		if (module_curr && module_curr->module_name[0])
			oapiWriteItem_string (hFile, ModuleItem, module_curr->module_name);
		else
			oapiWriteItem_string (hFile, ModuleItem, "[None]");
		oapiCloseFile (hFile, FILE_OUT);
	}
}

bool AtmConfig::clbkOpen (HWND hLaunchpad)
{
	// respond to user double-clicking the item in the list
	return OpenDialog (gParams.hInst, hLaunchpad, IDD_CONFIG, DlgProc);
}

void AtmConfig::InitDialog (HWND hWnd)
{
	ListCelbodies (hWnd);
}

void AtmConfig::ListCelbodies (HWND hWnd)
{
	ScanCelbodies (hWnd);
	if (!SendDlgItemMessage (hWnd, IDC_COMBO2, CB_GETCOUNT, 0, 0)) return;
	int idx = SendDlgItemMessage (hWnd, IDC_COMBO2, CB_FINDSTRINGEXACT, -1, (LPARAM)"Earth");
	if (idx == CB_ERR) idx = 0;
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_SETCURSEL, idx, 0);
	CelbodyChanged (hWnd);
}

void AtmConfig::ListModules (HWND hWnd)
{
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"[None]");

	if (!celbody[0]) return; // nothing to do

	ScanModules (celbody);
	Read (celbody);

	MODULESPEC *ms = module_first;
	while (ms) {
		SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)ms->model_name);
		ms = ms->next;
	}
	int idx = 0;
	if (module_curr) {
		MODULESPEC *ms = module_first;
		for (idx = 0; ms && ms != module_curr; ms = ms->next, idx++);
		idx++;
	}
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_SETCURSEL, idx, 0);
	ModelChanged (hWnd);
}

void AtmConfig::UpdateData (HWND hWnd)
{
	int i, model = (int)SendDlgItemMessage (hWnd, IDC_COMBO1, CB_GETCURSEL, 0, 0);
	if (!model) {
		module_curr = 0;
	} else {
		for (module_curr = module_first, i = 1; module_curr && i < model; module_curr = module_curr->next, i++);
	}
}

void AtmConfig::CelbodyChanged (HWND hWnd)
{
	int idx = SendDlgItemMessage (hWnd, IDC_COMBO2, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_GETLBTEXT, idx, (LPARAM)celbody);
	ListModules (hWnd);
}

void AtmConfig::ModelChanged (HWND hWnd)
{
	int i, model = (int)SendDlgItemMessage (hWnd, IDC_COMBO1, CB_GETCURSEL, 0, 0);
	if (!model) {
		SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), "Atmosphere effects disabled.");
	} else {
		MODULESPEC *ms = module_first;
		for (i = 1; i < model && ms; i++)
			ms = ms->next;
		if (ms) SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), ms->model_desc);
	}
}

void AtmConfig::Apply (HWND hWnd)
{
	UpdateData (hWnd);
	Write (celbody);
}

void AtmConfig::OpenHelp (HWND hWnd)
{
	HELPCONTEXT hc = {
		"html/Orbiter.chm",
		"extra_atmconfig",
		0, 0
	};
	oapiOpenLaunchpadHelp (&hc);
}

void AtmConfig::ScanCelbodies (HWND hWnd)
{
	SendDlgItemMessage (hWnd, IDC_COMBO2, CB_RESETCONTENT, 0, 0);

	char filespec[256];
	sprintf (filespec, "%s\\*.*", CelbodyDir);
	_finddata_t info, subinfo;
	intptr_t id = _findfirst (filespec, &info);
	if (id >= 0) {
		intptr_t res;
		do {
			if (info.attrib & _A_SUBDIR) {
				sprintf (filespec, "%s\\%s\\atmosphere", CelbodyDir, info.name);
				intptr_t id2 = _findfirst (filespec, &subinfo);
				if (id2 >= 0 && (subinfo.attrib & _A_SUBDIR)) {
					SendDlgItemMessage (hWnd, IDC_COMBO2, CB_ADDSTRING, 0, (LPARAM)info.name);
				}
				_findclose (id2);
			}
			res = _findnext (id, &info);
		} while (!res);
	}
	_findclose (id);
}

void AtmConfig::ScanModules (const char *celbody)
{
	ClearModules ();

	char filespec[256];
	sprintf (filespec, "%s\\%s\\Atmosphere\\*.dll", CelbodyDir, celbody);
	_finddata_t info;
	intptr_t id = _findfirst (filespec, &info);
	if (id >= 0) {
		int res;
		MODULESPEC *module_last = 0;
		do {
			info.name[strlen(info.name)-4] = '\0'; // cut off '.dll' extension
			char path[256];
			sprintf (path, "%s\\%s\\Atmosphere\\%s", CelbodyDir, celbody, info.name);
			char *model_name = 0;
			MODULESPEC *ms = new MODULESPEC;
			if (module_last) module_last->next = ms;
			else             module_first = ms;
			module_last = ms;
			strncpy (ms->module_name, info.name, 255);
			strncpy (ms->model_name, info.name, 255);
			ms->model_desc[0] = '\0';
			ms->next = 0;

			// get info from the module
			HINSTANCE hModule = LoadLibrary (path);
			if (hModule) {
				char *(*name_func)() = (char*(*)())GetProcAddress (hModule, "ModelName");
				if (name_func) strncpy (ms->model_name, name_func(), 255);
				char *(*desc_func)() = (char*(*)())GetProcAddress (hModule, "ModelDesc");
				if (desc_func) strncpy (ms->model_desc, desc_func(), 511);
				FreeLibrary (hModule);
			}
			res = _findnext (id, &info);
		} while (!res);
	}
	_findclose (id);
}

INT_PTR CALLBACK AtmConfig::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		SetWindowLongPtr (hWnd, DWLP_USER, (LONG)lParam); // store class instance for later reference
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
}

// ==============================================================
// The DLL entry point
// ==============================================================

DLLCLBK void InitModule (HINSTANCE hDLL)
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

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister the launchpad items
	oapiUnregisterLaunchpadItem (gParams.item);
	delete gParams.item;
}