#define STRICT 1
#define ORBITER_MODULE
#include "orbitersdk.h"
#include "DGC_resource.h"
#include <stdio.h>
#include <io.h>

class VesselConfig;
class DGConfig;

static char *hires_enabled = "Textures2\\DG";
static char *hires_disabled = "Textures2\\~DG";

struct {
	HINSTANCE hInst;
	DGConfig *item;
} gParams;

class DGConfig: public LaunchpadItem {
public:
	DGConfig(): LaunchpadItem() {}
	char *Name() { return "DG Configuration"; }
	char *Description();
	bool clbkOpen (HWND hLaunchpad);
	bool HiresEnabled() const;
	void EnableHires (bool enable);
	void InitDialog (HWND hWnd);
	void Apply (HWND hWnd);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
};

char *DGConfig::Description()
{
	static char *desc = "Global configuration for the default Delta-glider.";
	return desc;
}

bool DGConfig::clbkOpen (HWND hLaunchpad)
{
	// respond to user double-clicking the item in the list
	return OpenDialog (gParams.hInst, hLaunchpad, IDD_DGCONFIG, DlgProc);
}

bool DGConfig::HiresEnabled () const
{
	// check if the DG highres texture directory is present
	return (_access (hires_enabled, 0) != -1);
}

void DGConfig::EnableHires (bool enable)
{
	if (HiresEnabled() == enable) return; // nothing to do

	if (enable) {
		rename (hires_disabled, hires_enabled);
	} else {
		// to disable the highres textures, we simply rename the directory
		// so that orbiter's texture manager can't find it
		rename (hires_enabled, hires_disabled);
	}
}

void DGConfig::InitDialog (HWND hWnd)
{
	bool hires = HiresEnabled();
	SendDlgItemMessage (hWnd, IDC_RADIO1, BM_SETCHECK, hires?BST_CHECKED:BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_RADIO2, BM_SETCHECK, hires?BST_UNCHECKED:BST_CHECKED, 0);
}

void DGConfig::Apply (HWND hWnd)
{
	bool enable = (SendDlgItemMessage (hWnd, IDC_RADIO1, BM_GETCHECK, 0, 0) == BST_CHECKED);
	EnableHires (enable);
}

BOOL CALLBACK DGConfig::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((DGConfig*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDOK:
			((DGConfig*)GetWindowLong (hWnd, DWL_USER))->Apply (hWnd);
			EndDialog (hWnd, 0);
			return 0;
		case IDCANCEL:
			EndDialog (hWnd, 0);
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
	gParams.item = new DGConfig;
	// create the new config item
	LAUNCHPADITEM_HANDLE root = oapiFindLaunchpadItem ("Vessel configuration");
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