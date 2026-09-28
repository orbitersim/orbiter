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
#include "DGC_resource.h"
#include <stdio.h>
#ifndef __linux__
#include <io.h>
#else // __linux__
#include <unistd.h>
#include <QAbstractButton>
#include <QDialog>
#endif // __linux__

class VesselConfig;
class DGConfig;

#ifndef __linux__
static const char *hires_enabled = "Textures2\\DG";
static const char *hires_disabled = "Textures2\\~DG";
#else // __linux__
static const char *hires_enabled = "Textures2/DG";
static const char *hires_disabled = "Textures2/~DG";
#endif // __linux__

struct {
#ifndef __linux__
	HINSTANCE hInst;
#else // __linux__
	void *hInst;
#endif // __linux__
	DGConfig *item;
} gParams;

class DGConfig: public LaunchpadItem {
public:
	DGConfig(): LaunchpadItem() {}
	char *Name() { return (char*)"DG Configuration"; }
	char *Description();
#ifndef __linux__
	bool clbkOpen (HWND hLaunchpad);
#else // __linux__
	bool clbkOpen (QWidget *hLaunchpad);
#endif // __linux__
	bool HiresEnabled() const;
	void EnableHires (bool enable);
#ifndef __linux__
	void InitDialog (HWND hWnd);
	void Apply (HWND hWnd);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void InitDialog (QWidget *hWnd);
	void Apply (QWidget *hWnd);
	static void DlgProc (QWidget *hWnd, void *context);
#endif // __linux__
};

char *DGConfig::Description()
{
	return (char*)"Global configuration for the default Delta-glider.";
}

#ifndef __linux__
bool DGConfig::clbkOpen (HWND hLaunchpad)
#else // __linux__
bool DGConfig::clbkOpen (QWidget *hLaunchpad)
#endif // __linux__
{
	// respond to user double-clicking the item in the list
	return OpenDialog (gParams.hInst, hLaunchpad, IDD_DGCONFIG, DlgProc);
}

bool DGConfig::HiresEnabled () const
{
	// check if the DG highres texture directory is present
#ifndef __linux__
	return (_access (hires_enabled, 0) != -1);
#else // __linux__
	return (access (oapiResolvePath (hires_enabled).c_str(), F_OK) != -1);
#endif // __linux__
}

void DGConfig::EnableHires (bool enable)
{
	if (HiresEnabled() == enable) return; // nothing to do

	if (enable) {
#ifndef __linux__
		rename (hires_disabled, hires_enabled);
#else // __linux__
		rename (oapiResolvePath (hires_disabled).c_str(), oapiResolvePath (hires_enabled).c_str());
#endif // __linux__
	} else {
		// to disable the highres textures, we simply rename the directory
		// so that orbiter's texture manager can't find it
#ifndef __linux__
		rename (hires_enabled, hires_disabled);
#else // __linux__
		rename (oapiResolvePath (hires_enabled).c_str(), oapiResolvePath (hires_disabled).c_str());
#endif // __linux__
	}
}

#ifndef __linux__
void DGConfig::InitDialog (HWND hWnd)
#else // __linux__
void DGConfig::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	bool hires = HiresEnabled();
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_RADIO1, BM_SETCHECK, hires?BST_CHECKED:BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_RADIO2, BM_SETCHECK, hires?BST_UNCHECKED:BST_CHECKED, 0);
#else // __linux__
	DlgItem<QAbstractButton> (hWnd, IDC_RADIO1)->setChecked (hires);
	DlgItem<QAbstractButton> (hWnd, IDC_RADIO2)->setChecked (!hires);
#endif // __linux__
}

#ifndef __linux__
void DGConfig::Apply (HWND hWnd)
#else // __linux__
void DGConfig::Apply (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	bool enable = (SendDlgItemMessage (hWnd, IDC_RADIO1, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	bool enable = DlgItem<QAbstractButton> (hWnd, IDC_RADIO1)->isChecked();
#endif // __linux__
	EnableHires (enable);
}

#ifndef __linux__
INT_PTR CALLBACK DGConfig::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void DGConfig::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG:
		((DGConfig*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDOK:
			((DGConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->Apply (hWnd);
			EndDialog (hWnd, 0);
			return 0;
		case IDCANCEL:
			EndDialog (hWnd, 0);
			return 0;
		}
		break;
	}
	return 0;
#else // __linux__
	// WM_INITDIALOG
	((DGConfig*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDOK:
			((DGConfig*)context)->Apply (hWnd); // DWLP_USER: the item is the dialog context
			qobject_cast<QDialog*> (hWnd)->done (0);
			return;
		case IDCANCEL:
			qobject_cast<QDialog*> (hWnd)->done (0);
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
