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
#include "AC_resource.h"
#include <stdio.h>
#ifndef __linux__
#include <io.h>
#else // __linux__
#include <unistd.h>
#include <QAbstractButton>
#include <QDialog>
#endif // __linux__

class VesselConfig;
class AtlantisConfig;

#ifndef __linux__
static const char *tex_hires_enabled = "Textures2\\Atlantis";
static const char *tex_hires_disabled = "Textures2\\~Atlantis";
#else // __linux__
static const char *tex_hires_enabled = "Textures2/Atlantis";
static const char *tex_hires_disabled = "Textures2/~Atlantis";
#endif // __linux__

#ifndef __linux__
static const char *vcmsh_fname = "Meshes\\Atlantis\\AtlantisVC.msh";
static const char *msh_hires_bkup = "Meshes\\Atlantis\\~AtlantisVC_hi.msh";
static const char *msh_lores_bkup = "Meshes\\Atlantis\\~AtlantisVC_lo.msh";
#else // __linux__
static const char *vcmsh_fname = "Meshes/Atlantis/AtlantisVC.msh";
static const char *msh_hires_bkup = "Meshes/Atlantis/~AtlantisVC_hi.msh";
static const char *msh_lores_bkup = "Meshes/Atlantis/~AtlantisVC_lo.msh";

// not upstream: io.h _access and rename on Orbiter data paths (resolved case-insensitively)
static int access_path (const char *path, int mode)
{
	return access (oapiResolvePath (path).c_str(), mode);
}

static int rename_path (const char *from, const char *to)
{
	return rename (oapiResolvePath (from).c_str(), oapiResolvePath (to).c_str());
}

// not upstream: BM_SETCHECK / BM_GETCHECK on a dialog control
static void SetCheck (QWidget *hDlg, int id, bool check)
{
	if (QAbstractButton *b = DlgItem<QAbstractButton> (hDlg, id)) b->setChecked (check);
}

static bool IsChecked (QWidget *hDlg, int id)
{
	QAbstractButton *b = DlgItem<QAbstractButton> (hDlg, id);
	return (b && b->isChecked());
}
#endif // __linux__

struct {
#ifndef __linux__
	HINSTANCE hInst;
#else // __linux__
	void *hInst;
#endif // __linux__
	AtlantisConfig *item;
} gParams;

class AtlantisConfig: public LaunchpadItem {
public:
	AtlantisConfig(): LaunchpadItem() {}
	char *Name() { return (char*)"Atlantis Configuration"; }
	char *Description();
#ifndef __linux__
	bool clbkOpen (HWND hLaunchpad);
#else // __linux__
	bool clbkOpen (QWidget *hLaunchpad);
#endif // __linux__
	bool TexHiresEnabled() const;
	void TexEnableHires (bool enable);
	bool MshHiresEnabled() const;
	bool MshEnableHires (bool enable);
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

char *AtlantisConfig::Description()
{
	return (char*)"Global configuration for the default Space Shuttle Atlantis.";
}

#ifndef __linux__
bool AtlantisConfig::clbkOpen (HWND hLaunchpad)
#else // __linux__
bool AtlantisConfig::clbkOpen (QWidget *hLaunchpad)
#endif // __linux__
{
	// respond to user double-clicking the item in the list
	return OpenDialog (gParams.hInst, hLaunchpad, IDD_ACONFIG, DlgProc);
}

bool AtlantisConfig::TexHiresEnabled () const
{
	// check if the Atlantis highres texture directory is present
#ifndef __linux__
	return (_access (tex_hires_enabled, 0) != -1);
#else // __linux__
	return (access_path (tex_hires_enabled, 0) != -1);
#endif // __linux__
}

void AtlantisConfig::TexEnableHires (bool enable)
{
	if (TexHiresEnabled() == enable) return; // nothing to do

	if (enable) {
#ifndef __linux__
		rename (tex_hires_disabled, tex_hires_enabled);
#else // __linux__
		rename_path (tex_hires_disabled, tex_hires_enabled);
#endif // __linux__
	} else {
		// to disable the highres textures, we simply rename the directory
		// so that orbiter's texture manager can't find it
#ifndef __linux__
		rename (tex_hires_enabled, tex_hires_disabled);
#else // __linux__
		rename_path (tex_hires_enabled, tex_hires_disabled);
#endif // __linux__
	}
}

bool AtlantisConfig::MshHiresEnabled () const
{
	// check if backup of low-res mesh is present
#ifndef __linux__
	return (_access (msh_lores_bkup, 0) != -1);
#else // __linux__
	return (access_path (msh_lores_bkup, 0) != -1);
#endif // __linux__
}

bool AtlantisConfig::MshEnableHires (bool enable)
{
	if (MshHiresEnabled() == enable) return false; // nothing to do

	if (enable) {
#ifndef __linux__
		if (_access (msh_hires_bkup, 0) == -1) return false; // high-res backup not found
		if (_access (vcmsh_fname, 0) != -1 && _access (msh_lores_bkup, 0) == -1)
			rename (vcmsh_fname, msh_lores_bkup); // back up low-res mesh
		rename (msh_hires_bkup, vcmsh_fname); // activate high-res mesh
#else // __linux__
		if (access_path (msh_hires_bkup, 0) == -1) return false; // high-res backup not found
		if (access_path (vcmsh_fname, 0) != -1 && access_path (msh_lores_bkup, 0) == -1)
			rename_path (vcmsh_fname, msh_lores_bkup); // back up low-res mesh
		rename_path (msh_hires_bkup, vcmsh_fname); // activate high-res mesh
#endif // __linux__
	} else {
#ifndef __linux__
		if (_access (msh_lores_bkup, 0) == -1) return false; // low-res backup not found
		if (_access(vcmsh_fname, 0) != -1 && _access (msh_hires_bkup, 0) == -1)
			rename (vcmsh_fname, msh_hires_bkup); // back up high-res mesh
		rename (msh_lores_bkup, vcmsh_fname); // activate low-res mesh
#else // __linux__
		if (access_path (msh_lores_bkup, 0) == -1) return false; // low-res backup not found
		if (access_path (vcmsh_fname, 0) != -1 && access_path (msh_hires_bkup, 0) == -1)
			rename_path (vcmsh_fname, msh_hires_bkup); // back up high-res mesh
		rename_path (msh_lores_bkup, vcmsh_fname); // activate low-res mesh
#endif // __linux__
	}
	return true;
}

#ifndef __linux__
void AtlantisConfig::InitDialog (HWND hWnd)
#else // __linux__
void AtlantisConfig::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	bool texhires = TexHiresEnabled();
	bool mshhires = MshHiresEnabled();
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_RADIO1, BM_SETCHECK, texhires?BST_CHECKED:BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_RADIO2, BM_SETCHECK, texhires?BST_UNCHECKED:BST_CHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_RADIO3, BM_SETCHECK, mshhires?BST_CHECKED:BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_RADIO4, BM_SETCHECK, mshhires?BST_UNCHECKED:BST_CHECKED, 0);
#else // __linux__
	SetCheck (hWnd, IDC_RADIO1, texhires);
	SetCheck (hWnd, IDC_RADIO2, !texhires);
	SetCheck (hWnd, IDC_RADIO3, mshhires);
	SetCheck (hWnd, IDC_RADIO4, !mshhires);
#endif // __linux__
}

#ifndef __linux__
void AtlantisConfig::Apply (HWND hWnd)
#else // __linux__
void AtlantisConfig::Apply (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	bool texhires = (SendDlgItemMessage (hWnd, IDC_RADIO1, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	bool texhires = IsChecked (hWnd, IDC_RADIO1);
#endif // __linux__
	TexEnableHires (texhires);
#ifndef __linux__
	bool mshhires = (SendDlgItemMessage (hWnd, IDC_RADIO3, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	bool mshhires = IsChecked (hWnd, IDC_RADIO3);
#endif // __linux__
	MshEnableHires (mshhires);
}

#ifndef __linux__
INT_PTR CALLBACK AtlantisConfig::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void AtlantisConfig::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG:
		((AtlantisConfig*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDOK:
			((AtlantisConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->Apply (hWnd);
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
	((AtlantisConfig*)context)->InitDialog (hWnd);
	// WM_COMMAND (the item passed as DWLP_USER is the context)
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDOK:
			((AtlantisConfig*)context)->Apply (hWnd);
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
	gParams.item = new AtlantisConfig;
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
