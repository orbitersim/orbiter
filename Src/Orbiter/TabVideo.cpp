// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// VideoTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#include <windows.h>
#include <commctrl.h>
#include <io.h>
#include "Orbiter.h"
#include "TabVideo.h"
#include "resource.h"

using namespace std;

//-----------------------------------------------------------------------------
// DefVideoTab class

orbiter::DefVideoTab::DefVideoTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
	idxClient = 0;
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_VID);

	static int item[] = {
		IDC_VID_LABEL_MODULE, IDC_VID_COMBO_MODULE, IDD_PAGE_DEV
	};

	RegisterItemPositions (item, 3);
}

//-----------------------------------------------------------------------------

static INT_PTR CALLBACK msgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return FALSE;
}

BOOL orbiter::DefVideoTab::InitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	HWND hT = CreateDialogParam(AppInstance(), MAKEINTRESOURCE(IDD_PAGE_DEV), hWnd, TabProcHook, (LPARAM)this);
	SetWindowLongPtr(hT, GWLP_ID, IDD_PAGE_DEV); // set the control ID to make GetDlgItem work for the sub-dialog
	ShowWindow(hT, SW_HIDE);

	EnumerateClients(hWnd);
	return TRUE;
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SetConfig (Config *cfg)
{
	// retrieve standard parameters from client, if available
	oapi::GraphicsClient *gc = pLp->App()->GetGraphicsClient();
	if (gc) {
		gc->clbkRefreshVideoData();
		oapi::GraphicsClient::VIDEODATA *data = gc->GetVideoData();
		cfg->CfgDevPrm.bFullscreen = data->fullscreen;
		cfg->CfgDevPrm.bNoVsync    = data->novsync;
		cfg->CfgDevPrm.bPageflip   = data->pageflip;
		cfg->CfgDevPrm.bTryStencil = data->trystencil;
		cfg->CfgDevPrm.bForceEnum  = data->forceenum;
		cfg->CfgDevPrm.WinW        = data->winw;
		cfg->CfgDevPrm.WinH        = data->winh;
		cfg->CfgDevPrm.Device_idx  = data->deviceidx;
		cfg->CfgDevPrm.Device_mode = data->modeidx;
	} else {
		// should not be required
		cfg->CfgDevPrm.bFullscreen = false;
		cfg->CfgDevPrm.bNoVsync    = true;
		cfg->CfgDevPrm.bPageflip   = true;
		cfg->CfgDevPrm.bTryStencil = false;
		cfg->CfgDevPrm.bForceEnum  = true;
		cfg->CfgDevPrm.WinW        = 400;
		cfg->CfgDevPrm.WinH        = 300;
		cfg->CfgDevPrm.Device_idx  = 0;
		cfg->CfgDevPrm.Device_mode = 0;
	}
	cfg->CfgDevPrm.bStereo = false; // not currently set
}

//-----------------------------------------------------------------------------

bool orbiter::DefVideoTab::OpenHelp ()
{
	OpenTabHelp ("tab_video");
	return true;
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::EnumerateClients(HWND hTab)
{
	const PSTR strConsole = "Console mode (no engine loaded)";
	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_ADDSTRING, 0, (LPARAM)strConsole);

	ScanDir(hTab, "Modules\\Plugin");

	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_SETCURSEL, 0, 0);
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::ScanDir(HWND hTab, const PSTR dir)
{
	char pattern[256], name[256];
	sprintf(pattern, "%s\\*.dll", dir);
	struct _finddata_t fdata;
	intptr_t fh = _findfirst(pattern, &fdata);
	if (fh == -1) return; // nothing found
	do {
		sprintf(name, "%s\\%s", dir, fdata.name);
		HMODULE hMod = LoadLibraryEx(name, 0, LOAD_LIBRARY_AS_DATAFILE);
		if (hMod) {
			char catstr[256];
			// read category string
			if (LoadString(hMod, 1001, catstr, 256)) {
				if (!strcmp(catstr, "Graphics engines")) {
					char clientname[256];
					strncpy(clientname, fdata.name, strlen(fdata.name) - 4);
					clientname[strlen(fdata.name) - 4] = '\0';
					SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_ADDSTRING, 0, (LPARAM)clientname);
				}
			}
		}
	} while (!_findnext(fh, &fdata));
	_findclose(fh);
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SelectClientIndex(UINT idx)
{
	char name[256];
	if (idxClient) {
		SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_GETLBTEXT, idxClient, (LPARAM)name);
		pCfg->DelModule(name);
		pLp->App()->UnloadModule(name);
	}
	if (idxClient = idx) {
		const char* path = "Modules\\Plugin";
		SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_GETLBTEXT, idxClient, (LPARAM)name);
		pCfg->AddModule(name);
		pLp->App()->LoadModule(path, name);
	}
	ShowWindow(GetDlgItem(hTab, IDD_PAGE_DEV), idx ? SW_SHOW : SW_HIDE);
}

//-----------------------------------------------------------------------------

INT_PTR orbiter::DefVideoTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		if (LOWORD(wParam) == IDC_VID_COMBO_MODULE && HIWORD(wParam) == CBN_SELCHANGE) {
			UINT idx = (UINT)SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_GETCURSEL, 0, 0);
			if (idx != CB_ERR) SelectClientIndex(idx);
			return 0;
		}
		break;
	}

	// divert video parameters to graphics clients
	oapi::GraphicsClient *gc = pLp->App()->GetGraphicsClient();
	if (gc)
		gc->LaunchpadVideoWndProc (hWnd, uMsg, wParam, lParam);

	return FALSE;
}