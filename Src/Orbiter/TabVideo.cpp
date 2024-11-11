// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// VideoTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#include <windows.h>
#include "Orbiter.h"
#include "TabVideo.h"
#include "resource.h"

using namespace std;

static PCSTR strInfo_Default = "No graphics engine has been selected. Orbiter will run in console mode.";

//-----------------------------------------------------------------------------
// DefVideoTab class

orbiter::DefVideoTab::DefVideoTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
	idxClient = 0;
	strInfo = 0;
	SetInfoString(strInfo_Default);
}

//-----------------------------------------------------------------------------

orbiter::DefVideoTab::~DefVideoTab()
{
	if (strInfo) {
		delete []strInfo;
		strInfo = NULL;
	}
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_DEV);
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::ShowInterface(HWND hTab, bool show)
{
	static int item[] = {
		IDC_VID_STATIC1, IDC_VID_STATIC2, IDC_VID_STATIC3, IDC_VID_STATIC5,
		IDC_VID_STATIC6, IDC_VID_STATIC7, IDC_VID_STATIC8, IDC_VID_STATIC9,
		IDC_VID_DEVICE, IDC_VID_ENUM, IDC_VID_STENCIL,
		IDC_VID_FULL, IDC_VID_WINDOW, IDC_VID_MODE, IDC_VID_BPP, IDC_VID_VSYNC,
		IDC_VID_PAGEFLIP, IDC_VID_WIDTH, IDC_VID_HEIGHT, IDC_VID_ASPECT,
		IDC_VID_4X3, IDC_VID_16X10, IDC_VID_16X9, IDC_VID_INFO
	};
	for (int i = 0; i < ARRAYSIZE(item); i++) {
		ShowWindow(GetDlgItem(hTab, item[i]), show ? SW_SHOW : SW_HIDE);
	}
}

//-----------------------------------------------------------------------------

BOOL orbiter::DefVideoTab::OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	ShowInterface(hWnd, false);
	EnumerateClients(hWnd);
	return TRUE;
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::OnGraphicsClientLoaded(oapi::GraphicsClient* gc, const PSTR moduleName)
{
	char fname[256];
	_splitpath(moduleName, NULL, NULL, fname, NULL);

	int newIdx = SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_FINDSTRING, -1, (LPARAM)fname);
	if (newIdx != idxClient) {
		SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_SETCURSEL, newIdx, 0);
		ShowInterface(hTab, newIdx > 0);
		pCfg->AddActiveModule(fname);
		idxClient = newIdx;
	}

	HMODULE hMod = LoadLibraryEx(moduleName, 0, LOAD_LIBRARY_AS_DATAFILE);
	if (hMod) {
		char buf[1024];
		// read module info string
		if (LoadString(hMod, 1000, buf, 1024)) {
			buf[1023] = '\0';
			SetInfoString(buf);
		}
		FreeLibrary(hMod);
	}
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
		cfg->CfgDevPrm.Device_out  = data->outputidx;
		cfg->CfgDevPrm.Device_style= data->style;
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
		cfg->CfgDevPrm.Device_out  = 0;
		cfg->CfgDevPrm.Device_style= 1;
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
	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_RESETCONTENT, 0, 0);
	PCSTR strConsole = "Console mode (no engine loaded)";
	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_ADDSTRING, 0, (LPARAM)strConsole);
	ScanDir(hTab, "Modules\\Plugin");
	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_SETCURSEL, 0, 0);
}

//-----------------------------------------------------------------------------
//! Find Graphics engine DLLs in dir
void orbiter::DefVideoTab::ScanDir(HWND hTab, const fs::path& dir)
{
	for (auto& entry : fs::directory_iterator(dir)) {
		fs::path modulepath;
		auto clientname = entry.path().stem().string();
		if (entry.is_directory()) {
			modulepath = dir / clientname / (clientname + ".dll");
			if (!fs::exists(modulepath))
				continue;
		}
		else if (entry.path().extension().string() == ".dll")
			modulepath = entry.path();
		else
			continue;

		// We've found a potential module DLL. Load it.
		HMODULE hMod = LoadLibraryEx(modulepath.string().c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
		if (hMod) {
			char catstr[256];
			// read category string
			if (LoadString(hMod, 1001, catstr, 256)) {
				if (!strcmp(catstr, "Graphics engines")) {
					SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_ADDSTRING, 0, (LPARAM)clientname.c_str());
				}
			}
		}
	}
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SelectClientIndex(UINT idx)
{
	ShowInterface(hTab, idx > 0);

	char name[256];
	if (idxClient) { // unload the current client
		SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_GETLBTEXT, idxClient, (LPARAM)name);
		pCfg->DelActiveModule(name);
		pLp->App()->UnloadModule(name);
		pCfg->CfgDevPrm.Device_idx = -1;
	}
	if (idx) { // load the new client
		const char* path = "Modules\\Plugin";
		SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_GETLBTEXT, idx, (LPARAM)name);
		pLp->App()->LoadModule(path, name);
	}
	else
		SetInfoString(strInfo_Default);
}

void orbiter::DefVideoTab::SetInfoString(PCSTR str)
{
	if (strInfo)
		delete []strInfo;
	strInfo = new char[strlen(str) + 1];
	strcpy(strInfo, str);
}

//-----------------------------------------------------------------------------

INT_PTR CALLBACK orbiter::DefVideoTab::InfoProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		SetWindowText(GetDlgItem(hWnd, IDC_MSG), (PSTR)lParam);
		return TRUE;
	case WM_COMMAND:
		if (IDOK == LOWORD(wParam) || IDCANCEL == LOWORD(wParam))
			EndDialog(hWnd, TRUE);
		return TRUE;
	}
	return FALSE;
}

//-----------------------------------------------------------------------------

BOOL orbiter::DefVideoTab::OnMessage (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_VID_COMBO_MODULE:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				UINT idx = (UINT)SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_GETCURSEL, 0, 0);
				if (idx != CB_ERR) SelectClientIndex(idx);
				return 0;
			}
			break;
		case IDC_VID_MODULE_INFO:
			DialogBoxParam(AppInstance(), MAKEINTRESOURCE(IDD_MSG), LaunchpadWnd(), InfoProc,
				(LPARAM)strInfo);
			return TRUE;
		}
		break;
	}

	// divert video parameters to graphics clients
	oapi::GraphicsClient *gc = pLp->App()->GetGraphicsClient();
	if (gc)
		gc->LaunchpadVideoWndProc (hWnd, uMsg, wParam, lParam);

	return FALSE;
}
