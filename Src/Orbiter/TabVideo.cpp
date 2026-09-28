// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// VideoTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#ifndef __linux__
#include <windows.h>
#endif // !__linux__
#include "Orbiter.h"
#include "TabVideo.h"
#include "resource.h"
#ifdef __linux__
#include "ResDialog.h"
#include <QComboBox>
#include <QDialog>
#include <QPlainTextEdit>
#include <QPushButton>
#endif // __linux__

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

#ifndef __linux__
void orbiter::DefVideoTab::ShowInterface(HWND hTab, bool show)
#else // __linux__
void orbiter::DefVideoTab::ShowInterface(QWidget *hTab, bool show)
#endif // __linux__
{
	static int item[] = {
		IDC_VID_STATIC1, IDC_VID_STATIC2, IDC_VID_STATIC3, IDC_VID_STATIC5,
		IDC_VID_STATIC6, IDC_VID_STATIC7, IDC_VID_STATIC8, IDC_VID_STATIC9,
		IDC_VID_DEVICE, IDC_VID_ENUM, IDC_VID_STENCIL,
		IDC_VID_FULL, IDC_VID_WINDOW, IDC_VID_MODE, IDC_VID_BPP, IDC_VID_VSYNC,
		IDC_VID_PAGEFLIP, IDC_VID_WIDTH, IDC_VID_HEIGHT, IDC_VID_ASPECT,
		IDC_VID_4X3, IDC_VID_16X10, IDC_VID_16X9, IDC_VID_INFO
	};
#ifndef __linux__
	for (int i = 0; i < ARRAYSIZE(item); i++) {
		ShowWindow(GetDlgItem(hTab, item[i]), show ? SW_SHOW : SW_HIDE);
#else // __linux__
	for (size_t i = 0; i < sizeof(item)/sizeof(item[0]); i++) {
		oapiResDlgItem(hTab, item[i])->setVisible(show);
#endif // __linux__
	}
}

//-----------------------------------------------------------------------------

#ifndef __linux__
BOOL orbiter::DefVideoTab::OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL orbiter::DefVideoTab::OnInitDialog(QWidget *hWnd)
#endif // __linux__
{
	ShowInterface(hWnd, false);
	EnumerateClients(hWnd);
#ifdef __linux__

	// WM_COMMAND
	QObject::connect(DlgItem<QComboBox>(hWnd, IDC_VID_COMBO_MODULE), &QComboBox::activated, hWnd, [this](int idx) {
		if (idx >= 0) SelectClientIndex(idx); // CBN_SELCHANGE
	});
	QObject::connect(DlgItem<QPushButton>(hWnd, IDC_VID_MODULE_INFO), &QPushButton::clicked, hWnd, [this]() {
		QDialog *dlg = qobject_cast<QDialog*>(oapiCreateResDialog(AppInstance(), IDD_MSG, LaunchpadWnd()));
		if (!dlg) return;
		InfoProc(dlg, strInfo);
		dlg->exec(); // DialogBoxParam
		delete dlg;
	});
#endif // __linux__
	return TRUE;
}

//-----------------------------------------------------------------------------

#ifndef __linux__
void orbiter::DefVideoTab::OnGraphicsClientLoaded(oapi::GraphicsClient* gc, const PSTR moduleName)
#else // __linux__
void orbiter::DefVideoTab::OnGraphicsClientLoaded(oapi::GraphicsClient* gc, const char *moduleName)
#endif // __linux__
{
#ifndef __linux__
	char fname[256];
	_splitpath(moduleName, NULL, NULL, fname, NULL);
#else // __linux__
	std::string fname = fs::path(moduleName).stem().string();
#endif // __linux__

#ifndef __linux__
	int newIdx = SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_FINDSTRING, -1, (LPARAM)fname);
	if (newIdx != idxClient) {
		SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_SETCURSEL, newIdx, 0);
#else // __linux__
	int newIdx = DlgItem<QComboBox>(hTab, IDC_VID_COMBO_MODULE)->findText(QString::fromStdString(fname), Qt::MatchStartsWith);
	if (newIdx != (int)idxClient) {
		DlgItem<QComboBox>(hTab, IDC_VID_COMBO_MODULE)->setCurrentIndex(newIdx);
#endif // __linux__
		ShowInterface(hTab, newIdx > 0);
#ifndef __linux__
		pCfg->AddActiveModule(fname);
#else // __linux__
		pCfg->AddActiveModule(fname.c_str());
#endif // __linux__
		idxClient = newIdx;
	}

#ifndef __linux__
	HMODULE hMod = LoadLibraryEx(moduleName, 0, LOAD_LIBRARY_AS_DATAFILE);
	if (hMod) {
		char buf[1024];
		// read module info string
		if (LoadString(hMod, 1000, buf, 1024)) {
			buf[1023] = '\0';
			SetInfoString(buf);
		}
		FreeLibrary(hMod);
#else // __linux__
	char buf[1024];
	// read module info string
	if (LoadModuleString(moduleName, 1000, buf, 1024)) {
		buf[1023] = '\0';
		SetInfoString(buf);
#endif // __linux__
	}
#ifdef __linux__

	// the client connects the signals of the video tab controls it uses (window procedure of the tab)
	gc->LaunchpadVideoWndProc(hTab);
#endif // __linux__
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

#ifndef __linux__
void orbiter::DefVideoTab::EnumerateClients(HWND hTab)
#else // __linux__
void orbiter::DefVideoTab::EnumerateClients(QWidget *hTab)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_RESETCONTENT, 0, 0);
#else // __linux__
	QComboBox *cb = DlgItem<QComboBox>(hTab, IDC_VID_COMBO_MODULE);
	cb->clear();
#endif // __linux__
	PCSTR strConsole = "Console mode (no engine loaded)";
#ifndef __linux__
	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_ADDSTRING, 0, (LPARAM)strConsole);
	ScanDir(hTab, "Modules\\Plugin");
	SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_SETCURSEL, 0, 0);
#else // __linux__
	oapiComboAddString(cb, strConsole);
	ScanDir(hTab, "Modules/Plugin");
	cb->setCurrentIndex(0);
#endif // __linux__
}

//-----------------------------------------------------------------------------
#ifndef __linux__
//! Find Graphics engine DLLs in dir
void orbiter::DefVideoTab::ScanDir(HWND hTab, const fs::path& dir)
#else // __linux__
//! Find Graphics engine modules in dir
void orbiter::DefVideoTab::ScanDir(QWidget *hTab, const fs::path& dir)
#endif // __linux__
{
#ifndef __linux__
	for (auto& entry : fs::directory_iterator(dir)) {
#else // __linux__
	std::error_code ec;
	fs::path rdir = oapiResolvePath(dir.string().c_str());
	for (auto& entry : fs::directory_iterator(rdir, ec)) {
#endif // __linux__
		fs::path modulepath;
		auto clientname = entry.path().stem().string();
		if (entry.is_directory()) {
#ifndef __linux__
			modulepath = dir / clientname / (clientname + ".dll");
#else // __linux__
			modulepath = oapiResolvePath((rdir / clientname / (clientname + ".so")).string().c_str());
#endif // __linux__
			if (!fs::exists(modulepath))
				continue;
		}
#ifndef __linux__
		else if (entry.path().extension().string() == ".dll")
#else // __linux__
		else if (entry.path().extension().string() == ".so")
#endif // __linux__
			modulepath = entry.path();
		else
			continue;

#ifndef __linux__
		// We've found a potential module DLL. Load it.
		HMODULE hMod = LoadLibraryEx(modulepath.string().c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
		if (hMod) {
			char catstr[256];
			// read category string
			if (LoadString(hMod, 1001, catstr, 256)) {
				if (!strcmp(catstr, "Graphics engines")) {
					SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_ADDSTRING, 0, (LPARAM)clientname.c_str());
				}
#else // __linux__
		// We've found a potential module. Read its strings without loading it.
		char catstr[256];
		// read category string
		if (LoadModuleString(modulepath.string().c_str(), 1001, catstr, 256)) {
			if (!strcmp(catstr, "Graphics engines")) {
				oapiComboAddString(DlgItem<QComboBox>(hTab, IDC_VID_COMBO_MODULE), clientname.c_str());
#endif // __linux__
			}
		}
	}
}

//-----------------------------------------------------------------------------

void orbiter::DefVideoTab::SelectClientIndex(UINT idx)
{
	ShowInterface(hTab, idx > 0);

	char name[256];
#ifdef __linux__
	QComboBox *cb = DlgItem<QComboBox>(hTab, IDC_VID_COMBO_MODULE);
#endif // __linux__
	if (idxClient) { // unload the current client
#ifndef __linux__
		SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_GETLBTEXT, idxClient, (LPARAM)name);
#else // __linux__
		snprintf(name, 256, "%s", cb->itemText(idxClient).toUtf8().constData());
		// drop the client's connections to the video controls with the client
		static int item[] = {
			IDC_VID_DEVICE, IDC_VID_ENUM, IDC_VID_STENCIL, IDC_VID_FULL, IDC_VID_WINDOW, IDC_VID_MODE, IDC_VID_BPP,
			IDC_VID_VSYNC, IDC_VID_PAGEFLIP, IDC_VID_WIDTH, IDC_VID_HEIGHT, IDC_VID_ASPECT, IDC_VID_4X3, IDC_VID_16X10,
			IDC_VID_16X9, IDC_VID_INFO
		};
		for (int id : item)
			QObject::disconnect(oapiResDlgItem(hTab, id), nullptr, nullptr, nullptr);
#endif // __linux__
		pCfg->DelActiveModule(name);
		pLp->App()->UnloadModule(name);
		pCfg->CfgDevPrm.Device_idx = -1;
	}
	if (idx) { // load the new client
#ifndef __linux__
		const char* path = "Modules\\Plugin";
		SendDlgItemMessage(hTab, IDC_VID_COMBO_MODULE, CB_GETLBTEXT, idx, (LPARAM)name);
#else // __linux__
		const char* path = "Modules/Plugin";
		snprintf(name, 256, "%s", cb->itemText(idx).toUtf8().constData());
#endif // __linux__
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

#ifndef __linux__
INT_PTR CALLBACK orbiter::DefVideoTab::InfoProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void orbiter::DefVideoTab::InfoProc(QWidget *hWnd, const char *info)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	DlgItem<QPlainTextEdit>(hWnd, IDC_MSG)->setPlainText(QString::fromUtf8(info));
	// WM_COMMAND
	QObject::connect(DlgItem<QPushButton>(hWnd, IDOK), &QPushButton::clicked, qobject_cast<QDialog*>(hWnd), &QDialog::accept);
#endif // __linux__
}

#ifndef __linux__
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
#else // __linux__
// the video parameters go to the graphics client through the signals it connects in LaunchpadVideoWndProc
#endif // __linux__
