// ==============================================================
// Class VideoTab (implementation)
// Manages the user selections in the "Video" tab of the Orbiter
// Launchpad dialog.
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2026 Martin Schweiger
//				 2010-2016 Jarmo Nikkanen (D3D9Client implementation)
// ==============================================================

#include "D3D9Client.h"
#include "VideoTab.h"
#include "resource.h"
#include "resource_video.h"
#include "VideoTab.h"
#include "AABBUtil.h"
#include "D3D9Config.h"
// Commctrl.h and richedit.h left out: the controls are Qt widgets
#include "OapiExtension.h"
#include "OrbiterResource.h"
#include <QAbstractButton>
#include <QComboBox>
#include <QDialog>
#include <QFile>
#include <QGuiApplication>
#include <QKeyEvent>
#include <QLineEdit>
#include <QMessageBox>
#include <QScreen>
#include <QSlider>
#include <QTextCharFormat>
#include <QTextCursor>
#include <QTextDocument>
#include <QTextEdit>
#include <map>
#include <QTreeWidget>
#include <QVulkanInstance>
#include <algorithm>
#include <cctype>
#include <filesystem>
#include <functional>
#include <string>
#include <strings.h>
#include <vector>
#include <sstream>

using namespace oapi;

const UINT IDC_SCENARIO_TREE = (oapiGetOrbiterVersion() >= 111105) ? 1090 : 1088;

// EnumChildWindows callback: the descendant that holds the scenario tree
bool EnumChildProc(QWidget *hwnd, QWidget **lParam)
{
	if (DlgItem<QTreeWidget>(hwnd, IDC_SCENARIO_TREE)) {
		*lParam = hwnd; 
		return false;
	}
	return true;
}

// not upstream: the adapters (IDirect3D9::GetAdapterCount/GetAdapterIdentifier) in vkEnumeratePhysicalDevices order, as D3D9Frame picks them
static std::vector<VkPhysicalDevice> Adapters()
{
	std::vector<VkPhysicalDevice> pd;
	UINT n = 0;
	if (!g_pD3DObject) return pd;
	vkEnumeratePhysicalDevices(g_pD3DObject->vkInstance(), &n, NULL);
	pd.resize(n);
	if (n) vkEnumeratePhysicalDevices(g_pD3DObject->vkInstance(), &n, pd.data());
	return pd;
}

// not upstream: D3DDISPLAYMODE of a screen (device pixels); Qt neither lists nor switches display modes
struct VideoMode { UINT Width, Height, RefreshRate; };
static VideoMode ScreenMode(QScreen *scr)
{
	if (!scr) return { 0, 0, 0 };
	qreal dpr = scr->devicePixelRatio();
	return { UINT(scr->geometry().width() * dpr), UINT(scr->geometry().height() * dpr), UINT(scr->refreshRate() + 0.5) };
}

// not upstream: GetAdapterModeCount/EnumAdapterModes: the current mode of each screen (D3D9Frame covers the window's screen)
static std::vector<VideoMode> AdapterModes()
{
	std::vector<VideoMode> modes;
	for (QScreen *scr : QGuiApplication::screens()) modes.push_back(ScreenMode(scr));
	return modes;
}

// not upstream: DialogBoxParam counterpart, a modal QDialog built from the module's resource; proc runs as WM_INITDIALOG
static void RunDialog(void *hInstance, int resId, QWidget *hParent, DLGINIT proc, void *lParam)
{
	QDialog *dlg = qobject_cast<QDialog*>(oapiCreateResDialog(hInstance, resId, hParent));
	if (!dlg) {
		LogErr("Dialog resource %d not found", resId);
		return;
	}
	proc(dlg, lParam);
	if (dlg->isVisible()) dlg->hide(); // WS_VISIBLE templates: exec shows it again, modal
	dlg->exec();
	delete dlg;
}

// not upstream: DefDlgProc's WM_CLOSE (and Esc) → IDCANCEL to the dialog procedure, as an event filter
class DlgEvents : public QObject
{
public:
	DlgEvents(QWidget *hWnd, std::function<void()> cancel) : QObject(hWnd), onCancel(cancel) { hWnd->installEventFilter(this); }
	bool eventFilter(QObject *o, QEvent *e) override
	{
		if (e->type() == QEvent::Close || (e->type() == QEvent::KeyPress && static_cast<QKeyEvent*>(e)->key() == Qt::Key_Escape)) {
			e->ignore();
			onCancel();
			return true;
		}
		return false;
	}
private:
	std::function<void()> onCancel;
};


// ==============================================================
// Constructor

VideoTab::VideoTab(D3D9Client *gc, void *_hInst, void *_hOrbiterInst, QWidget *hVideoTab)
{
	gclient      = gc;
	hInst        = _hInst;
	hOrbiterInst = _hOrbiterInst;
	hTab         = hVideoTab;
	aspect_idx	 = 0;
	SelectedAdapterIdx = 0;
}

VideoTab::~VideoTab()
{
	
}

// ==============================================================
// Dialog message handler

void VideoTab::WndProc(QWidget *hWnd)
{
	// WM_INITDIALOG: nothing to do

	// WM_COMMAND
	auto command = [this, hWnd](int id, int code) -> BOOL {
		GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

		switch (id) {

		case IDC_VID_DEVICE:
			if (code==RESN_SELCHANGE) {
				DWORD idx = DWORD(DlgItem<QComboBox>(hWnd, IDC_VID_DEVICE)->currentIndex());
				SelectAdapter(idx);
				return TRUE;
			}
			break;

		case IDC_VID_MODE:
			if (code == RESN_SELCHANGE) {
				DWORD idx = DWORD(DlgItem<QComboBox>(hWnd, IDC_VID_MODE)->currentIndex());
				SelectMode(idx);
				return TRUE;
			}
			break;

		case IDC_VID_BPP:
			if (code == RESN_SELCHANGE) {
				SelectFullscreen(data->fullscreen);
				return TRUE;
			}


		case IDC_VID_FULL:
			if (code == RESN_CLICKED) {
				SelectFullscreen(true);
				data->fullscreen = true;
				return TRUE;
			}
			break;

		case IDC_VID_WINDOW:
			if (code == RESN_CLICKED) {
				SelectFullscreen(false);
				data->fullscreen = false;
				return TRUE;
			}
			break;

		case IDC_VID_WIDTH:
			if (code == RESN_CHANGE) {
				SelectWidth ();
				return TRUE;
			}
			break;

		case IDC_VID_HEIGHT:
			if (code == RESN_CHANGE) {
				SelectHeight ();
				return TRUE;
			}
			break;

		case IDC_VID_STENCIL:
			return TRUE;
			break;
			
		case IDC_VID_ASPECT:
			if (code == RESN_CLICKED) {
				SelectWidth();
				return TRUE;
			}
			break;

		case IDC_VID_4X3:
		case IDC_VID_16X10:
		case IDC_VID_16X9:
			if (code == RESN_CLICKED) {
				aspect_idx = id - IDC_VID_4X3;
				SelectWidth();
				return TRUE;
			}
			break;

		case IDC_VID_INFO:
			RunDialog(hInst, IDD_D3D9SETUP, hTab, SetupDlgProcWrp, this); // DialogBoxParam
			return TRUE;
		}
		return FALSE;
	};
	// not upstream: only the video controls are connected, the core drops their connections when the client is unloaded
	for (int id : {IDC_VID_DEVICE, IDC_VID_MODE, IDC_VID_BPP})
		QObject::connect(DlgItem<QComboBox>(hWnd, id), &QComboBox::activated, hWnd, [command, id]() { command(id, RESN_SELCHANGE); });
	for (int id : {IDC_VID_FULL, IDC_VID_WINDOW, IDC_VID_STENCIL, IDC_VID_ASPECT, IDC_VID_4X3, IDC_VID_16X10, IDC_VID_16X9, IDC_VID_INFO})
		QObject::connect(DlgItem<QAbstractButton>(hWnd, id), &QAbstractButton::clicked, hWnd, [command, id]() { command(id, RESN_CLICKED); });
	for (int id : {IDC_VID_WIDTH, IDC_VID_HEIGHT})
		QObject::connect(DlgItem<QLineEdit>(hWnd, id), &QLineEdit::textChanged, hWnd, [command, id]() { command(id, RESN_CHANGE); });
}

// ==============================================================
// Initialise the Launchpad "video" tab

bool VideoTab::Initialise()
{
	VideoMode mode, curMode;
	VkPhysicalDeviceProperties info;

	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	data->forceenum = false;
	data->trystencil = false;

	DlgItem<QComboBox>(hTab, IDC_VID_DEVICE)->clear();
	DlgItem<QComboBox>(hTab, IDC_VID_MODE)->clear();
	DlgItem<QComboBox>(hTab, IDC_VID_BPP)->clear();

	ScanAtmoCfgs();

	char cbuf[32];
	std::vector<VkPhysicalDevice> adapter = Adapters();
	int nAdapter = int(adapter.size());

	if (nAdapter == 0) {
		LogErr("VideoTab::Initialize() No Vulkan Adapters Found");
		FailedDeviceError();
		return false;
	}

	if (data->deviceidx < 0 || (data->deviceidx)>=nAdapter) data->deviceidx = 0;

	for (int i=0;i<nAdapter;i++) {
		vkGetPhysicalDeviceProperties(adapter[i], &info); // GetAdapterIdentifier
		LogAlw("Adapter %d: %s", i, info.deviceName);
		oapiComboAddString(DlgItem<QComboBox>(hTab, IDC_VID_DEVICE), info.deviceName);
	}

	DlgItem<QComboBox>(hTab, IDC_VID_DEVICE)->setCurrentIndex(data->deviceidx);


	curMode = ScreenMode(QGuiApplication::primaryScreen()); // GetAdapterDisplayMode

	LogAlw("Current Mode W=%u, H=%u", curMode.Width, curMode.Height);

	std::vector<VideoMode> modes = AdapterModes();
	UINT nModes = UINT(modes.size());

	if (nModes == 0) {
		LogErr("VideoTab::Initialize() No Display Modes Available");
		FailedDeviceError();
	}

	for (UINT k=0;k<nModes;k++) {
		mode = modes[k]; // EnumAdapterModes (X8R8G8B8: the swapchain is B8G8R8A8)
		snprintf(cbuf,32,"%u x %u  %uHz", mode.Width, mode.Height, mode.RefreshRate);
		LogAlw("Index:%u %u x %u  %uHz (%u)", k, mode.Width, mode.Height, mode.RefreshRate, UINT(VK_FORMAT_B8G8R8A8_UNORM));
		oapiComboAddString(DlgItem<QComboBox>(hTab, IDC_VID_MODE), cbuf);
		DlgItem<QComboBox>(hTab, IDC_VID_MODE)->setItemData(k, (mode.Height<<16 | mode.Width));
	}

	oapiComboAddString(DlgItem<QComboBox>(hTab, IDC_VID_BPP), "True Full Screen (no alt-tab)");
	oapiComboAddString(DlgItem<QComboBox>(hTab, IDC_VID_BPP), "Full Screen Window");
	oapiComboAddString(DlgItem<QComboBox>(hTab, IDC_VID_BPP), "Window with Taskbar");
	DlgItem<QComboBox>(hTab, IDC_VID_BPP)->setCurrentIndex(data->style);

	//oapiSetDlgItemText(hTab, IDC_VID_STATIC5, "Resolution");
	oapiSetDlgItemText(hTab, IDC_VID_STATIC6, "Full Screen Mode");


	DlgItem<QComboBox>(hTab, IDC_VID_MODE)->setCurrentIndex(data->modeidx);
	DlgItem<QAbstractButton>(hTab, IDC_VID_VSYNC)->setChecked(data->novsync);
		
	oapiSetDlgItemText(hTab, IDC_VID_WIDTH, std::to_string(data->winw).c_str());
	oapiSetDlgItemText(hTab, IDC_VID_HEIGHT, std::to_string(data->winh).c_str());

	aspect_idx = 0;
		
	if (data->winw == (4*data->winh)/3 || data->winh == (3*data->winw)/4)	aspect_idx = 1;
	else if (data->winw == (16*data->winh)/10 || data->winh == (10*data->winw)/16) aspect_idx = 2;
	else if (data->winw == (16*data->winh)/9 || data->winh == (9*data->winw)/16) aspect_idx = 3;
		
	DlgItem<QAbstractButton>(hTab, IDC_VID_ASPECT)->setChecked(aspect_idx);
	if (aspect_idx) aspect_idx--;
	DlgItem<QAbstractButton>(hTab, IDC_VID_4X3+aspect_idx)->setChecked(true);

	DlgItem<QAbstractButton>(hTab, IDC_VID_STENCIL)->setChecked(data->trystencil); // GDI Compatibility mode
	DlgItem<QAbstractButton>(hTab, IDC_VID_ENUM)->setChecked(data->forceenum);  
	DlgItem<QAbstractButton>(hTab, IDC_VID_PAGEFLIP)->setChecked(data->pageflip);	  // Full scrren Window	

	bool bRet = SelectAdapter(data->deviceidx);

	SelectFullscreen(data->fullscreen);

	oapiResDlgItem(hTab, IDC_VID_INFO)->show();

	oapiSetDlgItemText(hTab, IDC_VID_INFO, "Advanced");

	return bRet;
}


// ==============================================================
// 
void VideoTab::SelectMode(DWORD index)
{
	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();
	DlgItem<QComboBox>(hTab, IDC_VID_MODE)->itemData(index);
	data->modeidx = index;
}


// ==============================================================
// Respond to user adapter selection
//
bool VideoTab::SelectAdapter(DWORD index)
{

	SelectedAdapterIdx = index; 

	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	if (g_pD3DObject == NULL) {
		LogErr("VideoTab::SelectAdapter(%u) Vulkan instance creation failed", index);
		return false;
	}
	else {

		char cbuf[32];
		VideoMode mode, curMode;
	
		if (Adapters().size()<=index) {
			LogErr("Adapter Index out of range");
			return false;
		}

		curMode = ScreenMode(QGuiApplication::primaryScreen()); // GetAdapterDisplayMode(D3DADAPTER_DEFAULT)

		DlgItem<QComboBox>(hTab, IDC_VID_MODE)->clear();

		std::vector<VideoMode> modes = AdapterModes();
		DWORD nModes = DWORD(modes.size());

		if (nModes == 0) {
			LogErr("VideoTab::SelectAdapter() No Display Modes Available");	
		}

		for (DWORD k=0;k<nModes;k++) {
			mode = modes[k]; // EnumAdapterModes
			snprintf(cbuf,32,"%u x %u %uHz", mode.Width, mode.Height, mode.RefreshRate);
			oapiComboAddString(DlgItem<QComboBox>(hTab, IDC_VID_MODE), cbuf);
			DlgItem<QComboBox>(hTab, IDC_VID_MODE)->setItemData(k, (mode.Height<<16 | mode.Width));
		}

		DlgItem<QComboBox>(hTab, IDC_VID_MODE)->setCurrentIndex(data->modeidx);
	}

	return true;
}



void VideoTab::SelectFullscreen(bool bFull)
{

	oapiSetDlgItemText(hTab, IDC_VID_ENUM, "(unused)");
	oapiSetDlgItemText(hTab, IDC_VID_STENCIL, "Force window size");
	oapiSetDlgItemText(hTab, IDC_VID_PAGEFLIP, "Multiple displays");

	DlgItem<QAbstractButton>(hTab, IDC_VID_FULL)->setChecked(bFull);
	DlgItem<QAbstractButton>(hTab, IDC_VID_WINDOW)->setChecked(!bFull);

	oapiResDlgItem(hTab, IDC_VID_ENUM)->setEnabled(false);
	oapiResDlgItem(hTab, IDC_VID_STENCIL)->setEnabled(true);

	if (bFull) {
		oapiResDlgItem(hTab, IDC_VID_ASPECT)->setEnabled(false);
		oapiResDlgItem(hTab, IDC_VID_WIDTH)->setEnabled(false);
		oapiResDlgItem(hTab, IDC_VID_HEIGHT)->setEnabled(false);
		oapiResDlgItem(hTab, IDC_VID_4X3)->setEnabled(false);
		oapiResDlgItem(hTab, IDC_VID_16X10)->setEnabled(false);
		oapiResDlgItem(hTab, IDC_VID_16X9)->setEnabled(false);
		oapiResDlgItem(hTab, IDC_VID_MODE)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_VSYNC)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_PAGEFLIP)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_BPP)->setEnabled(true);
	}
	else {
		oapiResDlgItem(hTab, IDC_VID_ASPECT)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_WIDTH)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_HEIGHT)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_4X3)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_16X10)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_16X9)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_MODE)->setEnabled(false);
		oapiResDlgItem(hTab, IDC_VID_VSYNC)->setEnabled(true);
		oapiResDlgItem(hTab, IDC_VID_PAGEFLIP)->setEnabled(false);
		oapiResDlgItem(hTab, IDC_VID_BPP)->setEnabled(false);
	}
}


static int aspect_wfac[3] = {4,16,16};
static int aspect_hfac[3] = {3,10,9};


void VideoTab::SelectWidth ()
{
	if (DlgItem<QAbstractButton>(hTab, IDC_VID_ASPECT)->isChecked()) {
		char cbuf[32];
		int w, h, wfac = aspect_wfac[aspect_idx], hfac = aspect_hfac[aspect_idx];
		oapiGetDlgItemText(hTab, IDC_VID_WIDTH, cbuf, 32); w = atoi(cbuf);
		oapiGetDlgItemText(hTab, IDC_VID_HEIGHT, cbuf, 32); h = atoi(cbuf);
		if (w != (wfac*h)/hfac) {
			h = (hfac*w)/wfac;
			oapiSetDlgItemText(hTab, IDC_VID_HEIGHT, std::to_string(h).c_str());
		}
	}
}

// ==============================================================
// Respond to user selection of render window height

void VideoTab::SelectHeight ()
{
	if (DlgItem<QAbstractButton>(hTab, IDC_VID_ASPECT)->isChecked()) {
		char cbuf[32];
		int w, h, wfac = aspect_wfac[aspect_idx], hfac = aspect_hfac[aspect_idx];
		oapiGetDlgItemText(hTab, IDC_VID_WIDTH, cbuf, 32); w = atoi(cbuf);
		oapiGetDlgItemText(hTab, IDC_VID_HEIGHT, cbuf, 32); h = atoi(cbuf);
		if (h != (hfac*w)/wfac) {
			w = (wfac*h)/hfac;
			oapiSetDlgItemText(hTab, IDC_VID_WIDTH, std::to_string(w).c_str());
		}
	}
}

// ==============================================================
// copy dialog state back to parameter structure

void VideoTab::UpdateConfigData()
{
	char cbuf[32];
	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	// device parameters
	data->deviceidx  = (int)DlgItem<QComboBox>(hTab, IDC_VID_DEVICE)->currentIndex();
	data->modeidx	 = (int)DlgItem<QComboBox>(hTab, IDC_VID_MODE)->currentIndex();
	data->style		 = DlgItem<QComboBox>(hTab, IDC_VID_BPP)->currentIndex();
	data->fullscreen = (DlgItem<QAbstractButton>(hTab, IDC_VID_FULL)->isChecked());
	data->novsync    = (DlgItem<QAbstractButton>(hTab, IDC_VID_VSYNC)->isChecked());
	data->pageflip   = (DlgItem<QAbstractButton>(hTab, IDC_VID_PAGEFLIP)->isChecked());
	data->trystencil = (DlgItem<QAbstractButton>(hTab, IDC_VID_STENCIL)->isChecked());
	data->forceenum  = (DlgItem<QAbstractButton>(hTab, IDC_VID_ENUM)->isChecked());

	oapiGetDlgItemText(hTab, IDC_VID_WIDTH, cbuf, 32); data->winw = atoi(cbuf);
	oapiGetDlgItemText(hTab, IDC_VID_HEIGHT, cbuf, 32); data->winh = atoi(cbuf);	


	QWidget *hChild = NULL;
	QWidget *hRoot = hTab->window(); // GetAncestor(GA_ROOT)

	for (QWidget *w : hRoot->findChildren<QWidget*>()) if (!EnumChildProc(w, &hChild)) break; // EnumChildWindows

	if (hChild) {

		QTreeWidget *hTree = DlgItem<QTreeWidget>(hChild, IDC_SCENARIO_TREE);

		if (hTree==NULL) {
			LogErr("FAILED to get a scenario tree control handle");
			return;
		}

		QTreeWidgetItem *item = hTree->currentItem(); // TreeView_GetSelection

		if (item == NULL) {
			LogErr("FAILED. Scenario not selected");
			return;
		}

		using std::vector;
		vector<QTreeWidgetItem*> hNodes;

		while (item) { // [ego, parent, grandparent, ...]
			hNodes.push_back( item );
			item = item->parent(); // TreeView_GetParent
		}

		using std::string;
		string path = OapiExtension::GetScenarioDir();
		path.erase( path.find_last_not_of( "\\/" )+1 ); // trim trailing path-delimiter

		// TVITEMA buffer left out: the item text is a QString

		for (auto it = hNodes.crbegin(); it != hNodes.crend(); ++it) {
			path += "/"; path += (*it)->text(0).toStdString(); // TreeView_GetItem; '/' separator
		}
		path += ".scn";

		gclient->SetScenarioName(path);

		LogAlw("Scenario = %s", path.c_str());
	}
	else {
		LogErr("FAILED to get a handle of a scenario dialog");
	}
}





// ***************************************************************************************************
// Advanced setup Dialog
// ***************************************************************************************************


void VideoTab::SetupDlgProcWrp(QWidget *hWnd, void *context)
{
	static class VideoTab *VTab = NULL;
	// WM_INITDIALOG
	VTab = (class VideoTab *)context;
	VTab->InitSetupDialog(hWnd);

	// WM_COMMAND, WM_HSCROLL
	if (VTab) VTab->SetupDlgProc(hWnd);
}



void VideoTab::SetupDlgProc(QWidget *hWnd)
{

	// WM_HSCROLL
	for (int id : {IDC_CONVERGENCE, IDC_SEPARATION}) {
		QSlider *tb = DlgItem<QSlider>(hWnd, id);
		QObject::connect(tb, &QSlider::sliderMoved, hWnd, [hWnd, tb](int value) { // TB_THUMBTRACK
			char lbl[32];
			WORD pos = WORD(value);
			if (tb==DlgItem<QSlider>(hWnd, IDC_CONVERGENCE)) {
				snprintf(lbl,32,"%1.2fm",float(pos)*0.01);
				oapiSetDlgItemText(hWnd, IDC_CONV_DSP, lbl);
			}
			if (tb==DlgItem<QSlider>(hWnd, IDC_SEPARATION)) {
				snprintf(lbl,32,"%1.0f%%",float(pos));
				oapiSetDlgItemText(hWnd, IDC_SEPA_DSP, lbl);
			}
		});
	}

	// WM_COMMAND
	auto command = [this, hWnd](int id, int code, QWidget *hCtrl) {
	switch (id) {

		case IDC_MESH_DEBUGGER:
			QMessageBox(QMessageBox::NoIcon, "Notification", "You must restart launchpad for changes to take effect", QMessageBox::Ok, hWnd).exec();
			break;

		case IDC_CREDITS:
			// LoadLibrary("riched20.dll") left out: the rich edit control is a QTextBrowser
			RunDialog(hInst, IDD_D3D9CREDITS, hWnd, CreditsDlgProcWrp, this); // DialogBoxParam
			break;

		case IDC_SRFPRELOAD:
			DlgItem<QAbstractButton>(hWnd, IDC_DEMAND)->setChecked(false);
			break;

		case IDC_DEMAND:
			DlgItem<QAbstractButton>(hWnd, IDC_SRFPRELOAD)->setChecked(false);
			break;

		case IDOK:
		case IDCANCEL:
			SaveSetupState(hWnd);
			qobject_cast<QDialog*>(hWnd)->done(0); // EndDialog
			break;
	}
	};
	oapiConnectDlgCommands(hWnd, command);
	new DlgEvents(hWnd, [command]() { command(IDCANCEL, RESN_CLICKED, NULL); });
}





void VideoTab::InitSetupDialog(QWidget *hWnd)
{

	char cbuf[32];
	DWORD aamax = 0;
	VkDevCaps caps;

	if (g_pD3DObject == NULL) {
		LogErr("VideoTab::SelectAdapter(%u) Vulkan instance creation failed", SelectedAdapterIdx);
		return;
	}

	std::vector<VkPhysicalDevice> adapter = Adapters();
	if (SelectedAdapterIdx >= adapter.size()) { // not upstream: GetDeviceCaps failed on a bad index and left caps undefined
		LogErr("Adapter Index out of range");
		return;
	}

	VkPhysicalDeviceProperties prop;
	vkGetPhysicalDeviceProperties(adapter[SelectedAdapterIdx], &prop); // GetDeviceCaps
	caps.MaxAnisotropy = DWORD(prop.limits.maxSamplerAnisotropy);

	// CheckDeviceMultiSampleType: the sample counts colour and depth targets both support (as D3D9Frame)
	VkSampleCountFlags sc = prop.limits.framebufferColorSampleCounts & prop.limits.framebufferDepthSampleCounts;
	if (sc & VK_SAMPLE_COUNT_2_BIT) aamax=2;
	if (sc & VK_SAMPLE_COUNT_4_BIT) aamax=4;
	if (sc & VK_SAMPLE_COUNT_8_BIT) aamax=8;
	
	LogAlw("InitSetupDialog() Enum Device AA capability = %u",aamax);


	// AA -----------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_AA)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AA), "None");
	if (aamax>=2)  oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AA), "2x");
	if (aamax>=4)  oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AA), "4x");
	if (aamax>=8)  oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AA), "8x");
	

	// AF -----------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_AF)->clear();
	if (caps.MaxAnisotropy>=2) oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AF), "2x");
	if (caps.MaxAnisotropy>=4) oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AF), "4x");
	if (caps.MaxAnisotropy>=8) oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AF), "8x");
	if (caps.MaxAnisotropy>=12) oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AF), "12x");
	if (caps.MaxAnisotropy>=16) oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_AF), "16x");
	

	// DEBUG --------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_DEBUG)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_DEBUG), "0");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_DEBUG), "1");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_DEBUG), "2");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_DEBUG), "3");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_DEBUG), "4");
	
	// SKETCHPAD --------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_FONT)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_FONT), "Crisp");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_FONT), "Antialiased");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_FONT), "Cleartype");
	
	// ENVMAP MODE --------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_ENVMODE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ENVMODE), "Disable (Debug)");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ENVMODE), "Planet Only");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ENVMODE), "Full Scene");
	DlgItem<QComboBox>(hWnd, IDC_ENVMODE)->setCurrentIndex(0);

	// CUSTOM CAMERA MODE --------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_CAMMODE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_CAMMODE), "Disable");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_CAMMODE), "Enabled");
	DlgItem<QComboBox>(hWnd, IDC_ENVMODE)->setCurrentIndex(0);

	// ENVMAP FACES --------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_ENVFACES)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ENVFACES), "Light");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ENVFACES), "Medimum");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ENVFACES), "Heavy");
	DlgItem<QComboBox>(hWnd, IDC_ENVFACES)->setCurrentIndex(0);

	// TEXTURE MIPMAP POLICY --------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_TEXMIPS)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TEXMIPS), "Load as defined");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TEXMIPS), "Autogen missing");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TEXMIPS), "Autogen all");
	DlgItem<QComboBox>(hWnd, IDC_TEXMIPS)->setCurrentIndex(0);

	// MICROTEX FILTER --------------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_MICROFILTER)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MICROFILTER), "Point (Fast/Good)");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MICROFILTER), "Linear (Fast/Bad)");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MICROFILTER), "Anisotropic 2x");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MICROFILTER), "Anisotropic 4x (Better)");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MICROFILTER), "Anisotropic 8x");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MICROFILTER), "Anisotropic 16x (Slow/Best)");
	DlgItem<QComboBox>(hWnd, IDC_MICROFILTER)->setCurrentIndex(0);
	
	// MICROTEX FILTER --------------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_MICROMODE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MICROMODE), "Disabled");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MICROMODE), "Enabled");
	DlgItem<QComboBox>(hWnd, IDC_MICROMODE)->setCurrentIndex(0);


	// MICROTEX BLEND MODE -----------------------------------------
	
	DlgItem<QComboBox>(hWnd, IDC_BLENDMODE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_BLENDMODE), "Soft light");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_BLENDMODE), "Normal light");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_BLENDMODE), "Hard light");
	DlgItem<QComboBox>(hWnd, IDC_BLENDMODE)->setCurrentIndex(0);

	// TILE MIPMAP POLICY -----------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_MIPMAPS)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MIPMAPS), "Disabled");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MIPMAPS), "Enabled (slow2load)");
	DlgItem<QComboBox>(hWnd, IDC_MIPMAPS)->setCurrentIndex(0);

	// ARCHIVE METHOD ------------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_ARCHIVE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ARCHIVE), "Cache only");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ARCHIVE), "Archive only");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_ARCHIVE), "Cache & Archive");
	DlgItem<QComboBox>(hWnd, IDC_ARCHIVE)->setCurrentIndex(0);

	// POSTPROCESSING METHOD ------------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_POSTPROCESS)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_POSTPROCESS), "None");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_POSTPROCESS), "Light glow");
	DlgItem<QComboBox>(hWnd, IDC_POSTPROCESS)->setCurrentIndex(0);

	// Local Lights -----------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "None");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "4x Partial");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "4x Full");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "8x Partial");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "8x Full");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "12x Partial");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "12x Full");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "16x Partial");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "16x Full");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "20x Partial");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG), "20x Full");

	// Shadows -----------------------------------------

	DlgItem<QComboBox>(hWnd, IDC_SELFSHADOWS)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SELFSHADOWS), "None");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SELFSHADOWS), "Focus + payload");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SELFSHADOWS), "Near by objects");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SELFSHADOWS), "All visible objects");

	DlgItem<QComboBox>(hWnd, IDC_SHADOWFILTER)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SHADOWFILTER), "9 samples");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SHADOWFILTER), "27 samples");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SHADOWFILTER), "27s dither");
	//oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SHADOWFILTER), "40 samples");
	//oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_SHADOWFILTER), "40s dither");

	DlgItem<QComboBox>(hWnd, IDC_TERRAIN)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TERRAIN), "None");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TERRAIN), "Stencil");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TERRAIN), "Projected");

	DlgItem<QComboBox>(hWnd, IDC_MESHRES)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MESHRES), "16");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_MESHRES), "32");

	DlgItem<QComboBox>(hWnd, IDC_TILECOUNT)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TILECOUNT), "600");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TILECOUNT), "1200");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_TILECOUNT), "2400");

	// gcGUI -----------------------------------------
	if (Config->gcGUIMode == 1) Config->gcGUIMode = 0;
	DlgItem<QComboBox>(hWnd, IDC_GUIMODE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_GUIMODE), "Disabled");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_GUIMODE), "(unused)");
	oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_GUIMODE), "Windowed");

	// Earth AtmoConfig -------------------------------
	DlgItem<QComboBox>(hWnd, IDC_EARTHVISCFG)->clear();
	for (auto x : AtmoCfgs["Earth"]) oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_EARTHVISCFG), x.cfg.c_str());
	for (int i = 0; i < AtmoCfgs["Earth"].size(); i++) {
		if (Config->AtmoCfg["Earth"] == AtmoCfgs["Earth"][i].file) {
			DlgItem<QComboBox>(hWnd, IDC_EARTHVISCFG)->setCurrentIndex(i);
			break;
		}
	}
		


	// Write values in controls ----------------

	bool bFS = (DlgItem<QComboBox>(hTab, IDC_VID_BPP)->currentIndex()==0 && DlgItem<QAbstractButton>(hTab, IDC_VID_FULL)->isChecked());
	bool bGB = (DlgItem<QAbstractButton>(hTab, IDC_VID_STENCIL)->isChecked());

	if (bFS || bGB) {
		Config->SceneAntialias = 0;
		oapiResDlgItem(hWnd, IDC_AA)->setEnabled(false);
	}
	else {
		oapiResDlgItem(hWnd, IDC_AA)->setEnabled(true);
	}


	DlgItem<QSlider>(hWnd, IDC_CONVERGENCE)->setMaximum(100);
	DlgItem<QSlider>(hWnd, IDC_CONVERGENCE)->setMinimum(5);
	DlgItem<QSlider>(hWnd, IDC_CONVERGENCE)->setTickInterval(5);
	
	DlgItem<QSlider>(hWnd, IDC_SEPARATION)->setMaximum(100);
	DlgItem<QSlider>(hWnd, IDC_SEPARATION)->setMinimum(10);
	DlgItem<QSlider>(hWnd, IDC_SEPARATION)->setTickInterval(5);
	
	DlgItem<QSlider>(hWnd, IDC_LODBIAS)->setMaximum(10);
	DlgItem<QSlider>(hWnd, IDC_LODBIAS)->setMinimum(-10);
	DlgItem<QSlider>(hWnd, IDC_LODBIAS)->setTickInterval(1);

	DlgItem<QSlider>(hWnd, IDC_MICROBIAS)->setMaximum(10);
	DlgItem<QSlider>(hWnd, IDC_MICROBIAS)->setMinimum(0);
	DlgItem<QSlider>(hWnd, IDC_MICROBIAS)->setTickInterval(1);
	

	snprintf(cbuf,32,"%1.1fm",float(Config->Convergence));
	oapiSetDlgItemText(hWnd, IDC_CONV_DSP, cbuf);
			
	snprintf(cbuf,32,"%1.0f%%",float(Config->Separation));
	oapiSetDlgItemText(hWnd, IDC_SEPA_DSP, cbuf);

	DlgItem<QSlider>(hWnd, IDC_CONVERGENCE)->setValue(int(Config->Convergence*100.0));
	DlgItem<QSlider>(hWnd, IDC_SEPARATION)->setValue(int(Config->Separation));
	DlgItem<QSlider>(hWnd, IDC_LODBIAS)->setValue(int(Config->LODBias*5.0));
	DlgItem<QSlider>(hWnd, IDC_MICROBIAS)->setValue(int(Config->MicroBias));

	DlgItem<QComboBox>(hWnd, IDC_TILECOUNT)->setCurrentIndex(Config->MaxTiles);
	DlgItem<QComboBox>(hWnd, IDC_MESHRES)->setCurrentIndex(Config->MeshRes);
	DlgItem<QComboBox>(hWnd, IDC_ARCHIVE)->setCurrentIndex(Config->PlanetTileLoadFlags-1);
	DlgItem<QComboBox>(hWnd, IDC_BLENDMODE)->setCurrentIndex(Config->BlendMode);
	DlgItem<QComboBox>(hWnd, IDC_MICROMODE)->setCurrentIndex(Config->MicroMode);
	DlgItem<QComboBox>(hWnd, IDC_MICROFILTER)->setCurrentIndex(Config->MicroFilter);
	DlgItem<QComboBox>(hWnd, IDC_TEXMIPS)->setCurrentIndex(Config->TextureMips);
	DlgItem<QComboBox>(hWnd, IDC_ENVMODE)->setCurrentIndex(Config->EnvMapMode);
	DlgItem<QComboBox>(hWnd, IDC_CAMMODE)->setCurrentIndex(Config->CustomCamMode);
	DlgItem<QComboBox>(hWnd, IDC_ENVFACES)->setCurrentIndex(Config->EnvMapFaces-1);
	DlgItem<QComboBox>(hWnd, IDC_FONT)->setCurrentIndex(Config->SketchpadFont);
	DlgItem<QComboBox>(hWnd, IDC_DEBUG)->setCurrentIndex(Config->DebugLvl);
	DlgItem<QComboBox>(hWnd, IDC_MIPMAPS)->setCurrentIndex(Config->TileMipmaps);
	DlgItem<QComboBox>(hWnd, IDC_POSTPROCESS)->setCurrentIndex(Config->PostProcess);
	DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG)->setCurrentIndex(Config->LightConfig);
	DlgItem<QComboBox>(hWnd, IDC_SELFSHADOWS)->setCurrentIndex(Config->ShadowMapMode);
	DlgItem<QComboBox>(hWnd, IDC_SHADOWFILTER)->setCurrentIndex(Config->ShadowFilter);
	DlgItem<QComboBox>(hWnd, IDC_TERRAIN)->setCurrentIndex(Config->TerrainShadowing);
	DlgItem<QComboBox>(hWnd, IDC_GUIMODE)->setCurrentIndex(Config->gcGUIMode);

	DlgItem<QAbstractButton>(hWnd, IDC_DEMAND)->setChecked(Config->PlanetPreloadMode==0);
	DlgItem<QAbstractButton>(hWnd, IDC_SRFPRELOAD)->setChecked(Config->PlanetPreloadMode==1);
	DlgItem<QAbstractButton>(hWnd, IDC_GLASSSHADE)->setChecked(Config->EnableGlass==1);
	DlgItem<QAbstractButton>(hWnd, IDC_MESH_DEBUGGER)->setChecked(Config->EnableMeshDbg==1);
	DlgItem<QAbstractButton>(hWnd, IDC_CLOUDMICRO)->setChecked(Config->CloudMicro == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_GDIOVERLAY)->setChecked(Config->GDIOverlay == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_ABSANIM)->setChecked(Config->bAbsAnims == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_CLOUDNORM)->setChecked(Config->bCloudNormals == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_FLATS)->setChecked(Config->bFlats == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_ESUNGLARE)->setChecked(Config->bGlares == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_ELIGHTSGLARE)->setChecked(Config->bLocalGlares == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_EIRRAD)->setChecked(Config->bIrradiance == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_ESCACHE)->setChecked(Config->ShaderCacheUse == 1);
	DlgItem<QAbstractButton>(hWnd, IDC_EAQUALITY)->setChecked(Config->bAtmoQuality == 1);


	DlgItem<QAbstractButton>(hWnd, IDC_NORMALMAPS)->setChecked(Config->UseNormalMap==1);
	DlgItem<QAbstractButton>(hWnd, IDC_BASEVIS)->setChecked(Config->PreLBaseVis==1);
	DlgItem<QAbstractButton>(hWnd, IDC_NEARPLANE)->setChecked(Config->NearClipPlane==1);
	DlgItem<QAbstractButton>(hWnd, IDC_BREAK)->setChecked(Config->DebugBreak == 1);
	
	snprintf(cbuf,32,"%d", Config->PlanetLoadFrequency);
	oapiSetDlgItemText(hWnd, IDC_HZ, cbuf);

	snprintf(cbuf,32,"%3.3f", Config->PlanetGlow);
	oapiSetDlgItemText(hWnd, IDC_PLANETGLOW, cbuf);

	DWORD af = min(caps.MaxAnisotropy, DWORD(Config->Anisotrophy));

	switch(af) {
		case 2: DlgItem<QComboBox>(hWnd, IDC_AF)->setCurrentIndex(0); break;
		default:
		case 4: DlgItem<QComboBox>(hWnd, IDC_AF)->setCurrentIndex(1); break;
		case 8: DlgItem<QComboBox>(hWnd, IDC_AF)->setCurrentIndex(2); break;
		case 12: DlgItem<QComboBox>(hWnd, IDC_AF)->setCurrentIndex(3); break;
		case 16: DlgItem<QComboBox>(hWnd, IDC_AF)->setCurrentIndex(4); break;
	}

	DWORD aa = min(aamax, DWORD(Config->SceneAntialias));

	switch(aa) {
		case 0: DlgItem<QComboBox>(hWnd, IDC_AA)->setCurrentIndex(0); break;
		case 2: DlgItem<QComboBox>(hWnd, IDC_AA)->setCurrentIndex(1); break;
		default:
		case 4: DlgItem<QComboBox>(hWnd, IDC_AA)->setCurrentIndex(2); break;
		case 8: DlgItem<QComboBox>(hWnd, IDC_AA)->setCurrentIndex(3); break;
	}
}




void VideoTab::SaveSetupState(QWidget *hWnd)
{
	char cbuf[32];
	// Combo boxes
	Config->SketchpadFont = (int)DlgItem<QComboBox>(hWnd, IDC_FONT)->currentIndex();
	Config->EnvMapMode	  = (int)DlgItem<QComboBox>(hWnd, IDC_ENVMODE)->currentIndex();
	Config->CustomCamMode = (int)DlgItem<QComboBox>(hWnd, IDC_CAMMODE)->currentIndex();
	Config->EnvMapFaces	  = (int)DlgItem<QComboBox>(hWnd, IDC_ENVFACES)->currentIndex() + 1;
	Config->TextureMips	  = (int)DlgItem<QComboBox>(hWnd, IDC_TEXMIPS)->currentIndex();
	Config->MicroMode	  = (int)DlgItem<QComboBox>(hWnd, IDC_MICROMODE)->currentIndex();
	Config->MicroFilter	  = (int)DlgItem<QComboBox>(hWnd, IDC_MICROFILTER)->currentIndex();
	Config->BlendMode	  = (int)DlgItem<QComboBox>(hWnd, IDC_BLENDMODE)->currentIndex();
	Config->TileMipmaps   = (int)DlgItem<QComboBox>(hWnd, IDC_MIPMAPS)->currentIndex();
	Config->PostProcess   = (int)DlgItem<QComboBox>(hWnd, IDC_POSTPROCESS)->currentIndex();
	Config->PlanetTileLoadFlags = (int)DlgItem<QComboBox>(hWnd, IDC_ARCHIVE)->currentIndex() + 1;
	Config->LightConfig   = (int)DlgItem<QComboBox>(hWnd, IDC_LIGHTCONFIG)->currentIndex();
	Config->ShadowMapMode = (int)DlgItem<QComboBox>(hWnd, IDC_SELFSHADOWS)->currentIndex();
	Config->ShadowFilter  = (int)DlgItem<QComboBox>(hWnd, IDC_SHADOWFILTER)->currentIndex();
	Config->TerrainShadowing = (int)DlgItem<QComboBox>(hWnd, IDC_TERRAIN)->currentIndex();
	Config->gcGUIMode	  = (int)DlgItem<QComboBox>(hWnd, IDC_GUIMODE)->currentIndex();
	Config->MeshRes		  = int(DlgItem<QComboBox>(hWnd, IDC_MESHRES)->currentIndex());
	Config->MaxTiles	  = int(DlgItem<QComboBox>(hWnd, IDC_TILECOUNT)->currentIndex());

	if (Config->gcGUIMode == 1) Config->gcGUIMode = 0;

	// Check boxes
	Config->UseNormalMap  = (int)DlgItem<QAbstractButton>(hWnd, IDC_NORMALMAPS)->isChecked();
	Config->PreLBaseVis   = (int)DlgItem<QAbstractButton>(hWnd, IDC_BASEVIS)->isChecked();
	Config->NearClipPlane = (int)DlgItem<QAbstractButton>(hWnd, IDC_NEARPLANE)->isChecked();
	Config->EnableGlass   = (int)DlgItem<QAbstractButton>(hWnd, IDC_GLASSSHADE)->isChecked();
	Config->EnableMeshDbg = (int)DlgItem<QAbstractButton>(hWnd, IDC_MESH_DEBUGGER)->isChecked();
	Config->CloudMicro    = (int)DlgItem<QAbstractButton>(hWnd, IDC_CLOUDMICRO)->isChecked();
	Config->GDIOverlay	  = (int)DlgItem<QAbstractButton>(hWnd, IDC_GDIOVERLAY)->isChecked();
	Config->bAbsAnims	  = (int)DlgItem<QAbstractButton>(hWnd, IDC_ABSANIM)->isChecked();
	Config->bCloudNormals = (int)DlgItem<QAbstractButton>(hWnd, IDC_CLOUDNORM)->isChecked();
	Config->bFlats		  = (int)DlgItem<QAbstractButton>(hWnd, IDC_FLATS)->isChecked();
	Config->DebugBreak	  = (int)DlgItem<QAbstractButton>(hWnd, IDC_BREAK)->isChecked();
	Config->bGlares		  = (int)DlgItem<QAbstractButton>(hWnd, IDC_ESUNGLARE)->isChecked();
	Config->bLocalGlares  = (int)DlgItem<QAbstractButton>(hWnd, IDC_ELIGHTSGLARE)->isChecked();
	Config->bIrradiance   = (int)DlgItem<QAbstractButton>(hWnd, IDC_EIRRAD)->isChecked();
	Config->ShaderCacheUse= (int)DlgItem<QAbstractButton>(hWnd, IDC_ESCACHE)->isChecked();
	Config->bAtmoQuality  = (int)DlgItem<QAbstractButton>(hWnd, IDC_EAQUALITY)->isChecked();

	// Sliders
	Config->Convergence   = double(DlgItem<QSlider>(hWnd, IDC_CONVERGENCE)->value()) * 0.01;
	Config->Separation	  = double(DlgItem<QSlider>(hWnd, IDC_SEPARATION)->value());
	Config->LODBias       = 0.2 * double(DlgItem<QSlider>(hWnd, IDC_LODBIAS)->value());
	Config->MicroBias     = int(DlgItem<QSlider>(hWnd, IDC_MICROBIAS)->value());

	// Other things
	oapiGetDlgItemText(hWnd, IDC_HZ, cbuf, 32);

	Config->PlanetLoadFrequency = atoi(cbuf);
	Config->PlanetPreloadMode = (int)DlgItem<QAbstractButton>(hWnd, IDC_SRFPRELOAD)->isChecked();

	oapiGetDlgItemText(hWnd, IDC_PLANETGLOW, cbuf, 32);
	Config->PlanetGlow = atof(cbuf);

	Config->DebugLvl = (int)DlgItem<QComboBox>(hWnd, IDC_DEBUG)->currentIndex();

	switch(DlgItem<QComboBox>(hWnd, IDC_AF)->currentIndex()) {
		default:
		case 0: Config->Anisotrophy = 2; break;
		case 1: Config->Anisotrophy = 4; break;
		case 2: Config->Anisotrophy = 8; break;
		case 3: Config->Anisotrophy = 12; break;
		case 4: Config->Anisotrophy = 16; break;
	}

	switch(DlgItem<QComboBox>(hWnd, IDC_AA)->currentIndex()) {
		default:
		case 0: Config->SceneAntialias = 0; break;
		case 1: Config->SceneAntialias = 2; break;
		case 2: Config->SceneAntialias = 4; break;
		case 3: Config->SceneAntialias = 8; break;
	}

	int EASel = (int)DlgItem<QComboBox>(hWnd, IDC_EARTHVISCFG)->currentIndex();
	if (EASel >= 0 && !AtmoCfgs["Earth"][EASel].file.empty()) Config->AtmoCfg["Earth"] = AtmoCfgs["Earth"][EASel].file; // EASel -1 (no selection) indexed out of range upstream
	else Config->AtmoCfg["Earth"] = "Earth.atm.cfg";
}



// ***************************************************************************************************
// Credist Dialog
// ***************************************************************************************************

void VideoTab::CreditsDlgProcWrp(QWidget *hWnd, void *context)
{
	static class VideoTab *VTab = NULL;
	// WM_INITDIALOG
	VTab = (class VideoTab *)context;
	VTab->InitCreditsDialog(hWnd);
	// WM_COMMAND
	if (VTab) VTab->CreditsDlgProc(hWnd);
}



void VideoTab::CreditsDlgProc(QWidget *hWnd)
{
	oapiConnectDlgCommands(hWnd, [hWnd](int id, int code, QWidget *hCtrl) {
	switch (id) {
		case IDOK:
		case IDCANCEL:
			qobject_cast<QDialog*>(hWnd)->done(0); // EndDialog
			break;
	}
	});
}

// not upstream: RTF reader for EM_SETTEXTEX (Qt has none): text, bold, underline, sizes, fonts, colours, field results
static void SetRtfText(QTextEdit *te, const char *rtf)
{
	struct Font { QString name; bool fixed = false; };
	struct State { bool b = false, ul = false, skip = false; int fs = 24, uc = 1, f = 0, cf = 0, dest = 0; }; // dest: 1 font table, 2 colour table
	std::vector<State> st(1);
	std::map<int, Font> fonts;
	std::vector<QColor> colors;
	int deff = 0, tblFont = 0, red = 0, green = 0, blue = 0;
	te->clear();
	QTextCursor cur(te->document());
	QString run;
	int pending = 0; // characters \uN replaces
	auto flush = [&]() {
		if (run.isEmpty()) return;
		const State &s = st.back();
		QTextCharFormat f;
		f.setFontWeight(s.b ? QFont::Bold : QFont::Normal);
		f.setFontUnderline(s.ul);
		f.setFontPointSize(s.fs * 0.5);
		auto fi = fonts.find(s.f);
		if (fi != fonts.end()) {
			f.setFontFamilies(QStringList(fi->second.name));
			f.setFontFixedPitch(fi->second.fixed);
			f.setFontStyleHint(fi->second.fixed ? QFont::Monospace : QFont::SansSerif); // the fallback when the face isn't installed
		}
		if (s.cf > 0 && s.cf < (int)colors.size() && colors[s.cf].isValid()) f.setForeground(colors[s.cf]);
		cur.insertText(run, f);
		run.clear();
	};
	auto put = [&](QChar ch) {
		State &s = st.back();
		if (s.skip) return;
		if (pending > 0) { pending--; return; }
		if (s.dest == 1) { // font table: "\fN ... name;"
			if (ch == ';') { fonts[tblFont].name = fonts[tblFont].name.trimmed(); return; }
			fonts[tblFont].name += ch;
			return;
		}
		if (s.dest == 2) { // colour table: "\redR\greenG\blueB;", an empty first entry is the default colour
			if (ch == ';') { colors.push_back(colors.empty() && !red && !green && !blue ? QColor() : QColor(red, green, blue)); red = green = blue = 0; }
			return;
		}
		run += ch;
	};
	for (const char *p = rtf; *p; ) {
		char c = *p++;
		if (c == '{') { flush(); st.push_back(st.back()); continue; }
		if (c == '}') { flush(); if (st.size() > 1) st.pop_back(); continue; }
		if (c == '\r' || c == '\n') continue;
		if (c != '\\') { put(QChar(uchar(c))); continue; }
		c = *p;
		if (c == '\'' && isxdigit((uchar)p[1]) && isxdigit((uchar)p[2])) {
			char h[3] = { p[1], p[2], 0 };
			put(QChar(uchar(strtol(h, NULL, 16))));
			p += 3;
			continue;
		}
		if (!isalpha((uchar)c)) { // control symbol
			if (c == '*') st.back().skip = true; // ignorable destination
			else if (c == '~') put(QChar(0xA0));
			else if (c == '\\' || c == '{' || c == '}') put(QChar(c));
			if (c) p++;
			continue;
		}
		std::string word;
		while (isalpha((uchar)*p)) word += *p++;
		bool has = false, neg = (*p == '-');
		int val = 0;
		if (neg) p++;
		while (isdigit((uchar)*p)) { has = true; val = val*10 + (*p++ - '0'); }
		if (neg) val = -val;
		if (*p == ' ') p++; // delimiter
		State &s = st.back();
		if (s.dest == 1) { // inside the font table
			if (word == "f" && has) { tblFont = val; fonts[val]; }
			else if ((word == "fprq" && val == 1) || word == "fmodern") fonts[tblFont].fixed = true;
			continue;
		}
		if (s.dest == 2) { // inside the colour table
			if (word == "red") red = val;
			else if (word == "green") green = val;
			else if (word == "blue") blue = val;
			continue;
		}
		if (word == "par") { flush(); if (!s.skip) cur.insertBlock(); }
		else if (word == "line") put(QChar::LineSeparator);
		else if (word == "tab") put(QChar('\t'));
		else if (word == "plain") { flush(); s.b = s.ul = false; s.fs = 24; s.f = deff; s.cf = 0; }
		else if (word == "b") { flush(); s.b = (!has || val != 0); }
		else if (word == "ul") { flush(); s.ul = (!has || val != 0); }
		else if (word == "ulnone") { flush(); s.ul = false; }
		else if (word == "fs") { flush(); if (has) s.fs = val; }
		else if (word == "f") { flush(); if (has) s.f = val; }
		else if (word == "cf") { flush(); if (has) s.cf = val; }
		else if (word == "deff") { if (has) { deff = val; s.f = val; } }
		else if (word == "uc") { if (has) s.uc = val; }
		else if (word == "u") { put(QChar(char16_t(val))); pending = s.uc; }
		else if (word == "fonttbl") s.dest = 1;
		else if (word == "colortbl") s.dest = 2;
		else if (word == "stylesheet" || word == "info" || word == "pict") s.skip = true;
	}
	flush();
}

void VideoTab::InitCreditsDialog(QWidget *hWnd)
{
	QFile hFile(QString::fromStdString(oapiResolvePath("Modules/VulkanClient/Credits.rtf"))); // CreateFile

	if (!hFile.open(QIODevice::ReadOnly)) {
		LogErr("Failed to open a file /Modules/VulkanClient/Credits.rtf");
		return;
	}

	QByteArray credits = hFile.readAll(); // GetFileSize, ReadFile

	if (hFile.error() == QFileDevice::NoError) {
		// SETTEXTEX (ST_DEFAULT, CP_ACP): the control reads the RTF
		SetRtfText(DlgItem<QTextEdit>(hWnd, IDC_CREDITSTEXT), credits.constData());
	}
	else LogErr("Failed to read a file /Modules/VulkanClient/Credits.rtf Error=%s",hFile.errorString().toUtf8().constData());

	// delete []credits, CloseHandle: QByteArray and QFile release themselves
}

bool VideoTab::GetConfigName(const char* file, string& cfg, string& planet)
{
	string filename = "GC\\" + string(file);
	FILEHANDLE hFile = oapiOpenFile(filename.c_str(), FILE_IN_ZEROONFAIL, CONFIG);
	if (hFile) {
		char ConfigName[32] = {}; char PlanetName[32] = {};
		bool bA = oapiReadItem_string(hFile, (char*)"ConfigName", ConfigName);
		bool bB = oapiReadItem_string(hFile, (char*)"Planet", PlanetName);
		oapiCloseFile(hFile, FILE_IN_ZEROONFAIL);
		cfg = string(ConfigName);
		planet = string(PlanetName);
		return bA && bB;
	}
	return false;
}

void VideoTab::ScanAtmoCfgs()
{
	_AtmoCfg cfg = { "Default", "Earth.atm.cfg"};
	AtmoCfgs["Earth"].push_back(cfg);

	// FindFirstFile/FindNextFile of GC\*_atm.cfg: the resolved folder's names matching case-insensitively, in NTFS (sorted) order
	std::error_code ec;
	std::filesystem::path name = oapiResolvePath((string(OapiExtension::GetConfigDir()) + "GC").c_str());
	std::vector<std::filesystem::directory_entry> FileInformation;
	for (auto &e : std::filesystem::directory_iterator(name, ec)) {
		string n = e.path().filename().string();
		if (n.size() >= 8 && !strcasecmp(n.c_str() + n.size() - 8, "_atm.cfg")) FileInformation.push_back(e);
	}
	std::sort(FileInformation.begin(), FileInformation.end(), [](const auto &a, const auto &b) {
		return strcasecmp(a.path().filename().c_str(), b.path().filename().c_str()) < 0;
	});

	for (auto &e : FileInformation) {
			string cFileName = e.path().filename().string();
			if (cFileName[0] != '.') {
				if (!e.is_directory(ec)) {		
					string cfgname, planet;
					if (GetConfigName(cFileName.c_str(), cfgname, planet)) {
						_AtmoCfg cfg = { cfgname, cFileName };
						AtmoCfgs[planet].push_back(cfg);
					}
					else oapiWriteLogV("File Not Found [%s]", cFileName.c_str());
				}
			}
	}
}



