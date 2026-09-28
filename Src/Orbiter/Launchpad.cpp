// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __linux__
#define STRICT 1
#include <windows.h>
#endif // !__linux__
#include <stdio.h>
#ifndef __linux__
#include <io.h>
#else // __linux__
#include <strings.h>
#endif // __linux__
#include <time.h>
#include <fstream>
#ifndef __linux__
#include "Uxtheme.h"
#include <commctrl.h>
#include "Resource.h"
#else // __linux__
#include "resource.h"
#endif // __linux__
#include "Orbiter.h"
#include "Launchpad.h"
#include "TabScenario.h"
#include "TabOptions.h"
#include "TabModule.h"
#include "TabVideo.h"
#include "TabExtra.h"
#include "TabAbout.h"
#include "Config.h"
#include "Log.h"
#include "Util.h"
#include "about.hpp"
#include "Help.h"
#include "Memstat.h"
#ifdef __linux__
#include "ResDialog.h"
#include <QApplication>
#include <QCloseEvent>
#include <QKeyEvent>
#include <QLabel>
#include <QPainter>
#include <QProgressBar>
#include <QPushButton>
#include <QResizeEvent>
#include <QScreen>
#include <QTimer>
#include <QTreeWidget>
#endif // __linux__

using namespace std;

#ifdef __linux__
// WM_SIZE modes
#define SIZE_RESTORED  0
#define SIZE_MINIMIZED 1

#endif // __linux__
//=============================================================================
// Name: class LaunchpadDialog
// Desc: Handles the startup dialog ("Launchpad")
//=============================================================================

static orbiter::LaunchpadDialog *g_pDlg = 0;
static time_t time0 = 0;
#ifndef __linux__
static UINT timerid = 0;
#endif // !__linux__

const DWORD dlgcol = 0xF0F4F8; // main dialog background colour

#ifdef __linux__
// COLORREF (0x00bbggrr) as QColor
static QColor Colorref (DWORD c)
{
	return QColor (c & 0xff, (c >> 8) & 0xff, (c >> 16) & 0xff);
}

// GetClientRect counterpart
static RECT ClientRect (QWidget *w)
{
	RECT r = {0, 0, w ? w->width() : 0, w ? w->height() : 0};
	return r;
}

#endif // __linux__
//static int mnubt[] = {
//	IDC_MNU_SCN, IDC_MNU_OPT, IDC_MNU_MOD,
//	IDC_MNU_VID, IDC_MNU_EXT, IDC_MNU_ABT
//};

//-----------------------------------------------------------------------------
// Name: LaunchpadDialog()
// Desc: This is the constructor for LaunchpadDialog
//-----------------------------------------------------------------------------
orbiter::LaunchpadDialog::LaunchpadDialog (Orbiter *app)
{
	hDlg    = NULL;
#ifdef __linux__
	hWait   = NULL;
#endif // __linux__
	hInst   = app->GetInstance();
	pApp    = app;
	pCfg    = app->Cfg();
	g_pDlg  = this; // for nonmember callbacks
	CTab    = NULL;
	hTabContainer = NULL;
#ifdef __linux__
	timer   = NULL;
#endif // __linux__
	m_bVisible = false;

#ifndef __linux__
	hDlgBrush = CreateSolidBrush (dlgcol);
	hShadowImg = LoadImage (hInst, MAKEINTRESOURCE(IDB_SHADOW), IMAGE_BITMAP, 0, 0, 0);
#else // __linux__
	hDlgBrush = new QBrush (Colorref (dlgcol));
	hShadowImg = oapiLoadResImage (hInst, IDB_SHADOW);
#endif // __linux__

}

//-----------------------------------------------------------------------------
// Name: ~LaunchpadDialog()
// Desc: This is the destructor for LaunchpadDialog
//-----------------------------------------------------------------------------
orbiter::LaunchpadDialog::~LaunchpadDialog ()
{
	for (auto tab : TabList)
		delete tab;
	TabList.clear();

#ifndef __linux__
	DestroyWindow (hWait);
	DeleteObject (hDlgBrush);
	DeleteObject (hShadowImg);
#else // __linux__
	delete hWait;
	delete hDlg;
	delete hDlgBrush;
	delete hShadowImg;
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: Create()
// Desc: Creates the main application dialog
//-----------------------------------------------------------------------------
bool orbiter::LaunchpadDialog::Create (bool startvideotab)
{
	if (!hDlg) {
#ifndef __linux__
		CreateDialogParam(hInst, MAKEINTRESOURCE(IDD_MAIN), NULL, s_DlgProc, (LPARAM)this);
		hTabContainer = GetDlgItem(hDlg, IDC_MNU_PAGECONTAINER);
#else // __linux__
		QWidget *hWnd = oapiCreateResDialog (hInst, IDD_MAIN, NULL);
		if (!hWnd) return false;
		OnInitDialog (hWnd);
		hTabContainer = oapiResDlgItem(hDlg, IDC_MNU_PAGECONTAINER);
#endif // __linux__
		AddTab (new ScenarioTab (this));
		AddTab(new OptionsTab(this));
		AddTab (new ModuleTab (this));
		AddTab (new DefVideoTab (this));
		AddTab (pExtra = new ExtraTab (this));
		AddTab (new AboutTab (this));
		InitTabControl (hDlg);
		InitSize (hDlg);
		SwitchTabPage (hDlg, 0);
		if (pCfg->CfgDemoPrm.bDemo) {
			SetDemoMode ();
			time0 = time (NULL);
#ifndef __linux__
			timerid = SetTimer (hDlg, 1, 1000, NULL);
#else // __linux__
			timer->start (1000);
#endif // __linux__
		}
		Resize (hDlg, client0.right, client0.bottom, SIZE_RESTORED);
		if (pCfg->rLaunchpad.right > pCfg->rLaunchpad.left) {
#ifndef __linux__
			RECT dr, lr = pCfg->rLaunchpad;
#else // __linux__
			RECT lr = pCfg->rLaunchpad;
#endif // __linux__
			int x = lr.left, y = lr.top, w = lr.right-lr.left, h = lr.bottom-lr.top;
#ifndef __linux__
			GetWindowRect (GetDesktopWindow(), &dr);
			x = min (max ((LONG)x, dr.left), dr.right-w);
			y = min (max ((LONG)y, dr.top), dr.bottom-h);
			SetWindowPos (hDlg, 0, x, y, w, h, 0);
#else // __linux__
			QRect dr = QApplication::primaryScreen()->virtualGeometry();
			x = min (max (x, dr.left()), dr.left()+dr.width()-w);
			y = min (max (y, dr.top()), dr.top()+dr.height()-h);
			// the stored rectangle is the outer window; the frame is added back by the window manager
			QSize frame = hDlg->frameGeometry().size() - hDlg->size();
			hDlg->move (x, y);
			hDlg->resize (w - frame.width(), h - frame.height());
#endif // __linux__
		}
#ifndef __linux__
		SetWindowText (GetDlgItem (hDlg, IDC_BLACKBOX), SIG4 "  \n" SIG2 "  \n" SIG1AA "  \n" SIG1AB "  ");
		SetWindowText (GetDlgItem (hDlg, IDC_VERSION), SIG7);
#else // __linux__
		oapiResDlgItem (hDlg, IDC_BLACKBOX)->setProperty ("text", SIG4 "  \n" SIG2 "  \n" SIG1AA "  \n" SIG1AB "  ");
		oapiResDlgItem (hDlg, IDC_VERSION)->setProperty ("text", SIG7);
#endif // __linux__
		Show();
		if (startvideotab) {
			SwitchTabPage (hDlg, PG_VID);
		}
	} else
		SwitchTabPage (hDlg, PG_SCN);

	return (hDlg != NULL);
}

//-----------------------------------------------------------------------------

void orbiter::LaunchpadDialog::Show()
{
#ifndef __linux__
	ShowWindow(hDlg, SW_SHOW);
#else // __linux__
	hDlg->show();
#endif // __linux__
	m_bVisible = true;
	for (auto tab : TabList)
		tab->LaunchpadShowing(true);
}

//-----------------------------------------------------------------------------

void orbiter::LaunchpadDialog::Hide()
{
#ifndef __linux__
	ShowWindow(hDlg, SW_HIDE);
#else // __linux__
	hDlg->hide();
#endif // __linux__
	m_bVisible = false;
	for (auto tab : TabList)
		tab->LaunchpadShowing(false);
}

//-----------------------------------------------------------------------------

#ifndef __linux__
bool orbiter::LaunchpadDialog::ConsumeMessage(LPMSG pmsg)
{
	return (bool)IsDialogMessage(hDlg, pmsg);
}

#endif // !__linux__
orbiter::LaunchpadTab* orbiter::LaunchpadDialog::GetTab(UINT i) const
{
	return (i < TabList.size() ? TabList[i] : nullptr);
}

//-----------------------------------------------------------------------------
// Name: AddTab()
// Desc: Inserts a new tab into the list
//-----------------------------------------------------------------------------
void orbiter::LaunchpadDialog::AddTab (LaunchpadTab *tab)
{
	TabList.push_back(tab);
}

//-----------------------------------------------------------------------------
// Name: InitTabControl()
// Desc: Sets up the tabs for the tab control interface
//-----------------------------------------------------------------------------
#ifndef __linux__
void orbiter::LaunchpadDialog::InitTabControl (HWND hWnd)
#else // __linux__
void orbiter::LaunchpadDialog::InitTabControl (QWidget *hWnd)
#endif // __linux__
{
	for (auto tab : TabList) {
		tab->Create();
		tab->GetConfig (pCfg);
	}
#ifndef __linux__
	hWait = CreateDialog (hInst, MAKEINTRESOURCE(IDD_PAGE_WAIT2), hWnd, WaitPageProc);
#else // __linux__
	hWait = oapiCreateResDialog (hInst, IDD_PAGE_WAIT2, hWnd);
	WaitProc (hWait);
	hWait->hide();
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: EnableLaunchButton()
// Desc: Enable/disable "Launch Orbiter" button
//-----------------------------------------------------------------------------
void orbiter::LaunchpadDialog::EnableLaunchButton (bool enable) const
{
#ifndef __linux__
	EnableWindow (GetDlgItem (hDlg, IDLAUNCH), enable ? TRUE:FALSE);
#else // __linux__
	oapiResDlgItem (hDlg, IDLAUNCH)->setEnabled (enable);
#endif // __linux__
}

//-----------------------------------------------------------------------------

#ifndef __linux__
void orbiter::LaunchpadDialog::InitSize (HWND hWnd)
#else // __linux__
void orbiter::LaunchpadDialog::InitSize (QWidget *hWnd)
#endif // __linux__
{
	RECT r, rl;
#ifndef __linux__
	GetClientRect (hWnd, &client0);
	GetClientRect (GetDlgItem (hWnd, IDC_BLACKBOX), &copyr0);
	GetClientRect (GetDlgItem (hWnd, IDC_SHADOW), &r);
#else // __linux__
	client0 = ClientRect (hWnd);
	copyr0 = ClientRect (oapiResDlgItem (hWnd, IDC_BLACKBOX));
	r = ClientRect (oapiResDlgItem (hWnd, IDC_SHADOW));
#endif // __linux__
	shadowh = r.bottom;
#ifndef __linux__
	r_launch0 = GetClientPos (hWnd, GetDlgItem (hWnd, IDLAUNCH));
	r_help0   = GetClientPos (hWnd, GetDlgItem (hWnd, 9));
	r_exit0   = GetClientPos (hWnd, GetDlgItem (hWnd, IDEXIT));
#else // __linux__
	r_launch0 = GetClientPos (hWnd, oapiResDlgItem (hWnd, IDLAUNCH));
	r_help0   = GetClientPos (hWnd, oapiResDlgItem (hWnd, 9));
	r_exit0   = GetClientPos (hWnd, oapiResDlgItem (hWnd, IDEXIT));
#endif // __linux__
	r_wait0   = GetClientPos (hWnd, hWait);
#ifndef __linux__
	r_data0   = GetClientPos (hWnd, GetDlgItem(hWnd, IDC_MNU_PAGECONTAINER));
	r_version0= GetClientPos (hWnd, GetDlgItem (hWnd, IDC_VERSION));
#else // __linux__
	r_data0   = GetClientPos (hWnd, oapiResDlgItem(hWnd, IDC_MNU_PAGECONTAINER));
	r_version0= GetClientPos (hWnd, oapiResDlgItem (hWnd, IDC_VERSION));
#endif // __linux__

#ifndef __linux__
	r = GetClientPos (hDlg, GetDlgItem (hDlg, IDC_MNU_SCN));
#else // __linux__
	r = GetClientPos (hDlg, oapiResDlgItem (hDlg, IDC_MNU_SCN));
#endif // __linux__
	int y0 = r.top;
#ifndef __linux__
	r = GetClientPos (hDlg, GetDlgItem (hDlg, IDC_MNU_OPT));
#else // __linux__
	r = GetClientPos (hDlg, oapiResDlgItem (hDlg, IDC_MNU_OPT));
#endif // __linux__
	dy_bt = r.top - y0;

#ifndef __linux__
	GetClientRect (GetDlgItem (hWnd, IDC_LOGO), &rl);
#else // __linux__
	rl = ClientRect (oapiResDlgItem (hWnd, IDC_LOGO));
#endif // __linux__
	if (rl.bottom != copyr0.bottom) {
#ifndef __linux__
		SetWindowPos (GetDlgItem (hWnd, IDC_LOGO), NULL, 0, 0,
			client0.right-copyr0.right, copyr0.bottom,
			SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER|SWP_NOCOPYBITS);
#else // __linux__
		oapiResDlgItem (hWnd, IDC_LOGO)->resize (client0.right-copyr0.right, copyr0.bottom);
#endif // __linux__
	}
}

//-----------------------------------------------------------------------------

#ifndef __linux__
BOOL orbiter::LaunchpadDialog::Resize (HWND hWnd, DWORD w, DWORD h, DWORD mode)
#else // __linux__
BOOL orbiter::LaunchpadDialog::Resize (QWidget *hWnd, DWORD w, DWORD h, DWORD mode)
#endif // __linux__
{
	if (mode == SIZE_MINIMIZED) return TRUE;

#ifndef __linux__
	int i, w4, h4;
#else // __linux__
	int w4, h4;
#endif // __linux__
	int dw = (int)w - (int)client0.right;   // width change compared to initial size
	int dh = (int)h - (int)client0.bottom;  // height change compared to initial size
	int xb1 = r_launch0.left, xb2, xb3;
	int bg = r_exit0.left - r_help0.right;  // button gap
	int wb1 = r_launch0.right-r_launch0.left;
	int wb2 = r_help0.right-r_help0.left;
	int wb3 = r_exit0.right-r_exit0.left;
	int ww = wb1+wb2+wb3;
	int wf = r_exit0.right-r_launch0.left+dw-2*bg;
	if (wf < ww) { // shrink buttons
		wb1 = (wb1*wf)/ww;
		wb2 = (wb2*wf)/ww;
		wb3 = (wb3*wf)/ww;
		xb2 = xb1 + wb1 + bg;
		xb3 = xb2 + wb2 + bg;
	} else {
		xb2 = r_help0.left + dw;
		xb3 = r_exit0.left + dw;
	}
	int bh = r_exit0.bottom - r_exit0.top;  // button height

#ifndef __linux__
	SetWindowPos (GetDlgItem (hWnd, IDC_BLACKBOX), NULL,
		0, 0, copyr0.right + dw, copyr0.bottom,
		SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER|SWP_NOCOPYBITS);
	SetWindowPos (GetDlgItem (hWnd, IDC_SHADOW), NULL,
		0, 0, w, shadowh,
		SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER);
#else // __linux__
	oapiResDlgItem (hWnd, IDC_BLACKBOX)->resize (copyr0.right + dw, copyr0.bottom);
	oapiResDlgItem (hWnd, IDC_SHADOW)->resize (w, shadowh);
#endif // __linux__
	w4 = r_exit0.right - r_data0.left + dw;
	h4 = max ((LONG)10, r_data0.bottom - r_data0.top + dh);
#ifndef __linux__
	SetWindowPos (GetDlgItem (hWnd, IDLAUNCH), NULL,
		xb1, r_launch0.top+dh, wb1, bh,
		SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOZORDER|SWP_NOCOPYBITS);
	SetWindowPos (GetDlgItem (hWnd, 9), NULL,
		xb2, r_help0.top+dh, wb2, bh,
		SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOZORDER|SWP_NOCOPYBITS);
	SetWindowPos (GetDlgItem (hWnd, IDEXIT), NULL,
		xb3, r_exit0.top+dh, wb3, bh,
		SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOZORDER|SWP_NOCOPYBITS);
	SetWindowPos (hWait, NULL,
		(w-(r_wait0.right-r_wait0.left))/2, max (r_wait0.top, r_wait0.top+((LONG)h-r_wait0.bottom)/2), 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hWnd, IDC_VERSION), NULL,
		r_version0.left, r_version0.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOZORDER|SWP_NOCOPYBITS|SWP_NOSIZE);
#else // __linux__
	oapiResDlgItem (hWnd, IDLAUNCH)->setGeometry (xb1, r_launch0.top+dh, wb1, bh);
	oapiResDlgItem (hWnd, 9)->setGeometry (xb2, r_help0.top+dh, wb2, bh);
	oapiResDlgItem (hWnd, IDEXIT)->setGeometry (xb3, r_exit0.top+dh, wb3, bh);
	hWait->move ((w-(r_wait0.right-r_wait0.left))/2, max (r_wait0.top, r_wait0.top+((LONG)h-r_wait0.bottom)/2));
	oapiResDlgItem (hWnd, IDC_VERSION)->move (r_version0.left, r_version0.top+dh);
#endif // __linux__
	DWORD tabAreaW = r_data0.right - r_data0.left + dw;
	DWORD tabAreaH = r_data0.bottom - r_data0.top + dh;
#ifndef __linux__
	SetWindowPos(GetDlgItem(hWnd, IDC_MNU_PAGECONTAINER), NULL, 0, 0, tabAreaW, tabAreaH,
		SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOZORDER);
#else // __linux__
	oapiResDlgItem(hWnd, IDC_MNU_PAGECONTAINER)->resize (tabAreaW, tabAreaH);
#endif // __linux__
	for (auto tab : TabList) {
		tab->TabAreaResized(tabAreaW, tabAreaH);
	}
	return FALSE;
}

//-----------------------------------------------------------------------------
// Name: SetDemoMode()
// Desc: Set launchpad controls into demo mode
//-----------------------------------------------------------------------------

void orbiter::LaunchpadDialog::SetDemoMode ()
{
	//EnableWindow (GetDlgItem (hDlg, IDC_MAINTAB), FALSE);
	//ShowWindow (GetDlgItem (hDlg, IDC_MAINTAB), FALSE);

	static int hide_mnu[] = {
		IDC_MNU_OPT, IDC_MNU_MOD,
		IDC_MNU_VID, IDC_MNU_EXT,
	};
#ifndef __linux__
	for (int i = 0; i < ARRAYSIZE(hide_mnu); i++) EnableWindow (GetDlgItem (hDlg, hide_mnu[i]), FALSE);
	if (pCfg->CfgDemoPrm.bBlockExit) EnableWindow (GetDlgItem (hDlg, IDEXIT), FALSE);
#else // __linux__
	for (size_t i = 0; i < sizeof(hide_mnu)/sizeof(hide_mnu[0]); i++) oapiResDlgItem (hDlg, hide_mnu[i])->setEnabled (false);
	if (pCfg->CfgDemoPrm.bBlockExit) oapiResDlgItem (hDlg, IDEXIT)->setEnabled (false);
	hDlg->setMouseTracking (true); // WM_MOUSEMOVE resets the idle timer
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: UpdateConfig()
// Desc: Save current dialog settings in configuration
//-----------------------------------------------------------------------------
void orbiter::LaunchpadDialog::UpdateConfig ()
{
	for (auto tab : TabList)
		tab->SetConfig (pCfg);

	// get launchpad window geometry (if not minimised)
#ifndef __linux__
	if (!IsIconic(hDlg))
		GetWindowRect (hDlg, &pCfg->rLaunchpad);
#else // __linux__
	if (!hDlg->isMinimized()) {
		QRect g = hDlg->frameGeometry();
		pCfg->rLaunchpad.left = g.left(), pCfg->rLaunchpad.top = g.top();
		pCfg->rLaunchpad.right = g.left()+g.width(), pCfg->rLaunchpad.bottom = g.top()+g.height();
	}
#endif // __linux__
}

//-----------------------------------------------------------------------------
#ifndef __linux__
// Name: DlgProc()
// Desc: Message callback function for main dialog
#else // __linux__
// Name: OnInitDialog()
// Desc: WM_INITDIALOG of the main dialog: hooks its events and connects its controls
#endif // __linux__
//-----------------------------------------------------------------------------
#ifndef __linux__
INT_PTR orbiter::LaunchpadDialog::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void orbiter::LaunchpadDialog::OnInitDialog (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	char cbuf[256];
#else // __linux__
	hDlg = hWnd;

	// WM_COMMAND
	static const int cmd[] = {IDLAUNCH, IDEXIT, IDHELP, IDC_MNU_SCN, IDC_MNU_OPT, IDC_MNU_MOD, IDC_MNU_VID, IDC_MNU_EXT, IDC_MNU_ABT};
	for (int id : cmd)
		if (QPushButton *b = DlgItem<QPushButton> (hWnd, id))
			QObject::connect (b, &QPushButton::clicked, hWnd, [this, id]() { OnCommand (id); });

	// window events, and the owner-drawn shadow bar and page container (WM_DRAWITEM)
	EventHook *hook = new EventHook (hWnd, [this](QObject *obj, QEvent *event) { return DlgProc (obj, event); });
	hook->Attach (oapiResDlgItem (hWnd, IDC_SHADOW));
	hook->Attach (oapiResDlgItem (hWnd, IDC_MNU_PAGECONTAINER));

	// WM_CTLCOLORSTATIC: light text on black for the copyright box
	QWidget *bb = oapiResDlgItem (hWnd, IDC_BLACKBOX);
	QPalette pal = bb->palette();
	pal.setColor (QPalette::WindowText, Colorref (0xF0B0B0));
	pal.setColor (QPalette::Window, Qt::black);
	bb->setPalette (pal);
	bb->setAutoFillBackground (true);

	// WM_GETMINMAXINFO
	hWnd->setMinimumSize (550, 350);
#endif // __linux__

#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG:
		EnableThemeDialogTexture(hWnd, ETDT_ENABLE);
		hDlg = hWnd;
		return FALSE;
	case WM_CLOSE:
		if (pCfg->CfgDemoPrm.bBlockExit) return TRUE;
		UpdateConfig ();
		DestroyWindow (hWnd);
		return TRUE;
	case WM_DESTROY:
		if (pCfg->CfgDemoPrm.bDemo && timerid) {
			KillTimer (hWnd, 1);
			timerid = 0;
		}
		PostQuitMessage (0);
		return TRUE;
	case WM_SIZE:
		return Resize (hWnd, LOWORD(lParam), HIWORD(lParam), wParam);
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDLAUNCH:
			if (((ScenarioTab*)TabList[0])->GetSelScenario (cbuf, 256) == 1) {
				UpdateConfig ();
				pApp->Launch (cbuf);
#else // __linux__
	// WM_TIMER: demo mode auto-launch
	timer = new QTimer (hWnd);
	QObject::connect (timer, &QTimer::timeout, hWnd, [this]() {
		if (difftime (time (NULL), time0) > pCfg->CfgDemoPrm.LPIdleTime) { // auto-launch a demo
			if (SelectDemoScenario ())
				QMetaObject::invokeMethod (hDlg, [this]() { OnCommand (IDLAUNCH); }, Qt::QueuedConnection);
		}
	});
}

//-----------------------------------------------------------------------------
// Name: OnCommand()
// Desc: WM_COMMAND of the main dialog's buttons
//-----------------------------------------------------------------------------
void orbiter::LaunchpadDialog::OnCommand (int id)
{
	char cbuf[256];

	switch (id) {
	case IDLAUNCH:
		if (((ScenarioTab*)TabList[0])->GetSelScenario (cbuf, 256) == 1) {
			UpdateConfig ();
			pApp->Launch (cbuf);
		}
		return;
	case IDEXIT:
		QMetaObject::invokeMethod (hDlg, "close", Qt::QueuedConnection);
		return;
	case IDHELP:
		if (CTab) CTab->OpenHelp ();
		return;
	case IDC_MNU_SCN:
		SwitchTabPage (hDlg, PG_SCN);
		return;
	case IDC_MNU_OPT:
		SwitchTabPage(hDlg, PG_OPT);
		return;
	case IDC_MNU_MOD:
		SwitchTabPage (hDlg, PG_MOD);
		return;
	case IDC_MNU_VID:
		SwitchTabPage (hDlg, PG_VID);
		return;
	case IDC_MNU_EXT:
		SwitchTabPage (hDlg, PG_EXT);
		return;
	case IDC_MNU_ABT:
		SwitchTabPage (hDlg, PG_ABT);
		return;
	}
}

//-----------------------------------------------------------------------------
// Name: DlgProc()
// Desc: Event callback function for main dialog
//-----------------------------------------------------------------------------
bool orbiter::LaunchpadDialog::DlgProc (QObject *obj, QEvent *event)
{
	if (obj == hDlg) {
		switch (event->type()) {
		case QEvent::Close:
			if (pCfg->CfgDemoPrm.bBlockExit) {
				event->ignore();
				return true;
#endif // __linux__
			}
#ifndef __linux__
			return TRUE;
		case IDEXIT:
			PostMessage (hWnd, WM_CLOSE, 0, 0);
			return TRUE;
		case IDHELP:
			if (CTab) CTab->OpenHelp ();
			return TRUE;
		case IDC_MNU_SCN:
			SwitchTabPage (hWnd, PG_SCN);
			return TRUE;
		case IDC_MNU_OPT:
			SwitchTabPage(hWnd, PG_OPT);
			return TRUE;
		case IDC_MNU_MOD:
			SwitchTabPage (hWnd, PG_MOD);
			return TRUE;
		case IDC_MNU_VID:
			SwitchTabPage (hWnd, PG_VID);
			return TRUE;
		case IDC_MNU_EXT:
			SwitchTabPage (hWnd, PG_EXT);
			return TRUE;
		case IDC_MNU_ABT:
			SwitchTabPage (hWnd, PG_ABT);
			return TRUE;
		}
		break;
	case WM_SHOWWINDOW:
		if (pCfg->CfgDemoPrm.bDemo) {
			if (wParam) {
				time0 = time (NULL);
				if (!timerid) timerid = SetTimer (hWnd, 1, 1000, NULL);
			} else {
				if (timerid) {
					KillTimer (hWnd, 1);
					timerid = 0;
#else // __linux__
			UpdateConfig ();
			// WM_DESTROY
			if (pCfg->CfgDemoPrm.bDemo && timer->isActive())
				timer->stop();
			QCoreApplication::quit(); // PostQuitMessage
			return false;
		case QEvent::Resize: {
			QSize s = static_cast<QResizeEvent*> (event)->size();
			Resize (hDlg, s.width(), s.height(), hDlg->isMinimized() ? SIZE_MINIMIZED : SIZE_RESTORED);
			} return false;
		case QEvent::Show:
		case QEvent::Hide:
			if (pCfg->CfgDemoPrm.bDemo) {
				if (event->type() == QEvent::Show) {
					time0 = time (NULL);
					if (!timer->isActive()) timer->start (1000);
				} else {
					if (timer->isActive()) timer->stop();
#endif // __linux__
				}
			}
#ifdef __linux__
			return false;
		case QEvent::MouseMove:
			if (pCfg->CfgDemoPrm.bDemo) time0 = time(NULL); // reset timer
			return false;
		case QEvent::KeyPress:
			if (pCfg->CfgDemoPrm.bDemo) time0 = time(NULL); // reset timer
			// Escape is IDCANCEL, which the Launchpad ignores (QDialog would hide itself)
			return static_cast<QKeyEvent*> (event)->key() == Qt::Key_Escape;
		default:
			return false;
		}
	}
	if (event->type() == QEvent::Paint) {
		QWidget *w = static_cast<QWidget*> (obj);
		if (obj == oapiResDlgItem (hDlg, IDC_SHADOW)) {
			QPainter p (w);
			if (hShadowImg)
				p.drawImage (QRect (0, 0, w->width(), w->height()), *hShadowImg, QRect (0, 0, 8, 8));
			return true;
		}
		else if (obj == oapiResDlgItem (hDlg, IDC_MNU_PAGECONTAINER)) {
			QPainter p (w);
			p.fillRect (w->rect(), w->palette().color (QPalette::Button)); // COLOR_3DFACE
			return true;
#endif // __linux__
		}
#ifndef __linux__
		return 0;
	case WM_CTLCOLORSTATIC:
		if (lParam == (LPARAM)GetDlgItem (hWnd, IDC_BLACKBOX)) {
			HDC hDC = (HDC)wParam;
			SetTextColor (hDC, 0xF0B0B0);
			SetBkColor (hDC,0);
			//break;
			return (LRESULT)(HBRUSH)GetStockObject(BLACK_BRUSH);
		} else break;
	case WM_DRAWITEM: {
		LPDRAWITEMSTRUCT lpDrawItem = (LPDRAWITEMSTRUCT)lParam;
		if (wParam == IDC_SHADOW) {
			HDC hDC = lpDrawItem->hDC;
			HDC mDC = CreateCompatibleDC (hDC);
			HANDLE hp = SelectObject (mDC, hShadowImg);
			StretchBlt (hDC, 0, 0, lpDrawItem->rcItem.right, lpDrawItem->rcItem.bottom, 
				mDC, 0, 0, 8, 8, SRCCOPY);
			SelectObject (mDC, hp);
			DeleteDC (mDC);
			return TRUE;
		}
		else if (wParam == IDC_MNU_PAGECONTAINER) {
			HDC hDC = lpDrawItem->hDC;
			HANDLE hpBrush = SelectObject(hDC, GetSysColorBrush(COLOR_3DFACE));
			HANDLE hpPen = SelectObject(hDC, GetStockObject(NULL_PEN));
			Rectangle(hDC, -1, -1, lpDrawItem->rcItem.right+1, lpDrawItem->rcItem.bottom+1);
			SelectObject(hDC, hpBrush);
			return TRUE;
		}
		} break;
	case WM_MOUSEMOVE:
		if (pCfg->CfgDemoPrm.bDemo) time0 = time(NULL); // reset timer
		break;
	case WM_KEYDOWN:
		if (pCfg->CfgDemoPrm.bDemo) time0 = time(NULL); // reset timer
		break;
//	case WM_CTLCOLORDLG:
//		return (LRESULT)hDlgBrush;
	case WM_GETMINMAXINFO: {
		LPMINMAXINFO lpMMI = (LPMINMAXINFO)lParam;
		lpMMI->ptMinTrackSize.x = 550;
		lpMMI->ptMinTrackSize.y = 350;
		}
		return 0;
	case WM_TIMER:
		if (difftime (time (NULL), time0) > pCfg->CfgDemoPrm.LPIdleTime) { // auto-launch a demo
			if (SelectDemoScenario ())
				PostMessage (hWnd, WM_COMMAND, IDLAUNCH, 0);
		}
		return 0;
#endif // !__linux__
	}
#ifndef __linux__
	return FALSE;
#else // __linux__
	return false;
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: WaitProc()
#ifndef __linux__
// Desc: Message callback function for wait page
#else // __linux__
// Desc: Set-up of the wait page (WM_INITDIALOG and colours)
#endif // __linux__
//-----------------------------------------------------------------------------
#ifndef __linux__
INT_PTR orbiter::LaunchpadDialog::WaitProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void orbiter::LaunchpadDialog::WaitProc (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG:
		SendDlgItemMessage (hWnd, IDC_PROGRESS1, PBM_SETRANGE, 0, MAKELPARAM(0, 1000));
		return TRUE;
	case WM_CTLCOLORSTATIC:
		if (lParam == (LPARAM)GetDlgItem (hWnd, IDC_WAITTEXT)) {
			HDC hDC = (HDC)wParam;
			//SetTextColor (hDC, 0xFFD0D0);
			//SetBkColor (hDC,0);
			SetBkMode (hDC, TRANSPARENT);
			return (LRESULT)hDlgBrush;
		} else break;
	case WM_CTLCOLORDLG:
		return (LRESULT)hDlgBrush;
	}
    return FALSE;
#else // __linux__
	QProgressBar *pb = DlgItem<QProgressBar> (hWnd, IDC_PROGRESS1);
	if (pb) pb->setRange (0, 1000);

	// WM_CTLCOLORDLG / WM_CTLCOLORSTATIC: dialog brush behind a transparent wait text
	QPalette pal = hWnd->palette();
	pal.setBrush (QPalette::Window, *hDlgBrush);
	hWnd->setPalette (pal);
	hWnd->setAutoFillBackground (true);
	if (QWidget *wt = oapiResDlgItem (hWnd, IDC_WAITTEXT))
		wt->setAutoFillBackground (false);
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: SwitchTabPage()
// Desc: Display a new page
//-----------------------------------------------------------------------------
#ifndef __linux__
void orbiter::LaunchpadDialog::SwitchTabPage (HWND hWnd, int cpg)
#else // __linux__
void orbiter::LaunchpadDialog::SwitchTabPage (QWidget *hWnd, int cpg)
#endif // __linux__
{
	for (size_t pg = 0; pg < TabList.size(); pg++)
#ifndef __linux__
		if (pg != cpg) TabList[pg]->Hide();
	CTab = (cpg >= 0 && cpg < TabList.size() ? TabList[cpg] : nullptr);
#else // __linux__
		if (pg != (size_t)cpg) TabList[pg]->Hide();
	CTab = (cpg >= 0 && cpg < (int)TabList.size() ? TabList[cpg] : nullptr);
#endif // __linux__
	if (CTab) CTab->Show();
}

//-----------------------------------------------------------------------------

void orbiter::LaunchpadDialog::ShowWaitPage (bool show, long mem_committed)
{
#ifndef __linux__
	int i;
#else // __linux__
	size_t i;
#endif // __linux__
	int item[3] = {IDLAUNCH, 9, IDEXIT};

	if (show) {
#ifndef __linux__
		for (i = 0; i < ARRAYSIZE(item); i++)
			ShowWindow(GetDlgItem(hDlg, item[i]), SW_HIDE);
#else // __linux__
		for (i = 0; i < 3; i++)
			oapiResDlgItem(hDlg, item[i])->hide();
#endif // __linux__
	}
	if (show) {
#ifndef __linux__
		SetCursor(LoadCursor(NULL, IDC_WAIT));
#else // __linux__
		QApplication::setOverrideCursor (Qt::WaitCursor);
#endif // __linux__
		for (auto tab : TabList)
			tab->Hide();
		mem_wait = mem_committed/1000;
		mem0 = pApp->memstat->HeapUsage();
#ifndef __linux__
		SendDlgItemMessage (hWait, IDC_PROGRESS1, PBM_SETPOS, 0, 0);
		ShowWindow (GetDlgItem (hWait, IDC_PROGRESS1), mem_wait ? SW_SHOW:SW_HIDE);
		ShowWindow (hWait, SW_SHOW);
#else // __linux__
		QProgressBar *pb = DlgItem<QProgressBar> (hWait, IDC_PROGRESS1);
		pb->setValue (0);
		pb->setVisible (mem_wait != 0);
		hWait->show();
#endif // __linux__
	} else {
#ifndef __linux__
		SetCursor(LoadCursor(NULL, IDC_ARROW));
		ShowWindow (hWait, SW_HIDE);
#else // __linux__
		QApplication::restoreOverrideCursor();
		hWait->hide();
#endif // __linux__
		SwitchTabPage (hDlg, 0);
	}
	if (!show)
#ifndef __linux__
		for (i = 0; i < ARRAYSIZE(item); i++)
			ShowWindow (GetDlgItem (hDlg, item[i]), SW_SHOW);
#else // __linux__
		for (i = 0; i < 3; i++)
			oapiResDlgItem (hDlg, item[i])->show();
#endif // __linux__

#ifndef __linux__
	RedrawWindow(hDlg, NULL, NULL, RDW_UPDATENOW | RDW_ALLCHILDREN);
#else // __linux__
	hDlg->repaint(); // RDW_UPDATENOW: the caller keeps the event loop busy
#endif // __linux__
}

void orbiter::LaunchpadDialog::UpdateWaitProgress ()
{
	if (mem_wait) {
		long mem = pApp->memstat->HeapUsage();
#ifndef __linux__
		SendDlgItemMessage (hWait, IDC_PROGRESS1, PBM_SETPOS, (mem0-mem)/mem_wait, 0);
#else // __linux__
		DlgItem<QProgressBar> (hWait, IDC_PROGRESS1)->setValue ((mem0-mem)/mem_wait);
#endif // __linux__
	}
}

//-----------------------------------------------------------------------------
// Name: GetDemoScenario()
// Desc: returns the name of an arbitrary scenario in the demo folder
//-----------------------------------------------------------------------------
int orbiter::LaunchpadDialog::SelectDemoScenario ()
{
#ifndef __linux__
	char cbuf[256];
	HWND hTree = GetDlgItem (GetTab(PG_SCN)->TabWnd(), IDC_SCN_LIST);
	HTREEITEM demo;
	TV_ITEM tvi;
	tvi.hItem = TreeView_GetRoot (hTree);
	tvi.pszText = cbuf;
	tvi.cchTextMax = 256;
	tvi.cChildren = 0;
	tvi.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_CHILDREN;
	while (tvi.hItem) {
		TreeView_GetItem (hTree, &tvi);
		if (!_stricmp (cbuf, "Demo")) break;
		tvi.hItem = TreeView_GetNextSibling (hTree, tvi.hItem);
#else // __linux__
	QTreeWidget *hTree = DlgItem<QTreeWidget> (GetTab(PG_SCN)->TabWnd(), IDC_SCN_LIST);
	QTreeWidgetItem *demo = NULL;
	for (int i = 0; i < hTree->topLevelItemCount(); i++) {
		QTreeWidgetItem *it = hTree->topLevelItem (i);
		if (!strcasecmp (it->text (0).toUtf8().constData(), "Demo")) {
			demo = it;
			break;
		}
#endif // __linux__
	}
#ifndef __linux__
	if (tvi.hItem) demo = tvi.hItem;
	else           return 0;
#else // __linux__
	if (!demo) return 0;
#endif // __linux__

	int seldemo, ndemo = 0;
#ifndef __linux__
	tvi.hItem = TreeView_GetChild (hTree, demo);
	while (tvi.hItem) {
		TreeView_GetItem (hTree, &tvi);
		if (!tvi.cChildren) ndemo++;
		tvi.hItem = TreeView_GetNextSibling (hTree, tvi.hItem);
	}
#else // __linux__
	for (int i = 0; i < demo->childCount(); i++)
		if (!demo->child (i)->childCount()) ndemo++;
#endif // __linux__
	if (!ndemo) return 0;
#ifndef __linux__
	seldemo = (rand()*ndemo)/(RAND_MAX+1);
#else // __linux__
	seldemo = (int)(((double)rand()*ndemo)/((double)RAND_MAX+1.0)); // glibc RAND_MAX is 2^31-1: no int product
#endif // __linux__
	ndemo = 0;
#ifndef __linux__
	tvi.hItem = TreeView_GetChild (hTree, demo);
	while (tvi.hItem) {
		TreeView_GetItem (hTree, &tvi);
		if (!tvi.cChildren) {
#else // __linux__
	for (int i = 0; i < demo->childCount(); i++) {
		QTreeWidgetItem *it = demo->child (i);
		if (!it->childCount()) {
#endif // __linux__
			if (ndemo == seldemo) {
#ifndef __linux__
				return (TreeView_SelectItem (hTree, tvi.hItem) != 0);
#else // __linux__
				hTree->setCurrentItem (it);
				return 1;
#endif // __linux__
			}
			ndemo++;
		}
#ifndef __linux__
		tvi.hItem = TreeView_GetNextSibling (hTree, tvi.hItem);
#endif // !__linux__
	}
	return 0;
}

// ****************************************************************************
// "Extra Parameters" page
// ****************************************************************************

static const char *desc_fixedstep = "Force Orbiter to advance the simulation by a fixed time interval in each frame.";

#ifndef __linux__
void OpenDynamics (HINSTANCE, HWND);
#else // __linux__
void OpenDynamics (void*, QWidget*);
#endif // __linux__

#ifndef __linux__
HTREEITEM orbiter::LaunchpadDialog::RegisterExtraParam (LaunchpadItem *item, HTREEITEM parent)
#else // __linux__
QTreeWidgetItem *orbiter::LaunchpadDialog::RegisterExtraParam (LaunchpadItem *item, QTreeWidgetItem *parent)
#endif // __linux__
{
	return pExtra->RegisterExtraParam (item, parent);
}

bool orbiter::LaunchpadDialog::UnregisterExtraParam (LaunchpadItem *item)
{
	return pExtra->UnregisterExtraParam (item);
}

#ifndef __linux__
HTREEITEM orbiter::LaunchpadDialog::FindExtraParam (const char *name, const HTREEITEM parent)
#else // __linux__
QTreeWidgetItem *orbiter::LaunchpadDialog::FindExtraParam (const char *name, QTreeWidgetItem *parent)
#endif // __linux__
{
	return pExtra->FindExtraParam (name, parent);
}

void orbiter::LaunchpadDialog::WriteExtraParams ()
{
	pExtra->WriteExtraParams ();
#ifndef __linux__
}

//-----------------------------------------------------------------------------
// Name: s_DlgProc()
// Desc: Static msg handler which passes messages from the main dialog
//       to the application class.
//-----------------------------------------------------------------------------
INT_PTR CALLBACK orbiter::LaunchpadDialog::s_DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return g_pDlg->DlgProc(hWnd, uMsg, wParam, lParam);
}


//=============================================================================
// Nonmember functions
//=============================================================================

//-----------------------------------------------------------------------------
// Name: WaitPageProc()
// Desc: Dummy function for wait page
//-----------------------------------------------------------------------------
INT_PTR CALLBACK WaitPageProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	return g_pDlg->WaitProc (hWnd, uMsg, wParam, lParam);
}

LONG_PTR FAR PASCAL MsgProc_CopyrightFrame (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	//switch (uMsg) {
	//case WM_PAINT:

	return DefWindowProc (hWnd, uMsg, wParam, lParam);
#endif // !__linux__
}
