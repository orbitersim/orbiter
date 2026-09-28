// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// Launchpad tab implementations
//=============================================================================

#ifndef __linux__
#define STRICT 1
#include <windows.h>
#include <commctrl.h>
#endif // !__linux__
#include "LpadTab.h"
#include "Launchpad.h"
#include "Log.h"
#include "Help.h"
#include "resource.h"
#ifdef __linux__
#include "ResDialog.h"
#include <QResizeEvent>
#include <algorithm>
#endif // __linux__

using std::max;

//-----------------------------------------------------------------------------
// LaunchpadTab base class

orbiter::LaunchpadTab::LaunchpadTab (const LaunchpadDialog *lp)
{
	pLp = lp;
	pCfg = lp->Cfg();
	hTab = NULL;
	bActive = false;
	nitem = 0;
	item = NULL;
	itempos = NULL;
}

//-----------------------------------------------------------------------------

orbiter::LaunchpadTab::~LaunchpadTab ()
{
#ifndef __linux__
	if (hTab) DestroyWindow (hTab);
#else // __linux__
	if (hTab) delete hTab;
#endif // __linux__
	if (nitem) {
		delete []item;
		item = NULL;
		delete []itempos;
		itempos = NULL;
	}
}

//-----------------------------------------------------------------------------

void orbiter::LaunchpadTab::Show ()
{
#ifndef __linux__
	if (hTab) ShowWindow (hTab, SW_SHOW);
#else // __linux__
	if (hTab) hTab->show();
#endif // __linux__
	bActive = true;
}

//-----------------------------------------------------------------------------

void orbiter::LaunchpadTab::Hide ()
{
#ifndef __linux__
	if (hTab) ShowWindow (hTab, SW_HIDE);
#else // __linux__
	if (hTab) hTab->hide();
#endif // __linux__
	bActive = false;
}

//-----------------------------------------------------------------------------

void orbiter::LaunchpadTab::OpenTabHelp(const char* topic)
{
	::OpenDefaultHelp(LaunchpadWnd(), topic);
}

//-----------------------------------------------------------------------------

void orbiter::LaunchpadTab::TabAreaResized(int w, int h)
{
	if (hTab) {
		if (DynamicSize())
#ifndef __linux__
			SetWindowPos(hTab, NULL, 0, 0, w, h,
				SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOZORDER);
#else // __linux__
			hTab->resize(w, h);
#endif // __linux__
		else {
#ifndef __linux__
			RECT r;
			GetClientRect(hTab, &r);
			int x0 = max((LONG)0, (w - r.right) / 2);
			int y0 = max((LONG)0, (h - r.bottom) / 2);
			SetWindowPos(hTab, NULL, x0, y0, 0, 0,
				SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOOWNERZORDER | SWP_NOZORDER);
#else // __linux__
			int x0 = max(0, (w - hTab->width()) / 2);
			int y0 = max(0, (h - hTab->height()) / 2);
			hTab->move(x0, y0);
#endif // __linux__
		}
	}
}

//-----------------------------------------------------------------------------

#ifndef __linux__
HWND orbiter::LaunchpadTab::CreateTab (int resid)
#else // __linux__
QWidget *orbiter::LaunchpadTab::CreateTab (int resid)
#endif // __linux__
{
#ifndef __linux__
	HWND hT = CreateDialogParam (AppInstance(), MAKEINTRESOURCE(resid), pLp->HTabContainer(), TabProcHook, (LPARAM)this);

	POINT p0, p1;
	GetClientRect (hT, &pos0);
	p0.x = p0.y = 0; ClientToScreen (LaunchpadWnd(), &p0);
	p1.x = p1.y = 0; ClientToScreen (hT, &p1);
	int dx = p1.x-p0.x, dy = p1.y-p0.y;
#else // __linux__
	QWidget *hT = oapiCreateResDialog (AppInstance(), resid, pLp->HTabContainer());
	new EventHook (hT, [this](QObject *obj, QEvent *event) { return TabProc (static_cast<QWidget*> (obj), event); });
	OnInitDialog (hT); // WM_INITDIALOG

	pos0.left = pos0.top = 0;
	pos0.right = hT->width(), pos0.bottom = hT->height();
	QPoint d = hT->mapTo (LaunchpadWnd(), QPoint (0, 0));
	int dx = d.x(), dy = d.y();
#endif // __linux__
	pos0.left += dx, pos0.right += dx;
	pos0.top += dy, pos0.bottom += dy;

	return hT;
}

//-----------------------------------------------------------------------------

BOOL orbiter::LaunchpadTab::OnSize(int w, int h)
{
	if (nitem) {
		int dx = max(0, (w - (int)(pos0.right - pos0.left)) / 2);
		int dy = max(0, (h - (int)(pos0.bottom - pos0.top)) / 2);
		for (int i = 0; i < nitem; i++) {
#ifndef __linux__
			SetWindowPos(GetDlgItem(hTab, item[i]), NULL,
				itempos[i].x + dx, itempos[i].y + dy, 0, 0,
				SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_NOCOPYBITS);
#else // __linux__
			oapiResDlgItem(hTab, item[i])->move(itempos[i].x + dx, itempos[i].y + dy);
#endif // __linux__
		}
		return FALSE;
	}
	return TRUE;
}

//-----------------------------------------------------------------------------

#ifndef __linux__
INT_PTR orbiter::LaunchpadTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
bool orbiter::LaunchpadTab::TabProc (QWidget *hWnd, QEvent *event)
#endif // __linux__
{
#ifndef __linux__
	switch (uMsg) {
	case WM_INITDIALOG:
		return OnInitDialog(hWnd, wParam, lParam);
	case WM_SIZE:
		return OnSize(LOWORD(lParam), HIWORD(lParam));
	case WM_NOTIFY:
		return OnNotify(hWnd, (int)wParam, (LPNMHDR)lParam);
#else // __linux__
	switch (event->type()) {
	case QEvent::Resize: {
		QSize s = static_cast<QResizeEvent*> (event)->size();
		OnSize(s.width(), s.height());
		} return false;
#endif // __linux__
	default:
#ifndef __linux__
		return OnMessage(hWnd, uMsg, wParam, lParam);
	}
	return FALSE;
}

//-----------------------------------------------------------------------------

INT_PTR CALLBACK orbiter::LaunchpadTab::TabProcHook (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LaunchpadTab* lt = nullptr;
	if (uMsg == WM_INITDIALOG) {
		lt = (LaunchpadTab*)lParam;
		SetWindowLongPtr(hWnd, DWLP_USER, (LONG_PTR)lParam);
	}
	else {
		lt = (LaunchpadTab*)GetWindowLongPtr(hWnd, DWLP_USER);
		int i = 1;
#else // __linux__
		return OnMessage(hWnd, event);
#endif // __linux__
	}
#ifndef __linux__
	return (lt ? lt->TabProc(hWnd, uMsg, wParam, lParam) : FALSE);
#else // __linux__
	return false;
#endif // __linux__
}
