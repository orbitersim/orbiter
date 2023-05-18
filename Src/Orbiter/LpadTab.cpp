// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// Launchpad tab implementations
//=============================================================================

#define STRICT 1
#include <windows.h>
#include <commctrl.h>
#include "LpadTab.h"
#include "Launchpad.h"
#include "Log.h"
#include "Help.h"
#include "resource.h"

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
	if (hTab) DestroyWindow (hTab);
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
	if (hTab) ShowWindow (hTab, SW_SHOW);
	bActive = true;
}

//-----------------------------------------------------------------------------

void orbiter::LaunchpadTab::Hide ()
{
	if (hTab) ShowWindow (hTab, SW_HIDE);
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
			SetWindowPos(hTab, NULL, 0, 0, w, h,
				SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOZORDER);
		else {
			RECT r;
			GetClientRect(hTab, &r);
			int x0 = max(0, (w - r.right) / 2);
			int y0 = max(0, (h - r.bottom) / 2);
			SetWindowPos(hTab, NULL, x0, y0, 0, 0,
				SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOOWNERZORDER | SWP_NOZORDER);
		}
	}
}

//-----------------------------------------------------------------------------

HWND orbiter::LaunchpadTab::CreateTab (int resid)
{
	HWND hT = CreateDialogParam (AppInstance(), MAKEINTRESOURCE(resid), pLp->HTabContainer(), TabProcHook, (LPARAM)this);

	POINT p0, p1;
	GetClientRect (hT, &pos0);
	p0.x = p0.y = 0; ClientToScreen (LaunchpadWnd(), &p0);
	p1.x = p1.y = 0; ClientToScreen (hT, &p1);
	int dx = p1.x-p0.x, dy = p1.y-p0.y;
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
			SetWindowPos(GetDlgItem(hTab, item[i]), NULL,
				itempos[i].x + dx, itempos[i].y + dy, 0, 0,
				SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_NOCOPYBITS);
		}
		return FALSE;
	}
	return TRUE;
}

//-----------------------------------------------------------------------------

INT_PTR orbiter::LaunchpadTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return OnInitDialog(hWnd, wParam, lParam);
	case WM_SIZE:
		return OnSize(LOWORD(lParam), HIWORD(lParam));
	case WM_NOTIFY:
		return OnNotify(hWnd, (int)wParam, (LPNMHDR)lParam);
	default:
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
	}
	return (lt ? lt->TabProc(hWnd, uMsg, wParam, lParam) : FALSE);
}
