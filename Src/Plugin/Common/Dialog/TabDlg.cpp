// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER SDK Utility: TabDlg
//                  Part of the ORBITER SDK
//
// TabDlg.cpp
// A helper class for supporting tabs in dialog boxes
// ==============================================================

#include "TabDlg.h"
#include "CommCtrl.h"
#include "Uxtheme.h"
#include "OrbiterAPI.h"

// ==============================================================
// class TabbedDialog: Dialog containing a single tab control
// ==============================================================

TabbedDialog::TabbedDialog (int _dlgId, int _tabId)
{
	dlgId = _dlgId;
	tabId = _tabId;
	nTab = 0;
	pTab = NULL;
	hDlg = NULL;
	hInst = NULL;
}

// --------------------------------------------------------------

TabbedDialog::~TabbedDialog ()
{
	Close ();
}

// --------------------------------------------------------------

void TabbedDialog::Open (HINSTANCE hInstance, bool allowMulti)
{
	if (hDlg) return;
	hInst = hInstance;
	hDlg = oapiOpenDialogEx (hInst, dlgId, DlgProcHook, allowMulti ? DLG_ALLOWMULTI:0, this);
}

// --------------------------------------------------------------

void TabbedDialog::Close ()
{
	if (hDlg) {
		ClearTabs ();
		oapiCloseDialog (hDlg);
		hDlg = NULL;
		Closed ();
	}
}

// --------------------------------------------------------------

int TabbedDialog::AddTab (TabPage *tab, const char *label)
{
	if (!hDlg) return -1; // sanity check

	char cbuf[256];
	strncpy (cbuf, label, 256);
	TC_ITEM tie;
	tie.mask = TCIF_TEXT;
	tie.iImage = -1;
	tie.pszText = cbuf;
	SendDlgItemMessage (hDlg, tabId, TCM_INSERTITEM, nTab, (LPARAM)&tie);
	TabPage **tmp = new TabPage*[nTab+1];
	if (nTab) {
		memcpy (tmp, pTab, nTab*sizeof(TabPage*));
		delete []pTab;
	}
	pTab = tmp;
	pTab[nTab] = tab;
	tab->Open();
	return nTab++;
}

// --------------------------------------------------------------

void TabbedDialog::SwitchTab ()
{
	int pg, cpg = TabCtrl_GetCurSel (GetDlgItem (hDlg, tabId));
	for (pg = 0; pg < nTab; pg++)
		if (pg != cpg) pTab[pg]->Show (false);
	pTab[cpg]->Show (true);
}

// --------------------------------------------------------------

void TabbedDialog::ClearTabs ()
{
	if (nTab) {
		for (int i = 0; i < nTab; i++)
			delete pTab[i];
		delete []pTab;
		nTab = 0;
		pTab = NULL;
	}
}

// --------------------------------------------------------------

int TabbedDialog::OnInitDialog (WPARAM wParam)
{
	SwitchTab();
	return FALSE;
}

// --------------------------------------------------------------

int TabbedDialog::OnClose ()
{
	Close ();
	return TRUE;
}

// --------------------------------------------------------------

int TabbedDialog::Closed ()
{
	return TRUE;
}

// --------------------------------------------------------------

INT_PTR TabbedDialog::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return OnInitDialog (wParam);
	case WM_NOTIFY: {
		LPNMHDR pnmh = (LPNMHDR)lParam;
		if (pnmh->idFrom == tabId) {
			if (pnmh->code == TCN_SELCHANGE) SwitchTab ();
			return TRUE;
		}
		} return -1;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDCANCEL:
			return OnClose ();
		}
		break;
	}
	return oapiDefDialogProc (hWnd, uMsg, wParam, lParam);
}

// --------------------------------------------------------------

static INT_PTR CALLBACK DlgProcHook (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if (uMsg == WM_INITDIALOG) {
		SetWindowLongPtr (hWnd, GWLP_USERDATA, (LONG_PTR)lParam);
		// store class pointer with window
		((TabbedDialog*)lParam)->hDlg = hWnd;
		// store window handle here so it's available in OnInitDialog
	}
	TabbedDialog *dlg = (TabbedDialog*)GetWindowLongPtr (hWnd, GWLP_USERDATA);
	if (dlg) return dlg->DlgProc (hWnd, uMsg, wParam, lParam);
	else     return oapiDefDialogProc (hWnd, uMsg, wParam, lParam);
}


// ==============================================================
// class TabPage: Base class representing a tab in the control
// ==============================================================

TabPage::TabPage (TabbedDialog *frame, int _pageId)
{
	dlg = frame;
	pageId = _pageId;
	active = false;
	hTab = NULL;
}

// --------------------------------------------------------------

void TabPage::Open ()
{
	hTab = CreateDialogParam (dlg->hInst, MAKEINTRESOURCE(pageId), dlg->hDlg, TabProcHook, (LPARAM)this);
}

// --------------------------------------------------------------

void TabPage::Show (bool show)
{
	ShowWindow (hTab, show ? SW_SHOW : SW_HIDE);
	active = show;
}

// --------------------------------------------------------------

int TabPage::OnInitTab (WPARAM wParam)
{
	return FALSE;
}

// --------------------------------------------------------------

INT_PTR TabPage::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return OnInitTab (wParam);
	case WM_COMMAND:
		return OnCommand (wParam, lParam);
	}
	return FALSE;
}

// --------------------------------------------------------------

static INT_PTR CALLBACK TabProcHook (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	if (uMsg == WM_INITDIALOG) {
		EnableThemeDialogTexture (hWnd, ETDT_ENABLETAB);
		SetWindowLongPtr (hWnd, GWLP_USERDATA, lParam);
		((TabPage*)lParam)->hTab = hWnd;
	}
	TabPage *pTab = (TabPage*)GetWindowLongPtr (hWnd, GWLP_USERDATA);
	if (pTab) return pTab->DlgProc (hWnd, uMsg, wParam, lParam);
	else      return FALSE;
}
