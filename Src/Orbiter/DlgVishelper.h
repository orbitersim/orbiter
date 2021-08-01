// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Visual helper options dialog
// ======================================================================

#ifndef __DLGVISHELPER_H
#define __DLGVISHELPER_H

#include "DialogWin.h"

class VhelperTab;

// ======================================================================

class DlgVishelper: public DialogWin {
public:
	DlgVishelper (HINSTANCE hInstance, HWND hParent, void *context);
	~DlgVishelper ();
	void Update ();
	BOOL OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl);
	BOOL OnNotify (HWND hDlg, int idCtrl, LPNMHDR pnmh);

protected:
	int AddTab (HWND hDlg, VhelperTab *tab, const char *label);
	void SwitchTab (HWND hDlg);
	void Clear ();

private:
	int nTab;
	VhelperTab **pTab;
	char *hcontext;
};

// ======================================================================
// Base class for vishelper dialog tabs

class VhelperTab {
public:
	VhelperTab (HWND hParentTab, int dlgId, DLGPROC dlgProc);
	virtual char *HelpContext() const = 0;
	virtual void Update () {}
	void Show (bool show);

protected:
	static INT_PTR CALLBACK DlgProcInit (HWND, UINT, WPARAM, LPARAM);
	HWND hParent;
	HWND hTab;
	bool active;
};

// ======================================================================
// Planetarium tab

class TabPlanetarium: public VhelperTab {
public:
	TabPlanetarium (HWND hParentTab);
	char *HelpContext () const;
	void Update ();

protected:
	void Refresh (HWND hTab);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
};

// ======================================================================
// Forces tab

class TabForces: public VhelperTab {
public:
	TabForces (HWND hParentTab);
	char *HelpContext () const;
	void Update ();

protected:
	void Refresh (HWND hDlg, bool tick);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
};

// ======================================================================
// Axes tab

class TabAxes: public VhelperTab {
public:
	TabAxes (HWND hParentTab);
	char *HelpContext () const;
	void Update ();

protected:
	void Refresh (HWND hDlg, bool tick);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
};

// ======================================================================
// Custom label subdialog

class DlgCustomLabels: public DialogWin {
public:
	DlgCustomLabels (HINSTANCE hInstance, HWND hParent, void *context);
	BOOL OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl);

protected:
	void Refresh (HWND hDlg);
	void Select (HWND hDlg);
	void SelectAll (HWND hDlg, bool active);
};

// ======================================================================
// Custom celestial label subdialog

class DlgCustomCLabels: public DialogWin {
public:
	DlgCustomCLabels (HINSTANCE hInstance, HWND hParent, void *context);
	BOOL OnInitDialog (HWND hDlg, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand (HWND hDlg, WORD id, WORD code, HWND hControl);

protected:
	void Refresh (HWND hDlg);
	void Select (HWND hDlg);
};

#endif // !__DLGVISHELPER_H