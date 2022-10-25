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
	BOOL OnCommand (HWND hDlg, WORD ctrlId, WORD notification, HWND hCtrl);
	BOOL OnNotify (HWND hDlg, int idCtrl, LPNMHDR pnmh);

protected:
	void AddTab (HWND hDlg, VhelperTab *tab, const char *label);
	void SwitchTab (HWND hDlg);
	void Clear ();

private:
	std::vector<VhelperTab*> m_pTab;
	char *hcontext;
};

// ======================================================================
// Base class for vishelper dialog tabs

class VhelperTab {
public:
	VhelperTab (HWND hParentTab);
	virtual ~VhelperTab();
	HWND Parent() const { return m_hParent; }
	HWND Tab() const { return m_hTab; }
	virtual void CreateInterface() = 0;
	virtual char *HelpContext() const = 0;
	virtual void Update () {}
	void Show (bool show);
	virtual INT_PTR DlgProc(HWND, UINT, WPARAM, LPARAM);

protected:
	void MakeTab(int dlgId);
	virtual BOOL OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam) { return TRUE; }
	virtual BOOL OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl) { return FALSE; }
	virtual BOOL OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam) { return FALSE; }
	virtual BOOL OnMessage(HWND hTab, UINT uMsg, WPARAM wParam, LPARAM lParam) { return FALSE; }

private:
	static INT_PTR CALLBACK s_DlgProc(HWND, UINT, WPARAM, LPARAM);
	HWND m_hParent;
	HWND m_hTab;
};

// ======================================================================
// Planetarium tab

class TabPlanetarium: public VhelperTab {
public:
	TabPlanetarium (HWND hParentTab);
	void CreateInterface();
	char *HelpContext () const;
	void Update ();

protected:
	BOOL OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl);
	BOOL OnMarkerSelectionChanged(HWND hTab);
	void OnItemClicked(HWND hTab, WORD ctrlId);
	void RescanMarkerList(HWND hTab);
};

// ======================================================================
// Markers/surface labels tab

class TabLabels : public VhelperTab {
public:
	TabLabels(HWND hParentTab);
	void CreateInterface();
	char* HelpContext() const;
	void Update();

protected:
	BOOL OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl);
	void OnItemClicked(HWND hTab, WORD ctrlId);
	void ScanPsysBodies(HWND hTab);
	void UpdateFeatureList(HWND hTab);
	void RescanFeatures(HWND hTab);
};

// ======================================================================
// Forces tab

class TabForces: public VhelperTab {
public:
	TabForces (HWND hParentTab);
	void CreateInterface();
	char *HelpContext () const;
	void Update ();

protected:
	BOOL OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl);
	BOOL OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam);
	void Refresh (HWND hDlg, bool tick);
};

// ======================================================================
// Axes tab

class TabAxes: public VhelperTab {
public:
	TabAxes (HWND hParentTab);
	void CreateInterface();
	char *HelpContext () const;
	void Update ();

protected:
	BOOL OnInitDialog(HWND hTab, WPARAM wParam, LPARAM lParam);
	BOOL OnCommand(HWND hTab, WORD ctrlId, WORD notification, HWND hCtrl);
	BOOL OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam);
	void Refresh (HWND hDlg, bool tick);
};

#endif // !__DLGVISHELPER_H