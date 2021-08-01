// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER SDK Utility: TabDlg
//                  Part of the ORBITER SDK
//
// TabDlg.h
// A helper class for supporting tabs in dialog boxes
// ==============================================================

#ifndef __TABDLG_H
#define __TABDLG_H

#include "windows.h"

// ==============================================================
// class TabbedDialog: Dialog containing a single tab control
// ==============================================================

class TabbedDialog {
	friend class TabPage;

public:
	TabbedDialog (int _dlgId, int _tabId);
	// Create a tabbed dialog object, where _dlgId and _tabId are
	// the resource IDs of the dialog and tab control, respectively

	virtual ~TabbedDialog ();

	inline HWND DlgHandle() const { return hDlg; }
	inline int TabCount() const { return nTab; }
	inline TabPage *Tab (int i) const { return pTab[i]; }

	void Open (HINSTANCE hInstance, bool allowMulti=false);
	// Open the dialog

	void Close ();
	// Deallocate tabs and close the dialog

	virtual int OnInitDialog (WPARAM wParam);
	// Dialog initialisation. Add tabs here
	// Default behaviour: nothing, returns FALSE

	virtual int OnClose ();
	// Dialog close request
	// Default behaviour: calls Close, returns TRUE

	virtual int Closed ();
	// Invoked after dialog is closed
	// Default behaviour: nothing, returns TRUE

protected:
	int AddTab (TabPage *tab, const char *label);
	// Add a tab page to the control. Can only be called once the
	// dialog box has been created, usually in OnInit

	void SwitchTab ();
	// Respond to a change in tab selection by displaying the requested tab

	void ClearTabs ();
	// Delete all tabs

	virtual BOOL DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// dialog message callback

private:
	friend INT_PTR CALLBACK DlgProcHook (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// entry point for dialog message callback

	HWND hDlg;       // Dialog handle
	HINSTANCE hInst; // Module instance handle
	int dlgId;       // dialog resource id
	int tabId;       // resource id of tab control
	int nTab;        // number of tabs in the tab control
	TabPage **pTab;  // list of tabs
};

// ==============================================================
// class TabPage: Base class representing a tab in the control
// ==============================================================

class TabPage {
public:
	TabPage (TabbedDialog *frame, int _pageId);
	void Open ();
	virtual void Show (bool show);
	HWND TabHandle () const { return hTab; }
	inline TabbedDialog *Dlg() { return dlg; }

	virtual BOOL DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// tab message callback

	virtual void Update (double simt) {}

	virtual int OnInitTab (WPARAM wParam);
	// WM_INITDIALOG callback: Tab page initialisation
	// Default behaviour: nothing, returns FALSE

	virtual int OnCommand (WPARAM wParam, LPARAM lParam) { return FALSE; }
	// WM_COMMAND callback
	// Default behaviour: nothing, returns FALSE

private:
	friend INT_PTR CALLBACK TabProcHook (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	// entry point for tab message callback

	TabbedDialog *dlg;
	int pageId;
	HWND hTab;
	bool active;
};

#endif // !__TABDLG_H