// ==============================================================
//              ORBITER MODULE: Scenario Editor
//                  Part of the ORBITER SDK
//            Copyright (C) 2006 Martin Schweiger
//                   All rights reserved
//
// Editor.h
//
// Interface definition for ScnEditor class and editor tab
// subclasses derived from ScnEditorTab.
// ==============================================================

#ifndef __SCNEDITOR_H
#define __SCNEDITOR_H

#include "ScnEditorAPI.h"
#include "Convert.h"
#include <commctrl.h>

class ScnEditorTab;
typedef void (*CustomButtonFunc)(OBJHANDLE);

// ==============================================================
// class ScnEditor
// ==============================================================

class ScnEditor {
public:
	ScnEditor (HINSTANCE hDLL);
	~ScnEditor ();
	void OpenDialog ();
	void CloseDialog ();
	void InitDialog (HWND hDlg);
	DWORD AddTab (ScnEditorTab *newTab);
	void DelCustomTabs ();
	void ShowTab (DWORD t);
	bool SaveScenario (HWND hDlg);
	int MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

	HWND DlgHandle () const { return hDlg; }
	HINSTANCE InstHandle () const { return hInst; }

	void ScanCBodyList (HWND hDlg, int hList, OBJHANDLE hSelect);
	void ScanPadList (HWND hDlg, int hList, OBJHANDLE hBase);
	void SetBasePosition (HWND hDlg);
	void SelectBase (HWND hDlg, int hList, OBJHANDLE hRef, OBJHANDLE hBase);
	bool CreateVessel (char *name, char *classname);
	void VesselDeleted (OBJHANDLE hV);
	void Pause (bool pause);
	char *ExtractVesselName (char *str);
	HINSTANCE LoadVesselLibrary (const VESSEL *vessel);

public:
	OBJHANDLE hVessel;   // vessel being edited
	HIMAGELIST imglist;  // image list for tree control icons
	int treeicon_idx[4]; // tree view icons

private:
	DWORD dwCmd;         // custom command handle
	DWORD nTab;          // total number of main dialog tabs
	DWORD nTab0;         // number of standard tabs (excluding custom)
	ScnEditorTab **pTab; // array of tab instances
	ScnEditorTab *cTab;  // currently displayed tab
	HWND  hDlg;          // main dialog handle
	HINSTANCE hInst;     // module instance handle
	HINSTANCE hEdLib;    // vessel editor library instance handle
};


// ==============================================================
// class ScnEditorTab
// ==============================================================

class ScnEditorTab {
public:
	ScnEditorTab (ScnEditor *editor);
	virtual ~ScnEditorTab ();
	ScnEditor *Editor() { return ed; }
	VESSEL *Vessel() { return oapiGetVesselInterface (ed->hVessel); }
	HWND CreateTab (HINSTANCE hInst, WORD ResId, DLGPROC TabProc);
	HWND CreateTab (WORD ResId, DLGPROC TabProc);
	void DestroyTab ();
	virtual void InitTab () {}
	HWND TabHandle () const { return hTab; }
	void SwitchTab (int newtab);
	virtual char *HelpTopic ();
	virtual void OpenHelp ();
	void Show ();
	void Hide ();
	virtual BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static ScnEditorTab *TabPointer (HWND, UINT, WPARAM, LPARAM);

protected:
	void ScanVesselList (int ResId, bool detail = false, OBJHANDLE hExclude = NULL);
	OBJHANDLE GetVesselFromList (int ResId);

	ScnEditor *ed;        // associated editor
	HWND hTab;            // tab window handle
};


// ==============================================================
// class EditorTab_Vessel
// ==============================================================

class EditorTab_Vessel: public ScnEditorTab {
public:
	EditorTab_Vessel (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	void SelectVessel (OBJHANDLE hV);
	void VesselSelected ();
	void VesselDeleted (OBJHANDLE hV);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
	
protected:
	void ScanVesselList ();
	bool DeleteVessel ();
};


// ==============================================================
// class EditorTab_New
// ==============================================================

class EditorTab_New: public ScnEditorTab {
public:
	EditorTab_New (ScnEditor *editor);
	~EditorTab_New ();
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	void ScanConfigDir (const char *ppath, HTREEITEM hti);
	void RefreshVesselTpList ();
	int GetSelVesselTp (char *name, int len);
	void VesselTpChanged ();
	bool CreateVessel ();
	bool UpdateVesselBmp ();
	void DrawVesselBmp ();

private:
	HBITMAP hVesselBmp;
	int imghmax;
};


// ==============================================================
// class EditorTab_Edit
// ==============================================================

class EditorTab_Edit: public ScnEditorTab {
public:
	EditorTab_Edit (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL AddFuncButton (EditorFuncSpec *efs);
	BOOL AddPageButton (EditorPageSpec *eps);
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

private:
	OBJHANDLE hVessel;
	int nCustom; // number of custom buttons
	CustomButtonFunc funcCustom[6];
	int CustomPage[6];
};


// ==============================================================
// class EditorTab_Save
// ==============================================================

class EditorTab_Save: public ScnEditorTab {
public:
	EditorTab_Save (ScnEditor *editor);
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
};


// ==============================================================
// class EditorTab_Date
// ==============================================================

class EditorTab_Date: public ScnEditorTab {
public:
	EditorTab_Date (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	void Apply ();
	void Refresh ();
	void UpdateDateTime ();
	void UpdateMJD (void);
	void UpdateJD (void);
	void UpdateJC (void);
	void UpdateEpoch (void);
	void SetUT (struct tm *new_date, bool reset_ut = false);
	void SetMJD (double new_mjd, bool reset_mjd = false);
	void SetJD (double new_jd, bool reset_jd = false);
	void SetJC (double new_jc, bool reset_jc = false);
	void SetEpoch (double new_epoch, bool reset_epoch = false);
	void OnChangeDateTime ();
	void OnChangeMjd ();
	void OnChangeJd ();
	void OnChangeJc ();
	void OnChangeEpoch ();

private:
	double mjd;
	struct tm date;
	bool bIgnore;
};


// ==============================================================
// class EditorTab_Elements
// ==============================================================

class EditorTab_Elements: public ScnEditorTab {
public:
	EditorTab_Elements (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	void Apply ();
	void Refresh ();
	void RefreshSecondaryParams (const ELEMENTS &el, const ORBITPARAM &prm);

private:
	ELEMENTS el;       // orbital elements of edited vessel
	ORBITPARAM prm;    // additional orbital parameters
	double elmjd;      // element epoch
};


// ==============================================================
// class EditorTab_Statevec
// ==============================================================

class EditorTab_Statevec: public ScnEditorTab {
public:
	EditorTab_Statevec (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	void ScanVesselList ();
	void DlgLabels ();
	void Refresh (OBJHANDLE hV = NULL);
	void Apply ();
};


// ==============================================================
// class EditorTab_Landed
// ==============================================================

class EditorTab_Landed: public ScnEditorTab {
public:
	EditorTab_Landed (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	void ScanVesselList ();
	void ScanCBodyList (HWND hDlg, int hList, OBJHANDLE hSelect);
	void ScanBaseList (HWND hDlg, int hList, OBJHANDLE hRef);
	void SelectCBody (OBJHANDLE hBody);
	void Refresh (OBJHANDLE hV = NULL);
	void Apply ();
};


// ==============================================================
// class EditorTab_Orientation
// ==============================================================

class EditorTab_Orientation: public ScnEditorTab {
public:
	EditorTab_Orientation (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	void Refresh ();
	void Apply ();
	void ApplyAngularVel ();
	void Rotate (int axis, double da);
};

// ==============================================================
// class EditorTab_AngularVel
// ==============================================================

class EditorTab_AngularVel: public ScnEditorTab {
public:
	EditorTab_AngularVel (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	void Refresh ();
	void Apply ();
	void Killrot ();
};

// ==============================================================
// class EditorTab_Propellant
// ==============================================================

class EditorTab_Propellant: public ScnEditorTab {
public:
	EditorTab_Propellant (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	void Refresh ();
	void RefreshTotals ();
	void Apply ();
	void SetLevel (double level, bool setall = false);

private:
	DWORD ntank;
	int lastedit;
};

// ==============================================================
// class EditorTab_Docking
// ==============================================================

class EditorTab_Docking: public ScnEditorTab {
public:
	EditorTab_Docking (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

protected:
	UINT DockNo ();
	void ScanTargetList ();
	void SetTargetDock (DWORD dock);
	void ToggleIDS ();
	void IncIDSChannel (int dch);
	void Dock ();
	void Undock ();
	void Refresh ();
	void DisplayErrorMsg (UINT err);
};

// ==============================================================
// class EditorTab_Custom
// ==============================================================

class EditorTab_Custom: public ScnEditorTab {
public:
	EditorTab_Custom (ScnEditor *editor, HINSTANCE hInst, WORD ResId, DLGPROC UserProc);
	void OpenHelp ();
	BOOL TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

private:
	DLGPROC usrProc;
};

#endif // !__SCNEDITOR_H
