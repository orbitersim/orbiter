// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//              ORBITER MODULE: Scenario Editor
//                  Part of the ORBITER SDK
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
#ifndef __linux__
#include <commctrl.h>
#else // __linux__
// commctrl.h left out: the common controls are Qt widgets
#include <QPixmap>
#include <QPointer>
#include <vector>
#endif // __linux__
#include <filesystem>
namespace fs = std::filesystem;

class ScnEditorTab;
#ifdef __linux__
class QTreeWidgetItem;
#endif // __linux__
typedef void (*CustomButtonFunc)(OBJHANDLE);

// ==============================================================
// class ScnEditor
// ==============================================================

class ScnEditor {
public:
#ifndef __linux__
	ScnEditor (HINSTANCE hDLL);
#else // __linux__
	ScnEditor (void *hDLL);
#endif // __linux__
	~ScnEditor ();
	void OpenDialog ();
	void CloseDialog ();
#ifndef __linux__
	void InitDialog (HWND hDlg);
#else // __linux__
	void InitDialog (QWidget *hDlg);
#endif // __linux__
	DWORD AddTab (ScnEditorTab *newTab);
	void DelCustomTabs ();
	void ShowTab (DWORD t);
#ifndef __linux__
	bool SaveScenario (HWND hDlg);
	INT_PTR MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
	bool SaveScenario (QWidget *hDlg);
	void MsgProc (QWidget *hDlg);
#endif // __linux__

#ifndef __linux__
	HWND DlgHandle () const { return hDlg; }
	HINSTANCE InstHandle () const { return hInst; }
#else // __linux__
	QWidget *DlgHandle () const { return hDlg; }
	void *InstHandle () const { return hInst; }
#endif // __linux__

#ifndef __linux__
	void ScanCBodyList (HWND hDlg, int hList, OBJHANDLE hSelect);
	void ScanPadList (HWND hDlg, int hList, OBJHANDLE hBase);
	void SetBasePosition (HWND hDlg);
	void SelectBase (HWND hDlg, int hList, OBJHANDLE hRef, OBJHANDLE hBase);
#else // __linux__
	void ScanCBodyList (QWidget *hDlg, int hList, OBJHANDLE hSelect);
	void ScanPadList (QWidget *hDlg, int hList, OBJHANDLE hBase);
	void SetBasePosition (QWidget *hDlg);
	void SelectBase (QWidget *hDlg, int hList, OBJHANDLE hRef, OBJHANDLE hBase);
#endif // __linux__
	bool CreateVessel (char *name, char *classname);
	void VesselDeleted (OBJHANDLE hV);
	void Pause (bool pause);
	char *ExtractVesselName (char *str);
#ifndef __linux__
	HINSTANCE LoadVesselLibrary (const VESSEL *vessel);
#else // __linux__
	void *LoadVesselLibrary (const VESSEL *vessel);
#endif // __linux__

public:
	OBJHANDLE hVessel;   // vessel being edited
#ifndef __linux__
	HIMAGELIST imglist;  // image list for tree control icons
#else // __linux__
	std::vector<QPixmap> imglist; // image list for tree control icons
#endif // __linux__
	int treeicon_idx[4]; // tree view icons

private:
	DWORD dwCmd;         // custom command handle
	int dwMenuCmd;       // custom menu command handle
	DWORD nTab;          // total number of main dialog tabs
	DWORD nTab0;         // number of standard tabs (excluding custom)
	ScnEditorTab **pTab; // array of tab instances
	ScnEditorTab *cTab;  // currently displayed tab
#ifndef __linux__
	HWND  hDlg;          // main dialog handle
	HINSTANCE hInst;     // module instance handle
	HINSTANCE hEdLib;    // vessel editor library instance handle
#else // __linux__
	QPointer<QWidget> hDlg; // main dialog handle (QPointer: the core destroys the dialog at session end)
	void *hInst;         // module instance handle
	void *hEdLib;        // vessel editor library instance handle
#endif // __linux__
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
#ifndef __linux__
	HWND CreateTab (HINSTANCE hInst, WORD ResId, DLGPROC TabProc);
	HWND CreateTab (WORD ResId, DLGPROC TabProc);
#else // __linux__
	QWidget *CreateTab (void *hInst, WORD ResId, DLGINIT TabProc);
	QWidget *CreateTab (WORD ResId, DLGINIT TabProc);
#endif // __linux__
	void DestroyTab ();
	virtual void InitTab () {}
#ifndef __linux__
	HWND TabHandle () const { return hTab; }
#else // __linux__
	QWidget *TabHandle () const { return hTab; }
#endif // __linux__
	void SwitchTab (int newtab);
	virtual char *HelpTopic ();
	virtual void OpenHelp ();
	void Show ();
	void Hide ();
#ifndef __linux__
	virtual INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static ScnEditorTab *TabPointer (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	virtual void TabProc (QWidget *hDlg);
	static ScnEditorTab *TabPointer (QWidget *hDlg, void *context = 0);
#endif // __linux__

protected:
	void ScanVesselList (int ResId, bool detail = false, OBJHANDLE hExclude = NULL);
	OBJHANDLE GetVesselFromList (int ResId);

	ScnEditor *ed;        // associated editor
#ifndef __linux__
	HWND hTab;            // tab window handle
#else // __linux__
	QPointer<QWidget> hTab; // tab window handle (QPointer: the core destroys the dialog at session end)
#endif // __linux__
};


// ==============================================================
// class EditorTab_Vessel
// ==============================================================

class EditorTab_Vessel: public ScnEditorTab {
public:
	EditorTab_Vessel (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
	void TabProc (QWidget *hDlg);
#endif // __linux__
	void SelectVessel (OBJHANDLE hV);
	void VesselSelected ();
	void VesselDeleted (OBJHANDLE hV);
#ifndef __linux__
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	static void DlgProc (QWidget*, void*);
#endif // __linux__
	
protected:
	void ScanVesselList ();
	bool DeleteVessel ();
	bool CanDelete (OBJHANDLE hVessel);
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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

protected:
#ifndef __linux__
	void ScanConfigDir (const fs::path &dir, HTREEITEM hti);
#else // __linux__
	void ScanConfigDir (const fs::path &dir, QTreeWidgetItem *hti);
#endif // __linux__
	void RefreshVesselTpList ();
	int GetSelVesselTp (char *name, int len);
	void VesselTpChanged ();
	bool CreateVessel ();
	bool UpdateVesselBmp ();
	void DrawVesselBmp ();

private:
#ifndef __linux__
	HBITMAP hVesselBmp;
#else // __linux__
	QImage *hVesselBmp;
#endif // __linux__
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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__
};


// ==============================================================
// class EditorTab_Date
// ==============================================================

class EditorTab_Date: public ScnEditorTab {
public:
	EditorTab_Date (ScnEditor *editor);
	void InitTab ();
	char *HelpTopic ();
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

protected:
	void ScanVesselList ();
#ifndef __linux__
	void ScanCBodyList (HWND hDlg, int hList, OBJHANDLE hSelect);
	void ScanBaseList (HWND hDlg, int hList, OBJHANDLE hRef);
#else // __linux__
	void ScanCBodyList (QWidget *hDlg, int hList, OBJHANDLE hSelect);
	void ScanBaseList (QWidget *hDlg, int hList, OBJHANDLE hRef);
#endif // __linux__
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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

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
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

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
#ifndef __linux__
	EditorTab_Custom (ScnEditor *editor, HINSTANCE hInst, WORD ResId, DLGPROC UserProc);
#else // __linux__
	EditorTab_Custom (ScnEditor *editor, void *hInst, WORD ResId, DLGINIT UserProc);
#endif // __linux__
	void OpenHelp ();
#ifndef __linux__
	INT_PTR TabProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);
#else // __linux__
	void TabProc (QWidget *hDlg);
	static void DlgProc (QWidget*, void*);
#endif // __linux__

private:
#ifndef __linux__
	DLGPROC usrProc;
#else // __linux__
	DLGINIT usrProc;
#endif // __linux__
};

#endif // !__SCNEDITOR_H
