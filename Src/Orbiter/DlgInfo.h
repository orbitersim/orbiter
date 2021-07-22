// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Object info window
// ======================================================================

#ifndef __DLGINFO_H
#define __DLGINFO_H

#include "DialogWin.h"
#include "DlgCtrl.h"

class Body;
class CelestialBody;
class Base;

// ======================================================================

class DlgInfo: public DialogWin {
public:
	DlgInfo (HINSTANCE hInstance, HWND hParent, void *context);
	void SetBody (Body *bd);

protected:
	void Init (HWND hDlg);
	void BuildObjectList (HWND hDlg, Body *b = NULL);
	int Size (DWORD width, DWORD height);
	void ExpandAll (HWND hDlg);
	void CollapseAll (HWND hDlg);
	void Update ();
	void SelectionChanged (HWND hDlg);
	void SetBody (HWND hDlg, Body *bd);
	void InitItems_vessel (HWND hDlg, const Vessel *vessel);
	void InitItems_celbody (HWND hDlg, const CelestialBody *cbody);
	void InitItems_base (HWND hDlg, const Base *base);
	void UpdateItems_vessel ();
	void UpdateItems_celbody ();
	void UpdateItems_base ();
	void OpenMap ();
	BOOL OnInitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL OnSize (HWND hWnd, WPARAM wParam, int w, int h);
	BOOL OnCommand (HWND hWnd, WORD id, WORD code, HWND hControl);

private:
	Body *body;
	int list_top, list_w, list_h;
	int client_w, client_h;
	PropertyList pl;
	double upd_t;
	double upd_dt;
	bool showth[3];
	HICON hIcon_dd, hIcon_du;

	enum ListMode { LIST_NONE, LIST_VESSEL, LIST_CBODY, LIST_BASE } listmode;

	struct VesselList {
		PropertyGroup *des;
		PropertyGroup *prm;
		PropertyGroup *thr;
		PropertyGroup *els;
		PropertyGroup *srf;
		PropertyGroup *atm;
		PropertyGroup *aer;
		PropertyGroup *dck;
		PropertyGroup *prp;
	} vlist;

	struct CBodyList {
		PropertyGroup *des;
		PropertyGroup *prm;
		PropertyGroup *atm;
		PropertyGroup *els;
		PropertyGroup *loc;
		PropertyGroup *ecl;
		PropertyGroup *prp;
	} cblist;

	struct BaseList {
		PropertyGroup *des;
		PropertyGroup *pad;
		PropertyGroup *rwy;
		PropertyGroup *vor;
	} blist;
};

#endif // !__DLGINFO_H