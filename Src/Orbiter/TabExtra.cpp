// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ExtraTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#include <windows.h>
#include <commctrl.h>
#include <winuser.h>
#include "Launchpad.h"
#include "TabExtra.h"
#include "Orbiter.h"
#include "Rigidbody.h"
#include "Log.h"
#include "Help.h"
#include "resource.h"
#include "resource2.h"

using std::max;

extern Orbiter *g_pOrbiter;

//-----------------------------------------------------------------------------
// ExtraTab class

orbiter::ExtraTab::ExtraTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
	m_internalPrm = 0;
}

//-----------------------------------------------------------------------------

orbiter::ExtraTab::~ExtraTab ()
{
	// at this point, only the internally created entries should be left
	// so they should be safe to delete
	if (m_ExtPrm.size() > m_internalPrm)
		LOGOUT_WARN("Orphaned Launchpad Extra entries: %d. Some plugins may not have un-registered their entries.",
			m_ExtPrm.size() - m_internalPrm);
	for (int i = 0; i < m_ExtPrm.size(); i++)
		delete m_ExtPrm[i];
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_EXT);

	r_lst0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_EXT_LIST));  // REMOVE!
	r_dsc0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_EXT_TEXT));  // REMOVE!
	r_pane  = GetClientPos (hTab, GetDlgItem (hTab, IDC_EXT_SPLIT1));
	r_edit0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_EXT_OPEN));
	splitListDesc.SetHwnd (GetDlgItem (hTab, IDC_EXT_SPLIT1), GetDlgItem (hTab, IDC_EXT_LIST), GetDlgItem (hTab, IDC_EXT_TEXT));
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::GetConfig (const Config *cfg)
{
	HTREEITEM ht;
	ht = RegisterExtraParam(new ExtraPropagation(this), NULL); TRACENEW
	RegisterExtraParam(new ExtraDynamics(this), ht); TRACENEW
	RegisterExtraParam(new ExtraStabilisation(this), ht); TRACENEW
	ht = RegisterExtraParam(new ExtraInstruments(this), NULL); TRACENEW
	RegisterExtraParam(new ExtraMfdConfig(this), ht); TRACENEW
	RegisterExtraParam(new ExtraVesselConfig(this), NULL); TRACENEW
	RegisterExtraParam(new ExtraPlanetConfig(this), NULL); TRACENEW
	ht = RegisterExtraParam(new ExtraDebug(this), NULL); TRACENEW
	RegisterExtraParam(new ExtraShutdown(this), ht); TRACENEW
	RegisterExtraParam(new ExtraFixedStep(this), ht); TRACENEW
	RegisterExtraParam(new ExtraRenderingOptions(this), ht); TRACENEW
	RegisterExtraParam(new ExtraTimerSettings(this), ht); TRACENEW
	RegisterExtraParam(new ExtraLaunchpadOptions(this), ht); TRACENEW
	RegisterExtraParam(new ExtraLogfileOptions(this), ht); TRACENEW
	RegisterExtraParam(new ExtraPerformanceSettings(this), ht); TRACENEW
	m_internalPrm = m_ExtPrm.size();
	SetWindowText (GetDlgItem (hTab, IDC_EXT_TEXT), "Advanced and addon-specific configuration parameters.\r\n\r\nClick on an item to get a description.\r\n\r\nDouble-click to open or expand.");
	int listw = cfg->CfgWindowPos.LaunchpadExtListWidth;
	if (!listw) {
		RECT r;
		GetClientRect (GetDlgItem (hTab, IDC_EXT_LIST), &r);
		listw = r.right;
	}
	splitListDesc.SetStaticPane (SplitterCtrl::PANE1, listw);
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::SetConfig (Config *cfg)
{
	cfg->CfgWindowPos.LaunchpadExtListWidth = splitListDesc.GetPaneWidth (SplitterCtrl::PANE1);
}

//-----------------------------------------------------------------------------

bool orbiter::ExtraTab::OpenHelp ()
{
	OpenTabHelp ("tab_extra");
	return true;
}

//-----------------------------------------------------------------------------

BOOL orbiter::ExtraTab::OnSize (int w, int h)
{
	int dw = w - (int)(pos0.right-pos0.left);
	int dh = h - (int)(pos0.bottom-pos0.top);
	int w0 = r_pane.right - r_pane.left; // initial splitter pane width
	int h0 = r_pane.bottom - r_pane.top; // initial splitter pane height

	// the elements below may need updating
	int lstw0 = r_lst0.right-r_lst0.left;
	int lsth0 = r_lst0.bottom-r_lst0.top;
	int dscw0 = r_dsc0.right-r_dsc0.left;
	int wg  = r_dsc0.right - r_lst0.left - lstw0 - dscw0;  // gap width
	int wl  = lstw0 + (dw*lstw0)/(lstw0+dscw0);
	wl = max (wl, lstw0/2);
	int xr = r_lst0.left+wl+wg;
	int wr = max(10,lstw0+dscw0+dw-wl);

	///SetWindowPos (GetDlgItem (hTab, IDC_EXT_LIST), NULL,
	//	0, 0, wl, lsth0+dh,
	//	SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	//SetWindowPos (GetDlgItem (hTab, IDC_EXT_TEXT), NULL,
	//	xr, r_dsc0.top, wr, lsth0+dh,
	//	SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_EXT_SPLIT1), NULL,
		0, 0, w0+dw, h0+dh,
		SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_EXT_OPEN), NULL,
		r_edit0.left, r_edit0.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);

	return NULL;
}

//-----------------------------------------------------------------------------

HTREEITEM orbiter::ExtraTab::RegisterExtraParam (LaunchpadItem *item, HTREEITEM parent)
{
	// first check that the item doesn't already exist
	HTREEITEM hti = FindExtraParam (item->Name(), parent);
	if (hti) return hti;

	// add extra parameter instance to list
	m_ExtPrm.push_back(item);

	// if a name is provided, add item to tree list
	char *name = item->Name();
	if (name) {
		TV_INSERTSTRUCT tvis;
		tvis.item.mask = TVIF_TEXT | TVIF_PARAM;
		tvis.item.pszText = name;
		tvis.item.lParam = (LPARAM)item;
		tvis.hInsertAfter = TVI_LAST;
		tvis.hParent = (parent ? parent : NULL);
		hti = TreeView_InsertItem (GetDlgItem (hTab, IDC_EXT_LIST), &tvis);
	} else hti = 0;
	item->hItem = (LAUNCHPADITEM_HANDLE)hti;
	return hti;
}

//-----------------------------------------------------------------------------

bool orbiter::ExtraTab::UnregisterExtraParam (LaunchpadItem *item)
{
	for (auto it = m_ExtPrm.begin(); it != m_ExtPrm.end(); it++) {
		if (*it == item) {
			TreeView_DeleteItem(GetDlgItem(hTab, IDC_EXT_LIST), item->hItem); // remove entry from UI
			item->clbkWriteConfig(); // allow item to save state before removing
			m_ExtPrm.erase(it);        // delete the container - the actual item has to be deleted by the caller
			return true;
		}
	}
	return false;
}

//-----------------------------------------------------------------------------

HTREEITEM orbiter::ExtraTab::FindExtraParam (const char *name, const HTREEITEM parent)
{
	HTREEITEM hti = FindExtraParamChild (parent);
	if (!name) return hti; // no name given - return first child

	char cbuf[256];
	HWND hCtrl = GetDlgItem (hTab, IDC_EXT_LIST);
	TV_ITEM tvi;
	tvi.pszText = cbuf;
	tvi.cchTextMax = 256;
	tvi.hItem = hti;
	tvi.mask = TVIF_HANDLE | TVIF_TEXT;
	
	// step through the list
	while (TreeView_GetItem (hCtrl, &tvi)) {
		if (!_stricmp (name, tvi.pszText)) return tvi.hItem;
		tvi.hItem = TreeView_GetNextSibling (hCtrl, tvi.hItem);
	}

	return 0;
}

//-----------------------------------------------------------------------------

HTREEITEM orbiter::ExtraTab::FindExtraParamChild (const HTREEITEM parent)
{
	HWND hCtrl = GetDlgItem (hTab, IDC_EXT_LIST);
	if (parent) return TreeView_GetChild (hCtrl, parent);
	else        return TreeView_GetRoot (hCtrl);
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::WriteExtraParams ()
{
	for (auto it = m_ExtPrm.begin(); it != m_ExtPrm.end(); it++)
		(*it)->clbkWriteConfig();
}

//-----------------------------------------------------------------------------

BOOL orbiter::ExtraTab::OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh)
{
	if (idCtrl == IDC_EXT_LIST) {
		NM_TREEVIEW* pnmtv = (NM_TREEVIEW FAR*)pnmh;
		switch (pnmtv->hdr.code) {
		case TVN_SELCHANGED: {
			LaunchpadItem* func = (LaunchpadItem*)pnmtv->itemNew.lParam;
			char* desc = func->Description();
			if (desc) SetWindowText(GetDlgItem(hDlg, IDC_EXT_TEXT), desc);
			else SetWindowText(GetDlgItem(hDlg, IDC_EXT_TEXT), "");
			} return TRUE;
		case NM_DBLCLK: {
			TVITEM tvi;
			tvi.hItem = TreeView_GetSelection(GetDlgItem(hDlg, IDC_EXT_LIST));
			tvi.mask = TVIF_PARAM;
			if (TreeView_GetItem(GetDlgItem(hDlg, IDC_EXT_LIST), &tvi) && tvi.lParam) {
				BuiltinLaunchpadItem* func = (BuiltinLaunchpadItem*)tvi.lParam;
				func->clbkOpen(LaunchpadWnd());
			}
			} return TRUE;
		}
	}
	return FALSE;
}

//-----------------------------------------------------------------------------

BOOL orbiter::ExtraTab::OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	NM_TREEVIEW *pnmtv;

	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_EXT_OPEN: {
			TVITEM tvi;
			tvi.hItem = TreeView_GetSelection (GetDlgItem (hWnd, IDC_EXT_LIST));
			tvi.mask = TVIF_PARAM;
			if (TreeView_GetItem (GetDlgItem (hWnd, IDC_EXT_LIST), &tvi) && tvi.lParam) {
				BuiltinLaunchpadItem *func = (BuiltinLaunchpadItem*)tvi.lParam;
				func->clbkOpen (LaunchpadWnd());
			}
			} return TRUE;
		}
		break;
	}
	return FALSE;
}


// ****************************************************************************
// ****************************************************************************

//-----------------------------------------------------------------------------
// Additional functions (under the "Extra" tab)
//-----------------------------------------------------------------------------

BuiltinLaunchpadItem::BuiltinLaunchpadItem (const orbiter::ExtraTab *tab): LaunchpadItem ()
{
	pTab = tab;
}

bool BuiltinLaunchpadItem::OpenDialog (HWND hParent, int resid, DLGPROC pDlg)
{
	return LaunchpadItem::OpenDialog (pTab->AppInstance(), hParent, resid, pDlg);
}

void BuiltinLaunchpadItem::Error (const char *msg)
{
	MessageBox (pTab->LaunchpadWnd(), msg, "Orbiter configuration error", MB_OK|MB_ICONERROR);
}

INT_PTR CALLBACK BuiltinLaunchpadItem::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		SetWindowLongPtr (hWnd, DWLP_USER, lParam);
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDCANCEL:
			EndDialog (hWnd, 0);
		}
		break;
	case WM_CLOSE:
		EndDialog (hWnd, 0);
		return 0;
	}
	return FALSE;
}

//-----------------------------------------------------------------------------
// Physics engine
//-----------------------------------------------------------------------------

char *ExtraPropagation::Name ()
{
	return (char*)"Time propagation";
}

char *ExtraPropagation::Description ()
{
	return (char*)"Select and configure the time propagation methods Orbiter uses to update vessel positions and velocities from one time frame to the next.";
}

//-----------------------------------------------------------------------------
// Physics engine: Parameters for dynamic state propagation
//-----------------------------------------------------------------------------

int ExtraDynamics::PropId[NPROP_METHOD] = {
	PROP_RK2, PROP_RK4, PROP_RK5, PROP_RK6, PROP_RK7, PROP_RK8,
	PROP_SY2, PROP_SY4, PROP_SY6, PROP_SY8
};

char *ExtraDynamics::Name ()
{
	return (char*)"Dynamic state propagators";
}

char *ExtraDynamics::Description ()
{
	return (char*)"Select the numerical integration methods used for dynamic state updates.\r\n\r\nState propagators affect the accuracy and stability of spacecraft orbits and trajectory calculations.";
}

bool ExtraDynamics::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_DYNAMICS, DlgProc);
	return true;
}

void ExtraDynamics::InitDialog (HWND hWnd)
{
	DWORD i, j;
	for (i = 0; i < 5; i++) {
		SendDlgItemMessage (hWnd, IDC_PROP_PROP0+i, CB_RESETCONTENT, 0, 0);
		for (j = 0; j < NPROP_METHOD; j++)
			SendDlgItemMessage (hWnd, IDC_PROP_PROP0+i, CB_ADDSTRING, 0, (LPARAM)RigidBody::PropagatorStr(j));
	}
	SetDialog (hWnd, pTab->Cfg()->CfgPhysicsPrm);
}

void ExtraDynamics::ResetDialog (HWND hWnd)
{
	extern CFG_PHYSICSPRM CfgPhysicsPrm_default;
	SetDialog (hWnd, CfgPhysicsPrm_default);
}

void ExtraDynamics::SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm)
{
	char cbuf[64];
	int i, j;
	int n = prm.nLPropLevel;
	for (i = 0; i < 5; i++) {
		SendDlgItemMessage (hWnd, IDC_PROP_ACTIVE0+i, BM_SETCHECK, i < n ? BST_CHECKED : BST_UNCHECKED, 0);
		EnableWindow (GetDlgItem (hWnd, IDC_PROP_ACTIVE0+i), i < n-1 || i > n || i == 0 ? FALSE : TRUE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_PROP0+i), i < n ? SW_SHOW : SW_HIDE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), i < n ? SW_SHOW : SW_HIDE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), i < n ? SW_SHOW : SW_HIDE);
		if (i < 4) {
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i), i < n-1 ? SW_SHOW : SW_HIDE);
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i), i < n-1 ? SW_SHOW : SW_HIDE);
		}
		if (i < n) {
			int id = prm.PropMode[i];
			for (j = 0; j < NPROP_METHOD; j++)
				if (id == PropId[j]) {
					SendDlgItemMessage (hWnd, IDC_PROP_PROP0+i, CB_SETCURSEL, j, 0);
					break;
				}
			sprintf (cbuf, "%0.2f", prm.PropTTgt[i]);
			SetWindowText (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), cbuf);
			sprintf (cbuf, "%0.1f", prm.PropATgt[i]*DEG);
			SetWindowText (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), cbuf);
			if (i < n-1) {
				sprintf (cbuf, "%0.2f", prm.PropTLim[i]);
				SetWindowText (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i), cbuf);
				sprintf (cbuf, "%0.1f", prm.PropALim[i]*DEG);
				SetWindowText (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i), cbuf);
			}
		}
	}
	sprintf (cbuf, "%d", prm.PropSubMax);
	SetWindowText (GetDlgItem (hWnd, IDC_PROP_MAXSAMPLE), cbuf);
}

void ExtraDynamics::Activate (HWND hWnd, int which)
{
	int i = which-IDC_PROP_ACTIVE0;
	int check = SendDlgItemMessage (hWnd, which, BM_GETCHECK, 0, 0);
	if (check == BST_CHECKED) {
		if (i < 4) EnableWindow (GetDlgItem (hWnd, which+1), TRUE);
		if (i > 0) EnableWindow (GetDlgItem (hWnd, which-1), FALSE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_PROP0+i), SW_SHOW);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), SW_SHOW);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), SW_SHOW);
		if (i > 0) {
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i-1), SW_SHOW);
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i-1), SW_SHOW);
		}
	} else {
		if (i > 1) EnableWindow (GetDlgItem (hWnd, which-1), TRUE);
		if (i < 4) EnableWindow (GetDlgItem (hWnd, which+1), FALSE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_PROP0+i), SW_HIDE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), SW_HIDE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), SW_HIDE);
		if (i > 0) {
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i-1), SW_HIDE);
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i-1), SW_HIDE);
		}
	}
}

bool ExtraDynamics::StoreParams (HWND hWnd)
{
	char cbuf[256];
	int i, n = 0;
	double ttgt[MAX_PROP_LEVEL], atgt[MAX_PROP_LEVEL], tlim[MAX_PROP_LEVEL], alim[MAX_PROP_LEVEL];
	int mode[MAX_PROP_LEVEL];
	for (i = 0; i < MAX_PROP_LEVEL; i++) {
		if (SendDlgItemMessage (hWnd, IDC_PROP_ACTIVE0+i, BM_GETCHECK, 0, 0) == BST_CHECKED)
			n++;
	}
	for (i = 0; i < n; i++) {
		mode[i] = SendDlgItemMessage (hWnd, IDC_PROP_PROP0+i, CB_GETCURSEL, 0, 0);
		if (mode[i] == CB_ERR) {
			sprintf (cbuf, "Invalid propagator for integration stage %d.", i+1);
			Error (cbuf);
			return false;
		}
		GetWindowText (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), cbuf, 256);
		if ((sscanf (cbuf, "%lf", ttgt+i) != 1) || (ttgt[i] <= 0)) {
			sprintf (cbuf, "Invalid time step target for integration stage %d.", i+1);
			Error (cbuf);
			return false;
		}
		GetWindowText (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), cbuf, 256);
		if ((sscanf (cbuf, "%lf", atgt+i) != 1) || (atgt[i] <= 0)) {
			sprintf (cbuf, "Invalid angle step target for integration stage %d.", i+1);
			Error (cbuf);
			return false;
		}
		if (i < n-1) {
			GetWindowText (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i), cbuf, 256);
			if ((sscanf (cbuf, "%lf", tlim+i) != 1) || (tlim[i] <= 0)) {
				sprintf (cbuf, "Invalid time step limit for integration stage %d -> %d.", i+1, i+2);
				Error (cbuf);
				return false;
			}
			GetWindowText (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i), cbuf, 256);
			if ((sscanf (cbuf, "%lf", alim+i) != 1) || (alim[i] <= 0)) {
				sprintf (cbuf, "Invalid angle step limit for integration stage %d -> %d.", i+1, i+2);
				Error (cbuf);
				return false;
			}
			if (i > 0 && (tlim[i] <= tlim[i-1] || alim[i] <= alim[i-1])) {
				Error ("Step limits must be in ascending order");
				return false;
			}
		}
	}

	Config *cfg = pTab->Cfg();
	cfg->CfgPhysicsPrm.nLPropLevel = n;
	for (i = 0; i < n; i++) {
		cfg->CfgPhysicsPrm.PropMode[i] = PropId[mode[i]];
		cfg->CfgPhysicsPrm.PropTTgt[i] = ttgt[i];
		cfg->CfgPhysicsPrm.PropATgt[i] = atgt[i]*RAD;
		cfg->CfgPhysicsPrm.PropTLim[i] = (i < n-1 ? tlim[i] : 1e10);
		cfg->CfgPhysicsPrm.PropALim[i] = (i < n-1 ? alim[i]*RAD : 1e10);
	}

	GetWindowText (GetDlgItem (hWnd, IDC_PROP_MAXSAMPLE), cbuf, 256);
	if ((sscanf (cbuf, "%d", &i) != 1) || i < 1) {
		Error ("Invalid value for max. subsamples (integer value >= 1 required).");
		return false;
	} else {
		cfg->CfgPhysicsPrm.PropSubMax = i;
	}

	return true;
}

bool ExtraDynamics::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, "extra_linprop");
	return true;
}

INT_PTR CALLBACK ExtraDynamics::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraDynamics*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_PROP_ACTIVE0:
		case IDC_PROP_ACTIVE1:
		case IDC_PROP_ACTIVE2:
		case IDC_PROP_ACTIVE3:
		case IDC_PROP_ACTIVE4:
			((ExtraDynamics*)GetWindowLongPtr (hWnd, DWLP_USER))->Activate (hWnd, LOWORD(wParam));
			break;
		case IDC_RESET:
			((ExtraDynamics*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDCHELP:
			((ExtraDynamics*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((ExtraDynamics*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}

//-----------------------------------------------------------------------------
// Physics engine: Parameters for angular state propagation
//-----------------------------------------------------------------------------
#ifdef UNDEF

int ExtraAngDynamics::PropId[NAPROP_METHOD] = {
	PROP_RK2, PROP_RK4, PROP_RK5, PROP_RK6, PROP_RK7, PROP_RK8
};

char *ExtraAngDynamics::Name ()
{
	return "Angular state propagators";
}

char *ExtraAngDynamics::Description ()
{
	static char *desc = "Select the numerical integration method for dynamic angular state updates.\r\n\r\nAngular propagators affect the simulation accuracy and stability of rotating vessels.";
	return desc;
}

bool ExtraAngDynamics::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_ADYNAMICS, DlgProc);
	return true;
}

void ExtraAngDynamics::InitDialog (HWND hWnd)
{
	static char *label[NAPROP_METHOD] = {
		"Runge-Kutta, 2nd order (RK2)", "Runge-Kutta, 4th order (RK4)", "Runge-Kutta, 5th order (RK5)",
		"Runge-Kutta, 6th order (RK6)", "Runge-Kutta, 7th order (RK7)", "Runge-Kutta, 8th order (RK8)"
	};

	int i, j;
	for (i = 0; i < 5; i++) {
		SendDlgItemMessage (hWnd, IDC_COMBO1+i, CB_RESETCONTENT, 0, 0);
		for (j = 0; j < NAPROP_METHOD; j++)
			SendDlgItemMessage (hWnd, IDC_COMBO1+i, CB_ADDSTRING, 0, (LPARAM)label[j]);
	}
	SetDialog (hWnd, pTab->Cfg()->CfgPhysicsPrm);
}

void ExtraAngDynamics::ResetDialog (HWND hWnd)
{
	extern CFG_PHYSICSPRM CfgPhysicsPrm_default;
	SetDialog (hWnd, CfgPhysicsPrm_default);
}

void ExtraAngDynamics::SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm)
{
	char cbuf[64];
	int i, j;
	int n = prm.nAPropLevel;
	for (i = 0; i < 5; i++) {
		SendDlgItemMessage (hWnd, IDC_CHECK1+i, BM_SETCHECK, i < n ? BST_CHECKED : BST_UNCHECKED, 0);
		EnableWindow (GetDlgItem (hWnd, IDC_CHECK1+i), i < n-1 || i > n || i == 0 ? FALSE : TRUE);
		ShowWindow (GetDlgItem (hWnd, IDC_COMBO1+i), i < n ? SW_SHOW : SW_HIDE);
		if (i < 4) {
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT1+i), i < n-1 ? SW_SHOW : SW_HIDE);
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT5+i), i < n-1 ? SW_SHOW : SW_HIDE);
		}
		if (i < n) {
			int id = prm.APropMode[i];
			for (j = 0; j < NAPROP_METHOD; j++)
				if (id == PropId[j]) {
					SendDlgItemMessage (hWnd, IDC_COMBO1+i, CB_SETCURSEL, j, 0);
					break;
				}
			if (i < n-1) {
				sprintf (cbuf, "%0.2f", prm.APropTLimit[i]);
				SetWindowText (GetDlgItem (hWnd, IDC_EDIT1+i), cbuf);
				sprintf (cbuf, "%0.1f", prm.PropALimit[i]*DEG);
				SetWindowText (GetDlgItem (hWnd, IDC_EDIT5+i), cbuf);
			}
		}
	}
	sprintf (cbuf, "%0.1f", prm.APropSubLimit*DEG);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT9), cbuf);
	sprintf (cbuf, "%d", prm.APropSubMax);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT10), cbuf);
	sprintf (cbuf, "%0.1f", prm.APropCouplingLimit*DEG);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT11), cbuf);
	sprintf (cbuf, "%0.1f", prm.APropTorqueLimit*DEG);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT12), cbuf);
}

void ExtraAngDynamics::Activate (HWND hWnd, int which)
{
	int i = which-IDC_CHECK1;
	int check = SendDlgItemMessage (hWnd, which, BM_GETCHECK, 0, 0);
	if (check == BST_CHECKED) {
		if (i < 4) EnableWindow (GetDlgItem (hWnd, which+1), TRUE);
		if (i > 0) EnableWindow (GetDlgItem (hWnd, which-1), FALSE);
		ShowWindow (GetDlgItem (hWnd, IDC_COMBO1+i), SW_SHOW);
		if (i > 0) {
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT1+i-1), SW_SHOW);
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT5+i-1), SW_SHOW);
		}
	} else {
		if (i > 1) EnableWindow (GetDlgItem (hWnd, which-1), TRUE);
		if (i < 4) EnableWindow (GetDlgItem (hWnd, which+1), FALSE);
		ShowWindow (GetDlgItem (hWnd, IDC_COMBO1+i), SW_HIDE);
		if (i > 0) {
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT1+i-1), SW_HIDE);
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT5+i-1), SW_HIDE);
		}
	}
}

bool ExtraAngDynamics::StoreParams (HWND hWnd)
{
	char cbuf[256];
	int i, n = 0;
	double val, tlimit[5], alimit[5], couplim, torqlim;
	int mode[5];
	for (i = 0; i < 5; i++) {
		if (SendDlgItemMessage (hWnd, IDC_CHECK1+i, BM_GETCHECK, 0, 0) == BST_CHECKED)
			n++;
	}
	for (i = 0; i < n-1; i++) {
		GetWindowText (GetDlgItem (hWnd, IDC_EDIT1+i), cbuf, 256);
		if ((sscanf (cbuf, "%lf", tlimit+i) != 1) || (tlimit[i] <= 0)) {
			Error ("Invalid step limit entry.");
			return false;
		}
		GetWindowText (GetDlgItem (hWnd, IDC_EDIT5+i), cbuf, 256);
		if ((sscanf (cbuf, "%lf", alimit+i) != 1) || (alimit[i] <= 0)) {
			Error ("Invalid angle step limit entry.");
			return false;
		}
		if (i > 0 && (tlimit[i] <= tlimit[i-1] || alimit[i] <= alimit[i-1])) {
			Error ("Step limits must be in ascending order");
			return false;
		}
	}
	for (i = 0; i < n; i++) {
		mode[i] = SendDlgItemMessage (hWnd, IDC_COMBO1+i, CB_GETCURSEL, 0, 0);
		if (mode[i] == CB_ERR) {
			Error ("Invalid propagator.");
			return false;
		}
	}
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT11), cbuf, 256);
	if ((sscanf (cbuf, "%lf", &couplim) != 1) || (couplim < 0.0)) {
		Error ("Invalid coupling step limit");
		return false;
	}
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT12), cbuf, 256);
	if ((sscanf (cbuf, "%lf", &torqlim) != 1) || (torqlim < couplim)) {
		Error ("Torque step limit must be greater than coupling limit");
		return false;
	}

	Config *cfg = pTab->Cfg();
	cfg->CfgPhysicsPrm.nAPropLevel = n;
	for (i = 0; i < n; i++) {
		cfg->CfgPhysicsPrm.APropMode[i] = PropId[mode[i]];
		cfg->CfgPhysicsPrm.APropTLimit[i] = (i < n-1 ? tlimit[i]     : 1e10);
		cfg->CfgPhysicsPrm.PropALimit[i] = (i < n-1 ? alimit[i]*RAD : 1e10);
	}
	cfg->CfgPhysicsPrm.APropCouplingLimit = couplim*RAD;
	cfg->CfgPhysicsPrm.APropTorqueLimit = torqlim*RAD;

	GetWindowText (GetDlgItem (hWnd, IDC_EDIT9), cbuf, 256);
	if ((sscanf (cbuf, "%lf", &val) != 1 || val < 0)) {
		Error ("Invalid subsampling target step.");
		return false;
	} else cfg->CfgPhysicsPrm.APropSubLimit = val*RAD;
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT10), cbuf, 256);
	if ((sscanf (cbuf, "%d", &i) != 1) || (i < 1)) {
		Error ("Invalid subsampling steps.");
		return false;
	} else cfg->CfgPhysicsPrm.APropSubMax = i;

	return true;
}

bool ExtraAngDynamics::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, pTab->Launchpad()->GetInstance(), "extra_angprop");
	return true;
}

INT_PTR CALLBACK ExtraAngDynamics::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraAngDynamics*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_CHECK1:
		case IDC_CHECK2:
		case IDC_CHECK3:
		case IDC_CHECK4:
		case IDC_CHECK5:
			((ExtraAngDynamics*)GetWindowLongPtr (hWnd, DWLP_USER))->Activate (hWnd, LOWORD(wParam));
			break;
		case IDC_BUTTON1:
			((ExtraAngDynamics*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDC_BUTTON2:
			((ExtraAngDynamics*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((ExtraAngDynamics*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}

#endif

//-----------------------------------------------------------------------------
// Physics engine: Parameters for orbit stabilisation
//-----------------------------------------------------------------------------

char *ExtraStabilisation::Name ()
{
	return (char*)"Orbit stabilisation";
}

char *ExtraStabilisation::Description ()
{
	return (char*)"Select the parameters that determine the conditions when Orbiter switches between dynamic and stabilised state updates.";
}

bool ExtraStabilisation::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_STABILISATION, DlgProc);
	return true;
}

void ExtraStabilisation::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgPhysicsPrm);
}

void ExtraStabilisation::ResetDialog (HWND hWnd)
{
	extern CFG_PHYSICSPRM CfgPhysicsPrm_default;
	SetDialog (hWnd, CfgPhysicsPrm_default);
}

void ExtraStabilisation::SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm)
{
	char cbuf[256];
	SendDlgItemMessage (hWnd, IDC_STAB_ENABLE, BM_SETCHECK, prm.bOrbitStabilise ? BST_CHECKED : BST_UNCHECKED, 0);
	sprintf (cbuf, "%0.4g", prm.Stabilise_PLimit*100.0);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf);
	sprintf (cbuf, "%0.4g", prm.Stabilise_SLimit*100.0);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT2), cbuf);
	sprintf (cbuf, "%0.4g", prm.PPropSubLimit*100.0);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT3), cbuf);
	sprintf (cbuf, "%d", prm.PPropSubMax);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT4), cbuf);
	sprintf (cbuf, "%0.4g", prm.PPropStepLimit*100.0);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT5), cbuf);
	ToggleEnable (hWnd);
}

bool ExtraStabilisation::StoreParams (HWND hWnd)
{
	char cbuf[256];
	int i;
	double plimit, slimit, val;
	Config *cfg = pTab->Cfg();
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf, 256);
	if (sscanf (cbuf, "%lf", &plimit) != 1 || plimit < 0.0 || plimit > 100.0) {
		Error ("Invalid perturbation limit.");
		return false;
	}
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT2), cbuf, 256);
	if (sscanf (cbuf, "%lf", &slimit) != 1 || slimit < 0.0) {
		Error ("Invalid step limit.");
		return false;
	}
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT3), cbuf, 256);
	if ((sscanf (cbuf, "%lf", &val) != 1 || val < 0)) {
		Error ("Invalid subsampling target step.");
		return false;
	} else cfg->CfgPhysicsPrm.PPropSubLimit = val*0.01;
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT4), cbuf, 256);
	if ((sscanf (cbuf, "%d", &i) != 1) || (i < 1)) {
		Error ("Invalid subsampling steps.");
		return false;
	} else cfg->CfgPhysicsPrm.PPropSubMax = i;
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT5), cbuf, 256);
	if ((sscanf (cbuf, "%lf", &val) != 1 || val < 0)) {
		Error ("Invalid perturbation limit value.");
		return false;
	} else cfg->CfgPhysicsPrm.PPropStepLimit = val*0.01;

	cfg->CfgPhysicsPrm.bOrbitStabilise = (SendDlgItemMessage (hWnd, IDC_STAB_ENABLE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	cfg->CfgPhysicsPrm.Stabilise_PLimit = plimit * 0.01;
	cfg->CfgPhysicsPrm.Stabilise_SLimit = slimit * 0.01;
	return true;
}

void ExtraStabilisation::ToggleEnable (HWND hWnd)
{
	int i;
	bool bstab = (SendDlgItemMessage (hWnd, IDC_STAB_ENABLE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	for (i = IDC_EDIT1; i <= IDC_EDIT5; i++)
		EnableWindow (GetDlgItem (hWnd, i), bstab);
	for (i = IDC_STATIC1; i <= IDC_STATIC13; i++)
		EnableWindow (GetDlgItem (hWnd, i), bstab);
}

bool ExtraStabilisation::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, "extra_orbitstab");
	return true;
}

INT_PTR CALLBACK ExtraStabilisation::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraStabilisation*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_STAB_ENABLE:
			if (HIWORD (wParam) == BN_CLICKED) {
				((ExtraStabilisation*)GetWindowLongPtr (hWnd, DWLP_USER))->ToggleEnable (hWnd);
				return TRUE;
			}
			break;
		case IDC_BUTTON1:
			((ExtraStabilisation*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDC_BUTTON2:
			((ExtraStabilisation*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((ExtraStabilisation*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams(hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}


//=============================================================================
// Instruments and panels
//=============================================================================

char *ExtraInstruments::Name ()
{
	return (char*)"Instruments and panels";
}

char *ExtraInstruments::Description ()
{
	return (char*)"Select general configuration parameters for spacecraft instruments, MFD displays and instrument panels.";
}

//-----------------------------------------------------------------------------
// Instruments and panels: MFDs
//-----------------------------------------------------------------------------

char *ExtraMfdConfig::Name()
{
	return (char*)"MFD parameter configuration";
}

char *ExtraMfdConfig::Description ()
{
	return (char*)"Select display parameters for multifunctional displays (MFD).";
}

bool ExtraMfdConfig::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_MFDCONFIG, DlgProc);
	return true;
}

void ExtraMfdConfig::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgInstrumentPrm);
}

void ExtraMfdConfig::ResetDialog (HWND hWnd)
{
	extern CFG_INSTRUMENTPRM CfgInstrumentPrm_default;
	SetDialog (hWnd, CfgInstrumentPrm_default);
}

void ExtraMfdConfig::SetDialog (HWND hWnd, const CFG_INSTRUMENTPRM &prm)
{
	char cbuf[256];
	int i, idx;
	for (i = 0; i < 3; i++)
		SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_SETCHECK, i == prm.bMfdPow2 ? BST_CHECKED : BST_UNCHECKED, 0);
	sprintf (cbuf, "%d", prm.MfdHiresThreshold);
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf);

	idx = (prm.VCMFDSize == 256 ? 0 : prm.VCMFDSize == 512 ? 1 : 2);
	for (i = 0; i < 3; i++)
		SendDlgItemMessage (hWnd, IDC_RADIO4+i, BM_SETCHECK, i == idx ? BST_CHECKED : BST_UNCHECKED, 0);
}

bool ExtraMfdConfig::StoreParams (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	char cbuf[256];
	int i, size, check;
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf, 256);
	if ((sscanf (cbuf, "%d", &size) != 1) || size < 8) {
		return false;
	} else
		cfg->CfgInstrumentPrm.MfdHiresThreshold = size;

	for (i = 0; i < 3; i++) {
		check = SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_GETCHECK, 0, 0);
		if (check == BST_CHECKED) {
			cfg->CfgInstrumentPrm.bMfdPow2 = i;
			break;
		}
	}

	size = 256;
	for (i = 0; i < 3; i++) {
		check = SendDlgItemMessage (hWnd, IDC_RADIO4+i, BM_GETCHECK, 0, 0);
		if (check == BST_CHECKED) {
			cfg->CfgInstrumentPrm.VCMFDSize = size;
			break;
		}
		size *= 2;
	}

	return true;
}

void ExtraMfdConfig::ToggleEnable (HWND hWnd)
{
	// todo
}

bool ExtraMfdConfig::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, "extra_mfdconfig");
	return true;
}

INT_PTR CALLBACK ExtraMfdConfig::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraMfdConfig*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BUTTON1:
			((ExtraMfdConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDC_BUTTON2:
			((ExtraMfdConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((ExtraMfdConfig*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}


//=============================================================================
// Root item for vessel configurations (sub-items to be added by modules)
//=============================================================================

char *ExtraVesselConfig::Name ()
{
	return (char*)"Vessel configuration";
}

char *ExtraVesselConfig::Description ()
{
	return (char*)"Configure spacecraft parameters";
}

//=============================================================================
// Root item for planet configurations (sub-items to be added by modules)
//=============================================================================

char *ExtraPlanetConfig::Name ()
{
	return (char*)"Celestial body configuration";
}

char *ExtraPlanetConfig::Description ()
{
	return (char*)"Configure options for celestial objects";
}

//=============================================================================
// Debugging parameters
//=============================================================================

char *ExtraDebug::Name ()
{
	return (char*)"Debugging options";
}

char *ExtraDebug::Description ()
{
	return (char*)"Various options that are useful for debugging and special tasks. Not generally used for standard simulation sessions.";
}

//-----------------------------------------------------------------------------
// Debugging parameters: shutdown options
//-----------------------------------------------------------------------------

char *ExtraShutdown::Name ()
{
	return (char*)"Orbiter shutdown options";
}

char *ExtraShutdown::Description ()
{
	return (char*)"Set the behaviour of Orbiter after closing the simulation window: return to Launchpad, respawn or terminate.";
}

bool ExtraShutdown::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_SHUTDOWN, DlgProc);
	return true;
}

void ExtraShutdown::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

void ExtraShutdown::ResetDialog (HWND hWnd)
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

void ExtraShutdown::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
{
	for (int i = 0; i < 3; i++)
		SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_SETCHECK, (i==prm.ShutdownMode ? BST_CHECKED:BST_UNCHECKED), 0);
}

bool ExtraShutdown::StoreParams (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	int mode;
	for (mode = 0; mode < 2; mode++)
		if (SendDlgItemMessage (hWnd, IDC_RADIO1+mode, BM_GETCHECK, 0, 0) == BST_CHECKED) break;
	cfg->CfgDebugPrm.ShutdownMode = mode;
	return true;
}

bool ExtraShutdown::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, "extra_shutdown");
	return true;
}

INT_PTR CALLBACK ExtraShutdown::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraShutdown*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BUTTON1:
			((ExtraShutdown*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDC_BUTTON2:
			((ExtraShutdown*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((ExtraShutdown*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}

//-----------------------------------------------------------------------------
// Debugging parameters: fixed time steps
//-----------------------------------------------------------------------------

char *ExtraFixedStep::Name ()
{
	return (char*)"Fixed time steps";
}

char *ExtraFixedStep::Description ()
{
	return (char*)"This option assigns a fixed simulation time interval to each frame. Useful for debugging, and when numerical accuracy and stability of the dynamic propagators are important (for example, to generate trajectory data or when recording high-fidelity playbacks).\r\n\r\nWarning: Selecting this option leads to nonlinear time flow and a simulation that is no longer real-time.";
}

bool ExtraFixedStep::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_FIXEDSTEP, DlgProc);
	return true;
}

void ExtraFixedStep::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

void ExtraFixedStep::ResetDialog (HWND hWnd)
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

void ExtraFixedStep::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
{
	char cbuf[256];
	double step = prm.FixedStep;

	if (pTab->Cfg()->CfgCmdlinePrm.FixedStep) {
		// fixed step is set by command line options - disable the dialog
		SendDlgItemMessage(hWnd, IDC_CHECK1, BM_SETCHECK, BST_CHECKED, 0);
		sprintf(cbuf, "%0.4g", pTab->Cfg()->CfgCmdlinePrm.FixedStep);
		SetWindowText(GetDlgItem(hWnd, IDC_EDIT1), cbuf);
		EnableWindow(GetDlgItem(hWnd, IDC_CHECK1), FALSE);
		EnableWindow(GetDlgItem(hWnd, IDC_EDIT1), FALSE);
	}
	else {
		SendDlgItemMessage(hWnd, IDC_CHECK1, BM_SETCHECK, step ? BST_CHECKED : BST_UNCHECKED, 0);
		sprintf(cbuf, "%0.4g", step ? step : 0.01);
		SetWindowText(GetDlgItem(hWnd, IDC_EDIT1), cbuf);
		ToggleEnable(hWnd);
	}
}

bool ExtraFixedStep::StoreParams (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	bool fixed = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);
	if (!fixed) {
		cfg->CfgDebugPrm.FixedStep = 0;
	} else {
		char cbuf[256];
		double dt;
		GetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf, 256);
		if (sscanf (cbuf, "%lf", &dt) != 1 || dt <= 0) {
			Error ("Invalid frame interval length");
			return false;
		}
		cfg->CfgDebugPrm.FixedStep = dt;
	}
	return true;
}

void ExtraFixedStep::ToggleEnable (HWND hWnd)
{
	bool fixed = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);
	EnableWindow (GetDlgItem (hWnd, IDC_STATIC1), fixed);
	EnableWindow (GetDlgItem (hWnd, IDC_EDIT1), fixed);
}

bool ExtraFixedStep::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, "extra_fixedstep");
	return true;
}

INT_PTR CALLBACK ExtraFixedStep::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraFixedStep*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_CHECK1:
			if (HIWORD (wParam) == BN_CLICKED) {
				((ExtraFixedStep*)GetWindowLongPtr (hWnd, DWLP_USER))->ToggleEnable (hWnd);
				return TRUE;
			}
			break;
		case IDC_BUTTON1:
			((ExtraFixedStep*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDC_BUTTON2:
			((ExtraFixedStep*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((ExtraFixedStep*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}

//-----------------------------------------------------------------------------
// Debugging parameters: rendering options
//-----------------------------------------------------------------------------

char *ExtraRenderingOptions::Name ()
{
	return (char*)"Rendering options";
}

char *ExtraRenderingOptions::Description ()
{
	return (char*)"Some rendering options that can be used for debugging problems.";
}

bool ExtraRenderingOptions::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_DBGRENDER, DlgProc);
	return true;
}

void ExtraRenderingOptions::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

void ExtraRenderingOptions::ResetDialog (HWND hWnd)
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

void ExtraRenderingOptions::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
{
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, prm.bWireframeMode ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_CHECK2, BM_SETCHECK, prm.bNormaliseNormals ? BST_CHECKED : BST_UNCHECKED, 0);
}

bool ExtraRenderingOptions::StoreParams (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	cfg->CfgDebugPrm.bWireframeMode = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);
	cfg->CfgDebugPrm.bNormaliseNormals = (SendDlgItemMessage (hWnd, IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_CHECKED);
	return true;
}

INT_PTR CALLBACK ExtraRenderingOptions::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraRenderingOptions*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BUTTON1:
			((ExtraRenderingOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
	//	case IDC_BUTTON2:
	//		((ExtraTimerSettings*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
	//		return 0;
		case IDOK:
			if (((ExtraRenderingOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}

//-----------------------------------------------------------------------------
// Debugging parameters: timer settings
//-----------------------------------------------------------------------------

char *ExtraTimerSettings::Name ()
{
	return (char*)"Timer settings";
}

char *ExtraTimerSettings::Description ()
{
	return (char*)"This option allows the selection of the timer used by Orbiter to calculate time step intervals. Useful for testing and working around buggy hardware timers.";
}

bool ExtraTimerSettings::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_TIMER, DlgProc);
	return true;
}

void ExtraTimerSettings::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

void ExtraTimerSettings::ResetDialog (HWND hWnd)
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

void ExtraTimerSettings::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
{
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"Automatic selection");
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"High-performance hardware timer");
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_ADDSTRING, 0, (LPARAM)"Software timer");
	SendDlgItemMessage (hWnd, IDC_COMBO1, CB_SETCURSEL, prm.TimerMode, 0);
}

bool ExtraTimerSettings::StoreParams (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	int idx = SendDlgItemMessage (hWnd, IDC_COMBO1, CB_GETCURSEL, 0, 0);
	if (idx == CB_ERR) idx = 0;
	cfg->CfgDebugPrm.TimerMode = idx;
	return true;
}

bool ExtraTimerSettings::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, "extra_timer");
	return true;
}

INT_PTR CALLBACK ExtraTimerSettings::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraTimerSettings*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BUTTON1:
			((ExtraTimerSettings*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDC_BUTTON2:
			((ExtraTimerSettings*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((ExtraTimerSettings*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}

//-----------------------------------------------------------------------------
// Debugging parameters: performance options
//-----------------------------------------------------------------------------

char *ExtraPerformanceSettings::Name ()
{
	return (char*)"Performance options";
}

char *ExtraPerformanceSettings::Description ()
{
	return (char*)"This option can be used to modify Windows environment parameters that can improve the simulator performance.";
}

bool ExtraPerformanceSettings::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_PERFORMANCE, DlgProc);
	return true;
}

void ExtraPerformanceSettings::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

void ExtraPerformanceSettings::ResetDialog (HWND hWnd)
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

void ExtraPerformanceSettings::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
{
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, prm.bDisableSmoothFont ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_CHECK2, BM_SETCHECK, prm.bForceReenableSmoothFont ? BST_CHECKED : BST_UNCHECKED, 0);
}

bool ExtraPerformanceSettings::StoreParams (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	cfg->CfgDebugPrm.bDisableSmoothFont = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED ? true : false);
	cfg->CfgDebugPrm.bForceReenableSmoothFont = (SendDlgItemMessage (hWnd, IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_CHECKED ? true : false);
	if (cfg->CfgDebugPrm.bDisableSmoothFont)
		g_pOrbiter->ActivateRoughType();
	else
		g_pOrbiter->DeactivateRoughType();
	return true;
}

bool ExtraPerformanceSettings::OpenHelp (HWND hWnd)
{
	OpenDefaultHelp (hWnd, "extra_performance");
	return true;
}

INT_PTR CALLBACK ExtraPerformanceSettings::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraPerformanceSettings*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BUTTON1:
			((ExtraPerformanceSettings*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		case IDC_BUTTON2:
			((ExtraPerformanceSettings*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
			return 0;
		case IDOK:
			if (((ExtraPerformanceSettings*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}


//-----------------------------------------------------------------------------
// Debugging parameters: launchpad options
//-----------------------------------------------------------------------------

char *ExtraLaunchpadOptions::Name ()
{
	return (char*)"Launchpad options";
}

char *ExtraLaunchpadOptions::Description ()
{
	return (char*)"Configure the behaviour of the Orbiter Launchpad dialog.";
}

bool ExtraLaunchpadOptions::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_LAUNCHPAD, DlgProc);
	return true;
}

void ExtraLaunchpadOptions::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

void ExtraLaunchpadOptions::ResetDialog (HWND hWnd)
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

void ExtraLaunchpadOptions::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
{
	int i;
	for (i = 0; i < 3; i++)
		SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_SETCHECK, prm.bHtmlScnDesc == i ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, prm.bSaveExitScreen ? BST_CHECKED : BST_UNCHECKED, 0);
}

bool ExtraLaunchpadOptions::StoreParams (HWND hWnd)
{
	int i;
	Config *cfg = pTab->Cfg();
	cfg->CfgDebugPrm.bSaveExitScreen = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED ? true : false);
	for (i = 0; i < 3; i++) {
		if (SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_GETCHECK, 0, 0) == BST_CHECKED) {
			break;
		}
	}
	if (i != cfg->CfgDebugPrm.bHtmlScnDesc) {
		cfg->CfgDebugPrm.bHtmlScnDesc = i;
		MessageBox (NULL, "You need to restart Orbiter for these changes to take effect.", "Orbiter settings", MB_OK | MB_ICONEXCLAMATION);
	}
	return true;
}

INT_PTR CALLBACK ExtraLaunchpadOptions::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraLaunchpadOptions*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BUTTON1:
			((ExtraLaunchpadOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		//case IDC_BUTTON2:
		//	((ExtraLaunchpadOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
		//	return 0;
		case IDOK:
			if (((ExtraLaunchpadOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}


//-----------------------------------------------------------------------------
// Debugging parameters: logfile options
//-----------------------------------------------------------------------------

char *ExtraLogfileOptions::Name ()
{
	return (char*)"Logfile options";
}

char *ExtraLogfileOptions::Description ()
{
	return (char*)"Configure options for log file output.";
}

bool ExtraLogfileOptions::clbkOpen (HWND hParent)
{
	OpenDialog (hParent, IDD_EXTRA_LOGFILE, DlgProc);
	return true;
}

void ExtraLogfileOptions::InitDialog (HWND hWnd)
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

void ExtraLogfileOptions::ResetDialog (HWND hWnd)
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

void ExtraLogfileOptions::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
{
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, prm.bVerboseLog ? BST_CHECKED : BST_UNCHECKED, 0);
}

bool ExtraLogfileOptions::StoreParams (HWND hWnd)
{
	Config *cfg = pTab->Cfg();
	cfg->CfgDebugPrm.bVerboseLog = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED ? true : false);
	return true;
}

INT_PTR CALLBACK ExtraLogfileOptions::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		((ExtraLogfileOptions*)lParam)->InitDialog (hWnd);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDC_BUTTON1:
			((ExtraLogfileOptions*)GetWindowLongPtr(hWnd, DWLP_USER))->ResetDialog (hWnd);
			return 0;
		//case IDC_BUTTON2:
		//	((ExtraLogfileOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->OpenHelp (hWnd);
		//	return 0;
		case IDOK:
			if (((ExtraLogfileOptions*)GetWindowLongPtr (hWnd, DWLP_USER))->StoreParams (hWnd))
				EndDialog (hWnd, 0);
			break;
		}
		break;
	}
	return BuiltinLaunchpadItem::DlgProc (hWnd, uMsg, wParam, lParam);
}

//-----------------------------------------------------------------------------
// class LaunchpadItem: addon-defined items for the "Extra" tab
// Interface in OrbiterAPI.h
//-----------------------------------------------------------------------------

LaunchpadItem::LaunchpadItem ()
{
	hItem = 0;
}

LaunchpadItem::~LaunchpadItem ()
{}

char *LaunchpadItem::Name ()
{
	return 0;
}

char *LaunchpadItem::Description ()
{
	return 0;
}

bool LaunchpadItem::OpenDialog (HINSTANCE hInst, HWND hLaunchpad, int resId, DLGPROC pDlg)
{
	DialogBoxParam (hInst, MAKEINTRESOURCE (resId), hLaunchpad, pDlg, (LPARAM)this);
	return true;
}

bool LaunchpadItem::clbkOpen (HWND hLaunchpad)
{
	return false;
}

int LaunchpadItem::clbkWriteConfig ()
{
	return 0;
}
