// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ExtraTab class
//=============================================================================

#define OAPI_IMPLEMENTATION

#ifndef __linux__
#include <windows.h>
#include <commctrl.h>
#include <winuser.h>
#endif // !__linux__
#include "Launchpad.h"
#include "TabExtra.h"
#include "Orbiter.h"
#include "Rigidbody.h"
#include "Log.h"
#include "Help.h"
#include "resource.h"
#include "resource2.h"
#ifdef __linux__
#include "ResDialog.h"
#include "Util.h"
#include <QAbstractButton>
#include <QComboBox>
#include <QDialog>
#include <QMessageBox>
#include <QPushButton>
#include <QTreeWidget>
#include <strings.h>

// BM_SETCHECK / BM_GETCHECK / EnableWindow / ShowWindow on a dialog control
static void SetCheck (QWidget *hDlg, int id, bool check)
{
	if (QAbstractButton *b = DlgItem<QAbstractButton> (hDlg, id)) b->setChecked (check);
}

static bool IsChecked (QWidget *hDlg, int id)
{
	QAbstractButton *b = DlgItem<QAbstractButton> (hDlg, id);
	return (b && b->isChecked());
}

static void EnableItem (QWidget *hDlg, int id, bool enable)
{
	if (QWidget *w = oapiResDlgItem (hDlg, id)) w->setEnabled (enable);
}

static void ShowItem (QWidget *hDlg, int id, bool show)
{
	if (QWidget *w = oapiResDlgItem (hDlg, id)) w->setVisible (show);
}
#endif // __linux__

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

#ifndef __linux__
	r_lst0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_EXT_LIST));  // REMOVE!
	r_dsc0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_EXT_TEXT));  // REMOVE!
	r_pane  = GetClientPos (hTab, GetDlgItem (hTab, IDC_EXT_SPLIT1));
	r_edit0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_EXT_OPEN));
	splitListDesc.SetHwnd (GetDlgItem (hTab, IDC_EXT_SPLIT1), GetDlgItem (hTab, IDC_EXT_LIST), GetDlgItem (hTab, IDC_EXT_TEXT));
#else // __linux__
	r_lst0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_EXT_LIST));  // REMOVE!
	r_dsc0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_EXT_TEXT));  // REMOVE!
	r_pane  = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_EXT_SPLIT1));
	r_edit0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_EXT_OPEN));
	splitListDesc.SetHwnd (oapiResDlgItem (hTab, IDC_EXT_SPLIT1), oapiResDlgItem (hTab, IDC_EXT_LIST), oapiResDlgItem (hTab, IDC_EXT_TEXT));
}

//-----------------------------------------------------------------------------

BOOL orbiter::ExtraTab::OnInitDialog (QWidget *hWnd)
{
	QTreeWidget *hTree = DlgItem<QTreeWidget> (hWnd, IDC_EXT_LIST);
	// WM_NOTIFY
	QObject::connect (hTree, &QTreeWidget::currentItemChanged, hWnd, [hWnd](QTreeWidgetItem *itemNew) {
		// TVN_SELCHANGED
		if (!itemNew) return;
		LaunchpadItem* func = (LaunchpadItem*)itemNew->data (0, Qt::UserRole).value<void*>();
		char* desc = func->Description();
		if (desc) oapiSetDlgItemText(hWnd, IDC_EXT_TEXT, desc);
		else oapiSetDlgItemText(hWnd, IDC_EXT_TEXT, "");
	});
	QObject::connect (hTree, &QTreeWidget::itemDoubleClicked, hWnd, [this, hTree]() {
		// NM_DBLCLK
		QTreeWidgetItem *it = hTree->currentItem();
		BuiltinLaunchpadItem* func = (it ? (BuiltinLaunchpadItem*)it->data (0, Qt::UserRole).value<void*>() : NULL);
		if (func) func->clbkOpen(LaunchpadWnd());
	});
	// WM_COMMAND
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_EXT_OPEN), &QPushButton::clicked, hWnd, [this, hTree]() {
		QTreeWidgetItem *it = hTree->currentItem();
		BuiltinLaunchpadItem *func = (it ? (BuiltinLaunchpadItem*)it->data (0, Qt::UserRole).value<void*>() : NULL);
		if (func) func->clbkOpen (LaunchpadWnd());
	});
	return FALSE;
#endif // __linux__
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::GetConfig (const Config *cfg)
{
#ifndef __linux__
	HTREEITEM ht;
#else // __linux__
	QTreeWidgetItem *ht;
#endif // __linux__
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
	RegisterExtraParam(new ExtraLaunchpadOptions(this), ht); TRACENEW
	RegisterExtraParam(new ExtraLogfileOptions(this), ht); TRACENEW
	RegisterExtraParam(new ExtraPerformanceSettings(this), ht); TRACENEW
	m_internalPrm = m_ExtPrm.size();
#ifndef __linux__
	SetWindowText (GetDlgItem (hTab, IDC_EXT_TEXT), "Advanced and addon-specific configuration parameters.\r\n\r\nClick on an item to get a description.\r\n\r\nDouble-click to open or expand.");
#else // __linux__
	oapiSetDlgItemText(hTab, IDC_EXT_TEXT, "Advanced and addon-specific configuration parameters.\r\n\r\nClick on an item to get a description.\r\n\r\nDouble-click to open or expand.");
#endif // __linux__
	int listw = cfg->CfgWindowPos.LaunchpadExtListWidth;
	if (!listw) {
#ifndef __linux__
		RECT r;
		GetClientRect (GetDlgItem (hTab, IDC_EXT_LIST), &r);
		listw = r.right;
#else // __linux__
		listw = oapiResDlgItem (hTab, IDC_EXT_LIST)->width();
#endif // __linux__
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
#ifndef __linux__
	SetWindowPos (GetDlgItem (hTab, IDC_EXT_SPLIT1), NULL,
		0, 0, w0+dw, h0+dh,
		SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_EXT_OPEN), NULL,
		r_edit0.left, r_edit0.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);
#else // __linux__
	oapiResDlgItem (hTab, IDC_EXT_SPLIT1)->resize (w0+dw, h0+dh);
	oapiResDlgItem (hTab, IDC_EXT_OPEN)->move (r_edit0.left, r_edit0.top+dh);
#endif // __linux__

#ifndef __linux__
	return NULL;
#else // __linux__
	return FALSE;
#endif // __linux__
}

//-----------------------------------------------------------------------------

#ifndef __linux__
HTREEITEM orbiter::ExtraTab::RegisterExtraParam (LaunchpadItem *item, HTREEITEM parent)
#else // __linux__
QTreeWidgetItem *orbiter::ExtraTab::RegisterExtraParam (LaunchpadItem *item, QTreeWidgetItem *parent)
#endif // __linux__
{
	// first check that the item doesn't already exist
#ifndef __linux__
	HTREEITEM hti = FindExtraParam (item->Name(), parent);
#else // __linux__
	QTreeWidgetItem *hti = FindExtraParam (item->Name(), parent);
#endif // __linux__
	if (hti) return hti;

	// add extra parameter instance to list
	m_ExtPrm.push_back(item);

	// if a name is provided, add item to tree list
	char *name = item->Name();
	if (name) {
#ifndef __linux__
		TV_INSERTSTRUCT tvis;
		tvis.item.mask = TVIF_TEXT | TVIF_PARAM;
		tvis.item.pszText = name;
		tvis.item.lParam = (LPARAM)item;
		tvis.hInsertAfter = TVI_LAST;
		tvis.hParent = (parent ? parent : NULL);
		hti = TreeView_InsertItem (GetDlgItem (hTab, IDC_EXT_LIST), &tvis);
#else // __linux__
		hti = new QTreeWidgetItem();
		hti->setText (0, QString::fromUtf8 (name));
		hti->setData (0, Qt::UserRole, QVariant::fromValue ((void*)item));
		if (parent) parent->addChild (hti); // TVI_LAST
		else DlgItem<QTreeWidget> (hTab, IDC_EXT_LIST)->addTopLevelItem (hti);
#endif // __linux__
	} else hti = 0;
	item->hItem = (LAUNCHPADITEM_HANDLE)hti;
	return hti;
}

//-----------------------------------------------------------------------------

bool orbiter::ExtraTab::UnregisterExtraParam (LaunchpadItem *item)
{
	for (auto it = m_ExtPrm.begin(); it != m_ExtPrm.end(); it++) {
		if (*it == item) {
#ifndef __linux__
			TreeView_DeleteItem(GetDlgItem(hTab, IDC_EXT_LIST), item->hItem); // remove entry from UI
#else // __linux__
			delete (QTreeWidgetItem*)item->hItem; // remove entry from UI
#endif // __linux__
			item->clbkWriteConfig(); // allow item to save state before removing
			m_ExtPrm.erase(it);        // delete the container - the actual item has to be deleted by the caller
			return true;
		}
	}
	return false;
}

//-----------------------------------------------------------------------------

#ifndef __linux__
HTREEITEM orbiter::ExtraTab::FindExtraParam (const char *name, const HTREEITEM parent)
#else // __linux__
QTreeWidgetItem *orbiter::ExtraTab::FindExtraParam (const char *name, QTreeWidgetItem *parent)
#endif // __linux__
{
#ifndef __linux__
	HTREEITEM hti = FindExtraParamChild (parent);
#else // __linux__
	QTreeWidgetItem *hti = FindExtraParamChild (parent);
#endif // __linux__
	if (!name) return hti; // no name given - return first child

#ifndef __linux__
	char cbuf[256];
	HWND hCtrl = GetDlgItem (hTab, IDC_EXT_LIST);
	TV_ITEM tvi;
	tvi.pszText = cbuf;
	tvi.cchTextMax = 256;
	tvi.hItem = hti;
	tvi.mask = TVIF_HANDLE | TVIF_TEXT;
	
#else // __linux__
	QTreeWidget *hCtrl = DlgItem<QTreeWidget> (hTab, IDC_EXT_LIST);
	int n = (parent ? parent->childCount() : hCtrl->topLevelItemCount());

#endif // __linux__
	// step through the list
#ifndef __linux__
	while (TreeView_GetItem (hCtrl, &tvi)) {
		if (!_stricmp (name, tvi.pszText)) return tvi.hItem;
		tvi.hItem = TreeView_GetNextSibling (hCtrl, tvi.hItem);
#else // __linux__
	for (int i = 0; i < n; i++) {
		hti = (parent ? parent->child (i) : hCtrl->topLevelItem (i));
		if (!strcasecmp (name, hti->text (0).toUtf8().constData())) return hti;
#endif // __linux__
	}

	return 0;
}

//-----------------------------------------------------------------------------

#ifndef __linux__
HTREEITEM orbiter::ExtraTab::FindExtraParamChild (const HTREEITEM parent)
#else // __linux__
QTreeWidgetItem *orbiter::ExtraTab::FindExtraParamChild (QTreeWidgetItem *parent)
#endif // __linux__
{
#ifndef __linux__
	HWND hCtrl = GetDlgItem (hTab, IDC_EXT_LIST);
	if (parent) return TreeView_GetChild (hCtrl, parent);
	else        return TreeView_GetRoot (hCtrl);
#else // __linux__
	QTreeWidget *hCtrl = DlgItem<QTreeWidget> (hTab, IDC_EXT_LIST);
	if (parent) return parent->child (0);
	else        return hCtrl->topLevelItem (0);
#endif // __linux__
}

//-----------------------------------------------------------------------------

void orbiter::ExtraTab::WriteExtraParams ()
{
	for (auto it = m_ExtPrm.begin(); it != m_ExtPrm.end(); it++)
		(*it)->clbkWriteConfig();
}

//-----------------------------------------------------------------------------

#ifndef __linux__
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
#else // __linux__
// WM_NOTIFY (TVN_SELCHANGED, NM_DBLCLK) and WM_COMMAND (IDC_EXT_OPEN) are connected in OnInitDialog
#endif // __linux__


// ****************************************************************************
// ****************************************************************************

//-----------------------------------------------------------------------------
// Additional functions (under the "Extra" tab)
//-----------------------------------------------------------------------------

BuiltinLaunchpadItem::BuiltinLaunchpadItem (const orbiter::ExtraTab *tab): LaunchpadItem ()
{
	pTab = tab;
}

#ifndef __linux__
bool BuiltinLaunchpadItem::OpenDialog (HWND hParent, int resid, DLGPROC pDlg)
#else // __linux__
bool BuiltinLaunchpadItem::OpenDialog (QWidget *hParent, int resid, DLGINIT pDlg)
#endif // __linux__
{
	return LaunchpadItem::OpenDialog (pTab->AppInstance(), hParent, resid, pDlg);
}

void BuiltinLaunchpadItem::Error (const char *msg)
{
#ifndef __linux__
	MessageBox (pTab->LaunchpadWnd(), msg, "Orbiter configuration error", MB_OK|MB_ICONERROR);
#else // __linux__
	QMessageBox::critical (pTab->LaunchpadWnd(), "Orbiter configuration error", msg);
#endif // __linux__
}

#ifndef __linux__
INT_PTR CALLBACK BuiltinLaunchpadItem::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void BuiltinLaunchpadItem::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	hWnd->setProperty ("LaunchpadItem", QVariant::fromValue (context)); // DWLP_USER
	// WM_COMMAND IDCANCEL (WM_CLOSE: closing a QDialog rejects it, which ends it with 0)
	if (QPushButton *b = DlgItem<QPushButton> (hWnd, IDCANCEL))
		QObject::connect (b, &QPushButton::clicked, hWnd, [hWnd]() { qobject_cast<QDialog*> (hWnd)->done (0); });
#endif // __linux__
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

#ifndef __linux__
bool ExtraDynamics::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraDynamics::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_DYNAMICS, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraDynamics::InitDialog (HWND hWnd)
#else // __linux__
void ExtraDynamics::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	DWORD i, j;
	for (i = 0; i < 5; i++) {
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_PROP_PROP0+i, CB_RESETCONTENT, 0, 0);
#else // __linux__
		DlgItem<QComboBox>(hWnd, IDC_PROP_PROP0+i)->clear();
#endif // __linux__
		for (j = 0; j < NPROP_METHOD; j++)
#ifndef __linux__
			SendDlgItemMessage (hWnd, IDC_PROP_PROP0+i, CB_ADDSTRING, 0, (LPARAM)RigidBody::PropagatorStr(j));
#else // __linux__
			oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_PROP_PROP0+i), RigidBody::PropagatorStr(j));
#endif // __linux__
	}
	SetDialog (hWnd, pTab->Cfg()->CfgPhysicsPrm);
}

#ifndef __linux__
void ExtraDynamics::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraDynamics::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_PHYSICSPRM CfgPhysicsPrm_default;
	SetDialog (hWnd, CfgPhysicsPrm_default);
}

#ifndef __linux__
void ExtraDynamics::SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm)
#else // __linux__
void ExtraDynamics::SetDialog (QWidget *hWnd, const CFG_PHYSICSPRM &prm)
#endif // __linux__
{
	char cbuf[64];
	int i, j;
	int n = prm.nLPropLevel;
	for (i = 0; i < 5; i++) {
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_PROP_ACTIVE0+i, BM_SETCHECK, i < n ? BST_CHECKED : BST_UNCHECKED, 0);
		EnableWindow (GetDlgItem (hWnd, IDC_PROP_ACTIVE0+i), i < n-1 || i > n || i == 0 ? FALSE : TRUE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_PROP0+i), i < n ? SW_SHOW : SW_HIDE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), i < n ? SW_SHOW : SW_HIDE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), i < n ? SW_SHOW : SW_HIDE);
#else // __linux__
		SetCheck(hWnd, IDC_PROP_ACTIVE0+i, i < n);
		EnableItem(hWnd, IDC_PROP_ACTIVE0+i, i < n-1 || i > n || i == 0 ? FALSE : TRUE);
		ShowItem(hWnd, IDC_PROP_PROP0+i, i < n);
		ShowItem(hWnd, IDC_PROP_TTGT0+i, i < n);
		ShowItem(hWnd, IDC_PROP_ATGT0+i, i < n);
#endif // __linux__
		if (i < 4) {
#ifndef __linux__
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i), i < n-1 ? SW_SHOW : SW_HIDE);
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i), i < n-1 ? SW_SHOW : SW_HIDE);
#else // __linux__
			ShowItem(hWnd, IDC_PROP_TLIMIT01+i, i < n-1);
			ShowItem(hWnd, IDC_PROP_ALIMIT01+i, i < n-1);
#endif // __linux__
		}
		if (i < n) {
			int id = prm.PropMode[i];
			for (j = 0; j < NPROP_METHOD; j++)
				if (id == PropId[j]) {
#ifndef __linux__
					SendDlgItemMessage (hWnd, IDC_PROP_PROP0+i, CB_SETCURSEL, j, 0);
#else // __linux__
					DlgItem<QComboBox>(hWnd, IDC_PROP_PROP0+i)->setCurrentIndex(j);
#endif // __linux__
					break;
				}
			sprintf (cbuf, "%0.2f", prm.PropTTgt[i]);
#ifndef __linux__
			SetWindowText (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), cbuf);
#else // __linux__
			oapiSetDlgItemText(hWnd, IDC_PROP_TTGT0+i, cbuf);
#endif // __linux__
			sprintf (cbuf, "%0.1f", prm.PropATgt[i]*DEG);
#ifndef __linux__
			SetWindowText (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), cbuf);
#else // __linux__
			oapiSetDlgItemText(hWnd, IDC_PROP_ATGT0+i, cbuf);
#endif // __linux__
			if (i < n-1) {
				sprintf (cbuf, "%0.2f", prm.PropTLim[i]);
#ifndef __linux__
				SetWindowText (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i), cbuf);
#else // __linux__
				oapiSetDlgItemText(hWnd, IDC_PROP_TLIMIT01+i, cbuf);
#endif // __linux__
				sprintf (cbuf, "%0.1f", prm.PropALim[i]*DEG);
#ifndef __linux__
				SetWindowText (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i), cbuf);
#else // __linux__
				oapiSetDlgItemText(hWnd, IDC_PROP_ALIMIT01+i, cbuf);
#endif // __linux__
			}
		}
	}
	sprintf (cbuf, "%d", prm.PropSubMax);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_PROP_MAXSAMPLE), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_PROP_MAXSAMPLE, cbuf);
#endif // __linux__
}

#ifndef __linux__
void ExtraDynamics::Activate (HWND hWnd, int which)
#else // __linux__
void ExtraDynamics::Activate (QWidget *hWnd, int which)
#endif // __linux__
{
	int i = which-IDC_PROP_ACTIVE0;
#ifndef __linux__
	int check = SendDlgItemMessage (hWnd, which, BM_GETCHECK, 0, 0);
	if (check == BST_CHECKED) {
		if (i < 4) EnableWindow (GetDlgItem (hWnd, which+1), TRUE);
		if (i > 0) EnableWindow (GetDlgItem (hWnd, which-1), FALSE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_PROP0+i), SW_SHOW);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), SW_SHOW);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), SW_SHOW);
#else // __linux__
	bool check = IsChecked (hWnd, which);
	if (check) {
		if (i < 4) EnableItem(hWnd, which+1, TRUE);
		if (i > 0) EnableItem(hWnd, which-1, FALSE);
		ShowItem(hWnd, IDC_PROP_PROP0+i, true);
		ShowItem(hWnd, IDC_PROP_TTGT0+i, true);
		ShowItem(hWnd, IDC_PROP_ATGT0+i, true);
#endif // __linux__
		if (i > 0) {
#ifndef __linux__
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i-1), SW_SHOW);
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i-1), SW_SHOW);
#else // __linux__
			ShowItem(hWnd, IDC_PROP_TLIMIT01+i-1, true);
			ShowItem(hWnd, IDC_PROP_ALIMIT01+i-1, true);
#endif // __linux__
		}
	} else {
#ifndef __linux__
		if (i > 1) EnableWindow (GetDlgItem (hWnd, which-1), TRUE);
		if (i < 4) EnableWindow (GetDlgItem (hWnd, which+1), FALSE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_PROP0+i), SW_HIDE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), SW_HIDE);
		ShowWindow (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), SW_HIDE);
#else // __linux__
		if (i > 1) EnableItem(hWnd, which-1, TRUE);
		if (i < 4) EnableItem(hWnd, which+1, FALSE);
		ShowItem(hWnd, IDC_PROP_PROP0+i, false);
		ShowItem(hWnd, IDC_PROP_TTGT0+i, false);
		ShowItem(hWnd, IDC_PROP_ATGT0+i, false);
#endif // __linux__
		if (i > 0) {
#ifndef __linux__
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i-1), SW_HIDE);
			ShowWindow (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i-1), SW_HIDE);
#else // __linux__
			ShowItem(hWnd, IDC_PROP_TLIMIT01+i-1, false);
			ShowItem(hWnd, IDC_PROP_ALIMIT01+i-1, false);
#endif // __linux__
		}
	}
}

#ifndef __linux__
bool ExtraDynamics::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraDynamics::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	char cbuf[256];
	int i, n = 0;
	double ttgt[MAX_PROP_LEVEL], atgt[MAX_PROP_LEVEL], tlim[MAX_PROP_LEVEL], alim[MAX_PROP_LEVEL];
	int mode[MAX_PROP_LEVEL];
	for (i = 0; i < MAX_PROP_LEVEL; i++) {
#ifndef __linux__
		if (SendDlgItemMessage (hWnd, IDC_PROP_ACTIVE0+i, BM_GETCHECK, 0, 0) == BST_CHECKED)
#else // __linux__
		if (IsChecked(hWnd, IDC_PROP_ACTIVE0+i))
#endif // __linux__
			n++;
	}
	for (i = 0; i < n; i++) {
#ifndef __linux__
		mode[i] = SendDlgItemMessage (hWnd, IDC_PROP_PROP0+i, CB_GETCURSEL, 0, 0);
		if (mode[i] == CB_ERR) {
#else // __linux__
		mode[i] = DlgItem<QComboBox>(hWnd, IDC_PROP_PROP0+i)->currentIndex();
		if (mode[i] == -1) { // CB_ERR
#endif // __linux__
			sprintf (cbuf, "Invalid propagator for integration stage %d.", i+1);
			Error (cbuf);
			return false;
		}
#ifndef __linux__
		GetWindowText (GetDlgItem (hWnd, IDC_PROP_TTGT0+i), cbuf, 256);
#else // __linux__
		oapiGetDlgItemText(hWnd, IDC_PROP_TTGT0+i, cbuf, 256);
#endif // __linux__
		if ((sscanf (cbuf, "%lf", ttgt+i) != 1) || (ttgt[i] <= 0)) {
			sprintf (cbuf, "Invalid time step target for integration stage %d.", i+1);
			Error (cbuf);
			return false;
		}
#ifndef __linux__
		GetWindowText (GetDlgItem (hWnd, IDC_PROP_ATGT0+i), cbuf, 256);
#else // __linux__
		oapiGetDlgItemText(hWnd, IDC_PROP_ATGT0+i, cbuf, 256);
#endif // __linux__
		if ((sscanf (cbuf, "%lf", atgt+i) != 1) || (atgt[i] <= 0)) {
			sprintf (cbuf, "Invalid angle step target for integration stage %d.", i+1);
			Error (cbuf);
			return false;
		}
		if (i < n-1) {
#ifndef __linux__
			GetWindowText (GetDlgItem (hWnd, IDC_PROP_TLIMIT01+i), cbuf, 256);
#else // __linux__
			oapiGetDlgItemText(hWnd, IDC_PROP_TLIMIT01+i, cbuf, 256);
#endif // __linux__
			if ((sscanf (cbuf, "%lf", tlim+i) != 1) || (tlim[i] <= 0)) {
				sprintf (cbuf, "Invalid time step limit for integration stage %d -> %d.", i+1, i+2);
				Error (cbuf);
				return false;
			}
#ifndef __linux__
			GetWindowText (GetDlgItem (hWnd, IDC_PROP_ALIMIT01+i), cbuf, 256);
#else // __linux__
			oapiGetDlgItemText(hWnd, IDC_PROP_ALIMIT01+i, cbuf, 256);
#endif // __linux__
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

#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_PROP_MAXSAMPLE), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_PROP_MAXSAMPLE, cbuf, 256);
#endif // __linux__
	if ((sscanf (cbuf, "%d", &i) != 1) || i < 1) {
		Error ("Invalid value for max. subsamples (integer value >= 1 required).");
		return false;
	} else {
		cfg->CfgPhysicsPrm.PropSubMax = i;
	}

	return true;
}

#ifndef __linux__
bool ExtraDynamics::OpenHelp (HWND hWnd)
#else // __linux__
bool ExtraDynamics::OpenHelp (QWidget *hWnd)
#endif // __linux__
{
	OpenDefaultHelp (hWnd, "extra_linprop");
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraDynamics::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraDynamics::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraDynamics*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_PROP_ACTIVE0:
		case IDC_PROP_ACTIVE1:
		case IDC_PROP_ACTIVE2:
		case IDC_PROP_ACTIVE3:
		case IDC_PROP_ACTIVE4:
			((ExtraDynamics*)context)->Activate (hWnd, id);
			break;
		case IDC_RESET:
			((ExtraDynamics*)context)->ResetDialog (hWnd);
			return;
		case IDCHELP:
			((ExtraDynamics*)context)->OpenHelp (hWnd);
			return;
		case IDOK:
			if (((ExtraDynamics*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraAngDynamics::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraAngDynamics::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_ADYNAMICS, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraAngDynamics::InitDialog (HWND hWnd)
#else // __linux__
void ExtraAngDynamics::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	static char *label[NAPROP_METHOD] = {
		"Runge-Kutta, 2nd order (RK2)", "Runge-Kutta, 4th order (RK4)", "Runge-Kutta, 5th order (RK5)",
		"Runge-Kutta, 6th order (RK6)", "Runge-Kutta, 7th order (RK7)", "Runge-Kutta, 8th order (RK8)"
	};

	int i, j;
	for (i = 0; i < 5; i++) {
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_COMBO1+i, CB_RESETCONTENT, 0, 0);
#else // __linux__
		DlgItem<QComboBox>(hWnd, IDC_COMBO1+i)->clear();
#endif // __linux__
		for (j = 0; j < NAPROP_METHOD; j++)
#ifndef __linux__
			SendDlgItemMessage (hWnd, IDC_COMBO1+i, CB_ADDSTRING, 0, (LPARAM)label[j]);
#else // __linux__
			oapiComboAddString(DlgItem<QComboBox>(hWnd, IDC_COMBO1+i), label[j]);
#endif // __linux__
	}
	SetDialog (hWnd, pTab->Cfg()->CfgPhysicsPrm);
}

#ifndef __linux__
void ExtraAngDynamics::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraAngDynamics::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_PHYSICSPRM CfgPhysicsPrm_default;
	SetDialog (hWnd, CfgPhysicsPrm_default);
}

#ifndef __linux__
void ExtraAngDynamics::SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm)
#else // __linux__
void ExtraAngDynamics::SetDialog (QWidget *hWnd, const CFG_PHYSICSPRM &prm)
#endif // __linux__
{
	char cbuf[64];
	int i, j;
	int n = prm.nAPropLevel;
	for (i = 0; i < 5; i++) {
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_CHECK1+i, BM_SETCHECK, i < n ? BST_CHECKED : BST_UNCHECKED, 0);
		EnableWindow (GetDlgItem (hWnd, IDC_CHECK1+i), i < n-1 || i > n || i == 0 ? FALSE : TRUE);
		ShowWindow (GetDlgItem (hWnd, IDC_COMBO1+i), i < n ? SW_SHOW : SW_HIDE);
#else // __linux__
		SetCheck(hWnd, IDC_CHECK1+i, i < n);
		EnableItem(hWnd, IDC_CHECK1+i, i < n-1 || i > n || i == 0 ? FALSE : TRUE);
		ShowItem(hWnd, IDC_COMBO1+i, i < n);
#endif // __linux__
		if (i < 4) {
#ifndef __linux__
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT1+i), i < n-1 ? SW_SHOW : SW_HIDE);
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT5+i), i < n-1 ? SW_SHOW : SW_HIDE);
#else // __linux__
			ShowItem(hWnd, IDC_EDIT1+i, i < n-1);
			ShowItem(hWnd, IDC_EDIT5+i, i < n-1);
#endif // __linux__
		}
		if (i < n) {
			int id = prm.APropMode[i];
			for (j = 0; j < NAPROP_METHOD; j++)
				if (id == PropId[j]) {
#ifndef __linux__
					SendDlgItemMessage (hWnd, IDC_COMBO1+i, CB_SETCURSEL, j, 0);
#else // __linux__
					DlgItem<QComboBox>(hWnd, IDC_COMBO1+i)->setCurrentIndex(j);
#endif // __linux__
					break;
				}
			if (i < n-1) {
				sprintf (cbuf, "%0.2f", prm.APropTLimit[i]);
#ifndef __linux__
				SetWindowText (GetDlgItem (hWnd, IDC_EDIT1+i), cbuf);
#else // __linux__
				oapiSetDlgItemText(hWnd, IDC_EDIT1+i, cbuf);
#endif // __linux__
				sprintf (cbuf, "%0.1f", prm.PropALimit[i]*DEG);
#ifndef __linux__
				SetWindowText (GetDlgItem (hWnd, IDC_EDIT5+i), cbuf);
#else // __linux__
				oapiSetDlgItemText(hWnd, IDC_EDIT5+i, cbuf);
#endif // __linux__
			}
		}
	}
	sprintf (cbuf, "%0.1f", prm.APropSubLimit*DEG);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT9), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT9, cbuf);
#endif // __linux__
	sprintf (cbuf, "%d", prm.APropSubMax);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT10), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT10, cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.1f", prm.APropCouplingLimit*DEG);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT11), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT11, cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.1f", prm.APropTorqueLimit*DEG);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT12), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT12, cbuf);
#endif // __linux__
}

#ifndef __linux__
void ExtraAngDynamics::Activate (HWND hWnd, int which)
#else // __linux__
void ExtraAngDynamics::Activate (QWidget *hWnd, int which)
#endif // __linux__
{
	int i = which-IDC_CHECK1;
#ifndef __linux__
	int check = SendDlgItemMessage (hWnd, which, BM_GETCHECK, 0, 0);
	if (check == BST_CHECKED) {
		if (i < 4) EnableWindow (GetDlgItem (hWnd, which+1), TRUE);
		if (i > 0) EnableWindow (GetDlgItem (hWnd, which-1), FALSE);
		ShowWindow (GetDlgItem (hWnd, IDC_COMBO1+i), SW_SHOW);
#else // __linux__
	bool check = IsChecked (hWnd, which);
	if (check) {
		if (i < 4) EnableItem(hWnd, which+1, TRUE);
		if (i > 0) EnableItem(hWnd, which-1, FALSE);
		ShowItem(hWnd, IDC_COMBO1+i, true);
#endif // __linux__
		if (i > 0) {
#ifndef __linux__
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT1+i-1), SW_SHOW);
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT5+i-1), SW_SHOW);
#else // __linux__
			ShowItem(hWnd, IDC_EDIT1+i-1, true);
			ShowItem(hWnd, IDC_EDIT5+i-1, true);
#endif // __linux__
		}
	} else {
#ifndef __linux__
		if (i > 1) EnableWindow (GetDlgItem (hWnd, which-1), TRUE);
		if (i < 4) EnableWindow (GetDlgItem (hWnd, which+1), FALSE);
		ShowWindow (GetDlgItem (hWnd, IDC_COMBO1+i), SW_HIDE);
#else // __linux__
		if (i > 1) EnableItem(hWnd, which-1, TRUE);
		if (i < 4) EnableItem(hWnd, which+1, FALSE);
		ShowItem(hWnd, IDC_COMBO1+i, false);
#endif // __linux__
		if (i > 0) {
#ifndef __linux__
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT1+i-1), SW_HIDE);
			ShowWindow (GetDlgItem (hWnd, IDC_EDIT5+i-1), SW_HIDE);
#else // __linux__
			ShowItem(hWnd, IDC_EDIT1+i-1, false);
			ShowItem(hWnd, IDC_EDIT5+i-1, false);
#endif // __linux__
		}
	}
}

#ifndef __linux__
bool ExtraAngDynamics::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraAngDynamics::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	char cbuf[256];
	int i, n = 0;
	double val, tlimit[5], alimit[5], couplim, torqlim;
	int mode[5];
	for (i = 0; i < 5; i++) {
#ifndef __linux__
		if (SendDlgItemMessage (hWnd, IDC_CHECK1+i, BM_GETCHECK, 0, 0) == BST_CHECKED)
#else // __linux__
		if (IsChecked(hWnd, IDC_CHECK1+i))
#endif // __linux__
			n++;
	}
	for (i = 0; i < n-1; i++) {
#ifndef __linux__
		GetWindowText (GetDlgItem (hWnd, IDC_EDIT1+i), cbuf, 256);
#else // __linux__
		oapiGetDlgItemText(hWnd, IDC_EDIT1+i, cbuf, 256);
#endif // __linux__
		if ((sscanf (cbuf, "%lf", tlimit+i) != 1) || (tlimit[i] <= 0)) {
			Error ("Invalid step limit entry.");
			return false;
		}
#ifndef __linux__
		GetWindowText (GetDlgItem (hWnd, IDC_EDIT5+i), cbuf, 256);
#else // __linux__
		oapiGetDlgItemText(hWnd, IDC_EDIT5+i, cbuf, 256);
#endif // __linux__
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
#ifndef __linux__
		mode[i] = SendDlgItemMessage (hWnd, IDC_COMBO1+i, CB_GETCURSEL, 0, 0);
		if (mode[i] == CB_ERR) {
#else // __linux__
		mode[i] = DlgItem<QComboBox>(hWnd, IDC_COMBO1+i)->currentIndex();
		if (mode[i] == -1) { // CB_ERR
#endif // __linux__
			Error ("Invalid propagator.");
			return false;
		}
	}
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT11), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT11, cbuf, 256);
#endif // __linux__
	if ((sscanf (cbuf, "%lf", &couplim) != 1) || (couplim < 0.0)) {
		Error ("Invalid coupling step limit");
		return false;
	}
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT12), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT12, cbuf, 256);
#endif // __linux__
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

#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT9), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT9, cbuf, 256);
#endif // __linux__
	if ((sscanf (cbuf, "%lf", &val) != 1 || val < 0)) {
		Error ("Invalid subsampling target step.");
		return false;
	} else cfg->CfgPhysicsPrm.APropSubLimit = val*RAD;
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT10), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT10, cbuf, 256);
#endif // __linux__
	if ((sscanf (cbuf, "%d", &i) != 1) || (i < 1)) {
		Error ("Invalid subsampling steps.");
		return false;
	} else cfg->CfgPhysicsPrm.APropSubMax = i;

	return true;
}

#ifndef __linux__
bool ExtraAngDynamics::OpenHelp (HWND hWnd)
#else // __linux__
bool ExtraAngDynamics::OpenHelp (QWidget *hWnd)
#endif // __linux__
{
	OpenDefaultHelp (hWnd, pTab->Launchpad()->GetInstance(), "extra_angprop");
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraAngDynamics::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraAngDynamics::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraAngDynamics*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_CHECK1:
		case IDC_CHECK2:
		case IDC_CHECK3:
		case IDC_CHECK4:
		case IDC_CHECK5:
			((ExtraAngDynamics*)context)->Activate (hWnd, id);
			break;
		case IDC_BUTTON1:
			((ExtraAngDynamics*)context)->ResetDialog (hWnd);
			return;
		case IDC_BUTTON2:
			((ExtraAngDynamics*)context)->OpenHelp (hWnd);
			return;
		case IDOK:
			if (((ExtraAngDynamics*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraStabilisation::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraStabilisation::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_STABILISATION, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraStabilisation::InitDialog (HWND hWnd)
#else // __linux__
void ExtraStabilisation::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	SetDialog (hWnd, pTab->Cfg()->CfgPhysicsPrm);
}

#ifndef __linux__
void ExtraStabilisation::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraStabilisation::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_PHYSICSPRM CfgPhysicsPrm_default;
	SetDialog (hWnd, CfgPhysicsPrm_default);
}

#ifndef __linux__
void ExtraStabilisation::SetDialog (HWND hWnd, const CFG_PHYSICSPRM &prm)
#else // __linux__
void ExtraStabilisation::SetDialog (QWidget *hWnd, const CFG_PHYSICSPRM &prm)
#endif // __linux__
{
	char cbuf[256];
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_STAB_ENABLE, BM_SETCHECK, prm.bOrbitStabilise ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hWnd, IDC_STAB_ENABLE, prm.bOrbitStabilise);
#endif // __linux__
	sprintf (cbuf, "%0.4g", prm.Stabilise_PLimit*100.0);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT1, cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.4g", prm.Stabilise_SLimit*100.0);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT2), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT2, cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.4g", prm.PPropSubLimit*100.0);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT3), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT3, cbuf);
#endif // __linux__
	sprintf (cbuf, "%d", prm.PPropSubMax);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT4), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT4, cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.4g", prm.PPropStepLimit*100.0);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT5), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT5, cbuf);
#endif // __linux__
	ToggleEnable (hWnd);
}

#ifndef __linux__
bool ExtraStabilisation::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraStabilisation::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	char cbuf[256];
	int i;
	double plimit, slimit, val;
	Config *cfg = pTab->Cfg();
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT1, cbuf, 256);
#endif // __linux__
	if (sscanf (cbuf, "%lf", &plimit) != 1 || plimit < 0.0 || plimit > 100.0) {
		Error ("Invalid perturbation limit.");
		return false;
	}
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT2), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT2, cbuf, 256);
#endif // __linux__
	if (sscanf (cbuf, "%lf", &slimit) != 1 || slimit < 0.0) {
		Error ("Invalid step limit.");
		return false;
	}
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT3), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT3, cbuf, 256);
#endif // __linux__
	if ((sscanf (cbuf, "%lf", &val) != 1 || val < 0)) {
		Error ("Invalid subsampling target step.");
		return false;
	} else cfg->CfgPhysicsPrm.PPropSubLimit = val*0.01;
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT4), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT4, cbuf, 256);
#endif // __linux__
	if ((sscanf (cbuf, "%d", &i) != 1) || (i < 1)) {
		Error ("Invalid subsampling steps.");
		return false;
	} else cfg->CfgPhysicsPrm.PPropSubMax = i;
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT5), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT5, cbuf, 256);
#endif // __linux__
	if ((sscanf (cbuf, "%lf", &val) != 1 || val < 0)) {
		Error ("Invalid perturbation limit value.");
		return false;
	} else cfg->CfgPhysicsPrm.PPropStepLimit = val*0.01;

#ifndef __linux__
	cfg->CfgPhysicsPrm.bOrbitStabilise = (SendDlgItemMessage (hWnd, IDC_STAB_ENABLE, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	cfg->CfgPhysicsPrm.bOrbitStabilise = (IsChecked(hWnd, IDC_STAB_ENABLE));
#endif // __linux__
	cfg->CfgPhysicsPrm.Stabilise_PLimit = plimit * 0.01;
	cfg->CfgPhysicsPrm.Stabilise_SLimit = slimit * 0.01;
	return true;
}

#ifndef __linux__
void ExtraStabilisation::ToggleEnable (HWND hWnd)
#else // __linux__
void ExtraStabilisation::ToggleEnable (QWidget *hWnd)
#endif // __linux__
{
	int i;
#ifndef __linux__
	bool bstab = (SendDlgItemMessage (hWnd, IDC_STAB_ENABLE, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	bool bstab = (IsChecked(hWnd, IDC_STAB_ENABLE));
#endif // __linux__
	for (i = IDC_EDIT1; i <= IDC_EDIT5; i++)
#ifndef __linux__
		EnableWindow (GetDlgItem (hWnd, i), bstab);
#else // __linux__
		EnableItem(hWnd, i, bstab);
#endif // __linux__
	for (i = IDC_STATIC1; i <= IDC_STATIC13; i++)
#ifndef __linux__
		EnableWindow (GetDlgItem (hWnd, i), bstab);
#else // __linux__
		EnableItem(hWnd, i, bstab);
#endif // __linux__
}

#ifndef __linux__
bool ExtraStabilisation::OpenHelp (HWND hWnd)
#else // __linux__
bool ExtraStabilisation::OpenHelp (QWidget *hWnd)
#endif // __linux__
{
	OpenDefaultHelp (hWnd, "extra_orbitstab");
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraStabilisation::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraStabilisation::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraStabilisation*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_STAB_ENABLE:
			if (code == RESN_CLICKED) {
				((ExtraStabilisation*)context)->ToggleEnable (hWnd);
				return;
			}
			break;
		case IDC_BUTTON1:
			((ExtraStabilisation*)context)->ResetDialog (hWnd);
			return;
		case IDC_BUTTON2:
			((ExtraStabilisation*)context)->OpenHelp (hWnd);
			return;
		case IDOK:
			if (((ExtraStabilisation*)context)->StoreParams(hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraMfdConfig::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraMfdConfig::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_MFDCONFIG, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraMfdConfig::InitDialog (HWND hWnd)
#else // __linux__
void ExtraMfdConfig::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	SetDialog (hWnd, pTab->Cfg()->CfgInstrumentPrm);
}

#ifndef __linux__
void ExtraMfdConfig::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraMfdConfig::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_INSTRUMENTPRM CfgInstrumentPrm_default;
	SetDialog (hWnd, CfgInstrumentPrm_default);
}

#ifndef __linux__
void ExtraMfdConfig::SetDialog (HWND hWnd, const CFG_INSTRUMENTPRM &prm)
#else // __linux__
void ExtraMfdConfig::SetDialog (QWidget *hWnd, const CFG_INSTRUMENTPRM &prm)
#endif // __linux__
{
	char cbuf[256];
	int i, idx;
	for (i = 0; i < 3; i++)
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_SETCHECK, i == prm.bMfdPow2 ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
		SetCheck(hWnd, IDC_RADIO1+i, i == prm.bMfdPow2);
#endif // __linux__
	sprintf (cbuf, "%d", prm.MfdHiresThreshold);
#ifndef __linux__
	SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf);
#else // __linux__
	oapiSetDlgItemText(hWnd, IDC_EDIT1, cbuf);
#endif // __linux__

	idx = (prm.VCMFDSize == 256 ? 0 : prm.VCMFDSize == 512 ? 1 : 2);
	for (i = 0; i < 3; i++)
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_RADIO4+i, BM_SETCHECK, i == idx ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
		SetCheck(hWnd, IDC_RADIO4+i, i == idx);
#endif // __linux__
}

#ifndef __linux__
bool ExtraMfdConfig::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraMfdConfig::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	Config *cfg = pTab->Cfg();
	char cbuf[256];
	int i, size, check;
#ifndef __linux__
	GetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf, 256);
#else // __linux__
	oapiGetDlgItemText(hWnd, IDC_EDIT1, cbuf, 256);
#endif // __linux__
	if ((sscanf (cbuf, "%d", &size) != 1) || size < 8) {
		return false;
	} else
		cfg->CfgInstrumentPrm.MfdHiresThreshold = size;

	for (i = 0; i < 3; i++) {
#ifndef __linux__
		check = SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_GETCHECK, 0, 0);
		if (check == BST_CHECKED) {
#else // __linux__
		check = IsChecked (hWnd, IDC_RADIO1+i);
		if (check) {
#endif // __linux__
			cfg->CfgInstrumentPrm.bMfdPow2 = i;
			break;
		}
	}

	size = 256;
	for (i = 0; i < 3; i++) {
#ifndef __linux__
		check = SendDlgItemMessage (hWnd, IDC_RADIO4+i, BM_GETCHECK, 0, 0);
		if (check == BST_CHECKED) {
#else // __linux__
		check = IsChecked (hWnd, IDC_RADIO4+i);
		if (check) {
#endif // __linux__
			cfg->CfgInstrumentPrm.VCMFDSize = size;
			break;
		}
		size *= 2;
	}

	return true;
}

#ifndef __linux__
void ExtraMfdConfig::ToggleEnable (HWND hWnd)
#else // __linux__
void ExtraMfdConfig::ToggleEnable (QWidget *hWnd)
#endif // __linux__
{
	// todo
}

#ifndef __linux__
bool ExtraMfdConfig::OpenHelp (HWND hWnd)
#else // __linux__
bool ExtraMfdConfig::OpenHelp (QWidget *hWnd)
#endif // __linux__
{
	OpenDefaultHelp (hWnd, "extra_mfdconfig");
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraMfdConfig::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraMfdConfig::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraMfdConfig*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_BUTTON1:
			((ExtraMfdConfig*)context)->ResetDialog (hWnd);
			return;
		case IDC_BUTTON2:
			((ExtraMfdConfig*)context)->OpenHelp (hWnd);
			return;
		case IDOK:
			if (((ExtraMfdConfig*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraShutdown::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraShutdown::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_SHUTDOWN, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraShutdown::InitDialog (HWND hWnd)
#else // __linux__
void ExtraShutdown::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

#ifndef __linux__
void ExtraShutdown::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraShutdown::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

#ifndef __linux__
void ExtraShutdown::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
#else // __linux__
void ExtraShutdown::SetDialog (QWidget *hWnd, const CFG_DEBUGPRM &prm)
#endif // __linux__
{
	for (int i = 0; i < 3; i++)
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_SETCHECK, (i==prm.ShutdownMode ? BST_CHECKED:BST_UNCHECKED), 0);
#else // __linux__
		SetCheck (hWnd, IDC_RADIO1+i, i==prm.ShutdownMode);
#endif // __linux__
}

#ifndef __linux__
bool ExtraShutdown::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraShutdown::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	Config *cfg = pTab->Cfg();
	int mode;
	for (mode = 0; mode < 2; mode++)
#ifndef __linux__
		if (SendDlgItemMessage (hWnd, IDC_RADIO1+mode, BM_GETCHECK, 0, 0) == BST_CHECKED) break;
#else // __linux__
		if (IsChecked(hWnd, IDC_RADIO1+mode)) break;
#endif // __linux__
	cfg->CfgDebugPrm.ShutdownMode = mode;
	return true;
}

#ifndef __linux__
bool ExtraShutdown::OpenHelp (HWND hWnd)
#else // __linux__
bool ExtraShutdown::OpenHelp (QWidget *hWnd)
#endif // __linux__
{
	OpenDefaultHelp (hWnd, "extra_shutdown");
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraShutdown::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraShutdown::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraShutdown*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_BUTTON1:
			((ExtraShutdown*)context)->ResetDialog (hWnd);
			return;
		case IDC_BUTTON2:
			((ExtraShutdown*)context)->OpenHelp (hWnd);
			return;
		case IDOK:
			if (((ExtraShutdown*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraFixedStep::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraFixedStep::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_FIXEDSTEP, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraFixedStep::InitDialog (HWND hWnd)
#else // __linux__
void ExtraFixedStep::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

#ifndef __linux__
void ExtraFixedStep::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraFixedStep::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

#ifndef __linux__
void ExtraFixedStep::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
#else // __linux__
void ExtraFixedStep::SetDialog (QWidget *hWnd, const CFG_DEBUGPRM &prm)
#endif // __linux__
{
	char cbuf[256];
	double step = prm.FixedStep;

	if (pTab->Cfg()->CfgCmdlinePrm.FixedStep) {
		// fixed step is set by command line options - disable the dialog
#ifndef __linux__
		SendDlgItemMessage(hWnd, IDC_CHECK1, BM_SETCHECK, BST_CHECKED, 0);
#else // __linux__
		SetCheck(hWnd, IDC_CHECK1, true);
#endif // __linux__
		sprintf(cbuf, "%0.4g", pTab->Cfg()->CfgCmdlinePrm.FixedStep);
#ifndef __linux__
		SetWindowText(GetDlgItem(hWnd, IDC_EDIT1), cbuf);
		EnableWindow(GetDlgItem(hWnd, IDC_CHECK1), FALSE);
		EnableWindow(GetDlgItem(hWnd, IDC_EDIT1), FALSE);
#else // __linux__
		oapiSetDlgItemText(hWnd, IDC_EDIT1, cbuf);
		EnableItem(hWnd, IDC_CHECK1, FALSE);
		EnableItem(hWnd, IDC_EDIT1, FALSE);
#endif // __linux__
	}
	else {
#ifndef __linux__
		SendDlgItemMessage(hWnd, IDC_CHECK1, BM_SETCHECK, step ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
		SetCheck(hWnd, IDC_CHECK1, step);
#endif // __linux__
		sprintf(cbuf, "%0.4g", step ? step : 0.01);
#ifndef __linux__
		SetWindowText(GetDlgItem(hWnd, IDC_EDIT1), cbuf);
#else // __linux__
		oapiSetDlgItemText(hWnd, IDC_EDIT1, cbuf);
#endif // __linux__
		ToggleEnable(hWnd);
	}
}

#ifndef __linux__
bool ExtraFixedStep::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraFixedStep::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	Config *cfg = pTab->Cfg();
#ifndef __linux__
	bool fixed = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	bool fixed = (IsChecked(hWnd, IDC_CHECK1));
#endif // __linux__
	if (!fixed) {
		cfg->CfgDebugPrm.FixedStep = 0;
	} else {
		char cbuf[256];
		double dt;
#ifndef __linux__
		GetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf, 256);
#else // __linux__
		oapiGetDlgItemText(hWnd, IDC_EDIT1, cbuf, 256);
#endif // __linux__
		if (sscanf (cbuf, "%lf", &dt) != 1 || dt <= 0) {
			Error ("Invalid frame interval length");
			return false;
		}
		cfg->CfgDebugPrm.FixedStep = dt;
	}
	return true;
}

#ifndef __linux__
void ExtraFixedStep::ToggleEnable (HWND hWnd)
#else // __linux__
void ExtraFixedStep::ToggleEnable (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	bool fixed = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);
	EnableWindow (GetDlgItem (hWnd, IDC_STATIC1), fixed);
	EnableWindow (GetDlgItem (hWnd, IDC_EDIT1), fixed);
#else // __linux__
	bool fixed = (IsChecked(hWnd, IDC_CHECK1));
	EnableItem(hWnd, IDC_STATIC1, fixed);
	EnableItem(hWnd, IDC_EDIT1, fixed);
#endif // __linux__
}

#ifndef __linux__
bool ExtraFixedStep::OpenHelp (HWND hWnd)
#else // __linux__
bool ExtraFixedStep::OpenHelp (QWidget *hWnd)
#endif // __linux__
{
	OpenDefaultHelp (hWnd, "extra_fixedstep");
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraFixedStep::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraFixedStep::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraFixedStep*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_CHECK1:
			if (code == RESN_CLICKED) {
				((ExtraFixedStep*)context)->ToggleEnable (hWnd);
				return;
			}
			break;
		case IDC_BUTTON1:
			((ExtraFixedStep*)context)->ResetDialog (hWnd);
			return;
		case IDC_BUTTON2:
			((ExtraFixedStep*)context)->OpenHelp (hWnd);
			return;
		case IDOK:
			if (((ExtraFixedStep*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraRenderingOptions::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraRenderingOptions::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_DBGRENDER, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraRenderingOptions::InitDialog (HWND hWnd)
#else // __linux__
void ExtraRenderingOptions::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

#ifndef __linux__
void ExtraRenderingOptions::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraRenderingOptions::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

#ifndef __linux__
void ExtraRenderingOptions::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
#else // __linux__
void ExtraRenderingOptions::SetDialog (QWidget *hWnd, const CFG_DEBUGPRM &prm)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, prm.bWireframeMode ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_CHECK2, BM_SETCHECK, prm.bNormaliseNormals ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hWnd, IDC_CHECK1, prm.bWireframeMode);
	SetCheck(hWnd, IDC_CHECK2, prm.bNormaliseNormals);
#endif // __linux__
}

#ifndef __linux__
bool ExtraRenderingOptions::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraRenderingOptions::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	Config *cfg = pTab->Cfg();
#ifndef __linux__
	cfg->CfgDebugPrm.bWireframeMode = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED);
	cfg->CfgDebugPrm.bNormaliseNormals = (SendDlgItemMessage (hWnd, IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	cfg->CfgDebugPrm.bWireframeMode = (IsChecked(hWnd, IDC_CHECK1));
	cfg->CfgDebugPrm.bNormaliseNormals = (IsChecked(hWnd, IDC_CHECK2));
#endif // __linux__
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraRenderingOptions::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraRenderingOptions::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraRenderingOptions*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_BUTTON1:
			((ExtraRenderingOptions*)context)->ResetDialog (hWnd);
			return;
	//	case IDC_BUTTON2:
	//		((ExtraTimerSettings*)context)->OpenHelp (hWnd);
	//		return;
		case IDOK:
			if (((ExtraRenderingOptions*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraPerformanceSettings::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraPerformanceSettings::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_PERFORMANCE, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraPerformanceSettings::InitDialog (HWND hWnd)
#else // __linux__
void ExtraPerformanceSettings::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

#ifndef __linux__
void ExtraPerformanceSettings::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraPerformanceSettings::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

#ifndef __linux__
void ExtraPerformanceSettings::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
#else // __linux__
void ExtraPerformanceSettings::SetDialog (QWidget *hWnd, const CFG_DEBUGPRM &prm)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, prm.bDisableSmoothFont ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_CHECK2, BM_SETCHECK, prm.bForceReenableSmoothFont ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hWnd, IDC_CHECK1, prm.bDisableSmoothFont);
	SetCheck(hWnd, IDC_CHECK2, prm.bForceReenableSmoothFont);
#endif // __linux__
}

#ifndef __linux__
bool ExtraPerformanceSettings::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraPerformanceSettings::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	Config *cfg = pTab->Cfg();
#ifndef __linux__
	cfg->CfgDebugPrm.bDisableSmoothFont = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED ? true : false);
	cfg->CfgDebugPrm.bForceReenableSmoothFont = (SendDlgItemMessage (hWnd, IDC_CHECK2, BM_GETCHECK, 0, 0) == BST_CHECKED ? true : false);
#else // __linux__
	cfg->CfgDebugPrm.bDisableSmoothFont = (IsChecked(hWnd, IDC_CHECK1) ? true : false);
	cfg->CfgDebugPrm.bForceReenableSmoothFont = (IsChecked(hWnd, IDC_CHECK2) ? true : false);
#endif // __linux__
	if (cfg->CfgDebugPrm.bDisableSmoothFont)
		g_pOrbiter->ActivateRoughType();
	else
		g_pOrbiter->DeactivateRoughType();
	return true;
}

#ifndef __linux__
bool ExtraPerformanceSettings::OpenHelp (HWND hWnd)
#else // __linux__
bool ExtraPerformanceSettings::OpenHelp (QWidget *hWnd)
#endif // __linux__
{
	OpenDefaultHelp (hWnd, "extra_performance");
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraPerformanceSettings::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraPerformanceSettings::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraPerformanceSettings*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_BUTTON1:
			((ExtraPerformanceSettings*)context)->ResetDialog (hWnd);
			return;
		case IDC_BUTTON2:
			((ExtraPerformanceSettings*)context)->OpenHelp (hWnd);
			return;
		case IDOK:
			if (((ExtraPerformanceSettings*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraLaunchpadOptions::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraLaunchpadOptions::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_LAUNCHPAD, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraLaunchpadOptions::InitDialog (HWND hWnd)
#else // __linux__
void ExtraLaunchpadOptions::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

#ifndef __linux__
void ExtraLaunchpadOptions::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraLaunchpadOptions::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

#ifndef __linux__
void ExtraLaunchpadOptions::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
#else // __linux__
void ExtraLaunchpadOptions::SetDialog (QWidget *hWnd, const CFG_DEBUGPRM &prm)
#endif // __linux__
{
	int i;
	for (i = 0; i < 3; i++)
#ifndef __linux__
		SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_SETCHECK, prm.bHtmlScnDesc == i ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, prm.bSaveExitScreen ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
		SetCheck(hWnd, IDC_RADIO1+i, prm.bHtmlScnDesc == i);
	SetCheck(hWnd, IDC_CHECK1, prm.bSaveExitScreen);
#endif // __linux__
}

#ifndef __linux__
bool ExtraLaunchpadOptions::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraLaunchpadOptions::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	int i;
	Config *cfg = pTab->Cfg();
#ifndef __linux__
	cfg->CfgDebugPrm.bSaveExitScreen = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED ? true : false);
#else // __linux__
	cfg->CfgDebugPrm.bSaveExitScreen = (IsChecked(hWnd, IDC_CHECK1) ? true : false);
#endif // __linux__
	for (i = 0; i < 3; i++) {
#ifndef __linux__
		if (SendDlgItemMessage (hWnd, IDC_RADIO1+i, BM_GETCHECK, 0, 0) == BST_CHECKED) {
#else // __linux__
		if (IsChecked(hWnd, IDC_RADIO1+i)) {
#endif // __linux__
			break;
		}
	}
	if (i != cfg->CfgDebugPrm.bHtmlScnDesc) {
		cfg->CfgDebugPrm.bHtmlScnDesc = i;
#ifndef __linux__
		MessageBox (NULL, "You need to restart Orbiter for these changes to take effect.", "Orbiter settings", MB_OK | MB_ICONEXCLAMATION);
#else // __linux__
		QMessageBox::warning (NULL, "Orbiter settings", "You need to restart Orbiter for these changes to take effect.");
#endif // __linux__
	}
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraLaunchpadOptions::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraLaunchpadOptions::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraLaunchpadOptions*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_BUTTON1:
			((ExtraLaunchpadOptions*)context)->ResetDialog (hWnd);
			return;
		//case IDC_BUTTON2:
		//	((ExtraLaunchpadOptions*)context)->OpenHelp (hWnd);
		//	return;
		case IDOK:
			if (((ExtraLaunchpadOptions*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool ExtraLogfileOptions::clbkOpen (HWND hParent)
#else // __linux__
bool ExtraLogfileOptions::clbkOpen (QWidget *hParent)
#endif // __linux__
{
	OpenDialog (hParent, IDD_EXTRA_LOGFILE, DlgProc);
	return true;
}

#ifndef __linux__
void ExtraLogfileOptions::InitDialog (HWND hWnd)
#else // __linux__
void ExtraLogfileOptions::InitDialog (QWidget *hWnd)
#endif // __linux__
{
	SetDialog (hWnd, pTab->Cfg()->CfgDebugPrm);
}

#ifndef __linux__
void ExtraLogfileOptions::ResetDialog (HWND hWnd)
#else // __linux__
void ExtraLogfileOptions::ResetDialog (QWidget *hWnd)
#endif // __linux__
{
	extern CFG_DEBUGPRM CfgDebugPrm_default;
	SetDialog (hWnd, CfgDebugPrm_default);
}

#ifndef __linux__
void ExtraLogfileOptions::SetDialog (HWND hWnd, const CFG_DEBUGPRM &prm)
#else // __linux__
void ExtraLogfileOptions::SetDialog (QWidget *hWnd, const CFG_DEBUGPRM &prm)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage (hWnd, IDC_CHECK1, BM_SETCHECK, prm.bVerboseLog ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hWnd, IDC_CHECK1, prm.bVerboseLog);
#endif // __linux__
}

#ifndef __linux__
bool ExtraLogfileOptions::StoreParams (HWND hWnd)
#else // __linux__
bool ExtraLogfileOptions::StoreParams (QWidget *hWnd)
#endif // __linux__
{
	Config *cfg = pTab->Cfg();
#ifndef __linux__
	cfg->CfgDebugPrm.bVerboseLog = (SendDlgItemMessage (hWnd, IDC_CHECK1, BM_GETCHECK, 0, 0) == BST_CHECKED ? true : false);
#else // __linux__
	cfg->CfgDebugPrm.bVerboseLog = (IsChecked(hWnd, IDC_CHECK1) ? true : false);
#endif // __linux__
	return true;
}

#ifndef __linux__
INT_PTR CALLBACK ExtraLogfileOptions::DlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void ExtraLogfileOptions::DlgProc (QWidget *hWnd, void *context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	((ExtraLogfileOptions*)context)->InitDialog (hWnd);
	// WM_COMMAND
	oapiConnectDlgCommands (hWnd, [hWnd, context](int id, int code, QWidget *hCtrl) {
		switch (id) {
		case IDC_BUTTON1:
			((ExtraLogfileOptions*)context)->ResetDialog (hWnd);
			return;
		//case IDC_BUTTON2:
		//	((ExtraLogfileOptions*)context)->OpenHelp (hWnd);
		//	return;
		case IDOK:
			if (((ExtraLogfileOptions*)context)->StoreParams (hWnd))
				qobject_cast<QDialog*> (hWnd)->done (0);
			break;
		}
	});
	BuiltinLaunchpadItem::DlgProc (hWnd, context);
#endif // __linux__
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

#ifndef __linux__
bool LaunchpadItem::OpenDialog (HINSTANCE hInst, HWND hLaunchpad, int resId, DLGPROC pDlg)
#else // __linux__
bool LaunchpadItem::OpenDialog (void *hInst, QWidget *hLaunchpad, int resId, DLGINIT pDlg)
#endif // __linux__
{
#ifndef __linux__
	DialogBoxParam (hInst, MAKEINTRESOURCE (resId), hLaunchpad, pDlg, (LPARAM)this);
#else // __linux__
	// DialogBoxParam: modal, the item is the context of the set-up function
	QDialog *dlg = qobject_cast<QDialog*> (oapiCreateResDialog (hInst, resId, hLaunchpad));
	if (!dlg) return true;
	if (pDlg) pDlg (dlg, this);
	dlg->exec();
	delete dlg;
#endif // __linux__
	return true;
}

#ifndef __linux__
bool LaunchpadItem::clbkOpen (HWND hLaunchpad)
#else // __linux__
bool LaunchpadItem::clbkOpen (QWidget *hLaunchpad)
#endif // __linux__
{
	return false;
}

int LaunchpadItem::clbkWriteConfig ()
{
	return 0;
}
