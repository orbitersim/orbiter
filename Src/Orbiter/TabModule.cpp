// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ModuleTab class
//=============================================================================

#ifndef __linux__
#include <windows.h>
#endif // !__linux__
#include "Orbiter.h"
#include "Launchpad.h"
#include "TabModule.h"
#include "resource.h"
#ifdef __linux__
#include "ResDialog.h"
#include "Util.h"
#include <QMessageBox>
#include <QPushButton>
#include <QSignalBlocker>
#include <QTreeWidget>
#include <algorithm>
#endif // __linux__
#include <filesystem>
namespace fs = std::filesystem;

using std::max;

extern char DBG_MSG[256];
static int counter = -1;

//-----------------------------------------------------------------------------

orbiter::ModuleTab::ModuleTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
	nmodulerec = 0;
}

//-----------------------------------------------------------------------------

orbiter::ModuleTab::~ModuleTab ()
{
	int i;

	if (nmodulerec) {
		for (i = 0; i < nmodulerec; i++) {
			delete []modulerec[i]->name;
			modulerec[i]->name = NULL;
			if (modulerec[i]->info) {
				delete []modulerec[i]->info;
				modulerec[i]->info = NULL;
			}
			delete modulerec[i];
		}
		delete []modulerec;
		modulerec = NULL;
	}
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_MOD);

#ifndef __linux__
	r_lst0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_TREE)); // REMOVE!
	r_dsc0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_INFO)); // REMOVE!
	r_pane  = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_SPLIT1));
	r_bt0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_BUTTON1));
	r_bt1 = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_BUTTON2));
	r_bt2 = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_DEACTALL));
	splitListDesc.SetHwnd (GetDlgItem (hTab, IDC_MOD_SPLIT1), GetDlgItem (hTab, IDC_MOD_TREE), GetDlgItem (hTab, IDC_MOD_INFO));
#else // __linux__
	r_lst0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_MOD_TREE)); // REMOVE!
	r_dsc0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_MOD_INFO)); // REMOVE!
	r_pane  = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_MOD_SPLIT1));
	r_bt0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_MOD_BUTTON1));
	r_bt1 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_MOD_BUTTON2));
	r_bt2 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_MOD_DEACTALL));
	splitListDesc.SetHwnd (oapiResDlgItem (hTab, IDC_MOD_SPLIT1), oapiResDlgItem (hTab, IDC_MOD_TREE), oapiResDlgItem (hTab, IDC_MOD_INFO));
#endif // __linux__
}

//-----------------------------------------------------------------------------

#ifndef __linux__
BOOL orbiter::ModuleTab::OnInitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	SetWindowLongPtr (GetDlgItem (hTab, IDC_MOD_TREE), GWL_STYLE, TVS_DISABLEDRAGDROP | TVS_SHOWSELALWAYS | TVS_NOTOOLTIPS | WS_BORDER | WS_TABSTOP);
	SetWindowPos (GetDlgItem (hTab, IDC_MOD_TREE), NULL, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOSIZE | SWP_NOZORDER);
#else // __linux__
BOOL orbiter::ModuleTab::OnInitDialog (QWidget *hWnd)
{
	QTreeWidget *hTree = DlgItem<QTreeWidget> (hWnd, IDC_MOD_TREE);
	hTree->setHorizontalScrollBarPolicy (Qt::ScrollBarAlwaysOff); // hack: hide horizontal scroll bar

	// WM_NOTIFY: TVN_SELCHANGED shows the module description
	QObject::connect (hTree, &QTreeWidget::currentItemChanged, hWnd, [hWnd](QTreeWidgetItem *item) {
		MODULEREC* rec = (item ? (MODULEREC*)item->data (0, Qt::UserRole).value<void*>() : NULL);
		if (rec && rec->info)
			oapiSetDlgItemText (hWnd, IDC_MOD_INFO, rec->info);
		else
			oapiSetDlgItemText (hWnd, IDC_MOD_INFO, "");
	});
	// a ticked or unticked item (de)activates its module once the initial ticks are set
	QObject::connect (hTree, &QTreeWidget::itemChanged, hWnd, [this](QTreeWidgetItem *item, int column) {
		if (counter == 4) ActivateFromList ();
	});

	// WM_COMMAND
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_MOD_DEACTALL), &QPushButton::clicked, hWnd, [this]() { DeactivateAll (); });
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_MOD_BUTTON1), &QPushButton::clicked, hWnd, [this]() { ExpandCollapseAll (true); });
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_MOD_BUTTON2), &QPushButton::clicked, hWnd, [this]() { ExpandCollapseAll (false); });
#endif // __linux__

	return FALSE;
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::GetConfig (const Config *cfg)
{
	RefreshLists();
#ifndef __linux__
	SetWindowText (GetDlgItem (hTab, IDC_MOD_INFO), "Optional Orbiter plugin modules.\r\n\r\nDouble-click on a category to show or hide its entries.\r\n\r\nCheck or uncheck items to activate the corresponding modules.\r\n\r\nSelect an item to see a description of the module function.");
#else // __linux__
	InitActivation(); // the ticks can be set right away (Win32 needed a deferred WM_USER for this)
	counter = 4;
	oapiSetDlgItemText (hTab, IDC_MOD_INFO, "Optional Orbiter plugin modules.\r\n\r\nDouble-click on a category to show or hide its entries.\r\n\r\nCheck or uncheck items to activate the corresponding modules.\r\n\r\nSelect an item to see a description of the module function.");
#endif // __linux__
	int listw = cfg->CfgWindowPos.LaunchpadModListWidth;
	if (!listw) {
#ifndef __linux__
		RECT r;
		GetClientRect (GetDlgItem (hTab, IDC_MOD_TREE), &r);
		listw = r.right;
#else // __linux__
		listw = oapiResDlgItem (hTab, IDC_MOD_TREE)->width();
#endif // __linux__
	}
	splitListDesc.SetStaticPane (SplitterCtrl::PANE1, listw);
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::SetConfig (Config *cfg)
{
	cfg->CfgWindowPos.LaunchpadModListWidth = splitListDesc.GetPaneWidth (SplitterCtrl::PANE1);
}

//-----------------------------------------------------------------------------

bool orbiter::ModuleTab::OpenHelp ()
{
	OpenTabHelp ("tab_modules");
	return true;
}

//-----------------------------------------------------------------------------

BOOL orbiter::ModuleTab::OnSize (int w, int h)
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

#ifndef __linux__
	SetWindowPos (GetDlgItem (hTab, IDC_MOD_SPLIT1), NULL,
		0, 0, w0+dw, h0+dh,
		SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_MOD_BUTTON1), NULL,
		r_bt0.left, r_bt0.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_MOD_BUTTON2), NULL,
		r_bt1.left, r_bt1.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_MOD_DEACTALL), NULL,
		r_bt2.left, r_bt2.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);
#else // __linux__
	oapiResDlgItem (hTab, IDC_MOD_SPLIT1)->resize (w0+dw, h0+dh);
	oapiResDlgItem (hTab, IDC_MOD_BUTTON1)->move (r_bt0.left, r_bt0.top+dh);
	oapiResDlgItem (hTab, IDC_MOD_BUTTON2)->move (r_bt1.left, r_bt1.top+dh);
	oapiResDlgItem (hTab, IDC_MOD_DEACTALL)->move (r_bt2.left, r_bt2.top+dh);
#endif // __linux__

#ifndef __linux__
	return NULL;
#else // __linux__
	return FALSE;
#endif // __linux__
}

//-----------------------------------------------------------------------------

void orbiter::ModuleTab::Show ()
{
	LaunchpadTab::Show();
}

//-----------------------------------------------------------------------------
#ifndef __linux__
void orbiter::ModuleTab::RefreshLists ()
#else // __linux__
// TVI_SORT: position of a new item among the children of parent (the top level if parent is NULL)
static int SortedIndex (QTreeWidget *hTree, QTreeWidgetItem *parent, const char *text)
#endif // __linux__
{
#ifndef __linux__
	HWND hTree = GetDlgItem (hTab, IDC_MOD_TREE);
	TreeView_DeleteAllItems (hTree);
#else // __linux__
	int n = (parent ? parent->childCount() : hTree->topLevelItemCount());
	QString s = QString::fromUtf8 (text);
	for (int i = 0; i < n; i++) {
		QTreeWidgetItem *it = (parent ? parent->child (i) : hTree->topLevelItem (i));
		if (QString::compare (s, it->text (0), Qt::CaseInsensitive) < 0) return i;
	}
	return n;
}
#endif // __linux__

#ifndef __linux__
	TV_INSERTSTRUCT tvis;
	tvis.item.mask = TVIF_TEXT | TVIF_PARAM;
#else // __linux__
void orbiter::ModuleTab::RefreshLists ()
{
	QTreeWidget *hTree = DlgItem<QTreeWidget> (hTab, IDC_MOD_TREE);
	QSignalBlocker block (hTree);
	hTree->clear();
#endif // __linux__

#ifndef __linux__
	int idx, len;
#else // __linux__
	int len;
#endif // __linux__
	char catstr[256];

#ifndef __linux__
	const fs::path moddir{ "Modules/Plugin" };
#else // __linux__
	const fs::path moddir{ oapiResolvePath ("Modules/Plugin") };
#endif // __linux__

#ifndef __linux__
	for (const auto& file : fs::directory_iterator(moddir)) {
		if (file.path().extension().string() == ".dll") {
#else // __linux__
	std::error_code ec;
	for (const auto& file : fs::directory_iterator(moddir, ec)) {
		if (file.path().extension().string() == ".so") {
#endif // __linux__
			auto name = file.path().filename().string();
			// add module record
			MODULEREC** tmp = new MODULEREC * [nmodulerec + 1];
			if (nmodulerec) {
				memcpy(tmp, modulerec, nmodulerec * sizeof(MODULEREC*));
				delete[]modulerec;
			}
			modulerec = tmp;

			MODULEREC* rec = modulerec[nmodulerec++] = new MODULEREC;
#ifndef __linux__
			len = name.length() - 4;
#else // __linux__
			len = name.length() - 3;
#endif // __linux__
			rec->name = new char[len + 1];
			strncpy(rec->name, name.c_str(), len);
			rec->name[len] = '\0';
			rec->info = 0;
			rec->active = false;
			rec->locked = false;

			// check if module is set active in config
			if (pCfg->IsActiveModule(rec->name))
				rec->active = true;

			// check if module is set active in command line
			if (std::find(pCfg->CfgCmdlinePrm.LoadPlugins.begin(), pCfg->CfgCmdlinePrm.LoadPlugins.end(), rec->name) != pCfg->CfgCmdlinePrm.LoadPlugins.end()) {
				rec->active = true;
				rec->locked = true; // modules activated from the command line are not to be unloaded
			}

#ifndef __linux__
			HMODULE hMod = LoadLibraryEx((moddir / name).string().c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
			if (hMod) {
				char buf[1024];
				// read module info string
				if (LoadString(hMod, 1000, buf, 1024)) {
					buf[1023] = '\0';
					rec->info = new char[strlen(buf) + 1];
					strcpy(rec->info, buf);
				}
				// read category string
				if (LoadString(hMod, 1001, buf, 1024)) {
					strncpy(catstr, buf, 255);
					catstr[255] = '\0';
				}
				else {
					strcpy(catstr, "Miscellaneous");
				}
				FreeLibrary(hMod);
#else // __linux__
			// read the module's strings from the file, without loading the module
			std::string modfile = (moddir / name).string();
			char buf[1024];
			// read module info string
			if (LoadModuleString(modfile.c_str(), 1000, buf, 1024)) {
				buf[1023] = '\0';
				rec->info = new char[strlen(buf) + 1];
				strcpy(rec->info, buf);
			}
			// read category string
			if (LoadModuleString(modfile.c_str(), 1001, buf, 1024)) {
				strncpy(catstr, buf, 255);
				catstr[255] = '\0';
			}
			else {
				strcpy(catstr, "Miscellaneous");
#endif // __linux__
			}

			if (!strcmp(catstr, "Graphics engines"))
				continue; // graphics client modules are loaded via the Video tab

			// find the category entry
#ifndef __linux__
			HTREEITEM catItem = GetCategoryItem(catstr);
#else // __linux__
			QTreeWidgetItem *catItem = GetCategoryItem(catstr);
#endif // __linux__

#ifndef __linux__
			// tree view entry
			tvis.item.pszText = rec->name;
			tvis.item.lParam = (LPARAM)rec;
			tvis.hInsertAfter = TVI_SORT;
			tvis.hParent = catItem;
			HTREEITEM hti = TreeView_InsertItem(hTree, &tvis);
#else // __linux__
			// tree view entry (checkable; TVI_SORT)
			QTreeWidgetItem *hti = new QTreeWidgetItem();
			hti->setText(0, QString::fromUtf8(rec->name));
			hti->setData(0, Qt::UserRole, QVariant::fromValue((void*)rec));
			hti->setFlags(hti->flags() | Qt::ItemIsUserCheckable);
			hti->setCheckState(0, Qt::Unchecked);
			catItem->insertChild(SortedIndex(hTree, catItem, rec->name), hti);
#endif // __linux__
		}
	}
	counter = 0;
}

#ifndef __linux__
HTREEITEM orbiter::ModuleTab::GetCategoryItem (char *cat)
#else // __linux__
QTreeWidgetItem *orbiter::ModuleTab::GetCategoryItem (char *cat)
#endif // __linux__
{
#ifndef __linux__
	HWND hTree = GetDlgItem (hTab, IDC_MOD_TREE);
	HTREEITEM root = TreeView_GetRoot (hTree);
	char cbuf[256];
	TVITEM item;
	item.mask = TVIF_TEXT;
	item.pszText = cbuf;
	item.cchTextMax = 256;
	item.hItem = root;

	while (TreeView_GetItem (hTree, &item)) {
		if (!strcmp (cat, cbuf)) return item.hItem;
		item.hItem = TreeView_GetNextSibling (hTree, item.hItem);
	}
	// not found - create new category item
	TV_INSERTSTRUCT tvis;
	tvis.item.mask = TVIF_TEXT | TVIF_PARAM;
	tvis.item.pszText = cat;
	tvis.item.lParam = NULL;
	tvis.hInsertAfter = TVI_SORT;
	tvis.hParent = NULL;
	return TreeView_InsertItem (hTree, &tvis);
#else // __linux__
	QTreeWidget *hTree = DlgItem<QTreeWidget> (hTab, IDC_MOD_TREE);
	for (int i = 0; i < hTree->topLevelItemCount(); i++) {
		QTreeWidgetItem *item = hTree->topLevelItem (i);
		if (!strcmp (cat, item->text (0).toUtf8().constData())) return item;
	}
	// not found - create new category item (no check box; TVI_SORT)
	QTreeWidgetItem *item = new QTreeWidgetItem();
	item->setText (0, QString::fromUtf8 (cat));
	item->setData (0, Qt::UserRole, QVariant::fromValue ((void*)NULL));
	hTree->insertTopLevelItem (SortedIndex (hTree, NULL, cat), item);
	return item;
#endif // __linux__
}

void orbiter::ModuleTab::ExpandCollapseAll (bool expand)
{
#ifndef __linux__
	HWND hTree = GetDlgItem (hTab, IDC_MOD_TREE);
	UINT code = (expand ? TVE_EXPAND : TVE_COLLAPSE);
	TVITEM catitem;
	catitem.mask = NULL;
	catitem.hItem = TreeView_GetRoot (hTree);
	while (TreeView_GetItem (hTree, &catitem)) {
		TreeView_Expand (hTree, catitem.hItem, code);
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
	}
#else // __linux__
	QTreeWidget *hTree = DlgItem<QTreeWidget> (hTab, IDC_MOD_TREE);
	for (int i = 0; i < hTree->topLevelItemCount(); i++)
		hTree->topLevelItem (i)->setExpanded (expand);
#endif // __linux__
}

void orbiter::ModuleTab::InitActivation ()
{
#ifndef __linux__
	HWND hTree = GetDlgItem (hTab, IDC_MOD_TREE);
	TVITEM catitem, subitem;
	catitem.mask = TVIF_PARAM;
	HTREEITEM hRoot = TreeView_GetRoot (hTree);
	catitem.hItem = hRoot;
	subitem.mask = TVIF_PARAM;
#else // __linux__
	QTreeWidget *hTree = DlgItem<QTreeWidget> (hTab, IDC_MOD_TREE);
	QSignalBlocker block (hTree);
#endif // __linux__

	// tick the active modules
#ifndef __linux__
	while (TreeView_GetItem (hTree, &catitem)) {
		subitem.hItem = TreeView_GetChild (hTree, catitem.hItem);
		while (TreeView_GetItem (hTree, &subitem)) {
			MODULEREC *rec = (MODULEREC*)subitem.lParam;
#else // __linux__
	for (int i = 0; i < hTree->topLevelItemCount(); i++) {
		QTreeWidgetItem *catitem = hTree->topLevelItem (i);
		for (int j = 0; j < catitem->childCount(); j++) {
			QTreeWidgetItem *subitem = catitem->child (j);
			MODULEREC *rec = (MODULEREC*)subitem->data (0, Qt::UserRole).value<void*>();
#endif // __linux__
			if (rec->active) {
#ifndef __linux__
				TreeView_SetCheckState (hTree, subitem.hItem, TRUE);
#else // __linux__
				subitem->setCheckState (0, Qt::Checked);
#endif // __linux__
			}
#ifndef __linux__
			subitem.hItem = TreeView_GetNextSibling (hTree, subitem.hItem);
#endif // !__linux__
		}
#ifndef __linux__
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
#endif // !__linux__
	}

#ifndef __linux__
	// remove check boxes from categories
	catitem.hItem = hRoot;
	while (TreeView_GetItem (hTree, &catitem)) {
		TreeView_SetItemState (hTree, catitem.hItem, 0, TVIS_STATEIMAGEMASK);
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
	}
#else // __linux__
	// categories have no check boxes (they are created without)
#endif // __linux__

	ExpandCollapseAll (true);
}

void orbiter::ModuleTab::ActivateFromList ()
{
#ifndef __linux__
	const char *path = "Modules\\Plugin";
#else // __linux__
	const char *path = "Modules/Plugin";

	QTreeWidget *hTree = DlgItem<QTreeWidget> (hTab, IDC_MOD_TREE);
#endif // __linux__

#ifndef __linux__
	HWND hTree = GetDlgItem (hTab, IDC_MOD_TREE);
	TVITEM catitem, subitem;
	catitem.mask = TVIF_PARAM;
	catitem.hItem = TreeView_GetRoot (hTree);
	subitem.mask = TVIF_PARAM;

	while (TreeView_GetItem (hTree, &catitem)) {
		subitem.hItem = TreeView_GetChild (hTree, catitem.hItem);
		while (TreeView_GetItem (hTree, &subitem)) {
			MODULEREC *rec = (MODULEREC*)subitem.lParam;
			bool checked = (TreeView_GetCheckState (hTree, subitem.hItem) != 0);
#else // __linux__
	for (int i = 0; i < hTree->topLevelItemCount(); i++) {
		QTreeWidgetItem *catitem = hTree->topLevelItem (i);
		for (int j = 0; j < catitem->childCount(); j++) {
			QTreeWidgetItem *subitem = catitem->child (j);
			MODULEREC *rec = (MODULEREC*)subitem->data (0, Qt::UserRole).value<void*>();
			bool checked = (subitem->checkState (0) != Qt::Unchecked);
#endif // __linux__
			if (checked != rec->active) {
				if (!rec->locked) {
					rec->active = checked;
					if (checked) {
						pCfg->AddActiveModule(rec->name);
						pLp->App()->LoadModule(path, rec->name);
					}
					else {
						pCfg->DelActiveModule(rec->name);
						pLp->App()->UnloadModule(rec->name);
					}
				}
				else {
#ifndef __linux__
					TreeView_SetCheckState(hTree, subitem.hItem, rec->active ? TRUE : FALSE);
					MessageBox(NULL, "This module has been requested on the command line and cannot be deactivated interactively.", "Orbiter: Plugin Modules", MB_ICONWARNING | MB_OK);
#else // __linux__
					{
						QSignalBlocker block (hTree);
						subitem->setCheckState(0, rec->active ? Qt::Checked : Qt::Unchecked);
					}
					QMessageBox::warning(NULL, "Orbiter: Plugin Modules", "This module has been requested on the command line and cannot be deactivated interactively.");
#endif // __linux__
				}
			}
#ifndef __linux__
			subitem.hItem = TreeView_GetNextSibling (hTree, subitem.hItem);
#endif // !__linux__
		}
#ifndef __linux__
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
#endif // !__linux__
	}
}

void orbiter::ModuleTab::DeactivateAll ()
{
#ifndef __linux__
	HWND hTree = GetDlgItem (hTab, IDC_MOD_TREE);
	TVITEM catitem, subitem;
	catitem.mask = NULL;
	catitem.hItem = TreeView_GetRoot (hTree);
	subitem.mask = NULL;

	while (TreeView_GetItem (hTree, &catitem)) {
		subitem.hItem = TreeView_GetChild (hTree, catitem.hItem);
		while (TreeView_GetItem (hTree, &subitem)) {
			TreeView_SetCheckState (hTree, subitem.hItem, FALSE);
			subitem.hItem = TreeView_GetNextSibling (hTree, subitem.hItem);
#else // __linux__
	QTreeWidget *hTree = DlgItem<QTreeWidget> (hTab, IDC_MOD_TREE);
	{
		QSignalBlocker block (hTree);
		for (int i = 0; i < hTree->topLevelItemCount(); i++) {
			QTreeWidgetItem *catitem = hTree->topLevelItem (i);
			for (int j = 0; j < catitem->childCount(); j++)
				catitem->child (j)->setCheckState (0, Qt::Unchecked);
#endif // __linux__
		}
#ifndef __linux__
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
#endif // !__linux__
	}
	ActivateFromList ();
}

#ifndef __linux__
//-----------------------------------------------------------------------------

BOOL orbiter::ModuleTab::OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh)
{
	if (idCtrl == IDC_MOD_TREE) {
		NM_TREEVIEW* pnmtv = (NM_TREEVIEW FAR*)pnmh;
		switch (pnmtv->hdr.code) {
		case TVN_SELCHANGED: {
			TVITEM item = pnmtv->itemNew;
			MODULEREC* rec = (MODULEREC*)item.lParam;
			if (rec && rec->info)
				SetWindowText(GetDlgItem(hDlg, IDC_MOD_INFO), rec->info);
			else
				SetWindowText(GetDlgItem(hDlg, IDC_MOD_INFO), "");
		} return TRUE;
		case NM_CUSTOMDRAW:
			// this is a terrible hack to set the initial activation ticks,
			// because for an unknown reason, setting the check state of the
			// tree items during creation gets undone halfway through the
			// initialisation process
			if (counter >= 0 && counter < 4) {
				if (counter == 2) PostMessage(hDlg, WM_USER, 0, 0);
				counter++;
			}
			else if (counter == 4) {
				ActivateFromList();
			}
			return 0;
		}
	}
	return FALSE;
}

//-----------------------------------------------------------------------------

BOOL orbiter::ModuleTab::OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	const int MAXSEL = 100;
	int i;
	NM_TREEVIEW *pnmtv;

	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_MOD_DEACTALL:
			DeactivateAll ();
			return TRUE;
		case IDC_MOD_BUTTON1:
			ExpandCollapseAll (true);
			return TRUE;
		case IDC_MOD_BUTTON2:
			ExpandCollapseAll (false);
			return TRUE;
		}
		break;
	case WM_USER:
		InitActivation();

		// hack: hide horizontal scroll bar
		LONG style = GetWindowLongPtr (GetDlgItem (hTab, IDC_MOD_TREE), GWL_STYLE);
		SetWindowLongPtr (GetDlgItem (hTab, IDC_MOD_TREE), GWL_STYLE, style & ~WS_HSCROLL);
		SetWindowPos (GetDlgItem (hTab, IDC_MOD_TREE), NULL, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOSIZE | SWP_NOZORDER);

		return 0;
	}
	return NULL;
}
#else // __linux__
// WM_NOTIFY (TVN_SELCHANGED, check box changes) and WM_COMMAND are connected in OnInitDialog
#endif // __linux__
