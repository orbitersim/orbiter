// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ModuleTab class
//=============================================================================

#include <windows.h>
#include <commctrl.h>
#include <io.h>
#include "Orbiter.h"
#include "Launchpad.h"
#include "TabModule.h"
#include "Help.h"
#include "resource.h"

extern char DBG_MSG[256];
static int counter = -1;

//-----------------------------------------------------------------------------

ModuleTab::ModuleTab (const MainDialog *lp): LaunchpadTab (lp)
{
	nmodulerec = 0;
}

//-----------------------------------------------------------------------------

ModuleTab::~ModuleTab ()
{
	int i;

	if (nmodulerec) {
		for (i = 0; i < nmodulerec; i++) {
			delete []modulerec[i]->name;
			if (modulerec[i]->info)
				delete []modulerec[i]->info;
			delete modulerec[i];
		}
		delete []modulerec;
	}
}

//-----------------------------------------------------------------------------

void ModuleTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_MOD);

	r_lst0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_TREE1)); // REMOVE!
	r_dsc0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_INFO)); // REMOVE!
	r_pane  = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_SPLIT1));
	r_bt0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_BUTTON1));
	r_bt1 = GetClientPos (hTab, GetDlgItem (hTab, IDC_BUTTON2));
	r_bt2 = GetClientPos (hTab, GetDlgItem (hTab, IDC_MOD_DEACTALL));
	splitListDesc.SetHwnd (GetDlgItem (hTab, IDC_MOD_SPLIT1), GetDlgItem (hTab, IDC_TREE1), GetDlgItem (hTab, IDC_MOD_INFO));
}

//-----------------------------------------------------------------------------

BOOL ModuleTab::InitDialog (HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	SetWindowLongPtr (GetDlgItem (hTab, IDC_TREE1), GWL_STYLE, TVS_DISABLEDRAGDROP | TVS_SHOWSELALWAYS | TVS_NOTOOLTIPS | WS_BORDER | WS_TABSTOP);
	SetWindowPos (GetDlgItem (hTab, IDC_TREE1), NULL, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOSIZE | SWP_NOZORDER);

	return FALSE;
}

//-----------------------------------------------------------------------------

void ModuleTab::GetConfig (const Config *cfg)
{
	RefreshLists();
	SetWindowText (GetDlgItem (hTab, IDC_MOD_INFO), "Optional Orbiter plugin modules.\r\n\r\nDouble-click on a category to show or hide its entries.\r\n\r\nCheck or uncheck items to activate the corresponding modules.\r\n\r\nSelect an item to see a description of the module function.");
	int listw = cfg->CfgWindowPos.LaunchpadModListWidth;
	if (!listw) {
		RECT r;
		GetClientRect (GetDlgItem (hTab, IDC_TREE1), &r);
		listw = r.right;
	}
	splitListDesc.SetStaticPane (SplitterCtrl::PANE1, listw);
}

//-----------------------------------------------------------------------------

void ModuleTab::SetConfig (Config *cfg)
{
	cfg->CfgWindowPos.LaunchpadModListWidth = splitListDesc.GetPaneWidth (SplitterCtrl::PANE1);
}

//-----------------------------------------------------------------------------

bool ModuleTab::OpenHelp ()
{
	OpenDefaultHelp (pLp->GetWindow(), pLp->GetInstance(), "tab_modules");
	return true;
}

//-----------------------------------------------------------------------------

BOOL ModuleTab::Size (int w, int h)
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

	SetWindowPos (GetDlgItem (hTab, IDC_MOD_SPLIT1), NULL,
		0, 0, w0+dw, h0+dh,
		SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_BUTTON1), NULL,
		r_bt0.left, r_bt0.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_BUTTON2), NULL,
		r_bt1.left, r_bt1.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_MOD_DEACTALL), NULL,
		r_bt2.left, r_bt2.top+dh, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER);

	return NULL;
}

//-----------------------------------------------------------------------------

void ModuleTab::Show ()
{
	LaunchpadTab::Show();
}

//-----------------------------------------------------------------------------

void ModuleTab::RefreshLists ()
{
	HWND hTree = GetDlgItem (hTab, IDC_TREE1);
	TreeView_DeleteAllItems (hTree);

	TV_INSERTSTRUCT tvis;
	tvis.item.mask = TVIF_TEXT | TVIF_PARAM;

	int idx, len;
	char cbuf[256], catstr[256];
	struct _finddata_t fdata;
	intptr_t fh = _findfirst ("Modules\\Plugin\\*.dll", &fdata);
	if (fh == -1) return; // no files found
	SendDlgItemMessage (hTab, IDC_MOD_ACTLIST, LB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_MOD_INACTLIST, LB_RESETCONTENT, 0, 0);
	do {
		// add module record
		MODULEREC **tmp = new MODULEREC*[nmodulerec+1];
		if (nmodulerec) {
			memcpy (tmp, modulerec, nmodulerec*sizeof(MODULEREC*));
			delete []modulerec;
		}
		modulerec = tmp;

		MODULEREC *rec = modulerec[nmodulerec++] = new MODULEREC;
		len = strlen(fdata.name)-4;
		rec->name = new char[len+1];
		strncpy (rec->name, fdata.name, len);
		rec->name[len] = '\0';
		rec->info = 0;
		rec->active = false;

		// check if module is set active in config
		for (idx = pCfg->nactmod-1; idx >= 0; idx--)
			if (!_stricmp (rec->name, pCfg->actmod[idx])) {
				rec->active = true;
				break;
			}

		sprintf (cbuf, "Modules\\Plugin\\%s", fdata.name);
		HMODULE hMod = LoadLibraryEx (cbuf, 0, LOAD_LIBRARY_AS_DATAFILE);
		if (hMod) {
			char buf[1024];
			// read module info string
			if (LoadString (hMod, 1000, buf, 1024)) {
				buf[1023] = '\0';
				rec->info = new char[strlen(buf)+1];
				strcpy (rec->info, buf);
			}
			// read category string
			if (LoadString (hMod, 1001, buf, 1024)) {
				strncpy (catstr, buf, 255);
			} else {
				strcpy (catstr, "Miscellaneous");
			}
			FreeLibrary (hMod);
		}

#ifdef INLINEGRAPHICS
		if (!strcmp (catstr, "Graphics engines"))
			continue; // don't display graphics client modules in orbiter.exe
#endif

		// find the category entry
		HTREEITEM catItem = GetCategoryItem (catstr);

		// tree view entry
		tvis.item.pszText = rec->name;
		tvis.item.lParam = (LPARAM)rec;
		tvis.hInsertAfter = TVI_SORT;
		tvis.hParent = catItem;
		HTREEITEM hti = TreeView_InsertItem (hTree, &tvis);

	} while (!_findnext (fh, &fdata));
	_findclose (fh);
	counter = 0;
}

HTREEITEM ModuleTab::GetCategoryItem (char *cat)
{
	HWND hTree = GetDlgItem (hTab, IDC_TREE1);
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
}

void ModuleTab::ExpandCollapseAll (bool expand)
{
	HWND hTree = GetDlgItem (hTab, IDC_TREE1);
	UINT code = (expand ? TVE_EXPAND : TVE_COLLAPSE);
	TVITEM catitem;
	catitem.mask = NULL;
	catitem.hItem = TreeView_GetRoot (hTree);
	while (TreeView_GetItem (hTree, &catitem)) {
		TreeView_Expand (hTree, catitem.hItem, code);
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
	}
}

void ModuleTab::InitActivation ()
{
	HWND hTree = GetDlgItem (hTab, IDC_TREE1);
	TVITEM catitem, subitem;
	catitem.mask = TVIF_PARAM;
	HTREEITEM hRoot = TreeView_GetRoot (hTree);
	catitem.hItem = hRoot;
	subitem.mask = TVIF_PARAM;

	// tick the active modules
	while (TreeView_GetItem (hTree, &catitem)) {
		subitem.hItem = TreeView_GetChild (hTree, catitem.hItem);
		while (TreeView_GetItem (hTree, &subitem)) {
			MODULEREC *rec = (MODULEREC*)subitem.lParam;
			if (rec->active) {
				TreeView_SetCheckState (hTree, subitem.hItem, TRUE);
			}
			subitem.hItem = TreeView_GetNextSibling (hTree, subitem.hItem);
		}
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
	}

	// remove check boxes from categories
	catitem.hItem = hRoot;
	while (TreeView_GetItem (hTree, &catitem)) {
		TreeView_SetItemState (hTree, catitem.hItem, 0, TVIS_STATEIMAGEMASK);
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
	}

	ExpandCollapseAll (true);
}

void ModuleTab::ActivateFromList ()
{
	const char *path = "Modules\\Plugin";

	HWND hTree = GetDlgItem (hTab, IDC_TREE1);
	TVITEM catitem, subitem;
	catitem.mask = TVIF_PARAM;
	catitem.hItem = TreeView_GetRoot (hTree);
	subitem.mask = TVIF_PARAM;

	while (TreeView_GetItem (hTree, &catitem)) {
		subitem.hItem = TreeView_GetChild (hTree, catitem.hItem);
		while (TreeView_GetItem (hTree, &subitem)) {
			MODULEREC *rec = (MODULEREC*)subitem.lParam;
			bool checked = (TreeView_GetCheckState (hTree, subitem.hItem) != 0);
			if (checked != rec->active) {
				rec->active = checked;
				if (checked) {
					pCfg->AddModule(rec->name);
					pLp->App()->LoadModule (path, rec->name);
				} else {
					pCfg->DelModule(rec->name);
					pLp->App()->UnloadModule (rec->name);
				}
			}
			subitem.hItem = TreeView_GetNextSibling (hTree, subitem.hItem);
		}
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
	}
}

void ModuleTab::DeactivateAll ()
{
	HWND hTree = GetDlgItem (hTab, IDC_TREE1);
	TVITEM catitem, subitem;
	catitem.mask = NULL;
	catitem.hItem = TreeView_GetRoot (hTree);
	subitem.mask = NULL;

	while (TreeView_GetItem (hTree, &catitem)) {
		subitem.hItem = TreeView_GetChild (hTree, catitem.hItem);
		while (TreeView_GetItem (hTree, &subitem)) {
			TreeView_SetCheckState (hTree, subitem.hItem, FALSE);
			subitem.hItem = TreeView_GetNextSibling (hTree, subitem.hItem);
		}
		catitem.hItem = TreeView_GetNextSibling (hTree, catitem.hItem);
	}
	ActivateFromList ();
}

//-----------------------------------------------------------------------------
// Name: ModuleActivate()
// Desc: Move modules between active/inactive lists
//-----------------------------------------------------------------------------
void ModuleTab::ModuleActivate (int idx, bool act)
{
	int fromlist, tolist, idx0, idx1, i, j;
	char cbuf[64];
	const char *path = "Modules\\Plugin";

	if (act) // activate
		fromlist = IDC_MOD_INACTLIST, tolist = IDC_MOD_ACTLIST;
	else     // deactivate
		fromlist = IDC_MOD_ACTLIST, tolist = IDC_MOD_INACTLIST;

	if (idx >= 0)
		idx0 = idx, idx1 = idx+1;
	else
		idx0 = 0, idx1 = SendDlgItemMessage (hTab, fromlist, LB_GETCOUNT, 0, 0);

	for (i = idx1-1; i >= idx0; i--)
		if (SendDlgItemMessage (hTab, fromlist, LB_GETTEXT, i, (LPARAM)cbuf) != LB_ERR) {
			char *info = (char*)SendDlgItemMessage (hTab, fromlist, LB_GETITEMDATA, i, 0);
			SendDlgItemMessage (hTab, fromlist, LB_DELETESTRING, i, 0);
			j = SendDlgItemMessage (hTab, tolist, LB_ADDSTRING, 0, (LPARAM)cbuf);
			SendDlgItemMessage (hTab, tolist, LB_SETITEMDATA, j, (LPARAM)info);
			if (act) pCfg->AddModule (cbuf), pLp->App()->LoadModule (path, cbuf);
			else     pCfg->DelModule (cbuf), pLp->App()->UnloadModule (cbuf);
		}
}

//-----------------------------------------------------------------------------

BOOL ModuleTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	const int MAXSEL = 100;
	int i;
	NM_TREEVIEW *pnmtv;

	// debug only
	static int count = 0;
	static bool active = false;
	static UINT lastmsg = 0;
	HTREEITEM hitem = TreeView_GetRoot(GetDlgItem (hTab, IDC_TREE1));
	if (hitem) {
		TVITEM item;
		item.mask = TVIF_STATE;
		item.hItem = hitem;
		if (TreeView_GetItem (GetDlgItem (hTab, IDC_TREE1), &item)) {
			if (count == 32)
				i = count;
			if ((item.state & TVIS_STATEIMAGEMASK) == 8192) {
				active = true;
				lastmsg = uMsg;
			} else if (active) {
				i = count;
			}
			if (active) count++;
		}
	}

	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_MOD_DEACTALL:
			DeactivateAll ();
			return TRUE;
		case IDC_BUTTON1:
			ExpandCollapseAll (true);
			return TRUE;
		case IDC_BUTTON2:
			ExpandCollapseAll (false);
			return TRUE;
		}
		break;
	case WM_NOTIFY:
		switch (LOWORD(wParam)) {
		case IDC_TREE1:

			// debug
			HTREEITEM hti = TreeView_GetRoot (GetDlgItem (hTab, IDC_TREE1));
			hti = TreeView_GetChild (GetDlgItem (hTab, IDC_TREE1), hti);
			BOOL isChecked = TreeView_GetCheckState (GetDlgItem (hTab, IDC_TREE1), hti);

			pnmtv = (NM_TREEVIEW FAR *)lParam;
			switch (pnmtv->hdr.code) {
			case TVN_SELCHANGED: {
				TVITEM item = pnmtv->itemNew;
				MODULEREC *rec = (MODULEREC*)item.lParam;
				if (rec && rec->info) {
					SetWindowText (GetDlgItem (hWnd, IDC_MOD_INFO), rec->info);
				} else {
					SetWindowText (GetDlgItem (hWnd, IDC_MOD_INFO), "");
				}
				} return TRUE;
			case NM_CUSTOMDRAW:
				// this is a terrible hack to set the initial activation ticks,
				// because for an unknown reason, setting the check state of the
				// tree items during creation gets undone halfway through the
				// initialisation process
				if (counter >= 0 && counter < 4) {
					if (counter == 2) PostMessage (hWnd, WM_USER, 0, 0);
					counter++;
				} else if (counter == 4) {
					ActivateFromList();
				}
				return 0;
			}
			break;
		}
		break;
	case WM_USER:
		InitActivation();

		// hack: hide horizontal scroll bar
		LONG style = GetWindowLongPtr (GetDlgItem (hTab, IDC_TREE1), GWL_STYLE);
		SetWindowLongPtr (GetDlgItem (hTab, IDC_TREE1), GWL_STYLE, style & ~WS_HSCROLL);
		SetWindowPos (GetDlgItem (hTab, IDC_TREE1), NULL, 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER | SWP_NOSIZE | SWP_NOZORDER);

		return 0;
	}
	return NULL;
}