// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ScenarioTab class
//=============================================================================

#ifndef __linux__
#include <windows.h>
#include <direct.h>
#else // __linux__
#include <unistd.h>
#endif // __linux__
#include <string>
#ifdef __linux__
#include <strings.h>
#endif // __linux__
#include "Orbiter.h"
#include "TabScenario.h"
#include "Launchpad.h"
//#include "Log.h"
#include "Help.h"
#include "htmlctrl.h"
#include "resource.h"
#ifdef __linux__
#include "ResDialog.h"
#include "Util.h"
#include <QCheckBox>
#include <QDialog>
#include <QFileSystemWatcher>
#include <QIcon>
#include <QLineEdit>
#include <QMessageBox>
#include <QPixmap>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSignalBlocker>
#include <QTreeWidget>
#endif // __linux__

using namespace std;

#ifndef __linux__
extern const TCHAR* CurrentScenario;
#else // __linux__
extern const char* CurrentScenario;
#endif // __linux__
const char *htmlstyle = "<style type=""text/css"">body{font-family:Arial;font-size:12px} p{margin-top:0;margin-bottom:0.5em} h1{font-size:150%;font-weight:normal;margin-bottom:0.5em;color:#000080;background-color:#E6E6FF;padding:0.1em}</style>";

//-----------------------------------------------------------------------------

#ifdef __linux__
static QPixmap TreeIcon (void *hInst, int resId)
{
	QImage *img = oapiLoadResImage (hInst, resId);
	oapiClearImageBackground (img); // not upstream: the white surround shows on a dark desktop theme; Windows' tree is always white
	QPixmap pm = (img ? QPixmap::fromImage (*img) : QPixmap());
	delete img;
	return pm;
}

#endif // __linux__
orbiter::ScenarioTab::ScenarioTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
#ifndef __linux__
	imglist = ImageList_Create (16, 16, ILC_COLOR8, 4, 0);
	treeicon_idx[0] = ImageList_Add (imglist, LoadBitmap (AppInstance(), MAKEINTRESOURCE (IDB_TREEICON_FOLDER1)), 0);
	treeicon_idx[1] = ImageList_Add (imglist, LoadBitmap (AppInstance(), MAKEINTRESOURCE (IDB_TREEICON_FOLDER2)), 0);
	treeicon_idx[2] = ImageList_Add (imglist, LoadBitmap (AppInstance(), MAKEINTRESOURCE (IDB_TREEICON_SCN1)), 0);
	treeicon_idx[3] = ImageList_Add (imglist, LoadBitmap (AppInstance(), MAKEINTRESOURCE (IDB_TREEICON_SCN2)), 0);
#else // __linux__
	// folders show the same image when selected; scenarios switch to their selected image
	treeicon[0] = new QIcon (TreeIcon (AppInstance(), IDB_TREEICON_FOLDER1));
	treeicon[1] = new QIcon (TreeIcon (AppInstance(), IDB_TREEICON_SCN1));
	treeicon[1]->addPixmap (TreeIcon (AppInstance(), IDB_TREEICON_SCN2), QIcon::Selected);
#endif // __linux__
	scnhelp[0] = '\0';
	htmldesc = pLp->App()->UseHtmlInline();
#ifdef __linux__
	hWatch = NULL;
#endif // __linux__
}

//-----------------------------------------------------------------------------

orbiter::ScenarioTab::~ScenarioTab ()
{
#ifndef __linux__
	ImageList_Destroy (imglist);
	TerminateThread (hThread, 0);
#else // __linux__
	delete treeicon[0];
	delete treeicon[1];
	// the watcher belongs to the tab window
#endif // __linux__
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_SCN);

	RefreshList(false);
#ifndef __linux__
	SendDlgItemMessage (hTab, IDC_SCN_LIST, TVM_SETIMAGELIST, (WPARAM)TVSIL_NORMAL, (LPARAM)imglist);
#else // __linux__
	DlgItem<QTreeWidget> (hTab, IDC_SCN_LIST)->setIconSize (QSize (16, 16)); // TVM_SETIMAGELIST
#endif // __linux__

#ifndef __linux__
	r_list0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_SCN_LIST)); // REMOVE!
	r_desc0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_SCN_HTML)); // REMOVE!
	r_pane  = GetClientPos (hTab, GetDlgItem (hTab, IDC_SCN_SPLIT1));
	r_save0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_SCN_SAVE));
	r_clear0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_SCN_DELQS));
	r_info0  = GetClientPos (hTab, GetDlgItem (hTab, IDC_SCN_INFO));
	r_pause0 = GetClientPos (hTab, GetDlgItem (hTab, IDC_SCN_PAUSED));
#else // __linux__
	r_list0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_SCN_LIST)); // REMOVE!
	r_desc0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_SCN_HTML)); // REMOVE!
	r_pane  = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_SCN_SPLIT1));
	r_save0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_SCN_SAVE));
	r_clear0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_SCN_DELQS));
	r_info0  = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_SCN_INFO));
	r_pause0 = GetClientPos (hTab, oapiResDlgItem (hTab, IDC_SCN_PAUSED));
#endif // __linux__

	if (pLp->App()->UseHtmlInline()) {
#ifndef __linux__
		ShowWindow (GetDlgItem (hTab, IDC_SCN_DESC), SW_HIDE);
		ShowWindow (GetDlgItem (hTab, IDC_SCN_HTML), SW_SHOW);
		ShowWindow (GetDlgItem (hTab, IDC_SCN_INFO), SW_HIDE);
#else // __linux__
		oapiResDlgItem (hTab, IDC_SCN_DESC)->hide();
		oapiResDlgItem (hTab, IDC_SCN_HTML)->show();
		oapiResDlgItem (hTab, IDC_SCN_INFO)->hide();
#endif // __linux__
		infoId = IDC_SCN_HTML;
	} else {
#ifndef __linux__
		ShowWindow (GetDlgItem (hTab, IDC_SCN_HTML), SW_HIDE);
		ShowWindow (GetDlgItem (hTab, IDC_SCN_DESC), SW_SHOW);
		ShowWindow (GetDlgItem (hTab, IDC_SCN_INFO), SW_SHOW);
#else // __linux__
		oapiResDlgItem (hTab, IDC_SCN_HTML)->hide();
		oapiResDlgItem (hTab, IDC_SCN_DESC)->show();
		oapiResDlgItem (hTab, IDC_SCN_INFO)->show();
#endif // __linux__
		infoId = IDC_SCN_DESC;
	}

#ifndef __linux__
	splitListDesc.SetHwnd (GetDlgItem (hTab, IDC_SCN_SPLIT1), GetDlgItem (hTab, IDC_SCN_LIST), GetDlgItem (hTab, infoId));
#else // __linux__
	splitListDesc.SetHwnd (oapiResDlgItem (hTab, IDC_SCN_SPLIT1), oapiResDlgItem (hTab, IDC_SCN_LIST), oapiResDlgItem (hTab, infoId));
#endif // __linux__

#ifndef __linux__
	// create a thread to monitor changes to the scenario list
	hThread = CreateThread (NULL, NULL, threadWatchScnList, this, NULL, NULL);
#else // __linux__
	// create a watcher to monitor changes to the scenario list
	hWatch = new QFileSystemWatcher (hTab);
	QObject::connect (hWatch, &QFileSystemWatcher::directoryChanged, hTab, [this]() {
		RefreshList (true);
		WatchScnList ();
	});
	WatchScnList ();
#endif // __linux__
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::GetConfig (const Config *cfg)
{
#ifndef __linux__
	SendDlgItemMessage (hTab, IDC_SCN_PAUSED, BM_SETCHECK,
		cfg->CfgLogicPrm.bStartPaused ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	DlgItem<QCheckBox> (hTab, IDC_SCN_PAUSED)->setChecked (cfg->CfgLogicPrm.bStartPaused);
#endif // __linux__
	int listw = cfg->CfgWindowPos.LaunchpadScnListWidth;
	if (!listw) {
#ifndef __linux__
		RECT r;
		GetClientRect (GetDlgItem (hTab, IDC_SCN_LIST), &r);
		listw = r.right-r.left;
#else // __linux__
		listw = oapiResDlgItem (hTab, IDC_SCN_LIST)->width();
#endif // __linux__
	}
	splitListDesc.SetStaticPane (SplitterCtrl::PANE1, listw);
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::SetConfig (Config *cfg)
{
#ifndef __linux__
	cfg->CfgLogicPrm.bStartPaused = (SendDlgItemMessage (hTab, IDC_SCN_PAUSED, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	cfg->CfgLogicPrm.bStartPaused = DlgItem<QCheckBox> (hTab, IDC_SCN_PAUSED)->isChecked();
#endif // __linux__
	cfg->CfgWindowPos.LaunchpadScnListWidth = splitListDesc.GetPaneWidth (SplitterCtrl::PANE1);
}

//-----------------------------------------------------------------------------

bool orbiter::ScenarioTab::OpenHelp ()
{
	OpenTabHelp ("tab_scenario");
	return true;
}

//-----------------------------------------------------------------------------

BOOL orbiter::ScenarioTab::OnSize (int w, int h)
{
	int dw = w - (int)(pos0.right-pos0.left);
	int dh = h - (int)(pos0.bottom-pos0.top);
	int w0 = r_pane.right - r_pane.left; // initial splitter pane width
	int h0 = r_pane.bottom - r_pane.top; // initial splitter pane height

	// the elements below may need updating
	int wl0 = r_list0.right - r_list0.left; // initial list width
	int wd0 = r_desc0.right - r_desc0.left; // initial description width
	int wg  = r_desc0.right - r_list0.left - wl0 - wd0;  // gap width
	int bg  = r_clear0.left - r_save0.right; // button gap
	int wb1 = r_save0.right - r_save0.left;
	int wb2 = r_clear0.right - r_clear0.left;
	int wb3 = r_info0.right - r_info0.left;
	int hb  = r_save0.bottom - r_save0.top;
	int wl  = wl0 + (dw*wl0)/(wl0+wd0);
	wl = max (wl, wl0/2);
	int xr = r_list0.left+wl+wg;
	int wr = max(10,wl0+wd0+dw-wl);
	int ww = wl+wr+wg-2*bg;
	wb3 = min (wb3, ww/3);
	ww -= wb3;
	wb1 = wb2 = min (wb1, ww/2);
	int xb2 = r_save0.left+wb1+bg;
	int xb3 = xr+wr-wb3;

#ifndef __linux__
	SetWindowPos (GetDlgItem (hTab, IDC_SCN_SPLIT1), NULL,
		0, 0, w0+dw, h0+dh,
		SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_SCN_SAVE), NULL,
		r_save0.left, r_save0.top+dh, wb1, hb,
		SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_SCN_DELQS), NULL,
		xb2, r_clear0.top+dh, wb2, hb,
		SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_SCN_INFO), NULL,
		xb3, r_info0.top+dh, wb3, hb,
		SWP_NOACTIVATE|SWP_NOOWNERZORDER|SWP_NOZORDER);
	SetWindowPos (GetDlgItem (hTab, IDC_SCN_PAUSED), NULL,
		r_pause0.left+dw, r_pause0.top, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOOWNERZORDER|SWP_NOZORDER|SWP_NOCOPYBITS);

	return NULL;
}

//-----------------------------------------------------------------------------
#else // __linux__
	oapiResDlgItem (hTab, IDC_SCN_SPLIT1)->resize (w0+dw, h0+dh);
	oapiResDlgItem (hTab, IDC_SCN_SAVE)->setGeometry (r_save0.left, r_save0.top+dh, wb1, hb);
	oapiResDlgItem (hTab, IDC_SCN_DELQS)->setGeometry (xb2, r_clear0.top+dh, wb2, hb);
	oapiResDlgItem (hTab, IDC_SCN_INFO)->setGeometry (xb3, r_info0.top+dh, wb3, hb);
	oapiResDlgItem (hTab, IDC_SCN_PAUSED)->move (r_pause0.left+dw, r_pause0.top);
#endif // __linux__

#ifndef __linux__
BOOL orbiter::ScenarioTab::OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh)
{
	if (idCtrl == IDC_SCN_LIST) {
		NM_TREEVIEW* pnmtv = (NM_TREEVIEW FAR*)pnmh;
		switch (pnmtv->hdr.code) {
		case TVN_SELCHANGED:
			ScenarioChanged();
			return TRUE;
		case NM_DBLCLK:
			PostMessage(LaunchpadWnd(), WM_COMMAND, IDLAUNCH, 0);
			return TRUE;
		}
	}
#endif // !__linux__
	return FALSE;
}

//-----------------------------------------------------------------------------

#ifndef __linux__
BOOL orbiter::ScenarioTab::OnMessage (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL orbiter::ScenarioTab::OnInitDialog (QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	NM_TREEVIEW *pnmtv;

	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_SCN_SAVE:
			SaveCurScenario();
			return TRUE;
		case IDC_SCN_DELQS:
			ClearQSFolder();
			return TRUE;
		case IDC_SCN_INFO:
			OpenScenarioHelp();
			return TRUE;
		}
		break;
	}
#else // __linux__
	// WM_NOTIFY
	QTreeWidget *hTree = DlgItem<QTreeWidget> (hWnd, IDC_SCN_LIST);
	QObject::connect (hTree, &QTreeWidget::currentItemChanged, hWnd, [this]() {
		ScenarioChanged(); // TVN_SELCHANGED
	});
	QObject::connect (hTree, &QTreeWidget::itemDoubleClicked, hWnd, [this]() {
		// NM_DBLCLK: WM_COMMAND IDLAUNCH to the Launchpad
		QMetaObject::invokeMethod (DlgItem<QPushButton> (LaunchpadWnd(), IDLAUNCH), "click", Qt::QueuedConnection);
	});

	// WM_COMMAND
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_SCN_SAVE), &QPushButton::clicked, hWnd, [this]() { SaveCurScenario(); });
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_SCN_DELQS), &QPushButton::clicked, hWnd, [this]() { ClearQSFolder(); });
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_SCN_INFO), &QPushButton::clicked, hWnd, [this]() { OpenScenarioHelp(); });
#endif // __linux__
	return FALSE;
}

#ifdef __linux__
// sibling after an item (TVGN_NEXT)
static QTreeWidgetItem *NextSibling (QTreeWidget *hTree, QTreeWidgetItem *it)
{
	QTreeWidgetItem *parent = it->parent();
	int idx = (parent ? parent->indexOfChild (it) : hTree->indexOfTopLevelItem (it)) + 1;
	return (parent ? parent->child (idx) : hTree->topLevelItem (idx));
}

#endif // __linux__
//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::RefreshList (bool preserveSelection)
{
	if (Launchpad()->Visible()) {
#ifndef __linux__
		char cbuf[256], ch[256], * pc, * c;
		GetSelScenario(cbuf, 256);
		SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_SELECTITEM, TVGN_CARET, NULL);
		// remove selection to avoid repeated TVN_SELCHANGED messages while the list is cleared
		//DWORD styles = GetWindowLongPtr(GetDlgItem(hTab, IDC_SCN_LIST), GWL_STYLE);
		SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_DELETEITEM, 0, (LPARAM)TVI_ROOT);
		//SetWindowLongPtr(GetDlgItem(hTab, IDC_SCN_LIST), GWL_STYLE, styles);
		ScanDirectory(pCfg->CfgDirPrm.ScnDir, NULL);
#else // __linux__
		char cbuf[256], * pc, * c;
		QTreeWidget *hTree = DlgItem<QTreeWidget>(hTab, IDC_SCN_LIST);
		if (!GetSelScenario(cbuf, 256)) cbuf[0] = '\0';
		{
			// remove selection to avoid repeated TVN_SELCHANGED messages while the list is cleared
			QSignalBlocker block(hTree);
			hTree->setCurrentItem(NULL);
			hTree->clear();
		}
		ScanDirectory(oapiResolvePath(pCfg->CfgDirPrm.ScnDir), NULL);
#endif // __linux__

#ifndef __linux__
		HTREEITEM hti = TreeView_GetRoot(GetDlgItem(hTab, IDC_SCN_LIST));
#else // __linux__
		QTreeWidgetItem *hti = hTree->topLevelItem(0);
#endif // __linux__
		if (preserveSelection) { // find the previous selection in the newly created list and re-select it
			pc = cbuf;
			while (*pc) {
#ifndef __linux__
				for (c = pc; *c && *c != '\\'; c++);
				bool isdir = (*c == '\\');
#else // __linux__
				for (c = pc; *c && *c != '/'; c++);
				bool isdir = (*c == '/');
#endif // __linux__
				*c = '\0';
#ifndef __linux__
				TV_ITEM tvi = { TVIF_HANDLE | TVIF_TEXT, 0, 0, 0, ch, 256 };
				for (tvi.hItem = hti; tvi.hItem; tvi.hItem = (HTREEITEM)SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETNEXTITEM, TVGN_NEXT, (LPARAM)tvi.hItem)) {
					SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETITEM, 0, (LPARAM)&tvi);
					if (!strcmp(tvi.pszText, pc)) {
						hti = tvi.hItem;
#else // __linux__
				for (QTreeWidgetItem *it = hti; it; it = NextSibling(hTree, it)) {
					if (!strcmp(it->text(0).toUtf8().constData(), pc)) {
						hti = it;
#endif // __linux__
						if (isdir)
#ifndef __linux__
							hti = (HTREEITEM)SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETNEXTITEM, TVGN_CHILD, (LPARAM)hti);
#else // __linux__
							hti = hti->child(0);
#endif // __linux__
						break;
					}
				}
				pc = c;
				if (isdir) pc++;
			}
		}
		else { // Select the "current" scenario
#ifndef __linux__
			TV_ITEM tvi = { TVIF_HANDLE | TVIF_TEXT, 0, 0, 0, ch, 256 };
			for (tvi.hItem = hti; tvi.hItem; tvi.hItem = (HTREEITEM)SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETNEXTITEM, TVGN_NEXT, (LPARAM)tvi.hItem)) {
				SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETITEM, 0, (LPARAM)&tvi);
				if (!strcmp(tvi.pszText, CurrentScenario)) {
					hti = tvi.hItem;
#else // __linux__
			for (QTreeWidgetItem *it = hti; it; it = NextSibling(hTree, it)) {
				if (!strcmp(it->text(0).toUtf8().constData(), CurrentScenario)) {
					hti = it;
#endif // __linux__
					break;
				}
			}
		}
#ifndef __linux__
		SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_SELECTITEM, TVGN_CARET, (LPARAM)hti);
#else // __linux__
		hTree->setCurrentItem(hti);
#endif // __linux__
	}
}

//-----------------------------------------------------------------------------

#ifdef __linux__
void orbiter::ScenarioTab::WatchScnList ()
{
	// FindFirstChangeNotification watches the whole tree; the Qt watcher takes each directory
	if (!hWatch) return;
	QStringList dirs = hWatch->directories();
	if (!dirs.isEmpty()) hWatch->removePaths(dirs);
	std::error_code ec;
	fs::path root = oapiResolvePath(pCfg->CfgDirPrm.ScnDir);
	if (!fs::is_directory(root, ec)) return;
	dirs = {QString::fromStdString(root.string())};
	for (auto& entry : fs::recursive_directory_iterator(root, ec))
		if (entry.is_directory(ec)) dirs << QString::fromStdString(entry.path().string());
	hWatch->addPaths(dirs);
}

//-----------------------------------------------------------------------------

#endif // __linux__
void orbiter::ScenarioTab::LaunchpadShowing(bool show)
{
	if (show) {
		RefreshList(false);
	}
}
//-----------------------------------------------------------------------------

#ifndef __linux__
void orbiter::ScenarioTab::ScanDirectory (const fs::path& path, HTREEITEM hti)
#else // __linux__
void orbiter::ScenarioTab::ScanDirectory (const fs::path& path, QTreeWidgetItem *hti)
#endif // __linux__
{
#ifndef __linux__
	TV_INSERTSTRUCT tvis;
	HTREEITEM ht, hts0, ht0;
	char cbuf[256];

	tvis.hParent = hti;
	tvis.item.mask = TVIF_TEXT | TVIF_CHILDREN | TVIF_IMAGE | TVIF_SELECTEDIMAGE;
	tvis.item.pszText = cbuf;
	tvis.hInsertAfter = TVI_SORT;
	tvis.item.cChildren = 1;
	tvis.item.iImage = treeicon_idx[0];
	tvis.item.iSelectedImage = treeicon_idx[0];
#else // __linux__
	QTreeWidget *hTree = DlgItem<QTreeWidget>(hTab, IDC_SCN_LIST);
	QTreeWidgetItem *ht, *hts0;
	std::error_code ec;
	auto count = [hTree, hti]() { return (hti ? hti->childCount() : hTree->topLevelItemCount()); };
	auto child = [hTree, hti](int i) { return (hti ? hti->child(i) : hTree->topLevelItem(i)); };
	auto insert = [hTree, hti](int i, QTreeWidgetItem *it) { if (hti) hti->insertChild(i, it); else hTree->insertTopLevelItem(i, it); };
#endif // __linux__

#ifndef __linux__
	for (auto& entry : fs::directory_iterator(path)) {
#else // __linux__
	// subdirectories (cChildren = 1, folder image; TVI_SORT)
	for (auto& entry : fs::directory_iterator(path, ec)) {
#endif // __linux__
		if (entry.is_directory()) {
#ifndef __linux__
			strcpy(cbuf, entry.path().stem().string().c_str());
			ht = (HTREEITEM)SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_INSERTITEM, 0, (LPARAM)&tvis);
#else // __linux__
			QString name = QString::fromStdString(entry.path().stem().string());
			ht = new QTreeWidgetItem();
			ht->setText(0, name);
			ht->setIcon(0, *treeicon[0]);
			ht->setChildIndicatorPolicy(QTreeWidgetItem::ShowIndicator);
			int i;
			for (i = 0; i < count(); i++)
				if (QString::compare(name, child(i)->text(0), Qt::CaseInsensitive) < 0) break;
			insert(i, ht);
#endif // __linux__
			ScanDirectory(entry.path(), ht);
		}
	}

#ifndef __linux__
	hts0 = (HTREEITEM)SendDlgItemMessage (hTab, IDC_SCN_LIST, TVM_GETNEXTITEM, TVGN_CHILD, (LPARAM)hti);
#else // __linux__
	hts0 = child(0);
#endif // __linux__
	// the first subdirectory entry in this folder

#ifndef __linux__
	// scan for files
	tvis.hInsertAfter = TVI_FIRST;
	tvis.item.cChildren = 0;
	tvis.item.iImage = treeicon_idx[2];
	tvis.item.iSelectedImage = treeicon_idx[3];
	for (auto& entry : fs::directory_iterator(path)) {
#else // __linux__
	// scan for files: they go ahead of the subdirectories, ordered by strcmp
	for (auto& entry : fs::directory_iterator(path, ec)) {
#endif // __linux__
		if (entry.is_regular_file() && entry.path().extension().string() == ".scn") {
#ifndef __linux__
			strcpy(cbuf, entry.path().stem().string().c_str());

			char ch[256];
			TV_ITEM tvi = { TVIF_HANDLE | TVIF_TEXT, 0, 0, 0, ch, 256 };

			ht0 = (HTREEITEM)SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETNEXTITEM, TVGN_CHILD, (LPARAM)hti);
			for (tvi.hItem = ht0; tvi.hItem && tvi.hItem != hts0; tvi.hItem = (HTREEITEM)SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETNEXTITEM, TVGN_NEXT, (LPARAM)tvi.hItem)) {
				SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETITEM, 0, (LPARAM)&tvi);
				if (strcmp(tvi.pszText, cbuf) > 0) break;
			}
			if (tvi.hItem) {
				ht = (HTREEITEM)SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_GETNEXTITEM, TVGN_PREVIOUS, (LPARAM)tvi.hItem);
				tvis.hInsertAfter = (ht ? ht : TVI_FIRST);
			}
			else {
				tvis.hInsertAfter = (hts0 ? TVI_FIRST : TVI_LAST);
			}
			(HTREEITEM)SendDlgItemMessage(hTab, IDC_SCN_LIST, TVM_INSERTITEM, 0, (LPARAM)&tvis);
#else // __linux__
			std::string cbuf = entry.path().stem().string();
			int i;
			for (i = 0; i < count() && child(i) != hts0; i++)
				if (strcmp(child(i)->text(0).toUtf8().constData(), cbuf.c_str()) > 0) break;
			ht = new QTreeWidgetItem();
			ht->setText(0, QString::fromStdString(cbuf));
			ht->setIcon(0, *treeicon[1]);
			insert(i, ht);
#endif // __linux__
		}
	}
}

//-----------------------------------------------------------------------------

char *ScanFileDesc (std::istream &is, const char *blockname)
{
	char *buf = 0;
	char blockbegin[256] = "BEGIN_";
	char blockend[256] = "END_";
	strncpy (blockbegin+6, blockname, 240);
	strncpy (blockend+4, blockname, 240);

	if (FindLine (is, blockbegin)) {
		int i, len, buflen = 0;
		const int linelen = 256;
		char line[linelen];
		for(i = 0;; i++) {
			if (!is.getline(line, linelen-2)) {
				if (is.eof()) break;
				else is.clear();
			}
#ifndef __linux__
			if (_strnicmp (line, blockend, strlen(blockend))) {
#else // __linux__
			if (strncasecmp (line, blockend, strlen(blockend))) {
#endif // __linux__
				len = strlen(line);
				if (len) strcat (line, " "), len++;    // convert newline to space
				else     strcpy (line, "\r\n"), len=2; // convert empty line to CR
				char *tmp = new char[buflen+len+1];
				if (buflen) {
					memcpy (tmp, buf, buflen*sizeof(char));
					delete []buf;
				}
				memcpy (tmp+buflen, line, len*sizeof(char));
				buflen += len;
				tmp[buflen] = '\0';
				buf = tmp;
			} else {
				break;
			}
		}
	}
	return buf;
}

void AppendChar (char *&line, int &linelen, char c, int pos)
{
	if (pos == linelen) {
		char *tmp = new char[linelen+256];
		memcpy (tmp, line, linelen);
		delete []line;
		line = tmp;
		linelen += 256;
	}
	line[pos] = c;
}

struct ReplacementPair {
	char *src, *tgt;
};

void Html2Text(std::string& str)
{
	std::string::size_type n0, n1;

	// 1. remove all newlines
	for (int i = str.size() - 1; i >= 0; i--)
		if (str[i] == '\r')
			str.erase(i, 1);
	for (int i = str.size() - 1; i >= 0; i--)
		if (str[i] == '\n')
			str[i] = ' ';

	// 2. substitute some html tags
	const std::string tag[4] = { "</h1> ", "</p> ", "</h1>", "</p>" };
	const std::string tag_subst[4] = { "\r\n\r\n", "\r\n\r\n", "\r\n\r\n", "\r\n\r\n" };
	for (int i = 0; i < 4; i++) {
		n0 = 0;
		while ((n0 = str.find(tag[i], n0)) != std::string::npos) {
			str.replace(n0, tag[i].size(), tag_subst[i]);
			n0 += tag_subst[i].size();
		}
	}

	// 3. remove remaining tags
	n0 = 0;
	while ((n0 = str.find("<", n0)) != std::string::npos) {
		n1 = str.find(">", n0);
		if (n1 != std::string::npos)
			str.erase(n0, n1 - n0 + 1);
	}

	// 4. substitute some symbols
	const std::string sym[5] = { "&gt;", "&lt;", "&ge;", "&le;", "&amp;" };
	const std::string sym_subst[5] = { ">", "<", ">=", "<=", "\001" };
	for (int i = 0; i < 5; i++) {
		n0 = 0;
		while ((n0 = str.find(sym[i], n0)) != std::string::npos) {
			str.replace(n0, sym[i].size(), sym_subst[i]);
			n0 += sym_subst[i].size();
		}
	}

	// 5. remove remaining symbols
	n0 = 0;
	while ((n0 = str.find("&", n0)) != std::string::npos) {
		n1 = str.find(";", n0);
		if (n1 != std::string::npos)
			str.erase(n0, n1 - n0 + 1);
	}

	// 6. restore ampersands
	for (int i = 0; i < str.size(); i++)
		if (str[i] == '\001')
			str[i] = '&';
}

void Text2Html(std::string& str)
{
	std::string::size_type n0;

	// 1. substitute some symbols
	const std::string sym[6] = { "&", ">=", "<=", ">", "<", "\r\n" }; // the order is relevant here
	const std::string sym_subst[6] = { "&amp;", "&ge;", "&le;", "&gt;", "&lt;", "<br />"};
	for (int i = 0; i < 6; i++) {
		n0 = 0;
		while ((n0 = str.find(sym[i], n0)) != std::string::npos) {
			str.replace(n0, sym[i].size(), sym_subst[i]);
			n0 += sym_subst[i].size();
		}
	}
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::ScenarioChanged ()
{
	const int linelen = 256;
	bool have_info = false;
	char cbuf[256], path[256], *pc;
	ifstream ifs;
	scnhelp[0] = '\0';

	switch (GetSelScenario (cbuf, 256)) {
	case 0: // error
		return;
	case 1: // scenario file
#ifndef __linux__
		ifs.open (pLp->App()->ScnPath (cbuf));
#else // __linux__
		ifs.open (oapiResolvePath (pLp->App()->ScnPath (cbuf)));
#endif // __linux__
		pLp->EnableLaunchButton (true);
		break;
	case 2: // subdirectory
		strcpy (path, pCfg->CfgDirPrm.ScnDir);
		strcat (path, cbuf);
#ifndef __linux__
		strcat (path, "\\Description.txt");
		ifs.open (path, ios::in);
#else // __linux__
		strcat (path, "/Description.txt");
		ifs.open (oapiResolvePath (path), ios::in);
#endif // __linux__
		pLp->EnableLaunchButton (false);
		break;
	}
	if (ifs) {
		if (!have_info) {
			char *buf;
			if (htmldesc) {
				buf = ScanFileDesc(ifs, "URLDESC");
				if (buf) {
#ifndef __linux__
					char url_ref[256], url[256], *path, *topic;
#else // __linux__
					char url_ref[256], url[512], cwd[256], *path, *topic;
#endif // __linux__
					strncpy(url_ref, trim_string(buf), 255);
#ifdef __linux__
					url_ref[255] = '\0';
#endif // __linux__
					path = strtok(url_ref, ",");
					topic = strtok(NULL, "\n");
					if (topic)
#ifndef __linux__
						sprintf(url, "its:Html\\Scenarios\\%s.chm::%s.htm", path, topic);
#else // __linux__
						snprintf(url, 512, "its:Html\\Scenarios\\%s.chm::%s.htm", path, topic);
#endif // __linux__
					else
#ifndef __linux__
						sprintf(url, "%s\\Html\\Scenarios\\%s.htm", _getcwd(url, 256), path);
					DisplayHTMLPage(GetDlgItem(hTab, IDC_SCN_HTML), url);
#else // __linux__
						snprintf(url, 512, "%s/Html/Scenarios/%s.htm", getcwd(cwd, 256), path);
					for (char *c = url; *c; c++) if (*c == '\\') *c = '/';
					DisplayHTMLPage(oapiResDlgItem(hTab, IDC_SCN_HTML), topic ? url : oapiResolvePath(url).c_str()); // "its:" URLs resolve in DisplayHTMLPage
#endif // __linux__
					have_info = true;
				}
				else {
					buf = ScanFileDesc(ifs, "HYPERDESC");
					if (!buf) {
						buf = ScanFileDesc(ifs, "DESC");
						if (buf) {
							std::string str(buf);
							Text2Html(str);
							delete[]buf;
							buf = new char[str.size() + 1];
							strcpy(buf, str.c_str());
						}
					}
					if (buf) { // prepend style preamble
						char* buf2 = new char[strlen(htmlstyle) + strlen(buf) + 1];
						strcpy(buf2, htmlstyle); strcat(buf2, buf);
						delete[]buf;
						buf = buf2;
#ifndef __linux__
						DisplayHTMLStr(GetDlgItem(hTab, IDC_SCN_HTML), buf);
#else // __linux__
						DisplayHTMLStr(oapiResDlgItem(hTab, IDC_SCN_HTML), buf);
#endif // __linux__
						have_info = true;
					}
				}
			} else {
#ifndef __linux__
				if (buf = ScanFileDesc (ifs, "DESC")) {
					SetWindowText(GetDlgItem(hTab, IDC_SCN_DESC), buf);
#else // __linux__
				if ((buf = ScanFileDesc (ifs, "DESC"))) {
					oapiSetDlgItemText(hTab, IDC_SCN_DESC, buf);
#endif // __linux__
					have_info = true;
#ifndef __linux__
				} else if (buf = ScanFileDesc (ifs, "HYPERDESC")) {
#else // __linux__
				} else if ((buf = ScanFileDesc (ifs, "HYPERDESC"))) {
#endif // __linux__
					std::string str(buf);
					Html2Text(str);
#ifndef __linux__
					SetWindowText(GetDlgItem(hTab, IDC_SCN_DESC), str.c_str());
#else // __linux__
					oapiSetDlgItemText(hTab, IDC_SCN_DESC, str.c_str());
#endif // __linux__
					have_info = true;
				}
			}
			if (buf) {
				delete []buf;
				buf = NULL;
			}
		}
	}

	if (!have_info) {
#ifndef __linux__
		if (htmldesc) DisplayHTMLStr (GetDlgItem (hTab, IDC_SCN_HTML), "");
		else          SetWindowText (GetDlgItem (hTab, IDC_SCN_DESC), "");
#else // __linux__
		if (htmldesc) DisplayHTMLStr (oapiResDlgItem (hTab, IDC_SCN_HTML), "");
		else          oapiSetDlgItemText (hTab, IDC_SCN_DESC, "");
#endif // __linux__
	}

	if (!htmldesc) {
		bool enable_info = false;
		for (int i = 0; scnhelp[i]; i++)
			if (scnhelp[i] == ',') {
				enable_info = true;
				break;
			}
#ifndef __linux__
		EnableWindow (GetDlgItem (hTab, IDC_SCN_INFO), enable_info ? TRUE:FALSE);
#else // __linux__
		oapiResDlgItem (hTab, IDC_SCN_INFO)->setEnabled (enable_info);
#endif // __linux__
	}
}

//-----------------------------------------------------------------------------

int orbiter::ScenarioTab::GetSelScenario (char *scn, int len)
{
#ifndef __linux__
	TV_ITEM tvi;
#endif // !__linux__
	char cbuf[256];
	int type;

#ifndef __linux__
	tvi.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_CHILDREN;
	tvi.hItem = TreeView_GetSelection (GetDlgItem (hTab, IDC_SCN_LIST));
	tvi.pszText = scn;
	tvi.cchTextMax = len;

	if (!TreeView_GetItem (GetDlgItem (hTab, IDC_SCN_LIST), &tvi)) return 0;
	type = (tvi.cChildren ? 2 : 1);
#else // __linux__
	if (!hTab) return 0;
	QTreeWidgetItem *it = DlgItem<QTreeWidget> (hTab, IDC_SCN_LIST)->currentItem();
	if (!it) return 0;
	snprintf (scn, len, "%s", it->text (0).toUtf8().constData());
	type = (it->childIndicatorPolicy() == QTreeWidgetItem::ShowIndicator ? 2 : 1);
#endif // __linux__

	// build path
#ifndef __linux__
	tvi.pszText = cbuf;
	tvi.cchTextMax = 256;
	while (tvi.hItem = TreeView_GetParent (GetDlgItem (hTab, IDC_SCN_LIST), tvi.hItem)) {
		if (TreeView_GetItem (GetDlgItem (hTab, IDC_SCN_LIST), &tvi)) {
			strcat (cbuf, "\\");
			strcat (cbuf, scn);
			strcpy (scn, cbuf);
		}
#else // __linux__
	while ((it = it->parent())) {
		snprintf (cbuf, 256, "%s/%s", it->text (0).toUtf8().constData(), scn);
		snprintf (scn, len, "%s", cbuf);
#endif // __linux__
	}
	return type;
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::SaveCurScenario ()
{
#ifndef __linux__
	ifstream ifs (pLp->App()->ScnPath (CurrentScenario), ios::in);
#else // __linux__
	ifstream ifs (oapiResolvePath (pLp->App()->ScnPath (CurrentScenario)), ios::in);
#endif // __linux__
	if (ifs) {
#ifndef __linux__
		DialogBoxParam (AppInstance(), MAKEINTRESOURCE(IDD_SAVESCN), LaunchpadWnd(), SaveProc, (LPARAM)this);
#else // __linux__
		QDialog *dlg = qobject_cast<QDialog*> (oapiCreateResDialog (AppInstance(), IDD_SAVESCN, LaunchpadWnd()));
		if (dlg) {
			SaveProc (dlg, this);
			dlg->exec(); // DialogBoxParam
			delete dlg;
		}
#endif // __linux__
	} else {
#ifndef __linux__
		MessageBox (LaunchpadWnd(), "No current simulation state available", "Save Error", MB_OK|MB_ICONEXCLAMATION);
#else // __linux__
		QMessageBox::warning (LaunchpadWnd(), "Save Error", "No current simulation state available");
#endif // __linux__
	}
}

//-----------------------------------------------------------------------------
// Name: SaveCurScenarioAs()
// Desc: copy current scenario file into 'name', replacing description with 'desc'.
//		 return value: 0=ok, 1=failed, 2=file exists (only checked if replace=false)
//-----------------------------------------------------------------------------
int orbiter::ScenarioTab::SaveCurScenarioAs (const char *name, char *desc, bool replace)
{
	string cbuf;
	bool skip = false;
#ifndef __linux__
	const char *path = pLp->App()->ScnPath (name);
#else // __linux__
	std::string path = oapiResolvePath (pLp->App()->ScnPath (name));
#endif // __linux__
	if (!replace) { // check if exists
		ifstream ifs (path, ios::in);
		if (ifs) return 2;
	}
	ofstream ofs (path);
	if (!ofs) return 1;
#ifndef __linux__
	ifstream ifs (pLp->App()->ScnPath (CurrentScenario));
#else // __linux__
	ifstream ifs (oapiResolvePath (pLp->App()->ScnPath (CurrentScenario)));
#endif // __linux__
	if (!ifs) return 1;
	int i, len = strlen(desc);
	for (i = 0; i < len-1; i++)
		if (desc[i] == '\r' && desc[i+1] == '\n') desc[i] = '\n';
	ofs << "BEGIN_DESC" << endl;
	ofs << desc << endl;
	ofs << "END_DESC" << endl;
	while (std::getline( ifs, cbuf ))
	{
		if (cbuf == "BEGIN_DESC")
			skip = true;
		else if (cbuf == "END_DESC")
			skip = false;
		else if (!skip)
			ofs << cbuf << endl;
	}
	return 0;
}

//-----------------------------------------------------------------------------
// Name: SaveProc()
#ifndef __linux__
// Desc: Scenario save dialog message proc
#else // __linux__
// Desc: Scenario save dialog set-up (WM_INITDIALOG) and command handlers
#endif // __linux__
//-----------------------------------------------------------------------------
#ifndef __linux__
INT_PTR CALLBACK orbiter::ScenarioTab::SaveProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void orbiter::ScenarioTab::SaveProc (QWidget *hWnd, ScenarioTab *pTab)
#endif // __linux__
{
#ifndef __linux__
	static ScenarioTab *pTab;
	int res, name_len, desc_len;
	static char name[64], *desc;

	switch (uMsg) {
	case WM_INITDIALOG:
		pTab = (ScenarioTab*)lParam;
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDOK:
			name_len = SendDlgItemMessage (hWnd, IDC_SAVE_NAME, WM_GETTEXTLENGTH, 0, 0);
			desc_len = SendDlgItemMessage (hWnd, IDC_SAVE_DESC, WM_GETTEXTLENGTH, 0, 0);
			if (name_len > 63) {
				MessageBox (hWnd, "Scenario name too long (max 63 characters)", "Save Error", MB_OK|MB_ICONEXCLAMATION);
				return TRUE;
			}
			desc = new char[desc_len+1];
			SendDlgItemMessage (hWnd, IDC_SAVE_NAME, WM_GETTEXT, 64, (LPARAM)name);
			SendDlgItemMessage (hWnd, IDC_SAVE_DESC, WM_GETTEXT, desc_len+1, (LPARAM)desc);
			res = pTab->SaveCurScenarioAs (name, desc);
			if (res == 2) {
				if (MessageBox (hWnd, "File exists. Overwrite?", "Warning", MB_YESNO|MB_ICONQUESTION) == IDYES)
					res = pTab->SaveCurScenarioAs (name, desc, true);
				else return TRUE;
			}
			if (res == 1) {
				MessageBox (hWnd, "Error writing scenario file.", "Save Error", MB_OK|MB_ICONEXCLAMATION);
				return TRUE;
			}
			delete []desc;
			desc = NULL;
			// fall through
		case IDCANCEL:
			EndDialog (hWnd, TRUE);
			return TRUE;
		}
	}
    return FALSE;
#else // __linux__
	QDialog *dlg = qobject_cast<QDialog*> (hWnd);

	// WM_COMMAND
	QObject::connect (DlgItem<QPushButton> (hWnd, IDOK), &QPushButton::clicked, dlg, [hWnd, dlg, pTab]() {
		int res, name_len, desc_len;
		char name[64], *desc;
		name_len = DlgItem<QLineEdit> (hWnd, IDC_SAVE_NAME)->text().toUtf8().size();
		desc_len = DlgItem<QPlainTextEdit> (hWnd, IDC_SAVE_DESC)->toPlainText().toUtf8().size();
		if (name_len > 63) {
			QMessageBox::warning (hWnd, "Save Error", "Scenario name too long (max 63 characters)");
			return;
		}
		desc = new char[desc_len+1];
		oapiGetDlgItemText (hWnd, IDC_SAVE_NAME, name, 64);
		oapiGetDlgItemText (hWnd, IDC_SAVE_DESC, desc, desc_len+1);
		res = pTab->SaveCurScenarioAs (name, desc);
		if (res == 2) {
			if (QMessageBox::question (hWnd, "Warning", "File exists. Overwrite?") == QMessageBox::Yes)
				res = pTab->SaveCurScenarioAs (name, desc, true);
			else { delete []desc; return; }
		}
		if (res == 1) {
			QMessageBox::warning (hWnd, "Save Error", "Error writing scenario file.");
			delete []desc;
			return;
		}
		delete []desc;
		desc = NULL;
		dlg->accept(); // EndDialog
	});
	QObject::connect (DlgItem<QPushButton> (hWnd, IDCANCEL), &QPushButton::clicked, dlg, &QDialog::reject);
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: ClearQSFolder()
// Desc: Delete all scenarios in the Quicksave folder
//-----------------------------------------------------------------------------
void orbiter::ScenarioTab::ClearQSFolder()
{
#ifndef __linux__
	fs::path scnpath{ pLp->App()->ScnPath("Quicksave") };
#else // __linux__
	fs::path scnpath{ oapiResolvePath(pLp->App()->ScnPath("Quicksave")) };
#endif // __linux__
	scnpath.replace_extension(); // remove ".scn"

	std::string msg = "Are you sure you want to delete all quicksaves? This affects:\n";
	int qsCount = 0;
	
	if (fs::exists(scnpath) && fs::is_directory(scnpath)) {
		for (auto& entry : fs::directory_iterator(scnpath)) {
			if (entry.is_regular_file() && entry.path().extension().string() == ".scn") {
				qsCount++;
				if (qsCount <= 10) {
					msg += "- " + entry.path().stem().string() + "\n";
				}
			}
		}
	}
	
	if (qsCount == 0) {
#ifndef __linux__
		MessageBox(LaunchpadWnd(), "There are no quicksaves to delete.", "Clear Quicksaves", MB_OK | MB_ICONINFORMATION);
#else // __linux__
		QMessageBox::information(LaunchpadWnd(), "Clear Quicksaves", "There are no quicksaves to delete.");
#endif // __linux__
		return;
	}
	
	if (qsCount > 10) {
		msg += "... and " + std::to_string(qsCount - 10) + " more.\n";
	}
	
#ifndef __linux__
	if (MessageBox(LaunchpadWnd(), msg.c_str(), "Clear Quicksaves", MB_YESNO | MB_ICONWARNING) != IDYES) {
#else // __linux__
	if (QMessageBox::warning(LaunchpadWnd(), "Clear Quicksaves", QString::fromStdString(msg), QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes) {
#endif // __linux__
		return;
	}

	std::error_code ec;
	fs::remove_all(scnpath, ec);
	if (!ec) {
		fs::create_directory(scnpath);
	}
}

//-----------------------------------------------------------------------------
// Name: OpenScenarioHelp()
// Desc: Opens the help file associated with the scenario
//-----------------------------------------------------------------------------
void orbiter::ScenarioTab::OpenScenarioHelp ()
{
	if (!scnhelp[0]) return;
	char str[256], path[256], *scenario, *topic;
	strncpy (str, scnhelp, 256);
	scenario = strtok (str, ",");
	topic = strtok (NULL, "\n");
#ifndef __linux__
	sprintf(path, "html\\scenarios\\%s.chm", scenario);
#else // __linux__
	snprintf(path, 256, "html/scenarios/%s.chm", scenario);
#endif // __linux__
	::OpenHelp(LaunchpadWnd(), path, topic);
}

#ifndef __linux__
//-----------------------------------------------------------------------------
// Thread function for scenario directory tree watcher
//-----------------------------------------------------------------------------
DWORD WINAPI orbiter::ScenarioTab::threadWatchScnList (LPVOID pPrm)
{
	ScenarioTab *tab = (ScenarioTab*)pPrm;
	HANDLE dwChangeHandle;
	DWORD dwWaitStatus;

	dwChangeHandle = FindFirstChangeNotification (
		tab->pCfg->CfgDirPrm.ScnDir,
		TRUE, FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME);

	while (true) {
		dwWaitStatus = WaitForSingleObject (dwChangeHandle, INFINITE);
		switch (dwWaitStatus) {
			case WAIT_OBJECT_0:
				tab->RefreshList(true);
				FindNextChangeNotification (dwChangeHandle);
				break;
		}
	}
	FindCloseChangeNotification(dwChangeHandle);
	return 0;
}
#else // __linux__
// the scenario directory tree watcher (upstream: a thread on FindFirstChangeNotification) is set up in Create
#endif // __linux__
