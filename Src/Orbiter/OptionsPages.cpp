// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Template for simulation options pages
// ======================================================================

#ifndef __linux__
#include <windows.h>
#endif // !__linux__
#include <array>
#ifdef __linux__
#include <fstream>
#include <iterator>
#include <strings.h>
#endif // __linux__
#include "OptionsPages.h"
#include "DlgCtrl.h"
#include "Orbiter.h"
#include "Psys.h"
#include "Camera.h"
#include "resource.h"
#ifndef __linux__
#include "Uxtheme.h"
#else // __linux__
#include "ResDialog.h"
#include "Util.h"
#include <QAbstractButton>
#include <QComboBox>
#include <QListWidget>
#include <QScrollBar>
#include <QSignalBlocker>
#include <QTreeWidget>
#endif // __linux__

using std::min;
using std::max;

extern Orbiter* g_pOrbiter;
extern PlanetarySystem* g_psys;
extern Camera* g_camera;

#ifdef __linux__
// BM_SETCHECK / BM_GETCHECK / EnableWindow / ShowWindow on a page control
static void SetCheck(QWidget *hPage, int id, bool check)
{
	if (QAbstractButton *b = DlgItem<QAbstractButton>(hPage, id)) b->setChecked(check);
}

static bool IsChecked(QWidget *hPage, int id)
{
	QAbstractButton *b = DlgItem<QAbstractButton>(hPage, id);
	return (b && b->isChecked());
}

static void EnableItem(QWidget *hPage, int id, bool enable)
{
	if (QWidget *w = oapiResDlgItem(hPage, id)) w->setEnabled(enable);
}

static void ShowItem(QWidget *hPage, int id, bool show)
{
	if (QWidget *w = oapiResDlgItem(hPage, id)) w->setVisible(show);
}

#endif // __linux__
// ======================================================================

OptionsPageContainer::OptionsPageContainer(Originator orig, Config* cfg)
	: m_orig(orig)
	, m_cfg(cfg)
{
	m_hDlg = 0;
	m_pageIdx = 0;
	m_vScrollPage = 0;
	m_vScrollRange = 0;
	m_vScrollPos = 0;
	m_contextHelp = 0;
}

// ----------------------------------------------------------------------

OptionsPageContainer::~OptionsPageContainer()
{
	Clear();
}

// ----------------------------------------------------------------------

OptionsPage* OptionsPageContainer::CurrentPage()
{
	return (m_pageIdx < m_pPage.size() ? m_pPage[m_pageIdx] : nullptr);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPageContainer::SetWindowHandles(HWND hDlg, HWND hSplitter, HWND hPane1, HWND hPane2)
#else // __linux__
void OptionsPageContainer::SetWindowHandles(QWidget *hDlg, QWidget *hSplitter, QWidget *hPane1, QWidget *hPane2)
#endif // __linux__
{
	m_hDlg = hDlg;
	m_hPageList = hPane1;
	m_hContainer = hPane2;
	m_splitter.SetHwnd(hSplitter, m_hPageList, m_hContainer);
	m_container.SetHwnd(m_hContainer);
	m_splitter.SetStaticPane(SplitterCtrl::PANE1, 120);
#ifdef __linux__

	// WM_NOTIFY of the page list
	if (QTreeWidget *hTree = qobject_cast<QTreeWidget*>(m_hPageList))
		QObject::connect(hTree, &QTreeWidget::currentItemChanged, hDlg, [this](QTreeWidgetItem *itemNew) { OnNotifyPagelist(itemNew); });
	// WM_VSCROLL of the page scroll bar, if the dialog has one
	if (QScrollBar *sb = DlgItem<QScrollBar>(hDlg, IDC_SCROLLBAR1))
		QObject::connect(sb, &QScrollBar::valueChanged, hDlg, [this, hDlg, sb](int pos) { VScroll(hDlg, pos, sb); });
#endif // __linux__
}

// ----------------------------------------------------------------------

void OptionsPageContainer::CreatePages()
{
#ifndef __linux__
	HTREEITEM parent;
#else // __linux__
	QTreeWidgetItem *parent;
#endif // __linux__
	if (m_orig == LAUNCHPAD) {
		AddPage(new OptionsPage_Visual(this));
		AddPage(new OptionsPage_Physics(this));
	}
	AddPage(new OptionsPage_Instrument(this));
	AddPage(new OptionsPage_Vessel(this));
	parent = AddPage(new OptionsPage_UI(this));
	AddPage(new OptionsPage_Joystick(this), parent);
	AddPage(new OptionsPage_CelSphere(this));
	parent = AddPage(new OptionsPage_VisHelper(this));
	AddPage(new OptionsPage_Planetarium(this), parent);
	AddPage(new OptionsPage_Labels(this), parent);
	AddPage(new OptionsPage_Forces(this), parent);
	AddPage(new OptionsPage_Axes(this), parent);
#ifndef __linux__
	TreeView_SelectItem(m_hPageList, TreeView_GetRoot(m_hPageList));
#else // __linux__
	QTreeWidget *hTree = qobject_cast<QTreeWidget*>(m_hPageList);
	hTree->setCurrentItem(hTree->topLevelItem(0));
#endif // __linux__
}

// ----------------------------------------------------------------------

void OptionsPageContainer::ExpandAll()
{
	bool expand = true;
#ifndef __linux__
	HWND hTree = GetDlgItem(m_hDlg, IDC_OPT_PAGELIST);
	UINT code = (expand ? TVE_EXPAND : TVE_COLLAPSE);
	TVITEM catitem;
	catitem.mask = NULL;
	catitem.hItem = TreeView_GetRoot(hTree);
	while (TreeView_GetItem(hTree, &catitem)) {
		TreeView_Expand(hTree, catitem.hItem, code);
		catitem.hItem = TreeView_GetNextSibling(hTree, catitem.hItem);
	}
#else // __linux__
	QTreeWidget *hTree = DlgItem<QTreeWidget>(m_hDlg, IDC_OPT_PAGELIST);
	for (int i = 0; i < hTree->topLevelItemCount(); i++)
		hTree->topLevelItem(i)->setExpanded(expand);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPageContainer::SetPageSize(HWND hDlg)
#else // __linux__
void OptionsPageContainer::SetPageSize(QWidget *hDlg)
#endif // __linux__
{
	if (m_pageIdx >= m_pPage.size()) return; // sanity check

#ifndef __linux__
	RECT r0, r1;
	GetClientRect(m_container.HWnd(), &r0);
	GetClientRect(m_pPage[m_pageIdx]->HPage(), &r1);
	bool bVscroll = r1.bottom > r0.bottom;
	ShowWindow(GetDlgItem(hDlg, IDC_SCROLLBAR1), bVscroll ? SW_SHOW : SW_HIDE);
#else // __linux__
	int h0 = m_container.HWnd()->height();
	int h1 = m_pPage[m_pageIdx]->HPage()->height();
	bool bVscroll = h1 > h0;
	QScrollBar *sb = DlgItem<QScrollBar>(hDlg, IDC_SCROLLBAR1);
	ShowItem(hDlg, IDC_SCROLLBAR1, bVscroll);
#endif // __linux__

	if (bVscroll) {
#ifndef __linux__
		m_vScrollRange = (r1.bottom - r0.bottom);
		SCROLLINFO scrollinfo;
		scrollinfo.cbSize = sizeof(SCROLLINFO);
		scrollinfo.nMin = 0;
		scrollinfo.nMax = r1.bottom;
		scrollinfo.nPage = m_vScrollPage = r0.bottom;
		scrollinfo.nPos = min(m_vScrollPos, m_vScrollRange);
		scrollinfo.fMask = SIF_PAGE | SIF_RANGE | SIF_POS;
		SetScrollInfo(GetDlgItem(hDlg, IDC_SCROLLBAR1), SB_CTL, &scrollinfo, TRUE);
		int dy = m_vScrollPos - scrollinfo.nPos;
		m_vScrollPos = scrollinfo.nPos;
#else // __linux__
		m_vScrollRange = (h1 - h0);
		int nPos = min(m_vScrollPos, m_vScrollRange);
		m_vScrollPage = h0;
		if (sb) {
			QSignalBlocker block(sb);
			sb->setRange(0, m_vScrollRange);
			sb->setPageStep(m_vScrollPage);
			sb->setValue(nPos);
		}
		int dy = m_vScrollPos - nPos;
		m_vScrollPos = nPos;
#endif // __linux__
		if (dy)
#ifndef __linux__
			ScrollWindow(m_pPage[m_pageIdx]->HPage(), 0, dy, NULL, NULL);
#else // __linux__
			m_pPage[m_pageIdx]->HPage()->scroll(0, dy);
#endif // __linux__
	}
	else if (m_vScrollPos) {
#ifndef __linux__
		ScrollWindow(m_pPage[m_pageIdx]->HPage(), 0, m_vScrollPos, NULL, NULL);
#else // __linux__
		m_pPage[m_pageIdx]->HPage()->scroll(0, m_vScrollPos);
#endif // __linux__
		m_vScrollPos = 0;
	}
}

// ----------------------------------------------------------------------

#ifndef __linux__
HTREEITEM OptionsPageContainer::AddPage(OptionsPage* pPage, HTREEITEM parent)
#else // __linux__
QTreeWidgetItem *OptionsPageContainer::AddPage(OptionsPage* pPage, QTreeWidgetItem *parent)
#endif // __linux__
{
	m_pPage.push_back(pPage);
	return pPage->CreatePage(m_hDlg, parent);
}

// ----------------------------------------------------------------------

const OptionsPage* OptionsPageContainer::FindPage(const char* name) const
{
	for (auto page : m_pPage)
		if (!strcmp(name, page->Name()))
			return page;
	return 0;
}

// ----------------------------------------------------------------------

void OptionsPageContainer::SwitchPage(const char* name)
{
#ifndef __linux__
	char cbuf[256];
	TVITEM tvi;
	tvi.hItem = TreeView_GetRoot(m_hPageList);
	tvi.pszText = cbuf;
	tvi.cchTextMax = 256;
	tvi.cChildren = 0;
	tvi.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_CHILDREN;
	while (tvi.hItem) {
		TreeView_GetItem(m_hPageList, &tvi);
		if (!stricmp(cbuf, name)) {
			TreeView_SelectItem(m_hPageList, tvi.hItem);
			break;
		}
		TVITEM tvi_child;
		tvi_child.hItem = TreeView_GetChild(m_hPageList, tvi.hItem);
		tvi_child.pszText = cbuf;
		tvi_child.cchTextMax = 256;
		tvi_child.cChildren = 0;
		tvi_child.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_CHILDREN;
		while (tvi_child.hItem) {
			TreeView_GetItem(m_hPageList, &tvi_child);
			if (!stricmp(cbuf, name)) {
				TreeView_SelectItem(m_hPageList, tvi_child.hItem);
#else // __linux__
	QTreeWidget *hTree = qobject_cast<QTreeWidget*>(m_hPageList);
	for (int i = 0; i < hTree->topLevelItemCount(); i++) {
		QTreeWidgetItem *item = hTree->topLevelItem(i);
		if (!strcasecmp(item->text(0).toUtf8().constData(), name)) {
			hTree->setCurrentItem(item);
			break;
		}
		for (int j = 0; j < item->childCount(); j++) {
			QTreeWidgetItem *child = item->child(j);
			if (!strcasecmp(child->text(0).toUtf8().constData(), name)) {
				hTree->setCurrentItem(child);
#endif // __linux__
				break;
			}
#ifndef __linux__
			tvi_child.hItem = TreeView_GetNextSibling(m_hPageList, tvi_child.hItem);
#endif // !__linux__
		}
#ifndef __linux__
		tvi.hItem = TreeView_GetNextSibling(m_hPageList, tvi.hItem);
#endif // !__linux__
	}
}


// ----------------------------------------------------------------------

void OptionsPageContainer::SwitchPage(size_t page)
{
#ifndef __linux__
	if (page < 0 || page >= m_pPage.size())
#else // __linux__
	if (page >= m_pPage.size())
#endif // __linux__
		return;
	m_pageIdx = page;
	for (size_t pg = 0; pg < m_pPage.size(); pg++)
		if (pg != m_pageIdx) m_pPage[pg]->Show(false);
	m_pPage[m_pageIdx]->Show(true);
	m_pPage[m_pageIdx]->UpdateControls(m_pPage[m_pageIdx]->HPage());
	m_vScrollPos = 0;
	m_vScrollRange = 0;
	m_vScrollPage = 0;
	m_contextHelp = m_pPage[m_pageIdx]->HelpContext();

	SetPageSize(m_hDlg);
#ifndef __linux__
	InvalidateRect(m_hDlg, NULL, TRUE);
#else // __linux__
	m_hDlg->update();
#endif // __linux__
}

// ----------------------------------------------------------------------

void OptionsPageContainer::SwitchPage(const OptionsPage* page)
{
	for (size_t i = 0; i < m_pPage.size(); i++)
		if (m_pPage[i] == page) {
			SwitchPage(i);
			break;
		}
}

// ----------------------------------------------------------------------

void OptionsPageContainer::Clear()
{
	for (auto pPage : m_pPage)
		delete pPage;
	m_pPage.clear();
}

#ifndef __linux__
void OptionsPageContainer::OnNotifyPagelist(LPNMHDR pnmh)
#else // __linux__
void OptionsPageContainer::OnNotifyPagelist(QTreeWidgetItem *itemNew)
#endif // __linux__
{
#ifndef __linux__
	NM_TREEVIEW* pnmtv = (NM_TREEVIEW FAR*)pnmh;
	if (pnmtv->hdr.code == TVN_SELCHANGED) {
		OptionsPage* page = (OptionsPage*)pnmtv->itemNew.lParam;
		SwitchPage(page);
		TreeView_Expand(GetDlgItem(m_hDlg, IDC_OPT_PAGELIST), pnmtv->itemNew.hItem, TVE_EXPAND);
	}
#else // __linux__
	// TVN_SELCHANGED
	if (!itemNew) return;
	OptionsPage* page = (OptionsPage*)itemNew->data(0, Qt::UserRole).value<void*>();
	SwitchPage(page);
	itemNew->setExpanded(true);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPageContainer::VScroll(HWND hDlg, WORD request, WORD curpos, HWND hControl)
#else // __linux__
BOOL OptionsPageContainer::VScroll(QWidget *hDlg, int pos, QWidget *hControl)
#endif // __linux__
{
#ifndef __linux__
	HWND hPage = CurrentPage()->HPage();

	SCROLLINFO scrollinfo;
	scrollinfo.cbSize = sizeof(SCROLLINFO);
	GetScrollInfo(hControl, SB_CTL, &scrollinfo);
	int pos = -1;
#else // __linux__
	// the scroll bar handles the line/page/thumb requests itself and reports the new position
	QWidget *hPage = CurrentPage()->HPage();
#endif // __linux__

#ifndef __linux__
	switch (request) {
	case SB_BOTTOM:
		pos = m_vScrollRange;
		break;
	case SB_TOP:
		pos = 0;
		break;
	case SB_LINEDOWN:
		pos = min(m_vScrollPos + 10, m_vScrollRange);
		break;
	case SB_LINEUP:
		pos = max(m_vScrollPos - 10, 0);
		break;
	case SB_PAGEDOWN:
		pos = min(m_vScrollPos + m_vScrollPage, m_vScrollRange);
		break;
	case SB_PAGEUP:
		pos = max(m_vScrollPos - m_vScrollPage, 0);
		break;
	case SB_THUMBPOSITION:
	case SB_THUMBTRACK:
		pos = curpos;
		break;
	}
#endif // !__linux__
	if (pos >= 0 && pos != m_vScrollPos) {
		int dy = -(pos - m_vScrollPos);
#ifndef __linux__
		scrollinfo.nPos = m_vScrollPos = pos;
		scrollinfo.fMask = SIF_POS;
		SetScrollInfo(hControl, SB_CTL, &scrollinfo, TRUE);
		ScrollWindow(hPage, 0, dy, NULL, NULL);
		UpdateWindow(hPage);
#else // __linux__
		m_vScrollPos = pos;
		hPage->scroll(0, dy);
#endif // __linux__
	}
	return FALSE;
}

// ----------------------------------------------------------------------

void OptionsPageContainer::UpdatePages(bool resetView)
{
	for (auto pPage : m_pPage)
		pPage->UpdateControls(pPage->HPage());
#ifndef __linux__
	if (resetView)
		TreeView_SelectItem(m_hPageList, TreeView_GetRoot(m_hPageList));
#else // __linux__
	if (resetView) {
		QTreeWidget *hTree = qobject_cast<QTreeWidget*>(m_hPageList);
		hTree->setCurrentItem(hTree->topLevelItem(0));
	}
#endif // __linux__
}

// ----------------------------------------------------------------------

void OptionsPageContainer::UpdateConfig()
{
	for (auto pPage : m_pPage)
		pPage->UpdateConfig(pPage->HPage());
}

// ======================================================================

OptionsPage::OptionsPage(OptionsPageContainer* container)
	: m_container(container)
	, m_hPage(0)
	, m_hItem(0)
{
}

// ----------------------------------------------------------------------

OptionsPage::~OptionsPage()
{
	// Remove the object reference from the window.
#ifndef __linux__
	// This is so that object methods will no longer be called from the message loop
	// while the window hasn't been destroyed.
#else // __linux__
	// The page window is owned by the container control and goes with it; its connections name the window
	// as context, so they end with it.
#endif // __linux__
	if (m_hPage)
#ifndef __linux__
		SetWindowLongPtr(m_hPage, DWLP_USER, 0);
#else // __linux__
		m_hPage->setProperty("OptionsPage", QVariant());
#endif // __linux__
}

// ----------------------------------------------------------------------

void OptionsPage::Show(bool bShow)
{
#ifndef __linux__
	ShowWindow(m_hPage, bShow ? SW_SHOW : SW_HIDE);
#else // __linux__
	m_hPage->setVisible(bShow);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
HWND OptionsPage::HParent() const
#else // __linux__
QWidget *OptionsPage::HParent() const
#endif // __linux__
{
	return m_container->ContainerControl()->HWnd();
}

// ----------------------------------------------------------------------

#ifndef __linux__
HTREEITEM OptionsPage::CreatePage(HWND hDlg, HTREEITEM parent)
#else // __linux__
QTreeWidgetItem *OptionsPage::CreatePage(QWidget *hDlg, QTreeWidgetItem *parent)
#endif // __linux__
{
	int winId = ResourceId();
#ifndef __linux__
	m_hPage = CreateDialogParam(g_pOrbiter->GetInstance(), MAKEINTRESOURCE(winId), HParent(), s_DlgProc, (LPARAM)this);
	if (!m_hPage) {
		DWORD err = GetLastError();
		int i = 1;
#else // __linux__
	m_hPage = oapiCreateResDialog(g_pOrbiter->GetInstance(), winId, HParent());
	if (m_hPage) {
		m_hPage->setProperty("OptionsPage", QVariant::fromValue((void*)this)); // DWLP_USER
		DlgProc(m_hPage);
		OnInitDialog(m_hPage); // WM_INITDIALOG
#endif // __linux__
	}

#ifndef __linux__
	char cbuf[256];
	strcpy(cbuf, Name());
	TV_INSERTSTRUCT tvis;
	tvis.item.mask = TVIF_TEXT | TVIF_PARAM;
	tvis.item.pszText = cbuf;
	tvis.item.lParam = (LPARAM)this;
	tvis.hInsertAfter = TVI_LAST;
	tvis.hParent = parent;
	HTREEITEM hti = TreeView_InsertItem(GetDlgItem(hDlg, IDC_OPT_PAGELIST), &tvis);
#else // __linux__
	QTreeWidgetItem *hti = new QTreeWidgetItem();
	hti->setText(0, QString::fromUtf8(Name()));
	hti->setData(0, Qt::UserRole, QVariant::fromValue((void*)this));
	if (parent) parent->addChild(hti);
	else DlgItem<QTreeWidget>(hDlg, IDC_OPT_PAGELIST)->addTopLevelItem(hti);
#endif // __linux__
	m_hItem = hti;
	return hti;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage::OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage::OnInitDialog(QWidget *hWnd)
#endif // __linux__
{
	UpdateControls(hWnd);
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
INT_PTR OptionsPage::DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return OnInitDialog(hWnd, wParam, lParam);
	case WM_COMMAND:
		return OnCommand(hWnd, LOWORD(wParam), HIWORD(wParam), (HWND)lParam);
	case WM_HSCROLL:
		return OnHScroll(hWnd, wParam, lParam);
	case WM_NOTIFY:
		return OnNotify(hWnd, (DWORD)wParam, (NMHDR*)lParam);
	default:
		return OnMessage(hWnd, uMsg, wParam, lParam);
	}
}

// ----------------------------------------------------------------------

INT_PTR CALLBACK OptionsPage::s_DlgProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void OptionsPage::DlgProc(QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage* pPage;
	switch (uMsg) {
	case WM_INITDIALOG:
		EnableThemeDialogTexture(hWnd, ETDT_ENABLE);
		SetWindowLongPtr(hWnd, DWLP_USER, lParam);
		pPage = (OptionsPage*)lParam;
		break;
	default:
		pPage = (OptionsPage*)GetWindowLongPtr(hWnd, DWLP_USER);
		break;
#else // __linux__
	// WM_COMMAND
	oapiConnectDlgCommands(hWnd, [this, hWnd](int id, int code, QWidget *hCtrl) { OnCommand(hWnd, id, code, hCtrl); });
	for (QObject *o : hWnd->children()) {
		int id = oapiResId(qobject_cast<QWidget*>(o));
		// WM_HSCROLL of gauge controls
		if (GaugeCtrl *g = qobject_cast<GaugeCtrl*>(o))
			QObject::connect(g, &GaugeCtrl::scrolled, hWnd, [this, hWnd, id](int request, int pos) { OnHScroll(hWnd, id, request, pos); });
		// WM_NOTIFY UDN_DELTAPOS of up-down controls
		else if (ResUpDown *ud = qobject_cast<ResUpDown*>(o))
			QObject::connect(ud, &ResUpDown::deltaPos, hWnd, [this, hWnd, id](int iDelta) { OnDeltaPos(hWnd, id, iDelta); });
#endif // __linux__
	}
#ifndef __linux__
	return (pPage ? pPage->DlgProc(hWnd, uMsg, wParam, lParam) : DefWindowProc(hWnd, uMsg, wParam, lParam));
#else // __linux__
	// other window events
	new EventHook(hWnd, [this, hWnd](QObject *obj, QEvent *event) { return obj == hWnd && OnMessage(hWnd, event); });
#endif // __linux__
}

// ======================================================================

OptionsPage_Visual::OptionsPage_Visual(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Visual::ResourceId() const
{
	return IDD_OPTIONS_VISUAL;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Visual::Name() const
{
	const char* name = "Visual settings";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Visual::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/tab_visual.htm"); // this needs to be updated
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Visual::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Visual::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	char cbuf[256];

#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_VIS_CLOUD, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bClouds ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_CSHADOW, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bCloudShadows ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_HAZE, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bHaze ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_FOG, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bFog ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_REFWATER, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bWaterreflect ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_RIPPLE, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bSpecularRipple ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_LIGHTS, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bNightlights ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_VIS_CLOUD, Cfg()->CfgVisualPrm.bClouds);
	SetCheck(hPage, IDC_OPT_VIS_CSHADOW, Cfg()->CfgVisualPrm.bCloudShadows);
	SetCheck(hPage, IDC_OPT_VIS_HAZE, Cfg()->CfgVisualPrm.bHaze);
	SetCheck(hPage, IDC_OPT_VIS_FOG, Cfg()->CfgVisualPrm.bFog);
	SetCheck(hPage, IDC_OPT_VIS_REFWATER, Cfg()->CfgVisualPrm.bWaterreflect);
	SetCheck(hPage, IDC_OPT_VIS_RIPPLE, Cfg()->CfgVisualPrm.bSpecularRipple);
	SetCheck(hPage, IDC_OPT_VIS_LIGHTS, Cfg()->CfgVisualPrm.bNightlights);
#endif // __linux__
	sprintf(cbuf, "%0.2f", Cfg()->CfgVisualPrm.LightBrightness);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_VIS_LTLEVEL), cbuf);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_ELEV, BM_SETCHECK,
		Cfg()->CfgVisualPrm.ElevMode ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_ELEVMODE, CB_SETCURSEL, Cfg()->CfgVisualPrm.ElevMode < 2 ? 0 : 1, 0);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_VIS_LTLEVEL, cbuf);
	SetCheck(hPage, IDC_OPT_VIS_ELEV, Cfg()->CfgVisualPrm.ElevMode);
	DlgItem<QComboBox>(hPage, IDC_OPT_VIS_ELEVMODE)->setCurrentIndex(Cfg()->CfgVisualPrm.ElevMode < 2 ? 0 : 1);
#endif // __linux__
	sprintf(cbuf, "%d", Cfg()->CfgVisualPrm.PlanetMaxLevel);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_VIS_MAXLEVEL), cbuf);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_VSHADOW, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bVesselShadows ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_REENTRY, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bReentryFlames ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_SHADOW, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bShadows ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_PARTICLE, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bParticleStreams ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_SPECULAR, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bSpecular ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_LOCALLIGHT, BM_SETCHECK,
		Cfg()->CfgVisualPrm.bLocalLight ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_VIS_MAXLEVEL, cbuf);
	SetCheck(hPage, IDC_OPT_VIS_VSHADOW, Cfg()->CfgVisualPrm.bVesselShadows);
	SetCheck(hPage, IDC_OPT_VIS_REENTRY, Cfg()->CfgVisualPrm.bReentryFlames);
	SetCheck(hPage, IDC_OPT_VIS_SHADOW, Cfg()->CfgVisualPrm.bShadows);
	SetCheck(hPage, IDC_OPT_VIS_PARTICLE, Cfg()->CfgVisualPrm.bParticleStreams);
	SetCheck(hPage, IDC_OPT_VIS_SPECULAR, Cfg()->CfgVisualPrm.bSpecular);
	SetCheck(hPage, IDC_OPT_VIS_LOCALLIGHT, Cfg()->CfgVisualPrm.bLocalLight);
#endif // __linux__
	sprintf(cbuf, "%d", Cfg()->CfgVisualPrm.AmbientLevel);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_VIS_AMBIENT), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_VIS_AMBIENT, cbuf);
#endif // __linux__

	VisualsChanged(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Visual::UpdateConfig(HWND hPage)
#else // __linux__
void OptionsPage_Visual::UpdateConfig(QWidget *hPage)
#endif // __linux__
{
	char cbuf[256];
	DWORD i;
	double d;

#ifndef __linux__
	Cfg()->CfgVisualPrm.bClouds = (SendDlgItemMessage(hPage, IDC_OPT_VIS_CLOUD, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bCloudShadows = (SendDlgItemMessage(hPage, IDC_OPT_VIS_CSHADOW, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bHaze = (SendDlgItemMessage(hPage, IDC_OPT_VIS_HAZE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bFog = (SendDlgItemMessage(hPage, IDC_OPT_VIS_FOG, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bWaterreflect = (SendDlgItemMessage(hPage, IDC_OPT_VIS_REFWATER, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bSpecularRipple = (SendDlgItemMessage(hPage, IDC_OPT_VIS_RIPPLE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bNightlights = (SendDlgItemMessage(hPage, IDC_OPT_VIS_LIGHTS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	GetWindowText(GetDlgItem(hPage, IDC_OPT_VIS_LTLEVEL), cbuf, 255);
#else // __linux__
	Cfg()->CfgVisualPrm.bClouds = (IsChecked(hPage, IDC_OPT_VIS_CLOUD));
	Cfg()->CfgVisualPrm.bCloudShadows = (IsChecked(hPage, IDC_OPT_VIS_CSHADOW));
	Cfg()->CfgVisualPrm.bHaze = (IsChecked(hPage, IDC_OPT_VIS_HAZE));
	Cfg()->CfgVisualPrm.bFog = (IsChecked(hPage, IDC_OPT_VIS_FOG));
	Cfg()->CfgVisualPrm.bWaterreflect = (IsChecked(hPage, IDC_OPT_VIS_REFWATER));
	Cfg()->CfgVisualPrm.bSpecularRipple = (IsChecked(hPage, IDC_OPT_VIS_RIPPLE));
	Cfg()->CfgVisualPrm.bNightlights = (IsChecked(hPage, IDC_OPT_VIS_LIGHTS));
	oapiGetDlgItemText(hPage, IDC_OPT_VIS_LTLEVEL, cbuf, 255);
#endif // __linux__
	if (!sscanf(cbuf, "%lf", &d)) d = 0.5; else if (d < 0) d = 0.0; else if (d > 1) d = 1.0;
	Cfg()->CfgVisualPrm.LightBrightness = d;
#ifndef __linux__
	Cfg()->CfgVisualPrm.ElevMode = (SendDlgItemMessage(hPage, IDC_OPT_VIS_ELEV, BM_GETCHECK, 0, 0) != BST_CHECKED ?
		0 : SendDlgItemMessage(hPage, IDC_OPT_VIS_ELEVMODE, CB_GETCURSEL, 0, 0) + 1);
	GetWindowText(GetDlgItem(hPage, IDC_OPT_VIS_MAXLEVEL), cbuf, 127);
	if (!sscanf(cbuf, "%lu", &i)) i = SURF_MAX_PATCHLEVEL2;
#else // __linux__
	Cfg()->CfgVisualPrm.ElevMode = (!IsChecked(hPage, IDC_OPT_VIS_ELEV) ?
		0 : DlgItem<QComboBox>(hPage, IDC_OPT_VIS_ELEVMODE)->currentIndex() + 1);
	oapiGetDlgItemText(hPage, IDC_OPT_VIS_MAXLEVEL, cbuf, 127);
	if (!sscanf(cbuf, "%u", &i)) i = SURF_MAX_PATCHLEVEL2;
#endif // __linux__
	Cfg()->CfgVisualPrm.PlanetMaxLevel = max((DWORD)1, min((DWORD)SURF_MAX_PATCHLEVEL2, i));
#ifndef __linux__
	Cfg()->CfgVisualPrm.bVesselShadows = (SendDlgItemMessage(hPage, IDC_OPT_VIS_VSHADOW, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bReentryFlames = (SendDlgItemMessage(hPage, IDC_OPT_VIS_REENTRY, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bShadows = (SendDlgItemMessage(hPage, IDC_OPT_VIS_SHADOW, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bParticleStreams = (SendDlgItemMessage(hPage, IDC_OPT_VIS_PARTICLE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bSpecular = (SendDlgItemMessage(hPage, IDC_OPT_VIS_SPECULAR, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgVisualPrm.bLocalLight = (SendDlgItemMessage(hPage, IDC_OPT_VIS_LOCALLIGHT, BM_GETCHECK, 0, 0) == BST_CHECKED);
	GetWindowText(GetDlgItem(hPage, IDC_OPT_VIS_AMBIENT), cbuf, 255);
	if (!sscanf(cbuf, "%lu", &i)) i = 15; else if (i > 255) i = 255;
#else // __linux__
	Cfg()->CfgVisualPrm.bVesselShadows = (IsChecked(hPage, IDC_OPT_VIS_VSHADOW));
	Cfg()->CfgVisualPrm.bReentryFlames = (IsChecked(hPage, IDC_OPT_VIS_REENTRY));
	Cfg()->CfgVisualPrm.bShadows = (IsChecked(hPage, IDC_OPT_VIS_SHADOW));
	Cfg()->CfgVisualPrm.bParticleStreams = (IsChecked(hPage, IDC_OPT_VIS_PARTICLE));
	Cfg()->CfgVisualPrm.bSpecular = (IsChecked(hPage, IDC_OPT_VIS_SPECULAR));
	Cfg()->CfgVisualPrm.bLocalLight = (IsChecked(hPage, IDC_OPT_VIS_LOCALLIGHT));
	oapiGetDlgItemText(hPage, IDC_OPT_VIS_AMBIENT, cbuf, 255);
	if (!sscanf(cbuf, "%u", &i)) i = 15; else if (i > 255) i = 255;
#endif // __linux__
	Cfg()->SetAmbientLevel(i);
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Visual::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Visual::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_ELEVMODE, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VIS_ELEVMODE, CB_ADDSTRING, 0, (LPARAM)"linear interpolation");
	SendDlgItemMessage(hPage, IDC_OPT_VIS_ELEVMODE, CB_ADDSTRING, 0, (LPARAM)"cubic interpolation");
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
	DlgItem<QComboBox>(hPage, IDC_OPT_VIS_ELEVMODE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_VIS_ELEVMODE), "linear interpolation");
	oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_VIS_ELEVMODE), "cubic interpolation");
#endif // __linux__
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Visual::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_Visual::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId)
	{
		case IDC_OPT_VIS_CLOUD:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_CLOUD, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_CLOUD));
#endif // __linux__
				Cfg()->CfgVisualPrm.bClouds = check;
				VisualsChanged( hPage );
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_CSHADOW:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_CSHADOW, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_CSHADOW));
#endif // __linux__
				Cfg()->CfgVisualPrm.bCloudShadows = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_HAZE:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_HAZE, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_HAZE));
#endif // __linux__
				Cfg()->CfgVisualPrm.bHaze = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_FOG:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_FOG, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_FOG));
#endif // __linux__
				Cfg()->CfgVisualPrm.bFog = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_REFWATER:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_REFWATER, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_REFWATER));
#endif // __linux__
				Cfg()->CfgVisualPrm.bWaterreflect = check;
				VisualsChanged( hPage );
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_RIPPLE:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_RIPPLE, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_RIPPLE));
#endif // __linux__
				Cfg()->CfgVisualPrm.bSpecularRipple = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_LIGHTS:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_LIGHTS, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_LIGHTS));
#endif // __linux__
				Cfg()->CfgVisualPrm.bNightlights = check;
				VisualsChanged( hPage );
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_LTLEVEL:
#ifndef __linux__
			if (notification == EN_CHANGE)
#else // __linux__
			if (notification == RESN_CHANGE)
#endif // __linux__
			{
				char cbuf[16];
				double d;
#ifndef __linux__
				GetWindowText( GetDlgItem( hPage, IDC_OPT_VIS_LTLEVEL ), cbuf, 16 );
#else // __linux__
				oapiGetDlgItemText(hPage, IDC_OPT_VIS_LTLEVEL, cbuf, 16);
#endif // __linux__
				if (!sscanf( cbuf, "%lf", &d )) d = 0.5;
				else if (d < 0) d = 0.0;
				else if (d > 1) d = 1.0;
				Cfg()->CfgVisualPrm.LightBrightness = d;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_ELEV:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				int elevmode = SendDlgItemMessage( hPage, IDC_OPT_VIS_ELEV, BM_GETCHECK, 0, 0 ) != BST_CHECKED ? 0 : (SendDlgItemMessage( hPage, IDC_OPT_VIS_ELEVMODE, CB_GETCURSEL, 0, 0 ) + 1);
#else // __linux__
				int elevmode = !IsChecked(hPage, IDC_OPT_VIS_ELEV) ? 0 : (DlgItem<QComboBox>(hPage, IDC_OPT_VIS_ELEVMODE)->currentIndex() + 1);
#endif // __linux__
				Cfg()->CfgVisualPrm.ElevMode = elevmode;
				VisualsChanged( hPage );
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_ELEVMODE:
#ifndef __linux__
			if (notification == CBN_SELCHANGE)
#else // __linux__
			if (notification == RESN_SELCHANGE)
#endif // __linux__
			{
#ifndef __linux__
				int elevmode = SendDlgItemMessage( hPage, IDC_OPT_VIS_ELEV, BM_GETCHECK, 0, 0 ) != BST_CHECKED ? 0 : (SendDlgItemMessage( hPage, IDC_OPT_VIS_ELEVMODE, CB_GETCURSEL, 0, 0 ) + 1);
#else // __linux__
				int elevmode = !IsChecked(hPage, IDC_OPT_VIS_ELEV) ? 0 : (DlgItem<QComboBox>(hPage, IDC_OPT_VIS_ELEVMODE)->currentIndex() + 1);
#endif // __linux__
				Cfg()->CfgVisualPrm.ElevMode = elevmode;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_MAXLEVEL:
#ifndef __linux__
			if (notification == EN_CHANGE)
#else // __linux__
			if (notification == RESN_CHANGE)
#endif // __linux__
			{
				char cbuf[16];
				DWORD i;
#ifndef __linux__
				GetWindowText( GetDlgItem( hPage, IDC_OPT_VIS_MAXLEVEL ), cbuf, 16 );
				if (!sscanf( cbuf, "%lu", &i )) i = SURF_MAX_PATCHLEVEL2;
#else // __linux__
				oapiGetDlgItemText(hPage, IDC_OPT_VIS_MAXLEVEL, cbuf, 16);
				if (!sscanf( cbuf, "%u", &i )) i = SURF_MAX_PATCHLEVEL2;
#endif // __linux__
				Cfg()->CfgVisualPrm.PlanetMaxLevel = max((DWORD)1, min((DWORD)SURF_MAX_PATCHLEVEL2, i));
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_VSHADOW:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_VSHADOW, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_VSHADOW));
#endif // __linux__
				Cfg()->CfgVisualPrm.bVesselShadows = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_REENTRY:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_REENTRY, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_REENTRY));
#endif // __linux__
				Cfg()->CfgVisualPrm.bReentryFlames = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_SHADOW:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_SHADOW, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_SHADOW));
#endif // __linux__
				Cfg()->CfgVisualPrm.bShadows = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_PARTICLE:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_PARTICLE, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_PARTICLE));
#endif // __linux__
				Cfg()->CfgVisualPrm.bParticleStreams = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_SPECULAR:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_SPECULAR, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_SPECULAR));
#endif // __linux__
				Cfg()->CfgVisualPrm.bSpecular = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_LOCALLIGHT:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_VIS_LOCALLIGHT, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_VIS_LOCALLIGHT));
#endif // __linux__
				Cfg()->CfgVisualPrm.bLocalLight = check;
				return FALSE;
			}
			break;
		case IDC_OPT_VIS_AMBIENT:
#ifndef __linux__
			if (notification == EN_CHANGE)
#else // __linux__
			if (notification == RESN_CHANGE)
#endif // __linux__
			{
				char cbuf[16];
				DWORD i;
#ifndef __linux__
				GetWindowText( GetDlgItem(hPage, IDC_OPT_VIS_AMBIENT ), cbuf, 16 );
				if (!sscanf( cbuf, "%lu", &i )) i = 15;
#else // __linux__
				oapiGetDlgItemText(hPage, IDC_OPT_VIS_AMBIENT, cbuf, 16);
				if (!sscanf( cbuf, "%u", &i )) i = 15;
#endif // __linux__
				else if (i > 255) i = 255;
				Cfg()->SetAmbientLevel( i );
				return FALSE;
			}
			break;
	}
	return TRUE;
}

//-----------------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Visual::VisualsChanged(HWND hPage)
#else // __linux__
void OptionsPage_Visual::VisualsChanged(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	EnableWindow(GetDlgItem(hPage, IDC_OPT_VIS_CSHADOW),
		SendDlgItemMessage(hPage, IDC_OPT_VIS_CLOUD, BM_GETCHECK, 0, 0) == BST_CHECKED);
	EnableWindow(GetDlgItem(hPage, IDC_OPT_VIS_RIPPLE),
		SendDlgItemMessage(hPage, IDC_OPT_VIS_REFWATER, BM_GETCHECK, 0, 0) == BST_CHECKED);
	EnableWindow( GetDlgItem( hPage, IDC_OPT_VIS_ELEVMODE ), SendDlgItemMessage( hPage, IDC_OPT_VIS_ELEV, BM_GETCHECK, 0, 0 ) == BST_CHECKED );
	EnableWindow( GetDlgItem( hPage, IDC_OPT_VIS_LTLEVEL ), SendDlgItemMessage( hPage, IDC_OPT_VIS_LIGHTS, BM_GETCHECK, 0, 0 ) == BST_CHECKED );
#else // __linux__
	EnableItem(hPage, IDC_OPT_VIS_CSHADOW, IsChecked(hPage, IDC_OPT_VIS_CLOUD));
	EnableItem(hPage, IDC_OPT_VIS_RIPPLE, IsChecked(hPage, IDC_OPT_VIS_REFWATER));
	EnableItem(hPage, IDC_OPT_VIS_ELEVMODE, IsChecked(hPage, IDC_OPT_VIS_ELEV));
	EnableItem(hPage, IDC_OPT_VIS_LTLEVEL, IsChecked(hPage, IDC_OPT_VIS_LIGHTS));
#endif // __linux__
	return;
}

// ======================================================================

OptionsPage_Physics::OptionsPage_Physics(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Physics::ResourceId() const
{
	return IDD_OPTIONS_PHYSICS;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Physics::Name() const
{
	const char* name = "Physics settings";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Physics::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/tab_param.htm"); // this needs to be updated
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Physics::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Physics::UpdateControls(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_PHYS_COMPLEXGRAV, BM_SETCHECK,
		Cfg()->CfgPhysicsPrm.bNonsphericalGrav ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PHYS_RPRESSURE, BM_SETCHECK,
		Cfg()->CfgPhysicsPrm.bRadiationPressure ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PHYS_DISTMASS, BM_SETCHECK,
		Cfg()->CfgPhysicsPrm.bDistributedMass ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PHYS_WIND, BM_SETCHECK,
		Cfg()->CfgPhysicsPrm.bAtmWind ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_PHYS_COMPLEXGRAV, Cfg()->CfgPhysicsPrm.bNonsphericalGrav);
	SetCheck(hPage, IDC_OPT_PHYS_RPRESSURE, Cfg()->CfgPhysicsPrm.bRadiationPressure);
	SetCheck(hPage, IDC_OPT_PHYS_DISTMASS, Cfg()->CfgPhysicsPrm.bDistributedMass);
	SetCheck(hPage, IDC_OPT_PHYS_WIND, Cfg()->CfgPhysicsPrm.bAtmWind);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Physics::UpdateConfig(HWND hPage)
#else // __linux__
void OptionsPage_Physics::UpdateConfig(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	Cfg()->CfgPhysicsPrm.bDistributedMass = (SendDlgItemMessage(hPage, IDC_OPT_PHYS_DISTMASS, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgPhysicsPrm.bNonsphericalGrav = (SendDlgItemMessage(hPage, IDC_OPT_PHYS_COMPLEXGRAV, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgPhysicsPrm.bRadiationPressure = (SendDlgItemMessage(hPage, IDC_OPT_PHYS_RPRESSURE, BM_GETCHECK, 0, 0) == BST_CHECKED);
	Cfg()->CfgPhysicsPrm.bAtmWind = (SendDlgItemMessage(hPage, IDC_OPT_PHYS_WIND, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
	Cfg()->CfgPhysicsPrm.bDistributedMass = (IsChecked(hPage, IDC_OPT_PHYS_DISTMASS));
	Cfg()->CfgPhysicsPrm.bNonsphericalGrav = (IsChecked(hPage, IDC_OPT_PHYS_COMPLEXGRAV));
	Cfg()->CfgPhysicsPrm.bRadiationPressure = (IsChecked(hPage, IDC_OPT_PHYS_RPRESSURE));
	Cfg()->CfgPhysicsPrm.bAtmWind = (IsChecked(hPage, IDC_OPT_PHYS_WIND));
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Physics::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Physics::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
#endif // __linux__
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Physics::OnCommand( HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl )
#else // __linux__
BOOL OptionsPage_Physics::OnCommand( QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl )
#endif // __linux__
{
	switch (ctrlId)
	{
		case IDC_OPT_PHYS_DISTMASS:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_PHYS_DISTMASS, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_PHYS_DISTMASS));
#endif // __linux__
				Cfg()->CfgPhysicsPrm.bDistributedMass = check;
				return FALSE;
			}
			break;
		case IDC_OPT_PHYS_COMPLEXGRAV:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_PHYS_COMPLEXGRAV, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_PHYS_COMPLEXGRAV));
#endif // __linux__
				Cfg()->CfgPhysicsPrm.bNonsphericalGrav = check;
				return FALSE;
			}
			break;
		case IDC_OPT_PHYS_RPRESSURE:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_PHYS_RPRESSURE, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_PHYS_RPRESSURE));
#endif // __linux__
				Cfg()->CfgPhysicsPrm.bRadiationPressure = check;
				return FALSE;
			}
			break;
		case IDC_OPT_PHYS_WIND:
#ifndef __linux__
			if (notification == BN_CLICKED)
#else // __linux__
			if (notification == RESN_CLICKED)
#endif // __linux__
			{
#ifndef __linux__
				bool check = (SendDlgItemMessage( hPage, IDC_OPT_PHYS_WIND, BM_GETCHECK, 0, 0 ) == BST_CHECKED);
#else // __linux__
				bool check = (IsChecked(hPage, IDC_OPT_PHYS_WIND));
#endif // __linux__
				Cfg()->CfgPhysicsPrm.bAtmWind = check;
				return FALSE;
			}
			break;
	}
	return TRUE;
}

// ======================================================================

OptionsPage_Instrument::OptionsPage_Instrument(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Instrument::ResourceId() const
{
	return IDD_OPTIONS_INSTRUMENT;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Instrument::Name() const
{
	const char* name = "Instruments & panels";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Instrument::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/tab_param.htm"); // this needs to be updated
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Instrument::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Instrument::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	char cbuf[256];
	double mfdUpdDt = Cfg()->CfgLogicPrm.InstrUpdDT;
	sprintf(cbuf, "%0.2f", mfdUpdDt);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_MFD_INTERVAL), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_MFD_INTERVAL, cbuf);
#endif // __linux__
	int mfdSize = Cfg()->CfgLogicPrm.MFDSize;
	sprintf(cbuf, "%d", mfdSize);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_MFD_SIZE), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_MFD_SIZE, cbuf);
#endif // __linux__
	bool enable = Cfg()->CfgLogicPrm.bMfdTransparent;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_MFD_TRANSP, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_MFD_TRANSP, enable);
#endif // __linux__
	int vcmfdsize = Cfg()->CfgInstrumentPrm.VCMFDSize;
	int idx = (vcmfdsize == 1024 ? 2 : vcmfdsize == 512 ? 1 : 0);
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_MFD_VCTEXSIZE, CB_SETCURSEL, idx, 0);
#else // __linux__
	DlgItem<QComboBox>(hPage, IDC_OPT_MFD_VCTEXSIZE)->setCurrentIndex(idx);
#endif // __linux__
	double scrollSpeed = Cfg()->CfgLogicPrm.PanelScrollSpeed;
	sprintf(cbuf, "%0.0f", scrollSpeed * 0.1);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_PANEL_SCROLLSPEED), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_PANEL_SCROLLSPEED, cbuf);
#endif // __linux__
	double panelSize = Cfg()->CfgLogicPrm.PanelScale;
	sprintf(cbuf, "%0.2f", panelSize);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_PANEL_SCALE), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_PANEL_SCALE, cbuf);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Instrument::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Instrument::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
	SendDlgItemMessage(hPage, IDC_OPT_MFD_VCTEXSIZE, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MFD_VCTEXSIZE, CB_ADDSTRING, 0, (LPARAM)"256 x 256");
	SendDlgItemMessage(hPage, IDC_OPT_MFD_VCTEXSIZE, CB_ADDSTRING, 0, (LPARAM)"512 x 512");
	SendDlgItemMessage(hPage, IDC_OPT_MFD_VCTEXSIZE, CB_ADDSTRING, 0, (LPARAM)"1024 x 1024");
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
	DlgItem<QComboBox>(hPage, IDC_OPT_MFD_VCTEXSIZE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_MFD_VCTEXSIZE), "256 x 256");
	oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_MFD_VCTEXSIZE), "512 x 512");
	oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_MFD_VCTEXSIZE), "1024 x 1024");
#endif // __linux__
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Instrument::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_Instrument::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_MFD_INTERVAL:
#ifndef __linux__
		if (notification == EN_CHANGE) {
#else // __linux__
		if (notification == RESN_CHANGE) {
#endif // __linux__
			char cbuf[256];
			double updDt;
#ifndef __linux__
			GetWindowText(GetDlgItem(hPage, IDC_OPT_MFD_INTERVAL), cbuf, 255);
#else // __linux__
			oapiGetDlgItemText(hPage, IDC_OPT_MFD_INTERVAL, cbuf, 255);
#endif // __linux__
			if (sscanf(cbuf, "%lf", &updDt)) {
				Cfg()->CfgLogicPrm.InstrUpdDT = max(0.01, updDt);
				g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDUPDATEINTERVAL);
			}
			return FALSE;
		}
		break;
	case IDC_OPT_MFD_SIZE:
#ifndef __linux__
		if (notification == EN_CHANGE) {
#else // __linux__
		if (notification == RESN_CHANGE) {
#endif // __linux__
			char cbuf[256];
			int size;
#ifndef __linux__
			GetWindowText(GetDlgItem(hPage, IDC_OPT_MFD_SIZE), cbuf, 256);
#else // __linux__
			oapiGetDlgItemText(hPage, IDC_OPT_MFD_SIZE, cbuf, 256);
#endif // __linux__
			if (sscanf(cbuf, "%d", &size)) {
				Cfg()->CfgLogicPrm.MFDSize = max(1, min(10, size));
				g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDGENERICSIZE);
			}
			return FALSE;
		}
		break;
	case IDC_OPT_MFD_TRANSP:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, IDC_OPT_MFD_TRANSP, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, IDC_OPT_MFD_TRANSP));
#endif // __linux__
			Cfg()->CfgLogicPrm.bMfdTransparent = check;
			g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDGENERICTRANSP);
			return FALSE;
		}
		break;
	case IDC_OPT_MFD_VCTEXSIZE:
#ifndef __linux__
		if (notification == CBN_SELCHANGE) {
#else // __linux__
		if (notification == RESN_SELCHANGE) {
#endif // __linux__
			int vcmfdsize[3] = { 256, 512, 1024 };
#ifndef __linux__
			DWORD idx = (DWORD)SendDlgItemMessage(hPage, IDC_OPT_MFD_VCTEXSIZE, CB_GETCURSEL, 0, 0);
#else // __linux__
			DWORD idx = DlgItem<QComboBox>(hPage, IDC_OPT_MFD_VCTEXSIZE)->currentIndex();
#endif // __linux__
			Cfg()->CfgInstrumentPrm.VCMFDSize = vcmfdsize[idx];
			g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDVCSIZE);
		}
		break;
	case IDC_OPT_PANEL_SCROLLSPEED:
#ifndef __linux__
		if (notification == EN_CHANGE) {
#else // __linux__
		if (notification == RESN_CHANGE) {
#endif // __linux__
			char cbuf[256];
			double speed;
#ifndef __linux__
			GetWindowText(GetDlgItem(hPage, IDC_OPT_PANEL_SCROLLSPEED), cbuf, 256);
#else // __linux__
			oapiGetDlgItemText(hPage, IDC_OPT_PANEL_SCROLLSPEED, cbuf, 256);
#endif // __linux__
			if (sscanf(cbuf, "%lf", &speed)) {
				Cfg()->CfgLogicPrm.PanelScrollSpeed = 10.0 * max(-100.0, min(100.0, speed));
				g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_PANELSCROLLSPEED);
			}
			return FALSE;
		}
		break;
	case IDC_OPT_PANEL_SCALE:
#ifndef __linux__
		if (notification == EN_CHANGE) {
#else // __linux__
		if (notification == RESN_CHANGE) {
#endif // __linux__
			char cbuf[256];
			double scale;
#ifndef __linux__
			GetWindowText(GetDlgItem(hPage, IDC_OPT_PANEL_SCALE), cbuf, 256);
#else // __linux__
			oapiGetDlgItemText(hPage, IDC_OPT_PANEL_SCALE, cbuf, 256);
#endif // __linux__
			if (sscanf(cbuf, "%lf", &scale)) {
				Cfg()->CfgLogicPrm.PanelScale = max(0.25, min(4.0, scale));
				g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_PANELSCALE);
			}
			return FALSE;
		}
		break;
	}
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Instrument::OnNotify(HWND hPage, DWORD ctrlId, const NMHDR* pNmHdr)
#else // __linux__
BOOL OptionsPage_Instrument::OnDeltaPos(QWidget *hPage, int ctrlId, int iDelta)
#endif // __linux__
{
#ifndef __linux__
	if (pNmHdr->code == UDN_DELTAPOS) {
		NMUPDOWN* nmud = (NMUPDOWN*)pNmHdr;
		int delta = -nmud->iDelta;
		switch (pNmHdr->idFrom) {
#else // __linux__
	{ // UDN_DELTAPOS
		int delta = -iDelta;
		switch (ctrlId) {
#endif // __linux__
		case IDC_OPT_MFD_INTERVALSPIN:
			Cfg()->CfgLogicPrm.InstrUpdDT = max(0.01, Cfg()->CfgLogicPrm.InstrUpdDT + delta * 0.01);
			g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDUPDATEINTERVAL);
			break;
		case IDC_OPT_MFD_SIZESPIN:
			Cfg()->CfgLogicPrm.MFDSize = max(1, min(10, Cfg()->CfgLogicPrm.MFDSize + delta));
			g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_MFDGENERICSIZE);
			break;
		case IDC_OPT_PANEL_SCROLLSPEEDSPIN:
			Cfg()->CfgLogicPrm.PanelScrollSpeed = 10.0 * max(-100.0, min(100.0, 0.1 * Cfg()->CfgLogicPrm.PanelScrollSpeed + delta));
			g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_PANELSCROLLSPEED);
			break;
		case IDC_OPT_PANEL_SCALESPIN:
			Cfg()->CfgLogicPrm.PanelScale = max(0.25, min(4.0, Cfg()->CfgLogicPrm.PanelScale + delta * 0.01));
			g_pOrbiter->OnOptionChanged(OPTCAT_INSTRUMENT, OPTITEM_INSTRUMENT_PANELSCALE);
			break;
		}
		UpdateControls(hPage);
		return TRUE;
	}
#ifndef __linux__
	return FALSE;
#endif // !__linux__
}

// ======================================================================

OptionsPage_Vessel::OptionsPage_Vessel(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Vessel::ResourceId() const
{
	return IDD_OPTIONS_VESSEL;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Vessel::Name() const
{
	const char* name = "Vessel settings";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Vessel::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/tab_param.htm"); // this needs to be updated
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Vessel::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Vessel::UpdateControls(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_VESSEL_FUELLIMIT, BM_SETCHECK,
		Cfg()->CfgLogicPrm.bLimitedFuel ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VESSEL_PADFUEL, BM_SETCHECK,
		Cfg()->CfgLogicPrm.bPadRefuel ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VESSEL_COMPLEXMODEL, BM_SETCHECK,
		Cfg()->CfgLogicPrm.FlightModelLevel ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VESSEL_DAMAGE, BM_SETCHECK,
		Cfg()->CfgLogicPrm.DamageSetting ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_VESSEL_FUELLIMIT, Cfg()->CfgLogicPrm.bLimitedFuel);
	SetCheck(hPage, IDC_OPT_VESSEL_PADFUEL, Cfg()->CfgLogicPrm.bPadRefuel);
	SetCheck(hPage, IDC_OPT_VESSEL_COMPLEXMODEL, Cfg()->CfgLogicPrm.FlightModelLevel);
	SetCheck(hPage, IDC_OPT_VESSEL_DAMAGE, Cfg()->CfgLogicPrm.DamageSetting);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Vessel::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Vessel::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
#endif // __linux__
	if (Container()->Environment() == OptionsPageContainer::INLINE) {
		UpdateControls(hPage);
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, IDC_OPT_VESSEL_COMPLEXMODEL), FALSE);
		EnableWindow(GetDlgItem(hPage, IDC_OPT_VESSEL_DAMAGE), FALSE);
#else // __linux__
		EnableItem(hPage, IDC_OPT_VESSEL_COMPLEXMODEL, FALSE);
		EnableItem(hPage, IDC_OPT_VESSEL_DAMAGE, FALSE);
#endif // __linux__
	}
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Vessel::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_Vessel::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_VESSEL_FUELLIMIT:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, IDC_OPT_VESSEL_FUELLIMIT, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, IDC_OPT_VESSEL_FUELLIMIT));
#endif // __linux__
			Cfg()->CfgLogicPrm.bLimitedFuel = check;
			g_pOrbiter->OnOptionChanged(OPTCAT_VESSEL, OPTITEM_VESSEL_LIMITEDFUEL);
			return FALSE;
		}
		break;
	case IDC_OPT_VESSEL_PADFUEL:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, IDC_OPT_VESSEL_PADFUEL, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, IDC_OPT_VESSEL_PADFUEL));
#endif // __linux__
			Cfg()->CfgLogicPrm.bPadRefuel = check;
			g_pOrbiter->OnOptionChanged(OPTCAT_VESSEL, OPTITEM_VESSEL_PADREFUEL);
			return FALSE;
		}
		break;
	case IDC_OPT_VESSEL_COMPLEXMODEL:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, IDC_OPT_VESSEL_COMPLEXMODEL, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, IDC_OPT_VESSEL_COMPLEXMODEL));
#endif // __linux__
			Cfg()->CfgLogicPrm.FlightModelLevel = check;
			return FALSE;
		}
		break;
	case IDC_OPT_VESSEL_DAMAGE:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, IDC_OPT_VESSEL_DAMAGE, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, IDC_OPT_VESSEL_DAMAGE));
#endif // __linux__
			Cfg()->CfgLogicPrm.DamageSetting = check;
			return FALSE;
		}
		break;
	}
	return TRUE;
}

// ======================================================================

OptionsPage_UI::OptionsPage_UI(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_UI::ResourceId() const
{
	return IDD_OPTIONS_UI;
}

// ----------------------------------------------------------------------

const char* OptionsPage_UI::Name() const
{
	const char* name = "User interface";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_UI::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/tab_param.htm"); // this needs to be updated
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_UI::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_UI::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	DWORD mode = Cfg()->CfgUIPrm.MouseFocusMode;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_UI_MOUSEFOCUSMODE, CB_SETCURSEL, mode, 0);
#else // __linux__
	DlgItem<QComboBox>(hPage, IDC_OPT_UI_MOUSEFOCUSMODE)->setCurrentIndex(mode);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_UI::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_UI::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
#endif // __linux__

#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_UI_MOUSEFOCUSMODE, CB_RESETCONTENT, 0, 0);
#else // __linux__
	DlgItem<QComboBox>(hPage, IDC_OPT_UI_MOUSEFOCUSMODE)->clear();
#endif // __linux__
	const char* strMouseMode[3] = { "Focus requires click", "Hybrid: Click required only for child windows", "Focus follows mouse" };
	for (int i = 0; i < 3; i++)
#ifndef __linux__
		SendDlgItemMessage(hPage, IDC_OPT_UI_MOUSEFOCUSMODE, CB_ADDSTRING, 0, (LPARAM)strMouseMode[i]);
#else // __linux__
		oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_UI_MOUSEFOCUSMODE), strMouseMode[i]);
#endif // __linux__

	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_UI::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_UI::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_UI_MOUSEFOCUSMODE:
#ifndef __linux__
		if (notification == CBN_SELCHANGE) {
			DWORD mode = (DWORD)SendDlgItemMessage(hPage, IDC_OPT_UI_MOUSEFOCUSMODE, CB_GETCURSEL, 0, 0);
#else // __linux__
		if (notification == RESN_SELCHANGE) {
			DWORD mode = DlgItem<QComboBox>(hPage, IDC_OPT_UI_MOUSEFOCUSMODE)->currentIndex();
#endif // __linux__
			Cfg()->CfgUIPrm.MouseFocusMode = mode;
			return FALSE;
		}
		break;
	}
	return TRUE;
}

// ======================================================================

OptionsPage_Joystick::OptionsPage_Joystick(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Joystick::ResourceId() const
{
	return IDD_OPTIONS_JOYSTICK;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Joystick::Name() const
{
	const char* name = "Joystick";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Joystick::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/tab_joystick.htm");
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Joystick::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Joystick::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	char cbuf[256];

#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_JOY_DEVICE, CB_SETCURSEL, (WPARAM)Cfg()->CfgJoystickPrm.Joy_idx, 0);
	SendDlgItemMessage(hPage, IDC_OPT_JOY_THROTTLE, CB_SETCURSEL, (WPARAM)Cfg()->CfgJoystickPrm.ThrottleAxis, 0);
	SendDlgItemMessage(hPage, IDC_OPT_JOY_INIT, BM_SETCHECK, Cfg()->CfgJoystickPrm.bThrottleIgnore ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	DlgItem<QComboBox>(hPage, IDC_OPT_JOY_DEVICE)->setCurrentIndex((int)Cfg()->CfgJoystickPrm.Joy_idx);
	DlgItem<QComboBox>(hPage, IDC_OPT_JOY_THROTTLE)->setCurrentIndex((int)Cfg()->CfgJoystickPrm.ThrottleAxis);
	SetCheck(hPage, IDC_OPT_JOY_INIT, Cfg()->CfgJoystickPrm.bThrottleIgnore);
#endif // __linux__

	int sat = Cfg()->CfgJoystickPrm.ThrottleSaturation / 10;
#ifndef __linux__
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_JOY_SAT), sat);
#else // __linux__
	oapiSetGaugePos(oapiResDlgItem(hPage, IDC_OPT_JOY_SAT), sat);
#endif // __linux__
	sprintf(cbuf, "%d", sat);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_JOY_STATIC1), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_JOY_STATIC1, cbuf);
#endif // __linux__

	int dz = Cfg()->CfgJoystickPrm.Deadzone / 10;
#ifndef __linux__
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_JOY_DEAD), dz);
#else // __linux__
	oapiSetGaugePos(oapiResDlgItem(hPage, IDC_OPT_JOY_DEAD), dz);
#endif // __linux__
	sprintf(cbuf, "%d", dz);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_JOY_STATIC2), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_JOY_STATIC2, cbuf);
#endif // __linux__

	int residJoystick[] = {
		IDC_OPT_JOY_THROTTLE, IDC_OPT_JOY_INIT, IDC_OPT_JOY_SAT, IDC_OPT_JOY_DEAD,
		IDC_OPT_JOY_STATIC1, IDC_OPT_JOY_STATIC2, IDC_OPT_JOY_STATIC3, IDC_OPT_JOY_STATIC4
	};
	bool enable = Cfg()->CfgJoystickPrm.Joy_idx > 0;
#ifndef __linux__
	for (int i = 0; i < ARRAYSIZE(residJoystick); i++) {
		EnableWindow(GetDlgItem(hPage, residJoystick[i]), enable);
#else // __linux__
	for (int i = 0; i < std::size(residJoystick); i++) {
		EnableItem(hPage, residJoystick[i], enable);
#endif // __linux__
	}
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Joystick::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Joystick::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
#endif // __linux__

	DWORD ndev;
#ifndef __linux__
	DIDEVICEINSTANCE* joylist;
#else // __linux__
	JoyDeviceInstance* joylist;
#endif // __linux__
	g_pOrbiter->GetDInput()->GetJoysticks(&joylist, &ndev);

#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_JOY_DEVICE, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage(hPage, IDC_OPT_JOY_DEVICE, CB_ADDSTRING, 0, (LPARAM)"<Disabled>");
	for (int i = 0; i < ndev; i++)
		SendDlgItemMessage(hPage, IDC_OPT_JOY_DEVICE, CB_ADDSTRING, 0, (LPARAM)(joylist[i].tszProductName));
#else // __linux__
	DlgItem<QComboBox>(hPage, IDC_OPT_JOY_DEVICE)->clear();
	oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_JOY_DEVICE), "<Disabled>");
	for (DWORD i = 0; i < ndev; i++)
		oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_JOY_DEVICE), (joylist[i].tszProductName));
#endif // __linux__

	const char* thmode[4] = { "<Keyboard only>", "Z-axis", "Slider 0", "Slider 1" };
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_JOY_THROTTLE, CB_RESETCONTENT, 0, 0);
	for (int i = 0; i < ARRAYSIZE(thmode); i++)
		SendDlgItemMessage(hPage, IDC_OPT_JOY_THROTTLE, CB_ADDSTRING, 0, (LPARAM)thmode[i]);
#else // __linux__
	DlgItem<QComboBox>(hPage, IDC_OPT_JOY_THROTTLE)->clear();
	for (int i = 0; i < std::size(thmode); i++)
		oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_JOY_THROTTLE), thmode[i]);
#endif // __linux__

	GAUGEPARAM gp = { 0, 1000, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
#ifndef __linux__
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_JOY_SAT), &gp);
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_JOY_DEAD), &gp);
#else // __linux__
	oapiSetGaugeParams(oapiResDlgItem(hPage, IDC_OPT_JOY_SAT), &gp);
	oapiSetGaugeParams(oapiResDlgItem(hPage, IDC_OPT_JOY_DEAD), &gp);
#endif // __linux__

	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Joystick::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_Joystick::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_JOY_DEVICE:
#ifndef __linux__
		if (notification == CBN_SELCHANGE) {
			DWORD idx = (DWORD)SendDlgItemMessage(hPage, IDC_OPT_JOY_DEVICE, CB_GETCURSEL, 0, 0);
#else // __linux__
		if (notification == RESN_SELCHANGE) {
			DWORD idx = DlgItem<QComboBox>(hPage, IDC_OPT_JOY_DEVICE)->currentIndex();
#endif // __linux__
			Cfg()->CfgJoystickPrm.Joy_idx = idx;
			g_pOrbiter->OnOptionChanged(OPTCAT_JOYSTICK, OPTITEM_JOYSTICK_DEVICE);
			UpdateControls(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_JOY_THROTTLE:
#ifndef __linux__
		if (notification == CBN_SELCHANGE) {
			DWORD axis = (DWORD)SendDlgItemMessage(hPage, IDC_OPT_JOY_THROTTLE, CB_GETCURSEL, 0, 0);
#else // __linux__
		if (notification == RESN_SELCHANGE) {
			DWORD axis = DlgItem<QComboBox>(hPage, IDC_OPT_JOY_THROTTLE)->currentIndex();
#endif // __linux__
			Cfg()->CfgJoystickPrm.ThrottleAxis = axis;
			g_pOrbiter->OnOptionChanged(OPTCAT_JOYSTICK, OPTITEM_JOYSTICK_PARAM);
			return FALSE;
		}
		break;
	case IDC_OPT_JOY_INIT:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, IDC_OPT_JOY_INIT, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, IDC_OPT_JOY_INIT));
#endif // __linux__
			Cfg()->CfgJoystickPrm.bThrottleIgnore = check;
			break;
		}
		break;
	}
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Joystick::OnHScroll(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Joystick::OnHScroll(QWidget *hPage, int ctrlId, int request, int pos)
#endif // __linux__
{
	int val;
#ifndef __linux__
	switch (GetDlgCtrlID((HWND)lParam)) {
#else // __linux__
	switch (ctrlId) {
#endif // __linux__
	case IDC_OPT_JOY_SAT:
#ifndef __linux__
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			val = HIWORD(wParam);
#else // __linux__
		switch (request) {
		case GAUGE_THUMBTRACK:
		case GAUGE_LINEDEC:
		case GAUGE_LINEINC:
			val = pos;
#endif // __linux__
			Cfg()->CfgJoystickPrm.ThrottleSaturation = val * 10;
			UpdateControls(hPage);
			g_pOrbiter->OnOptionChanged(OPTCAT_JOYSTICK, OPTITEM_JOYSTICK_PARAM);
			return 0;
		}
		break;
	case IDC_OPT_JOY_DEAD:
#ifndef __linux__
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			val = HIWORD(wParam);
#else // __linux__
		switch (request) {
		case GAUGE_THUMBTRACK:
		case GAUGE_LINEDEC:
		case GAUGE_LINEINC:
			val = pos;
#endif // __linux__
			Cfg()->CfgJoystickPrm.Deadzone = val * 10;
			UpdateControls(hPage);
			g_pOrbiter->OnOptionChanged(OPTCAT_JOYSTICK, OPTITEM_JOYSTICK_PARAM);
			return 0;
		}
		break;
	}
	return FALSE;
}

// ======================================================================

OptionsPage_CelSphere::OptionsPage_CelSphere(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_CelSphere::ResourceId() const
{
	return IDD_OPTIONS_CELSPHERE;
}

// ----------------------------------------------------------------------

const char* OptionsPage_CelSphere::Name() const
{
	const char* name = "Celestial sphere";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_CelSphere::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/opt_celsphere.htm");
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_CelSphere::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_CelSphere::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
#endif // __linux__

	GAUGEPARAM gp = { 0, 100, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
#ifndef __linux__
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_CSP_BGBRIGHTNESS), &gp);
#else // __linux__
	oapiSetGaugeParams(oapiResDlgItem(hPage, IDC_OPT_CSP_BGBRIGHTNESS), &gp);
#endif // __linux__

	PopulateStarmapList(hPage);
	PopulateBgImageList(hPage);
	UpdateControls(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_CelSphere::OnCommand(HWND hPage, WORD id, WORD code, HWND hControl)
#else // __linux__
BOOL OptionsPage_CelSphere::OnCommand(QWidget *hPage, WORD id, WORD code, QWidget *hControl)
#endif // __linux__
{
	switch (id) {
	case IDC_OPT_CSP_ENABLESTARPIX:
#ifndef __linux__
		if (code == BN_CLICKED) {
#else // __linux__
		if (code == RESN_CLICKED) {
#endif // __linux__
			StarPixelActivationChanged(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_CSP_ENABLESTARMAP:
#ifndef __linux__
		if (code == BN_CLICKED) {
#else // __linux__
		if (code == RESN_CLICKED) {
#endif // __linux__
			StarmapActivationChanged(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_CSP_ENABLEBKGMAP:
#ifndef __linux__
		if (code == BN_CLICKED) {
#else // __linux__
		if (code == RESN_CLICKED) {
#endif // __linux__
			BackgroundActivationChanged(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_CSP_STARMAPIMAGE:
#ifndef __linux__
		if (code == LBN_SELCHANGE) {
#else // __linux__
		if (code == RESN_SELCHANGE) {
#endif // __linux__
			StarmapImageChanged(hPage);
			return false;
		}
		break;
	case IDC_OPT_CSP_BKGIMAGE:
#ifndef __linux__
		if (code == LBN_SELCHANGE) {
#else // __linux__
		if (code == RESN_SELCHANGE) {
#endif // __linux__
			BackgroundImageChanged(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_CSP_STARMAPLIN:
	case IDC_OPT_CSP_STARMAPEXP:
#ifndef __linux__
		if (code == BN_CLICKED) {
#else // __linux__
		if (code == RESN_CLICKED) {
#endif // __linux__
			Cfg()->CfgVisualPrm.StarPrm.map_log = (id == IDC_OPT_CSP_STARMAPEXP);
			g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_STARDISPLAYPARAM);
			return FALSE;
		}
		break;
	}
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_CelSphere::OnHScroll(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_CelSphere::OnHScroll(QWidget *hPage, int ctrlId, int request, int pos)
#endif // __linux__
{
#ifndef __linux__
	switch (GetDlgCtrlID((HWND)lParam)) {
#else // __linux__
	switch (ctrlId) {
#endif // __linux__
	case IDC_OPT_CSP_BGBRIGHTNESS:
#ifndef __linux__
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			BackgroundBrightnessChanged(hPage, 0.01 * HIWORD(wParam));
#else // __linux__
		switch (request) {
		case GAUGE_THUMBTRACK:
		case GAUGE_LINEDEC:
		case GAUGE_LINEINC:
			BackgroundBrightnessChanged(hPage, 0.01 * pos);
#endif // __linux__
			return 0;
		}
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_CelSphere::OnNotify(HWND hPage, DWORD ctrlId, const NMHDR* pNmHdr)
#else // __linux__
BOOL OptionsPage_CelSphere::OnDeltaPos(QWidget *hPage, int ctrlId, int iDelta)
#endif // __linux__
{
#ifndef __linux__
	if (pNmHdr->code == UDN_DELTAPOS) {
		NMUPDOWN* nmud = (NMUPDOWN*)pNmHdr;
		int delta = -nmud->iDelta;
#else // __linux__
	{ // UDN_DELTAPOS
		int delta = -iDelta;
#endif // __linux__
		StarRenderPrm& prm = Cfg()->CfgVisualPrm.StarPrm;
#ifndef __linux__
		switch (pNmHdr->idFrom) {
#else // __linux__
		switch (ctrlId) {
#endif // __linux__
		case IDC_OPT_CSP_STARMAGHISPIN:
			prm.mag_hi = min(prm.mag_lo, max(-2.0, prm.mag_hi + delta * 0.1));
			break;
		case IDC_OPT_CSP_STARMAGLOSPIN:
			prm.mag_lo = min(15.0, max(prm.mag_hi, prm.mag_lo + delta * 0.1));
			break;
		case IDC_OPT_CSP_STARMINBRTSPIN:
			prm.brt_min = min(1.0, max(0.0, prm.brt_min + delta * 0.01));
			break;
		}
		UpdateControls(hPage);
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_STARDISPLAYPARAM);
		return TRUE;
	}
#ifndef __linux__
	return FALSE;
#endif // !__linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_CelSphere::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	char cbuf[256];

	std::string starpath = std::string(Cfg()->CfgVisualPrm.StarImagePath);
	for (int idx = 0; idx < m_pathStarmap.size(); idx++)
		if (!starpath.compare(m_pathStarmap[idx].second)) {
#ifndef __linux__
			SendDlgItemMessage(hPage, IDC_OPT_CSP_STARMAPIMAGE, CB_SETCURSEL, idx, 0);
#else // __linux__
			DlgItem<QComboBox>(hPage, IDC_OPT_CSP_STARMAPIMAGE)->setCurrentIndex(idx);
#endif // __linux__
			break;
		}

	std::string bgpath = std::string(Cfg()->CfgVisualPrm.CSphereBgPath);
	for (int idx = 0; idx < m_pathBgImage.size(); idx++)
		if (!bgpath.compare(m_pathBgImage[idx].second)) {
#ifndef __linux__
			SendDlgItemMessage(hPage, IDC_OPT_CSP_BKGIMAGE, CB_SETCURSEL, idx, 0);
#else // __linux__
			DlgItem<QComboBox>(hPage, IDC_OPT_CSP_BKGIMAGE)->setCurrentIndex(idx);
#endif // __linux__
			break;
		}

	bool checked = Cfg()->CfgVisualPrm.bUseStarImage;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_CSP_ENABLESTARMAP, BM_SETCHECK, checked ? BST_CHECKED : BST_UNCHECKED, 0);
	EnableWindow(GetDlgItem(hPage, IDC_OPT_CSP_STARMAPIMAGE), checked ? TRUE : FALSE);
#else // __linux__
	SetCheck(hPage, IDC_OPT_CSP_ENABLESTARMAP, checked);
	EnableItem(hPage, IDC_OPT_CSP_STARMAPIMAGE, checked ? TRUE : FALSE);
#endif // __linux__

	checked = Cfg()->CfgVisualPrm.bUseBgImage;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_CSP_ENABLEBKGMAP, BM_SETCHECK, checked ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_CSP_ENABLEBKGMAP, checked);
#endif // __linux__
	int brt = (int)(Cfg()->CfgVisualPrm.CSphereBgIntens * 100.0);
#ifndef __linux__
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_CSP_BGBRIGHTNESS), brt);
	EnableWindow(GetDlgItem(hPage, IDC_OPT_CSP_BKGIMAGE), checked ? TRUE : FALSE);
	EnableWindow(GetDlgItem(hPage, IDC_STATIC1), checked ? TRUE : FALSE);
	EnableWindow(GetDlgItem(hPage, IDC_OPT_CSP_BGBRIGHTNESS), checked ? TRUE : FALSE);
#else // __linux__
	oapiSetGaugePos(oapiResDlgItem(hPage, IDC_OPT_CSP_BGBRIGHTNESS), brt);
	EnableItem(hPage, IDC_OPT_CSP_BKGIMAGE, checked ? TRUE : FALSE);
	EnableItem(hPage, IDC_STATIC1, checked ? TRUE : FALSE);
	EnableItem(hPage, IDC_OPT_CSP_BGBRIGHTNESS, checked ? TRUE : FALSE);
#endif // __linux__

	checked = Cfg()->CfgVisualPrm.bUseStarDots;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_CSP_ENABLESTARPIX, BM_SETCHECK, checked ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_CSP_ENABLESTARPIX, checked);
#endif // __linux__
	sprintf(cbuf, "%0.1f", Cfg()->CfgVisualPrm.StarPrm.mag_hi);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_CSP_STARMAGHI), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_CSP_STARMAGHI, cbuf);
#endif // __linux__
	sprintf(cbuf, "%0.1f", Cfg()->CfgVisualPrm.StarPrm.mag_lo);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_CSP_STARMAGLO), cbuf);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_CSP_STARMAGLO, cbuf);
#endif // __linux__
	sprintf(cbuf, "%0.2f", Cfg()->CfgVisualPrm.StarPrm.brt_min);
#ifndef __linux__
	SetWindowText(GetDlgItem(hPage, IDC_OPT_CSP_STARMINBRT), cbuf);
	SendDlgItemMessage(hPage, IDC_OPT_CSP_STARMAPLIN, BM_SETCHECK,
		Cfg()->CfgVisualPrm.StarPrm.map_log ? BST_UNCHECKED : BST_CHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_CSP_STARMAPEXP, BM_SETCHECK,
		Cfg()->CfgVisualPrm.StarPrm.map_log ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	oapiSetDlgItemText(hPage, IDC_OPT_CSP_STARMINBRT, cbuf);
	SetCheck(hPage, IDC_OPT_CSP_STARMAPLIN, !(Cfg()->CfgVisualPrm.StarPrm.map_log));
	SetCheck(hPage, IDC_OPT_CSP_STARMAPEXP, Cfg()->CfgVisualPrm.StarPrm.map_log);
#endif // __linux__
	std::vector<int> ctrlStarPix{
		IDC_STATIC2, IDC_STATIC3, IDC_STATIC4, IDC_STATIC5, IDC_STATIC6,
		IDC_OPT_CSP_STARMAGHISPIN, IDC_OPT_CSP_STARMAGLOSPIN, IDC_OPT_CSP_STARMINBRTSPIN,
		IDC_OPT_CSP_STARMAGHI, IDC_OPT_CSP_STARMAGLO, IDC_OPT_CSP_STARMINBRT, IDC_OPT_CSP_STARMAPLIN, IDC_OPT_CSP_STARMAPEXP
	};
	for (auto ctrl : ctrlStarPix)
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, ctrl), checked ? TRUE : FALSE);
#else // __linux__
		EnableItem(hPage, ctrl, checked ? TRUE : FALSE);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::PopulateStarmapList(HWND hPage)
#else // __linux__
void OptionsPage_CelSphere::PopulateStarmapList(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_CSP_STARMAPIMAGE, CB_RESETCONTENT, 0, 0);
#else // __linux__
	DlgItem<QComboBox>(hPage, IDC_OPT_CSP_STARMAPIMAGE)->clear();
#endif // __linux__
	m_pathStarmap.clear();

#ifndef __linux__
	std::ifstream ifs(Cfg()->ConfigPath("CSphere\\bkgimage"));
#else // __linux__
	std::ifstream ifs(oapiResolvePath(Cfg()->ConfigPath("CSphere/bkgimage")));
#endif // __linux__
	if (ifs) {
		char* c;
		char cbuf[256];
		bool found = false;
		while (ifs.getline(cbuf, 256)) {
			if (!found) {
				if (!strcmp(cbuf, "BEGIN_STARMAPS"))
					found = true;
				continue;
			}
			if (!strcmp(cbuf, "END_STARMAPS"))
				break;
			c = strtok(cbuf, "|");
			if (c) {
#ifndef __linux__
				SendDlgItemMessage(hPage, IDC_OPT_CSP_STARMAPIMAGE, CB_ADDSTRING, 0, (LPARAM)c);
#else // __linux__
				oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_CSP_STARMAPIMAGE), c);
#endif // __linux__
				std::string label(c);
				c = strtok(NULL, "\n");
				std::string path(c);
				m_pathStarmap.push_back(std::make_pair(label, path));
			}
		}
	}
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::PopulateBgImageList(HWND hPage)
#else // __linux__
void OptionsPage_CelSphere::PopulateBgImageList(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_CSP_BKGIMAGE, CB_RESETCONTENT, 0, 0);
#else // __linux__
	DlgItem<QComboBox>(hPage, IDC_OPT_CSP_BKGIMAGE)->clear();
#endif // __linux__
	m_pathBgImage.clear();

#ifndef __linux__
	std::ifstream ifs(Cfg()->ConfigPath("CSphere\\bkgimage"));
#else // __linux__
	std::ifstream ifs(oapiResolvePath(Cfg()->ConfigPath("CSphere/bkgimage")));
#endif // __linux__
	if (ifs) {
		char* c;
		char cbuf[256];
		bool found = false;
		while (ifs.getline(cbuf, 256)) {
			if (!found) {
				if (!strcmp(cbuf, "BEGIN_BACKGROUNDS"))
					found = true;
				continue;
			}
			if (!strcmp(cbuf, "END_BACKGROUNDS"))
				break;
			c = strtok(cbuf, "|");
			if (c) {
#ifndef __linux__
				SendDlgItemMessage(hPage, IDC_OPT_CSP_BKGIMAGE, CB_ADDSTRING, 0, (LPARAM)c);
#else // __linux__
				oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_CSP_BKGIMAGE), c);
#endif // __linux__
				std::string label(c);
				c = strtok(NULL, "\n");
				std::string path(c);
				m_pathBgImage.push_back(std::make_pair(label, path));
			}
		}
	}
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::StarPixelActivationChanged(HWND hPage)
#else // __linux__
void OptionsPage_CelSphere::StarPixelActivationChanged(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	bool activated = SendDlgItemMessage(hPage, IDC_OPT_CSP_ENABLESTARPIX, BM_GETCHECK, 0, 0) == BST_CHECKED;
#else // __linux__
	bool activated = IsChecked(hPage, IDC_OPT_CSP_ENABLESTARPIX);
#endif // __linux__
	bool active = Cfg()->CfgVisualPrm.bUseStarDots;

	if (activated != active) {
		Cfg()->CfgVisualPrm.bUseStarDots = activated;
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATESTARDOTS);
	}
	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::StarmapActivationChanged(HWND hPage)
#else // __linux__
void OptionsPage_CelSphere::StarmapActivationChanged(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	bool activated = SendDlgItemMessage(hPage, IDC_OPT_CSP_ENABLESTARMAP, BM_GETCHECK, 0, 0) == BST_CHECKED;
#else // __linux__
	bool activated = IsChecked(hPage, IDC_OPT_CSP_ENABLESTARMAP);
#endif // __linux__
	bool active = Cfg()->CfgVisualPrm.bUseStarImage;

	if (activated != active) {
		Cfg()->CfgVisualPrm.bUseStarImage = activated;
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATESTARIMAGE);
	}
	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::StarmapImageChanged(HWND hPage)
#else // __linux__
void OptionsPage_CelSphere::StarmapImageChanged(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	int idx = SendDlgItemMessage(hPage, IDC_OPT_CSP_STARMAPIMAGE, CB_GETCURSEL, 0, 0);
#else // __linux__
	int idx = DlgItem<QComboBox>(hPage, IDC_OPT_CSP_STARMAPIMAGE)->currentIndex();
#endif // __linux__
	std::string& path = m_pathStarmap[idx].second;
	strncpy(Cfg()->CfgVisualPrm.StarImagePath, path.c_str(), 128);
	g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_STARIMAGECHANGED);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::BackgroundActivationChanged(HWND hPage)
#else // __linux__
void OptionsPage_CelSphere::BackgroundActivationChanged(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	bool activated = SendDlgItemMessage(hPage, IDC_OPT_CSP_ENABLEBKGMAP, BM_GETCHECK, 0, 0) == BST_CHECKED;
#else // __linux__
	bool activated = IsChecked(hPage, IDC_OPT_CSP_ENABLEBKGMAP);
#endif // __linux__
	bool active = Cfg()->CfgVisualPrm.bUseBgImage;

	if (activated != active) {
		Cfg()->CfgVisualPrm.bUseBgImage = activated;
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATEBGIMAGE);
	}
	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::BackgroundImageChanged(HWND hPage)
#else // __linux__
void OptionsPage_CelSphere::BackgroundImageChanged(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	int idx = SendDlgItemMessage(hPage, IDC_OPT_CSP_BKGIMAGE, CB_GETCURSEL, 0, 0);
#else // __linux__
	int idx = DlgItem<QComboBox>(hPage, IDC_OPT_CSP_BKGIMAGE)->currentIndex();
#endif // __linux__
	std::string& path = m_pathBgImage[idx].second;
	strncpy(Cfg()->CfgVisualPrm.CSphereBgPath, path.c_str(), 128);
	g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_BGIMAGECHANGED);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_CelSphere::BackgroundBrightnessChanged(HWND hPage, double level)
#else // __linux__
void OptionsPage_CelSphere::BackgroundBrightnessChanged(QWidget *hPage, double level)
#endif // __linux__
{
	if (level != Cfg()->CfgVisualPrm.CSphereBgIntens) {
		Cfg()->CfgVisualPrm.CSphereBgIntens = level;
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_BGIMAGEBRIGHTNESS);
	}
}

// ======================================================================

OptionsPage_VisHelper::OptionsPage_VisHelper(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_VisHelper::ResourceId() const
{
	return IDD_OPTIONS_VISHELPER;
}

// ----------------------------------------------------------------------

const char* OptionsPage_VisHelper::Name() const
{
	const char* name = "Visual helpers";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_VisHelper::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/vishelper.htm");
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_VisHelper::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_VisHelper::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	int& plnFlag = Cfg()->CfgVisHelpPrm.flagPlanetarium;
	bool enable = plnFlag & PLN_ENABLE;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_PLN, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_PLN, enable);
#endif // __linux__
	int& mkrFlag = Cfg()->CfgVisHelpPrm.flagMarkers;
	enable = mkrFlag & MKR_ENABLE;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_MKR, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_MKR, enable);
#endif // __linux__
	int vecFlag = Cfg()->CfgVisHelpPrm.flagBodyForce;
	enable = (vecFlag & BFV_ENABLE);
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_VEC, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_VEC, enable);
#endif // __linux__
	int crdFlag = Cfg()->CfgVisHelpPrm.flagFrameAxes;
	enable = (crdFlag & FAV_ENABLE);
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_CRD, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_CRD, enable);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_VisHelper::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_VisHelper::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
#endif // __linux__
	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_VisHelper::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_VisHelper::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_PLN:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, ctrlId));
#endif // __linux__
			DWORD flag = PLN_ENABLE;
			int& plnFlag = Cfg()->CfgVisHelpPrm.flagPlanetarium;
			if (check) plnFlag |= flag;
			else       plnFlag &= ~flag;
			return TRUE;
		}
		break;
	case IDC_OPT_MKR:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, ctrlId));
#endif // __linux__
			DWORD flag = MKR_ENABLE;
			int& mkrFlag = Cfg()->CfgVisHelpPrm.flagMarkers;
			if (check) mkrFlag |= flag;
			else       mkrFlag &= ~flag;
			return TRUE;
		}
		break;
	case IDC_OPT_VEC:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, ctrlId));
#endif // __linux__
			DWORD flag = BFV_ENABLE;
			int& vecFlag = Cfg()->CfgVisHelpPrm.flagBodyForce;
			if (check) vecFlag |= flag;
			else       vecFlag &= ~flag;
		}
		break;
	case IDC_OPT_CRD:
#ifndef __linux__
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == BST_CHECKED);
#else // __linux__
		if (notification == RESN_CLICKED) {
			bool check = (IsChecked(hPage, ctrlId));
#endif // __linux__
			DWORD flag = FAV_ENABLE;
			int& crdFlag = Cfg()->CfgVisHelpPrm.flagFrameAxes;
			if (check) crdFlag |= flag;
			else       crdFlag &= ~flag;
		}
		break;
	case IDC_OPT_VHELP_PLN:
#ifndef __linux__
		if (notification == BN_CLICKED)
#else // __linux__
		if (notification == RESN_CLICKED)
#endif // __linux__
			Container()->SwitchPage("Planetarium");
		break;
	case IDC_OPT_VHELP_MKR:
#ifndef __linux__
		if (notification == BN_CLICKED)
#else // __linux__
		if (notification == RESN_CLICKED)
#endif // __linux__
			Container()->SwitchPage("Labels");
		break;
	case IDC_OPT_VHELP_VEC:
#ifndef __linux__
		if (notification == BN_CLICKED)
#else // __linux__
		if (notification == RESN_CLICKED)
#endif // __linux__
			Container()->SwitchPage("Body forces");
		break;
	case IDC_OPT_VHELP_CRD:
#ifndef __linux__
		if (notification == BN_CLICKED)
#else // __linux__
		if (notification == RESN_CLICKED)
#endif // __linux__
			Container()->SwitchPage("Object axes");
		break;
	}
	return FALSE;
}

// ======================================================================

OptionsPage_Planetarium::OptionsPage_Planetarium(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Planetarium::ResourceId() const
{
	return IDD_OPTIONS_PLANETARIUM;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Planetarium::Name() const
{
	const char* name = "Planetarium";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Planetarium::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/vh_planetarium.htm");
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Planetarium::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Planetarium::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	std::array<int, 15> residPlanetarium = {
		IDC_OPT_PLN_CELGRID, IDC_OPT_PLN_ECLGRID, IDC_OPT_PLN_GALGRID, IDC_OPT_PLN_HRZGRID, IDC_OPT_PLN_EQU,
		IDC_OPT_PLN_CNSTLABEL, IDC_OPT_PLN_CNSTLABEL_FULL, IDC_OPT_PLN_CNSTLABEL_SHORT, IDC_OPT_PLN_CNSTBND,
		IDC_OPT_PLN_CNSTPATTERN, IDC_OPT_PLN_MARKER, IDC_OPT_PLN_MKRLIST,
		IDC_STATIC1, IDC_STATIC2, IDC_STATIC3
	};

	int& plnFlag = Cfg()->CfgVisHelpPrm.flagPlanetarium;
	bool enable = plnFlag & PLN_ENABLE;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_PLN, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_PLN, enable);
#endif // __linux__
	for (auto resid : residPlanetarium)
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, resid), enable ? TRUE : FALSE);
#else // __linux__
		EnableItem(hPage, resid, enable ? TRUE : FALSE);
#endif // __linux__
	if (enable && !(plnFlag & PLN_CNSTLABEL)) {
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, IDC_OPT_PLN_CNSTLABEL_FULL), FALSE);
		EnableWindow(GetDlgItem(hPage, IDC_OPT_PLN_CNSTLABEL_SHORT), FALSE);
#else // __linux__
		EnableItem(hPage, IDC_OPT_PLN_CNSTLABEL_FULL, FALSE);
		EnableItem(hPage, IDC_OPT_PLN_CNSTLABEL_SHORT, FALSE);
#endif // __linux__
	}
	if (enable && !(plnFlag & PLN_CCMARK))
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, IDC_OPT_PLN_MKRLIST), FALSE);
#else // __linux__
		EnableItem(hPage, IDC_OPT_PLN_MKRLIST, FALSE);
#endif // __linux__

#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_PLN_CELGRID, BM_SETCHECK, plnFlag & PLN_CGRID ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_ECLGRID, BM_SETCHECK, plnFlag & PLN_EGRID ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_GALGRID, BM_SETCHECK, plnFlag & PLN_GGRID ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_HRZGRID, BM_SETCHECK, plnFlag & PLN_HGRID ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_EQU, BM_SETCHECK, plnFlag & PLN_EQU ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_CNSTLABEL, BM_SETCHECK, plnFlag & PLN_CNSTLABEL ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_CNSTBND, BM_SETCHECK, plnFlag & PLN_CNSTBND ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_CNSTPATTERN, BM_SETCHECK, plnFlag & PLN_CONST ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_MARKER, BM_SETCHECK, plnFlag & PLN_CCMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_CNSTLABEL_FULL, BM_SETCHECK, plnFlag & PLN_CNSTLONG ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_PLN_CNSTLABEL_SHORT, BM_SETCHECK, plnFlag & PLN_CNSTLONG ? BST_UNCHECKED : BST_CHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_PLN_CELGRID, plnFlag & PLN_CGRID);
	SetCheck(hPage, IDC_OPT_PLN_ECLGRID, plnFlag & PLN_EGRID);
	SetCheck(hPage, IDC_OPT_PLN_GALGRID, plnFlag & PLN_GGRID);
	SetCheck(hPage, IDC_OPT_PLN_HRZGRID, plnFlag & PLN_HGRID);
	SetCheck(hPage, IDC_OPT_PLN_EQU, plnFlag & PLN_EQU);
	SetCheck(hPage, IDC_OPT_PLN_CNSTLABEL, plnFlag & PLN_CNSTLABEL);
	SetCheck(hPage, IDC_OPT_PLN_CNSTBND, plnFlag & PLN_CNSTBND);
	SetCheck(hPage, IDC_OPT_PLN_CNSTPATTERN, plnFlag & PLN_CONST);
	SetCheck(hPage, IDC_OPT_PLN_MARKER, plnFlag & PLN_CCMARK);
	SetCheck(hPage, IDC_OPT_PLN_CNSTLABEL_FULL, plnFlag & PLN_CNSTLONG);
	SetCheck(hPage, IDC_OPT_PLN_CNSTLABEL_SHORT, !(plnFlag & PLN_CNSTLONG));
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Planetarium::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Planetarium::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
#endif // __linux__
	RescanMarkerList(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Planetarium::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_Planetarium::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_PLN:
	case IDC_OPT_PLN_CELGRID:
	case IDC_OPT_PLN_ECLGRID:
	case IDC_OPT_PLN_GALGRID:
	case IDC_OPT_PLN_HRZGRID:
	case IDC_OPT_PLN_EQU:
	case IDC_OPT_PLN_CNSTLABEL:
	case IDC_OPT_PLN_CNSTBND:
	case IDC_OPT_PLN_CNSTPATTERN:
	case IDC_OPT_PLN_MARKER:
	case IDC_OPT_PLN_CNSTLABEL_FULL:
	case IDC_OPT_PLN_CNSTLABEL_SHORT:
#ifndef __linux__
		if (notification == BN_CLICKED) {
#else // __linux__
		if (notification == RESN_CLICKED) {
#endif // __linux__
			OnItemClicked(hPage, ctrlId);
			return TRUE;
		}
		break;
	case IDC_OPT_PLN_MKRLIST:
#ifndef __linux__
		if (notification == LBN_SELCHANGE)
#else // __linux__
		if (notification == RESN_SELCHANGE)
#endif // __linux__
			return OnMarkerSelectionChanged(hPage);
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Planetarium::OnItemClicked(HWND hPage, WORD ctrlId)
#else // __linux__
void OptionsPage_Planetarium::OnItemClicked(QWidget *hPage, WORD ctrlId)
#endif // __linux__
{
#ifndef __linux__
	bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
	bool check = (IsChecked(hPage, ctrlId));
#endif // __linux__
	DWORD flag;
	switch (ctrlId) {
	case IDC_OPT_PLN:                 flag = PLN_ENABLE;    break;
	case IDC_OPT_PLN_CELGRID:         flag = PLN_CGRID;     break;
	case IDC_OPT_PLN_ECLGRID:         flag = PLN_EGRID;     break;
	case IDC_OPT_PLN_GALGRID:         flag = PLN_GGRID;     break;
	case IDC_OPT_PLN_HRZGRID:         flag = PLN_HGRID;     break;
	case IDC_OPT_PLN_EQU:             flag = PLN_EQU;       break;
	case IDC_OPT_PLN_CNSTLABEL:       flag = PLN_CNSTLABEL; break;
	case IDC_OPT_PLN_CNSTBND:         flag = PLN_CNSTBND;   break;
	case IDC_OPT_PLN_CNSTPATTERN:     flag = PLN_CONST;     break;
	case IDC_OPT_PLN_MARKER:          flag = PLN_CCMARK;    break;
	case IDC_OPT_PLN_CNSTLABEL_FULL:  flag = PLN_CNSTLONG;  break;
	case IDC_OPT_PLN_CNSTLABEL_SHORT: flag = PLN_CNSTLONG; check = !check; break;
	default:                          flag = 0;             break;
	}
	int& plnFlag = Cfg()->CfgVisHelpPrm.flagPlanetarium;
	if (check) plnFlag |= flag;
	else       plnFlag &= ~flag;

	g_pOrbiter->OnOptionChanged(OPTCAT_PLANETARIUM, OPTITEM_PLANETARIUM_DISPFLAG);
	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Planetarium::OnMarkerSelectionChanged(HWND hPage)
#else // __linux__
BOOL OptionsPage_Planetarium::OnMarkerSelectionChanged(QWidget *hPage)
#endif // __linux__
{
	std::vector<oapi::GraphicsClient::LABELLIST>& list = g_psys->LabelList();
	if (list.size()) {
		for (int i = 0; i < list.size(); i++) {
#ifndef __linux__
			int sel = SendDlgItemMessage(hPage, IDC_OPT_PLN_MKRLIST, LB_GETSEL, i, 0);
#else // __linux__
			int sel = DlgItem<QListWidget>(hPage, IDC_OPT_PLN_MKRLIST)->item(i)->isSelected();
#endif // __linux__
			list[i].active = (sel ? true : false);
		}
#ifndef __linux__
		std::ifstream fcfg(Cfg()->ConfigPath(g_psys->Name().c_str()));
#else // __linux__
		std::ifstream fcfg(oapiResolvePath(Cfg()->ConfigPath(g_psys->Name().c_str())));
#endif // __linux__
		g_psys->ScanLabelLists(fcfg);
	}
	return 0;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Planetarium::RescanMarkerList(HWND hPage)
#else // __linux__
void OptionsPage_Planetarium::RescanMarkerList(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_PLN_MKRLIST, LB_RESETCONTENT, 0, 0);
#else // __linux__
	QSignalBlocker block(DlgItem<QListWidget>(hPage, IDC_OPT_PLN_MKRLIST)); // LB_ messages don't notify
	DlgItem<QListWidget>(hPage, IDC_OPT_PLN_MKRLIST)->clear();
#endif // __linux__

	if (!g_psys) return;
	const std::vector< oapi::GraphicsClient::LABELLIST>& list = g_psys->LabelList();
	if (!list.size()) return;

	int n = 0;
#ifndef __linux__
	g_psys->ForEach(FILETYPE_MARKER, [&](const fs::directory_entry& entry) {
		SendDlgItemMessage(hPage, IDC_OPT_PLN_MKRLIST, LB_ADDSTRING, 0, (LPARAM)entry.path().stem().string().c_str());
		if (n < list.size() && list[n].active)
			SendDlgItemMessage(hPage, IDC_OPT_PLN_MKRLIST, LB_SETSEL, TRUE, n);
		n++;
	});
#else // __linux__
	g_psys->ForEach(FILETYPE_MARKER, [&](const fs::directory_entry& entry) {
		DlgItem<QListWidget>(hPage, IDC_OPT_PLN_MKRLIST)->addItem(QString::fromUtf8(entry.path().stem().string().c_str()));
		if (n < list.size() && list[n].active)
			DlgItem<QListWidget>(hPage, IDC_OPT_PLN_MKRLIST)->item(n)->setSelected(true);
		n++;
	});
#endif // __linux__
}

// ======================================================================

OptionsPage_Labels::OptionsPage_Labels(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Labels::ResourceId() const
{
	return IDD_OPTIONS_LABELS;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Labels::Name() const
{
	const char* name = "Labels";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Labels::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/vh_labels.htm");
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Labels::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Labels::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	std::array<int, 10> residLabels = {
		IDC_OPT_MKR_VESSEL, IDC_OPT_MKR_CELBODY, IDC_OPT_MKR_FEATUREBODY, IDC_OPT_MKR_BASE,
		IDC_OPT_MKR_BEACON, IDC_OPT_MKR_FEATURES, IDC_OPT_MKR_FEATUREBODY, IDC_OPT_MKR_FEATURELIST,
		IDC_STATIC1, IDC_STATIC2
	};

	int& mkrFlag = Cfg()->CfgVisHelpPrm.flagMarkers;
	bool enable = mkrFlag & MKR_ENABLE;
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_MKR, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_MKR, enable);
#endif // __linux__
	for (auto resid : residLabels)
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, resid), enable ? TRUE : FALSE);
#else // __linux__
		EnableItem(hPage, resid, enable ? TRUE : FALSE);
#endif // __linux__
	if (enable && !(mkrFlag & MKR_LMARK)) {
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, IDC_OPT_MKR_FEATUREBODY), FALSE);
		EnableWindow(GetDlgItem(hPage, IDC_OPT_MKR_FEATURELIST), FALSE);
#else // __linux__
		EnableItem(hPage, IDC_OPT_MKR_FEATUREBODY, FALSE);
		EnableItem(hPage, IDC_OPT_MKR_FEATURELIST, FALSE);
#endif // __linux__
	}

#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_MKR_VESSEL, BM_SETCHECK, mkrFlag & MKR_VMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_CELBODY, BM_SETCHECK, mkrFlag & MKR_CMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_BASE, BM_SETCHECK, mkrFlag & MKR_BMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_BEACON, BM_SETCHECK, mkrFlag & MKR_RMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURES, BM_SETCHECK, mkrFlag & MKR_LMARK ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_MKR_VESSEL, mkrFlag & MKR_VMARK);
	SetCheck(hPage, IDC_OPT_MKR_CELBODY, mkrFlag & MKR_CMARK);
	SetCheck(hPage, IDC_OPT_MKR_BASE, mkrFlag & MKR_BMARK);
	SetCheck(hPage, IDC_OPT_MKR_BEACON, mkrFlag & MKR_RMARK);
	SetCheck(hPage, IDC_OPT_MKR_FEATURES, mkrFlag & MKR_LMARK);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Labels::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Labels::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
#ifndef __linux__
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
#else // __linux__
	OptionsPage::OnInitDialog(hPage);
#endif // __linux__
	ScanPsysBodies(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Labels::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_Labels::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_MKR:
	case IDC_OPT_MKR_VESSEL:
	case IDC_OPT_MKR_CELBODY:
	case IDC_OPT_MKR_BASE:
	case IDC_OPT_MKR_BEACON:
	case IDC_OPT_MKR_FEATURES:
#ifndef __linux__
		if (notification == BN_CLICKED) {
#else // __linux__
		if (notification == RESN_CLICKED) {
#endif // __linux__
			OnItemClicked(hPage, ctrlId);
			return TRUE;
		}
		break;
	case IDC_OPT_MKR_FEATUREBODY:
#ifndef __linux__
		if (notification == CBN_SELCHANGE)
#else // __linux__
		if (notification == RESN_SELCHANGE)
#endif // __linux__
			UpdateFeatureList(hPage);
		return TRUE;
	case IDC_OPT_MKR_FEATURELIST:
#ifndef __linux__
		if (notification == LBN_SELCHANGE)
#else // __linux__
		if (notification == RESN_SELCHANGE)
#endif // __linux__
			RescanFeatures(hPage);
		return TRUE;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Labels::OnItemClicked(HWND hPage, WORD ctrlId)
#else // __linux__
void OptionsPage_Labels::OnItemClicked(QWidget *hPage, WORD ctrlId)
#endif // __linux__
{
#ifndef __linux__
	bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
	bool check = (IsChecked(hPage, ctrlId));
#endif // __linux__
	DWORD flag;
	switch (ctrlId) {
	case IDC_OPT_MKR:          flag = MKR_ENABLE; break;
	case IDC_OPT_MKR_VESSEL:   flag = MKR_VMARK;  break;
	case IDC_OPT_MKR_CELBODY:  flag = MKR_CMARK;  break;
	case IDC_OPT_MKR_BASE:     flag = MKR_BMARK;  break;
	case IDC_OPT_MKR_BEACON:   flag = MKR_RMARK;  break;
	case IDC_OPT_MKR_FEATURES: flag = MKR_LMARK;  break;
	default:                   flag = 0;          break;
	}
	int& mkrFlag = Cfg()->CfgVisHelpPrm.flagMarkers;
	if (check) mkrFlag |= flag;
	else       mkrFlag &= ~flag;

	if (g_psys && ctrlId == IDC_OPT_MKR_FEATURES)
		g_psys->ActivatePlanetLabels(mkrFlag & MKR_ENABLE && mkrFlag & MKR_LMARK);

	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Labels::ScanPsysBodies(HWND hPage)
#else // __linux__
void OptionsPage_Labels::ScanPsysBodies(QWidget *hPage)
#endif // __linux__
{
	if (!g_psys) return;
	const Body* sel = nullptr;
	for (int i = 0; i < g_psys->nPlanet(); i++) {
		Planet* planet = g_psys->GetPlanet(i);
		if (planet == g_camera->Target())
			sel = planet;
		if (planet->isMoon())
			continue;
#ifndef __linux__
		SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_ADDSTRING, 0, (LPARAM)planet->Name());
#else // __linux__
		oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_MKR_FEATUREBODY), planet->Name());
#endif // __linux__
		for (int j = 0; j < planet->nSecondary(); j++) {
			char cbuf[256] = "    ";
			strncpy(cbuf + 4, planet->Secondary(j)->Name(), 252);
#ifndef __linux__
			SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_ADDSTRING, 0, (LPARAM)cbuf);
#else // __linux__
			oapiComboAddString(DlgItem<QComboBox>(hPage, IDC_OPT_MKR_FEATUREBODY), cbuf);
#endif // __linux__
		}
	}
	if (!sel) {
		Body* tgt = g_camera->Target();
		if (tgt->Type() == OBJTP_VESSEL)
			sel = ((Vessel*)tgt)->GetSurfParam()->ref;
	}
#ifndef __linux__
	int idx = (sel ? SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_FINDSTRINGEXACT, -1, (LPARAM)sel->Name()) : 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_SETCURSEL, idx, 0);
#else // __linux__
	int idx = (sel ? DlgItem<QComboBox>(hPage, IDC_OPT_MKR_FEATUREBODY)->findText(QString::fromUtf8(sel->Name()), Qt::MatchFixedString) : 0);
	DlgItem<QComboBox>(hPage, IDC_OPT_MKR_FEATUREBODY)->setCurrentIndex(idx);
#endif // __linux__
	UpdateFeatureList(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Labels::UpdateFeatureList(HWND hPage)
#else // __linux__
void OptionsPage_Labels::UpdateFeatureList(QWidget *hPage)
#endif // __linux__
{
	int n, nlist;
	char cbuf[256], cpath[256];
#ifndef __linux__
	int idx = SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_GETLBTEXT, idx, (LPARAM)cbuf);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_RESETCONTENT, 0, 0);
#else // __linux__
	int idx = DlgItem<QComboBox>(hPage, IDC_OPT_MKR_FEATUREBODY)->currentIndex();
	snprintf(cbuf, sizeof(cbuf), "%s", DlgItem<QComboBox>(hPage, IDC_OPT_MKR_FEATUREBODY)->itemText(idx).toUtf8().constData());
	QSignalBlocker block(DlgItem<QListWidget>(hPage, IDC_OPT_MKR_FEATURELIST)); // LB_ messages don't notify
	DlgItem<QListWidget>(hPage, IDC_OPT_MKR_FEATURELIST)->clear();
#endif // __linux__
	Planet* planet = g_psys->GetPlanet(trim_string(cbuf), true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST* list = planet->LabelList(&nlist);
		if (!nlist) return;

		n = 0;
#ifndef __linux__
		planet->ForEach(FILETYPE_MARKER, [&](const fs::directory_entry& entry) {
				SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_ADDSTRING, 0, (LPARAM)entry.path().stem().string().c_str());
				if (n < nlist && list[n].active)
					SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_SETSEL, TRUE, n);
				n++;
			});
#else // __linux__
		planet->ForEach(FILETYPE_MARKER, [&](const fs::directory_entry& entry) {
				DlgItem<QListWidget>(hPage, IDC_OPT_MKR_FEATURELIST)->addItem(QString::fromUtf8(entry.path().stem().string().c_str()));
				if (n < nlist && list[n].active)
					DlgItem<QListWidget>(hPage, IDC_OPT_MKR_FEATURELIST)->item(n)->setSelected(true);
				n++;
			});
#endif // __linux__
	}
	else {
		int nlabel = planet->NumLabelLegend();
		if (nlabel) {
			const oapi::GraphicsClient::LABELTYPE* lspec = planet->LabelLegend();
			for (int i = 0; i < nlabel; i++) {
#ifndef __linux__
				SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_ADDSTRING, 0, (LPARAM)lspec[i].name);
#else // __linux__
				DlgItem<QListWidget>(hPage, IDC_OPT_MKR_FEATURELIST)->addItem(QString::fromUtf8(lspec[i].name));
#endif // __linux__
				if (lspec[i].active)
#ifndef __linux__
					SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_SETSEL, TRUE, i);
#else // __linux__
					DlgItem<QListWidget>(hPage, IDC_OPT_MKR_FEATURELIST)->item(i)->setSelected(true);
#endif // __linux__
			}
		}
	}
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Labels::RescanFeatures(HWND hPage)
#else // __linux__
void OptionsPage_Labels::RescanFeatures(QWidget *hPage)
#endif // __linux__
{
	char cbuf[256];
	int nlist;

#ifndef __linux__
	int idx = SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_GETLBTEXT, idx, (LPARAM)cbuf);
#else // __linux__
	int idx = DlgItem<QComboBox>(hPage, IDC_OPT_MKR_FEATUREBODY)->currentIndex();
	snprintf(cbuf, sizeof(cbuf), "%s", DlgItem<QComboBox>(hPage, IDC_OPT_MKR_FEATUREBODY)->itemText(idx).toUtf8().constData());
#endif // __linux__
	Planet* planet = g_psys->GetPlanet(trim_string(cbuf), true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST* list = planet->LabelList(&nlist);
		if (!nlist) return;

		for (int i = 0; i < nlist; i++) {
#ifndef __linux__
			BOOL sel = SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_GETSEL, i, 0);
#else // __linux__
			BOOL sel = DlgItem<QListWidget>(hPage, IDC_OPT_MKR_FEATURELIST)->item(i)->isSelected();
#endif // __linux__
			list[i].active = (sel ? true : false);
		}

#ifndef __linux__
		std::ifstream fcfg(Cfg()->ConfigPath(planet->Name()));
#else // __linux__
		std::ifstream fcfg(oapiResolvePath(Cfg()->ConfigPath(planet->Name())));
#endif // __linux__
		planet->ScanLabelLists(fcfg);
	}
	else {
		nlist = planet->NumLabelLegend();
		for (int i = 0; i < nlist; i++) {
#ifndef __linux__
			BOOL sel = SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_GETSEL, i, 0);
#else // __linux__
			BOOL sel = DlgItem<QListWidget>(hPage, IDC_OPT_MKR_FEATURELIST)->item(i)->isSelected();
#endif // __linux__
			planet->SetLabelActive(i, sel ? true : false);
		}
	}
}

// ======================================================================

OptionsPage_Forces::OptionsPage_Forces(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Forces::ResourceId() const
{
	return IDD_OPTIONS_BODYFORCE;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Forces::Name() const
{
	const char* name = "Body forces";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Forces::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/vh_force.htm");
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Forces::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Forces::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	std::array<int, 16> residForces = {
		IDC_OPT_VEC_WEIGHT, IDC_OPT_VEC_THRUST, IDC_OPT_VEC_LIFT, IDC_OPT_VEC_DRAG, IDC_OPT_VEC_SIDEFORCE, IDC_OPT_VEC_TOTAL,
		IDC_OPT_VEC_TORQUE, IDC_OPT_VEC_LINSCL, IDC_OPT_VEC_LOGSCL, IDC_OPT_VEC_SCALE, IDC_OPT_VEC_OPACITY,
		IDC_STATIC1, IDC_STATIC2, IDC_STATIC3, IDC_STATIC4, IDC_STATIC5
	};

	DWORD vecFlag = Cfg()->CfgVisHelpPrm.flagBodyForce;
	bool enable = (vecFlag & BFV_ENABLE);
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_VEC, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_VEC, enable);
#endif // __linux__
	for (auto resid : residForces)
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, resid), enable ? TRUE : FALSE);
#else // __linux__
		EnableItem(hPage, resid, enable ? TRUE : FALSE);
#endif // __linux__

#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_VEC_WEIGHT, BM_SETCHECK, vecFlag & BFV_WEIGHT ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_THRUST, BM_SETCHECK, vecFlag & BFV_THRUST ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_LIFT, BM_SETCHECK, vecFlag & BFV_LIFT ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_DRAG, BM_SETCHECK, vecFlag & BFV_DRAG ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_SIDEFORCE, BM_SETCHECK, vecFlag & BFV_SIDEFORCE ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_TOTAL, BM_SETCHECK, vecFlag & BFV_TOTAL ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_TORQUE, BM_SETCHECK, vecFlag & BFV_TORQUE ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_LINSCL, BM_SETCHECK, vecFlag & BFV_LOGSCALE ? BST_UNCHECKED : BST_CHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_LOGSCL, BM_SETCHECK, vecFlag & BFV_LOGSCALE ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_VEC_WEIGHT, vecFlag & BFV_WEIGHT);
	SetCheck(hPage, IDC_OPT_VEC_THRUST, vecFlag & BFV_THRUST);
	SetCheck(hPage, IDC_OPT_VEC_LIFT, vecFlag & BFV_LIFT);
	SetCheck(hPage, IDC_OPT_VEC_DRAG, vecFlag & BFV_DRAG);
	SetCheck(hPage, IDC_OPT_VEC_SIDEFORCE, vecFlag & BFV_SIDEFORCE);
	SetCheck(hPage, IDC_OPT_VEC_TOTAL, vecFlag & BFV_TOTAL);
	SetCheck(hPage, IDC_OPT_VEC_TORQUE, vecFlag & BFV_TORQUE);
	SetCheck(hPage, IDC_OPT_VEC_LINSCL, !(vecFlag & BFV_LOGSCALE));
	SetCheck(hPage, IDC_OPT_VEC_LOGSCL, vecFlag & BFV_LOGSCALE);
#endif // __linux__

	int scalePos = (int)(25.0 * (1.0 + 0.5 * log(Cfg()->CfgVisHelpPrm.scaleBodyForce) / log(2.0)));
#ifndef __linux__
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_VEC_SCALE), scalePos);
#else // __linux__
	oapiSetGaugePos(oapiResDlgItem(hPage, IDC_OPT_VEC_SCALE), scalePos);
#endif // __linux__
	int opacPos = (int)(Cfg()->CfgVisHelpPrm.opacBodyForce * 50.0);
#ifndef __linux__
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_VEC_OPACITY), opacPos);
#else // __linux__
	oapiSetGaugePos(oapiResDlgItem(hPage, IDC_OPT_VEC_OPACITY), opacPos);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Forces::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Forces::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
	GAUGEPARAM gp = { 0, 50, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
#ifndef __linux__
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_VEC_SCALE), &gp);
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_VEC_OPACITY), &gp);
#else // __linux__
	oapiSetGaugeParams(oapiResDlgItem(hPage, IDC_OPT_VEC_SCALE), &gp);
	oapiSetGaugeParams(oapiResDlgItem(hPage, IDC_OPT_VEC_OPACITY), &gp);
#endif // __linux__

	UpdateControls(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Forces::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_Forces::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_VEC:
	case IDC_OPT_VEC_WEIGHT:
	case IDC_OPT_VEC_THRUST:
	case IDC_OPT_VEC_LIFT:
	case IDC_OPT_VEC_DRAG:
	case IDC_OPT_VEC_SIDEFORCE:
	case IDC_OPT_VEC_TOTAL:
	case IDC_OPT_VEC_TORQUE:
	case IDC_OPT_VEC_LINSCL:
	case IDC_OPT_VEC_LOGSCL:
#ifndef __linux__
		if (notification == BN_CLICKED) {
#else // __linux__
		if (notification == RESN_CLICKED) {
#endif // __linux__
			OnItemClicked(hPage, ctrlId);
			return FALSE;
		}
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Forces::OnItemClicked(HWND hPage, WORD ctrlId)
#else // __linux__
void OptionsPage_Forces::OnItemClicked(QWidget *hPage, WORD ctrlId)
#endif // __linux__
{
#ifndef __linux__
	bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
	bool check = (IsChecked(hPage, ctrlId));
#endif // __linux__
	DWORD flag;
	switch (ctrlId) {
	case IDC_OPT_VEC:           flag = BFV_ENABLE;    break;
	case IDC_OPT_VEC_WEIGHT:    flag = BFV_WEIGHT;    break;
	case IDC_OPT_VEC_THRUST:    flag = BFV_THRUST;    break;
	case IDC_OPT_VEC_LIFT:      flag = BFV_LIFT;      break;
	case IDC_OPT_VEC_DRAG:      flag = BFV_DRAG;      break;
	case IDC_OPT_VEC_SIDEFORCE: flag = BFV_SIDEFORCE; break;
	case IDC_OPT_VEC_TOTAL:     flag = BFV_TOTAL;     break;
	case IDC_OPT_VEC_TORQUE:    flag = BFV_TORQUE;    break;
	case IDC_OPT_VEC_LINSCL:    flag = BFV_LOGSCALE; check = false; break;
	case IDC_OPT_VEC_LOGSCL: flag = BFV_LOGSCALE; check = true;  break;
	default:                 flag = 0;           break;
	}
	int& vecFlag = Cfg()->CfgVisHelpPrm.flagBodyForce;
	if (check) vecFlag |= flag;
	else       vecFlag &= ~flag;

	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Forces::OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Forces::OnHScroll(QWidget *hTab, int ctrlId, int request, int pos)
#endif // __linux__
{
#ifndef __linux__
	switch (GetDlgCtrlID((HWND)lParam)) {
#else // __linux__
	switch (ctrlId) {
#endif // __linux__
	case IDC_OPT_VEC_SCALE:
#ifndef __linux__
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			Cfg()->CfgVisHelpPrm.scaleBodyForce = (float)pow(2.0, (HIWORD(wParam) - 25) * 0.08);
#else // __linux__
		switch (request) {
		case GAUGE_THUMBTRACK:
		case GAUGE_LINEDEC:
		case GAUGE_LINEINC:
			Cfg()->CfgVisHelpPrm.scaleBodyForce = (float)pow(2.0, (pos - 25) * 0.08);
#endif // __linux__
			return 0;
		}
		break;
	case IDC_OPT_VEC_OPACITY:
#ifndef __linux__
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			Cfg()->CfgVisHelpPrm.opacBodyForce = (float)(HIWORD(wParam) * 0.02);
#else // __linux__
		switch (request) {
		case GAUGE_THUMBTRACK:
		case GAUGE_LINEDEC:
		case GAUGE_LINEINC:
			Cfg()->CfgVisHelpPrm.opacBodyForce = (float)(pos * 0.02);
#endif // __linux__
			return 0;
		}
		break;
	}
	return FALSE;
}

// ======================================================================

OptionsPage_Axes::OptionsPage_Axes(OptionsPageContainer* container)
	: OptionsPage(container)
{
}

// ----------------------------------------------------------------------

int OptionsPage_Axes::ResourceId() const
{
	return IDD_OPTIONS_FRAMEAXES;
}

// ----------------------------------------------------------------------

const char* OptionsPage_Axes::Name() const
{
	const char* name = "Object axes";
	return name;
}

// ----------------------------------------------------------------------

const HELPCONTEXT* OptionsPage_Axes::HelpContext() const
{
	static HELPCONTEXT hcontext = g_pOrbiter->DefaultHelpPage("/vh_coord.htm");
	return &hcontext;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Axes::UpdateControls(HWND hPage)
#else // __linux__
void OptionsPage_Axes::UpdateControls(QWidget *hPage)
#endif // __linux__
{
	std::array<int, 10> residAxes = {
		IDC_OPT_CRD_VESSEL, IDC_OPT_CRD_CELBODY, IDC_OPT_CRD_BASE, IDC_OPT_CRD_NEGATIVE,
		IDC_OPT_CRD_SCALE, IDC_OPT_CRD_OPACITY,
		IDC_STATIC1, IDC_STATIC2, IDC_STATIC3, IDC_STATIC4
	};

	DWORD crdFlag = Cfg()->CfgVisHelpPrm.flagFrameAxes;
	bool enable = (crdFlag & FAV_ENABLE);
#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_CRD, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_CRD, enable);
#endif // __linux__
	for (auto resid : residAxes)
#ifndef __linux__
		EnableWindow(GetDlgItem(hPage, resid), enable ? TRUE : FALSE);
#else // __linux__
		EnableItem(hPage, resid, enable ? TRUE : FALSE);
#endif // __linux__

#ifndef __linux__
	SendDlgItemMessage(hPage, IDC_OPT_CRD_VESSEL, BM_SETCHECK, crdFlag & FAV_VESSEL ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_CRD_CELBODY, BM_SETCHECK, crdFlag & FAV_CELBODY ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_CRD_BASE, BM_SETCHECK, crdFlag & FAV_BASE ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_CRD_NEGATIVE, BM_SETCHECK, crdFlag & FAV_NEGATIVE ? BST_CHECKED : BST_UNCHECKED, 0);
#else // __linux__
	SetCheck(hPage, IDC_OPT_CRD_VESSEL, crdFlag & FAV_VESSEL);
	SetCheck(hPage, IDC_OPT_CRD_CELBODY, crdFlag & FAV_CELBODY);
	SetCheck(hPage, IDC_OPT_CRD_BASE, crdFlag & FAV_BASE);
	SetCheck(hPage, IDC_OPT_CRD_NEGATIVE, crdFlag & FAV_NEGATIVE);
#endif // __linux__

	int scalePos = (int)(25.0 * (1.0 + 0.5 * log(Cfg()->CfgVisHelpPrm.scaleFrameAxes) / log(2.0)));
#ifndef __linux__
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_CRD_SCALE), scalePos);
#else // __linux__
	oapiSetGaugePos(oapiResDlgItem(hPage, IDC_OPT_CRD_SCALE), scalePos);
#endif // __linux__
	int opacPos = (int)(Cfg()->CfgVisHelpPrm.opacFrameAxes * 50.0);
#ifndef __linux__
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_CRD_OPACITY), opacPos);
#else // __linux__
	oapiSetGaugePos(oapiResDlgItem(hPage, IDC_OPT_CRD_OPACITY), opacPos);
#endif // __linux__
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Axes::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Axes::OnInitDialog(QWidget *hPage)
#endif // __linux__
{
	GAUGEPARAM gp = { 0, 50, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
#ifndef __linux__
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_CRD_SCALE), &gp);
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_CRD_OPACITY), &gp);
#else // __linux__
	oapiSetGaugeParams(oapiResDlgItem(hPage, IDC_OPT_CRD_SCALE), &gp);
	oapiSetGaugeParams(oapiResDlgItem(hPage, IDC_OPT_CRD_OPACITY), &gp);
#endif // __linux__

	UpdateControls(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Axes::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
#else // __linux__
BOOL OptionsPage_Axes::OnCommand(QWidget *hPage, WORD ctrlId, WORD notification, QWidget *hCtrl)
#endif // __linux__
{
	switch (ctrlId) {
	case IDC_OPT_CRD:
	case IDC_OPT_CRD_VESSEL:
	case IDC_OPT_CRD_CELBODY:
	case IDC_OPT_CRD_BASE:
	case IDC_OPT_CRD_NEGATIVE:
#ifndef __linux__
		if (notification == BN_CLICKED) {
#else // __linux__
		if (notification == RESN_CLICKED) {
#endif // __linux__
			OnItemClicked(hPage, ctrlId);
			return FALSE;
		}
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

#ifndef __linux__
void OptionsPage_Axes::OnItemClicked(HWND hPage, WORD ctrlId)
#else // __linux__
void OptionsPage_Axes::OnItemClicked(QWidget *hPage, WORD ctrlId)
#endif // __linux__
{
#ifndef __linux__
	bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
#else // __linux__
	bool check = (IsChecked(hPage, ctrlId));
#endif // __linux__
	DWORD flag;
	switch (ctrlId) {
	case IDC_OPT_CRD:          flag = FAV_ENABLE;   break;
	case IDC_OPT_CRD_VESSEL:   flag = FAV_VESSEL;   break;
	case IDC_OPT_CRD_CELBODY:  flag = FAV_CELBODY;  break;
	case IDC_OPT_CRD_BASE:     flag = FAV_BASE;     break;
	case IDC_OPT_CRD_NEGATIVE: flag = FAV_NEGATIVE; break;
	default:                   flag = 0;            break;
	}
	int& crdFlag = Cfg()->CfgVisHelpPrm.flagFrameAxes;
	if (check) crdFlag |= flag;
	else       crdFlag &= ~flag;

	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

#ifndef __linux__
BOOL OptionsPage_Axes::OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL OptionsPage_Axes::OnHScroll(QWidget *hTab, int ctrlId, int request, int pos)
#endif // __linux__
{
#ifndef __linux__
	switch (GetDlgCtrlID((HWND)lParam)) {
#else // __linux__
	switch (ctrlId) {
#endif // __linux__
	case IDC_OPT_CRD_SCALE:
#ifndef __linux__
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			Cfg()->CfgVisHelpPrm.scaleFrameAxes = (float)pow(2.0, (HIWORD(wParam) - 25) * 0.08);
#else // __linux__
		switch (request) {
		case GAUGE_THUMBTRACK:
		case GAUGE_LINEDEC:
		case GAUGE_LINEINC:
			Cfg()->CfgVisHelpPrm.scaleFrameAxes = (float)pow(2.0, (pos - 25) * 0.08);
#endif // __linux__
			return 0;
		}
		break;
	case IDC_OPT_CRD_OPACITY:
#ifndef __linux__
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			Cfg()->CfgVisHelpPrm.opacFrameAxes = (float)(HIWORD(wParam) * 0.02);
#else // __linux__
		switch (request) {
		case GAUGE_THUMBTRACK:
		case GAUGE_LINEDEC:
		case GAUGE_LINEINC:
			Cfg()->CfgVisHelpPrm.opacFrameAxes = (float)(pos * 0.02);
#endif // __linux__
			return 0;
		}
		break;
	}
	return FALSE;
}
