// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// In-session options dialog
// ======================================================================

#define STRICT 1

#include <io.h>
#include <array>
#include "DlgOptions.h"
#include "Orbiter.h"
#include "OrbiterAPI.h"
#include "Psys.h"
#include "Camera.h"
#include "DlgCtrl.h"
#include "resource.h"
#include "Uxtheme.h"
#include <commctrl.h>

extern Orbiter* g_pOrbiter;
extern PlanetarySystem* g_psys;
extern Camera* g_camera;
extern HELPCONTEXT DefHelpContext;

// ======================================================================

DlgOptions::DlgOptions(HINSTANCE hInstance, HWND hParent, void* context)
	: DialogWin(hInstance, hParent, IDD_OPTIONS, 0, 0, context)
{
	pos = &g_pOrbiter->Cfg()->CfgWindowPos.DlgOptions;
	m_pageIdx = 0;
	m_vScrollPage = 0;
	m_vScrollRange = 0;
	m_vScrollPos = 0;
	m_contextHelp = 0;
}

// ----------------------------------------------------------------------

DlgOptions::~DlgOptions()
{
	Clear();
}

// ----------------------------------------------------------------------

void DlgOptions::Update()
{
	for (auto pPage : m_pPage)
		pPage->UpdateControls(pPage->HPage());
}

// ----------------------------------------------------------------------

BOOL DlgOptions::OnInitDialog(HWND hDlg, WPARAM wParam, LPARAM lParam)
{
	m_splitter.SetHwnd(GetDlgItem(hDlg, IDC_OPT_SPLIT), GetDlgItem(hDlg, IDC_OPT_PAGELIST), GetDlgItem(hDlg, IDC_OPT_PAGECONTAINER));
	m_container.SetHwnd(GetDlgItem(hDlg, IDC_OPT_PAGECONTAINER));
	m_splitter.SetStaticPane(SplitterCtrl::PANE1, 120);
	SetSize(hDlg);

	HTREEITEM parent;
	AddPage(hDlg, new OptionsPage_CelSphere(this));
	parent = AddPage(hDlg, new OptionsPage_VisHelper(this));
	AddPage(hDlg, new OptionsPage_Planetarium(this), parent);
	AddPage(hDlg, new OptionsPage_Labels(this), parent);
	AddPage(hDlg, new OptionsPage_Forces(this), parent);
	AddPage(hDlg, new OptionsPage_Axes(this), parent);
	SwitchPage(hDlg, (size_t)0);

	ExpandAll(hDlg);

	return TRUE;
}

// ----------------------------------------------------------------------

BOOL DlgOptions::OnCommand(HWND hDlg, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDHELP:
		if (notification == BN_CLICKED) {
			if (m_contextHelp)
			g_pOrbiter->OpenHelp(m_contextHelp);
			return TRUE;
		}
		break;
	}
	return DialogWin::OnCommand(hDlg, ctrlId, notification, hCtrl);
}

// ----------------------------------------------------------------------

BOOL DlgOptions::OnSize(HWND hDlg, WPARAM wParam, int w, int h)
{
	SetSize(hDlg);
	return 0;
}

// ----------------------------------------------------------------------

BOOL DlgOptions::OnVScroll(HWND hDlg, WORD request, WORD curpos, HWND hControl)
{
	HWND hPage = m_pPage[m_pageIdx]->HPage();

	SCROLLINFO scrollinfo;
	scrollinfo.cbSize = sizeof(SCROLLINFO);
	GetScrollInfo(hControl, SB_CTL, &scrollinfo);
	int pos = -1;

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
	if (pos >= 0 && pos != m_vScrollPos) {
		int dy = -(pos - m_vScrollPos);
		scrollinfo.nPos = m_vScrollPos = pos;
		scrollinfo.fMask = SIF_POS;
		SetScrollInfo(hControl, SB_CTL, &scrollinfo, TRUE);
		ScrollWindow(hPage, 0, dy, NULL, NULL);
		UpdateWindow(hPage);
	}
	return FALSE;
}

// ----------------------------------------------------------------------

BOOL DlgOptions::OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh)
{
	NM_TREEVIEW* pnmtv;

	switch (idCtrl) {
	case IDC_OPT_PAGELIST:
		pnmtv = (NM_TREEVIEW FAR*)pnmh;
		switch (pnmtv->hdr.code) {
		case TVN_SELCHANGED: {
			OptionsPage* page = (OptionsPage*)pnmtv->itemNew.lParam;
			SwitchPage(hDlg, page);
			TreeView_Expand(GetDlgItem(hDlg, IDC_OPT_PAGELIST), pnmtv->itemNew.hItem, TVE_EXPAND);
			}
			return TRUE;
		}
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

HTREEITEM DlgOptions::AddPage(HWND hDlg, OptionsPage* pPage, HTREEITEM parent)
{
	m_pPage.push_back(pPage);
	return pPage->CreatePage(hDlg, parent);
}

// ----------------------------------------------------------------------

void DlgOptions::SetSize(HWND hDlg)
{
	RECT r0;

	GetClientRect(GetDlgItem(hDlg, IDCANCEL), &r0);
	GetClientRect(hDlg, &r0);
	SetWindowPos(GetDlgItem(hDlg, IDC_OPT_SPLIT), HWND_BOTTOM, 9, 10, r0.right - 25, r0.bottom - 52, SWP_NOACTIVATE | SWP_NOOWNERZORDER);
	SetWindowPos(GetDlgItem(hDlg, IDC_SCROLLBAR1), NULL, r0.right - 16, 10, 14, r0.bottom - 52, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER);
	SetWindowPos(GetDlgItem(hDlg, IDCANCEL), NULL, r0.right - 84, r0.bottom - 33, 0, 0, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_NOSIZE);
	SetWindowPos(GetDlgItem(hDlg, IDHELP), NULL, r0.right - 165, r0.bottom - 33, 0, 0, SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOZORDER | SWP_NOSIZE);

	InvalidateRect(hDlg, NULL, TRUE);
	UpdateWindow(hDlg);

	SetPageSize(hDlg);
}

// ----------------------------------------------------------------------

void DlgOptions::SetPageSize(HWND hDlg)
{
	if (m_pageIdx >= m_pPage.size()) return; // sanity check

	RECT r0, r1;
	GetClientRect(m_container.HWnd(), &r0);
	GetClientRect(m_pPage[m_pageIdx]->HPage(), &r1);
	bool bVscroll = r1.bottom > r0.bottom;
	ShowWindow(GetDlgItem(hDlg, IDC_SCROLLBAR1), bVscroll ? SW_SHOW : SW_HIDE);

	if (bVscroll) {
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
		if (dy)
			ScrollWindow(m_pPage[m_pageIdx]->HPage(), 0, dy, NULL, NULL);
	}
	else if (m_vScrollPos) {
		ScrollWindow(m_pPage[m_pageIdx]->HPage(), 0, m_vScrollPos, NULL, NULL);
		m_vScrollPos = 0;
	}
}

// ----------------------------------------------------------------------

void DlgOptions::SwitchPage(const char* name)
{
	char cbuf[256];
	HWND hDlg = GetHwnd();
	HWND hTree = GetDlgItem(hDlg, IDC_OPT_PAGELIST);
	TVITEM tvi;
	tvi.hItem = TreeView_GetRoot(hTree);
	tvi.pszText = cbuf;
	tvi.cchTextMax = 256;
	tvi.cChildren = 0;
	tvi.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_CHILDREN;
	while (tvi.hItem) {
		TreeView_GetItem(hTree, &tvi);
		if (!stricmp(cbuf, name)) {
			TreeView_SelectItem(hTree, tvi.hItem);
			break;
		}
		TVITEM tvi_child;
		tvi_child.hItem = TreeView_GetChild(hTree, tvi.hItem);
		tvi_child.pszText = cbuf;
		tvi_child.cchTextMax = 256;
		tvi_child.cChildren = 0;
		tvi_child.mask = TVIF_HANDLE | TVIF_TEXT | TVIF_CHILDREN;
		while (tvi_child.hItem) {
			TreeView_GetItem(hTree, &tvi_child);
			if (!stricmp(cbuf, name)) {
				TreeView_SelectItem(hTree, tvi_child.hItem);
				break;
			}
			tvi_child.hItem = TreeView_GetNextSibling(hTree, tvi_child.hItem);
		}
		tvi.hItem = TreeView_GetNextSibling(hTree, tvi.hItem);
	}
}

// ----------------------------------------------------------------------

void DlgOptions::SwitchPage(HWND hDlg, size_t page)
{
	if (page < 0 || page >= m_pPage.size())
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

	SetPageSize(hDlg);
	InvalidateRect(hDlg, NULL, TRUE);
}

// ----------------------------------------------------------------------

void DlgOptions::SwitchPage(HWND hDlg, const OptionsPage* page)
{
	for (size_t i = 0; i < m_pPage.size(); i++)
		if (m_pPage[i] == page) {
			SwitchPage(hDlg, i);
			break;
		}
}

// ----------------------------------------------------------------------

const OptionsPage* DlgOptions::FindPage(const char* name) const
{
	for (auto page : m_pPage)
		if (!strcmp(name, page->Name()))
			return page;
	return 0;
}

// ----------------------------------------------------------------------

void DlgOptions::Clear()
{
	for (auto pPage : m_pPage)
		delete pPage;
	m_pPage.clear();
}

// ----------------------------------------------------------------------

void DlgOptions::ExpandAll(HWND hDlg)
{
	bool expand = true;
	HWND hTree = GetDlgItem(hDlg, IDC_OPT_PAGELIST);
	UINT code = (expand ? TVE_EXPAND : TVE_COLLAPSE);
	TVITEM catitem;
	catitem.mask = NULL;
	catitem.hItem = TreeView_GetRoot(hTree);
	while (TreeView_GetItem(hTree, &catitem)) {
		TreeView_Expand(hTree, catitem.hItem, code);
		catitem.hItem = TreeView_GetNextSibling(hTree, catitem.hItem);
	}
}

// ======================================================================

OptionsPage::OptionsPage(DlgOptions *dlg)
	: m_dlg(dlg)
	, m_hPage(0)
	, m_hItem(0)
{
}

// ----------------------------------------------------------------------

OptionsPage::~OptionsPage()
{
	// Remove the object reference from the window.
	// This is so that object methods will no longer be called from the message loop
	// while the window hasn't been destroyed.
	if (m_hPage)
		SetWindowLongPtr(m_hPage, DWLP_USER, 0);
}

// ----------------------------------------------------------------------

void OptionsPage::Show(bool bShow)
{
	ShowWindow(m_hPage, bShow ? SW_SHOW : SW_HIDE);
}

// ----------------------------------------------------------------------

HWND OptionsPage::HParent() const
{
	return m_dlg->PageContainer()->HWnd();
}

// ----------------------------------------------------------------------

HTREEITEM OptionsPage::CreatePage(HWND hDlg, HTREEITEM parent)
{
	int winId = ResourceId();
	m_hPage = CreateDialogParam(g_pOrbiter->GetInstance(), MAKEINTRESOURCE(winId), HParent(), s_DlgProc, (LPARAM)this);

	char cbuf[256];
	strcpy(cbuf, Name());
	TV_INSERTSTRUCT tvis;
	tvis.item.mask = TVIF_TEXT | TVIF_PARAM;
	tvis.item.pszText = cbuf;
	tvis.item.lParam = (LPARAM)this;
	tvis.hInsertAfter = TVI_LAST;
	tvis.hParent = parent;
	HTREEITEM hti = TreeView_InsertItem(GetDlgItem(hDlg, IDC_OPT_PAGELIST), &tvis);
	m_hItem = hti;
	return hti;
}

// ----------------------------------------------------------------------

BOOL OptionsPage::OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	UpdateControls(hWnd);
	return TRUE;
}

// ----------------------------------------------------------------------

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
{
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
	}
	return (pPage ? pPage->DlgProc(hWnd, uMsg, wParam, lParam) : DefWindowProc(hWnd, uMsg, wParam, lParam));
}

// ======================================================================

OptionsPage_CelSphere::OptionsPage_CelSphere(DlgOptions* dlg)
	: OptionsPage(dlg)
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

BOOL OptionsPage_CelSphere::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
{
	OptionsPage::OnInitDialog(hPage, wParam, lParam);

	GAUGEPARAM gp = { 0, 100, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_BGBRIGHTNESS), &gp);

	PopulateStarmapList(hPage);
	PopulateBgImageList(hPage);
	UpdateControls(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

BOOL OptionsPage_CelSphere::OnCommand(HWND hPage, WORD id, WORD code, HWND hControl)
{
	switch (id) {
	case IDC_OPT_ENABLESTARPIX:
		if (code == BN_CLICKED) {
			StarPixelActivationChanged(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_ENABLESTARMAP:
		if (code == BN_CLICKED) {
			StarmapActivationChanged(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_ENABLEBKGMAP:
		if (code == BN_CLICKED) {
			BackgroundActivationChanged(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_STARMAPIMAGE:
		if (code == LBN_SELCHANGE) {
			StarmapImageChanged(hPage);
			return false;
		}
		break;
	case IDC_OPT_BKGIMAGE:
		if (code == LBN_SELCHANGE) {
			BackgroundImageChanged(hPage);
			return FALSE;
		}
		break;
	case IDC_OPT_STARMAPLIN:
	case IDC_OPT_STARMAPEXP:
		if (code == BN_CLICKED) {
			g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.map_log = (id == IDC_OPT_STARMAPEXP);
			g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_STARDISPLAYPARAM);
			return FALSE;
		}
		break;
	}
	return TRUE;
}

// ----------------------------------------------------------------------

BOOL OptionsPage_CelSphere::OnHScroll(HWND hPage, WPARAM wParam, LPARAM lParam)
{
	switch (GetDlgCtrlID((HWND)lParam)) {
	case IDC_OPT_BGBRIGHTNESS:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			BackgroundBrightnessChanged(hPage, 0.01 * HIWORD(wParam));
			return 0;
		}
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

BOOL OptionsPage_CelSphere::OnNotify(HWND hPage, DWORD ctrlId, const NMHDR* pNmHdr)
{
	if (pNmHdr->code == UDN_DELTAPOS) {
		NMUPDOWN* nmud = (NMUPDOWN*)pNmHdr;
		int delta = -nmud->iDelta;
		StarRenderPrm& prm = g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm;
		switch (pNmHdr->idFrom) {
		case IDC_OPT_STARMAGHISPIN:
			prm.mag_hi = min(prm.mag_lo, max(-2.0, prm.mag_hi + delta * 0.1));
			break;
		case IDC_OPT_STARMAGLOSPIN:
			prm.mag_lo = min(15.0, max(prm.mag_hi, prm.mag_lo + delta * 0.1));
			break;
		case IDC_OPT_STARMINBRTSPIN:
			prm.brt_min = min(1.0, max(0.0, prm.brt_min + delta * 0.01));
			break;
		}
		UpdateControls(hPage);
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_STARDISPLAYPARAM);
		return TRUE;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::UpdateControls(HWND hPage)
{
	char cbuf[256];

	std::string starpath = std::string(g_pOrbiter->Cfg()->CfgVisualPrm.StarImagePath);
	for (int idx = 0; idx < m_pathStarmap.size(); idx++)
		if (!starpath.compare(m_pathStarmap[idx].second)) {
			SendDlgItemMessage(hPage, IDC_OPT_STARMAPIMAGE, CB_SETCURSEL, idx, 0);
			break;
		}

	std::string bgpath = std::string(g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgPath);
	for (int idx = 0; idx < m_pathBgImage.size(); idx++)
		if (!bgpath.compare(m_pathBgImage[idx].second)) {
			SendDlgItemMessage(hPage, IDC_OPT_BKGIMAGE, CB_SETCURSEL, idx, 0);
			break;
		}

	bool checked = g_pOrbiter->Cfg()->CfgVisualPrm.bUseStarImage;
	SendDlgItemMessage(hPage, IDC_OPT_ENABLESTARMAP, BM_SETCHECK, checked ? BST_CHECKED : BST_UNCHECKED, 0);
	EnableWindow(GetDlgItem(hPage, IDC_OPT_STARMAPIMAGE), checked ? TRUE : FALSE);

	checked = g_pOrbiter->Cfg()->CfgVisualPrm.bUseBgImage;
	SendDlgItemMessage(hPage, IDC_OPT_ENABLEBKGMAP, BM_SETCHECK, checked ? BST_CHECKED : BST_UNCHECKED, 0);
	int brt = (int)(g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgIntens * 100.0);
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_BGBRIGHTNESS), brt);
	EnableWindow(GetDlgItem(hPage, IDC_OPT_BKGIMAGE), checked ? TRUE : FALSE);
	EnableWindow(GetDlgItem(hPage, IDC_STATIC1), checked ? TRUE : FALSE);
	EnableWindow(GetDlgItem(hPage, IDC_OPT_BGBRIGHTNESS), checked ? TRUE : FALSE);

	checked = g_pOrbiter->Cfg()->CfgVisualPrm.bUseStarDots;
	SendDlgItemMessage(hPage, IDC_OPT_ENABLESTARPIX, BM_SETCHECK, checked ? BST_CHECKED : BST_UNCHECKED, 0);
	sprintf(cbuf, "%0.1f", g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.mag_hi);
	SetWindowText(GetDlgItem(hPage, IDC_OPT_STARMAGHI), cbuf);
	sprintf(cbuf, "%0.1f", g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.mag_lo);
	SetWindowText(GetDlgItem(hPage, IDC_OPT_STARMAGLO), cbuf);
	sprintf(cbuf, "%0.2f", g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.brt_min);
	SetWindowText(GetDlgItem(hPage, IDC_OPT_STARMINBRT), cbuf);
	SendDlgItemMessage(hPage, IDC_OPT_STARMAPLIN, BM_SETCHECK,
		g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.map_log ? BST_UNCHECKED : BST_CHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_STARMAPEXP, BM_SETCHECK,
		g_pOrbiter->Cfg()->CfgVisualPrm.StarPrm.map_log ? BST_CHECKED : BST_UNCHECKED, 0);
	std::vector<int> ctrlStarPix{
		IDC_STATIC2, IDC_STATIC3, IDC_STATIC4, IDC_STATIC5, IDC_STATIC6,
		IDC_OPT_STARMAGHISPIN, IDC_OPT_STARMAGLOSPIN, IDC_OPT_STARMINBRTSPIN,
		IDC_OPT_STARMAGHI, IDC_OPT_STARMAGLO, IDC_OPT_STARMINBRT, IDC_OPT_STARMAPLIN, IDC_OPT_STARMAPEXP
	};
	for (auto ctrl : ctrlStarPix)
		EnableWindow(GetDlgItem(hPage, ctrl), checked ? TRUE : FALSE);
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::PopulateStarmapList(HWND hPage)
{
	SendDlgItemMessage(hPage, IDC_OPT_STARMAPIMAGE, CB_RESETCONTENT, 0, 0);
	m_pathStarmap.clear();

	std::ifstream ifs(g_pOrbiter->Cfg()->ConfigPath("CSphere\\bkgimage"));
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
				SendDlgItemMessage(hPage, IDC_OPT_STARMAPIMAGE, CB_ADDSTRING, 0, (LPARAM)c);
				std::string label(c);
				c = strtok(NULL, "\n");
				std::string path(c);
				m_pathStarmap.push_back(std::make_pair(label, path));
			}
		}
	}
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::PopulateBgImageList(HWND hPage)
{
	SendDlgItemMessage(hPage, IDC_OPT_BKGIMAGE, CB_RESETCONTENT, 0, 0);
	m_pathBgImage.clear();

	std::ifstream ifs(g_pOrbiter->Cfg()->ConfigPath("CSphere\\bkgimage"));
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
				SendDlgItemMessage(hPage, IDC_OPT_BKGIMAGE, CB_ADDSTRING, 0, (LPARAM)c);
				std::string label(c);
				c = strtok(NULL, "\n");
				std::string path(c);
				m_pathBgImage.push_back(std::make_pair(label, path));
			}
		}
	}
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::StarPixelActivationChanged(HWND hPage)
{
	bool activated = SendDlgItemMessage(hPage, IDC_OPT_ENABLESTARPIX, BM_GETCHECK, 0, 0) == BST_CHECKED;
	bool active = g_pOrbiter->Cfg()->CfgVisualPrm.bUseStarDots;

	if (activated != active) {
		g_pOrbiter->Cfg()->CfgVisualPrm.bUseStarDots = activated;
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATESTARDOTS);
	}
	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::StarmapActivationChanged(HWND hPage)
{
	bool activated = SendDlgItemMessage(hPage, IDC_OPT_ENABLESTARMAP, BM_GETCHECK, 0, 0) == BST_CHECKED;
	bool active = g_pOrbiter->Cfg()->CfgVisualPrm.bUseStarImage;

	if (activated != active) {
		g_pOrbiter->Cfg()->CfgVisualPrm.bUseStarImage = activated;
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATESTARIMAGE);
	}
	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::StarmapImageChanged(HWND hPage)
{
	int idx = SendDlgItemMessage(hPage, IDC_OPT_STARMAPIMAGE, CB_GETCURSEL, 0, 0);
	std::string& path = m_pathStarmap[idx].second;
	strncpy(g_pOrbiter->Cfg()->CfgVisualPrm.StarImagePath, path.c_str(), 128);
	g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_STARIMAGECHANGED);
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::BackgroundActivationChanged(HWND hPage)
{
	bool activated = SendDlgItemMessage(hPage, IDC_OPT_ENABLEBKGMAP, BM_GETCHECK, 0, 0) == BST_CHECKED;
	bool active = g_pOrbiter->Cfg()->CfgVisualPrm.bUseBgImage;

	if (activated != active) {
		g_pOrbiter->Cfg()->CfgVisualPrm.bUseBgImage = activated;
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_ACTIVATEBGIMAGE);
	}
	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::BackgroundImageChanged(HWND hPage)
{
	int idx = SendDlgItemMessage(hPage, IDC_OPT_BKGIMAGE, CB_GETCURSEL, 0, 0);
	std::string& path = m_pathBgImage[idx].second;
	strncpy(g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgPath, path.c_str(), 128);
	g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_BGIMAGECHANGED);
}

// ----------------------------------------------------------------------

void OptionsPage_CelSphere::BackgroundBrightnessChanged(HWND hPage, double level)
{
	if (level != g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgIntens) {
		g_pOrbiter->Cfg()->CfgVisualPrm.CSphereBgIntens = level;
		g_pOrbiter->OnOptionChanged(OPTCAT_CELSPHERE, OPTITEM_CELSPHERE_BGIMAGEBRIGHTNESS);
	}
}

// ======================================================================

OptionsPage_VisHelper::OptionsPage_VisHelper(DlgOptions* dlg)
	: OptionsPage(dlg)
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

void OptionsPage_VisHelper::UpdateControls(HWND hPage)
{
	DWORD& plnFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagPlanetarium;
	bool enable = plnFlag & PLN_ENABLE;
	SendDlgItemMessage(hPage, IDC_OPT_PLN, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	DWORD& mkrFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagMarkers;
	enable = mkrFlag & MKR_ENABLE;
	SendDlgItemMessage(hPage, IDC_OPT_MKR, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	DWORD vecFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagBodyForce;
	enable = (vecFlag & BFV_ENABLE);
	SendDlgItemMessage(hPage, IDC_OPT_VEC, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	DWORD crdFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagFrameAxes;
	enable = (crdFlag & FAV_ENABLE);
	SendDlgItemMessage(hPage, IDC_OPT_CRD, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
}

// ----------------------------------------------------------------------

BOOL OptionsPage_VisHelper::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
{
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
	return TRUE;
}

// ----------------------------------------------------------------------

BOOL OptionsPage_VisHelper::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_OPT_PLN:
		if (notification == BN_CLICKED) {
			g_pOrbiter->TogglePlanetariumMode();
			return TRUE;
		}
		break;
	case IDC_OPT_MKR:
		if (notification == BN_CLICKED) {
			g_pOrbiter->ToggleLabelDisplay();
			return TRUE;
		}
		break;
	case IDC_OPT_VEC:
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
			DWORD flag = BFV_ENABLE;
			DWORD& vecFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagBodyForce;
			if (check) vecFlag |= flag;
			else       vecFlag &= ~flag;
		}
		break;
	case IDC_OPT_CRD:
		if (notification == BN_CLICKED) {
			bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
			DWORD flag = FAV_ENABLE;
			DWORD& crdFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagFrameAxes;
			if (check) crdFlag |= flag;
			else       crdFlag &= ~flag;
		}
		break;
	case IDC_OPT_VHELP_PLN:
		if (notification == BN_CLICKED)
			Dlg()->SwitchPage("Planetarium");
		break;
	case IDC_OPT_VHELP_MKR:
		if (notification == BN_CLICKED)
			Dlg()->SwitchPage("Labels");
		break;
	case IDC_OPT_VHELP_VEC:
		if (notification == BN_CLICKED)
			Dlg()->SwitchPage("Body forces");
		break;
	case IDC_OPT_VHELP_CRD:
		if (notification == BN_CLICKED)
			Dlg()->SwitchPage("Object axes");
		break;
	}
	return FALSE;
}

// ======================================================================

OptionsPage_Planetarium::OptionsPage_Planetarium(DlgOptions* dlg)
	: OptionsPage(dlg)
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

void OptionsPage_Planetarium::UpdateControls(HWND hPage)
{
	std::array<int, 15> residPlanetarium = {
		IDC_OPT_PLN_CELGRID, IDC_OPT_PLN_ECLGRID, IDC_OPT_PLN_GALGRID, IDC_OPT_PLN_HRZGRID, IDC_OPT_PLN_EQU,
		IDC_OPT_PLN_CNSTLABEL, IDC_OPT_PLN_CNSTLABEL_FULL, IDC_OPT_PLN_CNSTLABEL_SHORT, IDC_OPT_PLN_CNSTBND,
		IDC_OPT_PLN_CNSTPATTERN, IDC_OPT_PLN_MARKER, IDC_OPT_PLN_MKRLIST,
		IDC_STATIC1, IDC_STATIC2, IDC_STATIC3
	};

	DWORD& plnFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagPlanetarium;
	bool enable = plnFlag & PLN_ENABLE;
	SendDlgItemMessage(hPage, IDC_OPT_PLN, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	for (auto resid : residPlanetarium)
		EnableWindow(GetDlgItem(hPage, resid), enable ? TRUE : FALSE);
	if (enable && !(plnFlag & PLN_CNSTLABEL)) {
		EnableWindow(GetDlgItem(hPage, IDC_OPT_PLN_CNSTLABEL_FULL), FALSE);
		EnableWindow(GetDlgItem(hPage, IDC_OPT_PLN_CNSTLABEL_SHORT), FALSE);
	}
	if (enable && !(plnFlag & PLN_CCMARK))
		EnableWindow(GetDlgItem(hPage, IDC_OPT_PLN_MKRLIST), FALSE);

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
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Planetarium::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
{
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
	RescanMarkerList(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Planetarium::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_OPT_PLN:
		if (notification == BN_CLICKED) {
			g_pOrbiter->TogglePlanetariumMode();
			return TRUE;
		}
		break;
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
		if (notification == BN_CLICKED) {
			OnItemClicked(hPage, ctrlId);
			return TRUE;
		}
		break;
	case IDC_OPT_PLN_MKRLIST:
		if (notification == LBN_SELCHANGE)
			return OnMarkerSelectionChanged(hPage);
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

void OptionsPage_Planetarium::OnItemClicked(HWND hPage, WORD ctrlId)
{
	bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
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
	DWORD& plnFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagPlanetarium;
	if (check) plnFlag |= flag;
	else       plnFlag &= ~flag;

	g_pOrbiter->OnOptionChanged(OPTCAT_PLANETARIUM, OPTITEM_PLANETARIUM_DISPFLAG);
	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Planetarium::OnMarkerSelectionChanged(HWND hPage)
{
	std::vector<oapi::GraphicsClient::LABELLIST>& list = g_psys->LabelList();
	if (list.size()) {
		for (int i = 0; i < list.size(); i++) {
			int sel = SendDlgItemMessage(hPage, IDC_OPT_PLN_MKRLIST, LB_GETSEL, i, 0);
			list[i].active = (sel ? true : false);
		}
		std::ifstream cfg(g_pOrbiter->Cfg()->ConfigPath(g_psys->Name()));
		g_psys->ScanLabelLists(cfg);
	}
	return 0;
}

// ----------------------------------------------------------------------

void OptionsPage_Planetarium::RescanMarkerList(HWND hPage)
{
	SendDlgItemMessage(hPage, IDC_OPT_PLN_MKRLIST, LB_RESETCONTENT, 0, 0);

	const std::vector< oapi::GraphicsClient::LABELLIST>& list = g_psys->LabelList();
	if (!list.size()) return;

	char cbuf[256];
	_finddata_t fdata;
	intptr_t fh = g_psys->FindFirst(FILETYPE_MARKER, &fdata, cbuf);
	if (fh >= 0) {
		int n = 0;
		do {
			SendDlgItemMessage(hPage, IDC_OPT_PLN_MKRLIST, LB_ADDSTRING, 0, (LPARAM)trim_string(cbuf));
			if (n < list.size() && list[n].active)
				SendDlgItemMessage(hPage, IDC_OPT_PLN_MKRLIST, LB_SETSEL, TRUE, n);
			n++;
		} while (!g_psys->FindNext(fh, &fdata, cbuf));
		_findclose(fh);
	}
}

// ======================================================================

OptionsPage_Labels::OptionsPage_Labels(DlgOptions* dlg)
	: OptionsPage(dlg)
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

void OptionsPage_Labels::UpdateControls(HWND hPage)
{
	std::array<int, 10> residLabels = {
		IDC_OPT_MKR_VESSEL, IDC_OPT_MKR_CELBODY, IDC_OPT_MKR_FEATUREBODY, IDC_OPT_MKR_BASE,
		IDC_OPT_MKR_BEACON, IDC_OPT_MKR_FEATURES, IDC_OPT_MKR_FEATUREBODY, IDC_OPT_MKR_FEATURELIST,
		IDC_STATIC1, IDC_STATIC2
	};

	DWORD& mkrFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagMarkers;
	bool enable = mkrFlag & MKR_ENABLE;
	SendDlgItemMessage(hPage, IDC_OPT_MKR, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	for (auto resid : residLabels)
		EnableWindow(GetDlgItem(hPage, resid), enable ? TRUE : FALSE);
	if (enable && !(mkrFlag & MKR_LMARK)) {
		EnableWindow(GetDlgItem(hPage, IDC_OPT_MKR_FEATUREBODY), FALSE);
		EnableWindow(GetDlgItem(hPage, IDC_OPT_MKR_FEATURELIST), FALSE);
	}

	SendDlgItemMessage(hPage, IDC_OPT_MKR_VESSEL,   BM_SETCHECK, mkrFlag & MKR_VMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_CELBODY,  BM_SETCHECK, mkrFlag & MKR_CMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_BASE,     BM_SETCHECK, mkrFlag & MKR_BMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_BEACON,   BM_SETCHECK, mkrFlag & MKR_RMARK ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURES, BM_SETCHECK, mkrFlag & MKR_LMARK ? BST_CHECKED : BST_UNCHECKED, 0);
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Labels::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
{
	OptionsPage::OnInitDialog(hPage, wParam, lParam);
	ScanPsysBodies(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Labels::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_OPT_MKR:
		if (notification == BN_CLICKED) {
			g_pOrbiter->ToggleLabelDisplay();
			return TRUE;
		}
	case IDC_OPT_MKR_VESSEL:
	case IDC_OPT_MKR_CELBODY:
	case IDC_OPT_MKR_BASE:
	case IDC_OPT_MKR_BEACON:
	case IDC_OPT_MKR_FEATURES:
		if (notification == BN_CLICKED) {
			OnItemClicked(hPage, ctrlId);
			return TRUE;
		}
		break;
	case IDC_OPT_MKR_FEATUREBODY:
		if (notification == CBN_SELCHANGE)
			UpdateFeatureList(hPage);
		return TRUE;
	case IDC_OPT_MKR_FEATURELIST:
		if (notification == LBN_SELCHANGE)
			RescanFeatures(hPage);
		return TRUE;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

void OptionsPage_Labels::OnItemClicked(HWND hPage, WORD ctrlId)
{
	bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
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
	DWORD& mkrFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagMarkers;
	if (check) mkrFlag |= flag;
	else       mkrFlag &= ~flag;

	if (ctrlId == IDC_OPT_MKR_FEATURES)
		g_psys->ActivatePlanetLabels(mkrFlag & MKR_ENABLE && mkrFlag & MKR_LMARK);

	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

void OptionsPage_Labels::ScanPsysBodies(HWND hPage)
{
	const Body* sel = nullptr;
	for (int i = 0; i < g_psys->nPlanet(); i++) {
		Planet* planet = g_psys->GetPlanet(i);
		if (planet == g_camera->Target())
			sel = planet;
		if (planet->isMoon())
			continue;
		SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_ADDSTRING, 0, (LPARAM)planet->Name());
		for (int j = 0; j < planet->nSecondary(); j++) {
			char cbuf[256] = "    ";
			strncpy(cbuf + 4, planet->Secondary(j)->Name(), 252);
			SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_ADDSTRING, 0, (LPARAM)cbuf);
		}
	}
	if (!sel) {
		Body* tgt = g_camera->Target();
		if (tgt->Type() == OBJTP_VESSEL)
			sel = ((Vessel*)tgt)->GetSurfParam()->ref;
	}
	int idx = (sel ? SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_FINDSTRINGEXACT, -1, (LPARAM)sel->Name()) : 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_SETCURSEL, idx, 0);
	UpdateFeatureList(hPage);
}

// ----------------------------------------------------------------------

void OptionsPage_Labels::UpdateFeatureList(HWND hPage)
{
	int n, nlist;
	char cbuf[256], cpath[256];
	int idx = SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_GETLBTEXT, idx, (LPARAM)cbuf);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_RESETCONTENT, 0, 0);
	Planet* planet = g_psys->GetPlanet(trim_string(cbuf), true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST* list = planet->LabelList(&nlist);
		if (!nlist) return;
		_finddata_t fdata;
		long fh = planet->FindFirst(FILETYPE_MARKER, &fdata, cpath, cbuf);
		if (fh >= 0) {
			n = 0;
			do {
				SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_ADDSTRING, 0, (LPARAM)trim_string(cbuf));
				if (n < nlist && list[n].active)
					SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_SETSEL, TRUE, n);
				n++;
			} while (!planet->FindNext(fh, &fdata, cbuf));
			_findclose(fh);
		}
	}
	else {
		int nlabel = planet->NumLabelLegend();
		if (nlabel) {
			const oapi::GraphicsClient::LABELTYPE* lspec = planet->LabelLegend();
			for (int i = 0; i < nlabel; i++) {
				SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_ADDSTRING, 0, (LPARAM)lspec[i].name);
				if (lspec[i].active)
					SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_SETSEL, TRUE, i);
			}
		}
	}
}

// ----------------------------------------------------------------------

void OptionsPage_Labels::RescanFeatures(HWND hPage)
{
	char cbuf[256];
	int nlist;

	int idx = SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_GETCURSEL, 0, 0);
	SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATUREBODY, CB_GETLBTEXT, idx, (LPARAM)cbuf);
	Planet* planet = g_psys->GetPlanet(trim_string(cbuf), true);
	if (!planet) return;

	if (planet->LabelFormat() < 2) {
		oapi::GraphicsClient::LABELLIST* list = planet->LabelList(&nlist);
		if (!nlist) return;

		for (int i = 0; i < nlist; i++) {
			BOOL sel = SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_GETSEL, i, 0);
			list[i].active = (sel ? true : false);
		}

		std::ifstream cfg(g_pOrbiter->Cfg()->ConfigPath(planet->Name()));
		planet->ScanLabelLists(cfg);
	}
	else {
		nlist = planet->NumLabelLegend();
		for (int i = 0; i < nlist; i++) {
			BOOL sel = SendDlgItemMessage(hPage, IDC_OPT_MKR_FEATURELIST, LB_GETSEL, i, 0);
			planet->SetLabelActive(i, sel ? true : false);
		}
	}
}

// ======================================================================

OptionsPage_Forces::OptionsPage_Forces(DlgOptions* dlg)
	: OptionsPage(dlg)
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

void OptionsPage_Forces::UpdateControls(HWND hPage)
{
	std::array<int, 15> residForces = {
		IDC_OPT_VEC_WEIGHT, IDC_OPT_VEC_THRUST, IDC_OPT_VEC_LIFT, IDC_OPT_VEC_DRAG, IDC_OPT_VEC_TOTAL,
		IDC_OPT_VEC_TORQUE, IDC_OPT_VEC_LINSCL, IDC_OPT_VEC_LOGSCL, IDC_OPT_VEC_SCALE, IDC_OPT_VEC_OPACITY,
		IDC_STATIC1, IDC_STATIC2, IDC_STATIC3, IDC_STATIC4, IDC_STATIC5
	};

	DWORD vecFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagBodyForce;
	bool enable = (vecFlag & BFV_ENABLE);
	SendDlgItemMessage(hPage, IDC_OPT_VEC, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	for (auto resid : residForces)
		EnableWindow(GetDlgItem(hPage, resid), enable ? TRUE : FALSE);

	SendDlgItemMessage(hPage, IDC_OPT_VEC_WEIGHT, BM_SETCHECK, vecFlag & BFV_WEIGHT ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_THRUST, BM_SETCHECK, vecFlag & BFV_THRUST ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_LIFT,   BM_SETCHECK, vecFlag & BFV_LIFT   ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_DRAG,   BM_SETCHECK, vecFlag & BFV_DRAG   ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_TOTAL,  BM_SETCHECK, vecFlag & BFV_TOTAL  ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_TORQUE, BM_SETCHECK, vecFlag & BFV_TORQUE ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_LINSCL, BM_SETCHECK, vecFlag & BFV_LOGSCALE ? BST_UNCHECKED : BST_CHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_VEC_LOGSCL, BM_SETCHECK, vecFlag & BFV_LOGSCALE ? BST_CHECKED : BST_UNCHECKED, 0);

	int scalePos = (int)(25.0 * (1.0 + 0.5 * log(g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleBodyForce) / log(2.0)));
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_VEC_SCALE), scalePos);
	int opacPos = (int)(g_pOrbiter->Cfg()->CfgVisHelpPrm.opacBodyForce * 50.0);
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_VEC_OPACITY), opacPos);
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Forces::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
{
	GAUGEPARAM gp = { 0, 50, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_VEC_SCALE), &gp);
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_VEC_OPACITY), &gp);

	UpdateControls(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Forces::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_OPT_VEC:
	case IDC_OPT_VEC_WEIGHT:
	case IDC_OPT_VEC_THRUST:
	case IDC_OPT_VEC_LIFT:
	case IDC_OPT_VEC_DRAG:
	case IDC_OPT_VEC_TOTAL:
	case IDC_OPT_VEC_TORQUE:
	case IDC_OPT_VEC_LINSCL:
	case IDC_OPT_VEC_LOGSCL:
		if (notification == BN_CLICKED) {
			OnItemClicked(hPage, ctrlId);
			return FALSE;
		}
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

void OptionsPage_Forces::OnItemClicked(HWND hPage, WORD ctrlId)
{
	bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
	DWORD flag;
	switch (ctrlId) {
	case IDC_OPT_VEC:        flag = BFV_ENABLE;  break;
	case IDC_OPT_VEC_WEIGHT: flag = BFV_WEIGHT;  break;
	case IDC_OPT_VEC_THRUST: flag = BFV_THRUST;  break;
	case IDC_OPT_VEC_LIFT:   flag = BFV_LIFT;    break;
	case IDC_OPT_VEC_DRAG:   flag = BFV_DRAG;    break;
	case IDC_OPT_VEC_TOTAL:  flag = BFV_TOTAL;   break;
	case IDC_OPT_VEC_TORQUE: flag = BFV_TORQUE;  break;
	case IDC_OPT_VEC_LINSCL: flag = BFV_LOGSCALE; check = false; break;
	case IDC_OPT_VEC_LOGSCL: flag = BFV_LOGSCALE; check = true;  break;
	default:                 flag = 0;           break;
	}
	DWORD& vecFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagBodyForce;
	if (check) vecFlag |= flag;
	else       vecFlag &= ~flag;

	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Forces::OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam)
{
	switch (GetDlgCtrlID((HWND)lParam)) {
	case IDC_OPT_VEC_SCALE:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleBodyForce = (float)pow(2.0, (HIWORD(wParam) - 25) * 0.08);
			return 0;
		}
		break;
	case IDC_OPT_VEC_OPACITY:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			g_pOrbiter->Cfg()->CfgVisHelpPrm.opacBodyForce = (float)(HIWORD(wParam) * 0.02);
			return 0;
		}
		break;
	}
	return FALSE;
}

// ======================================================================

OptionsPage_Axes::OptionsPage_Axes(DlgOptions* dlg)
	: OptionsPage(dlg)
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

void OptionsPage_Axes::UpdateControls(HWND hPage)
{
	std::array<int, 10> residAxes = {
		IDC_OPT_CRD_VESSEL, IDC_OPT_CRD_CELBODY, IDC_OPT_CRD_BASE, IDC_OPT_CRD_NEGATIVE,
		IDC_OPT_CRD_SCALE, IDC_OPT_CRD_OPACITY,
		IDC_STATIC1, IDC_STATIC2, IDC_STATIC3, IDC_STATIC4
	};

	DWORD crdFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagFrameAxes;
	bool enable = (crdFlag & FAV_ENABLE);
	SendDlgItemMessage(hPage, IDC_OPT_CRD, BM_SETCHECK, enable ? BST_CHECKED : BST_UNCHECKED, 0);
	for (auto resid : residAxes)
		EnableWindow(GetDlgItem(hPage, resid), enable ? TRUE : FALSE);

	SendDlgItemMessage(hPage, IDC_OPT_CRD_VESSEL,   BM_SETCHECK, crdFlag & FAV_VESSEL   ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_CRD_CELBODY,  BM_SETCHECK, crdFlag & FAV_CELBODY  ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_CRD_BASE,     BM_SETCHECK, crdFlag & FAV_BASE     ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage(hPage, IDC_OPT_CRD_NEGATIVE, BM_SETCHECK, crdFlag & FAV_NEGATIVE ? BST_CHECKED : BST_UNCHECKED, 0);

	int scalePos = (int)(25.0 * (1.0 + 0.5 * log(g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleFrameAxes) / log(2.0)));
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_CRD_SCALE), scalePos);
	int opacPos = (int)(g_pOrbiter->Cfg()->CfgVisHelpPrm.opacFrameAxes * 50.0);
	oapiSetGaugePos(GetDlgItem(hPage, IDC_OPT_CRD_OPACITY), opacPos);
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Axes::OnInitDialog(HWND hPage, WPARAM wParam, LPARAM lParam)
{
	GAUGEPARAM gp = { 0, 50, GAUGEPARAM::LEFT, GAUGEPARAM::BLACK };
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_CRD_SCALE), &gp);
	oapiSetGaugeParams(GetDlgItem(hPage, IDC_OPT_CRD_OPACITY), &gp);

	UpdateControls(hPage);

	return TRUE;
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Axes::OnCommand(HWND hPage, WORD ctrlId, WORD notification, HWND hCtrl)
{
	switch (ctrlId) {
	case IDC_OPT_CRD:
	case IDC_OPT_CRD_VESSEL:
	case IDC_OPT_CRD_CELBODY:
	case IDC_OPT_CRD_BASE:
	case IDC_OPT_CRD_NEGATIVE:
		if (notification == BN_CLICKED) {
			OnItemClicked(hPage, ctrlId);
			return FALSE;
		}
		break;
	}
	return FALSE;
}

// ----------------------------------------------------------------------

void OptionsPage_Axes::OnItemClicked(HWND hPage, WORD ctrlId)
{
	bool check = (SendDlgItemMessage(hPage, ctrlId, BM_GETCHECK, 0, 0) == TRUE);
	DWORD flag;
	switch (ctrlId) {
	case IDC_OPT_CRD:          flag = FAV_ENABLE;   break;
	case IDC_OPT_CRD_VESSEL:   flag = FAV_VESSEL;   break;
	case IDC_OPT_CRD_CELBODY:  flag = FAV_CELBODY;  break;
	case IDC_OPT_CRD_BASE:     flag = FAV_BASE;     break;
	case IDC_OPT_CRD_NEGATIVE: flag = FAV_NEGATIVE; break;
	default:                   flag = 0;            break;
	}
	DWORD& crdFlag = g_pOrbiter->Cfg()->CfgVisHelpPrm.flagFrameAxes;
	if (check) crdFlag |= flag;
	else       crdFlag &= ~flag;

	UpdateControls(hPage);
}

// ----------------------------------------------------------------------

BOOL OptionsPage_Axes::OnHScroll(HWND hTab, WPARAM wParam, LPARAM lParam)
{
	switch (GetDlgCtrlID((HWND)lParam)) {
	case IDC_OPT_CRD_SCALE:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			g_pOrbiter->Cfg()->CfgVisHelpPrm.scaleFrameAxes = (float)pow(2.0, (HIWORD(wParam) - 25) * 0.08);
			return 0;
		}
		break;
	case IDC_OPT_CRD_OPACITY:
		switch (LOWORD(wParam)) {
		case SB_THUMBTRACK:
		case SB_LINELEFT:
		case SB_LINERIGHT:
			g_pOrbiter->Cfg()->CfgVisHelpPrm.opacFrameAxes = (float)(HIWORD(wParam) * 0.02);
			return 0;
		}
		break;
	}
	return FALSE;
}