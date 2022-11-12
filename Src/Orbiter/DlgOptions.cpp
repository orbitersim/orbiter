// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// In-session options dialog
// ======================================================================

#define STRICT 1

#include "DlgOptions.h"
#include "Orbiter.h"
#include "DlgCtrl.h"
#include "resource.h"
#include "Uxtheme.h"
#include <commctrl.h>

extern Orbiter* g_pOrbiter;

// ======================================================================

DlgOptions::DlgOptions(HINSTANCE hInstance, HWND hParent, void* context)
	: DialogWin(hInstance, hParent, IDD_OPTIONS, 0, 0, context)
{
	pos = &g_pOrbiter->Cfg()->CfgWindowPos.DlgOptions;
	m_pageIdx = 0;
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
	m_splitter.SetStaticPane(SplitterCtrl::PANE1, 100);
	SetSize(hDlg);

	AddPage(hDlg, new OptionsPage_CelSphere(m_container.HWnd()));
	SwitchPage(hDlg, 0);

	return TRUE;
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

void DlgOptions::SwitchPage(HWND hDlg, size_t page)
{
	if (page < 0 || page >= m_pPage.size())
		return;
	m_pageIdx = page;
	for (size_t pg = 0; pg < m_pPage.size(); pg++)
		if (pg != m_pageIdx) m_pPage[pg]->Show(false);
	m_pPage[m_pageIdx]->Show(true);
	m_vScrollPos = 0;
	m_vScrollRange = 0;
	m_vScrollPage = 0;

	SetPageSize(hDlg);
}

// ----------------------------------------------------------------------

void DlgOptions::Clear()
{
	for (auto pPage : m_pPage)
		delete pPage;
	m_pPage.clear();
}

// ======================================================================

OptionsPage::OptionsPage(HWND hParent)
	: m_hParent(hParent)
	, m_hPage(0)
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

HTREEITEM OptionsPage::CreatePage(HWND hDlg, HTREEITEM parent)
{
	int winId = ResourceId();
	m_hPage = CreateDialogParam(g_pOrbiter->GetInstance(), MAKEINTRESOURCE(winId), m_hParent, s_DlgProc, (LPARAM)this);

	char cbuf[256];
	strcpy(cbuf, Name().c_str());
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

OptionsPage_CelSphere::OptionsPage_CelSphere(HWND hParent)
	: OptionsPage(hParent)
{
}

// ----------------------------------------------------------------------

int OptionsPage_CelSphere::ResourceId() const
{
	return IDD_OPTIONS_CELSPHERE;
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
