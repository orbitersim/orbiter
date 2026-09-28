// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// OptionsTab class
//=============================================================================

#include "TabOptions.h"
#include "Help.h"
#include "resource.h"
#ifdef __linux__
#include "ResDialog.h"
#endif // __linux__

//=============================================================================

orbiter::OptionsTab::OptionsTab(const LaunchpadDialog* lp)
	: LaunchpadTab(lp)
	, OptionsPageContainer(OptionsPageContainer::LAUNCHPAD, lp->Cfg())
{
}

//-----------------------------------------------------------------------------

void orbiter::OptionsTab::Create()
{
	hTab = CreateTab(IDD_PAGE_OPT);
}

//-----------------------------------------------------------------------------

bool orbiter::OptionsTab::OpenHelp()
{
	const HELPCONTEXT* hc = (CurrentPage() ? CurrentPage()->HelpContext() : nullptr);
	if (hc) ::OpenHelp(LaunchpadWnd(), hc->helpfile, hc->topic);
	return true;
}

//-----------------------------------------------------------------------------

void orbiter::OptionsTab::LaunchpadShowing(bool show)
{
	if (show) UpdatePages(true);
}

// ----------------------------------------------------------------------

void orbiter::OptionsTab::SetConfig(Config* cfg)
{
	UpdateConfig();
}

//-----------------------------------------------------------------------------

#ifndef __linux__
BOOL orbiter::OptionsTab::OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL orbiter::OptionsTab::OnInitDialog(QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
	SetWindowHandles(hWnd, GetDlgItem(hWnd, IDC_OPT_SPLIT), GetDlgItem(hWnd, IDC_OPT_PAGELIST), GetDlgItem(hWnd, IDC_OPT_PAGECONTAINER));
#else // __linux__
	SetWindowHandles(hWnd, oapiResDlgItem(hWnd, IDC_OPT_SPLIT), oapiResDlgItem(hWnd, IDC_OPT_PAGELIST), oapiResDlgItem(hWnd, IDC_OPT_PAGECONTAINER));
#endif // __linux__
	CreatePages();
	ExpandAll();
	return TRUE;
}

//-----------------------------------------------------------------------------

BOOL orbiter::OptionsTab::OnSize(int w, int h)
{
#ifndef __linux__
	SetWindowPos(GetDlgItem(hTab, IDC_OPT_SPLIT), HWND_BOTTOM, 0, 0, w, h,
		SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER);
#else // __linux__
	QWidget *split = oapiResDlgItem(hTab, IDC_OPT_SPLIT);
	split->lower(); // HWND_BOTTOM
	split->resize(w, h);
#endif // __linux__

	return FALSE;
}

#ifndef __linux__
// ----------------------------------------------------------------------

BOOL orbiter::OptionsTab::OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh)
{
	if (idCtrl == IDC_OPT_PAGELIST) {
		OnNotifyPagelist(pnmh);
		return TRUE;
	}
	return FALSE;
}
#else // __linux__
// WM_NOTIFY of the page list is connected in SetWindowHandles
#endif // __linux__
