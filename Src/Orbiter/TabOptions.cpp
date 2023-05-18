// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// OptionsTab class
//=============================================================================

#include "TabOptions.h"
#include "Help.h"
#include "resource.h"

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

BOOL orbiter::OptionsTab::OnInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	SetWindowHandles(hWnd, GetDlgItem(hWnd, IDC_OPT_SPLIT), GetDlgItem(hWnd, IDC_OPT_PAGELIST), GetDlgItem(hWnd, IDC_OPT_PAGECONTAINER));
	CreatePages();
	ExpandAll();
	return TRUE;
}

//-----------------------------------------------------------------------------

BOOL orbiter::OptionsTab::OnSize(int w, int h)
{
	SetWindowPos(GetDlgItem(hTab, IDC_OPT_SPLIT), HWND_BOTTOM, 0, 0, w, h,
		SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOOWNERZORDER);

	return FALSE;
}

// ----------------------------------------------------------------------

BOOL orbiter::OptionsTab::OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh)
{
	if (idCtrl == IDC_OPT_PAGELIST) {
		OnNotifyPagelist(pnmh);
		return TRUE;
	}
	return FALSE;
}
