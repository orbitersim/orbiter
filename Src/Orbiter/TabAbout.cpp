// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// AboutTab class
//=============================================================================

#include <windows.h>
#include <commctrl.h>
#include <io.h>
#include "Orbiter.h"
#include "TabAbout.h"
#include "Util.h"
#include "Help.h"
#include "resource.h"
#include "about.hpp"

//-----------------------------------------------------------------------------
// AboutTab class

orbiter::AboutTab::AboutTab (const LaunchpadDialog *lp): LaunchpadTab (lp)
{
}

//-----------------------------------------------------------------------------

bool orbiter::AboutTab::OpenHelp ()
{
	OpenTabHelp ("tab_about");
	return true;
}

//-----------------------------------------------------------------------------

void orbiter::AboutTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_ABT);

	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_NAME), NAME1);
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_BUILDDATE), SIG4);
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_CPR), SIG1B);
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_WEBADDR), SIG2 "\n" SIG5 "\n" SIG6);
	SendDlgItemMessage(hTab, IDC_ABT_LBOX_COMPONENT, LB_ADDSTRING, 0,
		(LPARAM)"D3D9Client module by Jarmo Nikkanen and Peter Schneider"
	);
	SendDlgItemMessage(hTab, IDC_ABT_LBOX_COMPONENT, LB_ADDSTRING, 0,
		(LPARAM)"XRSound module Copyright (c) Doug Beachy"
	);
}

//-----------------------------------------------------------------------------

BOOL orbiter::AboutTab::OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_ABT_WEB:
			ShellExecute (NULL, "open", "http://orbit.medphys.ucl.ac.uk/", NULL, NULL, SW_SHOWNORMAL);
			return true;
		case IDC_ABT_DISCLAIM:
			DialogBoxParam (AppInstance(), MAKEINTRESOURCE(IDD_MSG), LaunchpadWnd(), AboutProc,
				IDT_DISCLAIMER);
			return TRUE;
		case IDC_ABT_CREDIT:
			::OpenHelp(hWnd, "html\\Credit.chm", "Credit");
			return TRUE;
		}
		break;
	}
	return FALSE;
}

//-----------------------------------------------------------------------------
// Name: AboutProc()
// Desc: Minimal message proc function for the about box
//-----------------------------------------------------------------------------
INT_PTR CALLBACK orbiter::AboutTab::AboutProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		SetWindowText(GetDlgItem(hWnd, IDC_MSG),
			(char*)LockResource(LoadResource(NULL, FindResource(NULL, MAKEINTRESOURCE(lParam), "TEXT")))
		);
		return TRUE;
	case WM_COMMAND:
		if (IDOK == LOWORD(wParam) || IDCANCEL == LOWORD(wParam))
			EndDialog (hWnd, TRUE);
		return TRUE;
	}
    return FALSE;
}

