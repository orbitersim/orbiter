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
#include "cryptstring.h"

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

	char cbuf[256];
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_NAME), uscram(NAME1));
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_BUILDDATE), uscram(SIG4));
	strcpy (cbuf, uscram(SIG1B));
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_CPR), cbuf);
	strcpy (cbuf, uscram(SIG2));
	strcat (cbuf, "\r\n");
	strcat (cbuf, uscram(SIG5));
	strcat (cbuf, "\r\n");
	strcat (cbuf, uscram(SIG6));
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_WEBADDR), cbuf);
	strcpy(cbuf, "XRSound module Copyright (c) Doug Beachy");
	SendDlgItemMessage(hTab, IDC_ABT_LBOX_COMPONENT, LB_ADDSTRING, 0, (LPARAM)cbuf);

	static int item[] = {
		IDC_ABT_GRP_ORBITER, IDC_ABT_GRP_WEB, IDC_ABT_ICON_DG, IDC_ABT_TXT_NAME,
		IDC_ABT_TXT_WEB, IDC_ABT_TXT_WEBADDR, IDC_ABT_TXT_CPR, IDC_ABT_TXT_LICENSE,
		IDC_ABT_GRP_COMPONENT, IDC_ABT_WEB, IDC_ABT_DISCLAIM, IDC_ABT_CREDIT,
		IDC_ABT_TXT_BUILDDATE, IDC_ABT_LBOX_COMPONENT
	};

	RegisterItemPositions (item, 14);
}

//-----------------------------------------------------------------------------

INT_PTR orbiter::AboutTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
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
		SetWindowText (GetDlgItem (hWnd, IDC_MSG),
			uscram((char*)LockResource (LoadResource (NULL,
			FindResource (NULL, MAKEINTRESOURCE(lParam), "TEXT")))));
		return TRUE;
	case WM_COMMAND:
		if (IDOK == LOWORD(wParam) || IDCANCEL == LOWORD(wParam))
			EndDialog (hWnd, TRUE);
		return TRUE;
	}
    return FALSE;
}

