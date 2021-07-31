// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// AboutTab class
//=============================================================================

#include <windows.h>
#include <commctrl.h>
#include <io.h>
#include "Orbiter.h"
#include "Launchpad.h"
#include "TabAbout.h"
#include "Util.h"
#include "Help.h"
#include "resource.h"
#include "cryptstring.h"

//-----------------------------------------------------------------------------
// AboutTab class

AboutTab::AboutTab (const MainDialog *lp): LaunchpadTab (lp)
{
}

//-----------------------------------------------------------------------------

bool AboutTab::OpenHelp ()
{
	OpenDefaultHelp (pLp->GetWindow(), pLp->GetInstance(), "tab_about");
	return true;
}

//-----------------------------------------------------------------------------

void AboutTab::Create ()
{
	hTab = CreateTab (IDD_PAGE_ABT);

	char cbuf[256];
	SetWindowText (GetDlgItem (hTab, IDC_ABT_STATIC4), uscram(NAME1));
	SetWindowText (GetDlgItem (hTab, IDC_BUILDDATE), uscram(SIG4));
	strcpy (cbuf, uscram(SIG1B));
	SetWindowText (GetDlgItem (hTab, IDC_ABT_STATIC7), cbuf);
	strcpy (cbuf, uscram(SIG2));
	strcat (cbuf, "\r\n\r\n");
	strcat (cbuf, uscram(SIG5));
	strcat (cbuf, "\r\n\r\n");
	strcat (cbuf, uscram(SIG6));
	SetWindowText (GetDlgItem (hTab, IDC_ABT_STATIC6), cbuf);

	static int item[] = {
		IDC_ABT_STATIC1, IDC_ABT_STATIC2, IDC_ABT_STATIC3, IDC_ABT_STATIC4,
		IDC_ABT_STATIC5, IDC_ABT_STATIC6, IDC_ABT_STATIC7, IDC_ABT_STATIC8,
		IDC_ABT_WEB, IDC_ABT_DISCLAIM, IDC_ABT_CREDIT, IDC_BUILDDATE
	};

	RegisterItemPositions (item, 12);
}

//-----------------------------------------------------------------------------

BOOL AboutTab::TabProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_ABT_WEB:
			ShellExecute (NULL, "open", "http://orbit.medphys.ucl.ac.uk/", NULL, NULL, SW_SHOWNORMAL);
			return true;
		case IDC_ABT_DISCLAIM:
			DialogBoxParam (pLp->GetInstance(), MAKEINTRESOURCE(IDD_MSG), pLp->GetWindow(), AboutProc,
				IDT_DISCLAIMER);
			return TRUE;
		case IDC_ABT_CREDIT:
			OpenCredits (hWnd, pLp->GetInstance());
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
INT_PTR CALLBACK AboutTab::AboutProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
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

