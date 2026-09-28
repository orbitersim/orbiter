// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// AboutTab class
//=============================================================================

#ifndef __linux__
#include <windows.h>
#include <commctrl.h>
#include <io.h>
#endif // !__linux__
#include "Orbiter.h"
#include "TabAbout.h"
#include "Util.h"
#include "Help.h"
#include "resource.h"
#include "about.hpp"
#ifdef __linux__
#include "ResDialog.h"
#include <QDesktopServices>
#include <QDialog>
#include <QLabel>
#include <QListWidget>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QUrl>
#endif // __linux__

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

#ifndef __linux__
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_NAME), NAME1);
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_BUILDDATE), SIG4);
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_CPR), SIG1B);
	SetWindowText (GetDlgItem (hTab, IDC_ABT_TXT_WEBADDR), SIG2 "\n" SIG5 "\n" SIG6);
	SendDlgItemMessage(hTab, IDC_ABT_LBOX_COMPONENT, LB_ADDSTRING, 0,
		(LPARAM)"D3D9Client module by Jarmo Nikkanen and Peter Schneider"
	);
#else // __linux__
	DlgItem<QLabel> (hTab, IDC_ABT_TXT_NAME)->setText (NAME1);
	DlgItem<QLabel> (hTab, IDC_ABT_TXT_BUILDDATE)->setText (SIG4);
	DlgItem<QLabel> (hTab, IDC_ABT_TXT_CPR)->setText (SIG1B);
	DlgItem<QLabel> (hTab, IDC_ABT_TXT_WEBADDR)->setText (SIG2 "\n" SIG5 "\n" SIG6);
	DlgItem<QListWidget> (hTab, IDC_ABT_LBOX_COMPONENT)->addItem (
		"D3D9Client module by Jarmo Nikkanen and Peter Schneider"
	);
#endif // __linux__
#ifndef __linux__
	SendDlgItemMessage(hTab, IDC_ABT_LBOX_COMPONENT, LB_ADDSTRING, 0,
		(LPARAM)"XRSound module Copyright (c) Doug Beachy"
	);
#else // __linux__
	DlgItem<QListWidget> (hTab, IDC_ABT_LBOX_COMPONENT)->addItem (
		"XRSound module Copyright (c) Doug Beachy"
	);
#endif // __linux__
}

//-----------------------------------------------------------------------------

#ifndef __linux__
BOOL orbiter::AboutTab::OnMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
BOOL orbiter::AboutTab::OnInitDialog(QWidget *hWnd)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_COMMAND
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_ABT_WEB), &QPushButton::clicked, hWnd, []() {
		QDesktopServices::openUrl (QUrl ("http://orbit.medphys.ucl.ac.uk/"));
	});
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_ABT_DISCLAIM), &QPushButton::clicked, hWnd, [this]() {
		QDialog *dlg = qobject_cast<QDialog*> (oapiCreateResDialog (AppInstance(), IDD_MSG, LaunchpadWnd()));
		if (!dlg) return;
		AboutProc (dlg, IDT_DISCLAIMER);
		dlg->exec(); // DialogBoxParam
		delete dlg;
	});
	QObject::connect (DlgItem<QPushButton> (hWnd, IDC_ABT_CREDIT), &QPushButton::clicked, hWnd, [hWnd]() {
		::OpenHelp(hWnd, "html\\Credit.chm", "Credit");
	});
#endif // __linux__
	return FALSE;
}

//-----------------------------------------------------------------------------
// Name: AboutProc()
#ifndef __linux__
// Desc: Minimal message proc function for the about box
#else // __linux__
// Desc: Minimal set-up function for the about box
#endif // __linux__
//-----------------------------------------------------------------------------
#ifndef __linux__
INT_PTR CALLBACK orbiter::AboutTab::AboutProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
void orbiter::AboutTab::AboutProc (QWidget *hWnd, int textId)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	// WM_INITDIALOG
	const RESDATA *txt = oapiFindResData (NULL, "TEXT", textId);
	if (txt)
		DlgItem<QPlainTextEdit> (hWnd, IDC_MSG)->setPlainText (QString::fromLatin1 ((const char*)txt->data, txt->size));
	// WM_COMMAND
	QDialog *dlg = qobject_cast<QDialog*> (hWnd);
	QObject::connect (DlgItem<QPushButton> (hWnd, IDOK), &QPushButton::clicked, dlg, &QDialog::accept);
#endif // __linux__
}

