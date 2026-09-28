// stdafx.cpp : source file that includes just the standard includes
//	Date.pch will be the pre-compiled header
//	stdafx.obj will contain the pre-compiled type information

#ifndef __linux__
#include "stdafx.h"
#else // __linux__
#include "StdAfx.h"
#include <QKeyEvent>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QScreen>
#include <cstdio>
#endif // __linux__

#ifdef __linux__
// not upstream: ResDlg (see StdAfx.h)
#endif // __linux__

#ifdef __linux__
QPointer<QWidget> ResDlg::hMainWnd;
#endif // __linux__

#ifdef __linux__
struct ExchangeFail {}; // CUserException thrown by CDataExchange::Fail

ResDlg::ResDlg (UINT nIDTemplate, QWidget *pParentWnd)
: m_nIDTemplate (nIDTemplate), m_pParentWnd (pParentWnd)
{}

ResDlg::~ResDlg ()
{
	delete hDlg;
}

QIcon ResDlg::LoadIcon (int nIDResource)
{
	QIcon icon;
	if (QImage *img = oapiLoadResImage (nullptr, nIDResource)) {
		icon = QIcon (QPixmap::fromImage (*img));
		delete img;
	}
	return icon;
}

BOOL ResDlg::Setup ()
{
	hDlg = qobject_cast<QDialog*> (oapiCreateResDialog (nullptr, m_nIDTemplate, m_pParentWnd ? m_pParentWnd : hMainWnd.data()));
	if (!hDlg) return FALSE;
	if (!hMainWnd) hMainWnd = hDlg;
	hDlg->hide (); // WS_VISIBLE: shown after WM_INITDIALOG
	oapiConnectDlgCommands (hDlg, [this](int id, int code, QWidget*) { if (!bLockout) OnCommand (id, code); });
	new EventHook (hDlg, [this](QObject *obj, QEvent *event) { return obj == hDlg && DlgEvent (event); });
	OnInitDialog ();
	// MFC centres the dialog on its owner or on the screen (AfxPostInitDialog)
	QWidget *owner = hDlg->parentWidget ();
	QRect area = (owner ? owner->window ()->frameGeometry () : hDlg->screen ()->availableGeometry ());
	hDlg->move (area.center () - hDlg->rect ().center ());
	return TRUE;
}

int ResDlg::DoModal ()
{
	if (!Setup ()) return -1;
	int nResult = hDlg->exec ();
	delete hDlg;
	return (nResult ? nResult : IDCANCEL); // 0: QDialog::reject
}

// Esc is IDCANCEL, Enter IDOK unless a default button takes it (IsDialogMessage); WM_CLOSE is IDCANCEL
bool ResDlg::DlgEvent (QEvent *event)
{
	if (event->type () == QEvent::KeyPress) {
		int key = static_cast<QKeyEvent*> (event)->key ();
		if (key == Qt::Key_Escape) {
			OnCommand (IDCANCEL, RESN_CLICKED);
			return true;
		}
		if (key == Qt::Key_Return || key == Qt::Key_Enter) {
			for (QPushButton *b : hDlg->findChildren<QPushButton*> ())
				if (b->isDefault () && b->isVisible ()) return false;
			OnCommand (IDOK, RESN_CLICKED);
			return true;
		}
	} else if (event->type () == QEvent::Close) {
		OnCommand (IDCANCEL, RESN_CLICKED);
		return true;
	}
	return false;
}

BOOL ResDlg::UpdateData (BOOL bSaveAndValidate)
{
	bool bOldLockout = bLockout;
	bLockout = true; // m_hLockoutNotifyWindow
	BOOL bOK = TRUE;
	try {
		DoDataExchange (bSaveAndValidate);
	} catch (const ExchangeFail&) {
		bOK = FALSE;
	}
	bLockout = bOldLockout;
	return bOK;
}

BOOL ResDlg::OnInitDialog ()
{
	UpdateData (FALSE);
	return TRUE;
}

BOOL ResDlg::OnCommand (int nID, int nCode)
{
	if (nCode != RESN_CLICKED) return FALSE;
	switch (nID) {
	case IDOK:     OnOK ();     return TRUE;
	case IDCANCEL: OnCancel (); return TRUE;
	}
	return FALSE;
}

void ResDlg::OnOK ()
{
	if (!UpdateData (TRUE)) return;
	EndDialog (IDOK);
}

void ResDlg::OnCancel ()
{
	EndDialog (IDCANCEL);
}

void ResDlg::EndDialog (int nResult)
{
	if (hDlg) hDlg->done (nResult);
}

void ResDlg::Fail (const char *msg)
{
	QMessageBox::warning (hDlg, QCoreApplication::applicationName (), msg); // AfxMessageBox, MB_ICONEXCLAMATION
	if (QWidget *w = GetDlgItem (idLastControl)) {
		w->setFocus ();
		if (QLineEdit *e = qobject_cast<QLineEdit*> (w)) e->selectAll ();
	}
	throw ExchangeFail ();
}

void ResDlg::ExchangeText (BOOL bSaveAndValidate, int nID, std::string &value)
{
	idLastControl = nID;
	if (bSaveAndValidate) {
		char cbuf[4096];
		oapiGetDlgText (GetDlgItem (nID), cbuf, 4096);
		value = cbuf;
	} else {
		oapiSetDlgText (GetDlgItem (nID), value.c_str ());
	}
}

void ResDlg::ValidateMaxChars (BOOL bSaveAndValidate, const std::string &value, int nChars)
{
	if (bSaveAndValidate) {
		if (QString::fromUtf8 (value).size () > nChars) {
			char msg[256];
			snprintf (msg, 256, "Please enter no more than %d characters.", nChars);
			Fail (msg);
		}
	} else if (QLineEdit *e = qobject_cast<QLineEdit*> (GetDlgItem (idLastControl))) {
		e->setMaxLength (nChars); // EM_LIMITTEXT
	}
}
#endif // __linux__
