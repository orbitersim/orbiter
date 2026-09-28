// stdafx.cpp : source file that includes just the standard includes
//	Shipedit.pch will be the pre-compiled header
//	stdafx.obj will contain the pre-compiled type information

#ifndef __linux__
#include "stdafx.h"
#else // __linux__
#include "StdAfx.h"
#include <QButtonGroup>
#include <QCheckBox>
#include <QKeyEvent>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QScreen>
#include <cfloat>
#include <climits>
#include <cstdio>
#include <cstdlib>
#endif // __linux__

#ifdef __linux__
// not upstream: ResDlg (see StdAfx.h)

QPointer<QWidget> ResDlg::hMainWnd;

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

BOOL ResDlg::Setup (bool &bVisible)
{
	hDlg = qobject_cast<QDialog*> (oapiCreateResDialog (nullptr, m_nIDTemplate, m_pParentWnd ? m_pParentWnd : hMainWnd.data()));
	if (!hDlg) return FALSE;
	if (!hMainWnd) hMainWnd = hDlg;
	bVisible = hDlg->isVisible ();
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
	bool bVisible;
	if (!Setup (bVisible)) return -1;
	int nResult = hDlg->exec ();
	delete hDlg;
	return (nResult ? nResult : IDCANCEL); // 0: QDialog::reject
}

BOOL ResDlg::Create (UINT nIDTemplate, QWidget *pParentWnd)
{
	bool bVisible;
	m_nIDTemplate = nIDTemplate;
	m_pParentWnd = pParentWnd;
	if (!Setup (bVisible)) return FALSE;
	if (bVisible) hDlg->show ();
	return TRUE;
}

void ResDlg::DestroyWindow ()
{
	if (QDialog *dlg = hDlg) {
		hDlg = nullptr;
		dlg->hide ();
		dlg->deleteLater ();
	}
}

// Esc is IDCANCEL, Enter IDOK unless a default button takes it (IsDialogMessage); WM_CLOSE goes to OnClose
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
		OnClose ();
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

void ResDlg::OnClose ()
{
	OnCommand (IDCANCEL, RESN_CLICKED);
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

std::string ResDlg::GetText (int nID)
{
	char cbuf[4096];
	oapiGetDlgText (GetDlgItem (nID), cbuf, 4096);
	return cbuf;
}

// _AfxSimpleScanf: blanks around one number; Windows long is 32 bit, so out-of-range values saturate
void ResDlg::ExchangeText (BOOL bSaveAndValidate, int nID, int &value)
{
	idLastControl = nID;
	if (bSaveAndValidate) {
		std::string s = GetText (nID);
		const char *c = s.c_str ();
		char *end;
		while (*c == ' ' || *c == '\t') c++;
		char chFirst = c[0];
		long l = strtol (c, &end, 10);
		while (*end == ' ' || *end == '\t') end++;
		if ((l == 0 && chFirst != '0') || *end) Fail ("Please enter an integer.");
		value = (int)(l < INT_MIN ? INT_MIN : l > INT_MAX ? INT_MAX : l);
	} else {
		char cbuf[32];
		snprintf (cbuf, 32, "%d", value);
		oapiSetDlgText (GetDlgItem (nID), cbuf);
	}
}

void ResDlg::ExchangeText (BOOL bSaveAndValidate, int nID, UINT &value)
{
	idLastControl = nID;
	if (bSaveAndValidate) {
		std::string s = GetText (nID);
		const char *c = s.c_str ();
		char *end;
		while (*c == ' ' || *c == '\t') c++;
		char chFirst = c[0];
		if (chFirst == '-') Fail ("Please enter a positive integer.");
		unsigned long l = strtoul (c, &end, 10);
		while (*end == ' ' || *end == '\t') end++;
		if ((l == 0 && chFirst != '0') || *end) Fail ("Please enter a positive integer.");
		value = (UINT)(l > UINT_MAX ? UINT_MAX : l);
	} else {
		char cbuf[32];
		snprintf (cbuf, 32, "%u", value);
		oapiSetDlgText (GetDlgItem (nID), cbuf);
	}
}

// _AfxSimpleFloatParse; written with FLT_DIG/DBL_DIG significant digits
static bool FloatParse (const char *c, double &d)
{
	char *end;
	while (*c == ' ' || *c == '\t') c++;
	char chFirst = c[0];
	d = strtod (c, &end);
	if (d == 0.0 && chFirst != '0') return false;
	while (*end == ' ' || *end == '\t') end++;
	return !*end;
}

void ResDlg::ExchangeText (BOOL bSaveAndValidate, int nID, float &value)
{
	idLastControl = nID;
	if (bSaveAndValidate) {
		double d;
		if (!FloatParse (GetText (nID).c_str (), d) || d < -FLT_MAX || d > FLT_MAX) Fail ("Please enter a number.");
		value = (float)d;
	} else {
		char cbuf[64];
		snprintf (cbuf, 64, "%.*g", FLT_DIG, value);
		oapiSetDlgText (GetDlgItem (nID), cbuf);
	}
}

void ResDlg::ExchangeText (BOOL bSaveAndValidate, int nID, double &value)
{
	idLastControl = nID;
	if (bSaveAndValidate) {
		double d;
		if (!FloatParse (GetText (nID).c_str (), d)) Fail ("Please enter a number.");
		value = d;
	} else {
		char cbuf[64];
		snprintf (cbuf, 64, "%.*g", DBL_DIG, value);
		oapiSetDlgText (GetDlgItem (nID), cbuf);
	}
}

void ResDlg::ExchangeText (BOOL bSaveAndValidate, int nID, std::string &value)
{
	idLastControl = nID;
	if (bSaveAndValidate) value = GetText (nID);
	else oapiSetDlgText (GetDlgItem (nID), value.c_str ());
}

// BM_GETCHECK/BM_SETCHECK: 0 unchecked, 1 checked, 2 indeterminate
void ResDlg::ExchangeCheck (BOOL bSaveAndValidate, int nID, BOOL &value)
{
	idLastControl = nID;
	QCheckBox *cb = DlgItem<QCheckBox> (hDlg, nID);
	if (!cb) return;
	if (bSaveAndValidate) {
		Qt::CheckState s = cb->checkState ();
		value = (s == Qt::Checked ? 1 : s == Qt::PartiallyChecked ? 2 : 0);
	} else {
		if (value < 0 || value > 2) value = 0;
		cb->setCheckState (value == 1 ? Qt::Checked : value == 2 ? Qt::PartiallyChecked : Qt::Unchecked);
	}
}

// index of the checked button in the radio group that starts at nID (-1: none)
void ResDlg::ExchangeRadio (BOOL bSaveAndValidate, int nID, int &value)
{
	idLastControl = nID;
	QAbstractButton *first = DlgItem<QAbstractButton> (hDlg, nID);
	QButtonGroup *group = (first ? first->group () : nullptr);
	if (bSaveAndValidate) value = -1;
	if (!group) return;
	QList<QAbstractButton*> bt = group->buttons ();
	bt = bt.mid (bt.indexOf (first));
	if (bSaveAndValidate) {
		for (int i = 0; i < bt.size (); i++)
			if (bt[i]->isChecked ()) value = i;
	} else {
		group->setExclusive (false);
		for (int i = 0; i < bt.size (); i++)
			bt[i]->setChecked (i == value);
		group->setExclusive (true);
	}
}

// DDV_MinMax*: checked when reading only
void ResDlg::ValidateMinMaxInt (BOOL bSaveAndValidate, int value, int minVal, int maxVal)
{
	if (!bSaveAndValidate || (value >= minVal && value <= maxVal)) return;
	char msg[256];
	snprintf (msg, 256, "Please enter an integer between %d and %d.", minVal, maxVal);
	Fail (msg);
}

void ResDlg::ValidateMinMaxUInt (BOOL bSaveAndValidate, UINT value, UINT minVal, UINT maxVal)
{
	if (!bSaveAndValidate || (value >= minVal && value <= maxVal)) return;
	char msg[256];
	snprintf (msg, 256, "Please enter an integer between %u and %u.", minVal, maxVal);
	Fail (msg);
}

void ResDlg::ValidateMinMaxFloat (BOOL bSaveAndValidate, float value, float minVal, float maxVal)
{
	if (!bSaveAndValidate || (value >= minVal && value <= maxVal)) return;
	char msg[256];
	snprintf (msg, 256, "Please enter a number between %.*g and %.*g.", FLT_DIG, minVal, FLT_DIG, maxVal);
	Fail (msg);
}

void ResDlg::ValidateMinMaxDouble (BOOL bSaveAndValidate, double value, double minVal, double maxVal)
{
	if (!bSaveAndValidate || (value >= minVal && value <= maxVal)) return;
	char msg[256];
	snprintf (msg, 256, "Please enter a number between %.*g and %.*g.", DBL_DIG, minVal, DBL_DIG, maxVal);
	Fail (msg);
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
