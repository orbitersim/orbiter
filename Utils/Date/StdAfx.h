// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__2749A3D2_C3AC_49E9_94A0_4873DA6265FB__INCLUDED_)
#define AFX_STDAFX_H__2749A3D2_C3AC_49E9_94A0_4873DA6265FB__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#ifndef __linux__
#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdtctl.h>		// MFC support for Internet Explorer 4 Common Controls
#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows Common Controls
#endif // _AFX_NO_AFXCMN_SUPPORT
#else // __linux__
// afxwin.h, afxext.h, afxdtctl.h, afxcmn.h left out: MFC is replaced by Qt 6 Widgets and ResDialog (.rc templates)
#include <QApplication>
#include <QDialog>
#include <QIcon>
#include <QPointer>
#include <string>
#include "ResDialog.h"
#endif // __linux__

#ifdef __linux__
// not upstream: the part of MFC's CDialog this app uses, on the Qt dialog ResDialog builds from the .rc template
class ResDlg {
public:
	ResDlg (UINT nIDTemplate, QWidget *pParentWnd = nullptr);
	virtual ~ResDlg ();
	int DoModal ();                                  // QDialog::exec: IDOK or IDCANCEL, -1 if the template is missing
	BOOL UpdateData (BOOL bSaveAndValidate = TRUE);  // DoDataExchange, control notifications locked out meanwhile
	QWidget *GetDlgItem (int nID) const { return oapiResDlgItem (hDlg, nID); }
	static QIcon LoadIcon (int nIDResource);         // CWinApp::LoadIcon
	QPointer<QDialog> hDlg;                          // m_hWnd
	static QPointer<QWidget> hMainWnd;               // AfxGetMainWnd: owner of dialogs opened without a parent

protected:
	virtual BOOL OnInitDialog ();                    // WM_INITDIALOG: UpdateData (FALSE)
	virtual void DoDataExchange (BOOL bSaveAndValidate) {}
	virtual BOOL OnCommand (int nID, int nCode);     // WM_COMMAND: message map entries, IDOK/IDCANCEL
	virtual void OnOK ();                            // UpdateData, then EndDialog (IDOK)
	virtual void OnCancel ();                        // EndDialog (IDCANCEL)
	void EndDialog (int nResult);
	// DDX/DDV counterparts: set the control (bSaveAndValidate FALSE) or read and check it; a bad entry fails UpdateData
	void ExchangeText (BOOL bSaveAndValidate, int nID, std::string &value);
	void ValidateMaxChars (BOOL bSaveAndValidate, const std::string &value, int nChars);
	UINT m_nIDTemplate;
	QWidget *m_pParentWnd;

private:
	BOOL Setup ();                                   // builds and connects the dialog, WM_INITDIALOG
	bool DlgEvent (QEvent *event);                   // IsDialogMessage (Enter, Esc) and WM_CLOSE
	void Fail (const char *msg);                     // CDataExchange::Fail
	bool bLockout = false;
	int idLastControl = 0;
};
#endif // __linux__

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__2749A3D2_C3AC_49E9_94A0_4873DA6265FB__INCLUDED_)
