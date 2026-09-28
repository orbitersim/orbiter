// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ShipeditDlg.h : header file
//

#if !defined(AFX_SHIPEDITDLG_H__2089FACA_79D2_409D_A3E7_D2F94DBCF16D__INCLUDED_)
#define AFX_SHIPEDITDLG_H__2089FACA_79D2_409D_A3E7_D2F94DBCF16D__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
// CShipeditDlg dialog

#ifndef __linux__
class CShipeditDlg : public CDialog
#else // __linux__
class CShipeditDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	CShipeditDlg(CShipeditApp *app, CWnd* pParent = NULL);	// standard constructor
#else // __linux__
	CShipeditDlg(CShipeditApp *app, QWidget* pParent = NULL);	// standard constructor
#endif // __linux__
	void Refresh ();
	void RefreshCalc ();

// Dialog Data
	//{{AFX_DATA(CShipeditDlg)
	enum { IDD = IDD_SHIPEDIT_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CShipeditDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);	// DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
#ifndef __linux__
	HICON m_hIcon;
#else // __linux__
	QIcon m_hIcon;
#endif // __linux__
	CShipeditApp *m_app;

	// Generated message map functions
	//{{AFX_MSG(CShipeditDlg)
	virtual BOOL OnInitDialog();
#ifndef __linux__
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnCalcstart();
	afx_msg void OnCalcstop();
	afx_msg void OnExit();
	afx_msg void OnClose();
	afx_msg void OnCheck();
#else // __linux__
	void OnSysCommand(UINT nID, LPARAM lParam);
	// OnPaint, OnQueryDragIcon left out: the window manager draws the minimised window's icon (setWindowIcon)
	void OnCalcstart();
	void OnCalcstop();
	void OnExit();
	virtual void OnClose();
	void OnCheck();
#endif // __linux__
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	virtual BOOL OnCommand(int nID, int nCode); // DECLARE_MESSAGE_MAP: the map is a WM_COMMAND switch
#endif // __linux__
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SHIPEDITDLG_H__2089FACA_79D2_409D_A3E7_D2F94DBCF16D__INCLUDED_)
