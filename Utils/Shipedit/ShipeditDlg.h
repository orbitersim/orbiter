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

class CShipeditDlg : public CDialog
{
// Construction
public:
	CShipeditDlg(CShipeditApp *app, CWnd* pParent = NULL);	// standard constructor
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
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;
	CShipeditApp *m_app;

	// Generated message map functions
	//{{AFX_MSG(CShipeditDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnCalcstart();
	afx_msg void OnCalcstop();
	afx_msg void OnExit();
	afx_msg void OnClose();
	afx_msg void OnCheck();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SHIPEDITDLG_H__2089FACA_79D2_409D_A3E7_D2F94DBCF16D__INCLUDED_)
