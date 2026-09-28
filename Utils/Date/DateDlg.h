// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// DateDlg.h : header file
//

#if !defined(AFX_DATEDLG_H__A0DC46B1_90BB_4A9A_A1D4_5DD52D2CED37__INCLUDED_)
#define AFX_DATEDLG_H__A0DC46B1_90BB_4A9A_A1D4_5DD52D2CED37__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CDateDlg dialog

#ifndef __linux__
class CDateDlg : public CDialog
#else // __linux__
class CDateDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	CDateDlg(CWnd* pParent = NULL);	// standard constructor
#else // __linux__
	CDateDlg(QWidget* pParent = NULL);	// standard constructor
#endif // __linux__
	void UpdateUT (void);
	void UpdateMJD (void);
	void UpdateJD (void);
	void UpdateJC (void);
	void UpdateEpoch (void);
	void SetMJD (double new_mjd, bool reset_mjd = false);
	void SetUT (struct tm *new_date, bool reset_ut = false);
	void SetJD (double new_jd, bool reset_jd = false);
	void SetJC (double new_jc, bool reset_jc = false);
	void SetEpoch (double new_epoch, bool reset_epoch = false);

// Dialog Data
	//{{AFX_DATA(CDateDlg)
	enum { IDD = IDD_DATE_DIALOG };
#ifndef __linux__
	CString	m_MJD;
#else // __linux__
	std::string	m_MJD;
#endif // __linux__
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDateDlg)
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
	double mjd;
	struct tm date;

	// Generated message map functions
	//{{AFX_MSG(CDateDlg)
	virtual BOOL OnInitDialog();
#ifndef __linux__
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnChangeMjd();
	afx_msg void OnChangeUtDay();
	afx_msg void OnChangeUtMonth();
	afx_msg void OnChangeUtYear();
	afx_msg void OnChangeUtHour();
	afx_msg void OnChangeUtMin();
	afx_msg void OnChangeUtSec();
	afx_msg void OnChangeJd();
	afx_msg void OnChangeJc();
	afx_msg void OnChangeEpoch();
#else // __linux__
	void OnSysCommand(UINT nID, LPARAM lParam);
	// OnPaint, OnQueryDragIcon left out: the window manager draws the minimised window's icon (setWindowIcon)
	void OnChangeMjd();
	void OnChangeUtDay();
	void OnChangeUtMonth();
	void OnChangeUtYear();
	void OnChangeUtHour();
	void OnChangeUtMin();
	void OnChangeUtSec();
	void OnChangeJd();
	void OnChangeJc();
	void OnChangeEpoch();
#endif // __linux__
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	virtual BOOL OnCommand(int nID, int nCode); // DECLARE_MESSAGE_MAP: the map is a WM_COMMAND switch
#endif // __linux__
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DATEDLG_H__A0DC46B1_90BB_4A9A_A1D4_5DD52D2CED37__INCLUDED_)
