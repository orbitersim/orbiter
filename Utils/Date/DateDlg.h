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

class CDateDlg : public CDialog
{
// Construction
public:
	CDateDlg(CWnd* pParent = NULL);	// standard constructor
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
	CString	m_MJD;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDateDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;
	double mjd;
	struct tm date;

	// Generated message map functions
	//{{AFX_MSG(CDateDlg)
	virtual BOOL OnInitDialog();
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
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DATEDLG_H__A0DC46B1_90BB_4A9A_A1D4_5DD52D2CED37__INCLUDED_)
