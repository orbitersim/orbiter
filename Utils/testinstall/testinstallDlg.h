// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// verinstDlg.h : header file
//

#pragma once
#include "afxcmn.h"


// CverinstDlg dialog
class CverinstDlg : public CDialog
{
// Construction
public:
	CverinstDlg(const CverinstApp* pParent);	// standard constructor

// Dialog Data
	enum { IDD = IDD_VERINST_DIALOG };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	LRESULT DefWindowProc (UINT msg, WPARAM wParam, LPARAM lParam);
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	DECLARE_MESSAGE_MAP()

private:
	void echo (const char *line);
	void SetAction (int item, int action);
	void PerformTests1 ();
	void PerformTests2 (DWORD res);
	bool TestSubdirs();
	bool TestRuntime();
	HANDLE StartTestDirectX();
	bool FinishTestDirectX (DWORD res);
	bool FinishTests();

	HANDLE hDxdiag;
	bool allsucceed;
	const CverinstApp *app;

public:
	afx_msg void OnEnChangeEdit1();
	afx_msg void OnBnClickedCfg();
	afx_msg void OnBnClickedOk();
};
