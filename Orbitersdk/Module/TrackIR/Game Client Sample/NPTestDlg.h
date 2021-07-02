// NPTestDlg.h : header file
//

#if !defined(AFX_NPTESTDLG_H__B7DF9FDC_28C8_4F10_BC7F_8D3ACC4AF3A1__INCLUDED_)
#define AFX_NPTESTDLG_H__B7DF9FDC_28C8_4F10_BC7F_8D3ACC4AF3A1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CNPTestDlg dialog

class CNPTestDlg : public CDialog
{
// Construction
public:
	CNPTestDlg(CWnd* pParent = NULL);	  // standard constructor

	void DisplayLine( LPCTSTR pszLine );  // Send text output lines to the edit box
	void DisplayData( CString tstr);  // Send text output lines to the edit box
    void TrackIR_Enhanced_Init();
    void TrackIR_Enhanced_Shutdown();

	void GetDllLocation(LPTSTR pszPath);
// Dialog Data
	//{{AFX_DATA(CNPTestDlg)
	enum { IDD = IDD_NPTEST_DIALOG };
	CEdit	m_eTextOut;
	CEdit	m_eNPShowData;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNPTestDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CNPTestDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnTimer(UINT nIDEvent);
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

private:
	int m_nDspBufMaxLineCount;   // Indicates the number of lines to be retained
							  //   in the text buffer displayed by m_eTextOut.
	int m_nTimerMessageNum;

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NPTESTDLG_H__B7DF9FDC_28C8_4F10_BC7F_8D3ACC4AF3A1__INCLUDED_)
