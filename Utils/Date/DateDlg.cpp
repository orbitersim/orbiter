// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// DateDlg.cpp : implementation file
//

#include "stdafx.h"
#include "Date.h"
#include "DateDlg.h"
#include "Convert.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static bool bIgnore = false;

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDateDlg dialog

CDateDlg::CDateDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CDateDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CDateDlg)
	m_MJD = _T("");
	//}}AFX_DATA_INIT
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CDateDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDateDlg)
	DDX_Text(pDX, IDC_MJD, m_MJD);
	DDV_MaxChars(pDX, m_MJD, 32);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CDateDlg, CDialog)
	//{{AFX_MSG_MAP(CDateDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_EN_CHANGE(IDC_MJD, OnChangeMjd)
	ON_EN_CHANGE(IDC_UT_DAY, OnChangeUtDay)
	ON_EN_CHANGE(IDC_UT_MONTH, OnChangeUtMonth)
	ON_EN_CHANGE(IDC_UT_YEAR, OnChangeUtYear)
	ON_EN_CHANGE(IDC_UT_HOUR, OnChangeUtHour)
	ON_EN_CHANGE(IDC_UT_MIN, OnChangeUtMin)
	ON_EN_CHANGE(IDC_UT_SEC, OnChangeUtSec)
	ON_EN_CHANGE(IDC_JD, OnChangeJd)
	ON_EN_CHANGE(IDC_JC, OnChangeJc)
	ON_EN_CHANGE(IDC_EPOCH, OnChangeEpoch)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDateDlg message handlers

BOOL CDateDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	SetMJD (MJD (time (NULL)), true);
	// initialise to current system time
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CDateDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CDateDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

HCURSOR CDateDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CDateDlg::UpdateUT (void)
{
	char cbuf[256];

	sprintf (cbuf, "%02d", date.tm_mday);
	bIgnore = true;
	GetDlgItem (IDC_UT_DAY)->SetWindowText (cbuf);
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_mon);
	bIgnore = true;
	GetDlgItem (IDC_UT_MONTH)->SetWindowText (cbuf);
	bIgnore = false;

	sprintf (cbuf, "%04d", date.tm_year+1900);
	bIgnore = true;
	GetDlgItem (IDC_UT_YEAR)->SetWindowText (cbuf);
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_hour);
	bIgnore = true;
	GetDlgItem (IDC_UT_HOUR)->SetWindowText (cbuf);
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_min);
	bIgnore = true;
	GetDlgItem (IDC_UT_MIN)->SetWindowText (cbuf);
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_sec);
	bIgnore = true;
	GetDlgItem (IDC_UT_SEC)->SetWindowText (cbuf);
	bIgnore = false;
}

void CDateDlg::UpdateMJD (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.6f", mjd);
	bIgnore = true;
	GetDlgItem (IDC_MJD)->SetWindowText (cbuf);
	bIgnore = false;
}

void CDateDlg::UpdateJD (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.6f", mjd + 2400000.5);
	bIgnore = true;
	GetDlgItem (IDC_JD)->SetWindowText (cbuf);
	bIgnore = false;
}

void CDateDlg::UpdateJC (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.10f", MJD2JC(mjd));
	bIgnore = true;
	GetDlgItem (IDC_JC)->SetWindowText (cbuf);
	bIgnore = false;
}

void CDateDlg::UpdateEpoch (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.8f", MJD2Jepoch (mjd));
	bIgnore = true;
	GetDlgItem (IDC_EPOCH)->SetWindowText (cbuf);
	bIgnore = false;
}

void CDateDlg::SetMJD (double new_mjd, bool reset_mjd)
{
	mjd = new_mjd;
	memcpy (&date, mjddate(mjd), sizeof (date));

	UpdateUT();
	UpdateJD();
	UpdateJC();
	UpdateEpoch();
	if (reset_mjd) UpdateMJD();
}

void CDateDlg::SetJD (double new_jd, bool reset_jd)
{
	mjd = new_jd - 2400000.5;
	memcpy (&date, mjddate(mjd), sizeof (date));

	UpdateUT();
	UpdateMJD();
	UpdateJC();
	UpdateEpoch();
	if (reset_jd) UpdateJD();
}

void CDateDlg::SetJC (double new_jc, bool reset_jc)
{
	mjd = JC2MJD (new_jc);
	memcpy (&date, mjddate(mjd), sizeof (date));

	UpdateUT();
	UpdateMJD();
	UpdateJD();
	UpdateEpoch();
	if (reset_jc) UpdateJC();
}

void CDateDlg::SetEpoch (double new_epoch, bool reset_epoch)
{
	mjd = Jepoch2MJD (new_epoch);
	memcpy (&date, mjddate(mjd), sizeof (date));

	UpdateUT();
	UpdateMJD();
	UpdateJD();
	UpdateJC();
	if (reset_epoch) UpdateEpoch();
}

void CDateDlg::SetUT (struct tm *new_date, bool reset_ut)
{
	mjd = date2mjd (new_date);
	UpdateMJD();
	UpdateJD();
	UpdateJC();
	UpdateEpoch();
	if (reset_ut) UpdateUT();
}

void CDateDlg::OnChangeMjd() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_mjd;
	GetDlgItem (IDC_MJD)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%lf", &new_mjd) == 1 && fabs (new_mjd-mjd) > 1e-6)
		SetMJD (new_mjd);
}

void CDateDlg::OnChangeJd() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_jd;
	GetDlgItem (IDC_JD)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%lf", &new_jd) == 1)
		SetJD (new_jd);
}

void CDateDlg::OnChangeJc() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_jc;
	GetDlgItem (IDC_JC)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%lf", &new_jc) == 1)
		SetJC (new_jc);
}

void CDateDlg::OnChangeEpoch() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_epoch;
	GetDlgItem (IDC_EPOCH)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%lf", &new_epoch) == 1)
		SetEpoch (new_epoch);
}

void CDateDlg::OnChangeUtDay() 
{
	if (bIgnore) return;
	char cbuf[256];
	int day;

	GetDlgItem (IDC_UT_DAY)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%d", &day) == 1 && day != date.tm_mday && day >= 1 && day <= 31) {
		date.tm_mday = day;
		SetUT (&date);
	}
}

void CDateDlg::OnChangeUtMonth() 
{
	if (bIgnore) return;
	char cbuf[256];
	int month;

	GetDlgItem (IDC_UT_MONTH)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%d", &month) == 1 && month != date.tm_mon && month >= 1 && month <= 12) {
		date.tm_mon = month;
		SetUT (&date);
	}
}

void CDateDlg::OnChangeUtYear() 
{
	if (bIgnore) return;
	char cbuf[256];
	int year;

	GetDlgItem (IDC_UT_YEAR)->GetWindowText (cbuf, 256);
	if ((sscanf (cbuf, "%d", &year) == 1) && ((year -= 1900) != date.tm_year)) {
		date.tm_year = year;
		SetUT (&date);
	}
}

void CDateDlg::OnChangeUtHour() 
{
	if (bIgnore) return;
	char cbuf[256];
	int hour;

	GetDlgItem (IDC_UT_HOUR)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%d", &hour) == 1 && hour != date.tm_hour) {
		date.tm_hour = hour;
		SetUT (&date);
	}
}

void CDateDlg::OnChangeUtMin() 
{
	if (bIgnore) return;
	char cbuf[256];
	int min;

	GetDlgItem (IDC_UT_MIN)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%d", &min) == 1 && min != date.tm_min) {
		date.tm_min = min;
		SetUT (&date);
	}
}

void CDateDlg::OnChangeUtSec() 
{
	if (bIgnore) return;
	char cbuf[256];
	int sec;

	GetDlgItem (IDC_UT_SEC)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%d", &sec) == 1 && sec != date.tm_sec) {
		date.tm_sec = sec;
		SetUT (&date);
	}
}
