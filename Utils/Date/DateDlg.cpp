// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// DateDlg.cpp : implementation file
//

#ifndef __linux__
#include "stdafx.h"
#else // __linux__
#include "StdAfx.h"
#endif // __linux__
#include "Date.h"
#include "DateDlg.h"
#include "Convert.h"
#ifdef __linux__
#include <QAction>
#include <cassert>
#include <cstring>
#endif // __linux__

#ifndef __linux__
#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
#else // __linux__
// DEBUG_NEW (_DEBUG) left out: MFC's debug allocator
#endif // __linux__

static bool bIgnore = false;

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

#ifndef __linux__
class CAboutDlg : public CDialog
#else // __linux__
class CAboutDlg : public ResDlg
#endif // __linux__
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
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#endif // !__linux__
};

#ifndef __linux__
CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
#else // __linux__
CAboutDlg::CAboutDlg() : ResDlg(CAboutDlg::IDD)
#endif // __linux__
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

#ifndef __linux__
void CAboutDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void CAboutDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
#ifndef __linux__
	CDialog::DoDataExchange(pDX);
#else // __linux__
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

#ifndef __linux__
BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
#else // __linux__
// BEGIN_MESSAGE_MAP(CAboutDlg): no message handlers, ResDlg::OnCommand ends the dialog on IDOK
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// CDateDlg dialog

#ifndef __linux__
CDateDlg::CDateDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CDateDlg::IDD, pParent)
#else // __linux__
CDateDlg::CDateDlg(QWidget* pParent /*=NULL*/)
	: ResDlg(CDateDlg::IDD, pParent)
#endif // __linux__
{
	//{{AFX_DATA_INIT(CDateDlg)
#ifndef __linux__
	m_MJD = _T("");
#else // __linux__
	m_MJD = "";
#endif // __linux__
	//}}AFX_DATA_INIT
#ifndef __linux__
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
#else // __linux__
	m_hIcon = ResDlg::LoadIcon(IDR_MAINFRAME);
#endif // __linux__
}

#ifndef __linux__
void CDateDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void CDateDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
#ifndef __linux__
	CDialog::DoDataExchange(pDX);
#else // __linux__
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(CDateDlg)
#ifndef __linux__
	DDX_Text(pDX, IDC_MJD, m_MJD);
	DDV_MaxChars(pDX, m_MJD, 32);
#else // __linux__
	ExchangeText(bSaveAndValidate, IDC_MJD, m_MJD);
	ValidateMaxChars(bSaveAndValidate, m_MJD, 32);
#endif // __linux__
	//}}AFX_DATA_MAP
}

#ifndef __linux__
BEGIN_MESSAGE_MAP(CDateDlg, CDialog)
#else // __linux__
// message map: WM_COMMAND notifications of the controls, connected in ResDlg (oapiConnectDlgCommands)
BOOL CDateDlg::OnCommand(int nID, int nCode)
{
#endif // __linux__
	//{{AFX_MSG_MAP(CDateDlg)
#ifndef __linux__
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
#else // __linux__
	// ON_WM_SYSCOMMAND: OnInitDialog connects the About entry of the context menu
	// ON_WM_PAINT, ON_WM_QUERYDRAGICON left out (see DateDlg.h)
	if (nCode == RESN_CHANGE) switch (nID) { // ON_EN_CHANGE
	case IDC_MJD:      OnChangeMjd();     return TRUE;
	case IDC_UT_DAY:   OnChangeUtDay();   return TRUE;
	case IDC_UT_MONTH: OnChangeUtMonth(); return TRUE;
	case IDC_UT_YEAR:  OnChangeUtYear();  return TRUE;
	case IDC_UT_HOUR:  OnChangeUtHour();  return TRUE;
	case IDC_UT_MIN:   OnChangeUtMin();   return TRUE;
	case IDC_UT_SEC:   OnChangeUtSec();   return TRUE;
	case IDC_JD:       OnChangeJd();      return TRUE;
	case IDC_JC:       OnChangeJc();      return TRUE;
	case IDC_EPOCH:    OnChangeEpoch();   return TRUE;
	}
#endif // __linux__
	//}}AFX_MSG_MAP
#ifndef __linux__
END_MESSAGE_MAP()
#else // __linux__
	return ResDlg::OnCommand(nID, nCode);
}
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// CDateDlg message handlers

BOOL CDateDlg::OnInitDialog()
{
#ifndef __linux__
	CDialog::OnInitDialog();
#else // __linux__
	ResDlg::OnInitDialog();
#endif // __linux__

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
#ifndef __linux__
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);
#else // __linux__
	assert((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	assert(IDM_ABOUTBOX < 0xF000);
#endif // __linux__

#ifndef __linux__
	CMenu* pSysMenu = GetSystemMenu(FALSE);
#else // __linux__
	QWidget* pSysMenu = hDlg; // the title bar menu is the window manager's: the entry goes to the dialog's context menu
#endif // __linux__
	if (pSysMenu != NULL)
	{
#ifndef __linux__
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
#else // __linux__
		char strAboutMenu[256];
		oapiLoadResString(nullptr, IDS_ABOUTBOX, strAboutMenu, 256);
		if (strAboutMenu[0])
#endif // __linux__
		{
#ifndef __linux__
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
#else // __linux__
			QAction* pSep = new QAction(pSysMenu);
			pSep->setSeparator(true);
			pSysMenu->addAction(pSep); // MF_SEPARATOR
			QAction* pAbout = new QAction(QString::fromUtf8(strAboutMenu), pSysMenu);
			QObject::connect(pAbout, &QAction::triggered, pSysMenu, [this]() { OnSysCommand(IDM_ABOUTBOX, 0); });
			pSysMenu->addAction(pAbout); // MF_STRING, IDM_ABOUTBOX
			pSysMenu->setContextMenuPolicy(Qt::ActionsContextMenu);
#endif // __linux__
		}
	}

#ifndef __linux__
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
#else // __linux__
	hDlg->setWindowIcon(m_hIcon);	// Set big and small icon
#endif // __linux__
	
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
#ifndef __linux__
		CDialog::OnSysCommand(nID, lParam);
#else // __linux__
		// CDialog::OnSysCommand left out: the other system commands belong to the window manager
#endif // __linux__
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

#ifndef __linux__
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
#else // __linux__
// OnPaint, OnQueryDragIcon left out: the window manager draws the minimised window's icon
#endif // __linux__

void CDateDlg::UpdateUT (void)
{
	char cbuf[256];

	sprintf (cbuf, "%02d", date.tm_mday);
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_UT_DAY)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_UT_DAY), cbuf);
#endif // __linux__
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_mon);
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_UT_MONTH)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_UT_MONTH), cbuf);
#endif // __linux__
	bIgnore = false;

	sprintf (cbuf, "%04d", date.tm_year+1900);
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_UT_YEAR)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_UT_YEAR), cbuf);
#endif // __linux__
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_hour);
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_UT_HOUR)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_UT_HOUR), cbuf);
#endif // __linux__
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_min);
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_UT_MIN)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_UT_MIN), cbuf);
#endif // __linux__
	bIgnore = false;

	sprintf (cbuf, "%02d", date.tm_sec);
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_UT_SEC)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_UT_SEC), cbuf);
#endif // __linux__
	bIgnore = false;
}

void CDateDlg::UpdateMJD (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.6f", mjd);
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_MJD)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_MJD), cbuf);
#endif // __linux__
	bIgnore = false;
}

void CDateDlg::UpdateJD (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.6f", mjd + 2400000.5);
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_JD)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_JD), cbuf);
#endif // __linux__
	bIgnore = false;
}

void CDateDlg::UpdateJC (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.10f", MJD2JC(mjd));
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_JC)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_JC), cbuf);
#endif // __linux__
	bIgnore = false;
}

void CDateDlg::UpdateEpoch (void)
{
	char cbuf[256];
	sprintf (cbuf, "%0.8f", MJD2Jepoch (mjd));
	bIgnore = true;
#ifndef __linux__
	GetDlgItem (IDC_EPOCH)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_EPOCH), cbuf);
#endif // __linux__
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
#ifndef __linux__
	GetDlgItem (IDC_MJD)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_MJD), cbuf, 256);
#endif // __linux__
	if (sscanf (cbuf, "%lf", &new_mjd) == 1 && fabs (new_mjd-mjd) > 1e-6)
		SetMJD (new_mjd);
}

void CDateDlg::OnChangeJd() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_jd;
#ifndef __linux__
	GetDlgItem (IDC_JD)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_JD), cbuf, 256);
#endif // __linux__
	if (sscanf (cbuf, "%lf", &new_jd) == 1)
		SetJD (new_jd);
}

void CDateDlg::OnChangeJc() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_jc;
#ifndef __linux__
	GetDlgItem (IDC_JC)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_JC), cbuf, 256);
#endif // __linux__
	if (sscanf (cbuf, "%lf", &new_jc) == 1)
		SetJC (new_jc);
}

void CDateDlg::OnChangeEpoch() 
{
	if (bIgnore) return;
	char cbuf[256];
	double new_epoch;
#ifndef __linux__
	GetDlgItem (IDC_EPOCH)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_EPOCH), cbuf, 256);
#endif // __linux__
	if (sscanf (cbuf, "%lf", &new_epoch) == 1)
		SetEpoch (new_epoch);
}

void CDateDlg::OnChangeUtDay() 
{
	if (bIgnore) return;
	char cbuf[256];
	int day;

#ifndef __linux__
	GetDlgItem (IDC_UT_DAY)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_UT_DAY), cbuf, 256);
#endif // __linux__
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

#ifndef __linux__
	GetDlgItem (IDC_UT_MONTH)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_UT_MONTH), cbuf, 256);
#endif // __linux__
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

#ifndef __linux__
	GetDlgItem (IDC_UT_YEAR)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_UT_YEAR), cbuf, 256);
#endif // __linux__
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

#ifndef __linux__
	GetDlgItem (IDC_UT_HOUR)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_UT_HOUR), cbuf, 256);
#endif // __linux__
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

#ifndef __linux__
	GetDlgItem (IDC_UT_MIN)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_UT_MIN), cbuf, 256);
#endif // __linux__
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

#ifndef __linux__
	GetDlgItem (IDC_UT_SEC)->GetWindowText (cbuf, 256);
#else // __linux__
	oapiGetDlgText (GetDlgItem (IDC_UT_SEC), cbuf, 256);
#endif // __linux__
	if (sscanf (cbuf, "%d", &sec) == 1 && sec != date.tm_sec) {
		date.tm_sec = sec;
		SetUT (&date);
	}
}
