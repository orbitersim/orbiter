// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ShipeditDlg.cpp : implementation file
//

#include "stdafx.h"
#include "Shipedit.h"
#include "ShipeditDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

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
// CShipeditDlg dialog

CShipeditDlg::CShipeditDlg(CShipeditApp *app, CWnd* pParent /*=NULL*/)
	: CDialog(CShipeditDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CShipeditDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_app = app;
}

void CShipeditDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CShipeditDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CShipeditDlg, CDialog)
	//{{AFX_MSG_MAP(CShipeditDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_COMMAND(MID_CALCSTART, OnCalcstart)
	ON_COMMAND(MID_CALCSTOP, OnCalcstop)
	ON_COMMAND(MID_EXIT, OnExit)
	ON_WM_CLOSE()
	ON_COMMAND(MID_CHECK, OnCheck)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CShipeditDlg message handlers

BOOL CShipeditDlg::OnInitDialog()
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
	
	// TODO: Add extra initialization here
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CShipeditDlg::OnSysCommand(UINT nID, LPARAM lParam)
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

void CShipeditDlg::OnPaint() 
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

HCURSOR CShipeditDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CShipeditDlg::Refresh ()
{
	char cbuf[256];
	sprintf (cbuf, "%d", m_app->ngrp);
	GetDlgItem (IDC_NGROUP)->SetWindowText (cbuf);
	sprintf (cbuf, "%d", m_app->nvtx);
	GetDlgItem (IDC_NVTX)->SetWindowText (cbuf);
	sprintf (cbuf, "%d", m_app->ntri);
	GetDlgItem (IDC_NTRI)->SetWindowText (cbuf);
	sprintf (cbuf, "[%0.2f %0.2f %0.2f] [%0.2f %0.2f %0.2f]",
		m_app->bbmin.x, m_app->bbmin.y, m_app->bbmin.z,
		m_app->bbmax.x, m_app->bbmax.y, m_app->bbmax.z);
	GetDlgItem (IDC_BB)->SetWindowText (cbuf);
}

void CShipeditDlg::RefreshCalc ()
{
	char cbuf[256];
	sprintf (cbuf, "Parameters (%d samples)", m_app->nop);
	GetDlgItem (IDC_NSAMPLE)->SetWindowText (cbuf);

	sprintf (cbuf, "%0.2f", m_app->vol);
	GetDlgItem (IDC_VOL)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f %0.2f %0.2f", m_app->cg.x, m_app->cg.y, m_app->cg.z);
	GetDlgItem (IDC_CG)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f %0.2f %0.2f", m_app->cs.x, m_app->cs.y, m_app->cs.z);
	GetDlgItem (IDC_CS)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f\t%0.2f\t%0.2f", m_app->J.m11, m_app->J.m12, m_app->J.m13);
	GetDlgItem (IDC_INERTIA1)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f\t%0.2f\t%0.2f", m_app->J.m12, m_app->J.m22, m_app->J.m23);
	GetDlgItem (IDC_INERTIA2)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f\t%0.2f\t%0.2f", m_app->J.m13, m_app->J.m23, m_app->J.m33);
	GetDlgItem (IDC_INERTIA3)->SetWindowText (cbuf);
}

void CShipeditDlg::OnCalcstart() 
{
	if (m_app->ngrp) // have mesh?
		m_app->bBackgroundOp = TRUE;
}

void CShipeditDlg::OnCalcstop() 
{
	m_app->bBackgroundOp = FALSE;
}

void CShipeditDlg::OnCheck() 
{
	DWORD i, nremoved, tot_removed = 0;
	if (m_app->ngrp) {// have mesh?
		Mesh &mesh = m_app->mesh;
		for (i = 0; i < mesh.nGroup(); i++) {
			mesh.CheckGroup (i, nremoved);
			tot_removed += nremoved;
		}
		if (tot_removed) {
			char cbuf[256];
			sprintf (cbuf, "Removed %d unused vertices from mesh.", tot_removed);
			MessageBox (cbuf, "Check result", MB_OK);
		} else {
			MessageBox ("No problems found", "Check result", MB_OK);
		}
	}
	m_app->InitMesh();
}

void CShipeditDlg::OnExit() 
{
	DestroyWindow ();
}

void CShipeditDlg::OnClose() 
{
	CDialog::OnClose();
	DestroyWindow ();
}
