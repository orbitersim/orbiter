// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ShipeditDlg.cpp : implementation file
//

#ifndef __linux__
#include "stdafx.h"
#else // __linux__
#include "StdAfx.h"
#endif // __linux__
#include "Shipedit.h"
#include "ShipeditDlg.h"
#ifdef __linux__
#include <QAction>
#include <QMessageBox>
#include <cassert>
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
// CShipeditDlg dialog

#ifndef __linux__
CShipeditDlg::CShipeditDlg(CShipeditApp *app, CWnd* pParent /*=NULL*/)
	: CDialog(CShipeditDlg::IDD, pParent)
#else // __linux__
CShipeditDlg::CShipeditDlg(CShipeditApp *app, QWidget* pParent /*=NULL*/)
	: ResDlg(CShipeditDlg::IDD, pParent)
#endif // __linux__
{
	//{{AFX_DATA_INIT(CShipeditDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
#ifndef __linux__
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
#else // __linux__
	m_hIcon = ResDlg::LoadIcon(IDR_MAINFRAME);
#endif // __linux__
	m_app = app;
}

#ifndef __linux__
void CShipeditDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void CShipeditDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
#ifndef __linux__
	CDialog::DoDataExchange(pDX);
#else // __linux__
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(CShipeditDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

#ifndef __linux__
BEGIN_MESSAGE_MAP(CShipeditDlg, CDialog)
#else // __linux__
// message map: menu commands (the menu bar of OnInitDialog); what the dialog doesn't handle goes to the app (CDialog::OnCmdMsg)
BOOL CShipeditDlg::OnCommand(int nID, int nCode)
{
#endif // __linux__
	//{{AFX_MSG_MAP(CShipeditDlg)
#ifndef __linux__
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_COMMAND(MID_CALCSTART, OnCalcstart)
	ON_COMMAND(MID_CALCSTOP, OnCalcstop)
	ON_COMMAND(MID_EXIT, OnExit)
	ON_WM_CLOSE()
	ON_COMMAND(MID_CHECK, OnCheck)
#else // __linux__
	// ON_WM_SYSCOMMAND: OnInitDialog connects the About entry of the context menu
	// ON_WM_PAINT, ON_WM_QUERYDRAGICON left out (see ShipeditDlg.h)
	// ON_WM_CLOSE: ResDlg calls OnClose on the window's close event
	if (nCode == RESN_CLICKED) switch (nID) { // ON_COMMAND
	case MID_CALCSTART: OnCalcstart(); return TRUE;
	case MID_CALCSTOP:  OnCalcstop();  return TRUE;
	case MID_EXIT:      OnExit();      return TRUE;
	case MID_CHECK:     OnCheck();     return TRUE;
	}
#endif // __linux__
	//}}AFX_MSG_MAP
#ifndef __linux__
END_MESSAGE_MAP()
#else // __linux__
	if (ResDlg::OnCommand(nID, nCode)) return TRUE;
	return nCode == RESN_CLICKED && m_app->OnCommand(nID);
}
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// CShipeditDlg message handlers

BOOL CShipeditDlg::OnInitDialog()
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
#else // __linux__
// OnPaint, OnQueryDragIcon left out: the window manager draws the minimised window's icon
#endif // __linux__

void CShipeditDlg::Refresh ()
{
	char cbuf[256];
	sprintf (cbuf, "%d", m_app->ngrp);
#ifndef __linux__
	GetDlgItem (IDC_NGROUP)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_NGROUP), cbuf);
#endif // __linux__
	sprintf (cbuf, "%d", m_app->nvtx);
#ifndef __linux__
	GetDlgItem (IDC_NVTX)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_NVTX), cbuf);
#endif // __linux__
	sprintf (cbuf, "%d", m_app->ntri);
#ifndef __linux__
	GetDlgItem (IDC_NTRI)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_NTRI), cbuf);
#endif // __linux__
	sprintf (cbuf, "[%0.2f %0.2f %0.2f] [%0.2f %0.2f %0.2f]",
		m_app->bbmin.x, m_app->bbmin.y, m_app->bbmin.z,
		m_app->bbmax.x, m_app->bbmax.y, m_app->bbmax.z);
#ifndef __linux__
	GetDlgItem (IDC_BB)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_BB), cbuf);
#endif // __linux__
}

void CShipeditDlg::RefreshCalc ()
{
	char cbuf[256];
	sprintf (cbuf, "Parameters (%d samples)", m_app->nop);
#ifndef __linux__
	GetDlgItem (IDC_NSAMPLE)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_NSAMPLE), cbuf);
#endif // __linux__

	sprintf (cbuf, "%0.2f", m_app->vol);
#ifndef __linux__
	GetDlgItem (IDC_VOL)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_VOL), cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.2f %0.2f %0.2f", m_app->cg.x, m_app->cg.y, m_app->cg.z);
#ifndef __linux__
	GetDlgItem (IDC_CG)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_CG), cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.2f %0.2f %0.2f", m_app->cs.x, m_app->cs.y, m_app->cs.z);
#ifndef __linux__
	GetDlgItem (IDC_CS)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_CS), cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.2f\t%0.2f\t%0.2f", m_app->J.m11, m_app->J.m12, m_app->J.m13);
#ifndef __linux__
	GetDlgItem (IDC_INERTIA1)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_INERTIA1), cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.2f\t%0.2f\t%0.2f", m_app->J.m12, m_app->J.m22, m_app->J.m23);
#ifndef __linux__
	GetDlgItem (IDC_INERTIA2)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_INERTIA2), cbuf);
#endif // __linux__
	sprintf (cbuf, "%0.2f\t%0.2f\t%0.2f", m_app->J.m13, m_app->J.m23, m_app->J.m33);
#ifndef __linux__
	GetDlgItem (IDC_INERTIA3)->SetWindowText (cbuf);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_INERTIA3), cbuf);
#endif // __linux__
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
#ifndef __linux__
			MessageBox (cbuf, "Check result", MB_OK);
#else // __linux__
			QMessageBox (QMessageBox::NoIcon, "Check result", cbuf, QMessageBox::Ok, hDlg).exec (); // MessageBox, MB_OK
#endif // __linux__
		} else {
#ifndef __linux__
			MessageBox ("No problems found", "Check result", MB_OK);
#else // __linux__
			QMessageBox (QMessageBox::NoIcon, "Check result", "No problems found", QMessageBox::Ok, hDlg).exec ();
#endif // __linux__
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
#ifndef __linux__
	CDialog::OnClose();
#else // __linux__
	ResDlg::OnClose();
#endif // __linux__
	DestroyWindow ();
}
