// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Date.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "Date.h"
#include "DateDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDateApp

BEGIN_MESSAGE_MAP(CDateApp, CWinApp)
	//{{AFX_MSG_MAP(CDateApp)
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDateApp construction

CDateApp::CDateApp()
{
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CDateApp object

CDateApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CDateApp initialization

BOOL CDateApp::InitInstance()
{
	// Standard initialization

	CDateDlg dlg;
	m_pMainWnd = &dlg;
	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
	}
	else if (nResponse == IDCANCEL)
	{
	}

	// Since the dialog has been closed, return FALSE so that we exit the
	//  application, rather than start the application's message pump.
	return FALSE;
}
