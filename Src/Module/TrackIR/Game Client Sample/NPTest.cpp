// NPTest.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "NPTest.h"
#include "NPTestDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNPTestApp

BEGIN_MESSAGE_MAP(CNPTestApp, CWinApp)
	//{{AFX_MSG_MAP(CNPTestApp)
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNPTestApp construction

CNPTestApp::CNPTestApp()
{
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CNPTestApp object

CNPTestApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CNPTestApp initialization

BOOL CNPTestApp::InitInstance()
{
	// Standard initialization

#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif

	CNPTestDlg dlg;
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
