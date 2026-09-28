// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Date.cpp : Defines the class behaviors for the application.
//

#ifndef __linux__
#include "stdafx.h"
#else // __linux__
#include "StdAfx.h"
#endif // __linux__
#include "Date.h"
#include "DateDlg.h"
#ifdef __linux__
#include <clocale>
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
// CDateApp

#ifndef __linux__
BEGIN_MESSAGE_MAP(CDateApp, CWinApp)
	//{{AFX_MSG_MAP(CDateApp)
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()
#else // __linux__
// BEGIN_MESSAGE_MAP left out: its only entry, ID_HELP -> CWinApp::OnHelp, opens the app's WinHelp file, and Date has none
#endif // __linux__

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
#ifdef __linux__
}

// not upstream: main() stands in for MFC's WinMain (AfxWinMain): InitInstance, then the message pump if it returns TRUE
int main(int argc, char *argv[])
{
	QApplication app(argc, argv);
	setlocale(LC_ALL, "C"); // Qt takes the environment locale; the number texts need "C" as on Windows
	if (theApp.InitInstance())
		app.exec(); // CWinApp::Run
	return 0;
#endif // __linux__
}
