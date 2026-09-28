// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Date.h : main header file for the DATE application
//

#if !defined(AFX_DATE_H__C7114870_6AAA_4AD8_A08F_CA2ADA650ED8__INCLUDED_)
#define AFX_DATE_H__C7114870_6AAA_4AD8_A08F_CA2ADA650ED8__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __linux__
#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif
#else // __linux__
#ifndef AFX_STDAFX_H__2749A3D2_C3AC_49E9_94A0_4873DA6265FB__INCLUDED_ // __AFXWIN_H__: StdAfx.h no longer brings afxwin.h
	#error include 'stdafx.h' before including this file for PCH
#endif
#endif // __linux__

#ifndef __linux__
#include "resource.h"		// main symbols
#else // __linux__
#include "Resource.h"		// main symbols
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// CDateApp:
// See Date.cpp for the implementation of this class
//

#ifndef __linux__
class CDateApp : public CWinApp
#else // __linux__
class CDateApp // CWinApp: main() in Date.cpp runs InitInstance with a QApplication
#endif // __linux__
{
public:
	CDateApp();
#ifdef __linux__
	ResDlg *m_pMainWnd; // CWinThread::m_pMainWnd
#endif // __linux__

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDateApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CDateApp)
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	// DECLARE_MESSAGE_MAP left out: see Date.cpp
#endif // __linux__
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DATE_H__C7114870_6AAA_4AD8_A08F_CA2ADA650ED8__INCLUDED_)
