// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Date.h : main header file for the DATE application
//

#if !defined(AFX_DATE_H__C7114870_6AAA_4AD8_A08F_CA2ADA650ED8__INCLUDED_)
#define AFX_DATE_H__C7114870_6AAA_4AD8_A08F_CA2ADA650ED8__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CDateApp:
// See Date.cpp for the implementation of this class
//

class CDateApp : public CWinApp
{
public:
	CDateApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDateApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CDateApp)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DATE_H__C7114870_6AAA_4AD8_A08F_CA2ADA650ED8__INCLUDED_)
