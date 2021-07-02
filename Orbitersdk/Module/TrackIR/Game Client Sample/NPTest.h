// NPTest.h : main header file for the NPTEST application
//

#if !defined(AFX_NPTEST_H__41FD3C39_3432_42CB_9EB4_AC71AB4149C4__INCLUDED_)
#define AFX_NPTEST_H__41FD3C39_3432_42CB_9EB4_AC71AB4149C4__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CNPTestApp:
// See NPTest.cpp for the implementation of this class
//

class CNPTestApp : public CWinApp
{
public:
	CNPTestApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNPTestApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CNPTestApp)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NPTEST_H__41FD3C39_3432_42CB_9EB4_AC71AB4149C4__INCLUDED_)
