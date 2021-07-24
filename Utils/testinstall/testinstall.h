// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// verinst.h : main header file for the PROJECT_NAME application
//

#pragma once

#ifndef __AFXWIN_H__
	#error "include 'stdafx.h' before including this file for PCH"
#endif

#include "resource.h"		// main symbols


class CverinstDlg;

// CverinstApp:
// See verinst.cpp for the implementation of this class
//

class CverinstApp : public CWinApp
{
public:
	CverinstApp();

// Overrides
public:
	virtual BOOL InitInstance();
	bool firstrun;

// Implementation

	DECLARE_MESSAGE_MAP()
};

extern CverinstApp theApp;