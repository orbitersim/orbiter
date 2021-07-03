// dxtex.h : main header file for the DXTEX application
//

#if !defined(AFX_DXTX_H__712C53C7_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_)
#define AFX_DXTX_H__712C53C7_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

#ifndef ReleasePpo
	#define ReleasePpo(ppo) \
		if (*(ppo) != NULL) \
		{ \
			(*(ppo))->Release(); \
			*(ppo) = NULL; \
		} \
		else (VOID)0
#endif

/////////////////////////////////////////////////////////////////////////////
// CDxtexDocManager:
// I override this class to customize DoPromptFileName to allow importing of
// BMPs as well as DDSs into CDxtexDocs.
//
class CDxtexDocManager : public CDocManager
{
public:
	virtual BOOL DoPromptFileName(CString& fileName, UINT nIDSTitle,
			DWORD lFlags, BOOL bOpenFileDialog, CDocTemplate* pTemplate);
};

/////////////////////////////////////////////////////////////////////////////
// CDxtexCommandLineInfo:
// I override this class to handle custom command-line options
//
class CDxtexCommandLineInfo : public CCommandLineInfo
{
public:
	CString m_strFileNameAlpha;
	CString m_strFileNameSave;
	DWORD m_dwFourCC;
	BOOL m_bAlphaComing;
	BOOL m_bMipMap;

	CDxtexCommandLineInfo::CDxtexCommandLineInfo(VOID);
	virtual void ParseParam(const TCHAR* pszParam, BOOL bFlag, BOOL bLast);

};

/////////////////////////////////////////////////////////////////////////////
// CDxtexApp:
// See dxtex.cpp for the implementation of this class
//

class CDxtexApp : public CWinApp
{
public:
	CDxtexApp();
	virtual ~CDxtexApp();
	LPDIRECTDRAW7 Pdd(VOID) { return m_pdd; }
	LPDIRECT3D7 Pd3d(VOID) { return m_pd3d; }

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDxtexApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation
	//{{AFX_MSG(CDxtexApp)
	afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
private:
	LPDIRECTDRAW7 m_pdd;
	LPDIRECT3D7 m_pd3d;
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DXTX_H__712C53C7_D63B_11D1_A8B5_00C04FC2DC22__INCLUDED_)
