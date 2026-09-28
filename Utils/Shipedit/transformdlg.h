// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#if !defined(AFX_TRANSFORMDLG_H__1900EF76_C581_47A3_8AF8_BCB3180148B4__INCLUDED_)
#define AFX_TRANSFORMDLG_H__1900EF76_C581_47A3_8AF8_BCB3180148B4__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// TransformDlg.h : header file
//

#include "Mesh.h"

/////////////////////////////////////////////////////////////////////////////
// TranslateDlg dialog

#ifndef __linux__
class TranslateDlg : public CDialog
#else // __linux__
class TranslateDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	TranslateDlg(Mesh *_mesh, CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	TranslateDlg(Mesh *_mesh, QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(TranslateDlg)
	enum { IDD = IDD_DIALOG1 };
	float	m_Translatex;
	float	m_Translatey;
	float	m_Translatez;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(TranslateDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	Mesh *mesh;

	// Generated message map functions
	//{{AFX_MSG(TranslateDlg)
	virtual void OnOK();
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	// DECLARE_MESSAGE_MAP: no entries
#endif // __linux__
};

/////////////////////////////////////////////////////////////////////////////
// RotateDlg dialog

#ifndef __linux__
class RotateDlg : public CDialog
#else // __linux__
class RotateDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	RotateDlg(Mesh *_mesh, CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	RotateDlg(Mesh *_mesh, QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(RotateDlg)
	enum { IDD = IDD_TRANSFORM_ROT };
	double	m_Rotx;
	double	m_Roty;
	double	m_Rotz;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(RotateDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	Mesh *mesh;

	// Generated message map functions
	//{{AFX_MSG(RotateDlg)
#ifndef __linux__
	afx_msg void OnDoRotx();
	afx_msg void OnDoRoty();
	afx_msg void OnDoRotz();
#else // __linux__
	void OnDoRotx();
	void OnDoRoty();
	void OnDoRotz();
#endif // __linux__
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	virtual BOOL OnCommand(int nID, int nCode); // DECLARE_MESSAGE_MAP: the map is a WM_COMMAND switch
#endif // __linux__
};
/////////////////////////////////////////////////////////////////////////////
// ScaleDlg dialog

#ifndef __linux__
class ScaleDlg : public CDialog
#else // __linux__
class ScaleDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	ScaleDlg(Mesh *_mesh, CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	ScaleDlg(Mesh *_mesh, QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(ScaleDlg)
	enum { IDD = IDD_TRANSFORM_SCALE };
	double	m_ScaleX;
	double	m_ScaleY;
	double	m_ScaleZ;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(ScaleDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	Mesh *mesh;

	// Generated message map functions
	//{{AFX_MSG(ScaleDlg)
	virtual void OnOK();
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	// DECLARE_MESSAGE_MAP: no entries
#endif // __linux__
};
/////////////////////////////////////////////////////////////////////////////
// ZerolevelDlg dialog

#ifndef __linux__
class ZerolevelDlg : public CDialog
#else // __linux__
class ZerolevelDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	ZerolevelDlg(Mesh *_mesh, CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	ZerolevelDlg(Mesh *_mesh, QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(ZerolevelDlg)
	enum { IDD = IDD_ZEROLEVEL };
	float	m_Zlevel;
	BOOL	m_ResetVtx;
	BOOL	m_ResetNml;
	BOOL	m_ResetTex;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(ZerolevelDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	Mesh *mesh;

	// Generated message map functions
	//{{AFX_MSG(ZerolevelDlg)
	virtual void OnOK();
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	// DECLARE_MESSAGE_MAP: no entries
#endif // __linux__
};
/////////////////////////////////////////////////////////////////////////////
// MergeDlg dialog

#ifndef __linux__
class MergeDlg : public CDialog
#else // __linux__
class MergeDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	MergeDlg(Mesh *_mesh, CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	MergeDlg(Mesh *_mesh, QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(MergeDlg)
	enum { IDD = IDD_MERGEGRP };
	UINT	m_Grp1;
	UINT	m_Grp2;
#ifndef __linux__
	CString	m_Label1;
	CString	m_Label2;
#else // __linux__
	std::string	m_Label1;
	std::string	m_Label2;
#endif // __linux__
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(MergeDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	Mesh *mesh;

	// Generated message map functions
	//{{AFX_MSG(MergeDlg)
	virtual void OnOK();
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	// DECLARE_MESSAGE_MAP: no entries
#endif // __linux__
};
/////////////////////////////////////////////////////////////////////////////
// NormalDlg dialog

#ifndef __linux__
class NormalDlg : public CDialog
#else // __linux__
class NormalDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	NormalDlg(Mesh *_mesh, CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	NormalDlg(Mesh *_mesh, QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(NormalDlg)
	enum { IDD = IDD_CALCNORMAL };
	int		m_Selgrp;
	int		m_Selvtx;
	UINT	m_Group;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(NormalDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	Mesh *mesh;

	// Generated message map functions
	//{{AFX_MSG(NormalDlg)
#ifndef __linux__
	afx_msg void OnNmlSelall();
	afx_msg void OnNmlSelone();
	afx_msg void OnNmlapply();
#else // __linux__
	void OnNmlSelall();
	void OnNmlSelone();
	void OnNmlapply();
#endif // __linux__
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	virtual BOOL OnCommand(int nID, int nCode); // DECLARE_MESSAGE_MAP: the map is a WM_COMMAND switch
#endif // __linux__
};
/////////////////////////////////////////////////////////////////////////////
// MirrorDlg dialog

#ifndef __linux__
class MirrorDlg : public CDialog
#else // __linux__
class MirrorDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	MirrorDlg(Mesh *_mesh, CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	MirrorDlg(Mesh *_mesh, QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(MirrorDlg)
	enum { IDD = IDD_MIRROR };
	int		m_MirrorX;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(MirrorDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	Mesh *mesh;

	// Generated message map functions
	//{{AFX_MSG(MirrorDlg)
	virtual void OnOK();
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	// DECLARE_MESSAGE_MAP: no entries
#endif // __linux__
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TRANSFORMDLG_H__1900EF76_C581_47A3_8AF8_BCB3180148B4__INCLUDED_)
