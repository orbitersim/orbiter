// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Shipedit.h : main header file for the SHIPEDIT application
//

#if !defined(AFX_SHIPEDIT_H__820CEA84_B7CD_4458_8801_283B73C584D4__INCLUDED_)
#define AFX_SHIPEDIT_H__820CEA84_B7CD_4458_8801_283B73C584D4__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __linux__
#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif
#else // __linux__
#ifndef AFX_STDAFX_H__8DE406CF_DC7E_4C3E_820F_4BED08745A8F__INCLUDED_ // __AFXWIN_H__: StdAfx.h no longer brings afxwin.h
	#error include 'stdafx.h' before including this file for PCH
#endif
#endif // __linux__

#ifndef __linux__
#include <d3d.h>
#else // __linux__
// d3d.h left out: the Direct3D 7 data types come from the SDK (see Mesh.h)
#endif // __linux__
#include "resource.h"		// main symbols
#include "Vecmat.h"
#include "Mesh.h"

typedef struct {
	float x1, y1, z1; // vtx 1
	float x2, y2, z2; // vtx 2
	float x3, y3, z3; // vtx 3
	float a, b, c, d; // plane params
	float d1, d2, d3; // dist of vtx i from opposite edge
} TriParam;

typedef struct {
	int gridx, gridy, gridz;
	int gridn;
	float dx, dy, dz;
	BYTE *grid;
} VOXGRID;

/////////////////////////////////////////////////////////////////////////////
// CShipeditApp:
// See Shipedit.cpp for the implementation of this class
//

#ifndef __linux__
class CShipeditApp : public CWinApp {
#else // __linux__
class CShipeditDlg; // g++: the friend declaration below doesn't make the name visible to m_pMainDlg

class CShipeditApp { // CWinApp: main() in Shipedit.cpp runs InitInstance and Run with a QApplication
#endif // __linux__
	friend class CShipeditDlg;
	friend class GridintDlg;
public:
	CShipeditApp ();
	void InitMesh ();
	Mesh mesh;
#ifdef __linux__
	int Run ();                    // CWinThread::Run: message loop with OnIdle, then ExitInstance
	BOOL OnCommand (int nID);      // CCmdTarget::OnCmdMsg: the app's message map, last in the command route
	ResDlg *m_pMainWnd;            // CWinThread::m_pMainWnd
#endif // __linux__

private:
	void ProcessPackage ();
	BOOL OnIdle (LONG lCount);
	CShipeditDlg *m_pMainDlg;
	DWORD ngrp, nvtx, nidx, ntri;  // mesh groups
#ifndef __linux__
	D3DVERTEX *vtx;
#else // __linux__
	NTVERTEX *vtx;
#endif // __linux__
	WORD *idx;
	TriParam *pp;
#ifndef __linux__
	D3DVECTOR bbmin, bbmax;        // bounding box
#else // __linux__
	oapi::FVECTOR3 bbmin, bbmax;   // bounding box
#endif // __linux__
	double bbvol;                  // bb volume
	Vector bbcs;                   // bb cross sections
	double vol;                    // volume
	Vector cg, cg_base, cg_add;    // centre of gravity
	Vector cs;                     // cross sections
	Matrix J, J_base, J_add;       // inertia tensor
	BOOL bBackgroundOp;
	int flushcount;
	int nop, nvol, ncs[3];

	// grid-integration related functions
	void setup_grid (VOXGRID &g, int level);
	bool scan_gridline (VOXGRID &g, int x, int y, int z, int dir_idx, int ntri, const TriParam *pp);
	void analyse_grid (VOXGRID &g, double &vol, Vector &com, Vector &cs, Matrix &pmi);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CShipeditApp)
	public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CShipeditApp)
#ifndef __linux__
	afx_msg void OnLoad();
	afx_msg void OnSaveas();
	afx_msg void OnTranslate();
	afx_msg void OnRotate();
	afx_msg void OnZerolevel();
	afx_msg void OnVoxint();
	afx_msg void OnMergegrp();
	afx_msg void OnCalcnormal();
	afx_msg void OnScale();
	afx_msg void OnMirror();
#else // __linux__
	void OnLoad();
	void OnSaveas();
	void OnTranslate();
	void OnRotate();
	void OnZerolevel();
	void OnVoxint();
	void OnMergegrp();
	void OnCalcnormal();
	void OnScale();
	void OnMirror();
#endif // __linux__
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
	afx_msg void OnFileAddmesh();
#else // __linux__
	// DECLARE_MESSAGE_MAP: OnCommand
	void OnFileAddmesh();
#endif // __linux__
};


/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// GridintDlg dialog

#ifndef __linux__
class GridintDlg : public CDialog
#else // __linux__
class GridintDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	GridintDlg(CShipeditApp *_app, CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	GridintDlg(CShipeditApp *_app, QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(GridintDlg)
	enum { IDD = IDD_GRIDINT };
	int		m_GridDim;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(GridintDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:
	CShipeditApp *app;

	// Generated message map functions
	//{{AFX_MSG(GridintDlg)
#ifndef __linux__
	afx_msg void OnGridintStart();
	afx_msg void OnChangeGridintDim();
#else // __linux__
	void OnGridintStart();
	void OnChangeGridintDim();
#endif // __linux__
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	virtual BOOL OnCommand(int nID, int nCode); // DECLARE_MESSAGE_MAP: the map is a WM_COMMAND switch
#endif // __linux__
};
/////////////////////////////////////////////////////////////////////////////
// AddMeshDlg dialog

#ifndef __linux__
class AddMeshDlg : public CDialog
#else // __linux__
class AddMeshDlg : public ResDlg // CDialog: ResDlg (StdAfx.h)
#endif // __linux__
{
// Construction
public:
#ifndef __linux__
	AddMeshDlg(CWnd* pParent = NULL);   // standard constructor
#else // __linux__
	AddMeshDlg(QWidget* pParent = NULL);   // standard constructor
#endif // __linux__

// Dialog Data
	//{{AFX_DATA(AddMeshDlg)
	enum { IDD = IDD_MERGEOVERRD };
	int		m_AddMode;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(AddMeshDlg)
	protected:
#ifndef __linux__
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
#else // __linux__
	virtual void DoDataExchange(BOOL bSaveAndValidate);    // DDX/DDV support
#endif // __linux__
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(AddMeshDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
#ifndef __linux__
	DECLARE_MESSAGE_MAP()
#else // __linux__
	// DECLARE_MESSAGE_MAP: no entries
#endif // __linux__
};
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SHIPEDIT_H__820CEA84_B7CD_4458_8801_283B73C584D4__INCLUDED_)
