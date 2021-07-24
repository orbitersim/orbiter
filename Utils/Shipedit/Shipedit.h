// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Shipedit.h : main header file for the SHIPEDIT application
//

#if !defined(AFX_SHIPEDIT_H__820CEA84_B7CD_4458_8801_283B73C584D4__INCLUDED_)
#define AFX_SHIPEDIT_H__820CEA84_B7CD_4458_8801_283B73C584D4__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include <d3d.h>
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

class CShipeditApp : public CWinApp {
	friend class CShipeditDlg;
	friend class GridintDlg;
public:
	CShipeditApp ();
	void InitMesh ();
	Mesh mesh;

private:
	void ProcessPackage ();
	BOOL OnIdle (LONG lCount);
	CShipeditDlg *m_pMainDlg;
	DWORD ngrp, nvtx, nidx, ntri;  // mesh groups
	D3DVERTEX *vtx;
	WORD *idx;
	TriParam *pp;
	D3DVECTOR bbmin, bbmax;        // bounding box
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
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	afx_msg void OnFileAddmesh();
};


/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// GridintDlg dialog

class GridintDlg : public CDialog
{
// Construction
public:
	GridintDlg(CShipeditApp *_app, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(GridintDlg)
	enum { IDD = IDD_GRIDINT };
	int		m_GridDim;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(GridintDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CShipeditApp *app;

	// Generated message map functions
	//{{AFX_MSG(GridintDlg)
	afx_msg void OnGridintStart();
	afx_msg void OnChangeGridintDim();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
/////////////////////////////////////////////////////////////////////////////
// AddMeshDlg dialog

class AddMeshDlg : public CDialog
{
// Construction
public:
	AddMeshDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(AddMeshDlg)
	enum { IDD = IDD_MERGEOVERRD };
	int		m_AddMode;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(AddMeshDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(AddMeshDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SHIPEDIT_H__820CEA84_B7CD_4458_8801_283B73C584D4__INCLUDED_)
