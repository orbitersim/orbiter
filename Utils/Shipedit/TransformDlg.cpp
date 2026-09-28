// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// TransformDlg.cpp : implementation file
//

#ifndef __linux__
#include "stdafx.h"
#else // __linux__
#include "StdAfx.h"
#endif // __linux__
#include "Shipedit.h"
#ifndef __linux__
#include "TransformDlg.h"
#else // __linux__
#include "transformdlg.h"
#include <cstring>
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

extern CShipeditApp theApp;

/////////////////////////////////////////////////////////////////////////////
// TranslateDlg dialog


#ifndef __linux__
TranslateDlg::TranslateDlg(Mesh *_mesh, CWnd* pParent)
: CDialog(TranslateDlg::IDD, pParent), mesh(_mesh)
#else // __linux__
TranslateDlg::TranslateDlg(Mesh *_mesh, QWidget* pParent)
: ResDlg(TranslateDlg::IDD, pParent), mesh(_mesh)
#endif // __linux__
{
	//{{AFX_DATA_INIT(TranslateDlg)
	m_Translatex = 0.0f;
	m_Translatey = 0.0f;
	m_Translatez = 0.0f;
	//}}AFX_DATA_INIT
}


#ifndef __linux__
void TranslateDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void TranslateDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
#ifndef __linux__
	CDialog::DoDataExchange(pDX);
#else // __linux__
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(TranslateDlg)
#ifndef __linux__
	DDX_Text(pDX, IDC_TRANSLATEX, m_Translatex);
	DDX_Text(pDX, IDC_TRANSLATEY, m_Translatey);
	DDX_Text(pDX, IDC_TRANSLATEZ, m_Translatez);
#else // __linux__
	ExchangeText(bSaveAndValidate, IDC_TRANSLATEX, m_Translatex);
	ExchangeText(bSaveAndValidate, IDC_TRANSLATEY, m_Translatey);
	ExchangeText(bSaveAndValidate, IDC_TRANSLATEZ, m_Translatez);
#endif // __linux__
	//}}AFX_DATA_MAP
}


#ifndef __linux__
BEGIN_MESSAGE_MAP(TranslateDlg, CDialog)
	//{{AFX_MSG_MAP(TranslateDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
#else // __linux__
// BEGIN_MESSAGE_MAP(TranslateDlg): no entries, ResDlg::OnCommand calls OnOK/OnCancel
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// TranslateDlg message handlers

void TranslateDlg::OnOK() 
{
	UpdateData();
	mesh->Translate (m_Translatex, m_Translatey, m_Translatez);
#ifndef __linux__
	CDialog::OnOK();
#else // __linux__
	ResDlg::OnOK();
#endif // __linux__
}
/////////////////////////////////////////////////////////////////////////////
// RotateDlg dialog


#ifndef __linux__
RotateDlg::RotateDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(RotateDlg::IDD, pParent), mesh(_mesh)
#else // __linux__
RotateDlg::RotateDlg(Mesh *_mesh, QWidget* pParent /*=NULL*/)
	: ResDlg(RotateDlg::IDD, pParent), mesh(_mesh)
#endif // __linux__
{
	//{{AFX_DATA_INIT(RotateDlg)
	m_Rotx = 0.0;
	m_Roty = 0.0;
	m_Rotz = 0.0;
	//}}AFX_DATA_INIT
}


#ifndef __linux__
void RotateDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void RotateDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
#ifndef __linux__
	CDialog::DoDataExchange(pDX);
#else // __linux__
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(RotateDlg)
#ifndef __linux__
	DDX_Text(pDX, IDC_ROTX, m_Rotx);
	DDV_MinMaxDouble(pDX, m_Rotx, -360., 360.);
	DDX_Text(pDX, IDC_ROTY, m_Roty);
	DDV_MinMaxDouble(pDX, m_Roty, -360., 360.);
	DDX_Text(pDX, IDC_ROTZ, m_Rotz);
	DDV_MinMaxDouble(pDX, m_Rotz, -360., 360.);
#else // __linux__
	ExchangeText(bSaveAndValidate, IDC_ROTX, m_Rotx);
	ValidateMinMaxDouble(bSaveAndValidate, m_Rotx, -360., 360.);
	ExchangeText(bSaveAndValidate, IDC_ROTY, m_Roty);
	ValidateMinMaxDouble(bSaveAndValidate, m_Roty, -360., 360.);
	ExchangeText(bSaveAndValidate, IDC_ROTZ, m_Rotz);
	ValidateMinMaxDouble(bSaveAndValidate, m_Rotz, -360., 360.);
#endif // __linux__
	//}}AFX_DATA_MAP
}


#ifndef __linux__
BEGIN_MESSAGE_MAP(RotateDlg, CDialog)
#else // __linux__
// message map: WM_COMMAND from the controls, connected in ResDlg (oapiConnectDlgCommands)
BOOL RotateDlg::OnCommand(int nID, int nCode)
{
#endif // __linux__
	//{{AFX_MSG_MAP(RotateDlg)
#ifndef __linux__
	ON_BN_CLICKED(IDC_DO_ROTX, OnDoRotx)
	ON_BN_CLICKED(IDC_DO_ROTY, OnDoRoty)
	ON_BN_CLICKED(IDC_DO_ROTZ, OnDoRotz)
#else // __linux__
	if (nCode == RESN_CLICKED) switch (nID) { // ON_BN_CLICKED
	case IDC_DO_ROTX:  OnDoRotx(); return TRUE;
	case IDC_DO_ROTY:  OnDoRoty(); return TRUE;
	case IDC_DO_ROTZ:  OnDoRotz(); return TRUE;
	}
#endif // __linux__
	//}}AFX_MSG_MAP
#ifndef __linux__
END_MESSAGE_MAP()
#else // __linux__
	return ResDlg::OnCommand(nID, nCode);
}
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// RotateDlg message handlers

void RotateDlg::OnDoRotx() 
{
	UpdateData();
	mesh->Rotate (mesh->ROTATE_X, (float)(RAD*m_Rotx));
}

void RotateDlg::OnDoRoty() 
{
	UpdateData();
	mesh->Rotate (mesh->ROTATE_Y, (float)(RAD*m_Roty));
}

void RotateDlg::OnDoRotz() 
{
	UpdateData();
	mesh->Rotate (mesh->ROTATE_Z, (float)(RAD*m_Rotz));
}
/////////////////////////////////////////////////////////////////////////////
// ScaleDlg dialog


#ifndef __linux__
ScaleDlg::ScaleDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(ScaleDlg::IDD, pParent), mesh(_mesh)
#else // __linux__
ScaleDlg::ScaleDlg(Mesh *_mesh, QWidget* pParent /*=NULL*/)
	: ResDlg(ScaleDlg::IDD, pParent), mesh(_mesh)
#endif // __linux__
{
	//{{AFX_DATA_INIT(ScaleDlg)
	m_ScaleX = 1.0;
	m_ScaleY = 1.0;
	m_ScaleZ = 1.0;
	//}}AFX_DATA_INIT
}


#ifndef __linux__
void ScaleDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void ScaleDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
#ifndef __linux__
	CDialog::DoDataExchange(pDX);
#else // __linux__
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(ScaleDlg)
#ifndef __linux__
	DDX_Text(pDX, IDC_SCALEX, m_ScaleX);
	DDX_Text(pDX, IDC_SCALEY, m_ScaleY);
	DDX_Text(pDX, IDC_SCALEZ, m_ScaleZ);
#else // __linux__
	ExchangeText(bSaveAndValidate, IDC_SCALEX, m_ScaleX);
	ExchangeText(bSaveAndValidate, IDC_SCALEY, m_ScaleY);
	ExchangeText(bSaveAndValidate, IDC_SCALEZ, m_ScaleZ);
#endif // __linux__
	//}}AFX_DATA_MAP
}


#ifndef __linux__
BEGIN_MESSAGE_MAP(ScaleDlg, CDialog)
	//{{AFX_MSG_MAP(ScaleDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
#else // __linux__
// BEGIN_MESSAGE_MAP(ScaleDlg): no entries, ResDlg::OnCommand calls OnOK/OnCancel
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// ScaleDlg message handlers

void ScaleDlg::OnOK() 
{
	UpdateData();
	mesh->Scale ((float)m_ScaleX, (float)m_ScaleY, (float)m_ScaleZ);
#ifndef __linux__
	CDialog::OnOK();
#else // __linux__
	ResDlg::OnOK();
#endif // __linux__
}
/////////////////////////////////////////////////////////////////////////////
// ZerolevelDlg dialog


#ifndef __linux__
ZerolevelDlg::ZerolevelDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(ZerolevelDlg::IDD, pParent), mesh(_mesh)
#else // __linux__
ZerolevelDlg::ZerolevelDlg(Mesh *_mesh, QWidget* pParent /*=NULL*/)
	: ResDlg(ZerolevelDlg::IDD, pParent), mesh(_mesh)
#endif // __linux__
{
	//{{AFX_DATA_INIT(ZerolevelDlg)
	m_Zlevel = 1e-5f;
	m_ResetVtx = FALSE;
	m_ResetNml = FALSE;
	m_ResetTex = FALSE;
	//}}AFX_DATA_INIT
}


#ifndef __linux__
void ZerolevelDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void ZerolevelDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
#ifndef __linux__
	CDialog::DoDataExchange(pDX);
#else // __linux__
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(ZerolevelDlg)
#ifndef __linux__
	DDX_Text(pDX, IDC_ZEROLEVEL, m_Zlevel);
	DDV_MinMaxFloat(pDX, m_Zlevel, 0.f, 1.f);
	DDX_Check(pDX, IDC_ZERO_VTX, m_ResetVtx);
	DDX_Check(pDX, IDC_ZERO_NML, m_ResetNml);
	DDX_Check(pDX, IDC_ZERO_TEX, m_ResetTex);
#else // __linux__
	ExchangeText(bSaveAndValidate, IDC_ZEROLEVEL, m_Zlevel);
	ValidateMinMaxFloat(bSaveAndValidate, m_Zlevel, 0.f, 1.f);
	ExchangeCheck(bSaveAndValidate, IDC_ZERO_VTX, m_ResetVtx);
	ExchangeCheck(bSaveAndValidate, IDC_ZERO_NML, m_ResetNml);
	ExchangeCheck(bSaveAndValidate, IDC_ZERO_TEX, m_ResetTex);
#endif // __linux__
	//}}AFX_DATA_MAP
}


#ifndef __linux__
BEGIN_MESSAGE_MAP(ZerolevelDlg, CDialog)
	//{{AFX_MSG_MAP(ZerolevelDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
#else // __linux__
// BEGIN_MESSAGE_MAP(ZerolevelDlg): no entries, ResDlg::OnCommand calls OnOK/OnCancel
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// ZerolevelDlg message handlers

void ZerolevelDlg::OnOK() 
{
	UpdateData();
	int which = 0;
	if (m_ResetVtx) which |= 1;
	if (m_ResetNml) which |= 2;
	if (m_ResetTex) which |= 4;
	mesh->ZeroThreshold (m_Zlevel, which);
#ifndef __linux__
	CDialog::OnOK();
#else // __linux__
	ResDlg::OnOK();
#endif // __linux__
}
/////////////////////////////////////////////////////////////////////////////
// MergeDlg dialog


#ifndef __linux__
MergeDlg::MergeDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(MergeDlg::IDD, pParent), mesh(_mesh)
#else // __linux__
MergeDlg::MergeDlg(Mesh *_mesh, QWidget* pParent /*=NULL*/)
	: ResDlg(MergeDlg::IDD, pParent), mesh(_mesh)
#endif // __linux__
{
	//{{AFX_DATA_INIT(MergeDlg)
	m_Grp1 = 1;
	m_Grp2 = 1;
#ifndef __linux__
	m_Label1 = _T("");
	m_Label2 = _T("");
#else // __linux__
	m_Label1 = "";
	m_Label2 = "";
#endif // __linux__
	//}}AFX_DATA_INIT
}


#ifndef __linux__
void MergeDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void MergeDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
	UINT maxgrp = (UINT)mesh->nGroup(), maxgrp1 = maxgrp+1;
	char cbuf1[32], cbuf2[32];
	sprintf (cbuf1, "Merge group (1-%d)", maxgrp);
	sprintf (cbuf2, "with group (1-%d)", maxgrp);
#ifndef __linux__
	m_Label1 = _T(cbuf1);
	m_Label2 = _T(cbuf2);
	CDialog::DoDataExchange(pDX);
#else // __linux__
	m_Label1 = cbuf1;
	m_Label2 = cbuf2;
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(MergeDlg)
#ifndef __linux__
	DDX_Text(pDX, IDC_MERGE_GRP1, m_Grp1);
	DDV_MinMaxUInt(pDX, m_Grp1, 1, maxgrp1);
	DDX_Text(pDX, IDC_MERGE_GRP2, m_Grp2);
	DDV_MinMaxUInt(pDX, m_Grp2, 1, maxgrp1);
	DDX_Text(pDX, IDC_MERGE_LABEL1, m_Label1);
	DDV_MaxChars(pDX, m_Label1, 32);
	DDX_Text(pDX, IDC_MERGE_LABEL2, m_Label2);
	DDV_MaxChars(pDX, m_Label2, 32);
#else // __linux__
	ExchangeText(bSaveAndValidate, IDC_MERGE_GRP1, m_Grp1);
	ValidateMinMaxUInt(bSaveAndValidate, m_Grp1, 1, maxgrp1);
	ExchangeText(bSaveAndValidate, IDC_MERGE_GRP2, m_Grp2);
	ValidateMinMaxUInt(bSaveAndValidate, m_Grp2, 1, maxgrp1);
	ExchangeText(bSaveAndValidate, IDC_MERGE_LABEL1, m_Label1);
	ValidateMaxChars(bSaveAndValidate, m_Label1, 32);
	ExchangeText(bSaveAndValidate, IDC_MERGE_LABEL2, m_Label2);
	ValidateMaxChars(bSaveAndValidate, m_Label2, 32);
#endif // __linux__
	//}}AFX_DATA_MAP
}


#ifndef __linux__
BEGIN_MESSAGE_MAP(MergeDlg, CDialog)
	//{{AFX_MSG_MAP(MergeDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
#else // __linux__
// BEGIN_MESSAGE_MAP(MergeDlg): no entries, ResDlg::OnCommand calls OnOK/OnCancel
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// MergeDlg message handlers

void MergeDlg::OnOK() 
{
	UpdateData();
	if (m_Grp1 != m_Grp2) {
		DWORD grp1 = m_Grp1-1, grp2 = m_Grp2-1;
#ifndef __linux__
		D3DVERTEX *vtx1, *vtx2, *vtx;
#else // __linux__
		NTVERTEX *vtx1, *vtx2, *vtx;
#endif // __linux__
		WORD *idx1, *idx2, *idx;
		DWORD i, nvtx1, nvtx2, nidx1, nidx2, nvtx, nidx;
		mesh->GetGroup (grp1, vtx1, nvtx1, idx1, nidx1);
		mesh->GetGroup (grp2, vtx2, nvtx2, idx2, nidx2);
		nvtx = nvtx1 + nvtx2;
		nidx = nidx1 + nidx2;
#ifndef __linux__
		vtx = new D3DVERTEX[nvtx];
#else // __linux__
		vtx = new NTVERTEX[nvtx];
#endif // __linux__
		idx = new WORD[nidx];
#ifndef __linux__
		memcpy (vtx, vtx1, nvtx1*sizeof(D3DVERTEX));
		memcpy (vtx+nvtx1, vtx2, nvtx2*sizeof(D3DVERTEX));
#else // __linux__
		memcpy (vtx, vtx1, nvtx1*sizeof(NTVERTEX));
		memcpy (vtx+nvtx1, vtx2, nvtx2*sizeof(NTVERTEX));
#endif // __linux__
		memcpy (idx, idx1, nidx1*sizeof(WORD));
		memcpy (idx+nidx1, idx2, nidx2*sizeof(WORD));
		// adjust indices
		for (i = nidx1; i < nidx; i++)
			idx[i] += (WORD)nvtx1;
		mesh->DeleteGroup (grp1 < grp2 ? grp2 : grp1);
		mesh->DeleteGroup (grp1 < grp2 ? grp1 : grp2);
		mesh->AddGroup (vtx, nvtx, idx, nidx);
	}

#ifndef __linux__
	CDialog::OnOK();
#else // __linux__
	ResDlg::OnOK();
#endif // __linux__
}
/////////////////////////////////////////////////////////////////////////////
// NormalDlg dialog


#ifndef __linux__
NormalDlg::NormalDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(NormalDlg::IDD, pParent), mesh(_mesh)
#else // __linux__
NormalDlg::NormalDlg(Mesh *_mesh, QWidget* pParent /*=NULL*/)
	: ResDlg(NormalDlg::IDD, pParent), mesh(_mesh)
#endif // __linux__
{
	//{{AFX_DATA_INIT(NormalDlg)
	m_Selgrp = 0;
	m_Selvtx = 0;
	m_Group = 1;
	//}}AFX_DATA_INIT
}


#ifndef __linux__
void NormalDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void NormalDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
	UINT maxgrp = (UINT)mesh->nGroup(), maxgrp1 = maxgrp+1;
	char cbuf[32];
	sprintf (cbuf, "Only for group (1-%d)", maxgrp);
#ifndef __linux__
	GetDlgItem(IDC_NML_SELONE)->SetWindowText (cbuf);
	CDialog::DoDataExchange(pDX);
#else // __linux__
	oapiSetDlgText (GetDlgItem (IDC_NML_SELONE), cbuf);
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(NormalDlg)
#ifndef __linux__
	DDX_Radio(pDX, IDC_NML_SELALL, m_Selgrp);
	DDX_Radio(pDX, IDC_NML_VTXALL, m_Selvtx);
	DDX_Text(pDX, IDC_NML_SELGRP, m_Group);
	DDV_MinMaxUInt(pDX, m_Group, 1, maxgrp);
#else // __linux__
	ExchangeRadio(bSaveAndValidate, IDC_NML_SELALL, m_Selgrp);
	ExchangeRadio(bSaveAndValidate, IDC_NML_VTXALL, m_Selvtx);
	ExchangeText(bSaveAndValidate, IDC_NML_SELGRP, m_Group);
	ValidateMinMaxUInt(bSaveAndValidate, m_Group, 1, maxgrp);
#endif // __linux__
	//}}AFX_DATA_MAP
}


#ifndef __linux__
BEGIN_MESSAGE_MAP(NormalDlg, CDialog)
#else // __linux__
// message map: WM_COMMAND from the controls, connected in ResDlg (oapiConnectDlgCommands)
BOOL NormalDlg::OnCommand(int nID, int nCode)
{
#endif // __linux__
	//{{AFX_MSG_MAP(NormalDlg)
#ifndef __linux__
	ON_BN_CLICKED(IDC_NML_SELALL, OnNmlSelall)
	ON_BN_CLICKED(IDC_NML_SELONE, OnNmlSelone)
	ON_BN_CLICKED(ID_NMLAPPLY, OnNmlapply)
#else // __linux__
	if (nCode == RESN_CLICKED) switch (nID) { // ON_BN_CLICKED
	case IDC_NML_SELALL:  OnNmlSelall(); return TRUE;
	case IDC_NML_SELONE:  OnNmlSelone(); return TRUE;
	case ID_NMLAPPLY:     OnNmlapply(); return TRUE;
	}
#endif // __linux__
	//}}AFX_MSG_MAP
#ifndef __linux__
END_MESSAGE_MAP()
#else // __linux__
	return ResDlg::OnCommand(nID, nCode);
}
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// NormalDlg message handlers

void NormalDlg::OnNmlSelall() 
{
#ifndef __linux__
	GetDlgItem (IDC_NML_SELGRP)->EnableWindow (FALSE);
#else // __linux__
	GetDlgItem (IDC_NML_SELGRP)->setEnabled (FALSE);
#endif // __linux__
}

void NormalDlg::OnNmlSelone() 
{
#ifndef __linux__
	GetDlgItem (IDC_NML_SELGRP)->EnableWindow (TRUE);
#else // __linux__
	GetDlgItem (IDC_NML_SELGRP)->setEnabled (TRUE);
#endif // __linux__
}

void NormalDlg::OnNmlapply() 
{
	UINT g1, g2, g;
	bool missing_only;

	UpdateData();
	if (m_Selgrp) g1 = m_Group-1, g2 = g1+1;
	else          g1 = 0, g2 = mesh->nGroup();
	missing_only = m_Selvtx == 1;
	for (g = g1; g < g2; g++)
		mesh->CalcNormals (g, missing_only);
	theApp.InitMesh();
}

/////////////////////////////////////////////////////////////////////////////
// MirrorDlg dialog


#ifndef __linux__
MirrorDlg::MirrorDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(MirrorDlg::IDD, pParent), mesh(_mesh)
#else // __linux__
MirrorDlg::MirrorDlg(Mesh *_mesh, QWidget* pParent /*=NULL*/)
	: ResDlg(MirrorDlg::IDD, pParent), mesh(_mesh)
#endif // __linux__
{
	//{{AFX_DATA_INIT(MirrorDlg)
	m_MirrorX = 0;
	//}}AFX_DATA_INIT
}


#ifndef __linux__
void MirrorDlg::DoDataExchange(CDataExchange* pDX)
#else // __linux__
void MirrorDlg::DoDataExchange(BOOL bSaveAndValidate)
#endif // __linux__
{
#ifndef __linux__
	CDialog::DoDataExchange(pDX);
#else // __linux__
	ResDlg::DoDataExchange(bSaveAndValidate);
#endif // __linux__
	//{{AFX_DATA_MAP(MirrorDlg)
#ifndef __linux__
	DDX_Radio(pDX, IDC_MIRRORX, m_MirrorX);
#else // __linux__
	ExchangeRadio(bSaveAndValidate, IDC_MIRRORX, m_MirrorX);
#endif // __linux__
	//}}AFX_DATA_MAP
}


#ifndef __linux__
BEGIN_MESSAGE_MAP(MirrorDlg, CDialog)
	//{{AFX_MSG_MAP(MirrorDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
#else // __linux__
// BEGIN_MESSAGE_MAP(MirrorDlg): no entries, ResDlg::OnCommand calls OnOK/OnCancel
#endif // __linux__

/////////////////////////////////////////////////////////////////////////////
// MirrorDlg message handlers

void MirrorDlg::OnOK() 
{
	UpdateData();
	switch (m_MirrorX) {
	case 0: mesh->Mirror (Mesh::MIRROR_X); break;
	case 1: mesh->Mirror (Mesh::MIRROR_Y); break;
	case 2: mesh->Mirror (Mesh::MIRROR_Z); break;
	}
#ifndef __linux__
	CDialog::OnOK();
#else // __linux__
	ResDlg::OnOK();
#endif // __linux__
}
