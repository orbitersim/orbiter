// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// TransformDlg.cpp : implementation file
//

#include "stdafx.h"
#include "Shipedit.h"
#include "TransformDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern CShipeditApp theApp;

/////////////////////////////////////////////////////////////////////////////
// TranslateDlg dialog


TranslateDlg::TranslateDlg(Mesh *_mesh, CWnd* pParent)
: CDialog(TranslateDlg::IDD, pParent), mesh(_mesh)
{
	//{{AFX_DATA_INIT(TranslateDlg)
	m_Translatex = 0.0f;
	m_Translatey = 0.0f;
	m_Translatez = 0.0f;
	//}}AFX_DATA_INIT
}


void TranslateDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(TranslateDlg)
	DDX_Text(pDX, IDC_TRANSLATEX, m_Translatex);
	DDX_Text(pDX, IDC_TRANSLATEY, m_Translatey);
	DDX_Text(pDX, IDC_TRANSLATEZ, m_Translatez);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(TranslateDlg, CDialog)
	//{{AFX_MSG_MAP(TranslateDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// TranslateDlg message handlers

void TranslateDlg::OnOK() 
{
	UpdateData();
	mesh->Translate (m_Translatex, m_Translatey, m_Translatez);
	CDialog::OnOK();
}
/////////////////////////////////////////////////////////////////////////////
// RotateDlg dialog


RotateDlg::RotateDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(RotateDlg::IDD, pParent), mesh(_mesh)
{
	//{{AFX_DATA_INIT(RotateDlg)
	m_Rotx = 0.0;
	m_Roty = 0.0;
	m_Rotz = 0.0;
	//}}AFX_DATA_INIT
}


void RotateDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(RotateDlg)
	DDX_Text(pDX, IDC_ROTX, m_Rotx);
	DDV_MinMaxDouble(pDX, m_Rotx, -360., 360.);
	DDX_Text(pDX, IDC_ROTY, m_Roty);
	DDV_MinMaxDouble(pDX, m_Roty, -360., 360.);
	DDX_Text(pDX, IDC_ROTZ, m_Rotz);
	DDV_MinMaxDouble(pDX, m_Rotz, -360., 360.);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(RotateDlg, CDialog)
	//{{AFX_MSG_MAP(RotateDlg)
	ON_BN_CLICKED(IDC_DO_ROTX, OnDoRotx)
	ON_BN_CLICKED(IDC_DO_ROTY, OnDoRoty)
	ON_BN_CLICKED(IDC_DO_ROTZ, OnDoRotz)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

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


ScaleDlg::ScaleDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(ScaleDlg::IDD, pParent), mesh(_mesh)
{
	//{{AFX_DATA_INIT(ScaleDlg)
	m_ScaleX = 1.0;
	m_ScaleY = 1.0;
	m_ScaleZ = 1.0;
	//}}AFX_DATA_INIT
}


void ScaleDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(ScaleDlg)
	DDX_Text(pDX, IDC_SCALEX, m_ScaleX);
	DDX_Text(pDX, IDC_SCALEY, m_ScaleY);
	DDX_Text(pDX, IDC_SCALEZ, m_ScaleZ);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(ScaleDlg, CDialog)
	//{{AFX_MSG_MAP(ScaleDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// ScaleDlg message handlers

void ScaleDlg::OnOK() 
{
	UpdateData();
	mesh->Scale ((float)m_ScaleX, (float)m_ScaleY, (float)m_ScaleZ);
	CDialog::OnOK();
}
/////////////////////////////////////////////////////////////////////////////
// ZerolevelDlg dialog


ZerolevelDlg::ZerolevelDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(ZerolevelDlg::IDD, pParent), mesh(_mesh)
{
	//{{AFX_DATA_INIT(ZerolevelDlg)
	m_Zlevel = 1e-5f;
	m_ResetVtx = FALSE;
	m_ResetNml = FALSE;
	m_ResetTex = FALSE;
	//}}AFX_DATA_INIT
}


void ZerolevelDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(ZerolevelDlg)
	DDX_Text(pDX, IDC_ZEROLEVEL, m_Zlevel);
	DDV_MinMaxFloat(pDX, m_Zlevel, 0.f, 1.f);
	DDX_Check(pDX, IDC_ZERO_VTX, m_ResetVtx);
	DDX_Check(pDX, IDC_ZERO_NML, m_ResetNml);
	DDX_Check(pDX, IDC_ZERO_TEX, m_ResetTex);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(ZerolevelDlg, CDialog)
	//{{AFX_MSG_MAP(ZerolevelDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

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
	CDialog::OnOK();
}
/////////////////////////////////////////////////////////////////////////////
// MergeDlg dialog


MergeDlg::MergeDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(MergeDlg::IDD, pParent), mesh(_mesh)
{
	//{{AFX_DATA_INIT(MergeDlg)
	m_Grp1 = 1;
	m_Grp2 = 1;
	m_Label1 = _T("");
	m_Label2 = _T("");
	//}}AFX_DATA_INIT
}


void MergeDlg::DoDataExchange(CDataExchange* pDX)
{
	UINT maxgrp = (UINT)mesh->nGroup(), maxgrp1 = maxgrp+1;
	char cbuf1[32], cbuf2[32];
	sprintf (cbuf1, "Merge group (1-%d)", maxgrp);
	sprintf (cbuf2, "with group (1-%d)", maxgrp);
	m_Label1 = _T(cbuf1);
	m_Label2 = _T(cbuf2);
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(MergeDlg)
	DDX_Text(pDX, IDC_MERGE_GRP1, m_Grp1);
	DDV_MinMaxUInt(pDX, m_Grp1, 1, maxgrp1);
	DDX_Text(pDX, IDC_MERGE_GRP2, m_Grp2);
	DDV_MinMaxUInt(pDX, m_Grp2, 1, maxgrp1);
	DDX_Text(pDX, IDC_MERGE_LABEL1, m_Label1);
	DDV_MaxChars(pDX, m_Label1, 32);
	DDX_Text(pDX, IDC_MERGE_LABEL2, m_Label2);
	DDV_MaxChars(pDX, m_Label2, 32);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(MergeDlg, CDialog)
	//{{AFX_MSG_MAP(MergeDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// MergeDlg message handlers

void MergeDlg::OnOK() 
{
	UpdateData();
	if (m_Grp1 != m_Grp2) {
		DWORD grp1 = m_Grp1-1, grp2 = m_Grp2-1;
		D3DVERTEX *vtx1, *vtx2, *vtx;
		WORD *idx1, *idx2, *idx;
		DWORD i, nvtx1, nvtx2, nidx1, nidx2, nvtx, nidx;
		mesh->GetGroup (grp1, vtx1, nvtx1, idx1, nidx1);
		mesh->GetGroup (grp2, vtx2, nvtx2, idx2, nidx2);
		nvtx = nvtx1 + nvtx2;
		nidx = nidx1 + nidx2;
		vtx = new D3DVERTEX[nvtx];
		idx = new WORD[nidx];
		memcpy (vtx, vtx1, nvtx1*sizeof(D3DVERTEX));
		memcpy (vtx+nvtx1, vtx2, nvtx2*sizeof(D3DVERTEX));
		memcpy (idx, idx1, nidx1*sizeof(WORD));
		memcpy (idx+nidx1, idx2, nidx2*sizeof(WORD));
		// adjust indices
		for (i = nidx1; i < nidx; i++)
			idx[i] += (WORD)nvtx1;
		mesh->DeleteGroup (grp1 < grp2 ? grp2 : grp1);
		mesh->DeleteGroup (grp1 < grp2 ? grp1 : grp2);
		mesh->AddGroup (vtx, nvtx, idx, nidx);
	}

	CDialog::OnOK();
}
/////////////////////////////////////////////////////////////////////////////
// NormalDlg dialog


NormalDlg::NormalDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(NormalDlg::IDD, pParent), mesh(_mesh)
{
	//{{AFX_DATA_INIT(NormalDlg)
	m_Selgrp = 0;
	m_Selvtx = 0;
	m_Group = 1;
	//}}AFX_DATA_INIT
}


void NormalDlg::DoDataExchange(CDataExchange* pDX)
{
	UINT maxgrp = (UINT)mesh->nGroup(), maxgrp1 = maxgrp+1;
	char cbuf[32];
	sprintf (cbuf, "Only for group (1-%d)", maxgrp);
	GetDlgItem(IDC_NML_SELONE)->SetWindowText (cbuf);
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(NormalDlg)
	DDX_Radio(pDX, IDC_NML_SELALL, m_Selgrp);
	DDX_Radio(pDX, IDC_NML_VTXALL, m_Selvtx);
	DDX_Text(pDX, IDC_NML_SELGRP, m_Group);
	DDV_MinMaxUInt(pDX, m_Group, 1, maxgrp);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(NormalDlg, CDialog)
	//{{AFX_MSG_MAP(NormalDlg)
	ON_BN_CLICKED(IDC_NML_SELALL, OnNmlSelall)
	ON_BN_CLICKED(IDC_NML_SELONE, OnNmlSelone)
	ON_BN_CLICKED(ID_NMLAPPLY, OnNmlapply)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// NormalDlg message handlers

void NormalDlg::OnNmlSelall() 
{
	GetDlgItem (IDC_NML_SELGRP)->EnableWindow (FALSE);
}

void NormalDlg::OnNmlSelone() 
{
	GetDlgItem (IDC_NML_SELGRP)->EnableWindow (TRUE);
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


MirrorDlg::MirrorDlg(Mesh *_mesh, CWnd* pParent /*=NULL*/)
	: CDialog(MirrorDlg::IDD, pParent), mesh(_mesh)
{
	//{{AFX_DATA_INIT(MirrorDlg)
	m_MirrorX = 0;
	//}}AFX_DATA_INIT
}


void MirrorDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(MirrorDlg)
	DDX_Radio(pDX, IDC_MIRRORX, m_MirrorX);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(MirrorDlg, CDialog)
	//{{AFX_MSG_MAP(MirrorDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

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
	CDialog::OnOK();
}
