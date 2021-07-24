// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Shipedit.cpp : Defines the class behaviors for the application.
//

#include <fstream>
#include "stdafx.h"
#include "Shipedit.h"
#include "ShipeditDlg.h"
#include "TransformDlg.h"

using namespace std;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

// prototypes
void srand2();
double rand2 ();
bool inside (int ntri, const TriParam *pp, const D3DVECTOR &pos);
bool xsect (int axis, int ntri, const TriParam *pp, const D3DVECTOR &pos);
bool intersect (const D3DVECTOR &pos, const D3DVECTOR &dir, const TriParam &pp, D3DVECTOR &X, float &t, bool half);
bool in_tri (const D3DVECTOR &X, const TriParam &pp);
bool in_tri_proj (const D3DVECTOR &X, const TriParam &pp, int dir_idx);
void setup_grid (VOXGRID &g, int level);

/////////////////////////////////////////////////////////////////////////////
// CShipeditApp

BEGIN_MESSAGE_MAP(CShipeditApp, CWinApp)
	//{{AFX_MSG_MAP(CShipeditApp)
	ON_COMMAND(MID_LOAD, OnLoad)
	ON_COMMAND(MID_SAVEAS, OnSaveas)
	ON_COMMAND(MID_TRANSLATE, OnTranslate)
	ON_COMMAND(MID_ROTATE, OnRotate)
	ON_COMMAND(MID_ZEROLEVEL, OnZerolevel)
	ON_COMMAND(MID_VOXINT, OnVoxint)
	ON_COMMAND(MID_MERGEGRP, OnMergegrp)
	ON_COMMAND(MID_CALCNORMAL, OnCalcnormal)
	ON_COMMAND(MID_SCALE, OnScale)
	ON_COMMAND(MID_MIRROR, OnMirror)
	//}}AFX_MSG_MAP
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CShipeditApp construction

CShipeditApp::CShipeditApp()
{
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CShipeditApp object

CShipeditApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CShipeditApp initialization

BOOL CShipeditApp::InitInstance()
{
	// Standard initialization

	m_pMainDlg = new CShipeditDlg (this);
	m_pMainWnd = m_pMainDlg;
	m_pMainDlg->Create (IDD_SHIPEDIT_DIALOG);

	ngrp = nvtx = nidx = ntri = 0;
	flushcount = 0;
	bBackgroundOp = FALSE;
	srand2();
	return TRUE;
}

int CShipeditApp::ExitInstance()
{
	MessageBeep (-1);
	delete m_pMainDlg;
	if (nvtx) delete []vtx;
	if (nidx) delete []idx;
	if (ntri) delete []pp;
	return 0;
}

void CShipeditApp::OnLoad() 
{
	int addmode = 0;
	if (mesh.nGroup() > 0) { // mesh already loaded - ask for merge or overwrite
		AddMeshDlg dlg;
		if (dlg.DoModal() != IDOK) return;
		addmode = dlg.m_AddMode;
	}
	CFileDialog dlg (TRUE, "*.msh", NULL, OFN_FILEMUSTEXIST,
		"Mesh files (*.msh)|*.msh|All files (*.*)|*.*||",
		m_pMainWnd);
	if (dlg.DoModal() == IDOK) {
		CString path = dlg.GetPathName();
		ifstream ifs (path);
		if (ifs) {
			if (addmode) {
				Mesh mesh2;
				ifs >> mesh2;
				mesh.AddMesh (mesh2);
			} else {
				ifs >> mesh;
			}
			InitMesh ();
		}
	}
}

void CShipeditApp::OnSaveas() 
{
	CFileDialog dlg (FALSE, "*.msh", NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		"Mesh files (*.msh)|*.msh|All files (*.*)|*.*||",
		m_pMainWnd);
	if (dlg.DoModal() == IDOK) {
		CString path = dlg.GetPathName();
		ofstream ofs (path);
		if (ofs) ofs << mesh;
	}
}

void CShipeditApp::OnVoxint() 
{
	if (!ngrp) return; // have mesh?
	GridintDlg dlg (this);
	dlg.DoModal();
}

void CShipeditApp::OnTranslate() 
{
	if (!ngrp) return; // have mesh?
	TranslateDlg dlg (&mesh);
	if (dlg.DoModal() == IDOK)
		InitMesh();
}


void CShipeditApp::OnRotate() 
{
	if (!ngrp) return; // have mesh?
	RotateDlg dlg (&mesh);
	dlg.DoModal();
	InitMesh();
}


void CShipeditApp::OnMirror() 
{
	if (!ngrp) return; // have mesh?
	MirrorDlg dlg (&mesh);
	if (dlg.DoModal() == IDOK)
		InitMesh();
}

void CShipeditApp::OnScale() 
{
	if (!ngrp) return; // have mesh?
	ScaleDlg dlg (&mesh);
	dlg.DoModal();
	InitMesh();
}


void CShipeditApp::OnMergegrp() 
{
	if (!ngrp) return; // have mesh?
	MergeDlg dlg (&mesh);
	if (dlg.DoModal() == IDOK)
		InitMesh();
}

void CShipeditApp::OnCalcnormal() 
{
	if (!ngrp) return; // have mesh?
	NormalDlg dlg (&mesh);
	dlg.DoModal();
}

void CShipeditApp::OnZerolevel() 
{
	ZerolevelDlg dlg (&mesh);
	if (dlg.DoModal() == IDOK)
		InitMesh();
}

void CShipeditApp::InitMesh ()
{
	DWORD g, n, i, j;

	// delete current mesh
	if (nvtx) delete []vtx;
	if (nidx) delete []idx;
	if (ntri) delete []pp;

	ngrp = mesh.nGroup();
	nvtx = nidx = 0;

//#define ISOLATE_REGION
#ifdef ISOLATE_REGION
	ofstream ofs("region.dat");
	ofstream mshf("region.msh");
	mshf << "MSHX1\nGROUPS ??\n";
#endif

	for (g = 0; g < ngrp; g++) {
		D3DVERTEX *gvtx;
		WORD *gidx;
		DWORD gnvtx, gnidx;
		mesh.GetGroup (g, gvtx, gnvtx, gidx, gnidx);
		nvtx += gnvtx;
		nidx += gnidx;
#ifdef ISOLATE_REGION
		for (i = 0; i < gnvtx; i++) {
			//double dx = gvtx[i].x - 2.7;
			//double dy = gvtx[i].y + 2.9;
			//double dz = gvtx[i].z + 8.45;
			//double dst = sqrt (dx*dx + dy*dy + dz*dz);
			//if (dst < 0.2)
			if (g == 27 && gvtx[i].z > 10.65 && gvtx[i].z < 10.85)
				ofs << "G=" << g << ", vtx=" << i << " (" << gvtx[i].x << ' ' << gvtx[i].y << ' ' << gvtx[i].z << ')' << endl;
		}
#ifdef UNDEF
		int tri_entries = 0;
		for (i = 0; i < gnidx/3; i++) {
			WORD nd[3] = {gidx[i*3],gidx[i*3+1],gidx[i*3+2]};
			for (j = 0; j < 3; j++) {
				if (g != 27) break;
				if (gvtx[nd[j]].z > 15.75) break;
				//if (gvtx[nd[j]].x < -1.2 || gvtx[nd[j]].x > 1) break;
				//if (gvtx[nd[j]].y < -4 || gvtx[nd[j]].y > 0.1) break;
				//if (gvtx[nd[j]].z < 8.5 || gvtx[nd[j]].z > 10.9) break;
			}
			if (j == 3) tri_entries++;
		}
		if (tri_entries) {
			mshf << "MATERIAL 0\nTEXTURE 0\n";
			mshf << "GEOM " << gnvtx << ' ' << tri_entries << endl;
			for (i = 0; i < gnvtx; i++)
				mshf << gvtx[i].x << ' ' << gvtx[i].y << ' ' << gvtx[i].z << ' '
					 << gvtx[i].nx << ' ' << gvtx[i].ny << ' ' << gvtx[i].nz << ' '
					 << gvtx[i].tu << ' ' << gvtx[i].tv << endl;
			for (i = 0; i < gnidx/3; i++) {
				WORD nd[3] = {gidx[i*3],gidx[i*3+1],gidx[i*3+2]};
				for (j = 0; j < 3; j++) {
					if (g != 27) break;
					if (gvtx[nd[j]].z > 15.75) break;
					//if (gvtx[nd[j]].x < -1.2 || gvtx[nd[j]].x > 1) break;
					//if (gvtx[nd[j]].y < -4 || gvtx[nd[j]].y > 0.1) break;
					//if (gvtx[nd[j]].z < 8.5 || gvtx[nd[j]].z > 10.9) break;
				}
				if (j == 3) {
					ofs << "g=" << g << ", ln=" << i << ": " << nd[0] << ' ' << nd[1] << ' ' << nd[2] << endl;
					mshf << nd[0] << ' ' << nd[1] << ' ' << nd[2] << endl;
				}
			}
		}
#endif
#endif
			
	}
	ntri = nidx/3;
	vtx = new D3DVERTEX[nvtx];
	idx = new WORD[nidx];
	pp  = new TriParam[ntri];

	nvtx = nidx = 0;
	for (g = 0; g < ngrp; g++) {
		D3DVERTEX *gvtx;
		WORD *gidx;
		DWORD gnvtx, gnidx;
		mesh.GetGroup (g, gvtx, gnvtx, gidx, gnidx);
		for (i = 0; i < gnidx; i++)
			idx[nidx+i] = gidx[i] + (WORD)nvtx;
		for (i = 0; i < gnvtx; i++)
			vtx[nvtx+i] = gvtx[i];
		nvtx += gnvtx;
		nidx += gnidx;
	}

	// bounding box
	bbmin.x = bbmin.y = bbmin.z = 1e10;
	bbmax.x = bbmax.y = bbmax.z = -1e10;
	for (n = 0; n < nvtx; n++) {
		float x = vtx[n].x;
		float y = vtx[n].y;
		float z = vtx[n].z;
		if (x < bbmin.x) bbmin.x = x;
		if (y < bbmin.y) bbmin.y = y;
		if (z < bbmin.z) bbmin.z = z;
		if (x > bbmax.x) bbmax.x = x;
		if (y > bbmax.y) bbmax.y = y;
		if (z > bbmax.z) bbmax.z = z;
	}
	bbvol = (bbmax.x-bbmin.x)*(bbmax.y-bbmin.y)*(bbmax.z-bbmin.z);
	bbcs.x = (bbmax.y-bbmin.y)*(bbmax.z-bbmin.z);
	bbcs.y = (bbmax.x-bbmin.x)*(bbmax.z-bbmin.z);
	bbcs.z = (bbmax.x-bbmin.x)*(bbmax.y-bbmin.y);

	// generate plane parameters E: ax+by+cz+d=0 for all triangles
	for (i = j = 0; i < ntri; i++) {
		pp[i].x1 = vtx[idx[j]].x, pp[i].y1 = vtx[idx[j]].y, pp[i].z1 = vtx[idx[j]].z;
		j++;
		pp[i].x2 = vtx[idx[j]].x, pp[i].y2 = vtx[idx[j]].y, pp[i].z2 = vtx[idx[j]].z;
		j++;
		pp[i].x3 = vtx[idx[j]].x, pp[i].y3 = vtx[idx[j]].y, pp[i].z3 = vtx[idx[j]].z;
		j++;
		pp[i].a = pp[i].y1*(pp[i].z2-pp[i].z3) - pp[i].z1*(pp[i].y2-pp[i].y3) + (pp[i].y2*pp[i].z3 - pp[i].y3*pp[i].z2);
		pp[i].b = -(pp[i].x1*(pp[i].z2-pp[i].z3) - pp[i].z1*(pp[i].x2-pp[i].x3) + (pp[i].x2*pp[i].z3 - pp[i].x3*pp[i].z2));
		pp[i].c = pp[i].x1*(pp[i].y2-pp[i].y3) - pp[i].y1*(pp[i].x2-pp[i].x3) + (pp[i].x2*pp[i].y3 - pp[i].x3*pp[i].y2);
		pp[i].d = -(pp[i].x1*(pp[i].y2*pp[i].z3-pp[i].y3*pp[i].z2) - pp[i].y1*(pp[i].x2*pp[i].z3-pp[i].x3*pp[i].z2) + pp[i].z1*(pp[i].x2*pp[i].y3-pp[i].x3*pp[i].y2));

		Vector dr, a;
		dr.Set (pp[i].x1-pp[i].x2, pp[i].y1-pp[i].y2, pp[i].z1-pp[i].z2);
		a.Set  (pp[i].x3-pp[i].x2, pp[i].y3-pp[i].y2, pp[i].z3-pp[i].z2);
		pp[i].d1 = (float)(dotp (dr, a)/a.length2());
		dr.Set (pp[i].x2-pp[i].x3, pp[i].y2-pp[i].y3, pp[i].z2-pp[i].z3);
		a.Set  (pp[i].x1-pp[i].x3, pp[i].y1-pp[i].y3, pp[i].z1-pp[i].z3);
		pp[i].d2 = (float)(dotp (dr, a)/a.length2());
		dr.Set (pp[i].x3-pp[i].x1, pp[i].y3-pp[i].y1, pp[i].z3-pp[i].z1);
		a.Set  (pp[i].x2-pp[i].x1, pp[i].y2-pp[i].y1, pp[i].z2-pp[i].z1);
		pp[i].d3 = (float)(dotp (dr, a)/a.length2());
	}

	vol = 0.0;
	cg.Set(0,0,0); cg_base.Set(0,0,0); cg_add.Set(0,0,0);
	cs.Set(0,0,0);
	J.Set(0,0,0,0,0,0,0,0,0); J_base.Set(J); J_add.Set(J);
	nop = 0, nvol = 0;
	for (i = 0; i < 3; i++) ncs[i] = 0;
	flushcount = 0;
	bBackgroundOp = false;

	m_pMainDlg->Refresh();
}

void CShipeditApp::ProcessPackage ()
{
	int i, j, n;
	Vector cg_tmp;
	Matrix J_tmp;

	for (i = n = 0; i < 1000; i++) {
		D3DVECTOR pos;
		pos.x = (float)rand2() * (bbmax.x-bbmin.x) + bbmin.x;
		pos.y = (float)rand2() * (bbmax.y-bbmin.y) + bbmin.y;
		pos.z = (float)rand2() * (bbmax.z-bbmin.z) + bbmin.z;
		if (inside (ntri, pp, pos)) {
			n++;
			double x = (double)pos.x, y = (double)pos.y, z = (double)pos.z;
			cg_tmp.x += x;
			cg_tmp.y += y;
			cg_tmp.z += z;
			J_tmp.m11 += y*y + z*z;
			J_tmp.m12 += x*y;
			J_tmp.m13 += x*z;
			J_tmp.m22 += x*x + z*z;
			J_tmp.m23 += y*z;
			J_tmp.m33 += x*x + y*y;
		}
	}
	nop += i;
	nvol += n;
	cg_add += cg_tmp;
	J_add += J_tmp;
	if (!(++flushcount % 1000)) {
		cg_base += cg_add;  cg_add.Set(0,0,0);
		J_base  += J_add;   J_add.Set (0,0,0,0,0,0,0,0,0);
	}
	vol = bbvol * (double)nvol / (double)nop;
	cg = (cg_base+cg_add) / (double)nvol;
	for (i = 0; i < 9; i++) J.data[i] = (J_base.data[i]+J_add.data[i])/(double)nvol;

	for (i = 0; i < 1000; i++) {
		D3DVECTOR pos;
		pos.x = (float)rand2() * (bbmax.x-bbmin.x) + bbmin.x;
		pos.y = (float)rand2() * (bbmax.y-bbmin.y) + bbmin.y;
		pos.z = (float)rand2() * (bbmax.z-bbmin.z) + bbmin.z;
		for (j = 0; j < 3; j++)
			if (xsect (j, ntri, pp, pos))
				ncs[j]++;
	}
	cs.x = bbcs.x * (double)ncs[0]/(double)nop;
	cs.y = bbcs.y * (double)ncs[1]/(double)nop;
	cs.z = bbcs.z * (double)ncs[2]/(double)nop;

	m_pMainDlg->RefreshCalc();
}

void CShipeditApp::setup_grid (VOXGRID &g, int level)
{
	g.gridx = level;
	g.gridy = level;
	g.gridz = level;
	g.gridn = g.gridx * g.gridy * g.gridz;
	g.grid  = new BYTE[g.gridn];
	for (int i = 0; i < g.gridn; i++) g.grid[i] = 0; // 'internal'
	g.dx = (bbmax.x-bbmin.x)/g.gridx;
	g.dy = (bbmax.y-bbmin.y)/g.gridy;
	g.dz = (bbmax.z-bbmin.z)/g.gridz;
}

bool CShipeditApp::scan_gridline (VOXGRID &g, int x, int y, int z, int dir_idx, int ntri, const TriParam *pp)
{
	static D3DVECTOR d[6] = {{1,0,0},{0,1,0},{0,0,1},{-1,0,0},{0,-1,0},{0,0,-1}};
	D3DVECTOR &dir = d[dir_idx];
	D3DVECTOR pos, X;
	float t, tmin = 1e10;
	int i;
	bool isinter = false;

	switch (dir_idx) {
	case 0: // +x
		pos.x = (float)(bbmin.x - 0.5*g.dx);
		pos.y = (float)(bbmin.y + (y+0.5)*g.dy);
		pos.z = (float)(bbmin.z + (z+0.5)*g.dz);
		break;
	case 1: // +y
		pos.x = (float)(bbmin.x + (x+0.5)*g.dx);
		pos.y = (float)(bbmin.y - 0.5*g.dy);
		pos.z = (float)(bbmin.z + (z+0.5)*g.dz);
		break;
	case 2: // +z
		pos.x = (float)(bbmin.x + (x+0.5)*g.dx);
		pos.y = (float)(bbmin.y + (y+0.5)*g.dy);
		pos.z = (float)(bbmin.z - 0.5*g.dz);
		break;
	case 3: // -x
		pos.x = (float)(bbmax.x + 0.5*g.dx);
		pos.y = (float)(bbmin.y + (y+0.5)*g.dy);
		pos.z = (float)(bbmin.z + (z+0.5)*g.dz);
		break;
	case 4: // -y
		pos.x = (float)(bbmin.x + (x+0.5)*g.dx);
		pos.y = (float)(bbmax.y + 0.5*g.dy);
		pos.z = (float)(bbmin.z + (z+0.5)*g.dz);
		break;
	case 5:
		pos.x = (float)(bbmin.x + (x+0.5)*g.dx);
		pos.y = (float)(bbmin.y + (y+0.5)*g.dy);
		pos.z = (float)(bbmax.z + 0.5*g.dz);
		break;
	}
	for (i = 0; i < ntri; i++) {
		if (intersect (pos, dir, pp[i], X, t, true)) {
			if (in_tri_proj (X, pp[i], dir_idx%3)) {
				isinter = true;
				if (t < tmin) tmin = t;
			}
		}
	}
	switch (dir_idx) {
	case 0:
		for (i = 0; i < g.gridx; i++)
			if (tmin > (i+1)*g.dx)
				g.grid[z*g.gridx*g.gridy + y*g.gridx + i] = 1;
		break;
	case 1:
		for (i = 0; i < g.gridy; i++)
			if (tmin > (i+1)*g.dy)
				g.grid[z*g.gridx*g.gridy + i*g.gridx + x] = 1;
		break;
	case 2:
		for (i = 0; i < g.gridz; i++)
			if (tmin > (i+1)*g.dz)
				g.grid[i*g.gridx*g.gridy + y*g.gridx + x] = 1;
		break;
	case 3:
		for (i = g.gridx-1; i >= 0; i--)
			if (tmin > (g.gridx-i)*g.dx)
				g.grid[z*g.gridx*g.gridy + y*g.gridx + i] = 1;
		break;
	case 4:
		for (i = g.gridy-1; i >= 0; i--)
			if (tmin > (g.gridy-i)*g.dy)
				g.grid[z*g.gridx*g.gridy + i*g.gridx + x] = 1;
		break;
	case 5:
		for (i = g.gridz-1; i >= 0; i--)
			if (tmin > (g.gridz-i)*g.dz)
				g.grid[i*g.gridx*g.gridy + y*g.gridx + x] = 1;
		break;
	}
	return isinter;
}		

void CShipeditApp::analyse_grid (VOXGRID &g, double &vol, Vector &com, Vector &cs, Matrix &pmi)
{
	int i, j, k, idx, ninside = 0;
	double x, y, z;
	double bbvol = (bbmax.x-bbmin.x) * (bbmax.y-bbmin.y) * (bbmax.z-bbmin.z);
	com.x = com.y = com.z = 0.0;

	for (k = idx = 0; k < g.gridz; k++) {
		z = bbmin.z + (k+0.5)*g.dz;
		for (j = 0; j < g.gridy; j++) {
			y = bbmin.y + (j+0.5)*g.dy;
			for (i = 0; i < g.gridx; i++) {
				x = bbmin.x + (i+0.5)*g.dx;
				if ((g.grid[idx++] & 1) == 0) {
					ninside++;
					com.x += x;
					com.y += y;
					com.z += z;
					pmi.m11 += y*y+z*z;
					pmi.m12 += x*y;
					pmi.m13 += x*z;
					pmi.m21 += y*x;
					pmi.m22 += x*x+z*z;
					pmi.m23 += y*z;
					pmi.m31 += z*x;
					pmi.m32 += z*y;
					pmi.m33 += x*x+y*y;
				}
			}
		}
	}
	vol = bbvol * (double)ninside / (double)g.gridn;
	com.x /= (double)ninside;
	com.y /= (double)ninside;
	com.z /= (double)ninside;
	for (i = 0; i < 9; i++) pmi.data[i] /= (double)ninside;

	// cross sections
	for (k = ninside = 0; k < g.gridz; k++) {
		for (j = 0; j < g.gridy; j++) {
			for (i = 0; i < g.gridx; i++) {
				if ((g.grid[k*g.gridx*g.gridy + j*g.gridx + i] & 1) == 0) {
					ninside++;
					break;
				}
			}
		}
	}
	cs.x = (bbmax.y-bbmin.y)*(bbmax.z-bbmin.z) * (double)ninside / (double)(g.gridy*g.gridz);
	
	for (k = ninside = 0; k < g.gridz; k++) {
		for (i = 0; i < g.gridx; i++) {
			for (j = 0; j < g.gridy; j++) {
				if ((g.grid[k*g.gridx*g.gridy + j*g.gridx + i] & 1) == 0) {
					ninside++;
					break;
				}
			}
		}
	}
	cs.y = (bbmax.x-bbmin.x)*(bbmax.z-bbmin.z) * (double)ninside / (double)(g.gridx*g.gridz);

	for (j = ninside = 0; j < g.gridy; j++) {
		for (i = 0; i < g.gridx; i++) {
			for (k = 0; k < g.gridz; k++) {
				if ((g.grid[k*g.gridx*g.gridy + j*g.gridx + i] & 1) == 0) {
					ninside++;
					break;
				}
			}
		}
	}
	cs.z = (bbmax.x-bbmin.x)*(bbmax.y-bbmin.y) * (double)ninside / (double)(g.gridx*g.gridy);
}

BOOL CShipeditApp::OnIdle (LONG lCount)
{
	CWinApp::OnIdle (lCount);
	if (bBackgroundOp)
		ProcessPackage ();
	return bBackgroundOp;
}

// ============================================================
// Local utility functions

bool inside (int ntri, const TriParam *pp, const D3DVECTOR &pos)
{
	int i, nx = 0;
	D3DVECTOR dir, X;
	float t;

	// find a search direction
	double phi = rand2()*Pi*2.0;
	double tht = 0.5*Pi-acos(rand2()*2.0-1.0);
	dir.x = (float)(cos(tht)*cos(phi));
	dir.y = (float)(cos(tht)*sin(phi));
	dir.z = (float)sin(tht);

	for (i = 0; i < ntri; i++) {
		if (!intersect (pos, dir, pp[i], X, t, true)) continue;
		if (in_tri (X, pp[i])) nx++;
	}
	return nx & 1; // odd number of intersections
}

bool xsect (int axis, int ntri, const TriParam *pp, const D3DVECTOR &pos)
{
	D3DVECTOR X, dir[3] = {{1,0,0}, {0,1,0}, {0,0,1}};
	float t;
	for (int i = 0; i < ntri; i++) {
		if (!intersect (pos, dir[axis], pp[i], X, t, false)) continue;
		if (in_tri (X, pp[i])) return true;
	}
	return false;
}

bool intersect (const D3DVECTOR &pos, const D3DVECTOR &dir, const TriParam &pp, D3DVECTOR &X, float &t, bool half)
{
	const float EPS = 1e-8f;
	float den = pp.a*dir.x + pp.b*dir.y + pp.c*dir.z;
	if (fabs (den) < EPS) return false;
	t = -(pp.a*pos.x + pp.b*pos.y + pp.c*pos.z + pp.d) / den;

	if (half && t < 0.0) return false;
	X.x = pos.x + t*dir.x;
	X.y = pos.y + t*dir.y;
	X.z = pos.z + t*dir.z;
	return true;
}

bool in_tri (const D3DVECTOR &X, const TriParam &pp)
{
	double a, b, den;
	double x1, x2, x3, xp, y1, y2, y3, yp;

	if (fabs(pp.a) > fabs(pp.b)) {
		if (fabs(pp.a) > fabs(pp.c)) { // project to yz axis
			x1 = pp.y1, x2 = pp.y2, x3 = pp.y3, xp = X.y;
			y1 = pp.z1, y2 = pp.z2, y3 = pp.z3, yp = X.z;
		} else {                       // project to xy axis
			x1 = pp.x1, x2 = pp.x2, x3 = pp.x3, xp = X.x;
			y1 = pp.y1, y2 = pp.y2, y3 = pp.y3, yp = X.y;
		}
	} else {
		if (fabs(pp.b) > fabs(pp.c)) { // project to xz axis
			x1 = pp.x1, x2 = pp.x2, x3 = pp.x3, xp = X.x;
			y1 = pp.z1, y2 = pp.z2, y3 = pp.z3, yp = X.z;
		} else {                       // project to xy axis
			x1 = pp.x1, x2 = pp.x2, x3 = pp.x3, xp = X.x;
			y1 = pp.y1, y2 = pp.y2, y3 = pp.y3, yp = X.y;
		}
	}

	den = (yp-y1)*(x3-x2) + (x1-xp)*(y3-y2);
	if (den == 0.0) return false;
	a = ((y2-y1)*(x3-x2) + (x1-x2)*(y3-y2))/den;
	if (a < 1.0) return false;
	if (den = x3-x2) {
		b = (x1 - x2 + a*(xp-x1))/den;
	} else if (den = y3-y2) {
		b = (y1 - y2 + a*(yp-y1))/den;
	} else if (den = x3-x2) {
		b = (x1 - x2 + a*(xp-x1))/den;
	} else return false;
	if (b < 0.0 || b > 1.0) return false;

	return true;
}

bool in_tri_proj (const D3DVECTOR &X, const TriParam &pp, int dir_idx)
{
	double x1, x2, x3, xp, y1, y2, y3, yp;

	switch (dir_idx) {
	case 0: // project on yz axis
		x1 = pp.y1, x2 = pp.y2, x3 = pp.y3, xp = X.y;
		y1 = pp.z1, y2 = pp.z2, y3 = pp.z3, yp = X.z;
		break;
	case 1: // project on xz axis
		x1 = pp.x1, x2 = pp.x2, x3 = pp.x3, xp = X.x;
		y1 = pp.z1, y2 = pp.z2, y3 = pp.z3, yp = X.z;
		break;
	case 2: // project on xy axis
		x1 = pp.x1, x2 = pp.x2, x3 = pp.x3, xp = X.x;
		y1 = pp.y1, y2 = pp.y2, y3 = pp.y3, yp = X.y;
		break;
	}
#ifdef UNDEF
	double a, b, den;
	den = (yp-y1)*(x3-x2) + (x1-xp)*(y3-y2);
	if (den == 0.0) return false;
	a = ((y2-y1)*(x3-x2) + (x1-x2)*(y3-y2))/den;
	if (a < 1.0) return false;
	if (den = x3-x2) {
		b = (x1 - x2 + a*(xp-x1))/den;
	} else if (den = y3-y2) {
		b = (y1 - y2 + a*(yp-y1))/den;
	} else if (den = x3-x2) {
		b = (x1 - x2 + a*(xp-x1))/den;
	} else return false;
	if (b < 0.0 || b > 1.0) return false;
#endif
	double x_0, x_1, x_2, y_0, y_1, y_2, y0r, yyr, fac;
	int i;
	const double EPS = 1e-10;
	for (i = 0; i < 3; i++) {
		switch (i) {
		case 0: x_0 = x1, y_0 = y1, x_1 = x2, y_1 = y2, x_2 = x3, y_2 = y3; break;
		case 1: x_0 = x2, y_0 = y2, x_1 = x3, y_1 = y3, x_2 = x1, y_2 = y1; break;
		case 2: x_0 = x3, y_0 = y3, x_1 = x1, y_1 = y1, x_2 = x2, y_2 = y2; break;
		}
		if (fabs (x_1-x_2) < EPS) {
			if ((x_0 < x_1 && xp > x_1) || (x_0 > x_1 && xp < x_1)) return false;
		} else {
			fac = (y_2-y_1)/(x_2-x_1);
			y0r = (x_0-x_1)*fac + y_1;
			yyr = (xp-x_1)*fac + y_1;
			if ((y_0 < y0r && yp > yyr) || (y_0 > y0r && yp < yyr)) return false;
		}
	}
	return true;
}

const int rtable_len = 64;
static int rtable[rtable_len];

void srand2()
{
	srand (0x12345678);
	//srand ((unsigned)time (NULL));
	for (int i = 0; i < rtable_len; i++)
		rtable[i] = rand();
}

double rand2 ()
{
	static const double irmax = 1.0/(double)RAND_MAX;
	int ridx = (rand()*64)/RAND_MAX;
	int r    = rtable[ridx];
	rtable[ridx] = rand();
	return (double)r*irmax;
}


/////////////////////////////////////////////////////////////////////////////
// GridintDlg dialog


GridintDlg::GridintDlg(CShipeditApp *_app, CWnd* pParent /*=NULL*/)
: CDialog(GridintDlg::IDD, pParent), app(_app)
{
	//{{AFX_DATA_INIT(GridintDlg)
	m_GridDim = 10;
	//}}AFX_DATA_INIT
}


void GridintDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(GridintDlg)
	DDX_Text(pDX, IDC_GRIDINT_DIM, m_GridDim);
	DDV_MinMaxInt(pDX, m_GridDim, 10, 1000);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(GridintDlg, CDialog)
	//{{AFX_MSG_MAP(GridintDlg)
	ON_BN_CLICKED(IDC_GRIDINT_START, OnGridintStart)
	ON_EN_CHANGE(IDC_GRIDINT_DIM, OnChangeGridintDim)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// GridintDlg message handlers

void GridintDlg::OnGridintStart() 
{
	int x, y, z;
	VOXGRID g;
	char cbuf[256];

	UpdateData();
	sprintf (cbuf, "Running (d=%d)", m_GridDim);
	GetDlgItem (IDC_GRIDINT_MSG)->SetWindowText (cbuf);

	app->setup_grid (g, m_GridDim);
	for (x = z = 0; x < g.gridx; x++) {
		for (y = 0; y < g.gridy; y++) {
			app->scan_gridline (g, x, y, z, 2, app->ntri, app->pp);
			app->scan_gridline (g, x, y, z, 5, app->ntri, app->pp);
		}
	}
	for (x = y = 0; x < g.gridx; x++) {
		for (z = 0; z < g.gridz; z++) {
			app->scan_gridline (g, x, y, z, 1, app->ntri, app->pp);
			app->scan_gridline (g, x, y, z, 4, app->ntri, app->pp);
		}
	}
	for (x = y = 0; y < g.gridy; y++) {
		for (z = 0; z < g.gridz; z++) {
			app->scan_gridline (g, x, y, z, 0, app->ntri, app->pp);
			app->scan_gridline (g, x, y, z, 3, app->ntri, app->pp);
		}
	}

//#ifdef UNDEF
	// use this to find boundary point for arbitrary ray
	D3DVECTOR pos = {-4.8f, app->bbmin.y, -4.69f}, dir = {0,1,0}, X;
	float t, tmin = 1e10;
	for (DWORD i = 0; i < app->ntri; i++) {
		if (intersect (pos, dir, app->pp[i], X, t, true)) {
			if (in_tri_proj (X, app->pp[i], 1)) {
				if (t < tmin) tmin = t;
			}
		}
	}
	ofstream ofs ("dbg.txt");
	ofs << "ymin=" << app->bbmin.y+tmin << endl;
//#endif

	double vol;
	Vector com, cs;
	Matrix pmi;
	app->analyse_grid (g, vol, com, cs, pmi);
	sprintf (cbuf, "%0.2f", vol);
	GetDlgItem (IDC_GRIDINT_VOL)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f %0.2f %0.2f", com.x, com.y, com.z);
	GetDlgItem (IDC_GRIDINT_COM)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f %0.2f %0.2f", cs.x, cs.y, cs.z);
	GetDlgItem (IDC_GRIDINT_CS)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f %0.2f %0.2f", pmi.m11, pmi.m12, pmi.m13);
	GetDlgItem (IDC_GRIDINT_PMI1)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f %0.2f %0.2f", pmi.m21, pmi.m22, pmi.m23);
	GetDlgItem (IDC_GRIDINT_PMI2)->SetWindowText (cbuf);
	sprintf (cbuf, "%0.2f %0.2f %0.2f", pmi.m31, pmi.m32, pmi.m33);
	GetDlgItem (IDC_GRIDINT_PMI3)->SetWindowText (cbuf);

	delete []g.grid;
	sprintf (cbuf, "Ready (d=%d)", m_GridDim);
	GetDlgItem (IDC_GRIDINT_MSG)->SetWindowText (cbuf);
	MessageBeep (-1);
}

void GridintDlg::OnChangeGridintDim() 
{
	char cbuf[256];
	double dim;
	GetDlgItem (IDC_GRIDINT_DIM)->GetWindowText (cbuf, 256);
	if (sscanf (cbuf, "%lf", &dim) && dim >= 0) {
		double mem = dim*dim*dim;
		if (mem > 1e6) sprintf (cbuf, "(%0.1f MByte)", mem*1e-6);
		else           sprintf (cbuf, "(%0.0f KByte)", mem*1e-3);
		GetDlgItem (IDC_GRIDINT_MEM)->SetWindowText (cbuf);
	}
}
/////////////////////////////////////////////////////////////////////////////
// AddMeshDlg dialog


AddMeshDlg::AddMeshDlg(CWnd* pParent /*=NULL*/)
	: CDialog(AddMeshDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(AddMeshDlg)
	m_AddMode = 0;
	//}}AFX_DATA_INIT
}


void AddMeshDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(AddMeshDlg)
	DDX_Radio(pDX, IDC_RADIO1, m_AddMode);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(AddMeshDlg, CDialog)
	//{{AFX_MSG_MAP(AddMeshDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// AddMeshDlg message handlers
