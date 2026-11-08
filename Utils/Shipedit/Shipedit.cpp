// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <algorithm>
#include <chrono>
#include <fstream>
#include <sstream>
#include <string>
#include "OrbiterPlatform.h"
#include "UtilApp.h"
#include "imgui.h"
#include "Vecmat.h"
#include "Mesh.h"

using namespace std;

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

void srand2();
double rand2 ();
bool inside (int ntri, const TriParam *pp, const D3DVECTOR &pos);
bool xsect (int axis, int ntri, const TriParam *pp, const D3DVECTOR &pos);
bool intersect (const D3DVECTOR &pos, const D3DVECTOR &dir, const TriParam &pp, D3DVECTOR &X, float &t, bool half);
bool in_tri (const D3DVECTOR &X, const TriParam &pp);
bool in_tri_proj (const D3DVECTOR &X, const TriParam &pp, int dir_idx);

// ============================================================
// Mesh analysis

class ShipeditApp {
public:
	ShipeditApp ();
	~ShipeditApp ();
	void InitMesh ();
	void ProcessPackage ();
	void setup_grid (VOXGRID &g, int level);
	bool scan_gridline (VOXGRID &g, int x, int y, int z, int dir_idx, int ntri, const TriParam *pp);
	void analyse_grid (VOXGRID &g, double &vol, Vector &com, Vector &cs, Matrix &pmi);

	Mesh mesh;
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
	bool bBackgroundOp;
	bool bCalcValid;
	int flushcount;
	int nop, nvol, ncs[3];
};

ShipeditApp::ShipeditApp ()
{
	ngrp = nvtx = nidx = ntri = 0;
	vtx = 0; idx = 0; pp = 0;
	flushcount = 0;
	bBackgroundOp = false;
	bCalcValid = false;
	nop = nvol = 0;
	srand2();
}

ShipeditApp::~ShipeditApp ()
{
	if (nvtx) delete []vtx;
	if (nidx) delete []idx;
	if (ntri) delete []pp;
}

void ShipeditApp::InitMesh ()
{
	DWORD g, n, i, j;

	// delete current mesh
	if (nvtx) delete []vtx;
	if (nidx) delete []idx;
	if (ntri) delete []pp;

	ngrp = mesh.nGroup();
	nvtx = nidx = 0;

	for (g = 0; g < ngrp; g++) {
		D3DVERTEX *gvtx;
		WORD *gidx;
		DWORD gnvtx, gnidx;
		mesh.GetGroup (g, gvtx, gnvtx, gidx, gnidx);
		nvtx += gnvtx;
		nidx += gnidx;
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
	bCalcValid = false;
}

void ShipeditApp::ProcessPackage ()
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
	bCalcValid = true;
}

void ShipeditApp::setup_grid (VOXGRID &g, int level)
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

bool ShipeditApp::scan_gridline (VOXGRID &g, int x, int y, int z, int dir_idx, int ntri, const TriParam *pp)
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

void ShipeditApp::analyse_grid (VOXGRID &g, double &vol, Vector &com, Vector &cs, Matrix &pmi)
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
	if ((den = x3-x2)) {
		b = (x1 - x2 + a*(xp-x1))/den;
	} else if ((den = y3-y2)) {
		b = (y1 - y2 + a*(yp-y1))/den;
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
	default: // project on xy axis
		x1 = pp.x1, x2 = pp.x2, x3 = pp.x3, xp = X.x;
		y1 = pp.y1, y2 = pp.y2, y3 = pp.y3, yp = X.y;
		break;
	}
	double x_0, x_1, x_2, y_0, y_1, y_2, y0r, yyr, fac;
	int i;
	const double EPS = 1e-10;
	for (i = 0; i < 3; i++) {
		switch (i) {
		case 0: x_0 = x1, y_0 = y1, x_1 = x2, y_1 = y2, x_2 = x3, y_2 = y3; break;
		case 1: x_0 = x2, y_0 = y2, x_1 = x3, y_1 = y3, x_2 = x1, y_2 = y1; break;
		default: x_0 = x3, y_0 = y3, x_1 = x1, y_1 = y1, x_2 = x2, y_2 = y2; break;
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
	for (int i = 0; i < rtable_len; i++)
		rtable[i] = rand();
}

double rand2 ()
{
	static const double irmax = 1.0/(double)RAND_MAX;
	int ridx = (int)(((long long)rand()*64)/RAND_MAX);
	if (ridx >= rtable_len) ridx = rtable_len-1;
	int r    = rtable[ridx];
	rtable[ridx] = rand();
	return (double)r*irmax;
}

// ============================================================
// User interface

namespace {

ShipeditApp app;

enum Dialog { DLG_NONE, DLG_ADDMESH, DLG_TRANSLATE, DLG_ROTATE, DLG_MIRROR, DLG_SCALE, DLG_MERGE, DLG_NORMALS, DLG_ZEROLEVEL,
	DLG_GRIDINT, DLG_CHECK, DLG_ABOUT };

const char *DialogTitle (Dialog d)
{
	switch (d) {
	case DLG_ADDMESH:   return "New mesh";
	case DLG_TRANSLATE: return "Translate mesh";
	case DLG_ROTATE:    return "Rotate mesh";
	case DLG_MIRROR:    return "Mirror coordinates";
	case DLG_SCALE:     return "Scale mesh";
	case DLG_MERGE:     return "Merge groups";
	case DLG_NORMALS:   return "Generate normals";
	case DLG_ZEROLEVEL: return "Zero level";
	case DLG_GRIDINT:   return "Grid integration";
	case DLG_CHECK:     return "Check result";
	case DLG_ABOUT:     return "About Shipedit";
	default:            return "";
	}
}

Dialog openDlg = DLG_NONE;
Dialog pendingDlg = DLG_NONE;

int addMode = 0;
float translate[3];
double rot[3];
double scale[3];
int mirrorDir = 0;
int mergeGrp[2];
int nmlSelGrp = 0, nmlSelVtx = 0, nmlGroup = 1;
float zeroLevel = 1e-5f;
bool zeroVtx = false, zeroNml = false, zeroTex = false;
int gridDim = 10;
int gridState = 0; // 0: idle, 1: about to run, 2: running
char gridMsg[64] = "Parameters";
char gridVol[64] = "[Undefined]", gridCom[64] = "[Undefined]", gridCs[64] = "[Undefined]";
char gridPmi[3][64] = {"[Undefined]", "", ""};
std::string checkMsg;

void OpenDialog (Dialog d)
{
	pendingDlg = d;
}

bool HaveMesh ()
{
	return app.ngrp != 0;
}

bool ReadMesh (const std::string &path, Mesh &mesh)
{
	std::ifstream ifs (path, std::ios::binary);
	if (!ifs) return false;
	std::stringstream ss;
	ss << ifs.rdbuf ();
	std::string text = ss.str ();
	text.erase (std::remove (text.begin (), text.end (), '\r'), text.end ());
	std::istringstream is (text);
	is >> mesh;
	return true;
}

void LoadMesh (int mode)
{
	std::string path = oplat::OpenFileDialog ("Load mesh", "", {});
	if (path.empty ()) return;
	if (mode) {
		Mesh mesh2;
		if (!ReadMesh (path, mesh2)) return;
		app.mesh.AddMesh (mesh2);
	} else {
		if (!ReadMesh (path, app.mesh)) return;
	}
	app.InitMesh ();
}

void OnLoad ()
{
	if (app.mesh.nGroup() > 0) {
		addMode = 0;
		OpenDialog (DLG_ADDMESH);
	} else {
		utilapp::Defer ([] { LoadMesh (0); });
	}
}

void OnSaveAs ()
{
	utilapp::Defer ([] {
		std::string path = oplat::SaveFileDialog ("Save mesh", "", "", {"msh"});
		if (path.empty ()) return;
		ofstream ofs (path);
		if (ofs) ofs << app.mesh;
	});
}

void OnCheck ()
{
	DWORD i, nremoved, tot_removed = 0;
	if (app.ngrp) {
		Mesh &mesh = app.mesh;
		for (i = 0; i < mesh.nGroup(); i++) {
			mesh.CheckGroup (i, nremoved);
			tot_removed += nremoved;
		}
		char cbuf[256];
		if (tot_removed) snprintf (cbuf, sizeof (cbuf), "Removed %u unused vertices from mesh.", (unsigned)tot_removed);
		else             snprintf (cbuf, sizeof (cbuf), "No problems found");
		checkMsg = cbuf;
		OpenDialog (DLG_CHECK);
	}
	app.InitMesh();
}

void ResetGridDialog ()
{
	gridState = 0;
	strcpy (gridMsg, "Parameters");
	strcpy (gridVol, "[Undefined]");
	strcpy (gridCom, "[Undefined]");
	strcpy (gridCs, "[Undefined]");
	strcpy (gridPmi[0], "[Undefined]");
	gridPmi[1][0] = gridPmi[2][0] = '\0';
}

void RunGridIntegration ()
{
	int x, y, z;
	VOXGRID g;

	app.setup_grid (g, gridDim);
	for (x = z = 0; x < g.gridx; x++) {
		for (y = 0; y < g.gridy; y++) {
			app.scan_gridline (g, x, y, z, 2, app.ntri, app.pp);
			app.scan_gridline (g, x, y, z, 5, app.ntri, app.pp);
		}
	}
	for (x = y = 0; x < g.gridx; x++) {
		for (z = 0; z < g.gridz; z++) {
			app.scan_gridline (g, x, y, z, 1, app.ntri, app.pp);
			app.scan_gridline (g, x, y, z, 4, app.ntri, app.pp);
		}
	}
	for (x = y = 0; y < g.gridy; y++) {
		for (z = 0; z < g.gridz; z++) {
			app.scan_gridline (g, x, y, z, 0, app.ntri, app.pp);
			app.scan_gridline (g, x, y, z, 3, app.ntri, app.pp);
		}
	}

	double vol;
	Vector com, cs;
	Matrix pmi;
	app.analyse_grid (g, vol, com, cs, pmi);
	snprintf (gridVol, sizeof (gridVol), "%0.2f", vol);
	snprintf (gridCom, sizeof (gridCom), "%0.2f %0.2f %0.2f", com.x, com.y, com.z);
	snprintf (gridCs, sizeof (gridCs), "%0.2f %0.2f %0.2f", cs.x, cs.y, cs.z);
	snprintf (gridPmi[0], sizeof (gridPmi[0]), "%0.2f %0.2f %0.2f", pmi.m11, pmi.m12, pmi.m13);
	snprintf (gridPmi[1], sizeof (gridPmi[1]), "%0.2f %0.2f %0.2f", pmi.m21, pmi.m22, pmi.m23);
	snprintf (gridPmi[2], sizeof (gridPmi[2]), "%0.2f %0.2f %0.2f", pmi.m31, pmi.m32, pmi.m33);

	delete []g.grid;
	snprintf (gridMsg, sizeof (gridMsg), "Ready (d=%d)", gridDim);
	MessageBeep (-1);
}

void MergeGroups ()
{
	if (mergeGrp[0] == mergeGrp[1]) return;
	DWORD grp1 = mergeGrp[0]-1, grp2 = mergeGrp[1]-1;
	Mesh *mesh = &app.mesh;
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
	for (i = nidx1; i < nidx; i++)
		idx[i] += (WORD)nvtx1;
	mesh->DeleteGroup (grp1 < grp2 ? grp2 : grp1);
	mesh->DeleteGroup (grp1 < grp2 ? grp1 : grp2);
	mesh->AddGroup (vtx, nvtx, idx, nidx);
}

enum { BTN_NONE, BTN_OK, BTN_CANCEL };

int OkCancelResult ()
{
	int res = BTN_NONE;
	ImGui::Spacing ();
	if (ImGui::Button ("OK", ImVec2 (80 * utilapp::Scale (), 0))) res = BTN_OK;
	ImGui::SameLine ();
	if (ImGui::Button ("Cancel", ImVec2 (80 * utilapp::Scale (), 0))) res = BTN_CANCEL;
	if (res != BTN_NONE) ImGui::CloseCurrentPopup ();
	return res;
}

bool OkCancel ()
{
	return OkCancelResult () == BTN_OK;
}

bool CloseButton ()
{
	ImGui::Spacing ();
	if (ImGui::Button ("Close", ImVec2 (80 * utilapp::Scale (), 0))) {
		ImGui::CloseCurrentPopup ();
		return true;
	}
	return false;
}

float FieldWidth ()
{
	return 120 * utilapp::Scale ();
}

void DrawDialog ()
{
	if (pendingDlg != DLG_NONE) {
		openDlg = pendingDlg;
		pendingDlg = DLG_NONE;
		ImGui::OpenPopup (DialogTitle (openDlg));
	}
	if (openDlg == DLG_NONE) return;

	const char *title = DialogTitle (openDlg);
	ImGui::SetNextWindowPos (ImGui::GetMainViewport ()->GetCenter (), ImGuiCond_Appearing, ImVec2 (0.5f, 0.5f));
	if (!ImGui::BeginPopupModal (title, nullptr, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
		openDlg = DLG_NONE;
		return;
	}

	switch (openDlg) {
	case DLG_ADDMESH:
		ImGui::RadioButton ("Replace current mesh", &addMode, 0);
		ImGui::RadioButton ("Merge into current mesh", &addMode, 1);
		if (OkCancel ()) {
			int mode = addMode;
			utilapp::Defer ([mode] { LoadMesh (mode); });
		}
		break;

	case DLG_TRANSLATE:
		ImGui::SeparatorText ("Translation vector");
		ImGui::SetNextItemWidth (FieldWidth ());
		ImGui::InputFloat ("dX", &translate[0], 0, 0, "%g");
		ImGui::SetNextItemWidth (FieldWidth ());
		ImGui::InputFloat ("dY", &translate[1], 0, 0, "%g");
		ImGui::SetNextItemWidth (FieldWidth ());
		ImGui::InputFloat ("dZ", &translate[2], 0, 0, "%g");
		if (OkCancel ()) {
			app.mesh.Translate (translate[0], translate[1], translate[2]);
			app.InitMesh ();
		}
		break;

	case DLG_ROTATE: {
		static const char *label[3] = {"Rotate around x-axis [deg.]", "Rotate around y-axis [deg.]", "Rotate around z-axis [deg.]"};
		static const char *button[3] = {"Rotate x", "Rotate y", "Rotate z"};
		static const Mesh::RotAxis axis[3] = {Mesh::ROTATE_X, Mesh::ROTATE_Y, Mesh::ROTATE_Z};
		ImGui::SeparatorText ("Rotation");
		for (int i = 0; i < 3; i++) {
			ImGui::PushID (i);
			ImGui::AlignTextToFramePadding ();
			ImGui::TextUnformatted (label[i]);
			ImGui::SameLine (220 * utilapp::Scale ());
			ImGui::SetNextItemWidth (80 * utilapp::Scale ());
			if (ImGui::InputDouble ("##rot", &rot[i], 0, 0, "%g"))
				rot[i] = std::clamp (rot[i], -360.0, 360.0);
			ImGui::SameLine ();
			if (ImGui::Button (button[i]))
				app.mesh.Rotate (axis[i], (float)(RAD*rot[i]));
			ImGui::PopID ();
		}
		if (CloseButton ()) app.InitMesh ();
		break;
	}

	case DLG_MIRROR:
		ImGui::SeparatorText ("Mirror direction");
		ImGui::RadioButton ("Mirror x", &mirrorDir, 0);
		ImGui::RadioButton ("Mirror y", &mirrorDir, 1);
		ImGui::RadioButton ("Mirror z", &mirrorDir, 2);
		if (OkCancel ()) {
			switch (mirrorDir) {
			case 0: app.mesh.Mirror (Mesh::MIRROR_X); break;
			case 1: app.mesh.Mirror (Mesh::MIRROR_Y); break;
			case 2: app.mesh.Mirror (Mesh::MIRROR_Z); break;
			}
			app.InitMesh ();
		}
		break;

	case DLG_SCALE: {
		ImGui::SeparatorText ("Scaling factor");
		ImGui::SetNextItemWidth (FieldWidth ());
		ImGui::InputDouble ("Scale X", &scale[0], 0, 0, "%g");
		ImGui::SetNextItemWidth (FieldWidth ());
		ImGui::InputDouble ("Scale Y", &scale[1], 0, 0, "%g");
		ImGui::SetNextItemWidth (FieldWidth ());
		ImGui::InputDouble ("Scale Z", &scale[2], 0, 0, "%g");
		int res = OkCancelResult ();
		if (res == BTN_OK)
			app.mesh.Scale ((float)scale[0], (float)scale[1], (float)scale[2]);
		if (res != BTN_NONE) app.InitMesh ();
		break;
	}

	case DLG_MERGE: {
		int maxgrp = (int)app.mesh.nGroup();
		char cbuf[64];
		ImGui::SeparatorText ("Merge");
		snprintf (cbuf, sizeof (cbuf), "Merge group (1-%d)", maxgrp);
		ImGui::SetNextItemWidth (FieldWidth ());
		if (ImGui::InputInt (cbuf, &mergeGrp[0])) mergeGrp[0] = std::clamp (mergeGrp[0], 1, maxgrp);
		snprintf (cbuf, sizeof (cbuf), "with group (1-%d)", maxgrp);
		ImGui::SetNextItemWidth (FieldWidth ());
		if (ImGui::InputInt (cbuf, &mergeGrp[1])) mergeGrp[1] = std::clamp (mergeGrp[1], 1, maxgrp);
		if (OkCancel ()) {
			MergeGroups ();
			app.InitMesh ();
		}
		break;
	}

	case DLG_NORMALS: {
		int maxgrp = (int)app.mesh.nGroup();
		char cbuf[64];
		ImGui::SeparatorText ("Calculate normals for");
		ImGui::RadioButton ("All mesh groups", &nmlSelGrp, 0);
		snprintf (cbuf, sizeof (cbuf), "Only for group (1-%d)", maxgrp);
		ImGui::RadioButton (cbuf, &nmlSelGrp, 1);
		ImGui::SameLine ();
		ImGui::BeginDisabled (nmlSelGrp == 0);
		ImGui::SetNextItemWidth (FieldWidth ());
		if (ImGui::InputInt ("##grp", &nmlGroup)) nmlGroup = std::clamp (nmlGroup, 1, maxgrp);
		ImGui::EndDisabled ();
		ImGui::SeparatorText ("Static");
		ImGui::RadioButton ("Normals for all vertices", &nmlSelVtx, 0);
		ImGui::RadioButton ("Missing normals only", &nmlSelVtx, 1);
		ImGui::Spacing ();
		if (ImGui::Button ("Apply", ImVec2 (80 * utilapp::Scale (), 0))) {
			UINT g1, g2, g;
			if (nmlSelGrp) g1 = nmlGroup-1, g2 = g1+1;
			else           g1 = 0, g2 = app.mesh.nGroup();
			bool missing_only = nmlSelVtx == 1;
			for (g = g1; g < g2; g++)
				app.mesh.CalcNormals (g, missing_only);
			app.InitMesh ();
		}
		ImGui::SameLine ();
		if (ImGui::Button ("Close", ImVec2 (80 * utilapp::Scale (), 0))) ImGui::CloseCurrentPopup ();
		break;
	}

	case DLG_ZEROLEVEL:
		ImGui::SeparatorText ("Zero level");
		ImGui::TextUnformatted ("Reset vertex coordinates to zero if magnitude below");
		ImGui::SetNextItemWidth (FieldWidth ());
		if (ImGui::InputFloat ("##zero", &zeroLevel, 0, 0, "%g")) zeroLevel = std::clamp (zeroLevel, 0.0f, 1.0f);
		ImGui::SeparatorText ("Apply to");
		ImGui::Checkbox ("Vertex coordinates", &zeroVtx);
		ImGui::Checkbox ("Vertex normals", &zeroNml);
		ImGui::Checkbox ("Texture coordinates", &zeroTex);
		if (OkCancel ()) {
			int which = 0;
			if (zeroVtx) which |= 1;
			if (zeroNml) which |= 2;
			if (zeroTex) which |= 4;
			app.mesh.ZeroThreshold (zeroLevel, which);
			app.InitMesh ();
		}
		break;

	case DLG_GRIDINT: {
		ImGui::BeginDisabled (gridState != 0);
		ImGui::SeparatorText ("Grid");
		ImGui::AlignTextToFramePadding ();
		ImGui::TextUnformatted ("Grid dimension");
		ImGui::SameLine ();
		ImGui::SetNextItemWidth (FieldWidth ());
		if (ImGui::InputInt ("##dim", &gridDim)) gridDim = std::clamp (gridDim, 10, 1000);
		ImGui::SameLine ();
		double mem = (double)gridDim*gridDim*gridDim;
		if (mem > 1e6) ImGui::Text ("(%0.1f MByte)", mem*1e-6);
		else           ImGui::Text ("(%0.0f KByte)", mem*1e-3);
		ImGui::PushTextWrapPos (ImGui::GetCursorPosX () + 360 * utilapp::Scale ());
		ImGui::TextWrapped ("Warning: A high grid sampling density will improve the accuracy of the result, but require more time and memory.");
		ImGui::PopTextWrapPos ();
		ImGui::EndDisabled ();

		ImGui::SeparatorText (gridMsg);
		if (ImGui::BeginTable ("##gridres", 2)) {
			ImGui::TableSetupColumn ("##k", ImGuiTableColumnFlags_WidthFixed);
			ImGui::TableSetupColumn ("##v", ImGuiTableColumnFlags_WidthStretch);
			const char *keys[4] = {"Volume:", "Centre of mass:", "Cross sections:", "Inertia tensor:"};
			const char *vals[4] = {gridVol, gridCom, gridCs, gridPmi[0]};
			for (int i = 0; i < 4; i++) {
				ImGui::TableNextRow ();
				ImGui::TableNextColumn (); ImGui::TextUnformatted (keys[i]);
				ImGui::TableNextColumn (); ImGui::TextUnformatted (vals[i]);
			}
			for (int i = 1; i < 3; i++) {
				ImGui::TableNextRow ();
				ImGui::TableNextColumn ();
				ImGui::TableNextColumn (); ImGui::TextUnformatted (gridPmi[i]);
			}
			ImGui::EndTable ();
		}
		ImGui::Spacing ();
		ImGui::BeginDisabled (gridState != 0);
		if (ImGui::Button ("Start", ImVec2 (80 * utilapp::Scale (), 0))) {
			snprintf (gridMsg, sizeof (gridMsg), "Running (d=%d)", gridDim);
			gridState = 1;
		}
		ImGui::SameLine ();
		if (ImGui::Button ("Close", ImVec2 (80 * utilapp::Scale (), 0))) ImGui::CloseCurrentPopup ();
		ImGui::EndDisabled ();
		if (gridState == 1) {
			gridState = 2;
			utilapp::Defer ([] {
				RunGridIntegration ();
				gridState = 0;
			});
		}
		break;
	}

	case DLG_CHECK:
		ImGui::TextUnformatted (checkMsg.c_str ());
		ImGui::Spacing ();
		if (ImGui::Button ("OK", ImVec2 (80 * utilapp::Scale (), 0))) ImGui::CloseCurrentPopup ();
		break;

	case DLG_ABOUT:
		ImGui::TextUnformatted ("Shipedit version 1.0");
		ImGui::Spacing ();
		if (ImGui::Button ("OK", ImVec2 (80 * utilapp::Scale (), 0))) ImGui::CloseCurrentPopup ();
		break;

	default:
		break;
	}
	ImGui::EndPopup ();
}

void DrawMenu ()
{
	if (!ImGui::BeginMenuBar ()) return;
	if (ImGui::BeginMenu ("File")) {
		if (ImGui::MenuItem ("Load mesh...")) OnLoad ();
		if (ImGui::MenuItem ("Save as...")) OnSaveAs ();
		ImGui::Separator ();
		if (ImGui::MenuItem ("Exit")) utilapp::Quit ();
		ImGui::EndMenu ();
	}
	if (ImGui::BeginMenu ("Calc")) {
		if (ImGui::MenuItem ("Start/continue MC integration") && HaveMesh ()) app.bBackgroundOp = true;
		if (ImGui::MenuItem ("Stop MC integration")) app.bBackgroundOp = false;
		ImGui::Separator ();
		if (ImGui::MenuItem ("Grid integration...") && HaveMesh ()) {
			ResetGridDialog ();
			OpenDialog (DLG_GRIDINT);
		}
		ImGui::Separator ();
		if (ImGui::MenuItem ("Check mesh")) OnCheck ();
		ImGui::EndMenu ();
	}
	if (ImGui::BeginMenu ("Transform")) {
		if (ImGui::MenuItem ("Translate") && HaveMesh ()) {
			translate[0] = translate[1] = translate[2] = 0.0f;
			OpenDialog (DLG_TRANSLATE);
		}
		if (ImGui::MenuItem ("Rotate") && HaveMesh ()) {
			rot[0] = rot[1] = rot[2] = 0.0;
			OpenDialog (DLG_ROTATE);
		}
		if (ImGui::MenuItem ("Mirror") && HaveMesh ()) {
			mirrorDir = 0;
			OpenDialog (DLG_MIRROR);
		}
		if (ImGui::MenuItem ("Scale") && HaveMesh ()) {
			scale[0] = scale[1] = scale[2] = 1.0;
			OpenDialog (DLG_SCALE);
		}
		ImGui::Separator ();
		if (ImGui::MenuItem ("Merge groups...") && HaveMesh ()) {
			mergeGrp[0] = mergeGrp[1] = 1;
			OpenDialog (DLG_MERGE);
		}
		if (ImGui::MenuItem ("Generate normals...") && HaveMesh ()) {
			nmlSelGrp = nmlSelVtx = 0;
			nmlGroup = 1;
			OpenDialog (DLG_NORMALS);
		}
		ImGui::Separator ();
		if (ImGui::MenuItem ("Zero level")) {
			zeroLevel = 1e-5f;
			zeroVtx = zeroNml = zeroTex = false;
			OpenDialog (DLG_ZEROLEVEL);
		}
		ImGui::EndMenu ();
	}
	if (ImGui::BeginMenu ("Help")) {
		if (ImGui::MenuItem ("About Shipedit")) OpenDialog (DLG_ABOUT);
		ImGui::EndMenu ();
	}
	ImGui::EndMenuBar ();
}

void Row (const char *key, const char *value)
{
	ImGui::TableNextRow ();
	ImGui::TableNextColumn ();
	ImGui::TextUnformatted (key);
	ImGui::TableNextColumn ();
	ImGui::TextUnformatted (value);
}

void DrawInfo ()
{
	char cbuf[256];
	bool have = HaveMesh ();
	ImGui::SeparatorText ("Mesh info");
	if (ImGui::BeginTable ("##info", 2)) {
		ImGui::TableSetupColumn ("##k", ImGuiTableColumnFlags_WidthFixed, 110 * utilapp::Scale ());
		ImGui::TableSetupColumn ("##v", ImGuiTableColumnFlags_WidthStretch);
		snprintf (cbuf, sizeof (cbuf), "%u", (unsigned)app.ngrp);
		Row ("Groups:", have ? cbuf : "-");
		snprintf (cbuf, sizeof (cbuf), "%u", (unsigned)app.nvtx);
		Row ("Vertices:", have ? cbuf : "-");
		snprintf (cbuf, sizeof (cbuf), "%u", (unsigned)app.ntri);
		Row ("Triangles:", have ? cbuf : "-");
		snprintf (cbuf, sizeof (cbuf), "[%0.2f %0.2f %0.2f] [%0.2f %0.2f %0.2f]",
			app.bbmin.x, app.bbmin.y, app.bbmin.z, app.bbmax.x, app.bbmax.y, app.bbmax.z);
		Row ("Bounding box:", have ? cbuf : "-");
		ImGui::EndTable ();
	}

	bool calc = app.bCalcValid;
	if (calc) snprintf (cbuf, sizeof (cbuf), "Parameters (%d samples)", app.nop);
	else      snprintf (cbuf, sizeof (cbuf), "Parameters");
	ImGui::SeparatorText (cbuf);
	if (ImGui::BeginTable ("##calc", 4)) {
		ImGui::TableSetupColumn ("##k", ImGuiTableColumnFlags_WidthFixed, 110 * utilapp::Scale ());
		ImGui::TableSetupColumn ("##x", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn ("##y", ImGuiTableColumnFlags_WidthStretch);
		ImGui::TableSetupColumn ("##z", ImGuiTableColumnFlags_WidthStretch);
		auto vec = [&](const char *key, const double *v, int n) {
			ImGui::TableNextRow ();
			ImGui::TableNextColumn ();
			ImGui::TextUnformatted (key);
			for (int i = 0; i < 3; i++) {
				ImGui::TableNextColumn ();
				if (i < n) {
					if (calc) ImGui::Text ("%0.2f", v[i]);
					else if (i == 0) ImGui::TextUnformatted ("-");
				}
			}
		};
		vec ("Volume:", &app.vol, 1);
		double cgv[3] = {app.cg.x, app.cg.y, app.cg.z};
		vec ("Centre of mass:", cgv, 3);
		double csv[3] = {app.cs.x, app.cs.y, app.cs.z};
		vec ("Cross sections:", csv, 3);
		double j1[3] = {app.J.m11, app.J.m12, app.J.m13};
		double j2[3] = {app.J.m12, app.J.m22, app.J.m23};
		double j3[3] = {app.J.m13, app.J.m23, app.J.m33};
		vec ("Inertia tensor:", j1, 3);
		vec ("", j2, calc ? 3 : 0);
		vec ("", j3, calc ? 3 : 0);
		ImGui::EndTable ();
	}
}

void Frame ()
{
	if (app.bBackgroundOp) {
		auto t0 = std::chrono::steady_clock::now ();
		do {
			app.ProcessPackage ();
		} while (app.bBackgroundOp && std::chrono::steady_clock::now () - t0 < std::chrono::milliseconds (12));
	}
	utilapp::BeginMainWindow (true);
	DrawMenu ();
	DrawInfo ();
	DrawDialog ();
	utilapp::EndMainWindow ();
}

}

int main ()
{
	utilapp::AppDesc desc;
	desc.title = "Shipedit";
	desc.width = 480;
	desc.height = 380;
	int res = utilapp::Run (desc, Frame, [] { return app.bBackgroundOp; });
	MessageBeep (-1);
	return res;
}
