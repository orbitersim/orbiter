// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// MomentInd.cpp
// User interface for moment indicators on overhead panel
// ==============================================================

#define STRICT 1
#include "MomentInd.h"
#include "DeltaGlider.h"
#include "meshres_p0.h"
#include "meshres_vc.h"

// ==============================================================

//SURFHANDLE AngRateIndicator::srf[3] = {0,0,0};
int AngRateIndicator::bmp_w[3] = {40,50,50};
int AngRateIndicator::bmp_h[3] = {49,40,40};
int AngRateIndicator::ofs_x[3] = {207,140,74};
int AngRateIndicator::ofs_y[3] = {16,89,162};
int AngRateIndicator::label_ofs_x[3] = {207,150,84};

AngRateIndicator::AngRateIndicator (VESSEL3 *v, SURFHANDLE blitsrc)
: PanelElement(v), bsrc(blitsrc),
  xcnt(xcnt_2D), ycnt(ycnt_2D), zcnt(zcnt_2D)
{
	nvtx = 7*3*3;
	vtxbuf_2D = 0;
	vtxbuf_VC = 0;
	vtx0 = &vtxbuf_2D;
	w0 = &w0_2D;
	cost = &cost_2D;
	sint = &sint_2D;
}

AngRateIndicator::~AngRateIndicator ()
{
	if (vtxbuf_2D) delete []vtxbuf_2D;
	if (vtxbuf_VC) delete []vtxbuf_VC;
}

void AngRateIndicator::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_ANGVELDISP_P0);
	if (grp) {
		vtxofs = 0;
		if (!vtxbuf_2D) {
			vtxbuf_2D = new NTVERTEX[nvtx];
			memcpy (vtxbuf_2D, grp->Vtx+vtxofs, nvtx*sizeof(NTVERTEX));
			w0_2D = (double)vtxbuf_2D[0].x - (double)vtxbuf_2D[1].x;
			for (int i = 0; i < 3; i++) {
				xcnt_2D[i] = vtxbuf_2D[6+i*7].x;
				ycnt_2D[i] = vtxbuf_2D[6+i*21].y;
				zcnt_2D[i] = 0.0;
			}
			cost_2D = -1.0, sint_2D = 0.0;
		}
	}
}

void AngRateIndicator::ResetVC (DEVMESHHANDLE hMesh)
{
	int i, j, k;
	upt = 0.0;
	for (i = 0; i < 3; i++)
		for (j = 0; j < 3; j++)
			for (k = 0; k < 8; k++)
				label[i][j][k] = ' ';

	if (!vtxbuf_VC) {
		vtxbuf_VC = new NTVERTEX[nvtx];
		GROUPREQUESTSPEC grs = {vtxbuf_VC, (DWORD)nvtx, 0, 0, 0, 0, 0, 0};
		oapiGetMeshGroup(hMesh,	GRP_ANGVEL_DISP_OVR_VC, &grs);
		w0_VC = (double)vtxbuf_VC[0].x - (double)vtxbuf_VC[1].x;
		for (i = 0; i < 3; i++) {
			xcnt_VC[i] = vtxbuf_VC[6+i*7].x;
			ycnt_VC[i] = vtxbuf_VC[6+i*21].y;
			zcnt_VC[i] = vtxbuf_VC[6+i*21].z;
		}
		double tilt = atan2(zcnt_VC[0]-zcnt_VC[1], ycnt_VC[0]-ycnt_VC[1]);
		cost_VC = cos(tilt), sint_VC = sin(tilt);
	}
}

void AngRateIndicator::LoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	vtx0 = &vtxbuf_2D;
	w0 = &w0_2D;
	cost = &cost_2D, sint = &sint_2D;
	xcnt = xcnt_2D, ycnt = ycnt_2D, zcnt = zcnt_2D;
}

void AngRateIndicator::LoadVC (int vcid)
{
	vtx0 = &vtxbuf_VC;
	w0 = &w0_VC;
	cost = &cost_VC, sint = &sint_VC;
	xcnt = xcnt_VC, ycnt = ycnt_VC, zcnt = zcnt_VC;
}

void AngRateIndicator::UncoverScale (int which, int axis, double phi, NTVERTEX *vtx)
{
	int vofs = which*21 + axis*7;
	double x, y;
	if (axis < 2) {
		if (phi < 0) {
			if (phi >= -0.25*PI) {
				vtx[vofs+0].x = (float)(xcnt[axis] + *w0 * tan(phi));
			} else {
				vtx[vofs+0].x = vtx[vofs+1].x = (float)(xcnt[axis] - *w0);
				y = *w0 * tan(phi+PI05);
				vtx[vofs+0].y = vtx[vofs+1].y = (float)(ycnt[which] + y * *cost);
				vtx[vofs+0].z = vtx[vofs+1].z = (float)(zcnt[which] + y * *sint);
			}
		} else {
			if (phi <= 0.25*PI) {
				vtx[vofs+3].x = (float)(xcnt[axis] + *w0 * tan(phi));
			} else {
				vtx[vofs+3].x = vtx[vofs+4].x = (float)(xcnt[axis] + *w0);
				y = *w0 * tan(PI05-phi);
				vtx[vofs+3].y = vtx[vofs+4].y = (float)(ycnt[which] + y * *cost);
				vtx[vofs+3].z = vtx[vofs+4].z = (float)(zcnt[which] + y * *sint);
			}
		}
	} else {
		if (phi < 0) {
			if (phi >= -0.25*PI) {
				y = *w0 * tan(phi);
				vtx[vofs+3].y = (float)(ycnt[which] + y * *cost);
				vtx[vofs+3].z = (float)(zcnt[which] + y * *sint);
			} else {
				x = *w0 * tan(PI05+phi);
				vtx[vofs+3].x = vtx[vofs+4].x = (float)(xcnt[axis]+x);
				vtx[vofs+3].y = vtx[vofs+4].y;
				vtx[vofs+3].z = vtx[vofs+4].z;
			}
		} else {
			if (phi <= 0.25*PI) {
				y = *w0 * tan(phi);
				vtx[vofs+0].y = (float)(ycnt[which] + y * *cost);
				vtx[vofs+0].z = (float)(zcnt[which] + y * *sint);
			} else {
				x = *w0 * tan(PI05-phi);
				vtx[vofs+0].x = vtx[vofs+1].x = (float)(xcnt[axis]+x);
				vtx[vofs+0].y = vtx[vofs+1].y;
				vtx[vofs+0].z = vtx[vofs+1].z;
			}
		}
	}
}

bool AngRateIndicator::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	double t = oapiGetSimTime();
	if (t < upt && t > upt-1.0) return false;
	upt = t + 0.1;

	static NTVERTEX *vtx = 0;
	if (!vtx) vtx = new NTVERTEX[nvtx];
	memcpy (vtx, *vtx0, nvtx*sizeof(NTVERTEX));

	int axis;
	double v, av, phi;
	char cbuf[16];
	VECTOR3 prm;
	vessel->GetAngularVel(prm);
	for (axis = 0; axis < 3; axis++) {
		v = (axis == 0 ? -prm.y : axis == 1 ? prm.z : prm.x)*DEG;
		if ((av = fabs(v)) > 1e-1) {
			phi = min ((log10(av)+1.0)*40.0*RAD, 0.75*PI);
			if (v < 0) phi = -phi;
			UncoverScale (0, axis, phi, vtx);
		}
		if (surf) {
			ValStr (v, cbuf);
			BlitReadout (0, axis, cbuf, surf);
		}
	}
	vessel->GetAngularAcc(prm);
	for (axis = 0; axis < 3; axis++) {
		v = (axis == 0 ? -prm.y : axis == 1 ? prm.z : prm.x)*DEG;
		if ((av = fabs(v)) > 1e-1) {
			phi = min ((log10(av)+1.0)*40.0*RAD, 0.75*PI);
			if (v < 0) phi = -phi;
			UncoverScale (1, axis, phi, vtx);
		}
		if (surf) {
			ValStr (v, cbuf);
			BlitReadout (1, axis, cbuf, surf);
		}
	}
	vessel->GetAngularMoment(prm);
	for (axis = 0; axis < 3; axis++) {
		v = (axis == 0 ? -prm.y : axis == 1 ? prm.z : prm.x);
		if ((av = fabs(v*1e-3)) > 1e-1) {
			phi = min ((log10(av)+1.0)*40.0*RAD, 0.75*PI);
			if (v < 0) phi = -phi;
			UncoverScale (2, axis, phi, vtx);
		}
		if (surf) {
			ValStr (v, cbuf);
			BlitReadout (2, axis, cbuf, surf);
		}
	}

	GROUPEDITSPEC ges = {GRPEDIT_VTXCRD, 0, vtx, (DWORD)nvtx, 0};
	oapiEditMeshGroup(hMesh, GRP_ANGVEL_DISP_OVR_VC, &ges);
	return false;
}

bool AngRateIndicator::Redraw2D (SURFHANDLE surf)
{
	double t = oapiGetSimTime();
	if (t < upt && t > upt-1.0) return false;
	upt = t + 0.1;
	memcpy (grp->Vtx+vtxofs, *vtx0, nvtx*sizeof(NTVERTEX));

	int axis;
	double v, av, phi;
	VECTOR3 prm;
	vessel->GetAngularVel(prm);
	for (axis = 0; axis < 3; axis++) {
		v = (axis == 0 ? -prm.y : axis == 1 ? prm.z : prm.x)*DEG;
		if ((av = fabs(v)) > 1e-1) {
			phi = min ((log10(av)+1.0)*40.0*RAD, 0.75*PI);
			if (v < 0) phi = -phi;
			UncoverScale (0, axis, phi, grp->Vtx+vtxofs);
		}
	}
	vessel->GetAngularAcc(prm);
	for (axis = 0; axis < 3; axis++) {
		v = (axis == 0 ? -prm.y : axis == 1 ? prm.z : prm.x)*DEG;
		if ((av = fabs(v)) > 1e-1) {
			phi = min ((log10(av)+1.0)*40.0*RAD, 0.75*PI);
			if (v < 0) phi = -phi;
			UncoverScale (1, axis, phi, grp->Vtx+vtxofs);
		}
	}
	vessel->GetAngularMoment(prm);
	for (axis = 0; axis < 3; axis++) {
		v = (axis == 0 ? -prm.y : axis == 1 ? prm.z : prm.x);
		if ((av = fabs(v*1e-3)) > 1e-1) {
			phi = min ((log10(av)+1.0)*40.0*RAD, 0.75*PI);
			if (v < 0) phi = -phi;
			UncoverScale (2, axis, phi, grp->Vtx+vtxofs);
		}
	}
	return false;
}

void AngRateIndicator::ValStr (double v, char *str)
{
	char sgn = '+';
	if (v < 0.0) v = -v, sgn = '-';
	v = fabs(v);
	char pfix = ' ';
	if (v > 0.995e3) {
		v *= 1e-3, pfix = 'k';
		if (v > 0.995e3) {
			v *= 1e-3, pfix = 'M';
			if (v > 0.995e3) {
				v *= 1e-3, pfix = 'G';
				if (v > 9.995e3) {
					strcpy (str, "----");
					return;
				}
			}
		}
	}

	if      (v < 9.995) sprintf (str, "%c%0.2f%c", sgn, v, pfix);
	else if (v < 99.95) sprintf (str, "%c%0.1f%c", sgn, v, pfix);
	else                sprintf (str, "%c%0.0f%c", sgn, v, pfix);
}

void AngRateIndicator::BlitReadout (int which, int axis, const char *str, SURFHANDLE tgt)
{
	int tgtx = 3;
	int tgty = 3 + (which*3 + axis)*16;
	int srcx, srcy = 0;
	int w = 8;
	int h = 11;
	char c, *tgtstr = label[which][axis];
	for (int i = 0; i < 6; i++) {
		if ((c=str[i]) != tgtstr[i]) {
			if (c >= '0' && c <= '9') srcx = (c-'0')*8;
			else switch (c) {
				case '.': srcx = 10*8; break;
				case '+': srcx = 11*8; break;
				case '-': srcx = 12*8; break;
				case 'k': srcx = 13*8; break;
				case 'M': srcx = 14*8; break;
				case 'G': srcx = 15*8; break;
				default:  srcx = 16*8; break;
			}
			oapiBlt (tgt, bsrc, tgtx, tgty, srcx, srcy, w, h);
			tgtstr[i] = c;
		}
		tgtx += w;
	}
}
