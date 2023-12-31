// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// adiball.cpp
// Panel interface ADI ball
// ==============================================================

#define STRICT 1
#include "adiball.h"
#include "attref.h"

using std::min;
using std::max;

// Generic ADI texture parameters
static const float texw = 512.0f;  // ADI texture width
static const float texh = 512.0f;  // ADI texture height
static const float tx_x0 = 240.0f;
static const float tx_y0 = 2.0f;
static const float tx_dx   = 240.0f;
static const float tx_dy   = 480.0f;
static const float bank_tx_x0   = 1.0f;
static const float bank_tx_y0   = texh-24.0f;
static const float bank_tx_dx   = 18.0f;
static const float bank_tx_dy   = 23.0f;
static const float rate_tx_x0   = 23.5f;
static const float rate_tx_y0   = texh-20.5f;
static const float rate_tx_dx   = 13.0f;
static const float rate_tx_dy   = 19.0f;
static const float needle_tx_xcnt = 140.0f;
static const float needle_tx_w2   =  99.5f;
static const float needle_tx_y0   = texh-11.5f;
static const float needle_tx_dy   =  11.0f;
static const float tx_tf_x0     = 40.5;
static const float tx_tf_y0     = texh-19.5f;
static const float tx_tf_dx     = 20.0f;
static const float tx_tf_dy     =  8.0f;

// Shuttle-A panel specific parameters
static const float bb_cntx = (float)ADIBALL_X+84.0f;
static const float bb_cnty = (float)ADIBALL_Y+84.0f;

static const float bank_rad  =  59.0f;
static const float bank_dy   =  12.0f;
static const float bank_dx2  =   5.0f;

static const float rate_dr    = 73.0f;
static const float rate_w2    =  4.0f;
static const float rate_h     = 10.0f;

static const float needle_r1  = 67.0f;
static const float needle_r2  = 40.0f;
static const float needle_w2  =  4.0f;

static const float bb_tf_x0   = bb_cntx-77.5f;
static const float bb_tf_y0   = bb_cnty-59.5f;

static const double rad = 68.0;

// ==============================================================

ADIBall::ADIBall (VESSEL3 *v, AttitudeReference *attref): PanelElement (v)
{
	aref = attref;
	layout = 0;
	rho_curr = tht_curr = phi_curr = 0.0;
	tgtx_curr = tgty_curr = 0.0;
	ballvtx0 = 0;
	peuler = {0,0,0};
	vrot = {0,0,0};
	euler_t = 0;
	rate_local = true;
}

// ==============================================================

ADIBall::~ADIBall ()
{
	if (ballvtx0) delete []ballvtx0;
}

// ==============================================================

void ADIBall::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx_ball, DWORD grpidx_ind)
{
	// We need two separate mesh groups for the ball and indicators, because the ball is typically
	// rendered below the panel mesh, and the indicators above it

	static const DWORD nvtx_rect = 4;
	static const DWORD nidx_rect = 6;
	static const WORD idx_rect[nidx_rect] = { 0,1,2,  3,2,1 };

	DWORD i, nidx;
	NTVERTEX *vtx;
	WORD *idx;

	// ball mesh
	MakeBall (6, rad, vtx, nballvtx, idx, nidx);
	if (ballvtx0) delete []ballvtx0;
	ballvtx0 = new VECTOR3[nballvtx];
	for (i = 0; i < nballvtx; i++) {
		ballvtx0[i].x = vtx[i].x;
		ballvtx0[i].y = vtx[i].y;
		ballvtx0[i].z = vtx[i].z;
		vtx[i].x += bb_cntx;
		vtx[i].y += bb_cnty;
		vtx[i].z = 0;
	}
	if (layout == 1)
		for (i = 0; i < nballvtx; i++)
			vtx[i].tu += tx_dx/texw;

	AddGeometry (hMesh, grpidx_ball, vtx, nballvtx, idx, nidx);
	ballgrp = grp;
	ballofs = vtxofs;

	// Roll indicator mesh
	static const NTVERTEX bvtx[nvtx_rect] = {
		{bb_cntx-bank_dx2,bb_cnty-bank_rad,        0,  0,0,0,  bank_tx_x0/texw,             bank_tx_y0/texh             },
		{bb_cntx+bank_dx2,bb_cnty-bank_rad,        0,  0,0,0,  (bank_tx_x0+bank_tx_dx)/texw,bank_tx_y0/texh             },
		{bb_cntx-bank_dx2,bb_cnty-bank_rad+bank_dy,0,  0,0,0,  bank_tx_x0/texw,             (bank_tx_y0+bank_tx_dy)/texh},
		{bb_cntx+bank_dx2,bb_cnty-bank_rad+bank_dy,0,  0,0,0,  (bank_tx_x0+bank_tx_dx)/texw,(bank_tx_y0+bank_tx_dy)/texh}
	};
	AddGeometry (hMesh, grpidx_ind, bvtx, nvtx_rect, idx_rect, nidx_rect);
	indgrp = grp;
	rollindofs = vtxofs;

	// Pitch error needle
	static const NTVERTEX pevtx[nvtx_rect] = {
		{bb_cntx-needle_r2,bb_cnty+needle_w2,0,  0,0,0,  (needle_tx_xcnt-needle_tx_w2*needle_r2/needle_r1)/texw,needle_tx_y0/texh               },
		{bb_cntx-needle_r2,bb_cnty-needle_w2,0,  0,0,0,  (needle_tx_xcnt-needle_tx_w2*needle_r2/needle_r1)/texw,(needle_tx_y0+needle_tx_dy)/texh},
		{bb_cntx+needle_r1,bb_cnty+needle_w2,0,  0,0,0,  (needle_tx_xcnt+needle_tx_w2)/texw,                    needle_tx_y0/texh               },
		{bb_cntx+needle_r1,bb_cnty-needle_w2,0,  0,0,0,  (needle_tx_xcnt+needle_tx_w2)/texw,                    (needle_tx_y0+needle_tx_dy)/texh}
	};
	AddGeometry (hMesh, grpidx_ind, pevtx, nvtx_rect, idx_rect, nidx_rect);
	peofs = vtxofs;

	// Yaw error needle
	static const NTVERTEX yevtx[nvtx_rect] = {
		{bb_cntx-needle_w2,bb_cnty-needle_r2,0,  0,0,0,  (needle_tx_xcnt-needle_tx_w2*needle_r2/needle_r1)/texw,(needle_tx_y0+needle_tx_dy)/texh},
		{bb_cntx+needle_w2,bb_cnty-needle_r2,0,  0,0,0,  (needle_tx_xcnt-needle_tx_w2*needle_r2/needle_r1)/texw,needle_tx_y0/texh               },
		{bb_cntx-needle_w2,bb_cnty+needle_r1,0,  0,0,0,  (needle_tx_xcnt+needle_tx_w2)/texw,                    (needle_tx_y0+needle_tx_dy)/texh},
		{bb_cntx+needle_w2,bb_cnty+needle_r1,0,  0,0,0,  (needle_tx_xcnt+needle_tx_w2)/texw,                    needle_tx_y0/texh               }
	};
	AddGeometry (hMesh, grpidx_ind, yevtx, nvtx_rect, idx_rect, nidx_rect);
	yeofs = vtxofs;

	// To/From indicator
	static const NTVERTEX tfvtx[nvtx_rect] = {
		{bb_tf_x0,         bb_tf_y0,         0,  0,0,0,  tx_tf_x0/texw,           tx_tf_y0/texh},
		{bb_tf_x0+tx_tf_dx,bb_tf_y0,         0,  0,0,0,  (tx_tf_x0+tx_tf_dx)/texw,tx_tf_y0/texh},
		{bb_tf_x0,         bb_tf_y0+tx_tf_dy,0,  0,0,0,  tx_tf_x0/texw,           (tx_tf_y0+tx_tf_dy)/texh},
		{bb_tf_x0+tx_tf_dx,bb_tf_y0+tx_tf_dy,0,  0,0,0,  (tx_tf_x0+tx_tf_dx)/texw,(tx_tf_y0+tx_tf_dy)/texh}
	};
	AddGeometry (hMesh, grpidx_ind, tfvtx, nvtx_rect, idx_rect, nidx_rect);
	tfofs = vtxofs;

	// Pitch rate indicator mesh
	static const NTVERTEX prvtx[nvtx_rect] = {
		{bb_cntx+rate_dr,       bb_cnty+rate_w2,0,  0,0,0,  rate_tx_x0/texw,             rate_tx_y0/texh             },
		{bb_cntx+rate_dr,       bb_cnty-rate_w2,0,  0,0,0,  (rate_tx_x0+rate_tx_dx)/texw,rate_tx_y0/texh             },
		{bb_cntx+rate_dr+rate_h,bb_cnty+rate_w2,0,  0,0,0,  rate_tx_x0/texw,             (rate_tx_y0+rate_tx_dy)/texh},
		{bb_cntx+rate_dr+rate_h,bb_cnty-rate_w2,0,  0,0,0,  (rate_tx_x0+rate_tx_dx)/texw,(rate_tx_y0+rate_tx_dy)/texh}
	};
	AddGeometry (hMesh, grpidx_ind, prvtx, nvtx_rect, idx_rect, nidx_rect);
	prateofs = vtxofs;

	// Roll rate indicator mesh
	static const NTVERTEX rrvtx[nvtx_rect] = {
		{bb_cntx+rate_w2,bb_cnty-rate_dr,       0,  0,0,0,  rate_tx_x0/texw,             rate_tx_y0/texh             },
		{bb_cntx-rate_w2,bb_cnty-rate_dr,       0,  0,0,0,  (rate_tx_x0+rate_tx_dx)/texw,rate_tx_y0/texh             },
		{bb_cntx+rate_w2,bb_cnty-rate_dr-rate_h,0,  0,0,0,  rate_tx_x0/texw,             (rate_tx_y0+rate_tx_dy)/texh},
		{bb_cntx-rate_w2,bb_cnty-rate_dr-rate_h,0,  0,0,0,  (rate_tx_x0+rate_tx_dx)/texw,(rate_tx_y0+rate_tx_dy)/texh}
	};
	AddGeometry (hMesh, grpidx_ind, rrvtx, nvtx_rect, idx_rect, nidx_rect);
	brateofs = vtxofs;

	// Yaw rate indicator mesh
	static const NTVERTEX yrvtx[nvtx_rect] = {
		{bb_cntx-rate_w2,bb_cnty+rate_dr,       0,  0,0,0,  rate_tx_x0/texw,             rate_tx_y0/texh             },
		{bb_cntx+rate_w2,bb_cnty+rate_dr,       0,  0,0,0,  (rate_tx_x0+rate_tx_dx)/texw,rate_tx_y0/texh             },
		{bb_cntx-rate_w2,bb_cnty+rate_dr+rate_h,0,  0,0,0,  rate_tx_x0/texw,             (rate_tx_y0+rate_tx_dy)/texh},
		{bb_cntx+rate_w2,bb_cnty+rate_dr+rate_h,0,  0,0,0,  (rate_tx_x0+rate_tx_dx)/texw,(rate_tx_y0+rate_tx_dy)/texh}
	};
	AddGeometry (hMesh, grpidx_ind, yrvtx, nvtx_rect, idx_rect, nidx_rect);
	yrateofs = vtxofs;

	delete []vtx;
	delete []idx;
}

// ==============================================================

void ADIBall::SetLayout (int _layout)
{
	if (_layout == layout) return;

	layout = _layout;

	if (ballvtx0) {
		float dtu = (layout ? tx_dx : -tx_dx)/texw;
		for (DWORD i = 0; i < nballvtx; i++)
			ballgrp->Vtx[ballofs+i].tu += dtu;
	}

	aref->SetProjMode (layout);
}

// ==============================================================

bool ADIBall::Redraw2D (SURFHANDLE surf)
{
	VECTOR3 euler = aref->GetEulerAngles ();
	double rho = euler.x; // roll angle
	double tht = euler.y; // pitch angle
	double phi = euler.z; // yaw angle
	double dt = oapiGetSimStep();

	const double ballspeed = 3.0;
	double dangle_max = ballspeed*dt;

	double drho = rho-rho_curr;
	if (drho > PI)       drho -= PI2;
	else if (drho < -PI) drho += PI2;
	double dtht = tht-tht_curr;
	if (dtht > PI)       dtht -= PI2;
	else if (dtht < -PI) dtht += PI2;
	double dphi = phi-phi_curr;
	if (dphi > PI)       dphi -= PI2;
	else if (dphi < -PI) dphi += PI2;
	double dangle = max (fabs(drho), max (fabs(dtht), fabs(dphi)));
	if (dangle > dangle_max) {
		double scale = dangle_max/dangle;
		rho = rho_curr + drho*scale;
		tht = tht_curr + dtht*scale;
		phi = phi_curr + dphi*scale;
	}
	rho_curr = rho;
	tht_curr = tht;
	phi_curr = phi;

	DWORD i;

	double sinp = sin(phi), cosp = cos(phi);
	double sint = sin(tht), cost = cos(tht);
	double sinr = sin(rho), cosr = cos(rho);

	// Ball transformation
	double a1, b1, c1, a2, b2, c2, a3, b3, c3;

	if (layout == 0) {
		// below are the coefficients of the rows of the rotation matrix M for the ball,
		// given by M = RTP, with
		// MATRIX3 P = {cosp,0,-sinp,  0,1,0,  sinp,0,cosp};
		// MATRIX3 T = {1,0,0,  0,cost,-sint,  0,sint,cost};
		// MATRIX3 R = {cosr,sinr,0,  -sinr,cosr,0,  0,0,1};

		a1 =  cosr*cosp - sinr*sint*sinp;
		b1 =  sinr*cost;
		c1 = -cosr*sinp - sinr*sint*cosp;
		a2 = -sinr*cosp - cosr*sint*sinp;
		b2 =  cosr*cost;
		c2 =  sinr*sinp - cosr*sint*cosp;
		a3 =  cost*sinp;
		b3 =  sint;
		c3 =  cost*cosp;

	} else {
		// below are the coefficients of the rows of the rotation matrix M for the ball,
		// given by M = ZRPT, with
		// MATRIX3 Z = {0,1,0,  -1,0,0,  0,0,1};
		// MATRIX3 P = {1,0,0,  0,cosp,-sinp,  0,sinp,cosp};  // yaw
		// MATRIX3 T = {cost,0,sint,  0,1,0,  -sint,0,cost};  // pitch
		// MATRIX3 R = {cosr,sinr,0,  -sinr,cosr,0,  0,0,1};  // bank

		a1 = -sinr*cost + cosr*sinp*sint;
		b1 =  cosr*cosp;
		c1 = -sinr*sint - cosr*sinp*cost;
		a2 = -cosr*cost - sinr*sinp*sint;
		b2 = -sinr*cosp;
		c2 = -cosr*sint + sinr*sinp*cost;
		a3 = -cosp*sint;
		b3 =  sinp;
		c3 =  cosp*cost;
	}

	for (i = 0; i < nballvtx; i++) {
		ballgrp->Vtx[ballofs+i].x = bb_cntx + (float)(a1*ballvtx0[i].x + b1*ballvtx0[i].y + c1*ballvtx0[i].z);
		ballgrp->Vtx[ballofs+i].y = bb_cnty - (float)(a2*ballvtx0[i].x + b2*ballvtx0[i].y + c2*ballvtx0[i].z);
	}

	// Roll indicator transformation
	static const float bx[4] = {-bank_dx2,bank_dx2,-bank_dx2,bank_dx2};
	static const float by[4] = {-bank_rad,-bank_rad,-bank_rad+bank_dy,-bank_rad+bank_dy};
	for (i = 0; i < 4; i++) {
		indgrp->Vtx[rollindofs+i].x = bb_cntx + (float)(bx[i]*cosr-by[i]*sinr);
		indgrp->Vtx[rollindofs+i].y = bb_cnty + (float)(bx[i]*sinr+by[i]*cosr);
	}

	// error needles
	double tgtx, tgty;
	static const float yexofs[4] = {bb_cntx-needle_w2,bb_cntx+needle_w2,bb_cntx-needle_w2,bb_cntx+needle_w2};
	static const float peyofs[4] = {bb_cnty+needle_w2,bb_cnty-needle_w2,bb_cnty+needle_w2,bb_cnty-needle_w2};
	const double erange = 42.0;
	int tgtflag;
	VECTOR3 euler_tgt;
	if (!aref->GetTgtEulerAngles (euler_tgt)) {
		tgtx = tgty = 0.0;
		tgtflag = 0;
	} else {
		VECTOR3 tgt;
		double sint = sin(euler_tgt.y), cost = cos(euler_tgt.y);
		double sinp = sin(euler_tgt.z), cosp = cos(euler_tgt.z);
		if (layout == 0) {
			tgt = {rad*sinp*cost, rad*sint, rad*cosp*cost};
		} else {
			tgt = {-rad*sint*cosp, rad*sinp, rad*cost*cosp};
		}
		tgtx = min(max( (a1*tgt.x + b1*tgt.y + c1*tgt.z), -erange), erange);
		tgty = min(max(-(a2*tgt.x + b2*tgt.y + c2*tgt.z), -erange), erange);
		double tgtz =    a3*tgt.x + b3*tgt.y + c3*tgt.z;
		tgtflag = (tgtz >= 0.0 ? 1 : 2);
	}
	const double needlespeed = 50.0;
	double dneedlemax = needlespeed*dt;
	if (fabs(tgtx-tgtx_curr) > dneedlemax)
		tgtx = (tgtx > tgtx_curr ? tgtx_curr + dneedlemax : tgtx_curr - dneedlemax);
	if (fabs(tgty-tgty_curr) > dneedlemax)
		tgty = (tgty > tgty_curr ? tgty_curr + dneedlemax : tgty_curr - dneedlemax);
	tgtx_curr = tgtx;
	tgty_curr = tgty;
	for (i = 0; i < 4; i++) {
		indgrp->Vtx[yeofs+i].x = (float)tgtx+yexofs[i];
		indgrp->Vtx[peofs+i].y = (float)tgty+peyofs[i];
	}
	// to/from indicator
	static float tf_tu[4] = {tx_tf_x0/texw,(tx_tf_x0+tx_tf_dx)/texw,tx_tf_x0/texw,(tx_tf_x0+tx_tf_dx)/texw};
	float tf_dtu = tgtflag * tx_tf_dx/texw;
	for (i = 0; i < 4; i++)
		indgrp->Vtx[tfofs+i].tu = tf_tu[i] + tf_dtu;

	// Rate indicators
	const double rate_scale = DEG*4.0;
	const double rate_max = 41.0;
	if (rate_local) {
		vessel->GetAngularVel (vrot);
	} else {
		double t = oapiGetSimTime();
		if (t > euler_t) {
			VECTOR3 de = (euler-peuler)/(t-euler_t);
			vrot.x =  de.y;
			vrot.y = -de.z;
			vrot.z = -de.x;
			peuler = euler;
			euler_t = t;
		}
	}

	// Pitch rate indicator transformation
	static const float pry[4] = {bb_cnty+rate_w2,bb_cnty-rate_w2,bb_cnty+rate_w2,bb_cnty-rate_w2};
	float dp = (float)max (-rate_max, min (rate_max, vrot.x*rate_scale));
	for (i = 0; i < 4; i++) indgrp->Vtx[prateofs+i].y = pry[i]-dp;

	// Roll rate indicator transformation
	static const float brx[4] = {bb_cntx+rate_w2,bb_cntx-rate_w2,bb_cntx+rate_w2,bb_cntx-rate_w2};
	float db = (float)max (-rate_max, min (rate_max, vrot.z*rate_scale));
	for (i = 0; i < 4; i++) indgrp->Vtx[brateofs+i].x = brx[i]+db;

	// Yaw rate indicator transformation
	static const float yrx[4] = {bb_cntx-rate_w2,bb_cntx+rate_w2,bb_cntx-rate_w2,bb_cntx+rate_w2};
	float dy = (float)max (-rate_max, min (rate_max, vrot.y*rate_scale));
	for (i = 0; i < 4; i++) indgrp->Vtx[yrateofs+i].x = yrx[i]-dy;

	return false;
}

// ==============================================================

void ADIBall::MakeBall (int res, double rad, NTVERTEX *&vtx, DWORD &nvtx, WORD *&idx, DWORD &nidx)
{
	int i, j, k;
	nvtx = (res*2+1) * (res*4+1);
	vtx = new NTVERTEX[nvtx];
	memset (vtx, 0, nvtx*sizeof(NTVERTEX));
	double phi, tht, sinp, cosp, sint, cost;
	double scl = PI05/(double)res;
	float tu;

	for (j = k = 0; j <= res*2; j++) {
		tht = (double)j*scl;
		sint = sin(tht); cost = cos(tht);
		tu = (tx_x0 - (float)j/(float)(res*2)*tx_dx)/texw;
		for (i = 0; i <= res*4; i++) {
			phi = (double)i*scl;
			sinp = sin(phi); cosp = cos(phi);
			vtx[k].x = (float)(sinp*sint*rad);
			vtx[k].y = (float)(cost*rad);
			vtx[k].z = (float)(cosp*sint*rad);
			vtx[k].tu = tu;
			vtx[k].tv = (tx_y0 + (float)i/(float)(res*4)*tx_dy)/texh;
			k++;
		}
	}

	int nrow = res*4+1;
	nidx = res*2 * res*4 * 6;
	idx = new WORD[nidx];
	for (j = k = 0; j < res*2; j++) {
		for (i = 0; i < res*4; i++) {
			idx[k++] = j*nrow+i;
			idx[k++] = j*nrow+i+1;
			idx[k++] = (j+1)*nrow+i;
			idx[k++] = (j+1)*nrow+i;
			idx[k++] = j*nrow+i+1;
			idx[k++] = (j+1)*nrow+i+1;
		}
	}
}
