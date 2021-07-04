// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2011 Martin Schweiger
//                   All rights reserved
//
// needlepair.cpp
// Panel element: Pair of indicator needles
// ==============================================================

#define STRICT 1
#include "needlepair.h"
#include "paneltext.h"

static const float texw = (float)PANELEL_TEXW;
static const float texh = (float)PANELEL_TEXH;

// ==============================================================

NeedlePair::NeedlePair (VESSEL3 *v, float basex, float basey, float range, int readout_ty): PanelElement (v)
{
	bx  = basex;
	by  = basey;
	rng = range;
	rty = readout_ty;
	for (int i = 0; i < 2; i++)
		displevel[i] = 0.0;
}

// ==============================================================

void NeedlePair::AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx)
{
	static const DWORD nvtx = 6+4;
	static const DWORD nidx = 6+6;
	const NTVERTEX vtx[nvtx] = {
		// needles
		{bx-20.0f, by-5.0f,0,  0,0,0,  61.5f/texw,  (texh-44.0f)/texh},
		{bx- 5.0f, by,     0,  0,0,0,  78.5f/texw,  (texh-35.0f)/texh},
		{bx-20.0f, by+5.0f,0,  0,0,0,  61.5f/texw,  (texh-26.0f)/texh},
		{bx+20.0f, by-5.0f,0,  0,0,0,  61.5f/texw,  (texh-44.0f)/texh},
		{bx+ 5.0f, by,     0,  0,0,0,  78.5f/texw,  (texh-35.0f)/texh},
		{bx+20.0f, by+5.0f,0,  0,0,0,  61.5f/texw,  (texh-26.0f)/texh},
		// readout area
		{bx-14.0f, by-96.5f,0, 0,0,0,  (texw-207.0f)/texw, (texh-rty-15)/texh},
		{bx+11.0f, by-96.5f,0, 0,0,0,  (texw-182.0f)/texw, (texh-rty-15)/texh},
		{bx-14.0f, by-81.5f,0, 0,0,0,  (texw-207.0f)/texw, (texh-rty)/texh},
		{bx+11.0f, by-81.5f,0, 0,0,0,  (texw-182.0f)/texw, (texh-rty)/texh}
	};
	static const WORD idx[nidx] = {
		0,1,2,  3,5,4,
		6,7,8,  9,8,7
	};
	AddGeometry (hMesh, grpidx, vtx, nvtx, idx, nidx);
}

// ==============================================================

bool NeedlePair::Redraw2D (SURFHANDLE surf, double level[2])
{
	const float y[3] = {by-5.0f, by, by+5.0f};
	for (int needle = 0; needle < 2; needle++) {
		if (level[needle] != displevel[needle]) {
			int ofs = vtxofs + needle*3;
			for (int i = 0; i < 3; i++)
				grp->Vtx[ofs++].y = y[i] - (float)level[needle]*rng;
			displevel[needle] = level[needle];
		}
	}
	return false;
}

// ==============================================================
// ==============================================================

Throttle_NeedlePair::Throttle_NeedlePair (VESSEL3 *v, float basex, float basey, float range,
	int readout_ty, double maxflow, THRUSTER_HANDLE *hthrust)
: NeedlePair (v, basex, basey, range, readout_ty)
{
	th = hthrust;
	mxflow = maxflow;
	for (int i = 0; i < 2; i++)
		readout[i][0] = '\0';
}

// ==============================================================

bool Throttle_NeedlePair::Redraw2D (SURFHANDLE surf)
{
	double readout_val[2], acc = 0.0, F = 0.0;
	double flow[2];
	ShuttleA *sh = (ShuttleA*)vessel;
	for (int i = 0; i < 2; i++) {
		double Fi = sh->GetThrusterMax (th[i]) * sh->GetThrusterLevel(th[i]);
		F += Fi;
		flow[i] = Fi/sh->GetThrusterIsp (th[i]);
	}
	acc = F/sh->GetMass();
	readout_val[0] = F*1e-3;
	readout_val[1] = acc;
	Redraw2D_readouts (surf, readout_val);

	double level[2];
	for (int i = 0; i < 2; i++)
		level[i] = flow[i]/mxflow;
	return NeedlePair::Redraw2D (surf, level);
}

// ==============================================================

void Throttle_NeedlePair::Redraw2D_readouts (SURFHANDLE surf, double *val)
{
	char cbuf[256];
	sprintf (cbuf, "%04.0f", val[0]);
	BltStr (surf, surf, PANELEL_TEXW-184, PANELEL_TEXH-rty-15, cbuf, readout[0], ALIGN_RIGHT);
	sprintf (cbuf, "%0.2f", val[1]);
	BltStr (surf, surf, PANELEL_TEXW-184, PANELEL_TEXH-rty-8, cbuf, readout[1], ALIGN_RIGHT);
}

// ==============================================================
// ==============================================================

Propellant_NeedlePair::Propellant_NeedlePair (VESSEL3 *v, float basex, float basey, float range,
	int readout_ty, double maxmass, double maxflow, PROPELLANT_HANDLE hprop)
	: NeedlePair (v, basex, basey, range, readout_ty)
{
	ph = hprop;
	mxmass = maxmass;
	mxflow = maxflow;
	for (int i = 0; i < 2; i++)
		readout[i][0] = '\0';
}

// ==============================================================

bool Propellant_NeedlePair::Redraw2D (SURFHANDLE surf)
{
	double readout_val[2];
	ShuttleA *sh = (ShuttleA*)vessel;
	double fmass = sh->GetPropellantMass (ph);
	double mass  = sh->GetMass();
	double flow = sh->GetPropellantFlowrate (ph);
	double dv = fmass/mass * sh->GetISP(); // fix this
	readout_val[0] = fmass;
	readout_val[1] = dv;
	Redraw2D_readouts (surf, readout_val);

	double level[2];
	level[0] = fmass/mxmass;
	level[1] = flow/mxflow;
	return NeedlePair::Redraw2D (surf, level);
}

// ==============================================================

void Propellant_NeedlePair::Redraw2D_readouts (SURFHANDLE surf, double *val)
{
	char cbuf[256];
	sprintf (cbuf, "%04.0f", val[0]);
	BltStr (surf, surf, PANELEL_TEXW-182, PANELEL_TEXH-rty-15, cbuf, readout[0], ALIGN_RIGHT);
	sprintf (cbuf, "%04.0f", val[1]);
	BltStr (surf, surf, PANELEL_TEXW-182, PANELEL_TEXH-rty-8, cbuf, readout[1], ALIGN_RIGHT);
}

