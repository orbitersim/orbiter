// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//
// needlepair.h
// Panel element: Pair of indicator needles
// ==============================================================

#ifndef __NEEDLEPAIR_H
#define __NEEDLEPAIR_H

#include "ShuttleA.h"
#include "..\Common\Instrument.h"

// ==============================================================

class NeedlePair: public PanelElement {
public:
	NeedlePair (VESSEL3 *v, float basex, float basey, float range, int readout_ty);
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx);
	bool Redraw2D (SURFHANDLE surf, double level[2]);

protected:
	float bx, by, rng;
	double displevel[2];
	int rty;
};

// ==============================================================

class Throttle_NeedlePair: public NeedlePair {
public:
	Throttle_NeedlePair (VESSEL3 *v, float basex, float basey, float range, int readout_ty, double maxflow, THRUSTER_HANDLE *hthrust);
	bool Redraw2D (SURFHANDLE surf);
	void Redraw2D_readouts (SURFHANDLE surf, double *val);

protected:
	char readout[2][10];
	double mxflow;
	THRUSTER_HANDLE *th;
};

// ==============================================================

class MainThrottle_NeedlePair: public Throttle_NeedlePair {
public:
	MainThrottle_NeedlePair (VESSEL3 *v, float basex, float basey, float range, THRUSTER_HANDLE *hthrust)
		: Throttle_NeedlePair (v, basex, basey, range, 0, 40.0, hthrust)
	{}
};

// ==============================================================

class Propellant_NeedlePair: public NeedlePair {
public:
	Propellant_NeedlePair (VESSEL3 *v, float basex, float basey, float range, int readout_ty, double maxmass, double maxflow, PROPELLANT_HANDLE hprop);
	bool Redraw2D (SURFHANDLE surf);
	void Redraw2D_readouts (SURFHANDLE surf, double *val);

protected:
	char readout[2][10];
	double mxmass, mxflow;
	PROPELLANT_HANDLE ph;
};

#endif // !__NEEDLEPAIR_H