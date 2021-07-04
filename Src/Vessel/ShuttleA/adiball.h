// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// adiball.h
// Panel interface ADI ball
// ==============================================================

#ifndef __ADIBALL_H
#define __ADIBALL_H

#include "ShuttleA.h"
#include "..\Common\Instrument.h"

// ==============================================================

class ADIBall: public PanelElement {
public:
	ADIBall (VESSEL3 *v, AttitudeReference *attref);
	~ADIBall ();
	void AddMeshData2D (MESHHANDLE hMesh, DWORD grpidx_ball, DWORD grpidx_ind);
	void SetLayout (int _layout);
	inline void SetRateMode (bool local) { rate_local = local; }
	bool Redraw2D (SURFHANDLE surf);
	
protected:
	void MakeBall (int res, double rad, NTVERTEX *&vtx, DWORD &nvtx, WORD *&idx, DWORD &nidx);

private:
	int layout;           // 0: pitch range=-90..90, 1: pitch range=0..360
	AttitudeReference *aref;
	double rho_curr, tht_curr, phi_curr;  // current Euler angles
	double tgtx_curr, tgty_curr;          // current error needle positions
	VECTOR3 *ballvtx0;    // untransformed ball vertex coordinates
	MESHGROUP *ballgrp;
	MESHGROUP *indgrp;
	DWORD nballvtx;
	DWORD ballofs;
	DWORD rollindofs;
	DWORD prateofs, brateofs, yrateofs;
	DWORD yeofs, peofs, tfofs;

	VECTOR3 peuler;
	VECTOR3 vrot;
	double euler_t;
	bool rate_local;
};

#endif // !__ADIBALL_H