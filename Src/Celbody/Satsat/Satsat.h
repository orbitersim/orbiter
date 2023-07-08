// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __SATSAT_H
#define __SATSAT_H

#define SKIP_MFD_API
#include "OrbiterAPI.h"
#include "CelbodyAPI.h"

#define SAT_MIMAS     0
#define SAT_ENCELADUS 1
#define SAT_TETHYS    2
#define SAT_DIONE     3
#define SAT_RHEA      4
#define SAT_TITAN     5
#define SAT_HYPERION  6
#define SAT_IAPETUS   7

// ===========================================================
// class SATOBJ
// Base class for Saturn moons controlled by
// TASS17 solutions
// ===========================================================

class DLLEXPORT SATOBJ: public CELBODY2 {
public:
	SATOBJ (OBJHANDLE hObj, int is, double dt);
	bool bEphemeris() const;
	int  clbkEphemeris (double mjd, int req, double *ret);
	int  clbkFastEphemeris (double simt, int req, double *ret);

protected:
	int ksat;     // object id
};

DLLEXPORT void SaturnEphemeris (double mjd, double *ret);
DLLEXPORT void SaturnFastEphemeris (double simt, double *ret);
// Returns Saturn's true position w.r.t. the barycentre of the
// Saturn system (full and interpolated solutions)
// Only Titan is used for barycentre calculation. Contributions
// from other moons are considered negligible

// ===========================================================
// TASS17 driver functions
// ===========================================================

int posired (double dj, int is, double *xyz, double *vxyz);
int nterm (int is);
void ReadData (const char *fname, int res);

#endif // !__SATSAT_H