// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __GALSAT_H
#define __GALSAT_H

#include "OrbiterAPI.h"
#include "CelbodyAPI.h"

#define GAL_BARYCENTRE 0
#define GAL_IO         1
#define GAL_EUROPA     2
#define GAL_GANYMEDE   3
#define GAL_CALLISTO   4

// ===========================================================
// class GALOBJ
// Base class for Galilean Jupiter moons controlled by
// GALSAT solutions
// ===========================================================

class DLLEXPORT GALOBJ: public CELBODY2 {
public:
	GALOBJ (OBJHANDLE hObj);
	bool bEphemeris() const;
	int  clbkEphemeris (double mjd, int req, double *ret);
	int  clbkFastEphemeris (double simt, int req, double *ret);
	void clbkInit (FILEHANDLE cfg);

protected:
	int ksat;        // object id
	double interval; // sample interval
	Sample sp[2];    // interpolation samples
};

// ===========================================================
// This is used by Jupiter to obtain its barycentric position
// ===========================================================

DLLEXPORT void JupiterBaryEphemeris (double mjd, double *ret);
DLLEXPORT void JupiterBaryFastEphemeris (double simt, double *ret, Sample *sp);

// ===========================================================
// Lieske driver functions
// ===========================================================

int cd2com (char *fname);   // read data from file
void chkgal (void);         // set up data
void galsat (double *r, double *rorb, double tjd, int ksat, int kflag);
							// calculate ephemerides

#endif // !__GALSAT_H