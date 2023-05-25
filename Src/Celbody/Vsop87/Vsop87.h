// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VSOP_H
#define __VSOP_H

#include "OrbiterAPI.h"
#include "CelbodyAPI.h"

#define VSOP_MAXALPHA 5		// max power of time

typedef int IDX3[3];
typedef double TERM3[3];

// ===========================================================
// class VSOPOBJ
// Base class for planets controlled by VSOP87 solutions
// ===========================================================

class DLLEXPORT VSOPOBJ: public CELBODY2 {
public:
	VSOPOBJ (OBJHANDLE hCBody);
	virtual ~VSOPOBJ ();
	bool bEphemeris() const;
	void clbkInit (FILEHANDLE cfg);

protected:
	void SetSeries (char series);
	// Set VSOP series ('A' to 'E')

	bool ReadData (const char *name);
	// Read perturbation terms up to required accuracy from data file

	void Init ();

	void VsopEphem (double mjd, double *ret);
	// Calculate ephemerides

	void VsopFastEphem (double simt, double *ret);
	// Interpolated sequential ephemerides

	double a0;       // semi-major axis [AU]
	double prec;     // tolerance limit (1e-3 .. 1e-8)
	double interval; // sample interval for fast ephemeris [s]
	int fmtflag;     // data format flag
	int nalpha;      // order of time polynomials
	IDX3 *termidx;   // term index list
	IDX3 *termlen;   // term list lengths
	TERM3 *term;     // term list
	Sample sp[2];

private:
	void Interpolate (double t, double *data, const Sample *s0, const Sample *s1);

	char sid;
	int datatp;  // return data type: true pos or barycentric
};

#endif // !__VSOP_H