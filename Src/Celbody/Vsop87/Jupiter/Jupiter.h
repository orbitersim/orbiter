// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VSOP87_JUPITER
#define __VSOP87_JUPITER

#include "Vsop87.h"

// ======================================================================
// class Jupiter: interface
// ======================================================================

class Jupiter: public VSOPOBJ {
public:
	Jupiter (OBJHANDLE hCBody);
	void clbkInit (FILEHANDLE cfg);
	int clbkEphemeris (double mjd, int req, double *ret);
	int clbkFastEphemeris (double simt, int req, double *ret);

private:
	Sample bsp[2]; // barycentre offset interpolation
};

#endif // !__VSOP87_JUPITER