// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VSOP87_MARS
#define __VSOP87_MARS

#include "Vsop87.h"

// ======================================================================
// class Mars: interface
// ======================================================================

class Mars: public VSOPOBJ {
public:
	Mars (OBJHANDLE hCBody);
	void clbkInit (FILEHANDLE cfg);
	int clbkEphemeris (double mjd, int req, double *ret);
	int clbkFastEphemeris (double simt, int req, double *ret);
};

#endif // !__VSOP87_MARS