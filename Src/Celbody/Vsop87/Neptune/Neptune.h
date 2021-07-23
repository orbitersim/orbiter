// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VSOP87_NEPTUNE
#define __VSOP87_NEPTUNE

#include "Vsop87.h"

// ======================================================================
// class Neptune: interface
// ======================================================================

class Neptune: public VSOPOBJ {
public:
	Neptune (OBJHANDLE hCBody);
	void clbkInit (FILEHANDLE cfg);
	int clbkEphemeris (double mjd, int req, double *ret);
	int clbkFastEphemeris (double simt, int req, double *ret);
};

#endif // !__VSOP87_NEPTUNE