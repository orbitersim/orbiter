// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __VSOP87_EARTH
#define __VSOP87_EARTH

#include "Vsop87.h"

// ======================================================================
// class Earth: interface
// ======================================================================

class Earth: public VSOPOBJ {
public:
	Earth (OBJHANDLE hCBody);
	void clbkInit (FILEHANDLE cfg);
	int clbkEphemeris (double mjd, int req, double *ret);
	int clbkFastEphemeris (double simt, int req, double *ret);
};

#endif // !__VSOP87_EARTH