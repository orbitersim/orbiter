// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE

#include "Venus.h"

// ======================================================================
// class Venus: implementation
// ======================================================================

Venus::Venus (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0 = 0.72;  // semi-major axis [AU]
}

void Venus::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	ReadData ("Venus");
}

int Venus::clbkEphemeris (double mjd, int req, double *ret)
{
	VsopEphem (mjd, ret);
	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (int i = 6; i < 12; i++) ret[i] = ret[i-6];
	return req | fmtflag | (EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_BARYISTRUE);
}

int Venus::clbkFastEphemeris (double simt, int req, double *ret)
{
	VsopFastEphem (simt, ret);
	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (int i = 6; i < 12; i++) ret[i] = ret[i-6];
	return req | fmtflag | (EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_BARYISTRUE);
}

// ======================================================================
// API interface
// ======================================================================

DLLCLBK void InitModule (HINSTANCE hModule)
{}

DLLCLBK void ExitModule (HINSTANCE hModule)
{}

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Venus (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Venus*)body;
}
