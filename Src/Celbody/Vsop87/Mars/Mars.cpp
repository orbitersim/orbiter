// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE

#include "Mars.h"

// ======================================================================
// class Mars: implementation
// ======================================================================

Mars::Mars (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0 = 1.5;   // semi-major axis [AU]
}

void Mars::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	ReadData ("Mars");
}

int Mars::clbkEphemeris (double mjd, int req, double *ret)
{
	VsopEphem (mjd, ret+6);
	return fmtflag | EPHEM_BARYPOS | EPHEM_BARYVEL;
}

int Mars::clbkFastEphemeris (double simt, int req, double *ret)
{
	VsopFastEphem (simt, ret+6);
	return fmtflag | EPHEM_BARYPOS | EPHEM_BARYVEL;
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
	return new Mars (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Mars*)body;
}
