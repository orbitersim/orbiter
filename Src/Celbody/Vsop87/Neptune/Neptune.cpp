// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE

#include "Neptune.h"

// ======================================================================
// class Neptune: implementation
// ======================================================================

Neptune::Neptune (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0 = 30.1;   // semi-major axis [AU]
}

void Neptune::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	ReadData ("Neptune");
}

int Neptune::clbkEphemeris (double mjd, int req, double *ret)
{
	VsopEphem (mjd, ret+6);
	return fmtflag | EPHEM_BARYPOS | EPHEM_BARYVEL;
}

int Neptune::clbkFastEphemeris (double simt, int req, double *ret)
{
	VsopFastEphem (simt, ret+6);
	return fmtflag | EPHEM_BARYPOS | EPHEM_BARYVEL;
}


// ======================================================================
// API interface
// ======================================================================

#ifndef __linux__
DLLCLBK void InitModule (HINSTANCE hModule)
#else // __linux__
DLLCLBK void InitModule (void *hModule)
#endif // __linux__
{}

#ifndef __linux__
DLLCLBK void ExitModule (HINSTANCE hModule)
#else // __linux__
DLLCLBK void ExitModule (void *hModule)
#endif // __linux__
{}

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Neptune (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Neptune*)body;
}
