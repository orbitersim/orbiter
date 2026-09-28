// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE

#include "Uranus.h"

// ======================================================================
// class Uranus: implementation
// ======================================================================

Uranus::Uranus (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0 = 19.2;   // semi-major axis [AU]
}

void Uranus::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	ReadData ("Uranus");
}

int Uranus::clbkEphemeris (double mjd, int req, double *ret)
{
	VsopEphem (mjd, ret+6);
	return fmtflag | EPHEM_BARYPOS | EPHEM_BARYVEL;
}

int Uranus::clbkFastEphemeris (double simt, int req, double *ret)
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
	return new Uranus (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Uranus*)body;
}
