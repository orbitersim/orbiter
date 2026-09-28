// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE

#include "Earth.h"

// ======================================================================
// class Earth: implementation
// ======================================================================

Earth::Earth (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0         = 1.0;   // semi-major axis [AU]
}

void Earth::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	ReadData ("Earth");
}

int Earth::clbkEphemeris (double mjd, int req, double *ret)
{
	VsopEphem (mjd, ret);
	return fmtflag | EPHEM_TRUEPOS | EPHEM_TRUEVEL;
}

int Earth::clbkFastEphemeris (double simt, int req, double *ret)
{
	VsopFastEphem (simt, ret);
	return fmtflag | EPHEM_TRUEPOS | EPHEM_TRUEVEL;
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
	return new Earth (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Earth*)body;
}
