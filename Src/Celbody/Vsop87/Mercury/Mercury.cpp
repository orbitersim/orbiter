#define ORBITER_MODULE

#include "Mercury.h"

// ======================================================================
// class Mercury: implementation
// ======================================================================

Mercury::Mercury (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0 = 0.39;  // semi-major axis [AU]
}

void Mercury::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	ReadData ("Mercury");
}

int Mercury::clbkEphemeris (double mjd, int req, double *ret)
{
	VsopEphem (mjd, ret);
	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (int i = 6; i < 12; i++) ret[i] = ret[i-6];
	return req | fmtflag | (EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_BARYISTRUE);
}

int Mercury::clbkFastEphemeris (double simt, int req, double *ret)
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
	return new Mercury (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Mercury*)body;
}
