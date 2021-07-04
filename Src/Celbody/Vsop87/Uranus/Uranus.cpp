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

DLLCLBK void InitModule (HINSTANCE hModule)
{}

DLLCLBK void ExitModule (HINSTANCE hModule)
{}

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Uranus (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Uranus*)body;
}
