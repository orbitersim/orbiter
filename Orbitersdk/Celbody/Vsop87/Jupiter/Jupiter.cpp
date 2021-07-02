#define ORBITER_MODULE

#include "Jupiter.h"
#include "..\Galsat\Galsat.h"

// ======================================================================
// class Jupiter: implementation
// ======================================================================

Jupiter::Jupiter (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0 = 5.2;       // semi-major axis [AU]
	bsp[0].t = bsp[1].t = -1e20; // invalidate
	bsp[0].rad = bsp[1].rad = 1;
	for (int i = 0; i < 6; i++)
		bsp[0].param[i] = bsp[1].param[i] = 0.0;
}

void Jupiter::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	ReadData ("Jupiter");
}

int Jupiter::clbkEphemeris (double mjd, int req, double *ret)
{
	// This version adds barycentric offset to VSOP result
	// so that true Jupiter position is returned

	static double r[6];
	int i;

	// Get barycentre data from VSOP
	VsopEphem (mjd, r);

	// Convert to cartesian
	Pol2Crt (r, ret);

	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (i = 0; i < 6; i++) ret[i+6] = ret[i];

	if (req & (EPHEM_TRUEPOS | EPHEM_TRUEVEL)) {
		// Get barycentre offset from GALSAT
		JupiterBaryEphemeris (mjd, r);
		for (int i = 0; i < 6; i++) ret[i] += r[i];
	}

	return req & 0xF;
}

int Jupiter::clbkFastEphemeris (double simt, int req, double *ret)
{
	// This version adds barycentric offset to VSOP result
	// so that true Jupiter position is returned

	static double r[6];

	// Get barycentre interpolation from VSOP
	VsopFastEphem (simt, r);

	// Convert to cartesian
	Pol2Crt (r, ret+6);

	// Get barycentre offset interpolation from GALSAT
	JupiterBaryFastEphemeris (simt, r, bsp);
	for (int i = 0; i < 6; i++) ret[i] = ret[i+6]+r[i];

	return 0xF;
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
	return new Jupiter (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Jupiter*)body;
}
