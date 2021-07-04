#define ORBITER_MODULE

#include "Saturn.h"
#include "..\Satsat\Satsat.h"

// ======================================================================
// class Saturn: implementation
// ======================================================================

Saturn::Saturn (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0 = 9.6;    // semi-major axis [AU]
}

void Saturn::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	ReadData ("Saturn");
}

int Saturn::clbkEphemeris (double mjd, int req, double *ret)
{
	// This version adds barycentric offset to VSOP result
	// so that true Saturn position is returned

	static double r[6];
	int i;

	// Get barycentre data from VSOP
	VsopEphem (mjd, r);

	// Convert to cartesian
	Pol2Crt (r, ret);

	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (i = 0; i < 6; i++) ret[i+6] = ret[i];

	if (req & (EPHEM_TRUEPOS | EPHEM_TRUEVEL)) {
		// Get barycentre offset from SATSAT
		SaturnEphemeris (mjd, r);
		for (i = 0; i < 6; i++) ret[i] += r[i];
	}

	return req & 0xF;
	//return fmtflag | EPHEM_BARYPOS | EPHEM_BARYVEL;
}

int Saturn::clbkFastEphemeris (double simt, int req, double *ret)
{
	static double r[6];
	int i;

	// Get barycentre interpolation from VSOP
	VsopFastEphem (simt, r);

	// Convert to cartesian
	Pol2Crt (r, ret+6);

	// Get barycentre offset interpolation from SATSAT
	SaturnFastEphemeris (simt, r);
	for (i = 0; i < 6; i++) ret[i] = ret[i+6]+r[i];

	return EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_BARYPOS | EPHEM_BARYVEL;
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
	return new Saturn (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Saturn*)body;
}
