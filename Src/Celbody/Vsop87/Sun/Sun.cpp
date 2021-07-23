// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE

#include "Sun.h"

// ======================================================================
// class Sun: implementation
// ======================================================================

Sun::Sun (OBJHANDLE hCBody): VSOPOBJ (hCBody)
{
	a0 = 1.0;           // think about this
}

void Sun::clbkInit (FILEHANDLE cfg)
{
	VSOPOBJ::clbkInit (cfg);
	SetSeries ('E');
	fmtflag = 0;
	ReadData ("Sun");
}

int Sun::clbkEphemeris (double mjd, int req, double *ret)
{
	if (req & (EPHEM_TRUEPOS | EPHEM_TRUEVEL))
		VsopEphem (mjd, ret);
	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (int i = 6; i < 12; i++) ret[i] = 0.0;
	return (req & 0xF) | fmtflag;
}

int Sun::clbkFastEphemeris (double simt, int req, double *ret)
{
	if (req & (EPHEM_TRUEPOS | EPHEM_TRUEVEL))
		VsopFastEphem (simt, ret);
	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (int i = 6; i < 12; i++) ret[i] = 0.0;
	return (req & 0xF) | fmtflag;
}

// ======================================================================
// API interface
// ======================================================================

DLLCLBK void InitModule (HINSTANCE hModule)
{
}

DLLCLBK void ExitModule (HINSTANCE hModule)
{
}

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Sun (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Sun*)body;
}
