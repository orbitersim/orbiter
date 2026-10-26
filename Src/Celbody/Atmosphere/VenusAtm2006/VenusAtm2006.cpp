// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// class VenusAtmosphere_2006
// Venus atmosphere model, as used in Orbiter 2006
// ======================================================================

#define ORBITER_MODULE
#include "VenusAtm2006.h"

bool VenusAtmosphere_2006::clbkConstants (ATMCONST *atmc) const
{
	atmc->p0       = 9200e3;
	atmc->rho0     = 65;
	atmc->R        = 188.92;
	atmc->gamma    = 1.2857;
	atmc->altlimit = 200e3;
	return true;
}

const char *VenusAtmosphere_2006::clbkName () const
{
	return "2006 Edition model";
}

bool VenusAtmosphere_2006::clbkParams (const PRM_IN *prm_in, PRM_OUT *prm)
{
	double alt = (prm_in->flag & PRM_ALT ? prm_in->alt : 0.0);

	const double g0R = 0.0469506;
	// g0/R, where g0 is gravitational acceleration at sea level,
	// and R is specific gas constant for air

	// convert geometric to geopotential altitude
	const double rad = 6.05184e6;
	alt *= rad/(rad+alt);

	double e, t;

	if (alt > 90e3) {
		prm->T   = 180.0;
		e        = exp (-g0R/180.0*(alt-90e3));
		prm->p   = 1.849556e1 * e;
		prm->rho = 5.442431e-4 * e;
	} else if (alt > 70e3) {
		prm->T   = 230.0 - (alt-70e3)*0.0025;
		e        = pow (t = prm->T/230.0, -g0R/(-0.0025));
		prm->p   = 1.846393e3 * e;
		prm->rho = 4.252010e-2 * e / t;
	} else if (alt > 60e3) {
		prm->T   = 230.0;
		e        = exp (-g0R/230.0*(alt-60e3));
		prm->p   = 1.421879e4 * e;
		prm->rho = 3.274408e-1 * e;
	} else if (alt > 30e3) {
		prm->T   = 480.0 - (alt-30e3)*0.00833;
		e        = pow (t = prm->T/480.0, -g0R/(-0.00833));
		prm->p   = 8.967687e5 * e;
		prm->rho = 9.89979 * e / t;
	} else {
		prm->T   = 750.0 - alt*0.009;
		e        = pow (t = prm->T/750.0, -g0R/(-0.009));
		prm->p   = 9.2e6 * e;
		prm->rho = 65 * e / t;
	}

	return true;
}


// ======================================================================
// API interface
// ======================================================================

DLLCLBK void InitModule (HINSTANCE hModule)
{}

DLLCLBK void ExitModule (HINSTANCE hModule)
{}

DLLCLBK ATMOSPHERE *CreateAtmosphere (CELBODY2 *cbody)
{
	return new VenusAtmosphere_2006 (cbody);
}

DLLCLBK void DeleteAtmosphere (ATMOSPHERE *atm)
{
	delete (VenusAtmosphere_2006*)atm;
}

DLLCLBK char *ModelName ()
{
	return (char*)"Orbiter 2006 Venus atmosphere model";
}

DLLCLBK char *ModelDesc ()
{
	return (char*)"A simple static Venus atmosphere model. This is the model used in Orbiter 2006.";
}