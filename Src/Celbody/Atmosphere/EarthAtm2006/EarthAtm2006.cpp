// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// class EarthAtmosphere_2006
// Legacy atmospheric model
// ======================================================================

#define ORBITER_MODULE
#include "EarthAtm2006.h"

bool EarthAtmosphere_2006::clbkConstants (ATMCONST *atmc) const
{
	atmc->p0       = 101325;
	atmc->rho0     = 1.2250;
	atmc->R        = 286.91;
	atmc->gamma    = 1.4;
	atmc->altlimit = 200e3;
	return true;
}

const char *EarthAtmosphere_2006::clbkName () const
{
	return "2006 Edition model";
}

bool EarthAtmosphere_2006::clbkParams (const PRM_IN *prm_in, PRM_OUT *prm)
{
	double alt = (prm_in->flag & PRM_ALT ? prm_in->alt : 0.0);

	const double g0R = 0.0341643;
	// g0/R, where g0 is gravitational acceleration at sea level,
	// and R is specific gas constant for air

	// convert geometric to geopotential altitude
	const double rad = 6.37101e6;
	alt *= rad/(rad+alt);

	double e, t;

	if (alt > 105e3) {
		prm->T   = 225.66;
		e        = exp (-g0R/225.66*(alt-105e3));
		prm->p   = 0.007454273 * e;
		prm->rho = 1.15082e-7 * e;
	} else if (alt > 47e3) {
		if (alt > 79e3) {
			if (alt > 90e3) {
				prm->T   = (alt-90e3)*4e-3+165.66;
				e        = pow (t = prm->T/165.66, -g0R/4e-3);
				prm->p   = 0.1044559 * e;
				prm->rho = 2.19671e-6 *e / t;
			} else {
				prm->T   = 165.66;
				e        = exp (-g0R/165.66*(alt-79e3));
				prm->p   = 1.009601 * e;
				prm->rho = 2.12319e-5 * e;
			}
		} else {
			if (alt > 53e3) {
				prm->T   = (alt-53e3)*(-4.5e-3)+282.66;
				e        = pow (t = prm->T/282.66, -g0R/(-4.5e-3));
				prm->p   = 58.32623 * e;
				prm->rho = 0.000718878 * e / t;
			} else {
				prm->T   = 282.66;
				e        = exp (-g0R/282.66*(alt-47e3));
				prm->p   = 120.4524 * e;
				prm->rho = 0.00148459 * e;
			}
		}
	} else {
		if (alt > 11e3) {
			if (alt > 25e3) {
				prm->T   = (alt-25e3)*3e-3+216.66;
				e        = pow (t = prm->T/216.66, -g0R/3e-3);
				prm->p   = 2488.726 * e;
				prm->rho = 0.0400179 * e / t;
			} else {
				prm->T   = 216.66;
				e        = exp (-g0R/216.66*(alt-11e3));
				prm->p   = 2.26323e4 * e;
				prm->rho = 0.36392 * e;
			}
		} else {
			prm->T   = alt*(-6.5e-3)+288.16;
			e        = pow (t = prm->T/288.16, -g0R/(-6.5e-3));
			prm->p   = 1.01325e5 * e;
			prm->rho = 1.2250 * e / t;
		}
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
	return new EarthAtmosphere_2006 (cbody);
}

DLLCLBK void DeleteAtmosphere (ATMOSPHERE *atm)
{
	delete (EarthAtmosphere_2006*)atm;
}

DLLCLBK char *ModelName ()
{
	return (char*)"Orbiter 2006 Legacy Model";
}

DLLCLBK char *ModelDesc ()
{
	return (char*)"A simple static atmosphere model. This model was used in Orbiter 2006. It underestimates atmospheric density and pressure above ~120km and terminates at 200km, which simplifies orbit-keeping for LEO objects.";
}