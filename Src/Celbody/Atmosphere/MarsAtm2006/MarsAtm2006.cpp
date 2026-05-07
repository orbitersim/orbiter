// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// class MarsAtmosphere_2006
// Mars atmosphere model, as used in Orbiter 2006
// ======================================================================

#define ORBITER_MODULE
#include "MarsAtm2006.h"

bool MarsAtmosphere_2006::clbkConstants (ATMCONST *atmc) const
{
	atmc->p0       = 0.61e3;
	atmc->rho0     = 0.02;
	atmc->R        = 188.92;
	atmc->gamma    = 1.2941;
	atmc->altlimit = 100e3;
	return true;
}

const char *MarsAtmosphere_2006::clbkName () const
{
	return "2006 Edition model";
}

bool MarsAtmosphere_2006::clbkParams (const PRM_IN *prm_in, PRM_OUT *prm)
{
	double alt = (prm_in->flag & PRM_ALT ? prm_in->alt : 0.0);

	const double g0R = 0.0197275;
	// g0/R, where g0 is gravitational acceleration at sea level,
	// and R is specific gas constant

	// convert geometric to geopotential altitude
	const double rad = 3.38992e6;
	alt *= rad/(rad+alt);

	double e, t;

	if (alt > 14e3) {
		if (alt > 30e3) {
			prm->T = 165.0;
			e        = exp (-g0R/165.0*(alt-30e3));
			prm->p   = 2.39322e1 * e;
			prm->rho = 9.27323e-4 * e;
		} else if (alt > 20e3) {
			prm->T   = (alt-20e3)*(-1.5e-3)+180.0;
			e        = pow (t = prm->T/180.0, -g0R/(-1.5e-3));
			prm->p   = 7.51564e1 * e;
			prm->rho = 2.66947e-3 * e / t;
		} else {
			prm->T   = 180.0;
			e        = exp (-g0R/180.0*(alt-14e3));
			prm->p   = 1.45061e2 * e;
			prm->rho = 5.1524e-3 * e;
		}
	} else {
		if (alt > 4e3) {
			prm->T   = (alt-4e3)*(-2e-3)+200.0;
			e        = pow (t = prm->T/200.0, -g0R/(-2e-3));
			prm->p   = 4.101e2 * e;
			prm->rho = 1.31097e-2 * e / t;
		} else if (alt > 2e3) {
			prm->T   = 200.0;
			e        = exp (-g0R/200.0*(alt-2e3));
			prm->p   = 4.99534e2 * e;
			prm->rho = 1.59687e-2 * e;
		} else {
			prm->T   = alt*2.5e-3+195.0;
			e        = pow (t = prm->T/195.0, -g0R/2.5e-3);
			prm->p   = 610.0 * e;
			prm->rho = 0.02 * e / t;
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
	return new MarsAtmosphere_2006 (cbody);
}

DLLCLBK void DeleteAtmosphere (ATMOSPHERE *atm)
{
	delete (MarsAtmosphere_2006*)atm;
}

DLLCLBK char *ModelName ()
{
	return (char*)"Orbiter 2006 Mars atmosphere model";
}

DLLCLBK char *ModelDesc ()
{
	return (char*)"A simple static Mars atmosphere model. This is the model used in Orbiter 2006.";
}