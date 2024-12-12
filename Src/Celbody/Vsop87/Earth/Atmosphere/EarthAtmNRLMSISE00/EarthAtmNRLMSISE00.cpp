// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// class EarthAtmosphere_NRLMSISE00
// MSIS atmosphere model implementation
// ======================================================================

#define ORBITER_MODULE
#include "EarthAtmNRLMSISE00.h"
#include "nrlmsise-00.h"

EarthAtmosphere_NRLMSISE00::EarthAtmosphere_NRLMSISE00 (CELBODY2 *body): ATMOSPHERE (body)
{
	pmjd = -1000000;  // invalidate
	doy = 0;
}

const char *EarthAtmosphere_NRLMSISE00::clbkName () const
{
	return "NRLMSISE00";
}

bool EarthAtmosphere_NRLMSISE00::clbkConstants (ATMCONST *atmc) const
{
	atmc->p0       = 101325;
	atmc->rho0     = 1.2250;
	atmc->R        = 286.91;
	atmc->gamma    = 1.4;
	atmc->altlimit = 2500e3;
	return true;
}

bool EarthAtmosphere_NRLMSISE00::clbkParams (const PRM_IN *prm_in, PRM_OUT *prm)
{
	static struct nrlmsise_output output = {0};
	static struct nrlmsise_input input = {
		0,    // year, currently ignored
		172,  // day of year
		29000,// second in day
		0,    // altitude [km]
		0,    // geodetic latitude [deg]
		0,    // geodetic longitude [deg]
		0,    // local apparent solar time (hours)
		140,  // 81 day average of F10.7 flux (centered on doy)
		140,  // daily F10.7 flux for previous day
		3.0,  // magnetic index(daily)
		NULL  // pointer to detailed magnetic values
	};
	static struct nrlmsise_flags flags = {0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

	double mjd = oapiGetSimMJD();

	// second in the day calculation
	double h, ijd;
	h = 24.0 * modf (mjd, &ijd); // hour in the day
	
	// day in year calculation
	if ((int)mjd != pmjd) { // do this only when the day number changes
		double c, e, mjd2;
		int a, b, f, m, y;
		if (ijd < -100840) {
			c = ijd + 2401525.0;
		} else {
			b = (int)((ijd + 532784.75) / 36524.25);
			c = ijd + 2401526.0 + (b - b/4);
		}
		a = (int)((c-122.1)/365.25);
		e = 365.0 * a + a/4;
		f = (int)((c-e)/30.6001);
		m = f-1 - 12 * (f/14);
		y = a-4715 - ((7 + m)/10) - 1;
		double a2 = (double)(10000*y + 1231);
		if (a2 <= 15821004.1) b = (y+4716)/4 - 1181;
		else                  b = y/400 - y/100 + y/4;
		mjd2 = 365.0*y + b - 678576.0;
		doy = (int)(mjd-mjd2);
		pmjd = (int)mjd;
	}

	input.doy    = doy;
	input.sec    = h*3600.0;

	input.alt    = (prm_in->flag & PRM_ALT ? prm_in->alt*1e-3 : 0.0);
	input.g_long = (prm_in->flag & PRM_LNG ? prm_in->lng*DEG : 0.0);
	input.g_lat  = (prm_in->flag & PRM_LAT ? prm_in->lat*DEG : 0.0);
	input.lst    = h+input.g_long/15.0;
	input.f107A  = (prm_in->flag & PRM_FBR ? prm_in->f107bar : 140.0);
	input.f107   = (prm_in->flag & PRM_F   ? prm_in->f107 : input.f107A);
	input.ap     = (prm_in->flag & PRM_AP  ? prm_in->ap : 3.0);

	gtd7 (&input, &flags, &output);
	double n = output.d[0]+output.d[1]+output.d[2]+output.d[3]+output.d[4]+output.d[6]+output.d[7]; // total number density [1/cm^3]
	static double k = 1.38066e-23*1e6; // Boltzmann constant and scale from cm^-3 to m^-3
	prm->T = output.t[1];
	prm->p = n*k*prm->T;
	prm->rho = output.d[5]*1e3;

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
	return new EarthAtmosphere_NRLMSISE00 (cbody);
}

DLLCLBK void DeleteAtmosphere (ATMOSPHERE *atm)
{
	delete (EarthAtmosphere_NRLMSISE00*)atm;
}

DLLCLBK char *ModelName ()
{
	return (char*)"NRLMSIS-00 atmosphere model";
}

DLLCLBK char *ModelDesc ()
{
	return (char*)"An empirical atmosphere model developed by Mike Picone, Alan Hedin, and Doug Drob based on the MSISE90 model. The MSISE90 model describes the neutral temperature and densities in Earth's atmosphere from ground to thermospheric heights.\r\n\r\nSee \"Earth Atmosphere Model\" in \"Orbiter Technical Reference\" for details.";
}