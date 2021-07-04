#define ORBITER_MODULE
#include "Galsat.h"

#include <stdio.h> // temp

// ===========================================================
// Local prototypes
// ===========================================================

void GalEphem (int ksat, double mjd, double *ret);
void SampleEphem (int ksat, double simt, double interval, double *ret, Sample *sp);
double Radius (double *data);

// ===========================================================
// class GALOBJ
// Base class for Galilean Jupiter moons controlled by
// GALSAT solutions
// ===========================================================

GALOBJ::GALOBJ (OBJHANDLE hObj): CELBODY2 (hObj)
{
	sp[0].t = sp[1].t = -1e20; // invalidate
	for (int i = 0; i < 6; i++)
		sp[0].param[i] = sp[1].param[i] = 0.0;
	interval = 100.0;          // default sampling interval [s]
}

bool GALOBJ::bEphemeris () const
{
	return true;
}

int GALOBJ::clbkEphemeris (double mjd, int req, double *ret)
{
	GalEphem (ksat, mjd, ret);
	for (int i = 0; i < 6; i++) ret[i+6] = ret[i];
	return 0x1F;
}

int GALOBJ::clbkFastEphemeris (double simt, int req, double *ret)
{
	SampleEphem (ksat, simt, interval, ret, sp);
	for (int i = 0; i < 6; i++) ret[i+6] = ret[i];
	return 0x1F;
	
}

void GALOBJ::clbkInit (FILEHANDLE cfg)
{
	// Initialise the sampling points
	sp[0].t = 0;
	sp[1].t = interval;
	GalEphem (ksat, oapiTime2MJD(sp[0].t), sp[0].param);
	GalEphem (ksat, oapiTime2MJD(sp[1].t), sp[1].param);
	sp[0].rad = Radius (sp[0].param);
	sp[1].rad = Radius (sp[1].param);	
}

// ===========================================================
// Interface functions for Jupiter barycentre calculations
// ===========================================================

void JupiterBaryEphemeris (double mjd, double *ret)
{
	GalEphem (GAL_BARYCENTRE, mjd, ret);
}

void JupiterBaryFastEphemeris (double simt, double *ret, Sample *sp)
{
	SampleEphem (GAL_BARYCENTRE, simt, 100, ret, sp);
}

// ===========================================================
// Nonmember functions
// ===========================================================

// -----------------------------------------------------------
// GalEphem:
// Interface to galsat function. Maps results to Orbiter
// coordinates and units
// -----------------------------------------------------------

void GalEphem (int ksat, double mjd, double *ret)
{
	static double r[6];

	galsat (r, ret, mjd+2400000.5, ksat, 2);

	// map from default to orbiter frame of reference: xyz -> xzy
	// and change units from AU and AU/day to m and m/s

	static const double AU = 299792458.0 * 499.004783806;
	static const double AUd = AU / 86400.0;
	ret[0] = r[0] * AU;
	ret[1] = r[2] * AU;
	ret[2] = r[1] * AU;
	ret[3] = r[3] * AUd;
	ret[4] = r[5] * AUd;
	ret[5] = r[4] * AUd;
}

inline double Radius (double *data)
{
	return sqrt (data[0]*data[0] + data[1]*data[1] + data[2]*data[2]);
}

void Interpolate (double t, double *data, const Sample *s0, const Sample *s1)
{
	int i;
	double dt = s1->t - s0->t;

	// step 1: linear interpolation
	double fac = (t - s0->t) / dt;
	for (i = 0; i < 6; i++)
		data[i] = fac * (s1->param[i] - s0->param[i]) + s0->param[i];

	// step 2: radius interpolation
	fac = ((t - s0->t)/dt * s1->rad + (s1->t - t)/dt * s0->rad)/Radius(data);
	for (i = 0; i < 3; i++)
		data[i] *= fac;
	// Warning: velocities are not corrected here!
}

void SampleEphem (int ksat, double simt, double interval, double *ret, Sample *sp)
{
	Sample *s0, *s1;

	if (sp[0].t < sp[1].t) s0 = sp+0, s1 = sp+1;
	else                   s0 = sp+1, s1 = sp+0;

	if (simt >= s0->t && simt <= s1->t) { // interpolate
		Interpolate (simt, ret, s0, s1);
	} else if (simt > s1->t) {
		if (simt <= s1->t + interval) {
			s0->t = s1->t + interval;
			GalEphem (ksat, oapiTime2MJD (s0->t), s0->param);
			s0->rad = Radius (s0->param);
			Interpolate (simt, ret, s1, s0);
		} else {
			s0->t = simt;
			GalEphem (ksat, oapiTime2MJD (s0->t), s0->param);
			s0->rad = Radius (s0->param);
			for (int i = 0; i < 6; i++) ret[i] = s0->param[i];
		}
	} else {
		if (simt >= s0->t - interval) {
			s1->t = s0->t - interval;
			GalEphem (ksat, oapiTime2MJD (s1->t), s1->param);
			s1->rad = Radius (s1->param);
			Interpolate (simt, ret, s1, s0);
		} else {
			s1->t = simt;
			GalEphem (ksat, oapiTime2MJD (s1->t), s1->param);
			s1->rad = Radius (s1->param);
			s0->t = simt + interval;
			GalEphem (ksat, oapiTime2MJD (s0->t), s0->param);
			s0->rad = Radius (s0->param);
			for (int i = 0; i < 6; i++) ret[i] = s1->param[i];
		}
	}
}

// ===========================================================
// API interface
// ===========================================================

DLLCLBK void InitModule (HINSTANCE hModule)
{
	// Load the data for the Lieske perturbation solutions
	// into global data structures

	if (cd2com("Config\\Jupiter\\Data\\ephem_e15.dat")) {
		oapiWriteLogError("Galsat: file not found: Config\\Jupiter\\Data\\ephem_e15.dat");
	}
	chkgal();
}

