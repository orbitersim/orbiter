// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE
#include "Satsat.h"

#include <stdio.h> // temp
#define NSAT 8

using namespace std;

// ===========================================================
// Local prototypes
// ===========================================================

static void SatEphem (int ksat, double mjd, double *ret);
static void SampleEphem (int ksat, double simt, double *ret);

static const char *satname[NSAT] = {
	"Mimas", "Enceladus", "Tethys", "Dione", "Rhea", "Titan", "Hyperion", "Iapetus"
};

static double sample_dt[NSAT];   // sampling intervals
static Sample sample[NSAT][2];   // interpolation samples
static double pEphemT[NSAT];     // time of last full solution
static double pEphemP[NSAT][6];  // last full solution
static double pInterpT[NSAT];    // time of last interpolated solution
static double pInterpP[NSAT][6]; // last interpolated solution

// ===========================================================
// class SATOBJ
// Base class for Saturn moons controlled by
// TASS17 solutions
// ===========================================================

SATOBJ::SATOBJ (OBJHANDLE hObj, int is, double dt): CELBODY2 (hObj)
{
	ksat = is;                                     // body id
	sample_dt[ksat] = dt;                          // sampling interval
	sample[ksat][0].t = sample[ksat][1].t = -1e20; // invalidate
	pInterpT[ksat] = -1;                           // invalidate

	// write some statistics to the orbiter log
	oapiWriteLogV("SATSAT %s: Terms %d", satname[ksat], nterm(ksat));
}

bool SATOBJ::bEphemeris () const
{
	return true;
}

int SATOBJ::clbkEphemeris (double mjd, int req, double *ret)
{
	SatEphem (ksat, mjd, ret);
	for (int i = 0; i < 6; i++) ret[i+6] = ret[i];
	return 0x1F;
}

int SATOBJ::clbkFastEphemeris (double simt, int req, double *ret)
{
	SampleEphem (ksat, simt, ret);
	for (int i = 0; i < 6; i++) ret[i+6] = ret[i];

#ifdef UNDEF
	// temporary: compare interpolated with direct solution
	if (ksat == SAT_IAPETUS) {
		double r2[6];
		static double resmax = 0.0;
		SatEphem (ksat, oapiTime2MJD(simt), r2);
		double dst = sqrt ((r2[0]-ret[0])*(r2[0]-ret[0]) + (r2[1]-ret[1])*(r2[1]-ret[1]) + (r2[2]-ret[2])*(r2[2]-ret[2]));
		if (dst > resmax) resmax = dst;
		sprintf (oapiDebugString(), "dt=%f, residual=%g m", sample_dt[ksat], resmax);
	}
#endif

	return 0x1F;
}

// ===========================================================
// Nonmember functions
// ===========================================================

// -----------------------------------------------------------
// SatEphem:
// Interface to TASS1.7 function posired. Maps results to Orbiter
// coordinates and units
// -----------------------------------------------------------

void SatEphem (int ksat, double mjd, double *ret)
{
	int i;

	if (mjd == pEphemT[ksat]) {

		for (i = 0; i < 6; i++) ret[i] = pEphemP[ksat][i];

	} else {

		static double r[6];

		posired (mjd+2400000.5, ksat, r, r+3);

		// map from default to orbiter frame of reference: xyz -> xzy
		// and change units from AU and AU/year to m and m/s

		static const double AU = 299792458.0 * 499.004783806;
		static const double AUy = AU / (86400.0 * 365.25);
		ret[0] = r[0] * AU;
		ret[1] = r[2] * AU;
		ret[2] = r[1] * AU;
		ret[3] = r[3] * AUy;
		ret[4] = r[5] * AUy;
		ret[5] = r[4] * AUy;

		pEphemT[ksat] = mjd;
		for (i = 0; i < 6; i++) pEphemP[ksat][i] = ret[i];

	}
}

inline double Radius (double *data)
{
	return sqrt (data[0]*data[0] + data[1]*data[1] + data[2]*data[2]);
}

// ===========================================================
// Name: Interpolate()
// Desc: Interpolates position and velocity using a Cubic 
//       Hermite Spline. Replaces the old linear interpolation 
//       method that was causing the interpolated velocity to 
//       deviate from the derivative.
//       Values returned in 'data' are the interpolated
//       position (data[0..2]) and velocity (data[3..5]).
//       't' is the time to interpolate at, bounded by
//       samples 's0' and 's1'.
// ===========================================================
static void Interpolate (double t, double *data, const Sample *s0, const Sample *s1)
{
	int i;
	double dt = s1->t - s0->t;

	if (dt == 0.0) {
		for (i = 0; i < 6; i++) data[i] = s0->param[i];
		return;
	}
	
	// Step 1: Calculate the normalized time 'u' between the two sample points (range 0.0 to 1.0)
	double u = (t - s0->t) / dt;
	double u2 = u * u;
	double u3 = u2 * u;

	// Step 2: Compute the Hermite basis functions for position interpolation
	double h00 = 2.0 * u3 - 3.0 * u2 + 1.0;
	double h10 = u3 - 2.0 * u2 + u;
	double h01 = -2.0 * u3 + 3.0 * u2;
	double h11 = u3 - u2;

	// Step 3: Compute the derivatives of the Hermite basis functions for velocity interpolation
	double dh00 = 6.0 * u2 - 6.0 * u;
	double dh10 = 3.0 * u2 - 4.0 * u + 1.0;
	double dh01 = -6.0 * u2 + 6.0 * u;
	double dh11 = 3.0 * u2 - 2.0 * u;

	// Step 4: Apply the basis functions to the position and velocity vectors
	for (i = 0; i < 3; i++) {
		double p0 = s0->param[i];
		double p1 = s1->param[i];
		
		// Scale the endpoint velocities by the time interval (dt)
		double v0 = s0->param[i+3] * dt;
		double v1 = s1->param[i+3] * dt;

		// Calculate final interpolated position
		data[i] = h00 * p0 + h10 * v0 + h01 * p1 + h11 * v1;
		
		// Calculate final interpolated velocity (un-scaled by dt)
		data[i+3] = (dh00 * p0 + dh10 * v0 + dh01 * p1 + dh11 * v1) / dt;
	}
}

void SampleEphem (int ksat, double simt, double *ret)
{
	int i;

	if (simt == pInterpT[ksat]) {

		for (i = 0; i < 6; i++) ret[i] = pInterpP[ksat][i];

	} else {

		Sample *s0, *s1;
		Sample *sp = sample[ksat];
		double interval = sample_dt[ksat];

		if (sp[0].t < sp[1].t) s0 = sp+0, s1 = sp+1;
		else                   s0 = sp+1, s1 = sp+0;

		if (simt >= s0->t && simt <= s1->t) { // interpolate
			Interpolate (simt, ret, s0, s1);
		} else if (simt > s1->t) {
			if (simt <= s1->t + interval) {
				s0->t = s1->t + interval;
				SatEphem (ksat, oapiTime2MJD (s0->t), s0->param);
				s0->rad = Radius (s0->param);
				Interpolate (simt, ret, s1, s0);
			} else {
				s0->t = simt;
				SatEphem (ksat, oapiTime2MJD (s0->t), s0->param);
				s0->rad = Radius (s0->param);
				for (i = 0; i < 6; i++) ret[i] = s0->param[i];
			}
		} else {
			if (simt >= s0->t - interval) {
				s1->t = s0->t - interval;
				SatEphem (ksat, oapiTime2MJD (s1->t), s1->param);
				s1->rad = Radius (s1->param);
				Interpolate (simt, ret, s1, s0);
			} else {
				s1->t = simt;
				SatEphem (ksat, oapiTime2MJD (s1->t), s1->param);
				s1->rad = Radius (s1->param);
				s1->t = simt + interval;
				SatEphem (ksat, oapiTime2MJD (s0->t), s0->param);
				s0->rad = Radius (s0->param);
				for (i = 0; i < 6; i++) ret[i] = s1->param[i];
			}
		}
		pInterpT[ksat] = simt;
		for (i = 0; i < 6; i++) pInterpP[ksat][i] = ret[i];

	}
}

// ===========================================================
// Saturn barycentre calculations
// ===========================================================

static const double M_saturn = 5.6846272e+26;   // Saturn mass
static const double M_titan = 1.35e23/M_saturn; // fractional Titan mass
static const double M_iapetus = 1.6e21/M_saturn; // fractional Iapetus mass

void SaturnEphemeris (double mjd, double *ret)
{
	double r[6];
	int i;

	for (i = 0; i < 6; i++) ret[i] = 0.0;
	SatEphem (SAT_TITAN, mjd, r);
	for (i = 0; i < 6; i++) ret[i] -= r[i]*M_titan;
	SatEphem (SAT_IAPETUS, mjd, r);
	for (i = 0; i < 6; i++) ret[i] -= r[i]*M_iapetus;
}

void SaturnFastEphemeris (double simt, double *ret)
{
	double r[6];
	int i;

	for (i = 0; i < 6; i++) ret[i] = 0.0;
	SampleEphem (SAT_TITAN, simt, r);
	for (i = 0; i < 6; i++) ret[i] -= r[i]*M_titan;
	SampleEphem (SAT_IAPETUS, simt, r);
	for (i = 0; i < 6; i++) ret[i] -= r[i]*M_iapetus;

#ifdef UNDEF
	double dst = 0.0;
	for (i = 0; i < 3; i++) dst += ret[i]*ret[i];
	dst = sqrt(dst);
	sprintf (oapiDebugString(), "bc: %g", dst);
#endif
}

// ===========================================================
// API interface
// ===========================================================

DLLCLBK void InitModule (HINSTANCE hModule)
{
	// Load the data for the TASS 1.7 perturbation solutions
	// into global data structures

	ReadData ("Config\\Saturn\\Data\\tass17.dat", 0);

	// invalidate all data structures
	int i;
	for (i = 0; i < NSAT; i++) {
		pInterpT[i] = -1;
		sample[i][0].t = sample[i][1].t = -1e20; // invalidate
	}
}
