// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Vsop87.h"
#include <stdio.h>

#define DLLCLBK extern "C" __declspec(dllexport)

using namespace std;

inline double Radius (double *data)
{
	return sqrt (data[0]*data[0] + data[1]*data[1] + data[2]*data[2]);
}

// ===========================================================
// class VSOPOBJ
// Base class for planets controlled by VSOP87 solutions
// ===========================================================

VSOPOBJ::VSOPOBJ (OBJHANDLE hCBody): CELBODY2 (hCBody)
{
	a0 = 1.0;               // should be overwritten by derived class
	double interval = 10.0; // default sampling interval
	prec = 1e-6;            // default precision
	termidx = 0;
	termlen = 0;
	term = 0;
	sp[0].t = sp[1].t = -1e20; // invalidate
	SetSeries ('B');        // default series: spherical, J2000
}

VSOPOBJ::~VSOPOBJ ()
{
	if (termidx) delete []termidx;
	if (termlen) delete []termlen;
	if (term) delete []term;
}

bool VSOPOBJ::bEphemeris () const
{
	return true;
}

void VSOPOBJ::clbkInit (FILEHANDLE cfg)
{
	CELBODY2::clbkInit (cfg);
	oapiReadItem_float (cfg, (char*)"ErrorLimit", prec); // read custom precision from config file
	oapiReadItem_float (cfg, (char*)"SamplingInterval", interval);
}

void VSOPOBJ::SetSeries (char series)
{
	sid = toupper (series);
	fmtflag = 0;
	if (sid == 'B' || sid == 'D') fmtflag |= EPHEM_POLAR;
	if (sid == 'E')               fmtflag |= EPHEM_PARENTBARY;
}

bool VSOPOBJ::ReadData (const char *name)
{
	int nterm, cooidx, alpha, i, iused, nused = 0, ntot = 0;
	double a, b, c, tfac, err;
	TERM3 **ppterm, *pterm;

	char cbuf[256];
	sprintf (cbuf, "Config\\%s\\Data\\Vsop87%c.dat", name, sid);
	ifstream ifs (cbuf);
	if (!ifs) {
		oapiWriteLogError("VSOP87 %s: Data file not found: %s", name, cbuf);
		return false;
	}

	ifs >> nalpha;

	ppterm  = new TERM3*[(nalpha+1)*3];
	termidx = new IDX3[nalpha+1];
	termlen = new IDX3[nalpha+2];

	for (cooidx = 0; cooidx < 3; cooidx++) {
		tfac = 1.0;
		for (alpha = 0; alpha <= nalpha; alpha++) {
			ifs >> nterm;
			pterm = ppterm[cooidx*(nalpha+1)+alpha] = new TERM3[nterm];
			for (i = 0, iused = nterm; i < nterm; i++) {
				ifs >> a >> b >> c;
				if (iused == nterm) {
					pterm[i][0] = a, pterm[i][1] = b, pterm[i][2] = c;
					if (cooidx == 2) a /= a0; // radius in terms of mean SMa
					err = 2.0*sqrt (i+1.0)*a*tfac;
					if (err < prec) iused = i;
				}
			}
			termlen[alpha][cooidx] = iused;
			termidx[alpha][cooidx] = nused;
			nused += iused;
			ntot  += nterm;
			tfac *= 5.0; // don't ask
		}
		termlen[alpha][cooidx] = 0;
	}
	// now copy everything into a single array
	term = new TERM3[nused];
	for (cooidx = 0; cooidx < 3; cooidx++) {
		for (alpha = 0; alpha <= nalpha; alpha++) {
			memcpy (term+termidx[alpha][cooidx], ppterm[cooidx*(nalpha+1)+alpha], termlen[alpha][cooidx]*sizeof(TERM3));
			delete []ppterm[cooidx*(nalpha+1)+alpha];
		}
	}
	delete []ppterm;

	Init();

	oapiWriteLogV("VSOP87(%c) %s: Precision %0.1le, Terms %d/%d", sid, name, prec, nused, ntot);
	return true;
}

void VSOPOBJ::Init ()
{
	sp[0].t = 0;
	sp[1].t = interval;
	VsopEphem (oapiTime2MJD(sp[0].t), sp[0].param);
	VsopEphem (oapiTime2MJD(sp[1].t), sp[1].param);
	sp[0].rad = Radius (sp[0].param);
	sp[1].rad = Radius (sp[1].param);
}

// ===========================================================
// Name: VsopEphem()
// Desc: Return heliocentric ecliptic spherical positions and
//       velocity for planet 'obj' (VSOP_MERCURY to VSOP_NEPTUNE
//       at time 'mjd' for ecliptic and equinox J2000.
//       Values returned in 'ret' are
//       ret[0] = longitude l [rad]
//       ret[1] = latitude b  [rad]
//       ret[2] = radius r    [AU]
//       ret[3] = velocity in longitude [rad/s]
//       ret[4] = velocity in latitude  [rad/s]
//       ret[5] = radial velocity [AU/s]
// ===========================================================
void VSOPOBJ::VsopEphem (double mjd, double *ret)
{
	static const double mjd2000 = 51544.5;  // MJD date of epoch J2000
	static const double a1000   = 365250.0; // days per millenium
	static const double rsec    = 1.0/(a1000*86400.0); // 1/seconds per millenium

	static const double c0   = 299792458;     // speed of light [m/s]
	static const double tauA = 499.004783806; // light time for 1 AU [s]
	static const double AU   = c0*tauA;       // 1 AU in meters
	static const double pscl = AU;            // convert AU -> m
	static const double vscl = AU*rsec;       // convert AU/millenium -> m/s

	TERM3 *pterm;

	double a, b, c, arg, tm, termdot;
	int i, cooidx, alpha;

	// zero result array
	for (i = 0; i < 6; i++) ret[i] = 0.0;

	// set time and powers
	double t[VSOP_MAXALPHA+1];
	t[0] = 1.0;
	t[1] = (mjd-mjd2000)/a1000;
	for (i = 2; i <= VSOP_MAXALPHA; ++i) t[i] = t[i-1] * t[1];

	// term summation
	for (cooidx = 0; cooidx < 3; ++cooidx) { // loop over spatial dimensions

		for (alpha = 0; termlen[alpha][cooidx]; ++alpha) { // loop over powers of time

			pterm = term+termidx[alpha][cooidx];
			tm = termdot = 0.0;
		    for (i = 0; i < termlen[alpha][cooidx]; ++i) {
				a        = pterm[i][0];
				b        = pterm[i][1];
				c        = pterm[i][2];
				arg      = b + c * t[1];
				tm      += a * cos(arg);
				termdot -= c * a * sin(arg);
			}
			ret[cooidx] += t[alpha] * tm;
			ret[cooidx+3] += t[alpha] * termdot +
				(alpha > 0 ? alpha * t[alpha - 1] * tm : 0.0);

		} // end loop alpha
	} // end loop cooidx

	if (fmtflag & EPHEM_POLAR) {
		// convert millenium rate to second rate
		for (i = 3; i < 6; i++) ret[i] *= rsec;
		// should also convert radius from AU to m
	} else {
		double tmp;
		for (i = 0; i < 3; i++) ret[i] *= pscl;
		for (     ; i < 6; i++) ret[i] *= vscl;
		// swap y and z to map to orbiter system
		tmp = ret[1]; ret[1] = ret[2]; ret[2] = tmp;
		tmp = ret[4]; ret[4] = ret[5]; ret[5] = tmp;
	}
}

// ===========================================================
// Name: VsopFastEphem()
// Desc: Generate planetary position and velocity data at the
//       current simulation time using linear interpolation
//		 This interpolation scheme, with the sampling frequencies
//       defined below, produces the following position errors [m]:
//       Mercury: 1e-2
//       Venus:   5e-3
//       Earth:   1e-4 (?)
//       Mars:    1e-2
//       Jupiter: 7e-3
//       Saturn:  7e-3
// ===========================================================
void VSOPOBJ::VsopFastEphem (double simt, double *ret)
{
	Sample *s0, *s1;
	
	if (sp[0].t < sp[1].t) s0 = sp+0, s1 = sp+1;
	else                   s0 = sp+1, s1 = sp+0;

	if (simt >= s0->t && simt <= s1->t) { // interpolate
		Interpolate (simt, ret, s0, s1);
	} else if (simt > s1->t) {
		if (simt <= s1->t + interval) {
			s0->t = s1->t + interval;
			VsopEphem (oapiTime2MJD (s0->t), s0->param);
			if (fmtflag & EPHEM_POLAR) { // check for phase wrap in longitude
				if      (s0->param[0]-s1->param[0] >  PI) s1->param[0] += 2.0*PI;
				else if (s0->param[0]-s1->param[0] < -PI) s1->param[0] -= 2.0*PI;
			} else {
				s0->rad = Radius (s0->param);
			}
			Interpolate (simt, ret, s1, s0);
		} else {
			s0->t = simt;
			VsopEphem (oapiTime2MJD (s0->t), s0->param);
			if (!(fmtflag & EPHEM_POLAR))
				s0->rad = Radius (s0->param);
			for (int i = 0; i < 6; i++) ret[i] = s0->param[i];
		}
	} else {
		if (simt >= s0->t - interval) {
			s1->t = s0->t - interval;
			VsopEphem (oapiTime2MJD (s1->t), s1->param);
			if (fmtflag & EPHEM_POLAR) { // check for phase wrap in longitude
				if      (s1->param[0]-s0->param[0] >  PI) s0->param[0] += 2.0*PI;
				else if (s1->param[0]-s0->param[0] < -PI) s0->param[0] -= 2.0*PI;
			} else {
				s1->rad = Radius (s1->param);
			}
			Interpolate (simt, ret, s1, s0);
		} else {
			s1->t = simt;
			VsopEphem (oapiTime2MJD (s1->t), s1->param);
			s0->t = simt + interval;
			VsopEphem (oapiTime2MJD (s0->t), s0->param);
			if (fmtflag & EPHEM_POLAR) { // check for phase wrap in longitude
				if      (s0->param[0]-s1->param[0] >  PI) s1->param[0] += 2.0*PI;
				else if (s0->param[0]-s1->param[0] < -PI) s1->param[0] -= 2.0*PI;
			} else {
				s0->rad = Radius (s0->param);
				s1->rad = Radius (s1->param);
			}
			for (int i = 0; i < 6; i++) ret[i] = s1->param[i];
		}
	}
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
void VSOPOBJ::Interpolate (double t, double *data, const Sample *s0, const Sample *s1)
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


