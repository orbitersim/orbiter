// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE

#include "OrbiterAPI.h"
#include "CelbodyAPI.h"

// ===========================================================
// Local prototypes
// ===========================================================

void ELP82_init ();
void ELP82_exit ();
int  ELP82_read (double prec);
int  ELP82 (double mjd, double *r);
void Interpolate (double t, double *data, const Sample *s0, const Sample *s1);
inline double Radius (double *data)
{ return sqrt (data[0]*data[0] + data[1]*data[1] + data[2]*data[2]); }


// ======================================================================
// class Moon: interface
// ======================================================================

class Moon: public CELBODY2 {
public:
	Moon (OBJHANDLE hObj);
	void clbkInit (FILEHANDLE cfg);
	bool bEphemeris () const { return true; }
	int clbkEphemeris (double mjd, int req, double *ret);
	int clbkFastEphemeris (double simt, int req, double *ret);

private:
	double prec;      // tolerance limit
	double interval;  // sampling interval for fast ephemeris calculation
	Sample sp[2];
};

// ======================================================================
// class Moon: implementation
// ======================================================================

Moon::Moon (OBJHANDLE hObj): CELBODY2 (hObj)
{
	prec = 1e-6;
	interval = 71.0;     // sample interval [s] => interpolation error ~0.1m
	sp[0].t = sp[1].t = -1e20; // invalidate
}

void Moon::clbkInit (FILEHANDLE cfg)
{
	oapiReadItem_float (cfg, (char*)"ErrorLimit", prec);
	ELP82_read (prec);
	CELBODY2::clbkInit (cfg);

	// Initialise the sampling points
	sp[0].t = 0;
	sp[1].t = interval;
	ELP82 (oapiTime2MJD(sp[0].t), sp[0].param);
	ELP82 (oapiTime2MJD(sp[1].t), sp[1].param);
	sp[0].rad = Radius (sp[0].param);
	sp[1].rad = Radius (sp[1].param);	
}

int Moon::clbkEphemeris (double mjd, int req, double *ret)
{
	ELP82 (mjd, ret);
	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (int i = 6; i < 12; i++) ret[i] = ret[i-6];
	return req | (EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_BARYISTRUE);
}

int Moon::clbkFastEphemeris (double simt, int req, double *ret)
{
	Sample *s0, *s1;
	
	if (sp[0].t < sp[1].t) s0 = sp+0, s1 = sp+1;
	else                   s0 = sp+1, s1 = sp+0;

	if (simt >= s0->t && simt <= s1->t) { // interpolate
		Interpolate (simt, ret, s0, s1);
	} else if (simt > s1->t) {
		if (simt <= s1->t + interval) {
			s0->t = s1->t + interval;
			ELP82 (oapiTime2MJD (s0->t), s0->param);
			s0->rad = Radius (s0->param);
			Interpolate (simt, ret, s1, s0);
		} else {
			s0->t = simt;
			ELP82 (oapiTime2MJD (s0->t), s0->param);
			s0->rad = Radius (s0->param);
			for (int i = 0; i < 6; i++) ret[i] = s0->param[i];
		}
	} else {
		if (simt >= s0->t - interval) {
			s1->t = s0->t - interval;
			ELP82 (oapiTime2MJD (s1->t), s1->param);
			s1->rad = Radius (s1->param);
			Interpolate (simt, ret, s1, s0);
		} else {
			s1->t = simt;
			ELP82 (oapiTime2MJD (s1->t), s1->param);
			s1->rad = Radius (s1->param);
			s0->t = simt + interval;
			ELP82 (oapiTime2MJD (s0->t), s0->param);
			s0->rad = Radius (s0->param);
			for (int i = 0; i < 6; i++) ret[i] = s1->param[i];
		}
	}

	if (req & (EPHEM_BARYPOS | EPHEM_BARYVEL))
		for (int i = 6; i < 12; i++) ret[i] = ret[i-6];
	return req | (EPHEM_TRUEPOS | EPHEM_TRUEVEL | EPHEM_BARYISTRUE);
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


// ===========================================================
// DLL entry point
// ===========================================================

DLLCLBK void InitModule (HINSTANCE hModule)
{
	ELP82_init();
}

DLLCLBK void ExitModule (HINSTANCE hModule)
{
	ELP82_exit();
}

DLLCLBK CELBODY *InitInstance (OBJHANDLE hBody)
{
	return new Moon (hBody);
}

DLLCLBK void ExitInstance (CELBODY *body)
{
	delete (Moon*)body;
}
