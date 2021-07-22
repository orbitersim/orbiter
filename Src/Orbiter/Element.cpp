// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Orbiter.h"
#include "Element.h"
#include "Config.h"
#include <fstream>
#include <windows.h>
#include <stdio.h>

using namespace std;

// =======================================================================
// Externals

extern Orbiter *g_pOrbiter;
extern TimeData td;
extern char DBG_MSG[256];

static const double E_CIRCLE_LIMIT = 1e-8;
static const double I_NOINC_LIMIT  = 1e-8;

// =======================================================================
// class Elements

Elements::Elements ()
{
	a         = 1.0;
	e         = 0.0;
	i         = 0.0;
	theta     = 0.0;
	omegab    = 0.0;
	L         = 0.0;
	mjd_epoch = Jepoch2MJD (2000.0); // default
	t_epoch   = (mjd_epoch-td.MJD_ref)*86400.0;
	priv_ma0  = 1e10;
}

Elements::Elements (double _a, double _e, double _i,
	double _theta, double _omegab, double _L, double _mjd_epoch)
{
	a         = _a;
	e         = _e;
	i         = _i;
	theta     = _theta;
	omegab    = _omegab;
	L         = _L;
	mjd_epoch = _mjd_epoch;
	t_epoch   = (mjd_epoch-td.MJD_ref)*86400.0;
	priv_ma0  = 1e10;
}

Elements::Elements (const Elements &el)
{
	Set (el);
	priv_ma0  = 1e10;
}

Elements::Elements (char *fname)
{
	double epoch;
	ifstream ifs (g_pOrbiter->ConfigPath (fname));
	if (!GetItemReal (ifs, "Epoch", epoch))           epoch  = 2000.0;
	mjd_epoch = Jepoch2MJD (epoch);
	t_epoch   = (mjd_epoch-td.MJD_ref)*86400.0;
	if (!GetItemReal (ifs, "SemiMajorAxis", a))       a      = 1.0;
	if (!GetItemReal (ifs, "Eccentricity", e))        e      = 0.0;
	if (!GetItemReal (ifs, "Inclination", i))         i      = 0.0;
	if (!GetItemReal (ifs, "LongAscNode", theta))     theta  = 0.0;
	if (!GetItemReal (ifs, "LongPerihelion", omegab)) omegab = 0.0;
	if (!GetItemReal (ifs, "MeanLongitude", L))       L      = 0.0;
}

void Elements::Set (double _a, double _e, double _i,
	double _theta, double _omegab, double _L, double _mjd_epoch)
{
	a         = _a;
	e         = _e;
	i         = _i;
	theta     = _theta;
	omegab    = _omegab;
	L         = _L;
	mjd_epoch = _mjd_epoch;
	t_epoch   = (mjd_epoch-td.MJD_ref)*86400.0;
}

void Elements::Set (const Elements &el)
{
	a         = el.a;
	e         = el.e;
	i         = el.i;
	theta     = el.theta;
	omegab    = el.omegab;
	L         = el.L;
	mjd_epoch = el.mjd_epoch;
	Setup (el.priv_m, el.priv_M, el.mjd_epoch);
}

void Elements::SetMasses (double m, double M)
{
	priv_m  = m;
	priv_M  = M;
	priv_mu = Ggrav * (priv_M+priv_m);
}

void Elements::Setup (double m, double M, double mjd)
{
	// register masses
	SetMasses (m, M);

	// set derived elements
	priv_le  = a * e;                       // linear eccentricity
	priv_pd  = a - priv_le;                 // periapsis distance
	priv_ad  = a + priv_le;                 // apoapsis distance
	priv_p   = max (0.0, a * (1.0-e*e));    // parameter of conic section
	priv_muh = sqrt (priv_mu/priv_p);       // mu/h

	if (e < 1.0) {    // closed orbit
		priv_b   = a * sqrt (1.0-e*e);      // semi-minor axis
		priv_tmp = sqrt ((1.0+e)/(1.0-e));  // for calculation of true anomaly
		priv_n   = sqrt (priv_mu/(a*a*a));  // 2pi/T
	} else {          // open orbit
		priv_b   = a * sqrt (e*e-1.0);      // imaginary!
		priv_n   = sqrt (-priv_mu/(a*a*a)); // imaginary!
	}

	priv_T   = Pi2/priv_n;			        // orbital period

	// Adjust mean longitude to start time mjd
	L += (mjd-mjd_epoch)*86400.0*priv_n;
	//if ((L = fmod (L, Pi2)) < 0.0) L += Pi2;
	mjd_epoch  = mjd;                         // reference epoch (mjd format)
	t_epoch    = (mjd_epoch-td.MJD_ref)*86400.0; // reference epoch (simt format)
	priv_tau   = t_epoch - (L-omegab)/priv_n; // perihel time (as offset from mjd_epoch)
	priv_omega = omegab-theta;                // argument of periapsis

	sint = sin(theta),      cost = cos(theta);
	sini = sin(i),          cosi = cos(i);
	sino = sin(priv_omega), coso = cos(priv_omega);

	// specific angular momentum
	double h = sqrt(priv_mu * priv_p); // magnitude of momentum
	priv_H.Set(h*sini*sint, h*cosi, -h*sini*cost);
}

void Elements::Reset (double _a, double _e, double _i,
	double _theta, double _omegab, double _L, double _mjd_epoch)
{
	// set primary elements
	a         = _a;
	e         = _e;
	i         = _i;
	theta     = _theta;
	omegab    = _omegab;
	L         = _L;
	mjd_epoch = _mjd_epoch;
	Setup (priv_m, priv_M, mjd_epoch);
}

double Elements::TrueAnomaly_from_EccAnomaly (double ea) const
{
	if (e < 1.0) {
		return 2.0 * atan (priv_tmp * tan (0.5*ea));
	} else {
		// ea = normangle (ea);  // normangle seems to cause problems here!
		                         // ea can apparently get > Pi for hyperbolic orbits
		double chea = cosh (ea);
		double tra = acos ((e - chea)/(e*chea - 1.0));
		return (ea >= 0.0 ? tra : -tra);
		// check that sign is set correctly here!
	}
}

double Elements::EccAnomaly_from_TrueAnomaly (double ta) const
{
	if (e < 1.0) {
		return 2.0 * atan (tan (0.5*ta) / priv_tmp);
	} else { // for open orbits the following needs to be tested
		double costa = cos(ta);
		double ea = acosh ((e + costa)/(e*costa + 1.0));
		return (ta >= 0.0 ? ea : -ea);
	}
}

double Elements::MeanAnomaly_from_EccAnomaly (double ea) const
{
	if (e < 1.0) {
		return ea - e*sin(ea);
	} else {
		return e*sinh(ea) - ea;
		// check that sign is set correctly here!
	}
}

double Elements::EccAnomaly (double ma) const
{
	// iterative calculation of eccentric anomaly from mean anomaly

	const int niter = 16;
	const double tol = 1e-14;
	double res, E;
	int i;

	E = (fabs (ma-priv_ma0) < 1e-2 ? priv_ea0 : ma);
	// initial guess for E: use previous calculation or mean anomaly

	if (e < 1.0) { // closed orbit: solve M = E - e sin E
		res = ma - E + e * sin(E);
		if (fabs (res) > fabs (ma))
			E = 0.0, res = ma;
		for (i = 0; fabs(res) > tol && i < niter; i++) {
			E += (max (-1.0, min (1.0, res/(1.0 - e * cos(E)))));
			// limit step size to avoid numerical instabilities
			res = ma - E + e * sin(E);
		}
	} else {       // open orbit : solve M = e sinh E - E
		res = ma - e * sinh(E) + E;
		if (fabs (res) > fabs (ma)) // bad choice of initial E
			E = 0.0, res = ma;      // last resort
		for (i = 0; fabs(res) > tol && i < niter; i++) {
			E += (max (-1.0, min (1.0, res/(e * cosh(E) - 1.0))));
			// limit step size to avoid numerical instabilities
			res = ma - e * sinh(E) + E;
		}
	}

//#define OUTPUT_RESIDUAL
#ifdef OUTPUT_RESIDUAL
	static double res_max_closed = 0, res_max_open = 0;
	if (e < 1.0) {
		if (res > res_max_closed) res_max_closed = res;
	} else {
		if (res > res_max_open) res_max_open = res;
	}
	sprintf (DBG_MSG, "res_max_closed=%g, res_max_open=%g", res_max_closed, res_max_open);
#endif

	priv_ma0 = ma;
	priv_ea0 = E;  // store value to initialise next calculation
	return E;
}

bool Elements::AscendingNode (Vector &asc) const
{
	double d = Rdist (priv_omega);
	asc.Set (priv_N * d);
	return (d >= 0.0);
}

bool Elements::DescendingNode (Vector &desc) const
{
	double d = Rdist (priv_omega+Pi);
	desc.Set (priv_N * -d);
	return (d >= 0.0);
}

void Elements::RelPos (double &r, double &ta, double t) const
{
	//if (t < 0.0) t = SimT;

	if (e < E_CIRCLE_LIMIT) // circular orbit
		ta = priv_n * fmod (t-priv_tau, priv_T);
	else {
		double ma = MeanAnomaly (t);
		if (e < 1.0) ma = posangle (ma);
		// reduce to range [0,pi] for closed orbits to improve convergence
		ta = TrueAnomaly (ma);
	}
	r = priv_p / (1.0 + e * cos(ta));
}

void Elements::Pol2Crt (double r, double ta, Vector &pos) const
{
	double sinto = sin (ta + priv_omega);
	double costo = cos (ta + priv_omega);
	pos.x = r * (cost*costo - sint*sinto*cosi);
	pos.z = r * (sint*costo + cost*sinto*cosi);
	pos.y = r * sinto * sini;
}

void Elements::PosVel (Vector &pos, Vector &vel, double t) const
{
	double r, ta;

	RelPos (r, ta, t);
	Pol2Crt (r, ta, pos);

	// there should be something more efficient!
	double vx = -priv_muh * sin (ta);
	double vz =  priv_muh * (e + cos(ta));
	double thetav = atan2 (vz, vx);
	double rv = sqrt (vx*vx + vz*vz);
	double sinto = sin (thetav + priv_omega);
	double costo = cos (thetav + priv_omega);
	vel.x = rv * (cost*costo - sint*sinto*cosi);
	vel.z = rv * (sint*costo + cost*sinto*cosi);
	vel.y = rv * sinto * sini;
}

Vector Elements::Pos (double t) const
{
	double r, ta;
	Vector pos;
	RelPos (r, ta, t);
	Pol2Crt (r, ta, pos);
	return pos;
}

void Elements::PosVel_TA (Vector &pos, Vector &vel, double ta) const
{
	double r = Rdist (ta);
	Pol2Crt (r, ta, pos);

	// there should be something more efficient!
	double vx = -priv_muh * sin (ta);
	double vz =  priv_muh * (e + cos(ta));
	double thetav = atan2 (vz, vx);
	double rv = hypot (vx, vz);
	double sinto = sin (thetav + priv_omega);
	double costo = cos (thetav + priv_omega);
	vel.x = rv * (cost*costo - sint*sinto*cosi);
	vel.z = rv * (sint*costo + cost*sinto*cosi);
	vel.y = rv * sinto * sini;
}

double Elements::Spd_TA(double ta) const
{
	double vx = -priv_muh * sin(ta);
	double vz = priv_muh * (e + cos(ta));
	return hypot(vx, vz);
}

void Elements::Update (Vector &pos, Vector &vel)
{
	double sinto, costo, vx, vz, thetav;

	if (e < E_CIRCLE_LIMIT) { // circular orbit
		priv_ma = priv_ea = priv_tra = priv_n * fmod (td.SimT1-priv_tau, priv_T);
	} else {
		priv_ma = MeanAnomaly (td.SimT1);
		if (e < 1.0) priv_ma = posangle(priv_ma);
		// for periodic orbits, normalise mean anomaly to [0,pi]
		priv_ea = EccAnomaly (priv_ma);
		priv_tra = TrueAnomaly_from_EccAnomaly (priv_ea);
	}
	priv_r = priv_p / (1.0 + e * cos(priv_tra));

	sinto = sin (priv_tra + priv_omega);
	costo = cos (priv_tra + priv_omega);
	priv_R.x = priv_r * (cost*costo - sint*sinto*cosi);
	priv_R.z = priv_r * (sint*costo + cost*sinto*cosi);
	priv_R.y = priv_r * sinto * sini;

	// there should be something more efficient!
	vx = -priv_muh * sin (priv_tra);
	vz =  priv_muh * (e + cos(priv_tra));
	thetav = atan2 (vz, vx);
	priv_v = sqrt (vx*vx + vz*vz);
	sinto = sin (thetav + priv_omega);
	costo = cos (thetav + priv_omega);
	priv_V.x = priv_v * (cost*costo - sint*sinto*cosi);
	priv_V.z = priv_v * (sint*costo + cost*sinto*cosi);
	priv_V.y = priv_v * sinto * sini;

	priv_ml = posangle (priv_ma + omegab);
	priv_trl = posangle (priv_tra + omegab);

	// time of next periapsis and apoapsis passage
	if ((priv_Tpe = -priv_ma/priv_n) < 0.0) priv_Tpe += priv_T;
	if ((priv_Tap = priv_Tpe-0.5*priv_T) < 0.0) priv_Tap += priv_T;

	pos.Set (priv_R);
	vel.Set (priv_V);
}

void Elements::Calculate (const Vector &R, const Vector &V, double simt)
{
	bool closed_orbit;

	priv_R.Set (R); // store radius vector
	priv_V.Set (V); // store velocity vector

	// auxiliary vectors and norms
	double v2 = V.length2();
	priv_v    = sqrt (v2);
	priv_r    = R.length();
	priv_H.Set (crossp (V, R)); // left-handed coordinates!
	double h  = priv_H.length();
	double rv = dotp (R, V);

	// semi-major axis
	a = priv_r * priv_mu / (2.0*priv_mu - priv_r*v2);

	//priv_E.Set ((R * (v2-priv_mu/priv_r) - V * rv)/priv_mu);
	priv_E.Set (R * (1.0/priv_r - 1.0/a) - V * (rv/priv_mu));

	e = priv_E.length();
	closed_orbit = (e < 1.0);

	// inclination
	i = acos (priv_H.y/h);
	// '-' because we are in left-handed coord system!
	// i.e. cross product points the other way!
	sini = sin(i), cosi = cos(i);

	// derived shape parameters
	priv_le  = a * e;                      // linear eccentricity
	priv_pd  = a - priv_le;                // periapsis distance

	priv_p = max (0.0, a * (1.0-e*e));     // parameter of conic section
	priv_muh   = sqrt (priv_mu/priv_p);    // mu/h

	if (closed_orbit) {
		priv_ad  = a + priv_le;            // apoapsis distance
		priv_b   = a * sqrt (1.0-e*e);     // semi-minor axis
		priv_tmp = sqrt ((1.0+e)/(1.0-e)); // for calculation of true anomaly
		priv_n   = sqrt (priv_mu/(a*a*a)); // 2pi/T
		priv_T   = Pi2/priv_n;             // orbit period
	} else {
		priv_b = a * sqrt (e*e-1.0);       // imaginary axis
		priv_n = sqrt (priv_mu/(-a*a*a));
	}


	// longitude of ascending node
	if (i > I_NOINC_LIMIT) {
		double tmp = 1.0/_hypot (priv_H.z, priv_H.x);
		priv_N.Set (-priv_H.z*tmp, 0.0, priv_H.x*tmp); // unit vector
		theta = acos (priv_N.x);
		if (priv_N.z < 0.0) theta = Pi2-theta;
	} else {
		priv_N.Set (1.0, 0.0, 0.0);
		theta = 0.0; // convention for equatorial orbits
	}
	sint = sin(theta), cost = cos(theta);

	// argument of periapsis
	if (e > E_CIRCLE_LIMIT) {
		if (i > I_NOINC_LIMIT) {
			double arg = dotp (priv_N, priv_E)/e;
			if      (arg < -1.0) priv_omega = Pi;
			else if (arg >  1.0) priv_omega = 0.0;
			else                 priv_omega = acos (arg);
			if (priv_E.y < 0.0) priv_omega = Pi2-priv_omega;
		} else {
			if ((priv_omega = atan2 (priv_E.z, priv_E.x)) < 0.0)
				priv_omega += Pi2;
		}	
	} else {
		priv_omega = 0.0; // convention for circular orbits
	}
	sino = sin(priv_omega), coso = cos(priv_omega);

	// longitude of periapsis
	if ((omegab = theta + priv_omega) >= Pi2) omegab -= Pi2;

	// true anomaly
	if (e > E_CIRCLE_LIMIT) {
		priv_tra = acos (dotp (priv_E, R)/(e*priv_r));
		if (rv < 0.0) priv_tra = Pi2-priv_tra;
	} else {
		if (i > I_NOINC_LIMIT) {
			priv_tra = acos (dotp (priv_N, R)/priv_r);
			if (dotp (priv_N, V) > 0.0) priv_tra = Pi2-priv_tra;
		} else {
			priv_tra = acos (R.x/priv_r);
			if (V.x > 0.0) priv_tra = Pi2-priv_tra;
		}
	}

	// true longitude
	if ((priv_trl = omegab + priv_tra) >= Pi2) priv_trl -= Pi2;

	if (closed_orbit) {

		// eccentric anomaly
		if (e > E_CIRCLE_LIMIT) {
			priv_ea = atan2 (rv*sqrt (a/priv_mu), a-priv_r);
		} else {
			priv_ea = priv_tra;
		}

		// mean anomaly
		priv_ma = priv_ea - e*sin(priv_ea);

		// time of next periapsis and apoapsis passage
		if ((priv_Tpe = -priv_ma/priv_n) < 0.0) priv_Tpe += priv_T;
		if ((priv_Tap = priv_Tpe-0.5*priv_T) < 0.0) priv_Tap += priv_T;

	} else { // open orbit

		// eccentric anomaly
		double costra = cos (priv_tra);
		priv_ea = acosh ((e + costra) / (1.0 + e * costra));
		if (priv_tra >= Pi) priv_ea = -priv_ea;

		// mean anomaly
		priv_ma = e*sinh(priv_ea) - priv_ea;

		// time of periapsis passage
		priv_Tpe = -priv_ma/priv_n;
		//if (priv_ma < 0.0) priv_ma += Pi2;
	}

	priv_tau = simt + priv_Tpe;            // time of periapsis passage
	if (closed_orbit) priv_tau -= priv_T;  // last passage (why?)

	// mean longitude at epoch
	if (closed_orbit) {
		L = posangle (omegab + priv_n * (t_epoch-priv_tau));
		priv_ml = posangle (priv_ma + omegab);
	} else {
		L = omegab + priv_n * (t_epoch-priv_tau);
		priv_ml = priv_ma + omegab;
	}
	//L = omegab - priv_n*priv_tau;

	// mean longitude

#ifdef UNDEF
	// DEBUG OUTPUT
	if (!closed_orbit) {
		double tra_limit = acos(-1.0/e);
		sprintf (DBG_MSG, "tra=%f°, tra_limit=%f°, diff=%g°", priv_tra*DEG, tra_limit*DEG, (fabs(tra_limit)-fabs(priv_tra))*DEG);
	}
#endif
}

void Elements::PlaneCoeffs (const Vector &R, const Vector &V, double &a, double &b, double &c)
{
	// warning: may need unit vectors for R and V
	// for numerical stability
	a = R.y*V.z - R.z*V.y;
	b = R.x*V.z - R.z*V.x;
	c = R.x*V.y - R.y*V.x;
}
